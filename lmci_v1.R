####REQUIRED LIBRARIES####

require("restatapi")
library(tidyverse)
library(Hmisc)
library(dplyr)
library(ggplot2)
library(xlsx)
library(readxl)
library(openxlsx)
library(data.table)
library(lubridate)
library(stringr)
library(fst)
library(tidyr)
library(openxlsx)
library(hhi)
library(sf)
library(stringi)
library(gdata)
library(giscoR)
library(wihoja) # wihoja package is not available on CRAN and the repository is private. Please use:
#devtools::install_github("eurostat/wihoja", auth_token= "***REMOVED***")

####SOOURCE THE EXTERNAL FILE CONTAINING FUNCTIONS####

source("hhi_functions.R")

####CONNECT TO DATABASE####

open_oja_db()

####declaring function for calculating Labour market concentration index. Creates subfolder for each country####

lmcirun <- function(x){
  countrycodes <- get("cc",.restatapi_env)$EU27_2020
  countrycode<-countrycodes[x]
  path <- paste0(countrycode, "/")
  dir.create(countrycode)
  resultspath <- paste0(path,"Results/")
  dir.create (resultspath)
  
  
  data_table <- "estat_dsl2531b_oja.ft_document_en_v9"
  
  query <- paste0("SELECT general_id, grab_date, lang, idesco_level_4, esco_level_4, idcity, city, idprovince, province, idregion, region, idcountry, country, idcontract, contract, idsector, sector, sourcecountry, source, site, companyname  ",
                  "FROM ", data_table, " ",
                  "WHERE idcountry = '", countrycode,"' AND idprovince != '' AND idcontract != 'Internship'",
                  ";")
  data <- query_athena(query)
  
  saveRDS(data, file= paste0(path,"OJA",countrycode, ".rds"))
  
  #########################
  
  options(scipen = 999)
  
  dframe <- readRDS(data, file= paste0(path,"OJA",countrycode, ".rds"))
  setDT(dframe)
  rm(data)
  
  # mark deduplication
  #remove observations already marked as duplicate by CEDEFOP
  #"duplicate" observations differ in their content
  #therefore we keep, from each duplicate group, the observation with the lowest number of missing variables
  num_raw_obs <- nrow(dframe)
  dframe$na_count <- rowSums(is.na(dframe))
  
  dframe <- dframe %>% group_by(general_id) %>% arrange(na_count, .by_group = TRUE)
  setDT(dframe)
  
  dframe$dup <- ifelse(duplicated(dframe$general_id), 1, 0)
  
  # convert dates
  
  dframe$grab_date <- as.Date(dframe$grab_date, origin = "1970-01-01")
  
  # add quarter column 
  
  dframe <- dframe %>% mutate(qtr = paste0(year(grab_date), "-", "q", quarter(grab_date)))
  
  #applying empty as na function
  dframe <- dframe %>% mutate_at(c("companyname", "city", "idcity", "province", "idprovince", "region", "idregion", "idcontract", "contract", "idsector", "sector"), empty_as_na)
  
  #write.fst(dframe,paste0(path,"OJA",countrycode, ".fst"), 100)
  #dframe <- read.fst(paste0(path,"OJA",countrycode, ".fst"), as.data.table = TRUE)
  num_duplicates <- as.numeric(sum(dframe$dup == 1))
  dframe <- subset(dframe, dup == 0)
  num_obs_undup <- as.numeric(length(unique(dframe$general_id)))
  
  dframe <- subset(dframe, select =  -c(grab_date))
  
  no_geo <- as.numeric(sum(!startsWith(dframe$idprovince, countrycode)))
  no_contract <- as.numeric(sum(is.na(dframe$contract) | dframe$contract=="Internship"))
  no_isco <- as.numeric(sum(is.na(dframe$idesco_level_4)))
  
  
  
  # write.fst(dframe,paste0(path,"OJA",countrycode, "step1.fst"), 100)
  #dframe <- read.fst(paste0(path,"OJA",countrycode, "step1.fst"), as.data.table = TRUE)
  
  # dframe <- subset(dframe, !is.na(contract))
  # dframe <- subset(dframe, contract!="Internship")

  dframe <- subset(dframe, !is.na(idesco_level_4))
  #num_obs_noisco <- as.numeric(sum(is.na(dframe$idesco_level_4)))
  
  
  dframe <- dframe[startsWith(dframe$idprovince, countrycode), ]
  
  num_obs_after_filters <- nrow(dframe) 
  
  #write.fst(dframe,paste0(path,"ITtest.fst"), 100)
  #dframe <- read.fst(paste0(path,"OJA",countrycode, "step1.fst"), as.data.table = TRUE)
  # clean and order company names for LMC index --------
  
  ordered <- sapply(dframe$companyname, function(x) sep(x))
  dframe$companyname <- ordered
  
  # basic string standardization operations
  dframe$companyname <- str_to_lower(dframe$companyname)
  dframe$companyname <- str_trim(dframe$companyname)
  dframe$companyname <- gsub(" ","_",dframe$companyname)
  
  ####COMPANYNAME CONSOLIDATION#################################################################################################
  #################################################################################################  
  # reading the keywords for data cleaning from imported file
  #countrycode<-"IT"
  if (countrycode=="IT"|countrycode=="RO") {
    clean_names <- read.csv(paste0("companies_to_clean_" , countrycode , ".csv") , sep = ";")
  } else {
    clean_names <- read.csv(paste0("companies_to_clean_" , "EU" , ".csv") , sep = ";")
  }
  
  # run a loop to consolidate company names according to the previous rules and the input keywords found in the csv file
  for(i in 1:dim(clean_names)[1]) {
    #cleaning the company name
    dframe$companyname[str_detect(dframe$companyname, clean_names[i,3]) == TRUE & dframe$companyname!=clean_names[i,5] ] <- clean_names[i,2]
    dframe$companyname[dframe$companyname == clean_names[i,4] ] <- clean_names[i,2]
  }
  
  #####AGENCY FILTER#################################################################################################
  #################################################################################################  
  
  # import list of keywords to be used as filters
  
  staff_agencies <- read.csv("staff_agencies_EU.csv" , sep = ";")
  staff_agencies <- staff_agencies[staff_agencies$Language=="EN"|staff_agencies$Language==countrycode , ]
  
  blacklist <- staff_agencies[staff_agencies$exact != "exact" , 2]
  blacklist_exact <- staff_agencies[staff_agencies$exact == "exact" , 2]
  # filter staffing agencies
  filteredout <- filter(dframe, str_detect(dframe$companyname, paste(blacklist, collapse = '|')) | sub(paste(blacklist_exact, collapse = '|'),"",dframe$companyname) == "" )
  
  obs_agency_table <- as.data.frame(table(filteredout$companyname))
  obs_agency_table <- arrange(obs_agency_table, desc(Freq))
  num_obs_agency_list <- sum(as.numeric(obs_agency_table$Freq))
  num_distinct_agency_list <- nrow(obs_agency_table)
  
  filterlist <- as.character(filteredout$companyname)

  keep <- as.data.frame(clean_names$replace_with)
  colnames(keep) <- "companyname" 

  sumstats_by_company <-gen_sum_stats(idcountry = countrycode, filterlist = filteredout$companyname, keeplist = keep$companyname)
  str(sumstats_by_company)
  
  #generate logs
  sumstats_by_company$ln_esco3 <- log(sumstats_by_company$idesco_level_3)
  sumstats_by_company$ln_undup_n <- log(sumstats_by_company$tot_n - sumstats_by_company$tot_dups)
  sumstats_by_company$sqln_undup_n <- sumstats_by_company$ln_undup_n^2
  sumstats_by_company$culn_undup_n <- sumstats_by_company$ln_undup_n^3
  sumstats_by_company$quln_undup_n <- sumstats_by_company$ln_undup_n^4
  sumstats_by_company$ln_n <- log(sumstats_by_company$tot_n)
  
  testflag1 <- automflag(xvar2="sqln_undup_n", xvar3="culn_undup_n", xvar4="quln_undup_n")
  testflag2 <- automflag(yvar="ln_n", xvar1="ln_undup_n", xvar2="sqln_undup_n", flag_above=FALSE, flag_below=TRUE)
  automflag_output <- automflag_combine(automflag1= testflag1, automflag2= testflag2 )
  
  # automflag_output <- automflag(xvar2="sqln_undup_n", xvar3="culn_undup_n", xvar4="quln_undup_n")
  # comboflag <- as.character(automflag_output[[4]])
  automflag_output[[2]]
  
  #Add other list of companies to be filtered
  filterlist <- c(filterlist,as.character(automflag_output[[5]]))
  
  filterlist_m <- as.data.frame(filterlist)
  filterlist_m$agency <- 1
  colnames(filterlist_m) <- c("companyname","agency")
  filterlist_m <- subset(filterlist_m, !duplicated(filterlist_m$companyname))

  dframe <- merge(dframe, filterlist_m, all.x = TRUE)

  filterlist_model <- as.data.frame(automflag_output[[5]])
  filterlist_model$agency <- 1
  colnames(filterlist_model) <- c("companyname","agency_model")
  filterlist_model <- subset(filterlist_model, !duplicated(filterlist_model$companyname))
  dframe <- merge(dframe, filterlist_model, all.x = TRUE)
  
  filteredout_model <- subset(dframe, dframe$agency_model == 1 )
  obs_agency_model <- as.data.frame(table(filteredout_model$companyname))
  obs_agency_model <- arrange(obs_agency_model, desc(Freq))
  num_obs_agency_model <- sum(as.numeric(obs_agency_model$Freq))
  num_distinct_agency_model <- nrow(obs_agency_model)
 # num_distinct_agency_model <- length(automflag_output[[5]])
  
  dframe <- mutate(dframe, companyname = replace(dframe$companyname, dframe$agency == 1, NA))
  #save step2
  #write.fst(dframe,paste0(path,"OJA",countrycode, "step2.fst"), 100)
  
  
  #dframe <- read_fst((paste0(path,"OJA",countrycode, "step2.fst")), c("general_id", "expire_date", "idsector", "qtr", "idesco_level_4", "idregion", "companyname", "idprovince", "idcity", "site"), as.data.table = TRUE)
  #dframe <- read_fst((paste0(path,"OJA",countrycode, "step2.fst")), as.data.table = TRUE)
  
  #### DOWNLOAD GEO INFO FOR FUAs ======================================
  
  geoinfo <- giscoR::gisco_get_nuts(year = 2016,epsg = 3035, nuts_level = 0, country = countrycode,spatialtype = "RG", resolution = "01")
  sfile <-giscoR::gisco_get_urban_audit(year = 2020,epsg = 3035,country = countrycode, level = "FUA", spatialtype = "RG")
  sfile$geometry <- st_cast(sfile$geometry, "GEOMETRY")
  names(sfile)[names(sfile) == 'FID'] <- 'fua_id'
  sfile <- subset(sfile, select =  -c(URAU_CODE, URAU_CATG, CITY_CPTL, CITY_KERN, FUA_CODE, AREA_SQM))
  sfile$fua_id <- as.character(sfile$fua_id)
  sfilefuanum <- length(unique(sfile$fua_id))
  
  #### MERGE FUA DATA WITH OJA DATA ====================================
  
  #keep only obs with nuts non-missing
  # dframe <- read_fst((paste0(path,"OJA",countrycode, "step3.fst")), as.data.table = TRUE)
  
  #source code for matching LAU codes, NUTS codes and FUAid downloaded from Eurostat website
  fua <- createfua()
  
  fua <- subset(fua, fua$country == countrycode)
 
  totfuanum <- length(unique(fua$fua_id))-1
  
  fua_pop <- aggregate(cbind(population = as.numeric(fua$population), tot_area = round((fua$tot_area)/1000000)), by=list(fua_id=fua$fua_id), FUN=sum )
  
  #Handle country exceptions
  if (countrycode == "HR"){ fua$city <- capitalize(fua$city <- tolower(fua$city)) }
  if (countrycode == "PL"){dframe$fua_id = substr(dframe$fua_id,1,nchar(dframe$fua_id)-1)}
  if (countrycode == "EE"){fua$city <- gsub(pattern = " linn|vald" , replacement = "", fua$city)}
  if (countrycode == "SI"){fua$fua_id <- str_replace(fua$fua_id, "2$", "1")}
  if (countrycode == "LT"){
    dframe <- left_join(dframe,fua, by= "idcity")
    dframe$idprovince <- coalesce(dframe$idprovince.y, dframe$idprovince.x)
    dframe <- select(dframe, -c("country.x", "country.y", "var1", "idprovince.y", "idprovince.x", "city_latin"))
  }
  
  #corrects idcity (LAU code) in input OJA data by looking at cityname (LAU national name)
  if (countrycode %in% c("PT", "SE", "FR", "EL", "IE", "PL", "EE", "HR", "MT", "FI", "SK", "SI", "CY"))
  {
  fuadup <- fua %>% count(idprovince, city)
  fua2 <- merge (fua, fuadup)
  fua2 <- subset (fua2, fua2$n == 1)  
  dframe <- left_join(dframe,fua2, by=c ("city", "idprovince"))
  dframe$idcity <- coalesce(dframe$idcity.y, dframe$idcity.x)
  dframe <- select(dframe, -c("idcity.x", "idcity.y", "fua_id", "country.x", "country.y", "var1", "city_latin"))
  }
  #include quality check?How the matching by city name works.
  

  
  # Left join first by both idprovince and idcity
  dframe <- left_join(dframe,fua,by=c("idprovince","idcity"))
  
  # Left join by idprovince where possible (assign var=1)
  fua2 <- subset(fua, fua$var1 == 1 & !duplicated(fua$idprovince))
  dframe <- left_join(dframe, fua2, by=c("idprovince"))
  
  dframe$fua_id <- coalesce(dframe$fua_id.x, dframe$fua_id.y)
  names(dframe)[names(dframe) == 'idcity.x'] <- 'idcity'
  dframe <- select(dframe, -c("fua_id.y", "fua_id.x", "country.x", "country.y", "var1.x", "var1.y", "idcity.y"))
  
  num_obs_nofua <- as.numeric(sum(is.na(dframe$fua_id)))
  dframe <- dframe[!is.na(dframe$fua_id),]
  
  fuanum <- length(unique(dframe$fua_id))
  
  
  num_obs_final <- as.numeric(length(unique(dframe$general_id)))
  # write.fst(dframe,paste0(path,"OJA",countrycode, "step3.fst"), 100)
  dframe$companyname[dframe$companyname == ""] <- NA
  num_imputed_companynames <- as.numeric(sum(is.na(dframe$companyname)))
  dframeupper <- dframe[!is.na(dframe$companyname) , ]
  
  ####IMPUTATION OF MISSING COMPANYNAMES (i.e. Staffing agencies removed by the filter)####
  #replace all missing company names with unique strings
  no <- seq_len(length(dframe$companyname))
  no <- paste0("missing",no)
  dframe$companyname <- sapply(dframe$companyname, as.character)
  dframe$companyname[is.na(dframe$companyname)] <- " "
  dframe$companyname[dframe$companyname==" "] <- no[dframe$companyname==" "]
  rm(no)
  
  #write.fst(dframe,paste0(path,"OJA",countrycode, "step4fua.fst"), 100)
  #dframe <- read.fst(paste0(path,"OJA",countrycode, "step4fua.fst"), as.data.table = TRUE)
  
  ####CALCULATE THE HERFINDAHL HIRSCHMAN INDEX =============
  hhi <- calculate_hhi(dframe)
  hhiupper <- calculate_hhi(dframe=dframeupper)
  
  ###Quality Indicators
  quality <- as.data.frame(cbind(countrycode, num_raw_obs, num_obs_undup, num_duplicates, no_geo, no_isco, no_contract, num_obs_after_filters, num_obs_nofua, num_obs_final))
  saveRDS(quality, paste0(resultspath,"quality_",countrycode, ".rds"))
  
  companyname_stats <- as.data.frame(cbind(countrycode, num_obs_agency_list, num_obs_agency_model, num_imputed_companynames, num_distinct_agency_list, num_distinct_agency_model, automflag_output[[2]]))
  saveRDS(companyname_stats, paste0(resultspath,"companyname_stats_",countrycode, ".rds"))
  ###MERGE HHI RESULTS WITH GEO DATA (FUAs)============
  
  hhigeo <- create_hhigeo(hhi)
  hhigeoupper <- create_hhigeo(hhi=hhiupper)
  
  hhigeo <- merge(hhigeo, fua_pop)
  
  saveRDS(hhigeo, paste0(resultspath,"hhigeo",countrycode, ".rds"))
  saveRDS(hhigeoupper, paste0(resultspath,"hhigeoupper",countrycode, ".rds"))
  
  if (nrow(hhigeo) > 0){
    
    hhigeo_q3_2018 <- subset(hhigeo, qtr == "2018-q3")
    hhigeo_q3_2018$label <- paste0(hhigeo_q3_2018  $fua_name, "\n ", as.character(hhigeo_q3_2018$wmean))
    
    hhigeo_q4_2018 <- subset(hhigeo, qtr == "2018-q4")
    hhigeo_q4_2018$label <- paste0(hhigeo_q4_2018$fua_name, "\n ", as.character(hhigeo_q4_2018$wmean))
    
    hhigeo_q1_2019 <- subset(hhigeo, qtr == "2019-q1")
    hhigeo_q1_2019$label <- paste0(hhigeo_q1_2019$fua_name, "\n ", as.character(hhigeo_q1_2019$wmean))
    
    hhigeo_q2_2019 <- subset(hhigeo, qtr == "2019-q2")
    hhigeo_q2_2019$label <- paste0(hhigeo_q2_2019$fua_name, "\n ", as.character(hhigeo_q2_2019$wmean))
    
    hhigeo_q3_2019 <- subset(hhigeo, qtr == "2019-q3")
    hhigeo_q3_2019$label <- paste0(hhigeo_q3_2019$fua_name, "\n ", as.character(hhigeo_q3_2019$wmean))
    
    hhigeo_q4_2019 <- subset(hhigeo, qtr == "2019-q4")
    hhigeo_q4_2019$label <- paste0(hhigeo_q4_2019$fua_name, "\n ", as.character(hhigeo_q4_2019$wmean))
    
    
    hhigeo_pop <- subset(hhigeo, wmean > 2500)
    hhigeo_pop <- aggregate(cbind(population = hhigeo_pop$population), by= list(qtr = hhigeo_pop$qtr), FUN = sum)
    hhigeo_wmean <- aggregate(cbind(average_concentration = hhigeo$wmean), by= list(qtr = hhigeo$qtr), FUN = mean, subset = hhigeo$wmean > 2500)
    hhigeo_pop <- merge(hhigeo_pop, hhigeo_wmean)
    hhigeo_pop <- cbind(countrycode, hhigeo_pop, pop_share = hhigeo_pop$population/sum(as.numeric(fua$population)))
    
    # Graphs ===========
    
    ggplot(hhigeo_q3_2018) +
      geom_sf( aes(fill = wmean)) + theme_void() +
      theme(panel.grid.major = element_line(colour = "transparent")) +
      labs(title = "Labour market concentration index Q3-2018\naverage over all occupations") +
      scale_fill_continuous(name = "Labour market concentration index",low="blue", high="orange") +
      geom_sf_text(aes(label = label), size = 2.5, colour = "black")+
      geom_sf(data=geoinfo,alpha = 0)
    
    ggsave(paste0(resultspath,"HHI_q32018_", countrycode, ".png"), width = 15, height = 10, units = "cm")
    
    
    ggplot(hhigeo_q4_2018) +
      geom_sf(aes(fill = wmean)) + theme_void() +
      theme(panel.grid.major = element_line(colour = "transparent")) +
      labs(title = "Labour market concentration index Q4-2018\naverage over all occupations") +
      scale_fill_continuous(name = "Labour market concentration index", low="blue", high="orange") +
      geom_sf_text(aes(label = label), size = 2.5, colour = "black")+
      geom_sf(data=geoinfo,alpha = 0)
    
    ggsave(paste0(resultspath,"HHI_q42018_", countrycode, ".png"), width = 15, height = 10, units = "cm")
    
    
    ggplot(hhigeo_q1_2019) +
      geom_sf(aes(fill = wmean)) + theme_void() +
      theme(panel.grid.major = element_line(colour = "transparent")) +
      labs(title = "Labour market concentration index Q1-2019\naverage over all occupations") +
      scale_fill_continuous(name = "Labour market concentration index", low="blue", high="orange") +
      geom_sf_text(aes(label = label), size = 2.5, colour = "black")+
      geom_sf(data=geoinfo,alpha = 0)
    
    ggsave(paste0(resultspath,"HHI_q12019_", countrycode, ".png"), width = 15, height = 10, units = "cm")
    
    
    
    # HHI tables by region --------------------------
    
    
    table <- data.frame(cbind(hhigeo_q3_2018$fua_id, hhigeo_q3_2018$fua_name, hhigeo_q3_2018$wmean, hhigeo_q4_2018$wmean, hhigeo_q1_2019$wmean))
    
    table <- table[!is.na(table[,2]),]
    
    colnames(table) <- c("FUA", "Name", "Avg. Q3 2018", "Avg. Q4 2018", "Avg. Q1 2019" )
    
    write.xlsx(table,
               file = paste0(resultspath,"HHI_FUA_", countrycode, ".xlsx"), sheetName = "Sheet1",
               col.names = TRUE, append = FALSE
    )
    
    
    
    ############### Average HHI across all quarters =====================
    
    #hhi_tmean <- hhi %>% group_by(fua_id, idesco_level_4) %>% summarise(totalmean = mean(hhi))
    
    
    hhi_tmean <- hhi[, .(idesco_level_4, mshare, ms2, ncount, hhi, tmean = mean(hhi)), by = list(fua_id) ]
    
    hhigeo_tmean <- unique(hhi_tmean[, c("fua_id", "tmean")])
    
    hhigeo_tmean <- data.table(left_join(hhigeo_tmean, sfile, by = "fua_id"))
    names(hhigeo_tmean)[names(hhigeo_tmean) == 'URAU_NAME'] <- 'fua_name'
    
    hhigeo_tmean$fua_name <- as.character(hhigeo_tmean$fua_name)
    
    
    hhigeo_tmean$tmean <- round(hhigeo_tmean$tmean)
    
    st_geometry(hhigeo_tmean) <- hhigeo_tmean$geometry
    
    hhigeo_tmean <- st_zm(hhigeo_tmean, drop = TRUE, what = "ZM")
    
    hhigeo_tmean$label <- paste0(hhigeo_tmean$fua_name, "\n ", as.character(hhigeo_tmean$tmean))
    
    #test <- hhigeo_tmean[is.na(hhigeo_tmean$fuaname),]
    
    ggplot(hhigeo_tmean) +
      geom_sf( aes(fill = tmean)) + theme_void() +
      theme(panel.grid.major = element_line(colour = "transparent")) +
      labs(title = "Labour market concentration index 07.2018 - 03.2019\naverage over occupations and quarters") +
      scale_fill_continuous(name = "Labour market concentration index",low="blue", high="orange") +
      geom_sf_text(aes(label = fua_name), size = 2.5, colour = "black")+
      geom_sf(data=geoinfo,alpha = 0)
    
    ggsave(paste0(resultspath,"HHI_avgfrom_q32018_toq12019_", countrycode, ".png"), width = 20, height = 13.3, units = "cm")
    
  }
}

#run function to all 27MS
lapply(1:27,lmcirun)

#aggregate the results from countries and plot
filenames <- list.files(getwd(), recursive=T, pattern="hhigeo",full.names=T)
hhigeoTOT <- rbindlist(lapply(filenames,FUN= readRDS), fill = T)
geoinfoTOT <- giscoR::gisco_get_nuts(year = 2016,epsg = 3035, nuts_level = 0,spatialtype = "RG", resolution = "01")
hhigeoTOTq32018 <- subset(hhigeoTOT, qtr == "2018-q3")
hhigeoTOTq32018$label <- paste0(hhigeoTOTq32018$fua_name, "\n ", as.character(hhigeoTOTq32018$wmean))
hhigeoTOTq32018 <- st_as_sf(hhigeoTOTq32018)

ggplot(hhigeoTOTq32018) +
  geom_sf( aes(fill = wmean)) + theme_void() +
  theme(panel.grid.major = element_line(colour = "transparent")) +
  labs(title = "Labour market concentration index Q3-2018\naverage over all occupations") +
  scale_fill_continuous(name = "Labour market concentration index",low="blue", high="orange") +
  #geom_sf_text(aes(label = label), size = 2.5, colour = "black")+
  geom_sf(data=geoinfoTOT,alpha = 0)+
  coord_sf(xlim = c(2300000, 7050000),ylim = c(1390000, 5400000)) + theme_bw()

setDT(hhigeoTOT)
hhigeoTOT <- subset(hhigeoTOT, select = -geometry)
write.csv(hhigeoTOT,"hhigeo.csv")


filenamesq <- list.files(getwd(), recursive=T, pattern="quality_",full.names=T)
quality_TOT <- rbindlist(lapply(filenamesq,FUN= readRDS), fill = T)

filenamesc <- list.files(getwd(), recursive=T, pattern="companynames_stats",full.names=T)
companynames_stats_TOT <- rbindlist(lapply(filenamesc,FUN= readRDS), fill = T)
