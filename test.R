

####Connection to database and download####

#include also OLD connection details

reticulate::py_config()
# library(RJDBC)

# -- RJDBC library
# if you don't hvae rJava installed, uncomment the following line and run it
#install.packages("rJava")

# list.of.packages <- c('ggplot2','RJDBC','reshape','dplyr','ggplot2') 
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)


# library(reticulate)
# library(DBI)
library(RAthena)
library(wihoja)
reticulate::use_condaenv("RAthena")

# -- RJDBC library
# if you don't hvae rJava installed, uncomment the following line and run it
#install.packages("rJava")

# list.of.packages <- c('ggplot2','RJDBC','reshape','dplyr','ggplot2') 
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

open_oja_db()

get_data <- function(query){
  my_data <- dbGetQuery(con, query)
  return(my_data)
}

library(tidyverse)
require("restatapi")
library(Hmisc)
library(dplyr)
library(ggplot2)
library(xlsx)
library(readxl)
library(gmodels)
library(data.table)
library(lubridate)
library(stringr)
library(fst)
library(tidyr)
library(openxlsx)
library(hhi)
library(sf)
library(stringi)

source("hhi_functions.R")

empty_as_na <- function(y){
  
  y[!str_detect(y, "")] <- NA
  
  return(y)
}


sep <- function(linha) {
  resp <- strsplit(linha," |/|-")
  resp <- unlist(resp)
  resp <- gsub(",|;|\\.","",resp)
  resp <- sort(resp[which(nchar(resp) > 2)])
  resp <- paste0(resp,collapse=" ")
  resp <- tolower(resp)
}

#declaring function for calculating Labour market concentration index. Creates subfolder for each country

lmcirun <- function(x){
  countrycodes <- get("cc",.restatapi_env)$EU27_2020
  countrycode<-countrycodes[x]
  path <- paste0(countrycode, "/")
  dir.create(countrycode)
  resultspath <- paste0(path,"Results/")
  dir.create (resultspath)
  
  
  data_table <- "estat_dsl2531b_oja.ft_document_en_v8"
  
  query <- paste0("SELECT general_id, grab_date, lang, idesco_level_4, esco_level_4, idcity, city, idprovince, province, idregion, region, idcountry, country, idcontract, contract, idsector, sector, sourcecountry, source, site, companyname  ",
                  "FROM ", data_table, " ",
                  "WHERE idcountry = '", countrycode,"' AND idprovince != '' AND idcontract != 'Internship'",
                  ";")
  data <- query_athena(query)
  uniqueads <- length(unique(data$general_id))
  
  saveRDS(data, file= paste0(path,"OJA",countrycode, ".rds"))
  
  #########################
  
  options(scipen = 999)
  
  dframe <- readRDS(data, file= paste0(path,"OJA",countrycode, ".rds"))
  setDT(dframe)
  
  #rm(data)
  
  
  # mark deduplication
  #remove observations already marked as duplicate by CEDEFOP
  #"duplicate" observations differ in their content
  #therefore we keep, from each duplicate group, the observation with the lowest number of missing variables
  
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
  
  write.fst(dframe,paste0(path,"OJA",countrycode, ".fst"), 100)
  #dframe <- read.fst(paste0(path,"OJA",countrycode, ".fst"), as.data.table = TRUE)
  
  dframe <- subset(dframe, dup == 0)
  
  dframe <- subset(dframe, select =  -c(grab_date))
  
  write.fst(dframe,paste0(path,"OJA",countrycode, "step1.fst"), 100)
  #dframe <- read.fst(paste0(path,"OJA",countrycode, "step1.fst"), as.data.table = TRUE)
  
  dframe <- subset(dframe, !is.na(contract))
  dframe <- subset(dframe, contract!="Internship")
  
  dframe <- subset(dframe, !is.na(idesco_level_4))
  
  
  # clean and order company names for LMC index --------
  
  ordered <- sapply(dframe$companyname, function(x) sep(x))
  dframe$companyname <- ordered
  
  
  #################################################################################################
  ######################insert here agency filter
  # replacing the following heading in the original code:
  # filter staffing agencies ---------
  #################################################################################################  
  # basic string standardization operations
  dframe$companyname <- str_to_lower(dframe$companyname)
  dframe$companyname <- str_trim(dframe$companyname)
  dframe$companyname <- gsub(" ","_",dframe$companyname)
  # import list of keywords to be used as filters
  staff_agencies_csv <- paste0("staff_agencies_" , countrycode , ".csv")
  staff_agencies <- read.csv(staff_agencies_csv , sep = ";")
  blacklist <- staff_agencies[staff_agencies$exact != "exact" , 2]
  blacklist_exact <- staff_agencies[staff_agencies$exact == "exact" , 2]
  # filter staffing agencies
  filteredout <- filter(dframe, str_detect(dframe$companyname, paste(blacklist, collapse = '|')) | sub(paste(blacklist_exact, collapse = '|'),"",dframe$companyname) == "" )
  dframe <- mutate(dframe, companyname = replace(companyname, str_detect(dframe$companyname, paste(blacklist, collapse = '|')) | sub(paste(blacklist_exact, collapse = '|'),"",dframe$companyname) == "", NA))
  dframe <- dframe[!is.na(dframe$companyname) , ]
  
  #################################################################################################
  ######################insert here company name consolidation
  # immediately after the following heading in the original code:
  # filter staffing agencies ---------
  #################################################################################################  
  # reading the keywords for data cleaning from imported file
  #countrycode<-"IT"
  companies_to_clean_csv <- paste0("companies_to_clean_" , countrycode , ".csv")
  clean_names <- read.csv(companies_to_clean_csv , sep = ";")
  # generate a function to record all the company names that are replaced, so it is possible to check for wrong rules
  names_replaced <- function(i) {
    temp <- dframe$companyname[str_detect(dframe$companyname, clean_names[i,3]) ==TRUE | dframe$companyname==clean_names[i,4] ]
    temp <- temp[temp!=clean_names[i,5]]
    temp <- paste(temp, collapse=" ; ")
    return(temp)
  }
  # run this check-up function for all consolidated companynames and export the results
  names_replaced_list_csv <- apply(as.matrix(1:dim(clean_names)[1]),1,names_replaced)
  names_replaced_list_csv <- paste0("names_replaced_list_",countrycode,".csv")
  write.csv2(names_replaced_list , names_replaced_list_csv)
  
  # run a loop to consolidate company names according to the previous rules and the input keywords found in the csv file
  for(i in 1:dim(clean_names)[1]) {
    #cleaning the company name
    dframe$companyname[str_detect(dframe$companyname, clean_names[i,3]) == TRUE & dframe$companyname!=clean_names[i,5] ] <- clean_names[i,2]
    dframe$companyname[dframe$companyname == clean_names[i,4] ] <- clean_names[i,2]
  }
  
  
  
  #save step2
  write.fst(dframe,paste0(path,"OJA",countrycode, "step2.fst"), 100)
  
  
  #dframe <- read_fst((paste0(path,"OJA",countrycode, "step2.fst")), c("general_id", "expire_date", "idsector", "qtr", "idesco_level_4", "idregion", "companyname", "idprovince", "idcity", "site"), as.data.table = TRUE)
  #dframe <- read_fst((paste0(path,"OJA",countrycode, "step2.fst")), as.data.table = TRUE)
  
  dframe <- dframe[startsWith(dframe$idregion, countrycode), ]
  
  # recode missings to NA
  dframe$companyname[dframe$companyname == ""] <- NA
  
  # median imputation
  # count unique obs per occupation, region, quarter, company (preliminary, only for imputation)
  dframe[, ccount := .N, by = list(idesco_level_4, idregion, qtr, companyname)]
  
  #adding a variable called ccount, counting all the records with the same isco level, region, quarter and company name
  dframe[is.na(dframe$companyname),]$ccount <- NA
  
  #calculates the median. 
  med <- median(dframe$ccount, na.rm = TRUE)
  
  #ordering the dataframe
  dframe <- dframe[order(dframe$idregion, dframe$qtr, dframe$idesco_level_4, dframe$companyname), ]
  
  #creating a new variable called "imp" imputing a dummy company name. Using the median not to influence the index
  dframe[, imp := as.integer(rep((1:.N), each = med, length.out = .N)), by = list(idesco_level_4, idregion, qtr)]
  
  #adding the word "missing" to the variable imp
  dframe$imp <- paste0("missing", dframe$imp)
  
  #when company name is NA takes the value of the variable imp
  dframe$companyname[is.na(dframe$companyname)] <- dframe$imp[is.na(dframe$companyname)]
  
  write.fst(dframe,paste0(path,"OJA",countrycode, "step3.fst"), 100)
  
  
  # download geo information  ======================================
  
  geoinfo <- giscoR::gisco_get_nuts(year = 2016,epsg = 3035, nuts_level = 0, country = countrycode,spatialtype = "RG", resolution = "01")
  
  sfile <-giscoR::gisco_get_urban_audit(year = 2020,epsg = 3035,country = countrycode, level = "FUA", spatialtype = "RG")
  
  sfile$geometry <- st_cast(sfile$geometry, "GEOMETRY")
  
  names(sfile)[names(sfile) == 'FID'] <- 'fua_id'
  
  sfile <- subset(sfile, select =  -c(URAU_CODE, URAU_CATG, CITY_CPTL, CITY_KERN, FUA_CODE, AREA_SQM))
  
  sfile$fua_id <- as.character(sfile$fua_id)
  
  sfilefuanum <- length(unique(sfile$fua_id))
  
  # merge FUA data ====================================
  
  #keep only obs with nuts non-missing
  # dframe <- read_fst((paste0(path,"OJA",countrycode, "step3.fst")), as.data.table = TRUE)
  dframe <- subset(dframe, !is.na(idprovince))
  
  fua <- read_excel("Geodata/CorrespondenceTable.xlsx")
  fua <- subset(fua, fua$COUNTRY == countrycode)
  colnames(fua) <- c("country", "idprovince","idcity", "fua_id", "var1", "city", "city_latin")
  totfuanum <- length(unique(fua$fua_id))-1
  
  #Handle country exceptions
  if (countrycode == "HR"){  fua$city <- capitalize(fua$city <- tolower(fua$city)) }
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
  {dframe <- left_join(dframe,fua, by= "city")
  dframe$idcity <- coalesce(dframe$idcity.y, dframe$idcity.x)
  dframe$idprovince <- coalesce(dframe$idprovince.y, dframe$idprovince.x)
  dframe$fua_id <- dframe$fua_id.y
  dframe <- select(dframe, -c("idcity.x", "idcity.y", "fua_id.x", "fua_id.x", "country.x", "country.y", "var1.x", "idprovince.y", "idprovince.x", "city_latin.y"))
  }
  
  
  # Left join first by both idprovince and idcity
  dframe <- left_join(dframe,fua,by=c("idprovince","idcity"))
  
  # Left join by idprovince where possible (assign var=1)
  fua2 <- subset(fua, fua$var1 == 1 & !duplicated(fua$idprovince))
  dframe <- left_join(dframe, fua2, by=c("idprovince"))
  
  dframe$fua_id <- coalesce(dframe$fua_id.x, dframe$fua_id.y)
  names(dframe)[names(dframe) == 'idcity.x'] <- 'idcity'
  dframe <- select(dframe, -c("fua_id.y", "fua_id.x", "country.x", "country.y", "var1.x", "var1.y", "idcity.y"))
  
  dframe <- dframe[!is.na(dframe$fua_id),]
  
  fuanum <- length(unique(dframe$fua_id))
  
  write.fst(dframe,paste0(path,"OJA",countrycode, "step4fua.fst"), 100)
  
  #dframe <- read.fst(paste0(path,"OJA",countrycode, "step4fua.fst"), as.data.table = TRUE)
  
  
  # Calculate the Herfindahl Hirschman Index =============
  
  # compute market shares by quarter, FUA and esco level 4 occupation
  
  # create grid of occupation, geo unit and quarter
  
  grid <- expand.grid(esco = unique(dframe$idesco_level_4), geo = unique(dframe$fua_id), qtr = unique(dframe$qtr), stringsAsFactors = FALSE)
  
  # count obs per occupation, region, quarter
  setDT(dframe)
  dframe <- select(dframe, -c("ccount", "imp"))
  
  dframe[, ncount := .N, by = list(idesco_level_4, fua_id, qtr)]
  
  # count obs per occupation, region, quarter, company
  dframe[, ccount := .N, by = list(idesco_level_4, fua_id, qtr, companyname)]
  
  #market shares
  dframe$mshare <- ((dframe$ccount) / (dframe$ncount)) * 100
  
  dframe$ms2 <- (dframe$mshare)^2
  
  hhi <- data.frame()
  
  for (i in 1:dim(grid)[1]) {
    # count obs per cell and company
    subset <- unique(dframe[idesco_level_4 == grid[i, 1] & fua_id == grid[i, 2] & qtr == grid[i, 3], c("idesco_level_4", "fua_id", "qtr", "mshare", "ms2", "companyname", "ncount"), with = FALSE])
    
    subset$hhi <- sum(subset$ms2)
    
    subset <- subset[1, !c("companyname") ]
    
    hhi <- rbind(hhi, subset)
  }
  
  save(hhi, file = paste0(resultspath,"HHI_data_FUA_", countrycode, ".rdata"))
  
  # load(file = paste0(resultspath,"HHI_data_FUA_", countrycode, ".rdata"))
  
  hhi <- na.omit(hhi)
  
  totalmean <- mean(hhi$hhi)
  totalmean
  
  totalmedian <- median(hhi$hhi)
  totalmedian  
  
  #describe(hhi$hhi)
  
  #empirical cumulative distribution function for value 2500
  #ecdf(hhi$hhi)(2500) 
  
  hhi <- hhi[, .(idesco_level_4, mshare, ms2, ncount, hhi, wmean = mean(hhi)), by = list(fua_id, qtr) ]
  
  hhigeo <- unique(hhi[, c("fua_id", "qtr", "wmean")])
  
  hhigeo <- data.table(left_join(hhigeo, sfile, by = "fua_id"))
  
  names(hhigeo)[names(hhigeo) == 'URAU_NAME'] <- 'fua_name'
  
  hhigeo$fua_name <- as.character(hhigeo$fua_name)
  
  hhigeo$wmean <- round(hhigeo$wmean)
  
  hhigeo <- st_as_sf(hhigeo)
  hhigeo$geometry <- st_cast(hhigeo$geometry, "GEOMETRY")
  
  # st_geometry(hhigeo) <- hhigeo$geometry
  # 
  # hhigeo <- st_zm(hhigeo, drop = TRUE, what = "ZM")
  
  saveRDS(hhigeo, paste0(resultspath,"hhigeo",countrycode, ".rds"))
  
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

