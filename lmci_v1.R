####REQUIRED LIBRARIES####

require("restatapi")
library(data.table)
library(noctua)
library(giscoR)
library(sf)
library(DBI)
library(ggplot2)
library(tidyverse)
library(openxlsx)
#library(readxl)
#library(Hmisc)
#library(dplyr)
#library(openxlsx)
#library(lubridate)
#library(stringr)
#library(tidyr)
#library(hhi)
#library(stringi)
#library(gdata)


# clear up before start
rm(list=ls())

# set number of cores to be used for parallel processing and timestamp for logging
ts<-format(Sys.time(),"%Y%m%d%H%M%S")
options(mc.cores=2)
hhi_cores<-2
####SOURCE THE EXTERNAL FILE CONTAINING FUNCTIONS####

source("hhi_functions.R")

####CONNECT TO DATABASE####

open_oja_db()

####declaring function for calculating Labour market concentration index. Creates subfolder for each country####
countrycodes <- get("cc",.restatapi_env)$EU27_2020
# countrycode<-countrycodes[1]
# to delete the downloaded files uncomment the code below
# filenames<-unlist(lapply(countrycodes,function(x) {paste0(x, "/","OJA",x, ".rds")})) 
# unlink(filenames)

lmci_load <- function(countrycode){
  stime<- Sys.time()
  path <- paste0(countrycode, "/")
  dir.create(path)
  resultspath <- paste0(path,"Results/")
  dir.create (resultspath)
  
  
  data_table <- "estat_dsl2531b_oja.ft_document_en_v9"
  
  query <- paste0("SELECT general_id, grab_date, lang, idesco_level_4, esco_level_4, idcity, city, idprovince, province, idregion, region, idcountry, country, idcontract, contract, idsector, sector, sourcecountry, source, site, companyname  ",
                  "FROM ", data_table, " ",
                  "WHERE idcountry = '", countrycode,"' AND idprovince != '' AND contract != 'Internship'",
                  ";")
  filename<-paste0(path,"OJA",countrycode, ".rds")
  if(!file.exists(filename)){
    data <- query_athena(query)
    saveRDS(data,filename)
  }
  # nobs<-nrow(readRDS(filename))
  
  key_var = "companyname"
  vars = "grab_date, idesco_level_4, idesco_level_3, idcity, idprovince, idregion, idsector, idcategory_sector, (expire_date-grab_date) AS duration " 
  samplesize = "1000000"
  querytext <- paste0("SELECT " , key_var, ", general_id, " , vars , " FROM ", data_table ," WHERE idcountry='" , countrycode , "' ORDER BY RAND()  LIMIT " , samplesize)
  
  filename<-file.path(countrycode,paste0("gen_sum_stat_",countrycode,".rds"))
  if(!file.exists(filename)){
    data <- query_athena(querytext)
    saveRDS(data,filename)
  }
  message(paste(Sys.time(),"-",countrycode,"-",format(difftime(Sys.time(),stime))))
  # return(data.table(countrycode,nobs))
}  

nobs<-rbindlist(parallel::mclapply(countrycodes,lmci_load,mc.cores=hhi_cores))


#########################
lmci_calc<-function(countrycode,ts=Sys.Date(),hhi_cores){
  # tryCatch({
  system(paste("echo",paste(countrycode,format(Sys.time()),"11-starting calculation",sep="#"),paste0(">> timings",ts,".txt")))
  cat(format(Sys.time()),"-",countrycode,"\n")
  path <- paste0(countrycode, "/")
  resultspath <- paste0(path,"Results/")
  
  options(scipen = 999)
  
  dframe <- readRDS(data, file= paste0(path,"OJA",countrycode, ".rds"))
  setDT(dframe)
  
  # mark deduplication
  #remove observations already marked as duplicate by CEDEFOP
  #"duplicate" observations differ in their content
  #therefore we keep, from each duplicate group, the observation with the lowest number of missing variables
  num_raw_obs <- nrow(dframe)
  # dframe$na_count <- rowSums(is.na(dframe))
  dframe[,na_count:=rowSums(is.na(.SD))]
  
  # dframe <- dframe %>% group_by(general_id) %>% arrange(na_count, .by_group = TRUE)
  # setDT(dframe)
  dframe <- setorder(dframe,by=general_id,na_count)
  
  # dframe$dup <- ifelse(duplicated(dframe$general_id), 1, 0)
  dframe[,dup:=as.numeric(duplicated(general_id))]
  
  # convert dates
  
  # dframe$grab_date <- as.Date(dframe$grab_date, origin = "1970-01-01")
  dframe[,grab_date:=as.Date(grab_date, origin = "1970-01-01")]
  
  # add quarter column 
  
  # dframe <- dframe %>% mutate(qtr = paste0(year(grab_date), "-", "q", quarter(grab_date)))
  dframe[,qtr := paste0(year(grab_date), "-", "q", quarter(grab_date))]
  #applying empty as na function
  # dframe <- dframe %>% mutate_at(c("companyname", "city", "idcity", "province", "idprovince", "region", "idregion", "idcontract", "contract", "idsector", "sector"), empty_as_na)
  cols<-c("city", "idcity", "province", "idprovince", "region", "idregion", "idcontract", "contract", "idsector", "sector")
  dframe[,(cols):=lapply(.SD, empty_as_na2),.SDcols=cols]
  
  #write.fst(dframe,paste0(path,"OJA",countrycode, ".fst"), 100)
  #dframe <- read.fst(paste0(path,"OJA",countrycode, ".fst"), as.data.table = TRUE)
  num_duplicates <- as.numeric(sum(dframe$dup == 1))
  dframe <- subset(dframe, dup == 0)
  num_obs_undup <- as.numeric(length(unique(dframe$general_id)))
  
  dframe <- subset(dframe, select =  -c(grab_date))
  
  no_geo <- as.numeric(sum(!startsWith(dframe$idprovince, countrycode)))
  no_contract <- as.numeric(sum(is.na(dframe$contract)))
  no_isco <- as.numeric(sum(is.na(dframe$idesco_level_4)))
  
  # write.fst(dframe,paste0(path,"OJA",countrycode, "step1.fst"), 100)
  #dframe <- read.fst(paste0(path,"OJA",countrycode, "step1.fst"), as.data.table = TRUE)
  
  # dframe <- subset(dframe, !is.na(contract))
  # dframe <- subset(dframe, contract!="Internship")
  
  dframe <- subset(dframe, !is.na(idesco_level_4))
  #num_obs_noisco <- as.numeric(sum(is.na(dframe$idesco_level_4)))
  
  dframe <- dframe[startsWith(dframe$idprovince, countrycode), ]
  # dframe <- dframe[grepl(paste0("^",countrycode),idprovince), ]
  num_obs_after_filters <- nrow(dframe) 
  
  #write.fst(dframe,paste0(path,"ITtest.fst"), 100)
  #dframe <- read.fst(paste0(path,"OJA",countrycode, "step1.fst"), as.data.table = TRUE)
  # clean and order company names for LMC index --------
  
  ####COMPANYNAME CONSOLIDATION#################################################################################################
  #################################################################################################  
  # reading the keywords for data cleaning from imported file
  system(paste("echo",paste(countrycode,format(Sys.time()),"12-starting clean companynames",sep="#"),paste0(">> timings",ts,".txt")))
  
  # ordered <- sapply(dframe$companyname, function(x) sep(x))
  # dframe$companyname <- ordered
  # 
  # # basic string standardization operations
  # dframe$companyname <- str_to_lower(dframe$companyname)
  # dframe$companyname <- str_trim(dframe$companyname)
  # dframe$companyname <- gsub(" ","_",dframe$companyname)
  
  companynames_sep<-unlist(parallel::mclapply(tolower(dframe$companyname),sep2,mc.cores=hhi_cores))
  dframe[,companyname:=trimws(gsub(" ","_",ascii(companynames_sep)))]
  
  clean_names <- read.csv("companies_to_clean_EU.csv" , sep = ",")
  clean_names <- clean_names[clean_names$country=="EU"|clean_names$country==countrycode , ]
  
  # run a loop to consolidate company names according to the previous rules and the input keywords found in the csv file
  # for(i in 1:dim(clean_names)[1]) {
  #   #cleaning the company name
  #   dframe$companyname[str_detect(dframe$companyname, clean_names[i,3]) == TRUE & dframe$companyname!=clean_names[i,5] ] <- clean_names[i,2]
  #   dframe$companyname[dframe$companyname == clean_names[i,4] ] <- clean_names[i,2]
  # }
  dframe_names<-data.table(rn=dframe[companyname!="",which=T],dframe[companyname!="",c("companyname")])
  f_clean_names<-function(cl,dframe){
    dframe[(grepl(cl[[1]][3],companyname) & companyname!=cl[[1]][5]) |companyname==cl[[1]][4] ,companyname:=cl[[1]][2]][]
  }
  all<-rbindlist(unique(lapply(as.list(as.data.frame(t(clean_names))),f_clean_names,dframe=dframe_names)))
  dframe[all$rn,companyname:=all$companyname]
  table_all_names <- data.table(countrycode,table(all$companyname))
  saveRDS(table_all_names,paste0(resultspath, "table_all_names_", countrycode, ".rds"))

  # 
  #####AGENCY FILTER#################################################################################################
  #################################################################################################  
  system(paste("echo",paste(countrycode,format(Sys.time()),"13-starting agency filter",sep="#"),paste0(">> timings",ts,".txt")))
  
  # import list of keywords to be used as filters
  
  staff_agencies <- read.csv("staff_agencies_EU.csv" , sep = ",")
  staff_agencies <- staff_agencies[staff_agencies$country=="EU"|staff_agencies$country==countrycode , ]
  
  blacklist <- staff_agencies[staff_agencies$exact != "exact" , 2]
  blacklist_exact <- staff_agencies[staff_agencies$exact == "exact" , 2]
  # filter staffing agencies
  filteredout <- filter(dframe, str_detect(dframe$companyname, paste(blacklist, collapse = '|')) | sub(paste(blacklist_exact, collapse = '|'),"",dframe$companyname) == "" )
  
  obs_agency_table <- data.frame(countrycode,table(filteredout$companyname))
  saveRDS(obs_agency_table,paste0(resultspath, "obs_agency_table_", countrycode, ".rds"))
  obs_agency_table <- arrange(obs_agency_table, desc(Freq))
  num_obs_agency_list <- sum(as.numeric(obs_agency_table$Freq))
  num_distinct_agency_list <- nrow(obs_agency_table)
  
  filterlist <- as.character(filteredout$companyname)
  
  keep <- as.data.frame(clean_names$replace_with)
  colnames(keep) <- "companyname" 
  
  sumstats_by_company <-gen_sum_stats(idcountry = countrycode, filterlist = filteredout$companyname, keeplist = keep$companyname,consolidate = clean_names)
  # str(sumstats_by_company)
  
  #generate logs
  sumstats_by_company$ln_esco3 <- log(sumstats_by_company$idesco_level_3)
  sumstats_by_company$ln_undup_n <- log(sumstats_by_company$tot_n - sumstats_by_company$tot_dups)
  sumstats_by_company$sqln_undup_n <- sumstats_by_company$ln_undup_n^2
  sumstats_by_company$culn_undup_n <- sumstats_by_company$ln_undup_n^3
  sumstats_by_company$quln_undup_n <- sumstats_by_company$ln_undup_n^4
  sumstats_by_company$ln_n <- log(sumstats_by_company$tot_n)
  sumstats_by_company$ln_province <- log(sumstats_by_company$idprovince)
  sumstats_by_company$ln_sector <- log(sumstats_by_company$idsector)
  sumstats_by_company$ln_undup_prov <- sumstats_by_company$ln_province * sumstats_by_company$ln_undup_n
  
  testflag1 <- automflag(mydata=sumstats_by_company[sumstats_by_company$ln_undup_n>3,],xvar2="sqln_undup_n", xvar3="culn_undup_n", xvar4="quln_undup_n")
  testflag2 <- automflag(mydata=sumstats_by_company[sumstats_by_company$ln_undup_n>3,],yvar="ln_n", xvar1="ln_undup_n", xvar2="sqln_undup_n", flag_above=FALSE, flag_below=TRUE)
  testflag3 <- automflag(mydata=sumstats_by_company[sumstats_by_company$ln_undup_n>3,],yvar="ln_sector", xvar1="ln_prov", xvar2="ln_undup_n", xvar3="ln_undup_prov", flag_above=TRUE, flag_below=FALSE)
  automflag_output <- automflag_combine(mydata=sumstats_by_company[sumstats_by_company$ln_undup_n>3,],automflag1= testflag1, automflag2= testflag2 )
  automflag_output <- automflag_combine(mydata=sumstats_by_company[sumstats_by_company$ln_undup_n>3,],automflag1= automflag_output, automflag2= testflag3 )
  
  
  # automflag_output <- automflag(xvar2="sqln_undup_n", xvar3="culn_undup_n", xvar4="quln_undup_n")
  # comboflag <- as.character(automflag_output[[4]])
  automflag_output[[2]]
  
  #Add other list of companies to be filtered
  filterlist <- c(filterlist,as.character(automflag_output[[5]]))
  
  staff_agencies_from_model <- data.table(countrycode, automflag_output[[5]])
  saveRDS(staff_agencies_from_model, file = paste0(resultspath,"staff_agencies_from_model_", countrycode, ".rds"))
  
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
  system(paste("echo",paste(countrycode,format(Sys.time()),"14-starting geo download",sep="#"),paste0(">> timings",ts,".txt")))
  
  geoinfo <- giscoR::gisco_get_nuts(year = 2016,epsg = 3035, nuts_level = 0, country = countrycode,spatialtype = "RG", resolution = "01")
  sfile <-giscoR::gisco_get_urban_audit(year = 2020,epsg = 3035,country = countrycode, level = "FUA", spatialtype = "RG")
  sfile$geometry <- st_cast(sfile$geometry, "GEOMETRY")
  names(sfile)[names(sfile) == 'FID'] <- 'fua_id'
  sfile <- subset(sfile, select =  -c(URAU_CODE, URAU_CATG, CITY_CPTL, CITY_KERN, FUA_CODE, AREA_SQM))
  sfile$fua_id <- as.character(sfile$fua_id)
  sfilefuanum <- length(unique(sfile$fua_id))
  
  if (countrycode %in% c("IE","CY")){sfile$fua_id = substr(sfile$fua_id,1,nchar(sfile$fua_id)-2)}
  #if (countrycode == "CY"){sfile$fua_id[sfile$fua_id == "CY501"] <- "CY003"} old line in case NUTS LAU correspondence table 2019-2016 is used instead of 2018-2016. See createfua() function for details.
  
  #### MERGE FUA DATA WITH OJA DATA ====================================
  system(paste("echo",paste(countrycode,format(Sys.time()),"15-starting merge fua and oja",sep="#"),paste0(">> timings",ts,".txt")))
  
  #keep only obs with nuts non-missing
  # dframe <- read_fst((paste0(path,"OJA",countrycode, "step3.fst")), as.data.table = TRUE)
  
  #source code for matching LAU codes, NUTS codes and FUAid downloaded from Eurostat website
  fua <- createfua(countrycode)
  num_laus_infuas <- nrow(fua)
  
  totfuanum <- length(unique(fua$fua_id))
  
  #Handle country exceptions
  
  if (countrycode %in% c("IE", "HR", "PT")){ fua$city <- capitalize(fua$city <- tolower(fua$city)) }
  if (countrycode  %in% c("IE","CY")){fua$fua_id = substr(fua$fua_id,1,nchar(fua$fua_id)-2)}
  #if (countrycode == "PL"){fua$fua_id = substr(fua$fua_id,1,nchar(fua$fua_id)-1)} 2018-2016 used in the createfua() function.
  if (countrycode == "EE"){fua$city <- gsub(pattern = " linn|vald" , replacement = "", fua$city)}
  if (countrycode == "EE"){fua$city <- gsub(pattern = "Narva" , replacement = "Narva-Jõesuu", fua$city)}
  if (countrycode == "SI"){fua$fua_id <- str_replace(fua$fua_id, "2$", "1")}
  if (countrycode == "LT"){
    dframe <- left_join(dframe,fua, by= "idcity")
    dframe$idprovince <- coalesce(dframe$idprovince.y, dframe$idprovince.x)
    dframe <- select(dframe, -c("country.x", "country.y", "var1", "idprovince.y", "idprovince.x", "city_latin"))
  }
  
  fuadup <- fua %>% count(idprovince, city) # identify duplicates same city name and idprovice (maybe different idcity)
  fua2 <- merge (fua, fuadup)
  fua2 <- subset (fua2, fua2$n == 1)  
  num_undup_laus_infuas <- nrow(fua2)
  
  #corrects idcity (LAU code) in input OJA data by looking at cityname (LAU national name)
  if (countrycode %in% c("PT", "SE", "FR", "EL", "IE", "PL", "EE", "HR", "MT", "FI", "SK", "SI", "CY", "CZ", "HU"))
  {
    dframe <- left_join(dframe,fua2, by=c ("city", "idprovince"))
    dframe$idcity <- coalesce(dframe$idcity.y, dframe$idcity.x)
    dframe <- select(dframe, -c("idcity.x", "idcity.y", "fua_id", "country.x", "country.y", "var1", "city_latin"))
  }
  #include quality check?How the matching by city name works.
  
  fua_pop <- unique(fua[,c("fua_id","population","econ_active_pop")])[,.(population=sum(population),econ_active_pop=sum(econ_active_pop)),by=fua_id]
  fua_pop$share_active_pop <- fua_pop$econ_active_pop/fua_pop$population
  
  # Left join first by both idprovince and idcity
  dframe <- left_join(dframe,fua,by=c("idprovince","idcity"))
  
  # Left join by idprovince where possible (assign var=1)
  fua3 <- subset(fua, fua$var1 == 1 & !duplicated(fua$idprovince))
  dframe <- left_join(dframe, fua3, by=c("idprovince"))
  
  dframe$fua_id <- coalesce(dframe$fua_id.x, dframe$fua_id.y)
  names(dframe)[names(dframe) == 'idcity.x'] <- 'idcity'
  dframe <- select(dframe, -c("fua_id.y", "fua_id.x", "country.x", "country.y", "var1.x", "var1.y", "idcity.y"))
  
  num_obs_nofua <- as.numeric(sum(is.na(dframe$fua_id)))
  dframe <- dframe[!is.na(dframe$fua_id),]
  
  fuanum <- length(unique(dframe$fua_id))
  
  
  num_obs_final <- as.numeric(length(unique(dframe$general_id)))
  # write.fst(dframe,paste0(path,"OJA",countrycode, "step3.fst"), 100)
  dframe$companyname[dframe$companyname == ""] <- NA
  #num_imputed_companynames <- as.numeric(sum(is.na(dframe$companyname)))
  dframeupper <- dframe[!is.na(dframe$companyname) , ]
  
  ####IMPUTATION OF MISSING COMPANYNAMES (i.e. Staffing agencies removed by the filter)####
  #replace all missing company names with unique strings
  system(paste("echo",paste(countrycode,format(Sys.time()),"16-starting imputation of missing company names",sep="#"),paste0(">> timings",ts,".txt")))
  
  no <- seq_len(length(dframe$companyname))
  no <- paste0("missing",no)
  dframe$companyname <- sapply(dframe$companyname, as.character)
  dframe$companyname[is.na(dframe$companyname)] <- " "
  dframe$companyname[dframe$companyname==" "] <- no[dframe$companyname==" "]
  rm(no)
  
  
  #write.fst(dframe,paste0(path,"OJA",countrycode, "step4fua.fst"), 100)
  #dframe <- read.fst(paste0(path,"OJA",countrycode, "step4fua.fst"), as.data.table = TRUE)
  
  ####CALCULATE THE HERFINDAHL HIRSCHMAN INDEX =============
  system(paste("echo",paste(countrycode,format(Sys.time()),"17-starting hhi calculation",sep="#"),paste0(">> timings",ts,".txt")))
  cols<-c("idesco_level_4","fua_id","qtr","companyname")
  hhi_data<-dframe[,..cols]
  hhi <- calculate_hhi(hhi_data,hhi_cores)
  saveRDS(hhi, file = paste0(resultspath,"hhi_data_", countrycode, ".rds"))
  
  hhi_data<-dframeupper[,..cols]
  hhiupper <- calculate_hhi(hhi_data,hhi_cores)
  rm(hhi_data)
  gc()
  
  ####Quality Indicators
  quality <- as.data.frame(cbind(countrycode, num_raw_obs, num_obs_undup, num_duplicates, no_geo, no_isco, no_contract, num_obs_after_filters, num_obs_nofua, num_obs_final))
  saveRDS(quality, paste0(resultspath,"quality_",countrycode, ".rds"))
  
  companyname_stats <- as.data.frame(cbind(countrycode, num_obs_agency_list, num_obs_agency_model, num_distinct_agency_list, num_distinct_agency_model, automflag_output[[2]]))
  saveRDS(companyname_stats, paste0(resultspath,"companyname_stats_",countrycode, ".rds"))
  
  fua_stats <- as.data.frame(cbind(totfuanum, sfilefuanum, fuanum, num_laus_infuas, num_undup_laus_infuas))
  saveRDS(fua_stats, paste0(resultspath,"fua_stats_",countrycode, ".rds"))
  
  ###MERGE HHI RESULTS WITH GEO DATA (FUAs)============
  system(paste("echo",paste(countrycode,format(Sys.time()),"18-starting merge hhi with geo",sep="#"),paste0(">> timings",ts,".txt")))
  
  hhigeo <- create_hhigeo(hhi,sfile)
  hhigeoupper <- create_hhigeo(hhi=hhiupper,sfile)
  
  hhigeo <- merge(hhigeo, fua_pop)
  class(hhigeo$geometry)<-c("sfc_GEOMETRY","sfc")

  # hhigeo<-st_as_sf(hhigeo)
  saveRDS(hhigeo, paste0(resultspath,"hhigeo",countrycode, ".rds"))
  saveRDS(hhigeoupper, paste0(resultspath,"hhigeoupper",countrycode, ".rds"))
  system(paste("echo",paste(countrycode,format(Sys.time()),"19-starting plotting hhigeo",sep="#"),paste0(">> timings",ts,".txt")))
  
  # table(hhigeo$qtr)
  quarters<-unique(hhigeo$qtr) #c("2018-q3","2018-q4","2019-q1","2019-q2","2019-q3","2019-q4")
  hhigeo_q<-lapply(quarters,hhigeo_subset,data=hhigeo)
  names(hhigeo_q)<-quarters

  #merge with population data
  hhigeo_pop <- subset(hhigeo, wmean > 2500)
  hhigeo_pop <- aggregate(cbind(urbpopulation = hhigeo_pop$population), by= list(qtr = hhigeo_pop$qtr), FUN = sum)
  hhigeo_tot <- aggregate(cbind(toturbpopulation = hhigeo$population), by= list(qtr = hhigeo$qtr), FUN = sum)
  hhigeo_merged <- merge(hhigeo_pop, hhigeo_tot, all.x = TRUE)
  hhigeo_merged$share <- hhigeo_merged$urbpopulation/hhigeo_merged$toturbpopulation

  hhigeo_wmean <- aggregate(cbind(average_concentration = hhigeo$wmean), by= list(qtr = hhigeo$qtr), FUN = mean, subset = hhigeo$wmean > 2500)
  hhigeo_pop <- merge(hhigeo_merged, hhigeo_wmean)
  hhigeo_pop <- cbind(countrycode, hhigeo_pop)
  saveRDS(hhigeo_pop, paste0(resultspath,"hhigeo_pop",countrycode, ".rds"))

  
  #### Create Maps for each quarter ===========
  
  lapply(quarters, hhigeo_plot,hhigeo_q=hhigeo_q,geoinfo=geoinfo,resultspath=resultspath,countrycode=countrycode)

  #### Average HHI tables by FUAs and Quarter --------------------------
  
  # table <- data.frame(cbind(hhigeo_q3_2018$fua_id, hhigeo_q3_2018$fua_name, hhigeo_q3_2018$wmean, hhigeo_q4_2018$wmean, hhigeo_q1_2019$wmean))
  table<-dcast(st_set_geometry(hhigeo[,c("fua_id","fua_name","wmean","qtr")],NULL),fua_id+fua_name~qtr,value.var = "wmean")
  # table <- data.frame(cbind(hhigeo_q[[i]]$fua_id, hhigeo_q[[i]]$fua_name,rbindlist(sapply(quarters,function(x){eval(parse(text=paste0("hhigeo_q$`",x,"`$wmean")))}))
  
  table<-na.omit(table,cols="fua_name")
  # table <- table[!is.na(table[,2]),]
  
  colnames(table) <- c("FUA", "Name", paste("Avg. ",quarters))
  write.xlsx(table, file = paste0(resultspath,"HHI_fua_qtr", countrycode, ".xlsx"), sheetName = "Sheet1", col.names = TRUE, append = FALSE)
  saveRDS(table, paste0(resultspath,"HHI_fua_qtr",countrycode, ".rds"))
  
  
  ####Average HHI across all quarters (hhigeo_tmean) =====================
  
  #hhi_tmean <- hhi %>% group_by(fua_id, idesco_level_4) %>% summarise(totalmean = mean(hhi))
  system(paste("echo",paste(countrycode,format(Sys.time()),"20-starting avg hhi calculation",sep="#"),paste0(">> timings",ts,".txt")))
  
  
  hhi_tmean <- hhi[, .(idesco_level_4, ncount, hhi, tmean = mean(hhi)), by = list(fua_id) ]
  
  hhigeo_tmean <- unique(hhi_tmean[, c("fua_id", "tmean")])
  
  hhigeo_tmean <- data.table(left_join(hhigeo_tmean, sfile, by = "fua_id"))
  names(hhigeo_tmean)[names(hhigeo_tmean) == 'URAU_NAME'] <- 'fua_name'
  
  hhigeo_tmean$fua_name <- as.character(hhigeo_tmean$fua_name)
  
  
  hhigeo_tmean$tmean <- round(hhigeo_tmean$tmean)
  
  st_geometry(hhigeo_tmean) <- hhigeo_tmean$geometry
  #hhigeo_tmean <- st_zm(hhigeo_tmean, drop = TRUE, what = "ZM")
  class(hhigeo_tmean$geometry)<-c("sfc_GEOMETRY","sfc")
  hhigeo_tmean$label <- paste0(hhigeo_tmean$fua_name, "\n ", as.character(hhigeo_tmean$tmean))
  
  #test <- hhigeo_tmean[is.na(hhigeo_tmean$fuaname),]
  
  #save hhigeo_tmean
  saveRDS(hhigeo_tmean, paste0(resultspath,"hhigeo_tmean",countrycode, ".rds"))
  
  #plot and save graph
  ggplot(hhigeo_tmean) +
    geom_sf( aes(fill = tmean),lwd=0) + theme_void() +
    theme(panel.grid.major = element_line(colour = "transparent")) +
    labs(title = paste("Labour market concentration index",min(quarters),"-",max(quarters),"\naverage over occupations and quarters")) +
    scale_fill_continuous(name = "Labour market concentration index",low="blue", high="orange") +
    geom_sf_text(aes(label = fua_name), size = 2.5, colour = "black")+
    geom_sf(data=geoinfo,alpha = 0)
  
  ggsave(paste0(resultspath,"HHI_avgfrom_",min(quarters),"_",max(quarters),"_",countrycode, ".png"), width = 20, height = 13.3, units = "cm")
  
  system(paste("echo",paste(countrycode,format(Sys.time()),"21-finishing calculation",sep="#"),paste0(">> timings",ts,".txt")))
  
  # }, error=function(e){message(e)})
  
}

#single country run

# lmci_calc("MT",ts,hhi_cores)
#run function to all 27MS in parallel
parallel::mclapply(countrycodes,lmci_calc,ts=ts,hhi_cores)


########################################### RESULTS AGGREGATION AND VISUALIZATION #######################

#aggregate the results from countries
EU_resultspath <- "EU_results/"
dir.create(EU_resultspath)

#aggregate hhi
filenames1 <- list.files(getwd(), recursive=T, pattern="hhi_data_[A-Z][A-Z]",full.names=T)
hhiTOT <- rbindlist(lapply(filenames1,readRDS), fill = T)
saveRDS(hhiTOT, paste0(EU_resultspath,"hhiTOT.rds"))
write.xlsx(hhiTOT, paste0(EU_resultspath, "hhiTOT.xlsx"))

#aggregate hhigeo
filenames2 <- list.files(getwd(), recursive=T, pattern="hhigeo[A-Z][A-Z]",full.names=T)
hhigeoTOT <- rbindlist(lapply(filenames2,readRDS), fill = T)
saveRDS(hhigeoTOT, paste0(EU_resultspath,"hhigeo_TOT.rds"))

#aggregate hhigeoupper
filenames20 <- list.files(getwd(), recursive=T, pattern="hhigeoupper",full.names=T)
hhigeoup_TOT <- rbindlist(lapply(filenames20,readRDS), fill = T)
saveRDS(hhigeoup_TOT, paste0(EU_resultspath,"hhigeoup_TOT.rds"))

#aggregate hhigeo_pop
filenames3 <- list.files(getwd(), recursive=T, pattern="hhigeo_pop[A-Z][A-Z]",full.names=T)
tothhigeo_pop <- rbindlist(lapply(filenames3,readRDS), fill = T)
saveRDS(tothhigeo_pop, paste0(EU_resultspath,"tothhigeo_pop.rds"))

#aggregate hhi_fua_qtr
filenames4 <- list.files(getwd(), recursive=T, pattern="HHI_fua_qtr[A-Z][A-Z].*rds",full.names=T)
tot_hhi_fua_qtr <- rbindlist(lapply(filenames4,readRDS), fill = T)
saveRDS(tot_hhi_fua_qtr, paste0(EU_resultspath,"tot_hhi_fua_qtr.rds"))

#aggregate and plotting hhigeo_tmean
filenames40 <- list.files(getwd(), recursive=T, pattern="hhigeo_tmean",full.names=T)
tmean_hhigeo_tot <- rbindlist(lapply(filenames40,readRDS), fill = T)
saveRDS(tmean_hhigeo_tot, paste0(EU_resultspath,"tmean_hhigeo_tot.rds"))

ggplot(tmean_hhigeo_tot) +
  geom_sf( aes(geometry=geometry,fill = tmean),lwd=0) + theme_void() +
  theme(panel.grid.major = element_line(colour = "transparent")) +
  labs(title = paste("Labour market concentration index",min(quarters),"-",max(quarters),"\naverage over occupations and quarters")) +
  scale_fill_continuous(name = "Labour market concentration index",low="blue", high="orange") +
  geom_sf(data=geoinfoTOT,alpha = 0)+
  coord_sf(xlim = c(2300000, 7050000),ylim = c(1390000, 5400000))

ggsave(paste0(EU_resultspath,"HHI_avgfrom_",min(quarters),"_",max(quarters),"_.png"), width = 20, height = 13.3, units = "cm")

###prepare plotting hhigeoTOT
quarters<-unique(hhigeoTOT$qtr) #c("2018-q3","2018-q4","2019-q1","2019-q2","2019-q3","2019-q4")
hhigeoTOT <- st_as_sf(hhigeoTOT)
hhigeoTOT_q<-lapply(quarters,hhigeo_subset,data=hhigeoTOT)
names(hhigeoTOT_q)<-quarters
geoinfoTOT <- giscoR::gisco_get_nuts(year = 2016,epsg = 3035, nuts_level = 0,spatialtype = "RG", resolution = "01")#Create subsets for each quarter
hhigeo_qTOT<-lapply(quarters,hhigeo_subset,data=hhigeoTOT)
names(hhigeo_qTOT)<-quarters

#Creates EU map for each quarter using hhigeo_plot_tot function declared in the script hhi_functions.R
lapply(quarters, hhigeo_plot_tot,hhigeo_q=hhigeoTOT_q,geoinfo=geoinfoTOT,resultspath=getwd())

#Save the final hhigeo Data table (removing geometry column to allow .csv export)
setDT(hhigeoTOT)
hhigeoTOT <- subset(hhigeoTOT, select = -geometry)
write.csv(hhigeoTOT,paste0(EU_resultspath,"hhigeo.csv"))
saveRDS(hhigeoTOT, paste0(EU_resultspath,"hhigeo.rds"))

#plotting hhi values and share of economically active population
jpeg(paste0(EU_resultspath,"hhi_activepop_plot.png"))
plot(hhigeoTOT$wmean, hhigeoTOT$share_active_pop)
dev.off()

############## QUALITY INDICATORS #################

#aggregate quality indicators from all countries and save results

#quality_tot: indicator that tracks the number of job ads analysed through the various steps of the process
filenames5 <- list.files(getwd(), recursive=T, pattern="quality_",full.names=T)
tot_quality_stats <- rbindlist(lapply(filenames5,FUN= readRDS), fill = T)
saveRDS(tot_quality_stats, paste0(EU_resultspath,"tot_quality.rds"))

#companynames_stats_tot: indicator that tracks the company names identified as staff agencies using both keywords list and classification model
filenames6 <- list.files(getwd(), recursive=T, pattern="companyname_stats",full.names=T)
company_stats_tot <- rbindlist(lapply(filenames6,FUN= readRDS), fill = T)
saveRDS(company_stats_tot, paste0(EU_resultspath,"company_stats_tot.rds"))

#fua_stats_tot: indicator that tracks the number LAUs for each countries part of a FUA and the number of FUAs that have job positions from the ads database.
filenames7 <- list.files(getwd(), recursive=T, pattern="fua_stats",full.names=T)
fuas_stats_tot <- rbindlist(lapply(filenames7,FUN= readRDS), fill = T)
saveRDS(fuas_stats_tot, paste0(EU_resultspath,"fuas_stats_tot.rds"))

#staff_agencies_from_model: indicator that collects all the names of the companies flagged as staffing agency by the classification model. A random sample is extracted from this list and checked manually.
filenames8 <- list.files(getwd(), recursive=T, pattern="staff_agencies_from_model",full.names=T)
staff_agencies_model_tot <- rbindlist(lapply(filenames8,FUN= readRDS), fill = T)
saveRDS(staff_agencies_model_tot, paste0(EU_resultspath,"staff_agencies_model_tot.rds"))
# staff_agencies_sample <- sample_n(staff_agencies_model_tot, 50)
# write.csv(staff_agencies_sample,paste0(EU_resultspath, "staff_agencies_sample.csv"))

#stores the names of all the companies flagged as staffing agencies using the keywords list
filenames9 <- list.files(getwd(), recursive=T, pattern="obs_agency_table",full.names=T)
table_names_list <- rbindlist(lapply(filenames9,FUN= readRDS), fill = T)
saveRDS(table_names_list, paste0(EU_resultspath, "table_names_list.rds"))

#stores all names of companies
filenames10 <- list.files(getwd(), recursive=T, pattern="table_all_names_",full.names=T)
table_names <- rbindlist(lapply(filenames10,FUN= readRDS), fill = T)
saveRDS(table_names, paste0(EU_resultspath, "table_names.rds"))
write.csv(table_names, paste0(EU_resultspath, "table_names.csv"))
