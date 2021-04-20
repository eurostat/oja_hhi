#This file contains the functions used by the main code to calculate the Labour Market Concentration Index.
# List of functions:
# 1. ascii, sep, sep2
# 2. empty_as_na, empty_as_na2
# 3. createfua
#  
# 5. calculate_hhi
# 6. create_hhigeo
# 7. gen_sum_stats
# 8. automflag
# 9. automflag_combine
# 10. hhigeo_subset
# 11. hhigeo_plot

##Function for cleaning the 'companyname' column

#1. sep

ascii<-function(x){
 x<-gsub("é|ę","e",x)
 x<-gsub('á|ą',"a",x)
 return(x)
}

sep <- function(linha) {
  resp <- strsplit(linha," |/|-")
  resp <- unlist(resp)
  resp <- gsub(",|;|\\.|\u00A0","",resp)
  resp <- sort(resp[which(nchar(resp) > 2)])
  resp <- paste0(resp,collapse=" ")
  resp <- tolower(resp)
}

sep2 <- function(linha) {
  resp <- gsub(",|;|\\.|\u00A0","",unlist(strsplit(linha," |/|-")))
  paste0(sort(resp[which(nchar(resp) > 2)]),collapse=" ")
}


# "^[0-9]", "[0-9]$" write line of code to filter out the companynames composed by numbers (e.g. telephone numbers)

## Function for setting empty values to NA

#2. empty_as_na
empty_as_na <- function(y){
  
  y[!str_detect(y, "")] <- NA
  
  return(y)
}
empty_as_na2 <- function(y){
  y[y==""]<-NA 
  return(y)
}



## Function for creating the correspondence table between LAU, NUTS and FUA

#3. createfua
createfua <- function(countrycode){
  ### download file with eurostat classification if is not downloaded yet
  
  filename <- "EU-28-LAU-2019-NUTS-2016.xlsx"
  if (!file.exists(filename)) {download.file("https://ec.europa.eu/eurostat/documents/345175/501971/EU-28-LAU-2019-NUTS-2016.xlsx ", destfile=filename)}
  # options (timeout = 100)
  
  
  ###selecting countries
  # countrylist <- getSheetNames(filename)[4:31]
  #alternatively: countrylist <- c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE")
  
  
  #3b. assignFUA
  #create a function generating the "assign" variable
  
  # assignFUA <- function(countrycode) {
    #importing excel sheet and eliminating spaces from variable names
    DT <- setDT(read_excel(filename , sheet = countrycode))
    # colnames(DF) <- substr(str_replace_all(colnames(DF) , " " , "_") , 1 , 11)
    setnames(DT,gsub("\\s","_",colnames(DT)))
    setnames(DT,gsub("\\(.*\\)","",colnames(DT)))
    
    # DT<-DT[,which(unlist(lapply(DT, function(x)!all(is.na(x))))),with=F]
    
    #finding which LAUs belong to the same NUTS and FUAs
    DT[,NUTSFUA:= paste0(NUTS_3_CODE, FUA_ID)]
    DT[,dup:= as.numeric(!duplicated(NUTSFUA , fromLast=FALSE))]
    
    
    #Adding count of observations and of duplicates by NUTS
    ntable <- DT[,.(count=.N),by=NUTS_3_CODE]
    duptable <- DT[dup==1,.(dup_count=.N),by=NUTS_3_CODE]
    idtable <- DT[!is.na(FUA_ID),.(FUAID_count=.N),by=NUTS_3_CODE]  
    DT<-ntable[DT,on="NUTS_3_CODE"]
    DT<-duptable[DT,on="NUTS_3_CODE"]
    DT<-idtable[DT,on="NUTS_3_CODE"]
    DT[is.na(FUAID_count),FUAID_count:=-999] 
    
    
    #generate an assign variable equal to 1 if all LAUs in a given NUTS can be automatically assigned to the same FUA
    DT[,assign:= 0]
    DT[dup_count == 1,assign:=1] 
    DT[FUAID_count < count, assign:=0]
    # ?????? why DF$assign[DF$FUAID_count < DF$count] <- 0

    cols<-c("NUTS_3_CODE" , "LAU_CODE" , "FUA_ID", "LAU_NAME_NATIONAL" , "LAU_NAME_LATIN" , "POPULATION", "TOTAL_AREA_", "assign")
    DT<-DT[,..cols]
    
    # replace n.a. with NA in the columns
    DT<-DT[,lapply(.SD,function(x)gsub("^n.a.$", NA,x))]
    
    #defining the output of the function
    # return(DF)
  # }
  
  ###applying the previous function to the countries selected at the beginning of this code
  # DFlist <- lapply(countrylist,assignFUA)
  
  # x <- cbind.data.frame(0,0,0,0,0,0,0,0)[-1,]
  # colnames(x) <- c("NUTS_3_CODE" , "LAU_CODE" , "FUA_ID" , "LAU_NAME_NA" , "LAU_NAME_LA" , "POPULATION", "TOTAL_AREA_", "assign")
  # for(i in 1:length(countrylist)) {
  #   x <- rbind(x,DFlist[[i]])
  # }
  # 
  
  
  ### download the correspondence between NUTS2013 and NUTS2016. the OJA data uses NUTS2013, but the correspondence with FUA is available only for NUTS2016 and the associated LAU units. so it necessary to generate the "assign" variable with NUTS2016, and then change it to NUTS2013
  
  filename2 <- "NUTS2013-NUTS2016.xlsx"
  if (!file.exists(filename2)) {download.file("https://ec.europa.eu/eurostat/documents/345175/629341/NUTS2013-NUTS2016.xlsx", destfile=filename2)}
  
  
  ### open the table showing the NUTS units for which a change occurred between the 2013 and 2016 classification; generate a new "recoded" variable with the following values:
  # 2 for NUTS2016 units that have no direct (1:1) correspondence with any NUTS2013 units
  # 1 for NUTS2016 units which, compared to the NUTS2013 classification, remained identical but changed name 
  # 0 for NUTS2016 units for which there has been no change, compared to NUTS2013 (the value 0 is assigned later in the code, after the merge with the main table)
  DT2 <- setDT(read_excel(filename2 , sheet = "Correspondence NUTS-3"))
  setnames(DT2, gsub("\\s","_",colnames(DT2)))
  DT2[,recoded:=2]
  DT2[Change=="recoded",recoded:=1]
  DT2 <- DT2[,c("Code_2013", "Code_2016" , "recoded"),with=F]
  setnames(DT2, c("NUTS_3_2013" , "NUTS_3_CODE" , "recoded"))
  
  # input manually the NUTS2013-NUTS2016 correspondence for 3 NUTS2016 regions
  DT2[NUTS_3_2013=="DE915"|NUTS_3_2013=="DE919",NUTS_3_CODE:="DE91C"]  
  DT2[NUTS_3_2013=="DE915"|NUTS_3_2013=="DE919",recoded:=1]
  DT2[NUTS_3_2013=="NL322",recoded:=1] 
  DT2[NUTS_3_2013=="NL326",recoded:=1] 
  
  
  
  ### merge the NUTS2016-NUTS2013 correspondence table with the main NUTS-LAU-FUA areas and assign country variable
  DT<-DT2[DT,on="NUTS_3_CODE"]
  DT[is.na(recoded),recoded:=0] 
  DT[,country:=countrycode]
  
  ### change the value of NUTS_3_CODE to make it compatible with the 2013 classification. in short, when recoded=1, then the NUTS2013 code is used instead of the NUTS2016 code
  
  DT[recoded==1,NUTS_3_CODE:=NUTS_3_2013] 
  DT[recoded==2,NUTS_3_CODE:=0] 
  DT[recoded==2,assign:=0] 
  DT[,LAU_CODE:=as.character(LAU_CODE)]
  
  # final vector to be used for the calculation of the LMCI
  
  fua <- DT[!is.na(FUA_ID), c("country" , "NUTS_3_CODE" , "LAU_CODE" , "FUA_ID", "LAU_NAME_NATIONAL" , "LAU_NAME_LATIN" , "recoded" , "assign", "POPULATION", "TOTAL_AREA_"),with=F]
  setnames(fua,c("country" , "idprovince" , "idcity" , "fua_id" , "city" , "city_latin" , "recoded", "var1", "population", "tot_area"))
  if (all(is.na(fua$population))){
    imp_pop<-get_eurostat_data("urb_lpop1",filters=c("DE1001V",paste0("^",countrycode,"\\d.*")),perl=T,stringsAsFactors = F)
    # get the last year for a given fua code 
    dates<-imp_pop[,.(myear=max(time)),by=cities]
    # keep only the last year value
    imp_pop<-imp_pop[dates,on=.(time=myear,cities=cities)][,c("cities","values")]
    if(countrycode=="CY") {
      imp_pop<-unique(imp_pop[,.(joinid=substr(cities,1,5),population=values)])
      fua<-fua[,`:=`(population=NULL,joinid=substr(fua_id,1,5))]
    } else {
      imp_pop<-unique(imp_pop[,.(joinid=cities,population=values)])
      fua<-fua[,`:=`(population=NULL,joinid=fua_id)]
    }
    fua<-imp_pop[fua,on=.(joinid=joinid)][,joinid:=NULL]
  }
  fua[,tot_area:=as.numeric(tot_area)]
  return(fua)
}


#5. calculate_hhi

calculate_hhi <- function (dframe,cores=2) {
  
  # compute market shares by quarter, FUA and esco level 4 occupation
  # create grids of occupation, geo unit and quarter
  grid <- expand.grid(esco = unique(dframe$idesco_level_4), geo = unique(dframe$fua_id), qtr = unique(dframe$qtr), stringsAsFactors = FALSE)
  
  # count obs per occupation, region, quarter
  setDT(dframe)
  dframe[, ncount := .N, by = list(idesco_level_4, fua_id, qtr)]
  
  # count obs per occupation, region, quarter, company
  dframe[, ccount := .N, by = list(idesco_level_4, fua_id, qtr, companyname)]
  
  #market shares
  # dframe$mshare <- ((dframe$ccount) / (dframe$ncount)) * 100
  # dframe$ms2 <- (dframe$mshare)^2
  dframe[,mshare:=ccount/ncount*100][,ms2:=mshare^2]
  
  # Sys.time()
  # hhi <- data.frame()

  # for (i in 1:dim(grid)[1]) {
  #   # count obs per cell and company
  #   subset <- unique(dframe[idesco_level_4 == grid[i, 1] & fua_id == grid[i, 2] & qtr == grid[i, 3], c("idesco_level_4", "fua_id", "qtr", "mshare", "ms2", "companyname", "ncount"), with = FALSE])
  #   subset$hhi <- sum(subset$ms2)
  #   subset <- subset[1, !c("companyname") ]
  #   hhi <- rbind(hhi, subset)
  # }
  # Sys.time()
  
  f_calc_hhi<-function(gr,subset){
    subset <- unique(dframe[idesco_level_4 == gr[1] & fua_id == gr[2] & qtr == gr[3], c("idesco_level_4", "fua_id", "qtr", "mshare", "ms2", "companyname", "ncount"), with = FALSE])
    subset$hhi <- sum(subset$ms2)
    subset[1, !c("companyname") ]
   }
  hhi<-rbindlist(parallel::mclapply(as.list(as.data.frame(t(grid))),f_calc_hhi,subset=dframe,mc.cores=cores))
  
  # Sys.time()
  # load(file = paste0(resultspath,"HHI_data_FUA_", countrycode, ".rdata"))
  
  hhi <- na.omit(hhi)
  
  # totalmean <- mean(hhi$hhi)
  # totalmean
  # totalmedian <- median(hhi$hhi)
  # totalmedian  
  # 
  #describe(hhi$hhi)
  
  #empirical cumulative distribution function for value 2500
  #ecdf(hhi$hhi)(2500) 
  return (hhi)
}

#6. create_hhigeo
create_hhigeo <- function(hhi = hhi,sfile){
  hhi <- hhi[, .(idesco_level_4, mshare, ms2, ncount, hhi, wmean = mean(hhi)), by = list(fua_id, qtr) ]
  
  hhigeo <- unique(hhi[, c("fua_id", "qtr", "wmean")])
  
  hhigeo <- data.table(left_join(hhigeo, sfile, by = "fua_id"))
  
  names(hhigeo)[names(hhigeo) == 'URAU_NAME'] <- 'fua_name'
  
  hhigeo$fua_name <- as.character(hhigeo$fua_name)
  
  hhigeo$wmean <- round(hhigeo$wmean)
  
  hhigeo <- st_as_sf(hhigeo)
  hhigeo$geometry <- st_cast(hhigeo$geometry, "GEOMETRY")
  return (hhigeo)
}


#7. gen_sum_stats

  gen_sum_stats <- function(idcountry = countrycode, filterlist = filteredout$companyname, keeplist = keep$companyname, key_var = "companyname",sumstats = "n_distinct", standardise = TRUE, consolidate=clean_names, otherstats = c("avg_duration = mean(duration)" , "avg_grab = mean(grab_date)") ) {


  ### this function creates a list of summary statistics (sum stats) by key_var (in the default, by companyname) and merge them with some word lists that can be used as filter or categorise observations (in the default, with some lists called filteredout and keep). In addition, it creates a variable in the output dataset that combines the two lists (this variable is called filteredout).
  
  ## this function requires the packages "tidyverse" and "wihoja"; in addition, if the option standardise=TRUE is chosen, it requires calling the function sep() included in the set of functions developed for this project.
  ## the output of the function is a dataset with one observation per level of key_var, including summary statistics. In addition, the dataset contains identification variables for those levels of keyvar that are included in the lists of keywords.
  ## this is the list of default arguments given to the function:
  #vars <- "grab_date, idesco_level_4, idesco_level_3, idcity, idprovince, idregion, idsector, idcategory_sector "
  ## the variables in the OJA dataset for which sum stats are created
  #idcountry <- "RO"
  ## the country in the OJA dataset for which sum stats are computed
  #samplesize <- "1000000"
  ## sum stats are calculated for a sample of observations. this argument (a number expressed as text) determines the sample size
  #filterlist <- filteredout$companyname
  ## this argument (a "chr" object) provides a list of words/codes which are matched to the key_var argument, flagging observations accordingly. for example, this argument can indicate which observations we want to filter out for some subsequent analysis. an empty string ("") as an argument means that no list is provided.
  #keeplist <- clean_names$replace_with
  ## this argument (a "chr" object) provides a list of words/codes which are matched to the key_var argument, flagging observations accordingly. for example, this argument can indicate which observations we want to keep for some subsequent analysis.  an empty string ("") as an argument means that no list is provided.
  #key_var <- "companyname"
  ## this argument (the name of a variable in the OJA dataset) provides the key variable by which sum stats are computing. using the default, the function will calculate summary statistics by companyname
  #sumstats <- "n_distinct"
  ## this argument, expressed as one string, identifies the type of functions to be applied to the OJA data and so the type of summary statistics. the default counts the number of distinct values for each variable in vars
  #standardise <- TRUE
  ## this argument, when set (as in the default) as "TRUE", standardises the text of the levels of key_var
  #consolidate <- clean_names
  ## this argument takes the name of a table with keywords and, when different from "" or FALSE, uses this tabe to operate a consolidation of the entries in the observation identifier in the way described in the code consolidate_company_names.R within this project
  
  
  ### compile and run the query in the OJA dataset.
  
  general_query <- readRDS(paste0(idcountry,"/gen_sum_stat_",idcountry,".rds"))
  # dim(general_query)
  
  
  ### arrange the new dataframe as needed
  
  # rename the variable chosen as key_var, and leave the other names unchanged
  colnames(general_query) <- c("keyvar",colnames(general_query)[2:length(colnames(general_query))])
  
  #generate a "duplicate" variable indicating if the observation in the OJA dataset is a duplicate
  general_query$dup <- ifelse(duplicated(general_query$general_id), 1, 0)
  general_query$keyvar <- ascii(str_to_lower(general_query$keyvar))
  
  #standardize companyname
  if (standardise==TRUE) {
    ordered <- sapply(general_query$keyvar, function(x) sep(x))
    general_query$keyvar <- ordered
    general_query$keyvar <- str_trim(general_query$keyvar)
    general_query$keyvar <- gsub(" ","_",general_query$keyvar)
    # general_query$keyvar <- gsub("é","e",general_query$keyvar)    
  }

  # eliminate empty cells in keyvar
  general_query$notgood <- ifelse(general_query$keyvar=="",1,0)
  general_query <- general_query[general_query$notgood != 1 , ]
  
  #consolidate companyname  ????????????? not repitition of clean names
  if (any(consolidate!="" & consolidate!=FALSE)) {
    # run a loop to consolidate company names according to the previous rules and the input keywords found in the csv file
    for(i in 1:dim(consolidate)[1]) {
    general_query$keyvar[str_detect(general_query$keyvar, consolidate[i,3]) == TRUE & general_query$keyvar!=consolidate[i,5] ] <- consolidate[i,2]
    general_query$keyvar[general_query$keyvar == consolidate[i,4] ] <- consolidate[i,2]
    }
  }
  # f_clean_names<-function(cl,dframe){
  #   dframe[grepl(cl[[1]][3],companyname) & companyname!=cl[[1]][5],.(companyname:=cl[[1]][2])]
  #   dframe[companyname==cl[[1]][4],.(companyname:cl[[1]][2])]
  #   return(NULL)
  # }
  # if (consolidate!="" & consolidate!=FALSE) {
  #   fout<-lapply(as.list(as.data.frame(t(consolidate))),f_clean_names,dframe=general_query)
  # }
  
  
  ### generating summary statistics at the keyvar level. these summary statistics are the main output of the function
  
  # base set of stats
  DF <- general_query
  DF <- group_by(DF , keyvar)
  sumstats_by_company_1 <- summarise(DF , tot_n=n(), tot_dups=sum(dup))
  sumstats_by_company_2 <- summarise_all(DF, sumstats)
  sumstats_by_company <- merge(sumstats_by_company_1 , sumstats_by_company_2, all.x=TRUE)
  
  
  ### merge the newly extracted dataset with the lists provided as input (filterlist and keeplist) and code a variable (filteredout) that combines the two
  
  # preparing filterlist for merge
  filteredout_m <- as.data.frame(table(filterlist))
  filteredout_m$Freq <- 1
  colnames(filteredout_m) <- c("keyvar", "filteredout")
  
  # preparing keeplist for merge
  keep_m <- as.data.frame(table(keeplist))
  keep_m$Freq <- 1
  colnames(keep_m) <- c("keyvar", "keep")  
  
  # merge new dataset with "keep" and "filter" variables
  sumstats_by_company <- merge(sumstats_by_company, filteredout_m, all.x=TRUE)
  sumstats_by_company <- merge(sumstats_by_company, keep_m, all.x=TRUE)
  dim(sumstats_by_company)
  
  # coding a new variable (filteredout) combining the keeplist and the filterlist as follows:
  # 1 <- included in filterlist (in our first application of this function, this means flagged as agency)
  # 0 <- included in keeplist (in our first application of this function, this means identified as company)
  # -1 <- not included in either of the two lists
  sumstats_by_company$filteredout[sumstats_by_company$keep==1] <- 0
  sumstats_by_company$filteredout[is.na(sumstats_by_company$filteredout)==TRUE] <- -1
  
  
  ### prepare function output
  
  # rename keyvar back to its original name (the name of other variables have not been changed)
  colnames(sumstats_by_company) <- c(key_var,colnames(sumstats_by_company)[2:length(colnames(sumstats_by_company))])
  
  #define output
  return(sumstats_by_company)
  
}

#8. automflag
automflag <- function(mydata=sumstats_by_company[sumstats_by_company$ln_undup_n>3,] , flag="filteredout" , names="companyname" , yvar="ln_esco3", xvar1="ln_undup_n", xvar2="", xvar3="", xvar4="", percentile=50, flag_threshold=1.96, flag_above=TRUE, flag_below=FALSE, method="fit", error_pctile=90) {
  
  
  ### this function flags observations in a dataset, using an empirical rule based on a regression. the regression is run on a list of observations which have already been flagged by the user. this function identifies (and flags) other observations that are close to the estimated regression curve, and that had not been previously coded or flagged by the user
  
  ## this function does not require any particular package to be installed; it can be run with base R.
  ## this function has multiple outputs:
  # 1. a 2-column dataset where the observation name/code is in the first column and a binary (0/1) flag derived from the specified empirical rule in the second column. the flag essentially says if the observed value for the observation lies far away from a curve estimated from a subset of the observations. if so, the flag is set =1 and it means that the observation likely belongs to that group. 
  # 2. a matrix to evaluate the flag. this matrix is a confusion matrix counting true positives, false positives, etc.
  # 3. a table with the regression's coefficients
  # 4. a list of the names/codes of the observations that are flagged
  # 5. a list of the names/codes of the observations that are flagged and are not included in the list of flagged observations given as input to train the model
  ## this is the list of default argument of the function:  
  #mydata <- sumstats_by_company
  ## this is a dataset containing the variables needed to estimate the model. it must contain all the following variables as explained below: names, yvar, xvars, flag
  #flag <- "filteredout"
  ## this is a crucial input to the function. flag is a string identifying the name of the variable that, within mydata, identifies observations belonging to different groups. Observations can be flagged as agencies (value = 1) or identified as actual employers (0). Ideally, flag is a binary (0/1) variable, but the function will work as long as 1 is included in the values of flag. The regression model will be estimated only for observations for which flag==1.
  #names <- "companyname"
  ## this is a variable containing the observation identifier (name/code/id)
  #yvar <- "ln_esco3"
  ## this is the dependent variable in the model. this argument is required.
  #xvar1 <- "ln_undup_n"
  ## this is an indipendent variable in the model. this argument is required.
  #xvar2 <- ""
  ## this is an indipendent variable in the model. this argument is not required, and is empty in the default.
  #xvar3 <- ""
  ## this is an indipendent variable in the model. this argument is not required, and is empty in the default.
  #xvar4 <- ""
  ## this is an indipendent variable in the model. this argument is not required, and is empty in the default.
  #percentile <- 50
  ## the regression model is estimated twice, the first including all observations for which flag==1, the second removing observations lying far away from the curve (outliers). The observations removed from the second estimate are those with the largest error terms derived from the first estimate. The argument "percentile" identifies the percentage of observations to be removed before running the second estimate. 
  #method <- "fit"
  ## after calculating the second and final regression model, observations with yvar substantially different from the value predicted by the model are not flagged (they are assigned a value of 0 in the new variable "autom_flag"). In contrast, observations with relatively small deviations from the estimated curve are flagged (they are assigned 1 in the new variable "autom_flag"). What is "substantially different" is determined through a set of arguments. if method is set as "fit", as in the default, the model residual for an observation (i.e. the difference between the actual value and the fitted value) is compared to the standard error of the fitted value to see if the actual value lies within the confidence interval of the estimate. If method=="error", then the observations which are not flagged are those with the largest model residuals. 
  #flag_threshold<-1.96
  ## if method=="fit", then this argument determines how wide to make the confidence interval. The default value of 1.96 means that observations for which the actual value of yvar is outside the range {fitted value +/- 1.96 * standard error of fitted value} are not flagged
  #error_pctile <- 90
  ## if method=="error", then this argument determines what percentage of observation are not flagged based on the fact that they have the largest error. In the default, with error_pctile=90, the 10% of observations with the largest error is flagged
  #flag_above <- TRUE
  ## this argument, when TRUE, means that only observations with negative residuals may not be flagged. In other words, observations are flagged in they are close to the curve or above the curve.
  #flag_below <- FALSE
  ## this argument, when TRUE, means that only observations with positive residuals may not be flagged. In other words, observations are flagged in they are close to the curve or above the curve.
  
  
  ### create a vector of one (for model constant), filter the data so that only observations with flag==1 are used for the regression model, and create a Y and X matrices. a back-up of the unfiltered data (myfulldata) is also created. a number of "if" conditions is used because the user can specify different numbers of xvars

  # create vector of 1
  mydata$one <- 1
  
  # data backup
  myfulldata <- mydata
  
  # using the function string arguments to define the relevant variables
  fl <- eval(parse(text=paste("mydata$", flag, sep = "")))
  mydata <- mydata[fl==1,]  
  y <- eval(parse(text=paste("mydata$", yvar, sep = "")))
  x1 <- eval(parse(text=paste("mydata$", xvar1, sep = "")))
  if (xvar2!="") {
    x2 <- eval(parse(text=paste("mydata$", xvar2, sep = "")))
  }
  if (xvar3!="") {
    x3 <- eval(parse(text=paste("mydata$", xvar3, sep = "")))
  }  
  if (xvar4!="") {
    x4 <- eval(parse(text=paste("mydata$", xvar4, sep = "")))
  } 
  
  # create matrices Y and X
  Y <- as.matrix(y)
  if (xvar2=="") {
    X <- as.matrix(cbind(mydata$one , x1))
  } else if (xvar3=="") {
    X <- as.matrix(cbind(mydata$one , x1 , x2))
  } else if (xvar4=="") {
    X <- as.matrix(cbind(mydata$one , x1 , x2, x3))
  } else if (xvar4!="") {
    X <- as.matrix(cbind(mydata$one , x1 , x2 , x3, x4))
  }
  
  
  ### estimate the regression model
  
  # calculate regression coefficients and squared residuals for the first model (with all observations for which flag==1)
  b <- solve(t(X)%*%X)%*%t(X)%*%Y
  sq_e <- (Y - (X%*%b))^2
  
  # re-calculate regression coefficients and squared residuals for well-fitting data points
  X2 <- as.matrix(X[sq_e < quantile(sq_e, probs=percentile/100) , ])
  Y2 <- as.matrix(Y[sq_e < quantile(sq_e, probs=percentile/100)])
  b2 <- solve(t(X2)%*%X2)%*%t(X2)%*%Y2
  sq_e2 <- (Y2 - (X2%*%b2))^2
  
  # calculate coefficients'  variance matrix
  s2 <- sum(sq_e2) / (dim(X2)[1] - dim(X2)[2])
  Var_b2 <- s2*solve(t(X2)%*%X2)
  
  # format regression output: coefficients and standard errors. these will be included in the final output of the function.
  output3 <- cbind(b2,sqrt(diag(Var_b2)))
  colnames(output3) <- c("coeffs" , "std_err")
  if (xvar2=="") {
    rownames(output3) <- c("const", xvar1)
  } else if (xvar3=="") {
    rownames(output3) <- c("const", xvar1, xvar2)
  } else if (xvar4=="") {
    rownames(output3) <- c("const", xvar1, xvar2, xvar3)
  } else if (xvar4!="") {
    rownames(output3) <- c("const", xvar1, xvar2, xvar3, xvar4)
  }
  
  
  ### automatically flag observations lying close to the curve
  
  # generate a data matrix with all observations. this data matrix contains the observation identifier, the original flag variable inputted by the user and the variabe that have been included in the regression model
  mydata <- myfulldata
  nam <- eval(parse(text=paste("mydata$", names, sep = "")))
  ydata <- eval(parse(text=paste("mydata$", yvar, sep = "")))
  xdata1 <- eval(parse(text=paste("mydata$", xvar1, sep = "")))
  if (xvar2!="") {
    xdata2 <- eval(parse(text=paste("mydata$", xvar2, sep = "")))
  } else {
    xdata2 <- 0
  }
  if (xvar3!="") {
    xdata3 <- eval(parse(text=paste("mydata$", xvar3, sep = "")))
  } else {
    xdata3 <- 0
  } 
  if (xvar4!="") {
    xdata4 <- eval(parse(text=paste("mydata$", xvar4, sep = "")))
  } else {
    xdata4 <- 0
  }
  fl <- eval(parse(text=paste("mydata$", flag, sep = "")))
  
  #adapt the size of the matrices b2 and Var_b2 (previously estimated) to different numbers of regressors, so that they can be used more easily to estimate fitted values and standard errors
  if (xvar2=="") {
    b2 <- rbind(b2,0,0,0)
    Var_b2 <- cbind( rbind(Var_b2,c(0,0),c(0,0),c(0,0)) , c(0,0,0,0,0),c(0,0,0,0,0),c(0,0,0,0,0) )
  } else if (xvar3=="") {
    b2 <- rbind(b2,0,0)
    Var_b2 <- cbind( rbind(Var_b2,c(0,0,0),c(0,0,0)) , c(0,0,0,0,0),c(0,0,0,0,0) )
  } else if (xvar4=="") {
    b2 <- rbind(b2,0)
    Var_b2 <- cbind( rbind(Var_b2,c(0,0,0,0)) , c(0,0,0,0,0) )
  }
  
  # estimate fitted values, their standard errors and residuals based on the data
  mydata$fit_ydata <- b2[1] + b2[2]*xdata1 + b2[3]*xdata2 + b2[4]*xdata3 + b2[5]*xdata4
  mydata$Var_fit_ydata <- Var_b2[1,1] + Var_b2[2,2]*xdata1^2 + Var_b2[3,3]*xdata2^2 +  + Var_b2[4,4]*xdata3^2 + Var_b2[5,5]*xdata4^2 + 
    + 2*Var_b2[1,2]*xdata1 + 2*Var_b2[1,3]*xdata2 + 2*Var_b2[1,4]*xdata3 + 2*Var_b2[1,5]*xdata4 + 
    + 2*Var_b2[2,3]*xdata1*xdata2 + 2*Var_b2[2,4]*xdata1*xdata3 + 2*Var_b2[2,5]*xdata1*xdata4 +
    + 2*Var_b2[3,4]*xdata2*xdata3 + 2*Var_b2[3,5]*xdata2*xdata4 +
    + 2*Var_b2[4,5]*xdata3*xdata4
  mydata$Stderr_fit_ydata <- sqrt(mydata$Var_fit_ydata)
  mydata$err <- ydata - mydata$fit_ydata
  
  # flag observations lying close to the curve. if flag_above=TRUE, then also all values above the curve are flagged. if flag_below=TRUE, then also all values below the curve are flagged. two different flags (autom_flag1 and autom_flag2) are generated, and then one is chosen based on the chosen flagging method ("fit" or "error")
  if (flag_above==TRUE & flag_below==TRUE) {
    mydata$autom_flag1 <- ifelse(ydata < mydata$fit_ydata-flag_threshold*mydata$Stderr_fit_ydata | ydata > mydata$fit_ydata+flag_threshold*mydata$Stderr_fit_ydata, 0 , 1)
    mydata$autom_flag2 <- ifelse(mydata$err < quantile(mydata$err,probs=(error_pctile)/100) | mydata$err > quantile(mydata$err,probs=(100-error_pctile)/100), 0 , 1)
  } else if (flag_above==TRUE) {
    mydata$autom_flag1 <- ifelse(ydata < mydata$fit_ydata-flag_threshold*mydata$Stderr_fit_ydata , 0 , 1)
    mydata$autom_flag2 <- ifelse(mydata$err < quantile(mydata$err,probs=(error_pctile)/100) , 0 , 1)
  } else if (flag_below==TRUE) {
    mydata$autom_flag1 <- ifelse(ydata > mydata$fit_ydata+flag_threshold*mydata$Stderr_fit_ydata , 0 , 1)
    mydata$autom_flag2 <- ifelse(mydata$err > quantile(mydata$err,probs=(100-error_pctile)/100), 0 , 1)
  } else {
    mydata$autom_flag1 <- 1
    mydata$autom_flag2 <- 1
  }
  
  # determine the flag to be used based on the argument of the function "method"
  if (method=="fit") {
    mydata$autom_flag <- mydata$autom_flag1
  } else {
    mydata$autom_flag <- mydata$autom_flag2
  }
  
  
  ### combine the flagging variable provided by the user with the flag automatically generated through this function. the idea is that the values (0s and 1s) assigned by the user in its flag variable should not be overwritten. the automatically assigned flag applies only to observations that have not been coded by the user.
   
  # generate a data matrix containing the observation identifier (nam), the flag variable provided by the user (fl), and the flag variable automatically generated through the regression model (autom_flag). this is stored as output1  
  output1 <- as.data.frame(cbind(nam,mydata$autom_flag,fl))
  colnames(output1) <- c(names, "autom_flag", "comb")
  output1$autom_flag <- as.numeric(output1$autom_flag)
  output1$comb <- as.numeric(output1$comb)
  
  # generate a comboflag that combines the user-provided flag and the automatic flag. The values of the user-provided flag have priority, and the automatically generated values are used only for those observations for which the user had not provided input
  output1$comboflag <- 0
  output1$comboflag[output1$autom_flag==1 & output1$comb!=0] <- 1

  # generate a variable indicating only those observations that have been flagged on top of those already flagged by the user
  output1$newflag <- 0
  output1$newflag[output1$comb!=0 & output1$comb!=1 & output1$autom_flag==1] <- 1

  # define two lists of observation identifiers, which are going to be included among the function's outputs. one list (output 4) includes all observation names/codes that are flagged using the previously described combo-approach; the other list (output5) contains only only the names/codes of those flagged observations that were not previously coded by the user
  output4 <- output1$companyname[output1$comboflag==1]
  output5 <- output1$companyname[output1$newflag==1]
  
  
  ### calculate number of false/true positives/negatives that can be identified by comparing the automatically generated flag with the ("true") classification provided by the user for a subset of the observations. this is going to be included in the function's output
  
  # generate variables identifying true/false positives/negatives
  mydata$true_pos <- mydata$autom_flag==1 & fl == 1
  mydata$false_pos <-  mydata$autom_flag==1 & fl == 0
  mydata$true_neg <- mydata$autom_flag==0 & fl == 0
  mydata$false_neg <-  mydata$autom_flag==0 & fl == 1
  
  # generate variables identifying unknown positives/negatives. unknown positives/negatives are those observations for which the user has provided no information regarding which group they belong to.
  mydata$unkn_pos <- mydata$autom_flag==1 & fl != 1 & fl != 0
  mydata$unkn_neg <-  mydata$autom_flag==0 & fl != 1 & fl != 0
  
  # count true/false/unknown positives/negatives
  true_pos <- sum(mydata$true_pos)
  true_neg <- sum(mydata$true_neg)
  false_pos <- sum(mydata$false_pos)
  false_neg <- sum(mydata$false_neg)
  unkn_pos <- sum(mydata$unkn_pos)
  unkn_neg <- sum(mydata$unkn_neg)
  
  # store the information in a confusion matrix that is included in the output of the function
  output2 <- as.data.frame(cbind(true_pos, true_neg, false_pos, false_neg, unkn_pos, unkn_neg ))
  colnames(output2) <- cbind("true_pos", "true_neg", "false_pos", "false_neg", "unkn_pos", "unkn_neg")
 
  
  ### compile output
   
  output <- list(output1, output2, output3, output4, output5)
  return(output)
}


#9. automflag_combine
automflag_combine <- function(mydata=sumstats_by_company[sumstats_by_company$ln_undup_n>3,] , flag="filteredout" , names="companyname" , automflag1 , automflag2, condition="AND") {
  
  
  ### this function flags observations in a dataset, combining two empirical rules based on regressions. the two empirical rules are derived through the function automflag. In essence, this function uses two automflag functions as arguments and combines them through an AND or an OR condition. therefore, each observation is flagged if it is flagged by both empirical rules (AND) or by at least one empirical rule (OR).
  
  ## this function does not require any particular package to be installed; it can be run with base R.
  ## this function has the identical outputs (with identical names) as the automflag function:
  # 1. a 2-column dataset where the observation name/code is in the first column and a binary (0/1) flag derived from the specified empirical rule in the second column. the flag essentially says if the observed value for the observation lies far away from a curve estimated from a subset of the observations. if so, the flag is set =1 and it means that the observation likely belongs to that group. 
  # 2. a matrix to evaluate the flag. this matrix is a confusion matrix counting true positives, false positives, etc.
  # 3. a table with the regression's coefficients
  # 4. a list of the names/codes of the observations that are flagged
  # 5. a list of the names/codes of the observations that are flagged and are not included in the list of flagged observations given as input to train the model
  ## this is the list of default argument of the function:  
  #automflag1 <- automflag(mydata=sumstats_by_company[sumstats_by_company$ln_undup_n>3,], xvar2="sqln_undup_n", xvar3="culn_undup_n", xvar4="quln_undup_n")
  ## an automflag function
  #automflag2 <- automflag(mydata=sumstats_by_company[sumstats_by_company$ln_undup_n>3,] , yvar="ln_n", xvar1="ln_undup_n", xvar2="sqln_undup_n", flag_above=FALSE, flag_below=TRUE)
  ## another automflag function. this function should have the same dataset, observation identifier and user-provided flag variable as automflag1
  #mydata <- sumstats_by_company[sumstats_by_company$ln_undup_n>3,]
  ## the same dataset indicated as an argument in the two automflag functions
  #flag <- "filteredout"
  ## the same user-provided flag variable used as an argument in the two automflag functions
  #names <- "companyname"
  ## the same observation identifier used as an argument in the two automflag functions
  #condition <- "AND"
  ## the condition to join the two automflag functions. This argument can be equal to "AND" (an observation is flagged by the combined rule if it is flagged by both automflag functions) or to "OR" (an observation is flagged by the combined rule if it is flagged by at least one of the two automflag functions) 
  
  
  ### merge the two automatic flags and compare them with the user-provided flag
  
  # merge the two automatic flags
  firstflag <- automflag1[[1]][,1:2]
  colnames(firstflag) <- c("companyname" , "firstflag")
  firstflag$firstflag <- as.numeric(firstflag$firstflag)
  secondflag <- automflag2[[1]][,1:2]
  colnames(secondflag) <- c("companyname" , "secondflag")
  secondflag$secondflag <- as.numeric(secondflag$secondflag)
  twoflags <- merge(firstflag,secondflag)
  
  # store the underlying regression coefficients as function output. output 3 is the same type of output as the homonimous output in the automflag function. 
  output3 <- rbind(automflag1[[3]] , automflag2[[3]])
  
  # gen a combined flag based on the two automatic flags and store as function output. to combine the two flags, the "OR" condition is used if specified in the function arguments, and the condition "AND" in all other cases
  twoflags$thirdflag <- 0
  if (condition=="OR") {
    twoflags$thirdflag[twoflags$firstflag==1 | twoflags$secondflag==1] <- 1
  } else {
    twoflags$thirdflag[twoflags$firstflag==1 & twoflags$secondflag==1] <- 1
  }
  
  
  # merge with the user-provided flag
  filterdata <- cbind( eval(parse(text=paste("mydata$", names, sep = ""))) , eval(parse(text=paste("mydata$", flag, sep = ""))) )
  colnames(filterdata) <- c("companyname" , "filteredout")
  twoflags <- merge(twoflags, filterdata, all.x=TRUE)
  twoflags$filteredout <- as.numeric(twoflags$filteredout)
  
  
  ### calculate number of false/true positives/negatives and store it as output2, in the same way as is done for the automflag function
  
  twoflags$true_pos <- twoflags$thirdflag==1 & twoflags$filteredout == 1
  twoflags$false_pos <-  twoflags$thirdflag==1 & twoflags$filteredout == 0
  twoflags$true_neg <- twoflags$thirdflag==0 & twoflags$filteredout == 0
  twoflags$false_neg <-  twoflags$thirdflag==0 & twoflags$filteredout == 1
  twoflags$unkn_pos <- twoflags$thirdflag==1 & twoflags$filteredout == -1
  twoflags$unkn_neg <-  twoflags$thirdflag==0 & twoflags$filteredout == -1
  true_pos <- sum(twoflags$true_pos)
  true_neg <- sum(twoflags$true_neg)
  false_pos <- sum(twoflags$false_pos)
  false_neg <- sum(twoflags$false_neg)
  unkn_pos <- sum(twoflags$unkn_pos)
  unkn_neg <- sum(twoflags$unkn_neg)
  
  #output 1 is the same type of output as the homonimous output in the automflag function. 
  output1 <- as.data.frame(cbind(twoflags$companyname,twoflags$thirdflag,twoflags$filteredout))
  colnames(output1) <- c("companyname" , "autom_flag" , "comb")
  output1$comboflag <- output1$comb
  output1$comboflag[output1$comb!=0&output1$comb!=1] <- output1$autom_flag[output1$comb!=0&output1$comb!=1]
  
  #output 2 is the same type of output as the homonimous output in the automflag function. 
  output2 <- as.data.frame(cbind(true_pos, true_neg, false_pos, false_neg, unkn_pos, unkn_neg ))
  colnames(output2) <- cbind("true_pos", "true_neg", "false_pos", "false_neg", "unkn_pos", "unkn_neg")
  output2
  
  
  ### generate lists of observation identifiers equivalent to output4 and output5 in the automflag function
  
  #output 4 and output5 are the same type of outputs as the homonimous outputs in the automflag function. 
  output4 <- twoflags$companyname[twoflags$true_pos==TRUE | twoflags$unkn_pos==TRUE]
  output5 <- twoflags$companyname[twoflags$unkn_pos==TRUE]
  # View(twoflags[twoflags$false_pos==1,])
  
  ### compile output
  output <- list(output1, output2, output3, output4, output5)
  return(output)  
  
}


# 10. subsetting hhigeo per quarter
hhigeo_subset<-function(quarter,data){
  hhigeo_q <- subset(data, qtr == quarter)
  hhigeo_q$label <- paste0(hhigeo_q$fua_name, "\n ", as.character(hhigeo_q$wmean))
  return(hhigeo_q)
}


# 11. plotting hhigeo

hhigeo_plot<-function(qrtr,hhigeo_q,geoinfo,resultspath,countrycode){

  ggplot(eval(parse(text=paste0("hhigeo_q$`",qrtr,"`")))) +
    geom_sf( aes(fill = wmean)) + theme_void() +
    theme(panel.grid.major = element_line(colour = "transparent")) +
    labs(title = paste("Labour market concentration index", qrtr,"\naverage over all occupations")) +
    scale_fill_continuous(name = "Labour market concentration index",low="blue", high="orange") +
    geom_sf_text(aes(label = label), size = 2.5, colour = "black")+
    geom_sf(data=geoinfo,alpha = 0)
  
  ggsave(paste0(resultspath,"HHI_",qrtr,"_", countrycode, ".png"), width = 15, height = 10, units = "cm")
}
