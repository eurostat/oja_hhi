#This file contains the functions used by the main code to calculate the Labour Market Concentration Index.
# List of functions:
# 1. sep
# 2. empty_as_na
# 3. createfua

##Function for cleaning the 'companyname' column

sep <- function(linha) {
  resp <- strsplit(linha," |/|-")
  resp <- unlist(resp)
  resp <- gsub(",|;|\\.","",resp)
  resp <- sort(resp[which(nchar(resp) > 2)])
  resp <- paste0(resp,collapse=" ")
  resp <- tolower(resp)
}


## Function for setting empty values to NA

empty_as_na <- function(y){
  
  y[!str_detect(y, "")] <- NA
  
  return(y)
}


## Function for creating the correspondence table between LAU, NUTS and FUA

createfua <- function(){
  ### download file with eurostat classification
  
  download.file("https://ec.europa.eu/eurostat/documents/345175/501971/EU-28-LAU-2019-NUTS-2016.xlsx ", destfile="EU-28-LAU-2019-NUTS-2016.xlsx")
  filename <- "EU-28-LAU-2019-NUTS-2016.xlsx"
  
  ###selecting countries
  countrylist <- getSheetNames(filename)[4:31]
  #alternatively: countrylist <- c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE")
  
  ###create a function generating the "assign" variable
  assignFUA <- function(country) {
    #importing excel sheet and eliminating spaces from variable names
    DF <- read_excel(filename , sheet = country)
    colnames(DF) <- substr(str_replace_all(colnames(DF) , " " , "_") , 1 , 11)
    
    #finding which LAUs belong to the same NUTS and FUAs
    DF$NUTSFUA <- paste0(DF$NUTS_3_CODE , DF$FUA_ID)
    DF$dup <- ifelse(duplicated(DF$NUTSFUA , fromLast=FALSE) , 0 , 1)
    
    #Adding count of observations and of duplicates by NUTS
    ntable <- as.data.frame(table(DF$NUTS_3_CODE))
    colnames(ntable) <- c("NUTS_3_CODE" , "count")
    duptable <- as.data.frame(table(DF$NUTS_3_CODE[DF$dup == 1]))
    colnames(duptable) <- c("NUTS_3_CODE" , "dup_count")
    idtable <- as.data.frame(table(DF$NUTS_3_CODE[is.na(DF$FUA_ID) == FALSE]))
    colnames(idtable) <- c("NUTS_3_CODE" , "FUAID_count")
    DF <- merge(DF , ntable, all.x=TRUE)
    DF <- merge(DF , duptable, all.x=TRUE)
    DF <- merge(DF , idtable, all.x=TRUE)
    DF$FUAID_count[is.na(DF$FUAID_count)==TRUE] <- -999
    
    #generate an assign variable equal to 1 if all LAUs in a given NUTS can be automatically assigned to the same FUA
    DF$assign <- 0
    DF$assign[DF$dup_count == 1] <- 1 
    DF$assign[DF$FUAID_count < DF$count] <- 0
    DF <- select(DF, NUTS_3_CODE , LAU_CODE , FUA_ID, LAU_NAME_NA , LAU_NAME_LA , assign)
    
    #defining the output of the function
    return(DF)
  }
  
  ###applying the previous function to the countries selected at the beginning of this code
  DFlist <- lapply(countrylist,assignFUA)
  
  x <- cbind.data.frame(0,0,0,0,0,0)[-1,]
  colnames(x) <- c("NUTS_3_CODE" , "LAU_CODE" , "FUA_ID" , "LAU_NAME_NA" , "LAU_NAME_LA" , "assign")
  for(i in 1:length(countrylist)) {
    x <- rbind(x,DFlist[[i]])
  }
  
  
  
  ### download the correspondence between NUTS2013 and NUTS2016. the OJA data uses NUTS2013, but the correspondence with FUA is available only for NUTS2016 and the associated LAU units. so it necessary to generate the "assign" variable with NUTS2016, and then change it to NUTS2013
  
  download.file("https://ec.europa.eu/eurostat/documents/345175/629341/NUTS2013-NUTS2016.xlsx", destfile="NUTS2013-NUTS2016.xlsx")
  filename2 <- "NUTS2013-NUTS2016.xlsx"
  
  ### open the table showing the NUTS units for which a change occurred between the 2013 and 2016 classification; generate a new "recoded" variable with the following values:
  # 2 for NUTS2016 units that have no direct (1:1) correspondence with any NUTS2013 units
  # 1 for NUTS2016 units which, compared to the NUTS2013 classification, remained identical but changed name 
  # 0 for NUTS2016 units for which there has been no change, compared to NUTS2013 (the value 0 is assigned later in the code, after the merge with the main table)
  DF <- read_excel(filename2 , sheet = "Correspondence NUTS-3")
  colnames(DF) <- substr(str_replace_all(colnames(DF) , " " , "_") , 1 , 11)
  DF$recoded <- 2
  DF$recoded[DF$Change=="recoded"] <- 1
  DF <- select(DF , Code_2013 , Code_2016 , recoded)
  colnames(DF) <- c("NUTS_3_2013" , "NUTS_3_CODE" , "recoded")
  
  
  ### merge the NUTS2016-NUTS2013 correspondence table with the main NUTS-LAU-FUA areas and assign country variable
  x <- merge(x , DF , all.x=TRUE)
  x$recoded[is.na(x$recoded)==TRUE] <- 0
  x$country <- substr(x$NUTS_3_CODE,1,2)
  
  ### change the value of NUTS_3_CODE to make it compatible with the 2013 classification. in short, when recoded=1, then the NUTS2013 code is used instead of the NUTS2016 code
  
  x$NUTS_3_CODE[x$recoded==1] <- x$NUTS_3_2013[x$recoded==1]
  x$NUTS_3_CODE[x$recoded==2] <- 0
  x$assign[x$recoded==2] <- 0
  
  
  # final vector to be used for the calculation of the LMCI
  
  fua <- select(x, country , NUTS_3_CODE , LAU_CODE , FUA_ID, LAU_NAME_NA , LAU_NAME_LA , recoded , assign)
  colnames(fua) <- c("country" , "idprovince" , "idcity" , "fua_id" , "city" , "city_latin" , "recoded", "var1")
  
  rm(DF)
  rm(DFlist)
  rm(x, countrylist, filename, filename2, i, assignFUA)
  
  return(fua)
}