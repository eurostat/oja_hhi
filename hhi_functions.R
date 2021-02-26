#This file contains the functions used by the main code to calculate the Labour Market Concentration Index.
# List of functions:
# 1. sep
# 2. empty_as_na
# 3. createfua
# 4. calculate_hhi
# 5. create_hhigeo
# 6. gen_sum_stats
# 7. autom_flag

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
  options (timeout = 100)
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
  
  # input manually the NUTS2013-NUTS2016 correspondence for 3 NUTS2016 regions
  DF$NUTS_3_CODE[DF$NUTS_3_2013=="DE915"|DF$NUTS_3_2013=="DE919"] <- "DE91C"
  DF$recoded[DF$NUTS_3_2013=="DE915"|DF$NUTS_3_2013=="DE919"] <- 1
  DF$recoded[DF$NUTS_3_2013=="NL322"] <- 1
  DF$recoded[DF$NUTS_3_2013=="NL326"] <- 1
  
  
  
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


calculate_hhi <- function (dframe = dframe) {
  
  ####CALCULATE THE HERFINDAHL HIRSCHMAN INDEX =============
  # compute market shares by quarter, FUA and esco level 4 occupation
  # create grids of occupation, geo unit and quarter
  grid <- expand.grid(esco = unique(dframe$idesco_level_4), geo = unique(dframe$fua_id), qtr = unique(dframe$qtr), stringsAsFactors = FALSE)
  
  # count obs per occupation, region, quarter
  setDT(dframe)
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
  return (hhi)
}

create_hhigeo <- function(hhi = hhi){
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

gen_sum_stats <- function(idcountry = "IT", samplesize = "1000000", filterlist = filteredout$companyname, keeplist = keep$companyname, key_var = "companyname", vars = "grab_date, idesco_level_4, idesco_level_3, idcity, idprovince, idregion, idsector, idcategory_sector ") {
  
  ### this function creates a list of summary statistics by key_var (in the default, by companyname) and merge them with some lists that can be used as filters (in the default, with filteredout and keep)
  # this is a list of potential inputs that could be given to the function:
  #vars <- "grab_date, idesco_level_4, idesco_level_3, idcity, idprovince, idregion, idsector, idcategory_sector "
  #idcountry <- "IT"
  #samplesize <- "1000000"
  #filterlist <- filteredout$companyname
  #keeplist <- keep$companyname
  #key_var <- "companyname"
  
  
  ### compile and run query
  
  querytext <- paste0("SELECT " , key_var, ", general_id, " , vars , " FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE idcountry='" , idcountry , "' ORDER BY RAND()  LIMIT " , samplesize)
  general_query <- query_athena(querytext)
  dim(general_query)
  
  
  ### arrange the new dataframe as needed
  
  # rename the variable chosen as key_var
  colnames(general_query) <- c("keyvar",colnames(general_query)[2:length(colnames(general_query))])
  
  #generate a "duplicate" variable
  general_query$dup <- ifelse(duplicated(general_query$general_id), 1, 0)
  general_query$keyvar <- str_to_lower(general_query$keyvar)
  
  #standardize companyname
  ordered <- sapply(companies_names_dataframe$companyname, function(x) sep(x))
  companies_names_dataframe$companyname <- ordered
  #companies_names_dataframe$companyname <- gsub(",|;|.","",companies_names_dataframe$companyname)
  general_query$keyvar <- str_trim(general_query$keyvar)
  general_query$keyvar <- gsub(" ","_",general_query$keyvar)
  
  # eliminate missing data in keyvar
  general_query$notgood <- ifelse(general_query$keyvar=="",1,0)
  general_query <- general_query[general_query$notgood != 1 , ]
  
  
  ### generating summary statistics at the companyname level
  
  # base set of stats
  DF <- general_query
  DF <- group_by(DF , keyvar)
  sumstats_by_company_1 <- summarise(DF , tot_n=n(), tot_dups=sum(dup))
  sumstats_by_company_2 <- summarise_all(DF, n_distinct)
  sumstats_by_company <- merge(sumstats_by_company_1 , sumstats_by_company_2, all.x=TRUE)
  
  
  ### merge the newly extracted dataset with the lists provided as input (filteredout and keep in the default) and code a unique variable (filteredout) identifying agencies and companies
  
  # "filteredout" variable flagging agencies
  filteredout_m <- as.data.frame(table(filterlist))
  filteredout_m$Freq <- 1
  colnames(filteredout_m) <- c("keyvar", "filteredout")
  
  # "keep" variable identifying companynames that have been identified as belonging to a company
  keep_m <- as.data.frame(table(keeplist))
  keep_m$Freq <- 1
  colnames(keep_m) <- c("keyvar", "keep")  
  
  # merge new dataset with "keep" and "filteredout" variables
  sumstats_by_company <- merge(sumstats_by_company, filteredout_m, all.x=TRUE)
  sumstats_by_company <- merge(sumstats_by_company, keep_m, all.x=TRUE)
  dim(DF)
  
  # classification
  # coding the variable filteredout as follows:
  # 1 <- flagged as agency
  # 0 <- identified as company
  # -1 <- not checked (unknown)
  sumstats_by_company$filteredout[sumstats_by_company$keep==1] <- 0
  sumstats_by_company$filteredout[is.na(sumstats_by_company$filteredout)==TRUE] <- -1
  
  
  ### prepare function output
  
  # rename keyvar back to its original name
  colnames(sumstats_by_company) <- c(key_var,colnames(sumstats_by_company)[2:length(colnames(sumstats_by_company))])
  
  #define output
  return(sumstats_by_company)
  
}

automflag <- function(mydata=sumstats_by_company , flag="filteredout" , names="companyname" , yvar="ln_esco3", xvar1="ln_undup_n", xvar2="", xvar3="", xvar4="", percentile=50, flag_threshold=1.96, flag_above=TRUE, flag_below=FALSE, method="fit", error_pctile=90) {
  
  #mydata <- sumstats_by_company
  #flag <- "filteredout"
  #names <- "companyname"
  #yvar <- "ln_esco3"
  #xvar1 <- "ln_undup_n"
  #xvar2 <- "sqln_undup_n"
  #xvar3 <- ""
  #xvar4 <- ""
  #percentile <- 50
  #flag_threshold<-1.96
  #flag_above <- TRUE
  #flag_below <- FALSE
  #method <- "error"
  #error_pctile <- 90
  
  
  #create a vector of one (for model constant) and filter the data (myregdata)
  mydata$one <- 1
  myfulldata <- mydata
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
  
  
  #create matrices of Y and X
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
  
  
  # calculate regression coefficients and squared residuals
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
  
  # format regression output
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
  
  
  # apply automatic filter and save it as output1
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
  
  #adapt the size of the matrices b2 and Var_b2 to different numbers of regressors
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
  
  
  mydata$fit_ydata <- b2[1] + b2[2]*xdata1 + b2[3]*xdata2 + b2[4]*xdata3 + b2[5]*xdata4
  mydata$Var_fit_ydata <- Var_b2[1,1] + Var_b2[2,2]*xdata1^2 + Var_b2[3,3]*xdata2^2 +  + Var_b2[4,4]*xdata3^2 + Var_b2[5,5]*xdata4^2 + 
    + 2*Var_b2[1,2]*xdata1 + 2*Var_b2[1,3]*xdata2 + 2*Var_b2[1,4]*xdata3 + 2*Var_b2[1,5]*xdata4 + 
    + 2*Var_b2[2,3]*xdata1*xdata2 + 2*Var_b2[2,4]*xdata1*xdata3 + 2*Var_b2[2,5]*xdata1*xdata4 +
    + 2*Var_b2[3,4]*xdata2*xdata3 + 2*Var_b2[3,5]*xdata2*xdata4 +
    + 2*Var_b2[4,5]*xdata3*xdata4
  mydata$Stderr_fit_ydata <- sqrt(mydata$Var_fit_ydata)
  mydata$err <- ydata - mydata$fit_ydata
  
  if (flag_above==TRUE & flag_below==TRUE) {
    mydata$autom_flag1 <- ifelse(ydata < mydata$fit_ydata-flag_threshold*mydata$Stderr_fit_ydata | ydata > mydata$fit_ydata+flag_threshold*mydata$Stderr_fit_ydata, 0 , 1)
    mydata$autom_flag2 <- ifelse(mydata$err < quantile(mydata$err,probs=(100-error_pctile)/100) | mydata$err > quantile(mydata$err,probs=error_pctile/100), 0 , 1)
  } else if (flag_above==TRUE) {
    mydata$autom_flag1 <- ifelse(ydata < mydata$fit_ydata-flag_threshold*mydata$Stderr_fit_ydata , 0 , 1)
    mydata$autom_flag2 <- ifelse(mydata$err < quantile(mydata$err,probs=(100-error_pctile)/100) , 0 , 1)
  } else if (flag_below==TRUE) {
    mydata$autom_flag1 <- ifelse(ydata > mydata$fit_ydata+flag_threshold*mydata$Stderr_fit_ydata , 0 , 1)
    mydata$autom_flag2 <- ifelse(mydata$err > quantile(mydata$err,probs=error_pctile/100), 0 , 1)
  } else {
    mydata$autom_flag1 <- 0
    mydata$autom_flag2 <- 0
  }
  
  if (method=="fit") {
    mydata$autom_flag <- mydata$autom_flag1
  } else {
    mydata$autom_flag <- mydata$autom_flag2
  }
  
  
  output1 <- as.data.frame(cbind(nam,mydata$autom_flag))
  colnames(output1) <- c(names, "autom_flag")
  
  output4 <- as.data.frame(cbind(nam,mydata$autom_flag,fl))
  colnames(output4) <- c(names, "autom_flag", "comb")
  output4$comboflag <- 0
  output4$comboflag[output4$autom_flag==1 & output4$comb!=0] <- 1
  output4 <- cbind(output4[output4$comboflag==1,1] , output4[output4$comboflag==1,4])
  colnames(output4) <- c(names, "comboflag")
  
  # calculate number of false/true positives/negatives and store it as output2
  mydata$true_pos <- mydata$autom_flag==1 & fl == 1
  mydata$false_pos <-  mydata$autom_flag==1 & fl == 0
  mydata$true_neg <- mydata$autom_flag==0 & fl == 0
  mydata$false_neg <-  mydata$autom_flag==0 & fl == 1
  mydata$unkn_pos <- mydata$autom_flag==1 & fl == -1
  mydata$unkn_neg <-  mydata$autom_flag==0 & fl == -1
  true_pos <- sum(mydata$true_pos)
  true_neg <- sum(mydata$true_neg)
  false_pos <- sum(mydata$false_pos)
  false_neg <- sum(mydata$false_neg)
  unkn_pos <- sum(mydata$unkn_pos)
  unkn_neg <- sum(mydata$unkn_neg)
  output2 <- as.data.frame(cbind(true_pos, true_neg, false_pos, false_neg, unkn_pos, unkn_neg ))
  colnames(output2) <- cbind("true_pos", "true_neg", "false_pos", "false_neg", "unkn_pos", "unkn_neg")
  
  output <- list(output1, output2, output3, output4)
  return(output)
}