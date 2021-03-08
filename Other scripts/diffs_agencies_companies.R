

##### 
#with this code, i explore differencies between companynames belonging (and not belonging) to agencies, starting from the list of agencies in the "check_company_names" files 
#####


### sourcing the code "check_company_names"

getwd()
source("Other scripts/filter_out_agencies_country.R")
source("Other scripts/consolidate_company_names_country.R")

### making a query many variables with respect to which we could expect to find some differencies between agencies and non-agencies

# the query
general_query <- query_athena("SELECT companyname, general_id, grab_date, idesco_level_4, idesco_level_3, idcity, idprovince, idregion, idsector, idcategory_sector FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE idcountry='IT' ORDER BY RAND()  LIMIT 1000000")
dim(general_query)

#generating a "duplicate" variable and standardising companynames
general_query$dup <- ifelse(duplicated(general_query$general_id), 1, 0)
general_query$companyname <- str_trim(general_query$companyname)
general_query$companyname <- gsub(" ","_",general_query$companyname)
general_query$companyname <- gsub("é","e",general_query$companyname)
temp <- as.character(general_query$companyname)
ordered <- as.data.frame(sapply(temp, function(x) sep(x)))
colnames(ordered) <- "companyname_new"
general_query$companyname <- ordered$companyname_new
#dropping empty cells
general_query$notgood <- ifelse(general_query$companyname=="",1,0)
general_query <- general_query[general_query$notgood != 1 , -3]
str(general_query)




### merge the newly extracted dataset with two variable indicating if the companyname has been flagged as an agency (filteredout) or identified as a company (keep) in the code "check_company_names"

# "filtereout" variable flagging agencies

filterlist <- c(filteredout$companyname , add_filteredout)
filteredout_m <- as.data.frame(table(filterlist))
filteredout_m$Freq <- 1
colnames(filteredout_m) <- c("companyname", "filteredout")

# "keep" variable identifying companynames that have been identified as belonging to a company

keep_m <- companies_names_dataframe$companyname[companies_names_dataframe$Freq>109]
str(keep_m)
keep_m <- c(keep_m , add_keep)
str(keep_m)
keep_m <- as.data.frame(table(keep_m))
keep_m$Freq <- 1
colnames(keep_m) <- c("companyname", "keep")

# merge new dataset with "keep" and "filteredout" variables

DF <- merge(general_query, filteredout_m, all.x=TRUE)
DF <- merge(DF, keep_m, all.x=TRUE)
dim(DF)
table(DF$keep)

### generating summary statistics at the companyname level and coding a unique variable (filteredout) identifying agencies and companies


# generate summary stats by companyname

DF <- group_by(DF , companyname)
sumstats_by_company <- summarise(DF , tot_n=n(), nd_esco4=n_distinct(idesco_level_4), nd_esco3=n_distinct(idesco_level_3), tot_dups=sum(dup), nd_city=n_distinct(idcity), m_city=sum(idcity==""&dup==0), nd_prov=n_distinct(idprovince), m_prov=sum(idprovince==""), nd_regi=n_distinct(idregion), nd_sect=n_distinct(idcategory_sector), filteredout=median(filteredout), keep=median(keep))
#sumstats_by_company <- summarise(DF , tot_n=n(), nd_esco4=n_distinct(idesco_level_4), nd_esco3=n_distinct(idesco_level_3), tot_dups=sum(dup), nd_city=n_distinct(idcity), m_city=sum(idcity==""&dup==0), nd_prov=n_distinct(idprovince), m_prov=sum(idprovince==""), nd_regi=n_distinct(idregion), nd_sect=n_distinct(idcategory_sector), nd_grab=n_distinct(grab_date),  filteredout=median(filteredout), keep=median(keep))
sumstats_by_company <- arrange(sumstats_by_company , desc(tot_n))
sumstats_by_company$r_dup <- 100*sumstats_by_company$tot_dups / sumstats_by_company$tot_n
sumstats_by_company$r_esco4 <- 100*sumstats_by_company$nd_esco4 / sumstats_by_company$tot_n
sumstats_by_company$r_grab <- 100*sumstats_by_company$nd_grab / sumstats_by_company$tot_n
sumstats_by_company$r_sect <- 100*sumstats_by_company$nd_sect / sumstats_by_company$tot_n
sumstats_by_company$r_city <- 100*sumstats_by_company$nd_city / sumstats_by_company$tot_n
sumstats_by_company$rm_city <- 100*sumstats_by_company$m_city / (sumstats_by_company$tot_n-sumstats_by_company$tot_dups)


# coding the variable filteredout as follows:
# 1 <- flagged as agency
# 0 <- identified as company
# -1 <- not checked (unknown)

sumstats_by_company$filteredout[sumstats_by_company$keep==1] <- 0
sumstats_by_company$filteredout[is.na(sumstats_by_company$filteredout)==TRUE] <- -1


### summary stats by variable "filteredout", which is equal to 1 if the observation has been labelled as agency, 0 for company, -1 for not labelled yet

sumstats_by_company <- sumstats_by_company[sumstats_by_company$tot_n>29 , ]
DF2 <- group_by(sumstats_by_company , filteredout)
summarise(DF2, tot_n=n_distinct(companyname), sd_esco4=sd(r_esco4), r_esco4=mean(r_esco4), sd_m_esco4=sd(nd_esco4) , m_esco4=mean(nd_esco4), sd_city=sd(r_city), r_city=mean(r_city), sd_dup=sd(r_dup), r_dup=mean(r_dup))


### cross filteredout and other indicators through filters

# r_esco4 by filteredout
prova <- sumstats_by_company[sumstats_by_company$r_esco4>50 , ]
table(prova$filteredout)
prova <- sumstats_by_company[sumstats_by_company$r_esco4<1 , ]
table(prova$filteredout)

# r_city by filteredout
prova <- sumstats_by_company[sumstats_by_company$r_city>50 , ]
table(prova$filteredout)
prova <- sumstats_by_company[sumstats_by_company$r_city<1 , ]
table(prova$filteredout)


### cross variables with filteredout through charts 

ggplot(data = sumstats_by_company[sumstats_by_company$tot_n>99 , ]) + 
  geom_point(mapping = aes(x = r_esco4, y = filteredout))

ggplot(data = sumstats_by_company[sumstats_by_company$tot_n>99 , ]) + 
  geom_point(mapping = aes(x = r_city, y = filteredout))

ggplot(data = sumstats_by_company[sumstats_by_company$tot_n>99 , ]) + 
  geom_point(mapping = aes(x = r_dup, y = filteredout))

ggplot(data = sumstats_by_company[sumstats_by_company$tot_n>99 , ]) + 
  geom_point(mapping = aes(x = r_sect, y = filteredout))


ggplot(data = sumstats_by_company[sumstats_by_company$tot_n>99 , ]) + 
  geom_point(mapping = aes(x = r_grab, y = filteredout))


### creating a log-log plot between number of ads and number of distinct esco codes

#generate logs
sumstats_by_company$ln_esco4 <- log(sumstats_by_company$nd_esco4)
sumstats_by_company$sqln_esco4 <- log(sumstats_by_company$nd_esco4)^2
sumstats_by_company$ln_esco3 <- log(sumstats_by_company$nd_esco3)
sumstats_by_company$ln_n <- log(sumstats_by_company$tot_n)
sumstats_by_company$ln_undup_n <- log(sumstats_by_company$tot_n - sumstats_by_company$tot_dups)
sumstats_by_company$sqln_undup_n <- sumstats_by_company$ln_undup_n^2
sumstats_by_company$culn_undup_n <- sumstats_by_company$ln_undup_n^3
sumstats_by_company$quln_undup_n <- sumstats_by_company$ln_undup_n^4
sumstats_by_company$ln_dup_n <- log(sumstats_by_company$tot_dups)
sumstats_by_company$ln_prov <- log(sumstats_by_company$nd_prov)
sumstats_by_company$sqln_prov <- log(sumstats_by_company$nd_prov)^2
sumstats_by_company$ln_regi <- log(sumstats_by_company$nd_regi)
sumstats_by_company$small <- ifelse(sumstats_by_company$tot_n<6,1,0)
sumstats_by_company$ln_m_city <- log(sumstats_by_company$m_city)
sumstats_by_company$ln_m_prov <- log(sumstats_by_company$m_prov)
sumstats_by_company$ln_city <- log(sumstats_by_company$nd_city)
#sumstats_by_company$ln_grab <- log(sumstats_by_company$nd_grab)
#sumstats_by_company$sqln_grab <- log(sumstats_by_company$nd_grab)^2

### plots

#very good
plotdata <- sumstats_by_company[sumstats_by_company$tot_n>15 & sumstats_by_company$filteredout != -1, ]
ggplot(data = plotdata) + 
  geom_point(mapping = aes(x = ln_undup_n, y = ln_esco3, colour=filteredout))
#View(sumstats_by_company[sumstats_by_company$filteredout==0,])


#good
plotdata <- sumstats_by_company[sumstats_by_company$tot_n>30 & sumstats_by_company$filteredout != -1, ]
ggplot(data = plotdata) + 
  geom_point(mapping = aes(x = ln_prov, y = ln_esco4, colour=filteredout))

#good
plotdata <- sumstats_by_company[sumstats_by_company$tot_n>100 & sumstats_by_company$filteredout != -1, ]
ggplot(data = plotdata) + 
  geom_point(mapping = aes(x = ln_undup_n, y = ln_n, colour=filteredout))


plotdata <- sumstats_by_company[sumstats_by_company$tot_n>100 & sumstats_by_company$filteredout != -1, ]
ggplot(data = plotdata) + 
  geom_point(mapping = aes(x = ln_dup_n, y = ln_regi, colour=filteredout))




plotdata <- sumstats_by_company[sumstats_by_company$tot_n>100 & sumstats_by_company$filteredout != -1, ]
ggplot(data = plotdata) + 
  geom_point(mapping = aes(x = ln_prov, y = ln_grab, colour=filteredout))

plotdata <- sumstats_by_company[sumstats_by_company$tot_n>100 & sumstats_by_company$filteredout != -1, ]
ggplot(data = plotdata) + 
  geom_point(mapping = aes(x = ln_city, y = ln_m_city, colour=filteredout))

plotdata <- sumstats_by_company[sumstats_by_company$tot_n>100 & sumstats_by_company$filteredout != -1, ]
ggplot(data = plotdata) + 
  geom_point(mapping = aes(x = ln_undup_n, y = ln_grab, colour=filteredout))

plotdata <- sumstats_by_company[sumstats_by_company$tot_n>100 & sumstats_by_company$filteredout != -1, ]
ggplot(data = plotdata) + 
  geom_point(mapping = aes(x = ln_undup_n, y = ln_grab, colour=filteredout))

plotdata <- sumstats_by_company[sumstats_by_company$filteredout != -1, ]
ggplot(data = plotdata) + 
  geom_point(mapping = aes(x = ln_undup_n, y = ln_esco3, colour=filteredout))











##############################################################################################
#code starts here


##############################################################################################
# sumstats/query function
##############################################################################################

#ante-function
keep <- as.data.frame(companies_names_dataframe$companyname[companies_names_dataframe$Freq>99])
colnames(keep) <- "companyname"
keep <- rbind(keep,add_keep)
drop <- as.data.frame(filteredout$companyname)
colnames(drop) <- "companyname"
drop <- rbind(drop,add_filteredout)
#View(keep)



gen_sum_stats <- function(idcountry = "IT", samplesize = "1000000", filterlist = drop$companyname, keeplist = keep$companyname, key_var = "companyname", vars = "grab_date, idesco_level_4, idesco_level_3, idcity, idprovince, idregion, idsector, idcategory_sector ", sumstats = "n_distinct") {
  
  ### this function creates a list of summary statistics by key_var (in the default, by companyname) and merge them with some lists that can be used as filters (in the default, with filteredout and keep)
  # this is a list of potential inputs that could be given to the function:
  #vars <- "grab_date, idesco_level_4, idesco_level_3, idcity, idprovince, idregion, idsector, idcategory_sector "
  #idcountry <- "IT"
  #samplesize <- "1000000"
  #filterlist <- drop$companyname
  #keeplist <- keep$companyname
  #key_var <- "companyname"
  #sumstats <- "n_distinct"
  
  
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
  ordered <- sapply(general_query$keyvar, function(x) sep(x))
  general_query$keyvar <- ordered
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
  sumstats_by_company_2 <- summarise_all(DF, sumstats)
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
  dim(sumstats_by_company)
  
  # classification
  # coding the variable filteredout as follows:
  # 1 <- flagged as agency
  # 0 <- identified as company
  # -1 <- not checked (unknown)
  sumstats_by_company$filteredout[sumstats_by_company$keep==1] <- 0
  sumstats_by_company$filteredout[is.na(sumstats_by_company$filteredout)==TRUE] <- -1
  table(sumstats_by_company$filteredout)
  
  ### prepare function output
  
  # rename keyvar back to its original name
  colnames(sumstats_by_company) <- c(key_var,colnames(sumstats_by_company)[2:length(colnames(sumstats_by_company))])
  
  #define output
  return(sumstats_by_company)
  
}


sumstats_by_company <-gen_sum_stats(filterlist = filteredout$companyname, keeplist = keep$companyname)
str(sumstats_by_company)
table(sumstats_by_company$keep)

#generate logs
sumstats_by_company$ln_esco3 <- log(sumstats_by_company$idesco_level_3)
sumstats_by_company$ln_undup_n <- log(sumstats_by_company$tot_n - sumstats_by_company$tot_dups)
sumstats_by_company$sqln_undup_n <- sumstats_by_company$ln_undup_n^2
sumstats_by_company$culn_undup_n <- sumstats_by_company$ln_undup_n^3
sumstats_by_company$quln_undup_n <- sumstats_by_company$ln_undup_n^4
sumstats_by_company$ln_n <- log(sumstats_by_company$tot_n)

sumstats_by_company$ln_esco4 <- log(sumstats_by_company$idesco_level_4)
sumstats_by_company$sqln_esco4 <- log(sumstats_by_company$idesco_level_4)^2
sumstats_by_company$ln_prov <- log(sumstats_by_company$idprovince)
sumstats_by_company$sqln_prov <- log(sumstats_by_company$idprovince)^2
sumstats_by_company$ln_regi <- log(sumstats_by_company$idregion)
sumstats_by_company$ln_city <- log(sumstats_by_company$idcity)
sumstats_by_company$ln_grab <- log(sumstats_by_company$grab_date)
sumstats_by_company$sqln_grab <- log(sumstats_by_company$grab_date)^2
sumstats_by_company$small <- ifelse(sumstats_by_company$tot_n<6,1,0)
#dummy for small companies
sumstats_by_company$small <- ifelse(sumstats_by_company$tot_n<6,1,0)


##############################################################################################
# function for automatic flag
##############################################################################################





automflag <- function(mydata=sumstats_by_company , flag="filteredout" , names="companyname" , yvar="ln_esco3", xvar1="ln_undup_n", xvar2="", xvar3="", xvar4="", percentile=50, flag_threshold=1.96, flag_above=TRUE, flag_below=FALSE, method="fit", error_pctile=90) {
  
  mydata <- sumstats_by_company[sumstats_by_company$ln_undup_n>3,]
  flag <- "filteredout"
  names <- "companyname"
  yvar <- "ln_esco3"
  xvar1 <- "ln_undup_n"
  xvar2 <- "sqln_undup_n"
  xvar3 <- ""
  xvar4 <- ""
  percentile <- 50
  flag_threshold<-1.96
  flag_above <- TRUE
  flag_below <- FALSE
  method <- "fit"
  error_pctile <- 90
  
  
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
  #View(mydata[mydata$filteredout==0&mydata$autom_flag1==1,])
  #View(mydata[mydata$filteredout==1,])
  
  output1 <- as.data.frame(cbind(nam,mydata$autom_flag))
  colnames(output1) <- c(names, "autom_flag")
  
  output45 <- as.data.frame(cbind(nam,mydata$autom_flag,fl))
  colnames(output45) <- c(names, "autom_flag", "comb")
  output45$autom_flag <- as.numeric(output45$autom_flag)
  output45$comb <- as.numeric(output45$comb)
  output45$comboflag <- 0
  output45$comboflag[output45$autom_flag==1 & output45$comb!=0] <- 1
  output45$newflag <- 0
  output45$newflag[output45$comb!=0 & output45$comb!=1 & output45$autom_flag==1] <- 1
  output4 <- output45$companyname[output45$comboflag==1]
  output5 <- output45$companyname[output45$newflag==1]
  
  
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

#rule 1
automflag_output <- automflag(mydata=sumstats_by_company[sumstats_by_company$ln_undup_n>3,], xvar2="sqln_undup_n", xvar3="culn_undup_n", xvar4="quln_undup_n")
comboflag <- automflag_output[[4]]
automflag_output[[2]]


### code ends here
#######################################################################################################







automflag_combine <- function(mydata=sumstats_by_company , flag="filteredout" , names="companyname" , automflag1 , automflag2, condition="AND") {
  
  #automflag1 <- automflag(mydata=sumstats_by_company[sumstats_by_company$ln_undup_n>3,], xvar2="sqln_undup_n", xvar3="culn_undup_n", xvar4="quln_undup_n")
  #automflag2 <- automflag(mydata=sumstats_by_company[sumstats_by_company$ln_undup_n>3,] , yvar="ln_n", xvar1="ln_undup_n", xvar2="sqln_undup_n", flag_above=FALSE, flag_below=TRUE)
  ##automflag2 <- automflag(mydata=sumstats_by_company[sumstats_by_company$ln_undup_n>3,] , yvar="ln_n", xvar1="ln_undup_n", flag_above=TRUE, flag_below=TRUE)
  #mydata <- sumstats_by_company[sumstats_by_company$ln_undup_n>3,]
  #flag <- "filteredout"
  #names <- "companyname"
  #condition <- "AND"

    
  ### merge the two automatic flags and compare them withthe user-provided flag
  
  # merge the two automatic flags
  firstflag <- automflag1[[1]]
  colnames(firstflag) <- c("companyname" , "firstflag")
  firstflag$firstflag <- as.numeric(firstflag$firstflag)
  secondflag <- automflag2[[1]]
  colnames(secondflag) <- c("companyname" , "secondflag")
  secondflag$secondflag <- as.numeric(secondflag$secondflag)
  twoflags <- merge(firstflag,secondflag)

  # store the underlying regression coefficients as function output
  output3 <- rbind(automflag1[[3]] , automflag2[[3]])
  
  # gen a combined flag based on the two automatic flags and store as function output
  twoflags$thirdflag <- 0
  if (condition=="OR") {
    twoflags$thirdflag[twoflags$firstflag==1 | twoflags$secondflag==1] <- 1
  } else {
    twoflags$thirdflag[twoflags$firstflag==1 & twoflags$secondflag==1] <- 1
  }
  output1 <- as.data.frame(cbind(twoflags$companyname , twoflags$thirdflag))
  colnames(output1) <- c("companyname" , "autom_flag")
  
  # merge with the user-provided flag
  filterdata <- cbind( eval(parse(text=paste("mydata$", names, sep = ""))) , eval(parse(text=paste("mydata$", flag, sep = ""))) )
  colnames(filterdata) <- c("companyname" , "filteredout")
  twoflags <- merge(twoflags, filterdata, all.x=TRUE)
  twoflags$filteredout <- as.numeric(twoflags$filteredout)
  
  
  ### calculate number of false/true positives/negatives and store it as output2
  
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
  output2 <- as.data.frame(cbind(true_pos, true_neg, false_pos, false_neg, unkn_pos, unkn_neg ))
  colnames(output2) <- cbind("true_pos", "true_neg", "false_pos", "false_neg", "unkn_pos", "unkn_neg")
  output2
  
  
  ### generate lists of observation identifiers equivalent to output4 and output5 in the automflag function
  
  output4 <- twoflags$companyname[twoflags$true_pos==TRUE | twoflags$unkn_pos==TRUE]
  output5 <- twoflags$companyname[twoflags$unkn_pos==TRUE]
  # View(twoflags[twoflags$false_pos==1,])
  
  ### compile output
  output <- list(output1, output2, output3, output4, output5)
  return(output)  
  
}


testflag1 <- automflag(mydata=sumstats_by_company[sumstats_by_company$ln_undup_n>3,], xvar2="sqln_undup_n", xvar3="culn_undup_n", xvar4="quln_undup_n")
testflag1[[2]]
testflag2 <- automflag(mydata=sumstats_by_company[sumstats_by_company$ln_undup_n>3,] , yvar="ln_n", xvar1="ln_undup_n", xvar2="sqln_undup_n", flag_above=FALSE, flag_below=TRUE)
testflag2[[2]]
testflag3 <- automflag(mydata=sumstats_by_company[sumstats_by_company$ln_undup_n>3,] , yvar="ln_n", xvar1="ln_undup_n", flag_above=TRUE, flag_below=TRUE)
testflag3[[2]]

prova <- automflag_combine(mydata=sumstats_by_company , flag="filteredout" , names="companyname" , automflag1= testflag1, automflag2= testflag2 , condition="AND" )
prova[[2]]
prova3 <- automflag_combine(mydata=sumstats_by_company , flag="filteredout" , names="companyname" , automflag1= prova, automflag2= testflag3 , condition="OR" )
prova3[[2]]

str(testflag1[[1]])
str(prova[[1]])