

##### 
#with this code, i explore differencies between companynames belonging (and not belonging) to agencies, starting from the list of agencies in the "check_company_names" files 
#####


### sourcing the code "check_company_names"

source("filter_out_agencies_country.R")


### making a query many variables with respect to which we could expect to find some differencies between agencies and non-agencies

# the query
general_query <- query_athena("SELECT companyname, general_id, grab_date, idesco_level_4, idesco_level_3, idcity, idprovince, idsector, idcategory_sector FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE idcountry='IT' ORDER BY RAND()  LIMIT 1000000")
dim(general_query)

#generating a "duplicate" variable and standardising companynames
general_query$dup <- ifelse(duplicated(general_query$general_id), 1, 0)
general_query$companyname <- str_to_lower(general_query$companyname)
#companies_names_dataframe$companyname <- gsub(",|;|.","",companies_names_dataframe$companyname)
general_query$companyname <- str_trim(general_query$companyname)
general_query$companyname <- gsub(" ","_",general_query$companyname)
general_query$notgood <- ifelse(general_query$companyname=="",1,0)
general_query <- general_query[general_query$notgood != 1 , ]
str(general_query)

### merge the newly extracted dataset with two variable indicating if the companyname has been flagged as an agency (filteredout) or identified as a company (keep) in the code "check_company_names"

# "filtereout" variable flagging agencies

filteredout_m <- as.data.frame(table(filteredout$companyname))
filteredout_m$Freq <- 1
colnames(filteredout_m) <- c("companyname", "filteredout")

# "keep" variable identifying companynames that have been identified as belonging to a company

keep_m <- companies_names_dataframe[companies_names_dataframe$Freq>109 , ]
keep_m <- as.data.frame(table(keep_m$companyname))
keep_m$Freq <- 1
colnames(keep_m) <- c("companyname", "keep")

# merge new dataset with "keep" and "filteredout" variables

DF <- merge(general_query, filteredout_m, all.x=TRUE)
DF <- merge(DF, keep_m, all.x=TRUE)
dim(DF)


### generating summary statistics at the companyname level and coding a unique variable (filteredout) identifying agencies and companies


# generate summary stats by companyname

DF <- group_by(DF , companyname)
sumstats_by_company <- summarise(DF , tot_n=n(), nd_esco4=n_distinct(idesco_level_4), nd_esco3=n_distinct(idesco_level_3), tot_dups=sum(dup), nd_city=n_distinct(idcity), m_city=sum(idcity==""&dup==0), nd_prov=n_distinct(idprovince), m_prov=sum(idprovince==""), nd_sect=n_distinct(idcategory_sector), nd_grab=n_distinct(grab_date),  filteredout=median(filteredout), keep=median(keep))
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
sumstats_by_company$ln_esco3 <- log(sumstats_by_company$nd_esco3)
sumstats_by_company$ln_n <- log(sumstats_by_company$tot_n)
sumstats_by_company$ln_undup_n <- log(sumstats_by_company$tot_n - sumstats_by_company$tot_dups)

# plot
plotdata <- sumstats_by_company[sumstats_by_company$tot_n>100 & sumstats_by_company$filteredout != -1, ]
ggplot(data = plotdata) + 
  geom_point(mapping = aes(x = ln_undup_n, y = ln_esco3, colour=filteredout))







