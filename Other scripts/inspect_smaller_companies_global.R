

### this code generates a subsample of companies with between 20 and 100 ads in the 1 million job ads sample extracted in the code filter_out_agencies_country.R. this facilitates inspection of this subsample. in addition, users can use this script to manually code the companynames in this subsample as belonging to agencies or companies. 
# please note that this code starts by running the R code consolidate_company_names_country.R, which in turn sources the code filter_out_agencies_country.R. you must select in filter_out_agencies_country.R the country for which you want to run this one. in addition, you need to make sure that the csv files expected as input in both filter_out_agencies_country.R and consolidate_company_names_country.R are correctly formatted and stored.
# this code has two outputs:
# - a csv file (subsample_companynames.csv) with a list of 5 randomly selected companies per number of ads in the sample (e.g. 5 company names with 99 ads, 5 companies with 98 ads, etc.). an extra column is added next to each companyname so that a user can mark which are ones are companies and which ones are agencies
# - a list of companies and agencies (that the user can provide thorugh the input described just below) in a character object format that can be easily used in other R codes
# this code requires one input to be provided by the user:
# - a csv (subsample_companynames_import_COUNTRY.csv, where COUNTRY must be replaced with the actual country code) file with identical structure as the output subsample_companynames.csv, but where the user may have manually included, next to each companyname, a value (0 if company, 1 if agency; other values can be used for other categories like secondary job portal, if deemed interesting). 
# - two character objects (add_filteredout and add_keep) containing list of companynames that have been identified by the user as belonging to companies or agencies
# note that, if the goal is only to extract a subsample of companies without the intention to code them as companies or agencies, the user does not need to provide any actual input: it is enough to rename the output file subsample_companynames.csv as subsample_companynames_import_COUNTRY.csv


### sourcing the script consolidate_company_names_country.R
source("Other scripts/consolidate_company_names_country.R")


### generate a sample of companies between 20 and 99 ads

#generate 5 subsamples with one company per level of ads frequency. note that company names have been selected in random order in the script filter_out_agencies_country.R, which underpins this script 
subsample <- companies_names_dataframe_original[companies_names_dataframe_original$Freq<100 & companies_names_dataframe_original$Freq>19 , ]
subsample1 <- subsample[duplicated(subsample$Freq)==FALSE , ]
colnames(subsample1) <- c("companyname1", "Freq")
subsample1$if_agency1 <- NA
subsample <- subsample[duplicated(subsample$Freq)==TRUE , ]
subsample2 <- subsample[duplicated(subsample$Freq)==FALSE , ]
colnames(subsample2) <- c("companyname2", "Freq")
subsample2$if_agency2 <- NA
subsample <- subsample[duplicated(subsample$Freq)==TRUE , ]
subsample3 <- subsample[duplicated(subsample$Freq)==FALSE , ]
colnames(subsample3) <- c("companyname3", "Freq")
subsample3$if_agency3 <- NA
subsample <- subsample[duplicated(subsample$Freq)==TRUE , ]
subsample4 <- subsample[duplicated(subsample$Freq)==FALSE , ]
colnames(subsample4) <- c("companyname4", "Freq")
subsample4$if_agency4 <- NA
subsample <- subsample[duplicated(subsample$Freq)==TRUE , ]
subsample5 <- subsample[duplicated(subsample$Freq)==FALSE , ]
colnames(subsample5) <- c("companyname5", "Freq")
subsample5$if_agency5 <- NA

#merge the 5 subsamples and write the result as a csv file ready for export
subsample <- merge(subsample1 , subsample2, all.x=TRUE)
subsample <- merge(subsample , subsample3, all.x=TRUE)
subsample <- merge(subsample , subsample4, all.x=TRUE)
subsample <- merge(subsample , subsample5, all.x=TRUE)
subsample <- arrange(subsample, desc(Freq))
write.csv2(subsample, "Data/subsample_companynames.csv")

# -> manual editing (or at least renaming) of the exported file is necessary at this point
# -> re-import the file after manual input or renaming
subsample_import <- paste0( "Data/subsample_companynames_import_" , country , ".csv")
evaluation_sample <- read.csv(subsample_import , sep = csv_separator)

#generate vectors of companynames identified as agencies or companies
evaluation_filteredout <-  c(na.omit(evaluation_sample$companyname1[evaluation_sample$if_agency1==1|evaluation_sample$if_agency1==2]) , na.omit(evaluation_sample$companyname2[evaluation_sample$if_agency2==1|evaluation_sample$if_agency2==2]) , na.omit(evaluation_sample$companyname3[evaluation_sample$if_agency3==1|evaluation_sample$if_agency3==2]) , na.omit(evaluation_sample$companyname4[evaluation_sample$if_agency4==1|evaluation_sample$if_agency4==2]) , na.omit(evaluation_sample$companyname5[evaluation_sample$if_agency5==1|evaluation_sample$if_agency5==2]) )
evaluation_keep <-  c(na.omit(evaluation_sample$companyname1[evaluation_sample$if_agency1==0]) , na.omit(evaluation_sample$companyname2[evaluation_sample$if_agency2==0]) , na.omit(evaluation_sample$companyname3[evaluation_sample$if_agency3==0]) , na.omit(evaluation_sample$companyname4[evaluation_sample$if_agency4==0]) , na.omit(evaluation_sample$companyname5[evaluation_sample$if_agency5==0]) )
str(evaluation_filteredout)
str(evaluation_keep)

#prepare them for an easy merge
evaluation_filteredout_m <- as.data.frame(evaluation_filteredout)
evaluation_filteredout_m$agency <- 1
colnames(evaluation_filteredout_m) <- c("companyname" , "agency")
evaluation_keep_m <- as.data.frame(evaluation_keep)
evaluation_keep_m$actualemployer <- 1
colnames(evaluation_keep_m) <- c("companyname" , "actualemployer")
str(evaluation_filteredout_m)
str(evaluation_keep_m)







