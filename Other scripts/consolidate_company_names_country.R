


### running the code which extracts the sample of companies and applies the agency filter

# sourcing the code
source("Other scripts/filter_out_agencies_country.R")

# please note that the country local is defined in the source code
#country <- "IT"

# checking that the list of companies (cleaned of agencies) is there and backing it up
str(companies_names_dataframe)
companies_names_dataframe_backup <- companies_names_dataframe
# companies_names_dataframe <- companies_names_dataframe_backup


### exporting and importing files for the manual extraction of keywords

#exporting a file with all entries with >99 jobs ads in the sample. starting from this file, a new file will be manually generated with the keywords used in the companyname consolidation process
companies_to_clean <- companies_names_dataframe[companies_names_dataframe$Freq>99 , -3]
write.csv(companies_to_clean[ , 1] , "Other scripts/companies_to_clean_export.csv")
# reading the keywords for data cleaning from imported files
# NB if it fails to refresh the file, use a slightly different file name
companies_to_clean_import <- paste0( "Other scripts/companies_to_clean_import_" , country , ".csv")
clean_names <- read.csv(companies_to_clean_import , sep = ";")
head(clean_names)


### consolidating company names
# three conditions are applied to consolidate company names: 
# (1) every companyname that includes some text patterns is replaced with the consolidated name
# (2) some company names that match exactly a certain text pattern are replaced with the consolidated name
# (3) an exception is made for some company names that should be replaced according to criterion (1)

# generate a function to record all the company names that are replaced, so it is possible to check for wrong rules
names_replaced <- function(i) {
  temp <- companies_names_dataframe$companyname[str_detect(companies_names_dataframe$companyname, clean_names[i,3]) ==TRUE | companies_names_dataframe$companyname==clean_names[i,4] ]
  temp <- temp[temp!=clean_names[i,5]]
  temp <- paste(temp, collapse=" ; ")
  return(temp)
}
# run this function for all consolidated companynames and export the results
names_replaced_list <- apply(as.matrix(1:dim(clean_names)[1]),1,names_replaced)
str(names_replaced_list)
write.csv2(names_replaced_list,"Other scripts/names_replaced_list.csv")

#temp <- companies_names_dataframe$companyname[str_detect(companies_names_dataframe$companyname, clean_names[2,3]) ==TRUE | companies_names_dataframe$companyname==clean_names[2,4] ]
#View(temp)

# run a loop to consolidate company names according to the previous rules and the input keywords found in the csv file
for(i in 1:dim(clean_names)[1]) {
  #cleaning the company name
  companies_names_dataframe$companyname[str_detect(companies_names_dataframe$companyname, clean_names[i,3]) == TRUE & companies_names_dataframe$companyname!=clean_names[i,5] ] <- clean_names[i,2]
  companies_names_dataframe$companyname[companies_names_dataframe$companyname == clean_names[i,4] ] <- clean_names[i,2]
}

#consolidating the table with new companynames
companies_names_dataframe <- group_by(companies_names_dataframe,companyname)
companies_names_dataframe <- summarise(companies_names_dataframe, Freq=sum(Freq))
companies_names_dataframe <- arrange(companies_names_dataframe , desc(Freq))
str(companies_names_dataframe)


### generate a sample of companies between 20 and 99 ads

#generate 5 subsample with one company per level of ads frequency
subsample <- companies_names_dataframe[companies_names_dataframe$Freq<100 & companies_names_dataframe$Freq>19 , ]
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
write.csv2(subsample, "Other scripts/subsample_companynames.csv")

# -> manual input on the exported file is necessary at this point
#re-import the file after manual input
subsample_import <- paste0( "Other scripts/subsample_companynames_import_" , country , ".csv")
add_clean_names <- read.csv(subsample_import , sep = ";")

#generate vectors of companynames identified as agencies or companies
add_filteredout <-  c(na.omit(add_clean_names$companyname1[add_clean_names$if_agency1==1]) , na.omit(add_clean_names$companyname2[add_clean_names$if_agency2==1]) , na.omit(add_clean_names$companyname3[add_clean_names$if_agency3==1]) , na.omit(add_clean_names$companyname4[add_clean_names$if_agency4==1]) , na.omit(add_clean_names$companyname5[add_clean_names$if_agency5==1]) )
add_keep <-  c(na.omit(add_clean_names$companyname1[add_clean_names$if_agency1==0]) , na.omit(add_clean_names$companyname2[add_clean_names$if_agency2==0]) , na.omit(add_clean_names$companyname3[add_clean_names$if_agency3==0]) , na.omit(add_clean_names$companyname4[add_clean_names$if_agency4==0]) , na.omit(add_clean_names$companyname5[add_clean_names$if_agency5==0]) )
str(add_filteredout)
str(add_keep)




