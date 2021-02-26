

### running the code which extracts the sample of companies and applies the agency filter

# sourcing the code
source("Other scripts/filter_out_agencies_country.R")
# checking that the list of companies (cleaned of agencies) is there and backing it up
str(companies_names_dataframe)
companies_names_dataframe_backup <- companies_names_dataframe
# companies_names_dataframe <- companies_names_dataframe_backup


### exporting and importing files for the manual extraction of keywords

#exporting a file with all entries with >99 jobs ads in the sample. starting from this file, a new file will be manually generated with the keywords used in the companyname consolidation process
companies_to_clean <- companies_names_dataframe[companies_names_dataframe$Freq>99 , -3]
write.csv(companies_to_clean[ , 1] , "companies_to_clean_export.csv")
# reading the keywords for data cleaning from imported files
# NB if it fails to refresh the file, use a slightly different file name
clean_names <- read.csv("Other scripts/companies_to_clean_import.csv" , sep = ";")
#  clean_names <- read.csv("companies_to_clean.csv" , sep = ";")
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
write.csv2(names_replaced_list,"names_replaced_list.csv")

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











### generate a table with cumulative distributions

# generate a frequency table from the list of companies
companies_freqtable_clean <- as.data.frame(table(companies_names_dataframe$Freq))
colnames(companies_freqtable_clean) <- c("ads_per_company" , "n_companies")
companies_freqtable_clean <- arrange(companies_freqtable_clean,desc(ads_per_company))
# ensuring that the variables of this frequency table are numeric. NB: as.numeric does not work well for the variable ads_per_company, so I have to get the numeric value in a different way (through a merge)
companies_names_dataframe_m <- companies_names_dataframe[duplicated(companies_names_dataframe$ads_per_company)==FALSE , ]
companies_freqtable_clean <- merge(companies_freqtable_clean , companies_names_dataframe_m , all.x=TRUE)
companies_freqtable_clean <- arrange(companies_freqtable_clean , desc(ads_per_company))
companies_freqtable_clean$n_companies <- as.numeric(companies_freqtable_clean$n_companies)
# generate variables representing cumulative distributions
companies_freqtable_clean$tot_ads <- companies_freqtable_clean$n_companies * companies_freqtable_clean$ads_per_company
companies_freqtable_clean$cum_prop_ads <- 100 * cumsum(companies_freqtable_clean$tot_ads) / sum(companies_freqtable_clean$tot_ads)
companies_freqtable_clean$cum_prop_companies <- 100 * cumsum(companies_freqtable_clean$n_companies) / sum(companies_freqtable_clean$n_companies)
companies_freqtable_clean$cum_n_companies <- cumsum(companies_freqtable_clean$n_companies)
head(companies_freqtable_clean)


### output

# cumulative distributions
head(companies_freqtable_clean)
# consolidated list of companies
head(companies_names_dataframe)
# for comparison: pre-consolidation list of companies
head(companies_names_dataframe_2)


