

### this code helps consolidating company names, i.e. replacing similar names belonging to the same company with the same string. 
# please note that this code starts by running the R code filter_out_agencies_country.R. you must select in that code the country for which you want to run this one. in addition, you need to make sure that the csv file expected as input in filter_out_agencies_country.R is correctly formatted and stored.


### required input for this code
#in addition to the input required by the code filter_out_agencies_country.R, you will need to save in the working directory a csv file with the name companies_to_clean_import_COUNTRY.csv (where COUNTRY must be replaced by the country code indicated at the beginning of filter_out_agencies_country.R)
# the file companies_to_clean_import_COUNTRY.csv must have one row per companyname that you want to consolidate. it must have 5 columns:
#1. "oja_companyname" contains background information that supports you in filling in the other columns. it is not used by this code
#2. "replace_with" contains the new master name that you want to use to indicate the company for which you are doing the consolidation. for example, you may write "amazon" in this column if the company names you are consolidating are "amazon_group", "amazon_services" and "amazoncom". it is important that every master name in this column is included ONLY ONCE, otherwise the output file names_replaced_list.csv (see below) will fail to record accurately the replacements that have been executed
#3. "keywords" contains the keywords you want to search through a str_detect function and replace with the new master name you have chosen. you can include here multiple keywords separated by the "|" sign (e.g. "generali_italia|assicurazioni_generali|generali_group" all refer to a big insurance company). every companyname containing any of the keywords you include here will be replaced with the new master name. it is important that this column is NEVER empty, because otherwise the str_detect function goes crazy searching for empty strings 
#4. "exact" contains the keywords you want to search through an exact match and replace with the new master name you have chosen. you can include here multiple keywords separated by the "|" sign, but usually you will have only one (this column can be useful for keywords that are the name of a company but are quite general and can be found also inside the name of other companies, like "generali" in Italy). every companyname containing any of the keywords you include here will be replaced with the new master name. 
#5. "butnot" contains keywords you use to identify companynames that you DON'T want to replace with the master name. these are companynames that may contain soome keywords you included in the "keywords" column, but do not belong to the company you trying to consolidate. for example, you may include here "riparazioni_generali_group" to avoid that a repair shop with that name is not confused with the generali insurance group.


### this code produces two outputs that support you in the compilation of the companies_to_clean_import_COUNTRY.csv:
# - companies_to_clean_export.csv is a list of the company names with the largest number of advertisement in the country you are working on, after filtering out agency names. this provides you an initial list of companies that you can work on
# - names_replaced_list.csv shows you what has been replaced with the master names you included in the "replace_with" column of your input file. names_replaced_list.csv is very useful to guide you in the choice of the keywords you include in companies_to_clean_import_COUNTRY.csv


### running the code which extracts the sample of companies and applies the agency filter

# sourcing the code - please note that the country local is defined in the source code, and that a file called staff_agencies_COUNTRY.csv must be provided as input
source("Other scripts/filter_out_agencies_country.R")

# checking that the list of companies (cleaned of agencies) is there and backing it up
str(companies_names_dataframe)
companies_names_dataframe_backup <- companies_names_dataframe
#if you need to restore the original list of companies, you can run: 
#companies_names_dataframe <- companies_names_dataframe_backup


### exporting and importing files for the manual extraction and input of keywords

# exporting a file with all entries with >99 unduplicated jobs ads in the sample with 1 million job ads. starting from this file, a new file needs to be be manually generated with the keywords used in the companyname consolidation process (see explanation at the beginning of this code)
companies_to_clean <- companies_names_dataframe[companies_names_dataframe$Freq>99 , -3]
write.csv(companies_to_clean[ , 1] , "Other scripts/companies_to_clean_export.csv")

# reading the keywords for data cleaning from the file companies_to_clean_import_COUNTRY.csv
# NB if it fails to refresh the file, use a slightly different file name
companies_to_clean_import <- paste0( "Other scripts/companies_to_clean_import_" , country , ".csv")
clean_names <- read.csv(companies_to_clean_import , sep = ";" , colClasses = "character")
head(clean_names)


### consolidating company names
# three conditions are applied to consolidate company names (see also explanation of input files at the beginning of this code): 
# (1) every companyname that includes some text patterns is replaced with the consolidated (master) name
# (2) some company names that match exactly a certain text pattern are replaced with the master name
# (3) an exception is made for some company names that should be replaced according to criterion (1)

# generate a function to record all the company names that are replaced, so it is possible to export it (names_replaced_list.csv) and check the accuracy of the replacement rules defined by the file companies_to_clean_import_COUNTRY.csv
# this function identifies all replacements that are made for one row of the input file companies_to_clean_import_COUNTRY.csv based on its 5-column structure, which is illustrated at the beginning of this code.
# the output of this function (i.e., the character object called "temp") is a string containing all company names that have been replaced with a master name, separated by ";"
names_replaced <- function(i) {
  temp <- companies_names_dataframe$companyname[str_detect(companies_names_dataframe$companyname, clean_names[i,3]) ==TRUE | companies_names_dataframe$companyname==clean_names[i,4] ]
  temp <- temp[temp!=clean_names[i,5]]
  temp <- paste(temp, collapse=" ; ")
  return(temp)
}

# run this function for all master names included in the column "replace_with" of the file companies_to_clean_import_COUNTRY.csv (which, when imported, becomes the object clean_names that is used in the following lines of command)
names_replaced_list <- apply(as.matrix(1:dim(clean_names)[1]),1,names_replaced)
str(names_replaced_list)

# export the file names_replaced_list.csv (see a description of this file at the beginning of this code). this file can be used to iteratively improve the list of keywords in companies_to_clean_import_COUNTRY.csv
write.csv2(names_replaced_list,"Other scripts/names_replaced_list.csv")

# run a loop to consolidate company names according to the previous rules and the input keywords found in the csv file
# this loop executes the replacements that have been recorded in the file names_replaced_list.csv 
for(i in 1:dim(clean_names)[1]) {
  #cleaning the company name
  companies_names_dataframe$companyname[str_detect(companies_names_dataframe$companyname, clean_names[i,3]) == TRUE & companies_names_dataframe$companyname!=clean_names[i,5] ] <- clean_names[i,2]
  companies_names_dataframe$companyname[companies_names_dataframe$companyname == clean_names[i,4] ] <- clean_names[i,2]
}


### consolidating the table with new companynames

# put together all the company names that have been replaced with the same master name, so that each master name is listed once and the number of ads referring to it is correctly reported
companies_names_dataframe <- group_by(companies_names_dataframe,companyname)
companies_names_dataframe <- summarise(companies_names_dataframe, Freq=sum(Freq))
companies_names_dataframe <- arrange(companies_names_dataframe , desc(Freq))
str(companies_names_dataframe)


