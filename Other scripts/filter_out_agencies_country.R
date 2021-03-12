


### please insert below the country for which you want to run this code (2-letter isocode)

country <- "RO"


### this code helps with the development a list of recruiting agencies that can be removed from the OJA dataset when the analysis focuses on actual employers. it is divided in 4 main parts:
#1 -> a query for a data sample is launched and some standardisation operations are executed on the data
#2 -> a csv containing a list of all companynames with at least 100 observations is produced. this helps understanding which agencies are in the sample
#3 -> based on a csv file (filled with keywords) provided by the user and properly formatted and saved, some observations are filtered out because they are considered agencies (and stored in a matrix called "filteredout"), and some others are kept in the sample of actual employers (and stored in a matrix called "companies_names_dataframe".
#4 -> summary statistics (e.g. number of ads by companyname and cumulative frequencies) are calculated for the sample of actual employers 
#5 -> quick ways to visualise the information in the data are suggested


### input needed for this code to work: 
# a csv file must be prepared with lists of keywords. it must be called staff_agencies_COUNTRY.csv (where COUNTRY must be replaced with the actual country code) and saved in the working directory. 
# each row of the file contains a keyword to search among companynames in the OJA data sample. 
# the csv file must contain 4 columns: 
#1. "Language" indicating the language of the keyword 
#2. "staff_agency_keyword" which contains the keyword to be searched for within the companyname. it is important that this column is NEVER empty, because otherwise the str_detect function goes crazy searching for empty strings
#3. "staff_agency_site" which contains background information (and is not used by this R code)
#4. "exact" which contains information on whether an exact match must be searched for the keyword (in this case the word "exact" must be written in the column "exact" in the appropriate row).


### load libraries and data connection

#install.packages("wihoja")
library(wihoja)
open_oja_db()
#install.packages("tidyverse")
library(tidyverse)
source("hhi_functions.R")


### creating and exporting a table with company names' frequency, sorted by frequency or company name

# query and deduplication
query <- paste0("SELECT companyname, general_id FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE idcountry='",country,"' ORDER BY RAND()  LIMIT 1000000")
companies_names_query <- query_athena(query)
companies_names_query$dup <- ifelse(duplicated(companies_names_query$general_id), 1, 0)
companies_names_query <- companies_names_query[companies_names_query$dup==0]

# background checks
table(companies_names_query$dup)
dim(companies_names_query)

# creating a table with company names' frequency, sorted by frequency or company name
companies_names_dataframe <- as.data.frame(table(companies_names_query$companyname))
colnames(companies_names_dataframe) <- c("companyname","Freq")
companies_names_dataframe <- arrange(companies_names_dataframe , desc(Freq))
companies_names_dataframe_bynames <- arrange(companies_names_dataframe , companyname)
str(companies_names_dataframe)

# doing some standardisation of company names
companies_names_dataframe$companyname <- str_trim(companies_names_dataframe$companyname)
companies_names_dataframe$companyname <- gsub(" ","_",companies_names_dataframe$companyname)
companies_names_dataframe$companyname <- gsub("é","e",companies_names_dataframe$companyname)
temp <- as.character(companies_names_dataframe$companyname)
ordered <- as.data.frame(sapply(temp, function(x) sep(x)))
companies_names_dataframe <- cbind(ordered , companies_names_dataframe$Freq)
colnames(companies_names_dataframe) <- c("companyname" , "Freq")

# dropping empty entries in the companyname variable
companies_names_dataframe$notgood <- ifelse(companies_names_dataframe$companyname=="",1,0)
companies_names_dataframe <- companies_names_dataframe[companies_names_dataframe$notgood != 1 , -3]
str(companies_names_dataframe)

# export list of companynames with at least 100 ads
write.csv2(companies_names_dataframe[companynames_names_dataframe$Freq>99,], "Data/companies_list_atleast100ads.csv")


### applying the job agency filter

# importing the file with job agency filter keywords
filecsv <- paste0("Data/staff_agencies_" , country , ".csv")
staff_agencies <- read.csv(filecsv , sep = ";" , colClasses = "character")

# defining the lists of keywords to filter job agencies
blacklist <- staff_agencies[staff_agencies$exact != "exact" , 2]
blacklist_exact <- staff_agencies[staff_agencies$exact == "exact" , 2]
length(blacklist)

# applying the filter based on the lists of keywords and generating a "filteredout" matrix with agency names and their frequency
# the filter is applied through two loops that look for generic keywords through a str_detect function, and then for an exact match for those keywords for which the "exact" option was indicated in the csv file. each loop has two lines of command, one for adding filtered companynames to the matrix filteredout, and one for deleting them from the matrix companies_names_dataframe. if the loops will get too slow in the future, they can be replaced with the lines of command that are hash-tagged just below. 
filteredout <- cbind.data.frame(0,0)[-1,]
colnames(filteredout) <- c("companyname" , "Freq")
for(i in 1:length(blacklist)) {
  filteredout <- rbind(filteredout , filter(companies_names_dataframe, str_detect(companies_names_dataframe$companyname, blacklist[i]) ) )
  companies_names_dataframe <- filter(companies_names_dataframe, str_detect(companies_names_dataframe$companyname, blacklist[i] , negate = TRUE))
}
for(i in 1:length(blacklist_exact)) {
  filteredout <- rbind(filteredout, filter(companies_names_dataframe, blacklist_exact[i] == companies_names_dataframe$companyname) )
  companies_names_dataframe <- filter(companies_names_dataframe, blacklist_exact[i] != companies_names_dataframe$companyname)
}
filteredout <- arrange(filteredout , desc(Freq))
#the following commands would be equivalent to the previous loops
#filteredout <- filter(companies_names_dataframe, str_detect(companies_names_dataframe$companyname, paste(blacklist, collapse = '|')) | sub(paste(blacklist_exact, collapse = '|'),"",companies_names_dataframe$companyname) == "" )
#companies_names_dataframe <- mutate(companies_names_dataframe, companyname = replace(companyname, str_detect(companies_names_dataframe$companyname, paste(blacklist, collapse = '|')) | sub(paste(blacklist_exact, collapse = '|'),"",companies_names_dataframe$companyname) == "", NA))
#companies_names_dataframe <- companies_names_dataframe[!is.na(companies_names_dataframe$companyname) , ]

# background checks:
dim(filteredout)
dim(companies_names_dataframe)


### generating a table of summary statistics for the actual employers - number of companies having x ads, cumulative frequencies

# generate a frequency table from the list of companies
companies_freqtable <- as.data.frame(table(companies_names_dataframe$Freq))
colnames(companies_freqtable) <- c("ads_per_company" , "n_companies")

# ensuring that the variables of this table are numeric. NB: as.numeric does not work well for the variable ads_per_company, so I have to get the numeric value in a different way (through a merge)
companies_names_dataframe$ads_per_company <- as.factor(companies_names_dataframe$Freq)
companies_freqtable <- merge(companies_freqtable , companies_names_dataframe[duplicated(companies_names_dataframe$ads_per_company) == FALSE , -1])[ , -1]
colnames(companies_freqtable) <- c("n_companies" , "ads_per_company")
companies_freqtable$n_companies <- as.numeric(companies_freqtable$n_companies)

# calculating the cumulative number of ads for the x biggest company names
companies_freqtable <- arrange(companies_freqtable , desc(ads_per_company))
companies_freqtable$tot_ads <- companies_freqtable$n_companies * companies_freqtable$ads_per_company
companies_freqtable$cum_prop_ads <- 100 * cumsum(companies_freqtable$tot_ads) / sum(companies_freqtable$tot_ads)
companies_freqtable$cum_prop_companies <- 100 * cumsum(companies_freqtable$n_companies) / sum(companies_freqtable$n_companies)
companies_freqtable$cum_n_companies <- cumsum(companies_freqtable$n_companies)

# background checks
str(companies_freqtable)
head(companies_freqtable)


### print and view output

# print output
write.csv(companies_freqtable , "Data/companies_freqtable.csv")
write.csv(companies_names_dataframe , "Data/companies_names_dataframe.csv")
write.csv(filteredout , "Data/filteredout.csv")

# top view of the cumulative distribution of ads and company names
head(companies_freqtable)

# list of company names by number of ads and alphabetical order (top view)
head(companies_names_dataframe)
head(companies_names_dataframe_bynames)

#total number of distinct company names found (with an upper bound due to the limit of obs allowed)
sum(companies_freqtable$n_companies)

#total number of distinct company and of job ads that are filtered out and that remain in the sample
sum(filteredout$Freq)
sum(companies_names_dataframe$Freq)

# top view of the filteredout matrix and full view of the blacklists of keywords used to create it
head(filteredout)
blacklist
blacklist_exact





