# OJA_HHI

R code for calculating a Labour Market Concentration Index using the [Herfindahl-Hirschmann Index  (HHI)](https://en.wikipedia.org/wiki/Herfindahl%E2%80%93Hirschman_Index) from Online Job Advertisements (OJA).

The main code is contained in the script _"lmci_v1.R"_

The main functions used in the code are declared in the script _"hhi_functions.R"_

The main R script takes as inputs two .csv files:
1. [_companies_to_clean_EU_](https://github.com/eurostat/oja_hhi/blob/main/companies_to_clean_EU.csv) to clean the company names of the original OJA dataset
2. [_staff_agencies_EU_](https://github.com/eurostat/oja_hhi/blob/main/staff_agencies_EU.csv) to filter out staffing agencies (i.e. where the variable _companyname_ reports the name of the staffing agency instead of the name of the company that has the actual job post adverstised)
