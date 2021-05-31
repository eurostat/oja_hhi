# oja_hhi/ Calculating EU Labour Markets Concentration Index using Online Job Advertisement data

R code for calculating a Labour Market Concentration Index using the [Herfindahl-Hirschmann Index  (HHI)](https://en.wikipedia.org/wiki/Herfindahl%E2%80%93Hirschman_Index) from Online Job Advertisements data (OJA).

This code is part of the methodology used to produce of one of [Eurostat's experimental statistics](https://ec.europa.eu/eurostat/web/experimental-statistics).

The main code is contained in the script [_lmci_v1.R_](https://github.com/eurostat/oja_hhi/blob/main/lmci_v1.R) 

The main functions used in the code are declared in the script [_hhi_functions.R_](https://github.com/eurostat/oja_hhi/blob/main/hhi_functions.R)

The main R script takes as inputs two .csv files:
1. [_companies_to_clean_EU_](https://github.com/eurostat/oja_hhi/blob/main/companies_to_clean_EU.csv) to clean the company names of the original OJA dataset
2. [_staff_agencies_EU_](https://github.com/eurostat/oja_hhi/blob/main/staff_agencies_EU.csv) to filter out staffing agencies (i.e. where the variable _companyname_ reports the name of the staffing agency instead of the name of the company that has the actual job post adverstised)
