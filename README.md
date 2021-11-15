# _oja_hhi_/ Calculating EU Labour Markets Concentration Index using Online Job Advertisement data

R code for calculating a Labour Market Concentration Index using the [Herfindahl-Hirschmann Index  (HHI)](https://en.wikipedia.org/wiki/Herfindahl%E2%80%93Hirschman_Index) from Online Job Advertisements data (OJA).

This R code is used to produce the experimental results described in the [Statistical Working Paper - Competition in Urban Hiring Markets: Evidence from Online Job Advertisements](https://ec.europa.eu/eurostat/publications/statistical-working-papers).

The main code is contained in the script [_lmci_v1.R_](https://github.com/eurostat/oja_hhi/blob/main/lmci_v1.R) 

The main functions used in the code are declared in the script [_hhi_functions.R_](https://github.com/eurostat/oja_hhi/blob/main/hhi_functions.R)

The main R script takes as inputs two .csv files:
1. [_companies_to_clean_EU_](https://github.com/eurostat/oja_hhi/blob/main/companies_to_clean_EU.csv) to clean the company names of the original OJA dataset
2. [_staff_agencies_EU_](https://github.com/eurostat/oja_hhi/blob/main/staff_agencies_EU.csv) to filter out staffing agencies (i.e. where the variable _companyname_ reports the name of the staffing agency instead of the name of the company that has the actual job post adverstised)

The folder [_Other script_](https://github.com/eurostat/oja_hhi/tree/main/Other%20scripts) contains mainly scripts and data used for the creation and evaluation of the model dealing with company names.
