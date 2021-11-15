# _oja_hhi_/ Calculating EU Labour Markets Concentration Index using Online Job Advertisement data

R code for calculating a Labour Market Concentration Index using the [Herfindahl-Hirschmann Index  (HHI)](https://en.wikipedia.org/wiki/Herfindahl%E2%80%93Hirschman_Index) from Online Job Advertisements data (OJA).

## Background

The code is based on the work within the [ESSnet Big Data II project on Online Job Vacancies](https://github.com/OnlineJobVacanciesESSnetBigData/Labour-market-concentration-index-from-CEDEFOP-data). 


This R code is used to produce the experimental results described in the [Statistical Working Paper - Competition in Urban Hiring Markets: Evidence from Online Job Advertisements](https://ec.europa.eu/eurostat/publications/statistical-working-papers). The visualizations produced with ggplot are not the same used in the paper, which are reformatted to respect the graphical standards of Eurostat's publications. 

## Structure

The main code is contained in the script [_lmci_v1.R_](https://github.com/eurostat/oja_hhi/blob/main/lmci_v1.R) 
The execution of the code is parallelized to reduce the time needed to process the data for all the 27 countries. 

The main functions used in the code are declared in the script [_hhi_functions.R_](https://github.com/eurostat/oja_hhi/blob/main/hhi_functions.R)

The folder [_Other script_](https://github.com/eurostat/oja_hhi/tree/main/Other%20scripts) contains mainly scripts and data used for the creation and evaluation of the model dealing with company names.

## OJA Data
The data used in this study consist of 116 851 363 distinct online jobs ads collected from 316 distinct sources in all EU countries. OJAs refer to advertisements published on the World Wide Web revealing an employer’s interest in recruiting workers with certain characteristics for performing certain work. Employers can publish job ads for various reasons, for example to fill a current vacancy or to explore potential recruiting opportunities. 

The OJAs used in this study are available thanks to the cooperation between Eurostat (representing the ESS) and the European Centre for the Development of Vocational Training (CEDEFOP) and their [formal agreement for a joint approach to online job advertisement data](https://www.cedefop.europa.eu/en/news/cedefop-and-eurostat-formalise-joint-approach-online-job-advertisement-data)

The timeliness of the data release has been improving over time, starting with a lag of 7 months between collection and data release, which has been reduced to the current lag of 2 months. The results presented in this document are based on the most recent version of the dataset (i.e. v9) released during the first quarter of 2021. This dataset contains data from the third quarter of 2018 to the end of 2020. However, only data from 2019 has been used in this study, because of its better coverage of important sources.
Access to OJA data can be granted to interested users on a case-by-case basis, following a formal request. Please send your enquiries to ESTAT-WIH@ec.europa.eu.

### Other Inputs

The main R script takes as inputs two .csv files:
1. [_companies_to_clean_EU_](https://github.com/eurostat/oja_hhi/blob/main/companies_to_clean_EU.csv) to clean the company names of the original OJA dataset
2. [_staff_agencies_EU_](https://github.com/eurostat/oja_hhi/blob/main/staff_agencies_EU.csv) to filter out staffing agencies (i.e. where the variable _companyname_ reports the name of the staffing agency instead of the name of the company that has the actual job post adverstised)


## Workflow

![image](https://user-images.githubusercontent.com/57686282/141798897-d3c2c501-b43a-44a3-b09f-a81ee07a597d.png)


