# OJA_HHI

R code for calculating the Herfindahl-Hirschmann Index (HHI) from Online Job Advertisements (OJA).

The main code is contained in the script _"lmci_v1.R"_

The main functions used in the code are declared in the script _"hhi_functions.R"_

The main R script reads two .csv files for cleaning the company names of the original OJA dataset and for filtering out staffing agencies (i.e. where the variable _companyname_ reports the name of the staffing agency instead of the name of the company that has the actual job post adverstised)
