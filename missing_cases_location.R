

source("hhi_functions.R")
library(openxlsx)
library(readxl)
fua <- createfua()

#libraries and data connection
#install.packages("wihoja")
library(wihoja)
open_oja_db()
#install.packages("tidyverse")
library(tidyverse)


#query and de-duplicate
missregion_query <- query_athena("SELECT general_id, idcity, idprovince FROM estat_dsl2531b_oja.ft_document_en_v8 WHERE year_grab_date=2019 ORDER BY RAND()  LIMIT 1000000")
missregion_query$dup <- ifelse(duplicated(missregion_query$general_id), 1, 0)
missregion_query <- missregion_query[missregion_query$dup==0]
dim(missregion_query)

cities <- as.data.frame(table(missregion_query$idcity))
dim(cities)
totalmissing <- 100*cities$Freq[cities$Var1==""]/sum(cities$Freq)
totalmissing

fua_m <- select(fua, idprovince, var1)
fua_m <- fua_m[duplicated(fua_m$idprovince)==FALSE,]

DF <- merge(missregion_query, fua_m, all.x=TRUE)

DF$idcity[DF$var1==1] <- 999

cities_withimputation <- as.data.frame(table(DF$idcity))
dim(cities_withimputation)
totalmissing_withimputation <- 100*cities_withimputation$Freq[cities_withimputation$Var1==""]/sum(cities_withimputation$Freq)
totalmissing_withimputation
