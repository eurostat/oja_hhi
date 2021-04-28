

##### 
#with this code, i explore differencies between companynames belonging (and not belonging) to agencies, starting from the list of agencies in the "check_company_names" files 
#####


### sourcing the code "check_company_names"

getwd()
source("hhi_functions.R")
source("Other scripts/inspect_smaller_companies_global.R")



### getting the stats needed for the automatic imputation process

# getting summary stats for OJA vars with gen_sum_stats
sumstats_by_company <-gen_sum_stats(idcountry = country, filterlist = filteredout$companyname, keeplist = clean_names$replace_with)
sumstats_by_company <- arrange(sumstats_by_company, desc(tot_n))
str(sumstats_by_company)

#generate logs
sumstats_by_company$ln_esco3 <- log(sumstats_by_company$idesco_level_3)
sumstats_by_company$ln_undup_n <- log(sumstats_by_company$tot_n - sumstats_by_company$tot_dups)
sumstats_by_company$sqln_undup_n <- sumstats_by_company$ln_undup_n^2
sumstats_by_company$culn_undup_n <- sumstats_by_company$ln_undup_n^3
sumstats_by_company$quln_undup_n <- sumstats_by_company$ln_undup_n^4
sumstats_by_company$ln_n <- log(sumstats_by_company$tot_n)

#more logs
sumstats_by_company$ln_esco4 <- log(sumstats_by_company$idesco_level_4)
sumstats_by_company$sqln_esco4 <- log(sumstats_by_company$idesco_level_4)^2
sumstats_by_company$culn_esco4 <- log(sumstats_by_company$idesco_level_4)^3
sumstats_by_company$quln_esco4 <- log(sumstats_by_company$idesco_level_4)^4
sumstats_by_company$ln_province <- log(sumstats_by_company$idprovince)
sumstats_by_company$ln_city <- log(sumstats_by_company$idcity)
sumstats_by_company$ln_region <- log(sumstats_by_company$idregion)
sumstats_by_company$ln_sector <- log(sumstats_by_company$idsector)
sumstats_by_company$ln_undup_prov <- sumstats_by_company$ln_province * sumstats_by_company$ln_undup_n
sumstats_by_company$sqln_sector <- log(sumstats_by_company$idsector)^2
sumstats_by_company$culn_sector <- log(sumstats_by_company$idsector)^3
sumstats_by_company$quln_sector <- log(sumstats_by_company$idsector)^4
sumstats_by_company$ln_grab <- log(sumstats_by_company$grab_date)
sumstats_by_company$ln_duration <- log(sumstats_by_company$duration)
#sumstats_by_company$sqavg_duration <- sumstats_by_company$avg_duration^2
#sumstats_by_company$cuavg_duration <- sumstats_by_company$avg_duration^3
#sumstats_by_company$quavg_duration <- sumstats_by_company$avg_duration^4
#sumstats_by_company$duration120 <- 0
#sumstats_by_company$duration120[sumstats_by_company$avg_duration==120] <- 1
#sumstats_by_company$durationneg <- 0
#sumstats_by_company$durationneg[sumstats_by_company$avg_duration <= (-120)] <- 1


### create a dataset for the model which includes:
# variables for the manually-coded vectors evaluation_filteredout and evaluation_keep
# all observations with at least 20 ads OR that are anyways included in the vectors evaluation_filteredout and evaluation_keep

# merge sumstats with evaluation_filteredout and evaluation_keep
if (country=="PT") {
  # matching Portuguese lost companynames
  sumstats_by_company$companyname <- gsub("ó","o",sumstats_by_company$companyname)
  sumstats_by_company$companyname <- gsub("ç","c",sumstats_by_company$companyname)
  sumstats_by_company$companyname <- gsub("ã","a",sumstats_by_company$companyname)
  sumstats_by_company$companyname <- gsub("ê","e",sumstats_by_company$companyname)
  sumstats_by_company$companyname <- gsub("á","a",sumstats_by_company$companyname)
  sumstats_by_company$companyname <- gsub("õ","o",sumstats_by_company$companyname)
  sumstats_by_company$companyname <- gsub("í","i",sumstats_by_company$companyname)
  sumstats_by_company$companyname <- gsub("ú","u",sumstats_by_company$companyname)
  filteredout$companyname <- gsub("ó","o",filteredout$companyname)
  filteredout$companyname <- gsub("ç","c",filteredout$companyname)
  filteredout$companyname <- gsub("ã","a",filteredout$companyname)
  filteredout$companyname <- gsub("ê","e",filteredout$companyname)
  filteredout$companyname <- gsub("á","a",filteredout$companyname)
  filteredout$companyname <- gsub("õ","o",filteredout$companyname)
  filteredout$companyname <- gsub("í","i",filteredout$companyname)
  filteredout$companyname <- gsub("ú","u",filteredout$companyname)
}
dataset_model <- merge(sumstats_by_company , evaluation_filteredout_m , all.x = TRUE)
dataset_model <- merge(dataset_model , evaluation_keep_m , all.x = TRUE)
dim(dataset_model)

# generate a keepme variable identifying the observations we want to keep
dataset_model$keepme <- 0
dataset_model$keepme[dataset_model$ln_undup_n>3] <- 1
sum(dataset_model$keepme)
dataset_model$keepme[dataset_model$agency==1] <- 1
sum(dataset_model$keepme)
dataset_model$keepme[dataset_model$actualemployer==1] <- 1
sum(dataset_model$keepme)

# dropping observations for which keepme==0
dataset_model <- dataset_model[dataset_model$keepme==1,]
dim(dataset_model)
#View(sumstats_by_company)
#View(dataset_model)
#View(keep)


### running the automatic imputation model on dataset_model (an alternative, simpler dataset_model can be created by running the hash-tagged line below)

# run and combine the three empirical rules
#dataset_model <- sumstats_by_company[sumstats_by_company$ln_undup_n>3,]
testflag1 <- automflag(xvar2="sqln_undup_n", xvar3="culn_undup_n", xvar4="quln_undup_n", mydata=dataset_model)
testflag2 <- automflag(yvar="ln_n", xvar1="ln_undup_n", xvar2="sqln_undup_n", flag_above=FALSE, flag_below=TRUE, mydata=dataset_model,)
testflag3 <- automflag(yvar="ln_sector", xvar1="ln_province", xvar2="ln_undup_n", xvar3="ln_undup_prov", flag_above=TRUE, flag_below=FALSE, mydata=dataset_model)
automflag_output_combo <- automflag_combine(automflag1= testflag1, automflag2= testflag2, mydata=dataset_model )
automflag_output_combo <- automflag_combine(automflag1= automflag_output_combo, automflag2= testflag3, mydata=dataset_model, )
automflag_output_combo[[2]]
#automflag_output_combo[[5]]

# Wanna compare with a four-rules imputation functions? Run the commands below
#testflag4 <- automflag(yvar="ln_grab", xvar1="ln_esco4", xvar2="sqln_esco4", xvar3="culn_esco4", xvar4="quln_esco4", flag_above=FALSE, flag_below=TRUE)
#automflag_output_combo <- automflag_combine(automflag1= automflag_output_combo, automflag2= testflag4 )
#automflag_output_combo[[2]]


### evaluate

# get output nr. 1 (datacombo) from the automflag fct, which contains the dataset with companynames and flags
datacombo <- automflag_output_combo[[1]]
str(datacombo)


# merge the modelevaluation dataset with the full filteredout list to assign agencies that were identified thanks to the manually-inputted keywords
filteredout_m <- filteredout
filteredout_m$Freq <- 1
colnames(filteredout_m) <- c("companyname","filteredout_m")
filteredout_m <- arrange(filteredout_m,companyname)
modelevaluation <- merge(datacombo , filteredout_m , all.x = TRUE)
modelevaluation$dup <- duplicated(modelevaluation$companyname)
modelevaluation <- modelevaluation[modelevaluation$dup==FALSE,]
str(modelevaluation)


if (country=="PT") {
  # matching Portuguese lost companynames
  modelevaluation$companyname[modelevaluation$companyname=="eqs"]<- "_engenharia_qualidade_e_seguranca eqs_"
  } else if (country=="RO") {
  # matching Romanian lost companynames
  modelevaluation$companyname[modelevaluation$companyname=="accenture"]<- "accenture_technology"
  modelevaluation$companyname[modelevaluation$companyname=="alten"]<- "alten_iasi"
  modelevaluation$companyname[modelevaluation$companyname=="amazon"]<- "amazoncom_inc"
  modelevaluation$companyname[modelevaluation$companyname=="capgemini"]<- "capgemini_romania"
  modelevaluation$companyname[modelevaluation$companyname=="adecco_italia_spa"]<- "adecco_italia"
  modelevaluation$companyname[modelevaluation$companyname=="hccm_consulting"]<- "hccm_consulting_hccm"
  modelevaluation$companyname[modelevaluation$companyname=="british_american_tobacco"]<- "british_american_tobacco_nigeria"
  modelevaluation$companyname[modelevaluation$companyname=="cameron"]<- "cameron_romania"
  modelevaluation$companyname[modelevaluation$companyname=="deutsche_telekom_services_europe_romania"]<- "deutsche_telekom_pan net_sro"
      } else if (country=="IT") {
  # matching Italian lost companynames
  modelevaluation$companyname[modelevaluation$companyname=="action_ict"]<- "action_ict_srl"
  modelevaluation$companyname[modelevaluation$companyname=="avangarde"]<- "avangarde_consulting"
  modelevaluation$companyname[modelevaluation$companyname=="hughes"]<- "baker_hughes"
  modelevaluation$companyname[modelevaluation$companyname=="burger_king"]<- "burger_king_restaurants_italia_spa"
  modelevaluation$companyname[modelevaluation$companyname=="organisational"]<- "european_organisational_research_group_ltd"
  modelevaluation$companyname[modelevaluation$companyname=="sicuritalia"]<- "sicuritalia_ivri_spa"  
}



# merge data with the manually-coded input (evaluation_keep and evaluation_filteredout) to create a modelevaluation dataset
modelevaluation <- merge(modelevaluation , evaluation_filteredout_m , all = TRUE)
modelevaluation <- merge(modelevaluation , evaluation_keep_m , all = TRUE)

# create an agency variable in the modelevaluation dataset which says if the companyname belongs to an agency (=1) or not (=0) according to the manual coding specifically undertaken to evaluate the model (so this is the "true" classification)
modelevaluation$agency[modelevaluation$actualemployer==1] <- 0


# augment the comboflag variable by including information from the first stage of the imputation model, that based on keywords only. this is done by changing comboflag to 1 if filteredout==1
modelevaluation$comboflag[modelevaluation$filteredout_m==1] <- 1
modelevaluation$filteredout_m[is.na(modelevaluation$filteredout_m) == TRUE] <- 0
#View(modelevaluation)
table(modelevaluation$agency)


### evaluation results

# table automflag against the agency variable to see the performance of the automatic imputation model in itself. NB: this is of relatively little practical interest because, for the computation of the indicator, the model is used in combination with the manually-inputted keywords
# true pos: comboflag==1 & agency==1; false pos:  comboflag==1 & agency==0;  false neg:  comboflag==0 & agency==1;  true neg:  comboflag==0 & agency==0
table(modelevaluation$autom_flag[modelevaluation$agency==1])
table(modelevaluation$autom_flag[modelevaluation$agency==0])
table(modelevaluation$agency[is.na(modelevaluation$comboflag)==TRUE])

# table filteredout against the agency variable to see the performance of the first stage of the model, the one only based on keywords
# true pos: filteredout==1 & agency==1; false pos:  filteredout==1 & agency==0;  false neg:  filteredout==0 & agency==1;  true neg:  filteredout==0 & agency==0
table(modelevaluation$filteredout_m[modelevaluation$agency==1 & is.na(modelevaluation$comboflag)==FALSE])
table(modelevaluation$filteredout_m[modelevaluation$agency==0 & is.na(modelevaluation$comboflag)==FALSE])
table(modelevaluation$filteredout_m[(modelevaluation$agency==0 | modelevaluation$agency==1) & is.na(modelevaluation$comboflag)==TRUE])

# table comboflag against the agency variable to see the performance of the imputation model as a whole, including both the imputation based on keywords and the automflag-based automatic imputation
# true pos: comboflag==1 & agency==1; false pos:  comboflag==1 & agency==0;  false neg:  comboflag==0 & agency==1;  true neg:  comboflag==0 & agency==0
table(modelevaluation$comboflag[modelevaluation$agency==1])
table(modelevaluation$comboflag[modelevaluation$agency==0])
table(modelevaluation$agency[is.na(modelevaluation$comboflag)==TRUE])










### final cross-checks on a voluntary basis

modelevaluation_alphab <- arrange(modelevaluation , companyname)
#View(modelevaluation_alphab)

modelevaluation$confmatrix <- 99
modelevaluation$confmatrix[modelevaluation$agency==0 & modelevaluation$comboflag==0] <- 1
modelevaluation$confmatrix[modelevaluation$agency==0 & modelevaluation$comboflag==1] <- 2
modelevaluation$confmatrix[modelevaluation$agency==1 & modelevaluation$comboflag==0] <- 3
modelevaluation$confmatrix[modelevaluation$agency==1 & modelevaluation$comboflag==1] <- 4
modelevaluation_confmatrix <- arrange(modelevaluation , confmatrix)
#View(modelevaluation_confmatrix)


modelevaluation$confmatrix2[modelevaluation$agency==0 & modelevaluation$autom_flag==0] <- 1
modelevaluation$confmatrix2[modelevaluation$agency==0 & modelevaluation$autom_flag==1] <- 2
modelevaluation$confmatrix2[modelevaluation$agency==1 & modelevaluation$autom_flag==0] <- 3
modelevaluation$confmatrix2[modelevaluation$agency==1 & modelevaluation$autom_flag==1] <- 4
modelevaluation_confmatrix2 <- arrange(modelevaluation , confmatrix2)
#View(modelevaluation_confmatrix2)



str(filteredout)

#View(modelevaluation[is.na(modelevaluation$comboflag)==TRUE,])
modelevaluation <- arrange(modelevaluation , companyname)
#View(modelevaluation)


### experimenting with automflag (not needed for the evaluation)

# experimental rules
#automflag_output <- automflag(method="error", error_pctile=90)
automflag_output <- automflag(yvar="ln_sector", xvar1="ln_province", xvar2="ln_undup_n", xvar3="ln_undup_prov", flag_above=TRUE, flag_below=FALSE)
automflag_output[[2]]
automflag_output <- automflag(yvar="ln_grab", xvar1="ln_esco4", xvar2="sqln_esco4", xvar3="culn_esco4", xvar4="quln_esco4", mydata=dataset_model, flag_above=FALSE, flag_below=TRUE)
automflag_output[[2]]
automflag_output <- automflag(yvar="ln_grab", xvar1="ln_undup_n", xvar2="sqln_undup_n", xvar3="culn_undup_n", xvar4="quln_undup_n", mydata=dataset_model, flag_above=TRUE, flag_below=FALSE)
automflag_output[[2]]
#View(automflag_output[[1]])

# data plot to get an idea of how the data looks like
plotdata <- sumstats_by_company[sumstats_by_company$tot_n>15 & sumstats_by_company$filteredout != -1, ]
ggplot(data = plotdata) + 
  geom_point(mapping = aes(x = ln_esco4, y = ln_grab, colour=filteredout))





















