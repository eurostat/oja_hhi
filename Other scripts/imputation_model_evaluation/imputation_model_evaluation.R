

# this codes provides metrics for an overall evaluation of the agency imputation model


# upload and prepare evaluation files
mysample <- read.csv("imputation_model_evaluation/sampleforevaluation1_200.csv")
mysample$myname <- paste0(mysample$countrycode, mysample$V1)
mysample <- cbind(mysample$myname, mysample$N, mysample$agency)
colnames(mysample) <- c("myname", "N", "agency")
ontology <- readRDS("imputation_model_evaluation/table_names_list.rds")
ontology$myname <- paste0(ontology$countrycode, ontology$Var1)
ontology$ontology_agency <- 1
ontology <- cbind(ontology$myname, ontology$ontology_agency)
colnames(ontology) <- c("myname", "ontology_agency")
automodel <- readRDS("imputation_model_evaluation/staff_agencies_model_tot.rds")
automodel$myname <- paste0(automodel$countrycode, automodel$V2)
automodel$automodel_agency <- 1
automodel <- cbind(automodel$myname, automodel$automodel_agency)
colnames(automodel) <- c("myname", "automodel_agency")

# merge evaluation files
evaluationdata <- merge(mysample, ontology, all.x=T)
evaluationdata$ontology_agency[is.na(evaluationdata$ontology_agency)==T] <- 0
evaluationdata <- merge(evaluationdata, automodel, all.x=T)
evaluationdata$automodel_agency[is.na(evaluationdata$automodel_agency)==T] <- 0
evaluationdata$comboflag_agency <- as.numeric(evaluationdata$ontology_agency) + as.numeric(evaluationdata$automodel_agency)

# preliminary check
table(evaluationdata$agency)
table(evaluationdata$ontology_agency)
table(evaluationdata$automodel_agency)
table(evaluationdata$comboflag_agency)

# calculate evaluation variables for ontology and related company-level totals
evaluationdata$TP_ontology <- ifelse(evaluationdata$ontology_agency==1 & evaluationdata$agency==1, 1, 0)
evaluationdata$FP_ontology <- ifelse(evaluationdata$ontology_agency==1 & evaluationdata$agency==0, 1, 0)
evaluationdata$TN_ontology <- ifelse(evaluationdata$ontology_agency==0 & evaluationdata$agency==0, 1, 0)
evaluationdata$FN_ontology <- ifelse(evaluationdata$ontology_agency==0 & evaluationdata$agency==1, 1, 0)
TP_ontology <- sum(evaluationdata$TP_ontology)
FP_ontology <- sum(evaluationdata$FP_ontology)
TN_ontology <- sum(evaluationdata$TN_ontology)
FN_ontology <- sum(evaluationdata$FN_ontology)
accuracy_ontology <- round((TP_ontology + TN_ontology) / (TP_ontology + TN_ontology + FP_ontology + FN_ontology), 4)
precision_ontology <- round((TP_ontology) / (TP_ontology + FP_ontology), 4)
recall_ontology <- round((TP_ontology) / (TP_ontology + FN_ontology), 4)

# calculate evaluation variables for full model and related company-level totals
evaluationdata$TP <- ifelse(evaluationdata$comboflag_agency==1 & evaluationdata$agency==1, 1, 0)
evaluationdata$FP <- ifelse(evaluationdata$comboflag_agency==1 & evaluationdata$agency==0, 1, 0)
evaluationdata$TN <- ifelse(evaluationdata$comboflag_agency==0 & evaluationdata$agency==0, 1, 0)
evaluationdata$FN <- ifelse(evaluationdata$comboflag_agency==0 & evaluationdata$agency==1, 1, 0)
TP <- sum(evaluationdata$TP)
FP <- sum(evaluationdata$FP)
TN <- sum(evaluationdata$TN)
FN <- sum(evaluationdata$FN)
accuracy <- round((TP + TN) / (TP + TN + FP + FN), 4)
precision <- round((TP) / (TP + FP), 4)
recall <- round((TP) / (TP + FN), 4)
#View(evaluationdata[evaluationdata$FP==1,])

# calculate evaluation variables for ontology and related ad-level totals
evaluationdata$TP_ontology_ad <- as.numeric( ifelse(evaluationdata$ontology_agency==1 & evaluationdata$agency==1, evaluationdata$N, 0) )
evaluationdata$FP_ontology_ad <- as.numeric( ifelse(evaluationdata$ontology_agency==1 & evaluationdata$agency==0, evaluationdata$N, 0) )
evaluationdata$TN_ontology_ad <- as.numeric( ifelse(evaluationdata$ontology_agency==0 & evaluationdata$agency==0, evaluationdata$N, 0) )
evaluationdata$FN_ontology_ad <- as.numeric( ifelse(evaluationdata$ontology_agency==0 & evaluationdata$agency==1, evaluationdata$N, 0) )
TP_ontology_ad <- sum(evaluationdata$TP_ontology_ad)
FP_ontology_ad <- sum(evaluationdata$FP_ontology_ad)
TN_ontology_ad <- sum(evaluationdata$TN_ontology_ad)
FN_ontology_ad <- sum(evaluationdata$FN_ontology_ad)
accuracy_ontology_ad <- round((TP_ontology_ad + TN_ontology_ad) / (TP_ontology_ad + TN_ontology_ad + FP_ontology_ad + FN_ontology_ad), 4)
precision_ontology_ad <- round((TP_ontology_ad) / (TP_ontology_ad + FP_ontology_ad), 4)
recall_ontology_ad <- round((TP_ontology_ad) / (TP_ontology_ad + FN_ontology_ad), 4)

# calculate evaluation variables for full model and related ad-level totals
evaluationdata$TP_ad <- as.numeric( ifelse(evaluationdata$comboflag_agency==1 & evaluationdata$agency==1, evaluationdata$N, 0) )
evaluationdata$FP_ad <- as.numeric( ifelse(evaluationdata$comboflag_agency==1 & evaluationdata$agency==0, evaluationdata$N, 0) )
evaluationdata$TN_ad <- as.numeric( ifelse(evaluationdata$comboflag_agency==0 & evaluationdata$agency==0, evaluationdata$N, 0) )
evaluationdata$FN_ad <- as.numeric( ifelse(evaluationdata$comboflag_agency==0 & evaluationdata$agency==1, evaluationdata$N, 0) )
TP_ad <- sum(evaluationdata$TP_ad)
FP_ad <- sum(evaluationdata$FP_ad)
TN_ad <- sum(evaluationdata$TN_ad)
FN_ad <- sum(evaluationdata$FN_ad)
accuracy_ad <- round((TP_ad + TN_ad) / (TP_ad + TN_ad + FP_ad + FN_ad), 4)
precision_ad <- round((TP_ad) / (TP_ad + FP_ad), 4)
recall_ad <- round((TP_ad) / (TP_ad + FN_ad), 4)


# compile and save evaluation results
evaluation_results <- as.data.frame(rbind(   cbind("full_model_companynames",TP,TN,FP,FN,accuracy,precision,recall),   cbind("keywords_companynames",TP_ontology,TN_ontology,FP_ontology,FN_ontology,accuracy_ontology,precision_ontology,recall_ontology),   cbind("full_model_ads",TP_ad,TN_ad,FP_ad,FN_ad,accuracy_ad,precision_ad,recall_ad),   cbind("keywords_ads",TP_ontology_ad,TN_ontology_ad,FP_ontology_ad,FN_ontology_ad,accuracy_ontology_ad,precision_ontology_ad,recall_ontology_ad)))
colnames(evaluation_results) <- c("model", "true_positives", "true_negatives", "false_positives", "false_negatives", "accuracy_rate", "precision_rate", "recall_rate")
write.csv(evaluation_results, "imputation_model_evaluation/evaluation_results.csv")


