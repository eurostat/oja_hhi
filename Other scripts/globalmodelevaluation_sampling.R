

originalrds <- readRDS("EU_results/table_names.rds")
samplingframe <- originalrds
samplingframe$id <- paste0(samplingframe$V1,samplingframe$countrycode)
dim(samplingframe)
##View(samplingframe)

sample1 <- as.data.frame(sample(samplingframe$id, size=200, prob=samplingframe$N))
colnames(sample1) <- "id"
sample1$sample1 <- 1
samplingframe <- merge(samplingframe, sample1, all.x=T)
samplingframe$sample1[is.na(samplingframe$sample1)==T] <- 0
#View(samplingframe[samplingframe$sample1==1])
table(samplingframe$sample1)
sum(as.numeric(samplingframe$N[samplingframe$sample1==1]))

# create sample to use for evaluation
sampleforevaluation <- samplingframe[samplingframe$sample1==1]
write.csv(sampleforevaluation, "sampleforevaluation.csv")


sample2 <- as.data.frame(sample(samplingframe$id, size=200,))
colnames(sample2) <- "id"
sample2$sample2 <- 1
samplingframe <- merge(samplingframe, sample2, all.x=T)
samplingframe$sample2[is.na(samplingframe$sample2)==T] <- 0
#View(samplingframe[samplingframe$sample2==1])
table(samplingframe$sample2)
sum(as.numeric(samplingframe$N[samplingframe$sample2==1]))

