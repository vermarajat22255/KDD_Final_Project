# remove all objects
rm(list=ls())

library("e1071")

#input the dataset
filename <- file.choose()
dsn <- read.csv(filename)
dsn_copy <- read.csv(filename)

View(dsn)

#replace all NA in TERMINATION_YEAR to 0
dsn[c("TERMINATION_YEAR")][is.na(dsn[c("TERMINATION_YEAR")])] <- 0

#remove the STATUS column
dsn <- within(dsn, rm("STATUS"))

#factoring the data
dsn$TERMINATION_YEAR <- factor(ifelse(dsn$TERMINATION_YEAR>0, "working", "terminated"))

dev.off()

#SVM

x <- subset(dsn, select=-TERMINATION_YEAR)
y <- dsn$TERMINATION_YEAR

svm_model <- svm(TERMINATION_YEAR ~ ., data=dsn)
summary(svm_model)

pred <- predict(svm_model,x)

table(pred,y)


SVM_wrong2<-sum(dsn[,14]!=pred)
error_rate<-SVM_wrong2/length(dsn[,14])
error_rate * 100

success_rate = 1 - error_rate
success_rate * 100

