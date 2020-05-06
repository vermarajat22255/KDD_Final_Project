rm(list=ls())
#install.packages("randomForest")
library(randomForest)

file_name <- file.choose()
attr<- read.csv(file_name,strip.white = TRUE, na.strings = c("","?",NA))

#Remove unused columns 
attr<-attr[-1] #employee id
attr<-attr[-13] #termination year
attr<- attr[-20] #Job Group

#Misssing list ----------------
#Ethnicity 1 missing
# Referal --> Unknowns 472, NA 445
#Termination year -> NA 5394

#Preprocess Ethnicity
attr$ETHNICITY<-as.numeric(attr$ETHNICITY)
median(attr$ETHNICITY, na.rm = TRUE)
attr$ETHNICITY[which(is.na(attr$ETHNICITY))] <- median(attr$ETHNICITY, na.rm = TRUE)

#Termination Year
#attr[c("TERMINATION_YEAR")][is.na(attr[c("TERMINATION_YEAR")])] <- 0
#attr$TERMINATION_YEAR <- factor(ifelse(attr$TERMINATION_YEAR>0, "working", "terminated"))
#is.factor(attr$TERMINATION_YEAR)

# Pre process Referal remove NA
attr<-na.omit(attr)
View(attr)

#Job group as numeric
#attr[c("JOB_GROUP")]<-as.numeric(attr$JOB_GROUP)

#Loading 70% data in training dataset
idx<-sort(sample(nrow(attr),as.integer(.70*nrow(attr))))
training<-attr[idx,]

#Loading 30% in test
test <- attr[-idx, ]

#Preparing & Plotting

#cc = training[,c(1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,22,23,24,25,26 )];
fit <- randomForest( STATUS~., data = training, importance=TRUE, ntree=1000)
importance(fit)
#summary(training$JOB_GROUP)
prediction<- predict(fit, test)
table(actual=test[,19],prediction)
#str(prediction)
wrong<-sum(test$STATUS!=prediction)
error_rate<-wrong/length(test[,19])
error_rate *100

successrate <- 1 - error_rate
successrate*100
