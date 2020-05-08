rm(list=ls())
#install.packages("randomForest")
library(randomForest)

file_name <- file.choose()
employee_data<- read.csv(file_name,strip.white = TRUE, na.strings = c("","?",NA))

employee_data[is.na(employee_data)] <- 0 #setting NA value to 0

employee_data$TERMINATION_YEAR<-findInterval(employee_data$TERMINATION_YEAR, c(2008, 2011, 2017))
employee_data$TERMINATION_YEAR <- as.factor(employee_data$TERMINATION_YEAR)

# Removing Employee ID,JOBCODE,REFERAL_SOURCE,JOB_GROUP column
dataSet <- subset(employee_data, select = -c(1,4,11,22))

# Removing ANNUAL_RATE column as ANNUAL_RATE and HRLY_RATE column are co-related
#remove highly correlated variables
dataSet <- subset(dataSet, select = -c(1))

# Pre process Referal remove NA
#sum(is.na(dataSet))
dataSet<-na.omit(dataSet)
View(dataSet)

#Loading 70% data in training dataset
idx<-sort(sample(nrow(dataSet),as.integer(.70*nrow(dataSet))))
training<-dataSet[idx,]

#Loading 30% in test
test <- dataSet[-idx, ]

#Preparing & Plotting
fit <- randomForest( STATUS~., data = training, importance=TRUE, ntree=1000)
importance(fit)
varImpPlot(fit)

#summary(training$JOB_GROUP)
prediction<- predict(fit, test)
table(actual=test$STATUS,prediction)

wrong<-sum(test$STATUS!=prediction)
error_rate<-wrong/length(test$STATUS)
error_rate *100

successrate <- 1 - error_rate
successrate*100
