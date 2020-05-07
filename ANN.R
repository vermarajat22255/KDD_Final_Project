
rm(list=ls())

install.packages("neuralnet") 

library(neuralnet)

employee_data <- read.csv (file ="/Users/meghanabhat/Downloads/attrition_data.csv")
View(employee_data)

employee_data$TERMINATION_YEAR[!is.na(employee_data$TERMINATION_YEAR)] <- TRUE
employee_data$TERMINATION_YEAR[is.na(employee_data$TERMINATION_YEAR)] <- FALSE
employee_data[employee_data == ""] <- NA
employee_data <-na.omit(employee_data)

#factoring the data

employee_data <- data.frame(lapply(na.omit(employee_data),as.numeric))

##data is duvided into training data and test data

idx <- sort(sample(nrow(employee_data), as.integer(.80*nrow(employee_data))))

training<-employee_data[idx,]

test<-employee_data[-idx,]


dev.off()
model <- neuralnet(STATUS~., training[-1,-14], hidden = 5, threshold = 0.01)

plot(model)

predictData <- predict(model,test)

ann_cat<-ifelse(predictData <1.5,1,2)

length(ann_cat)
length(test$STATUS)

table(ann_cat,test$STATUS)


wrong <-(test$STATUS != ann_cat)
error_rate <- sum(wrong)/length(wrong)
error_rate*100
susccess_rate <- 1 - error_rate
susccess_rate*100
