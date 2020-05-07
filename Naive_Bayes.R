
rm(list=ls())

#install.packages('e1071', dependencies = TRUE)

library(e1071)
library(class) 


employee_data <- read.csv ("C:\\Users\\sanam\\Documents\\Desk\\CS 513 B\\dataset\\attrition_data.csv", na.strings="?")
View(employee_data)

employee_data$TERMINATION_YEAR[!is.na(employee_data$TERMINATION_YEAR)] <- TRUE
employee_data$TERMINATION_YEAR[is.na(employee_data$TERMINATION_YEAR)] <- FALSE
employee_data[employee_data == ""] <- NA
employee_data <-na.omit(employee_data)


##data is divided into training data and test data

idx <- sample(nrow(employee_data), size = floor(.70*nrow(employee_data)), replace = F) 

training<-employee_data[idx,]

test<-employee_data[-idx,]


nBayes_all<- naiveBayes(STATUS ~., data =training[-1,-14])

category_all <- predict(nBayes_all, test)

table(nBayes_all=category_all,Class=test$STATUS)

prop.table(table(nBayes_all=category_all,Class=test$STATUS)) 

# Naive Bayes classification using all variables 

category_all<-predict(nBayes_all,employee_data  )

table(nBayes_all=category_all,Class=employee_data$STATUS)

NB_wrong<-sum(category_all!=employee_data$STATUS)
print(NB_wrong)

NB_error_rate<-NB_wrong/length(category_all)
print(NB_error_rate)

print(NB_error_rate*100)
success_rate <- 1-NB_error_rate
print(success_rate*100)

plot(category_all)


