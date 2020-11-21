rm(list=ls())
#install.packages("kohonen")

library("kohonen")

employee_data <- read.csv ("C:\\Users\\sanam\\Documents\\Desk\\CS 513 B\\dataset\\attrition_data.csv", na.strings="?")
View(employee_data)
View(employee_data)
employee_data$TERMINATION_YEAR[!is.na(employee_data$TERMINATION_YEAR)] <- TRUE
employee_data$TERMINATION_YEAR[is.na(employee_data$TERMINATION_YEAR)] <- FALSE
employee_data[employee_data == ""] <- NA
employee_data <-na.omit(employee_data)
employee_data <- sapply( employee_data, as.numeric )
dev.off()


##data is duvided into training data and test data


training2 <- employee_data[,-14]
training2 <- training2[,-1]
training2 <- training2[,-19]


employee_data_som <- som(as.matrix(training2), grid = somgrid(2,1))

summary(employee_data_som)
str(employee_data_som)
employee_data_som$unit.classif

table(cluster = employee_data_som$unit.classif, employee_data[,21])

map1 <- som(as.matrix(training2), grid = somgrid(2,1), alpha = c(0.05, 0.01), radius = 1)
plot(map1, type = 'changes')


