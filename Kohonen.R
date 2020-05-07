rm(list=ls())
install.packages("kohonen")

library("kohonen")

employee_data <- read.csv (file ="/Users/meghanabhat/Downloads/attrition_data.csv")
View(employee_data)
employee_data$TERMINATION_YEAR[!is.na(employee_data$TERMINATION_YEAR)] <- TRUE
employee_data$TERMINATION_YEAR[is.na(employee_data$TERMINATION_YEAR)] <- FALSE
employee_data[employee_data == ""] <- NA
employee_data <-na.omit(employee_data)
employee_data <- sapply( employee_data, as.numeric )
dev.off()

##data is duvided into training data and test data

idx <- sort(sample(nrow(employee_data), as.integer(.70*nrow(employee_data))))

training<-employee_data[idx,]

test<-employee_data[-idx,]

employee_data_som <- som(as.matrix(training[-1,-14]), grid = somgrid(3,1))

summary(employee_data_som)
str(employee_data_som)
employee_data_som$unit.classif


