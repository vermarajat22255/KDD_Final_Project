rm(list=ls())
#install.packages("corrplot")

library("corrplot")

employee_data <- read.csv ("C:\\Users\\sanam\\Documents\\Desk\\CS 513 B\\dataset\\attrition_data.csv", na.strings="?")

employee_data <- data.frame(employee_data)

employee_data$TERMINATION_YEAR[!is.na(employee_data$TERMINATION_YEAR)] <- TRUE
employee_data$TERMINATION_YEAR[is.na(employee_data$TERMINATION_YEAR)] <- FALSE

employee_data[employee_data == ""] <- NA
employee_data <- na.omit(employee_data)
employee_data[, 1] <- as.numeric(as.character( employee_data[, 1] ))
employee_data[, 2] <- as.numeric(as.character( employee_data[, 2] ))
employee_data[, 3] <- as.numeric(as.character( employee_data[, 3] ))
employee_data[, 4] <- as.numeric(as.character( employee_data[, 4] ))



employee_data[, 5] <- as.numeric( employee_data[, 5] )
employee_data[, 6] <- as.numeric( employee_data[, 6] )
employee_data[, 7] <- as.numeric( employee_data[, 7] )
employee_data[, 8] <- as.numeric(as.character( employee_data[, 8] ))
employee_data[, 9] <- as.numeric(as.character( employee_data[, 9] ))
employee_data[, 10] <- as.numeric( employee_data[, 10] )
employee_data[, 11] <- as.numeric( employee_data[, 11] )
employee_data[, 12] <- as.numeric( employee_data[, 12] )
employee_data[, 13] <- as.numeric(employee_data[, 13] )
employee_data[, 14] <- as.numeric(employee_data[, 14] )

employee_data[, 15] <- as.numeric(employee_data[, 15] )
employee_data[, 16] <- as.numeric( employee_data[, 16] )
employee_data[, 17] <- as.numeric(as.character( employee_data[, 17] ))
employee_data[, 18] <- as.numeric( employee_data[, 18] )
employee_data[, 19] <- as.numeric( employee_data[, 19] )
employee_data[, 20] <- as.numeric( employee_data[, 20] )


employee_data[, 21] <- as.numeric(employee_data[, 21] )
employee_data[, 22] <- as.numeric( employee_data[, 22] )
employee_data[, 23] <- as.numeric(as.character( employee_data[, 23] ))
employee_data[, 24] <- as.numeric(as.character( employee_data[, 24] ))
employee_data[, 25] <- as.numeric(as.character( employee_data[, 25] ))
employee_data[, 26] <- as.numeric(as.character( employee_data[, 26] ))
employee_data[, 27] <- as.numeric(as.character( employee_data[, 27] ))

employee_data1 <- employee_data[,-1]
employee_data1 <- employee_data1[,-13]
employee_data1 <- employee_data1[,-19]

correlations <- cor(employee_data1,method="pearson")
corrplot(correlations, number.cex = .9, method = "circle", type = "full", tl.cex=0.8,tl.col = "black")