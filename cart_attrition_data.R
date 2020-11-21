# remove all objects
rm(list=ls())

#installed.packages()

#install.packages("rpart")
#install.packages("rpart.plot")     # Enhanced tree plots
#install.packages("rattle")         # Fancy tree plot
#install.packages("RColorBrewer")   # colors needed for rattle
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle

#input the dataset
filename <- file.choose()
dsn <- read.csv(filename)

View(dsn)

#replace all NA in TERMINATION_YEAR to 0
dsn[c("TERMINATION_YEAR")][is.na(dsn[c("TERMINATION_YEAR")])] <- 0

#remove the STATUS column
dsn <- within(dsn, rm("STATUS"))

#factoring the data
dsn$TERMINATION_YEAR <- factor(ifelse(dsn$TERMINATION_YEAR>0, "working", "terminated"))

#train and test data
index<-sort(sample(nrow(dsn),round(.7*nrow(dsn))))
training<-dsn[-index,]
test<-dsn[index,]

dev.off()

#CART
CART_class<-rpart( TERMINATION_YEAR~.,data=training)
rpart.plot(CART_class)
CART_predict<-predict(CART_class,test, type="class")
CART_wrong2<-sum(test[,14]!=CART_predict)
CART_error_rate2<-CART_wrong2/length(test[,14])
CART_error_rate2 * 100

CART_success_rate = 1 - CART_error_rate2
CART_success_rate * 100

library(rpart.plot)
prp(CART_class)


# much fancier graph
fancyRpartPlot(CART_class)
