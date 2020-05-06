# remove all objects
rm(list=ls())

library(rpart)
library(rpart.plot)

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

dev.off()

v <- dsn$TERMINATION_YEAR

table(v)

#train and test data
index<-sort(sample(nrow(dsn),round(.7*nrow(dsn))))
training<-dsn[-index,]
test<-dsn[index,]

treeFit <- rpart(TERMINATION_YEAR~.,data=training,method = 'class')
#print(treeFit)

rpart.plot(treeFit)

Prediction1 <- predict(treeFit,newdata=dsn[-14],type = 'class')

table(Prediction1,dsn$TERMINATION_YEAR)

dtrees_wrong<-sum(dsn[,14]!=Prediction1)
error_rate<-dtrees_wrong/length(dsn[,14])
error_rate

