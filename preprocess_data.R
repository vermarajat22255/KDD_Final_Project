rm(list=ls())

filename <- file.choose()
dsn <- read.csv(filename)

View(dsn)

#replace all NA in TERMINATION_YEAR to 0
dsn[c("TERMINATION_YEAR")][is.na(dsn[c("TERMINATION_YEAR")])] <- 0

#factoring the data
dsn$TERMINATION_YEAR <- factor(ifelse(dsn$TERMINATION_YEAR>0, "working", "terminated"))

#training and test data
index<-sort(sample(nrow(dsn),round(.7*nrow(dsn))))
training<-dsn[-index,]
test<-dsn[index,]