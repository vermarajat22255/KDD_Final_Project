
rm(list=ls())

employee_data <- read.csv (file ="/Users/meghanabhat/Downloads/attrition_data.csv")
View(employee_data)
employee_data$TERMINATION_YEAR[!is.na(employee_data$TERMINATION_YEAR)] <- TRUE
employee_data$TERMINATION_YEAR[is.na(employee_data$TERMINATION_YEAR)] <- FALSE
employee_data[employee_data == ""] <- NA
employee_data <-na.omit(employee_data)

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


#employee_data <- sapply( employee_data, as.numeric )

mmnorm<-function(x,minx,maxx)
{
  z<-((x-minx)/(maxx-minx))
  return(z)
}



employee_Data_normalized<-as.data.frame ( 
  
  cbind(  F1=mmnorm(employee_data[,1],min(employee_data[,1]),max(employee_data[,1] ))
          ,F2=mmnorm(employee_data[,2],min(employee_data[,2]),max(employee_data[,2] ))
          ,F3=mmnorm(employee_data[,3],min(employee_data[,3]),max(employee_data[,3] ))
          , F4=mmnorm(employee_data[,4],min(employee_data[,4]),max(employee_data[,4] ))
          ,F5=as.character(employee_data[,5])
          ,F6=as.character(employee_data[,6])
          ,F7=as.character(employee_data[,7])
          ,F8=mmnorm(employee_data[,8],min(employee_data[,8]),max(employee_data[,8] ))
          ,F9=mmnorm(employee_data[,9],min(employee_data[,9]),max( employee_data[,9] ))
          ,F10=as.character(employee_data[,10])
          ,F11=as.character(employee_data[,11])
          ,F12=as.character(employee_data[,12])
          ,F13=as.character(employee_data[,13])
          ,F14=mmnorm(employee_data[,14],min(employee_data[,14]),max(employee_data[,14] ))
          ,F15=as.character(employee_data[,15])
          ,F16=as.character(employee_data[,16])
          ,F17=mmnorm(employee_data[,17],min(employee_data[,17]),max(employee_data[,17] ))
          ,F18=as.character(employee_data[,18])
          ,F19=as.character(employee_data[,19])
          ,F20=as.character(employee_data[,20])
          ,F21=as.character(employee_data[,21])
          ,F22=as.character(employee_data[,22])
          ,F23=mmnorm(employee_data[,23],min(employee_data[,23]),max(employee_data[,23] ))
          ,F24=mmnorm(employee_data[,24],min(employee_data[,24]),max(employee_data[,24] ))
          ,F25=mmnorm(employee_data[,25],min(employee_data[,25]),max(employee_data[,25] ))
          ,F26=mmnorm(employee_data[,26],min(employee_data[,26]),max(employee_data[,26] ))
          ,F27=mmnorm(employee_data[,27],min(employee_data[,27]),max(employee_data[,27] ))
          
          
  )
)


employee_Data_normalized <- employee_Data_normalized[,-1,-14]
kmeans <- kmeans(employee_Data_normalized[,-1,-14],2,nstart = 10)
table(kmeans$cluster,employee_Data_normalized[,1])

