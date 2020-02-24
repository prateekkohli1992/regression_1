library(rcompanion)
data <- read.csv('Data/CarPrice_Assignment.csv')
drops <- c("car_ID","price")
data<-data[ , !(names(data) %in% drops)]
data$symboling<-as.factor(data['symboling'])
data_cat<-data[ ,sapply(data, is.factor)]
data_cont<-data[ ,!sapply(data, is.factor)]