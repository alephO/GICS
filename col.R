## This script read the cleaned CSV file, and return a classification

if(!require("gWidgetstcltk")){
  install.packages("gWidgetstcltk",dep=TRUE)
}
if(!require("cluster")){
  install.packages("cluster",dep=TRUE)
}
require(Matrix)
require(graphics)
require(zoo)

if(!exists("to_time_series",mode="function")) source("tools.R")

#filename<-file.choose()
filename <- "1.csv"
origin_data <- read.csv(filename)
date <- origin_data$Date
mkt <- origin_data$MRKT
ind1 <- origin_data$S5AUCO.INDEX
#plot(mkt,type = "l",col="red")
#par(new=TRUE)
#plot(ind1,type="l",col="blue")
formated_date <- as.Date(date,"%m/%d/%Y")


