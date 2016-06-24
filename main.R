require(Matrix)

## Read Table
origin_data <- read.csv("sp500_industrygroup.csv")

## Remove "Index" from tags
names(origin_data) <- sub("[ .]*index$",'',names(origin_data),ignore.case = TRUE)

## Assign Useful tages
date <- "Date"
market <- "MRKT"
prices <- names(origin_data)[3:26]


## Calculate Increase Rate
if(!exists("cal_increase_rate", mode="function")) source("cal-rate.R")
ic_data<-cal_increase_rate(origin_data,date,market,prices)

## Remove rows with ALL data missing
ic_data <- ic_data[rowSums(is.na(ic_data[,c(market,prices)]))<length(c(market,prices)),]


## Calculate mrk_rel
mr_data <- cal_mrk_rel(ic_data,market,prices)
