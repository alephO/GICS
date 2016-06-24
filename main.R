require(Matrix)

## Read Table
origin_data <- read.csv("sp500_industrygroup.csv")

## Remove "Index" from tags
names(origin_data) <- sub("[ .]*index$",'',names(origin_data),ignore.case = TRUE)

## Assign Useful tages
date <- "DATE"
market <- "MRKT"
prices <- names(origin_data)[3:26]


## Process with data
