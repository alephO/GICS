require(Matrix)

## functions
cal_increase_rate <- function(ori_data,date,market,prices){
  #calculate increase rate
  len <- length(ori_data[[date]])
  d <- ori_data[[date]]
  for(ind in c(market,prices)){
    arr <- ori_data[[ind]]
    for(i in 2:len){
      oldn <- arr[i-1]
      newn <- arr[i]
      if(is.na(oldn)||is.na(newn)||!is.numeric(oldn) || !is.numeric(newn) || oldn==0){
        warning(sprintf("increase rate of %s at %s is unavailibale",ind,d[i]))
        arr[i-1] <- NA
      } else {
        arr[i-1] <- (newn-oldn)/oldn
      }
    }
    ori_data[ind] <- c(NA,arr[1:length(arr)-1])
  }
  return(ori_data)
}

cal_mrk_rel <- function(nw_data,market,prices){
  mkp = nw_data[[market]]
  nw_data[market]<-NULL
  for(ind in prices){
    nw_data[ind] <- nw_data[[ind]] - mkp
  }
  return(nw_data)
}


## Read Table
filename<-file.choose()
origin_data <- read.csv(filename)

## Remove "Index" from tags
names(origin_data) <- sub("[ .]*index$",'',names(origin_data),ignore.case = TRUE)

## Assign Useful tages
date <- names(origin_data)[1]
market <- names(origin_data)[2]
prices <- names(origin_data)[3:length(origin_data)]


## Calculate Increase Rate
#if(!exists("cal_increase_rate", mode="function")) source("cal-rate.R")
ic_data<-cal_increase_rate(origin_data,date,market,prices)

## Remove rows with ALL data missing
ic_data <- ic_data[rowSums(is.na(ic_data[,c(market,prices)]))<length(c(market,prices)),]


## Calculate mrk_rel
mr_data <- cal_mrk_rel(ic_data,market,prices)

## Save file
options(scipen=10)
write.csv(ic_data,sub(".csv$","_incRt.csv",filename),row.names = FALSE)
write.csv(mr_data,sub(".csv$","_mrtRel.csv",filename),row.names = FALSE)
options(scipen=0)
