require(Matrix)
find_date<-function(n,dataset){
  # This function transfer the number to a date in the known dataset  
  y<-n%/%12
  m<-n%%12
  if(m==0){
    m<-12
    y<-y-1
  }
  for(p in dataset){
    d<-as.Date(p,origin="1970-01-01")
    if(as.numeric(format(d,"%Y"))==y&as.numeric(format(d,"%m"))==m){
      # If found the date, return it as string
      return(format(d,"%m/%d/%Y"))
    }
  }
  # In case of not, return as it was 30th, since day is not as important as year and month
  return(paste0(m,"/30/",y))
}

get_data_from_bloomberg <- function(filename){
  # Require library Bloomberg API
  if(!require("Rblpapi")){
    install.packages("Rblpapi")
  }

  # read the list of items
  oitems<-as.character(read.table(filename)[[1]])
  items<-paste(oitems,"INDEX")
  num <- length(items)
  blpConnect()
  
  # monthly data
  opt <- c("periodicitySelection"="MONTHLY")
  
  # get data from Bloomberg
  datatable <- bdh(securities = c(items),fields = c("px_last"),start.date = as.Date("1989-01-01"),end.date = Sys.Date(),options = opt)
  
  # initalize an empty date set
  dates_set<-c(as.Date("1989-01-31"))
  for(i in 1:num){
    # transfer date to Date class
    date <- as.Date(datatable[[i]][[1]],"%m/%d/%Y")
    # add dates to date set
    dates_set<-union(dates_set,date)
    # transfer year and month into numberic, such as merging is possible
    datatable[[i]][1] <- as.numeric(format(date,"%Y"))*12+as.numeric(format(date,"%m"))
    # rename the column
    names(datatable[[i]])[2]<-oitems[i]
  }
  
  # merge data by year and month 
  res <- datatable[[1]]
  for(i in 2:num){
    res <- merge(res,datatable[[i]],all=T,by<-"date")
  }
  
  # transfer date from numeric to normal format
  res[,1]<-sapply(res[,1],function(x){return(find_date(x,dates_set))})
  return(res)
}



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
  
  # Remove the all NA rows at the beginning of the file
  ori_data <- ori_data[min(which(rowSums(!is.na(ori_data[,3:dim(ori_data)[2]]))>0)):dim(ori_data)[1],]
  
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

clean_and_save<-function(filename,origin_data){
  # clean and save bloomberg data
  print(origin_data)
  # turn off scientific notation to write
  options(scipen=10)
  write.csv(origin_data,sub(".[ct][sx][vt]$","_cleaned.csv",filename),row.names = FALSE)
  
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
  write.csv(ic_data,sub(".[ct][sx][vt]$","_incRt.csv",filename),row.names = FALSE)
  write.csv(mr_data,sub(".[ct][sx][vt]$","_mrtRel.csv",filename),row.names = FALSE)
  options(scipen=0)
  
}

# "main" part of the script
filename<-file.choose()
datatable<-get_data_from_bloomberg(filename)
clean_and_save(filename,datatable)
