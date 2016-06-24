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
