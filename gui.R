if(!require("gWidgetstcltk")){
  install.packages("gWidgetstcltk",dep=TRUE)
}
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

date <- ""
market <- ""
prices <- vector(mode="character")

options("guiToolkit"="tcltk")
mw <- gwindow(title="Calculator",visible = FALSE,height = "1024")
gp1 <- ggroup(container = mw)
gl1 <- glabel("File Directory",container = gp1)
gt1 <- gedit(container = gp1)
gb1 <- gbutton("Choose"
               ,handler = function(h,...) {
                  svalue(gt1)<-file.choose()
                  focus(mw)}
               ,container = gp1)
gb2 <- gbutton("Load",container = gp1,handler = function(h,...){
  filename<<-svalue(gt1)
  origin_data <<- read.csv(filename)
  names(origin_data) <<- sub("[ .]*index$",'',names(origin_data),ignore.case = TRUE)
  gbs<-vector(mode="list",length = length(names(origin_data)))
  rds<<-vector(mode="list",length = length(names(origin_data)))
  #invisible(gp2)
  for(i in 1:length(names(origin_data))){
    gbs[[i]]<-ggroup(container = gp2)
    glabel(names(origin_data)[i],container = gbs[[i]])
    rds[[i]]<<-gradio(c("Date","Market","Other indexs","Irrelevant"),container = gbs[[i]],horizontal = TRUE,selected = 3)
  }
  svalue(rds[[1]],index = TRUE)<-1
  svalue(rds[[2]],index = TRUE)<-2
  #visible(gp2)
})
gb3 <- gbutton("Run & Save",container = gp1,handler = function(h,...){
  for(i in 1:length(names(origin_data))){
    tmp<-svalue(rds[[i]])
    if(tmp=="Date"){
      date<<-names(origin_data)[i]
    } else if (tmp=="Market") {
      market<<-names(origin_data)[i]
    }else if(tmp=="Other indexs") {
      prices[[length(prices)+1]]<<-names(origin_data)[i]
    }
    
  }
 
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
  gmessage("convert OK",icon = "info",parent = mw) 
  dispose(mw)
  
})
gp2<- ggroup(container= mw, use.scrollwindow = TRUE, horizontal = FALSE)
visible(mw) <- TRUE
