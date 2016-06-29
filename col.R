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


gw_openfile <- gwindow("Open CSV file")
gp_file1 <- ggroup(container = gw_openfile)
glabel("File Path:",container = gp_file1)
gfn <- gedit(container = gp_file1)

gbutton("Choose",handler = function(h,...){
  svalue(gfn)<-file.choose()
  focus(gw_openfile)
},container = gp_file1)



filename<-""
origin_data<-NULL
date <- NULL
formated_date<-NULL
formated_yearmonth<-NULL
new_data<-NULL
distance<-NULL
clu<-NULL
date_format<-1
ignore_second<-1

# Choose date format
gp_file2 <- ggroup(container = gw_openfile)
glabel("Date format:",container = gp_file2)
gdateformat<-gradio(c("mm/dd/yyyy","mm/yyyy"),container = gp_file2,horizontal = T)
gp_file3 <- ggroup(container = gw_openfile)
glabel("Ignore second conlum (Since they may be the market value)?",container = gp_file3)
gignoresecond <- gradio(c("Yes","No"),container = gp_file3,horizontal = T)

stage2 <- function(){
  
  date <<- origin_data[[1]]
  if(date_format==1){
    formated_date <<- as.Date(date,"%m/%d/%Y")
    formated_yearmonth <<- as.yearmon(formated_date)
  } else {
    formated_yearmonth <<- as.yearmon(date,"%m/%Y")
  }
  origin_data[1]<<-NULL
  if (ignore_second==1){
    origin_data[1]<<-NULL
  }
  
  true_matrix <- !is.na(origin_data)
  always_exclude <- names(origin_data)[which(!apply(true_matrix,2,any))]
  narrow <-which(apply(true_matrix,1,all))
  extended <- which(apply(true_matrix,1,any))
  narrow_start<-min(narrow)
  narrow_end<-max(narrow)
  extended_start<-min(extended)
  extended_end<-max(extended)
  
  
  
  gw_option <- gwindow("Options")
  glabel(filename,container = gw_option)
  gp_option<-ggroup(horizontal = F,container = gw_option)

  gp2_option <- gframe(container = gp_option)
  glabel("CLuster Method:",container = gp2_option)
  gclumethod <- gdroplist(c("K-mean"),container = gp2_option)
  #apply(true_matrix,2,any)
  glabel("Corelation Method",container = gp2_option)
  gcormethod <- gdroplist(c("pearson"),container = gp2_option)
  
  gp3_option <- ggroup(container = gp_option)
  gss<-gslider(from = extended_start,to=extended_end,container = gp3_option,expand = T)
  gesy<-gedit(format(formated_yearmonth[svalue(gss)],"%Y"),container = gp3_option,width = 4)
  gesm<-gedit(format(formated_yearmonth[svalue(gss)],"%m"),container = gp3_option,width = 2)
  gbstart <- gbutton("GO",container = gw_option)
  
  

  addHandlerClicked(gbstart,function(h,...){
    new_data<<-origin_data[147:321,]
    distance <<- 1 - cor(new_data)
    clu <<- pam(distance, 10, diss = TRUE)
    print(plot(clu))
  })
}

gfileOK = gbutton("OK",container = gw_openfile,handler = function(h,...){
  filename<<-svalue(gfn)
  origin_data <<- read.csv(filename)
  # if (svalue(gdateformat)=="mm/dd/yyyy"){
  #   date_format<<-1
  # } else {
  #   date_format<<-2
  # }
  date_format<<-svalue(gdateformat,index = T)
  ignore_second<<-svalue(gignoresecond,index = T)
  dispose(gw_openfile)
  stage2()
})



#mkt <- origin_data$MRKT
#ind1 <- origin_data$S5AUCO.INDEX
#plot(mkt,type = "l",col="red")
#par(new=TRUE)
#plot(ind1,type="l",col="blue")



