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
options(warn=-1)

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
ttt <- NULL

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
  included_data <- origin_data[which(apply(true_matrix,2,any))]
  true_matrix <- !is.na(included_data)
  narrow <-which(apply(true_matrix,1,all))
  extended <- which(apply(true_matrix,1,any))
  narrow_start<-min(narrow)
  narrow_end<-max(narrow)
  extended_start<-min(extended)
  extended_end<-max(extended)
  t1<-data.frame(character(0))
  names(t1)<-"Excluded because of lacking data in that period"
  t2<-data.frame(always_exclude)
  names(t2)<-"Always excluded since no data availiable"
  
  
  
  gw_option <- gwindow("Options")
  glabel(filename,container = gw_option)
  #gp_option<-ggroup(horizontal = F,container = gw_option)
  gp_option <- gw_option
  
  gp2_option <- gframe(container = gp_option)
  glabel("CLuster Method:",container = gp2_option)
  gclumethod <- gdroplist(c("K-mean","AGNES"),container = gp2_option, 
                          handler=function(h,...){
                            if(svalue(gclumethod,index = T)==1){
                              enabled(groupnum)<-T
                            } else if (svalue(gclumethod,index = T)==2){
                              enabled(groupnum)<-F
                            }
                          })
  #apply(true_matrix,2,any)
  glabel("Corelation Method",container = gp2_option)
  gcormethod <- gdroplist(c("pearson"),container = gp2_option)
  
  gp21_option<- gframe(container = gp_option)
  glabel("Number of groups:",container = gp21_option)
  groupnum <- gspinbutton(from = 1,to = 1000, value = 10,container = gp21_option)
  
  ggp<-ggroup(container = gp_option,horizontal = F)
  gp3_option <- ggroup(container = ggp)
  glabel("Start: ",container = gp3_option)
  gss<-gslider(from = extended_start,to=extended_end,container = gp3_option,expand = T,value = narrow_start
               ,handler = function(h,...){
                 newym <- formated_yearmonth[svalue(gss)]
                 svalue(gesy)<-format(newym,"%Y")
                 svalue(gesm)<-format(newym,"%m")
                 update_gt1()
               })
  # size(gss)<-c(500,50)
  gesy<-gedit(format(formated_yearmonth[svalue(gss)],"%Y"),container = gp3_option,width = 4)
  gesm<-gedit(format(formated_yearmonth[svalue(gss)],"%m"),container = gp3_option,width = 2)
  updatestart<-function(h,...){
    #print(yearmon_to_row(formated_yearmonth[1],as.yearmon(paste(svalue(gesy),svalue(gesm)),"%Y %m")))
    svalue(gss)<-yearmon_to_row(formated_yearmonth[1],as.yearmon(paste(svalue(gesy),svalue(gesm)),"%Y %m"))
    update_gt1()
  }
  addHandlerKeystroke(gesy,handler = updatestart)
  addHandlerKeystroke(gesm,handler = updatestart)
  
  gp4_option <- ggroup(container = ggp)
  glabel("End:   ",container = gp4_option)
  gse<-gslider(from = extended_start,to=extended_end,container = gp4_option,expand = T,value = narrow_end
               ,handler = function(h,...){
                 newym <- formated_yearmonth[svalue(gse)]
                 svalue(geey)<-format(newym,"%Y")
                 svalue(geem)<-format(newym,"%m")
                 update_gt1()
               })
  geey<-gedit(format(formated_yearmonth[svalue(gse)],"%Y"),container = gp4_option,width = 4)
  geem<-gedit(format(formated_yearmonth[svalue(gse)],"%m"),container = gp4_option,width = 2)
  updateend<-function(h,...){
    #print(yearmon_to_row(formated_yearmonth[1],as.yearmon(paste(svalue(geey),svalue(geem)),"%Y %m")))
    svalue(gse)<-yearmon_to_row(formated_yearmonth[1],as.yearmon(paste(svalue(geey),svalue(geem)),"%Y %m"))
    update_gt1()
  }
  addHandlerKeystroke(geey,handler = updateend)
  addHandlerKeystroke(geem,handler = updateend)
  
  gbstart <- gbutton("GO",container = gp_option)
  
  
  update_gt1<-function(){
    cr_ex<-names(included_data)[which(!apply(!is.na(included_data[svalue(gss):svalue(gse),]),2,all))]
    t1<-data.frame(cr_ex)
    names(t1)<-"Excluded because of lacking data in that period"
    gt1[,]<-t1
  }
  
  gp5_option <- gpanedgroup(container = gp_option)
  # gp5l<-ggroup(container = gp5_option,use.scrollwindow = T)
  # gp5r<-ggroup(container = gp5_option,use.scrollwindow = T)
  gt1<-gtable(t1,container = gp5_option)
  gt2<-gtable(t2,container = gp5_option)
  # size(gp5l)<-c(400,300)
  # size(gt1)<-c(400,300)  
  # size(gp5r)<-c(400,200)
  # size(gt2)<-c(400,300)
 

  
  #print(always_exclude)
  
  #size(gw_option)<-c(800,600)

  addHandlerClicked(gbstart,function(h,...){
    new_data<<-included_data[svalue(gss):svalue(gse),which(apply(!is.na(included_data[svalue(gss):svalue(gse),]),2,all))]
    distance <<- 1 - cor(new_data)
    if(svalue(gclumethod,index = T)==1){
      clu <<- pam(distance, svalue(groupnum), diss = TRUE)
    } else if (svalue(gclumethod,index = T)==2){
      clu <<- agnes(distance, diss = TRUE)
    }
    plot(clu)
    result<-vector(mode = "list",length = svalue(groupnum))
    for(i in 1:svalue(groupnum)){
      print(paste0("Class ",i,":"))
      print(names(new_data)[which(clu$clustering==i)])
    }
  })
  
  addHandlerDestroy(gw_option,function(h,...){
    options(warn=0)
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


