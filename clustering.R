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

############### This part is useless now############################
to_time_series<-function(formated_date,data_vector){
  #This function transfer a data vector to time series
  non_na_indexes <- which(!is.na(data_vector))
  min_index<-min(non_na_indexes)
  max_index<-max(non_na_indexes)
  return(ts(data_vector[min_index:max_index],start = c(as.numeric(format(formated_date[min_index],"%Y")),as.numeric(format(formated_date[min_index],"%m"))),
            end = c(as.numeric(format(formated_date[max_index],"%Y")),as.numeric(format(formated_date[max_index],"%m"))),frequency = 12))
}

get_start_date<-function(time_series){
  s<-start(time_series)
  return(as.yearmon(paste(s[1],s[2]),"%Y %m"))
}

get_end_date<-function(time_series){
  e<-end(time_series)
  return(as.yearmon(paste(e[1],e[2]),"%Y %m"))
}
#############################  End  ################################


# From yearmon object to coresponding row number
yearmon_to_row<-function(bg,ym){
  yy<-as.numeric(format(ym,"%Y"))-as.numeric(format(bg,"%Y"))
  mm<-as.numeric(format(ym,"%m"))-as.numeric(format(bg,"%m"))
  return(12*yy+mm+1)
}

options(warn=-1)

# Create the first window
gw_openfile <- gwindow("Open CSV file")

# Pannel Group1: filepath
gp_file1 <- ggroup(container = gw_openfile)
glabel("File Path:",container = gp_file1)
gfn <- gedit(container = gp_file1)
gbutton("Choose",handler = function(h,...){
  svalue(gfn)<-file.choose()
  focus(gw_openfile)
},container = gp_file1)


# Some global varibles
# Generally, initalizing varible in R is not neccessary
# However, modifying them in callback functions without initalizing them may causs an error
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

# Pannel Group 2
# Choose date format
# Since the data is monthly, giving data without day is also OK
gp_file2 <- ggroup(container = gw_openfile)
glabel("Date format:",container = gp_file2)
gdateformat<-gradio(c("mm/dd/yyyy","mm/yyyy"),container = gp_file2,horizontal = T)

# Pannel Group 3
# If ignore column 2, since they maybe market value
gp_file3 <- ggroup(container = gw_openfile)
glabel("Ignore second conlum (Since they may be the market value)?",container = gp_file3)
gignoresecond <- gradio(c("Yes","No"),container = gp_file3,horizontal = T)

# Stage 2 is a seperate function, otherwise this part will in the callback function in the "OK" button clicked
# It will be ugly
stage2 <- function(){
  
  date <<- origin_data[[1]]
  
  # reformating date
  if(date_format==1){
    formated_date <<- as.Date(date,"%m/%d/%Y")
    formated_yearmonth <<- as.yearmon(formated_date)
  } else {
    formated_yearmonth <<- as.yearmon(date,"%m/%Y")
  }
  origin_data[1]<<-NULL
  
  # If ignoring second line
  if (ignore_second==1){
    origin_data[1]<<-NULL
  }
  
  # True matrix: true if not NA
  true_matrix <- !is.na(origin_data)
  
  # always excluded: no data provided
  always_exclude <- names(origin_data)[which(!apply(true_matrix,2,any))]
  
  # included items: not in the above
  included_data <- origin_data[which(apply(true_matrix,2,any))]
  true_matrix <- !is.na(included_data)
  
  # narrow date range is the range to guarantee all (not always excluded) items included
  # extended date range is the range for some items included
  narrow <-which(apply(true_matrix,1,all))
  extended <- which(apply(true_matrix,1,any))
  narrow_start<-min(narrow)
  narrow_end<-max(narrow)
  extended_start<-min(extended)
  extended_end<-max(extended)
  
  # Two data from to display at the bottom of the window
  t1<-data.frame(character(0))
  names(t1)<-"Excluded because of lacking data in that period"
  t2<-data.frame(always_exclude)
  names(t2)<-"Always excluded since no data availiable"
  
  
  # open another window: Options
  gw_option <- gwindow("Options")
  glabel(filename,container = gw_option)
  gp_option <- gw_option
  
  # Pannel group one : cluster method
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
  glabel("Corelation Method",container = gp2_option)
  gcormethod <- gdroplist(c("pearson"),container = gp2_option)
  
  # Pannel group 2: enter the number of groups in cluster
  gp21_option<- gframe(container = gp_option)
  glabel("Number of groups:",container = gp21_option)
  groupnum <- gspinbutton(from = 1,to = 1000, value = 10,container = gp21_option)
  
  # pannel group 3: slice the bar( or enter) the start date
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
  gesy<-gedit(format(formated_yearmonth[svalue(gss)],"%Y"),container = gp3_option,width = 4)
  gesm<-gedit(format(formated_yearmonth[svalue(gss)],"%m"),container = gp3_option,width = 2)
  updatestart<-function(h,...){
    svalue(gss)<-yearmon_to_row(formated_yearmonth[1],as.yearmon(paste(svalue(gesy),svalue(gesm)),"%Y %m"))
    update_gt1()
  }
  addHandlerKeystroke(gesy,handler = updatestart)
  addHandlerKeystroke(gesm,handler = updatestart)
  
  # pannel group 4: slice the bar( or enter) the ending date
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
    svalue(gse)<-yearmon_to_row(formated_yearmonth[1],as.yearmon(paste(svalue(geey),svalue(geem)),"%Y %m"))
    update_gt1()
  }
  addHandlerKeystroke(geey,handler = updateend)
  addHandlerKeystroke(geem,handler = updateend)
  
  
  # the go button
  gbstart <- gbutton("GO",container = gp_option)
  
  
  # update table 1: excluded in current date period
  update_gt1<-function(){
    cr_ex<-names(included_data)[which(!apply(!is.na(included_data[svalue(gss):svalue(gse),]),2,all))]
    t1<-data.frame(cr_ex)
    names(t1)<-"Excluded because of lacking data in that period"
    gt1[,]<-t1
  }
  
  # Show the table 1 and table 2 at the bottom of the window
  gp5_option <- gpanedgroup(container = gp_option)
  gt1<-gtable(t1,container = gp5_option)
  gt2<-gtable(t2,container = gp5_option)


  # Callback function on "go" button clicked
  addHandlerClicked(gbstart,function(h,...){
    # new data is the data in selected time range
    new_data<<-included_data[svalue(gss):svalue(gse),which(apply(!is.na(included_data[svalue(gss):svalue(gse),]),2,all))]
    # distance is 1 - corelation
    distance <<- 1 - cor(new_data)
    
    # clustering by selected method
    if(svalue(gclumethod,index = T)==1){
      clu <<- pam(distance, svalue(groupnum), diss = TRUE)
    } else if (svalue(gclumethod,index = T)==2){
      clu <<- agnes(distance, diss = TRUE)
    }
    
    # draw a plot
    plot(clu)
    
    # print the result
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

# the call back function of "OK" button in former window
gfileOK = gbutton("OK",container = gw_openfile,handler = function(h,...){
  filename<<-svalue(gfn)
  origin_data <<- read.csv(filename)
  date_format<<-svalue(gdateformat,index = T)
  ignore_second<<-svalue(gignoresecond,index = T)
  dispose(gw_openfile)
  # Call stage 2 here
  stage2()
})



