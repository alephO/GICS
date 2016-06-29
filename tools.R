require(Matrix)
require(graphics)
require(zoo)


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
  # actually it is not 01, however, we cannot go without days...
  return(as.yearmon(paste(s[1],s[2]),"%Y %m"))
}

get_end_date<-function(time_series){
  e<-end(time_series)
  return(as.yearmon(paste(e[1],e[2]),"%Y %m"))
}

yearmon_to_row<-function(bg,ym){
  yy<-as.numeric(format(ym,"%Y"))-as.numeric(format(bg,"%Y"))
  mm<-as.numeric(format(ym,"%m"))-as.numeric(format(bg,"%m"))
  return(12*yy+mm+1)
}