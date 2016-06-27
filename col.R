#filename<-file.choose()
filename <- "1.csv"
origin_data <- read.csv(filename)
mkt <- origin_data$MRKT
plot(market)