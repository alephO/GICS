if(!require("Rblpapi")){
  install.packages("Rblpapi",dep=TRUE)
}
con <- blpConnect()     # automatic if option("blpAutoConnect") is TRUE

spx <- bdh(securities = "SPX Index", 
           fields = "PX_LAST", 
           start.date = as.Date("2013-03-01"))

spx_ndx <- bdh(securities = c("SPX Index","NDX Index"), 
               fields = "PX_LAST",
               start.date = as.Date("2013-03-01"), 
               include.non.trading.days = TRUE)

monthlyOptions <- structure(c("ACTUAL", "MONTHLY"),
                            names = c("periodicityAdjustment",
                                      "periodicitySelection"))
spx_ndx_monthly <- bdh(securities = c("SPX Index","NDX Index"), 
                       fields = "PX_LAST",
                       start.date = as.Date("2012-01-01"), 
                       options = monthly.options)

goog_ge_div <- bdh(securities = c("GOOG Equity","GE Equity"),
                   fields = c("PX_LAST","CF_DVD_PAID"), 
                   start.date = as.Date("2012-11-01"))

goog_ge_px <- bdp(securities = c("GOOG Equity","GE Equity"),
                  fields = c("PX_LAST","DS002"))