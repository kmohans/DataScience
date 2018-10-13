install.packages("microbenchmark")
library(data.table)
hp.dt <- data.table(hp)
library(dplyr)
microbenchmark::microbenchmark(
  df = hp %>% filter(Bedrooms==3) %>% summarise(meanPrice = mean(Price)),
  dt= hp.dt[Bedrooms==3,mean(Price)],times = 100
)

hp.dt[,mean(Price),by=Bedrooms]
hp.dt[,.(AvgPrice = mean(Price)),by=.(Fireplace,Bedrooms)]
l <- sapply(hp,is.character)
library(tidyr)
hp.dt[,lapply(.SD,mean),keyby=Bedrooms,
      .SDcols=c("Price","Living.Area")]
hp.dt[,c(.N,lapply(.SD,mean)),keyby=Bedrooms,
         .SDcols=c("Price","Living.Area")][order(-N)]
hp.dt[,PPS := Price/Living.Area]
names(hp.dt)
hp.dt[,':='(PP_Age = Price/Age,
            PP_lot = Price/Lot.Size)]
hp.dt[,c("PP_Age","PP_lot"):= NULL]
