hf <- hflights::hflights
dim(hf)
head(hf)
tail(hf)
summary(hf)
str(hf)
colMeans(is.na(hf))
library(dplyr)
select(hf,Year:DayOfWeek,
       ActualElapsedTime,
       contains("taxi"),
       ends_with("delay"))
filter(hf,ArrDelay>0)
names(hf)
mutate(hf,TotDelay = ArrDelay+DepDelay)

summarise(hf, avgdelay = mean(ArrDelay+DepDelay,na.rm = T),
          total= n(),
          uniquecarriercount = n_distinct(UniqueCarrier),
          median_delay = median(ArrDelay+DepDelay,na.rm = T))
unique(hf$UniqueCarrier)

hf1 <- group_by(hf,UniqueCarrier,Month)
h2 <- summarise(hf1,avgdelay = mean(ArrDelay+DepDelay,na.rm = T),
          total= n(),
          uniquecarriercount = n_distinct(UniqueCarrier),
          median_delay = median(ArrDelay+DepDelay,na.rm = T))
filter(h2,avgdelay<0)


hf %>% group_by(UniqueCarrier,Month) %>% 
  summarise(avgdelay = mean(ArrDelay+DepDelay,na.rm = T),
            total= n(),
            uniquecarriercount = n_distinct(UniqueCarrier),
            median_delay = median(ArrDelay+DepDelay,na.rm = T)) %>% 
  filter(avgdelay<0)
head(hf)
hf %>% 
  mutate(TotDelay=ArrDelay+DepDelay) %>% 
  filter(TotDelay<0) %>% group_by(UniqueCarrier) %>%
  summarise(avgdelay = mean(TotDelay,na.rm=T),count= n()) %>% 
  filter(avgdelay <0) %>% arrange((avgdelay)) %>% View()

library(tidyr)
hf %>% group_by(UniqueCarrier,Month) %>% 
  summarise(cancelled_ratio = mean(Cancelled==1)) %>%
   spread(UniqueCarrier,cancelled_ratio) %>% gather(key = "UniqueCarrier" ,value="avgdelay",-Month)
head(hf)

hf %>% group_by(UniqueCarrier,Dest) %>% 
  summarise(count = n()) %>% arrange(-count) %>% slice(1:3)

hf %>% group_by(UniqueCarrier,Origin,Dest,Month) %>% 
  summarise(count = n()) %>% arrange(-count) %>% slice(1:5)

