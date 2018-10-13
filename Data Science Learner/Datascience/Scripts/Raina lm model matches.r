library(tidyr)
del<- fread("C:/users/Anu Joe/Documents/deliveries.csv")
csk <- del[batting_team=="Chennai Super Kings",]
csk[,sum(batsman_runs),by=batsman][order(-V1)]
raina <- csk[batsman=='SK Raina',sum(batsman_runs),by=.(match_id,batsman)] %>% 
  spread(batsman,V1) %>% 
  data.table()

head(raina)
mat <- fread("C:/users/Anu Joe/Desktop/Anu data science/matches (1).csv")
head(mat)
mat_sub<- mat[,.SD,.SDcols=c("id","city","team1","team2","toss_decision","venue")]
library(dplyr)
names(mat_sub)[1]<- "match_id"
mat_mer <- inner_join(raina,mat_sub,by=c("match_id"))
head(mat_mer)
names(mat_mer)[2] <- "Raina"
mod <- lm(Raina~.,data=mat_mer[,-1])
summary(mod)
