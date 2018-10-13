### Model - Linear Regression
## Pre requisites
# 1. No Auto corelation- Meaning no correlation of the error terms
      # identified by the durbin-watson test 
# 2. No Multicolinearity- Meaning the independant variables must not be
#    related to each other, no correlation
       #identified by the cor table or the corrplot visualization and 
       # the VIF test.
# 3. Should exhibit Homoskedasticity- meaning the error terms must have
#    constant variance 
       # identified by the funnel plot
# 4. Error terms must be normally distributed 
       # identified by the QQ plot
# 5. Relationship between the independant and the dependant must be linear
#    and additive. 
      # identified by EDA and plotting
library(dplyr)
library(ggplot2)
library(gridExtra)


a <- ggplot(hp,aes(Living.Area,Price)) + 
  geom_point() + 
  geom_smooth(method='lm',se=F) +
  facet_wrap(~Bedrooms)
b <- ggplot(hp,aes(Living.Area,Price,col=factor(Bedrooms))) + 
  geom_point(alpha=0.2) + 
  geom_smooth(method='lm',se=F)
do.call(grid.arrange,list(a,b))

hp_no6 <- hp %>% filter(Bedrooms!=6)
mod <- lm(Price~.,data=hp_no6)
mod <- lm(Price~.,data=hp[,"Bedrooms"!= c(2,3) ])
summary(mod)
summary(hp_no6)
model <- lm(Price~Bedrooms,data=hp)
summary(model)

with(hp,plot(Living.Area,Price))
train <- hp[1:nrow(hp)*0.7,]
test <- hp[-(1:nrow(hp)*0.7),]
mod <- lm(Price~.,data=train)
summary(mod)
library(car)
### Checking for Autocorrelation
durbinWatsonTest(mod)
vif(mod)
par(mfrow=c(1,1))
plot(mod)

mod_trf <- lm(log(Price)~.,data=hp)
durbinWatsonTest(mod_trf)
vif(mod_trf)
par(mfrow=c(2,2))
plot(mod_trf)
summary(mod_trf)

p1 <- predict(mod,newdata=train)
p2 <- predict(mod, newdata=test)
library("Metrics")
tr_rmse <- rmse(actual = train$Price,predicted = p1)
tes_rmse <- rmse(actual = test$Price,predicted = p2)
(tes_rmse-tr_rmse)/tr_rmse
fitted(mod)

mod1 <- lm(Price~.,data=hp)
head(fitted(mod1))
head(residuals(mod1))

View(data.frame(Price = hp$Price,
                Regression = fitted(mod1),
                Error = residuals(mod1)))
