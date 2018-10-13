#Dichotomous 
# Principle of Logistic Regression 
  # MLE  - Maximum Likelihood Estimation
# 6 Steps
#1. Calculate Log Likelihood ratio
#2. Explore McFadden Value
#3. Explore Summary table for statistical significance of variables
#4. Investigate the log odds ratio - MOST IMPORTANT
#5. Investigate the confusion matrix
#6. Explore the AUC of the ROC curve.
library(lmtest)
mod <- glm(Loyalty~Brand+Product+Shopping,data=nm,family="binomial")
### Step 1 ####
lrtest(mod)
### Step 2 ####
library(pscl)
pR2(mod)
#### Step 3 #####
summary(mod)
#### Step 4 ####
a <- exp(coef(mod))/(exp(coef(mod))+1)
a
### Step 5 ####
# e^z/ e^z+1
# z = b0 + b1x1 + b2x2 + ... +bnxn
prd <- predict(mod,newdata=nm,type='response')
head(prd)
cls <- ifelse(prd>0.5,1,0)
table(Actual = nm$Loyalty,Predicted = cls)
caret::confusionMatrix(cls,nm$Loyalty)
### Step 6 ###
library(pROC)
pred <- prediction(cls,nm$Loyalty)
roc_curve <- performance(pred,measure = "tpr",x.measure = "fpr")
plot(roc_curve)
abline(0,1)
auc <- performance(pred,"auc")
auc@y.values[[1]]

