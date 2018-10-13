### Load Libraries
library(magrittr)
library(data.table)
library(rpart)
library(rpart.plot)
### reading the data
dev <- fread("/Users/mac/Google Drive/Data Science Learner/Datascience/Datsets/DEV_SAMPLE.csv")
### DT CART Model
dev <- dev[,-1]
cont <- rpart.control(minsplit = 90,minbucket = 30,cp=0,xval=10)
tree <- rpart(Target~.,data=dev,control = cont,method = "class")
## Print and plot the tree
print(tree)
plot(tree)
rpart.plot(tree,cex=0.6)
caret::confusionMatrix(pred,dev$Target)
pred <- predict(tree,newdata=dev,type="class")
dev$pred = pred
table(Actual= dev$Target,predicted = pred)
cp <-printcp(tree)
ptree <- prune(tree,0.00404858,"CP")
rpart.plot(ptree,cex=0.6)

dev[Holding_Period <10.5 & Occupation =="SELF-EMP",]
treeProb <- predict(ptree,newdata=dev,type="prob")
treePred <- predict(ptree,newdata=dev,type="class")
head(treeProb)
head(dev)
g1 <- floor(treeProb[,2]+0.9)

caret::confusionMatrix(data=dev$Target,treePred)
caret::confusionMatrix(data=dev$Target,g1)
mean(dev$Target==1) * 100

library("ROCR")
p <- prediction(labels = dev$Target,predictions = treeProb[,2])
perf <- performance(p,measure="tpr",x.measure = "fpr")
plot(perf)
auc <- performance(p,"auc")
auc@y.values[[1]]


View(iris)
iris.tree <- rpart(Species~.,data=iris,control=rpart.control(minsplit = 9,minbucket = 3,cp=0,xval=10))
print(iris.tree)
rpart.plot(iris.tree)

iris.pred <- predict(iris.tree,newdata=iris,type="class")
iris.prob <- predict(iris.tree,newdata=iris,type="prob")

caret::confusionMatrix(iris.pred,iris$Species)


