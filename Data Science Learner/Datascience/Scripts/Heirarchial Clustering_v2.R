# Heirarchial clustering
# Dendrogram
# 1. Bottom up
# 2. Top down
df <- data.frame(x=runif(10,0,5),y=runif(10,0,5),z=1:10)
with(df,plot(x,y))
with(df,text(x=x,y=y,label=z,pos = 2))
hc <- hclust(d=dist(df))
plot(hc)
abline(h=7,col="red")
cluster  <- cutree(hc,h=7)
cluster
