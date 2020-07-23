library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(cluster)    # for agnes function

########### data
app=read.csv("C:/Users/JANG/Documents/DUKSUNG/전공/정보통계학과/SENIOR/다변량 및 빅데이터분석/논문/prep_googleapp_use.csv")
app_num=app[,c("Rating", "Reviews", "Size", "Installs", "Price")]

app_num=scale(app_num)
head(app_num)

############## 계층적 방법
dist <- dist(app_num, method = "euclidean")
distance <- get_dist(app_num)
# fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

hc1 <- agnes(dist, method = "single" ) #single Linkage
hc2 <- agnes(dist, method = "complete" ) #Complete Linkage
hc3 <- agnes(dist, method = "average" ) #average Linkage
hc4 <- agnes(dist, method = "ward" ) #Ward's minimum variance method

########### kmeans 방법 이용한 cluster
app.kmeans <- kmeans(app_num, centers = 4, nstart = 25)
str(app.kmeans)
p1 <- fviz_cluster(app.kmeans, geom = "point", data = app_num) + ggtitle("k = 4")
p1

############## cluster 표, 시각화
app_clus=cbind(app_num, app.kmeans$cluster)
table(app$Installs, app.kmeans$cluster) # 앱 다운로드수와 군집
table(app$Rating, app.kmeans$cluster) # 앱 별점수와 군집
table(app$Content.Rating, app.kmeans$cluster)

set.seed(123)
fviz_nbclust(app_num, FUN = kmeans, method = "wss")
fviz_nbclust(app_num, FUN = kmeans, method = "silhouette")
