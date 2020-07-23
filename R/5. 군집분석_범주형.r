library(tidyverse)
library(ca)
library(factoextra)
library(cluster)
library(igraph)

data=read.csv("C:/Users/JANG/Documents/DUKSUNG/전공/정보통계학과/SENIOR/다변량 및 빅데이터분석/논문/prep_googleapp_use.csv", sep=",")
app %>% head

########### 상위 70% 카테고리 
cat=app %>% 
  group_by(Category) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  mutate(per=round(cumsum(n)/sum(n), 2)) %>% 
  mutate(pper=round(n/sum(n), 2)) %>% 
  filter(per<0.7)

############# 카테고리와 연령대 데이터프레임
cat.rat=app %>% 
  filter(Category == cat$Category) %>% 
  select(Category, Content.Rating) %>% 
  mutate(n=1) %>% 
  group_by(Category, Content.Rating) %>% 
  summarise(n=n()) %>% 
  spread(Content.Rating, n)

############ 결측치 제거
cat.rat[is.na(cat.rat)]=0

# 데이터 프레임으로 만들기
cat.rat=data.frame(cat.rat)
# 카테고리를 행으로 만들기
rownames(cat.rat)=cat.rat$Category
# 첫번째 열 제거
cat.rat=cat.rat[,-1]
cat.rat

################# 군집분석을 위한 최적 클러스터 개수 찾기
set.seed(123)
fviz_nbclust(cat.rat, FUN = kmeans, method = "wss")
fviz_nbclust(cat.rat, FUN = kmeans, method = "silhouette")

############## 군집분석
set.seed(1234)
km.cr=kmeans(cat.rat, centers = 3, nstart = 25)
p2 <- fviz_cluster(km.cr, geom = c("point", "text"), data = cat.rat) + ggtitle("Category and Content.Rating made 3 cluster")
p2

############## 군집분석 나무 그림
res.dist <- dist(cat.rat, method = "euclidean")
res.hc <- hclust( d = res.dist, method = "ward.D2")
fviz_dend( res.hc, k = 3,  # Cut in four groups 
           cex = 0.5, # label size 
           k_colors = c("#2E9FDF", "#00AFBB", "#E7B800"), 
           color_labels_by_k = TRUE, # color labels by groups 
           rect = TRUE # Add rectangle around groups 
)
