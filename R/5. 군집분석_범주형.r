library(tidyverse)
library(ca)
library(factoextra)
library(cluster)
library(igraph)

data=read.csv("C:/Users/JANG/Documents/DUKSUNG/����/��������а�/SENIOR/�ٺ��� �� �����ͺм�/��/prep_googleapp_use.csv", sep=",")
app %>% head

########### ���� 70% ī�װ� 
cat=app %>% 
  group_by(Category) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  mutate(per=round(cumsum(n)/sum(n), 2)) %>% 
  mutate(pper=round(n/sum(n), 2)) %>% 
  filter(per<0.7)

############# ī�װ��� ���ɴ� ������������
cat.rat=app %>% 
  filter(Category == cat$Category) %>% 
  select(Category, Content.Rating) %>% 
  mutate(n=1) %>% 
  group_by(Category, Content.Rating) %>% 
  summarise(n=n()) %>% 
  spread(Content.Rating, n)

############ ����ġ ����
cat.rat[is.na(cat.rat)]=0

# ������ ���������� �����
cat.rat=data.frame(cat.rat)
# ī�װ��� ������ �����
rownames(cat.rat)=cat.rat$Category
# ù��° �� ����
cat.rat=cat.rat[,-1]
cat.rat

################# �����м��� ���� ���� Ŭ������ ���� ã��
set.seed(123)
fviz_nbclust(cat.rat, FUN = kmeans, method = "wss")
fviz_nbclust(cat.rat, FUN = kmeans, method = "silhouette")

############## �����м�
set.seed(1234)
km.cr=kmeans(cat.rat, centers = 3, nstart = 25)
p2 <- fviz_cluster(km.cr, geom = c("point", "text"), data = cat.rat) + ggtitle("Category and Content.Rating made 3 cluster")
p2

############## �����м� ���� �׸�
res.dist <- dist(cat.rat, method = "euclidean")
res.hc <- hclust( d = res.dist, method = "ward.D2")
fviz_dend( res.hc, k = 3,  # Cut in four groups 
           cex = 0.5, # label size 
           k_colors = c("#2E9FDF", "#00AFBB", "#E7B800"), 
           color_labels_by_k = TRUE, # color labels by groups 
           rect = TRUE # Add rectangle around groups 
)
