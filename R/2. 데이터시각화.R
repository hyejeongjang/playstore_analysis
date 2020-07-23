app=read.csv("prep_googleapp_use.csv")
head(app)
str(app)
initial=read.csv("googleplaystore.csv")
ins=app$Installs
ins=as.double(ins)
app$Installs=ins

### Category

# TREEMAP
library(treemap)
treemap(app,
        index="Category",
        vSize="Installs",
        vColor="Installs",
        type="index")

# 빈도 그래프-상위 33개
#df1 = df.groupby(['Category'])['Installs'].sum().sort_values(ascending = False).reset_index()
library(data.table)
data=data.table(app)
dt1=data[,sum(Installs), by="Category"]
setDF(dt1)
dt1=dt1[order(dt1$V1),]
dt2=dt1[23:33,]
ggplot(app, aes(x=Category), fill="blue")+
  geom_bar(alpha=.3, fill="blue")+coord_flip() 

# 빈도 그래프- 상위 11개
library(ggplot2)
ggplot(dt2, aes(Category, V1), color="blue")+
  geom_bar(stat='identity', fill="")+coord_flip() 

### Rating
ggplot(app, aes(x=Rating), fill="blue")+
  geom_density(alpha=.3, fill="purple")

### Reviews
ggplot(app, aes(x=Reviews), fill="blue")+
  geom_histogram(alpha=.3, fill="purple")
max(app$Reviews) # 44893888
min(app$Reviews) #0

### Size
ggplot(app, aes(x=Size), fill="blue")+
  geom_histogram(alpha=.3, fill="blue")
ggplot(app, aes(x=Size), fill="blue")+
  geom_density(alpha=.3, fill="blue")
ggplot(app, aes(x=Size), fill="blue")+
  geom_histogram(alpha=.3, fill="blue")

### Installs
ggplot(app, aes(x=Installs), fill="blue")+
  geom_histogram(alpha=.3, fill="orange")

### Price
ggplot(app, aes(x=Price), fill="blue")+
  geom_histogram(alpha=.3, fill="orange")

### Content.Rating
ggplot(app, aes(x=Content.Rating), fill="blue")+
  geom_bar(alpha=.3, fill="blue")+coord_flip() 

### 기초통계량
summary(app)

# 상관행렬
app_num=app[,c("Rating", "Reviews", "Size", "Installs", "Price")]
co=cor(app_num)
co
plot(app_num)

library(corrplot)
corrplot(co, method="circle")

corrplot(co,method="shade", # 색 입힌 사각형
         addshade="all", # 상관관계 방향선 제시
         # shade.col=NA, # 상관관계 방향선 미제시
         tl.col="red", # 라벨 색 지정
         tl.srt=30, # 위쪽 라벨 회전 각도
         diag=FALSE, # 대각선 값 미제시
         addCoef.col="black", # 상관계수 숫자 색
         order="FPC" # "FPC": First Principle Component
         # "hclust" : hierarchical clustering
         # "AOE" : Angular Order of Eigenvectors
)