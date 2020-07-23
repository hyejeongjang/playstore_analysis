####### 주성분 분석을 위해 사용할 수치형 변수들(Rating, Reviews, Installs, Price)만을 이용해서 분석한다.
app=read.csv("prep_googleapp_use.csv")
app_num=app[,c("Rating", "Reviews", "Size", "Installs", "Price")]

# 상관행렬, 평균행렬, 공분산행렬 구한다.
m=colMeans(app_num)
m
S=cov(app_num)
S
s2=cov(app_num)
s2
r=cor(app_num)
r
r2=cor(app_num)
r2
eigen(S)
eigen(r)

p_cor=princomp(app_num, cor=TRUE)
summary(p_cor)
p_cor$loadings
p_cor2=princomp(r, cor=TRUE)
summary(p_cor2)
p_cor2$loadings

# 그래프
library(graphics)
screeplot(p_cor2, npcs=5, type="lines", main="scree plot-cov")
biplot(p_cor2)