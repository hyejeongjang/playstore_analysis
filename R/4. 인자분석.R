####### 인자 분석을 위해 사용할 수치형 변수들(Rating, Reviews, Installs, Price)만을 이용해서 분석한다.
app=read.csv("prep_googleapp_use.csv")
app_num=app[,c("Rating", "Reviews", "Size", "Installs", "Price")]
app_num[1:5,]

# 데이터 표준화
app_num=transform(app_num, 
                  z.Rating=scale(Rating), 
                  z.Reviews=scale(Reviews), 
                  z.Size=scale(Size), 
                  z.Installs=scale(Installs), 
                  z.Price=scale(Price))

app_num_z=app_num[,6:10]
head(app_num_z)

# Scree Plot
library(graphics)
prin=princomp(app_num_z)
screeplot(prin, npcs=5, type="lines", main="scree plot")
# -> 2번째 주성분에서 급격하게 꺾임, 인자 개수2개

# No Rotation=fact1
fact1=factanal(app_num_z, factors=2, rotation="none")
fact1

# Varimax Rotation=fact2
fact2=factanal(app_num_z, factors=2, scores="regression")
fact2

# Promax Rotation=fact3
fact3=factanal(app_num_z, factors=2, rotation="promax")
fact3

# 인자분석 결과 다이어그램
namevar=names(fact2$loadings)=c("Rating", "Reviews", "Size", "Installs", "Price")
plot(fact2$loadings[,1], fact2$loadings[,2], pch=16, xlab="factor1", ylab="factor2", main="factor pattern")
text(x=fact2$loadings[,1], y=fact2$loadings[,2], labels=namevar, adj=0)
abline(v=0, h=0)
