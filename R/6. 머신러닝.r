########## 데이터 나누기
app=read.csv("C:/Users/JANG/Documents/DUKSUNG/전공/정보통계학과/SENIOR/다변량 및 빅데이터분석/논문/prep_googleapp_use.csv")
app_num=app[,4:8]
app=app[,3:9]
app_z=scale(app_num)

set.seed(12345)
V = 2
n =  NROW(app_num)
id = sample(1:V, n, prob = c(0.8,0.2), replace = T) # Partitioning 7:3
ii = which(id==1)
app2.train=app[ii,]
app2.test=app[-ii,]
app.train = app_num[ii,]
app.test  = app_num[-ii,]
app3.train=app_z[ii,]
app3.test=app_z[-ii,]

library(randomForest)
##### grow trees
set.seed(1234)
fit = randomForest(Rating~., data=app.train, ntree=100, mtry=4, importance=T)
fit2=randomForest(Rating~., data=app2.train, ntree=100, mtry=5, importance=T)
fit3=randomForest(Rating~., data=app3.train, ntree=100, mtry=4, importance=T)

plot(fit, type="l")
legend(80, 0.5, legend=c("OOB: overall", "OOB:0's", "OOB:1's"), lty=c(1:3), col=c(1:3))
importance(fit)

#prediction
pred = predict(fit, newdata=app.test) ######## number 변수만
pred2=predict(fit2, newdata=app2.test)  ###### 모든 변수

new.apptest=cbind(app.test$Rating, pred, pred2)

head(new.apptest)
tail(new.apptest)
### 예측평균제곱오차
mean((app.test$Rating-pred)^2)
mean((app2.test$Rating-pred2)^2)


### Prediction

pred = predict.boosting(fit, newdata=german)
cutoff = 0.5
yhat = ifelse(pred$prob[,2] > cutoff, 1, 0)
ctable = table(german$y, yhat, dnn=c("Actual", "Predicted")); ctable #classification table

set.seed(12345)
V = 2
n =  NROW(app_num)
id = sample(1:V, n, prob = c(0.8,0.2), replace = T) # Partitioning 7:3
ii = which(id==1)
app.train = app_num[ii,]
app.test  = app_num[-ii,]

###### 1. xgboost 이용
library(xgboost)
app_num=app[,c("Rating", "Reviews", "Size", "Installs", "Price")]

train_y=app.train[,'Rating']
train_x=app.train[, names(app.train)!='Rating']

test_y=app.test[,'Rating']
test_x=app.test[, names(app.test)!='Rating']
dtrain = xgb.DMatrix(data =  as.matrix(train_x), label = train_y )
dtest = xgb.DMatrix(data =  as.matrix(test_x), label = test_y)

model=xgboost(data=dtrain, max.depth=4, nfold=7, nround=3000, early_stopping_rounds=10,  gamma=7)

pred=predict(model, dtest)
pred=round(pred, digits=1)
table(pred, app.test$Rating)
result=cbind(app.test$Rating, pred)
result
head(result)
tail(result)
mean((app.test$Rating-pred)^2)

