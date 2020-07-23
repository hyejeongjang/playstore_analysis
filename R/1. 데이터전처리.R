####### 데이터 전처리
## 1. 연속형 변수가 많이 없으므로, 숫자 변수들을 연속형 변수로 변환한다. -> 여러 숫자 변수에 있는 문자, 기호 제거
## 2. 중복된 의미가 있는 변수들 제거
## 3. 사용하기 애매한 변수들을 제거한다.

app=read.csv("googleplaystore.csv", header=TRUE)
str(app)

# App의 이름에서 na값이 있는지 확인한다.
sum(is.na(app$App))

# Android.ver(지원하는 안드로이드 버전 제거)
app=app[,1:12]

# Last.Updated 제거
app=app[,c(1:10, 12)]

# Type변수와 Price변수는 모두 어플의 가격을 나타내는 변수이므로 Type변수를 제거한다.
app=app[,c(1:6, 8,9,10,11)]
sum(is.na(app$Price)) #0

# Installs에서 ',' '+' 문자 제거해서 연속형 변수로 만든다
#app$Installs=as.numeric(gsub(",", app$Installs))
install=app$Installs
result= gsub("[,+\n]","", install) # 기호 제거
app$Installs=as.numeric(result) # 수치형 변수로 변환

# Size 변수에서 문자 제거
app$Size=gsub("M", "", app$Size)
app$Size=gsub("+", "", app$Size)
app3$Size=as.numeric(app3$Size) # 수치형 변수로 변환

# Genre변수와 Category 변수는 비슷한 성질을 가지고 있기 때문에, Genres 변수 제거
app=app[,c(1,2,3,4,5,6,7,8,10)]

# Rating의 NAN을 0으로 바꿔서 결측값 제거
i=1
for (i in 1:10841) {
  if (app$Rating[i]=="NaN") {
    app$Rating[i]=0
  }
}

# Price에서 $ 표시 없애기
app$Price=gsub("\\$", "", app$Price)

# Reviews에서 M을 100000으로 고치기
app$Reviews=gsub("M", "000000", app$Reviews)

# Varies with device 변수 제거
app3=subset(app2, Size!='Varies with device')

# 전처리 결과 새로운 파일에 저장
write.csv(app, "prep_googleapp.csv")