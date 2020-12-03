set.seed(1)
x1 <- runif(100)
x2 <- 0.5*x1+rnorm(100)/10
y2 <- 2*x1+0.3*x2+rnorm(100)+2
y2

cor(x1,x2)
plot(x1,x2)

fit <- lm(y2~x1+x2)
summary(fit)

fit1 <- lm(y2~x1)
summary(fit1)

fit2 <- lm(y2~x2)
summary(fit2)

x1=c(x1, 0.1)
x2=c(x2, 0.8)
y2=c(y2,6)
fit <- lm(y2~x1+x2)
summary(fit)
fit1 <- lm(y2~x1)
summary(fit1)
fit2 <- lm(y2~x2)
summary(fit2)

par(mfrow=c(2,2)) 
plot(fit)

plot(fit1)

plot(fit2)

library(MASS)
colnames(Boston)
attach(Boston)
Boston


fk1 <- lm( crim~zn,data=Boston) 
fk2 <- lm( crim~indus,data=Boston) 
fk3 <- lm( crim~chas,data=Boston) 
fk4 <- lm( crim~nox,data=Boston) 
fk5 <- lm( crim~rm,data=Boston)
fk6 <- lm( crim~age,data=Boston)
fk7 <- lm( crim~dis,data=Boston)
fk8 <- lm( crim~rad,data=Boston)
fk9 <- lm( crim~tax,data=Boston)
fk10 <- lm( crim~ptratio,data=Boston) 
fk11 <- lm( crim~black,data=Boston)
fk12 <- lm( crim~lstat,data=Boston) 
fk13 <- lm( crim~medv,data=Boston) 
summary(fk3)

fk <-lm( crim~.,data=Boston) 
summary(fk)

(C)
data(Boston) 
table1<-Map(summary,(Map(function(x)lm(crim~x,data=Boston),Boston[,-1]))) 
a=data.frame(13,2) 

for(i in 1:13) 
  {a[i,1]<-Map(function(x)lm(crim~x,data=Boston),Boston[,-1])[[i]]$coef[1] 
   a[i,2]<-table1[[i]]$coefficients[7] 
   a[i,3]<-Map(function(x)lm(crim~x,data=Boston),Boston[,-1])[[i]]$coef[2] 
   a[i,4]<-table1[[i]]$coefficients[8] 
   } 

colnames(a)<-c("b_0","p-value","b_1","p-value") 
rownames(a)<-colnames(Boston)[-1] 
a


table2<-cbind(a[,3],lm(crim~.,data=Boston)$coef[-1]) 
plot(table2,col=2,xlab = "simple",ylab = "multi") 

##(D)

table2<-Map(summary,(Map(function(x)lm(crim~poly(x,3),data=Boston),Boston[,c(-1,-4,-9)]))) 
a2<-data.frame(11,8) 
for(i in 1:11)
{a2[i,1]<-Map(function(x)lm(crim~poly(x,3),data=Boston),Boston[,c(-1,-4, -9)])[[i]]$coef[1] 
  a2[i,2]<-table2[[i]]$coefficients[13] 
  a2[i,3]<-Map(function(x)lm(crim~poly(x,3),data=Boston),Boston[,c(-1,-4, -9)])[[i]]$coef[2] 
  a2[i,4]<-table2[[i]]$coefficients[14] 
  a2[i,5]<-Map(function(x)lm(crim~poly(x,3),data=Boston),Boston[,c(-1,-4, -9)])[[i]]$coef[3] 
  a2[i,6]<-table2[[i]]$coefficients[15] 
  a2[i,7]<-Map(function(x)lm(crim~poly(x,3),data=Boston),Boston[,c(-1,-4, -9)])[[i]]$coef[4] 
  a2[i,8]<-table2[[i]]$coefficients[16] 
  } 
colnames(a2)<-c("b_0","p-value","b_1","p-value","b_2","p-value","b_3","p-value") 
rownames(a2)<-colnames(Boston)[c(-1,-4,-9)] 
a2 






#library(MASS)
#a = Map(function(x)summary(x),Map(function(x)lm(crim~x,Boston),Boston[,-1]))
#b = Map(function(x)a[[x]]$coefficients,c(1:ncol(Boston[-1])))
#c = Map(function(x)names(a[x]),c(1:ncol(Boston[-1])))
#names(b)<-c;b
