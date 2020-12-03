p <- function(x1,x2)
{ z <- exp(-6 + 0.05*x1 + 1*x2); return( round(z/(1+z),2))}
p(40,3.5)

data(Weekly)
summary(Weekly)
plot(Weekly)

fit <- glm(Direction ~.-Today -Year, data=Weekly, family="binomial")
summary(fit)

#(c)
glm.probs <- predict(fit,type="response")
class.glm <- car::recode(glm.probs,"0:0.499999999='down';0.5:1='Up'")
table(class.glm ,Weekly$Direction)

54+557/(54+48+430+557)
#(d)
attach(Weekly)
train <- (Year<2009)
test <-  Weekly[!train ,] 
fit1 <- glm(Direction ~ Lag2, data=Weekly, subset=train, family="binomial")
glm.probs <- predict(fit1, type="response", newdata=test)
class.glm1 <- car::recode(glm.probs,"0:0.499999999='down';0.5:1='Up'")
table(class.glm1 ,test$Direction)
(9+56)/(9+5+34+56)

#e
train <- (Year<2009)
test <-  Weekly[!train ,] 
fit1 <- lda(Direction ~ Lag2, data=Weekly, subset=train, family="binomial")
glm.probs <- predict(fit1, type="response", newdata=test)
table(glm.probs$class,test$Direction)
#f
fit1 <- qda(Direction ~ Lag2, data=Weekly, subset=train, family="binomial")
glm.probs <- predict(fit1, type="response", newdata=test)
table(glm.probs$class,test$Direction)
#g
library(class)
train.k = Weekly[train, c("Lag2", "Direction")]
knn.pred = knn(train=data.frame(train.k$Lag2), test=data.frame(test$Lag2), cl=train.k$Direction, k=1)
hah <- table(test$Direction, knn.pred)
hah
(21+31)/(21+22+30+31)

##(i)
# Logistic regression with Lag2:Lag1
fit.glm3 <- glm(Direction ~ Lag2:Lag1, data = Weekly, family = binomial, subset = train)
probs3 <- predict(fit.glm3, Weekly.20092010, type = "response")
pred.glm3 <- rep("Down", length(probs3))
pred.glm3[probs3 > 0.5] = "Up"
table(pred.glm3, Direction.20092010)

mean(pred.glm3 == Direction.20092010)

# LDA with Lag2 interaction with Lag1
fit.lda2 <- lda(Direction ~ Lag2:Lag1, data = Weekly, subset = train)
pred.lda2 <- predict(fit.lda2, Weekly.20092010)
mean(pred.lda2$class == Direction.20092010)

# QDA with sqrt(abs(Lag2))
fit.qda2 <- qda(Direction ~ Lag2 + sqrt(abs(Lag2)), data = Weekly, subset = train)
pred.qda2 <- predict(fit.qda2, Weekly.20092010)
table(pred.qda2$class, Direction.20092010)
mean(pred.qda2$class == Direction.20092010)

#knn
set.seed(1)

results <- data.frame(k=1:50, acc=NA)
for(i in 1:50){
  knn.pred = knn(train=data.frame(train.g$Lag2), test=data.frame(testset$Lag2), cl=train.g$Direction, k=i)
  cm <- table(testset$Direction, knn.pred)
  acc <- (cm["Down", "Down"] + cm["Up", "Up"])/sum(cm)
  results$acc[i] <- acc
}

plot(x=results$k, y=results$acc, type="l", xlab="K", ylab="accuracy", ylim=c(.4,.65))







#11
#(a)
Auto$mpg01 <- ifelse(Auto$mpg>median(Auto$mpg), "1", "0")
#(b)
library(ggplot2)
str(Auto)
ggplot(data = Auto, mapping = aes(x = mpg01, y=cylinders)) + 
  geom_boxplot()+
  coord_flip()
ggplot(data = Auto, mapping = aes(x = mpg01, y=displacement)) + 
  geom_boxplot()+
  coord_flip()

ggplot(data = Auto, mapping = aes(x = mpg01, y=horsepower)) + 
  geom_boxplot()+
  coord_flip()

ggplot(data = Auto, mapping = aes(x = mpg01, y=weight )) + 
  geom_boxplot()+
  coord_flip()

ggplot(data = Auto, mapping = aes(x = mpg01, y=acceleration )) + 
  geom_boxplot()+
  coord_flip()

ggplot(data = Auto, mapping = aes(x = mpg01, y=year )) + 
  geom_boxplot()+
  coord_flip()
ggplot(data = Auto, mapping = aes(x = mpg01, y=origin)) + 
  geom_boxplot()+
  coord_flip()


#c
rows <- sample(x=nrow(Auto), size=.75*nrow(Auto))
train <- Auto[rows, ]
test <- Auto[-rows, ]
#d
lda.fit <- lda(mpg01 ~ displacement+horsepower+weight+acceleration+year+cylinders+origin, data=train)
lda.pred <- predict(lda.fit, test)
table(test$mpg01, lda.pred$class)
#e
qda.fit <- qda(mpg01 ~ displacement+horsepower+weight+acceleration+year+cylinders+origin, data=train)
qda.pred <- predict(qda.fit, test)
table(test$mpg01, qda.pred$class)
#f
fit1 <- glm(as.factor(mpg01) ~ displacement+horsepower+weight+acceleration+year+cylinders+origin, data=train, family="binomial")
glm.probs <- predict(fit1, type="response", newdata=test)
class.glm1 <- car::recode(glm.probs,"0:0.499999999='down';0.5:1='Up'")
table(class.glm1 ,test$mpg01)
#g
library(class)

sel.variables <- which(names(trainset)%in%c("mpg01", "displacement", "horsepower", "weight", "acceleration", "year", "cylinders", "origin"))

set.seed(1)
accuracies <- data.frame("k"=1:10, acc=NA)
for(k in 1:10){
  knn.pred <- knn(train=trainset[, sel.variables], test=testset[, sel.variables], cl=trainset$mpg01, k=k)
  
  # test-error
  accuracies$acc[k]= round(sum(knn.pred!=testset$mpg01)/nrow(testset)*100,2)
}

accuracies
