x <- 1:100000
y=sapply(x,function(x){1-((1-(1/x))^x)})
plot(x,y,xlab="n",ylab="Probability jth observation is in the bootstrap sample",log="x")


store=rep(NA, 100000) 
for(i in 1:100000) 
  { store[i]=sum(sample (1:100, rep=TRUE)==4) >0 
  }
mean(store)

set.seed(1)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
plot(x,y)
#c
library(boot)
set.seed(1)
Data <- data.frame(x, y)
fit.glm.1 <- glm(y ~ x)
cv.glm(Data, fit.glm.1)$delta[1]

fit.glm.2 <- glm(y ~ poly(x, 2))
cv.glm(Data, fit.glm.2)$delta[1]

fit.glm.3 <- glm(y ~ poly(x, 3))
cv.glm(Data, fit.glm.3)$delta[1]

fit.glm.4 <- glm(y ~ poly(x, 4))
cv.glm(Data, fit.glm.4)$delta[1]

#d
set.seed(666666666)
fit.glm.1 <- glm(y ~ x)
cv.glm(Data, fit.glm.1)$delta[1]

fit.glm.2 <- glm(y ~ poly(x, 2))
cv.glm(Data, fit.glm.2)$delta[1]

fit.glm.3 <- glm(y ~ poly(x, 3))
cv.glm(Data, fit.glm.3)$delta[1]

fit.glm.4 <- glm(y ~ poly(x, 4))
cv.glm(Data, fit.glm.4)$delta[1]

summary(fit.glm.4)

#9
library(MASS)
attach(Boston)
str(Boston)
X <- mean(medv)
X
dim(Boston)
se.hat <- sd(medv) / sqrt(506)
se.hat

#c
boot.fn <- function(data,i) {
  mu <- mean(data[i])
  return (mu)
}
boot(medv, boot.fn, 100)

#d
t.test(medv)

aaa <- c(22.532-2*0.4050308,22.532+2*0.4050308)
aaa

#e
med.hat <- median(medv)
med.hat
#f
boot.fn <- function(data, i) {
  mu <- median(data[i])
  return (mu)
}
boot(medv, boot.fn, 100)
#g
percent10 <- quantile(medv, 0.1)
percent10
#h
boot.fn <- function(data, i) {
  mu <- quantile(data[i],0.1)
  return (mu)
}
boot(medv, boot.fn, 100)

