set.seed(1)
x <- rnorm(100)
y=2*x+rnorm(100)

a=lm(y~x+0)
summary(a)

b=lm(x~y+0)
summary(b)


set.seed(1)
x <- rnorm(100)

eps <- rnorm(100, mean = 0, sd = 0.5)

y <- -1+0.5*x+eps
length(y)

plot(x,y)

a=lm(y~x)
summary(a)



abline(a,col="blue")
legend(a,c("Least square", "Regression"))



plot(x, y)
abline(a, col = "green")
abline(-1, 0.5, col = "blue")
legend("topright", c("Least square", "Regression"), col = c("red", "blue"), lty = c(3, 3))


lm.fit=lm(y~x+I(x^2))
summary(lm.fit)












set.seed(1)
eps <- rnorm(100,mean=0,sd = 0.1)
x <- rnorm(100)
y <- -1 + 0.5 * x + eps
plot(x, y)
haha <- lm(y ~ x)
summary(haha)
abline(haha, col = "red")
abline(-1, 0.5, col = "blue")
legend("topright", c("Least square", "Regression"), col = c("red", "blue"), lty = c(3, 3))













set.seed(1)
eps <- rnorm(100,mean=0,sd = 1)
x <- rnorm(100)
y <- -1 + 0.5 * x + eps
plot(x, y)
haha1 <- lm(y ~ x)
summary(haha1)
abline(haha, col = "red")
abline(-1, 0.5, col = "blue")
legend("topright", c("Least square", "Regression"), col = c("red", "blue"), lty = c(3, 3))



confint(a)
confint(haha)
confint(haha1)
