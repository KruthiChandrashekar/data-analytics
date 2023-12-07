#a scatterplot matrix which includes all of the variables
pairs(Auto, cex = 0.2)

#b correlations between the variables
x <- subset(Auto, select = -name)
cor(x)

#c
fit <- lm(mpg ~ ., data = x)
summary(fit)

#d trend and high lev at a point
par(mfrow = c(2, 2))
plot(fit, cex = 0.2)

#e
summary(lm(mpg ~ . + weight:horsepower, data = x))
summary(lm(mpg ~ . + acceleration:horsepower, data = x))
summary(lm(mpg ~ . + cylinders:weight, data = x))

#f log is more linear
par(mfrow = c(2, 2))
plot(Auto$horsepower, Auto$mpg, cex = 0.2)
plot(log(Auto$horsepower), Auto$mpg, cex = 0.2)
plot(sqrt(Auto$horsepower), Auto$mpg, cex = 0.2)
plot(Auto$horsepower ^ 2, Auto$mpg, cex = 0.2)

x <- subset(Auto, select = -name)
x$horsepower <- log(x$horsepower)
fit <- lm(mpg ~ ., data = x)
summary(fit)
par(mfrow = c(2, 2))
plot(fit, cex = 0.2)

