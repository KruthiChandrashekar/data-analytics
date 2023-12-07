#a relationship between the predictor and the response
library(ISLR2)
fit <- lm(mpg ~ horsepower, data = Auto)
summary(fit)

predict(fit, data.frame(horsepower = 98), interval = "confidence")
predict(fit, data.frame(horsepower = 98), interval = "prediction")

#b least square regression line
plot(Auto$horsepower, Auto$mpg, xlab = "horsepower", ylab = "mpg")
abline(fit)

#c shows non-linear relationship
par(mfrow = c(2, 2))
plot(fit, cex = 0.2)
