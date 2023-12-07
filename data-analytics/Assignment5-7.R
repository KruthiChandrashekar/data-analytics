library(ISLR2)
data <- Auto
data$high_mpg <- as.factor(as.numeric(data$mpg > median(data$mpg)))

set.seed(42)
costs <- 10^seq(-4, 3, by = 0.5)
results <- list()
f <- high_mpg ~ displacement + horsepower + weight
results$linear <- tune(svm, f, data = data, kernel = "linear", 
                       ranges = list(cost = costs))
summary(results$linear)

results$polynomial <- tune(svm, f, data = data, kernel = "polynomial", 
                           ranges = list(cost = costs, degree = 1:3))
summary(results$polynomial)

results$radial <- tune(svm, f, data = data, kernel = "radial", 
                       ranges = list(cost = costs, gamma = 10^(-2:1)))
summary(results$radial)

sapply(results, function(x) x$best.performance)
sapply(results, function(x) x$best.parameters)

table(predict(results$radial$best.model, data), data$high_mpg)

plot(results$radial$best.model, data, horsepower~displacement)
plot(results$radial$best.model, data, horsepower~weight)
plot(results$radial$best.model, data, displacement~weight)

