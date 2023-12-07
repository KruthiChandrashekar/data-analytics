library(e1071)

set.seed(42)
train <- data.frame(
  x1 = runif(500) - 0.5,
  x2 = runif(500) - 0.5
)
train$y <- factor(as.numeric((train$x1^2 - train$x2^2 > 0)))

p <- ggplot(train, aes(x = x1, y = x2, color = y)) + 
  geom_point(size = 2)
p

fit1 <- glm(y ~ ., data = train, family = "binomial")

plot_model <- function(fit) {
  if (inherits(fit, "svm")) {
    train$p <- predict(fit)
  } else {
    train$p <- factor(as.numeric(predict(fit) > 0))
  }
  ggplot(train, aes(x = x1, y = x2, color = p)) + 
    geom_point(size = 2)
}

plot_model(fit1)

fit2 <- glm(y ~ poly(x1, 2) + poly(x2, 2), data = train, family = "binomial")

plot_model(fit2)

fit3 <- svm(y ~ x1 + x2, data = train, kernel = "linear")
plot_model(fit3)

fit4 <- svm(y ~ x1 + x2, data = train, kernel = "polynomial", degree = 2)
plot_model(fit4)

