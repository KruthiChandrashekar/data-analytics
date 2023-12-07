data <- data.frame(
  X1 = c(3, 2, 4, 1, 2, 4, 4),
  X2 = c(4, 2, 4, 4, 1, 3, 1),
  Y  = c(rep("Red", 4), rep("Blue", 3))
)
p <- ggplot(data, aes(x = X1, y = X2, color = Y)) + 
  geom_point(size = 2) + 
  scale_colour_identity() +
  coord_cartesian(xlim = c(0.5, 4.5), ylim = c(0.5, 4.5))
p

library(e1071)

fit <- svm(as.factor(Y) ~ ., data = data, kernel = "linear", cost = 10, scale = FALSE)

# Extract beta_0, beta_1, beta_2
beta <- c(
  -fit$rho,
  drop(t(fit$coefs) %*% as.matrix(data[fit$index, 1:2]))
)
names(beta) <- c("B0", "B1", "B2")
p <- p + geom_abline(intercept = -beta[1] / beta[3], slope = -beta[2] / beta[3], lty = 2)
p

p <- p + geom_ribbon(
  aes(x = x, ymin = ymin, ymax = ymax),
  data = data.frame(x = c(0, 5), ymin = c(-1, 4), ymax = c(0, 5)),
  alpha = 0.1, fill = "blue",
  inherit.aes = FALSE
)
p

p <- p + geom_point(data = data[fit$index, ], size = 4)
p

p + geom_point(data = data[7, , drop = FALSE], size = 4, color = "purple")

p + geom_abline(intercept = 1, slope = 0.5, lty = 2, col = "red")

p + geom_point(data = data.frame(X1 = 1, X2 = 3, Y  = "Blue"), shape = 15, size = 4)

