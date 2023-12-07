set.seed(42)
train <- sample(c(TRUE, FALSE), nrow(Carseats), replace = TRUE)

library(tree)
tr <- tree(Sales ~ ., data = Carseats[train, ])
summary(tr)
plot(tr)
text(tr, pretty = 0, digits = 2, cex = 0.8)

carseats_mse <- function(model) {
  p <- predict(model, newdata = Carseats[!train, ])
  mean((p - Carseats[!train, "Sales"])^2)
}
carseats_mse(tr)

res <- cv.tree(tr)
plot(res$size, res$dev, type = "b", xlab = "Tree size", ylab = "Deviance")
min <- which.min(res$dev)
abline(v = res$size[min], lty = 2, col = "red")

bagged <- randomForest(Sales ~ ., data = Carseats[train, ], mtry = 10, 
                       ntree = 200, importance = TRUE)
carseats_mse(bagged)
importance(bagged)

rf <- randomForest(Sales ~ ., data = Carseats[train, ], mtry = 3, 
                   ntree = 500, importance = TRUE)
carseats_mse(rf)
importance(rf)


library(BART)

predict.wbart <- function(model, ...) model$yhat.test.mean

bartfit <- gbart(Carseats[train, 2:11], Carseats[train, 1], 
                 x.test = Carseats[!train, 2:11])
carseats_mse(bartfit)

