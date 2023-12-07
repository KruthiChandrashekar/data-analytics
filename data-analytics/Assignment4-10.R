dat <- Hitters
dat <- dat[!is.na(dat$Salary), ]
dat$Salary <- log(dat$Salary)

train <- 1:200
test <- setdiff(1:nrow(dat), train)

library(gbm)
set.seed(42)
lambdas <- 10 ^ seq(-3, 0, by = 0.1)
fits <- lapply(lambdas, function(lam) {
  gbm(Salary ~ ., data = dat[train, ], distribution = "gaussian", 
      n.trees = 1000, shrinkage = lam)
})
errs <- sapply(fits, function(fit) {
  p <- predict(fit, dat[train, ], n.trees = 1000)
  mean((p - dat[train, ]$Salary)^2)
})
plot(lambdas, errs, type = "b", xlab = "Shrinkage values", 
     ylab = "Training MSE", log = "xy")

errs <- sapply(fits, function(fit) {
  p <- predict(fit, dat[test, ], n.trees = 1000)
  mean((p - dat[test, ]$Salary)^2)
})
plot(lambdas, errs, type = "b", xlab = "Shrinkage values", 
     ylab = "Training MSE", log = "xy")
min(errs)
abline(v = lambdas[which.min(errs)], lty = 2, col = "red")

fit1 <- lm(Salary ~ ., data = dat[train, ])
mean((predict(fit1, dat[test, ]) - dat[test, "Salary"])^2)

library(glmnet)
x <- model.matrix(Salary ~ ., data = dat[train, ])
x.test <- model.matrix(Salary ~ ., data = dat[test, ])
y <- dat[train, "Salary"]
fit2 <- glmnet(x, y, alpha = 1)
mean((predict(fit2, s = 0.1, newx = x.test) - dat[test, "Salary"])^2)

summary(fits[[which.min(errs)]])

set.seed(42)
bagged <- randomForest(Salary ~ ., data = dat[train, ], mtry = 19, ntree = 1000)
mean((predict(bagged, newdata = dat[test, ]) - dat[test, "Salary"])^2)
