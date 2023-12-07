library(gam)
set.seed(42)
train <- sample(1:nrow(College), 400)
test <- setdiff(1:nrow(College), train)

# Linear regression
lr <- gam(Outstate ~ ., data = College[train, ])

# GAM from chapter 7
gam <- gam(Outstate ~ Private + s(Room.Board, 2) + s(PhD, 2) + 
             s(perc.alumni, 2) + s(Expend, 2) + s(Grad.Rate, 2), data = College[train, ])

# Boosting
boosted <- gbm(Outstate ~ ., data = College[train, ], n.trees = 1000, shrinkage = 0.01)


# Bagging (random forest with mtry = no. predictors)
bagged <- randomForest(Outstate ~ ., data = College[train, ], mtry = 17, ntree = 1000)

# Random forest with mtry = sqrt(no. predictors)
rf <- randomForest(Outstate ~ ., data = College[train, ], mtry = 4, ntree = 1000)

# BART
pred <- setdiff(colnames(College), "Outstate")
bart <- gbart(College[train, pred], College[train, "Outstate"], 
              x.test = College[test, pred])

mse <- function(model, ...) {
  pred <- predict(model, College[test, ], ...)
  mean((College$Outstate[test] - pred)^2)
}

res <- c(
  "Linear regression" = mse(lr),
  "GAM" = mse(gam),
  "Boosting" = mse(boosted, n.trees = 1000),
  "Bagging" = mse(bagged),
  "Random forest" = mse(rf),
  "BART" = mse(bart)
)
res <- data.frame("MSE" = res)
res$Model <- factor(row.names(res), levels = rev(row.names(res)))
ggplot(res, aes(Model, MSE)) + coord_flip() + 
  geom_bar(stat = "identity", fill = "steelblue")

