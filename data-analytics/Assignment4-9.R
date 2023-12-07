set.seed(42)
train <- sample(1:nrow(OJ), 800)
test <- setdiff(1:nrow(OJ), train)

tr <- tree(Purchase ~ ., data = OJ[train, ])
summary(tr)

tr

plot(tr)
text(tr, pretty = 0, digits = 2, cex = 0.8)

table(predict(tr, OJ[test, ], type = "class"), OJ[test, "Purchase"])

set.seed(42)
res <- cv.tree(tr)

plot(res$size, res$dev, type = "b", xlab = "Tree size", ylab = "Deviance")
min <- which.min(res$dev)
abline(v = res$size[min], lty = 2, col = "red")

res$size[min]

ptr <- prune.tree(tr, best = res$size[min])
plot(ptr)
text(ptr, pretty = 0, digits = 2, cex = 0.8)

oj_misclass <- function(model) {
  summary(model)$misclass[1] / summary(model)$misclass[2]
}
oj_misclass(tr)
oj_misclass(ptr)

oj_err <- function(model) {
  p <- predict(model, newdata = OJ[test, ], type = "class")
  mean(p != OJ[test, "Purchase"])
}
oj_err(tr)
oj_err(ptr)

