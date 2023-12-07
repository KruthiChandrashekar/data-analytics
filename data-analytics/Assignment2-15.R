#a
pred <- subset(Boston, select = -crim)


fits <- lapply(pred, function(x) lm(Boston$crim ~ x))
printCoefmat(do.call(rbind, lapply(fits, function(x) coef(summary(x))[2, ])))

plot(Boston$rm, Boston$crim)
abline(fits[[5]])

#b
mfit <- lm(crim ~ ., data = Boston)
summary(mfit)

#c estimated coefficient differ dramatically
plot(sapply(fits, function(x) coef(x)[2]), coef(mfit)[-1], 
     xlab = "Univariate regression", 
     ylab = "multiple regression")

#d except for black, others indicate non-linar relationship
pred <- subset(pred, select = -chas)
fits <- lapply(names(pred), function(p) {
  f <- paste0("crim ~ poly(", p, ", 3)")
  lm(as.formula(f), data = Boston)
})
for (fit in fits) printCoefmat(coef(summary(fit)))
