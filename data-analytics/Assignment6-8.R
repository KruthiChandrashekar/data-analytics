set.seed(1)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)

plot(x, y)

library(boot)
set.seed(42)
dat <- data.frame(x, y)
sapply(1:4, function(i) cv.glm(dat, glm(y ~ poly(x, i)))$delta[1])

set.seed(43)
dat <- data.frame(x, y)
sapply(1:4, function(i) cv.glm(dat, glm(y ~ poly(x, i)))$delta[1])

for (i in 1:4) printCoefmat(coef(summary(glm(y ~ poly(x, i), data = dat))))

