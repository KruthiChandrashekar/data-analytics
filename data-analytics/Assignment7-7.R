dat <- t(scale(t(USArrests)))
d1 <- dist(dat)^2
d2 <- as.dist(1 - cor(t(dat)))
plot(d1, d2)
