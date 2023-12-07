points <- expand.grid(
  X1 = seq(-4, 2, length.out = 100), 
  X2 = seq(-1, 5, length.out = 100)
)
p <- ggplot(points, aes(x = X1, y = X2, z = (1 + X1)^2 + (2 - X2)^2 - 4)) + 
  geom_contour(breaks = 0, colour = "black") + 
  theme_bw()
p

p + geom_point(aes(color = (1 + X1)^2 + (2 - X2)^2 - 4 > 0), size = 0.1)

points <- data.frame(
  X1 = c(0, -1, 2, 3),
  X2 = c(0, 1, 2, 8)
)

ifelse((1 + points$X1)^2 + (2 - points$X2)^2 > 4, "blue", "red")

