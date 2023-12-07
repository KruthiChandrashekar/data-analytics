library(ggplot2)
xlim <- c(-10, 10)
ylim <- c(-30, 30)
points <- expand.grid(
  X1 = seq(xlim[1], xlim[2], length.out = 50), 
  X2 = seq(ylim[1], ylim[2], length.out = 50)
)
p <- ggplot(points, aes(x = X1, y = X2)) + 
  geom_abline(intercept = 1, slope = 3) +  # X2 = 1 + 3X1 
  theme_bw()
p + geom_point(aes(color = 1 + 3*X1 - X2 > 0), size = 0.1) + 
  scale_color_discrete(name = "1 + 3X1 − X2 > 0")

p + geom_abline(intercept = 1, slope = -1/2) +  # X2 = 1 - X1/2
  geom_point(
    aes(color = interaction(1 + 3*X1 - X2 > 0, -2 + X1 + 2*X2 > 0)), 
    size = 0.5
  ) + 
  scale_color_discrete(name = "(1 + 3X1 − X2 > 0).(−2 + X1 + 2X2 > 0)")
