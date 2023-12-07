library(tidyr)

p <- seq(0, 1, length.out = 100)
data.frame(
  x = p,
  "Gini index" = p * (1 - p) * 2,
  "Entropy" = -(p * log(p) + (1 - p) * log(1 - p)),
  "Classification error" = 1 - pmax(p, 1 - p),
  check.names = FALSE
) |>
  pivot_longer(!x) |>
  ggplot(aes(x = x, y = value, color = name)) + 
  geom_line(na.rm = TRUE)

