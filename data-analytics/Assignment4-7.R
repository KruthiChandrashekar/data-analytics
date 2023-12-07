library(ISLR2)
library(randomForest)
library(ggplot2)
library(tidyr)
library(tidyverse)

set.seed(42)

train <- sample(c(TRUE, FALSE), nrow(Boston), replace = TRUE)

rf_err <- function(mtry) {
  randomForest(
    Boston[train, -13], 
    y = Boston[train, 13], 
    xtest = Boston[!train, -13], 
    ytest = Boston[!train, 13], 
    mtry = mtry, 
    ntree = 500
  )$test$mse
}
res <- lapply(c(1, 2, 3, 5, 7, 10, 12), rf_err)
names(res) <- c(1, 2, 3, 5, 7, 10, 12)
data.frame(res, check.names = FALSE) |>
  mutate(n = 1:500) |>
  pivot_longer(!n) |>
  ggplot(aes(x = n, y = value, color = name)) + 
  geom_line(na.rm = TRUE) + 
  xlab("Number of trees") + 
  ylab("Error") +
  scale_y_log10() +
  scale_color_discrete(name = "No. variables at\neach split")

