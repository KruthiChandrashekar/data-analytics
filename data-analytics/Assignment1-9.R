#Removing missing values
x <- read.table("Auto.data", header = TRUE, na.strings = "?")
x <- na.omit(x)

#a Qualitative & Quantitative Predictor
sapply(x, class)
numeric <- which(sapply(x, class) == "numeric")
names(numeric)

#b Range of Quantitative Predictors
sapply(x[, numeric], function(x) diff(range(x)))

#c Mean & SD of Quanititive Predictors
library(tidyverse)
library(knitr)

x[, numeric] |>
  pivot_longer(everything()) |>
  group_by(name) |>
  summarise(
    Mean = mean(value),
    SD = sd(value)
  ) |>
  kable()

#d Mean & SD of 10th - 85th
x[-(10:85), numeric] |>
  pivot_longer(everything()) |>
  group_by(name) |>
  summarise(
    Range = diff(range(value)),
    Mean = mean(value),
    SD = sd(value)
  ) |>
  kable()

#e Analysing relationships bw predictors
pairs(x[, numeric], cex = 0.2) 
cor(x[, numeric]) |>
  kable()

heatmap(cor(x[, numeric]), cexRow = 1.1, cexCol = 1.1, margins = c(8, 8))

