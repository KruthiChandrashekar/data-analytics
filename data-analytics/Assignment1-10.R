# Reading Boston Dataset
library(ISLR2)
Boston
?Boston
dim(Boston)

#b
library(ggplot2)
library(tidyverse)

ggplot(Boston, aes(nox, rm)) + geom_point()
ggplot(Boston, aes(ptratio, rm)) + geom_point()
heatmap(cor(Boston, method = "spearman"), cexRow = 1.1, cexCol = 1.1)

#c - Yes

#d
Boston |> 
  pivot_longer(cols = 1:13) |> 
  filter(name %in% c("crim", "tax", "ptratio")) |> 
  ggplot(aes(value)) + 
  geom_histogram(bins = 20) + 
  facet_wrap(~name, scales="free", ncol= 1)

#e
sum(Boston$chas)

#f
median(Boston$ptratio)

#g
Boston[Boston$medv == min(Boston$medv), ] |>
  kable()

sapply(Boston, quantile) |>
  kable()

#h
dim(subset(Boston, rm > 7))

dim(subset(Boston, rm > 8))

summary(subset(Boston, rm > 8))
summary(Boston)

