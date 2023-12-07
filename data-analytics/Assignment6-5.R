library(ISLR2)
set.seed(42)
fit <- glm(default ~ income + balance, data = Default, family = "binomial")

train <- sample(nrow(Default), nrow(Default) / 2)
fit <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
pred <- ifelse(predict(fit, newdata = Default[-train, ], type = "response") > 0.5, "Yes", "No")
table(pred, Default$default[-train]) 
mean(pred != Default$default[-train])

replicate(3, {
  train <- sample(nrow(Default), nrow(Default) / 2)
  fit <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
  pred <- ifelse(predict(fit, newdata = Default[-train, ], type = "response") > 0.5, "Yes", "No")
  mean(pred != Default$default[-train])
})

replicate(3, {
  train <- sample(nrow(Default), nrow(Default) / 2)
  fit <- glm(default ~ income + balance + student, data = Default, family = "binomial", subset = train)
  pred <- ifelse(predict(fit, newdata = Default[-train, ], type = "response") > 0.5, "Yes", "No")
  mean(pred != Default$default[-train])
})
