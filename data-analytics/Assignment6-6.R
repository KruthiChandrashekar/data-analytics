fit <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(fit)

boot.fn <- function(x, i) {
  fit <- glm(default ~ income + balance, data = x[i, ], family = "binomial")
  coef(fit)[-1]
}

library(boot)
set.seed(42)
boot(Default, boot.fn, R = 1000)
