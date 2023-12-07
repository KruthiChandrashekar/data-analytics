x <- cbind(
  ISLR2::Boston[, -1], 
  data.frame("highcrim" = Boston$crim > median(Boston$crim))
)
set.seed(1)
train <- sample(seq_len(nrow(x)), nrow(x) * 2/3)

# We can find the most associated variables by performing wilcox tests.

ord <- order(sapply(1:12, function(i) {
  p <- wilcox.test(as.numeric(x[train, i]) ~ x[train, ]$highcrim)$p.value
  setNames(log10(p), colnames(x)[i])
}))
ord <- names(x)[ord]
ord

# Variables `nox` (nitrogen oxides concentration) followed by `dis` (distance to
# employment center) appear to be most associated with high crime.

#Let's reorder columns by those most associated with highcrim (in the training data)

x <- x[, c(ord, "highcrim")]

# Let's look at univariate associations with `highcrim` (in the training data)

x[train, ] |>
  pivot_longer(!highcrim) |>
  mutate(name = factor(name, levels = ord)) |>
  ggplot(aes(highcrim, value)) + 
  geom_boxplot() + 
  facet_wrap(~name, scale = "free")

# Fit lda, logistic regression, naive Bayes and KNN models (with k = 1..50) for a
# set of specific predictors and return the error rate. We fit models using
# increasing numbers of predictors: column 1, then columns 1 and 2 etc.

fit_models <- function(cols, k_vals = 1:50) {
  dat_train <- x[train, cols, drop = FALSE]
  dat_test <- x[-train, cols, drop = FALSE]
  
  fit <- lda(x$highcrim[train] ~ ., data = dat_train)
  pred <- predict(fit, dat_test, type = "response")$class
  lda_err <- mean(pred != x$highcrim[-train])
  
  fit <- glm(x$highcrim[train] ~ ., data = dat_train, family = binomial)
  pred <- predict(fit, dat_test, type = "response") > 0.5
  logreg_err <- mean(pred != x$highcrim[-train])
  
  fit <- naiveBayes(x$highcrim[train] ~ ., data = dat_train)
  pred <- predict(fit, dat_test, type = "class")
  nb_err <- mean(pred != x$highcrim[-train])
  
  res <- sapply(k_vals, function(k) {
    fit <- knn(dat_train, dat_test, x$highcrim[train], k = k)
    mean(fit != x$highcrim[-train])
  })
  knn_err <- min(res)
  
  c("LDA" = lda_err, "LR" = logreg_err, "NB" = nb_err, "KNN" = knn_err)
}

res <- sapply(1:12, function(max) fit_models(1:max))
res <- as_tibble(t(res))
res$n_var <- 1:12
pivot_longer(res, cols = !n_var) |>
  ggplot(aes(n_var, value, col = name)) + 
  geom_line() + 
  xlab("Number of predictors") + 
  ylab("Error rate")

# KNN appears to perform better (if we tune $k$) for all numbers of predictors.

fit <- knn(
  x[train, "nox", drop = FALSE],
  x[-train, "nox", drop = FALSE],
  x$highcrim[train],
  k = 1
)
table(fit, x[-train, ]$highcrim)
mean(fit != x[-train, ]$highcrim) * 100

#Surprisingly, the best model (with an error rate of <5%) uses $k = 1$ and
#assigns crime rate categories based on the town with the single most similar
#nitrogen oxide concentration (`nox`). This might be, for example, because nearby
#towns have similar crime rates, and we can obtain good predictions by predicting
#crime rate based on a nearby town.

#But what if we only consider $k = 20$.

res <- sapply(1:12, function(max) fit_models(1:max, k_vals = 20))
res <- as_tibble(t(res))
res$n_var <- 1:12
pivot_longer(res, cols = !n_var) |>
  ggplot(aes(n_var, value, col = name)) +
  geom_line() +
  xlab("Number of predictors") +
  ylab("Error rate")


#KNN still performs best with a single predictor (`nox`), but logistic regression
#with 12 predictors also performs well and has an error rate of ~12%.

vars <- names(x)[1:12]
dat_train <- x[train, vars]
dat_test <- x[-train, vars]

fit <- glm(x$highcrim[train] ~ ., data = dat_train, family = binomial)
pred <- predict(fit, dat_test, type = "response") > 0.5
table(pred, x[-train, ]$highcrim)
mean(pred != x$highcrim[-train]) * 100
summary(fit)
