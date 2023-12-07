set.seed(42)
data <- matrix(rnorm(60 * 50), ncol = 50)
classes <- rep(c("A", "B", "C"), each = 20)
dimnames(data) <- list(classes, paste0("v", 1:50))
data[classes == "B", 1:10] <- data[classes == "B", 1:10] + 1.2
data[classes == "C", 5:30] <- data[classes == "C", 5:30] + 1

pca <- prcomp(data)
ggplot(data.frame(Class = classes, PC1 = pca$x[, 1], PC2 = pca$x[, 2]),
       aes(x = PC1, y = PC2, col = Class)) + 
  geom_point()

km <- kmeans(data, 3)$cluster
table(km, names(km))

km <- kmeans(data, 2)$cluster
table(km, names(km))

km <- kmeans(data, 4)$cluster
table(km, names(km))

km <- kmeans(pca$x[, 1:2], 3)$cluster
table(km, names(km))

km <- kmeans(scale(data), 3)$cluster
table(km, names(km))
