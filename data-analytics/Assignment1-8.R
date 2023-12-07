college <- read.csv("college.csv")
View(college)

rownames(college) <- college[, 1]
view(college)
college <- college[, -1]
view(college)

summary(college)

college$Private <- college$Private == "Yes"
pairs(college[, 1:10], cex = 0.2)

plot(college$Outstate ~ factor(college$Private), xlab = "Private", ylab = "Outstate")

college$Elite <- factor(ifelse(college$Top10perc > 50, "Yes", "No"))
summary(college$Elite)
plot(college$Outstate ~ college$Elite, xlab = "Elite", ylab = "Outstate")

par(mfrow = c(2,2))
for (n in c(5, 10, 20, 50)) {
  hist(college$Enroll, breaks = n, main = paste("n =", n), xlab = "Enroll")
}

chisq.test(college$Private, college$Elite)

# if college is private and elite is not random