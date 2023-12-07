#fix(Carseats)
carseats_lm = lm(Sales~Price+Urban+US,data=Carseats)
summary(carseats_lm)

attach(Carseats)
contrasts(US)
contrasts(Urban)

carseats_all_lm = lm(Sales~.,data=Carseats)
summary(carseats_all_lm)

carseats_all_lm2 = lm(Sales~.-Education-Urban-US-Population,data=Carseats)
summary(carseats_all_lm2)

confint(carseats_all_lm2)

par(mfrow=c(2,2))
plot(carseats_all_lm2)

rstudent(carseats_all_lm2)[which(rstudent(carseats_all_lm2)>3)]

hatvalues(carseats_all_lm2)[order(hatvalues(carseats_all_lm2), decreasing = T)][1]
