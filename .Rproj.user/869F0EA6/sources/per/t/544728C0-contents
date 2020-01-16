library(MASS)

fix(Boston)
names(Boston)
lr=lm(medv~lstat,data=Boston)
summary(lr)

confint (lr)

predict (lr ,data.frame(lstat =(c(5 ,10 ,15) )),interval ="confidence")

predict (lr ,data.frame(lstat =(c(5 ,10 ,15) )),interval ="prediction")

attach(Boston)

plot(lstat,medv)
abline (lr)

par(mfrow =c(2,2))
plot(lr)

plot(predict (lr), residuals (lr))

plot(hatvalues (lr ))
which.max (hatvalues (lr))

lr =lm(medv ∼ lstat+age ,data=Boston )
summary (lr)

lr =lm(medv ∼ . ,data=Boston )
summary (lr)

lr=lm(formula = medv ∼ lstat + I(lstat ^2))
summary(lr)
lr1 =lm(medv∼lstat)
summary(lr1)

anova(lr ,lr1)
