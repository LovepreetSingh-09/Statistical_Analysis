library(ISLR)

fix(College)
college=College
str(college)
names(college)
summary(college)
pairs(college[1:10])

boxplot(college$Outstate,college$Private)
Elite =rep ("No",nrow(college ))
Elite
Elite [college$Top10perc >50]=" Yes"
Elite =as.factor (Elite)
college =data.frame(college ,Elite)
summary(college)

fix(Auto)
summary(Auto)

cor(Auto$mpg,Auto[c(1:8)])
cor(Auto$mpg,Auto[1:8])

