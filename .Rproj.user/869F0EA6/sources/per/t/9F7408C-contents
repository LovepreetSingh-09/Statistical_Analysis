library(psych)
challanger=read.csv('challanger.txt')
insurance=read.csv('insurance.txt')
str(insurance)
str(challanger)

# wight by covariance and variance matrix formula
m=cov(challanger$temperature,challanger$distress_ct)/var(challanger$temperature)
m
# intercept
b=mean(challanger$distress_ct)-m*(mean(challanger$temperature))
b
cor(challanger$temperature,challanger$distress_ct)

# function for getting both intercept and weights
reg=function(x,y){
  a=as.matrix(x)
  # adding a new column with all values=1
  a=cbind(Intercept=1,a)
  # %*% is for matrix multiplication
  w=solve(t(a)%*%a)%*%t(a)%*%y
  colnames(w)<-'estimate'
  print(w)
}
reg(x=challanger$temperature,y=challanger$distress_ct)
reg(y = challanger$distress_ct, x = challanger[2:4])

# Predicting Medical Expenses
str(insurance)
summary(insurance$charges)
hist(insurance$charges,col='blue')
table(insurance$region)

# Exploring relationship b/w all variables by pearson correlation
# The diagonal here will be 1 becoz of variables covariance with itself and its variance is same
cor(insurance[c("age", "bmi", "children", "charges")])

# scatter plot
pairs(insurance[c("age", "bmi", "children", "charges")])
# scatter plot using psych
# It shows corelation and loess curve
pairs.panels(insurance[c("age", "bmi", "children", "charges")])

# model
# you can write . in place of all independent variables
model=lm(charges~age+children + bmi + sex + smoker + region, data = insurance)
model
summary(model) # all estimates, p-value and R2 value
# We got R2 value of 0.75

# Convert bmi into binary
insurance$bmi2=ifelse(insurance$bmi>=30,1,0)
insurance$age2=insurance$age^2 # adding non-linear function
# Combined effect is known as interaction and it is represent by : b/w variables
# While using * b/w we use the 2 separate original variable as well as their interaction
# we tried to add interaction of bmi and smoker becoz their combined effect will decide the medical cost very effectively.
model=lm(charges~age + age2 + children + bmi + sex + smoker + region + bmi2*smoker,data=insurance)
model
# Now we got R2 value of 0.85 which is way better and higher than the previous one
summary(model)

