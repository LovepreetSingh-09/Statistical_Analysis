library (ISLR)
library(leaps)

fix(Hitters )
names(Hitters )
dim(Hitters )
sum(is.na(Hitters$Salary))
Hitters =na.omit(Hitters )
hitters =na.omit(Hitters )
dim(hitters)

subset=regsubsets(Salary∼.,Hitters)
summary(subset)

subset=regsubsets(Salary~.,Hitters,nvmax=10)
names(summary(subset))
summary(subset)$rsq

par(mfrow=c(2,2))
plot(summary(subset)$rss ,xlab=" Number of Variables ",ylab=" RSS",type="l")
plot(summary(subset)$adjr2 ,xlab =" Number of Variables ",ylab=" Adjusted RSq",type="l")

which.max(summary(subset)$adjr2)
points(10,summary(subset)$adjr2[10],col='red',cex=2,pch=20)

coef(subset,10)

subset_fwd=regsubsets(Salary~.,Hitters,nvmax=10,method='forward')
subset_bwd=regsubsets(Salary~.,Hitters,nvmax=10,method = 'backward')
coef(subset_fwd,9)
coef(subset_bwd,9)


set.seed (1)
train=sample (c(TRUE ,FALSE), nrow(hitters ),rep=TRUE)
train
test =(! train )
table(test)

hitters=na.omit(hitters)
subset_best=regsubsets(Salary∼.,data=hitters[train ,],nvmax =10)
test_matrix=model.matrix (Salary∼.,data=hitters[test ,])
nrow(test_matrix)
hitters$Salary[test]
errors=rep(NA,10)
for (i in 1:10){
  coefi=coef(subset_best,id=i)
  pred=test_matrix[,names(coefi)]%*%coefi
  errors[i]=mean((hitters$Salary[test]-pred)^2)
}
errors
which.min(errors)

predict.regsubsets =function (object ,newdata ,id ,...){
   form=as.formula (object$call [[2]])
   mat=model.matrix (form ,newdata )
   coefi =coef(object ,id=id)
   xvars =names (coefi )
   mat[,xvars ]%*% coefi
}
k=10
set.seed (1)
folds=sample (1:k,nrow(Hitters ),replace =TRUE)
cv.errors =matrix (NA ,k,19, dimnames =list(NULL , paste (1:19) ))
cv.errors

for(j in 1:k){
   best.fit =regsubsets(Salary∼.,data=Hitters [folds !=j,], nvmax =19)
   for(i in 1:10) {
     pred=predict(best.fit ,hitters [folds ==j,], id=i)
     cv.errors [j,i]=mean((hitters$Salary[folds ==j]-pred)^2)
     }
   }

# Ridge Regression
# alpha=0 for ridge and 1 for lasso
library(glmnet)
x=model.matrix(Salary~.,hitters)[,-1]
x
y=hitters$Salary
y
grid=10^seq(10,-2,length=100)
grid
# glmnet automatically standardize the variables
?glmnet
ridge=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge))
ridge$lambda[50]
coef(ridge)[,50]
sqrt(sum(coef(ridge)[-1,50]^2))
predict(ridge,s=50,type='coefficients')

set.seed (1)
train=sample (1: nrow(x), nrow(x)/2)
test=(- train )
y.test=y[test]
ridge=glmnet(x[train,],y[train],lambda=grid,alpha=0,thresh = 1e-12)
pred=predict(ridge,s=5,newx=x[test,])
mean((y[test]-pred)^2)

# glmnet() yield the exact least squares coefficients when λ = 0, for this we use the argument exact=T
pred=predict(ridge,s=0, newx=x[test,], exact=TRUE)

lm(y∼x, subset =train)
predict(ridge ,s=0, exact =T,type= 'coefficients')

library (pls)
set.seed (2)
pcr=pcr(Salary∼., data=Hitters ,scale=TRUE , validation ="CV")
summary(pcr)
validationplot(pcr ,val.type= 'MSEP')

set.seed (1)
pls=plsr(Salary∼., data=Hitters ,subset =train ,scale=TRUE ,validation ="CV")
summary(pls)
validationplot(pls ,val.type="MSEP")
pls.pred=predict (pls ,x[test ,], ncomp =2)
mean((pls.pred -y.test)^2)
