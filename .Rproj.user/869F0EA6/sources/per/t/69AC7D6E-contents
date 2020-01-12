library(stats)
attach(mtcars)

str(mtcars)
summary(mtcars)

norm=function(x){
  (x-min(x))/(max(x)-min(x))
}

mtcars=as.data.frame(lapply(mtcars, norm))
summary(mtcars)

?stats

library(help='stats')

?hclust

mtcars=na.omit(mtcars)

clusters=hclust(dist(mtcars),"cen")
clusters
plot(clusters)

clusters$height
clusters$order

m=matrix(c(1:6,7:12,13:18,19:24),nrow=6,ncol=4,byrow=TRUE)
m
m[3,2]
diag(m)
rev(diag(m))
sum(m)

?data.frame
a=data.frame('Roll No.'=c(21,22,23),"name"=c('riya','john','vicky'),'section'=c('A','B','C'),'marks'=c(26,35,44))
a
