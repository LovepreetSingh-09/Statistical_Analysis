library(arules)

bakery=read.transactions('bakery.csv')
bakery=unique(bakery)
summary(bakery)
bakery

inspect(bakery[1:20])
itemFrequency(bakery[,1:5])
itemFrequencyPlot(bakery,support=0.001)
itemFrequencyPlot(bakery,topN=10)

image(sample(bakery,1000))
bake=apriori(bakery,parameter=list(support=0.001,confidence=0.005,minlen=2))
bake
summary(bake)
inspect(bake)      
