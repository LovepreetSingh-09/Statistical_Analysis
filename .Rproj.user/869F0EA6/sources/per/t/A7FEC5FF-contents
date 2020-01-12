
library(arules)

# read.transactions creates a sparse matrix to save memory and computational efficiency
groceries=read.transactions('groceries_data.txt',sep=',')
fix(groceries)
summary(groceries)
groceries

# First 3 purchases
inspect(groceries[1:3])
itemFrequency(groceries[,1:3])

# Frequency on the basis of conditions
itemFrequencyPlot(groceries,support=0.1)
itemFrequencyPlot(groceries,topN=10)

# visualize sparse matrix
image(groceries[1:5])

# sparse matrix of 100 random purchases
image(sample(groceries,100))

# Train model
gro=apriori(groceries,parameter=list(support=0.006,confidence=0.30,minlen=2))
gro
summary(gro)
# Rules and info of first 3 rules
inspect(gro[1:3])
# Sorting of rules by lift indescending order
inspect(sort(gro,by='lift')[1:5])

# Getting rules having product berries
berries=subset(gro,items %in% 'berries')
# Rules with a word fruit anywhere in the Rules like pip fruit, citrus fruit, tropical fruits etc.
fruits=subset(gro,items %pin% 'fruit')
inspect(berries)
inspect(fruits)

# Witing or creating a file consists of found rules
write(gro,'groceries.csv',sep=',',quote=TRUE,row.names=FALSE)

# reading or importing the file consist oof rules in a data frame
groc=read.csv('groceries.csv')
str(groc)
summary(groc)

# converting the rules in data frame format
gr=as(gro,'data.frame')
# The rules column in data frame is converted into factors 
str(gr)

