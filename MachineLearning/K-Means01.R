

dataset <- read.csv('Mall_Customers.csv')
X <- dataset[4:5]

#Using elbow method to find optimal amount of clusters
set.seed(6)
wcss <- vector()
for (i in 1:10) wcss[i] <- sum(kmeans(X,i)$withinss)
plot(1:10,wcss,type='b',main=paste('Clusters of clients'),xlab='Number of Clusters',ylab='WCSS')

# From graph, clusters = 5

set.seed(29)
kmeans1 <- kmeans(X,5,iter.max=300,nstart=10)

# Visualizing the clusters
library(cluster)
clusplot(X,kmeans1$cluster,lines = 0,shade = TRUE,color = TRUE,labels = 2,plotchar = FALSE,
         span = TRUE, main = paste('Clusters of clients'),xlab='Annual Inc',ylab='Spend Score')
