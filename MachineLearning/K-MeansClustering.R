
#Assignment 5
# Problem 2 / Part 1
#install.packages('cluster')   -> REMOVE COMMENT WHEN SUBMITTING
#install.packages('factoextra')   -> REMOVE COMMENT WHEN SUBMITTING
library(cluster)
library(factoextra)
set.seed(109)
print('')
print('Assignment 5')
print('Problem 2 / Part 1')
dataset <- read.csv('seeds.csv',header=TRUE,sep=',')
print('head(dataset)')
print(head(dataset))


# Problem 2 / Part 2
print('')
print('Problem 2 / Part 2')
dataset2 <- complete.cases(dataset)
#table(data2)
goodRows = length(which(dataset2==TRUE))
badRows = length(which(dataset2==FALSE))
print(paste('Out of',length(data2),'rows of data:'))
print(paste(goodRows,'are good and',badRows,'are bad.'))


# Problem 2 / Part 3
print('')
print('Problem 2 / Part 3')
k_mean_ds <- kmeans(dataset,2,nstart=25)
print('See cluster plot')
fviz_cluster(k_mean_ds,dataset[,-8],main='Cluster Plot (Assignment 5 / Problem 2 / Part 3)')


# Problem 2 / Part 4
print('')
print('Problem 2 / Part 4')
print('See elbow plot')
fviz_nbclust(dataset[,-8],FUNcluster=kmeans,method='wss')
k_mean_ds_3 <- kmeans(dataset,3,nstart=25)
print('See cluster plot')
fviz_cluster(k_mean_ds_3,dataset[,-8],main='Cluster Plot (Assignment 5 / Problem 2 / Part 4)')


# Problem 2 / Part 5
print('')
print('Problem 2 / Part 5')
cluster_vect <- k_mean_ds_3$cluster
cm = table(dataset$variety, cluster_vect)
print(cm)
correct = cm[1,1]+cm[2,2]+cm[3,3]
print(paste0('Accuaracy = ',round(correct/sum(cm)*100,2),'%'))
print('')




