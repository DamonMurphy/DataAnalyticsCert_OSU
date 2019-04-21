
#Assignment 2
library(ISLR)
library(caret)
library(e1071)

# Part 1
print('')
print('Assignment 2 / Part 1')
data <- Smarket
print(head(data))

# Part 2
print('')
print('Assignment 2 / Part 2')
data2 <- complete.cases(data)
#table(data2)
goodRows = length(which(data2==TRUE))
badRows = length(which(data2==FALSE))
print(paste('Out of',length(data2),'rows of data:'))
print(paste(goodRows,'are good and',badRows,'are bad.'))

# Part 3
print('')
print('Assignment 2 / Part 3')
target_column <- as.numeric(replace(as.character(replace(
                    as.character(data$Direction),data$Direction=="Up","1")),
                    data$Direction=="Down","0"))
data$target_column <- target_column
print(head(data))


# Part 4
print('')
print('Assignment 2 / Part 4')
set.seed(100)
trainIndex <- createDataPartition(data$target_column,p=0.7,list=FALSE,times = 1)
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]
# Remove superfluous columns
trainData <- trainData[,2:10]
trainData <- trainData[,-6:-8]
testData <- testData[,2:10]
testData <- testData[,-6:-8]
print('trainData distribution:')
print(table(trainData$target_column))
print('')
print('testData distribution:')
print(table(testData$target_column))


# Part 5A - Logistic Regression
print('')
print('Assignment 2 / Part 5A - Logistic Regression')
#FItting Logistic Regression to the Training Set
classifier_lr = glm(formula= target_column ~.,
                 family = binomial,
                 data = trainData)
print('Logistic Regression Classifier:')
print(classifier_lr)
print('')

#Predicting the testData results
prob_pred = predict(classifier_lr,type='response',newdata=testData[-6])
y_pred = ifelse(prob_pred>0.5,1,0)

#Making the Confusion Matrix - Logistic Regression
cm_lr = table(testData[,6],y_pred)


# Part 5B - SVM
print('')
print('Assignment 2 / Part 5B - SVM')
#FItting Classifier to the Training Set
classifier_svm = svm(formula = target_column ~.,
                 data = trainData,
                 type = 'C-classification',
                 kernel='linear',
                 cost=1)
print('SVM Classifier:')
print(classifier_svm)
print('')

#Predicting the testData results
y_pred <- predict(classifier_svm,newdata=testData[,-6])


#Making the Confusion Matrix - SVM
cm_svm = table(testData[,6],y_pred)


# Part 6
print('')
print('Assignment 2 / Part 6')
print('Logistic Regression prediction accuracy (%):')
print(round(((cm_lr[1,1]+cm_lr[2,2])/sum(cm_lr))*100,2))
print('')

print('SVM prediction accuracy (%):')
print(round(((cm_svm[1,1]+cm_svm[2,2])/sum(cm_svm))*100,2))
print('')
print('SVM classifier performed slighly better.')
print('')

# Part 7
print('')
print('Assignment 2 / Part 7')
print('Substituting different values in the \'kernel\' parameter')
print('Cost = 1 (default) for SVM')
print('    Linear = 48.53%')
print('    Polynomial = 47.73%')
print('    Radial = 47.20%')
print('    Sigmoid = 51.73%')
print('')
print('No significant difference adjusting \'cost\' value')
