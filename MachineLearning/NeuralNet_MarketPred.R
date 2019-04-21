
#Assignment 4
library(ISLR)
library(caret)
library(e1071)

# Problem 2 / Part 1
print('')
print('Problem 2 / Part 1')
data <- Smarket
print(head(data))
train1 <- data[data$Year==2001,]
test1 <- data[data$Year!=2001,]
print(paste('training set is',nrow(train1),'x',
            ncol(train1)))
print(paste('test set is',nrow(test1),'x',
            ncol(test1)))


# Problem 2 / Part 2
print('')
print('Problem 2 / Part 2')
library(neuralnet)
n_train <- names(train1)
f_nn <- as.formula(paste("Volume + Today ~",
              paste(n_train[!n_train %in% "Volume" & !n_train %in% "Today"
                            & !n_train %in% "Direction"]
              , collapse = " + ")))
#nn_1 <- neuralnet(Volume + Today ~ Lag1 + Lag2 + Lag3 + 
#                    Lag4 + Lag5, train1, hidden = c(3), threshold=0.1)
nn_1 <- neuralnet(f_nn,data=train1,hidden = c(3),threshold=0.1)
predict_nn_1 <- compute(nn_1,test1)

nn_2 <- neuralnet(f_nn,data=train1,hidden = c(3,3),threshold=0.1)
predict_nn_2 <- compute(nn_2,test1)

nn_3 <- neuralnet(f_nn,data=train1,hidden = c(3,3,3),threshold=0.1)
predict_nn_3 <- compute(nn_3,test1)
print('formula (f_nn):')
print(f_nn) 
print('nn_1$call:')
print(nn_1$call)
print('nn_2$call:')
print(nn_2$call)
print('nn_3$call:')
print(nn_3$call)


# Problem 2 / Part 3
print('')
print('Problem 2 / Part 3')
test_Vol <- (test1$Volume)*(max(data$Volume)-min(data$Volume))+
  min(data$Volume)
test_Today <- (test1$Today)*(max(data$Today)-min(data$Today))+
  min(data$Today)


Vol1_tr <- predict_nn_1$net.result[,1]*(max(data$Volume)-min(data$Volume))+
  min(data$Volume)
Tod1_tr <- predict_nn_1$net.result[,2]*(max(data$Today)-min(data$Today))+
  min(data$Today)
MSE_1 <- sum((test_Vol - Vol1_tr)^2 + (test_Today - Tod1_tr)^2)/
            nrow(test1)

Vol2_tr <- predict_nn_2$net.result[,1]*(max(data$Volume)-min(data$Volume))+
  min(data$Volume)
Tod2_tr <- predict_nn_2$net.result[,2]*(max(data$Today)-min(data$Today))+
  min(data$Today)
MSE_2 <- sum((test_Vol - Vol2_tr)^2 + (test_Today - Tod2_tr)^2)/
  nrow(test1)

Vol3_tr <- predict_nn_3$net.result[,1]*(max(data$Volume)-min(data$Volume))+
  min(data$Volume)
Tod3_tr <- predict_nn_3$net.result[,2]*(max(data$Today)-min(data$Today))+
  min(data$Today)
MSE_3 <- sum((test_Vol - Vol3_tr)^2 + (test_Today - Tod3_tr)^2)/
  nrow(test1)

print(paste('MSE (1 Hidden Layer) :',round(MSE_1,4)))
print(paste('MSE (2 Hidden Layers):',round(MSE_2,4)))
print(paste('MSE (3 Hidden Layers):',round(MSE_3,4)))


# Problem 2 / Part 3-2
print('')
print('Problem 2 / Part 3-2')
print('For One Hidden Layer')

nn_1_1 <- neuralnet(f_nn,data=train1,hidden = c(1),threshold=0.1)
predict_nn_1_1 <- compute(nn_1_1,test1)

nn_1_2 <- neuralnet(f_nn,data=train1,hidden = c(2),threshold=0.1)
predict_nn_1_2 <- compute(nn_1_2,test1)

nn_1_3 <- neuralnet(f_nn,data=train1,hidden = c(3),threshold=0.1)
predict_nn_1_3 <- compute(nn_1_3,test1)

nn_1_4 <- neuralnet(f_nn,data=train1,hidden = c(4),threshold=0.1)
predict_nn_1_4 <- compute(nn_1_4,test1)

nn_1_5 <- neuralnet(f_nn,data=train1,hidden = c(5),threshold=0.1)
predict_nn_1_5 <- compute(nn_1_5,test1)

Vol1_1_tr <- predict_nn_1_1$net.result[,1]*(max(data$Volume)-min(data$Volume))+
  min(data$Volume)
Tod1_1_tr <- predict_nn_1_1$net.result[,2]*(max(data$Today)-min(data$Today))+
  min(data$Today)
MSE_1_1 <- sum((test_Vol - Vol1_1_tr)^2 + (test_Today - Tod1_1_tr)^2)/
  nrow(test1)

Vol1_2_tr <- predict_nn_1_2$net.result[,1]*(max(data$Volume)-min(data$Volume))+
  min(data$Volume)
Tod1_2_tr <- predict_nn_1_2$net.result[,2]*(max(data$Today)-min(data$Today))+
  min(data$Today)
MSE_1_2 <- sum((test_Vol - Vol1_2_tr)^2 + (test_Today - Tod1_2_tr)^2)/
  nrow(test1)

Vol1_3_tr <- predict_nn_1_3$net.result[,1]*(max(data$Volume)-min(data$Volume))+
  min(data$Volume)
Tod1_3_tr <- predict_nn_1_3$net.result[,2]*(max(data$Today)-min(data$Today))+
  min(data$Today)
MSE_1_3 <- sum((test_Vol - Vol1_3_tr)^2 + (test_Today - Tod1_3_tr)^2)/
  nrow(test1)


# Problem 2 / Part 3-2 (Continued)
Vol1_4_tr <- predict_nn_1_4$net.result[,1]*(max(data$Volume)-min(data$Volume))+
  min(data$Volume)
Tod1_4_tr <- predict_nn_1_4$net.result[,2]*(max(data$Today)-min(data$Today))+
  min(data$Today)
MSE_1_4 <- sum((test_Vol - Vol1_4_tr)^2 + (test_Today - Tod1_4_tr)^2)/
  nrow(test1)

Vol1_5_tr <- predict_nn_1_5$net.result[,1]*(max(data$Volume)-min(data$Volume))+
  min(data$Volume)
Tod1_5_tr <- predict_nn_1_5$net.result[,2]*(max(data$Today)-min(data$Today))+
  min(data$Today)
MSE_1_5 <- sum((test_Vol - Vol1_5_tr)^2 + (test_Today - Tod1_5_tr)^2)/
  nrow(test1)


print(paste('MSE (1 Node) :',round(MSE_1_1,4)))
print(paste('MSE (2 Nodes):',round(MSE_1_2,4)))
print(paste('MSE (3 Nodes):',round(MSE_1_3,4)))
print(paste('MSE (4 Nodes):',round(MSE_1_4,4)))
print(paste('MSE (5 Nodes):',round(MSE_1_5,4)))

