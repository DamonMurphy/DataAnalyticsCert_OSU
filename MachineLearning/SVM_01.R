
library(ggplot2)
library(e1071)
library(ISLR)
library(RColorBrewer)

set.seed(13)
x <- matrix(rnorm(40*2), ncol = 2) 
y <- c(rep(-1,10), rep(1,10)) 
x[y==1,] <- x[y==1,] + 3   #separate two classes by adding a constant
dat <- data.frame(x=x, y=as.factor(y))

# Plot data 
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) + 
  scale_color_manual(values=c("#000000", "#FF0000")) + 
  theme(legend.position = "none")

# Fit Support Vector Machine model to data set 
svmfit <- svm(y~., data = dat, kernel = "linear", scale = FALSE) 

# Plot Results 
plot(svmfit, dat)

tune.out=tune(svm ,y~.,data=dat,kernel="linear",
              ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100) ))

summary(tune.out)
bestModel <- tune.out$best.model
summary(bestModel)



