
#Assignment 4
# Problem 1 / Part a)
print('')
print('Assignment 4')
print('Problem 1 / Part a)')
image_data <- read.table("image_data.txt",sep=";")
labels1 <- read.table("labels.txt")
W1 <- read.table("W1.txt",sep=";")
W2 <- read.table("W2.txt",sep=";")

print('')
print(paste('image_data.txt is',nrow(image_data),'x',
            ncol(image_data)))
print(paste('labels.txt is',nrow(labels1),'x',
            ncol(labels1)))
print(paste('W1.txt is',nrow(W1),'x',
            ncol(W1)))
print(paste('W2.txt is',nrow(W2),'x',
            ncol(W2)))
print('')


# Problem 1 / Part b)
print('')
print('Problem 1 / Part b)')
A <- matrix(c(0.8558,0.5236,0.6708,0.2988),nrow=2,ncol=2,byrow=TRUE)
print('A:')
print(A)
sigmoid <- function(z) {
  (1+exp(1)^(-z))^(-1)
}
print('sigmoid(A):')
print(sigmoid(A))


# Problem 1 / Part c)
print('')
print('Problem 1 / Part c)')
label_guess <- vector()
for(i in 1:nrow(image_data)) {
  # Step 1:
  s1 <- image_data[i,]
  s1b <- t(s1)
  s1b <- matrix(c(1,s1b),nrow=401,ncol=1)
  
  # Step 2:
  s2 <- sigmoid(as.matrix(W1) %*% s1b)
  
  # Step 3:
  s2b <- matrix(c(1,s2),nrow=21,ncol=1)
  
  # Step 4:
  s3 <- sigmoid(as.matrix(W2) %*% s2b)
  
  # Step 5:
  label_guess <- c(label_guess,which.max(s3))
}
print('Estimated images under each label:')
print(table(label_guess))


# Problem 1 / Part d)
print('')
print('Problem 1 / Part d)')
correct1 <- 0
for (j in 1:length(label_guess)) {
#  print(label_guess[j])
#  print(labels1[j,])
#  print(label_guess[j] == labels1[j,])
  if(label_guess[j] == labels1[j,]){
    correct1 <- correct1 + 1
  }
  
}

pct_correct <- round(correct1/length(label_guess)*100,2)
print(paste0('Percent Correct = ',pct_correct,'%'))


# Problem 1 / Part e)
print('')
print('Problem 1 / Part e)')
rand_img <- matrix(runif(400),nrow=20,ncol=20)
plot(1:400,rand_img,main='Plot of 20x20 Random Matrix')
r1b <- matrix(c(1,rand_img),nrow=401,ncol=1)
r2 <- sigmoid(as.matrix(W1) %*% r1b)
r2b <- matrix(c(1,r2),nrow=21,ncol=1)
r3 <- sigmoid(as.matrix(W2) %*% r2b)
rand_guess <- which.max(r3)
print('Output of last layer:')
print(round(r3,4))
print(paste('Label for randomly generated image is:',rand_guess))


