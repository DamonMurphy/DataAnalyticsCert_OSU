
#Assignment 5
# Problem 1 / Part a)
print('')
print('Assignment 5')
print('Problem 1 / Part a)')

X <- read.table('my_data_pca.txt',sep=',')
X<- as.matrix(X)
print(paste('The dimensions of X are',nrow(X),'rows x',ncol(X),'columns'))


# Problem 1 / Part b)
print('')
print('Problem 1 / Part b)')
X_mean <- apply(X, 1, mean)
Y <- sweep(X, 1, X_mean,'-')
print('Matrix Y created, each row has mean of 0.')
print('As an example, the means of the first 5 rows of X:')
print(round(rowMeans(X[1:5,]),4))
print('And the means of the first 5 columns of Y:')
print(round(rowMeans(Y[1:5,]),4))



# Problem 1 / Part c)
print('')
print('Problem 1 / Part c)')
C <- (1/799)*(t(Y)%*%Y)
C_eigen <- eigen(C)
print('As an example, the first five eigenvalues of C:')
print(round(C_eigen$values[1:5],4))
print('and the first five eigenvectors of C (first five rows):')
print(round(C_eigen$vectors[1:5,1:5],4))


# Problem 1 / Part e)
print('')
print('Problem 1 / Part e)')
C_ordered <- sort(C_eigen$values,decreasing=TRUE)
plot(1:800,C_ordered,main='Plot of Ordered Eigenvalues of Matrix C',sub='Assignment 5 / Problem 1 / Part e')


# Problem 1 / Part f)
print('')
print('Problem 1 / Part f)')
C_12 <- C_ordered[1:12]
# Create Matrix V using Eigenvectors of the 12 largest Eigenvalues (C_12)
V <- vector()
for (i in 1:12) {
  # Use 'which' function to find the Eigenvector corresponding to the
  # ith largest Eigenvalue
  j <- which(C_eigen$values==C_12[i])
  V <- cbind(V,C_eigen$vectors[,j])
}
print('Matrix V created')


# Problem 1 / Part g)
print('')
print('Problem 1 / Part g)')
E <- Y %*% V
print(paste('The dimensions of E are',nrow(E),'rows x',ncol(E),'columns'))


# Problem 1 / Part h)
print('')
print('Problem 1 / Part h)')
# E_images will be a list of 85x60 matrices when finished
E_images <- list()
for (i in 1:12) {
  # Convert each column of 5100 values into a 85x60 matrix
  E_images[[i]] <- matrix(data=E[,i],nrow=85,ncol=60,byrow=FALSE)
}

for (i in 1:12) {
  image(E_images[[i]])
}
