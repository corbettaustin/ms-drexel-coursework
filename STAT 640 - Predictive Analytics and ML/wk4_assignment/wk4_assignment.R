dat_train <- read.csv("HousingBoston_train.csv", stringsAsFactors = TRUE)
dat_test <- read.csv("HousingBoston_test.csv", stringsAsFactors = TRUE)

library(class)
set.seed(42)

#q1
#check the target variable dist
table(dat_train$mvalue)
table(dat_test$mvalue)
prop.table(table(dat_train$mvalue))
prop.table(table(dat_test$mvalue))

#check correlation btwn rad and tax
cor(dat_train$rad, dat_train$tax)

#check correlation btwn dis and age
cor(dat_train$dis, dat_train$age)

#proportions similar, positive and neg correlations
q1 <- c(2, 4)

#q2
#create boxplot
boxplot(dat_train$zn, main="Boxplot of zn variable")

#calc outliers
Q1 <- quantile(dat_train$zn, 0.25)
Q3 <- quantile(dat_train$zn, 0.75)
IQR_val <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

outliers <- dat_train$zn[dat_train$zn < lower_bound | dat_train$zn > upper_bound]
num_outliers <- length(outliers)
num_outliers

q2 <- 2

# scale values for analysis, not including the mvalue column
Xtrain <- scale(dat_train[, -13])
Xtest <- scale(dat_test[, -13])
# define the target variables for training and test set
cl <- dat_train$mvalue
Xval <- dat_test$mvalue

#q3 run knn with k=12
knn_pred_12 <- knn(train = Xtrain, test = Xtest, cl = cl, k = 12)

#calc misclassification rate
misclass_12 <- mean(knn_pred_12 != Xval)
q3 <- misclass_12

#q4
#it is the percentage of a predicted class being assigned to the wrong class
q4 <- 3

#q5
#run knn where prob=true
knn_pred_12_prob <- knn(train = Xtrain, test = Xtest, cl = cl, k = 12, prob = TRUE)
q5 <- 9

#q6
#get probabilities
probs <- attr(knn_pred_12_prob, "prob")

#tie occurs
ties <- sum(probs == 0.5)

q6 <- ties

#q7
#randomly select a class
q7 <- 1

#q8 
# more neighbors, less complex, less agile
q8 <- c(2, 3, 5)

#q9
#initialize vectors
train_errors <- numeric(50)
test_errors <- numeric(50)

#loop through k values 1-50
for (k in 1:50) {
knn_train <- knn(train = Xtrain, test = Xtrain, cl = cl, k = k)
train_errors[k] <- mean(knn_train != cl)

#test and train errors
knn_test <- knn(train = Xtrain, test = Xtest, cl = cl, k = k)
test_errors[k] <- mean(knn_test != Xval)

knn_test <- knn(train = Xtrain, test = Xtest, cl = cl, k = k)
test_errors[k] <- mean(knn_test != Xval)
}

#plot misclassification rates
plot(1:50, train_errors, type="b", col="blue", ylim=c(0, max(c(train_errors, test_errors))),
     xlab="k", ylab="Misclassification Rate", main="k-NN Misclassification Rates")
lines(1:50, test_errors, type="b", col="red")
legend("topright", legend=c("Train", "Test"), col=c("blue", "red"), lty=1)

q9 <- 4

#q10
#apply PCA for variable selection
pc <- prcomp(Xtrain)

#cumulative proportion
pve <- cumsum(pc$sdev^2)/sum(pc$sdev^2)
plot(pve, t="b", col="blue", xlab="PC", xaxp=c(1,13,3),
     main="Proportion of variance explained", ylab="")

#90% variance
d <- which(pve > 0.9)[1]

pcTrain <- pc$x[, 1:d]

pcTest <- predict(pc, newdata=Xtest)[, 1:d]

#initialize vectors for PCA errors
pca_train_errors <- numeric(50)
pca_test_errors <- numeric(50)

#loop through k from 1-50 with PCA data
knn_train_pca <- knn(train = pcTrain, test = pcTrain, cl = cl, k = k)
pca_train_errors[k] <- mean(knn_train_pca != cl)

#plot PCA misclassification rate
plot(1:50, pca_train_errors, type="b", col="blue", ylim=c(0, max(c(pca_train_errors, pca_test_errors))),
     xlab="k", ylab="Misclassification Rate", main="k-NN with PCA Misclassification Rates")
lines(1:50, pca_test_errors, type="b", col="red")
legend("topright", legend=c("Train", "Test"), col=c("blue", "red"), lty=1)

#calculate mean errors
mean_pca_train <- mean(pca_train_errors)
mean_pca_test <- mean(pca_test_errors)
mean_nonpca_train <- mean(train_errors)
mean_nonpca_test <- mean(test_errors)

#calculate relative errors
rel_err_train <- mean_pca_train / mean_nonpca_train
rel_err_test <- mean_pca_test / mean_nonpca_test

q10 <- c(rel_err_train, rel_err_test)
