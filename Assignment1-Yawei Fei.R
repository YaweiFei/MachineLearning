rm(list=ls())
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    library(thispackage, character.only = T)
  }
}
needed  <-  c("ISLR", 'FNN')  #class contains the knn() function
installIfAbsentAndLoad(needed)
##################### 
#### QUESTION 1 #### 
#####################
HomePrice <- read.table('HomePrices.txt', header = T, sep ='\t')
n <- nrow(HomePrice)
p <- ncol(HomePrice)-1
#calculate the MSE
print(paste("MSE of Predicting House Price:", 
            (mean((HomePrice$medv-mean(HomePrice$medv))^2))))

print(paste("Variance of Predicting House Price:",
            (var(HomePrice$medv)*(n-1)/n)))
HomePrice[1:p] <- scale(HomePrice[1:p], center = T, scale = T)
head(HomePrice)
#we want a 75/15/10 split of rows for training,validation and test respectively
set.seed(5072)
trainprop <- 0.75
validateprop <- 0.15
################################################################ 
## Create three random collectively exhaustive and mutually
## exclusive subsets with the above proportions of observations   
################################################################ 
train <- sample(n, n*trainprop)
validate <- sample(setdiff(1:n, train), validateprop*n) 
test <- setdiff(setdiff(1:n, train), validate)

# Create the data frames using the indices created in the three vectors above
trainset <- HomePrice[train,]
validateset <- HomePrice[validate,]
testset <- HomePrice[test,]
head(trainset, n=1) # verify the dataframes
head(validateset, n=1) # verify the dataframes
head(testset, n=1)  # verify the dataframes
#create training,validate and test rows and training,validate,test rows from medv column
train.x <- trainset[-13]
train.y <- as.numeric(trainset$medv)
validate.x <- validateset[-13]
validate.y <- as.numeric(validateset$medv)
test.x <- testset[-13]
test.y <- as.numeric(testset$medv)
#run knn for train and validate sample and calculate MSEs(k=19,17,15,13,...1)
validateMSEs <- rep(0,19)
trainMSEs <- rep(0,19)
kset <- seq(1, 19, by = 2)
for(k in kset) {
  validate.pred <- knn.reg(train.x, 
                           validate.x,  
                           train.y, 
                           k = k)
  
  validateMSEs[k] <- mean((validate.y -validate.pred$pred)^2)
  train.pred <- knn.reg(train.x, 
                        train.x, 
                        train.y, 
                        k = k)
  
  trainMSEs[k] <- mean((train.y - train.pred$pred)^2)   
}

validateMSEs <- validateMSEs[kset]
trainMSEs <- trainMSEs[kset]
#plot the MSEs when k=1,3,5,7...19
plot(NULL, NULL, type='n', 
     xlim=c(19, 1), 
     ylim=c(0,max(c(validateMSEs, trainMSEs))), 
     xlab='Increasing Flexibility (Decreasing k)', 
     ylab='Mean Squared Errors', 
     main='MSE as a Function of \n Flexibility for KNN Regression')
lines(seq(19, 1, by= -2), 
      validateMSEs[length(validateMSEs):1], 
      type='b', 
      col=2, 
      pch=16)
lines(seq(19, 1, by= -2), 
      trainMSEs[length(trainMSEs):1], 
      type='b', 
      col=1, 
      pch=16)
legend("topright", legend = c("Validation MSEs", "Training MSEs"), 
       col=c(2, 1), 
       cex=.75, 
       pch=16)
print(paste("My 'best' validate MSE occurred with k =", kset[which.min(validateMSEs)],
            "and produced a validate MSE of", validateMSEs[which.min(validateMSEs)]))
print(paste("My 'best' train MSE occurred with k =", kset[which.min(trainMSEs)],
            "and produced a train MSE of", trainMSEs[which.min(trainMSEs)]))
#predict medv using k=3 and calculate the MSE for test sample
test.pred <- knn.reg(train.x, 
                     test.x, 
                     train.y, 
                     k = 3)
testMSEs <- mean((test.y - test.pred$pred)^2)  
print(paste("when k = 3, the MSE for testset is", testMSEs))

##################### 
#### QUESTION 2 #### 
#####################
LoanData<-read.table('LoanData.csv',header=T,sep=',',stringsAsFactors = F)
Yes_ErrorRate<-sum(LoanData$loan_repaid!='Yes')/length((LoanData$loan_repaid))
paste("The error rate of always predicting Yes is:", Yes_ErrorRate)
LoanData[1:7]<-scale(LoanData[1:7], center = T, scale=T)
head(LoanData)
n=nrow(LoanData)
set.seed(5072)
#create training,validateand test sample 75/15/10
trainprop<-0.75
validateprop<-0.15
train<-sample(n,n*trainprop)
validate<-sample(setdiff(1:n,train),validateprop*n)
test<-setdiff(setdiff(1:n,train),validate)
trainset<-LoanData[train,]
validateset<-LoanData[validate,]
testset<-LoanData[test,]
head(trainset,n=1)#verify dataframe
head(validateset,n=1)#verify dataframe
head(testset,n=1)#verify dataframe
#create training,validate and test rows and training,validate,test rows from loan.repaid column
train.x <- trainset[,-8]
train.y <- trainset[,8]
validate.x <- validateset[,-8]
validate.y <- validateset[,8]
test.x <- testset[,-8]
test.y <- testset[,8]

validate.errors <- rep(0,19 %/% 2+1)
train.errors <- rep(0,19 %/% 2+1)

kset <- seq(1, 19, by = 2)
for(k in kset) {
  validate.pred <- knn(train.x, 
                       validate.x,  
                       train.y, 
                       k = k)
  
  validate.errors[k %/% 2+1] <- mean(validate.y != validate.pred)
  
  train.pred <- knn(train.x, 
                    train.x, 
                    train.y, 
                    k = k)
  
  train.errors[k %/% 2+1] <- mean(train.y != train.pred)
}
plot(NULL, NULL, type='n', 
     xlim=c(19, 1), 
     ylim=c(0,max(c(validate.errors, train.errors))), 
     xlab='Increasing Flexibility (Decreasing k)', 
     ylab='Error Rates', 
     main='Error Rates as a Function of \n Flexibility for KNN Classification')
lines(seq(19, 1, by= -2), 
      validate.errors[length(validate.errors):1], 
      type='b', 
      col=2, 
      pch=16)
lines(seq(19, 1, by= -2), 
      train.errors[length(train.errors):1], 
      type='b', 
      col=1, 
      pch=16)

legend("topleft", legend = c("Validation Error Rate", "Train Error Rate"), 
       col=c(2, 1), 
       cex=.70, 
       pch=16)

print(paste("My 'best' validate error occurred with k =", kset[which.min(validate.errors)],
            "and produced a validate error rate of", validate.errors[which.min(validate.errors)]))

print(paste("My 'best' train error occurred with k =", kset[which.min(train.errors)],
            "and produced a train error rate of", train.errors[which.min(train.errors)]))

set.seed(5072)
test.pred <- knn(train.x, 
                 test.x,  
                 train.y, 
                 k = 11)
test.errors <- mean(test.y != test.pred)
print(paste("My test error when k = 11 is", test.errors))
##################### 
#### QUESTION 3 #### 
#####################
HomePrice <- read.table('HomePrices.txt', header = T, sep ='\t')
n <- nrow(HomePrice)
p <- ncol(HomePrice)-1
print(paste("MSE of Predicting House Price:", 
            (mean((HomePrice$medv-mean(HomePrice$medv))^2))))

print(paste("Variance of Predicting House Price:",
            (var(HomePrice$medv)*(n-1)/n)))
HomePrice[1:p] <- scale(HomePrice[1:p], center = T, scale = T)

set.seed(5072)
trainprop <- 0.75
validateprop <- 0.15

validateBestMSEs <- rep(0,50)
testBestMSEs <- rep(0,50)
for(i in 1:50){
  train <- sample(n, n*trainprop)
  validate <- sample(setdiff(1:n, train), validateprop*n) 
  test <- setdiff(setdiff(1:n, train), validate)
  trainset <- HomePrice[train,]
  validateset <- HomePrice[validate,]
  testset <- HomePrice[test,]
  
  train.x <- trainset[-13]
  train.y <- as.numeric(trainset$medv)
  validate.x <- validateset[-13]
  validate.y <- as.numeric(validateset$medv)
  test.x <- testset[-13]
  test.y <- as.numeric(testset$medv)
  
  validateMSEs <- rep(0,19)
  trainMSEs <- rep(0,19)
  krange <- seq(1, 19, by = 2)
  
  for(k in krange) {
    validate.pred <- knn.reg(train.x, 
                             validate.x,  
                             train.y, 
                             k = k)
    
    validateMSEs[k] <- mean((validate.y -validate.pred$pred)^2)
  }
  
  validateMSEs <- validateMSEs[krange]
  validateBestMSEs[i] <- validateMSEs[which.min(validateMSEs)];
  print(paste("Best validate MSE is ", validateMSEs[which.min(validateMSEs)], " when K = ", krange[which.min(validateMSEs)]))
  
  test.pred <- knn.reg(train.x, 
                       test.x, 
                       train.y, 
                       k = krange[which.min(validateMSEs)])
  testMSEs <- mean((test.y - test.pred$pred)^2)  
  testBestMSEs[i] <- testMSEs;
}

print(paste("My mean value of validate MSEs is:", mean(validateBestMSEs)))
print(paste("My sdandard deviation of validate MSEs is:", sd(validateBestMSEs)))
print(paste("My mean value of test MSEs is:", mean(testBestMSEs)))
print(paste("My sdandard deviation of test MSEs is:", sd(testBestMSEs)))

plot(NULL, NULL, type='n', 
     xlim=c(1, 50), 
     ylim=c(0,max(c(validateBestMSEs, testBestMSEs))), 
     xlab='Replication', 
     ylab='MSEs', 
     main='Test and Best Validation MSEs for Many Partitionings of the Data')
lines(seq(1, 50, by= 1), 
      validateBestMSEs, 
      type='b', 
      col=2, 
      pch=16)
lines(seq(1, 50, by= 1), 
      rep(mean(validateBestMSEs), 50), 
      type='b', 
      col=2,
      pch='-')
lines(seq(1, 50, by= 1), 
      testBestMSEs, 
      type='b', 
      col=1, 
      pch=16)
lines(seq(1, 50, by= 1), 
      rep(mean(testBestMSEs), 50), 
      type='b', 
      col=1,
      pch='-')
legend("topright",legend=c("validation MSEs","validation MSE mean","Test MSEs","Test MSE mean"),
       col=c(2,2,1,1),
       lty=1:2,
       cex = 0.75,
       pch=16)
set.seed(seed=NULL)
print(paste("Based on the t test,the p value is 0.38873 >0.05, We cannot reject the null hypothesis that he discrepancy in Question 1
is the same with the discrepancies in the two paired validate and test samples. it is not that we are unlucky "))
##################### 
#### QUESTION 4 #### 
#####################
ApplicationData <- read.table('Applications.train.csv', header = T, sep =',')

P <- ncol(ApplicationData)
n <- nrow(ApplicationData)

head(ApplicationData)

ApplicationData[2:P] <- scale(ApplicationData[2:P], center = T, scale = T)

trainprop <- 0.75
validateprop <- 0.25
krange <- seq(1, 19, by = 2)

train <- function(data){
  train <- sample(n, n*trainprop)
  validate <- sample(setdiff(1:n, train), validateprop*n) 
  
  trainset <- data[train,]
  validateset <- data[validate,]
  
  head(trainset, n=1)
  head(validateset, n=1)
  
  train.x <- trainset[-1]
  train.y <- as.numeric(trainset$Application)
  validate.x <- validateset[-1]
  validate.y <- as.numeric(validateset$Application)
  
  validateMSEs <- rep(0,19)
  
  for(k in krange) {
    validate.pred <- knn.reg(train.x, 
                             validate.x,  
                             train.y, 
                             k = k)
    
    validateMSEs[k] <- mean((validate.y -validate.pred$pred)^2)
  }
  
  return(validateMSEs)
}

pickDataAndTrain <- function(removedColumns){
  data <- ApplicationData[-removedColumns]
  set.seed(5072)
  validateMSEs <- rep(0, 19)
  for(i in 1:50){
    validateMSEs <- validateMSEs + train(data)
  }
  
  validateMSEs <- validateMSEs[krange]
  bestK <- krange[which.min(validateMSEs)]
  MSE <- validateMSEs[which.min(validateMSEs)] / 50
  return(list(bestK, MSE))
}

print("Testing removing different variables")
for(i in 2:17){
  print(paste("Testing removing", colnames(ApplicationData)[i]))
  result <- pickDataAndTrain(i)
  print(paste(i, "best k:", result[[1]], "MSE:", result[[2]]))
}

result = pickDataAndTrain(c(2,3,4,5,6,7,8,9,12,14,16))

print("My best calculation is removing columns: PT.Undergrads, RoomAndBoard, OutOfState, Percent.Donating.Alumni, Spending.On.Students, Book.Costs, Top25.Percent, PhD.Students, Student.Faculty.Ratio, Terminal.Degrees, Personal.Costs")
print(paste("My average MSE of 50 runs is:", result[[2]], "at K = ", result[[1]]))

