rm(list=ls())
##########################################################################################
### Functions
##########################################################################################
installIfAbsentAndLoad  <-  function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
##############################
### Load required packages ###
##############################
# MASS contains the Boston data
needed   <-   c("ISLR", "MASS", "corrplot", "class")      #MASS contains the qda() function
installIfAbsentAndLoad(needed)
##########################
#### QUESTION 1 Part a ####
##########################
set.seed(5072)
##########################
#### QUESTION 1 Part b ####
##########################
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = Weekly,family = binomial)
summary(glm.fit)
print(paste('The only statistically significant predictor is Lag2'))
##########################
#### QUESTION 1 Part c ####
##########################
glm.probs=predict(glm.fit,type="response")
glm.pred <- rep("Down", nrow(Weekly))
glm.pred[glm.probs > .5] <- "Up"
confusion_matrix <-print(table(Weekly$Direction, glm.pred))
##########################
#### QUESTION 1 Part d ####
##########################
correctRate<-(confusion_matrix[1,1]+confusion_matrix[2,2])/sum(confusion_matrix)
errorRate<-1-correctRate
Type1Error<-confusion_matrix[1,2]/(confusion_matrix[1,1]+confusion_matrix[1,2])
Type2Error<-confusion_matrix[2,1]/(confusion_matrix[2,1]+confusion_matrix[2,2])
powerofmodel<-1-Type2Error
precisionofmodel<-confusion_matrix[2,2]/(confusion_matrix[1,2]+confusion_matrix[2,2])
print(paste('The overall fraction of correct predictions is',correctRate))
print(paste('The overall error rate is',errorRate))
print(paste('TypeI error rate is',Type1Error))
print(paste('TypeII error rate is',Type2Error))
print(paste('The power of the model is',powerofmodel))
print(paste('The precision of the model is',precisionofmodel))
##########################
#### QUESTION 1 Part e ####
##########################
train<-Weekly$Year<=2008    #train set
test<-Weekly[!train,]       #test row
glm.fit2 <- glm(Direction~Lag2, 
               data=Weekly, 
               family=binomial, 
               subset=train)
##########################
#### QUESTION 1 Part f ####
##########################
glm.probs2<- predict(glm.fit2,type="response",newdata = test)
glm.pred2 <- rep("Down", nrow(test))
glm.pred2[glm.probs2 > .5] <- "Up"
confusion_matrix2<-print(table(test$Direction,glm.pred2))
correctRate2<-(confusion_matrix2[1,1]+confusion_matrix2[2,2])/sum(confusion_matrix2)
errorRate2<-(confusion_matrix2[2,1]+confusion_matrix2[1,2])/sum(confusion_matrix2)
Type1Error2<-confusion_matrix2[1,2]/(confusion_matrix2[1,1]+confusion_matrix2[1,2])
Type2Error2<-confusion_matrix2[2,1]/(confusion_matrix2[2,1]+confusion_matrix2[2,2])
powerofmodel2<- 1-Type2Error2
precisionofmodel2<-confusion_matrix2[2,2]/(confusion_matrix2[1,2]+confusion_matrix2[2,2])
print(paste('The overall fraction of correct predictions is',correctRate2))
print(paste('The overall error rate is',errorRate2))
print(paste('TypeI error rate is',Type1Error2))
print(paste('TypeII error rate is',Type2Error2))
print(paste('The power of the model is',powerofmodel2))
print(paste('The precision of the model is',precisionofmodel2))
##########################
#### QUESTION 1 Part g ####
##########################
lda.fit<-lda(Direction~Lag2,data = Weekly,subset = train)
lda.predict<-predict(lda.fit,test)
ldacm<-print(table(test$Direction,lda.predict$class))
correctRate3<-mean(lda.predict$class==test$Direction)
errorRate3<-1-correctRate3
Type1Error3<-ldacm[1,2]/(ldacm[1,1]+ldacm[1,2])
Type2Error3<-ldacm[2,1]/(ldacm[2,1]+ldacm[2,2])
powerofmodel3<-1-Type2Error3
precisionofmodel3<-ldacm[2,2]/(ldacm[1,2]+ldacm[2,2])
print(paste('The overall fraction of correct predictions is',correctRate3))
print(paste('The overall error rate is',errorRate3))
print(paste('TypeI error rate is',Type1Error3))
print(paste('TypeII error rate is',Type2Error3))
print(paste('The power of the model is',powerofmodel3))
print(paste('The precision of the model is',precisionofmodel3))
##########################
#### QUESTION 1 Part h ####
##########################
qda.fit<-qda(Direction~Lag2,data=Weekly,subset = train)
qda.pred<-predict(qda.fit,test)
qdacm<-print(table(test$Direction,qda.pred$class))
correctRate4<-mean(qda.pred$class==test$Direction)
errorRate4<-1-correctRate4
Type1Error4<-qdacm[1,2]/(qdacm[1,1]+qdacm[1,2])
Type2Error4<-qdacm[2,1]/(qdacm[2,1]+qdacm[2,2])
powerofmodel4<-1-Type2Error4
precisionofmodel4<-qdacm[2,2]/(qdacm[1,2]+qdacm[2,2])
print(paste('The overall fraction of correct predictions is',correctRate4))
print(paste('The overall error rate is',errorRate4))
print(paste('TypeI error rate is',Type1Error4))
print(paste('TypeII error rate is',Type2Error4))
print(paste('The power of the model is',powerofmodel4))
print(paste('The precision of the model is',precisionofmodel4))
##########################
#### QUESTION 1 Part i ####
##########################
train.x<-cbind(Weekly$Lag2[train==TRUE])
test.x<-cbind(Weekly$Lag2[train==FALSE])
train.y<-Weekly$Direction[train==TRUE]
erroroverall<-c()
i<-1
for (k in seq(1,99,2)){
  knn.pred<-knn(train.x,test.x,train.y,k=k)
  knncm<-table(test$Direction,knn.pred)
  erroroverall[i]<-(knncm[1,2]+knncm[2,1])/sum(knncm)
  i<-i+1
}

index<-which(erroroverall==min(erroroverall))
bestk<-2*index-1
##########################
#### QUESTION 1 Part j ####
##########################
knn.pred<-knn(train.x,test.x,train.y,k=bestk)
knncm<-print(table(test$Direction,knn.pred))
knncorrect<-(knncm[1,1]+knncm[2,2])/sum(knncm)
knnerror<-1-knncorrect
knntype1error<-knncm[1,2]/(knncm[1,1]+knncm[1,2])
knntype2error<-knncm[2,1]/(knncm[2,1]+knncm[2,2])
knnpower<-1-knntype2error
knnprecision<-knncm[2,2]/(knncm[1,2]+knncm[2,2])
print(paste('The overall fraction of correct predictions is',knncorrect))
print(paste('The overall error rate is',knnerror))
print(paste('TypeI error rate is',knntype1error))
print(paste('TypeII error rate is',knntype2error))
print(paste('The power of the model is',knnpower))
print(paste('The precision of the model is',knnprecision))
##########################
#### QUESTION 1 Part k 
##########################
print(paste('Judging by overall error rate Logistic regression and LDA provide the lowest overall error rate on the test data'))
##########################
#### QUESTION 2 Part a 
##########################
rm(list=ls())
set.seed(5072)
##########################
#### QUESTION 2 Part b 
##########################
Auto$mpg01<-with(ifelse(mpg>median(mpg), "1", "0"), data=Auto)
##########################
#### QUESTION 2 Part c 
##########################
trainrows<- sample(1:nrow(Auto), .80*nrow(Auto))
trainset<-Auto[trainrows,]
testset<-Auto[-trainrows,]
##########################
#### QUESTION 2 Part d 
##########################
glm.fit <- glm(as.factor(mpg01)~ cylinders + displacement + weight, 
               data=trainset, 
               family=binomial)
##########################
#### QUESTION 2 Part e 
##########################
glm.probs <- predict(glm.fit, testset,type="response")
predicted<-rep('0',nrow(testset))
predicted[glm.probs>0.5]<-'1'
actuals<-testset$mpg01
mytesttable <- table(actuals, predicted)
rownames(mytesttable)<-c('Below','Above')
colnames(mytesttable)<-c('Below','Above')
print(mytesttable)
correct<-(mytesttable[1,1]+mytesttable[2,2])/sum(mytesttable)
error<-1-correct
type1error<-mytesttable[1,2]/(mytesttable[1,1]+mytesttable[1,2])
type2error<-mytesttable[2,1]/(mytesttable[2,1]+mytesttable[2,2])
power<-1-type2error
precision<-mytesttable[2,2]/(mytesttable[1,2]+mytesttable[2,2])
print(paste('The overall fraction of correct predictions is',correct))
print(paste('The overall error rate is',error))
print(paste('TypeI error rate is',type1error))
print(paste('TypeII error rate is',type2error))
print(paste('The power of the model is',power))
print(paste('The precision of the model is',precision))
##########################
#### QUESTION 2 Part f 
##########################
lda.fit<-lda(as.factor(mpg01)~cylinders + displacement + weight,data=trainset)
lda.pred<-predict(lda.fit,testset)
actuals<-testset$mpg01
predicted<-lda.pred$class
mytable<-table(actuals,predicted)
rownames(mytable)<-c('Below','Above')
colnames(mytable)<-c('Below','Above')
print(mytable)
correct1<-(mytable[1,1]+mytable[2,2])/sum(mytable)
error1<-1-correct1
ldatype1error<-mytable[1,2]/(mytable[1,1]+mytable[1,2])
ldatype2error<-mytable[2,1]/(mytable[2,1]+mytable[2,2])
ldapower<-1-ldatype2error
ldaprecision<-mytable[2,2]/(mytable[1,2]+mytable[2,2])
print(paste('The overall fraction of correct predictions is',correct1))
print(paste('The overall error rate is',error1))
print(paste('TypeI error rate is',ldatype1error))
print(paste('TypeII error rate is',ldatype2error))
print(paste('The power of the model is',ldapower))
print(paste('The precision of the model is',ldaprecision))
##########################
#### QUESTION 2 Part g 
##########################
qda.fit<-qda(as.factor(mpg01)~cylinders + displacement + weight,data=trainset)
qda.pred<-predict(qda.fit,testset)
actuals<-testset$mpg01
predicted<-qda.pred$class
mytable<-table(actuals,predicted)
rownames(mytable)<-c('Below','Above')
colnames(mytable)<-c('Below','Above')
print(mytable)
correct2<-(mytable[1,1]+mytable[2,2])/sum(mytable)
error2<-1-correct2
qdatype1error<-mytable[1,2]/(mytable[1,1]+mytable[1,2])
qdatype2error<-mytable[2,1]/(mytable[2,1]+mytable[2,2])
qdapower<-1-qdatype2error
qdaprecision<-mytable[2,2]/(mytable[1,2]+mytable[2,2])
print(paste('The overall fraction of correct predictions is',correct2))
print(paste('The overall error rate is',error2))
print(paste('TypeI error rate is',qdatype1error))
print(paste('TypeII error rate is',qdatype2error))
print(paste('The power of the model is',qdapower))
print(paste('The precision of the model is',qdaprecision))
##########################
#### QUESTION 2 Part h 
##########################
Auto1<-scale(Auto[,c('cylinders','displacement','weight')])
train.x <- Auto1[trainrows, ]
train.y<-cbind(Auto$mpg01)[trainrows,]
test.x<-Auto1[-trainrows, ]
erroroverall<-c()
i<-1
for (k in seq(1,11,2)){
  knn.pred<-knn(train.x,test.x,train.y,k=k)
  knncm<-table(testset$mpg01,knn.pred)
  erroroverall[i]<-(knncm[1,2]+knncm[2,1])/sum(knncm)
  i<-i+1
}
index<-which(erroroverall==min(erroroverall))
bestk<-2*index-1
##########################
#### QUESTION 2 Part i 
##########################
predicted<-knn(train.x,test.x,train.y,k=bestk)
actuals<-testset$mpg01
knncm<-table(actuals,predicted)
rownames(knncm)<-c('Below','Above')
colnames(knncm)<-c('Below','Above')
print(knncm)
knncorrectrate<-(knncm[1,1]+knncm[2,2])/sum(knncm)
knnerrorrate<-1-knncorrectrate
knntype1Er<-knncm[1,2]/(knncm[1,1]+knncm[1,2])
knntype2Er<-knncm[2,1]/(knncm[2,1]+knncm[2,2])
knnmodelpower<-1-knntype2Er
knnmodelprecision<-knncm[2,2]/(knncm[1,2]+knncm[2,2])
print(paste('The overall fraction of correct predictions is',knncorrectrate))
print(paste('The overall error rate is',knnerrorrate))
print(paste('TypeI error rate is',knntype1Er))
print(paste('TypeII error rate is',knntype2Er))
print(paste('The power of the model is',knnmodelpower))
print(paste('The precision of the model is',knnmodelprecision))
##########################
#### QUESTION 2 Part j 
##########################
print(paste('Judging by the overall error rate knn model has the lowest overall error rate'))
##########################
#### QUESTION 3 Part a 
##########################
rm(list=ls())
set.seed(5072)
##########################
#### QUESTION 3 Part b 
##########################
library(MASS)
trainrow<-sample(1:nrow(Boston),size=0.80*nrow(Boston))
Boston$crim01<-with(ifelse(crim>median(crim), "1", "0"), data=Boston)
trainset<-Boston[trainrow,]
testset<-Boston[-trainrow,]
##########################
#### QUESTION 3 Part c 
##########################
glm.fit <- glm(as.factor(trainset$crim01)~nox+rad+dis,data=trainset,family=binomial)
glm.prob<-predict(glm.fit,testset,type="response")
##########################
#### QUESTION 3 Part d 
##########################
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
needed  <-  c("pROC", "verification") 
installIfAbsentAndLoad(needed)
predicted <- rep("0", nrow(testset))  
predicted[glm.prob>.5] <- "1"
actuals<-testset$crim01
logistictable1<- table(actuals, predicted)


###Plot the ROC curve.
roc.plot(as.integer(as.numeric(actuals)),glm.prob,main="ROC Curve for Logistic Regression")
aucc <- roc.area(as.numeric(actuals),glm.prob)$A
## AUC
print(paste("AUC:",aucc))
## confusion matrix with cutoff of 0.5
print(logistictable1)
## confusion matrix with cutoff of 0.4
predicted <- rep("0", nrow(testset))  
predicted[glm.prob>.4] <- "1"
actuals<-testset$crim01
logistictable2<- table(actuals, predicted)
print(logistictable2)
##########################
#### QUESTION 3 Part e 
##########################
lda.fit<-lda(as.factor(trainset$crim01)~nox+rad+dis,data=trainset)
lda.pred<-predict(lda.fit, testset)
predicted<-lda.pred$class
###Plot the ROC curve.
roc.plot(as.integer(as.numeric(actuals)),lda.pred$posterior[,2], 
         main="ROC Curve for LDA")
##AUC
aucc <- roc.area(as.numeric(actuals),lda.pred$posterior[,2])$A
print(paste('AUC:',aucc))
##confusion matrix with cutoff 0.5
predicted <- rep("0", nrow(testset))  
predicted[lda.pred$posterior[,2]>.5] <- "1"
actuals<-testset$crim01
ldatable1<- table(actuals, predicted)
print(ldatable1)
##confusion matrix with cutoff 0.4
predicted <- rep("0", nrow(testset))  
predicted[lda.pred$posterior[,2]>.4] <- "1"
actuals<-testset$crim01
ldatable2<- table(actuals, predicted)
print(ldatable2)
##########################
#### QUESTION 3 Part f 
##########################
qda.fit<-qda(as.factor(trainset$crim01)~nox+rad+dis,data=trainset)
qda.pred<-predict(qda.fit, testset)
predicted<-qda.pred$class
###Plot the ROC curve.
roc.plot(as.integer(as.numeric(actuals)),qda.pred$posterior[,2], 
         main="ROC Curve for QDA")
##AUC
aucc <- roc.area(as.numeric(actuals),qda.pred$posterior[,2])$A
print(paste('AUC:',aucc))
##confusion matrix with cutoff 0.5
predicted <- rep("0", nrow(testset))  
predicted[qda.pred$posterior[,2]>.5] <- "1"
actuals<-testset$crim01
qdatable1<- table(actuals, predicted)
print(qdatable1)
##confusion matrix with cutoff 0.4
predicted <- rep("0", nrow(testset))  
predicted[qda.pred$posterior[,2]>.4] <- "1"
actuals<-testset$crim01
qdatable2<- table(actuals, predicted)
print(qdatable2)
##########################
#### QUESTION 3 Part g 
##########################
Boston1<-scale(Boston[,c('nox','rad','dis')])
train.x<-Boston1[trainrow,]
train.y<-cbind(Boston$crim01)[trainrow,]
test.x<-Boston1[-trainrow,]
errorrate<-c()
i<-1
for (k in seq(1,49,2)){
  knn.pred <- knn(train.x, test.x,train.y, k=k)
  knncm<-table(testset$crim01,knn.pred)
  errorrate[i]<-(knncm[1,2]+knncm[2,1])/sum(knncm)
  i<-i+1
}
index<-which.min(errorrate)  #index<-which(errorrate==min(errorrate))
bestk<-2*index-1
##########################
#### QUESTION 3 Part h 
##########################
predicted<-knn(train.x,test.x,train.y,k=bestk)
actuals<-testset$crim01
knncm<-print(table(actuals,predicted))
print(paste('The lowest test error rate occured when k=',bestk,'and produced a test error of',errorrate[index]))
##########################
#### QUESTION 3 Part i 
##########################
logisticerror<-(logistictable1[1,2]+logistictable1[2,1])/sum(logistictable1)
ldaerror<-(ldatable1[1,2]+ldatable1[2,1])/sum(ldatable1)
qdatype1error<-(qdatable1[1,2]+qdatable1[2,1])/sum(qdatable1)
knnerror<-errorrate[index]
print(paste("It appears that when cutoff=0.5, knn model has the lowest errorrate among three 
when k=1 and knn testerror is",knnerror))
##########################
#### QUESTION 3 Part j 
##########################
logisticpower2<-logistictable2[2,2]/(logistictable2[2,1]+logistictable2[2,2])
ldapower2<-ldatable2[2,2]/(ldatable2[2,1]+ldatable2[2,2])
qdapower2<-qdatable2[2,2]/(qdatable2[2,1]+qdatable2[2,2])
print(paste('it appears that the power of my logistic model at cutoff=0.4 was the highest at',logisticpower2))