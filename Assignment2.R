rm(list=ls())
#######################
### Functions
#######################
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    library(thispackage, character.only = T)
  }
}
##############################
### Load required packages ###
##############################
## MASS contains the Boston data, car contains the vif()
## function
needed  <-  c("ISLR", "MASS", "car")      
installIfAbsentAndLoad(needed)

##########################
#### QUESTION 1 Part a ####
##########################
set.seed(5072)
##########################
#### QUESTION 1 Part b ####
##########################
x<-rnorm(100,mean=0,sd=1)
##########################
#### QUESTION 1 Part c ####
##########################
eps<-rnorm(100,mean=0,sd=0.5)
##########################
#### QUESTION 1 Part d ####
##########################
y<- -1+0.5*x+eps
##########################
#### QUESTION 1 Part e ####
##########################
length(y)
##########################
#### QUESTION 1 Part f ####
##########################
print(paste('Population parameter value of  β0 is -1 and β1 is 0.5'))
##########################
#### QUESTION 1 Part g ####
##########################
plot(x,y,title('Moderate Error in the Population'))
##########################
#### QUESTION 1 Part h ####
##########################
print(paste('the relationship of x and y are positive and they have linear relationship with some noise
by the eps variable'))
##########################
#### QUESTION 1 Part i ####
##########################
lm.fit1 <- lm(y ~ x)
summary(lm.fit1)
##########################
#### QUESTION 1 Part j ####
##########################
a<-coef(lm.fit1)
print(paste('Coefficient estimate β0 hat is', a[1],'Coefficient estimate β1 hat is', a[2]))
print(paste('β0 hat is very close to real coefficience β0 but β1 hat has slightly difference from
            population coefficient β1'))
##########################
#### QUESTION 1 Part k ####
##########################
abline(lm.fit1, col="black")
##########################
#### QUESTION 1 Part l ####
##########################
intercept<- -1
slope<- 0.5
abline(intercept,slope,pch=2,col=2)
##########################
#### QUESTION 1 Part m ####
##########################
legend("bottomright", legend=c("least Squares", 'Population'),
       col=c("black", "red"), lty=1:1, cex=0.8)
##########################
#### QUESTION 1 Part n ####
##########################
lm.fit2=lm(y~x+I(x^2))
summary(lm.fit2)
##########################
#### QUESTION 1 Part o ####
##########################
anova(lm.fit1,lm.fit2)
print(paste('Quadratic term does not improve model fit. The null hypothesis of anova test is that
            two model fits equally well and P value is greater than 0.05 so we cannot reject 
            the null hypothesis'))
##########################
#### QUESTION 1 Part p ####
##########################
eps2<-rnorm(100,mean=0,sd=sqrt(0.1))
y.less<- -1+0.5*x+eps2
length(y.less)
print(paste('Population parameter value of  β0 is -1 and β1 is 0.5'))
plot(x,y.less,title('Less Error in the Population'))
print(paste('the relationship of x and y are positive and they have very high linearity and the variance is reduced'))
lm.fit3 <- lm(y.less ~ x)
summary(lm.fit3)
b<-coef(lm.fit3)
print(paste('Coefficient estimate beta0 is', b[1],'Coefficient estimate beta1 is', b[2]))
print(paste('the coefficient estimates are very close to real coefficience β0 and β1'))
abline(lm.fit3, col="black")
intercept<- -1
slope<- 0.5
abline(intercept,slope,pch=2,col=2)
legend("bottomright", legend=c("least Squares", 'Population'),
       col=c("black", "red"), lty=1:1, cex=0.8)
##########################
#### QUESTION 1 Part q ####
##########################
eps3<-rnorm(100,mean=0,sd=sqrt(0.5))
y.more<- -1+0.5*x+eps3
length(y.more)
print(paste('Population parameter value of  β0 is -1 and β1 is 0.5'))
plot(x,y.more,title('Higher Error in the Population'))
print(paste('the relationship of x and y are positive and they have low linearity and the variance is large'))
lm.fit4 <- lm(y.more ~ x)
c<-coef(lm.fit4)
print(paste('Coefficient estimate beta0 is', c[1],'Coefficient estimate beta1 is', c[2]))
print(paste('the coefficient estimates are close to real coefficience β0 and β1 but not as close as when the variance is lower'))
abline(lm.fit4, col="black")
intercept<- -1
slope<- 0.5
abline(intercept,slope,pch=2,col=2)
legend("bottomright", legend=c("least Squares", 'Population'),
       col=c("black", "red"), lty=1:1, cex=0.8)
##########################
#### QUESTION 1 Part r ####
##########################
print(paste('The linear fit to the population is closest when the variance is the least, the linear fit to the polulation is moderate when the variance
is moderate among the three and the linear fit to the polulation is least close when the variance is highest among the three'))
##########################
#### QUESTION 1 Part s ####
##########################
confint(lm.fit1)  ## Original dataset
confint(lm.fit4)  ## noisier dataset
confint(lm.fit3)  ## less noisy dataset
##########################
#### QUESTION 1 Part t ####
##########################
print(paste('The larger the variance is in dependent variable,the wider of the confidence intervals
            are observed. Because the larger the variance is in dependent variable, it is harder to
            predict, so on the same confidence level,the coefficience estimates range is wider'))
##########################
#### QUESTION 2 Part a ####
##########################
set.seed(5072)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)
##########################
#### QUESTION 2 Part b ####
##########################
print(paste('The population parameter value of β0 is 2, β1 is 2 and β2 is 0.3'))
##########################
#### QUESTION 2 Part c ####
##########################
cor(x1,y,method = 'pearson')
cor(x2,y,method = 'pearson')
cor(x1,x2,method = 'pearson')
##########################
#### QUESTION 2 Part d 
##########################
pairs(data.frame(y,x1,x2))
##########################
#### QUESTION 2 Part e ####
##########################
print(paste('According to the scatterplots we can see that only x1 and x2 has obvious linear correlation
while x1 and y or x2 and y has no obvious linear correlation'))
##########################
#### QUESTION 2 Part f ####
##########################
lm.fit.both<-lm(y~x1+x2)
summary(lm.fit.both)
##########################
#### QUESTION 2 Part g ####
##########################
summary(lm.fit.both)$coefficients[,1]
##########################
#### QUESTION 2 Part h ####
##########################
print(paste('the p-value of β0 hat and β1 hat are both less than 0.05 so 
β0 hat and β1 hat are significant while β2 hat is not significant'))
##########################
#### QUESTION 2 Part i ####
##########################
print(paste('Since the p-value for β1 hat is less than 0.05,we may reject H0 for β1, 
however we may not reject H0 for β2 becuase the p-value is higher than 0.05 '))
##########################
#### QUESTION 2 Part j ####
##########################
lm.fit.justx1<-lm(y~x1)
summary(lm.fit.justx1)
##########################
#### QUESTION 2 Part k ####
##########################
print(paste('The coefficient for x1 in this last model is very different from 
the one with both x1 and x2 as predictors.In this model, 
x1 is highly significant because its p-value is very low, so we may reject H0'))
##########################
#### QUESTION 2 Part l ####
##########################
lm.fit.justx2<-lm(y~x2)
summary(lm.fit.justx2)
##########################
#### QUESTION 2 Part m ####
##########################
print(paste('Since the p-value for x2 is less than 0.05, we may reject H0 '))
##########################
#### QUESTION 2 Part n ####
##########################
print(paste('The result obtainted in j-m does not contradict the results obtained in f-i,becuase 
the predictors x1 and x2 are highly correlated, the collinearity of x1 and x2 makes it hard to predict
how significant each predictor is in the multiple linear regression.In this situation, due to collinearity
the significance of predictor x2 is masked'))
##########################
#### QUESTION 2 Part o ####
##########################
x1<-c(x1,0.1)
x2<-c(x2,0.8)
y<-c(y,6)
##########################
#### QUESTION 2 Part p ####
##########################
lm.fit5<-lm(y~x1+x2)
lm.fit6<-lm(y~x1)
lm.fit7<-lm(y~x2)
summary(lm.fit5)
summary(lm.fit6)
summary(lm.fit7)
##########################
#### QUESTION 2 Part q ####
##########################
print(paste('Comparing the coefficience before and after the new point, we can conclude that
the new observation has effect on the model with both x1 x2 as predictor.The new observation
does not affect much when predictor is only x1 or only x2'))
##########################
#### QUESTION 2 Part r ####
##########################
par(mfrow=c(2,2))
plot(lm.fit5)
plot(lm.fit6)
plot(lm.fit7)
print(paste('In the model with both predictors, point 101 is both an outlier and a high-leverage point '))
print(paste('In the model with only x1 as predictor, point 101 is an outlier'))
print(paste('In the model with only x2 as predictor, the cook distance is 0.5, so point 101
is a suspicious high leverage point'))
par(mfrow=c(1,1))
##########################
#### QUESTION 3 Part a ####
##########################
mytable<-data.frame()
lim=list()
for (i in 2:14){
  lim[[i]]<-lm(Boston$crim~Boston[,i])
  mytable[i-1,1]<-names(Boston[i])
  mytable[i-1,2]<-summary(lim[[i]])$fstatistic[1]
  mytable[i-1,3]<-anova(lim[[i]])$'Pr(>F)'[1]
  mytable[i-1,4]<-coef(lim[[i]])[1]
  mytable[i-1,5]<-coef(lim[[i]])[2]
}
colnames(mytable)=c('predictor','F-statistic','p-value','ß0 hat','ß1 hat')
mytable
##########################
#### QUESTION 3 Part b ####
##########################
mytable[,1][mytable[,3]<0.05]
print(paste('Models to predict crim using the single predictor as above are significant at an
alpha=0.05 level'))
##########################
#### QUESTION 3 Part c ####
##########################
par(mfrow=c(4,3),mar = c(5,5,3,1))
for (i in 2:14){
  if (mytable[i-1,3]<0.05){
    plot(Boston[,i],Boston$crim,xlab = 'x',title(names(Boston[i])))
    abline(lim[[i]],col=2,lty=1,lwd=2)
  }
}
par(mfrow=c(1,1))
##########################
#### QUESTION 3 Part d ####
##########################
fit.all <- lm(crim ~ ., data = Boston)
summary(fit.all)
##########################
#### QUESTION 3 Part e ####
##########################
rownames(coef(summary(fit.all)))[coef(summary(fit.all))[,4]<0.05][-1]
##########################
#### QUESTION 3 Part f ####
##########################
simple<-c()
for (i in 2:14){
  simple[i-1]<-lim[[i]]$coefficients[2]
}
multiple<- vector("numeric", 0)
multiple<- c(multiple, fit.all$coefficients)
multiple<- multiple[-1]
plot(simple, multiple)
print(paste('simple and multiple regressions do not agree'))
print(paste('multiple predictor regression is better than simple regression because it takes
more significant variables into consideration and the variance of the coefficient of multiple
regression are less than variance of the coefficient of simple regression'))
##########################
#### QUESTION 3 Part g ####
##########################
mytable1<-data.frame()
poly=list()
for (i in 2:14){
  poly[[i]]<-lm(Boston$crim ~ Boston[,i]+I((Boston[,i])^2)+I((Boston[,i])^3))
  mytable1[i-1,1]<-names(Boston[i])
  mytable1[i-1,2]<-anova(lim[[i]],poly[[i]])$'F'[2]
  mytable1[i-1,3]<-anova(lim[[i]],poly[[i]])$'Pr(>F)'[2]
}
colnames(mytable1)=c('predictor','fstat','pvalueofFstat')
mytable1<-mytable1[order(mytable1$pvalueofFstat),]
mytable1<-mytable1[-13,]
mytable1
mytable1[,1][mytable1[,3]<0.05]
print(paste('For the predictors shown above, we can reject the null hypothesis that there is no 
difference between the fit of the two models at an alpha=0.05 level'))