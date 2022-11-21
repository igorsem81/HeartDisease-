#################################################################
#######################  PROJECT - HEART  ####################### 
#################################################################

### source https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction ###
install.packages("InformationValue")
library(InformationValue)

install.packages("mccr")
library(mccr)
library(InformationValue)

library(pastecs)

library(gridExtra)
library(ggplot2)
library(VIM)
library(corrplot)
library(car)
library(ResourceSelection)
library(glmulti)
library(tree)
library(randomForest)
library(ISLR)
library(class)
library(pROC)
library(gtools)
library(tidyverse)


############## 1.  Show the Data  ###################

getwd()
data = read.csv("G:/My Drive/studying/DATA SCIENCE/technion/3 - statistic learning R/project - R/heart.csv")

colnames(data)
head(data)
summary(data)
table(data$HeartDisease)


##############  2. Preparing the Data  ###################

#Numerical and Categorical variables

data$Age = as.numeric(data$Age)
data$RestingBP = as.numeric(data$RestingBP)
data$Cholesterol = as.numeric(data$Cholesterol)
data$MaxHR = as.numeric(data$MaxHR)
data$Oldpeak = as.numeric(data$Oldpeak)

#Simplify ChestPainType & turn into factor, only check for Asymptomatic pain
data$ChestPainType[data$ChestPainType!= "ASY"] = 0
data$ChestPainType[data$ChestPainType == "ASY"] = 1
data$ChestPainType = as.factor(data$ChestPainType)
summary(data$ChestPainType)

#Simplify RestingECG & turn into factor, only check for ST
data$RestingECG[data$RestingECG != "ST"] = 0
data$RestingECG[data$RestingECG == "ST"] = 1
data$RestingECG = as.factor(data$RestingECG)
summary(data$RestingECG)

#Simplify ST_Slope & turn into factor, only check for Down/Flat
data$ST_Slope[data$ST_Slope != "Up"] = 0
data$ST_Slope[data$ST_Slope == "Up"] = 1
data$ST_Slope = as.factor(data$ST_Slope)
summary(data$ST_Slope)

#Turn Sex into factor F=0 M=1
data$Sex[data$Sex != "M"] = 0
data$Sex[data$Sex == "M"] = 1
data$Sex = as.factor(data$Sex)
summary(data$Sex)

#Turn ExerciseAngina into factor N=0 Y=1
data$ExerciseAngina[data$ExerciseAngina != "Y"] = 0
data$ExerciseAngina[data$ExerciseAngina == "Y"] = 1
data$ExerciseAngina = as.factor(data$ExerciseAngina)
summary(data$ExerciseAngina)


#Change variable to data type "factor"
data$HeartDisease = as.factor(data$HeartDisease) #  HeartDisease
data$FastingBS = as.factor(data$FastingBS)       #  FastingBS

str(data)

##### Age ####
par(mfrow = c(1,3))

Age.hist = hist(data$Age, main=colnames(data)[1], cex.axis = 1.5, cex.main = 2, xlab = "")
Age.box = boxplot(data$Age~data$HeartDisease, main=colnames(data)[1], cex.axis = 1.5, cex.main = 2, xlab = "")
Age_out = Age.box$out
qqnorm.age = qqnorm(data$Age)
qqline.age = qqline(data$Age)
#x = data
#x<- data[-which(data$Age %in% c),]

#plot1.box = boxplot(x$Age~x$HeartDisease, main=colnames(x)[1], cex.axis = 1.5, cex.main = 2, xlab = "")
#plot1 = hist(x$Age, main=colnames(x)[1], cex.axis = 1.5, cex.main = 2, xlab = "")
#summary(x)
#plot1.box = boxplot(data.numeric$Age~data$HeartDisease, main=colnames(data.numeric)[1], cex.axis = 1.5, cex.main = 2, xlab = "")
#qqnorm(data.numeric$Age1)

##### RestingBP ####
par(mfrow = c(1,3))

RestingBP.hist = hist(data$RestingBP, main=colnames(data)[4], cex.axis = 1.5, cex.main = 2, xlab = "")
RestingBP.box = boxplot(data$RestingBP~data$HeartDisease, main=colnames(data)[4], cex.axis = 1.5, cex.main = 2, xlab = "")
RestingBP.out = RestingBP.box$out
qqnorm.RestingBP = qqnorm(data$RestingBP)
qqline.RestingBP = qqline(data$RestingBP)

##### Cholesterol  ####
par(mfrow = c(1,3))

Cholesterol.hist = hist(data$Cholesterol , main=colnames(data)[5], cex.axis = 1.5, cex.main = 2, xlab = "")
Cholesterol.box = boxplot(data$Cholesterol ~data$HeartDisease, main=colnames(data)[5], cex.axis = 1.5, cex.main = 2, xlab = "")
Cholesterol.out = Cholesterol.box$out
qqnorm.Cholesterol = qqnorm(data$Cholesterol)
qqline.Cholesterol = qqline(data$Cholesterol)

##### MaxHR  ####
par(mfrow = c(1,3))

MaxHR.hist = hist(data$MaxHR , main=colnames(data)[8], cex.axis = 1.5, cex.main = 2, xlab = "")
MaxHR.box = boxplot(data$MaxHR ~data$HeartDisease, main=colnames(data)[8], cex.axis = 1.5, cex.main = 2, xlab = "")
MaxHR.out = MaxHR.box$out
qqnorm.MaxHR = qqnorm(data$MaxHR)
qqline.MaxHR = qqline(data$MaxHR)

##### Oldpeak  ####
par(mfrow = c(1,3))

Oldpeak.hist = hist(data$Oldpeak , main=colnames(data)[10], cex.axis = 1.5, cex.main = 2, xlab = "")
Oldpeak.box = boxplot(data$Oldpeak ~data$HeartDisease, main=colnames(data)[10], cex.axis = 1.5, cex.main = 2, xlab = "")
Oldpeak.out = Oldpeak.box$out
qqnorm.Oldpeak = qqnorm(data$Oldpeak)
qqline.Oldpeak = qqline(data$Oldpeak)


############## 2. Clean Data ############## 

# zeros to NA
length(data$Cholesterol[data$Cholesterol==0])
length(data$RestingBP[data$RestingBP==0])
length(data$Oldpeak[data$Oldpeak<0])

data$Cholesterol[data$Cholesterol<=0] = NA
data$RestingBP[data$RestingBP==0] = NA
data$Oldpeak[data$Oldpeak<0] = NA

#Correlation plot, check for correlation between predictor variables

corrplot.predictors = cor(na.omit(data[,c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")]))
corrplot(corrplot.predictors, method='number', number.digits = 1, number.cex = 1, tl.cex = 1)
summary(corrplot.predictors)


###########################################################
###################### Logistic Regression ################
###########################################################


### Train and Test ###

set.seed(3)
train.size = floor(nrow(data)*0.7)
train.index = sample(1:nrow(data),train.size, replace = F)
train.set = data[train.index,]
test.set = data[-train.index,]

str(data)
logit_reg = glm(HeartDisease ~., data = train.set,family = "binomial")
summary(logit_reg)

# Extracting coefficients

#coef(logit_reg)[1]
#coef(logit_reg)[2]

# Prediction

PredictionGLM = predict(logit_reg, test.set, type = "response")  # "response" for logistic regression
# "terms" linear regression

# ROC

plotROC(test.set$HeartDisease, PredictionGLM,)


## Confusion Matrix ##

# The columns are actual, while rows are predicted

confusionMatrix(test.set$HeartDisease, PredictionGLM, threshold = 0.5)

(93+89)/220



############################################################################## 
####################################### Decision tree #########################
############################################################################## 

#data = read.csv("G:/My Drive/studying/DATA SCIENCE/technion/3 - statistic learning R/project - R/heart.csv")

## Regression Tree ##
names(data)
set.seed(500)

train = sample(1:nrow(data), nrow(data)/2)


tree.data = tree(HeartDisease~.,data, subset=train)
summary(tree.data)

## Plotting Regression Tree ##

plot(tree.data)
text(tree.data,pos = 2)

## Cross - Validation Analysis ##

cv.data = cv.tree(tree.data, K = 10)
plot(cv.data$size,cv.data$dev,type='b')

prune.data=prune.tree(tree.data,best=6)

plot(prune.data)
text(prune.data)

# Prediction #

yhat=predict(tree.data,newdata=data[-train,])
data.test=data[-train,"HeartDisease"]
plot(yhat,data.test)
abline(0,1)
mean((yhat-data.test)^2)


# 6 Terminal nodes #

prune.data=prune.tree(tree.data,best=6)

plot(prune.data)
text(prune.data)

yhat=predict(prune.data,newdata=data[-train,])
data.test=data[-train,"HeartDisease"]
plot(yhat,data.test)
abline(0,1)
mean((yhat-data.test)^2)


### Random Forest ###

train = sample(1:nrow(data), nrow(data)/2)

rf = randomForest(HeartDisease ~., data = data[train,], ntree=1000, mtry = 4 )

# In order to find the optimal number of trees in random forest

opt_tree = which.min(rf$mse)
plot(rf$mse)

pred = predict(rf, data[-train,])

# mtry = p/3

print(rf)
importance(rf)
varImpPlot(rf)

# The values indicates the importance of the variable for the model accuracy.
# Important variables receive higher values. 

plot(pred, data.test)
mean((pred-data.test)^2)

## Cross - Validation Random Forest ##

rf_cv = rfcv(data[train,!names(data)=="HeartDisease"],data$HeartDisease[train], 
             step = 0.8, cv.fold = 10 )

mtry_min = rf_cv$n.var[rf_cv$error.cv==min(rf_cv$error.cv)]

rf = randomForest(formula = HeartDisease~., data = data[train,], 
                  ntree=opt_tree, mtry = mtry_min )

pred = predict(rf, data[-train,])
mean((pred-data.test)^2)

