##Housing Price Prediction using R 

library(caret)
library(pls)
library(e1071)
library(MASS)
library(car)
library(rpart)
##read gp1

datagp1<- read.csv(file.choose())
##dropping 1st index column
datagp1<-datagp1[,!names(datagp1) %in% c('X','X.1')]
datagp1

summary(datagp1)##summary shows no null values

trainnew$PriceDirection <- factor(trainnew$PriceDirection, levels = c(1:2))

rmse_fun = function() { 
  return(c(mean( (train$Close.. - train$PriceDirection)^2, na.rm = TRUE ) ^0.5,
           mean( (dataTestX$pred - dataTestX$medv)^2, na.rm = TRUE ) ^0.5))
      }


library(corrplot)

corrplot(cor(datagp1), method="circle",shade.col=NA, tl.col = "black", tl.srt=45)

##data splitting


###create training and test data set
###set 75 percent of rows for training and rest for test
bound<-floor(0.75*nrow(datagp1))
data.train <- datagp1[1:bound, ]            
data.test <- datagp1[(bound+1):nrow(datagp1), ] 
nrow(data.test)
nrow(data.train)
dataTrainX<-data.train
dataTestX<-data.test

##reg$rowno <- 1:dim(reg)[1]

##train <- subset(reg,rowno <= dim(reg)[1]*0.7)

##test <- subset(reg,rowno > dim(reg)[1]*0.7)


cor.mtest(datagp1)

stepwise<- lm(medv ~ .,data = dataTrainX)

summary(stepwise)
##r squared error - 4.3

##rm,age,dis,tax,ptratio,black,lstat are the significant factors identified using summary

##validating the same by using those factors and checking whether the same is available for the combinations

stepwise_imp<- lm(medv ~ rm+age+dis+tax+ptratio+black+lstat,data = dataTrainX)

summary(stepwise_imp)

##r squared error - 5.0

##from summary the factor black and lstat seems to be less important or significant

stepwise_final <- lm(medv ~ rm+age+dis+ptratio,data = dataTrainX)

summary(stepwise_final)

##r squared error - 5.45

dataTrainX$pred <- predict(stepwise_final, newdata = dataTrainX)
dataTestX$pred <- predict(stepwise_final, newdata = dataTestX)
rmse_fun()

##train error 5.41,test error - 8.41

##all factors are having 3* ratings in the model.

step(lm(medv ~.,data = dataTrainX),direction = "forward")

pairs(~ rm+age+dis+ptratio,data = dataTrainX)

cor.test(rm+age+dis+ptratio,data = dataTrainX)

##stepwise regression

model1 <- lm(medv ~ 1,data = dataTrainX)
model2 <- lm(medv ~ .,data = dataTrainX)
step(model1 , scope = list(lower = model1,upper = model2),direction = "forward")
step(model2,scope = list(lower = model2,upper = model1),direction = "backward")

###do a scatterplot matrix to  see linearity
library(lattice)
splom(datagp1)
nrow(datagp1)
head(datagp1)




###check skewness 
skewness(dataTrainX$rad) ##3.06472
histogram(dataTrainX$rad)
###apply box cox transformation
boxcox<-preProcess(dataTrainX,method ="BoxCox") 
dataTrainXtrans<-predict(boxcox,dataTrainX)
head(dataTrainXtrans)
hist(dataTrainXtrans$crim)
hist(dataTrainX$crim)

datatestXtrans<-predict(boxcox,dataTestX)
head(datatestXtrans)
hist(datatestXtrans$medv)
hist(dataTestX$medv)

###create training data
trainingData<-dataTrainXtrans
trainingData<-dataTrainX
head(trainingData)

###fit the model-OLS
model<-lm(medv~.,data=trainingData)
summary(model)

defaultSummary(model)
par(mfrow=c(1,1))
plot(model)

###predict values
pred<-predict(model,datatestXtrans)
###create obs,pred data frame
df<-data.frame(obs=datatestXtrans$medv,pred=pred)
df
defaultSummary(df)
##higher RMSE should select appropriate varaibles to build model.


###cross-validation
ctrl<-trainControl(method="cv",n=10)
set.seed(100)
tmp<-subset(dataTrainXtrans,select =-medv)
head(tmp)
modcv<-train(x=tmp,y=dataTrainXtrans$medv,method="lm",trControl =ctrl)
print(modcv)
##rmse - 0.02

##r-sq - 0.78

train_control <- trainControl(method="boot", number=100)
# train the model
model_rprt <- train(x=tmp,y=dataTrainXtrans$medv,method="rpart",trControl =train_control)
print(model_rprt)
##cp value is 0.06,with RMSE - 0.0296               
##r - sq - 0.59

###check for multicollinearality
library(car)
vif(model) ##nox and rad has score greater than 4 therefore it is not good to include those in the model.
###vif levels shows collinearity in the dataset
###use ridge regression to check for estimate variation
library(MASS)

ridge<-lm.ridge(medv~.,data=trainingData,lambda =seq(0,100,by=1))
plot(ridge)
summary(ridge)
###ridge estimates and plot suggest multicollinarity exists
##but it does not cause significant change in the 
###estimate values.To estimate parameters by reducing correlation, a PCA is done


###pca analysis 
pca<-datagp1
###standardize independent variables
x<-subset(pca,select=-medv)
head(x)
x<-scale(x)
###center the dependent variable
y<-pca$medv
y<-scale(y,scale =F)
###do pca on indepenedent variables
comp<-princomp(x,cor =F,scores =T)
comp
plot(comp)
biplot(comp,scale = 0)

plot(comp,type = 'l')
summary(comp)
###nine principal components explain 95% of the total variance
comp$scores
comp$loadings

pcr<-pcr(medv~.,data=trainingData,validation="CV")
summary(pcr)
###choose nine components for prediction
xpcr=subset(datatestXtrans,select=-medv)
pcrpred<-predict(pcr,xpcr,ncomp =9)

pcrdf<-data.frame(obs=datatestXtrans$medv,pred=pcrpred)
###find rmse
rmsepcr<-sqrt(mean((pcrdf$obs-pcrdf$medv.9.comps)^2))
###rmse is reduced to  0.087

###pls regression is a better variation of PCR.
##It accounts for the variation in response when selecting weights
###use pls package, plsr function

plsFit<-plsr(medv~.,data=trainingData,validation="CV")
###predict first five MEDV values using 1 and 2 components
pls.pred<-predict(plsFit,datatestXtrans[1:5,],ncomp=1:2)
summary(plsFit)
validationplot(plsFit,val.type ="RMSEP")
pls.RMSEP<-RMSEP(plsFit,estimate="CV")
plot(pls.RMSEP,main="RMSEP PLS",xlab="Components")
min<-which.min(pls.RMSEP$val)
points(min,min(pls.RMSEP$val),pch=1,col="red")
plot(plsFit, ncomp=9, asp=1, line=True)
###use 9 components
pls.pred2<-predict(plsFit,datatestXtrans,ncomp=9)
pls.eval<-data.frame(obs=datatestXtrans$medv,pred=pls.pred2[,1,1])
defaultSummary(pls.eval)
summary(pls.eval)
##rpart model

plsFit2<-glm(medv~.,data=trainingData,validation="CV")
model2<-rpart(medv ~ .,data=trainingData)
summary(model2)
par(mfrow=c(2,2))
rpart.plot(model2)


###predict values
pred<-predict(model2 , datatestXtrans,ncomp= 9)
###create obs,pred data frame
df2<-data.frame(obs = datatestXtrans$medv,pred=pred)
defaultSummary(pls.eval)
df2
defaultSummary(df2)



######################################################## LSTM
install.packages("keras")
library(keras)
keras::install_keras()
install.packages("devtools")
library('devtools')
devtools::install_github("rstudio/keras")
library(kerasR)
install_keras(tensorflow = "1.5.0")
use_condaenv("r-tensorflow", required = TRUE)
install_tensorflow(gpu=TRUE)
install_tensorflow(method = "conda", conda = "auto",
                   version = "1.5.0", envname = "r-tensorflow")
install.packages("tensorflow")
library(tensorflow)
library(reticulate)
use_condaenv("r-tensorflow", required = TRUE)
py_run_string("import tensorflow as tf")
py_run_string("import keras")

LSTMBTC <- mergedBTC 
N = nrow(LSTMBTC)
p = ncol(LSTMBTC)

library(dummies)
X = dummy.data.frame(LSTMBTC[, -p])
Y = LSTMBTC[, p]
## For a multi-class target variable add the following code, here:
# Y = to_categorical(Y)

data = cbind(X, Y)

Ind = sample(N, N*0.8, replace = FALSE) 
p = ncol(data)
Y_train = data.matrix(data[Ind, p])
X_train  = data.matrix(data[Ind, -p])

Y_test = data.matrix(data[-Ind, p])
X_test  = data.matrix(data[-Ind, -p])

k = ncol(X_train)

# reticulate::py_available()
# reticulate::py_config()
# reticulate::import("keras.models")
library(reticulate)
use_python("D:/Python35/")
require('kerasR')
model <- keras_model_Sequential() 
model %>% 
  layer_dense(units = 60, activation = 'relu', input_shape = k) %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = 'sigmoid')


summary(model)

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam( lr= 0.0001 , decay = 1e-6 ),  
  metrics = c('accuracy')
)

track = model %>% fit(X_train, Y_train, epochs = 150, batch_size = 10,
                      callbacks = callback_early_stopping(patience = 2, monitor = 'acc'),
                      validation_split = 0.3
)
plot(track)

##prediction 
pred <- model %>% predict(X_test, batch_size = 128)
Y_pred = round(pred)
# Confusion matrix
CM = table(Y_pred, Y_test)

# evaluate the model
evals <- model %>% evaluate(X_test, Y_test, batch_size = 10)

accuracy = evals[2][[1]]* 100


############################################ RNN

final<-read.csv("BTCFirstData.csv",header=TRUE)
LSTMBTC<-final[,-1]

library(rnn)
guj_Check_p<-LSTMBTC[,c('Day','Month','Close..')]
guj_Check_r<-LSTMBTC[,c('Day','Month','High')]
guj_Check_i<-LSTMBTC[,c('Day','Month','Open.')]


X1 <- reshape::cast(guj_Check_r,Month~Day)
X2 <- reshape::cast(guj_Check_i,Month~Day)
Y <- reshape::cast(guj_Check_p,Month~Day)

X1<-X1[,-1]
X2<-X2[,-1]
Y<-Y[,-1]

# Standardize in the interval 0 - 1
X1_norm <- (X1 - min(X1)) / (max(X1) - min(X1))
X2_norm <- (X2 - min(X2)) / (max(X2) - min(X2))
Y_norm <- (Y - min(Y)) / (max(Y) - min(Y))

# Transpose
X1_norm <- t(X1_norm)
X2_norm <- t(X2_norm)
Y_norm <- t(Y_norm)

# Training-testing sets
library(caret)
set.seed(1337)
sample <- createDataPartition(LSTMBTC$Close, p = .75, list = FALSE) 
train <- LSTMBTC[sample, ]
test <- LSTMBTC[-sample, ]
#train <- 1:3
#test <- 4

# Create 3d array: dim 1: samples; dim 2: time; dim 3: variables.
X_norm_f <- array( c(X1_norm,X2_norm), dim=c(dim(X1_norm),2) )
Y_norm_f <- array( Y_norm, dim=c(dim(Y_norm),1) ) 


# train the model
model <- trainr(Y=Y_norm_f[c(1:3),dim(Y_norm_f)[2]:1,,drop=F], # we inverse the time dimension
                X=X_norm_f[c(1:3),dim(X_norm_f)[2]:1,,drop=F], # we inverse the time dimension
                learningrate   =  0.07,
                hidden_dim     = 15,
                #batch_size = 100,
                numepochs = 1000)

plot(colMeans(model$error),type='l',
     xlab='epoch',
     ylab='errors'                  )

# create test inputs
A1 = X1_norm[4,]
A2 = X2_norm[4,]

# create 3d array: dim 1: samples; dim 2: time; dim 3: variables
A <-array( c(A1,A2), dim=c(1,12,2) )

# predict
B  <- predictr(model, A[,dim(A)[2]:1,,drop=F])
B = B[,dim(B)[2]:1]

denormalized_B = (B)*(max(Y)-min(Y))+min(Y)
denormalized_A1 = (A1)*(max(X1)-min(X1))+min(X1)
denormalized_A2 = (A2)*(max(X2)-min(X2))+min(X2)

# plot the difference
hist( denormalized_B-(denormalized_A1+denormalized_A2) )

#install.packages("MLmetrics")
library(MLmetrics)
library(DiceEVal)
str(denormalized_B)
str(Y)
c<-  MLmetrics::MAE( denormalized_B, t(Y[,4])) 

obs_pred<-data.frame(rbind(denormalized_B, t(Y[,4])))

h<-t(Y[,4])
denormalized_B

denormalized_B
#[1] 4.654867 4.613094 4.610380 4.654108 4.654213 4.654345 4.650666 4.647399 4.637228 4.476743 4.185765 4.036646



######################################### RSNNS

install.packages('RSNNS')
library('RSNNS')
require( RSNNS )
elman_Train_x<- model.matrix(Close ~ ., train)
elman_Train_y <- c(train$Close)
elman_inputsTest<- model.matrix(Close ~ ., test)
elman_targetsTest <- c(test$Close)

nn4 <- elman(train_sub,
             #elman_Train_x,elman_Train_y
             train[,4],size = c(5), maxit = 100,
             initFunc = "JE_Weights", initFuncParams = c(1, -1, 0.3, 1, 0.5),
             learnFunc = "JE_BP", learnFuncParams = c(0.2), updateFunc = "JE_Order",
             updateFuncParams = c(0), shufflePatterns = FALSE, linOut = TRUE,
             outContext = FALSE)#, #inputsTest = elman_inputsTest, targetsTest = elman_targetsTest)
train_sub<-subset(train,select = c(1:3,5:11))

fit <- elman ( train_sub,train[,4], size =100,learnFuncParams =c (0.1) , maxit =1000)

test_sub<-subset(test,select = c(1:3,5:11))
pred1 <- predict(nn4, test_sub)
res <- predict(nn4, train_sub)


sqrt(mean((round(test$Close)-round(pred1))^2))
#predicted = Rcpp::predict.RSNNS(nn4,elman_inputsTest)

pred <- predict ( fit , model.matrix(numericTest))#mydata.test[,2:19] )

predicted = predict


########################## GRANGER TEST
library(lmtest)
grangertest(Close ~ Open,order = 2 , data = LSTMBTC)
grangertest(Close ~ High,order = 2 , data = LSTMBTC)
grangertest(Close ~ Low,order = 1 , data = LSTMBTC)
grangertest(Close ~ Difficulty,order = 3 , data = LSTMBTC)
grangertest(Close ~ Volume,order = 1 , data = LSTMBTC)
grangertest(Close ~ Market_Cap,order = 1 , data = LSTMBTC)

grangertest(Close ~ HashRate,order = 1 , data = LSTMBTC)
grangertest(Close ~ CostPerTransaction,order = 2 , data = LSTMBTC)
grangertest(Close ~ NoOfTransactions,order = 1 , data = LSTMBTC)
grangertest(Close ~ RevenueUSD,order = 1 , data = LSTMBTC)

###############################################

#FINAL LSTM CODE RUNNING

#finaldata1 <- read.csv("BTCSecondData_BkpforANN.csv")
#finaldata1$PriceDirection <- as.factor(finaldata1$PriceDirection)

str(finaldata1)
N = nrow(finaldata1)
p = ncol(finaldata1)


Ind = sample(N, N*0.8, replace = FALSE) # 80% split for train and test
p = ncol(data)
Y_train = data.matrix(data[Ind, p])     #2x2 matrix---
X_train  = data.matrix(data[Ind, -p])

Y_test = data.matrix(data[-Ind, p])
X_test  = data.matrix(data[-Ind, -p])

k = ncol(X_train)

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 60, activation = 'relu', input_shape = k) %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = 'sigmoid')

summary(model)

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam( lr= 0.0001 , decay = 1e-6 ),  
  metrics = c('accuracy')
)


track = model %>% fit(X_train, Y_train, epochs = 60, batch_size = 50,
                      callbacks = callback_early_stopping(patience = 2, monitor = 'acc'),
                      validation_split = 0.3
)
#plot(track)

##prediction 
pred <- model %>% predict(X_test, batch_size = 128)
Y_pred = round(pred)

# Confusion matrix
CM = table(Y_test, Y_pred )

# evaluate the model
evals <- model %>% evaluate(X_test, Y_test, batch_size = 50)

accuracy = evals[2][[1]]* 100

Accuracy  #54.5
specificity #51.04
senstitivity #49.12
Kappa #51.02

