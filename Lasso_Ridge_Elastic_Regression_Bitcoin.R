############# ADM Project on Prediction of Bitcoin/Eth using () Models

############# Libraries
library(htmltab)
library(anytime)
library(lubridate)


############# Data Source: https://coinmarketcap.com/currencies/bitcoin/historical-data/ and
#############             Discarded: https://www.blockchain.com/stats
getwd()
#####WORKING DIRECTORY#########

#setwd("C:\\NCI Materials\\ADM\\CA2 Project")
############# Fetch Data from Source
endDate<-gsub("-","",as.Date(Sys.Date(),"%Y%M%d"))
#Records only from 1st January 2014 to be considered
startDate<-"20140101"
BitcoinURL <- paste("https://coinmarketcap.com/currencies/bitcoin/historical-data/?start=",startDate,"&end=",endDate,sep="") 
BTC <-  htmltab(doc=BitcoinURL, which=1)   #?start=20170401&end=20180721
TempDate <- gsub(",","",BTC$Date)
BTC$Date<- as.Date(TempDate,"%B%d%Y")
rm(TempDate)
BTC$Month<-as.numeric(format(BTC$Date,"%m"))#month(as.POSIXlt(y, format="%d/%m/%Y")),"%B")
BTC$Day<-as.numeric(format(BTC$Date,"%d"))
BTC$Week<-as.numeric(format(BTC$Date,"%W"))
BTC$Year<-as.numeric(format(BTC$Date,"%Y"))
BTC$`Open*` <- as.numeric(gsub(",","",BTC$`Open*`))
colnames(BTC)[2]<-"Open"
BTC$High <- as.numeric(gsub(",","",BTC$High))
BTC$Low <- as.numeric(gsub(",","",BTC$Low))
BTC$`Close**` <- as.numeric(gsub(",","",BTC$`Close**`))
colnames(BTC)[5]<-"Close"

BTC$Volume <- (as.numeric(gsub(",","",BTC$Volume)))/1000000
BTC$`Market Cap` <- (as.numeric(gsub(",","",(BTC$`Market Cap`))))/1000000
colnames(BTC)[7]<-"Market_Cap"
mCap <- c(BTC$Market_Cap)
options("scipen"=-100, "digits"=4)
options("scipen"=100, "digits"=4)
BTC$Market_Cap <-mCap
rm(mCap)
summary(BTC)
sapply(BTC,function(x) sum(is.na(x)))

#############################
# BLOCKCHAIN
############################################

############################# Hash Rate
#install.packages('Quandl')
library('Quandl')
bitCoin_Hash<-Quandl("BCHAIN/HRATE", api_key="yco-E8f8_XWuFHf68jxD")#read.csv("hash-rate.csv",header = FALSE)
colnames(bitCoin_Hash)[2] <- "HashRate"
hash <- c(bitCoin_Hash$HashRate )
options("scipen"=-100, "digits"=4)
options("scipen"=100, "digits"=4)
bitCoin_Hash$HashRate <-hash/1000000
rm(hash)

########################### Difficulty
bitCoin_Difficulty<-Quandl("BCHAIN/DIFF", api_key="yco-E8f8_XWuFHf68jxD")#read.csv("difficulty.csv",header = FALSE)
colnames(bitCoin_Difficulty)[2] <-"Difficulty"
bitCoin_Difficulty$Difficulty <- as.numeric(gsub(",","",(bitCoin_Difficulty$Difficulty)))
diff <- c(bitCoin_Difficulty$Difficulty )
options("scipen"=-100, "digits"=4)
options("scipen"=100, "digits"=4)
bitCoin_Difficulty$Difficulty <-diff/1000000
rm(diff)

########################### No of Transactions
bitCoin_NoOfTransactions<-Quandl("BCHAIN/NTRAN", api_key="yco-E8f8_XWuFHf68jxD")#read.csv("n-transactions.csv",header = FALSE)
colnames(bitCoin_NoOfTransactions)[2] <- "NoOfTransactions"
tran <- c(bitCoin_NoOfTransactions$NoOfTransactions)
options("scipen"=-100, "digits"=4)
options("scipen"=100, "digits"=4)
bitCoin_NoOfTransactions$NoOfTransactions <-tran/1000000
rm(tran)

########################### Cost of Transactions
bitCoin_CostPerTransaction<-Quandl("BCHAIN/CPTRA", api_key="yco-E8f8_XWuFHf68jxD")#read.csv("cost-per-transaction.csv",header = FALSE)
colnames(bitCoin_CostPerTransaction)[2] <- "CostPerTransaction"
cost <- c(bitCoin_CostPerTransaction$CostPerTransaction)
options("scipen"=-100, "digits"=4)
#cost
options("scipen"=100, "digits"=4)
bitCoin_CostPerTransaction$CostPerTransaction <-cost
rm(cost)


########################### Miners Revenue USD
bitCoin_MinersRevenue<-Quandl("BCHAIN/MIREV", api_key="yco-E8f8_XWuFHf68jxD")#read.csv("miners-revenue.csv",header = FALSE)
colnames(bitCoin_MinersRevenue)[2] <- "RevenueUSD"
revenue <- c(bitCoin_MinersRevenue$RevenueUSD)
options("scipen"=-100, "digits"=4)
options("scipen"=100, "digits"=4)
bitCoin_MinersRevenue$RevenueUSD <-revenue/1000000
rm(revenue)


####################### MERGE DATA
combinedData<-merge(bitCoin_Hash,bitCoin_Difficulty,by.x = "Date")
combinedData<-merge(combinedData,bitCoin_CostPerTransaction,by.x = "Date")
combinedData<-merge(combinedData,bitCoin_NoOfTransactions,by.x = "Date")
combinedData<-merge(combinedData,bitCoin_MinersRevenue,by.x= "Date")
combinedData<-combinedData[order(combinedData$Date),]
BTC<-BTC[order(BTC$Date),]
#summary(combinedData)
#summary(BTC)
#str(BTC)
#head(BTC)
mergedBTC<-merge(BTC,combinedData,by.x = "Date")
sapply(mergedBTC,function(x) sum(is.na(x)))
#str(mergedBTC)
##################################################################
#####GRANGERS TEST
##################################################################
#install.packages('lmtest')
library(lmtest)
grangertest(Close ~ Open,order = 1 , data = mergedBTC)
grangertest(Close ~ High,order = 3 , data = mergedBTC)
grangertest(Close ~ Low,order = 1 , data = mergedBTC)
grangertest(Close ~ Difficulty,order = 2 , data = mergedBTC)
grangertest(Close ~ Volume,order = 1 , data = mergedBTC)
grangertest(Close ~ Market_Cap,order = 1 , data = mergedBTC)
grangertest(Close ~ HashRate,order = 2 , data = mergedBTC)
grangertest(Close ~ CostPerTransaction,order = 3 , data = mergedBTC)
grangertest(Close ~ NoOfTransactions,order = 1 , data = mergedBTC)
grangertest(Close ~ RevenueUSD,order = 3 , data = mergedBTC)


boxplot(mergedBTC)
summary(mergedBTC)
#Formatted Data records CSV to split among classification and regression
write.csv(mergedBTC,file="BTCFirstData.csv",row.names = FALSE)


############### MODEL FOR CLASSIFICATION 

mergedBTC<-mergedBTC[order(mergedBTC$Date),]
a<-0
b<-0

mergedBTC[c("PriceDirection")] <- "Higher"

PriceDir_rowNum<-as.numeric(length(mergedBTC))
for(i in seq(1,nrow(mergedBTC)-1)){
  a<-mergedBTC[i,5]
  b<-mergedBTC[i+1,5]
  diff<-b-a
  if(diff>0)
  {
    mergedBTC[i,PriceDir_rowNum] <- "Higher"
  }
  if(diff<0)
  {
    mergedBTC[i,PriceDir_rowNum] <- "Lower"
  }
  if(diff==0)
  {
    mergedBTC<-mergedBTC[-i,]
  }
}
mergedBTC$PriceDirection<-as.factor(mergedBTC$PriceDirection)
mergedBTC$PriceDirection <- factor(mergedBTC$PriceDirection, labels=c(0,1)
                                   , levels=c("Lower", "Higher"))
#summary(mergedBTC)

##### Just to reuse with values again
write.csv(mergedBTC,file="BTCFullData.csv",row.names = FALSE)
#mergedBTC <-read.csv("BTCFullData.csv")

#########################################################
####OUTLIER HANDLING
#########################################################
outliers_handling<-function(col){
  mean_impute<-mean(col)
  outliers<-boxplot.stats(col)$out
  while(length(outliers)!=0){
    newCol<-ifelse(col %in% outliers,mean_impute,col)
    col<-newCol
    outliers<-boxplot.stats(col)$out
  }
  return(col)
}
boxplot(mergedBTC)
mergedBTC$Open<-outliers_handling(mergedBTC$Open)
mergedBTC$Close<-outliers_handling(mergedBTC$Close)
mergedBTC$High<-outliers_handling(mergedBTC$High)
mergedBTC$Low<-outliers_handling(mergedBTC$Low)
mergedBTC$Volume<-outliers_handling(mergedBTC$Volume)
mergedBTC$Market_Cap<-outliers_handling(mergedBTC$Market_Cap)
mergedBTC$HashRate<-outliers_handling(mergedBTC$HashRate)
mergedBTC$Difficulty<-outliers_handling(mergedBTC$Difficulty)
mergedBTC$NoOfTransactions<-outliers_handling(mergedBTC$NoOfTransactions)
mergedBTC$CostPerTransaction<-outliers_handling(mergedBTC$CostPerTransaction)
mergedBTC$RevenueUSD<-outliers_handling(mergedBTC$RevenueUSD)

########################### CORRELATION

cor(subset(mergedBTC, select=c(2:12)))

##Visalize the correlation
#install.packages("GGally")
library(GGally)
ggcorr(mergedBTC, label=F,main = "Pearson Correlation")

########################### NORMALIZATION
#install.packages('ggplot2')
library(ggplot2)
#install.packages('caret')
library(caret)
#install.packages('glmnet')
library(glmnet)
#install.packages('mlbench')
library(mlbench)
#install.packages('psych')
library('psych')
#detach("package:RSNNS", unload=TRUE)
library(RSNNS) #For normalize and denormalize functions
normalizedData<-mergedBTC[,-1]
#normalizedData<-normalizedData[,-12]#delete Price Direction column if present
boxplot(normalizedData) #Check to see if outliers are handled
set.seed(1669)
sample <- createDataPartition(normalizedData$Close, p = .70, list = FALSE) 
training_data <-  normalizedData[sample, ]
testing_data <- normalizedData[-sample, ] 
############################################################################
#MACHINE LEARNING CODE
############################################################################
#PCA
###########################################################################
pca_train<-prcomp(training_data[,-4],scale. = TRUE)
pca_train$center #Mean of Variables
########################################################
# Open               High                Low            Volume         Market_Cap 
# 1076.0521          1105.5031          1044.8645       435.1351         17077.3385 
# HashRate         Difficulty CostPerTransaction   NoOfTransactions         RevenueUSD 
# 2.3371        313132.1999            18.7602             0.1801             2.7363 
#######################################################
pca_train$scale #STD DEVIATION OF VARIABLES
#######################################################
# Open               High                Low              Volume         Market_Cap 
# 1019.12368         1050.45538          983.65543         622.81314        17182.09888 
# HashRate         Difficulty CostPerTransaction   NoOfTransactions         RevenueUSD 
# 2.58479       347049.71254           13.50685            0.08664            2.05990
#######################################################

pca_train$rotation

######################################################
#                     PC1      PC2      PC3      PC4      PC5     PC6        PC7      PC8       PC9      PC10
# Open               0.3457 -0.04383 -0.28450 -0.12087  0.10810 -0.1527  0.0044217  0.24379 -0.343667 -0.755290
# High               0.3456 -0.04413 -0.27447 -0.09608  0.06777 -0.1791 -0.0370098 -0.86830  0.034482  0.029251
# Low                0.3453 -0.04046 -0.28966 -0.12876  0.12907 -0.1591  0.0163907  0.32178  0.790980  0.084810
# Volume             0.3327 -0.01098  0.08231  0.54274 -0.72005 -0.2585  0.0023587  0.04440  0.021249 -0.011060
# Market_Cap         0.3461 -0.01623 -0.29062 -0.08377  0.11024 -0.1503 -0.0005454  0.28117 -0.504112  0.648508
# HashRate           0.3331  0.16112  0.30858  0.31572  0.36148  0.1738 -0.7118819  0.01473  0.013279 -0.012495
# Difficulty         0.3322  0.16483  0.31780  0.32358  0.38454  0.1299  0.7002815 -0.03937 -0.001827 -0.007984
# CostPerTransaction 0.1875 -0.68773  0.57716 -0.31133  0.01675 -0.2460 -0.0077458  0.01408 -0.016550  0.022323
# NoOfTransactions   0.2096  0.65699  0.36453 -0.56011 -0.24733 -0.1289 -0.0007181  0.00408 -0.003174  0.003538
# RevenueUSD         0.3330 -0.19072 -0.07981 -0.20725 -0.31055  0.8401  0.0332522 -0.00813  0.003253  0.008527
#############################################################################
dim(pca_train$x)#[1] 1170   10
biplot(pca_train,scale = 0)
pca_train_sd<-pca_train$sdev
pca_var_sd<-pca_train_sd^2
#proportion of variance explained
propVarex <- pca_var_sd/sum(pca_var_sd)
propVarex[1:10]
#scree plot
plot(propVarex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")
#cumulative scree plot
plot(cumsum(propVarex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
#add a training set with principal components
#normalizeClose
nrmlzd_close<-normalizeData(training_data$Close,type = "0_1")
train_pca <- data.frame(Close = nrmlzd_close, pca_train$x)
test_close_normalized<-normalizeData(testing_data$Close,type = "0_1")
#we are interested in first 8 PCAs
train_pca <- train_pca[,1:9]

################################################################
####NORMALITY TEST
################################################################
hist(training_data$Close)
#DATA IS NEGATIVELY SKEWED AS OUR RECORDS HAVE OLD DATA WHEN BITCOIN WAS NOT SIGNIFICANT


#transform test into PCA
test_pca <- predict(pca_train, newdata = testing_data[,-4])
test_pca <- as.data.frame(test_pca)

#select the first 8 components
test_pca <- test_pca[,1:9]


###########################################################################
#REGRESSION
###########################################################################
customControl <- caret::trainControl(method="repeatedcv", number = 10, repeats = 5,verboseIter = TRUE)

psych::pairs.panels(train_pca)

#LINEAR MODEL
set.seed(1669)
linearModel<-caret::train(Close ~ .,
                              train_pca,
                              method='lm',
                              trControl = customControl)
linearModel$results
summary(linearModel)
##############################################################################
# Call:
#   lm(formula = .outcome ~ ., data = dat)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.14286 -0.00022  0.00058  0.00144  0.02798 
# 
# Coefficients:
#   Estimate Std. Error t value            Pr(>|t|)    
# (Intercept)  0.1934008  0.0002336   827.9 <0.0000000000000002 ***
#   PC1          0.0757445  0.0000821   922.9 <0.0000000000000002 ***
#   PC2         -0.0091690  0.0002076   -44.2 <0.0000000000000002 ***
#   PC3         -0.0610875  0.0004348  -140.5 <0.0000000000000002 ***
#   PC4         -0.0226468  0.0005750   -39.4 <0.0000000000000002 ***
#   PC5          0.0186184  0.0007661    24.3 <0.0000000000000002 ***
#   PC6         -0.0399479  0.0010247   -39.0 <0.0000000000000002 ***
#   PC7         -0.0050502  0.0016814    -3.0              0.0027 ** 
#   PC8         -0.1831663  0.0033498   -54.7 <0.0000000000000002 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.00799 on 1161 degrees of freedom
# Multiple R-squared:  0.999,	Adjusted R-squared:  0.999 
# F-statistic: 1.1e+05 on 8 and 1161 DF,  p-value: <0.0000000000000002
############################################################################

#install.packages("gvlma")

library('lmtest')
library("gvlma")


#par(mfrow=c(1,1)) # 4 charts in 1 panel
#CHECK FOR HOMESCEDASTICITY ASSUMPTION
plot(linearModel$finalModel)


#WE FAILED TO ACHIEVE HOMOSCEDASTICITY
#SOLVING USING addition of residuals into dataset

dist_new<-normalizeData(linearModel$finalModel$residuals,type = "0_1")
lm_train<-cbind(train_pca,Residuals=dist_new)#lm_train<-cbind(train_pca,dist_new) #base model for all regression models
set.seed(1669)
linear_model_new<-caret::train(Close ~ .,
                               lm_train,
                               method='lm',
                               trControl = customControl)
bptest(linear_model_new$finalModel)
plot(linear_model_new$finalModel)


#RIDGE IMPLEMENTATION
set.seed(1669)
ridgeRegression<-caret::train(Close ~ .,
                       lm_train,
                       method='glmnet',
                       tuneGrid = expand.grid(alpha = 0,lambda = seq(0.0001,0.2,length=5)),
                       trControl = customControl)
#Visualising Ridge Results
plot(ridgeRegression)
ridgeRegression

#denormalizedData<-denormalizeData(ridgePrediction,normParams = getNormParameters(nrmlzd_close))
plot(ridgeRegression$finalModel,label = TRUE, xlab = "L2 Norm")
plot(ridgeRegression$finalModel,xvar = "lambda",label = TRUE)
plot(ridgeRegression$finalModel,xvar = "dev",label = TRUE)
plot(varImp(ridgeRegression,scale=FALSE),type="o", ylab="Components", col = "blue", main="Variable Importance Ridge", cex = 1.5)

#FOR RIDGE PREDICTION
#ridgePrediction<-ridgeRegression$modelInfo$predict(ridgeRegression$finalModel,newdata=test_pca)
#ridgeDenormalizedPrediction<-denormalizeData(ridgePrediction,normParams = getNormParameters(nrmlzd_close))


#LASSO REGRESSION IMPLEMENTATION
set.seed(1669)
lassoRegression<-caret::train(Close ~ .,
                       lm_train,
                       method='glmnet',
                       tuneGrid = expand.grid(alpha = 1,lambda = seq(0.0001,0.2,length=5)),
                       trControl = customControl)
#Visualising Lasso Results
plot(lassoRegression)
lassoRegression
plot(lassoRegression$finalModel,label = TRUE)
plot(lassoRegression$finalModel,xvar = "lambda",label = TRUE)
plot(lassoRegression$finalModel,xvar = "dev",label = TRUE)
plot(varImp(lassoRegression,scale=TRUE),type="o", ylab="Components", col = "red", main="Variable Importance Lasso", cex = 1.5)

#FOR LASSO PREDICTION
#lassoPrediction<-lassoRegression$modelInfo$predict(lassoRegression$finalModel,newdata=test_pca)
#lassoDenormalizedPrediction<-denormalizeData(lassoPrediction,normParams = getNormParameters(nrmlzd_close))

#ELASTIC REGRESSION IMPLEMENTATION
set.seed(1669)

elasticRegression<-caret::train(Close ~ .,
                       lm_train,
                       method='glmnet',
                       tuneGrid = expand.grid(alpha = seq(0, 1,length=10),lambda = seq(0.0001, 0.2,length=5)),
                       trControl = customControl)
#Visualising Elastic Results
plot(elasticRegression$finalModel, label=TRUE)
elasticRegression

plot(elasticRegression$finalModel,xvar = "lambda",label = TRUE)
plot(elasticRegression$finalModel,xvar = "dev",label = TRUE)
plot(varImp(elasticRegression,scale=TRUE),type="o", ylab="Components", col = "dark green", main="Variable Importance Elastic Net", cex = 1.5)


#Compare Models
model_list<-list(RidgeModel=ridgeRegression,LassoModel=lassoRegression,ElasticNetModel=elasticRegression)
results<-resamples(model_list)
summary(results)

#ELASTIC NET REGRESSION HAS LOWEST MEAN SQ ERROR AND HIGH R-SQRD VALUE SO WE USE IT FOR PREDICTION
#################TRAIN PREDICTION
elasticTrainPrediction<-predict(elasticRegression, lm_train)
elasticTrainDenormalizedPrediction<-denormalizeData(elasticTrainPrediction,normParams = getNormParameters(nrmlzd_close))


################TEST PREDICTION
elasticPrediction<-elasticRegression$modelInfo$predict(elasticRegression$finalModel,newdata = test_pca)
elasticDenormalizedPrediction<-denormalizeData(elasticPrediction,normParams = getNormParameters(nrmlzd_close))

#COMPARING PREDICTION OF ELASTIC NET REGRESSION FOR ACCURACY
compare<-cbind (actual=testing_data$Close, predicted=elasticDenormalizedPrediction)
mean(apply(compare, 1, min)/apply(compare, 1, max)) # calculated accuracy to be 0.5322 or 53.22%
#ELASTIC RMSE WITH NORMALIZATION #0.06525 or 6.5%
sqrt(mean(((test_close_normalized)-(elasticPrediction))^2))


#LASSO ACCURACY
#compare<-cbind (actual=testing_data$Close, predicted=lassoDenormalizedPrediction)
#mean(apply(compare, 1, min)/apply(compare, 1, max)) # calculated accuracy to be 0.6234 or 62.34%

#LASSO RMSE
#sqrt(mean(((test_close_normalized)-(lassoPrediction))^2))

#RIDGE ACCURACY
#compare<-cbind (actual=testing_data$Close, predicted=ridgeDenormalizedPrediction)
#mean(apply(compare, 1, min)/apply(compare, 1, max)) # calculated accuracy to be 0.5215 or 0.5215%

#RMSE FOR RIDGE
#sqrt(mean(((test_close_normalized)-(ridgePrediction))^2))
