# bit-coin-prediction
Prediction of Bitcoin Close Price using machine learning algorithms in R.

The objective of the project is to determine the accuracy with which the closing price of the bitcoin can be predicted with the help of classification and linear regression methods. For classification, we have implemented several ANN models with different layers and neurons to find the model with the best accuracy and compared the result with LSTM.  Using LSTM an accuracy of 54.35% was achieved with a log loss of 7.18 to predict the direction of the close price. Also, the best ANN model had an accuracy of 55.1 % which was almost at power with LSTM accuracy.  Using multiple linear regression models, we deduced that elastic net performed better in comparison to lasso and ridge model as it had lower RMSE and R squared value. RMSE value recorded for elastic net regression is 0.00808 which was lowest when compared to other regression models.
