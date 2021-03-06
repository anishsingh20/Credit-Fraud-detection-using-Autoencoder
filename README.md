# Credit-Fraud-detection-using-Autoencoder

Credit fraud detection using keras and Autoencoder in R.

The credit data set was downloaded from Kaggle and using that dataset I have trained an Autoendocer to predict which cases will be fradulent and non-fradulent. Mainly the autoencodes tries to understand the distribution of Fradulent and non-fradulent cases. It learns by learning to differentiate between the distribution between the 2 classes of transactions. So the differences in the distribution of Fradulent and Non-fradulent cases is visualized below.


## Plot for the differences in distribution of Fradulent and normal transactions


![github logo](https://github.com/anishsingh20/Credit-Fraud-detection-using-Autoencoder/blob/master/Plots/DistributionOfFraudCases.png)

From the above plot we can see that distribution of all features for normal and __Fradulent__ transactions differ in a lot, except __Time__ variable. 

--------------------------------


## Scatter plot of Time vs Amount colored and grouped by the type of transactions-

![github logo](https://github.com/anishsingh20/Credit-Fraud-detection-using-Autoencoder/blob/master/Plots/ScatterPlotTimeVsAmount.png)
