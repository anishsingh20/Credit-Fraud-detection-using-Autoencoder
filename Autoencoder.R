#Credit card fraud detection using a AUTOENCODER And KERAS in R
require(dplyr)
require(ggplot2)
require(readr)#faster data reading

#reading the data

fraud<-read_csv("../Datasets/creditcardfraud/creditcard.csv", 
                col_types = list(Time = col_number()))