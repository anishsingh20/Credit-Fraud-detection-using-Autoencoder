#Credit card fraud detection using a AUTOENCODER And KERAS in R
require(dplyr)
require(ggplot2)
require(readr)#faster data reading
require(highcharter)

#reading the data

fraud<-read_csv("../Datasets/creditcardfraud/creditcard.csv", 
                col_types = list(Time = col_number()))

#the dataset has all the attributes transformed into factors/Latent variables via PCA.
#We only have Time,Amount and Class label(0=non-fradulent, 1=fradulent) as variables. 


#Feature 'Time' contains the seconds elapsed between each transaction and the first transaction in the dataset. 
#The feature 'Amount' is the transaction Amount.

#checking out the data
glimpse(fraud)
attach(fraud)

table(Class)
#highly unbalanced dataset- only 492 fradulent cases


#modifying time variable in hours

time<-fraud %>% select(Time)

time<- time %>% mutate(Time = (Time/(60*60)))



#let's do some plotting and explore the dataset

#histogram of time taken for each transaction
hchart(time$Time,color="orange",name="Time in hours") %>% 
  hc_title(text="Histogram of Time taken for each transaction in Hours",align="center") %>%
    hc_exporting(enabled=T) %>% 
     hc_add_theme(hc_theme_elementary()) 

  





