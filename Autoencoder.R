#Credit card fraud detection using a AUTOENCODER And KERAS in R
require(dplyr)
require(ggplot2)
require(readr)#faster data reading
library(ggridges)
require(highcharter)
require(purrr)
require(keras)

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
hchart(time$Time,color="purple",name="Time in hours") %>% 
  hc_title(text="Histogram of Time taken for each transaction in Hours",align="center") %>%
    hc_exporting(enabled=T) %>% 
     hc_add_theme(hc_theme_elementary()) 

  


#now let's check the distribution of Amount variable- the amount of transcation
summary(Amount)

quantile(Amount,c(0.90)) #finding 90 th percentile-it gives us a values which divides the 
#Amount into 2 parts, i.e 90% values lying below it and only 10% elements having values above this


#histogram of amount till 99 percentile
ggplot(aes(x=Amount),data = fraud) +
  geom_histogram(color="black",fill="green",alpha=0.8,bins=30) +
  scale_x_continuous(breaks=seq(0,1000,100),limits=c(0,quantile(Amount,c(0.99)))) +
  scale_y_continuous(limits=c(0,30000)) +
  ggtitle("Histogram of Transaction Amount") +
  xlab("Amount till 99 percentile") +
  ylab("Frequency")
  


#For an autoencoder to work well we have a strong initial assumption: 
#that the distribution of variables for normal transactions is different from the distribution for 
#fraudulent ones. Let’s make some plots to verify this. 
#Variables were transformed to a [0,1] interval for plotting.




fraud %>%
  gather(variable, value, -Class) %>%
  ggplot(aes(y = as.factor(variable), 
             fill = as.factor(Class), 
             x = percent_rank(value))) +
  geom_density_ridges() +
  labs(x="Normalized variable",y="Variable",fill="Distribution of Fraud and Non-fraud")


#We can see that distributions of variables for fraudulent transactions are very different then from normal ones, except for the Time variable, 
#which seems to have the exact same distribution.




#let's check the distribution of Amount and The normal and fradulent transactions

ggplot(fraud,aes(y=Amount,x=Class)) + 
  geom_boxplot(aes(group=Class)) +
  scale_y_continuous(limits=c(0,quantile(Amount,c(0.99))))


ggplot(fraud,aes(x=Time,y=Amount,color=as.factor(Class))) + 
  geom_point() +
  #adding custom colors
  scale_color_manual(values=wes_palette(n=2, name="Cavalcanti")) +
  labs(x="Time",y="amount",title="Scatter plot of Time vs Amount colored by type of transaction",
       color="Fraud")
  


#-----------------------------------PREDICTIVE MODELLING----------------------

#Data pre-processing

#splitting the data into training and testing part

#we will use the first 200000 for training and rest for testting


#randomly shuffling the data without replacement
random_fraud<-sample(1:nrow(fraud),nrow(fraud),replace=F)


#shuffling the rows and making a shuffled data set
random_fraud<- fraud[random_fraud,]
  

#training data set-selecting 200000 observations for training data
#we will not use Time variable as it has same distribution for Normal and fradulent cases
train_df<-random_fraud[1:200000,2:ncol(random_fraud)]

#testing dataset
test_df<-random_fraud[200001:nrow(random_fraud),2:ncol(random_fraud)]


#Normalizing the inputs

#min-max normalization

#function to extract descriptive statistic params to be used in a min-max normalization function
desc_stat<- function(x)
{
  map(x,~list(
    min = min(.x),
    max = max(.x),
    mean = mean(.x),
    sd = sd(.x)
  ))
}


# Given a dataset and normalization constants it will create a min-max normalized
# version of the dataset.
minmax_norm <- function(x, desc) {
  map2_dfc(x, desc, ~(.x - .y$min)/(.y$max - .y$min))
}




#creating normalized versions of the dataset

#getting descriptive stattistics parameters for train and test data
desc_train<-train_df %>% 
  select(-Class) %>% 
    desc_stat()


desc_test <-test_df %>% 
  select(-Class) %>% 
  desc_stat()


#training inputs
x_train<- train_df %>% select(-Class) %>% 
    minmax_norm(desc_train) %>% 
    as.matrix()

#class lebels for training data
y_train<-train_df %>% select(Class) 


#testing inputs
x_test<-test_df %>% select(-Class) %>% 
  minmax_norm(desc_test) %>% 
  as.matrix()

#Class labels for test data
y_test<-test_df %>% select(Class)  



#--------------------MODEL DEFINATION----------------#


#generating a symmetric autoencodes with 3 dense layers

model<-keras_model_sequential()

model %>% 
  layer_dense(units=15,activation = "tanh", input_shape = ncol(x_train)) %>% 
  layer_dense(units=10,activation = "tanh") %>% 
  layer_dense(units = 18, activation = "tanh")

#let's check the summary of the model
summary(model)



