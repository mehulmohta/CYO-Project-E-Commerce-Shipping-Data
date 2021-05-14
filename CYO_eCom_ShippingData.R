# Note: this process could take a couple of minutes as we will load packages and library

#Install packages we may need
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")

#Libraries we are going to work 
library(tidyverse)
library(caret)
library(dplyr)
library(rpart)
library(randomForest)
library(matrixStats)
library(gbm)
library(data.table)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(rpart.plot)


#Download the eCommerce data file

d1 <- tempfile()
download.file("https://raw.githubusercontent.com/mehulmohta/CYO-Project-E-Commerce-Shipping-Data/main/Train.csv", d1)

#Read the csv file 
ecomdata <- read.csv(d1)

#The dataset used for model building contained 10999 observations of 12 variables.
#The data contains the following information:
  
#ID: ID Number of Customers.
#Warehouse block: The Company have big Warehouse which is divided in to block such as A,B,C,D,E.
#Mode of shipment:The Company Ships the products in multiple way such as Ship, Flight and Road.
#Customer care calls: The number of calls made by customer for enquiry of the shipment.
#Customer rating: The rating for every customer. 1 is the lowest (Worst), 5 is the highest (Best).
#Cost of the product: Cost of the Product in US Dollars.
#Prior purchases: The Number of Prior Purchases.
#Product importance: The company has categorized the product in the various parameter such as low, medium, high.
#Gender: Male and Female.
#Discount offered: Discount offered on that specific product. This is also in US Dollars
#Weight in gms: It is the weight in grams.
#Reached on time: It is the target variable, 1 Indicates that the product has NOT reached on time and 0 indicates it has reached on time.

##########################################################
# Exploratory analysis
##########################################################

#Examine the structure of the dataset
str(ecomdata)

#Missing value analysis 
colSums(is.na(ecomdata))

#Summary view of data
summary(ecomdata) 

#Converting variables to factors
names <- c(2,3,8,9,12)
ecomdata[,names] <- lapply(ecomdata[,names], factor)
str(ecomdata)

#But before we do not we would simplify the column headers
ecomdata <- ecomdata %>% dplyr::rename(ID= ï..ID) 
ecomdata <- ecomdata %>% dplyr::rename (Wr_bl = Warehouse_block)
ecomdata <- ecomdata %>% dplyr::rename (M_Ship=Mode_of_Shipment)
ecomdata <- ecomdata %>% dplyr::rename (Cust_Call=Customer_care_calls)
ecomdata <- ecomdata %>% dplyr::rename (Cust_Rat=Customer_rating)
ecomdata <- ecomdata %>% dplyr::rename (Cost_Prd=Cost_of_the_Product)
ecomdata <- ecomdata %>% dplyr::rename (Pri_Pur=Prior_purchases)
ecomdata <- ecomdata %>% dplyr::rename (Prd_Imp=Product_importance)
ecomdata <- ecomdata %>% dplyr::rename (Dis_Off= Discount_offered)
ecomdata <- ecomdata %>% dplyr::rename (Wt_gms=Weight_in_gms)
ecomdata <- ecomdata %>% dplyr::rename (Rch_Time = Reached.on.Time_Y.N)

#We will first see the distribution of the target variable Reached in Time
ecomdata %>% ggplot(aes(Rch_Time)) +
  geom_bar(aes(fill = Rch_Time),color = 'black') +
  theme_classic() +
  xlab("Reached in Time") + 
  ylab("Count of Shipments") +
  scale_fill_discrete(name = "Received On Time", labels = c("Yes", "No")) +
  labs(title = "Reached in Time Distribution")

prop.table(table(ecomdata$Rch_Time))*100

#We will see the distribution of Warehouse against the target variable Reached in Time
ecomdata %>% ggplot(aes(Wr_bl)) +
  geom_bar(aes(fill = Rch_Time),color = 'black') +
  theme_classic() +
  xlab("Warehouses") + 
  ylab("Count of Shipments") +
  scale_fill_discrete(name = "Received On Time", labels = c("Yes", "No")) +
  labs(title = "Warehouse Distribution over Reached in Time") 

prop.table(table(ecomdata$Wr_bl,ecomdata$Rch_Time))*100             
prop.table(table(ecomdata$Wr_bl,ecomdata$Rch_Time),1)*100

#We will now see the distribution of Mode of Shipping against the target variable Reached in Time
ecomdata %>% ggplot(aes(M_Ship)) +
  geom_bar(aes(fill = Rch_Time),color = 'black') +
  theme_classic() +
  xlab("Mode of Shipping") + 
  ylab("Count of Shipments") +
  scale_fill_discrete(name = "Received On Time", labels = c("Yes", "No")) +
  labs(title = "Mode of Shipping Distribution over Reached in Time") 
                                                                               
prop.table(table(ecomdata$M_Ship,ecomdata$Rch_Time))*100
prop.table(table(ecomdata$M_Ship,ecomdata$Rch_Time),1)*100

#We will see the distribution of Customer Calls against the target variable Reached in Time
ecomdata %>% ggplot(aes(Cust_Call)) +
  geom_bar(aes(fill = Rch_Time),color = 'black') +
  theme_classic() +
  xlab("Number of Customer Calls") + 
  ylab("Count of Shipments") +
  scale_fill_discrete(name = "Received On Time", labels = c("Yes", "No")) +
  labs(title = "No. of Customer Calls Distribution over Reached in Time")

prop.table(table(ecomdata$Cust_Call,ecomdata$Rch_Time))*100
prop.table(table(ecomdata$Cust_Call,ecomdata$Rch_Time),1)*100

#We will see the distribution of Customer Rating against the target variable Reached in Time
ecomdata %>% ggplot(aes(Cust_Rat)) +
  geom_bar(aes(fill = Rch_Time),color = 'black') +
  theme_classic() +
  xlab("Customer Rating") + 
  ylab("Count of Shipments") +
  scale_fill_discrete(name = "Received On Time", labels = c("Yes", "No")) +
  labs(title = "Customer Rating Distribution over Reached in Time")
  
prop.table(table(ecomdata$Cust_Rat,ecomdata$Rch_Time))*100
prop.table(table(ecomdata$Cust_Rat,ecomdata$Rch_Time),1)*100

#We will see the distribution of Cost of Product against the target variable Reached in Time
ecomdata %>% ggplot(aes(Cost_Prd)) +
  geom_histogram(aes(fill = Rch_Time),color= 'black' ,binwidth=20) +
  theme_classic() +
  xlab("Product Cost in $") + 
  ylab("Count of Shipments") +
  scale_fill_discrete(name = "Received On Time", labels = c("Yes", "No")) +
  labs(title= "Cost of the Product Distribution over Reached in Time") 

#We will see the distribution of Prior Purchases against the target variable Reached in Time
ecomdata %>% ggplot(aes(Pri_Pur)) +
  geom_bar(aes(fill = Rch_Time),color = 'black') +
  theme_classic()  +
  xlab("Number of Prior Purchases") + 
  ylab("Count of Shipments") +
  scale_fill_discrete(name = "Received On Time", labels = c("Yes", "No")) +
  labs(title= "Number of Prior Purchases Distribution over Reached in Time") 
  
prop.table(table(ecomdata$Pri_Pur,ecomdata$Rch_Time))*100
prop.table(table(ecomdata$Pri_Pur,ecomdata$Rch_Time),1)*100
#We will  see the distribution of Product Importance against the target variable Reached in Time
ecomdata %>% ggplot(aes(Prd_Imp)) +
  geom_bar(aes(fill = Rch_Time),color = 'black') +
  theme_classic()  +
  xlab("Product Importance Level") + 
  ylab("Count of Shipments") +
  scale_fill_discrete(name = "Received On Time", labels = c("Yes", "No")) +
  labs(title= "Number of Product Importance Distribution over Reached in Time") 
  
prop.table(table(ecomdata$Prd_Imp,ecomdata$Rch_Time))*100
prop.table(table(ecomdata$Prd_Imp,ecomdata$Rch_Time),1)*100

#We will see the distribution of Gender against the target variable Reached in Time
qplot(Rch_Time, data = ecomdata, fill = Gender) + facet_grid (. ~ Gender) +
  xlab("Gender") + 
  ylab("Count of Shipments") +
  scale_fill_discrete(name = "Received On Time", labels = c("Yes", "No")) +
  labs(title= "Gender Distribution over Reached in Time")

#We will see the distribution of Discount Offered against the target variable Reached in Time
ecomdata %>% ggplot(aes(Dis_Off)) +
  geom_histogram(aes(fill = Rch_Time),color= 'black' ,binwidth=5) +
  theme_classic() +
  xlab("Discount Offered in $") + 
  ylab("Count of Shipments") +
  scale_fill_discrete(name = "Received On Time", labels = c("Yes", "No")) +
  labs(title= "Discounts Offered Distribution over Reached in Time") 
  
v2 <- ecomdata %>% filter(Dis_Off >1.9, Dis_Off<6.1) %>% nrow()
v2/nrow(ecomdata)

#We will see the distribution of Product Weight in grams against the target variable Reached in Time
ecomdata %>% ggplot(aes(Wt_gms)) +
geom_histogram(aes(fill = Rch_Time),color= 'black' ,binwidth=500) +
theme_classic() +
xlab("Product Weight in gms") + 
ylab("Count of Shipments") +
scale_fill_discrete(name = "Received On Time", labels = c("Yes", "No")) +
labs(title= "Weight of product (in grams) Distribution over Reached in Time")

v3 <- ecomdata %>% filter(Wt_gms >3999, Wt_gms<6001) %>% nrow()
v3/nrow(ecomdata)

##########################################################
# Modelling & Data Analysis
##########################################################
#Let us perform correlation analysis, which is used to quantify the association between two variables. However since correlations will work on only numbers, we will convert the target variable to integer
ecomdata$Rch_Time <- as.integer(as.factor(ecomdata$Rch_Time))

##Correlation analysis
correlation <- cor(ecomdata[sapply(ecomdata, function(x) !is.factor(x))])
diag (correlation) = 0 #Remove self correlations
corrplot.mixed(correlation,tl.pos = "lt")

# We will turn the target variable back into factor class so that we can use in all our modelling exercises
ecomdata$Rch_Time <- as.factor(ecomdata$Rch_Time)

#We will now split the dataset into train and validation sets.
#Splitting the dataset for validation and training.
set.seed(1,sample.kind = "Rounding")  #if using R3.5 or earlier set.seed(1)
test_index <- createDataPartition(ecomdata$Rch_Time, times = 1, p = 0.2, list = FALSE)
              ecomdata_validation <- ecomdata[test_index, ]
              ecomdata_training <- ecomdata[-test_index, ]

#We will split the ecomdata_training dataset once more so we can use it to test out various models before choosing a final model.
#Splitting the ecomdata_training dataset again for testing
set.seed(10,sample.kind = "Rounding")  #if using R3.5 or earlier set.seed(10)
test_indexsplit <- createDataPartition(ecomdata_training$Rch_Time, times = 1, p = 0.2, list = FALSE)
testing <- ecomdata_training[test_indexsplit, ]
training <- ecomdata_training[-test_indexsplit, ]

## Machine Learning Models
#Now we will begin to test various machine learning models to see which has the highest overall accuracy in predicting target variable

### knn (K nearest neighbors) Model
#Train a knn model on our training dataset optimizing k as the tuning parameter
set.seed(100,sample.kind = "Rounding")  #if using R3.5 or earlier set.seed(100)
#Using a 10 fold cross-validation method to make our code run faster.
control <- trainControl(method = "cv", number = 10)
train_knn <- train(Rch_Time ~ ., 
method = "knn", 
data = training, 
tuneGrid = data.frame(k = seq(5,40,2)), 
trControl = control)

#Highlighting the optimized k value on this plot
ggplot(train_knn, highlight = TRUE)

#Use this code to see the best k value
train_knn$bestTune

#Compute the accuracy of the knn model on the testing dataset
knn_accuracy <- confusionMatrix(predict(train_knn, testing, type = "raw"), 
testing$Rch_Time)$overall["Accuracy"]

#Create a table to save our results for each model
accuracy_results <- tibble(method = "knn", Accuracy = knn_accuracy)

#View the knn accuracy results in our table
accuracy_results %>% knitr::kable()


### gbm (Gradient Boosting Machines) Model
#Train a gbm model on our training dataset using 10-fold cross-validation
set.seed(200,sample.kind = "Rounding") #if using R3.5 or earlier set.seed(200)
#Using a 10 fold cross-validation method to make our code run faster
trCtrl <- trainControl (method = "cv", number = 10)

#Train a gbm model
train_gbm <- train (Rch_Time~ .,
trControl = trCtrl, 
method = "gbm",
preProc="zv",
data = training,
verbose = FALSE)

#Compute the accuracy of our gbm model on the testing dataset
gbm_accuracy <- confusionMatrix(predict(train_gbm, testing, type = "raw"), 
testing$Rch_Time)$overall["Accuracy"]

#Save the gbm accuracy results to our table
accuracy_results <- bind_rows(accuracy_results, tibble(method="gbm",
Accuracy = gbm_accuracy))

#View the gbm accuracy results in our table
accuracy_results %>% knitr::kable()


### Classification Tree Model
#Train a Classification Tree model using rpart and optimizing for the complexity parameter
set.seed(300,sample.kind = "Rounding")  #if using R3.5 or earlier set.seed(300)
train_rpart <- train(Rch_Time ~ .,
method = "rpart",
tuneGrid = data.frame(cp = seq(0, 0.02, len=50)),
data = training)

#Highlight the optimized complexity parameter
ggplot(train_rpart, highlight=TRUE)

#Use this code to see the best cp value
train_rpart$bestTune

#Compute the accuracy of our Classification Tree model on the testing dataset
rpart_accuracy <- confusionMatrix(predict(train_rpart, testing),
testing$Rch_Time)$overall["Accuracy"]

#Save the Classification Tree model accuracy results to our table
accuracy_results <- bind_rows(accuracy_results,
tibble(method="rpart", Accuracy = rpart_accuracy))

#View the rpart accuracy results in our table
accuracy_results %>% knitr::kable()


### Random Forest Model
#Train a random forest model on our training dataset
set.seed(400,sample.kind = "Rounding")  #if using R3.5 or earlier set.seed(400)
train_rf <- randomForest(Rch_Time ~ ., data = training)

#Compute the accuracy of our random forest model on the testing dataset
rf_accuracy <- confusionMatrix(predict(train_rf, testing),
testing$Rch_Time)$overall["Accuracy"]

#Save the random forest accuracy results to our table
accuracy_results <- bind_rows(accuracy_results,
tibble(method="random forest", Accuracy = rf_accuracy))

#View the random forest accuracy results in our table
accuracy_results %>% knitr::kable()


### Testing the Final Model on our Validation Set
#Train our final random forest model on our census_training dataset
set.seed(300, sample.kind = "Rounding") #if using R3.5 or earlier set.seed(300)
final_train_rpart <- train(Rch_Time ~ .,
method = "rpart",
tuneGrid = data.frame(cp = seq(0, 0.02, len=50)), 
data = ecomdata_training)

#Compute the accuracy of our final random forest model on the validation set
final_accuracy <- confusionMatrix(predict(final_train_rpart,
ecomdata_validation),
ecomdata_validation$Rch_Time)$overall["Accuracy"]

#Save the classification tree accuracy results to our table.
accuracy_results <- bind_rows(accuracy_results,
tibble(method="Final rpart",
Accuracy = final_accuracy))

#View the final classification tree model accuracy results in our table
accuracy_results %>% knitr::kable()

##########################################################
# Results & Conclusion
##########################################################

#The goal of this project (as defined at the beginning) was to train a machine learning algorithm using the inputs in one (training) set to predict delivery reaching in time in the other (validation) set. The data set used was from Kaggle Data Repository - https://www.kaggle.com/prachi13/customer-analytics. We first analyzed each variable independently, and then we analyzed 4 different algorithms - knn, gbm, rpart & random forest. We saw rpart (Classification Tree Model) estimated best accuracy. 
#We can see the same in table below, the classification tree model has the highest overall accuracy of 69.4% on the data we set aside for training and testing purposes. After choosing classification tree as our final model, we tested our predictions on the validation dataset and achieved an overall accuracy of 69.6% for predicting whether the delivery via eCommerce will reach in time or not.

### Learnings & Limitations 
#As I studied this data closely, some of the thoughts I would like to share before closing the report. Data analysis and modeling exercise made it very clear that there is a relationship between variables and if studied effectively can give very valuable insights into consumer shopping behavior. Though at certain points the data looked little distant from reality *(as seen in some of the exploratory analysis - almost near perfect distribution).* Also there could have been a couple of more variables like option for faster delivery and classification of product type as either *'Consumer Durable' or 'Fashion' or 'Home Improvement' or 'Grocery* cause the type of product purchased could have inherent expectation on delivery time. 
