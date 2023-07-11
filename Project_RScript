### The code for each of the three tasks is designed to work independently from the other tasks.
### Due to the highly resource-hungry code for the Demand & Forecasting part - TASK 3 -, which fills up the RAM quickly, we suggest to run the code for each of the models separately (i.e. run model 1 -> get result, run model 2-> get result, and so on), unless you have a machine that is powerful enough.

### ------------------------------------- TASK 1 --------------------------------------###

library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)

transactions <- read_csv("transactions.csv")
products <- read_csv("products.csv")

options(stringsAsFactors = FALSE)

#we join the transactions and products datasets into one dataset called transactionsANDproducts
transactionsANDproducts <-full_join(transactions, products, by ="UPC")
class(transactionsANDproducts$WEEK_END_DATE)

# Then use as.Date() to convert text strings into dates, 
transactionsANDproducts$WEEK_END_DATE<-as.Date(transactionsANDproducts$WEEK_END_DATE,"%d-%b-%y")

# and format() to change the way the dates are formatted we use %b(month abbreviated name)-%Y(all 4 digits of the year)
transactionsANDproducts$month_year<-format(transactionsANDproducts$WEEK_END_DATE,"%b-%Y")

t2<-write.csv(transactionsANDproducts,"transactionsproducts.csv", row.names=FALSE)

#We import transactionsproducts and change month_year from character to date as "%b-%Y"

transactionsproducts <- read_csv("transactionsproducts.csv",
                                 col_types = cols(month_year = col_date(format = "%b-%Y")))

transactionsproducts %>%
  ggplot(aes(x=month_year, y=PRICE, group=CATEGORY, color=CATEGORY))+
  geom_line(show.legend=FALSE)+
  labs(title="Prices over Time")+
  facet_wrap(CATEGORY ~ .)+
  theme(axis.text.x=element_text(angle=90),
        plot.title= element_text(hjust=0.5),
        strip.text.x= element_text(size=5))

# we observe the price through 1 year to observe seasonal changes

#After we join the transactions and products datasets into one dataset,
#we take a subset of year 2010 by extracting  the relevant rows and columns

year_2010 <- transactionsANDproducts[c(164275:343688),c(1,3,8,15)]
class(year_2010$WEEK_END_DATE)

year_2010$WEEK_END_DATE<-as.Date(year_2010$WEEK_END_DATE,"%d-%b-%y")
year_2010$month_year<-format(year_2010$WEEK_END_DATE,"%b-%Y")

t2<-write.csv(year_2010,"transactionsproducts_2010.csv", row.names=FALSE)

#We turn our 'month_year' axis into a character vector
year_2010$month_year<- as.character(year_2010$month_year)

#Then turn it back into a factor with the levels in the correct order
year_2010$month_year <- factor(year_2010$month_year, levels=unique(year_2010$month_year))

#As before we import transactionsproducts and change month_year to date as "%b-%Y"and plot the graph

year_2010 %>%
  ggplot(aes(x=month_year, y=PRICE, group=CATEGORY, color=CATEGORY))+
  geom_line(show.legend=FALSE)+
  labs(title="Prices over year 2010")+
  facet_wrap(CATEGORY ~ .)+
  theme(axis.text.x=element_text(angle=90),
        plot.title= element_text(hjust=0.5),
        strip.text.x= element_text(size=5))

#finally to observe if the changes are consistent we also look at year 2011

#After we join the transactions and products datasets into one dataset,we take a subset of year 2011 

year_2011 <- transactionsANDproducts[c(343689:521623),c(1,3,8,15)]
class(year_2011$WEEK_END_DATE)

year_2011$WEEK_END_DATE<-as.Date(year_2011$WEEK_END_DATE,"%d-%b-%y")
year_2011$month_year<-format(year_2011$WEEK_END_DATE,"%b-%Y")

t2<-write.csv(year_2011,"transactionsproducts_2011.csv", row.names=FALSE)

#As before we import transactionsproducts and change month_year to date as "%b-%Y"and plot the graph
#We turn our 'month_year' axis into a character vector
year_2011$month_year<- as.character(year_2011$month_year)

#Then turn it back into a factor with the levels in the correct order
year_2011$month_year <- factor(year_2011$month_year, levels=unique(year_2011$month_year))

#As before we import transactionsproducts and change month_year to date as "%b-%Y"and plot the graph
year_2011 %>%
  ggplot(aes(x=month_year, y=PRICE, group=CATEGORY, color=CATEGORY))+
  geom_line(show.legend=FALSE)+
  labs(title="Prices over year 2011")+
  facet_wrap(CATEGORY ~ .)+
  theme(axis.text.x=element_text(angle=90),
        plot.title= element_text(hjust=0.5),
        strip.text.x= element_text(size=5))



#We create an enlarged transactions dataset, selecting only the additional columns that we need from the products dataset
transactions_new <- left_join(transactions, products %>% select(UPC, CATEGORY))

#We create a new set that contains the sum of all sells made overtime for each category
TotalTime <- transactions_new %>%
  group_by(CATEGORY) %>%
  summarize(transactions_new_TotalSales = sum(SPEND))

TotalTime

#We plot into a bar plot the data obtain just above
ggplot(TotalTime, aes(x = CATEGORY, y = transactions_new_TotalSales, color = CATEGORY)) +
  geom_col() +
  ggtitle("Total Sales per Category in USD") + 
  labs(y = "Value of Sales in USD")

#We create a new set that contains the sum of all features and displays advertising made overtime for each category
TotalFeatureAndDisplay <- transactions_new %>%
  group_by(CATEGORY) %>%
  summarize(transactions_new_TotalFeatureAndDisplay = sum(FEATURE, DISPLAY))

TotalFeatureAndDisplay

#We plot into a bar plot the data obtain just above
ggplot(TotalFeatureAndDisplay, aes(x = CATEGORY, y = transactions_new_TotalFeatureAndDisplay, color = CATEGORY)) +
  geom_col() +
  ggtitle("Total features or displays advertising per Category") +
  labs(y = "Amount of Feature/Display Advertising")

#We create a new set that contains the sum of all sells made overtime for each category
TotalUnits <- transactions_new %>%
  group_by(CATEGORY) %>%
  summarize(transactions_new_TotalUnits = sum(UNITS))

TotalUnits

#We plot into a bar plot the data obtain just above
ggplot(TotalUnits, aes(x = CATEGORY, y = transactions_new_TotalUnits, color = CATEGORY)) +
  geom_col() +
  ggtitle("Total number of Sales per Category") +
  labs(y = "Total Amount of Products Sold")


#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

### ------------------------------------- TASK 2 --------------------------------------###

library(readr)
library(dplyr)
library(ggplot2)

#We import the datasets
transactions <- read_csv("transactions.csv")
products <- read_csv("products.csv")

#We visualize the datasets
View(transactions)
View(products)

#We create an enlarged transactions dataset, selecting only the additional columns that we need from the products dataset
transactions_new <- left_join(transactions, products %>% select(UPC, CATEGORY))

#We add new columns that contain LOG values of units and price, that we will need to fit our model
transactions_new <- mutate(transactions_new, LOG_UNITS = log(UNITS), LOG_PRICE = log(PRICE))

#We now filter the dataset and divide it into four smaller datasets according to category

transactions_bagSnacks <- filter(transactions_new, CATEGORY == "BAG SNACKS")

transactions_bagSnacks

transactions_oralHygiene <- filter(transactions_new, CATEGORY == "ORAL HYGIENE PRODUCTS")

transactions_oralHygiene

transactions_coldCereal <- filter(transactions_new, CATEGORY == "COLD CEREAL")

transactions_coldCereal

transactions_frozenPizza <- filter(transactions_new, CATEGORY == "FROZEN PIZZA")

transactions_frozenPizza

#We fit our model for the Bag Snacks category, using LOG_PRICE, FEATURE and DISPLAY as coefficients
lm_bagsnacks <- lm(formula = LOG_UNITS ~ LOG_PRICE + FEATURE + DISPLAY, data = subset(transactions_bagSnacks, PRICE > 0))

#We visualize the model
summary (lm_bagsnacks)

#We extract the coefficients
pc_bagsnacks <- summary(lm_bagsnacks)$coefficients[2]
feat_bagsnacks <- summary(lm_bagsnacks)$coefficients[3]
disp_bagsnacks <- summary(lm_bagsnacks)$coefficients[4]

#We fit our model for the Oral Hygiene category, again using LOG_PRICE, FEATURE and DISPLAY as coefficients
lm_oralHygiene <- lm(formula = LOG_UNITS ~ LOG_PRICE + FEATURE + DISPLAY, data = subset(transactions_oralHygiene, PRICE > 0))

#We visualize the model
summary (lm_oralHygiene)

#We extract the coefficients
pc_oralHygiene <- summary(lm_oralHygiene)$coefficients[2]
feat_oralHygiene <- summary(lm_oralHygiene)$coefficients[3]
disp_oralHygiene <- summary(lm_oralHygiene)$coefficients[4]

#We fit our model for the Cold Cereal category, again using LOG_PRICE, FEATURE and DISPLAY as coefficients
lm_coldCereal <- lm(formula = LOG_UNITS ~ LOG_PRICE + FEATURE + DISPLAY, data = subset(transactions_coldCereal, PRICE > 0))

#We visualize the model
summary (lm_coldCereal)

#We extract the coefficients
pc_coldCereal <- summary(lm_coldCereal)$coefficients[2]
feat_coldCereal <- summary(lm_coldCereal)$coefficients[3]
disp_coldCereal <- summary(lm_coldCereal)$coefficients[4]

#We fit our model for the Frozen Pizza category, again using LOG_PRICE, FEATURE and DISPLAY as coefficients
lm_frozenPizza <- lm(formula = LOG_UNITS ~ LOG_PRICE + FEATURE + DISPLAY, data = subset(transactions_frozenPizza, PRICE > 0))
summary (lm_frozenPizza)

#We extract the coefficients
pc_frozenPizza <- summary(lm_frozenPizza)$coefficients[2]
feat_frozenPizza <- summary(lm_frozenPizza)$coefficients[3]
disp_frozenPizza <- summary(lm_frozenPizza)$coefficients[4]

#We create a data frame containing the coefficients we have extracted for price elasticity
pc_all_category <- c("Bagsnacks","Oral Hygiene","Cold Cereal","Frozen Pizza")
pc_all_elasticity <- c(pc_bagsnacks, pc_oralHygiene, pc_coldCereal, pc_frozenPizza)
price_coefficients_data <- data.frame(pc_all_category, pc_all_elasticity)

#We visualize the data frame
price_coefficients_data

#We plot the price elasticity coefficients using a bar chart
ggplot(price_coefficients_data, aes(x = pc_all_category, y = pc_all_elasticity, fill = pc_all_category)) + 
  geom_col() +
  labs(title = "Price Elasticity for Different Categories", y = "Price Elasticity", x = "Category")

#We create another data frame for the coefficients we have extracted for feature advertising
feat_all_category <- c("Bagsnacks","Oral Hygiene","Cold Cereal","Frozen Pizza")
feat_all_elasticity <- c(feat_bagsnacks, feat_oralHygiene, feat_coldCereal, feat_frozenPizza)
feature_coefficients_data <- data.frame(feat_all_category, feat_all_elasticity)

#We visualize the data frame
feature_coefficients_data

#We plot the feature advertising coefficients using a bar chart
ggplot(feature_coefficients_data, aes(x = feat_all_category, y = feat_all_elasticity, fill = feat_all_category)) + 
  geom_col() +
  labs(title = "Impact of Feature Advertising on Sales", y = "Impact of Feature Adv.", x = "Category")

#Finally we create a data frame for the coefficients we have extracted for display advertising
disp_all_category <- c("Bagsnacks","Oral Hygiene","Cold Cereal","Frozen Pizza")
disp_all_elasticity <- c(disp_bagsnacks, disp_oralHygiene, disp_coldCereal, disp_frozenPizza)
display_coefficients_data <- data.frame(disp_all_category, disp_all_elasticity)

#We visualize the data frame
display_coefficients_data

#We plot the display advertising coefficients using a bar chart
ggplot(display_coefficients_data, aes(x = disp_all_category, y = disp_all_elasticity, fill = disp_all_category)) + 
  geom_col() +
  labs(title = "Impact of Display Advertising on Sales", y = "Impact of Display Adv.", x = "Category")


#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

### ------------------------------------- TASK 3 --------------------------------------###


#Load functions
library(readr)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
require(caret)
require(mlbench)

# Read in data
train.df <- read_csv("train.csv")
test.df <- read_csv("test.csv")

### Linear Regression
## Partitioning of data
# Set random seed
set.seed(111) 

# Create index matrix
index <- createDataPartition(train.df$UNITS, p=0.8, list=FALSE, times=1)

# Convert df into actual data frame (from tibble)
train.df <- as.data.frame(train.df)

# Create train_df and test_df
train_df <- train.df[index,]
test_df <- train.df[-index,]

# Specify training method and number of folds for cross-validation
ctrlspecs <- trainControl(method="cv", number=10)

# Set random seed before every model
# Specify linear regression model (predictor variables:  PRICE, FEATURE, DISPLAY, TPR_ONLY + BASE_PRICE)
# Print models and compare their in-sample RMSE
# Compare the models’ out-sample RMSE
set.seed(111)
model1 <- train(UNITS ~ PRICE + BASE_PRICE,
                data=train_df,
                method="lm",
                trControl=ctrlspecs)

print(model1)
lm.pred.model1.out = predict(model1, newdata=test_df)
sqrt(mean((lm.pred.model1.out - test_df$UNITS)^2))

set.seed(111)
model2 <- train(UNITS ~ PRICE + FEATURE,
                data=train_df,
                method="lm",
                trControl=ctrlspecs)

print(model2)
lm.pred.model2.out = predict(model2, newdata=test_df)
sqrt(mean((lm.pred.model2.out - test_df$UNITS)^2))

set.seed(111)
model3 <- train(UNITS ~ PRICE + DISPLAY,
                data=train_df,
                method="lm",
                trControl=ctrlspecs)

print(model3)
lm.pred.model3.out = predict(model3, newdata=test_df)
sqrt(mean((lm.pred.model3.out - test_df$UNITS)^2))

set.seed(111)
model4 <- train(UNITS ~ PRICE + TPR_ONLY,
                data=train_df,
                method="lm",
                trControl=ctrlspecs)

print(model4)
lm.pred.model4.out = predict(model4, newdata=test_df)
sqrt(mean((lm.pred.model4.out - test_df$UNITS)^2))

set.seed(111)
model5 <- train(UNITS ~ BASE_PRICE + FEATURE,
                data=train_df,
                method="lm",
                trControl=ctrlspecs)

print(model5)
lm.pred.model5.out = predict(model5, newdata=test_df)
sqrt(mean((lm.pred.model5.out - test_df$UNITS)^2))

set.seed(111)
model6 <- train(UNITS ~ BASE_PRICE + DISPLAY,
                data=train_df,
                method="lm",
                trControl=ctrlspecs)

print(model6)
lm.pred.model6.out = predict(model6, newdata=test_df)
sqrt(mean((lm.pred.model6.out - test_df$UNITS)^2))

set.seed(111)
model7 <- train(UNITS ~ BASE_PRICE + TPR_ONLY,
                data=train_df,
                method="lm",
                trControl=ctrlspecs)

print(model7)
lm.pred.model7.out = predict(model7, newdata=test_df)
sqrt(mean((lm.pred.model7.out - test_df$UNITS)^2))

set.seed(111)
model8 <- train(UNITS ~ FEATURE + DISPLAY,
                data=train_df,
                method="lm",
                trControl=ctrlspecs)

print(model8)
lm.pred.model8.out = predict(model8, newdata=test_df)
sqrt(mean((lm.pred.model8.out - test_df$UNITS)^2))

set.seed(111)
model9 <- train(UNITS ~ FEATURE + TPR_ONLY,
                data=train_df,
                method="lm",
                trControl=ctrlspecs)

print(model9)
lm.pred.model9.out = predict(model9, newdata=test_df)
sqrt(mean((lm.pred.model9.out - test_df$UNITS)^2))

set.seed(111)
model10 <- train(UNITS ~ DISPLAY + TPR_ONLY,
                 data=train_df,
                 method="lm",
                 trControl=ctrlspecs)

print(model10)
lm.pred.model10.out = predict(model10, newdata=test_df)
sqrt(mean((lm.pred.model10.out - test_df$UNITS)^2))

set.seed(111)
model11 <- train(UNITS ~ PRICE + BASE_PRICE + FEATURE,
                 data=train_df,
                 method="lm",
                 trControl=ctrlspecs)

print(model11)
lm.pred.model11.out = predict(model11, newdata=test_df)
sqrt(mean((lm.pred.model11.out - test_df$UNITS)^2))

set.seed(111)
model12 <- train(UNITS ~ PRICE + BASE_PRICE + DISPLAY,
                 data=train_df,
                 method="lm",
                 trControl=ctrlspecs)

print(model12)
lm.pred.model12.out = predict(model12, newdata=test_df)
sqrt(mean((lm.pred.model12.out - test_df$UNITS)^2))

set.seed(111)
model13 <- train(UNITS ~ PRICE + BASE_PRICE + TPR_ONLY,
                 data=train_df,
                 method="lm",
                 trControl=ctrlspecs)

print(model13)
lm.pred.model13.out = predict(model13, newdata=test_df)
sqrt(mean((lm.pred.model13.out - test_df$UNITS)^2))

set.seed(111)
model14 <- train(UNITS ~ PRICE + FEATURE + DISPLAY,
                 data=train_df,
                 method="lm",
                 trControl=ctrlspecs)

print(model14)
lm.pred.model14.out = predict(model14, newdata=test_df)
sqrt(mean((lm.pred.model14.out - test_df$UNITS)^2))

set.seed(111)
model15 <- train(UNITS ~ PRICE + FEATURE + TPR_ONLY,
                 data=train_df,
                 method="lm",
                 trControl=ctrlspecs)

print(model15)
lm.pred.model15.out = predict(model15, newdata=test_df)
sqrt(mean((lm.pred.model15.out - test_df$UNITS)^2))

set.seed(111)
model16 <- train(UNITS ~ PRICE + DISPLAY + TPR_ONLY,
                 data=train_df,
                 method="lm",
                 trControl=ctrlspecs)

print(model16)
lm.pred.model16.out = predict(model16, newdata=test_df)
sqrt(mean((lm.pred.model16.out - test_df$UNITS)^2))

set.seed(111)
model17 <- train(UNITS ~ BASE_PRICE + FEATURE + DISPLAY,
                 data=train_df,
                 method="lm",
                 trControl=ctrlspecs)

print(model17)
lm.pred.model17.out = predict(model17, newdata=test_df)
sqrt(mean((lm.pred.model17.out - test_df$UNITS)^2))

set.seed(111)
model18 <- train(UNITS ~ BASE_PRICE + FEATURE + TPR_ONLY,
                 data=train_df,
                 method="lm",
                 trControl=ctrlspecs)

print(model18)
lm.pred.model18.out = predict(model18, newdata=test_df)
sqrt(mean((lm.pred.model18.out - test_df$UNITS)^2))

set.seed(111)
model19 <- train(UNITS ~ BASE_PRICE + DISPLAY + TPR_ONLY,
                 data=train_df,
                 method="lm",
                 trControl=ctrlspecs)

print(model19)
lm.pred.model19.out = predict(model19, newdata=test_df)
sqrt(mean((lm.pred.model19.out - test_df$UNITS)^2))

set.seed(111)
model20 <- train(UNITS ~ FEATURE + DISPLAY + TPR_ONLY,
                 data=train_df,
                 method="lm",
                 trControl=ctrlspecs)

print(model20)
lm.pred.model20.out = predict(model20, newdata=test_df)
sqrt(mean((lm.pred.model20.out - test_df$UNITS)^2))

set.seed(111)
model21 <- train(UNITS ~ PRICE + BASE_PRICE + FEATURE + DISPLAY,
                 data=train_df,
                 method="lm",
                 trControl=ctrlspecs)

print(model21)
lm.pred.model21.out = predict(model21, newdata=test_df)
sqrt(mean((lm.pred.model21.out - test_df$UNITS)^2))

set.seed(111)
model22 <- train(UNITS ~ PRICE + BASE_PRICE + FEATURE + TPR_ONLY,
                 data=train_df,
                 method="lm",
                 trControl=ctrlspecs)

print(model22)
lm.pred.model22.out = predict(model22, newdata=test_df)
sqrt(mean((lm.pred.model22.out - test_df$UNITS)^2))

set.seed(111)
model23 <- train(UNITS ~ PRICE + BASE_PRICE +  DISPLAY + TPR_ONLY,
                 data=train_df,
                 method="lm",
                 trControl=ctrlspecs)

print(model23)
lm.pred.model23.out = predict(model23, newdata=test_df)
sqrt(mean((lm.pred.model23.out - test_df$UNITS)^2))

set.seed(111)
model24 <- train(UNITS ~ PRICE + FEATURE + DISPLAY + TPR_ONLY,
                 data=train_df,
                 method="lm",
                 trControl=ctrlspecs)

print(model24)
lm.pred.model24.out = predict(model24, newdata=test_df)
sqrt(mean((lm.pred.model24.out - test_df$UNITS)^2))

set.seed(111)
model25 <- train(UNITS ~ BASE_PRICE +  FEATURE + DISPLAY + TPR_ONLY,
                 data=train_df,
                 method="lm",
                 trControl=ctrlspecs)

print(model25)
lm.pred.model25.out = predict(model25, newdata=test_df)
sqrt(mean((lm.pred.model25.out - test_df$UNITS)^2))

set.seed(111)
model26 <- train(UNITS ~ PRICE + BASE_PRICE +  FEATURE + DISPLAY + TPR_ONLY,
                 data=train_df,
                 method="lm",
                 trControl=ctrlspecs)

print(model26)
lm.pred.model26.out = predict(model26, newdata=test_df)
sqrt(mean((lm.pred.model26.out - test_df$UNITS)^2))


summary(model21)
summary(model26)

# Model 21 and 26  have similar R-squared value and similar training in-sample and out-sample RMSE. However, the TPR_ONLY variable in Model 26 is not significant while all the variables in Model 21 are significant. Therefore, Model 21 is the best model for linear regression.

# Test Model 21's Out-Sample RMSE
lm.pred.model21.test.out = predict(model21, newdata=test.df)
sqrt(mean((lm.pred.model21.test.out - test.df$UNITS)^2))

### Regression Tree
# Model 1
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.1=rpart(UNITS ~  PRICE + BASE_PRICE, data=train.data, method="anova")
  tree.pred.1 = predict(tree.fit.1, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.1 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 2
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.2=rpart(UNITS ~  PRICE + FEATURE, data=train.data, method="anova")
  tree.pred.2 = predict(tree.fit.2, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.2 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 3
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.3=rpart(UNITS ~  PRICE + DISPLAY, data=train.data, method="anova")
  tree.pred.3 = predict(tree.fit.3, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.3 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 4
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.4=rpart(UNITS ~  PRICE + TPR_ONLY, data=train.data, method="anova")
  tree.pred.4 = predict(tree.fit.4, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.4 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 5
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.5=rpart(UNITS ~ BASE_PRICE + FEATURE, data=train.data, method="anova")
  tree.pred.5 = predict(tree.fit.5, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.5 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 6
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.6=rpart(UNITS ~  BASE_PRICE + DISPLAY, data=train.data, method="anova")
  tree.pred.6 = predict(tree.fit.6, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.6 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 7
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.7=rpart(UNITS ~  BASE_PRICE + TPR_ONLY, data=train.data, method="anova")
  tree.pred.7 = predict(tree.fit.7, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.7 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 8
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.8=rpart(UNITS ~  FEATURE + DISPLAY, data=train.data, method="anova")
  tree.pred.8 = predict(tree.fit.8, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.8 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 9
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.9=rpart(UNITS ~ FEATURE + TPR_ONLY, data=train.data, method="anova")
  tree.pred.9 = predict(tree.fit.9, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.9 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 10
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.10=rpart(UNITS ~  DISPLAY + TPR_ONLY, data=train.data, method="anova")
  tree.pred.10 = predict(tree.fit.10, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.10 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 11
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.11=rpart(UNITS ~  PRICE + BASE_PRICE + FEATURE, data=train.data, method="anova")
  tree.pred.11 = predict(tree.fit.11, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.11 - test.data$UNITS)^2))
}
mean(tree.rmse)
rpart.plot(tree.fit.11)


# Model 12
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.12=rpart(UNITS ~  PRICE + BASE_PRICE + DISPLAY, data=train.data, method="anova")
  tree.pred.12 = predict(tree.fit.12, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.12 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 13
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.13=rpart(UNITS ~  PRICE + BASE_PRICE + TPR_ONLY, data=train.data, method="anova")
  tree.pred.13 = predict(tree.fit.13, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.13 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 14
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.14=rpart(UNITS ~  PRICE + FEATURE + DISPLAY, data=train.data, method="anova")
  tree.pred.14 = predict(tree.fit.14, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.14 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 15
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.15=rpart(UNITS ~  PRICE + FEATURE + TPR_ONLY, data=train.data, method="anova")
  tree.pred.15 = predict(tree.fit.15, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.15 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 16
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.16=rpart(UNITS ~  PRICE + DISPLAY + TPR_ONLY, data=train.data, method="anova")
  tree.pred.16 = predict(tree.fit.16, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.16 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 17
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.17=rpart(UNITS ~  BASE_PRICE + FEATURE + DISPLAY, data=train.data, method="anova")
  tree.pred.17 = predict(tree.fit.17, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.17 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 18
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.18=rpart(UNITS ~  BASE_PRICE + FEATURE + TPR_ONLY, data=train.data, method="anova")
  tree.pred.18 = predict(tree.fit.18, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.18 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 19
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.19=rpart(UNITS ~  BASE_PRICE + DISPLAY + TPR_ONLY, data=train.data, method="anova")
  tree.pred.19 = predict(tree.fit.19, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.19 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 20
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.20=rpart(UNITS ~  FEATURE + DISPLAY + TPR_ONLY, data=train.data, method="anova")
  tree.pred.20 = predict(tree.fit.20, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.20 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 21
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.21=rpart(UNITS ~  PRICE + BASE_PRICE + FEATURE + DISPLAY, data=train.data, method="anova")
  tree.pred.21 = predict(tree.fit.21, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.21 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 22
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.22=rpart(UNITS ~  PRICE + BASE_PRICE + FEATURE + TPR_ONLY, data=train.data, method="anova")
  tree.pred.22 = predict(tree.fit.22, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.22 - test.data$UNITS)^2))
}
mean(tree.rmse)
rpart.plot(tree.fit.22)


# Model 23
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.23=rpart(UNITS ~  PRICE + BASE_PRICE + DISPLAY + TPR_ONLY, data=train.data, method="anova")
  tree.pred.23 = predict(tree.fit.23, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.23 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 24
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.24=rpart(UNITS ~  PRICE + FEATURE + DISPLAY + TPR_ONLY, data=train.data, method="anova")
  tree.pred.24 = predict(tree.fit.24, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.24 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 25
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.25=rpart(UNITS ~  BASE_PRICE + FEATURE + DISPLAY + TPR_ONLY, data=train.data, method="anova")
  tree.pred.25 = predict(tree.fit.25, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.25 - test.data$UNITS)^2))
}
mean(tree.rmse)

# Model 26
set.seed(111)
k = 10
folds = createFolds(1:nrow(train.df), k=k)
lm.rmse = double(k)
tree.rmse = double(k)
for(i in 1:k){
  test.data = train.df[folds[[i]],]
  train.data = train.df[-folds[[i]],]
  tree.fit.26=rpart(UNITS ~  PRICE + BASE_PRICE + FEATURE + DISPLAY + TPR_ONLY, data=train.data, method="anova")
  tree.pred.26 = predict(tree.fit.26, newdata=test.data)
  tree.rmse[i] = sqrt(mean((tree.pred.26 - test.data$UNITS)^2))
}
mean(tree.rmse)


## Model 11 and 22 have the smallest RMSE of 25.48388. To choose the better model, the tree plots of the two models are looked at. The tree plot for Model 11 contains all its variables while the tree plot for Model 22 does not contain all its variables. Therefore, Model 11 is the best model for the regression tree. 

# Test Model 11's Out-Sample RMSE
tree.fit.11.test.out = predict(tree.fit.11, newdata=test.df)
sqrt(mean((tree.fit.11.test.out - test.df$UNITS)^2))


### Random Forest
str(train)

#Random Forest

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf1 <- train(UNITS ~ PRICE + BASE_PRICE ,
                     data=na.exclude(train.df),
                     method = "rf",
                     trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf1$bestTune

plot(varImp(modelFitrf1,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf2 <- train(UNITS ~ PRICE + FEATURE ,
                     data=na.exclude(train.df),
                     method = "rf",
                     trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf2$bestTune

plot(varImp(modelFitrf2,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf3 <- train(UNITS ~ PRICE + DISPLAY ,
                     data=na.exclude(train.df),
                     method = "rf",
                     trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf3$bestTune

plot(varImp(modelFitrf3,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf4 <- train(UNITS ~ PRICE + TPR_ONLY ,
                     data=na.exclude(train.df),
                     method = "rf",
                     trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf4$bestTune

plot(varImp(modelFitrf4,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf5 <- train(UNITS ~ BASE_PRICE + FEATURE ,
                     data=na.exclude(train.df),
                     method = "rf",
                     trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf5$bestTune

plot(varImp(modelFitrf5,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf6 <- train(UNITS ~ BASE_PRICE + DISPLAY ,
                     data=na.exclude(train.df),
                     method = "rf",
                     trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf5$bestTune

plot(varImp(modelFitrf5,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf7 <- train(UNITS ~ BASE_PRICE + TPR_ONLY ,
                     data=na.exclude(train.df),
                     method = "rf",
                     trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf7$bestTune

plot(varImp(modelFitrf7,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf8 <- train(UNITS ~ FEATURE + DISPLAY ,
                     data=na.exclude(train.df),
                     method = "rf",
                     trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf8$bestTune

plot(varImp(modelFitrf8,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf9 <- train(UNITS ~ FEATURE + TPR_ONLY ,
                     data=na.exclude(train.df),
                     method = "rf",
                     trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf9$bestTune

plot(varImp(modelFitrf9,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf10 <- train(UNITS ~ DISPLAY + TPR_ONLY,
                      data=na.exclude(train.df),
                      method = "rf",
                      trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf10$bestTune

plot(varImp(modelFitrf10,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf11 <- train(UNITS ~ PRICE + BASE_PRICE + FEATURE,
                      data=na.exclude(train.df),
                      method = "rf",
                      trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf11$bestTune

plot(varImp(modelFitrf11,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf12 <- train(UNITS ~ PRICE + BASE_PRICE + DISPLAY,
                      data=na.exclude(train.df),
                      method = "rf",
                      trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf12$bestTune

plot(varImp(modelFitrf12,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf13 <- train(UNITS ~ PRICE + BASE_PRICE + TPR_ONLY,
                      data=na.exclude(train.df),
                      method = "rf",
                      trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf13$bestTune

plot(varImp(modelFitrf13,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf14 <- train(UNITS ~ PRICE + FEATURE + DISPLAY,
                      data=na.exclude(train.df),
                      method = "rf",
                      trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf14$bestTune

plot(varImp(modelFitrf14,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf15 <- train(UNITS ~ PRICE + FEATURE + TPR_ONLY,
                      data=na.exclude(train.df),
                      method = "rf",
                      trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf15$bestTune

plot(varImp(modelFitrf15,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf16 <- train(UNITS ~ PRICE + DISPLAY + TPR_ONLY,
                      data=na.exclude(train.df),
                      method = "rf",
                      trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf16$bestTune

plot(varImp(modelFitrf16,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf17 <- train(UNITS ~ BASE_PRICE + FEATURE + DISPLAY,
                      data=na.exclude(train.df),
                      method = "rf",
                      trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf17$bestTune

plot(varImp(modelFitrf17,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf18 <- train(UNITS ~ BASE_PRICE + FEATURE + TPR_ONLY,
                      data=na.exclude(train.df),
                      method = "rf",
                      trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf18$bestTune

plot(varImp(modelFitrf18,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf19 <- train(UNITS ~ BASE_PRICE + DISPLAY + TPR_ONLY,
                      data=na.exclude(train.df),
                      method = "rf",
                      trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf19$bestTune

plot(varImp(modelFitrf19,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf20 <- train(UNITS ~ FEATURE + DISPLAY + TPR_ONLY,
                      data=na.exclude(train.df),
                      method = "rf",
                      trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf20$bestTune

plot(varImp(modelFitrf20,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf21 <- train(UNITS ~ PRICE + BASE_PRICE + FEATURE + DISPLAY,
                      data=na.exclude(train.df),
                      method = "rf",
                      trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf21$bestTune

plot(varImp(modelFitrf21,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf22 <- train(UNITS ~ PRICE + BASE_PRICE + FEATURE + TPR_ONLY,
                      data=na.exclude(train.df),
                      method = "rf",
                      trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf22$bestTune

plot(varImp(modelFitrf22,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf23 <- train(UNITS ~ PRICE + BASE_PRICE + DISPLAY + TPR_ONLY,
                      data=na.exclude(train.df),
                      method = "rf",
                      trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf23$bestTune

plot(varImp(modelFitrf23,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf24 <- train(UNITS ~ PRICE + FEATURE + DISPLAY + TPR_ONLY,
                      data=na.exclude(train.df),
                      method = "rf",
                      trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf24$bestTune

plot(varImp(modelFitrf24,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf25 <- train(UNITS ~ BASE_PRICE + FEATURE + DISPLAY + TPR_ONLY,
                      data=na.exclude(train.df),
                      method = "rf",
                      trcontrol = fitControl1,tuneLength = 3,ntree=1)
modelFitrf25$bestTune

plot(varImp(modelFitrf25,scale=F), mian = "VAR Tmp : RF  5 fold cv")

set.seed(123)
fitControl1 <- trainControl(method = "repeatedcv",
                            number = 5,
                            search = "random" , repeats = 3,savePredictions = T)
modelFitrf26 <- train(UNITS ~ PRICE + BASE_PRICE + FEATURE + DISPLAY + TPR_ONLY,
                      data=na.exclude(train.df),
                      method = "rf",
                      trcontrol = fitControl1,tuneLength = 3,ntree=1)

modelFitrf26$bestTune

plot(varImp(modelFitrf26,scale=F), mian = "VAR Tmp : RF  5 fold cv")

# Fitting a random forest
set.seed(123)
output.forest <- randomForest(UNITS ~ PRICE + BASE_PRICE + DISPLAY, data = train.df, importance=TRUE, ntree=50, replace=TRUE)
# Print the output.forest object
print(output.forest)
# Get Predict
units.pred <- predict(output.forest, test.df)
# RMSE
rmse <- sqrt(mean((as.numeric(units.pred) - test.df$UNITS)^2))
