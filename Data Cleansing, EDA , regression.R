library(tidyverse)
library(lubridate)
library(imputeTS)
library(caret)
library(dplyr)
library(gutenbergr)
library(parallel)
library(doParallel)

#--------------------------------Import Data------------------------------------

# data_og <- read.csv('vehicles.csv')
data_og <- read.csv('/Users/huiwang/Downloads/vehicles.csv')




#--------------------------Data Cleansing - Remove Outliers---------------------

str(data)
summary(data)

#Save url for future verification
# data <- data_og %>%
#   mutate(cylinders_N = as.numeric(gsub("[^0-9]","",cylinders)))

data = subset(data_og,select = -c(model,region_url,county,lat,long, VIN, 
                                  image_url, image_url, description,region))

data$posting_date <- round_date(as.Date(data$posting_date),"day")

#Remove entries with price=0 and some potential outliers
summary(data$price)
boxplot(data$price)

#Price = 0
length(which(data$price == 0))

#0 < Price < 1000
length(which(data$price > 0 & data$price <= 1000))

#100k and above, check data and confirm >350k are wrong data
tenk = data[which(data$price > 100000), ]

#Remain 500<price<350k
data = data%>%filter(data$price %in% (1000:350000))
boxplot(data$price)

#Year 2000 and prior
#The scope of the PJ to consider fair trade of a used car for commute. 
#Hence, vintage car would be excluded

length(which(data$year < 2000))
data = data%>%filter(data$year >= 2000)

#Mileage, consider 50 and above.
length(which(data$odometer < 50))
length(which(data$odometer > 300000))
data = data%>%filter(data$odometer %in% (50:300000))




#--------------------------Data Cleansing - Handling NA-------------------------

#All Entries are unique
summary(duplicated(data$id))

data[data == ""] <- NA

colSums(is.na(data))

#Apparently, some variables are not missing by random, some column have the missing rate are 
#as high as 70%

#"cutoff" for missing data is to consider to 50%, if above, del variable and 
#The "missing-at-random" assumptions needed for multiple imputation don't hold in our case, so NA can't be imputed.



#install.packages("VIM")
library(VIM)
na_plot <- aggr(data, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(data), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))

#Drop 'size' because it has over 70% na and 'type' contains similar info.
#Other columns won't be dropped even with 40% because it might have very high information about the pricing. 
###!!! In Improvement Session, we can talk about this more, for better data etc!!!

data_remove_size = subset(data,select = -c(size))
colSums(is.na(data_remove_size))



str(data)

#If del rows with any NA
data_removena = data_remove_size[complete.cases(data_remove_size), ]
colSums(is.na(data_removena))



#--------------------------------feature engineering---------------------------

#Group Brand
levels(data$manufacturer)

luxury = c('acura','alfa-romeo','aston-martin', 'audi', 'bmw','cadillac','ferrari','harley-davidson',
           'infiniti','jaguar','land rover','lexus','lincoln','mercedes-benz','mini','morgan',
           'porsche','rover','tesla','volvo')
economy = c('buick', 'chevrolet','chrysler','dodge','fiat','ford','gmc','honda','hyundai','jeep','kia',
            'mazda','mercury','mitsubishi','nissan','pontiac','ram','saturn','subaru','toyota','volkswagen')


data_removena = data_removena%>%
  mutate(brand = ifelse(data_removena$manufacturer %in% luxury, "luxury","common"))

#summary(data_removena$brand)


#-----------------------------------EDA-----------------------------------------
#Do EDA on data_removena

#Distribution of price
hist(data_removena$price) 

#Distribution among condition
data_removena$condition = factor(data_removena$condition, levels=c("new","like new","excellent","good","fair","salvage"))
ggplot(data_removena, aes(condition, fill=condition)) +
  geom_bar(stat = 'count') + 
  labs(title="Count of Condition")

#See avg price for each condition group
aggregate(data$price, list(data$condition), FUN=mean) 
aggregate(data_removena$price, list(data_removena$condition), FUN=mean) 

#See price for each condition group
library(ggplot2)
ggplot(data_removena, aes(x=condition, y=price, fill=condition)) + 
  geom_boxplot() +
  labs(title="Price of Condition")


#Avg.Price by state
library(choroplethr)
library(choroplethrMaps)
state_avgprice = data.frame(aggregate(data_removena$price, list(data_removena$state), FUN=mean))
state_avgprice
# change column name
names(state_avgprice) = c("region", "value")
state_avgprice$region
#unique(df_president$region)
state_avgprice$region = c("alaska","alabama","arkansas","arizona","california","colorado","connecticut",
                          "district of columbia","delaware","florida","georgia","hawaii","iowa","idaho",
                          "illinois","indiana","kansas","kentucky","louisiana","massachusetts","maryland",
                          "maine","michigan","minnesota","missouri","mississippi","montana","north carolina",
                          "north dakota","nebraska","new hampshire","new jersey","new mexico","nevada",
                          "new york","ohio","oklahoma","oregon","pennsylvania","rhode island","south carolina",
                          "south dakota","tennessee","texas","utah","virginia","vermont","washington",
                          "wisconsin","west virginia","wyoming") 
# create side-by-side choropleth maps
state_choropleth(state_avgprice, title  = "Avg.Price of Used Car by State in the US")


#count by manufacturer
ggplot(data_removena, aes(x=manufacturer,color=brand)) +
  geom_bar(fill="white") +
  labs(title="Count of Manufacturer")+ 
  coord_flip()


#Price by model


#Most common car
sort(table(data_removena$model), decreasing = TRUE)[1:5]
sort(table(data_removena$manufacturer), decreasing = TRUE)[1:5]
sort(table(data_removena$year), decreasing = TRUE)[1:5]
sort(table(data_removena$drive), decreasing = TRUE)[1:3]

#Which day has the most posting? Which days tend to have more expensive posting?
sort(table(data_removena$posting_date), decreasing = TRUE)[1:5]
ggplot(data_removena, aes(x=posting_date,color=brand)) +
  geom_bar(fill="white") +
  labs(title="Count of Posting_date by Brand")

#Price over Odo by brand
ggplot(data_removena, aes(x=odometer, y=price, color=brand, shape=brand)) + 
  geom_point(shape=23) +
  labs(title="Price over Odometer by Brand")

# ggplot(data_removena, aes(x=odometer, y=price, color=brand, shape=brand)) + 
#   geom_point(shape=23) + 
#   geom_smooth(method=lm)
  

#count by color
ggplot(data_removena, aes(x=paint_color,color=brand)) +
  geom_bar(fill="white") +
  labs(title="Count of Paint Color") + 
  coord_flip()

# avg.price by color
data_color_price = data.frame(aggregate(data_removena$price, list(data_removena$paint_color, data_removena$brand), FUN=mean)) 
ggplot(data_color_price, aes(x=x,y=Group.1,color=Group.2,shape=Group.2)) +
  geom_point() + 
  labs(title="Count of Paint Color")

#Might not need to do it, since we only have 2 continuous variables
#Multicollinearity 
source("http://www.sthda.com/upload/rquery_cormat.r")
























#!!!!!!!!!!!!!!!!!!!!!!!Save for Later!!!!!!!!!!!!!!!!!


#--------------------------------Data Prep-------------------------------------

#data_removena$cylinders_N = as.character(data_removena$cylinders_N)

str(data_removena)

#Scale numeric variables
data_removena$year = as.numeric(data_removena$year)

data_removena$year = scale(data_removena$year)
data_removena$odometer = scale(data_removena$odometer)

str(data_removena)

#Change categorical variables to correct data types
data_removena[sapply(data_removena, is.character)] = lapply(data_removena[sapply(data_removena, is.character)], as.factor)


set.seed(123)
split = createDataPartition(data_removena$price,p = 0.7,list = FALSE)
train = data_removena[split,]

#test set, which is considered as unseen data, 
#is saved for the best model after models' performance comparison
test =  data_removena[-split,]

#Validation set
split_again = createDataPartition(train$price,p = 0.7,list = FALSE)

train1 = train[split_again,]
validation = train[-split_again,]


#--------------------------------Modeling-------------------------------------

#Modeling on train1

# Predict price
# linear regression: Subset, Ridge, Lasso

class(train1$odometer)
head(train1)
library(corrplot)
# finding correlation between numeric columns and charges
# numeric.column <- sapply(train1, is.numeric)
# corr <- cor(train1[, numeric.column]) #, use = 'pairwise.complete.obs'
# corrplot(corr)

# train1$year = as.factor(train1$year)
hist(data_removena$price)
hist(log(data_removena$price))
summary(train1)
lmod <- lm(log(price) ~ year+manufacturer+condition+fuel+odometer+
             title_status+transmission+drive+type+paint_color+cylinders, data=train1)
summary(lmod)

# subset selection
library(leaps)
formula_lm = formula(lmod)
lmod_subset = regsubsets(formula_lm, data=train1, method="forward")
lmod_subset_summary = summary(lmod_subset)
which.min(lmod_subset_summary$bic)
which.max(lmod_subset_summary$adjr2)
par(mfrow = c(1, 2))
plot(lmod_subset_summary$bic, xlab = "Subset Size", ylab = "BIC", pch = 20, type = "l",
     main = "BIC")
points(8,lmod_subset_summary$bic[14],col="red",cex=2,pch=20)
plot(lmod_subset_summary$adjr2, xlab = "Subset Size", ylab = "Adjusted R2", pch = 20, type = "l",
     main = "Adjusted R2")
points(8,lmod_subset_summary$adjr2[19],col="red",cex=2,pch=20)
coef(lmod_subset, 8)  
# we drop manufacturer, title_status, transmission, paint_color
lmod_bic = lm(log(price)~
                year+
                condition+
                fuel+
                odometer+
                drive+
                type+
                cylinders,data=train1)
summary(lmod_bic)
lmod_pred = predict(lmod_bic, validation)
lmod_rmse = mean((lmod_pred-log(validation$price))^2) %>% sqrt()
# test rmse in original scale
lmod_rmse_ori = mean((exp(lmod_pred)-validation$price)^2) %>% sqrt()


# create model matrix for Ridge, Lasso and Random Forest
X_train = model.matrix(log(price) ~ year+manufacturer+condition+fuel+odometer+
                         title_status+transmission+drive+type+paint_color+cylinders, train1)[ ,-1] # remove intercept
Y_train = log(train1$price)
X_validation = model.matrix(log(price) ~ year+manufacturer+condition+fuel+odometer+
                         title_status+transmission+drive+type+paint_color+cylinders, validation)[ ,-1] # remove intercept
Y_validation = log(validation$price)

# Ridge
library(glmnet)
ridge_cv = cv.glmnet(X_train, Y_train, alpha = 0)
ridge_lam = ridge_cv$lambda.min
ridge_mod = glmnet(X_train, Y_train, alpha = 0, lambda = ridge_lam)
ridge_pred = predict(ridge_mod, s=ridge_lam, newx=X_validation)
ridge_rmse = mean((ridge_pred-Y_validation)^2) %>% sqrt()
ridge_rmse_ori = mean((exp(ridge_pred)-validation$price)^2) %>% sqrt()

# Lasso
lasso_cv = cv.glmnet(X_train, Y_train, alpha = 1)
lasso_lam = lasso_cv$lambda.min
lasso_mod = glmnet(X_train, Y_train, alpha = 1, lambda = lasso_lam)
coef_lasso = coef(lasso_mod) # keep all predictors, same as linear regression
lasso_pred = predict(lasso_mod, s=lasso_lam, newx=X_validation)
lasso_rmse = mean((lasso_pred-Y_validation)^2) %>% sqrt()
lasso_rmse_ori = mean((exp(lasso_pred)-validation$price)^2) %>% sqrt()


#Random Forest
install.packages("randomForest")
library(randomForest)
price.rf <- randomForest(log(price) ~ year+manufacturer+condition+fuel+odometer+
                           title_status+transmission+drive+type+paint_color+cylinders, 
                         train1,importance=TRUE)
print(price.rf)
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 3
# 
# Mean of squared residuals: 0.08321768
# % Var explained: 88.08
plot(price.rf)

rfmod_pred=predict(price.rf, newdata=validation)
rfmod_rmse = mean((rfmod_pred-Y_validation)^2) %>% sqrt()
rfmod_rmse_ori = mean((exp(rfmod_pred)-validation$price)^2) %>% sqrt()

rfmod_pred_test=predict(price.rf, newdata=test)
rfmod_rmse_test = mean((rfmod_pred_test-log(test$price))^2) %>% sqrt()
rfmod_rmse_ori_test = mean((exp(rfmod_pred_test)-test$price)^2) %>% sqrt()


##subset linear, drop manufacturer, title_status, transmission, paint_color
lmod_rmse # 0.3845
lmod_rmse_ori # 6682
##ridge, keep all predictors
ridge_rmse # 0.3607
ridge_rmse_ori # 6269
##lasso, keep all predictors
lasso_rmse # 0.3572
lasso_rmse_ori # 6308
##random forest
rfmod_rmse # 0.2872
rfmod_rmse_ori # 4505


# Adaboost


# validation
# price.rf <- randomForest(log(price) ~ year+manufacturer+condition+fuel+odometer+
#                            title_status+transmission+drive+type+paint_color+cylinders, 
#                          validation,importance=TRUE)
# print(price.rf)
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 3
# 
# Mean of squared residuals: 0.09670256    【RMSE：0.31097035228】
# % Var explained: 86.05






#### PCR and PLS are not suitable for the data!

# PCR
# set.seed (1)
# pcr.fit=pcr(log(price) ~ year+manufacturer+condition+fuel+odometer+
#               title_status+transmission+drive+type+paint_color+cylinders, 
#             data=train1, scale=TRUE, validation ="CV")
# summary (pcr.fit)
# validationplot(pcr.fit,val.type="MSEP")
# pcr.pred=predict(pcr.fit, X_validation, ncomp =25)
# sqrt(mean((as.numeric(pcr.pred)-Y_validation)^2))
# pcr.pred=predict(pcr.fit, X_validation, ncomp =18)
# mean((as.numeric(pcr.pred)-Y_validation)^2)%>% sqrt()
# pcr.pred=predict(pcr.fit, X_validation, ncomp =1) # smallest model, not so large rmse
# mean((as.numeric(pcr.pred)-Y_validation)^2)%>% sqrt()
# print("test rmse in original scale")
# mean((exp(as.numeric(pcr.pred))-validation$price)^2)%>% sqrt()
# 
# pcr.pred=predict(pcr.fit, X_validation, ncomp =2)
# mean((as.numeric(pcr.pred)-Y_validation)^2)%>% sqrt()
# 
# PLS
# 
# set.seed (1)
# pls.fit=plsr(log(shares)~.-ranks, data=df_train, scale=TRUE, validation ="CV")
# summary(pls.fit)
# pls.pred=predict(pls.fit, X_test, ncomp =15)  # smallest error
# mean((pls.pred -Y_test)^2)%>% sqrt()
# print("test rmse in original scale")
# mean((exp(pls.pred) - df_test$shares)^2)%>% sqrt()
# pls.pred=predict(pls.fit, X_test, ncomp =24)
# mean((pls.pred -Y_test)^2)%>% sqrt()

######### SVM,LDA,QDA,Logistic Regression,
######### Random forest,GBM,XGBoost,Neural network,Tree Models,Bagging,
