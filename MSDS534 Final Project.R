library(tidyverse)
library(lubridate)
library(imputeTS)
library(caret)
library(dplyr)
library(gutenbergr)
library(parallel)
library(doParallel)
library(tree)
library(gbm)

#--------------------------------Import Data------------------------------------

data_og <- read.csv('/Users/huiwang/Downloads/vehicles.csv')

#--------------------------Data Cleansing - Remove Outliers---------------------

str(data_og)
summary(data_og)

#Save url for future verification
data = subset(data_og,select = -c(url,model,region_url,county,lat,long, VIN, 
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

data_removena = data_removena %>%
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


#Might not need to do it, since we only have 2 continuous variables
#Multicollinearity 
source("http://www.sthda.com/upload/rquery_cormat.r")



#-------------------------Data Prep for Price Prediction------------------------

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


#-------------------------Price Prediction Modeling-----------------------------
#Modeling on train1

# Predict price
# linear regression, Subset, Ridge, Lasso, Random Forest, Boosting

class(train1$odometer)
head(train1)
hist(data_removena$price)
hist(log(data_removena$price))
summary(train1)
lmod <- lm(log(price) ~ year+manufacturer+condition+fuel+odometer+
             title_status+transmission+drive+type+paint_color+cylinders, data=train1)
summary(lmod)

#coef(lmod)

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


# create model matrix for Ridge, Lasso, Random Forest and Boosting
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


# Random Forest
# install.packages("randomForest")
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


# Boosting

gbm.fit <- gbm(
  formula = log(price) ~ year+manufacturer+condition+fuel+odometer+
    title_status+transmission+drive+type+paint_color+cylinders,
  distribution = "gaussian",
  data = train1,
  n.trees = 500,
  interaction.depth = 3,
  shrinkage = 0.5,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)

# print results
print(gbm.fit)

gbm_pred = predict(gbm.fit, newdata=validation)
gbm_rmse = mean((gbm_pred-Y_validation)^2) %>% sqrt()
gbm_rmse_ori = mean((exp(gbm_pred)-validation$price)^2) %>% sqrt()


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
##boosting
gbm_rmse # 0.3172
gbm_rmse_ori # 5053

# random forest is the best, so apply it on test set
rfmod_pred_test=predict(price.rf, newdata=test)
rfmod_rmse_test = mean((rfmod_pred_test-log(test$price))^2) %>% sqrt()
rfmod_rmse_ori_test = mean((exp(rfmod_pred_test)-test$price)^2) %>% sqrt()

importance(price.rf, type = 1)

varImpPlot(price.rf, main="Variable Importance Plots of Random Forest in Regression")



#---------------------Data Prep for Brand Classification------------------------

#Scale numeric variables price
data_removena$price = scale(data_removena$price)

#remove not needed fields
str(data_removena)
data_removena = data_removena[,-c(1, 4, 14)]

set.seed(123)
split_brand = createDataPartition(data_removena$brand,p = 0.7,list = FALSE)
train_brand = data_removena[split_brand,]

#test set, which is considered as unseen data, 
#is saved for the best model after models' performance comparison
test_brand =  data_removena[-split_brand,]

#Validation set
split_again_brand = createDataPartition(train_brand$brand,p = 0.7,list = FALSE)

train1_brand = train_brand[split_again_brand,]
validation_brand = train_brand[-split_again_brand,]



#--------------------Brand Classification Modeling------------------------------

#Modeling on train1_brand


#Need of oversampling for unbalanced

ggplot(train1_brand, aes(brand, fill = brand))+geom_bar()

library(unbalanced)
levels(train1_brand$brand) <- c(0,1)
levels(test_brand$brand) <- c(0,1)
levels(validation_brand$brand) <- c(0,1)
input <- train1_brand[,1:12]
response <- train1_brand$brand
balance <- ubOver(X=input, Y=response) 
new_train <- cbind(balance$X, balance$Y)
colnames(new_train)[13] <- "brand"

ggplot(new_train, aes(brand, fill = brand))+geom_bar()




#----------------------Modeling (Logistic Regression)---------------------------
set.seed(123)
# define training control 10 fold cross validation
train_control <- trainControl(method = "cv", number = 10, allowParallel = FALSE)
# train the model on training set using logistic Regression
model_logit <- caret::train(brand ~ .,data = new_train,
                            trControl = train_control,
                            method = "glm",
                            family=binomial())
summary(model_logit)

print(model_logit)
#training acc: 0.7758135  


#Use the logit model to make prediction 
predict_logit <- predict(model_logit, newdata = validation_brand)

#Generate Confusion Matrix and F1 Score.
result_logit <- confusionMatrix(data = predict_logit, reference = validation_brand$brand, mode = "prec_recall")

F1_logit <- result_logit$byClass[7]
result_logit #Validation Acc : 0.7404
F1_logit #F1 0.824902 



#----------------------Modeling (LDA)-------------------------------------------

set.seed(123)
model_lda <- caret::train(brand ~ .,data = new_train, method = "lda",
                          trControl=train_control,
                          verbose = TRUE)
model_lda
#training acc: 0.7708944  

predict_lda <- predict(model_lda, newdata = validation_brand)

result_lda <- confusionMatrix(data = predict_lda, reference = validation_brand$brand, mode = "prec_recall")
F1_lda <- result_lda$byClass[7]
result_lda # Validation Acc : 0.7269          
F1_lda #F1 0.8139829


#----------------------Modeling (SVM Linear)------------------------------------
# Run time is too long and result is not even that good
set.seed(123)

cluster <- makeCluster(detectCores() - 2)
registerDoParallel(cluster)

train_control_parallel <- trainControl(method = "cv", number = 3, allowParallel = TRUE)

model_svm <- caret::train(brand ~ .,data = new_train, method = "svmLinear",
                          trControl=train_control_parallel,
                          tuneGrid = expand.grid(C = c(0.1, 1,5)),
                          verbose = TRUE)

stopCluster(cluster)

predict_svm <- predict(model_svm, newdata = validation_brand)

result_svm <- confusionMatrix(data = predict_svm, reference = validation_brand$brand, mode = "prec_recall")
F1_svm <- result_svm$byClass[7]
result_svm
F1_svm

#cv takes so long, or just use below without cv
#library(e1071)
#set.seed (123)

#cluster <- makeCluster(detectCores() - 2)
#registerDoParallel(cluster)

#svm_tune1 = tune(svm, brand~.,data=new_train,kernel="linear",
                # ranges=list(cost=c(0.1, 1,5)))

#stopCluster(cluster)




#----------------------Modeling (SVM Radial)------------------------------------
set.seed(123)

cluster <- makeCluster(detectCores() - 2)
registerDoParallel(cluster)

train_control_parallel <- trainControl(method = "cv", number = 3, allowParallel = TRUE)

model_svm_Rad <- caret::train(brand ~ .,data = new_train, method = "svmRadial",
                              trControl=train_control_parallel,
                              tuneLength = 2,
                              verbose = TRUE)

stopCluster(cluster)

#sigma 0.007345854, C 0.5
model_svm_Rad$bestTune

predict_svm_Rad <- predict(model_svm_Rad, newdata = validation_brand)

result_svm_Rad <- confusionMatrix(data = predict_svm_Rad, reference = validation_brand$brand, mode = "prec_recall")
F1_svm_Rad <- result_svm_Rad$byClass[7]
result_svm_Rad
#0.8633746
F1_svm_Rad



#----------------------Modeling (Random Forest)---------------------------------
set.seed(123)

cluster <- makeCluster(detectCores() - 2)
registerDoParallel(cluster)
train_control_parallel <- trainControl(method = "cv", number = 3, allowParallel = TRUE)

model_rf <- caret::train(brand ~ .,data = new_train, method = "rf",
                         trControl=train_control_parallel,
                         tuneGrid = expand.grid(.mtry = c(5,10,12)),
                         metric="Kappa",
                         verbose = TRUE)

stopCluster(cluster)

#  mtry  Accuracy   Kappa    
#5    0.8843265  0.7686530
#10    0.9433203  0.8866407
#15    0.9601255  0.9202511
model_rf #use mtry=15

predict_rf <- predict(model_rf, newdata = validation_brand)
result_rf <- confusionMatrix(data = predict_rf, reference = validation_brand$brand, mode = "prec_recall")
F1_rf <- result_rf$byClass[7]
result_rf
F1_rf #0.9497753


#---------Use randomForest to tune tree and # of variable split

set.seed(123)
#Variable Importance Plot
library(randomForest)

#Choose mtry = 12 based on above
model_rf1 <- randomForest(brand ~ .,data = new_train,mtry =12, importance = TRUE)
print(model_rf1)

pred_rf1 <- predict(model_rf1, validation_brand)

result_rf1 = caret::confusionMatrix(pred_rf1, validation_brand$brand, mode = "prec_recall")

#0.946374
result_rf1$byClass[7]

#Find optimal tree size, 100 seems sufficient
plot(model_rf1,main="Error as ntree increases")

set.seed(123)

#Final Model for RF
model_rf2 <- randomForest(brand ~ .,data = new_train,mtry =12, ntree = 100, importance = TRUE)
print(model_rf2)


pred_rf2 <- predict(model_rf2, validation_brand)

result_rf2 = caret::confusionMatrix(pred_rf2, validation_brand$brand, mode = "prec_recall")

#0.9462329
result_rf2$byClass[7]

importance(model_rf2, type = 1)

varImpPlot(model_rf2, main="Variable Importance Plots")


#-------------------Final Model on test set (best f1 score)---------------------
pred_rf_test <- predict(model_rf2, test_brand)

result_rf_test = caret::confusionMatrix(pred_rf_test, test_brand$brand, mode = "prec_recall")

#0.9479709 F1
result_rf_test$byClass[7]

result_rf_test




















