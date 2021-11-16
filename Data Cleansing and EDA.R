library(tidyverse)
library(lubridate)
library(imputeTS)
library(caret)
library(dplyr)
library(gutenbergr)
library(parallel)
library(doParallel)

#--------------------------------Import Data------------------------------------

data_og <- read.csv('vehicles.csv')



#--------------------------Data Cleansing - Remove Outliers---------------------

str(data)
summary(data)

#Save url for future verification
data = subset(data_og,select = -c(region_url,county,lat,long, VIN, 
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

#Mileage, consider 500k and below.


#--------------------------Data Cleansing - Handling NA-------------------------

#All Entries are unique
summary(duplicated(data$id))

data[data == ""] <- NA

colSums(is.na(data))

str(data)

#If del rows with any NA
data_removena = data[complete.cases(data), ]



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





#-----------------------------------EDA-----------------------------------------
#Do EDA on data_removena

#Chart to show insightful distribution

#Price by state

#Price by model

#Most common car

#Which day has the most posting? Which days tend to have more expensive posting?

#common vs luxury distribution?

#......





#Multicollinearity 
source("http://www.sthda.com/upload/rquery_cormat.r")
























#!!!!!!!!!!!!!!!!!!!!!!!Save for Later!!!!!!!!!!!!!!!!!


#--------------------------------Data Prep-------------------------------------



#Scale numeric variables

#Change categorical variables to correct data types
data[sapply(data, is.character)] = lapply(data[sapply(data, is.character)], as.factor)


set.seed(123)
split = createDataPartition(data$price,p = 0.7,list = FALSE)
train = data[split,]

#test set, which is considered as unseen data, 
#is saved for the best model after models' performance comparison
test =  data[-split,]

#Validation set
split_again = createDataPartition(train$Churn,p = 0.7,list = FALSE)

train1 = train[split_again,]
validation = train[-split_again,]


