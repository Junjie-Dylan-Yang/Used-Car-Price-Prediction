library(tidyverse)
library(lubridate)
library(imputeTS)
library(caret)
library(dplyr)
library(gutenbergr)
library(parallel)
library(doParallel)

#--------------------------------Import Data------------------------------------

data_og <- read.csv('/Users/huiwang/Downloads/vehicles.csv')



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

summary(data_removena$brand)


#-----------------------------------EDA-----------------------------------------
#Do EDA on data_removena

#See price for each condition group
library(ggplot2)
ggplot(data_removena, aes(x=condition, y=price, fill=condition)) + 
  geom_boxplot() +
  labs(title="Price of Condition")

#not working
#aggregate(data$price, list(data$condition), FUN=mean)
# condition_price = data.frame(aggregate(data_removena$price, list(data_removena$condition), FUN=mean))
# condition_price$x = round(condition_price$x, digits = 0)
# names(condition_price) = c("condition", "avg.price")
# ggplot(condition_price, aes(x=condition, y=avg.price, )) +
#   geom_point(size=2, shape=23) + 
#   geom_text(label=condition_price$avg.price) +
#   labs(title="Avg. Price of Condition", x="Condition", y="Avg.price")


#Chart to show insightful distribution



#Avg.Price by state
library(choroplethr)
library(choroplethrMaps)
state_avgprice = data.frame(aggregate(data_removena$price, list(data_removena$state), FUN=mean))
state_avgprice
# change column name
names(state_avgprice) = c("region", "value")
state_avgprice$region
unique(df_president$region)
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
  labs(title="Count of Manufacturer")


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


#common vs luxury distribution?

#......





#Multicollinearity 
source("http://www.sthda.com/upload/rquery_cormat.r")
























#!!!!!!!!!!!!!!!!!!!!!!!Save for Later!!!!!!!!!!!!!!!!!


#--------------------------------Data Prep-------------------------------------



#Scale numeric variables

#Change categorical variables to correct data types
data_removena[sapply(data_removena, is.character)] = lapply(data_removena[sapply(data_removena, is.character)], as.factor)


set.seed(123)
split = createDataPartition(data_removena$price,p = 0.7,list = FALSE)
train = data_removena[split,]

#test set, which is considered as unseen data, 
#is saved for the best model after models' performance comparison
test =  data_removena[-split,]

#Validation set
split_again = createDataPartition(train$Churn,p = 0.7,list = FALSE)

train1 = train[split_again,]
validation = train[-split_again,]


