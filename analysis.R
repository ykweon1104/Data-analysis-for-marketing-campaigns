---
title: "Mightyhive"
output: pdf_document
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2);library(gridExtra);library(dplyr);library(tidyverse);library(ggmap);library(ggalt);library(maps);library(caret);library(glmnet)
```
Draw charts and maps to target areas for marketing campaign
```{r}
data <- read.csv("data.csv") #Read data
data1 <- within(data, country<-factor(country,levels = names(sort(table(country),decreasing = FALSE)))) #Sort Country data
ggplot(data1)+ aes(x=country)+ geom_bar(position ="dodge",colour="orange")+ ggtitle("Top 10 countries with the highest number of users")+ ylab("Total number of users")+ scale_x_discrete(expand = c(0,0.5))+ coord_flip(xlim = c(length(unique(data1$country))-9, length(unique(data1$country)))) + theme(plot.title = element_text(hjust=0.5,colour = "Steelblue")) #Barplot for top 10 countries
 
data_usa <-data1[data1[,"country"] == "United States",]
data_usa <-data_usa[data_usa[,"city"] !="not available in demo dataset",]
data_usa <-data_usa[data_usa[,"city"] !="(not set)",] #Delete not available data in the USA
data_usa[is.na(data_usa)]<-0
data_usa1 <- within(data_usa, city<-factor(city,levels = names(sort(table(city),decreasing = FALSE)))) #set na to 0 and sort cities for plots
                   
ggplot(data_usa1)+ aes(city,fill=channelGrouping)+ geom_bar(position ="dodge")+ ggtitle("Top 10 cities of highest number of users in the USA by source type")+ ylab("Total number of users")+ scale_x_discrete(expand = c(0,0.5))+ coord_flip(xlim = c(length(unique(data_usa$city))-9, length(unique(data_usa$city)))) + theme(plot.title = element_text(hjust=0.5,colour = "Red"),axis.text.y = element_text(size=8,color = "Blue"))#Bar plot for top 10 cities in the USA group by source

data_usa1$lat <-ifelse(data_usa1$city =="Mountain View", 37.386051, ifelse(data_usa1$city == "New York",	40.730610,    ifelse(data_usa1$city =="San Francisco",37.773972, ifelse(data_usa1$city =="Sunnyvale",37.368832, ifelse(data_usa1$city =="San Jose",37.335480, ifelse(data_usa1$city =="Los Angeles",34.052235, ifelse(data_usa1$city =="Chicago",41.881832, ifelse(data_usa1$city =="Seattle",47.608013, ifelse(data_usa1$city =="Austin",30.266666, ifelse(data_usa1$city =="Santa Clara",37.773972,0)))))))))) #Add latitude to draw a map                 
data_usa1$long <-ifelse(data_usa1$city =="Mountain View", -122.083855, ifelse(data_usa1$city == "New York",	-73.935242,  ifelse(data_usa1$city =="San Francisco",-122.431297, ifelse(data_usa1$city =="Sunnyvale",-122.036346, ifelse(data_usa1$city =="San Jose",-121.893028, ifelse(data_usa1$city =="Los Angeles",-118.243683, ifelse(data_usa1$city =="Chicago",-87.623177, ifelse(data_usa1$city =="Seattle",-122.335167, ifelse(data_usa1$city =="Austin", -97.733330, ifelse(data_usa1$city =="Santa Clara",-121.955238,0))))))))))#Add longitude to draw a map
    
ggmap::register_google(key = "AIzaSyDbJlVM2aYCKNe8_YIaMNX5TWx7cZ9kvgs") ##Get Google API
myMap <- get_stamenmap(bbox = c(left = -130, bottom = 27,right = -70, top = 50), maptype = "terrain", crop = TRUE, zoom = 5) #Get USA Map
ggmap(myMap)+geom_point(aes(x = long, y = lat,size=city,color=city), data = subset(data_usa1,city %in% c("Mountain View","New York","San Jose","San Francisco","Sunnyvale","Los Angeles","Chicago","Seattle","Austin","Santa Clara")),shape=1)+geom_encircle(aes(x = -119.417931, y = 36.778259), data = data_usa1, size = 2,color="blue")+ ggtitle("Hot spots of USA users")+ theme(plot.title = element_text(hjust=0.5)) ##Draw a map for the USA
  
ggmap(get_googlemap(center = c(lon = 	-119.417931, lat =36.778259),zoom=7,maptype ='roadmap',color = 'color'))+geom_point(aes(x = long, y = lat,size=city,color=city),data = subset(data_usa1,city %in% c("Mountain View","San Jose","San Francisco","Sunnyvale","Los Angeles","Santa Clara")),shape=21)+ ggtitle("California")+ theme(plot.title = element_text(hjust=0.5)) #Focus on California
```
Create data to determine whether a user buy or not in california cities
```{r}
dat <- data_usa; dat$transactions <-ifelse(dat$transactions >=1, 1,0)#Indicate wether a customer buy or not
dat_cali <- subset(dat, city %in% c("Mountain View","San Jose","San Francisco","Sunnyvale","Los Angeles","Santa Clara"))#Select cities in California

```
Estimate probabilities by sources grouped by general rules
```{r}
dat_cali_organic <- subset(dat_cali, channelGrouping %in% "Organic Search") #Select only Organic search source type
convert <- sapply(dat_cali_organic,is.factor)
dat_cali_organic1 <- sapply(dat_cali_organic[,convert],unclass)
dat_cali_organic2 <- cbind(dat_cali_organic[,!convert],dat_cali_organic1) #Change categorical variables to numerical variables 

mod = glm(transactions~., data=dat_cali_organic2)
best_model <- step(mod,direction ="both")#get the best model

dat_cali_organic3<- dat_cali_organic %>% dplyr::select(transactions,date, bounces, hits, pageviews, timeOnSite, source, deviceCategory) #Select the best model
inTrain_cali_organic = createDataPartition(y = dat_cali_organic$transactions,p = 0.75, list = FALSE) #Create trainning set (75%)

TrainingSet_cali_organic= dat_cali_organic3[inTrain_cali_organic, ] #collect training sets
TestSet_cali_organic= dat_cali_organic3[-inTrain_cali_organic, ]#collect test sets
model.cali.organic = glm(transactions~., data=dat_cali_organic3)

ptrain_cali_organic = predict(model.cali.organic, newdata = TrainingSet_cali_organic)#Estimate the probability for the training set 
ptest_cali_organic  = predict(model.cali.organic, newdata = TestSet_cali_organic)#Estimate the probability for the test set

o_search <- mean(ptrain_cali_organic) #organic search probability, mean(ptest_cali_organic): test sets

###From this lines there will be exactly same formats for coding.Only source type would be different. So, I intentionally wrote this not that readable to reduce line 

dat_cali_Referral <- subset(dat_cali, channelGrouping %in% "Referral"); convert <- sapply(dat_cali_Referral,is.factor); dat_cali_Referral1 <-sapply(dat_cali_Referral[,convert],unclass); dat_cali_Referral2 <- cbind(dat_cali_Referral[,!convert],dat_cali_Referral1) ;dat_cali_Referral3<- dat_cali_Referral2 %>% dplyr::select(transactions,date, bounces, hits, pageviews, timeOnSite, source, deviceCategory);inTrain_cali_Referral = createDataPartition(y = dat_cali_Referral$transactions,p = 0.75,list = FALSE) ;TrainingSet_cali_Referral = dat_cali_Referral3[inTrain_cali_Referral, ]; TestSet_cali_Referral = dat_cali_Referral3[-inTrain_cali_Referral, ];model.cali.Referral = glm(transactions~., data=dat_cali_Referral3);ptest_cali_Referral  = predict(model.cali.Referral, newdata = TestSet_cali_Referral); ptrain_cali_Referral = predict(model.cali.Referral, newdata = TrainingSet_cali_Referral);referral <- mean(ptrain_cali_Referral) #cali_Referral probability, mean(ptest_cali_Referral) for test sets

dat_cali_Direct <- subset(dat_cali, channelGrouping %in% "Direct");convert <- sapply(dat_cali_Direct,is.factor); dat_cali_Direct1 <- sapply(dat_cali_Direct[,convert],unclass); dat_cali_Direct2 <- cbind(dat_cali_Direct[,!convert],dat_cali_Direct1) ;dat_cali_Direct3<- dat_cali_Direct2 %>% dplyr::select(transactions,date, bounces, hits, pageviews, timeOnSite, source, deviceCategory);inTrain_cali_Direct = createDataPartition(y = dat_cali_Direct$transactions,p = 0.75,list = FALSE);TrainingSet_cali_Direct = dat_cali_Direct3[inTrain_cali_Direct, ];TestSet_cali_Direct = dat_cali_Direct3[-inTrain_cali_Direct, ]
model.cali.Direct = glm(transactions~., data=dat_cali_Direct3);ptest_cali_Direct  = predict(model.cali.Direct, newdata = TestSet_cali_Direct);ptrain_cali_Direct = predict(model.cali.Direct, newdata = TrainingSet_cali_Direct);direct <- mean(ptrain_cali_Direct) #cali_Direct probability mean(ptest_cali_Direct) for test sets

dat_cali_Paid <- subset(dat_cali, channelGrouping %in% "Paid Search");convert <- sapply(dat_cali_Paid,is.factor); dat_cali_Paid1 <-sapply(dat_cali_Paid[,convert],unclass) ;dat_cali_Paid2 <-cbind(dat_cali_Paid[,!convert],dat_cali_Paid1);dat_cali_Paid3<- dat_cali_Paid2 %>% dplyr::select(transactions,date, bounces, hits, pageviews, timeOnSite, source, deviceCategory);inTrain_cali_Paid = createDataPartition(y = dat_cali_Paid$transactions,p = 0.75,list = FALSE);TrainingSet_cali_Paid = dat_cali_Paid3[inTrain_cali_Paid, ]; TestSet_cali_Paid = dat_cali_Paid3[-inTrain_cali_Paid, ]
model.cali.Paid = glm(transactions~., data=dat_cali_Paid3);ptest_cali_Paid  = predict(model.cali.Paid, newdata = TestSet_cali_Paid); ptrain_cali_Paid = predict(model.cali.Paid, newdata = TrainingSet_cali_Paid);paid <- mean(ptrain_cali_Paid) #cali_Paid probability 2.2%, mean(ptest_cali_Paid)

dat_cali_Social <- subset(dat_cali, channelGrouping %in% "Social");convert <- sapply(dat_cali_Social,is.factor);dat_cali_Social1 <- sapply(dat_cali_Social[,convert],unclass); dat_cali_Social2 <- cbind(dat_cali_Social[,!convert],dat_cali_Social1) ;dat_cali_Social3<- dat_cali_Social2 %>% dplyr::select(transactions,date, bounces, hits, pageviews, timeOnSite, source, deviceCategory);inTrain_cali_Social = createDataPartition(y = dat_cali_Social$transactions,p = 0.75,list = FALSE);TrainingSet_cali_Social = dat_cali_Social3[inTrain_cali_Social, ];TestSet_cali_Social = dat_cali_Social3[-inTrain_cali_Social, ];model.cali.Social = glm(transactions~., data=dat_cali_Social3); ptest_cali_Social  = predict(model.cali.Social, newdata = TestSet_cali_Social);ptrain_cali_Social = predict(model.cali.Social, newdata = TrainingSet_cali_Social);social<-mean(ptrain_cali_Social) 

dat_cali_Display <- subset(dat_cali, channelGrouping %in% "Display");convert <- sapply(dat_cali_Display,is.factor);dat_cali_Display1 <- sapply(dat_cali_Display[,convert],unclass); dat_cali_Display2 <- cbind(dat_cali_Display[,!convert],dat_cali_Display1) ;dat_cali_Display3<- dat_cali_Display2 %>% dplyr::select(transactions,date, bounces, hits, pageviews, timeOnSite, source, deviceCategory);inTrain_cali_Display = createDataPartition(y = dat_cali_Display$transactions,p = 0.75,list = FALSE);TrainingSet_cali_Display = dat_cali_Display3[inTrain_cali_Display, ];TestSet_cali_Display = dat_cali_Display3[-inTrain_cali_Display, ];model.cali.Display = glm(transactions~., data=dat_cali_Display3); ptest_cali_Display  = predict(model.cali.Display, newdata = TestSet_cali_Display);ptrain_cali_Display = predict(model.cali.Display, newdata = TrainingSet_cali_Display);Display<-mean(ptrain_cali_Display)

df <- data.frame(Source = rep(c("Organic search", "Referral", "Direct" ,"Paid Search","Social","Display","Affiliates")), Probability = c(o_search, referral, direct, paid, social,Display,0))#Make a data frame to draw a barchart of probabilities

ggplot(df, aes(x = reorder(Source,-Probability), y = Probability))+ geom_bar(stat ="identity")+labs(x="Source type")+ggtitle("Probability that a user will buy by source type in California")+ theme(plot.title = element_text(hjust=0.5,colour = "Blue"))###Draw a barchart of probabilities in California by source type

```

