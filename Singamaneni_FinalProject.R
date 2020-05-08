
Title: "BigData_FinalProject"
author: "Anusha singamaneni"
date: "5/2/2020"


## Loading Data
require(dplyr)
# Load the dataset and run summary()
health.data = read.csv("Big_Cities_Health_Data_Inventory.csv")
summary (health.data)

# Extracting and Refining Students data

require(dplyr)

# Drinking data for High School Students
drink.st <- health.data %>%
  filter(Indicator == "Percent of High School Students Who Binge Drank")

# Smoking data for Students
smoke.st <- health.data %>%
  filter(Indicator == "Percent of High School Students Who Currently Smoke")

# Remove unwanted variables and rename some columns

drink.st <- drink.st[c(3:7)]
drink.st$Ethnicity <- drink.st$Race..Ethnicity
drink.st <- drink.st[-c(3)]

smoke.st <- smoke.st[c(3:7)]
smoke.st$Ethnicity <- smoke.st$Race..Ethnicity
smoke.st <- smoke.st[-c(3)]


# Drinking data for Adults
drink.ad <- health.data %>%
  filter(Indicator == "Percent of Adults Who Binge Drank")

# Smoking data for Adults
smoke.ad <- health.data %>%
  filter(Indicator == "Percent of Adults Who Currently Smoke")

# Remove unwanted variables and rename some columns
drink.ad <- drink.ad[c(3:7)]
drink.ad$Ethnicity <- drink.ad$Race..Ethnicity
drink.ad <- drink.ad[-c(3)]

smoke.ad <- smoke.ad[c(3:7)]
smoke.ad$Ethnicity <- smoke.ad$Race..Ethnicity
smoke.ad <- smoke.ad[-c(3)]

# Remove missing data which has been found only in smoke.ad dataset
drink.st <- drink.st %>%
  filter(Value != "NA")
smoke.st <- smoke.st %>%
  filter(Value != "NA")
drink.ad <- drink.ad %>%
  filter(Value != "NA")
smoke.ad <- smoke.ad %>%
  filter(Value != "NA")



# Drinking data for Students
mean(drink.st$Value)
# The average is 14.85%

median(drink.st$Value)
# The median value is 12.7

sd(drink.st$Value)
# The standard deviation is 9.09

hist(drink.st$Value)
#Based on the graph, it seems to be often 40 drinks high school students binge on.

barplot(drink.st$Value)

#pie(drink.st$Value, cex = 0.5)

# Smoking data for Students

mean(smoke.st$Value) 
# The average is 10.89 %

median(smoke.st$Value) 
# The middle value is 10

sd(smoke.st$Value) 
# The standard deviation is 4.69

hist(smoke.st$Value) 
# Based on the histogram, it seems that the students who currently smoke do smoke more than 60 cigarettes often.

barplot(smoke.st$Value)

#pie(smoke.st$Value, cex = 0.5)

# Drinking data for Adults

mean(drink.ad$Value)
#The average was 21.81%

median(drink.ad$Value) 
# The middle value is 19.7

sd(drink.ad$Value) 
# The standard deviation is 10.75

hist(drink.ad$Value) 
#Shown from the histogram, the adults frequently consume more than 120 drinks.
barplot(drink.ad$Value) 

#pie(drink.ad$Value, cex = 0.5)

# Smoking data for Adults

mean(smoke.ad$Value) 
# The average was 17.61%

median(smoke.ad$Value)
# The median value is 17.4

sd(smoke.ad$Value) 
# The standard deviation is 5.53

hist(smoke.ad$Value) 
# Adults smoke frequently more than 80 cigarettes.

barplot(smoke.ad$Value) 

#pie(smoke.ad$Value, cex = 0.8)

library(tidyverse)
library(ggplot2)


New_data <- health.data[c(3:7)]
New_data
Data_MF<-New_data %>%
  filter(Year %in%  c(2012, 2013,2014))%>%
  filter(Gender!= "Both")%>%
  filter(Place %in% c("Atlanta (Fulton County), GA" , "Baltimore, MD"))
#Data_MF

ggplot(Data_MF, aes(Place ,Year, color = Gender)) +geom_line()+ geom_point()


Data_Ind <- health.data %>%
  group_by(Indicator.Category , Year)%>%
  
  filter(Place == "Atlanta (Fulton County), GA") %>%
  filter(Indicator.Category ==  "HIV/AIDS")%>%
  filter(Indicator.Category %in% c("Cancer", "HIV/AIDS")) 
#Data_Ind

ggplot(Data_Ind, aes(Year ,Indicator.Category,  color= Gender))+ geom_point()+geom_line()


#Analysis on Drinking data for high school students

ggplot(drink.st, aes(Year, Ethnicity, color = Gender))+geom_boxplot()+geom_point()

#Performed linear regression model for Drinking and smoking data for high school students

# Students data who Drink
lm.fitg=lm(Value~Gender,data=drink.st) 
summary(lm.fitg)

par(mfrow = c(2, 2))
plot(lm.fitg)

# High School students who smoke
ggplot(smoke.st, aes(Year, Place , color = Gender))+geom_boxplot()+geom_point()

#Fit on data 
lm.fitg=lm(Value~Gender,data=smoke.st)
summary(lm.fitg)

par(mfrow = c(2, 2))
plot(lm.fitg)

# Multiple Linear Regression

#Using Ethnicity , Place and Gender as Independent Variables on High school students drink data.
# Drink data

lm.fit=lm(Value~.,data=drink.st) 
summary(lm.fit)
par(mfrow = c(2, 2))
plot(lm.fit)
HighLeverage <- cooks.distance(lm.fit) > (4/nrow(drink.st))

drink.st <- drink.st[!HighLeverage,]
lm.fit=lm(Value~.,data=drink.st) 
summary(lm.fit)
par(mfrow = c(2, 2))
plot(lm.fit)

#Smoke data:

ggplot(smoke.st, aes(Place, Ethnicity, color = Gender))+geom_boxplot()+geom_point()

## Performing Multiple regression on High school students Smoke data.

#Smoke Data

lm.fitall=lm(Value~.,data=smoke.st) 
summary(lm.fitall)
par(mfrow = c(2, 2))
plot(lm.fitall)

HighLeverage <- cooks.distance(lm.fitall) > (4/nrow(smoke.st))

smoke.st <- smoke.st[!HighLeverage,]
lm.fitall=lm(Value~.,data=smoke.st) 
summary(lm.fitall)
par(mfrow = c(2, 2))
plot(lm.fitall)

#  Adults drinking data
ggplot(drink.ad , aes(Ethnicity , Place , color = Gender))+geom_boxplot()+geom_point()
## Multiple Regression analysis on Adults drinking data
# Similarly run the fit on Adults data
lm.fitAd=lm(Value~Gender,data=drink.ad) 
summary(lm.fitAd)

lm.fitAd=lm(Value~.,data=drink.ad) 
summary(lm.fitAd)
par(mfrow = c(2, 2))
plot(lm.fitAd)


HighLeverage <- cooks.distance(lm.fitAd) > (4/nrow(drink.ad))

drink.ad <- drink.ad[!HighLeverage,]
lm.fitAd=lm(Value~.,data=drink.ad) 
summary(lm.fitAd)
par(mfrow = c(2, 2))
plot(lm.fitAd)

#Adults smoke data
ggplot(smoke.ad, aes(Ethnicity, Place, color = Gender))+geom_boxplot()+geom_point()

# Multiple Regression analysis on Adults smoking data
#Adults who smoke
lm.fitall=lm(Value~.,data=smoke.ad) 
summary(lm.fitall)
par(mfrow = c(2, 2))
plot(lm.fitall)

HighLeverage <- cooks.distance(lm.fitall) > (4/nrow(smoke.ad))

smoke.ad <- smoke.ad[!HighLeverage,]
lm.fitall=lm(Value~.,data=smoke.ad) 
summary(lm.fitall)
par(mfrow = c(2, 2))
plot(lm.fitall)


#Basic Regression Trees:

require(tree)

#a) Drinking data for Students

set.seed(1) 

## Create the Training dataset

train = sample(1:nrow(drink.st), nrow(drink.st)/2)
tree.drink.st=tree(Value~.,drink.st,subset=train)
summary(tree.drink.st)

plot(tree.drink.st)
text(tree.drink.st,pretty=0, cex = 0.6)
tree.drink.st

# will prune the tree now
cv.drink.st=cv.tree(tree.drink.st)
plot(cv.drink.st$size,cv.drink.st$dev,type='b') 

prune.drink.st=prune.tree(tree.drink.st,best=5) 
plot(prune.drink.st) 
text(prune.drink.st,pretty=0, cex = 0.6) 
prune.drink.st

# Making Predictions on test data

yhat=predict(tree.drink.st,newdata=drink.st[-train,]) 
drink.st.test = drink.st[-train,"Value"] 
plot(yhat,drink.st.test)
abline(0,1)
mean((yhat-drink.st.test)^2) 

# Boosting
require(gbm)
set.seed(1)
boost.drink.st=gbm(Value~.,data=drink.st[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)

summary(boost.drink.st)

par(mfrow=c(1,2)) 
plot(boost.drink.st,i="Place", type = "l")
plot(boost.drink.st,i="Ethnicity", type = "l")

#b) Smoking data for Students   

set.seed(1) 

## Create the Training dataset

train = sample(1:nrow(smoke.st), nrow(smoke.st)/2)
tree.smoke.st=tree(Value~.,smoke.st,subset=train)
summary(tree.smoke.st)

plot(tree.smoke.st)
text(tree.smoke.st,pretty=0, cex = 0.6)
tree.smoke.st


# Pruning

cv.smoke.st=cv.tree(tree.smoke.st)
plot(cv.smoke.st$size,cv.smoke.st$dev,type='b') 

prune.smoke.st=prune.tree(tree.smoke.st,best=7) 
plot(prune.smoke.st) 
text(prune.smoke.st,pretty=0, cex = 0.6) 
prune.smoke.st

# Making Predictions on test data

yhat=predict(tree.smoke.st,newdata=smoke.st[-train,]) 
smoke.st.test = smoke.st[-train,"Value"] 
plot(yhat,smoke.st.test)
abline(0,1)
mean((yhat-smoke.st.test)^2) 

# Boosting
require(gbm)
set.seed(1)
boost.smoke.st=gbm(Value~.,data=smoke.st[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)

summary(boost.smoke.st)

par(mfrow=c(1,2)) 
plot(boost.smoke.st,i="Place", type = "l") 
plot(boost.smoke.st,i="Ethnicity", type = "l")

#c) Drinking data for Adults

set.seed(1) 

## Create the Training dataset

train = sample(1:nrow(drink.ad), nrow(drink.ad)/2)
tree.drink.ad=tree(Value~.,drink.ad,subset=train)
summary(tree.drink.ad)

plot(tree.drink.ad)
text(tree.drink.ad,pretty=0, cex = 0.6)
tree.drink.ad

# Pruning

cv.drink.ad=cv.tree(tree.drink.ad)
plot(cv.drink.ad$size,cv.drink.ad$dev,type='b') 

prune.drink.ad=prune.tree(tree.drink.ad,best=8) 
plot(prune.drink.ad) 
text(prune.drink.ad,pretty=0, cex = 0.6) 
prune.drink.ad

# Making Predictions on test data

yhat=predict(tree.drink.ad,newdata=drink.ad[-train,]) 
drink.ad.test = drink.ad[-train,"Value"] 
plot(yhat,drink.ad.test)
abline(0,1)

mean((yhat-drink.ad.test)^2) 

#d) Smoking data for Adults

set.seed(1) 

## Create the Training dataset

train = sample(1:nrow(smoke.ad), nrow(smoke.ad)/2)
tree.smoke.ad=tree(Value~.,smoke.ad,subset=train)
summary(tree.smoke.ad)

plot(tree.smoke.ad)
text(tree.smoke.ad,pretty=0, cex = 0.6)
tree.smoke.ad

## Pruning

cv.smoke.ad=cv.tree(tree.smoke.ad)
plot(cv.smoke.ad$size,cv.smoke.ad$dev,type='b') 

prune.smoke.ad=prune.tree(tree.smoke.ad,best=8) 
plot(prune.smoke.ad) 
text(prune.smoke.ad,pretty=0, cex = 0.6) 
prune.smoke.ad

# Making Predictions on test data

yhat=predict(tree.smoke.ad,newdata=smoke.ad[-train,]) 
smoke.ad.test = smoke.ad[-train,"Value"] 
plot(yhat,smoke.ad.test)
abline(0,1)

mean((yhat-smoke.ad.test)^2) 

# Boosting

require(gbm)
set.seed(1)
boost.smoke.ad=gbm(Value~.,data=smoke.ad[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)

summary(boost.smoke.ad)

par(mfrow=c(1,2)) 
plot(boost.smoke.ad,i="Place", type = "l") 
plot(boost.smoke.ad,i="Ethnicity", type = "l")

