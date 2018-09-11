
#------------------------------Preparing the environment for MLRM---------------------------------------#

library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)


#--------------------------------Setting the Working Directory-----------------------------------------#
Path<-"C:/Users/Ravi/Desktop/shubham data/R/MLRM case study 2"
setwd(Path)
getwd()

data=read.csv("elantra.csv")
data1=data #To create a backup of original data


#------------------------------------Basic Exploration of the data--------------------------------------------# 
str(data1)
summary(data1)
dim(data1)



# -----------------renaming the dependent variable----------------------------

colnames(data1) [which(names(data) == "ElantraSales")] <- "sales"



#===================detecting Outliers==========================

boxplot(data1$sales) # found no outliers




#------------- checking for the missing values------ 
as.data.frame(colSums(is.na(data1)))


#--------------------------Splitting the data into training and test data set------------------------#

train <- data1[data1$Year<= 2012,]
test<- data1[data1$Year > 2012,]
dim(train)
dim(test)
str(train)
summary(train)
summary(test)

#------------------------------------------Bulding the model---------------------------------------#
#Iteration.1 We start with testing all variables

MOdel1 <- lm(sales~., train)
summary(MOdel1)


Model2 <- lm(sales~ Unemployment + Queries+CPI_energy +  CPI_all , train)
summary(Model2)

Model3 <- lm(sales~ Unemployment + Queries , train)
summary(Model3)

#R squared is very low which indicates that our model is not good
# converting month variable to factor

data1$Month <- as.factor(data1$Month)
str(data1)

train <- data1[data1$Year<= 2012,]
test<- data1[data1$Year > 2012,]
str(train)
summary(train)
summary(test)

#=============Building model=================

#Iteration1

MOdel0 <- lm(sales ~ ., train)
summary(MOdel0)

#Iteration 2

MOdel1 <- lm(sales ~ Month + Unemployment + CPI_energy +  CPI_all , train)
summary(MOdel1)

#Iteration 3

MOdel2 <- lm(sales ~ I(Month == "3") +I(Month == "4") +I(Month == "5") +I(Month == "6") + I(Month == "7") + I(Month == "8")
                      + I(Month == "9") + I(Month == "12") + Unemployment + CPI_energy +  CPI_all , train)
summary(MOdel2)



vif(MOdel2)


#Iteration 4

MOdel3 <- lm(sales ~ I(Month == "3") +I(Month == "4") +I(Month == "5") +I(Month == "6") + I(Month == "7") + I(Month == "8")
               + CPI_energy  , train)
summary(MOdel3)


#variation inflation factor
vif(MOdel3)




# getting fitted values

fitted(MOdel3)

par(mfrow=c(2,2))
plot(MOdel3)


## MAPE
train$pred <- fitted(MOdel3)
write.csv(original.data,"mape.csv")


#Calculating MAPE
attach(train)
MAPE<-print((sum((abs(sales-pred))/sales))/nrow(train))

#getting predicted values
prediction <- predict(MOdel3, newdata = test)

#sum of squared errors
SSE = sum((prediction - test$sales)^2)
SSE



#====================Fitting model on test dataset===================


Fit1 <- lm(sales ~ I(Month == "3") +I(Month == "4") +I(Month == "5") +I(Month == "6") + I(Month == "7") + I(Month == "8")
           + CPI_energy  , test)
summary(Fit1)

Fit2 <- lm(sales ~ I(Month == "3") +I(Month == "4") +I(Month == "5")  + I(Month == "7") + I(Month == "8")
            , test)
summary(Fit2)

# maximum absolute error
max(abs(predictions - test$sales))

#period having largest absolute error
which.max(abs(predictions - test$sales))

test[5,]  # month 3 and year 2013

