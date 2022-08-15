# 10 December 2020 
# Student Name: Tom Meehan
# Student ID: 18220975

setwd("C:/Users/meeha/OneDrive/College/Statistics/Project")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(car)

# Question 1 - Load the pima Indians dataset into R

data = read.csv(file = "MA6101data.csv",header = TRUE)

#Question 2 - Set the random number seed in R as your UL student number

set.seed(18220975)

#Question 3 - Extract a random sample of 320 rows

mydata = sample_n(data,320)

#Question 4 - Explore the mydata dataset

#a) Summarise each variable and provide appropriate statistics and confidence intervals

# Summarises the entire data set
summary(mydata)

# Summarises Pregnancies 
mean(mydata$pregnant)
median(mydata$pregnant)
var(mydata$pregnant)
quantile(mydata$pregnant,c(0.25,0.75))
hist(mydata[ ,"pregnant"])

#Pregnancy 95% confidence intervals

s1 <- sqrt(var(mydata[,2]))
x_bar1 <- mean(mydata[,2])
x_bar1+c(-1.96,1.96)*s1/sqrt(320)

#Summarises glucose levels
mean(mydata$glucose)
median(mydata$glucose)
var(mydata$glucose)
quantile(mydata$glucose,c(0.25,0.75))
hist(mydata[ ,"glucose"])

#Glucose 95% confidence intervals

s2 <- sqrt(var(mydata[,3]))
x_bar2 <- mean(mydata[,3])
x_bar2+c(-1.96,1.96)*s2/sqrt(320)

#Summarises blood pressure
mean(mydata$pressure)
median(mydata$pressure)
var(mydata$pressure)
quantile(mydata$pressure,c(0.25,0.75))
hist(mydata[ ,"pressure"])

#Blood Pressure 95% confidence intervals

s3 <- sqrt(var(mydata[,4]))
x_bar3 <- mean(mydata[,4])
x_bar3+c(-1.96,1.96)*s3/sqrt(320)

#Summarises triceps
mean(mydata$triceps)
median(mydata$triceps)
var(mydata$triceps)
quantile(mydata$triceps,c(0.25,0.75))
hist(mydata[ ,"triceps"])

#Triceps 95% confidence intervals

s4 <- sqrt(var(mydata[,5]))
x_bar4 <- mean(mydata[,5])
x_bar4+c(-1.96,1.96)*s5/sqrt(320)

#Summarises Insulin level
mean(mydata$insulin)
median(mydata$insulin)
var(mydata$insulin)
quantile(mydata$insulin,c(0.25,0.75))
hist(mydata[ ,"insulin"])

#Insulin 95% confidence intervals

s5 <- sqrt(var(mydata[,6]))
x_bar5 <- mean(mydata[,6])
x_bar5+c(-1.96,1.96)*s5/sqrt(320)

#Summarises BMI
mean(mydata$mass)
median(mydata$mass)
var(mydata$mass)
quantile(mydata$mass,c(0.25,0.75))
hist(mydata[ ,"mass"])

#Mass 95% confidence intervals

s6 <- sqrt(var(mydata[,7]))
x_bar6 <- mean(mydata[,7])
x_bar6+c(-1.96,1.96)*s6/sqrt(320)

#Summarises age
mean(mydata$age)
median(mydata$age)
var(mydata$age)
quantile(mydata$age,c(0.25,0.75))
hist(mydata[ ,"age"])

#Age function 95% confidence intervals

s7 <- sqrt(var(mydata[,8]))
x_bar7 <- mean(mydata[,8])
x_bar7+c(-1.96,1.96)*s7/sqrt(320)

#Summarises diabetes

# Create a function for finding the mode.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

summary(mydata$diabetes)
getmode(mydata$diabetes)


#Summarises risk
mean(mydata$Risk)
median(mydata$Risk)
var(mydata$Risk)
quantile(mydata$Risk,c(0.25,0.75))
hist(mydata[ ,"Risk"])

#Risk 95% confidence intervals

s9 <- sqrt(var(mydata[,10]))
x_bar9 <- mean(mydata[,10])
x_bar9+c(-1.96,1.96)*s9/sqrt(320)

#b) 
#Compare the distribution for people with and without diabetes for the following variables:
#glucose
#pressure
#insulin

#Comparing Glucose and diabetes

ggplot(aes(y=glucose,x = diabetes),data=mydata) + geom_boxplot()+ylab("Glucose")

#Comparing pressure and diabetes

ggplot(aes(y=pressure,x = diabetes),data=mydata) + geom_boxplot()+ylab("Pressure")

#Comparing Insulin and diabetes

ggplot(aes(y=insulin,x = diabetes),data=mydata) + geom_boxplot()+ylab("Insulin")

#C) Test the significance of the mean glucose for those with and without diabetes

# Creates data for all negative and positive results.
mydata_neg<-mydata[mydata$diabetes == "neg",]
mydata_pos<-mydata[mydata$diabetes == "pos",]

# H0 : m1 - m2 = 0, Ha : m1 - m2 != 0
t.test(mydata_neg$glucose, mydata_pos$glucose,
       alternative = c("two.sided"),
       var.equal=TRUE,
       mu = 0)

#D) Use scatter plots and correlation to predict risk score.

# Risk V Pregnancy
model1 <- lm(Risk ~pregnant,data = mydata)
summary(model1)

plot(model1)
hist(model1$residuals)


plot(mydata$Risk,mydata$pregnant)
abline(model1,col="blue")

#Predictions

predict(model1, newdata = list(pregnant = 3), interval = "prediction",level=0.95)

# Risk V Glucose
model2 <- lm(Risk ~glucose,data = mydata)
summary(model2)

plot(model2)
hist(model2$residuals)


plot(mydata$Risk,mydata$glucose)
abline(model2,col="blue")

#Predictions

predict(model2, newdata = list(glucose = 122), interval = "prediction",level=0.95)

# Risk V Pressure
model3 <- lm(Risk ~pressure,data = mydata)
summary(model3)

plot(model3)
hist(model3$residuals)


plot(mydata$Risk,mydata$pressure)
abline(model3,col="blue")

#Predictions

predict(model3, newdata = list(pressure = 71), interval = "prediction",level=0.95)

# Risk V Triceps
model4 <- lm(Risk ~triceps,data = mydata)
summary(model4)

plot(model4)
hist(model4$residuals)

plot(mydata$Risk,mydata$triceps)
abline(model1,col="blue")

#Predictions

predict(model4, newdata = list(triceps = 29), interval = "prediction",level=0.95)

# Risk V Insulin
model5 <- lm(Risk ~insulin,data = mydata)
summary(model5)

plot(model5)
hist(model5$residuals)

plot(mydata$Risk,mydata$insulin)
abline(model1,col="blue")

#Predictions

predict(model5, newdata = list(insulin = 157), interval = "prediction",level=0.95)

# Risk V mass
model6 <- lm(Risk ~mass,data = mydata)
summary(model6)

plot(model6)
hist(model6$residuals)


plot(mydata$Risk,mydata$mass)
abline(model6,col="blue")

#Predictions

predict(model6, newdata = list(mass = 33), interval = "prediction",level=0.95)

# Risk V Age
model7 <- lm(Risk ~age,data = mydata)
summary(model7)

plot(model7)
hist(model7$residuals)


plot(mydata$Risk,mydata$age)
abline(model7,col="blue")

#Predictions

predict(model7, newdata = list(age = 31), interval = "prediction",level=0.95)

# Risk V Diabetes
model8 <- lm(Risk ~diabetes,data = mydata)
summary(model8)

plot(model8)
hist(model8$residuals)

#Predictions

predict(model8, newdata = list(diabetes = "pos"), interval = "prediction",level=0.95)
predict(model8, newdata = list(diabetes = "neg"), interval = "prediction",level=0.95)
