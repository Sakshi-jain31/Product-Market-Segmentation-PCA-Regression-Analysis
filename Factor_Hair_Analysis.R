#### Exploratory Data Analysis####

setwd("C:/Users/Sakshi/Desktop/Great Learning/Sample R Projects/Factor_Hair_Analysis")
Hair=read.csv("Factor-Hair-Revised.csv", header = TRUE)
library(ggplot2)
library(psych)
library(corrgram)
library(car)
library(corrplot)
library(nFactors)
library(dplyr)
library(DataExplorer)
library(kableExtra)

str(Hair) #Structure of the dataset
any(is.na(Hair)) #Missing Values
summary(Hair) #Summary of dataset
dim(Hair) #Rows no of rows & columns
plot_intro(Hair) #Plot of missing values

Hair1=Hair[,2:12] #creating New Data frame, removing 1st Column

cor.h=round(cor(Hair1), 3) #Correlations between Independent variables
cor.h
corrplot(cor.h, method="shade") #Correlations Plot
attach(Hair1)
attach(Hair)
Hair2=lm(Satisfaction~., data=Hair1) #Combined Linear Regression
summary(Hair2)
vif(Hair2) #Evidence of Multicollearnity


#####Simple Linear Models Summary######
summary(Hair2)
Model1=lm(Satisfaction~ProdQual, data=Hair)
summary(Model1)
Model2=lm(Satisfaction~Ecom, data=Hair)
summary(Model2)
Model3=lm(Satisfaction~TechSup, data=Hair)
summary(Model3)
Model4=lm(Satisfaction~CompRes,data=Hair)
summary(Model4)
Model5=lm(Satisfaction~Advertising, data=Hair)
summary(Model5)
Model6=lm(Satisfaction~ProdLine, data=Hair)
summary(Model6)
Model7=lm(Satisfaction~SalesFImage, data=Hair)
summary(Model7)
Model8=lm(Satisfaction~ComPricing, data=Hair)
summary(Model8)
Model9=lm(Satisfaction~WartyClaim, data=Hair)
summary(Model9)
Model10=lm(Satisfaction~OrdBilling, data=Hair)
summary(Model10)
Model11=lm(Satisfaction~DelSpeed, data=Hair)
summary(Model11)


###To run Factor analysis two tests need to be done ######
cortest.bartlett(cor.h, nrow(Hair1))

#####PCA/Factor Analysis #######

library(nFactors)
EV=eigen(cor(Hair1))
Eigenvalue=EV$values
Factor=c(1,2,3,4,5,6,7,8,9,10,11)
scree=data.frame(Factor, Eigenvalue)
plot(scree, main="Scree Values", col="blue", ylim=c(0,4))
lines(scree, col="red")
Unrotate=principal(Hair1, nfactors = 4, rotate = "none")
Unrotate
fa.diagram(Unrotate)
Rotate=principal(Hair1, nfactors = 4, rotate = "Varimax")
Rotate
fa.diagram(Rotate)


##### Multiple Regression Analysis #######
Scores=round((Rotate$scores),2)
as.data.frame(Scores)
colnames(Scores)=c("Buyepr", "Brand", "AfSSr", "Prodt")
Hair3=Hair %>% select("Satisfaction")
Hair3
Hair_New=cbind(Hair3, Scores)
Hair_New
attach(Hair_New)
Model_New=lm(Satisfaction~Buyepr+Brand+AfSSr+Prodt, data=Hair_New)
summary(Model_New)


#### Predicting the Satisfaction ######

Predict=predict(Model_New)
as.data.frame(Predict)
Predicted=round(Predict,1)
Predicted
Hair_New=cbind(Hair_New, Predicted)
Hair_New
PredictedSatisfaction=Hair_New$Predicted
BackTrack=data.frame(Hair_New$Satisfaction, PredictedSatisfaction)
plot(Hair_New$Satisfaction, col="red")
lines(Hair_New$Satisfaction, col="red")
plot(PredictedSatisfaction, col="blue")
lines(PredictedSatisfaction, col="blue")

