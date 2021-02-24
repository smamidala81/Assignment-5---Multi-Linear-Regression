#Assignement 5 - MLR - 50 startup

startup<-read.csv('E:\\Sree-Official\\Sree-Personal\\EXCELR\\Data Science\\Assignments\\Multi Linear Regression 5\\50_Startups.csv')
View(startup)
startup50=startup[,-c(4)]
attach(startup50)

#measures of central tendancy & 
mean(Profit)
median(Profit)
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(Profit)

#Measures of Dispersion
var(Profit)
sd(Profit)
range(Profit)
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Profit)

# Measures of skweness & kurtosis
library(moments)
skewness(Profit)
kurtosis(Profit)

hist(Administration)
hist(Marketing.Spend)
hist(Profit)
hist(R.D.Spend)

boxplot(startup50)

pairs(startup50)
correlation=cor(startup50)

#1-Regression Model and Summary
startup_model=lm(Profit~.,data=startup50)
summary(startup_model)

# Experiment #
startup_Admin=lm(Profit~Administration,data=startup50)
summary(startup_Admin)

startup_M_Spend=lm(Profit~Marketing.Spend,data=startup50)
summary(startup_M_Spend)

startup_Admin_M_Spend=lm(Profit~Administration+Marketing.Spend,data=startup50)
summary(startup_Admin_M_Spend)

#Multi-colinearity
install.packages('car')
library(car)
car::vif(startup_model)

#Diagnostic plots:
#Residual plots,QQ plot, Residual vs fitted
plot(startup_model)

#Residual vs regressors
residualPlots(startup_model)

#Added variable plots
avPlots(startup_model)

#QQ plot for studentized residuals
qqPlot(startup_model)

#Deletion Diagnostics
influenceIndexPlot(startup_model)

####Iteration 1 
#Remove 46,50 observation
startup50_2<-startup50[-c(46,50),]
startup_model5=lm(Profit~.,data=startup50_2)
summary(startup_model5)
car::vif(startup_model5)
plot(startup_model5)
residualPlots(startup_model5)
qqPlot(startup_model5)
influenceIndexPlot(startup_model5)

library(MASS)
stepAIC(startup_model5)

startup_model6=lm(Profit~R.D.Spend + Marketing.Spend,data=startup50_2)
summary(startup_model6)


# Transformation technique
# using multiply function
startup50['Administration2']=Administration*Administration
View(startup50)

startup_model2=lm(Profit~.,data=startup50)
summary(startup_model2)

# using log function
startup50['Administration2']=log(Administration)
View(startup50)

startup_model3=lm(Profit~.,data=startup50)
summary(startup_model3)

# using Sqrt function
startup50['Administration2']=sqrt(Administration)
View(startup50)

startup_model4=lm(Profit~.,data=startup50)
summary(startup_model4)

##Predict for new data
attach(startup50_2)
pred=predict(startup_model5)
pred
pred=predict(startup_model5)
Startup50_Finaldata=data.frame(startup50_2,pred,"Error"=startup50_2$Profit-pred)

write.csv(Startup50_Finaldata,'Startup50_Finaldata.csv',row.names = FALSE)

