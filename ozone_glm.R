#From Statistics with R by Christoph Scherber
plot(Ozone~Wind,airquality)

#lm is used for linear regression, normal errors, constant variance
model1=lm(Ozone~Wind,airquality)
plot(model1)
# The residuals plot shows non-linearity and non-constant variance
#So the model is clearly wrong for this set of data

coef(model1)
# (Intercept)        Wind 
# 96.872895   -5.550923

#Predictions for ozone at wind speed of 19 and 20 mph
Ozone1=coef(model1)[1]+coef(model1)[2]*19  #Ozone1 = -8.59
Ozone2=coef(model1)[1]+coef(model1)[2]*20  #Ozone2 = -14.1
#The model predicted negative ozone concentration at high wind speed which is wrong!
Ozone1
Ozone2

#Force the model to be non-linear
model2=glm(Ozone~Wind,airquality,family=poisson)
coef(model2)
# (Intercept)        Wind 
# 5.0795877  -0.1488753 

Ozone1.glm=exp(coef(model2)[1]+coef(model2)[2]*19)  #Ozone1.glm = 
Ozone2.glm=exp(coef(model2)[1]+coef(model2)[2]*20)  #Ozone2.glm = 
Ozone1.glm
Ozone2.glm

##

library(nlme)
summary(airquality$Ozone)
# Min. 1st Qu.  Median    Mean 3rd Qu. 
# 1.00   18.00   31.50   42.13   63.25 
# Max.    NA's 
#  168.00      37
model3=gls(Ozone~Wind,airquality,na.action=na.exclude) 

head(airquality)

Date=as.Date(paste(1973,airquality$Month,airquality$Day,sep="-"))

library(lattice)
xyplot(Ozone~Date,airquality)

model4=gls(Ozone~Wind*Date,airquality,na.action=na.exclude) 
#plot(ACF(model4)) #Error because missing values in the data

# Create new data set
air2=subset(airquality,complete.cases(Ozone))
x=!is.na(airquality$Ozone)
Date.air2=Date[x]
model5=gls(Ozone~Wind*Date.air2,air2)
plot(ACF(model5,form=~Date.air2),alpha=0.05)

model6=update(model5,correlation=corAR1())

#Compare model5 and model6 #multi-model inference - MuMIn
install.packages("MuMIn") 
library(MuMIn)

AICc(model5,model6)
#         df    AICc
# model5  5     1099.40
# model6  6     1095.21

summary(model6)

# Generalized least squares fit by REML
# Model: Ozone ~ Wind * Date.air2 
# Data: air2 
# AIC     BIC    logLik
# 1094.439 1110.75 -541.2197
# 
# Correlation Structure: AR(1)
# Formula: ~1 
# Parameter estimate(s):
#   Phi 
# 0.2811007 
# Coefficients:
#   Value Std.Error
# (Intercept)    -310.82785 224.46119
# Wind             26.34403  18.92699
# Date.air2         0.30485   0.17265
# Wind:Date.air2   -0.02370   0.01462
# t-value p-value
# (Intercept)    -1.384773  0.1689
# Wind            1.391876  0.1667
# Date.air2       1.765721  0.0802
# Wind:Date.air2 -1.621035  0.1078
# 
# Correlation: 
#   (Intr) Wind   Dat.r2
# Wind           -0.909              
# Date.air2      -0.999  0.910       
# Wind:Date.air2  0.907 -0.999 -0.909
# 
# Standardized residuals:
#   Min         Q1        Med         Q3 
# -1.5165120 -0.7094215 -0.1900599  0.6459231 
# Max 
# 3.3907429 
# 
# Residual standard error: 26.69287 
# Degrees of freedom: 116 total; 112 residual
