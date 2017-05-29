#Linear Regression of Ozone and Solar Radiation
data("airquality")

# "airquality" data have 6 columns: Ozone Solar.R Wind Temp Month Day
Ozone=airquality$Ozone
Solar.R=airquality$Solar.R
mean.Ozone=mean(Ozone,na.rm=T)

#Plot ozone and solar radiation
plot(Solar.R,Ozone) 

#Plot mean value of ozone ("h" = horizontal line)
abline(h=mean.Ozone) 

#Use a linear model to fit a regression line
model1=lm(Ozone~Solar.R)

#
model1 

#Plot the line
abline(model1,col="red")  


#Look at the residuals
plot(model1)

#
termplot(model1)

#summarize the model
summary(model1)

termplot(model1)