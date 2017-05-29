#Model joined influence of solar radiation and wind speed on ozone

plot(Ozone~Solar.R,airquality)
plot(Ozone~Wind,airquality) 
coplot(Ozone~Solar.R|Wind,panel=panel.smooth,airquality)

#multi regression model
model2=lm(Ozone~Solar.R*Wind,airquality)
plot(model2)
summary(model2)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -48.694 -17.200  -4.384  12.740  78.218 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  34.467686  17.634602   1.955 0.053246 .  
# Solar.R       0.324141   0.083928   3.862 0.000193 ***
#   Wind         -1.594546   1.508979  -1.057 0.293026    
# Solar.R:Wind -0.020279   0.007246  -2.799 0.006089 ** 
#   ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 24.16 on 107 degrees of freedom
# (42 observations deleted due to missingness)
# Multiple R-squared:  0.487,	Adjusted R-squared:  0.4727 
# F-statistic: 33.86 on 3 and 107 DF,  p-value: 1.807e-15

termplot(model2)

# summary(airquality$Solar.R)
# Min. 1st Qu.  Median    Mean 3rd Qu. 
# 7.0   115.8   205.0   185.9   258.8 
# Max.    NA's 
# 334.0       7 

summary(airquality$Wind)
# Min. 1st Qu.  Median    Mean 3rd Qu. 
# 1.700   7.400   9.700   9.958  11.500 
# Max. 
# 20.700 

#Create particular values of solar radiation that interests us
Solar1=mean(airquality$Solar.R,na.rm=T)
Solar2=100
Solar3=300

#Predict ozone concentrations
p1=predict(model2,data.frame(Solar.R=Solar1,Wind=1:20)) #Solar1 = 185.9315
p2=predict(model2,data.frame(Solar.R=100,Wind=1:20))
p3=predict(model2,data.frame(Solar.R=300,Wind=1:20))


plot(Ozone~Wind,airquality) 
lines(1:20,p1)
lines(1:20,p2,col="red")
lines(1:20,p3,col="blue")
