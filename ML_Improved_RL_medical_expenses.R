## Chapter 6: LR
## Machine Learning with R: improving predicting medical bills

library(readr)
library(psych)
library(stats)

## Step 1. Get data
url_data <- "https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/insurance.csv"
insurance <- read_csv(url(url_data))

## Step 2. Analyzing data
str(insurance)
summary(insurance$charges)
hist(insurance$charges)
table(insurance$region)

## 2.1 Exploring relationships among features - correlation matrix
cor(insurance[c("age","bmi","children",'charges')])
pairs(insurance[c("age","bmi","children",'charges')])
pairs.panels(insurance[c("age","bmi","children",'charges')])

## Step 3: Creating model 
ins_model <- lm(charges ~ ., data = insurance)

## Step 4: Evaluating the model
ins_model

## Step 5: Improving the model
## Adding non-linear term for age
## Change age since a linear function to cuadratic function
insurance$age2 <- insurance$age^2

## Created an indicator for obesity
insurance$bmi30 <- ifelse(insurance$bmi > 30, 1,0)

## Specified an interaction between obesity and smoking
## Including bmi30*smoker, and that will include in the model each of the individual variables plus the multiplication of both
## bmi30 + smokeryes + bmi30*smokeryes

ins_model2 <- lm(charges ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)

## Step 6: Evaluation of the improved model
summary(ins_model2)

# Call:
#   lm(formula = charges ~ age + age2 + children + bmi + sex + bmi30 * 
#        smoker + region, data = insurance)
# 
# Residuals:
#   Min       1Q          Median      3Q          Max 
# -4260.3     -1644.6     -1272.7     -784.7      24192.7 
# 
# Coefficients:
#                     Estimate  Std. Error    t value     Pr(>|t|)    
# (Intercept)          69.2494  1353.2349      0.051      0.959195    
# age                 -21.6786    59.4956     -0.364      0.715638    
# age2                  3.5978     0.7422      4.847      1.40e-06 ***
# children            661.5105   105.2784      6.283      4.48e-10 ***
# bmi                 114.2920    34.0816      3.353      0.000821 ***
# sexmale            -475.6760   242.9293     -1.958      0.050430 .  
# bmi30              -938.5116   420.5807     -2.231      0.025817 *  
# smokeryes         13421.6370   435.9158     30.790       < 2e-16 ***
# regionnorthwest    -275.6659   347.2730     -0.794      0.427453    
# regionsoutheast    -826.1187   349.6181     -2.363      0.018275 *  
# regionsouthwest   -1164.8152   348.5123     -3.342      0.000854 ***
# bmi30:smokeryes   19912.6072   600.8493     33.141       < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4419 on 1326 degrees of freedom
# Multiple R-squared:  0.8679,	Adjusted R-squared:  0.8668 
# F-statistic: 792.1 on 11 and 1326 DF,  p-value: < 2.2e-16

