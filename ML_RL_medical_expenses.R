## Chapter 6: LR
## Machine Learning with R: predicting medical bills
install.packages("psych")
library(readr)
library(psych)
library(stats)
## Step 1. Get data
url_data <- "https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/insurance.csv"
insurance <- read_csv(url(url_data))

## Step 2. Analyzing data
str(insurance)
summary(insurance$charges)
# Min.    1st Qu.   Median    Mean    3rd Qu.     Max. 
# 1122    4740      9382      13270   16640       63770 

hist(insurance$charges)
table(insurance$region)
# northeast   northwest   southeast   southwest 
# 324         325         364         325 

## 2.1 Exploring relationships among features - correlation matrix
cor(insurance[c("age","bmi","children",'charges')])
pairs(insurance[c("age","bmi","children",'charges')])
pairs.panels(insurance[c("age","bmi","children",'charges')])

## Step 3: Creating model 
ins_model <- lm(charges ~ ., data = insurance)

## Step 4: Evaluating the model
ins_model
# Call:
#   lm(formula = charges ~ ., data = insurance)
# 
# Coefficients:
#   (Intercept)               age                   sexmale                 
#   -11938.5                  256.9                 -131.3                         
#   bmi                       children              smokeryes
#   339.2                     475.5                 23848.5
#   regionnorthwest           regionsoutheast       regionsouthwest  
#   -353.0                    -1035.0               -960.1  

summary(ins_model)
# Call:
#   lm(formula = charges ~ ., data = insurance)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -11304.9  -2848.1   -982.1   1393.9  29992.8 
# 
# Coefficients:
#   Estimate      Std. Error          t   value   Pr(>|t|)    
#   (Intercept)     -11938.5      987.8  -12.086  < 2e-16 ***
#   age                256.9       11.9   21.587  < 2e-16 ***
#   sexmale           -131.3      332.9   -0.394 0.693348    
#   bmi                339.2       28.6   11.860  < 2e-16 ***
#   children           475.5      137.8    3.451 0.000577 ***
#   smokeryes        23848.5      413.1   57.723  < 2e-16 ***
#   regionnorthwest   -353.0      476.3   -0.741 0.458769    
#   regionsoutheast  -1035.0      478.7   -2.162 0.030782 *  
#   regionsouthwest   -960.0      477.9   -2.009 0.044765 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6062 on 1329 degrees of freedom
# Multiple R-squared:  0.7509,	Adjusted R-squared:  0.7494 
# F-statistic: 500.8 on 8 and 1329 DF,  p-value: < 2.2e-16


