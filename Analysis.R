## Analysis
install.packages("Hmisc")
install.packages("corrplot")
install.packages("regclass")
library(Hmisc)
library(tidyverse)
library(corrplot)
library(regclass)
library(ppcor)
# Reading in data
bot_data <- read_csv("activity_botscore.csv")

# Exploring data through summaries and visualizations
head(bot_data)
summary(bot_data)
pairs(~bot_score_english + age + count + activity, pch = 19, data = bot_data)
## Random distribution between all features except count and activity, which
## have a very strong positive linear relationship. 
hist.data.frame(bot_data)
## All relevant features (user_id is not relevant) have a heavily right-tailed
## distribution. This indicates that most of the observations fall in the lower
## echelon of values for each feature. 
all_correlations <- cor(bot_data)
corrplot(all_correlations)
## We can see a very large negative correlation between user_id and age and a
## very large correlation between activity and count. Besides these all 
## correlations are quite minimal

## Creating dataframe with only desired columns
bot_data_filtered <- bot_data %>% dplyr::select(bot_score_english, age, activity)

## Looking at partial correlations
pcor(bot_data_filtered)

# Model creation
model_noint = lm(bot_score_english~age+activity, data = bot_data)
model_int = lm(bot_score_english~age+activity+age*activity, data = bot_data)
summary(model_noint, conf.int = TRUE)
summary(model_int, conf.int=TRUE)

# Checking VIF for each model 
VIF(model_noint)
## Variables both have approximately 1 VIF, indicating low multicollinearity 
## and that the variables are not correlated strongly
VIF(model_int)
## Activity and the interaction term now have high VIF, indicating high 
## multicollinearity between these terms. Therefore the correlation is quite
## high between these variables, which can skew the model results. 


# Plotting residuals plot and qqplot for each model
plot(model_noint)
# Residual plot shows a random scatter with no apparent pattern, indicating
# a linear model is appropriate for modelling the data. QQplot is right-
# skewed with the right half of the plot being above the qqline.
plot(model_int)
# Likewise, the residual plot seems to bare no apparent pattern indicating
# a linear model is appropriate here. The QQplot is also similar to the 
# previous model being right-skewed.  
