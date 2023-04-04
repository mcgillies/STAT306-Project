## Analysis
install.packages("Hmisc")
install.packages("corrplot")
library(Hmisc)
library(tidyverse)
library(corrplot)
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

# Model creation
model_noint = lm(bot_score_english~age+activity, data = bot_data)
model_int = lm(bot_score_english~age+activity+age*activity, data = bot_data)
summary(model_noint)
summary(model_int)
