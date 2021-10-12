library(tidyverse)
library(caret)
library(leaps)
library(MASS)
library(readxl)
library(car)
library(psych)
library(StatMatch)
data <- read_excel("Case data and variable description.xls")
#Define Datasets
str(data)
summary(data)
# Regression 								
# Fit model
Model <- lm(PERMNO ~., data = data)
summary(Model)
predict(Model, newdata = data, interval = 'prediction')
predicted <- predict(Model, data,  interval = "confidence")
 plot(data$Exchange, data$PERMNO, pch = 20, type = 'p', las = 1, xlab = 'PermNO', ylab = 'Exchange')
abline(Model)
summary(Model)
# ANCOVA
test <- with(data, leveneTest(PERMNO, Exchange))
Model <- lm(PERMNO ~ Exchange |`Firm size` + `Past return` + `Past trading volume` + `Industry code`, data = data)
summary(Model)
# Clustering
k <- kmeans(data[1], centers = 2, nstart=25)
summary(k)
scatterplot(data$Exchange, data$`Stock return`)
# Actual Anova
fit <- aov(PERMNO ~ Exchange, data)plot..
# Reporting
describeBy(data$PERMNO, data$Exchange)
# plot
ggplot(data,aes(y=PERMNO, x=Exchange, fill=Exchange))+
  stat_summary(fun.y="mean", geom="bar",position="dodge")+
  stat_summary(fun.data = mean_se, geom = "errorbar", position="dodge",width=.8)
