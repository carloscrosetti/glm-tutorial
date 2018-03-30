# From Carlos Crosetti (carlos.crosetti@outlook.com) as of 3/30/2018
# DISClAIMER 
# Tested under R 3.4.3
# Code loaded from https://rstatisticsandresearch.weebly.com/logistic-regression-glm.html
# Minor changes to scrip to run the code without any issues.
#
# Instructions to run this code:
#
# Create a folder in your computer and save this script along with the 
# data file named logreg.txt
#
# Adjust the path where the input file logreg.txt is stored
#
# Manually install the 4 packages listed below
#
# Run this script
# 
# install.packages("car")
# install.packages("pscl")
# install.packages("ROCR")
# install.packages("ggplot2")

library(ggplot2)
library(ROCR)
library(car)
library(pscl)

# this allows to scroll the graphics device
windows(record=TRUE)

#####Logistic regression - finding the best model#####
# Generalized linear model (GLM) for binomial data (0 or 1) (logistic regression)

# WARNING you must adjust the path to funtion read.table to reflect the data file location

datalr <- read.table("C:/Users/Carlos/OneDrive/DS/2018 LR GLM 1/logreg.txt", h=T)
attach(datalr)
str(datalr)

#Model 1 - Full model
GLM1 <- glm(Event == 1 ~ Predator_density+Turbidity+Algae, family = binomial(link="logit"), data=datalr)
summary(GLM1)

vif(GLM1)  #We do not want large values (> 10) of the variance inflation factor (collinearity)

plot(datalr$Turbidity,datalr$Algae)

#Model 2 - Reduced model a
GLM2 <- glm(Event ==1 ~ Predator_density+Algae, family = binomial(link="logit"), data=datalr)
summary(GLM2)

vif(GLM2)

#Model 3 - Reduced model b
GLM3 <- glm(Event ==1 ~ Predator_density, family = binomial(link="logit"), data=datalr)
summary(GLM3)

anova(GLM2,GLM3, test="Chisq")   #If non-significant, go for the reduced model

#####Assessing model fit and performance#####


pR2(GLM3)


prob <- predict(GLM3, type=c("response"))
pred <- prediction(prob, datalr$Event)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf,xlab="Specificity (true negatives)",ylab="Sensitivity (true positives)") 
abline(0,1)

#Calculate the area under the curve as a measure for model performance
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc #Target = auc> 0.8



#####Data visualization#####

plot1a <- ggplot(data = datalr, aes(x=Predator_density, y=Event)) + geom_point() + theme_bw() + labs(x= "Predator density (predators/pond)", y = expression("Probability of death")) + ylim(0,1)
plot1a


#Extract intercept and slope
intercept <- coef(GLM3)[1]
slope <-  coef(GLM3)[2]

plot1b <- plot1a + stat_function(fun = function(x) { 1/(1+exp(-intercept -slope *(x)))})
plot1b

#Extract the 50% inflection point
p <- 0.5
x <- (log(p/(1-p)) - intercept) / slope
x


