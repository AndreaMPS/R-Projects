#Andrea Smith | Fin.Ex. NHANES Analysis | 11/19/2025

library(tidyverse)
library(labelled)
#library(epiR)
library(broom)
library(prediction)
library(ggplot2)
library(dplyr)

load("final25.RData")
glimpse(df)

#Question 1-3.) Descriptive Statistics #####################################################################
tapply(df$dbp, df$male, mean, na.rm = T) #Mean of diastolic for sex
tapply(df$dbp, df$male, sd, na.rm = T)

tapply(df$dbp, df$racebin, mean, na.rm = T) #Mean of diastolic for race
tapply(df$dbp, df$racebin, sd, na.rm = T)

tapply(df$dbp, df$married, mean, na.rm = T) #Mean of diastolic for marital
tapply(df$dbp, df$married, sd, na.rm = T)

#trpcopd_box <- ggplot(prac, aes(x=trp, y=copd)) +
 # geom_boxplot() + labs(title = "Distribution of Patient Triglyceride(mg/dl) by Chronic Obstructive Pulmonary Disease")
#print(trpcopd_box)
#tapply(prac$trp, prac$copd, summary, na.rm = T)

#tapply(prac$trp, prac$racethn, mean, na.rm = T)

#Question 4.) Null Hypothesis - systolic and insurance ###################################################
t.test(df$sbp ~ df$insur, var.equal = TRUE)

#Question 5.) Multiple Linear Regression - bmi(explanatory) and scl(total cholesterol) (response) with adjusted variables #################
bmiscl_mlr <- lm(scl ~ bmi +age+male+racebin+married+pir, data = df)
summary(bmiscl_mlr)

prediction(bmiscl_mlr, at=list(scl = c(), race='non-White', age=60, married='Never Married', pir=1, bmi=40))

#186.3 + 0.127(bmi) + 0.101(age) -6.953(male) + 0.561(non-white) - 2.476(widowed) - 8.931(Never married) - 2.784(pir)
#Hand Calc 186.3 + 0.127(40) + 0.101(60) -6.953(0) + 0.561(1) - 8.931(1) - 2.784(1)

#Question 6.) Hypothesis - Poverty line and hypertension#########################################################################

prop.table(table(df$ht, df$pir), margin = 2) * 100


hypertpoverty_chi <- chisq.test(table(df$ht, df$pir))
hypertpoverty_chi

hypertpoverty_slogr <- glm(ht ~ pir, family = binomial(link="logit"), data = df)
hypertpoverty_slogr

#Odds ratio 
OR <- tidy(hypertpoverty_slogr, exponentiate = TRUE, conf.int = TRUE)
OR
#exp(confint(hypertpoverty_slogr)) # confidence intervals for odds ratio

#Question 7.) Logistic Regression - hypertension and pir by race interaction with adjusted variables
hypertpovertyrace_log <- glm(ht ~ racebin*pir, family = binomial(link="logit"), data = df)
summary (hypertpovertyrace_log)

OR3 <- tidy(hypertpovertyrace_log, exponentiate = TRUE,
            conf.int=TRUE)
OR3

#Exposure Table
exp(0.346)
exp(0.096)
#0.5766+0.6708-0.0553 #Interaction coefficient 

exp(0.3456+0.0955+0.2053) #Interaction Odds Ratio


#Question 8.) Multiple Logistic Regression - hypertension and poverty with adjusted variables
hypertpoverty_mlogr <- glm(ht ~ pir +age+male+racebin+married+smoke, family = binomial(link="logit"), data = df)
hypertpoverty_mlogr

#Odds ratio 
OR2 <- tidy(hypertpoverty_mlogr, exponentiate = TRUE,
            conf.int=TRUE)
OR2

#Prediction
prediction(hypertpoverty_mlogr, at=list(ht = ('High BP'), race='non-White', age=50, smoke='Current Smoker', pir=1, male=1))

