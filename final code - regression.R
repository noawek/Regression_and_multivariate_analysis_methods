fname <- file.choose("movies_big_five") # Get the full path to the file
mydata <- read.csv(fname, header = TRUE) # read the file in that path

#----------question 1--------------------------------------------------------

#shirin
model1 <- lm(avg_rating ~ is_personalized,
               data = mydata)

summary(model1)

#adi
model2 <- lm(avg_rating ~ openness + agreeableness,
             data = mydata)

summary(model2)

#----------question 2--------------------------------------------------------

summary(model1) #proved to be right- significant
summary(model2) # not predicting well

#----------question 3--------------------------------------------------------

install.packages("parameters")
library(parameters)
model_parameters(model1, standardize = "basic")
model_parameters(model2, standardize = "basic")

#----------question 5--------------------------------------------------------

library(parameters)
model1.5 <- lm(avg_rating ~ enjoy_watching + is_personalized, data = mydata)
model_parameters(model1.5)
summary(model1.5)

model2.5 <- lm(avg_rating ~ enjoy_watching + is_personalized + openness
             + agreeableness + emotional_stability + conscientiousness + extraversion, 
             data = mydata)
model_parameters(model2.5) 
summary(model2.5)

anova(model1.5, model2.5)

###################################or#####################################

install.packages("BayesFactor")
library(BayesFactor)
model1.5B <- lmBF(avg_rating ~ enjoy_watching + is_personalized, data = mydata)
model2.5B <- lmBF(avg_rating ~ enjoy_watching + is_personalized + openness
                + agreeableness + emotional_stability + conscientiousness + extraversion, 
                data = mydata)
summary(model1.5B)
summary(model2.5B)
model2.5B / model1.5B

#----------question 8--------------------------------------------------------

install.packages("correlation")
install.packages("ppcor")
library(correlation)
library(ppcor)

cor(mydata$avg_rating,mydata$enjoy_watching,method="pearson")   #simple correlation just to compare to the outcome 

spcor.test(mydata$avg_rating,              #part correlation
           mydata$enjoy_watching,
           z = mydata$is_personalized)     #significant

#################demonstration of the process behind the simple code#############################

enjoy_watching_clean_from_is_personalized_model<-lm(mydata$enjoy_watching~mydata$is_personalized)
mydata$res <- residuals(enjoy_watching_clean_from_is_personalized_model)

cor(mydata$res,mydata$avg_rating)

