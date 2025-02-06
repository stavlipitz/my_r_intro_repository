#### Intro to R - Assignment 10 - Stav Lipitz 305644676
# load libraries
library(tidyverse)
library(dplyr)
library(pROC)

# load data
df = read.csv(file = 'Titanic.csv')

# change variable name
df = df |> 
  mutate(gender = Sex) |>
  mutate(gender = as.factor(gender),
         PClassBinary = factor(ifelse(PClass == "1st", "1st", "Other")))

#set 'male' as base level for gender factor
contrasts(df$gender) = c(1,0)
contrasts(df$gender)

# set '1st' as base level for class factor 
contrasts(df$PClassBinary) = c(0,1)
contrasts(df$PClassBinary)

# descriptives for gender, class, and survivors
summary(df)
df |> select(gender, PClass, Survived) |>
  group_by(gender, PClass, Survived) |> 
  summarise(obs_num = n())

#### logistic regression ----
# model 1 - survival by intercept
model1 = glm(Survived ~ 1, data = df, family = binomial)
summary(model1)
exp(coef(model1))

# model 2 - survival by intercept+gender
model2 = glm(Survived ~ 1 + gender, data = df, family = binomial)
summary(model2)
exp(coef(model2))

# model 3 - survival by intercept+gender+class
model3 = glm(Survived ~ 1 + gender + PClassBinary, data = df, family = binomial)
summary(model3)
exp(coef(model3))

## ROC
# model predictions
model1_predictions = predict(model1, type = "response")
model2_predictions = predict(model2, type = "response")
model3_predictions = predict(model3, type = "response")

roc_model1 = roc(df$Survived, model1_predictions)
roc_model2 = roc(df$Survived, model2_predictions)
roc_model3 = roc(df$Survived, model3_predictions)

#AUC
auc(roc_model1)
auc(roc_model2)
auc(roc_model3)

# AUC graph
plot(roc_model1, col = "blue", main = "ROC Curve Comparison")
plot(roc_model2, add = TRUE, col = "red")
plot(roc_model3, add = TRUE, col = "green")
legend("bottomright", legend = c("Model 1", "Model 2", "Model 3"), col = c("blue","red", "green"), lwd = 2)
