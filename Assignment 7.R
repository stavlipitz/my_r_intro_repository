# Introduction to R Assignment 7
# Stav Lipitz #id 305644676

#### Part 1 raw data creation ----
### open libraries
library(tidyverse)
library(dplyr)

### binding csv's togther
file_names = dir(".", pattern = "\\.csv$") #save all csv's to this directory
df         = data.frame() #create df to contain all csv's
for (file in file_names) {
  temp_data = read.csv(file.path(".", file))  
  df = rbind(df, temp_data) 
} # open each csv in this directory and bind it to the same file

### create task and congruency columns

df = df |> mutate(task       = ifelse(grepl("ink_naming", condition), "ink_naming", "word_reading"), 
             congruency = ifelse(grepl("incong", condition), "incongruent", "congruent"),
             acc        = ifelse(correct_response == participant_response, 1, 0)) |>
### save numeric columns as numerics & character columns as factors
  mutate(subject        = as.factor(subject),
         block          = as.numeric(block),
         trial          = as.numeric(trial),
         task           = factor(task, levels = c("ink_naming", "word_reading")),
         congruency     = factor(congruency, levels = c("congruent", "incongruent")),
         acc            = as.numeric(acc),
         rt             = as.numeric(rt)) |> 
### keep only relevant columns
  select(subject, block, trial, task, congruency, acc, rt)
summary(df)

#set effect coding for factors
contrasts(df$task) = contr.sum(2)
contrasts(df$congruency) = contr.sum(2)

#check coding
contrasts(df$task)
contrasts(df$congruency)

save(df, file = "raw_data.rdata")

#### Part 2 filtered data creation
load("raw_data.rdata")

#check number of participants
length(unique(df$subject))

#omit rows with NA values & omit rows with outlier rt's
df_filtered = df |> 
  na.omit(df)    |> 
  filter(rt < 3000 & rt > 300)

#calculate % remaining trials after omittion
df_filtered |> 
  group_by(subject) |>
  
  #per subject
  summarise(trials_remaining = n()/400) |>
  print() |>
  
  #overall 
  summarise(mean_remaining_trial_rate = mean(trials_remaining),
            sd = sd(trials_remaining))
#save
save(df_filtered, file = "filtered_data.rdata")

#### Part 3 Descriptives ----

### config
min_rt  = min(df_filtered$rt)
max_rt  = max(df_filtered$rt)

#summary table for rt and acc per congruency & task
df_summarised = df_filtered |> 
  group_by(congruency, task) |>
  summarise(mean_rt = mean(rt),
            sd_rt = sd(rt),
            mean_acc = mean(acc),
            sd_acc = sd(acc)) |>
  print()

### open ggplot
library(ggplot2)

#ggplot for accuracy
ggplot(data = df_summarised, mapping = aes(x = congruency, y = mean_acc, fill = task)) +
  geom_col(position = position_dodge(0.7), color = "black") +  # יצירת עמודות עם הפרדה בין task
  geom_errorbar(aes(ymin = mean_acc - sd_acc, ymax = mean_acc + sd_acc),
                width = 0.2, position = position_dodge(0.7)) +  # הוספת פסי שגיאה
  labs(
    title = "Mean Accuracy by Task and Congruency",
    x = "Word-Color Congruency",
    y = "Mean Accuracy"
  ) +
  theme_classic()


#ggplot for reaction times
ggplot(data = df_summarised, mapping = aes(x = congruency, y = mean_rt, color = task)) +
  geom_point(size = 3,
             position = position_dodge(0.5)) +
  geom_errorbar(mapping = aes(ymin = mean_rt - sd_rt,
                              ymax = mean_rt + sd_rt),
                width = 0.2,
                position = position_dodge(0.5)) +
  ylim(min = min_rt - 20, max = max_rt +20) +
  ylab("Reaction Times") +
  xlab("Color-Word congruency") 
