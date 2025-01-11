library(tidyverse)
library(dplyr)

#### config ----
set.seed(123)
N = 120
df = data.frame(
  subject    = seq(from = 1, to = N, by = 1),
  gender     = sample(x = c("male", "female"), size = N, replace = T),
  age        = runif(n = N, min = 18, max = 50),
  rt         = rnorm(n = N, mean = 3100, sd = 1450),
  depression = runif(n = N, min = 0, max = 100),
  sleep_duration = rnorm(n = N, mean = 7, sd = 2.5)
  )

source("functions.R")
results = summarise_df(df, 50, 100)


