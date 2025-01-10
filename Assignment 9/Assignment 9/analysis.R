#### config ----
set.seed(123)
df = data.frame(
  subject    = seq(from = 1, to = 120, by = 1),
  rt         = rnorm(n = 120, mean = 3100, sd = 1450),
  depression = runif(n = 120, min = 0, max = 100),
  sleep_duration = rnorm(n = 120, mean = 7, sd = 2.5)
  )