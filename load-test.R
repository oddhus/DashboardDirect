library(shinyloadtest)
library(dplyr)

#https://rstudio.github.io/shinyloadtest/

record_session('http://localhost:3838/')

df <- load_runs(
  `1 user` = "test_sessions/2/demo1",
  `4 users` = "test_sessions/2/demo4",
  `16 users` = "test_sessions/2/demo16"
)

shinyloadtest_report(df, "report2.html")
