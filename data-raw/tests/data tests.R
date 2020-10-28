library(tidyverse)

dat <- read.csv("./data-raw/tests/openworldindata.csv")

dat <- dat %>% filter(location=="Germany", !is.na(total_tests)) %>%
  arrange(date) %>%
  mutate(
    new.tests = total_tests-lag(total_tests),
    date = as.Date(date)
  )


tests <- dat
use_data(tests,overwrite = TRUE)

