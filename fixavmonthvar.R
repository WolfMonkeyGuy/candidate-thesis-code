library(DT)
library(readxl)
library(dplyr)
library(modelsummary)
library(knitr)
library(kableExtra)
library(lubridate)
library(readr)

derived_dataset = read.csv("", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date))

# Swedish general election dates
election_dates = as.Date(c(
  "1988-09-18", "1991-09-15", "1994-09-18", "1998-09-20",
  "2002-09-15", "2006-09-17", "2010-09-19", "2014-09-14",
  "2018-09-09", "2022-09-11", "2026-09-13"
))

derived_dataset = derived_dataset %>%
  mutate(
    Date = as.Date(Date),
    next_election = election_dates[sapply(Date, function(d) which(election_dates >= d)[1])],
    Months_til_elect = (year(next_election) - year(Date)) * 12 +
      (month(next_election) - month(Date))
  )


datatable(derived_dataset)

write_csv(derived_dataset, "")
