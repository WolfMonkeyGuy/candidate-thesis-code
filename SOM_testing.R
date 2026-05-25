setwd("")

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(modelsummary)
  library(marginaleffects)
  library(DT)
})

# ---------------------

aktie16 = read.csv("C:/SkolUppgifter/Kandidat/SOM/Aktie_16_29.csv")
aktie30 = read.csv("C:/SkolUppgifter/Kandidat/SOM/Aktie_30_49.csv")
aktie50 = read.csv("C:/SkolUppgifter/Kandidat/SOM/Aktie_50_64.csv")
aktie65 = read.csv("C:/SkolUppgifter/Kandidat/SOM/Aktie_65plus.csv")

aktie_höger = read.csv("C:/SkolUppgifter/Kandidat/SOM/Aktie_höger.csv")
aktie_mitten = read.csv("C:/SkolUppgifter/Kandidat/SOM/Aktie_mitten.csv")
aktie_vänster = read.csv("C:/SkolUppgifter/Kandidat/SOM/Aktie_vänster.csv")

aktie_ut_låg = read.csv("C:/SkolUppgifter/Kandidat/SOM/Aktie_ut_låg.csv")
aktie_ut_mitten = read.csv("C:/SkolUppgifter/Kandidat/SOM/Aktie_ut_medel.csv")
aktie_ut_hög = read.csv("C:/SkolUppgifter/Kandidat/SOM/Aktie_ut_hög.csv")

favorit_parti = read.csv("C:/SkolUppgifter/Kandidat/SOM/Favorit_Parti.csv")
partianhängare = read.csv("C:/SkolUppgifter/Kandidat/SOM/Partianhängare.csv")

# ---------------------

aktie_alder = bind_rows(
  aktie16,
  aktie30,
  aktie50,
  aktie65
  ) %>%
  arrange(År, Ålder..4.gradig.)

aktie_vänster_höger = bind_rows(
  aktie_höger,
  aktie_mitten,
  aktie_vänster
  ) %>%
  arrange(År, Subjektiv.vänster.högerplacering)

aktie_utbildning = bind_rows(
  aktie_ut_låg,
  aktie_ut_mitten,
  aktie_ut_hög
  ) %>%
  arrange(År, Utbildning..3.gradig.)

# ------------------------
datatable(aktie_alder)
datatable(aktie_vänster_höger)
datatable(aktie_utbildning)

# ------------------------

partianhängare = as.factor(partianhängare)


model = glm(favorit_parti ~ aktie_alder + aktie_vänster_höger + aktie_utbildning + partianhängare)
summary(model)
