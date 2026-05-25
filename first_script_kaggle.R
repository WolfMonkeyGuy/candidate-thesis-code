# %% [markdown] {"jupyter":{"outputs_hidden":false}}
# Get data from politicos Poll Of Polls and import libaries
# 
# Data set: Data from which polling company and which day the polling data is from

# %% [code] {"execution":{"iopub.status.busy":"2026-04-20T20:46:30.157638Z","iopub.execute_input":"2026-04-20T20:46:30.159356Z","iopub.status.idle":"2026-04-20T20:46:45.498474Z","shell.execute_reply":"2026-04-20T20:46:45.496516Z"},"jupyter":{"outputs_hidden":false}}
#Download Packages here :-)
install.packages("DT")

# %% [code] {"execution":{"iopub.status.busy":"2026-04-20T21:25:47.516756Z","iopub.execute_input":"2026-04-20T21:25:47.518344Z","iopub.status.idle":"2026-04-20T21:25:47.539770Z","shell.execute_reply":"2026-04-20T21:25:47.537992Z"},"jupyter":{"outputs_hidden":false}}
#add all libraries here :-)
library(tidyr)
library(dplyr)
library(readxl)
library(readr)
library(lubridate)
library(purrr)
library(DT)

# %% [code] {"execution":{"iopub.status.busy":"2026-04-20T21:54:36.998166Z","iopub.execute_input":"2026-04-20T21:54:36.999948Z","iopub.status.idle":"2026-04-20T21:54:37.563398Z","shell.execute_reply":"2026-04-20T21:54:37.561270Z"},"_kg_hide-output":true,"jupyter":{"outputs_hidden":false}}
Polldata = read.csv("/kaggle/input/datasets/williambrjesson/swedish-polls-companies-and-dates-politico/PoliticoPollsCoDates.csv")
OMXS30 = read_excel("/kaggle/input/datasets/williambrjesson/omxs30-1986-2026/OMXS30_1986_2026.xlsx")
Arbetslos1 = read.csv("/kaggle/input/datasets/williambrjesson/1987-2020-aku-old-16-64/1987_2020_AKU_16-64yr_old.csv", skip = 2, fileEncoding = "latin1", check.names = FALSE)
Arbetslos2 = read.csv("/kaggle/input/datasets/williambrjesson/2001-2026-aku-15-74/2001_2026_AKU.csv", skip = 2, fileEncoding = "latin1", check.names = FALSE)
KPI = read.csv("/kaggle/input/datasets/williambrjesson/kpi-1980-2026/KPI_1980-2026.csv", skip = 2, fileEncoding = "latin1", check.names = FALSE)

Arbetslos_long1 = Arbetslos1 %>%
  pivot_longer(
    cols = matches("^\\d{4}M\\d{2}$"),
    names_to = "period",
    values_to = "value") %>%
  mutate(Date = ymd(paste0(gsub("M", "-", period), "-01"))) %>%
  mutate(AKU = value, value = NULL)

Arbetslos_long2 = Arbetslos2 %>%
  pivot_longer(
    cols = matches("^\\d{4}M\\d{2}$"),
    names_to = "period",
    values_to = "value") %>%
  mutate(Date = ymd(paste0(gsub("M", "-", period), "-01"))) %>%
  mutate(AKU = value, value = NULL)

Arbetslos_long1 = select(Arbetslos_long1, Date, AKU)
Arbetslos_long2 = select(Arbetslos_long2, Date, AKU)

KPI_long = KPI %>%
  select(-1) %>%
  pivot_longer(
    cols = matches("\\d{4}M\\d{2}"),
    names_to = c("Metric", "period"),
    names_sep = " ",
    values_to = "KPI",
    values_transform = list(KPI = as.character)) %>%
  mutate(Date = ymd(paste0(gsub("M", "-", period), "-01")),
         KPI = suppressWarnings(as.numeric(KPI))) %>%
  pivot_wider(names_from = Metric, values_from = KPI) %>%
  select(-period) %>%
  rename(YearChangeKPI = Årsförändring) %>%
  rename(MonthChangeKPI = Månadsförändring)



head(Polldata,5)
tail(Polldata,5)
head(OMXS30,5)
tail(OMXS30,5)

tail(Arbetslos_long1)
head(Arbetslos_long2)

datatable(KPI_long)

# %% [code] {"execution":{"iopub.status.busy":"2026-04-20T21:54:45.066287Z","iopub.execute_input":"2026-04-20T21:54:45.067784Z","iopub.status.idle":"2026-04-20T21:54:45.419296Z","shell.execute_reply":"2026-04-20T21:54:45.417371Z"},"jupyter":{"outputs_hidden":false}}
#filter dates and then combine datasets to make one dataset
Filtered_dates = Polldata %>%
  filter(as.Date(date) >= as.Date("1986-10-01")) %>%
  rename(Date = date)

Filtered_dates = Filtered_dates %>%
  mutate(Date = as.Date(Date))

OMXS30 = OMXS30 %>%
  mutate(Date = as.Date(Date))

Arbetslos_long1 = Arbetslos_long1 %>%
  mutate(Date = as.Date(Date)) %>%
  filter(as.Date(Date) < as.Date("2001-01-01"))


Arbetslos_long2 = Arbetslos_long2 %>%
  mutate(Date = as.Date(Date))

Arbet_long = bind_rows(Arbetslos_long1, Arbetslos_long2)

Arbet_long = Arbet_long %>%
  mutate(YearMonth = format(Date, "%Y-%m")) %>%
  select(-Date)

KPI_long = KPI_long %>%
  mutate(YearMonth = format(Date, "%Y-%m")) %>%
  select(-Date)

combined_dataset = Filtered_dates %>%
  inner_join(OMXS30, by = "Date")


#make a new change column for the correct values in percentage and cabinet variable
combined_dataset = combined_dataset %>%
  arrange(Date) %>%
  mutate(ChangeV2 = (Price - lag(Price)) / lag(Price) * 100) %>%
  mutate(ChangeV2 = coalesce(ChangeV2, 0)) %>%
  mutate(Cabinet_coalition = case_when(
    Date >= as.Date("2022-10-18") ~ "Kristersson med stöd genom tidöavtalet (M+KD+L)",
    Date >= as.Date("2021-11-30") & Date < as.Date("2022-10-18") ~ "Andersson med stöd genom V + MP + C och vilde Kakabaveh(-V) (S)",
    Date >= as.Date("2021-07-09") & Date < as.Date("2021-11-30") ~ "Löfven III med stöd genom C och V samt vilde Kakabaveh(-V) (S+MP)",
    Date >= as.Date("2021-01-21") & Date < as.Date("2021-07-09") ~ "Löfven II med stöd av V samt genom Januariavtalet (JA) (S+MP)",
    Date >= as.Date("2014-10-03") & Date < as.Date("2019-01-21") ~ "Löfven I med stöd av V samt genom Decemberöverenskommenlsen (DÖ) (S+MP)",
    Date >= as.Date("2010-10-05") & Date < as.Date("2014-10-03") ~ "Reinfeldt Minoritet blandat (M+KD+L+C)",
    Date >= as.Date("2006-10-06") & Date < as.Date("2010-10-05") ~ "Reinfeldt Majoritet (M+KD+L+C)",
    Date >= as.Date("1998-10-05") & Date < as.Date("2006-10-06") ~ "Persson med stöd av V + MP (S)",
    Date >= as.Date("1996-03-22") & Date < as.Date("1998-10-05") ~ "Persson med stöd av C (S)",
    Date >= as.Date("1994-10-07") & Date < as.Date("1996-03-22") ~ "Carlsson III med stöd genom V och C (S)",
    Date >= as.Date("1991-10-04") & Date < as.Date("1994-10-07") ~ "Bildt med tidigt stöd av NYD (M+KD+L+C)",
    Date < as.Date("1991-10-04") ~ "Carlsson I & II med stöd av V (S)",
    TRUE ~ "Other"
  )
  )

datatable(combined_dataset)
datatable(Arbet_long)
datatable(KPI_long)

# %% [code] {"execution":{"iopub.status.busy":"2026-04-20T21:55:28.373797Z","iopub.execute_input":"2026-04-20T21:55:28.375380Z","iopub.status.idle":"2026-04-20T21:55:28.391706Z","shell.execute_reply":"2026-04-20T21:55:28.389932Z"},"jupyter":{"outputs_hidden":false}}
#skandaler dataset
skandaler = data.frame(
  Date = as.Date(c(
    "1987-03-01", "1988-06-01", "1989-03-01", "1990-02-15", "1993-09-01", 
    "1993-10-01", "1994-03-01", "1994-06-16", "1995-10-07", "1995-11-01", 
    "1996-06-01", "2000-09-01", "2003-01-01", "2004-05-17", "2005-01-01", 
    "2006-03-21", "2006-09-04", "2006-10-11", "2006-10-01", "2007-10-24", 
    "2010-07-07", "2010-12-01", "2011-10-07", "2012-03-06", "2012-11-14", 
    "2013-02-01", "2014-10-01", "2016-01-13", "2016-04-13", "2016-08-13", 
    "2017-07-06", "2019-04-01", "2020-12-30", "2021-07-01", "2021-12-01", 
    "2023-01-01", "2023-05-01", "2024-04-01", "2024-05-07", "2024-09-01",
    "2025-06-01", "2025-07-01", "2025-10-01", "2025-11-01"
  )),
  Skandal = c(
    "Boforsaffären", "Ebbe Carlsson-affären", "Lidboms KU-förhör", "Feldts avgång", "Allan Larsson-drevet",
    "Schymans alkoholerkännande", "Bildts Rysslandsresa", "Olof Johanssons avgång", "Tobleroneaffären", "Motalaskandalen",
    "Marjasinaffären", "Freivaldsaffären", "Schymanaffären", "Egyptenavvisningarna", "Tsunamibanden",
    "Stängningen av SD-sajt", "Dataintrångsaffären", "Ministerkrisen", "Bildts optioner", "Schenströmaffären",
    "Littorinaffären", "Primeaffären", "Juholtaffären", "Saudiaffären", "Järnrörsskandalen",
    "Nuon-affären", "Romsons husbåt", "Kommunalskandalen", "Kaplanskandalen", "Hadzialic-affären",
    "Transportstyrelsen", "M-toppars bostadsfiffel", "Eliassons Kanarieresa", "Buschs förtalsdom", "Karkiainen-affären",
    "Ålfiskeskandalen", "El-Haj-affären", "Kinberg Batra-affären", "Trollfabriken", "S-lotteriskandalen",
    "Mats Perssons avgång", "Johan Forssell-affären", "Biståndsskandalen Somalia", "Tidöpartiernas klimatnota"
  ),
  Parti = c(
    "S", "S", "S", "S", "S", 
    "V", "M", "C", "S", "S", 
    "S", "S", "V", "S", "S", 
    "S", "L", "M", "M", "M", 
    "M", "S", "S", "M", "SD", 
    "C", "MP", "S", "MP", "S", 
    "S", "M", "S", "KD", "S", 
    "M", "S", "M", "SD", "S",
    "L", "M", "M", "M"
  )
)

# %% [code] {"execution":{"iopub.status.busy":"2026-04-20T21:55:30.680978Z","iopub.execute_input":"2026-04-20T21:55:30.682561Z","iopub.status.idle":"2026-04-20T21:55:32.034201Z","shell.execute_reply":"2026-04-20T21:55:32.032264Z"},"scrolled":true,"jupyter":{"outputs_hidden":false}}
#Produce control variables (no unemployment or inflation)
#Ideology, year index and ISK dummy
combined_dataset = combined_dataset %>%
  mutate(ISK_dummy = as.numeric(Date >= as.Date("2012-01-01"))) %>%
  mutate(Year_index = year(Date) - min(year(Date), na.rm = TRUE)) %>%
  mutate(Months_til_elect = (9 - month(Date))%%12) %>%
  mutate(Ideology = case_when(
    Date >= as.Date("2022-10-18") ~ "Liberal-Conservatism",
    Date >= as.Date("2021-11-30") & Date < as.Date("2022-10-18") ~ "Social Democracy",
    Date >= as.Date("2021-07-09") & Date < as.Date("2021-11-30") ~ "Social Democracy / Green politics",
    Date >= as.Date("2021-01-21") & Date < as.Date("2021-07-09") ~ "Social Democracy / Social Liberalism",
    Date >= as.Date("2014-10-03") & Date < as.Date("2019-01-21") ~ "Social Democracy / Green politics",
    Date >= as.Date("2010-10-05") & Date < as.Date("2014-10-03") ~ "Liberal Conservatism",
    Date >= as.Date("2006-10-06") & Date < as.Date("2010-10-05") ~ "Liberal Conservatism",
    Date >= as.Date("1998-10-05") & Date < as.Date("2006-10-06") ~ "Social Democracy",
    Date >= as.Date("1996-03-22") & Date < as.Date("1998-10-05") ~ "Social Democracy",
    Date >= as.Date("1994-10-07") & Date < as.Date("1996-03-22") ~ "Social Democracy",
    Date >= as.Date("1991-10-04") & Date < as.Date("1994-10-07") ~ "Liberal Conservatism",
    Date < as.Date("1991-10-04") ~ "Social Democracy",
    TRUE ~ "Other")) %>%
  mutate(Honeymoon = as.integer(Ideology != lag(Ideology, default = first(Ideology)))) %>%
  mutate(Honeymoon = map_int(Date, ~ as.integer(any(Honeymoon == 1 & Date >= (.x %m-% months(5)) & Date <= .x)))) %>%
  mutate(Seasons = case_when(
    month(Date) %in% c(12,1,2) ~ "Winter",
    month(Date) %in% c(3,4,5) ~ "Spring",
    month(Date) %in% c(6,7,8) ~ "Summer",
    month(Date) %in% c(9,10,11) ~ "Fall"),
    value = 1) %>%
  pivot_wider(names_from = Seasons, values_from = value, values_fill = 0) %>%
  mutate(across(contains("parties."), ~ NA, .names = "{sub('parties.', 'Scandal_', .col, fixed=TRUE)}"))




datatable(combined_dataset)
#volatility control variable?????? VIX+???+

# %% [code] {"execution":{"iopub.status.busy":"2026-04-20T21:59:42.005398Z","iopub.execute_input":"2026-04-20T21:59:42.007224Z","iopub.status.idle":"2026-04-20T21:59:42.255407Z","shell.execute_reply":"2026-04-20T21:59:42.252941Z"},"jupyter":{"outputs_hidden":false}}
skandaler = skandaler %>%
  mutate(YearMonth = format(Date, "%Y-%m"))

combined_dataset = combined_dataset %>%
  mutate(YearMonth = format(Date, "%Y-%m"))

Scandals_wide = skandaler %>%
  select(YearMonth, Parti) %>%
  distinct() %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = Parti, names_prefix = "Scandal_", values_from = value, values_fill = 0)

combined_dataset = combined_dataset %>%
  select(-starts_with("Scandal_")) %>%
  left_join(Scandals_wide, by = "YearMonth") %>%
  mutate(across(starts_with("Scandal_"), ~replace_na(.x, 0)))

combined_dataset = combined_dataset %>%
  inner_join(Arbet_long, by = "YearMonth") %>%
  inner_join(KPI_long, by = "YearMonth")


datatable(combined_dataset)

write.csv(combined_dataset, "/kaggle/working/combined_dataset.csv", row.names = FALSE)

# %% [code] {"jupyter":{"outputs_hidden":false}}
#(all_regression = lapply(split(combined_dataset, combined_dataset$Cabinet), function(df) {
#  lm(Support_Change ~ Change, data = df)
#})
#
#lapply(all_regression, summary))

# %% [markdown] {"jupyter":{"outputs_hidden":false}}
# 

# %% [code] {"jupyter":{"outputs_hidden":false}}
