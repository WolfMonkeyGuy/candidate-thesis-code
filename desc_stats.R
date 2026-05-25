library(DT)
library(readxl)
library(dplyr)
library(modelsummary)
library(knitr)
library(kableExtra)

df = read.csv("", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date))
omx = read_excel("")
datatable(df)
datatable(omx)

valid_points = list(!is.na(frame$firm))
valid_points
unique(frame$firm)


est_sample = df %>%
  filter(!is.na(dSupport_min),
         !is.na(ret_30d),
         !is.na(AKU),
         !is.na(YearChangeKPI),
         !is.na(Months_til_elect))

datasummary(
  (`$\\Delta$Cabinet support` = dSupport_min) +
    (`OMXS30 return, 30-day (\\%)` = ret_30d) +
    (`OMXS30 return, 60-day (\\%)` = ret_60d) +
    (`OMXS30 return, 90-day (\\%)` = ret_90d) +
    (`Unemployment (AKU)` = AKU) +
    (`CPI year-on-year change` = YearChangeKPI) +
    (`Months to next election` = Months_til_elect) +
    (`Honeymoon period (0/1)` = Honeymoon) +
    (`ISK era, post-2012 (0/1)` = ISK_dummy) +
    (`Cabinet scandal (0/1)` = Scandal_cabinet)
  ~ N + Mean + SD + Min + Max,
  data = est_sample,
  output = "latex",
  fmt = 2,
  title = "Summary statistics for the estimation sample"
)


est_sample = df %>%
  filter(!is.na(dSupport_min),
         !is.na(ret_30d),
         !is.na(AKU),
         !is.na(YearChangeKPI),
         !is.na(Months_til_elect))

# Chronological cabinet order
cab_order = c("Carlsson_I_II","Bildt","Carlsson_III","Persson_II","Persson_III",
              "Reinfeldt_I","Reinfeldt_II","Lofven_I","Lofven_II","Lofven_III",
              "Andersson","Kristersson")
cab_labels = c("Carlsson I \\& II","Bildt","Carlsson III","Persson II","Persson III",
               "Reinfeldt I","Reinfeldt II","Löfven I","Löfven II","Löfven III",
               "Andersson","Kristersson")

cab_table = est_sample %>%
  group_by(Cabinet) %>%
  summarise(N_polls = n(),
            N_firms = n_distinct(firm),
            Start = min(Date),
            End = max(Date),
            .groups = "drop") %>%
  mutate(Cabinet = factor(Cabinet, levels = cab_order, labels = cab_labels),
         Share = sprintf("%.1f", 100 * N_polls / sum(N_polls)),
         Tenure = sprintf("%s--%s", format(Start, "%b %Y"), format(End, "%b %Y"))) %>%
  arrange(Cabinet) %>%
  select(Cabinet, Tenure, N_polls, N_firms, Share)

# LaTeX output for Overleaf
cab_table %>%
  kable("latex", booktabs = TRUE, linesep = "",
        col.names = c("Cabinet","Period","Polls","Firms","\\% of sample"),
        align = c("l","l","r","r","r"),
        caption = "Polling coverage by cabinet in the estimation sample",
        label = "tab:cabpolls",
        escape = FALSE) %>%
  kable_styling(latex_options = c("booktabs","hold_position"))

