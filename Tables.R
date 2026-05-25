library(readr)
library(dplyr)
library(lubridate)
library(knitr)
library(kableExtra)

df = read_csv("", show_col_types = FALSE)

# Swedish month abbreviations
sv_month = c("jan", "feb", "mar", "apr", "maj", "jun",
              "jul", "aug", "sep", "okt", "nov", "dec")

fmt_month = function(x) {
  paste(sv_month[month(x)], year(x))
}

cabinet_order = c(
  "Carlsson_I_II",
  "Bildt",
  "Carlsson_III",
  "Persson_III",
  "Reinfeldt_I",
  "Reinfeldt_II",
  "Lofven_I",
  "Lofven_II",
  "Lofven_III",
  "Andersson",
  "Kristersson"
)

cabinet_labels = c(
  "Carlsson_I_II" = "Carlsson I & II",
  "Bildt" = "Bildt",
  "Carlsson_III" = "Carlsson III",
  "Persson_III" = "Persson III",
  "Reinfeldt_I" = "Reinfeldt I",
  "Reinfeldt_II" = "Reinfeldt II",
  "Lofven_I" = "Löfven I",
  "Lofven_II" = "Löfven II",
  "Lofven_III" = "Löfven III",
  "Andersson" = "Andersson",
  "Kristersson" = "Kristersson"
)

# Define the estimation sample.
est = df %>%
  mutate(
    Date = as.Date(Date),
    Cabinet = na_if(Cabinet, ""),
    firm = na_if(firm, "")
  ) %>%
  filter(
    Cabinet %in% cabinet_order,
    !is.na(Date),
    !is.na(firm),
    !is.na(Cabinet_support_min),
    !is.na(dSupport_min)
  )

coverage_table = est %>%
  mutate(
    Cabinet = factor(Cabinet, levels = cabinet_order),
    Cabinet_label = cabinet_labels[as.character(Cabinet)]
  ) %>%
  group_by(Cabinet, Cabinet_label) %>%
  summarise(
    Period = paste0(fmt_month(min(Date)), "--", fmt_month(max(Date))),
    Polls = n(),
    Firms = n_distinct(firm),
    .groups = "drop"
  ) %>%
  arrange(Cabinet) %>%
  mutate(`% of sample` = sprintf("%.1f", 100 * Polls / sum(Polls))) %>%
  select(
    Cabinet = Cabinet_label,
    Period,
    Polls,
    Firms,
    `% of sample`
  )

latex_table = kable(
  coverage_table,
  format = "latex",
  booktabs = TRUE,
  linesep = "",
  align = c("l", "l", "r", "r", "r"),
  caption = "Polling coverage by cabinet in the estimation sample",
  label = "tab:polling_coverage",
  escape = TRUE
) %>%
  kable_styling(
    position = "center",
    latex_options = "hold_position"
  )

cat(latex_table)