library(modelsummary)
library(fixest)
library(dplyr)

# Coefficient labels (consistent across all tables)
coef_map = c(
  "ret_7d" = "OMXS30 return (7d)",
  "ret_30d" = "OMXS30 return (30d)",
  "ret_60d" = "OMXS30 return (60d)",
  "ret_90d" = "OMXS30 return (90d)",
  "AKU" = "Unemployment",
  "YearChangeKPI" = "Inflation (YoY)",
  "Honeymoon" = "Honeymoon",
  "Months_til_elect" = "Months to election",
  "ISK_dummy" = "ISK regime",
  "Winter" = "Winter",
  "Spring" = "Spring",
  "Summer" = "Summer",
  "Scandal_cabinet" = "Cabinet scandal",
  "dSupport_min_L1" = "Lagged $\\Delta$Support"
)

gof_map = tibble::tribble(
  ~raw, ~clean, ~fmt,
  "nobs", "Observations", 0,
  "r.squared", "$R^2$", 3,
  "adj.r.squared", "Adj. $R^2$", 3,
  "within.r2", "Within $R^2$", 3,
  "rmse", "RMSE", 3
)

#table
main_models = list(
  "Bivariate" = m1,
  "+ Macro" = m2,
  "Full (30d)" = m3,
  "Full (60d)" = m4,
  "Full (90d)" = m5,
  "Full-bloc DV" = m6,
  "ARDL" = m3_ardl,
  "Driscoll-Kraay" = m3_dk
)

modelsummary(
  main_models,
  output = "latex",
  coef_map = coef_map,
  gof_map = gof_map,
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  fmt = 3,
  escape = FALSE,
  title = "Main specifications: OMXS30 returns and cabinet polling support, 1987--2026 \\label{tab:main_models}",
  notes = "Dependent variable: first difference in monthly cabinet polling support (minority-bloc parties), except column~6 which uses the full-bloc DV. All fixed-effects specifications include cabinet and polling-firm fixed effects. Standard errors clustered at the cabinet level, except column~8 (Driscoll--Kraay with $L=4$). Significance: $^{*}p<0.05$, $^{**}p<0.01$, $^{***}p<0.001$."
) %>%
  add_rows(
    tibble::tribble(
      ~term,        ~`Bivariate`, ~`+ Macro`, ~`Full (30d)`, ~`Full (60d)`, ~`Full (90d)`, ~`Full-bloc DV`, ~`ARDL`, ~`Driscoll-Kraay`,
      "Cabinet FE", "No", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
      "Firm FE", "No", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
      "SE", "Clust.", "Clust.", "Clust.", "Clust.", "Clust.", "Clust.", "Clust.","DK(4)"
    )
  )