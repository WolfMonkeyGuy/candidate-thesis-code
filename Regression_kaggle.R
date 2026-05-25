# %% [code] {"jupyter":{"outputs_hidden":false},"execution":{"iopub.status.busy":"2026-05-14T21:36:16.383746Z","iopub.execute_input":"2026-05-14T21:36:16.386375Z","iopub.status.idle":"2026-05-14T21:39:36.478896Z","shell.execute_reply":"2026-05-14T21:39:36.477122Z"}}
install.packages("modelsummary")
install.packages("lmtest")
install.packages("tseries")
install.packages("car")

# %% [code] {"execution":{"iopub.status.busy":"2026-05-15T22:39:43.297128Z","iopub.execute_input":"2026-05-15T22:39:43.298860Z","iopub.status.idle":"2026-05-15T22:39:58.222321Z","shell.execute_reply":"2026-05-15T22:39:58.220370Z"},"jupyter":{"outputs_hidden":false}}

# Stock market returns and ruling cabinet polling support — Sweden 1987–2026

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(lubridate); library(fixest); library(ggplot2)
  library(readxl); library(zoo); library(modelsummary)
  library(lmtest); library(tseries); library(car)
})

out = capture.output({
  df = read.csv("/kaggle/input/datasets/williambrjesson/derived-set-v2/derived_set_clean_v2.csv", stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date))
  
  # AKU.x == AKU.y and KPI.x == KPI.y because YearMonth was joined twice.
  df = df %>%
    select(-ends_with(".y")) %>%
    rename_with(~ sub("\\.x$", "", .x), ends_with(".x"))
  
  df = df %>%
    mutate(firm = str_squish(firm),
           firm = dplyr::recode(firm, "Inzio" = "Inizio"))
  
  
  df = df %>%
    mutate(Cabinet = case_when(
      Date >= as.Date("2022-10-18")                              ~ "Kristersson",
      Date >= as.Date("2021-11-30") & Date < as.Date("2022-10-18") ~ "Andersson",
      Date >= as.Date("2021-07-09") & Date < as.Date("2021-11-30") ~ "Lofven_III",
      Date >= as.Date("2019-01-21") & Date < as.Date("2021-07-09") ~ "Lofven_II",
      Date >= as.Date("2014-10-03") & Date < as.Date("2019-01-21") ~ "Lofven_I",
      Date >= as.Date("2010-10-05") & Date < as.Date("2014-10-03") ~ "Reinfeldt_II",
      Date >= as.Date("2006-10-06") & Date < as.Date("2010-10-05") ~ "Reinfeldt_I",
      Date >= as.Date("1998-10-05") & Date < as.Date("2006-10-06") ~ "Persson_III",
      Date >= as.Date("1996-03-22") & Date < as.Date("1998-10-05") ~ "Persson_II",
      Date >= as.Date("1994-10-07") & Date < as.Date("1996-03-22") ~ "Carlsson_III",
      Date >= as.Date("1991-10-04") & Date < as.Date("1994-10-07") ~ "Bildt",
      Date <  as.Date("1991-10-04")                               ~ "Carlsson_I_II"
    ))
  
  cab_min = list(
    Kristersson   = c("M","KD","L"),
    Andersson     = c("S"),
    Lofven_III    = c("S","MP"),
    Lofven_II     = c("S","MP"),
    Lofven_I      = c("S","MP"),
    Reinfeldt_II  = c("M","KD","L","C"),
    Reinfeldt_I   = c("M","KD","L","C"),
    Persson_III   = c("S"),
    Persson_II    = c("S"),
    Carlsson_III  = c("S"),
    Bildt         = c("M","KD","L","C"),
    Carlsson_I_II = c("S")
  )
  cab_full = list(
    Kristersson   = c("M","KD","L","SD"),
    Andersson     = c("S","V","MP","C"),
    Lofven_III    = c("S","MP","V","C"),
    Lofven_II     = c("S","MP","V","C","L"),
    Lofven_I      = c("S","MP","V"),
    Reinfeldt_II  = c("M","KD","L","C"),
    Reinfeldt_I   = c("M","KD","L","C"),
    Persson_III   = c("S","V","MP"),
    Persson_II    = c("S","C"),
    Carlsson_III  = c("S","V","C"),
    Bildt         = c("M","KD","L","C","NYD"),
    Carlsson_I_II = c("S","V")
  )
  
  compute_cab_support = function(data, parties_map) {
    vapply(seq_len(nrow(data)), function(i) {
      cb = data$Cabinet[i]
      if (is.na(cb) || !cb %in% names(parties_map)) return(NA_real_)
      cols = paste0("parties.", parties_map[[cb]])
      cols = cols[cols %in% names(data)]
      if (length(cols) == 0L) return(NA_real_)
      vals = suppressWarnings(as.numeric(data[i, cols]))
      sum(vals, na.rm = TRUE)   # NA treated as 0 = "party didn't exist yet"
    }, numeric(1))
  }
  df$Cabinet_support_min  = compute_cab_support(df, cab_min)
  df$Cabinet_support_full = compute_cab_support(df, cab_full)
  
  
  omx_daily = read_excel("/kaggle/input/datasets/williambrjesson/omxs30-1986-2026/OMXS30_1986_2026.xlsx", sheet = 1) %>%
    mutate(Date = as.Date(Date)) %>%
    select(Date, Price_daily = Price) %>%
    distinct(Date, .keep_all = TRUE) %>%
    arrange(Date)
  
  lookback_return = function(target_dates, lag_days) {
    te        = target_dates - lag_days
    idx_now   = findInterval(target_dates, omx_daily$Date)    
    idx_past  = findInterval(te,           omx_daily$Date)    
    idx_now[idx_now   == 0L] = NA_integer_
    idx_past[idx_past == 0L] = NA_integer_
    (omx_daily$Price_daily[idx_now] / omx_daily$Price_daily[idx_past] - 1) * 100
  }
  df = df %>%
    mutate(ret_7d  = lookback_return(Date, 7),
           ret_30d = lookback_return(Date, 30),
           ret_60d = lookback_return(Date, 60),
           ret_90d = lookback_return(Date, 90))
  
  
  df$Scandal_cabinet = vapply(seq_len(nrow(df)), function(i) {
    cb = df$Cabinet[i]
    if (is.na(cb) || !cb %in% names(cab_min)) return(0L)
    cols = paste0("Scandal_", cab_min[[cb]])
    cols = cols[cols %in% names(df)]
    if (length(cols) == 0L) return(0L)
    as.integer(any(suppressWarnings(as.numeric(df[i, cols])) > 0, na.rm = TRUE))
  }, integer(1))
  
  
  df = df %>%
    arrange(firm, Date) %>%
    group_by(firm, Cabinet) %>%
    mutate(dSupport_min     = Cabinet_support_min  - lag(Cabinet_support_min),
           dSupport_full    = Cabinet_support_full - lag(Cabinet_support_full),
           dSupport_min_L1  = lag(dSupport_min),
           dSupport_min_L2  = lag(dSupport_min, n=2),
           dSupport_full_L1 = lag(dSupport_full),
           days_since       = as.numeric(Date - lag(Date))) %>%
    ungroup()
  
  
  # Model 1 — bivariate
  m1 = feols(dSupport_min ~ ret_30d,
             data = df, cluster = ~Cabinet)
  
  # Model 2 — add macro controls
  m2 = feols(dSupport_min ~ ret_30d + AKU + YearChangeKPI,
             data = df, cluster = ~Cabinet)
  
  # Model 3 — full specification with fixed effects
  m3 = feols(dSupport_min ~ ret_30d + AKU + YearChangeKPI +
               Honeymoon + Months_til_elect + ISK_dummy +
               Winter + Spring + Summer +
               Scandal_cabinet |
               firm + Cabinet,
             data = df, cluster = ~Cabinet)
  
  # Model 3 ARDL
  
  m3_ardl = feols(dSupport_min ~ dSupport_min_L1 + ret_30d + AKU + YearChangeKPI +
                    Honeymoon + Months_til_elect + ISK_dummy +
                    Winter + Spring + Summer +
                    Scandal_cabinet |
                    firm + Cabinet,
                  data = df, cluster = ~Cabinet)
  
  # Model 3.2 ARDL 
  m3_ardl_2 = feols(dSupport_min ~ dSupport_min_L1 + dSupport_min_L2 + ret_30d + AKU + YearChangeKPI +
                      Honeymoon + Months_til_elect + ISK_dummy +
                      Winter + Spring + Summer +
                      Scandal_cabinet |
                      firm + Cabinet,
                    data = df, cluster = ~Cabinet)
  
  # Model 3b — same as M3 but without macro controls 
  m3_nomacro = feols(dSupport_min ~ ret_30d +
                       Honeymoon + Months_til_elect + ISK_dummy +
                       Winter + Spring + Summer +
                       Scandal_cabinet |
                       firm + Cabinet,
                     data = df, cluster = ~Cabinet)
  
  # Model 4 — same controls, 60-day return (robustness)
  m4 = feols(dSupport_min ~ ret_60d + AKU + YearChangeKPI +
               Honeymoon + Months_til_elect + ISK_dummy +
               Winter + Spring + Summer +
               Scandal_cabinet |
               firm + Cabinet,
             data = df, cluster = ~Cabinet)
  
  # Model 5 — 90-day return
  m5 = feols(dSupport_min ~ ret_90d + AKU + YearChangeKPI +
               Honeymoon + Months_til_elect + ISK_dummy +
               Winter + Spring + Summer +
               Scandal_cabinet |
               firm + Cabinet,
             data = df, cluster = ~Cabinet)
  
  # Model 6 — robustness: full supporting bloc as DV
  m6 = feols(dSupport_full ~ ret_30d + AKU + YearChangeKPI +
               Honeymoon + Months_til_elect + ISK_dummy +
               Winter + Spring + Summer +
               Scandal_cabinet |
               firm + Cabinet,
             data = df, cluster = ~Cabinet)
  
  cat("\n\n### MAIN TABLE: first-differenced cabinet support ###\n")
  print(etable(m1, m2, m3, m3_ardl, m3_nomacro, m4, m5, m6,
               headers = c("Bivar","+Macro","Full 30d","Full 30d\nARDL",
                           "Full 30d\n(no macro)","Full 60d","Full 90d","Full-bloc"),
               digits = 3))
  
  # Model 7 — interaction: effect by cabinet
  # Restrict to cabinets with enough observations to estimate a slope.
  big_cabs = df %>%
    filter(!is.na(dSupport_min), !is.na(ret_30d)) %>%
    count(Cabinet) %>% filter(n >= 20) %>% pull(Cabinet)
  
  m7 = feols(dSupport_min ~ i(Cabinet, ret_30d, ref = "Lofven_I") + ret_30d +
               AKU + YearChangeKPI + Honeymoon + Months_til_elect +
               ISK_dummy + Winter + Spring + Summer + Scandal_cabinet |
               firm + Cabinet,
             data = filter(df, Cabinet %in% big_cabs),
             cluster = ~Cabinet)
  
  cat("\n\n### INTERACTION: ret_30d × Cabinet ###\n")
  print(summary(m7))
  
  # Cabinet-specific slopes (cleaner to read than interaction dummies).
  # NOTE: n >= 20 filter excludes six short-lived cabinets
  
  cat("\n\n### Cabinet-specific slopes on ret_30d ###\n")
  for (cb in sort(big_cabs)) {
    sub = df %>% filter(Cabinet == cb, !is.na(dSupport_min), !is.na(ret_30d))
    if (nrow(sub) < 10) next
    fit = feols(dSupport_min ~ ret_30d, data = sub)
    co  = coef(fit)["ret_30d"]; se = sqrt(diag(vcov(fit)))["ret_30d"]
    cat(sprintf("  %-15s  n=%3d   beta = %+6.4f   SE = %.4f   t = %+5.2f\n",
                cb, nrow(sub), co, se, co/se))
  }
  
  
  
  m_isk = feols(dSupport_min ~ ret_30d * ISK_dummy +
                  AKU + YearChangeKPI + Honeymoon + Months_til_elect +
                  Winter + Spring + Summer + Scandal_cabinet |
                  firm,
                data = df, cluster = ~Cabinet)
  
  # Same ISK interaction, dropping AKU/KPI
  m_isk_nomacro = feols(dSupport_min ~ ret_30d * ISK_dummy +
                          Honeymoon + Months_til_elect +
                          Winter + Spring + Summer + Scandal_cabinet |
                          firm,
                        data = df, cluster = ~Cabinet)
  
  cat("\n\n### ISK INTERACTION: ret_30d × ISK_dummy ###\n")
  print(etable(m_isk, m_isk_nomacro,
               headers = c("With macro", "No macro"),
               digits = 3))
  print(summary(m_isk_nomacro))
  
  b = coef(m_isk); V = vcov(m_isk)
  ir = which(names(b) == "ret_30d")
  ix = which(names(b) == "ret_30d:ISK_dummy")
  post_slope = b[ir] + b[ix]
  post_se    = sqrt(V[ir,ir] + V[ix,ix] + 2 * V[ir,ix])
  cat(sprintf("\n  Pre-ISK slope : %+.4f\n", b[ir]))
  cat(sprintf("  Post-ISK slope: %+.4f  (SE %.4f, t = %+.2f)\n",
              post_slope, post_se, post_slope / post_se))
  
  # Joint F-test
  cat("\n  Joint Wald test: ISK_dummy = ret_30d:ISK_dummy = 0\n  ")
  print(wald(m_isk, keep = c("ISK_dummy", "ret_30d:ISK_dummy")))
  
  cat("\n\n### Phase-in robustness (later ISK cutoffs) ###\n")
  for (cut in as.Date(c("2013-01-01","2014-01-01","2015-01-01"))) {
    df_alt = df %>% mutate(ISK_alt = as.integer(Date >= cut))
    mi = feols(dSupport_min ~ ret_30d * ISK_alt +
                 AKU + YearChangeKPI + Honeymoon + Months_til_elect +
                 Winter + Spring + Summer + Scandal_cabinet |
                 firm,
               data = df_alt, cluster = ~Cabinet)
    b2 = coef(mi)["ret_30d:ISK_alt"]
    s2 = sqrt(diag(vcov(mi)))["ret_30d:ISK_alt"]
    cat(sprintf("  cutoff = %s   β(ret_30d:ISK_alt) = %+.4f  (SE %.4f, t = %+.2f)\n",
                cut, b2, s2, b2 / s2))
  }
  
  # Loss aversion
  df = df %>% mutate(neg_ret = as.integer(ret_30d < 0))
  m_isk_asym = feols(dSupport_min ~ ret_30d * ISK_dummy * neg_ret +
                       AKU + YearChangeKPI + Honeymoon + Months_til_elect +
                       Winter + Spring + Summer + Scandal_cabinet |
                       firm,
                     data = df, cluster = ~Cabinet)
  cat("\n\n### LOSS AVERSION: ret_30d × ISK_dummy × I(ret_30d<0) ###\n")
  print(summary(m_isk_asym))
  cat("\n  Key coefficient is `ret_30d:ISK_dummy:neg_ret`. If positive & significant,\n",
      " losses hurt cabinet support more post-ISK than pre-ISK.\n", sep="")
  
  
  m_w7  = feols(dSupport_min ~ ret_7d  + AKU + YearChangeKPI + Honeymoon +
                  Months_til_elect + ISK_dummy + Winter + Spring + Summer +
                  Scandal_cabinet | firm + Cabinet,
                data = df, cluster = ~Cabinet)
  cat("\n\n### ALTERNATIVE RETURN WINDOWS ###\n")
  print(etable(m_w7, m3, m4, m5,
               headers = c("7-day","30-day","60-day","90-day"),
               digits = 3))
  
  
  df = df %>%
    mutate(ret_pos = pmax(ret_30d, 0),
           ret_neg = pmin(ret_30d, 0))
  
  m_asym = feols(dSupport_min ~ ret_pos + ret_neg +
                   AKU + YearChangeKPI + Honeymoon + Months_til_elect +
                   ISK_dummy + Winter + Spring + Summer + Scandal_cabinet |
                   firm + Cabinet,
                 data = df, cluster = ~Cabinet)
  cat("\n\n### LOSS AVERSION (main effect): asymmetric slopes ###\n")
  print(summary(m_asym))
  
  b_a  = coef(m_asym); V_a = vcov(m_asym)
  diff = b_a["ret_pos"] - b_a["ret_neg"]
  se_d = sqrt(V_a["ret_pos","ret_pos"] + V_a["ret_neg","ret_neg"]
              - 2 * V_a["ret_pos","ret_neg"])
  cat(sprintf("\n  β(ret_pos) − β(ret_neg) = %+.4f   SE = %.4f   t = %+.2f   p = %.3f\n",
              diff, se_d, diff / se_d, 2 * (1 - pnorm(abs(diff / se_d)))))
  
  
  df = df %>% mutate(
    months_til_c = Months_til_elect - mean(Months_til_elect, na.rm = TRUE))
  
  m_elect = feols(dSupport_min ~ ret_30d * months_til_c +
                    AKU + YearChangeKPI + Honeymoon +
                    ISK_dummy + Winter + Spring + Summer + Scandal_cabinet |
                    firm + Cabinet,
                  data = df, cluster = ~Cabinet)
  cat("\n\n### ELECTION-PROXIMITY INTERACTION: ret_30d × Months_til_elect ###\n")
  print(summary(m_elect))
  cat("  Sign interpretation: negative β(ret_30d:months_til_c) ⇒ stock effects\n",
      " get stronger as elections approach.\n", sep = "")
  
  
  m_honey = feols(dSupport_min ~ ret_30d * Honeymoon +
                    AKU + YearChangeKPI + Months_til_elect +
                    ISK_dummy + Winter + Spring + Summer + Scandal_cabinet |
                    firm + Cabinet,
                  data = df, cluster = ~Cabinet)
  cat("\n\n### HONEYMOON INTERACTION: ret_30d × Honeymoon ###\n")
  print(summary(m_honey))
  
  
  s_led = c("Carlsson_I_II","Carlsson_III","Persson_II","Persson_III",
            "Lofven_I","Lofven_II","Lofven_III","Andersson")
  df = df %>% mutate(Right_led = as.integer(!Cabinet %in% s_led))
  
  m_ideol = feols(dSupport_min ~ ret_30d * Right_led +
                    AKU + YearChangeKPI + Honeymoon + Months_til_elect +
                    ISK_dummy + Winter + Spring + Summer + Scandal_cabinet |
                    firm,
                  data = df, cluster = ~Cabinet)
  cat("\n\n### IDEOLOGY INTERACTION: ret_30d × Right_led ###\n")
  print(summary(m_ideol))
  cat("  Positive β(ret_30d:Right_led) ⇒ right-led cabinets gain/lose more from\n",
      " the market than S-led cabinets do. (Cabinet FE dropped — collinear.)\n", sep="")
  
  
  omx_daily = omx_daily %>%
    mutate(ret_daily = Price_daily / lag(Price_daily) - 1,
           vol_90d   = zoo::rollapply(ret_daily, width = 90, FUN = sd,
                                      fill = NA, align = "right") *
             sqrt(252) * 100)
  idx_v = findInterval(df$Date, omx_daily$Date)
  idx_v[idx_v == 0L] = NA_integer_
  df$vol_90d = omx_daily$vol_90d[idx_v]
  df = df %>% mutate(
    HighVol = as.integer(vol_90d > median(vol_90d, na.rm = TRUE)))
  
  m_vol = feols(dSupport_min ~ ret_30d * HighVol +
                  AKU + YearChangeKPI + Honeymoon + Months_til_elect +
                  ISK_dummy + Winter + Spring + Summer + Scandal_cabinet |
                  firm + Cabinet,
                data = df, cluster = ~Cabinet)
  cat("\n\n### VOLATILITY-REGIME INTERACTION: ret_30d × HighVol ###\n")
  print(summary(m_vol))
  cat("  Positive β(ret_30d:HighVol) ⇒ returns move polling more during\n",
      " high-volatility periods (news-salience hypothesis).\n", sep="")
  
  m_postisk      = feols(dSupport_min ~ ret_30d +
                           AKU + YearChangeKPI + Honeymoon + Months_til_elect +
                           Winter + Spring + Summer + Scandal_cabinet |
                           firm + Cabinet,
                         data = filter(df, ISK_dummy == 1),
                         cluster = ~Cabinet)
  m_postisk_asym = feols(dSupport_min ~ ret_pos + ret_neg +
                           AKU + YearChangeKPI + Honeymoon + Months_til_elect +
                           Winter + Spring + Summer + Scandal_cabinet |
                           firm + Cabinet,
                         data = filter(df, ISK_dummy == 1),
                         cluster = ~Cabinet)
  cat("\n\n### POST-ISK SUBSAMPLE (ISK_dummy == 1 only) ###\n")
  print(etable(m_postisk, m_postisk_asym,
               headers = c("Post-ISK, symmetric", "Post-ISK, asymmetric"),
               digits = 3))
  
  m7_90d = feols(dSupport_min ~ i(Cabinet, ret_90d, ref = "Lofven_I") + ret_90d +
                   AKU + YearChangeKPI + Honeymoon + Months_til_elect +
                   ISK_dummy + Winter + Spring + Summer + Scandal_cabinet |
                   firm + Cabinet,
                 data = filter(df, Cabinet %in% big_cabs),
                 cluster = ~Cabinet)
  cat("\n\n### CABINET HETEROGENEITY (90-day window) ###\n")
  print(summary(m7_90d))
  
  cat("\n\n### Residual autocorrelation (Model 3, no lagged DV) ###\n")
  res = residuals(m3)
  cat(sprintf("  ACF(1) = %.3f   ACF(2) = %.3f   ACF(5) = %.3f\n",
              acf(res, plot = FALSE, lag.max = 5)$acf[2],
              acf(res, plot = FALSE, lag.max = 5)$acf[3],
              acf(res, plot = FALSE, lag.max = 5)$acf[6]))
  
  
  cat("\n\n### Breusch–Godfrey on M3 ARDL residuals ###\n")
  res_ardl = residuals(m3_ardl)
  for (ord in c(1, 4, 8)) {
    bg = bgtest(res_ardl ~ 1, order = ord)
    cat(sprintf("  order = %d:  LM = %.3f   df = %d   p = %.4f\n",
                ord, bg$statistic, bg$parameter, bg$p.value))
  }
  
  cat("\n\n### ACF on M3 ARDL residuals ###\n")
  cat(sprintf("  ACF(1) = %.3f   ACF(2) = %.3f   ACF(5) = %.3f\n",
              acf(res_ardl, plot = FALSE, lag.max = 5)$acf[2],
              acf(res_ardl, plot = FALSE, lag.max = 5)$acf[3],
              acf(res_ardl, plot = FALSE, lag.max = 5)$acf[6]))
  
  
  cat("\n\n### Augmented Dickey–Fuller tests on level variables ###\n")
  adf_series = df %>%
    distinct(Date, .keep_all = TRUE) %>%
    arrange(Date) %>%
    select(Date, AKU, YearChangeKPI)
  
  for (v in c("AKU", "YearChangeKPI")) {
    x = na.omit(adf_series[[v]])
    if (length(x) < 10) { cat(sprintf("  %s: too few obs\n", v)); next }
    a = suppressWarnings(adf.test(x))
    cat(sprintf("  %-14s  Dickey-Fuller = %.3f   lag = %d   p = %.4f\n",
                v, a$statistic, a$parameter, a$p.value))
  }
  cat("  Null = unit root. Reject (p < 0.05) means the series is stationary.\n")
  
  
  cat("\n\n### Variance Inflation Factors (M3 controls) ###\n")
  vif_lm = lm(dSupport_min ~ ret_30d + AKU + YearChangeKPI + Honeymoon +
                Months_til_elect + ISK_dummy + Winter + Spring + Summer +
                Scandal_cabinet, data = df)
  print(round(vif(vif_lm), 2))
  cat("  Rule of thumb: VIF > 5 attention, VIF > 10 problem.\n")
  
  
  cat("\n\n### Model 3 with Driscoll–Kraay SE (lag = 4) ###\n")
  m3_dk = feols(dSupport_min ~ ret_30d + AKU + YearChangeKPI +
                  Honeymoon + Months_til_elect + ISK_dummy +
                  Winter + Spring + Summer +
                  Scandal_cabinet |
                  firm + Cabinet,
                data = df, panel.id = ~firm + Date,
                vcov = DK(4))
  print(summary(m3_dk))
  
  cat("\n\n### Headline models side-by-side: M3, M3 ARDL, Driscoll–Kraay ###\n")
  print(etable(m3, m3_ardl, m3_dk,
               headers = c("M3: Cluster","M3 ARDL: Cluster","M3: Driscoll–Kraay(4)"),
               digits = 3))
  
  
  
  # Group the models
  models_main = list("Bivar"=m1, "+Macro"=m2, "Full 30d"=m3, "Full 30d ARDL"=m3_ardl,
                     "Full 30d (no)"=m3_nomacro, "Full 60d"=m4, "Full 90d"=m5,
                     "Full-bloc"=m6)
  models_cab = list("30-day Cabinet"=m7, "90-day Cabinet"=m7_90d)
  models_isk = list("ISK w/ Macro"=m_isk, "ISK No Macro"=m_isk_nomacro, "ISK Loss Aversion"=m_isk_asym)
  models_interact = list("Election Proximity"=m_elect, "Honeymoon"=m_honey, "Right-led"=m_ideol, "High Volatility"=m_vol)
  models_return = list("7-day Window"=m_w7, "Loss Aversion (Main)"=m_asym)
  models_postisk = list("Post-ISK Sym"=m_postisk, "Post-ISK Asym"=m_postisk_asym)
  models_diag = list("M3 Cluster"=m3, "M3 ARDL"=m3_ardl, "M3 Driscoll-Kraay(4)"=m3_dk)
  models_ALL = list("Bivar"=m1, "+Macro"=m2, "Full 30d"=m3, "Full 30d ARDL"=m3_ardl,
                    "Full 30d (no)"=m3_nomacro, "Full 60d"=m4, "Full 90d"=m5,
                    "Full-bloc"=m6, "30-day Cabinet"=m7, "90-day Cabinet"=m7_90d, 
                    "ISK w/ Macro"=m_isk, "ISK No Macro"=m_isk_nomacro, "ISK Loss Aversion"=m_isk_asym,
                    "lection Proximity"=m_elect, "Honeymoon"=m_honey, "Right-led"=m_ideol, "High Volatility"=m_vol,
                    "7-day Window"=m_w7, "Loss Aversion (Main)"=m_asym, "Post-ISK Sym"=m_postisk, "Post-ISK Asym"=m_postisk_asym,
                    "M3 Cluster"=m3, "M3 ARDL"=m3_ardl, "M3 Driscoll-Kraay(4)"=m3_dk)
  
  # Display them directly in your Kaggle notebook (renders as rich HTML)
  modelsummary(models_main, stars = TRUE, title = "Main Models", output = "/kaggle/working/main_models.html")
  modelsummary(models_cab, stars = TRUE, title = "Cabinet Heterogeneity", output = "/kaggle/working/cab_models.html")
  modelsummary(models_isk, stars = TRUE, title = "ISK Mechanisms", output = "/kaggle/working/isk_models.html")
  modelsummary(models_interact, stars = TRUE, title = "Additional Interactions", output = "/kaggle/working/interact_models.html")
  modelsummary(models_return, stars = TRUE, title = "Return Windows & Asymmetric Slopes", output = "/kaggle/working/return_models.html")
  modelsummary(models_postisk, stars = TRUE, title = "Post-ISK Subsample (2012+)", output = "/kaggle/working/post_isk_models.html")
  modelsummary(models_diag, stars = TRUE, title = "Robust Standard Errors Comparison", output = "/kaggle/working/diag_models.html")
  modelsummary(models_ALL, stars = TRUE, title = "ALL models", output = "/kaggle/working/ALL_models_derived_from_god_v2.html")
  
  
})
writeLines(out, "/kaggle/working/full_output_derived_from_god_v2.txt")
write.csv(df, "/kaggle/working/fulldataset_maybe.csv")

# %% [code] {"execution":{"iopub.status.busy":"2026-05-15T22:41:45.623309Z","iopub.execute_input":"2026-05-15T22:41:45.624945Z","iopub.status.idle":"2026-05-15T22:41:47.614933Z","shell.execute_reply":"2026-05-15T22:41:47.613129Z"},"jupyter":{"outputs_hidden":false}}

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

# Build the table
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
)

# %% [code] {"execution":{"iopub.status.busy":"2026-05-15T23:16:02.981097Z","iopub.execute_input":"2026-05-15T23:16:02.982653Z","iopub.status.idle":"2026-05-15T23:16:04.726976Z","shell.execute_reply":"2026-05-15T23:16:04.725138Z"},"jupyter":{"outputs_hidden":false}}
interaction_models = list(
  "Loss aversion" = m_isk_asym,
  "ISK regime" = m_isk,
  "High volatility" = m_vol,
  "Honeymoon" = m_honey,
  "Right-led" = m_ideol,
  "Election proximity" = m_elect
)

coef_map_int = c(
  "ret_30d" = "OMXS30 return (30d)",
  "ret_pos" = "Return (positive)",
  "ret_neg" = "Return (negative)",
  "ret_30d:ISK_dummy" = "Return $\\times$ ISK",
  "ret_30d:HighVol" = "Return $\\times$ High vol.",
  "ret_30d:Honeymoon" = "Return $\\times$ Honeymoon",
  "ret_30d:Right_led" = "Return $\\times$ Right-led",
  "ret_30d:months_til_c" = "Return $\\times$ Months to elect.",
  "HighVol" = "High volatility",
  "Right_led" = "Right-led",
  "ISK_dummy" = "ISK regime",
  "Honeymoon" = "Honeymoon",
  "Months_til_elect" = "Months to election"
)

modelsummary(
  interaction_models,
  output = "latex",
  coef_map = coef_map_int,
  gof_map = gof_map,
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  fmt = 3,
  escape = FALSE,
  coef_omit = "AKU|YearChangeKPI|Winter|Spring|Summer|Scandal_cabinet",
  title = "Conditioning effects on the return-support relationship \\label{tab:interactions}",
  notes = "All models include cabinet and polling-firm fixed effects, the full macroeconomic and political-cycle control set, and seasonal indicators (coefficients on controls suppressed). Standard errors clustered at the cabinet level. Significance: $^{*}p<0.05$, $^{**}p<0.01$, $^{***}p<0.001$."
)

# %% [code] {"execution":{"iopub.status.busy":"2026-05-15T23:19:01.877270Z","iopub.execute_input":"2026-05-15T23:19:01.878880Z","iopub.status.idle":"2026-05-15T23:19:01.937577Z","shell.execute_reply":"2026-05-15T23:19:01.935786Z"},"jupyter":{"outputs_hidden":false}}
cabinet_slopes = tibble::tribble(
  ~Cabinet, ~n, ~beta, ~se, ~t,
  "Persson III", 433L, 0.0028, 0.0166, 0.17,
  "Reinfeldt I", 279L, 0.0138, 0.0194, 0.71,
  "Reinfeldt II", 384L, -0.0001, 0.0261, -0.00,
  "Löfven I", 446L, 0.0393, 0.0182, 2.16,
  "Löfven II", 105L, -0.0363, 0.0306, -1.19,
  "Andersson", 54L, -0.0952, 0.0585, -1.63,
  "Kristersson", 57L, -0.0230, 0.0544, -0.42
)

kable(
  cabinet_slopes,
  format = "latex",
  booktabs = TRUE,
  digits = c(0, 0, 4, 4, 2),
  col.names = c("Cabinet", "$n$", "$\\hat{\\beta}_{\\text{ret\\_30d}}$", "SE", "$t$"),
  escape = FALSE,
  caption = "Cabinet-specific within-cabinet slopes on the 30-day OMXS30 return. \\label{tab:cabinet_slopes}"
) %>%
  kable_styling(latex_options = "hold_position") %>%
  row_spec(4, bold = TRUE) %>%
  footnote(
    general = "Each row is a separate OLS regression of the first-differenced polling support on the 30-day return, the full control set and polling-firm fixed effects, restricted to the cabinet in question. Bolded row indicates the only slope significantly different from zero at the 5\\% level.",
    threeparttable = TRUE,
    escape = FALSE
  )