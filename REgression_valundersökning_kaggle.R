# %% [code] {"execution":{"iopub.status.busy":"2026-04-26T21:42:12.741892Z","iopub.execute_input":"2026-04-26T21:42:12.743050Z","iopub.status.idle":"2026-04-26T21:42:20.043319Z","shell.execute_reply":"2026-04-26T21:42:20.041840Z"}}
install.packages("tidyverse")

# %% [code] {"_execution_state":"idle","execution":{"iopub.status.busy":"2026-04-26T21:42:42.351109Z","iopub.execute_input":"2026-04-26T21:42:42.352569Z","iopub.status.idle":"2026-04-26T21:42:43.981153Z","shell.execute_reply":"2026-04-26T21:42:43.979868Z"}}
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(modelsummary)
  library(marginaleffects)
  library(sandwich)
  library(lmtest)
  library(fixest)
  library(stringr)  
})

# %% [code] {"execution":{"iopub.status.busy":"2026-04-26T21:44:23.566325Z","iopub.execute_input":"2026-04-26T21:44:23.567799Z","iopub.status.idle":"2026-04-26T21:44:24.193084Z","shell.execute_reply":"2026-04-26T21:44:24.191865Z"}}

VU2022 = read_excel("/kaggle/input/datasets/williambrjesson/vu2022cses/VU2022_CSES.xlsx")

# Build labelled factor for q371b
trade_levels = c(
  "Aldrig",
  "Någon gång senaste 12 mån",
  "Någon gång senaste 6 mån",
  "Någon gång senaste 3 mån",
  "Någon gång senaste månaden",
  "Någon gång i veckan",
  "Flera gånger i veckan"
)

plot_df = VU2022 %>%
  filter(!is.na(q371b)) %>%
  mutate(q371b_lbl = factor(q371b, levels = 1:7, labels = trade_levels))

# Bar chart with percentages
ggplot(plot_df, aes(x = q371b_lbl)) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count)) * 100),
           fill = "steelblue") +
  geom_text(aes(y = after_stat(count) / sum(after_stat(count)) * 100,
                label = sprintf("%.1f%%", after_stat(count) / sum(after_stat(count)) * 100)),
            stat = "count", vjust = -0.4, size = 3.2) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  labs(title = "Hur ofta har du handlat på börsen senaste 12 månaderna?",
       subtitle = sprintf("SNES 2022 CSES, n = %s", format(nrow(plot_df), big.mark = " ")),
       x = NULL, y = "Andel (%)") +
  theme_minimal(base_size = 11) +
  theme(plot.title.position = "plot")



trade_levels = c(
  "Förbättrats Mycket",
  "Förbättrats Något",
  "Varken Eller",
  "Försämrats Något",
  "Försämrats Mycket"
)

plot_grej = VU2022 %>%
  filter(!is.na(q349)) %>%
  mutate(q349_lbl = factor(q349, levels = 1:5, labels = trade_levels))


ggplot(plot_grej, aes(x = q349_lbl)) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count)) * 100),
           fill = "steelblue") +
  geom_text(aes(y = after_stat(count) / sum(after_stat(count)) * 100,
                label = sprintf("%.1f%%", after_stat(count) / sum(after_stat(count)) * 100)),
            stat = "count", vjust = -0.4, size = 3.2) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  labs(title = "Hur tycker du det ekonomiska läget i Sverige har utvecklats senaste 12 månaderna",
       subtitle = sprintf("SNES 2022 CSES, n = %s", format(nrow(plot_df), big.mark = " ")),
       x = NULL, y = "Andel (%)") +
  theme_minimal(base_size = 11) +
  theme(plot.title.position = "plot")

# %% [code] {"execution":{"iopub.status.busy":"2026-04-26T18:55:41.501891Z","iopub.execute_input":"2026-04-26T18:55:41.503534Z","iopub.status.idle":"2026-04-26T18:55:43.997217Z","shell.execute_reply":"2026-04-26T18:55:43.994817Z"}}
dat = VU2022 %>%
  transmute(
    voted_S = if_else(v7000 == 2, 1L, 0L),
    econ_worse = q349 - 3,
    traded_any = as.integer(q371b > 1),
    traded_n = q371b,
    female = as.integer(v7100 == 2),
    age = v7200,
    educ = v7302,
    income = v7405,
    lr_self = q328
  ) %>%
  filter(!is.na(voted_S), !is.na(econ_worse), !is.na(traded_any))

cat("Analysis n =", nrow(dat),
    " | voted S =", sum(dat$voted_S),
    " | traders =", sum(dat$traded_any), "\n")

m0 = glm(voted_S ~ econ_worse + traded_any,
         data = dat, family = binomial)

m1 = glm(voted_S ~ econ_worse * traded_any,
         data = dat, family = binomial)

m2 = glm(voted_S ~ econ_worse * traded_any +
           female + age + educ + income + lr_self,
         data = dat, family = binomial)

modelsummary(list("Bivariate" = m0, "+ interaction" = m1, "+ controls" = m2),
             stars = TRUE, gof_omit = "AIC|BIC|Log.Lik|F|RMSE", output = "/kaggle/working/VU2022.html")


newdat = expand.grid(
  econ_worse = -2:2,
  traded_any = 0:1,
  female = mean(dat$female, na.rm = TRUE),
  age = mean(dat$age, na.rm = TRUE),
  educ = median(dat$educ, na.rm = TRUE),
  income = median(dat$income, na.rm = TRUE),
  lr_self = mean(dat$lr_self, na.rm = TRUE)
)
newdat$p_vote_S = predict(m2, newdat, type = "response")
print(newdat)

# %% [code] {"execution":{"iopub.status.busy":"2026-04-26T20:06:39.729041Z","iopub.execute_input":"2026-04-26T20:06:39.730772Z","iopub.status.idle":"2026-04-26T20:06:41.825281Z","shell.execute_reply":"2026-04-26T20:06:41.822342Z"}}
left_bloc = c(1, 2, 3, 7)   # V, S, C, MP in v7000 coding

dat = VU2022 %>%
  transmute(
    voted_S = if_else(v7000 == 2, 1L, 0L),
    voted_left = if_else(v7000 %in% left_bloc, 1L, 0L),
    econ_worse = q349 - 3,
    traded_any = as.integer(q371b > 1),
    traded_n = q371b - 1,                # 0..6, 0 = never
    female = as.integer(v7100 == 2),
    age = v7200,
    educ = v7302,
    income = v7405,
    lr_self = q328,
    county = factor(v7500)
  ) %>%
  # Same-sample analysis for valid LRT comparisons across nested models
  filter(!if_any(c(voted_S, econ_worse, traded_any,
                   female, age, educ, income, lr_self), is.na))

cat("Analysis n =", nrow(dat),
    " | voted S =", sum(dat$voted_S),
    " | traders =", sum(dat$traded_any), "\n\n")

cat("Cell counts (econ_worse x traded_any x voted_S):\n")
dat %>% count(econ_worse, traded_any, voted_S) %>% print(n = Inf)

m0 = glm(voted_S ~ econ_worse + traded_any,
         data = dat, family = binomial)

m1 = glm(voted_S ~ econ_worse * traded_any,
         data = dat, family = binomial)

m2 = glm(voted_S ~ econ_worse * traded_any +
           female + age + educ + income + lr_self,
         data = dat, family = binomial)

cat("\n=== Model comparison ===\n")
modelsummary(list("Bivariate" = m0, "+ interaction" = m1, "+ controls" = m2),
             stars = TRUE, gof_omit = "AIC|BIC|F|RMSE",
             title = "Economic voting x stock trading, SNES 2022")

cat("\n=== LRT: does each step improve fit? ===\n")
print(anova(m0, m1, m2, test = "LRT"))

# Cluster SEs on county. AMEs first (the "publish this" numbers).
cat("\n=== Average marginal effects (clustered by county) ===\n")
print(avg_slopes(m2, vcov = ~county))


# %% [code] {"execution":{"iopub.status.busy":"2026-04-26T20:09:12.360791Z","iopub.execute_input":"2026-04-26T20:09:12.362370Z","iopub.status.idle":"2026-04-26T20:09:15.552464Z","shell.execute_reply":"2026-04-26T20:09:15.550529Z"}}
# traders and non-traders
cat("\n=== Slope of econ_worse, by trader status (pairwise difference) ===\n")
print(avg_slopes(m2, variables = "econ_worse", by = "traded_any",
                 hypothesis = ~pairwise, vcov = ~county))

# Predicted probabilities across the 5-point econ scale, each group
preds = predictions(m2,
                    newdata = datagrid(econ_worse = -2:2, traded_any = 0:1),
                    vcov = ~county)
cat("\n=== Predicted P(vote S) ===\n")
print(preds)

p_df = as.data.frame(preds) %>%
  mutate(
    Trader = factor(traded_any, levels = 0:1,
                    labels = c("Non-trader", "Direct stock trader")),
    EconLabel = factor(econ_worse, levels = -2:2,
                       labels = c("Improved\na lot", "Improved", "Neither",
                                  "Deteriorated", "Deteriorated\na lot")))

p_interaction = ggplot(p_df, aes(EconLabel, estimate,
                                 group = Trader, colour = Trader)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Trader),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Predicted probability of voting Social Democrats (incumbent)",
       subtitle = "By retrospective economic evaluation x direct stock trading",
       x = "View of Swedish economy vs. 12 months ago",
       y = "P(vote S)", colour = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", plot.title.position = "plot")
print(p_interaction)

m2_cont = glm(voted_S ~ econ_worse * traded_n +
                female + age + educ + income + lr_self,
              data = dat, family = binomial)
cat("\n=== Robustness 1: continuous q371b (0..6) ===\n")
print(avg_slopes(m2_cont, variables = "econ_worse",
                 newdata = datagrid(traded_n = c(0, 3, 6)),
                 vcov = ~county))

m2_lpm = feols(voted_S ~ econ_worse * traded_any +
                 female + age + educ + income + lr_self,
               data = dat, cluster = ~county)
cat("\n=== Robustness 2: LPM (feols, cluster by county) ===\n")
print(summary(m2_lpm))

m2_bloc = glm(voted_left ~ econ_worse * traded_any +
                female + age + educ + income + lr_self,
              data = dat, family = binomial)
cat("\n=== Robustness 3: voted left bloc (S+V+MP+C) ===\n")
print(avg_slopes(m2_bloc, variables = "econ_worse", by = "traded_any",
                 hypothesis = ~pairwise, vcov = ~county))

m2_nolr = glm(voted_S ~ econ_worse * traded_any +
                female + age + educ + income,
              data = dat, family = binomial)
cat("\n=== Robustness 4: without left-right self-placement ===\n")
print(avg_slopes(m2_nolr, variables = "econ_worse", by = "traded_any",
                 hypothesis = ~pairwise, vcov = ~county))

cat("\n=== Separation check (looking for |est|>5 with SE>5) ===\n")
sm = summary(m2)$coefficients
print(round(sm[, c("Estimate", "Std. Error")], 3))

dat$phat = predict(m2, type = "response")
cat("\n=== Calibration: observed vs predicted (10 bins) ===\n")
print(dat %>%
        mutate(bin = cut(phat, breaks = seq(0, 1, 0.1),
                         include.lowest = TRUE)) %>%
        group_by(bin) %>%
        summarise(predicted = mean(phat),
                  observed  = mean(voted_S),
                  n = n()),
      n = Inf)