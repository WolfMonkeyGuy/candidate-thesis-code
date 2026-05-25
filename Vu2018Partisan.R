suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(modelsummary)
  library(marginaleffects)
  library(kableExtra)
})

setwd("")
vu18 = read_excel("")

#Bar chart: stock trading frequency (q100b
trade_levels = c(
  "Aldrig",
  "Någon gång senaste 12 mån",
  "Någon gång senaste 6 mån",
  "Någon gång senaste 3 mån",
  "Någon gång senaste månaden",
  "Någon gång i veckan",
  "Flera gånger i veckan"
)

plot_df = vu18 %>%
  filter(!is.na(q100b)) %>%
  mutate(q100b_lbl = factor(q100b, levels = 1:7, labels = trade_levels))

ggplot(plot_df, aes(x = q100b_lbl)) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count)) * 100),
           fill = "steelblue") +
  geom_text(aes(y = after_stat(count) / sum(after_stat(count)) * 100,
                label = sprintf("%.1f%%", after_stat(count) / sum(after_stat(count)) * 100)),
            stat = "count", vjust = -0.4, size = 3.2) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  labs(title    = "Hur ofta har du handlat med aktier senaste 12 månaderna?",
       subtitle = sprintf("VU2018, n = %d (q100b)", nrow(plot_df)),
       x = NULL, y = "Procent") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))


dat = vu18 %>%
  mutate(
    pid_party = coalesce(v7020, q321c),
    pid_S     = case_when(pid_party == 2~ 1L,
                          !is.na(pid_party)~ 0L,
                          v7021 == 4~ 0L,
                          q321a == 2 & q321b == 2 ~ 0L,
                          TRUE~ NA_integer_),
    pid_MP    = case_when(pid_party == 7~ 1L,
                          !is.na(pid_party)~ 0L,
                          v7021 == 4~ 0L,
                          q321a == 2 & q321b == 2 ~ 0L,
                          TRUE~ NA_integer_),
    pid_strength = case_when(
      # A+B path via v7021
      v7021 == 1~ 2L,   
      v7021 %in% c(2, 3)~ 1L,   
      v7021 == 4~ 0L,   
      # CSES path via q321a/b/
      q321a == 1 & q321d == 1~ 2L,   
      q321a == 1 & q321d %in% c(2, 3)~ 1L,   
      q321a == 2 & q321b == 1~ 1L,   
      q321a == 2 & q321b == 2~ 0L,   
      TRUE~ NA_integer_),
    pid_member = case_when(q49 == 1 ~ 1L,
                           q49 == 2 ~ 0L,
                           TRUE~ NA_integer_)
  ) %>%
  transmute(
    etapp,
    etapp_edition = factor(etapp_edition),
    post_election = ifelse(etapp %in% c("B", "C"), 1L, 0L),
    voted_S = case_when(v7000 == 2~ 1L,
                        v7000 %in% c(1, 3:9, 10)~ 0L,
                        TRUE~ NA_integer_),
    econ_worse = q69a - 3,
    traded_any = ifelse(q100b >= 2, 1L, 0L),
    pid_S, pid_MP, pid_strength, pid_member,
    pol_interest = 4 - coalesce(q5, q300),
    pol_int_high = ifelse(coalesce(q5, q300) <= 2, 1L, 0L),
    news_idx = rowMeans(cbind(5 - q1a, 5 - q1b, 5 - q1c, 5 - q1d,
                              5 - q2a, 5 - q2b, 5 - q2c),
                        na.rm = TRUE),
    female = ifelse(v7100 == 2, 1L, 0L),
    age = v7200,
    edu = factor(v7300, levels = 1:3,
                     labels = c("Primary", "Upper sec", "University")),
    income = factor(v7405, levels = 1:5,
                     labels = c("Low", "R_low", "Middle", "R_high", "High")),
    lr_self = q58 - 5,
    voted_S_or_MP = case_when(v7000 %in% c(2, 7)        ~ 1L,
                              v7000 %in% c(1, 3:6, 8:10) ~ 0L,
                              TRUE                        ~ NA_integer_)
  ) %>%
  filter(!is.na(voted_S), !is.na(econ_worse), !is.na(traded_any))

dat$news_idx[is.nan(dat$news_idx)] = NA_real_
dat$etapp_edition = droplevels(dat$etapp_edition)

cat(sprintf("\nAnalysis sample n = %d  (S-voters = %d, %.1f%%)\n",
            nrow(dat), sum(dat$voted_S), mean(dat$voted_S) * 100))
cat(sprintf("Any trading = %d  (%.1f%%)\n",
            sum(dat$traded_any), mean(dat$traded_any) * 100))
cat("\nPID coverage:\n")
cat(sprintf("  pid_S       non-missing: %d (%.1f%%)\n",
            sum(!is.na(dat$pid_S)), mean(!is.na(dat$pid_S)) * 100))
cat(sprintf("  pid_strength non-missing: %d (%.1f%%)\n",
            sum(!is.na(dat$pid_strength)), mean(!is.na(dat$pid_strength)) * 100))
cat(sprintf("  pid_member  non-missing: %d (%.1f%%)\n",
            sum(!is.na(dat$pid_member)), mean(!is.na(dat$pid_member)) * 100))
cat("\nSample composition by etapp_edition:\n")
print(table(dat$etapp_edition))

#Headline models

m1 = glm(voted_S ~ econ_worse,
         data = dat, family = binomial("logit"))

m1_smp = glm(voted_S_or_MP ~ econ_worse,
         data = dat, family = binomial("logit"))

m2 = glm(voted_S ~ econ_worse * traded_any,
         data = dat, family = binomial("logit"))

m2_smp = glm(voted_S_or_MP ~ econ_worse * traded_any,
         data = dat, family = binomial("logit"))

m3 = glm(voted_S ~ econ_worse * traded_any +
           female + age + edu + income +
           pid_S + pid_strength + pid_member + etapp_edition,
         data = dat, family = binomial("logit"))

m3_smp = glm(voted_S_or_MP ~ econ_worse * traded_any +
               female + age + edu + income +
               pid_S + pid_MP + pid_strength + pid_member + etapp_edition,
             data = dat, family = binomial("logit"))


modelsummary(list("Bivariate" = m1_smp, "+ Interaction" = m2_smp, "+ Controls" = m3_smp),
             stars = TRUE,
             #coef_omit = "etapp_edition",
             title = "SNES 2018: Vote for S or MP ~ econ x trading",
             output = "snes18_models_S_or_MP.html")

modelsummary(list("Bivariate" = m1, "+ Interaction" = m2, "+ Controls" = m3),
             stars = TRUE,
             #coef_omit = "etapp_edition",
             title = "SNES 2018: Vote for Lofven (S) ~ econ x trading",
             output = "snes18_models.html")

#Marginal probabilities grid

grid = expand.grid(
  econ_worse = -2:2,
  traded_any = 0:1,
  female = 0,
  age = mean(dat$age, na.rm = TRUE),
  edu = "Upper sec",
  income = "Middle",
  pid_S = 0,
  pid_strength = 1,
  pid_member = 0,
  etapp_edition = factor("22", levels = levels(dat$etapp_edition))
)
grid$pred_S = predict(m3, newdata = grid, type = "response")
print(grid)

me = avg_slopes(m3, variables = "econ_worse", by = "traded_any",
                hypothesis = ~pairwise)
print(me)



ee_levels = sort(unique(as.character(dat$etapp_edition)))
ee_labels = c("11" = "A1 (pre)", "12" = "A2 (pre)",
              "22" = "B2 (post)", "30" = "C / CSES (post)")
models_by_ee = lapply(ee_levels, function(ee) {
  glm(voted_S_or_MP ~ econ_worse * traded_any +
        female + age + edu + income +
        pid_S + pid_MP + pid_strength + pid_member,
      data = filter(dat, etapp_edition == ee),
      family = binomial("logit"))
})
names(models_by_ee) = ee_labels[ee_levels]

modelsummary(models_by_ee, stars = TRUE,
             title = "Headline spec by etapp_edition",
             output = "snes18_by_etapp_S_MP.html")

# Heterogeneity 
cat("\n\n=== HETEROGENEITY 1: POLITICAL INTEREST ===\n")
cat(sprintf("pol_int_high = 1 (n=%d), 0 (n=%d)\n",
            sum(dat$pol_int_high == 1, na.rm = TRUE),
            sum(dat$pol_int_high == 0, na.rm = TRUE)))

m_int_hi = glm(voted_S ~ econ_worse * traded_any +
                 female + age + edu + income +
                 pid_S + pid_strength + pid_member + etapp_edition,
               data = filter(dat, pol_int_high == 1),
               family = binomial("logit"))
m_int_lo = glm(voted_S ~ econ_worse * traded_any +
                 female + age + edu + income +
                 pid_S + pid_strength + pid_member + etapp_edition,
               data = filter(dat, pol_int_high == 0),
               family = binomial("logit"))
m_int_3w = glm(voted_S ~ econ_worse * traded_any * pol_interest +
                 female + age + edu + income +
                 pid_S + pid_strength + pid_member + etapp_edition,
               data = dat, family = binomial("logit"))

modelsummary(list("High interest" = m_int_hi,
                  "Low interest"  = m_int_lo,
                  "3-way (full)"  = m_int_3w),
             stars = TRUE, coef_omit = "etapp_edition",
             title = "Heterogeneity by political interest",
             output = "snes18_het_polint.html")

#Heterogeneity 2
dat_ab = dat %>% filter(!is.na(news_idx))
news_med = median(dat_ab$news_idx, na.rm = TRUE)
dat_ab$news_high = as.integer(dat_ab$news_idx > news_med)

cat(sprintf("\n\n=== HETEROGENEITY 2: NEWS CONSUMPTION (A+B subsample) ===\n"))
cat(sprintf("A+B with news_idx: n = %d  (median index = %.2f)\n",
            nrow(dat_ab), news_med))

m_news_hi = glm(voted_S ~ econ_worse * traded_any +
                  female + age + edu + income +
                  pid_S + pid_strength + pid_member + etapp_edition,
                data = filter(dat_ab, news_high == 1),
                family = binomial("logit"))
m_news_lo = glm(voted_S ~ econ_worse * traded_any +
                  female + age + edu + income +
                  pid_S + pid_strength + pid_member + etapp_edition,
                data = filter(dat_ab, news_high == 0),
                family = binomial("logit"))
m_news_3w = glm(voted_S ~ econ_worse * traded_any * news_idx +
                  female + age + edu + income +
                  pid_S + pid_strength + pid_member + etapp_edition,
                data = dat_ab, family = binomial("logit"))

modelsummary(list("High news" = m_news_hi,
                  "Low news" = m_news_lo,
                  "3-way (full)" = m_news_3w),
             stars = TRUE, coef_omit = "etapp_edition",
             title = "Heterogeneity by news consumption (A+B sample)",
             output = "snes18_het_news.html")

#Heterogeneity 3: PARTISANSHIP
cat("\n\n=== HETEROGENEITY 3: PARTISANSHIP ===\n")
cat(sprintf("pid_strength = 0 (none): %d, = 1 (weak): %d, = 2 (strong): %d\n",
            sum(dat$pid_strength == 0, na.rm = TRUE),
            sum(dat$pid_strength == 1, na.rm = TRUE),
            sum(dat$pid_strength == 2, na.rm = TRUE)))

m_pid_str = glm(voted_S ~ econ_worse * traded_any +
                   female + age + edu + income + etapp_edition,
                 data = filter(dat, pid_strength == 2),
                 family = binomial("logit"))
m_pid_weak = glm(voted_S ~ econ_worse * traded_any +
                   female + age + edu + income + etapp_edition,
                 data = filter(dat, pid_strength == 1),
                 family = binomial("logit"))
m_pid_none = glm(voted_S ~ econ_worse * traded_any +
                   female + age + edu + income + etapp_edition,
                 data = filter(dat, pid_strength == 0),
                 family = binomial("logit"))
m_pid_3w = glm(voted_S ~ econ_worse * traded_any * pid_strength +
                   female + age + edu + income +
                   pid_S + pid_member + etapp_edition,
                 data = dat, family = binomial("logit"))



modelsummary(list("Strong PID" = m_pid_str,
                  "Weak PID" = m_pid_weak,
                  "No PID" = m_pid_none,
                  "3-way (full)" = m_pid_3w),
             stars = TRUE, coef_omit = "etapp_edition",
             title = "Heterogeneity by partisanship strength",
             output = "snes18_het_pid.html")

#S_MP_Heterogeneity things

m_int_3w_smp = update(m_int_3w, voted_S_or_MP ~ .)
m_news_3w_smp = update(m_news_3w, voted_S_or_MP ~ .)
m_pid_3w_smp = update(m_pid_3w, voted_S_or_MP ~ .)

modelsummary(list("Pol interest" = m_int_3w_smp,
                  "News" = m_news_3w_smp,
                  "Partisanship" = m_pid_3w_smp),
             stars = TRUE, coef_omit = "etapp_edition",
             title = "Heterogeneity, S+MP DV",
             output = "snes18_het_smp_3way.html")


#Political interest, S+MP DV
m_int_hi_smp = glm(voted_S_or_MP ~ econ_worse * traded_any +
                     female + age + edu + income +
                     pid_S + pid_MP + pid_strength + pid_member + etapp_edition,
                   data = filter(dat, pol_int_high == 1),
                   family = binomial("logit"))
m_int_lo_smp = glm(voted_S_or_MP ~ econ_worse * traded_any +
                     female + age + edu + income +
                     pid_S + pid_MP + pid_strength + pid_member + etapp_edition,
                   data = filter(dat, pol_int_high == 0),
                   family = binomial("logit"))
m_int_3w_smp = glm(voted_S_or_MP ~ econ_worse * traded_any * pol_interest +
                     female + age + edu + income +
                     pid_S + pid_MP + pid_strength + pid_member + etapp_edition,
                   data = dat, family = binomial("logit"))

modelsummary(list("High interest" = m_int_hi_smp,
                  "Low interest"  = m_int_lo_smp,
                  "3-way (full)"  = m_int_3w_smp),
             stars = TRUE, coef_omit = "etapp_edition",
             title = "Heterogeneity by political interest -- S+MP DV",
             notes = "DV: Voted S or MP (Lofven I coalition) vs other party.",
             output = "snes18_het_polint_SMP.html")

# News consumption, S+MP DV
m_news_hi_smp = glm(voted_S_or_MP ~ econ_worse * traded_any +
                      female + age + edu + income +
                      pid_S + pid_MP + pid_strength + pid_member + etapp_edition,
                    data = filter(dat_ab, news_high == 1),
                    family = binomial("logit"))
m_news_lo_smp = glm(voted_S_or_MP ~ econ_worse * traded_any +
                      female + age + edu + income +
                      pid_S + pid_MP + pid_strength + pid_member + etapp_edition,
                    data = filter(dat_ab, news_high == 0),
                    family = binomial("logit"))
m_news_3w_smp = glm(voted_S_or_MP ~ econ_worse * traded_any * news_idx +
                      female + age + edu + income +
                      pid_S + pid_MP + pid_strength + pid_member + etapp_edition,
                    data = dat_ab, family = binomial("logit"))

modelsummary(list("High news" = m_news_hi_smp,
                  "Low news" = m_news_lo_smp,
                  "3-way (full)" = m_news_3w_smp),
             stars = TRUE, coef_omit = "etapp_edition",
             title = "Heterogeneity by news consumption -- S+MP DV (A+B sample)",
             notes = "DV: Voted S or MP (Lofven I coalition) vs other party.",
             output = "snes18_het_news_SMP.html")

# Partisanship, S+MP DV
m_pid_str_smp  = glm(voted_S_or_MP ~ econ_worse * traded_any +
                       female + age + edu + income + etapp_edition,
                     data = filter(dat, pid_strength == 2),
                     family = binomial("logit"))
m_pid_weak_smp = glm(voted_S_or_MP ~ econ_worse * traded_any +
                       female + age + edu + income + etapp_edition,
                     data = filter(dat, pid_strength == 1),
                     family = binomial("logit"))
m_pid_none_smp = glm(voted_S_or_MP ~ econ_worse * traded_any +
                       female + age + edu + income + etapp_edition,
                     data = filter(dat, pid_strength == 0),
                     family = binomial("logit"))
m_pid_3w_smp   = glm(voted_S_or_MP ~ econ_worse * traded_any * pid_strength +
                       female + age + edu + income +
                       pid_S + pid_MP + pid_member + etapp_edition,
                     data = dat, family = binomial("logit"))

modelsummary(list("Strong PID" = m_pid_str_smp,
                  "Weak PID" = m_pid_weak_smp,
                  "No PID" = m_pid_none_smp,
                  "3-way (full)" = m_pid_3w_smp),
             stars = TRUE, coef_omit = "etapp_edition",
             title = "Heterogeneity by partisanship strength -- S+MP DV",
             notes = "DV: Voted S or MP (Lofven I coalition) vs other party.",
             output = "snes18_het_pid_SMP.html")


#Etapp: post-election respondents only (B+C).
m3_postonly = glm(voted_S ~ econ_worse * traded_any +
                    female + age + edu + income +
                    pid_S + pid_MP + pid_strength + pid_member + etapp_edition,
                  data = filter(dat, post_election == 1),
                  family = binomial("logit"))

#Drop both PID controls AND L-R
m3_no_pid_lr = glm(voted_S ~ econ_worse * traded_any +
                     female + age + edu + income + etapp_edition,
                   data = dat, family = binomial("logit"))

#L-R instead of PID (old spec, for comparability with prior runs).
m3_lr_only = glm(voted_S ~ econ_worse * traded_any +
                   female + age + edu + income + lr_self + etapp_edition,
                 data = dat, family = binomial("logit"))



modelsummary(list("Headline (PID controls)" = m3,
                  "Post-election only (B+C)" = m3_postonly,
                  "Drop PID + L-R" = m3_no_pid_lr,
                  "L-R only (old spec)" = m3_lr_only),
             stars = TRUE, coef_omit = "etapp_edition",
             title = "Sensitivities",
             output = "snes18_sensitivities.html")

# Marginal effects plot from the headline model (m3)


preds_smp = predictions(
  m3_smp,
  newdata = datagrid(econ_worse = -2:2, traded_any = 0:1,
                     etapp_edition = "22"))

cat("\n=== Predicted P(vote S + vote MP) ===\n")
print(preds_smp)

p_df = as.data.frame(preds_smp) %>%
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
  labs(title = "Predicted probability of voting for cabinet",
       subtitle = "Headline model, controls held at a reference profile",
       x = "View of Swedish economy vs. 12 months ago",
       y = "P(vote Cabinet)", colour = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", plot.title.position = "plot")

print(p_interaction)


tab_df = as.data.frame(preds_smp) %>%
  mutate(
    Trader = factor(traded_any, levels = 0:1,
                    labels = c("Non-trader", "Direct stock trader")),
    EconLabel = factor(econ_worse, levels = -2:2,
                       labels = c("Improved a lot", "Improved", "Neither",
                                  "Deteriorated", "Deteriorated a lot")),
    cell = sprintf("%.1f\\%% [%.1f, %.1f]",
                   estimate * 100, conf.low * 100, conf.high * 100)) %>%
  select(EconLabel, Trader, cell) %>%
  tidyr::pivot_wider(names_from = Trader, values_from = cell) %>%
  rename(`Economic evaluation` = EconLabel)

kbl(tab_df, format = "latex", booktabs = TRUE, escape = FALSE,
    align = "lcc",
    caption = paste("Predicted probability of voting for cabinet by",
                    "retrospective sociotropic evaluation and direct stock",
                    "trading. Headline model, controls held at the reference",
                    "profile. 95\\% confidence intervals in brackets."),
    label = "tab:pred_probs") %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("snes18_pred_probs_smp.tex")
library(marginaleffects)

# Difference in AME of econ_worse (trader vs non-trader) within each subgroup
het_ame = function(model, label) {
  avg_slopes(model, variables = "econ_worse", by = "traded_any",
             hypothesis = ~pairwise) %>%
    as_tibble() %>%
    mutate(group = label)
}

bind_rows(
  het_ame(m_int_hi_smp, "High interest"),
  het_ame(m_int_lo_smp, "Low interest"),
  het_ame(m_news_hi_smp, "High news"),
  het_ame(m_news_lo_smp, "Low news"),
  het_ame(m_pid_str_smp, "Strong PID"),
  het_ame(m_pid_weak_smp, "Weak PID"),
  het_ame(m_pid_none_smp, "No PID")
) %>%
  select(group, estimate, std.error, p.value)
