suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(modelsummary)
  library(marginaleffects)
})

setwd("")
vu18 = read_excel("")


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

#Build analysis dataset


dat = vu18 %>%
  transmute(
    # Sample identifiers
    etapp = etapp,
    etapp_edition = factor(etapp_edition),
    post_election = ifelse(etapp %in% c("B", "C"), 1L, 0L),
    # DV
    voted_S = case_when(v7000 == 2 ~ 1L,
                        v7000 %in% c(1, 3:9, 10) ~ 0L,
                        TRUE ~ NA_integer_),
    # Main IV (centered)
    econ_worse = q69a - 3,
    # Moderator
    traded_any = ifelse(q100b >= 2, 1L, 0L),
    # Heterogeneity moderators
    pol_interest = 4 - coalesce(q5, q300),
    pol_int_high = ifelse(coalesce(q5, q300) <= 2, 1L, 0L),
    # News consumption index (only A+B respondents have q1/q2)
    news_idx = rowMeans(cbind(5 - q1a, 5 - q1b, 5 - q1c, 5 - q1d,
                              5 - q2a, 5 - q2b, 5 - q2c),
                        na.rm = TRUE),
    # Controls
    female = ifelse(v7100 == 2, 1L, 0L),
    age = v7200,
    edu = factor(v7300, levels = 1:3,
                     labels = c("Primary", "Upper sec", "Tertiary")),
    income = factor(v7405, levels = 1:5,
                     labels = c("Q1", "Q2", "Q3", "Q4", "Q5")),
    lr_self = q58 - 5
  ) %>%
  filter(!is.na(voted_S), !is.na(econ_worse), !is.na(traded_any))

dat$news_idx[is.nan(dat$news_idx)] = NA_real_

cat(sprintf("\nHeadline analysis n = %d  (S-voters = %d, %.1f%%)\n",
            nrow(dat), sum(dat$voted_S), mean(dat$voted_S) * 100))
cat(sprintf("Any trading = %d  (%.1f%%)\n",
            sum(dat$traded_any), mean(dat$traded_any) * 100))
cat("\nSample composition by etapp_edition:\n")
print(table(dat$etapp_edition))
cat(sprintf("Pre-election (A):  %d  (%.1f%%)\n",
            sum(dat$post_election == 0), mean(dat$post_election == 0) * 100))
cat(sprintf("Post-election (B+C): %d  (%.1f%%)\n",
            sum(dat$post_election == 1), mean(dat$post_election == 1) * 100))

# Headline models


m1 = glm(voted_S ~ econ_worse,
         data = dat, family = binomial("logit"))

m2 = glm(voted_S ~ econ_worse * traded_any,
         data = dat, family = binomial("logit"))

m3 = glm(voted_S ~ econ_worse * traded_any +
           female + age + edu + income + lr_self + etapp_edition,
         data = dat, family = binomial("logit"))

modelsummary(list("Bivariate" = m1, "+ Interaction" = m2, "+ Controls" = m3),
             stars = TRUE,
             title = "SNES 2018: Vote for Löfven (S) ~ econ x trading",
             output = "snes18_models.html")

#Marginal probabilities grid
grid = expand.grid(
  econ_worse = -2:2,
  traded_any = 0:1,
  female = 0,
  age = mean(dat$age, na.rm = TRUE),
  edu = "Upper sec",
  income = "Q3",
  lr_self = 0,
  etapp_edition = factor("22", levels = levels(dat$etapp_edition))
)
grid$pred_S = predict(m3, newdata = grid, type = "response")
print(grid)

me = avg_slopes(m3, variables = "econ_worse", by = "traded_any",
                hypothesis = ~pairwise)
print(me)

#By etapp_edition

ee_levels = sort(unique(as.character(dat$etapp_edition)))
ee_labels = c("11" = "A1 (pre)", "12" = "A2 (pre)",
              "22" = "B2 (post)", "30" = "C / CSES (post)")

models_by_ee = lapply(ee_levels, function(ee) {
  glm(voted_S ~ econ_worse * traded_any +
        female + age + edu + income + lr_self,
      data = filter(dat, etapp_edition == ee),
      family = binomial("logit"))
})
names(models_by_ee) = ee_labels[ee_levels]

modelsummary(models_by_ee,
             stars = TRUE,
             title = "Headline spec by etapp_edition",
             output = "snes18_by_etapp.html")

#ell-count
cat("\nCell counts per etapp_edition (S-voters x traders):\n")
for (ee in ee_levels) {
  sub = filter(dat, etapp_edition == ee)
  cat(sprintf("\n%s (n=%d):\n", ee_labels[ee], nrow(sub)))
  print(with(sub, table(voted_S, traded_any)))
}

#Heterogeneity pol int


cat("\n\n=== HETEROGENEITY 1: POLITICAL INTEREST ===\n")
cat(sprintf("pol_int_high = 1 (n=%d), 0 (n=%d)\n",
            sum(dat$pol_int_high == 1, na.rm = TRUE),
            sum(dat$pol_int_high == 0, na.rm = TRUE)))

m_int_hi = glm(voted_S ~ econ_worse * traded_any +
                 female + age + edu + income + lr_self + etapp_edition,
               data = filter(dat, pol_int_high == 1),
               family = binomial("logit"))
m_int_lo = glm(voted_S ~ econ_worse * traded_any +
                 female + age + edu + income + lr_self + etapp_edition,
               data = filter(dat, pol_int_high == 0),
               family = binomial("logit"))

m_int_3w = glm(voted_S ~ econ_worse * traded_any * pol_interest +
                 female + age + edu + income + lr_self + etapp_edition,
               data = dat, family = binomial("logit"))

modelsummary(list("High interest" = m_int_hi,
                  "Low interest" = m_int_lo,
                  "3-way (full)" = m_int_3w),
             stars = TRUE,
             coef_omit = "etapp_edition",
             title = "Heterogeneity by political interest",
             output = "snes18_het_polint.html")

grid_int = expand.grid(
  econ_worse = c(-1, 0, 1),
  traded_any = 0:1,
  pol_interest = c(1, 3),
  female = 0,
  age = mean(dat$age, na.rm = TRUE),
  edu = "Upper sec",
  income = "Q3",
  lr_self = 0,
  etapp_edition = factor("22", levels = levels(dat$etapp_edition))
)
grid_int$pred_S = predict(m_int_3w, newdata = grid_int, type = "response")
cat("\nPredicted P(vote S) -- political interest grid:\n")
print(grid_int)

#Heterogeneity news


dat_ab = dat %>% filter(!is.na(news_idx))
news_med = median(dat_ab$news_idx, na.rm = TRUE)
dat_ab$news_high = as.integer(dat_ab$news_idx > news_med)

cat(sprintf("\n\n=== HETEROGENEITY 2: NEWS CONSUMPTION (A+B subsample) ===\n"))
cat(sprintf("A+B with news_idx: n = %d  (median index = %.2f)\n",
            nrow(dat_ab), news_med))

m_news_hi = glm(voted_S ~ econ_worse * traded_any +
                  female + age + edu + income + lr_self + etapp_edition,
                data = filter(dat_ab, news_high == 1),
                family = binomial("logit"))
m_news_lo = glm(voted_S ~ econ_worse * traded_any +
                  female + age + edu + income + lr_self + etapp_edition,
                data = filter(dat_ab, news_high == 0),
                family = binomial("logit"))

m_news_3w = glm(voted_S ~ econ_worse * traded_any * news_idx +
                  female + age + edu + income + lr_self + etapp_edition,
                data = dat_ab, family = binomial("logit"))

modelsummary(list("High news" = m_news_hi,
                  "Low news" = m_news_lo,
                  "3-way (full)" = m_news_3w),
             stars = TRUE,
             coef_omit = "etapp_edition",
             title = "Heterogeneity by news consumption (A+B sample)",
             output = "snes18_het_news.html")

grid_news = expand.grid(
  econ_worse = c(-1, 0, 1),
  traded_any = 0:1,
  news_idx = c(1, 3),
  female = 0,
  age = mean(dat_ab$age, na.rm = TRUE),
  edu = "Upper sec",
  income = "Q3",
  lr_self = 0,
  etapp_edition = factor("22", levels = levels(dat_ab$etapp_edition))
)
grid_news$pred_S = predict(m_news_3w, newdata = grid_news, type = "response")
cat("\nPredicted P(vote S) -- news consumption grid:\n")
print(grid_news)

#Etapp sensitivity: post-election respondents only

m3_preonly = glm(voted_S ~ econ_worse * traded_any + 
                   female + age + edu + income + lr_self, 
                 data = filter(dat, etapp == "A"), 
                 family = binomial("logit"))

m3_postB = glm(voted_S ~ econ_worse * traded_any + 
                   female + age + edu + income + lr_self, 
                 data = filter(dat, etapp == "B"), 
                 family = binomial("logit"))


m3_postonly = glm(voted_S ~ econ_worse * traded_any +
                    female + age + edu + income + lr_self + etapp_edition,
                  data = filter(dat, post_election == 1),
                  family = binomial("logit"))

modelsummary(list("Headline (full sample)" = m3,
                  "Post-election only (B+C)" = m3_postonly,
                  "Pre-election only (A)" = m3_preonly),
             stars = TRUE,
             coef_omit = "etapp_edition",
             title = "Etapp sensitivity",
             output = "snes18_etapp_sensitivity.html")