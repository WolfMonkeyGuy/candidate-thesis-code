suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(modelsummary)
  library(marginaleffects)
})


theme_set(theme_minimal(base_size = 11))
sav = function(name, w = 7.5, h = 4.6) ggsave(name, width = w, height = h, dpi = 200)

setwd("")
vu22 = read_excel("")

dat = vu22 %>%
  mutate(
    pid_S = case_when(q332c == 2~ 1L, 
                      !is.na(q332c)~ 0L,
                      q332a == 2 & q332b == 2~ 0L,   
                      TRUE~ NA_integer_),
    pid_strength = case_when(
      q332a == 1 & q332d == 1~ 2L,   
      q332a == 1 & q332d %in% c(2, 3)~ 1L,   
      q332a == 2 & q332b == 1~ 1L,   
      q332a == 2 & q332b == 2~ 0L,   
      TRUE~ NA_integer_),
    pid_member = case_when(q333 == 1 ~ 1L,
                           q333 == 2 ~ 0L,
                           TRUE~ NA_integer_)
  ) %>%
  transmute(
    voted_S = case_when(v7000 == 2~ 1L,
                        v7000 %in% c(1, 3:9, 10)~ 0L,
                        TRUE~ NA_integer_),
    econ_worse = q349 - 3,
    traded_any = ifelse(q371b >= 2, 1L, 0L),
    pid_S, pid_strength, pid_member,
    pol_interest = 4 - q301,
    pol_int_high = ifelse(q301 <= 2, 1L, 0L),
    news_idx = rowMeans(cbind(q302a, q302b, q302c, q302d, q302e, q302f),
                        na.rm = TRUE),
    female = ifelse(v7100 == 2, 1L, 0L),
    age = v7200,
    edu = factor(v7300, levels = 1:3,
                     labels = c("Primary", "Upper sec", "University")),
    income = factor(v7405, levels = 1:5,
                     labels = c("Low", "R_low", "Middle", "R_high", "High")),
    lr_self = q328 - 5
  ) %>%
  filter(!is.na(voted_S), !is.na(econ_worse), !is.na(traded_any))

dat$news_idx[is.nan(dat$news_idx)] = NA_real_
dat = dat %>% droplevels()

cat(sprintf("\nAnalysis sample n = %d  (S-voters = %d, %.1f%%)\n",
            nrow(dat), sum(dat$voted_S), mean(dat$voted_S) * 100))
cat(sprintf("Any trading = %d  (%.1f%%)\n",
            sum(dat$traded_any), mean(dat$traded_any) * 100))

dat_news = dat %>% filter(!is.na(news_idx))
news_med = median(dat_news$news_idx, na.rm = TRUE)
dat_news$news_high = as.integer(dat_news$news_idx > news_med)

B = binomial("logit")

# Headline build
m1 = glm(voted_S ~ econ_worse, data = dat, family = B)
m2 = glm(voted_S ~ econ_worse * traded_any, data = dat, family = B)
m3 = glm(voted_S ~ econ_worse * traded_any +
           female + age + edu + income +
           pid_S + pid_strength + pid_member,
         data = dat, family = B)

# Heterogeneity: political interest
m_int_hi = glm(voted_S ~ econ_worse * traded_any + female + age + edu +
                 income + pid_S + pid_strength + pid_member,
               data = filter(dat, pol_int_high == 1), family = B)
m_int_lo = glm(voted_S ~ econ_worse * traded_any + female + age + edu +
                 income + pid_S + pid_strength + pid_member,
               data = filter(dat, pol_int_high == 0), family = B)
m_int_3w = glm(voted_S ~ econ_worse * traded_any * pol_interest +
                 female + age + edu + income + pid_S +
                 pid_strength + pid_member,
               data = dat, family = B)

# Heterogeneity: news consumption (all CSES respondents with q302)
m_news_hi = glm(voted_S ~ econ_worse * traded_any + female + age + edu +
                  income + pid_S + pid_strength + pid_member,
                data = filter(dat_news, news_high == 1), family = B)
m_news_lo = glm(voted_S ~ econ_worse * traded_any + female + age + edu +
                  income + pid_S + pid_strength + pid_member,
                data = filter(dat_news, news_high == 0), family = B)
m_news_3w = glm(voted_S ~ econ_worse * traded_any * news_idx +
                  female + age + edu + income + pid_S +
                  pid_strength + pid_member,
                data = dat_news, family = B)

# Heterogeneity: partisanship strength
m_pid_str  = glm(voted_S ~ econ_worse * traded_any + female + age +
                   edu + income,
                 data = filter(dat, pid_strength == 2), family = B)
m_pid_weak = glm(voted_S ~ econ_worse * traded_any + female + age +
                   edu + income,
                 data = filter(dat, pid_strength == 1), family = B)
m_pid_none = glm(voted_S ~ econ_worse * traded_any + female + age +
                   edu + income,
                 data = filter(dat, pid_strength == 0), family = B)
m_pid_3w   = glm(voted_S ~ econ_worse * traded_any * pid_strength +
                   female + age + edu + income + pid_S + pid_member,
                 data = dat, family = B)

# Sensitivities (2022 analogues; no post-election sensitivity because the
# whole CSES sample is post-election)
m3_no_pid_lr = glm(voted_S ~ econ_worse * traded_any + female + age +
                     edu + income, data = dat, family = B)
m3_lr_only   = glm(voted_S ~ econ_worse * traded_any + female + age +
                     edu + income + lr_self, data = dat, family = B)

#Marginal-effects helper
me_std = function(model, label) {
  has_tr = "traded_any" %in% all.vars(formula(model))
  grab = function(obj) tibble(estimate = obj$estimate,
                              std.error = obj$std.error,
                              conf.low = obj$conf.low,
                              conf.high = obj$conf.high,
                              p.value = obj$p.value)
  rows = list(
    tibble(model = label, quantity = "AME econ_worse (overall)", group = NA_character_) %>%
      bind_cols(grab(avg_slopes(model, variables = "econ_worse")))
  )
  if (has_tr) {
    by_tr = avg_slopes(model, variables = "econ_worse", by = "traded_any")
    rows[[length(rows) + 1]] = tibble(
      model = label, quantity = "AME econ_worse (by trader)",
      group = paste0("trader=", by_tr$traded_any)) %>% bind_cols(grab(by_tr))
    dif = avg_slopes(model, variables = "econ_worse", by = "traded_any",
                     hypothesis = ~pairwise)
    rows[[length(rows) + 1]] = tibble(
      model = label, quantity = "AME econ_worse: trader - nontrader",
      group = "difference") %>% bind_cols(grab(dif))
    rows[[length(rows) + 1]] = tibble(
      model = label, quantity = "AME traded_any (overall)", group = NA_character_) %>%
      bind_cols(grab(avg_slopes(model, variables = "traded_any")))
  }
  bind_rows(rows)
}

#Master table over every standard model
standard_models = c(
  list("Bivariate" = m1,
       "+ Interaction" = m2,
       "+ Controls" = m3),
  list("High interest" = m_int_hi,
       "Low interest" = m_int_lo,
       "High news" = m_news_hi,
       "Low news" = m_news_lo,
       "Strong PID" = m_pid_str,
       "Weak PID" = m_pid_weak,
       "No PID" = m_pid_none,
       "Sens: drop PID+LR" = m3_no_pid_lr,
       "Sens: LR only" = m3_lr_only)
)

master = bind_rows(lapply(names(standard_models), function(nm) {
  tryCatch(me_std(standard_models[[nm]], nm),
           error = function(e) tibble(model = nm, quantity = "ERROR",
                                      group = conditionMessage(e)))
}))

master = master %>%
  mutate(
    block = case_when(
      str_detect(model, "^Sens:") ~ "Sensitivity",
      model %in% c("Bivariate", "+ Interaction",
                   "+ Controls") ~ "Headline",
      TRUE ~ "Heterogeneity"),
    sig = cut(p.value, c(-Inf, .001, .01, .05, .1, Inf),
              labels = c("***", "**", "*", "+", "")))

write.csv(master, "me_marginal_effects_master_2022.csv", row.names = FALSE)
tryCatch(datasummary_df(master %>%
                          mutate(across(c(estimate, std.error, conf.low, conf.high, p.value),
                                        ~round(.x, 3))),
                        output = "me_marginal_effects_master_2022.tex"),
         error = function(e) message("LaTeX export skipped: ",
                                     conditionMessage(e)))
print(master, n = Inf)

#Per-family AME tables via modelsummary
ame_table = function(models, file, ttl) {
  obj = lapply(models, function(m)
    avg_slopes(m, variables = intersect(c("econ_worse", "traded_any"),
                                        all.vars(formula(m)))))
  modelsummary(obj, statistic = "conf.int", stars = TRUE,
               title = ttl, output = file)
}
ame_table(list("Bivariate" = m1, "+ Interaction" = m2,
               "+ Controls" = m3),
          "me_ame_headline_2022.html", "AME, headline (cabinet, S 2022)")
ame_table(list("High int." = m_int_hi, "Low int." = m_int_lo,
               "High news" = m_news_hi, "Low news" = m_news_lo,
               "Strong PID" = m_pid_str, "Weak PID" = m_pid_weak,
               "No PID" = m_pid_none),
          "me_ame_het_2022.html", "AME by subgroup (cabinet, S 2022)")
ame_table(list("Drop PID+LR" = m3_no_pid_lr, "LR only" = m3_lr_only),
          "me_ame_sens_2022.html", "AME, sensitivities (cabinet, S 2022)")

#hree-way models
me_3way = function(model, label, mod, kind) {
  mf = model.frame(model)
  if (kind == "cont") {
    qs = quantile(mf[[mod]], c(.1, .25, .5, .75, .9), na.rm = TRUE)
    args = list(model = model, traded_any = c(0L, 1L)); args[[mod]] = unname(qs)
    nd = do.call(datagrid, args)
    s = slopes(model, variables = "econ_worse", newdata = nd)
    tibble(model = label, moderator = mod,
           mod_value = as.numeric(s[[mod]]),
           traded_any = as.character(s$traded_any),
           estimate = s$estimate, std.error = s$std.error,
           conf.low = s$conf.low, conf.high = s$conf.high, p.value = s$p.value)
  } else {
    out = lapply(sort(unique(mf$pid_strength)), function(k) {
      d = avg_slopes(model, variables = "econ_worse", by = "traded_any",
                     newdata = subset(mf, pid_strength == k),
                     hypothesis = ~pairwise)
      tibble(model = label, moderator = mod, mod_value = as.numeric(k),
             traded_any = "trader - nontrader",
             estimate = d$estimate, std.error = d$std.error,
             conf.low = d$conf.low, conf.high = d$conf.high, p.value = d$p.value)
    })
    bind_rows(out)
  }
}

threeway = bind_rows(
  me_3way(m_int_3w,  "Pol interest x3", "pol_interest", "cont"),
  me_3way(m_news_3w, "News x3", "news_idx", "cont"),
  me_3way(m_pid_3w,  "Partisanship x3", "pid_strength", "disc")
)
write.csv(threeway, "me_threeway_2022.csv", row.names = FALSE)
print(threeway, n = Inf)

# Plots

#Predicted-probability curves
plot_predictions(m3, condition = list("econ_worse", "traded_any")) +
  labs(title = "Predicted P(S vote) by economic evaluation and trading, 2022",
       subtitle = "Andersson single-party Social Democratic cabinet",
       x = "Economy got worse (q349, centred)", y = "P(vote S)",
       colour = "Trader", fill = "Trader")
sav("fig_pp_headline_2022.png")

#The trader gap across every model
gap = master %>%
  filter(quantity == "AME econ_worse: trader - nontrader") %>%
  mutate(lab = model,
         block = factor(block, levels = c("Headline", "Heterogeneity",
                                          "Sensitivity")))
ggplot(gap, aes(estimate, reorder(lab, estimate))) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), colour = "steelblue") +
  facet_grid(block ~ ., scales = "free_y", space = "free_y") +
  labs(title = "Trader minus non-trader difference in the AME of economic evaluation, 2022",
       subtitle = "Each point is one cabinet model. Overlap with the dashed line means no differential.",
       x = "Difference in AME of econ_worse", y = NULL)
sav("fig_trader_gap_forest_2022.png", h = 6.5)

# Three-way
plot_slopes(m_int_3w, variables = "econ_worse",
            condition = c("pol_interest", "traded_any")) +
  labs(title = "Slope of economic evaluation vs political interest, 2022",
       y = "Marginal effect of econ_worse", colour = "Trader", fill = "Trader")
sav("fig_3way_polint_2022.png")

plot_slopes(m_news_3w, variables = "econ_worse",
            condition = c("news_idx", "traded_any")) +
  labs(title = "Slope of economic evaluation vs news consumption, 2022",
       y = "Marginal effect of econ_worse", colour = "Trader", fill = "Trader")
sav("fig_3way_news_2022.png")

plot_slopes(m_pid_3w, variables = "econ_worse",
            condition = c("pid_strength", "traded_any")) +
  labs(title = "Slope of economic evaluation by partisanship strength, 2022",
       x = "Partisanship strength (0/1/2)",
       y = "Marginal effect of econ_worse", colour = "Trader", fill = "Trader")
sav("fig_3way_pid_2022.png")

#Predicted-probability table

suppressPackageStartupMessages(library(tidyr))
tryCatch({
  g = datagrid(model = m3, econ_worse = -2:2, traded_any = 0:1)
  p = predictions(m3, newdata = g)
  lv = c("Improved a lot", "Improved", "Neither",
         "Deteriorated", "Deteriorated a lot")
  out = p %>%
    transmute(econ = factor(econ_worse, levels = -2:2, labels = lv),
              traded_any,
              cell = sprintf("%.1f%% [%.1f, %.1f]",
                             100 * estimate, 100 * conf.low, 100 * conf.high)) %>%
    arrange(econ) %>%
    pivot_wider(names_from = traded_any, values_from = cell) %>%
    rename(`Economic evaluation` = econ,
           `Non-trader` = `0`, `Direct stock trader` = `1`)
  datasummary_df(out, output = "pred_probs_2022.tex",
                 title = "Predicted P(S vote) by evaluation and trading, 2022")
}, error = function(e) message("pred-prob table skipped: ", conditionMessage(e)))

message("\nDone. CSVs, .tex tables, .html tables and .png figures written to ",
        getwd())


vu22 %>%
  filter(!is.na(q371b)) %>%
  summarise(n_traders = sum(q371b >= 2),
            n_total = n(),
            pct = mean(q371b >= 2) * 100)
