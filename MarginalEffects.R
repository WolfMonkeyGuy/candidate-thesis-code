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
vu18 = read_excel("")

dat = vu18 %>%
  mutate(
    pid_party = coalesce(v7020, q321c),
    pid_S = case_when(pid_party == 2~ 1L,
                          !is.na(pid_party)~ 0L,
                          v7021 == 4~ 0L,
                          q321a == 2 & q321b == 2 ~ 0L,
                          TRUE~ NA_integer_),
    pid_MP    = case_when(pid_party == 7~ 1L,
                          !is.na(pid_party)~ 0L,
                          v7021 == 4~ 0L,
                          q321a == 2 & q321b == 2 ~ 0L,
                          TRUE                     ~ NA_integer_),
    pid_strength = case_when(
      v7021 == 1~ 2L,
      v7021 %in% c(2, 3)~ 1L,
      v7021 == 4~ 0L,
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
    income  = factor(v7405, levels = 1:5,
                     labels = c("Low", "R_low", "Middle", "R_high", "High")),
    lr_self = q58 - 5,
    voted_S_or_MP = case_when(v7000 %in% c(2, 7)        ~ 1L,
                              v7000 %in% c(1, 3:6, 8:10) ~ 0L,
                              TRUE                        ~ NA_integer_)
  ) %>%
  filter(!is.na(voted_S), !is.na(econ_worse), !is.na(traded_any))

dat$news_idx[is.nan(dat$news_idx)] = NA_real_

dat = dat %>% droplevels()         

dat_ab = dat %>% filter(!is.na(news_idx))
news_med = median(dat_ab$news_idx, na.rm = TRUE)
dat_ab$news_high = as.integer(dat_ab$news_idx > news_med)

#Refit every S+MP model (formulas mirror Vu2018Partisan.R
B = binomial("logit")

m1_smp = glm(voted_S_or_MP ~ econ_worse, data = dat, family = B)
m2_smp = glm(voted_S_or_MP ~ econ_worse * traded_any, data = dat, family = B)
m3_smp = glm(voted_S_or_MP ~ econ_worse * traded_any +
               female + age + edu + income +
               pid_S + pid_MP + pid_strength + pid_member + etapp_edition,
             data = dat, family = B)

# By etapp_edition, one fit per edition
ee_levels = sort(unique(as.character(dat$etapp_edition)))
ee_labels = c("11" = "A1 (pre)", "12" = "A2 (pre)",
              "22" = "B2 (post)", "30" = "C / CSES (post)")
models_by_ee = lapply(ee_levels, function(ee) {
  glm(voted_S_or_MP ~ econ_worse * traded_any +
        female + age + edu + income +
        pid_S + pid_MP + pid_strength + pid_member,
      data = filter(dat, etapp_edition == ee), family = B)
})
names(models_by_ee) = ee_labels[ee_levels]

# Heterogeneity: political interest
m_int_hi_smp = glm(voted_S_or_MP ~ econ_worse * traded_any + female + age + edu +
                     income + pid_S + pid_MP + pid_strength + pid_member +
                     etapp_edition, data = filter(dat, pol_int_high == 1), family = B)
m_int_lo_smp = glm(voted_S_or_MP ~ econ_worse * traded_any + female + age + edu +
                     income + pid_S + pid_MP + pid_strength + pid_member +
                     etapp_edition, data = filter(dat, pol_int_high == 0), family = B)
m_int_3w_smp = glm(voted_S_or_MP ~ econ_worse * traded_any * pol_interest +
                     female + age + edu + income + pid_S + pid_MP +
                     pid_strength + pid_member + etapp_edition,
                   data = dat, family = B)

# Heterogeneity: news consumption (A+B subsample)
m_news_hi_smp = glm(voted_S_or_MP ~ econ_worse * traded_any + female + age + edu +
                      income + pid_S + pid_MP + pid_strength + pid_member +
                      etapp_edition, data = filter(dat_ab, news_high == 1), family = B)
m_news_lo_smp = glm(voted_S_or_MP ~ econ_worse * traded_any + female + age + edu +
                      income + pid_S + pid_MP + pid_strength + pid_member +
                      etapp_edition, data = filter(dat_ab, news_high == 0), family = B)
m_news_3w_smp = glm(voted_S_or_MP ~ econ_worse * traded_any * news_idx +
                      female + age + edu + income + pid_S + pid_MP +
                      pid_strength + pid_member + etapp_edition,
                    data = dat_ab, family = B)

# Heterogeneity: partisanship strength
m_pid_str_smp  = glm(voted_S_or_MP ~ econ_worse * traded_any + female + age +
                       edu + income + etapp_edition,
                     data = filter(dat, pid_strength == 2), family = B)
m_pid_weak_smp = glm(voted_S_or_MP ~ econ_worse * traded_any + female + age +
                       edu + income + etapp_edition,
                     data = filter(dat, pid_strength == 1), family = B)
m_pid_none_smp = glm(voted_S_or_MP ~ econ_worse * traded_any + female + age +
                       edu + income + etapp_edition,
                     data = filter(dat, pid_strength == 0), family = B)
m_pid_3w_smp   = glm(voted_S_or_MP ~ econ_worse * traded_any * pid_strength +
                       female + age + edu + income +
                       pid_S + pid_MP + pid_member + etapp_edition,
                     data = dat, family = B)

# Sensitivities: S+MP analogues of the Vu2018Partisan.R sensitivities
# (originally S-only). Same control sets as the S+MP headline.
m3_postonly_smp  = glm(voted_S_or_MP ~ econ_worse * traded_any + female + age +
                         edu + income + pid_S + pid_MP + pid_strength +
                         pid_member + etapp_edition,
                       data = filter(dat, post_election == 1), family = B)
m3_no_pid_lr_smp = glm(voted_S_or_MP ~ econ_worse * traded_any + female + age +
                         edu + income + etapp_edition, data = dat, family = B)
m3_lr_only_smp   = glm(voted_S_or_MP ~ econ_worse * traded_any + female + age +
                         edu + income + lr_self + etapp_edition,
                       data = dat, family = B)

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
  list("Bivariate" = m1_smp,
       "+ Interaction" = m2_smp,
       "+ Controls" = m3_smp),
  setNames(models_by_ee, paste0("Edition: ", names(models_by_ee))),
  list("High interest" = m_int_hi_smp,
       "Low interest" = m_int_lo_smp,
       "High news" = m_news_hi_smp,
       "Low news" = m_news_lo_smp,
       "Strong PID" = m_pid_str_smp,
       "Weak PID" = m_pid_weak_smp,
       "No PID" = m_pid_none_smp,
       "Sens: post-only" = m3_postonly_smp,
       "Sens: drop PID+LR" = m3_no_pid_lr_smp,
       "Sens: LR only" = m3_lr_only_smp)
)

master = bind_rows(lapply(names(standard_models), function(nm) {
  tryCatch(me_std(standard_models[[nm]], nm),
           error = function(e) tibble(model = nm, quantity = "ERROR",
                                      group = conditionMessage(e)))
}))

master = master %>%
  mutate(
    block = case_when(
      str_detect(model, "^Edition:") ~ "Survey edition",
      str_detect(model, "^Sens:") ~ "Sensitivity",
      model %in% c("Bivariate", "+ Interaction",
                   "+ Controls") ~ "Headline",
      TRUE ~ "Heterogeneity"),
    sig = cut(p.value, c(-Inf, .001, .01, .05, .1, Inf),
              labels = c("***", "**", "*", "+", "")))

write.csv(master, "me_marginal_effects_master.csv", row.names = FALSE)
tryCatch(datasummary_df(master %>%
                          mutate(across(c(estimate, std.error, conf.low, conf.high, p.value),
                                        ~round(.x, 3))),
                        output = "me_marginal_effects_master.tex"),
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
ame_table(list("Bivariate" = m1_smp, "+ Interaction" = m2_smp,
               "+ Controls" = m3_smp),
          "me_ame_headline_SMP.html", "AME, headline (cabinet)")
ame_table(models_by_ee, "me_ame_etapp_SMP.html", "AME by survey edition (cabinet)")
ame_table(list("High int." = m_int_hi_smp, "Low int." = m_int_lo_smp,
               "High news" = m_news_hi_smp, "Low news" = m_news_lo_smp,
               "Strong PID" = m_pid_str_smp, "Weak PID" = m_pid_weak_smp,
               "No PID" = m_pid_none_smp),
          "me_ame_het_SMP.html", "AME by subgroup (cabinet)")
ame_table(list("Post-only" = m3_postonly_smp, "Drop PID+LR" = m3_no_pid_lr_smp,
               "LR only" = m3_lr_only_smp),
          "me_ame_sens_SMP.html", "AME, sensitivities (cabinet)")

#Three-way models: trader gap as a function of the moderator
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
  me_3way(m_int_3w_smp, "Pol interest x3", "pol_interest", "cont"),
  me_3way(m_news_3w_smp, "News x3", "news_idx", "cont"),
  me_3way(m_pid_3w_smp, "Partisanship x3", "pid_strength", "disc")
)
write.csv(threeway, "me_threeway.csv", row.names = FALSE)
print(threeway, n = Inf)

# Plots

#Predicted-probability curves
plot_predictions(m3_smp, condition = list("econ_worse", "traded_any")) +
  labs(title = "Predicted P(cabinet vote) by economic evaluation and trading",
       x = "Economy got worse (q69a, centred)", y = "P(vote S or MP)",
       colour = "Trader", fill = "Trader")
sav("fig_pp_headline_SMP.png")

#Predicted probabilities by survey edition
pp_ee = bind_rows(lapply(names(models_by_ee), function(nm) {
  predictions(models_by_ee[[nm]],
              newdata = datagrid(model = models_by_ee[[nm]],
                                 econ_worse = -2:2, traded_any = 0:1)) %>%
    transmute(edition = nm, econ_worse, traded_any,
              estimate, conf.low, conf.high)
}))
ggplot(pp_ee, aes(econ_worse, estimate,
                  colour = factor(traded_any), fill = factor(traded_any))) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .15, colour = NA) +
  geom_line() + geom_point(size = 1) +
  facet_wrap(~edition) +
  labs(title = "Predicted P(cabinet vote) by survey edition",
       x = "Economy got worse (centred)", y = "P(vote S or MP)",
       colour = "Trader", fill = "Trader")
sav("fig_pp_etapp_SMP.png", h = 5)

# trader gap across every model
gap = master %>%
  filter(quantity == "AME econ_worse: trader - nontrader") %>%
  mutate(lab = str_replace(model, "^Edition: ", ""),
         block = factor(block, levels = c("Headline", "Survey edition",
                                          "Heterogeneity", "Sensitivity")))
ggplot(gap, aes(estimate, reorder(lab, estimate))) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), colour = "steelblue") +
  facet_grid(block ~ ., scales = "free_y", space = "free_y") +
  labs(title = "Trader minus non-trader difference in the AME of economic evaluation",
       subtitle = "Each point is one cabinet model. Overlap with the dashed line means no differential.",
       x = "Difference in AME of econ_worse", y = NULL)
sav("fig_trader_gap_forest.png", h = 7.5)

#Three-way
plot_slopes(m_int_3w_smp, variables = "econ_worse",
            condition = c("pol_interest", "traded_any")) +
  labs(title = "Slope of economic evaluation vs political interest",
       y = "Marginal effect of econ_worse", colour = "Trader", fill = "Trader")
sav("fig_3way_polint_SMP.png")

plot_slopes(m_news_3w_smp, variables = "econ_worse",
            condition = c("news_idx", "traded_any")) +
  labs(title = "Slope of economic evaluation vs news consumption",
       y = "Marginal effect of econ_worse", colour = "Trader", fill = "Trader")
sav("fig_3way_news_SMP.png")

plot_slopes(m_pid_3w_smp, variables = "econ_worse",
            condition = c("pid_strength", "traded_any")) +
  labs(title = "Slope of economic evaluation by partisanship strength",
       x = "Partisanship strength (0/1/2)",
       y = "Marginal effect of econ_worse", colour = "Trader", fill = "Trader")
sav("fig_3way_pid_SMP.png")

#Predicted-probability table
suppressPackageStartupMessages(library(tidyr))
tryCatch({
  g = datagrid(model = m3_smp, econ_worse = 2:-2, traded_any = 0:1)
  p = predictions(m3_smp, newdata = g)
  lv = c("Improved a lot", "Improved", "Neither",
         "Deteriorated", "Deteriorated a lot")
  out = p %>%
    transmute(econ = factor(2 - (econ_worse + 2), labels = lv),
              traded_any,
              cell = sprintf("%.1f%% [%.1f, %.1f]",
                             100 * estimate, 100 * conf.low, 100 * conf.high)) %>%
    pivot_wider(names_from = traded_any, values_from = cell) %>%
    rename(`Economic evaluation` = econ,
           `Non-trader` = `0`, `Direct stock trader` = `1`)
  datasummary_df(out, output = "pred_probs_SMP.tex",
                 title = "Predicted P(cabinet vote) by evaluation and trading")
}, error = function(e) message("pred-prob table skipped: ", conditionMessage(e)))

message("\nDone. CSVs, .tex tables, .html tables and .png figures written to ",
        getwd())



vu18 %>%
  filter(!is.na(q100b)) %>%
  summarise(n_traders = sum(q100b >= 2),
            n_total = n(),
            pct = mean(q100b >= 2) * 100)