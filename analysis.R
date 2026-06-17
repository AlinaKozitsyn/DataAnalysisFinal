# =====================================================================
# Internet Use and Mental Health in Adolescents
# Hours Online vs. the Feeling of Being Addicted
#
# Reproducible analysis script.
# Data: data/new_data.xlsx  (n = 819 school students, ages 13-19)
# Instruments: IAT (internet addiction), PHQ-9 (depression),
#              UCLA Loneliness Scale.
#
# Run:  Rscript analysis.R
# Outputs: all numbers reported in the paper + figures/ PNGs.
# =====================================================================

# ---- packages ----
library(tidyverse)   # dplyr, ggplot2, etc.
library(readxl)      # read_excel
library(broom)       # tidy model output
library(car)         # vif()
library(pROC)        # roc(), auc()

set.seed(42)
dir.create("figures", showWarnings = FALSE)

# ---------------------------------------------------------------------
# 1. LOAD + DERIVE VARIABLES
# ---------------------------------------------------------------------
raw <- read_excel("data/new_data.xlsx")

# Ordinal exposure buckets -> numeric midpoints (hours/day)
hour_mid <- c("Less than 1 hour" = 0.5, "1-2 hours" = 1.5,
              "2-3 hours" = 2.5, "3-4 hours" = 3.5, "More than 4 hours" = 4.5)

df <- raw %>%
  mutate(
    sm_hours     = recode(socialmediatime, !!!hour_mid),
    net_hours    = recode(internettime,   !!!hour_mid),
    male         = if_else(gender == "Male", 1L, 0L),
    exercise_yes = if_else(exercise == "Yes", 1L, 0L),
    friends_good = if_else(friendsrelationship == "Good", 1L, 0L),
    bullied_yes  = if_else(bullied == "Yes", 1L, 0L),
    # outcomes
    depression   = totalphq,                 # PHQ-9 continuous (0-27)
    loneliness   = lonelinesstotal,          # UCLA continuous
    dep10        = if_else(totalphq >= 10, 1L, 0L),   # clinically significant
    # ordered factors for severity lenses (NO bar charts downstream)
    phq_sev = factor(categoryphq, levels = 1:5,
                     labels = c("Minimal","Mild","Moderate","Mod.severe","Severe"),
                     ordered = TRUE),
    iac_sev = factor(IAC, levels = 1:4,
                     labels = c("Normal","Mild","Moderate","Severe"),
                     ordered = TRUE)
  )

predictors <- c("sm_hours", "TotalIA", "male", "exercise_yes", "friends_good")

# ---------------------------------------------------------------------
# 2. TRAIN / TEST SPLIT  *BEFORE* ANY SCALING  (fixes data leakage)
#    Scaling parameters are learned on TRAIN ONLY and applied to both.
# ---------------------------------------------------------------------
scale_train_apply <- function(train, test, cols) {
  mu <- map_dbl(train[cols], mean)
  sdv <- map_dbl(train[cols], sd)
  for (c in cols) {
    train[[c]] <- (train[[c]] - mu[[c]]) / sdv[[c]]
    test[[c]]  <- (test[[c]]  - mu[[c]]) / sdv[[c]]
  }
  list(train = train, test = test)
}

make_split <- function(data, strat = NULL, p = 0.7) {
  if (is.null(strat)) {
    idx <- sample(seq_len(nrow(data)), size = floor(p * nrow(data)))
  } else {
    idx <- data %>% mutate(.row = row_number()) %>%
      group_by({{ strat }}) %>%
      slice_sample(prop = p) %>% pull(.row)
  }
  list(train = data[idx, ], test = data[-idx, ])
}

scale_cols <- c("sm_hours", "TotalIA")  # the two continuous predictors

# ---------------------------------------------------------------------
# 3. LINEAR MODELS  (continuous outcomes: depression, loneliness)
#    Exploratory question: hours vs. perceived addiction.
# ---------------------------------------------------------------------
fit_linear <- function(outcome) {
  sp <- make_split(df)
  sc <- scale_train_apply(sp$train, sp$test, scale_cols)
  f  <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  m  <- lm(f, data = sc$train)
  cat("\n=== LINEAR:", outcome, "| adj R2 =", round(summary(m)$adj.r.squared, 3), "===\n")
  print(tidy(m))
  cat("VIF:\n"); print(round(vif(m), 2))   # multicollinearity check (car)
  m
}

m_dep <- fit_linear("depression")
m_lon <- fit_linear("loneliness")

# ---------------------------------------------------------------------
# 4. LOGISTIC MODEL  (predictive question: clinically significant depression)
#    Stratified split keeps the 36.8% positive rate in both sets.
# ---------------------------------------------------------------------
sp <- make_split(df, strat = dep10)
sc <- scale_train_apply(sp$train, sp$test, scale_cols)
f  <- as.formula(paste("dep10 ~", paste(predictors, collapse = " + ")))
m_log <- glm(f, data = sc$train, family = binomial())

cat("\n=== LOGISTIC: dep10 (PHQ-9 >= 10) ===\n")
or_tab <- tidy(m_log) %>% mutate(OR = exp(estimate))
print(or_tab)

prob <- predict(m_log, newdata = sc$test, type = "response")
roc_obj <- roc(sc$test$dep10, prob, quiet = TRUE)
cat("Test AUC =", round(as.numeric(auc(roc_obj)), 3), "\n")

# ---------------------------------------------------------------------
# 4b. MID PREDICTIVE: forecast amount of usage from ALL data, incl. the
#     "am I addicted" sentiment (IAT). Tests whether the addiction feeling
#     tracks actual behaviour.  Target = social-media hours.
# ---------------------------------------------------------------------
df <- df %>% mutate(
  sleep_h = recode(sleeptime, "Less than 6 hours"=5.5, "5-6 hours"=5.5,
                   "6-7 hours"=6.5, "More than 7 hours"=7.5),
  study_h = recode(studytime, "Less than 3 hours"=2.5, "3-4 hours"=3.5,
                   "4-5 hours"=4.5, "More than 5 hours"=5.5),
  bullied_yes = if_else(bullied == "Yes", 1L, 0L)
)
usage_feats <- c("TotalIA","depression","loneliness","age","male",
                 "exercise_yes","friends_good","bullied_yes","sleep_h","study_h")
usage_cont  <- c("TotalIA","depression","loneliness","age","sleep_h","study_h")

sp <- make_split(df)
sc <- scale_train_apply(sp$train, sp$test, usage_cont)
f  <- as.formula(paste("sm_hours ~", paste(usage_feats, collapse = " + ")))
m_use <- lm(f, data = sc$train)
pred_use <- predict(m_use, newdata = sc$test)
test_r2 <- 1 - sum((sc$test$sm_hours - pred_use)^2) /
               sum((sc$test$sm_hours - mean(sc$test$sm_hours))^2)
cat("\n=== MID PREDICTIVE: forecast social-media usage (incl. IAT) ===\n")
cat("Train adj R2 =", round(summary(m_use)$adj.r.squared, 3),
    "| Test R2 =", round(test_r2, 3), "\n")
print(tidy(m_use) %>% arrange(p.value))

# IAT-only baseline: how much of usage does the addiction sentiment alone explain?
m_use_iat <- lm(sm_hours ~ scale(TotalIA), data = df)
cat("IAT-only R2 =", round(summary(m_use_iat)$r.squared, 3), "\n")

# Fig 8: standardized coefficients for the usage-forecast model
coef_tab <- tidy(m_use) %>% filter(term != "(Intercept)") %>%
  mutate(sig = p.value < 0.05) %>% arrange(estimate)
ggplot(coef_tab, aes(estimate, reorder(term, estimate), colour = sig)) +
  geom_segment(aes(x = 0, xend = estimate, yend = term)) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("FALSE"="#BBBBBB","TRUE"="#2E6FB7"), guide = "none") +
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.4) +
  labs(x = "Standardized coefficient (predicting social-media usage)", y = NULL,
       title = "What forecasts usage? Perceived addiction dominates")
ggsave("figures/fig8_usage_forecast_coefficients.png", width = 7.2, height = 4.6, dpi = 130)

# ---------------------------------------------------------------------
# 5. THIRD LENS: outcomes across ordered ADDICTION categories (IAC)
# ---------------------------------------------------------------------
lens <- df %>% group_by(iac_sev) %>%
  summarise(n = n(),
            mean_PHQ = round(mean(depression), 1),
            mean_loneliness = round(mean(loneliness), 1), .groups = "drop")
cat("\n=== 3rd LENS: outcomes across addiction bands ===\n"); print(lens)

# ---------------------------------------------------------------------
# 6. EDA + RESULT FIGURES  (ordered factors -> jitter/boxplots, no bars)
# ---------------------------------------------------------------------
theme_set(theme_minimal(base_size = 11))

# Fig 3: IAT across PHQ severity (ordered factor) - jitter + boxplot
ggplot(df, aes(phq_sev, TotalIA)) +
  geom_boxplot(outlier.shape = NA, fill = "#DCE6F2", colour = "#2E6FB7") +
  geom_jitter(width = 0.12, alpha = 0.35, colour = "#2E6FB7", size = 0.9) +
  labs(x = "PHQ-9 depression severity (ordered)", y = "IAT internet-addiction score",
       title = "Internet-addiction score rises across depression severity")
ggsave("figures/fig3_addiction_across_phq_severity.png", width = 7, height = 4.6, dpi = 130)

# Fig 4: depression vs addiction vs raw hours
core <- df %>% select(depression, TotalIA, sm_hours) %>%
  pivot_longer(c(TotalIA, sm_hours), names_to = "measure", values_to = "value") %>%
  mutate(measure = recode(measure, TotalIA = "Perceived addiction (IAT)",
                          sm_hours = "Daily social-media hours"))
ggplot(core, aes(value, depression)) +
  geom_point(alpha = 0.3, colour = "#2E6FB7", size = 0.9) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap(~measure, scales = "free_x") +
  labs(x = NULL, y = "PHQ-9 depression",
       title = "Depression vs perceived addiction vs raw hours")
ggsave("figures/fig4_depression_addiction_vs_hours.png", width = 9, height = 4.2, dpi = 130)

# Fig 6: ROC
png("figures/fig6_roc.png", width = 650, height = 600, res = 130)
plot(roc_obj, col = "#2E6FB7", lwd = 2,
     main = sprintf("ROC: clinically-significant depression (AUC = %.2f)",
                    as.numeric(auc(roc_obj))))
dev.off()

# Fig 7: residual diagnostics for the depression model (appendix)
png("figures/fig7_residual_diagnostics.png", width = 900, height = 400, res = 120)
par(mfrow = c(1, 2)); plot(m_dep, which = c(1, 2)); dev.off()

cat("\nDone. Figures written to figures/.\n")
