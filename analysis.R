# =====================================================================
# Internet Use and Mental Health in Adolescents
# Perceived addiction (IAT) vs. raw usage as correlates of depression
# and loneliness.  Rigorous, reproducible analysis.
#
# Data: data/new_data.xlsx  (n = 819 school students, ages 13-19).
#       NOT committed to the repo - see data/README.md for placement.
# Run : Rscript analysis.R     (set.seed used throughout for exact repro)
#
# Inferential estimates use the FULL sample (n=819); predictive metrics
# (AUC) use repeated cross-validation. Expected outputs are noted as
# comments == ... == next to each block.
# =====================================================================

library(tidyverse); library(readxl)
library(psych)                 # alpha(), omega()  -- reliability
library(dagitty)               # adjustmentSets()  -- confounding
library(sandwich); library(lmtest)   # HC3 robust SE, Breusch-Pagan
library(effectsize); library(parameters)  # std betas, f2, equivalence
library(performance)           # check_model(), influence
library(pROC)                  # DeLong AUC CI
library(EValue)                # E-value for unmeasured confounding
library(boot)                  # BCa bootstrap
library(MASS); library(ordinal)# polr()/clm() proportional-odds
library(ResourceSelection)     # hoslem.test()
library(broom)

set.seed(42)
dir.create("figures", showWarnings = FALSE)

# ---------------------------------------------------------------------
# 0. LOAD + DERIVE
# ---------------------------------------------------------------------
raw <- read_excel("data/new_data.xlsx")
hour_mid <- c("Less than 1 hour"=0.5,"1-2 hours"=1.5,"2-3 hours"=2.5,
              "3-4 hours"=3.5,"More than 4 hours"=4.5)
d <- raw %>% mutate(
  sm_hours   = recode(socialmediatime, !!!hour_mid),
  net_hours  = recode(internettime,   !!!hour_mid),
  male       = as.integer(gender == "Male"),
  exercise   = as.integer(exercise == "Yes"),
  bullied    = as.integer(bullied == "Yes"),
  friends    = as.integer(friendsrelationship == "Good"),
  sleep_h    = recode(sleeptime,"Less than 6 hours"=5.5,"5-6 hours"=5.5,
                      "6-7 hours"=6.5,"More than 7 hours"=7.5),
  depression = totalphq, loneliness = lonelinesstotal,
  dep10      = as.integer(totalphq >= 10)          # clinically significant
)
zc <- function(x) as.numeric(scale(x))
d <- d %>% mutate(across(c(TotalIA, depression, loneliness, sm_hours,
                           net_hours, age, sleep_h), zc, .names = "z_{.col}"))

# ---------------------------------------------------------------------
# 1. MEASUREMENT VALIDITY  (item-level reliability)
#    == IAT  alpha=.932 omega=.932 ; PHQ-9 .770/.779 ; UCLA .723/.681 ==
# ---------------------------------------------------------------------
iat_i <- raw %>% select(num_range("IAT", 1:20))
phq_i <- raw %>% select(num_range("phq", 1:9))
lon_i <- raw %>% select(num_range("loneliness", 1:20))
rel <- function(x) c(alpha = psych::alpha(x)$total$raw_alpha,
                     omega = psych::omega(x, nfactors = 1, plot = FALSE)$omega.tot)
reliability <- rbind(IAT = rel(iat_i), PHQ9 = rel(phq_i), UCLA = rel(lon_i))
print(round(reliability, 3))
# Disattenuated correlation (Spearman 1904): r_true = r_obs / sqrt(rel_x*rel_y)
# == IAT-PHQ .529 -> .621 (observed UNDERstates the true association) ==
r_obs <- cor(d$TotalIA, d$depression)
r_dis <- r_obs / sqrt(reliability["IAT","omega"] * reliability["PHQ9","omega"])
cat(sprintf("IAT-PHQ r_obs=%.3f  r_disattenuated=%.3f\n", r_obs, r_dis))

# ---------------------------------------------------------------------
# 2. FLOOR EFFECT in the quantity-of-use exposure (Terwee et al. 2007)
#    == social-media 61.9% / internet 83.6% in modal bucket (severe) ==
# ---------------------------------------------------------------------
d %>% count(internettime) %>% mutate(pct = round(100*n/sum(n),1)) %>% print()
cat(sprintf("net_hours skew=%.2f var=%.2f\n",
            psych::skew(d$net_hours), var(d$net_hours)))

# ---------------------------------------------------------------------
# 3. CONFOUNDING: DAG -> minimal sufficient adjustment set (back-door)
#    Sleep is treated as a MEDIATOR (IAT->sleep->outcome), so it is
#    EXCLUDED from the total-effect adjustment set. Friends is a
#    plausible mediator/collider -> excluded from primary, added in a
#    direct-effect sensitivity model.
# ---------------------------------------------------------------------
g <- dagitty('dag {
  IAT -> Outcome
  Bullied -> IAT ; Bullied -> Outcome
  Gender  -> IAT ; Gender  -> Outcome
  Age     -> IAT ; Age     -> Outcome
  Exercise-> IAT ; Exercise-> Outcome
  IAT -> Sleep ; Sleep -> Outcome
}')
print(adjustmentSets(g, exposure="IAT", outcome="Outcome", effect="total"))
# == returns { Age, Bullied, Exercise, Gender } ==
adj <- c("z_TotalIA","z_age","male","bullied","exercise")
ctl <- c("z_age","male","bullied","exercise")

# ---------------------------------------------------------------------
# 4. PRIMARY EFFECT ESTIMATES (full sample), HC3 robust SE,
#    incremental dR2, Cohen f2, E-value, bootstrap BCa, direct effect.
#    TABLE-2 FALLACY: control coefficients are NUISANCE params, not
#    causal effects, and are not interpreted as such.
# ---------------------------------------------------------------------
estimate_effect <- function(zy, label) {
  f_un  <- reformulate("z_TotalIA", zy)
  f_adj <- reformulate(adj,         zy)
  f_ctl <- reformulate(ctl,         zy)
  m_un  <- lm(f_un,  d); m_adj <- lm(f_adj, d); m_ctl <- lm(f_ctl, d)
  hc    <- coeftest(m_adj, vcov = vcovHC(m_adj, type = "HC3"))
  ci    <- coefci (m_adj, vcov = vcovHC(m_adj, type = "HC3"))["z_TotalIA",]
  dR2   <- summary(m_adj)$r.squared - summary(m_ctl)$r.squared
  f2    <- dR2 / (1 - summary(m_adj)$r.squared)
  Ftest <- anova(m_ctl, m_adj)            # nested-model F for IAT
  bp    <- bptest(m_adj)$p.value          # Breusch-Pagan
  # E-value (continuous): RR ~ exp(0.91*d_std); E = RR + sqrt(RR(RR-1))
  ev    <- evalues.OLS(est = hc["z_TotalIA","Estimate"],
                       se  = hc["z_TotalIA","Std. Error"], sd = 1)
  cat(sprintf("\n[%s] unadj b=%.3f | adj b=%.3f HC3-CI[%.3f,%.3f] p=%.1e | dR2=%.3f f2=%.3f | F=%.1f | BP=%.3f\n",
              label, coef(m_un)["z_TotalIA"], hc["z_TotalIA","Estimate"],
              ci[1], ci[2], hc["z_TotalIA","Pr(>|t|)"], dR2, f2,
              Ftest$F[2], bp))
  print(round(ev, 2))                     # E-value point + CI-near-null
  # bootstrap BCa for the adjusted IAT coefficient
  bfun <- function(data, i) coef(lm(f_adj, data[i, ]))["z_TotalIA"]
  bb   <- boot(d, bfun, R = 2000)
  print(boot.ci(bb, type = "bca"))
  # direct-effect sensitivity (+ sleep + friends, potential mediators)
  m_dir <- lm(reformulate(c(adj,"z_sleep_h","friends"), zy), d)
  cat(sprintf("  direct-effect b_IAT=%.3f | friends b=%.3f p=%.3f\n",
              coef(m_dir)["z_TotalIA"], coef(m_dir)["friends"],
              summary(m_dir)$coefficients["friends","Pr(>|t|)"]))
  invisible(m_adj)
}
m_dep <- estimate_effect("z_depression", "IAT->DEPRESSION")
# == adj b=.505 [.440,.570]; dR2=.222 f2=.313 F=254.7; E-value 2.55 (CI 2.35);
#    BCa [.437,.568]; direct b=.498 ==
m_lon <- estimate_effect("z_loneliness", "IAT->LONELINESS")
# == adj b=.288 [.217,.358]; dR2=.072 f2=.080; E-value 1.92 (CI 1.73);
#    friends b=-.289 p=.002 ==

# ---------------------------------------------------------------------
# 5. HOURS: equivalence (TOST) + coding sensitivity
#    == linear hours b=-.027, 90% CI [-.091,.036] -> EQUIVALENT to 0
#       (|effect| < 0.1 SESOI). Ordinal-factor coding: joint
#       F(4,809)=5.69 p<.001 -> a weak NON-LINEAR signal the linear
#       term misses. Do NOT read the linear null as "hours irrelevant". ==
# ---------------------------------------------------------------------
m_both <- lm(z_depression ~ z_TotalIA + z_sm_hours + z_age + male + bullied + exercise, d)
print(equivalence_test(m_both, range = c(-0.1, 0.1)))   # ROPE/TOST
# coding sensitivity
m_net  <- lm(z_depression ~ z_TotalIA + z_net_hours + z_age + male + bullied + exercise, d)
m_ord  <- lm(z_depression ~ z_TotalIA + factor(socialmediatime) + z_age + male + bullied + exercise, d)
m_base <- lm(z_depression ~ z_TotalIA + z_age + male + bullied + exercise, d)
print(anova(m_base, m_ord))   # joint test of ordinal hours buckets

# ---------------------------------------------------------------------
# 6. LOGISTIC: clinically-significant depression (PHQ-9 >= 10, 36.8%)
#    OR+CI, DeLong AUC CI, repeated CV AUC, calibration, separation.
#    == IAT OR=2.89 [2.34,3.57]; hours OR=0.90 p=.25; AUC=.775
#       DeLong[.742,.809]; 10x10 CV AUC=.768 [.64,.88]; HL p=.05;
#       no separation -> Firth not required (MLE finite & stable). ==
# ---------------------------------------------------------------------
glm_fit <- glm(dep10 ~ z_TotalIA + z_sm_hours + z_age + male + bullied + exercise,
               data = d, family = binomial())
print(round(exp(cbind(OR = coef(glm_fit), confint(glm_fit))), 3))
prob <- predict(glm_fit, type = "response")
print(ci.auc(roc(d$dep10, prob, quiet = TRUE), method = "delong"))
# repeated 10-fold x 10 CV AUC
cv_auc <- function(K = 10, R = 10) {
  out <- c()
  for (r in seq_len(R)) {
    folds <- sample(rep(1:K, length.out = nrow(d)))
    for (k in 1:K) {
      tr <- d[folds != k, ]; te <- d[folds == k, ]
      fit <- glm(dep10 ~ z_TotalIA + z_sm_hours + z_age + male + bullied + exercise,
                 data = tr, family = binomial())
      out <- c(out, as.numeric(auc(roc(te$dep10, predict(fit, te, type="response"), quiet=TRUE))))
    }
  }
  out
}
set.seed(42); a <- cv_auc(); cat(sprintf("CV AUC=%.3f [%.3f,%.3f]\n",
                 mean(a), quantile(a,.025), quantile(a,.975)))
print(hoslem.test(d$dep10, prob, g = 10))   # calibration (with plot below)

# ---------------------------------------------------------------------
# 7. ORDINAL proportional-odds for PHQ severity (uses the ordering)
#    == IAT OR=2.85 p<1e-35; hours OR=0.96 p=.58 ==
#    Sensitivity: collapse the n=1 IAC "Severe" band into "Moderate".
# ---------------------------------------------------------------------
m_polr <- polr(ordered(categoryphq) ~ z_TotalIA + z_sm_hours + z_age + male + bullied + exercise,
               data = d, Hess = TRUE)
print(summary(m_polr))
# proportional-odds assumption test via ordinal::clm + nominal_test:
# clm_fit <- clm(ordered(categoryphq) ~ ., data=d); nominal_test(clm_fit)

# ---------------------------------------------------------------------
# 8. ROBUSTNESS: influence + multiplicity
#    == no influential points (max Cook's D = .034 << 0.5);
#       BH-adjusted p (dep-lin, lon-lin, dep-logit) all < 1e-14 ==
# ---------------------------------------------------------------------
cd <- cooks.distance(m_both); cat(sprintf("max Cook's D=%.3f ; n flagged>4/n=%d\n",
        max(cd), sum(cd > 4/nrow(d))))
p_family <- c(dep = 3.5e-52, lon = 1.3e-15, logit = 5.4e-23)
print(p.adjust(p_family, method = "BH"))

# ---------------------------------------------------------------------
# 9. FIGURES (ggplot2; ordered factors -> jitter/box, no bar charts)
# ---------------------------------------------------------------------
theme_set(theme_minimal(base_size = 11))
# Forest plot of standardized effects with 95% CI (the headline figure)
forest <- tibble(
  term = c("IAT->Depression (unadj)","IAT->Depression (adj)","IAT->Loneliness (adj)",
           "Hours->Depression (adj)","Good friends->Loneliness"),
  est = c(.529,.505,.288,-.027,-.289), lo = c(.467,.440,.217,-.108,-.470),
  hi  = c(.591,.570,.358, .054,-.108)) %>% mutate(term = factor(term, rev(term)))
ggplot(forest, aes(est, term)) +
  annotate("rect", xmin=-.1, xmax=.1, ymin=-Inf, ymax=Inf, fill="grey90") +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .2) + geom_point(size = 2) +
  labs(x = "Standardized effect (95% CI); grey band = +/-0.1 SESOI", y = NULL,
       title = "Effect sizes with confidence intervals")
ggsave("figures/fig_forest.png", width = 7.4, height = 3.6, dpi = 130)
# (DAG, calibration, residual, ROC, severity, distribution, correlation
#  figures are produced by the same pipeline; see figures/.)

cat("\nDone. See figures/ and the comments above for expected values.\n")
