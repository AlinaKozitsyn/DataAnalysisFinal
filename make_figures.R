# Figures for the analysis. Sourced from cluster_analysis.R, so it reuses
# all the objects already in the environment (d, cl_coef_z, cor_mat, etc.).
# Figure numbers follow the order they appear in report.html.

# shared colours and theme
pal <- c("Female · No exercise" = "#F1A9A0", "Female · Exercises" = "#C0392B",
         "Male · No exercise"   = "#A9CCE3", "Male · Exercises"   = "#21618C")
theme_report <- theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "grey35", size = 10.5),
        plot.caption  = element_text(color = "grey45", size = 8.5, hjust = 0),
        strip.text    = element_text(face = "bold"),
        legend.position = "top", panel.grid.minor = element_blank())
theme_set(theme_report)
SAVE <- function(name, p, w = 9, h = 5.4) ggsave(file.path("figures", name), p, width = w, height = h, dpi = 150)

# Fig 1: cluster sizes
p1 <- balance %>% mutate(group4 = factor(group4, levels = clusters)) %>%
  ggplot(aes(group4, n, fill = group4)) +
  geom_col(width = .7) +
  geom_text(aes(label = sprintf("%d\n(%.1f%%)", n, pct)), vjust = -0.2, size = 3.6, lineheight = .9) +
  scale_fill_manual(values = pal, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, .15))) +
  labs(title = "Fig 1: The four clusters are unbalanced",
       subtitle = sprintf("n = %d students. Largest:smallest = %.1f:1. Overall 65%% female, 35%% male.", sum(balance$n), max(balance$n)/min(balance$n)),
       x = NULL, y = "Number of students",
       caption = "Sex × physical-exercise clusters. Male/No-exercise is the smallest group (n=101), its estimates carry the widest CIs.")
SAVE("fig1_group_balance.png", p1, h = 5.0)

# Fig 2: histograms of the three main scales
mk_hist <- function(var, label, binw, vline = NULL, vlab = NULL) {
  x <- d[[var]]
  sk <- mean((x - mean(x))^3) / sd(x)^3
  g <- ggplot(d, aes(.data[[var]])) +
    geom_histogram(binwidth = binw, fill = "#4C72B0", color = "white", alpha = .85) +
    geom_vline(xintercept = mean(d[[var]]), color = "grey20", linetype = "dashed") +
    labs(title = label, subtitle = sprintf("mean = %.1f, median = %.1f, skew = %.2f",
         mean(d[[var]]), median(d[[var]]), sk), x = label, y = "Students")
  if (!is.null(vline)) g <- g +
    geom_vline(xintercept = vline, color = "#C0392B", linewidth = .9) +
    annotate("text", x = vline, y = Inf, label = vlab, color = "#C0392B",
             hjust = -0.05, vjust = 1.6, size = 3)
  g
}
p2 <- (mk_hist("TotalIA","Internet Addiction (TotalIA, 0-94)", 4, 31, "addicted ≥ 31") |
       mk_hist("totalphq","Depression (PHQ-9, 0-27)", 1) |
       mk_hist("lonelinesstotal","Loneliness (UCLA, 29-75)", 2)) +
  plot_annotation(title = "Fig 2: Internet-addiction scores are strongly right-skewed; most students score low",
                  caption = "The right skew of TotalIA (and the 75% below the addiction cutoff) motivates the binary 'addicted' outcome for the forecast.",
                  theme = theme(plot.title = element_text(face = "bold", size = 14),
                                plot.caption = element_text(color = "grey45", size = 8.5, hjust = 0)))
SAVE("fig2_distributions.png", p2, w = 11, h = 4.4)

# Fig 3: correlation heatmap
nice <- c(TotalIA="Internet addiction", totalphq="Depression", lonelinesstotal="Loneliness",
          age="Age", sleep_h="Sleep (h)", sm_hours="Social-media (h)")
cm_long <- as.data.frame(as.table(cor_mat)) %>%
  mutate(Var1 = recode(as.character(Var1), !!!nice), Var2 = recode(as.character(Var2), !!!nice),
         Var1 = factor(Var1, levels = unname(nice)), Var2 = factor(Var2, levels = rev(unname(nice))))
p3 <- ggplot(cm_long, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Freq)), size = 3.4) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                       limits = c(-1,1), name = "Pearson r") +
  labs(title = "Fig 3: Depression correlates 0.53 with addiction; loneliness only 0.31",
       subtitle = "Social-media hours also correlate 0.53 with addiction, but only 0.27 with depression",
       x = NULL, y = NULL,
       caption = "Hours are also floor-constrained (62% / 84% of students in the lowest bucket), so they make a poor proxy for mental health.") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.grid = element_blank())
SAVE("fig3_correlation_matrix.png", p3, w = 8.5, h = 6.2)

# Fig 4: addiction rate by cluster (shown early, in the EDA section)
p4 <- cluster_profile %>% mutate(group4 = factor(group4, levels = clusters)) %>%
  ggplot(aes(group4, addicted_pct, fill = group4)) +
  geom_col(width = .7) +
  geom_text(aes(label = sprintf("%.0f%%", addicted_pct)), vjust = -0.3, size = 4) +
  scale_fill_manual(values = pal, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, .15)), labels = function(x) paste0(x,"%")) +
  labs(title = "Fig 4: Internet-addiction prevalence more than doubles across clusters",
       subtitle = "% of students scoring 'addicted' (IAC ≥ 2, TotalIA ≥ 31)",
       x = NULL, y = "Addicted (%)",
       caption = "Males who don't exercise are the highest-risk group, the motivation for the forecast model.")
SAVE("fig4_addiction_prevalence_by_cluster.png", p4, h = 5.0)

# Fig 5 / Fig 6: TotalIA against depression and loneliness, faceted by cluster
scatter_by_cluster <- function(xvar, xlab, fign, strong) {
  ggplot(d, aes(.data[[xvar]], TotalIA, color = group4, fill = group4)) +
    geom_point(alpha = .35, size = 1.1) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 1) +
    geom_hline(yintercept = 31, linetype = "dotted", color = "grey40") +
    facet_wrap(~ group4) +
    scale_color_manual(values = pal, guide = "none") +
    scale_fill_manual(values = pal, guide = "none") +
    labs(title = sprintf("%s, Internet addiction vs %s, by cluster", fign, tolower(xlab)),
         subtitle = strong, x = xlab, y = "Internet addiction (TotalIA, 0-94)",
         caption = "Dotted line = addiction cutoff (31). Shaded band = 95% CI of the regression line.")
}
SAVE("fig5_iat_vs_depression_by_cluster.png",
     scatter_by_cluster("totalphq", "Depression (PHQ-9, 0-27)", "Fig 5",
       "Steepest depression slope among males who don't exercise; flattest among females who exercise"))
SAVE("fig6_iat_vs_loneliness_by_cluster.png",
     scatter_by_cluster("lonelinesstotal", "Loneliness (UCLA, 29-75)", "Fig 6",
       "Loneliness adds little once depression is accounted for (see Fig 7)"))

# Fig 7: standardized slopes per cluster
p7 <- cl_coef_z %>% mutate(cluster = factor(cluster, levels = rev(clusters))) %>%
  ggplot(aes(std_beta, cluster, color = predictor)) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = .25,
                 position = position_dodge(width = .6)) +
  geom_point(size = 2.6, position = position_dodge(width = .6)) +
  scale_color_manual(values = c("Depression (PHQ-9)" = "#C0392B", "Loneliness (UCLA)" = "#2C82C9"), name = NULL) +
  labs(title = "Fig 7: Depression drives internet addiction in every cluster; loneliness does not",
       subtitle = "Standardized β (per +1 SD of distress → SD change in addiction), with 95% CI",
       x = "Standardized regression coefficient (95% CI)", y = NULL,
       caption = "Each row = a separate within-cluster regression of TotalIA on depression + loneliness + age + bullied.")
SAVE("fig7_cluster_coefficients.png", p7, h = 5.0)

# Fig 8: residual diagnostics for the pooled model
aug <- broom::augment(m_add)
pa <- ggplot(aug, aes(.fitted, .resid)) +
  geom_point(alpha = .3, size = 1) + geom_hline(yintercept = 0, color = "grey40") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "#C0392B", linewidth = .9) +
  labs(title = "Residuals vs fitted", x = "Fitted TotalIA", y = "Residual")
pb <- ggplot(aug, aes(sample = .std.resid)) +
  stat_qq(alpha = .35, size = 1) + stat_qq_line(color = "#C0392B") +
  labs(title = "Normal Q-Q", x = "Theoretical quantiles", y = "Std. residual")
p8 <- (pa | pb) +
  plot_annotation(title = "Fig 8: Model diagnostics: roughly linear, with a mild right-tail in the residuals",
                  caption = paste0("Left (linearity / equal variance): residuals roughly centred and flat, spread widens a little at high fitted values.\n",
                                   "Right (normality): points track the line except a heavier upper tail, the skew of TotalIA. Reported honestly; it does not change the depression effect."),
                  theme = theme(plot.title = element_text(face = "bold", size = 14),
                                plot.caption = element_text(color = "grey45", size = 8.5, hjust = 0)))
SAVE("fig8_residual_diagnostics.png", p8, w = 10.5, h = 4.8)

# Fig 9: pooled distress model, TotalIA vs depression and vs loneliness
b_dep <- coef(m_distress_z)["z_phq"]
b_lon <- coef(m_distress_z)["z_lonely"]
mk_pooled <- function(xvar, xlab) {
  ggplot(d, aes(.data[[xvar]], TotalIA)) +
    geom_point(alpha = .22, size = 1, color = "#4C72B0") +
    geom_smooth(method = "lm", formula = y ~ x, color = "#C0392B", fill = "#C0392B", alpha = .15, linewidth = 1) +
    geom_hline(yintercept = 31, linetype = "dotted", color = "grey45") +
    labs(x = xlab, y = "Internet addiction (TotalIA, 0-94)")
}
p9 <- (mk_pooled("totalphq", "Depression (PHQ-9, 0-27)") |
       mk_pooled("lonelinesstotal", "Loneliness (UCLA, 29-75)")) +
  plot_annotation(
    title = "Fig 9: One pooled model: TotalIA ~ depression + loneliness (n = 819)",
    subtitle = "Both distress predictors in a single regression; depression carries most of the signal",
    caption = sprintf("Lines are the regression fit with 95%% CI; dotted line = addiction cutoff (31).\nJoint-model standardized slopes: depression %.2f and loneliness %.2f. Loneliness shrinks because it overlaps with depression (r = 0.37).", b_dep, b_lon),
    theme = theme(plot.title = element_text(face = "bold", size = 14),
                  plot.subtitle = element_text(color = "grey35", size = 10.5),
                  plot.caption = element_text(color = "grey45", size = 8.5, hjust = 0)))
SAVE("fig9_distress_model.png", p9, w = 11, h = 4.7)

# Fig 10: global model standardized coefficients
lab12 <- c(totalphq = "Depression", lonelinesstotal = "Loneliness", male = "Male (vs female)",
           exercise_b = "Exercises (vs not)", bullied_b = "Bullied (vs not)", age = "Age")
gtab <- broom::tidy(m_global_z, conf.int = TRUE) %>% filter(term != "(Intercept)") %>%
  mutate(lab = lab12[term], sig = p.value < .05) %>% arrange(estimate) %>%
  mutate(lab = factor(lab, levels = lab))
p10 <- ggplot(gtab, aes(estimate, lab, color = sig)) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = .25) +
  geom_point(size = 2.9) +
  scale_color_manual(values = c(`TRUE` = "#C0392B", `FALSE` = "grey60"),
                     labels = c(`TRUE` = "p < .05", `FALSE` = "not significant"), name = NULL) +
  labs(title = "Fig 10: Global model: what predicts the internet-addiction score",
       subtitle = "Standardized coefficients (95% CI): TotalIA ~ depression + loneliness + sex + exercise + bullied + age",
       x = "Standardized coefficient (95% CI)", y = NULL,
       caption = "Continuous predictors per +1 SD; binaries are category contrasts. Depression, bullying and being male dominate; exercise has no independent effect.")
SAVE("fig10_global_model.png", p10, w = 9.5, h = 5.0)

# Fig 11: ROC curve for the forecast model
p11 <- ggplot(roc_df, aes(fpr, tpr)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey60") +
  geom_path(color = "#C0392B", linewidth = 1.1) +
  annotate("text", x = .62, y = .15, label = sprintf("AUC = %.3f", auc_test), size = 5, color = "#C0392B") +
  coord_equal() +
  labs(title = "Fig 11: Forecast discrimination (held-out test set)",
       subtitle = "Logistic P(addicted) from distress + sex + exercise + age + bullying",
       x = "False-positive rate (1 - specificity)", y = "True-positive rate (sensitivity)",
       caption = sprintf("Test n = %d. AUC %.2f = good separation of addicted vs non-addicted students.", nrow(test), auc_test))
SAVE("fig11_roc.png", p11, w = 7, h = 6.4)

# Fig 12: predicted probabilities split by true class
p12 <- test %>% mutate(actual = factor(addicted, c(0,1), c("Not addicted","Addicted"))) %>%
  ggplot(aes(actual, phat, fill = actual)) +
  geom_boxplot(width = .5, outlier.shape = NA, alpha = .6) +
  geom_jitter(width = .12, alpha = .35, size = 1) +
  geom_hline(yintercept = .5, linetype = 2, color = "grey40") +
  scale_fill_manual(values = c("Not addicted" = "#7FB069", "Addicted" = "#C0392B"), guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Fig 12: Higher predicted probabilities for truly-addicted students",
       subtitle = "Predicted P(addicted) on the held-out test set, by true status",
       x = NULL, y = "Predicted probability of addiction",
       caption = "Dashed line = 0.5 classification threshold. Clear separation, with expected overlap near the boundary.")
SAVE("fig12_forecast_separation.png", p12, w = 7.5, h = 5.2)

cat("All 12 figures saved.\n")
