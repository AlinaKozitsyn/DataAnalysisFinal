# builds report.html from results.rds + the figures (no pandoc needed).
# run after cluster_analysis.R; re-run to refresh the numbers.
suppressMessages({ library(tidyverse); library(knitr) })
r <- readRDS("results.rds")

# helpers
esc <- function(x){
  x <- gsub("&","&amp;",x,fixed=TRUE)
  x <- gsub("<","&lt;",x,fixed=TRUE)
  gsub(">","&gt;",x,fixed=TRUE)
}
fp  <- function(p) ifelse(p < .001, "&lt; .001", sprintf("%.3f", p))
uri <- function(f) knitr::image_uri(file.path("figures", f))
fig <- function(f, n, cap) sprintf('<figure><img alt="Figure %s" src="%s"/><figcaption><span class="fn">Fig %s.</span> %s</figcaption></figure>', n, uri(f), n, cap)
ktab <- function(df, cap="") knitr::kable(df, format="html", escape=FALSE, align="l",
                                          table.attr='class="tbl"', caption=if(nchar(cap)) cap else NULL)
pre <- function(key, title) sprintf('<details><summary>%s</summary><pre>%s</pre></details>', title, esc(r$LOG[[key]]))

ord <- c("Female · No exercise","Female · Exercises","Male · No exercise","Male · Exercises")

# numbers for prose
cc <- r$cl_coef; g <- r$cl_glance
dep <- cc %>% filter(term=="totalphq");        depi <- dep[match(ord, dep$cluster),]
lon <- cc %>% filter(term=="lonelinesstotal"); loni <- lon[match(ord, lon$cluster),]
gi  <- g[match(ord, g$cluster),]
zdep <- r$cl_coef_z %>% filter(predictor=="Depression (PHQ-9)"); zdep <- zdep[match(ord, zdep$cluster),]
zlon <- r$cl_coef_z %>% filter(predictor=="Loneliness (UCLA)");  zlon <- zlon[match(ord, zlon$cluster),]
cp  <- r$cluster_profile[match(ord, r$cluster_profile$group4),]
prof<- r$prof[match(ord, as.character(r$prof$group4)),]
ot  <- r$or_tab %>% filter(term!="(Intercept)")
orv <- function(t) ot %>% filter(term==t)
m   <- r$metrics; cm <- r$cm
intF<- r$interaction$F; intp<- r$interaction$p; intd1<- r$interaction$df1; intd2<- r$interaction$df2

# tables
clreg <- tibble(
  Cluster = ord, n = depi$n,
  `Depression slope (IAT pts / PHQ pt)` = sprintf("%.2f", depi$estimate),
  `std β` = sprintf("%.2f", zdep$std_beta),
  `95% CI (std)` = sprintf("[%.2f, %.2f]", zdep$conf.low, zdep$conf.high),
  `p (dep)` = fp(depi$p.value),
  `Loneliness std β` = sprintf("%.2f", zlon$std_beta),
  `p (lon)` = fp(loni$p.value),
  `R²` = sprintf("%.2f", gi$r.squared),
  `adj R²` = sprintf("%.2f", gi$adj.r.squared))

lab <- c(totalphq="Depression (per +1 PHQ-9 point)", lonelinesstotal="Loneliness (per +1 UCLA point)",
         male="Male (vs. female)", exercise_b="Exercises (vs. not)", age="Age (per +1 year)", bullied_b="Bullied (vs. not)")
ortab <- tibble(Predictor = lab[ot$term],
                `Odds ratio` = sprintf("%.2f", ot$estimate),
                `95% CI` = sprintf("[%.2f, %.2f]", ot$conf.low, ot$conf.high),
                `p` = fp(ot$p.value))

cmtab <- tibble(` ` = c("<b>Predicted: Not addicted</b>","<b>Predicted: Addicted</b>"),
                `Actual: Not addicted` = c(cm["0","0"], cm["1","0"]),
                `Actual: Addicted`     = c(cm["0","1"], cm["1","1"]))

proftab <- tibble(Cluster = ord, n = cp$n,
                  `Addicted (observed)` = sprintf("%.0f%%", cp$addicted_pct),
                  `Mean IAT` = sprintf("%.1f", cp$mean_IAT),
                  `Mean PHQ-9` = sprintf("%.1f", cp$mean_PHQ),
                  `Mean loneliness` = sprintf("%.1f", cp$mean_lonely),
                  `Model P(addicted)†` = sprintf("%.0f%%", 100*prof$phat))

feat <- tibble(
  `Engineered feature` = c("group4","male / exercise_b / bullied_b / friends_good","sleep_h",
                           "sm_hours / net_hours","iat_band","addicted (primary outcome)",
                           "addicted_strict (robustness)","dep_clinical","z_phq / z_lonely / z_age / z_iat","distress"),
  `How it was built` = c(
    "sex × exercise → the 4 cluster labels",
    "Yes/No and Female/Male recoded to 0/1 dummies",
    "sleeptime buckets → numeric midpoints (5.0, 5.5, 6.5, 7.5 h)",
    "social-media / internet time buckets → midpoints (0.5…4.5 h)",
    "IAC 1-4 → ordered labels Normal/Mild/Moderate/Severe",
    "1 if IAC ≥ 2 (TotalIA ≥ 31, Young cutoff), else 0",
    "1 if IAC ≥ 3 (TotalIA ≥ 50, moderate+), else 0",
    "1 if totalphq ≥ 10 (clinically significant depression)",
    "z-scored versions of the scales (comparable slopes)",
    "z(depression) + z(loneliness): a single standardized distress index"),
  `Used for` = c("stratification (the 4 regressions)","logistic predictors / dummies",
                 "EDA / correlation matrix","EDA / 'raw hours' contrast","descriptive prevalence (Fig 4)",
                 "forecast target","sensitivity analysis","descriptive","standardized betas (Fig 7)","optional composite"))

hyp <- tibble(
  `#` = c("H1a","H1b","H2","H3","H4","H5","-"),
  Hypothesis = c(
    "Higher depression → higher internet-addiction score (every cluster)",
    "Higher loneliness → higher internet-addiction score (every cluster)",
    "Physical exercise <i>buffers</i> the distress→addiction link (flatter slope in exercisers)",
    "Males have higher internet-addiction prevalence than females",
    "Perceived addiction (IAT) tracks distress far better than raw screen-time hours",
    "Distress + cluster + controls can forecast the chance of being addicted better than chance",
    "Emergent: clusters differ in addiction <i>level/prevalence</i>, not in the <i>slope</i> of distress"),
  Grounding = c("Soriano-Molina 2025","Wang &amp; Zeng 2024","Wang et al. 2025 (exercise)","Su 2019; Islam et al. 2023",
                "screen-time floor-effect literature","predictive aim","(post-hoc)"),
  Verdict = c(
    sprintf('<span class="ok">CONFIRMED</span>, std β %.2f-%.2f, all p &lt; .001', min(zdep$std_beta), max(zdep$std_beta)),
    '<span class="mid">PARTIAL</span>, small but significant in the pooled models (β 0.11-0.13, p&lt;.001); near zero in 3 of 4 individual clusters',
    sprintf('<span class="no">CONTRADICTED</span>, interaction F(%d,%d)=%.2f, p=%.2f (slopes equal)', intd1, intd2, intF, intp),
    sprintf('<span class="ok">CONFIRMED</span>, %.0f%% vs %.0f%%; male OR=%.2f, p=%s', max(cp$addicted_pct), min(cp$addicted_pct), orv("male")$estimate, sub("^&lt; ","<",fp(orv("male")$p.value))),
    '<span class="ok">CONFIRMED</span>, IAT tracks depression at r=0.53; social-media hours track it only 0.27',
    sprintf('<span class="mid">PARTIAL</span>, AUC=%.2f, accuracy=%.2f &gt; %.2f baseline', r$auc_test, m["acc"], m["base_acc"]),
    '<span class="ok">SUPPORTED</span>, Fig 7 (parallel slopes) + Fig 4 (different heights)'))

# build the HTML, one add() call per block
H <- c()
add <- function(...) H <<- c(H, paste0(...))

add('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">')
add('<title>Internet Addiction across Sex × Exercise Clusters, Analysis Report</title>')
add('<style>
:root{--ink:#1a1a1a;--mut:#5b6470;--line:#e4e7ec;--accent:#C0392B;--accent2:#21618C;--bg:#fbfbfc;--soft:#f4f6f8;}
*{box-sizing:border-box} html{scroll-behavior:smooth}
body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;color:var(--ink);
  background:var(--bg);line-height:1.62;margin:0;font-size:16.5px}
.wrap{max-width:920px;margin:0 auto;padding:54px 26px 90px}
h1{font-size:30px;line-height:1.22;margin:0 0 6px;letter-spacing:-.01em}
h2{font-size:23px;margin:46px 0 12px;padding-bottom:7px;border-bottom:2px solid var(--accent);letter-spacing:-.01em}
h3{font-size:18.5px;margin:30px 0 8px;color:#222}
h4{font-size:15.5px;margin:20px 0 4px;color:var(--accent2);text-transform:uppercase;letter-spacing:.04em}
p{margin:10px 0} a{color:var(--accent2)} small{color:var(--mut)}
.sub{color:var(--mut);font-size:15px;margin:2px 0}
.meta{color:var(--mut);font-size:14.5px;margin:14px 0 0;padding:12px 16px;background:var(--soft);border-radius:10px}
.lead{font-size:17.5px;color:#2a2f36}
figure{margin:22px 0;text-align:center} figure img{max-width:100%;height:auto;border:1px solid var(--line);border-radius:8px;background:#fff}
figcaption{font-size:13.5px;color:var(--mut);margin-top:7px;text-align:left} .fn{color:var(--accent);font-weight:700}
table.tbl{border-collapse:collapse;width:100%;margin:16px 0;font-size:14px}
table.tbl caption{caption-side:top;text-align:left;color:var(--mut);font-size:13px;margin-bottom:6px}
table.tbl th{background:var(--accent);color:#fff;text-align:left;padding:8px 10px;font-weight:600}
table.tbl td{padding:7px 10px;border-bottom:1px solid var(--line);vertical-align:top}
table.tbl tr:nth-child(even) td{background:#fff} table.tbl tr:nth-child(odd) td{background:var(--soft)}
.callout{border-left:4px solid var(--accent2);background:#eef4fa;padding:14px 18px;border-radius:0 8px 8px 0;margin:18px 0}
.finding{border-left:4px solid var(--accent);background:#fbeeec;padding:14px 18px;border-radius:0 8px 8px 0;margin:18px 0}
.rq{background:#fff;border:1px solid var(--line);border-radius:10px;padding:16px 20px;margin:16px 0;box-shadow:0 1px 2px rgba(0,0,0,.03)}
.ok{color:#1d7a3e;font-weight:700} .no{color:#b3261e;font-weight:700} .mid{color:#9a6a00;font-weight:700}
ul,ol{margin:10px 0 10px 4px;padding-left:22px} li{margin:5px 0}
details{margin:8px 0;border:1px solid var(--line);border-radius:8px;background:#fff}
summary{cursor:pointer;padding:9px 14px;font-weight:600;color:var(--accent2)}
details pre{margin:0;padding:12px 14px;border-top:1px solid var(--line);background:#0f1722;color:#d6e2f0;
  font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;font-size:12px;line-height:1.5;overflow-x:auto;border-radius:0 0 8px 8px}
kbd{background:var(--soft);border:1px solid var(--line);border-radius:4px;padding:1px 6px;font-size:13px}
.toc{columns:2;font-size:14.5px;color:var(--accent2)} .toc a{display:block;margin:3px 0}
hr{border:0;border-top:1px solid var(--line);margin:34px 0}
.tag{display:inline-block;background:var(--accent);color:#fff;font-size:11px;font-weight:700;letter-spacing:.06em;
  padding:3px 9px;border-radius:20px;text-transform:uppercase}
</style></head><body><div class="wrap">')

# title
add('<span class="tag">Data Analysis · Final Project</span>')
add('<h1>Does psychological distress predict internet addiction differently across sex × exercise groups of adolescents?</h1>')
add('<p class="sub">An exploratory and predictive analysis of 819 Bangladeshi adolescents, treating perceived internet addiction (IAT) as the outcome of depression and loneliness within four sex × physical-activity clusters.</p>')
add('<div class="meta"><b>Authors:</b> Noam Avizmil · Tomer Golshani · May Mandalaoui · Alina Kozitsyna &nbsp;|&nbsp; <b>Course:</b> CIS2601, Introduction to Data Science (2025-2026)<br>',
    '<b>Data:</b> Mahmud et&nbsp;al. (2024), Mendeley Data V2, <a href="https://doi.org/10.17632/yzcwfyrxy2.2">doi:10.17632/yzcwfyrxy2.2</a> (CC&nbsp;BY&nbsp;4.0) &nbsp;|&nbsp; <b>Analysis:</b> R 4.5.1, <kbd>cluster_analysis.R</kbd>, <kbd>make_figures.R</kbd>, <kbd>build_report.R</kbd><br>',
    '<small>This is an internal discovery report: it documents every hypothesis we tested, every design decision we made, and the full data-exploration log.</small></div>')

# abstract
add('<h2 id="abstract">Abstract</h2>')
add(sprintf('<p class="lead">We ask whether depression and loneliness turn into <i>perceived internet addiction</i> (Internet Addiction Test, IAT) differently depending on a student’s sex and whether they exercise. Across 819 adolescents we fit one linear regression of the IAT score per cluster (4 clusters = sex × exercise) and a logistic regression that forecasts the <i>chance</i> of being internet-addicted (IAT ≥ 31, the Young cutoff). <b>Depression is a strong, significant predictor of internet addiction in every cluster</b> (standardized β %.2f-%.2f, all p &lt; .001), explaining %.0f-%.0f%% of the variance, while loneliness is a much weaker predictor, small but statistically significant in the pooled models, and near zero within the individual clusters. Contrary to our hypothesis, exercise does not flatten the distress-to-addiction slope (cluster interaction p = %.2f). What the clusters differ in is level: %.0f%% of non-exercising boys score as addicted, versus about %.0f%% of girls. The forecast model reaches <b>AUC = %.2f</b> (accuracy %.2f vs %.2f baseline), with depression, being male, age and being bullied all raising the odds. So the short version is that distress hits every subgroup about equally hard, but non-exercising boys start from a much higher baseline.</p>',
            min(zdep$std_beta), max(zdep$std_beta), 100*min(gi$r.squared), 100*max(gi$r.squared),
            intp, max(cp$addicted_pct), mean(cp$addicted_pct[1:2]), r$auc_test, m["acc"], m["base_acc"]))

# TOC
add('<h2 id="toc">Contents</h2><div class="toc">',
    '<a href="#intro">1 · Introduction &amp; research questions</a>',
    '<a href="#hyp">2 · Hypotheses</a>',
    '<a href="#decisions">3 · Design decisions (our analytical journey)</a>',
    '<a href="#data">4 · Data overview &amp; how it was collected</a>',
    '<a href="#methods">5 · Methods, preprocessing &amp; feature engineering</a>',
    '<a href="#eda">6 · Exploratory data analysis</a>',
    '<a href="#explore">7 · Exploratory results: the four cluster regressions</a>',
    '<a href="#forecast">8 · Forecast: the chance of being internet-addicted</a>',
    '<a href="#verdicts">9 · Hypotheses vs. results</a>',
    '<a href="#limits">10 · Limitations &amp; future work</a>',
    '<a href="#repro">11 · References &amp; reproducibility</a>',
    '<a href="#log">Appendix · Full data-exploration log</a>',
    '</div>')

# 1 Introduction
add('<h2 id="intro">1 · Introduction &amp; research questions</h2>')
add('<div class="rq"><h4>Exploratory question</h4><p>Within each of the four sex × exercise clusters (girls who exercise, girls who don’t, boys who exercise, boys who don’t), do <b>depression</b> and <b>loneliness</b> predict a higher internet-addiction score (IAT), and is that relationship stronger in some clusters than in others?</p>',
    '<h4>Predictive question</h4><p>Given a student’s distress and cluster, can we forecast the chance that they are internet-addicted (IAT ≥ 31)?</p></div>')
add('<p><b>Why it matters.</b> Adolescent internet addiction is rising worldwide and tends to come with depression, anxiety, poor sleep and suicidal ideation. If distress is what turns ordinary internet use into compulsive, out-of-control use, then mental-health support (and not just screen-time limits) is the thing to act on. The problem is hard for a few reasons. Addiction, depression and loneliness are all latent psychological states, measured by self-report at a single moment. Raw "hours online" is a poor proxy for losing control. And the effects may differ by sex and lifestyle, so a single pooled model can hide the real structure.</p>')
add('<h4>What prior work tells us</h4><ul>',
    '<li>A meta-analysis of 33 studies (303,243 adolescents) found internet addiction positively correlated with depression (pooled r⁺ = 0.318) and other mental-health harms (Soriano-Molina et&nbsp;al., 2025).</li>',
    '<li>A meta-analysis of 32 effect sizes found loneliness linked to internet addiction (r = 0.291), and the link <i>weakened as the share of males rose</i> and was weaker in adolescents than in adults (Wang &amp; Zeng, 2024).</li>',
    '<li>A meta-analysis of 9 RCTs found that exercise interventions reduced internet-addiction scores (SMD = -1.11), so exercise has causal support as a protective factor rather than being only a correlate (Wang et&nbsp;al., 2025).</li>',
    '<li>A meta-analysis across 34 jurisdictions found males are more likely to be internet-addicted (g = 0.145), and the gap was <i>largest in Asia</i> (Su et&nbsp;al., 2019).</li>',
    '<li>In Bangladesh specifically, a study of 502 adolescents reported very high internet-addiction and loneliness prevalence during COVID-19 (Islam et&nbsp;al., 2023).</li></ul>')
add('<p><b>The gap and our contribution.</b> These facts have been established <i>separately</i>. No study we found, and none using this Bangladeshi dataset, asks whether distress predicts perceived addiction <i>differently across sex × exercise subgroups within one sample</i>. We take two documented main effects (sex moderates the link, and exercise lowers addiction) and turn them into a single interaction test that can be falsified: does exercise buffer the distress-to-addiction link, and does that buffering differ by sex? We also keep <i>subjective</i> addiction (IAT) separate from <i>raw exposure</i> (hours), which a lot of earlier cross-sectional work mixes together.</p>')

# 2 Hypotheses
add('<h2 id="hyp">2 · Hypotheses</h2>')
add('<p>We stated these expectations <i>before</i> modelling, grounded in the literature above. The right-hand column is filled in from our results (full detail in §9).</p>')
add(ktab(hyp))

# 3 Design decisions
add('<h2 id="decisions">3 · Design decisions (our analytical journey)</h2>')
add('<p>This section records the choices we made and why, including the methods we deliberately avoided so as to stay within what the course covers.</p>')
add('<h4>1. Outcome = perceived addiction (IAT), not "hours online"</h4>',
    '<p>We chose the IAT score as the thing to explain and forecast, rather than self-reported social-media or internet hours, for three reasons. First, the hours questions are 5-bucket self-reports that pile up at the floor (62% of students sit in the lowest social-media bucket, 84% for internet), so there is little variance left to model. Second, hours correlate only weakly with distress (<b>r = 0.27</b> with depression), while IAT correlates <b>r = 0.53</b>. Third, IAT measures loss of control, which is the construct that actually tracks mental health (see Fig 3).</p>')
add('<h4>2. Forecast = the <i>chance</i> of being addicted (logistic), not the raw score</h4>',
    '<p>For prediction we model <b>P(internet-addicted)</b> with logistic regression. "Addicted" = <b>IAC ≥ 2 (IAT ≥ 31)</b>, the standard Young cutoff for "problems beyond normal use" (25% of students; well-distributed across clusters). We re-checked everything against a stricter cutoff (IAC ≥ 3, IAT ≥ 50; 10%) as a robustness lens.</p>')
add('<h4>3. Predictors and controls</h4>',
    '<p>Our main predictors are depression and loneliness. The cluster variables (sex, exercise) are held fixed by stratification in the 4 linear models, and they enter as predictors in the single pooled forecast. For controls we use age and bullied, since both tend to raise distress and online withdrawal. We left out sleep on purpose: it is plausibly a downstream consequence of addiction (more addiction → less sleep, a post-treatment variable), so putting it on the predictor side would bias the effect. We check this directly in §7. We also dropped the many app and device flags, which add noise and would lead to the over-slicing our proposal feedback warned us about.</p>')
add('<h4>4. Methods we kept at course level</h4>',
    '<p>We check the model assumptions the way the course teaches them, that is, visually (Residuals-vs-Fitted and Normal Q-Q, Fig 8) and through the simple correlation between predictors for collinearity (r = ',
    sprintf('%.2f', r$pred_cor), '). We did not use a Youden-optimal threshold, the Breusch-Pagan test, or robust (HC3) standard errors. Those were beyond the syllabus. So where heteroscedasticity shows up we report it honestly, and we lean on the fact that the depression effect shows up again in all four independent cluster regressions, with CIs far from zero.</p>')
add('<h4>5. Reproducibility</h4>',
    '<p>One Google-Form export gives one clean table (819×91, zero missing). All randomness is seeded with <kbd>set.seed(42)</kbd>, including a seed placed right before the train/test split, so the forecast metrics reproduce even if the code upstream changes.</p>')

# 4 Data overview
add('<h2 id="data">4 · Data overview &amp; how it was collected</h2>')
add('<p><b>Source.</b> Mahmud, Siddik, Arefin &amp; Rahman (2024), <i>"A data set exploring the link and associated factors of internet addiction, loneliness, and depression among adolescents in Bangladesh"</i>, Mendeley Data V2 (<a href="https://doi.org/10.17632/yzcwfyrxy2.2">doi:10.17632/yzcwfyrxy2.2</a>, CC&nbsp;BY&nbsp;4.0). The <kbd>Data in Brief … Google Forms.pdf</kbd> in our repo is the <i>survey instrument</i>, not a paper.</p>')
add('<p><b>Entities and size.</b> One row is one school or college student. There are <b>n = 819</b> adolescents, ages <b>13-19</b> (mean about 15.8), from Bangladesh (Sylhet division, per the authors’ companion paper), collected by Google Forms behind a voluntary informed-consent gate. The data is cross-sectional, with <b>no missing values</b> in any of the 91 columns. Sampling was by convenience (non-probability), so the sample is not representative. It skews <b>65% female</b> and toward ages 14-16.</p>')
add('<p><b>How the key variables were measured (validated instruments).</b></p><ul>',
    '<li><b>Internet Addiction Test</b> (IAT; Young, 1998), 20 items → <kbd>TotalIA</kbd> (0-94). Nominal bands: Normal 0-30, Mild 31-49, Moderate 50-79, Severe 80+ (in this sample scores top out at 94).</li>',
    '<li><b>PHQ-9 depression</b> (Kroenke et&nbsp;al., 2001), 9 items → <kbd>totalphq</kbd> (0-27).</li>',
    '<li><b>UCLA Loneliness Scale v3</b> (Russell, 1996), 20 items → <kbd>lonelinesstotal</kbd> (observed 29-75).</li>',
    '<li>Plus ~36 self-report items: demographics, who they live with, device, sleep, exercise, sports, bullying, friendship and parent relationships, social-media tenure and daily time.</li></ul>')
add('<div class="callout"><b>Data-quality sanity check.</b> The UCLA score requires reverse-coding of half its items, so we checked that it behaves correctly. <kbd>lonelinesstotal</kbd> correlates <i>positively</i> with depression (r = 0.37) and addiction (r = 0.31), which is what a valid loneliness score should do, so the scoring direction is sound.</div>')
add('<p><b>Outcomes.</b> Exploratory Y = <kbd>TotalIA</kbd> (continuous). Forecast Y = <kbd>addicted</kbd> = 1 if IAT ≥ 31.</p>')
add('<p><b>Class balance.</b> The four clusters are unbalanced (largest to smallest is about 2.8:1; Fig 1), and the addiction outcome is 25% positive, which is balanced enough to model. The smallest cluster, non-exercising boys (n = 101), carries the widest confidence intervals, and we flag this throughout.</p>')
add(fig("fig1_group_balance.png", 1, "Sample size of each sex × exercise cluster. The design is observational and unbalanced; non-exercising boys are the smallest group."))

# 5 Methods
add('<h2 id="methods">5 · Methods, preprocessing &amp; feature engineering</h2>')
add('<p><b>Models.</b> A continuous score calls for linear regression, and a yes/no outcome calls for logistic regression. So we fit (i) four linear regressions, one per cluster, of <kbd>TotalIA ~ depression + loneliness + age + bullied</kbd>; (ii) a pooled interaction model that tests whether the depression and loneliness slopes differ across clusters; (iii) once that interaction comes back null, two whole-sample models, a distress-only model and a global additive model with every variable, plus a quick test of whether sleep is a downstream consequence of addiction; and (iv) one logistic forecast of P(addicted) from depression + loneliness + sex + exercise + age + bullied.</p>')
add('<p><b>Preprocessing.</b> There was no missing data to impute. For the forecast we use a 70/30 train/test split, stratified by cluster, fitting on train and scoring on the held-out test set; the inferential odds ratios use the full sample. We did not upsample, since at 25% positives the imbalance is mild and AUC does not depend on the threshold.</p>')
add('<h4>Feature engineering</h4><p>Everything we derived, and where it is used. All of it is written into <kbd>data/enriched_data.csv</kbd>.</p>')
add(ktab(feat))

# 6 EDA
add('<h2 id="eda">6 · Exploratory data analysis</h2>')
add('<p>The three scales behave very differently. Internet addiction is strongly right-skewed, with most students near the floor (skew about 1.0; Fig 2). Depression uses its full range, and loneliness is the most symmetric of the three. The right skew of IAT, together with the fact that about 75% sit below the addiction cutoff, is why the forecast uses a binary outcome.</p>')
add(fig("fig2_distributions.png", 2, "Distributions of the three core scales, with means (dashed) and the IAT addiction cutoff (red). IAT is heavily right-skewed."))
add('<p><b>Depression correlates with internet addiction at r = 0.53</b>, far ahead of loneliness (0.31). Social-media hours correlate with addiction just as strongly (also 0.53), but that is expected, since hours and the IAT both partly measure internet use. What matters for us is that hours barely track <i>distress</i> (only r = 0.27 with depression), so they are a poor stand-in for mental health. Depression and loneliness correlate only modestly with each other (r = 0.37), so both can sit in the same model without collinearity trouble.</p>')
add(fig("fig3_correlation_matrix.png", 3, "Pearson correlations among the focal numeric features. Depression and social-media hours both correlate 0.53 with addiction, but hours barely track distress (0.27 with depression)."))
add('<p>Splitting by cluster shows the pattern that motivated the project: <b>addiction prevalence more than doubles</b> from girls (about 19-20%) to non-exercising boys (about 45%).</p>')
add(fig("fig4_addiction_prevalence_by_cluster.png", 4, "Observed share of students scoring as internet-addicted (IAT ≥ 31) per cluster. Non-exercising boys are the clear high-risk group."))

# 7 Exploratory results
add('<h2 id="explore">7 · Exploratory results: the four cluster regressions</h2>')
add('<p>In every cluster, <b>depression is a strong, highly significant predictor</b> of the internet-addiction score, with a clear positive slope in all four panels (Fig 5). Each extra PHQ-9 point is worth roughly <b>1.2-1.5 IAT points</b>. Loneliness is much weaker (Fig 6): within the individual clusters its standardized slope sits near zero and is significant only for girls who exercise, though in the pooled models below, with the full sample’s power, it turns out small but significant. Each model explains a meaningful 25-34% of the variance.</p>')
add(ktab(clreg, "Per-cluster regression of TotalIA on depression + loneliness (+ age, bullied). std β = standardized slope; CIs are 95%."))
add(fig("fig5_iat_vs_depression_by_cluster.png", 5, "Internet addiction vs depression within each cluster, with fitted line and 95% CI. The positive slope is clear and similar in all four panels."))
add(fig("fig6_iat_vs_loneliness_by_cluster.png", 6, "Internet addiction vs loneliness within each cluster. Slopes are shallow, so loneliness carries little independent signal."))
add(fig("fig7_cluster_coefficients.png", 7, "Standardized depression (red) and loneliness (blue) slopes per cluster, 95% CI. Depression is strong and significant everywhere; loneliness overlaps zero in three of four clusters."))
add(sprintf('<div class="finding"><b>Do the slopes differ across clusters?</b> No. The pooled interaction test is <b>not significant: F(%d,&nbsp;%d) = %.2f, p = %.2f</b>. Statistically, depression pushes internet addiction just as hard for non-exercising boys as it does for exercising girls. This contradicts our hypothesis (H2) that exercise buffers the distress-to-addiction link.</div>', intd1, intd2, intF, intp))
add('<div class="finding"><b>A closer look: levels, not slopes.</b> If exercise doesn’t flatten the slope, why are non-exercising boys so much more addicted (Fig 4)? The reason is that the clusters differ in where they start, not in how steeply distress bites. Non-exercising boys carry the highest mean depression (10.2 vs 7.7-8.0 for girls and 8.7 for exercising boys) and a higher male baseline, so the same slope sitting on a higher intercept lands many more of them above the addiction line. In practical terms, distress drives addiction across the board, while sex and inactivity set the baseline risk. Prevention should target the high-baseline subgroup, but the underlying mechanism, distress, is shared by everyone.</div>')

add('<h3>Checking the model assumptions</h3>')
add(sprintf('<p>We check the regression assumptions visually, the way the course teaches. The <b>Residuals-vs-Fitted</b> plot is broadly flat, so linearity holds and there is no systematic curve, and the <b>Normal Q-Q</b> points track the reference line apart from a mildly heavy upper tail (the right-skew of IAT). The two predictors correlate only <b>r = %.2f</b>, so multicollinearity is not a concern and both stay in the model. The mild unevenness in residual spread does not change the sign or the large size of the depression effect, and that effect shows up again in all four independent cluster regressions with confidence intervals far from zero.</p>', r$pred_cor))
add(fig("fig8_residual_diagnostics.png", 8, "Residuals-vs-Fitted (linearity / equal variance) and Normal Q-Q (residual normality) for the pooled model. Both are acceptable, with a mild right-tail reported honestly."))

# 7b pooled whole-sample models
add('<h3>Pooling the clusters: two whole-sample models</h3>')
add(sprintf('<p>Because the slopes don’t differ across clusters (the interaction was not significant), we can drop the stratification and fit single models on all %d students. <b>Model A</b> keeps just the two distress predictors. <b>Model B</b> is a global additive model that also brings in sex, exercise, bullying and age.</p>', r$n))

# Model A: distress only
mdt <- r$m_distress$tidy %>% filter(term != "(Intercept)")
sbA <- c(totalphq = r$m_distress$beta_dep, lonelinesstotal = r$m_distress$beta_lon)
modelA_tab <- tibble(
  Predictor = c("Depression (per +1 PHQ-9 pt)", "Loneliness (per +1 UCLA pt)"),
  `b (95% CI)` = sprintf("%.2f [%.2f, %.2f]", mdt$estimate, mdt$conf.low, mdt$conf.high),
  `std β` = sprintf("%.2f", sbA[mdt$term]),
  `p` = fp(mdt$p.value))
gA <- r$m_distress$glance
add('<h4>Model A, distress only</h4>')
add(sprintf('<p>Both predictors are significant, but depression dominates (Fig 9): its standardized slope is %.2f against loneliness’s %.2f, and each extra PHQ-9 point adds about %.2f IAT points. Together they explain R² = %.2f of the variance.</p>',
            r$m_distress$beta_dep, r$m_distress$beta_lon, mdt$estimate[1], gA$r.squared))
add(ktab(modelA_tab, sprintf("Model A: TotalIA ~ depression + loneliness (n = %d). R² = %.2f, adj. R² = %.2f, RSE = %.1f. p-value shown for each predictor.", r$n, gA$r.squared, gA$adj.r.squared, gA$sigma)))
add(fig("fig9_distress_model.png", 9, "The two distress predictors against internet addiction (whole sample). Depression has a steep, tight slope; loneliness is shallow and shrinks further in the joint model because it overlaps with depression."))

# Model B: global model
mgt <- r$m_global$tidy %>% filter(term != "(Intercept)")
mgz <- r$m_global$z
labB <- c(totalphq = "Depression (per +1 PHQ-9 pt)", lonelinesstotal = "Loneliness (per +1 UCLA pt)",
          male = "Male (vs female)", exercise_b = "Exercises (vs not)", bullied_b = "Bullied (vs not)", age = "Age (per +1 yr)")
modelB_tab <- tibble(
  Predictor = labB[mgt$term],
  `b (95% CI)` = sprintf("%.2f [%.2f, %.2f]", mgt$estimate, mgt$conf.low, mgt$conf.high),
  `std β` = sprintf("%.2f", mgz$estimate[match(mgt$term, mgz$term)]),
  `p` = fp(mgt$p.value),
  `Sig.` = ifelse(mgt$p.value < .05, "✓", "-"))
gB <- r$m_global$glance; nab <- r$nested_AB
gv <- function(t) mgz$estimate[mgz$term == t]
add('<h4>Model B, the global model</h4>')
add(sprintf('<p>Adding sex, exercise, bullying and age lifts R² to %.2f. The biggest standardized effects (Fig 10) are depression (%.2f), being bullied (%.2f) and being male (%.2f); being bullied or male each adds roughly 5 IAT points. Loneliness (%.2f) and age (%.2f) are smaller but still significant. The one null is <b>exercise</b>: its coefficient is essentially zero (std β %.2f, p = %s). So once distress and sex are in the model, whether a student exercises adds nothing to their predicted addiction score, which echoes the non-significant exercise term in the forecast. The four extra variables do jointly improve the fit (nested F(%d, %d) = %.1f, p &lt; .001).</p>',
            gB$r.squared, gv("totalphq"), gv("bullied_b"), gv("male"), gv("lonelinesstotal"), gv("age"),
            gv("exercise_b"), fp(mgt$p.value[mgt$term == "exercise_b"]), nab$df1, nab$df2, nab$F))
add(ktab(modelB_tab, sprintf("Model B (global): TotalIA ~ depression + loneliness + sex + exercise + bullied + age. n = %d, R² = %.2f, adj. R² = %.2f, RSE = %.1f. Every variable’s p-value and significance is shown.", r$n, gB$r.squared, gB$adj.r.squared, gB$sigma)))
add(fig("fig10_global_model.png", 10, "Standardized coefficients with 95% CI for the global model. Red = significant (p < .05), grey = not. Depression, bullying and being male lead; exercise sits on zero."))

# sleep mediator test
st <- r$sleep_test; bs <- st$tidy %>% filter(term == "TotalIA"); bsa <- st$adj
bb <- st$by_band %>% arrange(mean_IAT)
sleeptab <- tibble(`IAT band` = as.character(bb$iat_band), n = bb$n,
                   `Mean sleep (h)` = sprintf("%.2f", bb$mean_sleep),
                   `Mean IAT` = sprintf("%.1f", bb$mean_IAT))
add('<h4>Why sleep is not a predictor: a quick mediator check</h4>')
add(sprintf('<div class="callout">We leave <b>sleep</b> out of the model on purpose, because sleep is more plausibly a <i>consequence</i> of addiction than a cause of it. The data agrees. Regressing sleep on addiction (<kbd>sleep_h ~ TotalIA</kbd>) gives only a tiny negative slope, about %.0f minutes less sleep per +10 IAT points (b = %.3f h per point, p = %s), and it disappears once we control for age and sex (p = %s). So sleep is at most a weak downstream effect of addiction, not a driver of it, which is exactly why it does not belong on the predictor side. Mean sleep does edge down across the addiction bands (below; the Severe band is one student).</div>',
            abs(10 * bs$estimate * 60), bs$estimate, fp(bs$p.value), fp(bsa$p.value)))
add(ktab(sleeptab, "Mean sleep by IAT severity band (descriptive)."))

# 8 Forecast
add('<h2 id="forecast">8 · Forecast: the chance of being internet-addicted</h2>')
add('<p>The logistic model predicts the probability that a student is addicted. Reading off the odds ratios, each extra <b>PHQ-9 point raises the odds of addiction by about 19%</b>. Being male, being older, and having been bullied all raise the odds too. Exercise points in a protective direction but is not significant once distress and sex are accounted for.</p>')
add(ktab(ortab, "Logistic regression for P(internet-addicted). Odds ratios with 95% CIs (full sample)."))
add(sprintf('<p><b>How well does it forecast?</b> On the held-out test set (n = %d, %.0f%% addicted) the model reaches <b>AUC = %.2f</b> (Fig 11), which is fair-to-good discrimination, and <b>accuracy %.2f</b>, beating the %.2f majority-class baseline. At the default 0.5 cutoff it is cautious, with <b>specificity %.2f</b> (it rarely false-alarms) but <b>sensitivity %.2f</b> (it misses some true cases). That is the trade-off you expect with a 25%%-positive class. Since AUC does not depend on the cutoff, it is the fairer summary of how well the model ranks students, and the predicted probabilities clearly separate the two groups (Fig 12).</p>',
            m["test_n"], 100*m["test_pos"], r$auc_test, m["acc"], m["base_acc"], m["spec"], m["sens"]))
add('<div style="display:flex;gap:18px;flex-wrap:wrap;align-items:flex-start">')
add('<div style="flex:1;min-width:280px">', as.character(ktab(cmtab, "Confusion matrix (test set, 0.5 threshold).")), '</div>')
add('<div style="flex:1;min-width:280px">', as.character(ktab(proftab, "Observed vs model-predicted addiction by cluster. †Model P at each cluster’s mean distress.")), '</div></div>')
add(fig("fig11_roc.png", 11, "ROC curve on the held-out test set. The curve bows well above the chance diagonal."))
add(fig("fig12_forecast_separation.png", 12, "Predicted probabilities are clearly higher for students who really are addicted, with the expected overlap near the boundary."))

# 9 verdicts
add('<h2 id="verdicts">9 · Hypotheses vs. results</h2>')
add(ktab(hyp))
add('<p>Putting it together: depression is the main predictor of perceived internet addiction, well ahead of loneliness, and it works at about the same rate in every subgroup. What changes from group to group is the starting point. Non-exercising boys are far likelier to cross into addiction because they begin with more distress and a higher male baseline, not because distress affects them more strongly.</p>')

# 10 limitations
add('<h2 id="limits">10 · Limitations &amp; future work</h2>')
add('<h4>Limitations</h4><ul>',
    '<li><b>Cross-sectional, single questionnaire.</b> Addiction, depression and loneliness were all measured at one moment on one Google Form. So we cannot establish direction (does distress drive addiction, or the other way round?), and the shared method inflates the correlations (common-method bias). Our language is associational, never causal.</li>',
    '<li><b>Convenience sample.</b> 819 Bangladeshi adolescents, 65% female, concentrated at ages 14-16, almost all living with family. The results may not generalize to other ages, countries, or out-of-school youth.</li>',
    '<li><b>Small and unbalanced clusters.</b> Non-exercising boys (n = 101) carry the widest CIs, and the null interaction test has limited power to detect modest slope differences, so "no moderation" really means "no <i>detectable</i> moderation."</li>',
    '<li><b>Self-report and coarse exposure.</b> Screen-time is binned and floor-constrained, exercise is a yes/no with no dose or intensity, and perceived addiction is subjective.</li>',
    '<li><b>Skew and mild heteroscedasticity.</b> IAT is right-skewed and the residual spread widens at high fitted values (Fig 8). This slightly affects precision, but not the sign or the large size of the depression effect.</li>',
    '<li><b>Omitted variables.</b> We did not model socio-economic status, parental mental health, academic pressure or prior clinical history.</li></ul>')
add('<h4>Future work</h4><ul>',
    '<li><b>Longitudinal data</b> (≥2 waves) to test the depression⇄addiction direction with cross-lagged models.</li>',
    '<li><b>Objective exposure</b>, such as device screen-time logs instead of buckets, plus an <b>exercise dose</b> (frequency and intensity) to test a real protective gradient.</li>',
    '<li><b>Larger, representative, multi-country sampling</b>; multilevel models nesting students within schools.</li>',
    '<li><b>Mediation analysis</b> (distress → sleep loss → addiction) and a formal test of the "levels-not-slopes" account with bigger subgroups.</li></ul>')

# 11 references
add('<h2 id="repro">11 · References &amp; reproducibility</h2>')
add('<h4>Reproduce</h4><p>R 4.5.1 with <kbd>tidyverse, readxl, broom, ggplot2, patchwork, scales, knitr</kbd>. Run <kbd>Rscript cluster_analysis.R</kbd> (loads the Excel, engineers features → <kbd>data/enriched_data.csv</kbd>, fits all models, writes <kbd>results.rds</kbd> and all figures), then <kbd>Rscript build_report.R</kbd> to regenerate this page. All seeds fixed.</p>')
add('<h4>References (APA)</h4><ol style="font-size:14px">',
    '<li>Mahmud, A., Siddik, M. A. B., Arefin, M. M., &amp; Rahman, M. M. (2024). <i>A data set exploring the link and associated factors of internet addiction, loneliness, and depression among adolescents in Bangladesh</i> [Data set]. Mendeley Data, V2. https://doi.org/10.17632/yzcwfyrxy2.2</li>',
    '<li>Young, K. S. (1998). <i>Caught in the Net</i>. Wiley. (Internet Addiction Test)</li>',
    '<li>Kroenke, K., Spitzer, R. L., &amp; Williams, J. B. W. (2001). The PHQ-9. <i>Journal of General Internal Medicine, 16</i>(9), 606-613.</li>',
    '<li>Russell, D. W. (1996). UCLA Loneliness Scale (Version 3). <i>Journal of Personality Assessment, 66</i>(1), 20-40.</li>',
    '<li>Soriano-Molina, E., Liminana-Gras, R. M., Patro-Hernandez, R. M., &amp; Rubio-Aparicio, M. (2025). The association between internet addiction and adolescents’ mental health: A meta-analytic review. <i>Behavioral Sciences, 15</i>(2), 116.</li>',
    '<li>Wang, Y., &amp; Zeng, Y. (2024). Relationship between loneliness and internet addiction: a meta-analysis. <i>BMC Public Health</i>.</li>',
    '<li>Su, W., Han, X., Jin, C., Yan, Y., &amp; Potenza, M. N. (2019). Are males more likely to be addicted to the internet than females? <i>Computers in Human Behavior, 99</i>, 86-100.</li>',
    '<li>Wang, L., Chen, Y., Cho, J. H., Yang, H., &amp; Seo, J. C. (2025). Effectiveness of exercise as an intervention for internet addiction in adolescents: a meta-analysis. <i>PeerJ, 13</i>, e19999.</li>',
    '<li>Islam, M. R., Apu, M. M. H., Akter, R., Tultul, P. S., Anjum, R., Nahar, Z., Shahriar, M., &amp; Bhuiyan, M. A. (2023). Internet addiction and loneliness among school-going adolescents in Bangladesh in the context of the COVID-19 pandemic: Findings from a cross-sectional study. <i>Heliyon, 9</i>(2), e13340.</li></ol>')

# appendix log
add('<h2 id="log">Appendix · Full data-exploration log</h2>')
add('<p>Every glimpse we ran on the data, captured verbatim from R (expand each).</p>')
add(pre("dim","Dataset size"))
add(pre("glimpse","glimpse() of the key variables"))
add(pre("na","Missing-value check"))
add(pre("summary_key","summary() of the key numeric variables"))
add(pre("cat_levels","Category levels (counts) for the categorical variables"))
add(pre("band_map","IAT severity band → score range (dataset cutoffs)"))
add(pre("thresholds","Binary 'addicted' threshold counts"))
add(pre("balance","Cluster balance"))
add(pre("cluster_profile","Per-cluster profile (prevalence + mean scores)"))
add(pre("cormat","Correlation matrix"))
add(pre("cl_models","The four cluster regressions (full coefficients)"))
add(pre("interaction","Interaction test (do slopes differ?)"))
add(pre("assumptions","Model-assumption checks"))
add(pre("m_distress","Model A, distress-only regression"))
add(pre("m_global","Model B, global model (all variables + significance)"))
add(pre("sleep_test","Sleep mediator test (IAT → less sleep?)"))
add(pre("forecast","Logistic forecast (odds ratios + test metrics)"))

add('<hr><p><small>Generated by build_report.R from results.rds. All figures are embedded (base64) so this file is fully self-contained.</small></p>')
add('</div></body></html>')

writeLines(paste(H, collapse="\n"), "report.html")
cat("Wrote report.html (", round(file.size("report.html")/1024), "KB )\n")
