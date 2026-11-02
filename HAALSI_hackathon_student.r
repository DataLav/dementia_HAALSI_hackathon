---
title: "HAALSI Hackathon: Risk Factors for Cognitive Impairment"
subtitle: "Waves 1-3 longitudinal analysis"
date: "`r Sys.Date()`"
output:
  html_document:
    toc: true
    toc_float: true
    number_sections: true
params:
  data_path: ""
  rebuild: false
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo    = TRUE,
  message = FALSE,
  warning = FALSE,
  error   = TRUE
)

pkgs <- c(
  "haven", "dplyr", "tidyr", "ggplot2", "broom",
  "lme4", "lmerTest", "knitr"
)
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(broom)
  library(lme4)
  library(lmerTest)
})

show_lm <- function(model, pattern) {
  tidy(model, conf.int = TRUE) |>
    filter(grepl(pattern, term)) |>
    select(term, estimate, conf.low, conf.high, p.value) |>
    mutate(across(where(is.numeric), ~ round(.x, 3))) |>
    knitr::kable()
}

show_glm <- function(model, pattern) {
  tidy(model, conf.int = TRUE, exponentiate = TRUE) |>
    filter(grepl(pattern, term)) |>
    select(term, estimate, conf.low, conf.high, p.value) |>
    mutate(across(where(is.numeric), ~ round(.x, 3))) |>
    knitr::kable()
}

show_lmer <- function(model, pattern) {
  co <- summary(model)$coefficients
  co <- co[grepl(pattern, rownames(co)), , drop = FALSE]
  data.frame(
    term      = rownames(co),
    estimate  = co[, "Estimate"],
    conf.low  = co[, "Estimate"] - 1.96 * co[, "Std. Error"],
    conf.high = co[, "Estimate"] + 1.96 * co[, "Std. Error"],
    p.value   = co[, ncol(co)],
    row.names = NULL
  ) |>
    mutate(across(where(is.numeric), ~ round(.x, 3))) |>
    knitr::kable()
}
```

## How to use this file

The data-cleaning section reads the original Stata dataset, creates a cleaned longitudinal dataset, and saves it as an RDS file. Later runs reuse the RDS file.

## Data cleaning

```{r data-cleaning, cache=TRUE}
dta_path <- params$data_path
rds_path <- file.path(
  dirname(dta_path),
  paste0(tools::file_path_sans_ext(basename(dta_path)), "_long.rds")
)

if (params$rebuild || !file.exists(rds_path)) {
  if (!file.exists(dta_path)) {
    stop(
      "Cannot find the dataset: ", dta_path,
      ". Check the filename and folder."
    )
  }

  wave_vars <- c(
    age = "w?c_rage_calc", marital_status = "w?c_bd_mar",
    hhsize = "w?c_hhsize", hhnum = "w?c_hhnum",
    wealth = "w?c_wealthindex", asset = "w?c_hh_assetindex",
    employment = "w?c_ep_employed",
    memory = "w?cn001", concentration = "w?cn002",
    learning = "w?cn003", cesd = "w?c_cd_cesd",
    bmi = "w?c_bs_bmi", bmi_category = "w?c_bs_bmicat",
    hypertension_diag = "w?c_cm_hypertension_srdiag",
    htn_on_med_raw = "w?cm005",
    diabetes_diag = "w?c_cm_diabetes_srdiag",
    dm_on_med_raw = "w?cm010",
    adls_walk = "w?c_pf_diff_walk",
    adls_dress = "w?c_pf_diff_dress",
    adls_bath = "w?c_pf_diff_bath",
    adls_eat = "w?c_pf_diff_eat",
    adls_toilet = "w?c_pf_diff_toilet",
    systolic_bp = "w?c_bs_mean_sys",
    diastolic_bp = "w?c_bs_mean_dia",
    glucose = "w?c_bs_glucose",
    smoking = "w?cm059", alcohol = "w?cm068",
    status = "w?status", int_year = "w?c_int_year",
    int_month = "w?c_int_month",
    imrec1 = "w?c_cn_imrec1", delrec = "w?c_cn_delrec",
    orient_sum = "w?cn214"
  )

  fixed_vars <- c("w2c_rsex", "w1c_bd_educ4", "w1c_bd_country")
  w1_cog_items <- c(
    "w1cn013", paste0("w1cn007s", 1:10),
    paste0("w1cn012s", 1:10), paste0("w1cn01", 4:7)
  )

  wanted <- c(
    "prim_key", fixed_vars, w1_cog_items,
    unlist(lapply(1:3, function(w) {
      sub("?", w, wave_vars, fixed = TRUE)
    }))
  )

  all_names <- names(read_dta(dta_path, n_max = 0))
  raw <- read_dta(
    dta_path,
    col_select = all_of(intersect(wanted, all_names))
  )
  raw <- zap_labels(zap_missing(raw))

  neg_to_na <- function(x) {
    x <- as.numeric(x)
    x[x < 0] <- NA
    x
  }

  yes_no <- function(x) {
    x <- as.numeric(x)
    case_when(
      x == 1 ~ 1,
      x %in% c(0, 2) ~ 0,
      TRUE ~ NA_real_
    )
  }

  w1_recall <- function(stem) {
    m <- as.matrix(raw[, paste0(stem, "s", 1:10)])
    n <- rowSums(m > 0, na.rm = TRUE)
    n[rowSums(m == -97, na.rm = TRUE) > 0] <- NA
    n[is.na(raw$w1cn013)] <- NA
    n
  }

  orient_w1 <- sapply(paste0("w1cn01", 4:7), function(v) {
    x <- as.numeric(raw[[v]])
    case_when(
      x == 1 ~ 1,
      x %in% c(2, -98) ~ 0,
      TRUE ~ NA_real_
    )
  })

  w1_cog <- tibble(
    prim_key = raw$prim_key,
    wave = 1,
    imrec1_w1 = w1_recall("w1cn007"),
    delrec_w1 = w1_recall("w1cn012"),
    orient_w1 = rowSums(orient_w1, na.rm = FALSE)
  )

  long <- lapply(1:3, function(w) {
    cols <- sub("?", w, wave_vars, fixed = TRUE)
    out <- tibble(prim_key = raw$prim_key, wave = w)
    for (nm in names(wave_vars)) {
      out[[nm]] <- if (cols[[nm]] %in% names(raw)) {
        as.numeric(raw[[cols[[nm]]]])
      } else {
        NA_real_
      }
    }
    out
  }) |>
    bind_rows() |>
    left_join(w1_cog, by = c("prim_key", "wave")) |>
    mutate(
      imrec1     = if_else(wave == 1, imrec1_w1, imrec1),
      delrec     = if_else(wave == 1, delrec_w1, delrec),
      orient_sum = if_else(wave == 1, orient_w1, orient_sum)
    ) |>
    select(-imrec1_w1, -delrec_w1, -orient_w1)

  fixed <- tibble(
    prim_key = raw$prim_key,
    sex = case_when(
      raw$w2c_rsex == 1 ~ 1,
      raw$w2c_rsex == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    education_category = neg_to_na(raw$w1c_bd_educ4),
    born_moz = case_when(
      raw$w1c_bd_country == 2 ~ 1,
      raw$w1c_bd_country == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )

  long <- long |>
    left_join(fixed, by = "prim_key") |>
    mutate(
      status = if_else(wave == 1, 1, status),
      across(
        c(
          age, marital_status, hhsize, hhnum, wealth, employment,
          memory, concentration, learning, cesd, bmi, bmi_category,
          systolic_bp, diastolic_bp, glucose, imrec1, delrec, orient_sum
        ),
        neg_to_na
      ),
      employment = if_else(
        wave == 3 & employment == 0, 2, employment
      ),
      across(
        c(
          hypertension_diag, diabetes_diag, smoking, alcohol,
          htn_on_med_raw, dm_on_med_raw
        ),
        yes_no
      ),
      across(
        starts_with("adls_"),
        ~ case_when(
          .x %in% c(1, 3) ~ 1,
          .x == 2 ~ 0,
          TRUE ~ NA_real_
        )
      )
    )

  long <- long |>
    mutate(
      htn_group = factor(
        case_when(
          hypertension_diag == 0 ~ "No hypertension",
          hypertension_diag == 1 & htn_on_med_raw == 1 ~
            "Hypertension, treated",
          hypertension_diag == 1 ~ "Hypertension, untreated",
          TRUE ~ NA_character_
        ),
        levels = c(
          "No hypertension", "Hypertension, untreated",
          "Hypertension, treated"
        )
      ),
      dm_group = factor(
        case_when(
          diabetes_diag == 0 ~ "No diabetes",
          diabetes_diag == 1 & dm_on_med_raw == 1 ~
            "Diabetes, treated",
          diabetes_diag == 1 ~ "Diabetes, untreated",
          TRUE ~ NA_character_
        ),
        levels = c(
          "No diabetes", "Diabetes, untreated", "Diabetes, treated"
        )
      ),
      adl_count = rowSums(
        across(starts_with("adls_")), na.rm = FALSE
      ),
      any_adl = if_else(
        is.na(adl_count), NA_real_,
        as.numeric(adl_count > 0)
      ),
      int_date = int_year + (int_month - 0.5) / 12
    ) |>
    select(-htn_on_med_raw, -dm_on_med_raw)

  ref <- filter(long, wave == 1)
  zref <- function(x, r) {
    (x - mean(r, na.rm = TRUE)) / sd(r, na.rm = TRUE)
  }

  long <- long |>
    mutate(
      cog_raw = (
        zref(orient_sum, ref$orient_sum) +
          zref(imrec1, ref$imrec1) +
          zref(delrec, ref$delrec)
      ) / 3
    ) |>
    mutate(
      objective_cognition_z = zref(
        cog_raw, cog_raw[wave == 1]
      )
    ) |>
    select(-cog_raw)

  cut20 <- quantile(
    long$objective_cognition_z[long$wave == 1],
    0.20, na.rm = TRUE
  )
  
long <- long |>
  mutate(
    low_cognition = if_else(
      is.na(objective_cognition_z), NA_real_,
      as.numeric(objective_cognition_z < cut20)
    ),
    asset_sd = zref(asset, asset[wave == 1])
  ) |>
  group_by(wave) |>
  mutate(
    cesd_z = (cesd - mean(cesd, na.rm = TRUE)) /
      sd(cesd, na.rm = TRUE)
  ) |>
  ungroup() |>
  group_by(prim_key) |>
  mutate(
    years_since_bl = int_date -
      int_date[wave == 1][1]
  ) |>
  ungroup()

    group_by(wave) |>
    mutate(
      cesd_z = (cesd - mean(cesd, na.rm = TRUE)) /
        sd(cesd, na.rm = TRUE)
    ) |>
    ungroup() |>
    group_by(prim_key) |>
    mutate(
      years_since_bl = int_date -
        int_date[wave == 1][1]
    ) |>
    ungroup() |>
    mutate(
      sex_f = factor(sex, 0:1, c("Female", "Male")),
      education_f = factor(
        education_category, 1:4,
        c("No formal", "Some primary", "Some secondary", "Secondary+")
      ),
      marital_f = factor(
        marital_status, 0:3,
        c("Never married", "Separated/divorced", "Widowed", "Married")
      ),
      hhsize_f = factor(
        hhsize, 0:3, c("Alone", "2 people", "3-6 people", "7+ people")
      ),
      wealth_f = factor(
        wealth, 1:5,
        c("Q1 (poorest)", "Q2", "Q3", "Q4", "Q5 (richest)")
      ),
      employment_f = factor(
        employment, 1:3, c("Employed", "Not working", "Homemaker")
      ),
      bmi_category_f = factor(
        bmi_category, 0:3,
        c("Underweight", "Normal", "Overweight", "Obese")
      )
    ) |>
    arrange(prim_key, wave)

  saveRDS(long, rds_path)
}

haalsi <- readRDS(rds_path)
cat(
  rds_path, ":", nrow(haalsi), "person-waves,",
  n_distinct(haalsi$prim_key), "participants
"
)
```

## About the data

`objective_cognition_z` averages orientation, immediate recall, and delayed recall, standardized to Wave 1. `low_cognition` indicates scores below the Wave 1 20th percentile. Cross-sectional analyses use Wave 1; longitudinal analyses use all three waves. Wealth is measured using asset quintiles and a continuous standardized asset index. CES-D measures depressive symptoms. Hypertension and diabetes groups distinguish untreated and treated conditions. Self-reported cognition scores are coded so higher values indicate greater difficulty.

```{r}
haalsi |>
  group_by(wave) |>
  summarise(
    completed = sum(status == 1, na.rm = TRUE),
    with_cognition = sum(!is.na(objective_cognition_z)),
    mean_years_since_bl = round(
      mean(years_since_bl[status == 1], na.rm = TRUE), 1
    ),
    mean_cognition = round(
      mean(objective_cognition_z, na.rm = TRUE), 2
    )
  ) |>
  knitr::kable()
```

```{r}
w1 <- filter(haalsi, wave == 1) |>
  mutate(bmi_category_f = relevel(bmi_category_f, ref = "Normal"))

baseline <- w1 |>
  transmute(
    prim_key,
    age_bl = age,
    age10_bl = (age - 60) / 10,
    sex_f, education_f, born_moz, marital_f,
    hhsize_bl_f = hhsize_f,
    wealth_bl = wealth,
    asset_sd_bl = asset_sd,
    employment_bl_f = employment_f,
    cesd_bl_z = cesd_z,
    htn_bl = hypertension_diag,
    htn_group_bl = htn_group,
    dm_bl = diabetes_diag,
    bmi_bl = bmi,
    bmi_cat_bl_f = bmi_category_f,
    smoking_bl = smoking,
    alcohol_bl = alcohol
  )

cog_long <- haalsi |>
  filter(
    !is.na(objective_cognition_z),
    !is.na(years_since_bl)
  ) |>
  select(
    prim_key, wave, years = years_since_bl,
    objective_cognition_z, low_cognition
  ) |>
  left_join(baseline, by = "prim_key")

cat(
  "Wave 1 sample:", nrow(w1),
  "| person-waves with cognition:", nrow(cog_long), "\n"
)
```

## Question 1: Which intervention could have the greatest benefit?

### 1a. Is household wealth associated with cognition?

```{r}
m1a_cat <- lm(
  objective_cognition_z ~ wealth_f + age + sex_f + education_f,
  data = w1
)
show_lm(m1a_cat, "wealth_f")

m1a_sd <- lm(
  objective_cognition_z ~ asset_sd + age + sex_f + education_f,
  data = w1
)
show_lm(m1a_sd, "asset_sd")

m1a_low <- glm(
  low_cognition ~ asset_sd + age + sex_f + education_f,
  family = binomial, data = w1
)
show_glm(m1a_low, "asset_sd")

m1a_slope <- lmer(
  objective_cognition_z ~ years *
    (asset_sd_bl + age10_bl + sex_f + education_f) +
    (1 + years | prim_key),
  data = cog_long
)
show_lmer(m1a_slope, "asset_sd_bl")
```

**Interpretation:** Describe the wealth coefficients, their confidence intervals, and the time interaction. Associations do not by themselves establish causality.

### 1b. Is hypertension associated with cognition?

```{r}
m1b <- lm(
  objective_cognition_z ~ htn_group + age + sex_f +
    education_f + wealth_f,
  data = w1
)
show_lm(m1b, "htn_group")

m1b_low <- glm(
  low_cognition ~ htn_group + age + sex_f +
    education_f + wealth_f,
  family = binomial, data = w1
)
show_glm(m1b_low, "htn_group")

m1b_slope <- lmer(
  objective_cognition_z ~ years *
    (htn_group_bl + age10_bl + sex_f + education_f) +
    (1 + years | prim_key),
  data = cog_long
)
show_lmer(m1b_slope, "htn_group_bl")
```

**Interpretation:** Compare untreated and treated hypertension with no hypertension. Consider confounding, access to care, and survival differences.

### 1c. Is depression associated with objective and subjective cognition?

```{r}
w1_std <- w1 |>
  mutate(
    across(
      c(objective_cognition_z, memory, concentration, learning),
      ~ as.numeric(scale(.x)),
      .names = "{.col}_std"
    )
  )

outcomes <- c(
  "objective_cognition_z_std", "memory_std",
  "concentration_std", "learning_std"
)

q1c <- lapply(outcomes, function(y) {
  f <- reformulate(
    c("cesd_z", "age", "sex_f", "education_f", "wealth_f"),
    response = y
  )

  tidy(lm(f, data = w1_std), conf.int = TRUE) |>
    filter(term == "cesd_z") |>
    mutate(outcome = y)
}) |>
  bind_rows()

q1c |>
  select(outcome, estimate, conf.low, conf.high, p.value) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  knitr::kable()

m1c_low <- glm(
  low_cognition ~ cesd_z + age + sex_f +
    education_f + wealth_f,
  family = binomial, data = w1
)
show_glm(m1c_low, "cesd_z")
```

**Interpretation:** Compare the absolute standardized coefficients for objective and subjective cognition. Consider whether self-reports can screen depressive symptoms reliably.

### 1d. Intervention scenarios

```{r}
prev_shift <- function(model, change) {
  d0 <- model.frame(model) |> as.data.frame()
  d1 <- change(d0)
  c(
    before = mean(predict(model, d0, type = "response")),
    after  = mean(predict(model, d1, type = "response"))
  )
}

scenarios <- list(
  "Cash transfer: assets +0.5 SD" =
    prev_shift(
      m1a_low,
      function(d) mutate(d, asset_sd = asset_sd + 0.5)
    ),

  "HTN care: treat all untreated hypertension" =
    prev_shift(
      m1b_low,
      function(d) {
        d$htn_group <- factor(
          ifelse(
            as.character(d$htn_group) == "Hypertension, untreated",
            "Hypertension, treated",
            as.character(d$htn_group)
          ),
          levels = levels(d$htn_group)
        )
        d
      }
    ),

  "Psychotherapy: CES-D -0.5 SD if symptomatic" =
    prev_shift(
      m1c_low,
      function(d) {
        d$cesd_z <- ifelse(
          d$cesd_z > 0, d$cesd_z - 0.5, d$cesd_z
        )
        d
      }
    )
)

do.call(rbind, scenarios) |>
  as.data.frame() |>
  mutate(
    change_pct_points = round(100 * (after - before), 2),
    before = round(100 * before, 1),
    after  = round(100 * after, 1)
  ) |>
  knitr::kable(
    col.names = c(
      "Low cognition before (%)", "After (%)",
      "Change (percentage points)"
    )
  )
```

**Discussion:** Discuss whether the assumed intervention changes are realistic. These are model-based scenarios using observational associations, not proven causal effects.

## Question 2: Is self-reported memory associated with objective cognition?

```{r}
w1 |>
  select(objective_cognition_z, memory, concentration, learning) |>
  cor(use = "pairwise.complete.obs", method = "spearman") |>
  round(2) |>
  knitr::kable(caption = "Spearman correlations (Wave 1)")

m2 <- lm(
  objective_cognition_z ~ factor(memory) + age +
    sex_f + education_f,
  data = w1
)
show_lm(m2, "factor$$memory$$")

screen_data <- w1 |>
  filter(!is.na(memory), !is.na(low_cognition))

screen_tab <- with(
  screen_data,
  table(
    memory_poor   = memory >= 4,
    low_cognition = low_cognition == 1
  )
)

knitr::kable(screen_tab)

sensitivity <- screen_tab["TRUE", "TRUE"] /
  sum(screen_tab[, "TRUE"])

specificity <- screen_tab["FALSE", "FALSE"] /
  sum(screen_tab[, "FALSE"])

cat("Sensitivity:", round(sensitivity, 3), "\n")
cat("Specificity:", round(specificity, 3), "\n")
```

**Interpretation:** Discuss the correlation, sensitivity, and specificity before deciding whether self-reported memory could substitute for objective testing.

## Question 3: Is diabetes associated with objective and subjective cognition?

```{r}
q3 <- lapply(outcomes, function(y) {
  f <- reformulate(
    c("dm_group", "age", "sex_f", "education_f", "wealth_f"),
    response = y
  )

  tidy(lm(f, data = w1_std), conf.int = TRUE) |>
    filter(grepl("dm_group", term)) |>
    mutate(outcome = y)
}) |>
  bind_rows()

q3 |>
  select(outcome, term, estimate, conf.low, conf.high, p.value) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  knitr::kable()
```

**Interpretation:** Compare objective and subjective associations. Note the uncertainty arising from small treated groups.

## Question 4: Is BMI category associated with objective cognition?

```{r}
m4 <- lm(
  objective_cognition_z ~ bmi_category_f + age +
    sex_f + education_f + wealth_f,
  data = w1
)
show_lm(m4, "bmi_category_f")

w1_cc <- w1 |>
  drop_na(
    objective_cognition_z, bmi, age,
    sex_f, education_f, wealth_f
  )

ggplot(
  filter(w1_cc, bmi > 12, bmi < 55),
  aes(bmi, objective_cognition_z)
) +
  geom_smooth(method = "loess") +
  labs(x = "BMI", y = "Objective cognition (z)")

m4_linear <- lm(
  objective_cognition_z ~ bmi + age + sex_f +
    education_f + wealth_f,
  data = w1_cc
)

m4_spline <- lm(
  objective_cognition_z ~ splines::ns(bmi, df = 3) +
    age + sex_f + education_f + wealth_f,
  data = w1_cc
)

anova(m4_linear, m4_spline) |>
  knitr::kable(caption = "Linear versus nonlinear BMI models")
```

**Interpretation:** Discuss whether the relationship is linear or nonlinear, and consider reverse causation.

## Question 5: Are hypertension and diabetes associated with functional disability?

```{r}
w1_adl <- w1 |>
  mutate(
    comorbidity = factor(
      case_when(
        hypertension_diag == 0 & diabetes_diag == 0 ~ "Neither",
        hypertension_diag == 1 & diabetes_diag == 0 ~ "Hypertension only",
        hypertension_diag == 0 & diabetes_diag == 1 ~ "Diabetes only",
        hypertension_diag == 1 & diabetes_diag == 1 ~ "Both",
        TRUE ~ NA_character_
      ),
      levels = c(
        "Neither", "Hypertension only",
        "Diabetes only", "Both"
      )
    )
  )

w1_adl |>
  count(comorbidity) |>
  knitr::kable()

adl_outcomes <- c(
  "adls_walk", "adls_dress", "adls_bath",
  "adls_eat", "adls_toilet", "any_adl"
)

sapply(w1_adl[adl_outcomes], mean, na.rm = TRUE) |>
  round(3)

q5 <- lapply(adl_outcomes, function(y) {
  f <- reformulate(
    c("comorbidity", "age", "sex_f", "education_f", "wealth_f"),
    response = y
  )

  tidy(
    glm(f, family = binomial, data = w1_adl),
    conf.int = TRUE, exponentiate = TRUE
  ) |>
    filter(grepl("^comorbidity", term)) |>
    mutate(outcome = y)
}) |>
  bind_rows()

q5 |>
  select(outcome, term, estimate, conf.low, conf.high, p.value) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  knitr::kable()

m5_interaction <- glm(
  any_adl ~ hypertension_diag * diabetes_diag +
    age + sex_f + education_f + wealth_f,
  family = binomial, data = w1_adl
)

show_glm(m5_interaction, "hypertension_diag:diabetes_diag")
```

**Interpretation:** Compare odds ratios and confidence intervals. Rare outcomes may produce unstable estimates.

## Question 6: Which baseline factors predict cognitive decline?

```{r}
m6 <- lmer(
  objective_cognition_z ~ years *
    (age10_bl + sex_f + education_f + asset_sd_bl +
       cesd_bl_z + htn_bl + dm_bl + bmi_cat_bl_f +
       smoking_bl + alcohol_bl) +
    (1 + years | prim_key),
  data = cog_long
)

show_lmer(m6, "^years")
```

**Interpretation:** Negative time interactions indicate faster decline relative to the reference group or comparison value; positive interactions indicate slower decline. Consider dropout, death, and regression to the mean.

## Question 7: Who is most resilient to cognitive decline?

### Approach A

```{r}
m7 <- lmer(
  objective_cognition_z ~ years *
    (education_f + asset_sd_bl + employment_bl_f +
       cesd_bl_z + htn_bl + dm_bl + bmi_bl) +
    age10_bl + sex_f +
    (1 + years | prim_key),
  data = cog_long
)

show_lmer(
  m7,
  "^years:(education_f|asset_sd_bl|employment_bl_f|cesd_bl_z|htn_bl|dm_bl|bmi_bl)"
)
```

### Approach B

```{r}
resil <- cog_long |>
  filter(wave %in% c(1, 3)) |>
  select(prim_key, wave, objective_cognition_z, years) |>
  pivot_wider(
    names_from = wave,
    values_from = c(objective_cognition_z, years)
  ) |>
  rename(
    cog_w1   = objective_cognition_z_1,
    cog_w3   = objective_cognition_z_3,
    years_w3 = years_3
  ) |>
  filter(!is.na(cog_w1), !is.na(cog_w3)) |>
  left_join(baseline, by = "prim_key")

m_resid <- lm(
  cog_w3 ~ cog_w1 + age_bl + I(age_bl^2) +
    sex_f + years_w3,
  data = resil
)

resil$resid <- NA_real_
resid_rows <- as.integer(rownames(model.frame(m_resid)))
resil$resid[resid_rows] <- resid(m_resid)

resil <- resil |>
  mutate(
    resilient = as.numeric(
      resid >= quantile(resid, 0.8, na.rm = TRUE)
    )
  )

resil |>
  group_by(resilient) |>
  summarise(
    n = n(),
    mean_age = mean(age_bl, na.rm = TRUE),
    mean_asset_sd = mean(asset_sd_bl, na.rm = TRUE),
    mean_cesd_z = mean(cesd_bl_z, na.rm = TRUE),
    proportion_female = mean(sex_f == "Female", na.rm = TRUE),
    .groups = "drop"
  ) |>
  knitr::kable(digits = 3)

m7b <- glm(
  resilient ~ education_f + asset_sd_bl +
    employment_bl_f + cesd_bl_z + htn_bl +
    dm_bl + bmi_bl + age10_bl + sex_f,
  family = binomial, data = resil
)

show_glm(
  m7b,
  "^(education|asset|employment|cesd|htn|dm|bmi)"
)
```

**Interpretation:** Compare the factors associated with resilience in approaches A and B. Remember that participants had to survive and remain in the study to reach Wave 3.

## Conclusions

- Which intervention appears most promising in the model-based scenarios?
- What are the strongest threats to a causal interpretation?
- What other approach might be more beneficial?
- What study design, such as using pension eligibility at age 60 as a natural experiment, could answer the question more convincingly?

## Session info

```{r}
sessionInfo()
```
