#!/usr/bin/env Rscript

# Mortality risk, lifestyle behaviour and private insurance
# ----------------------------------------------------------
# Usage:
#   Rscript code/mortality-risk-analysis.R "/path/to/IPUMSNHIS_Processed.csv"
#
# The raw linked survey file is not included in the public portfolio.

required <- c("data.table", "survival")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))

library(data.table)
library(survival)

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("Pass the path to the processed IPUMS NHIS CSV.")

input_path <- args[[1]]
dt <- fread(input_path, na.strings = c("NA", ""), showProgress = FALSE)

required_columns <- c(
  "age_at_survey", "SEX", "MARSTLEG", "EDUCREC2", "EARNINGS", "BMI",
  "HIPRIVATEE", "CANBREASTYRC", "CANPROSTYRC", "CANSCRYRC",
  "SMOKESTATUS1", "survey_year", "exit_year", "death"
)
missing_columns <- setdiff(required_columns, names(dt))
if (length(missing_columns)) stop("Missing columns: ", paste(missing_columns, collapse = ", "))
if (any(dt$exit_year < dt$survey_year, na.rm = TRUE)) stop("Exit year precedes survey year.")
if (!all(na.omit(unique(dt$death)) %in% c(0, 1))) stop("death must be coded 0/1.")

yes_no <- function(x) fifelse(x == "Yes", 1L, fifelse(x == "No", 0L, NA_integer_))

dt[, `:=`(
  follow_up_years = exit_year - survey_year,
  breast_cancer = yes_no(CANBREASTYRC),
  prostate_cancer = yes_no(CANPROSTYRC),
  colorectal_cancer = yes_no(CANSCRYRC),
  phi = factor(
    fifelse(HIPRIVATEE == "Yes", "Has PHI",
            fifelse(HIPRIVATEE == "No", "No PHI", NA_character_)),
    levels = c("No PHI", "Has PHI")
  ),
  bmi_group = factor(fcase(
    is.na(BMI), NA_character_,
    BMI < 18.5, "Underweight",
    BMI < 25, "Normal",
    BMI < 30, "Overweight",
    default = "Obese"
  ), levels = c("Normal", "Underweight", "Overweight", "Obese")),
  smoking = factor(fcase(
    SMOKESTATUS1 == "Never smoked", "Never",
    SMOKESTATUS1 == "Ex-smoker", "Former",
    SMOKESTATUS1 == "Current smoker", "Current",
    default = NA_character_
  ), levels = c("Never", "Former", "Current")),
  education = factor(fcase(
    EDUCREC2 %chin% c("High school level", "Never attended"), "High school or below",
    EDUCREC2 == "College or higher", "College or higher",
    default = NA_character_
  ), levels = c("High school or below", "College or higher")),
  income = factor(
    fifelse(EARNINGS %chin% c("Low income", "Middle income", "High income"),
            EARNINGS, NA_character_),
    levels = c("Low income", "Middle income", "High income")
  )
)]

# Do not turn a missing cancer field into a negative result. A positive response
# identifies a recorded case; otherwise, the status remains unknown unless every
# relevant field is observed and negative.
dt[, any_recorded_cancer := fcase(
  breast_cancer == 1L | prostate_cancer == 1L | colorectal_cancer == 1L,
  "Recorded cancer",
  breast_cancer == 0L & prostate_cancer == 0L & colorectal_cancer == 0L,
  "No recorded cancer",
  default = NA_character_
)]

cat("\nDATA AUDIT\n")
print(dt[, .(
  records = .N,
  deaths = sum(death),
  mean_age = mean(age_at_survey),
  first_survey = min(survey_year),
  last_survey = max(survey_year)
)])
print(dt[, .N, by = any_recorded_cancer][order(any_recorded_cancer)])

# Question 1: post-survey survival among recorded breast-cancer respondents.
breast <- dt[breast_cancer == 1L]
breast[, favourable_profile := factor(fcase(
  bmi_group == "Normal" & smoking == "Never", "Favourable",
  !is.na(bmi_group) & !is.na(smoking), "Other",
  default = NA_character_
), levels = c("Other", "Favourable"))]

breast_profile <- breast[!is.na(favourable_profile)]
breast_km <- survfit(
  Surv(follow_up_years, death) ~ favourable_profile,
  data = breast_profile
)
breast_logrank <- survdiff(
  Surv(follow_up_years, death) ~ favourable_profile,
  data = breast_profile
)

breast_cox_data <- breast[
  !is.na(bmi_group) & !is.na(smoking) & !is.na(education) &
    !is.na(MARSTLEG) & MARSTLEG != "Unknown"
]
breast_cox_data[, MARSTLEG := factor(MARSTLEG)]

breast_cox <- coxph(
  Surv(follow_up_years, death) ~ age_at_survey + bmi_group + smoking +
    education + MARSTLEG,
  data = breast_cox_data,
  x = TRUE
)

cat("\nBREAST-CANCER COHORT\n")
print(breast[, .(records = .N, deaths = sum(death))])
print(breast_profile[, .(records = .N, deaths = sum(death)), by = favourable_profile])
print(summary(breast_km, times = c(5, 10, 15), extend = TRUE))
cat("Log-rank p-value:",
    pchisq(breast_logrank$chisq, df = 1, lower.tail = FALSE), "\n")
print(summary(breast_cox))
print(cox.zph(breast_cox))

# Question 2: PHI association with mortality.
# The unadjusted Kaplan-Meier sample is much larger than the complete-case Cox
# sample. Both denominators are reported explicitly.
phi_observed <- dt[!is.na(phi)]
phi_km <- survfit(Surv(follow_up_years, death) ~ phi, data = phi_observed)

phi_cox_data <- dt[
  !is.na(phi) & !is.na(bmi_group) & !is.na(smoking) &
    !is.na(education) & !is.na(income)
]

phi_cox <- coxph(
  Surv(follow_up_years, death) ~ phi + age_at_survey + SEX + education +
    income + bmi_group + smoking + strata(survey_year),
  data = phi_cox_data
)

cat("\nPRIVATE HEALTH INSURANCE\n")
print(phi_observed[, .(records = .N, deaths = sum(death)), by = phi])
print(summary(phi_km, times = c(5, 10, 15, 20), extend = TRUE))
cat("Adjusted complete-case records:", nrow(phi_cox_data),
    "| deaths:", sum(phi_cox_data$death), "\n")
print(summary(phi_cox))

cat("\nINTERPRETATION NOTE\n")
cat(paste(
  "These are observational associations, not causal effects.",
  "PHI status, income, education and lifestyle measures may retain unmeasured",
  "confounding, and the file does not include survey weights.\n"
))

