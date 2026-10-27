# Election Polling and Demographic Analysis
# Cleaned portfolio version of an ACTL2131 individual project.

library(readxl)
library(dplyr)
library(tidyr)

# The original dataset is not published with this portfolio.
# Place an authorised copy at data/USpollGA.xlsx to reproduce the analysis.
data_path <- "data/USpollGA.xlsx"

if (!file.exists(data_path)) {
  stop("Add an authorised copy of USpollGA.xlsx to the data folder before running.")
}

poll <- read_excel(data_path)

required_columns <- c("Candidate", "Gender", "Race", "AgeGroup", "Income")
missing_columns <- setdiff(required_columns, names(poll))

if (length(missing_columns) > 0) {
  stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
}

# 1. Overall candidate support and exact binomial confidence intervals --------

support <- poll |>
  count(Candidate, name = "votes") |>
  mutate(
    sample_size = sum(votes),
    estimate = votes / sample_size
  )

support_intervals <- support |>
  rowwise() |>
  mutate(
    interval = list(binom.test(votes, sample_size)$conf.int),
    lower_95 = interval[[1]],
    upper_95 = interval[[2]]
  ) |>
  ungroup() |>
  select(-interval)

print(support_intervals)

# 2. Conditional demographic distributions -----------------------------------

demographic_distribution <- function(data, variable) {
  data |>
    count({{ variable }}, Candidate, name = "votes") |>
    group_by({{ variable }}) |>
    mutate(percentage = 100 * votes / sum(votes)) |>
    ungroup()
}

gender_distribution <- demographic_distribution(poll, Gender)
race_distribution <- demographic_distribution(poll, Race)
age_distribution <- demographic_distribution(poll, AgeGroup)
income_distribution <- demographic_distribution(poll, Income)

print(gender_distribution)
print(race_distribution)
print(age_distribution)
print(income_distribution)

# 3. Test whether candidate support differs by gender -------------------------

test_candidate_by_gender <- function(data, candidate_name) {
  total_by_gender <- table(data$Gender)
  support_by_gender <- table(data$Gender[data$Candidate == candidate_name])
  genders <- intersect(names(total_by_gender), names(support_by_gender))

  result <- prop.test(
    x = support_by_gender[genders],
    n = total_by_gender[genders],
    alternative = "two.sided",
    correct = FALSE
  )

  tibble(
    candidate = candidate_name,
    statistic = unname(result$statistic),
    p_value = result$p.value,
    conclusion = if_else(
      result$p.value < 0.05,
      "Evidence of a gender difference",
      "No statistically significant gender difference detected"
    )
  )
}

harris_gender_test <- test_candidate_by_gender(poll, "Harris")
trump_gender_test <- test_candidate_by_gender(poll, "Trump")

bind_rows(harris_gender_test, trump_gender_test) |>
  print()

# 4. Illustrative sample-size calculation with multinomial dependence ---------

p_trump <- support |> filter(Candidate == "Trump") |> pull(estimate)
p_harris <- support |> filter(Candidate == "Harris") |> pull(estimate)
difference <- abs(p_trump - p_harris)

# Because candidate proportions come from one multinomial sample,
# Cov(p_trump, p_harris) = -p_trump * p_harris / n.
variance_component <- p_trump + p_harris - difference^2
z_99_one_sided <- qnorm(0.99)

n_required <- ceiling(
  z_99_one_sided^2 * variance_component / difference^2
)

message("Illustrative minimum sample size: ", n_required)

# This calculation is conditional on the observed proportions and a normal
# approximation. It does not account for weighting, design effects,
# non-response bias or changes in voter intention after the polling date.
