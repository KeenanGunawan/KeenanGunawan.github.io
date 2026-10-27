# Bonus-malus scheme analysis
# Portfolio version: corrected, reproducible, and independent of specialist packages.

data_path <- if (length(commandArgs(trailingOnly = TRUE)) > 0) {
  commandArgs(trailingOnly = TRUE)[1]
} else {
  "data/AgeLevelData.csv"
}

policy <- read.csv(data_path)
required_columns <- c("policynumber", "age", "ncdlevel25")
missing_columns <- setdiff(required_columns, names(policy))

if (length(missing_columns) > 0) {
  stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
}

if (anyNA(policy[required_columns])) stop("Required fields contain missing values.")
if (anyDuplicated(policy$policynumber)) stop("Policy numbers must be unique.")
if (!all(policy$ncdlevel25 %in% -1:3)) stop("Unexpected starting discount level.")

lambda_young <- 0.22
lambda_old <- 0.16
base_young <- 700
base_old <- 600
claim_cost <- 2500

young <- policy[policy$age <= 25, ]
old <- policy[policy$age >= 50, ]

if (nrow(young) + nrow(old) != nrow(policy)) {
  stop("Every policyholder must belong to one of the two modelled age groups.")
}

build_transition_matrix <- function(level_count, lambda) {
  p_zero <- exp(-lambda)
  p_one <- lambda * exp(-lambda)
  p_two_plus <- 1 - p_zero - p_one
  transition <- matrix(0, nrow = level_count, ncol = level_count)

  for (state in seq_len(level_count)) {
    transition[state, max(state - 1, 1)] <-
      transition[state, max(state - 1, 1)] + p_zero
    transition[state, min(state + 1, level_count)] <-
      transition[state, min(state + 1, level_count)] + p_one
    transition[state, min(state + 2, level_count)] <-
      transition[state, min(state + 2, level_count)] + p_two_plus
  }

  transition
}

stationary_distribution <- function(transition) {
  level_count <- nrow(transition)
  system <- t(transition) - diag(level_count)
  system[level_count, ] <- 1
  target <- c(rep(0, level_count - 1), 1)
  as.numeric(solve(system, target))
}

scheme_multipliers <- list(
  `Option 0` = c(0.8, 1.0, 1.2, 1.4, 1.6),
  `Option 1` = c(1.0, 1.2, 1.4),
  `Option 2` = c(0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8)
)

# Option 0: one-year Monte Carlo simulation using each age group's actual
# starting-level distribution from the source data.
simulate_group <- function(starting_levels, lambda) {
  claims <- rpois(length(starting_levels), lambda)
  movement <- ifelse(claims == 0, -1, ifelse(claims == 1, 1, 2))
  ending_levels <- pmax(-1, pmin(3, starting_levels + movement))

  list(
    counts = tabulate(match(ending_levels, -1:3), nbins = 5),
    claims = sum(claims)
  )
}

simulate_current_scheme <- function(iterations = 2000, seed = 1234) {
  set.seed(seed)
  output <- matrix(0, nrow = iterations, ncol = 2)
  colnames(output) <- c("premium", "profit")
  young_counts <- matrix(0, nrow = iterations, ncol = 5)
  old_counts <- matrix(0, nrow = iterations, ncol = 5)

  for (iteration in seq_len(iterations)) {
    young_result <- simulate_group(young$ncdlevel25, lambda_young)
    old_result <- simulate_group(old$ncdlevel25, lambda_old)

    premium <-
      sum(young_result$counts * base_young * scheme_multipliers$`Option 0`) +
      sum(old_result$counts * base_old * scheme_multipliers$`Option 0`)
    claims <- young_result$claims + old_result$claims

    output[iteration, ] <- c(premium, premium - claim_cost * claims)
    young_counts[iteration, ] <- young_result$counts
    old_counts[iteration, ] <- old_result$counts
  }

  list(
    outcomes = as.data.frame(output),
    average_counts = data.frame(
      level = -1:3,
      young = colMeans(young_counts),
      old = colMeans(old_counts)
    )
  )
}

simulation <- simulate_current_scheme()

simulation_summary <- data.frame(
  metric = c("Premium", "Profit"),
  mean = c(mean(simulation$outcomes$premium), mean(simulation$outcomes$profit)),
  standard_deviation = c(sd(simulation$outcomes$premium), sd(simulation$outcomes$profit)),
  percentile_05 = c(
    unname(quantile(simulation$outcomes$premium, 0.05)),
    unname(quantile(simulation$outcomes$profit, 0.05))
  ),
  median = c(median(simulation$outcomes$premium), median(simulation$outcomes$profit)),
  percentile_95 = c(
    unname(quantile(simulation$outcomes$premium, 0.95)),
    unname(quantile(simulation$outcomes$profit, 0.95))
  )
)

# Long-run profit uses the correct claim frequency for each age group.
long_run_profit <- function(young_multipliers, old_multipliers) {
  young_stationary <- stationary_distribution(
    build_transition_matrix(length(young_multipliers), lambda_young)
  )
  old_stationary <- stationary_distribution(
    build_transition_matrix(length(old_multipliers), lambda_old)
  )

  premium <-
    nrow(young) * base_young * sum(young_stationary * young_multipliers) +
    nrow(old) * base_old * sum(old_stationary * old_multipliers)
  expected_claim_cost <- claim_cost * (
    lambda_young * nrow(young) + lambda_old * nrow(old)
  )

  c(premium = premium, claim_cost = expected_claim_cost, profit = premium - expected_claim_cost)
}

long_run_results <- rbind(
  `Option 0` = long_run_profit(
    scheme_multipliers$`Option 0`, scheme_multipliers$`Option 0`
  ),
  `Option 1` = long_run_profit(
    scheme_multipliers$`Option 1`, scheme_multipliers$`Option 1`
  ),
  `Option 2` = long_run_profit(
    scheme_multipliers$`Option 2`, scheme_multipliers$`Option 0`
  )
)

mean_premium <- function(lambda_values, multipliers) {
  vapply(lambda_values, function(lambda) {
    stationary <- stationary_distribution(
      build_transition_matrix(length(multipliers), lambda)
    )
    sum(stationary * multipliers)
  }, numeric(1))
}

loimaranta_efficiency <- function(lambda_values, multipliers) {
  premium <- mean_premium(lambda_values, multipliers)
  derivative <- diff(premium) / diff(lambda_values)
  efficiency <- lambda_values[-length(lambda_values)] /
    premium[-length(premium)] * derivative
  c(efficiency, tail(efficiency, 1))
}

lambda_grid <- seq(0.01, 1.00, by = 0.01)
efficiency <- sapply(
  scheme_multipliers,
  function(multipliers) loimaranta_efficiency(lambda_grid, multipliers)
)

efficiency_peaks <- data.frame(
  scheme = colnames(efficiency),
  peak_efficiency = apply(efficiency, 2, max),
  lambda_at_peak = apply(efficiency, 2, function(values) lambda_grid[which.max(values)])
)

print(simulation_summary, row.names = FALSE)
print(round(simulation$average_counts, 1), row.names = FALSE)
print(round(long_run_results, 2))
print(transform(efficiency_peaks,
  peak_efficiency = round(peak_efficiency, 3),
  lambda_at_peak = round(lambda_at_peak, 2)
), row.names = FALSE)

if (interactive()) {
  matplot(
    lambda_grid, efficiency,
    type = "l", lty = 1, lwd = 2,
    col = c("#0c7b72", "#d5a340", "#123044"),
    xlab = "Claim frequency (lambda)",
    ylab = "Loimaranta efficiency"
  )
  legend(
    "topright", legend = colnames(efficiency),
    col = c("#0c7b72", "#d5a340", "#123044"),
    lty = 1, lwd = 2, bty = "n"
  )
}
