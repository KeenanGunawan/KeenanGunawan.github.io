# Copula Market Risk and ARMA Forecasting
# Portfolio version of an ACTL3301 project.
#
# The script downloads paired NVDA and AMD adjusted prices, measures standalone
# and portfolio tail risk, compares alternative dependence structures, fits an
# asymmetric Archimedean copula, and evaluates short-horizon ARMA forecasts.

suppressPackageStartupMessages({
  library(quantmod)
  library(tidyverse)
  library(VineCopula)
  library(forecast)
})

start_date <- as.Date("2016-06-01")
end_date <- as.Date("2026-06-06")
alpha <- c(0.95, 0.99, 0.995)
n_sim <- 10000
output_dir <- file.path(getwd(), "market-risk-figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Download and align both price series before calculating returns. This avoids
# silently pairing observations from different trading dates.
nvda <- getSymbols(
  "NVDA", src = "yahoo", from = start_date, to = end_date,
  auto.assign = FALSE
)
amd <- getSymbols(
  "AMD", src = "yahoo", from = start_date, to = end_date,
  auto.assign = FALSE
)
prices <- na.omit(merge(Ad(nvda), Ad(amd), join = "inner"))
colnames(prices) <- c("NVDA", "AMD")
returns <- na.omit(diff(log(prices)))
X1 <- as.numeric(returns[, "NVDA"])
X2 <- as.numeric(returns[, "AMD"])
L1 <- 1 - exp(X1)
L2 <- 1 - exp(X2)

inverse_ecdf <- function(x, u) {
  as.numeric(quantile(x, probs = u, type = 1, names = FALSE))
}

portfolio_loss <- function(u1, u2) {
  0.5 * (
    1 - exp(inverse_ecdf(X1, u1)) +
      1 - exp(inverse_ecdf(X2, u2))
  )
}

risk_table <- function(loss, model) {
  value_at_risk <- as.numeric(
    quantile(loss, probs = alpha, type = 1, names = FALSE)
  )
  expected_shortfall <- map_dbl(
    value_at_risk,
    ~ mean(loss[loss >= .x])
  )

  tibble(
    Model = model,
    Alpha = alpha,
    VaR = value_at_risk,
    ES = expected_shortfall
  )
}

tail_probability <- function(u1, u2) {
  map_dbl(alpha, function(a) {
    threshold <- 1 - a
    denominator <- sum(u2 <= threshold)
    if (denominator == 0) return(NA_real_)
    sum(u1 <= threshold & u2 <= threshold) / denominator
  })
}

# 1. Standalone empirical risk
standalone_risk <- bind_rows(
  risk_table(L1, "NVDA"),
  risk_table(L2, "AMD")
)

# 2. Direct concordance calculation and rank dependence
pair_counts <- function(x, y) {
  concordant <- discordant <- ties <- 0
  for (i in seq_len(length(x) - 1)) {
    j <- (i + 1):length(x)
    product <- (x[i] - x[j]) * (y[i] - y[j])
    concordant <- concordant + sum(product > 0)
    discordant <- discordant + sum(product < 0)
    ties <- ties + sum(product == 0)
  }
  tibble(
    Concordant = concordant,
    Discordant = discordant,
    Ties = ties,
    Kendall_tau = (concordant - discordant) / choose(length(x), 2),
    Spearman_rho = cor(ecdf(x)(x), ecdf(y)(y))
  )
}

rank_dependence <- pair_counts(L1, L2)
rho_hat <- cor(X1, X2)

# 3. Simulate benchmark copulas while keeping empirical marginals fixed
RNGversion("4.3.0")
set.seed(2026)
independence <- list(U1 = runif(n_sim), U2 = runif(n_sim))
independence$L <- portfolio_loss(independence$U1, independence$U2)

RNGversion("4.3.0")
set.seed(2026)
z1 <- rnorm(n_sim)
z2 <- rho_hat * z1 + sqrt(1 - rho_hat^2) * rnorm(n_sim)
gaussian <- list(U1 = pnorm(z1), U2 = pnorm(z2))
gaussian$L <- portfolio_loss(gaussian$U1, gaussian$U2)

simulate_t_copula <- function(df) {
  sigma <- matrix(c(1, rho_hat, rho_hat, 1), nrow = 2)
  normals <- matrix(rnorm(2 * n_sim), ncol = 2) %*% chol(sigma)
  t_values <- normals / sqrt(rchisq(n_sim, df = df) / df)
  uniforms <- cbind(pt(t_values[, 1], df), pt(t_values[, 2], df))
  list(
    U1 = uniforms[, 1],
    U2 = uniforms[, 2],
    L = portfolio_loss(uniforms[, 1], uniforms[, 2])
  )
}

RNGversion("4.3.0")
set.seed(2026)
t3 <- simulate_t_copula(3)
t10 <- simulate_t_copula(10)
t10000 <- simulate_t_copula(10000)

RNGversion("4.3.0")
set.seed(2026)
u <- runif(n_sim)
comonotonic <- list(U1 = u, U2 = u)
countermonotonic <- list(U1 = u, U2 = 1 - u)
comonotonic$L <- portfolio_loss(comonotonic$U1, comonotonic$U2)
countermonotonic$L <- portfolio_loss(
  countermonotonic$U1, countermonotonic$U2
)

empirical <- list(
  U1 = ecdf(X1)(X1),
  U2 = ecdf(X2)(X2),
  L = 0.5 * (L1 + L2)
)

models <- list(
  Empirical = empirical,
  Independence = independence,
  Gaussian = gaussian,
  `t-copula (df = 3)` = t3,
  `t-copula (df = 10)` = t10,
  `t-copula (df = 10000)` = t10000,
  Comonotonicity = comonotonic,
  Countermonotonicity = countermonotonic
)

portfolio_risk <- imap_dfr(models, ~ risk_table(.x$L, .y))
portfolio_tail <- imap_dfr(models, ~ tibble(
  Model = .y,
  Alpha = alpha,
  Conditional_tail_probability = tail_probability(.x$U1, .x$U2)
))

empirical_benchmark <- portfolio_risk |>
  filter(Model == "Empirical") |>
  transmute(Alpha, Empirical_VaR = VaR, Empirical_ES = ES)

model_comparison <- portfolio_risk |>
  left_join(portfolio_tail, by = c("Model", "Alpha")) |>
  left_join(empirical_benchmark, by = "Alpha") |>
  mutate(
    VaR_error = abs(VaR - Empirical_VaR),
    ES_error = abs(ES - Empirical_ES)
  )

model_ranking <- model_comparison |>
  filter(Model != "Empirical") |>
  group_by(Model) |>
  summarise(
    Risk_MAE = mean(c(VaR_error, ES_error)),
    .groups = "drop"
  ) |>
  arrange(Risk_MAE)

# 4. Maximum-likelihood Archimedean copula selection
pseudo_observations <- function(x) {
  ecdf(x)(x) * length(x) / (length(x) + 1)
}

best_copula <- BiCopSelect(
  pseudo_observations(X1),
  pseudo_observations(X2),
  familyset = 3:10,
  selectioncrit = "logLik",
  indeptest = FALSE,
  rotations = TRUE,
  method = "mle"
)

selected_copula_summary <- tibble(
  Family = BiCopName(best_copula$family, short = FALSE),
  Parameter_1 = best_copula$par,
  Parameter_2 = if (best_copula$npars == 2) best_copula$par2 else NA_real_,
  Kendall_tau = best_copula$tau,
  Lower_tail_dependence = best_copula$taildep$lower,
  Upper_tail_dependence = best_copula$taildep$upper,
  Log_likelihood = best_copula$logLik
)

RNGversion("4.3.0")
set.seed(2026)
selected_u <- BiCopSim(n_sim, obj = best_copula)
selected_loss <- portfolio_loss(selected_u[, 1], selected_u[, 2])
selected_risk <- risk_table(
  selected_loss,
  selected_copula_summary$Family
) |>
  mutate(
    Conditional_tail_probability = tail_probability(
      selected_u[, 1], selected_u[, 2]
    )
  )

# 5. Conditional-mean time-series models
fit_nvda <- auto.arima(X1, ic = "aic")
fit_amd <- auto.arima(X2, ic = "aic")

model_label <- function(fit) {
  order <- arimaorder(fit)
  sprintf("ARIMA(%s,%s,%s)", order["p"], order["d"], order["q"])
}

manual_arma_forecast <- function(fit, x, horizon = 5) {
  order <- arimaorder(fit)
  p <- unname(order["p"])
  d <- unname(order["d"])
  q <- unname(order["q"])
  if (d != 0) stop("This forecast routine requires d = 0.")

  coefficients <- coef(fit)
  phi <- if (p > 0) unname(coefficients[paste0("ar", 1:p)]) else numeric()
  theta <- if (q > 0) unname(coefficients[paste0("ma", 1:q)]) else numeric()
  mu <- if ("intercept" %in% names(coefficients)) {
    unname(coefficients["intercept"])
  } else {
    0
  }

  values <- c(as.numeric(x), rep(NA_real_, horizon))
  innovations <- c(replace_na(as.numeric(resid(fit)), 0), rep(0, horizon))
  sample_size <- length(x)

  for (step in seq_len(horizon)) {
    current <- sample_size + step
    ar_part <- if (p > 0) {
      sum(phi * (values[current - seq_len(p)] - mu))
    } else 0
    ma_part <- if (q > 0) {
      sum(theta * innovations[current - seq_len(q)])
    } else 0
    values[current] <- mu + ar_part + ma_part
  }
  tail(values, horizon)
}

nvda_forecast <- manual_arma_forecast(fit_nvda, X1)
amd_forecast <- manual_arma_forecast(fit_amd, X2)

residual_test <- function(fit, stock, lag = 20) {
  order <- arimaorder(fit)
  result <- Box.test(
    na.omit(resid(fit)),
    lag = lag,
    type = "Ljung-Box",
    fitdf = unname(order["p"] + order["q"])
  )
  tibble(
    Stock = stock,
    Model = model_label(fit),
    Q_statistic = unname(result$statistic),
    P_value = result$p.value
  )
}

time_series_models <- tibble(
  Stock = c("NVDA", "AMD"),
  Model = c(model_label(fit_nvda), model_label(fit_amd)),
  AIC = c(fit_nvda$aic, fit_amd$aic),
  Innovation_variance = c(fit_nvda$sigma2, fit_amd$sigma2)
)

residual_results <- bind_rows(
  residual_test(fit_nvda, "NVDA"),
  residual_test(fit_amd, "AMD")
)

# Save one compact comparison chart in a writeable, predictable directory.
risk_plot <- portfolio_risk |>
  filter(Alpha == 0.99, Model != "Empirical") |>
  pivot_longer(c(VaR, ES), names_to = "Measure", values_to = "Loss") |>
  ggplot(aes(reorder(Model, Loss), 100 * Loss, fill = Measure)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "99% portfolio tail risk by dependence model",
    x = NULL,
    y = "Loss on one dollar (%)"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  file.path(output_dir, "portfolio-risk-comparison.png"),
  risk_plot,
  width = 8,
  height = 5,
  dpi = 180,
  bg = "white"
)

cat("\nPAIRED RETURNS\n")
print(length(X1))
cat("\nSTANDALONE RISK\n")
print(standalone_risk)
cat("\nRANK DEPENDENCE\n")
print(rank_dependence)
cat("\nMODEL RANKING\n")
print(model_ranking)
cat("\nSELECTED COPULA\n")
print(selected_copula_summary)
print(selected_risk)
cat("\nTIME-SERIES MODELS\n")
print(time_series_models)
cat("\nFIVE-STEP FORECASTS\n")
print(tibble(Step = 1:5, NVDA = nvda_forecast, AMD = amd_forecast))
cat("\nRESIDUAL TESTS\n")
print(residual_results)
