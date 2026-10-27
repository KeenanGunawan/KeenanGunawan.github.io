# Melbourne Airbnb price modelling
# Reproducible comparison of a linear model and a Gamma GLM.

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Install the 'readxl' package before running this script.")
}

args <- commandArgs(trailingOnly = TRUE)
data_path <- if (length(args) >= 1) args[[1]] else "data/AirbnbMelbourne.xlsx"
output_dir <- if (length(args) >= 2) args[[2]] else NA_character_

if (!file.exists(data_path)) {
  stop("Data file not found: ", data_path)
}

raw <- readxl::read_excel(data_path, sheet = "AirbnbMelbourne-OriginalFile")

required_columns <- c(
  "price", "room_type", "neighbourhood", "accommodates", "bedrooms",
  "bathrooms", "minimum_nights", "availability_365", "number_of_reviews",
  "amenities"
)
missing_columns <- setdiff(required_columns, names(raw))
if (length(missing_columns) > 0) {
  stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
}

model_data <- raw[complete.cases(raw[required_columns]), required_columns]
model_data <- model_data[
  model_data$price > 0 &
    model_data$accommodates > 0 &
    model_data$bedrooms >= 0 &
    model_data$bathrooms > 0,
]

model_data$amenity_count <- vapply(
  strsplit(model_data$amenities, ",", fixed = TRUE),
  length,
  integer(1)
)
model_data$log_minimum_nights <- log1p(model_data$minimum_nights)
model_data$log_reviews <- log1p(model_data$number_of_reviews)
model_data$room_type <- droplevels(factor(model_data$room_type))
model_data$neighbourhood <- droplevels(factor(model_data$neighbourhood))

if (nrow(model_data) < 100) stop("Too few valid rows remain after cleaning.")

set.seed(3142)
train_index <- sample.int(nrow(model_data), floor(0.8 * nrow(model_data)))
train <- model_data[train_index,]
test <- model_data[-train_index,]

model_formula <- price ~
  room_type + neighbourhood +
  accommodates + I(accommodates^2) + bedrooms + bathrooms +
  log_minimum_nights + availability_365 + log_reviews + amenity_count

linear_model <- lm(model_formula, data = train)
gamma_model <- glm(model_formula, family = Gamma(link = "log"), data = train)

predictions <- data.frame(
  actual = test$price,
  baseline = rep(median(train$price), nrow(test)),
  linear = as.numeric(predict(linear_model, newdata = test)),
  gamma = as.numeric(predict(gamma_model, newdata = test, type = "response"))
)

model_metrics <- function(actual, predicted) {
  data.frame(
    MAE = mean(abs(actual - predicted)),
    RMSE = sqrt(mean((actual - predicted)^2)),
    RMSLE = sqrt(mean((log1p(actual) - log1p(pmax(predicted, 0)))^2)),
    MedianAE = median(abs(actual - predicted)),
    Negative_predictions = sum(predicted < 0)
  )
}

metrics <- rbind(
  Baseline = model_metrics(predictions$actual, predictions$baseline),
  Linear = model_metrics(predictions$actual, predictions$linear),
  Gamma = model_metrics(predictions$actual, predictions$gamma)
)

room_summary <- do.call(
  data.frame,
  aggregate(price ~ room_type, model_data, function(x) {
    c(Listings = length(x), Median = median(x), Mean = mean(x))
  })
)
names(room_summary) <- c("Room_type", "Listings", "Median_price", "Mean_price")

fit_summary <- data.frame(
  Model = c("Linear", "Gamma"),
  AIC = c(AIC(linear_model), AIC(gamma_model)),
  BIC = c(BIC(linear_model), BIC(gamma_model))
)

cat("Data quality\n")
cat("Raw rows:", nrow(raw), "\n")
cat("Model rows:", nrow(model_data), "\n")
cat("Training rows:", nrow(train), "\n")
cat("Test rows:", nrow(test), "\n\n")

cat("Price distribution\n")
print(quantile(model_data$price, c(0, 0.25, 0.5, 0.75, 0.9, 0.99, 1)))
cat("\nRoom-type summary\n")
print(room_summary, row.names = FALSE)
cat("\nOut-of-sample metrics\n")
print(round(metrics, 3))
cat("\nIn-sample information criteria (same response and training rows)\n")
fit_summary$AIC <- round(fit_summary$AIC, 2)
fit_summary$BIC <- round(fit_summary$BIC, 2)
print(fit_summary, row.names = FALSE)

if (!is.na(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  png(file.path(output_dir, "airbnb-price-distribution.png"), 1200, 760, res = 140)
  hist(
    model_data$price,
    breaks = seq(0, 2000, by = 50),
    col = "#0C7B72",
    border = "white",
    main = "Melbourne Airbnb nightly prices",
    xlab = "Advertised nightly price (AUD)",
    ylab = "Listings"
  )
  abline(v = median(model_data$price), col = "#D5A340", lwd = 3, lty = 2)
  dev.off()

  png(file.path(output_dir, "airbnb-gamma-holdout.png"), 1200, 760, res = 140)
  plot(
    predictions$actual,
    predictions$gamma,
    pch = 16,
    col = rgb(12 / 255, 123 / 255, 114 / 255, 0.28),
    xlab = "Actual nightly price (AUD)",
    ylab = "Predicted nightly price (AUD)",
    main = "Gamma GLM holdout predictions"
  )
  abline(0, 1, col = "#D5A340", lwd = 3)
  dev.off()
}
