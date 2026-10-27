# Melbourne Airbnb: satisfaction classification and price prediction
# Portfolio version of ACTL3142 Part 2
#
# Run from a terminal with:
# Rscript code/airbnb-advanced-modelling.R "/path/to/AirbnbMelbourne.xlsx"

required_packages <- c(
  "caret", "dplyr", "forcats", "glmnet", "pROC", "randomForest",
  "readxl", "recipes", "tibble", "xgboost"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Install these packages first: ", paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(caret)
  library(dplyr)
  library(forcats)
  library(glmnet)
  library(pROC)
  library(randomForest)
  library(readxl)
  library(recipes)
  library(tibble)
  library(xgboost)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1 || !file.exists(args[[1]])) {
  stop("Pass the Airbnb workbook path as the first argument.")
}

set.seed(3142)

# Helpers -----------------------------------------------------------------

parse_rate <- function(x) {
  value <- suppressWarnings(as.numeric(gsub("%", "", as.character(x))))
  percent_scale <- which(!is.na(value) & value > 1.5)
  value[percent_scale] <- value[percent_scale] / 100
  value
}

count_amenities <- function(x) {
  text <- ifelse(is.na(x), "", as.character(x))
  ifelse(text %in% c("", "[]"), 0, lengths(strsplit(text, "\",\"")))
}

property_group <- function(x) {
  x <- tolower(as.character(x))
  case_when(
    grepl("apartment|condo|loft", x) ~ "Apartment / condo",
    grepl("house|home|townhouse|villa", x) ~ "House / villa",
    grepl("hotel|hostel|boutique", x) ~ "Hotel / hostel",
    TRUE ~ "Other"
  )
}

metric_table <- function(actual, predicted) {
  tibble(
    MAE = mean(abs(actual - predicted)),
    RMSE = sqrt(mean((actual - predicted)^2)),
    RMSLE = sqrt(mean((log1p(actual) - log1p(pmax(predicted, 0)))^2))
  )
}

classification_metrics <- function(actual, probability, threshold) {
  predicted <- factor(ifelse(probability >= threshold, "Yes", "No"),
                      levels = c("No", "Yes"))
  truth <- as.integer(actual == "Yes")
  tp <- sum(predicted == "Yes" & actual == "Yes")
  tn <- sum(predicted == "No" & actual == "No")
  fp <- sum(predicted == "Yes" & actual == "No")
  fn <- sum(predicted == "No" & actual == "Yes")

  tibble(
    AUC = as.numeric(roc(actual, probability, levels = c("No", "Yes"),
                         direction = "<", quiet = TRUE)$auc),
    Brier = mean((probability - truth)^2),
    Accuracy = (tp + tn) / length(actual),
    Sensitivity = tp / (tp + fn),
    Specificity = tn / (tn + fp),
    Precision = tp / (tp + fp)
  )
}

make_split <- function(response, train_share = 0.60) {
  train_id <- createDataPartition(response, p = train_share, list = FALSE)[, 1]
  remainder_id <- setdiff(seq_along(response), train_id)
  validation_local <- createDataPartition(response[remainder_id], p = 0.50,
                                           list = FALSE)[, 1]
  list(
    train = train_id,
    validation = remainder_id[validation_local],
    test = remainder_id[-validation_local]
  )
}

make_recipe <- function(training_predictors) {
  recipe(~ ., data = training_predictors) |>
    step_unknown(all_nominal_predictors(), new_level = "Missing") |>
    step_novel(all_nominal_predictors(), new_level = "New") |>
    step_other(all_nominal_predictors(), threshold = 0.01, other = "Rare") |>
    step_impute_median(all_numeric_predictors()) |>
    step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
    step_zv(all_predictors()) |>
    prep(training = training_predictors, retain = TRUE)
}

bake_matrix <- function(prepped_recipe, new_data) {
  as.matrix(bake(prepped_recipe, new_data = new_data))
}

# Data preparation ---------------------------------------------------------

raw <- read_excel(args[[1]])

review_columns <- c(
  "review_scores_accuracy", "review_scores_cleanliness",
  "review_scores_checkin", "review_scores_communication",
  "review_scores_location", "review_scores_value", "review_scores_rating"
)

required_columns <- c("price", review_columns)
if (!all(required_columns %in% names(raw))) {
  stop("The workbook does not contain the expected price and review-score columns.")
}

# The Part 2 population excludes listings priced above $1,000.
# The workbook's latest recorded review is 18 March 2024, used as a fixed
# reference date for tenure and recency features rather than re-estimated later.
snapshot_date <- as.Date("2024-03-18")
data <- raw |>
  filter(price <= 1000) |>
  mutate(
    avg_score = rowMeans(across(all_of(review_columns))),
    HAS = factor(ifelse(avg_score > 4.9, "Yes", "No"),
                 levels = c("No", "Yes")),
    host_response_rate_num = parse_rate(host_response_rate),
    host_acceptance_rate_num = parse_rate(host_acceptance_rate),
    amenity_count = count_amenities(amenities),
    host_age_days = as.numeric(snapshot_date - as.Date(host_since)),
    review_recency_days = as.numeric(snapshot_date - as.Date(last_review)),
    property_group = factor(property_group(property_type))
  )

cat("\nModelling population:", nrow(data), "listings\n")
cat("HAS prevalence:", round(mean(data$HAS == "Yes") * 100, 1), "%\n")

common_predictors <- c(
  "host_response_rate_num", "host_acceptance_rate_num", "host_age_days",
  "host_listings_count", "host_total_listings_count", "host_has_profile_pic",
  "host_identity_verified", "neighbourhood", "latitude", "longitude",
  "property_group", "room_type", "accommodates", "bathrooms", "bedrooms",
  "beds", "amenity_count", "minimum_nights", "maximum_nights",
  "availability_30", "availability_90", "availability_365",
  "number_of_reviews", "number_of_reviews_ltm", "number_of_reviews_l30d",
  "review_recency_days", "instant_bookable", "reviews_per_month"
)

# Classification -----------------------------------------------------------

class_split <- make_split(data$HAS)
class_x <- data |> select(all_of(common_predictors))
class_y <- data$HAS

class_recipe <- make_recipe(class_x[class_split$train, ])
x_class_train <- bake_matrix(class_recipe, class_x[class_split$train, ])
x_class_validation <- bake_matrix(class_recipe, class_x[class_split$validation, ])
x_class_test <- bake_matrix(class_recipe, class_x[class_split$test, ])
y_class_train <- class_y[class_split$train]
y_class_validation <- class_y[class_split$validation]
y_class_test <- class_y[class_split$test]

# LASSO selects lambda using cross-validation inside the training sample.
lasso_class <- cv.glmnet(
  x_class_train, as.integer(y_class_train == "Yes"),
  family = "binomial", alpha = 1, type.measure = "auc", nfolds = 5
)
class_prob_lasso_validation <- as.numeric(predict(
  lasso_class, x_class_validation, s = "lambda.1se", type = "response"
))
class_prob_lasso_test <- as.numeric(predict(
  lasso_class, x_class_test, s = "lambda.1se", type = "response"
))

# Random forest and XGBoost use the same prepared predictors and samples.
rf_class <- randomForest(
  x = x_class_train, y = y_class_train,
  ntree = 350, mtry = max(2, floor(sqrt(ncol(x_class_train)))), importance = TRUE
)
class_prob_rf_validation <- predict(rf_class, x_class_validation, type = "prob")[, "Yes"]
class_prob_rf_test <- predict(rf_class, x_class_test, type = "prob")[, "Yes"]

dclass_train <- xgb.DMatrix(x_class_train, label = as.integer(y_class_train == "Yes"))
dclass_validation <- xgb.DMatrix(
  x_class_validation, label = as.integer(y_class_validation == "Yes")
)
dclass_test <- xgb.DMatrix(x_class_test)

class_grid <- list(
  list(max_depth = 3, eta = 0.05, min_child_weight = 5),
  list(max_depth = 5, eta = 0.03, min_child_weight = 8)
)

class_xgb_candidates <- lapply(class_grid, function(grid_row) {
  xgb.train(
    params = c(
      list(
        objective = "binary:logistic", eval_metric = "auc",
        subsample = 0.85, colsample_bytree = 0.85
      ),
      grid_row
    ),
    data = dclass_train,
    nrounds = 1200,
    watchlist = list(validation = dclass_validation),
    early_stopping_rounds = 60,
    verbose = 0
  )
})

class_xgb_auc <- vapply(class_xgb_candidates, function(model) {
  max(model$evaluation_log$validation_auc)
}, numeric(1))
class_xgb <- class_xgb_candidates[[which.max(class_xgb_auc)]]
class_prob_xgb_validation <- predict(class_xgb, x_class_validation)
class_prob_xgb_test <- predict(class_xgb, dclass_test)

validation_auc <- c(
  LASSO = as.numeric(roc(y_class_validation, class_prob_lasso_validation,
                         levels = c("No", "Yes"), direction = "<", quiet = TRUE)$auc),
  `Random forest` = as.numeric(roc(y_class_validation, class_prob_rf_validation,
                                   levels = c("No", "Yes"), direction = "<", quiet = TRUE)$auc),
  XGBoost = as.numeric(roc(y_class_validation, class_prob_xgb_validation,
                           levels = c("No", "Yes"), direction = "<", quiet = TRUE)$auc)
)

selected_classifier <- names(which.max(validation_auc))
selected_validation_probability <- switch(
  selected_classifier,
  LASSO = class_prob_lasso_validation,
  `Random forest` = class_prob_rf_validation,
  XGBoost = class_prob_xgb_validation
)
selected_test_probability <- switch(
  selected_classifier,
  LASSO = class_prob_lasso_test,
  `Random forest` = class_prob_rf_test,
  XGBoost = class_prob_xgb_test
)

validation_probabilities <- list(
  LASSO = class_prob_lasso_validation,
  `Random forest` = class_prob_rf_validation,
  XGBoost = class_prob_xgb_validation
)
validation_thresholds <- vapply(validation_probabilities, function(probability) {
  validation_roc <- roc(
    y_class_validation, probability,
    levels = c("No", "Yes"), direction = "<", quiet = TRUE
  )
  as.numeric(coords(
    validation_roc, "best", best.method = "youden", ret = "threshold"
  )[1])
}, numeric(1))
selected_threshold <- validation_thresholds[[selected_classifier]]

classification_results <- bind_rows(
  classification_metrics(y_class_test, class_prob_lasso_test, validation_thresholds[["LASSO"]]) |>
    mutate(Model = "LASSO"),
  classification_metrics(y_class_test, class_prob_rf_test,
                         validation_thresholds[["Random forest"]]) |>
    mutate(Model = "Random forest"),
  classification_metrics(y_class_test, class_prob_xgb_test, validation_thresholds[["XGBoost"]]) |>
    mutate(Model = "XGBoost")
) |>
  select(Model, everything())

cat("\nClassification validation AUC\n")
print(round(validation_auc, 4))
cat("Selected classifier:", selected_classifier, "\n")
cat("Validation-selected threshold:", round(selected_threshold, 3), "\n")
cat("Untouched test metrics\n")
print(classification_results |> mutate(across(where(is.numeric), ~ round(.x, 4))))

# A sparse logistic model provides directional associations for discussion.
class_coefficients <- as.matrix(coef(lasso_class, s = "lambda.1se"))[, 1]
class_coefficients <- sort(class_coefficients[names(class_coefficients) != "(Intercept)"],
                           decreasing = TRUE)
cat("\nLargest positive LASSO associations\n")
print(round(head(class_coefficients[class_coefficients > 0], 8), 3))
cat("Largest negative LASSO associations\n")
print(round(head(sort(class_coefficients[class_coefficients < 0]), 8), 3))

# Regression ---------------------------------------------------------------

regression_predictors <- c(common_predictors, review_columns)
reg_x <- data |> select(all_of(regression_predictors))
reg_y <- data$price
reg_split <- make_split(reg_y)

reg_recipe <- make_recipe(reg_x[reg_split$train, ])
x_reg_train <- bake_matrix(reg_recipe, reg_x[reg_split$train, ])
x_reg_validation <- bake_matrix(reg_recipe, reg_x[reg_split$validation, ])
x_reg_test <- bake_matrix(reg_recipe, reg_x[reg_split$test, ])
y_reg_train <- reg_y[reg_split$train]
y_reg_validation <- reg_y[reg_split$validation]
y_reg_test <- reg_y[reg_split$test]
log_y_reg_train <- log1p(y_reg_train)

lasso_reg <- cv.glmnet(
  x_reg_train, log_y_reg_train,
  family = "gaussian", alpha = 1, type.measure = "mse", nfolds = 5
)
lasso_reg_train_log <- as.numeric(predict(lasso_reg, x_reg_train, s = "lambda.min"))
lasso_smearing <- mean(exp(log_y_reg_train - lasso_reg_train_log))
reg_pred_lasso_validation <- pmax(
  0, exp(as.numeric(predict(lasso_reg, x_reg_validation, s = "lambda.min"))) *
    lasso_smearing - 1
)
reg_pred_lasso_test <- pmax(
  0, exp(as.numeric(predict(lasso_reg, x_reg_test, s = "lambda.min"))) *
    lasso_smearing - 1
)

rf_reg <- randomForest(
  x = x_reg_train, y = log_y_reg_train,
  ntree = 350, mtry = max(2, floor(ncol(x_reg_train) / 3)), importance = TRUE
)
rf_reg_train_log <- predict(rf_reg, x_reg_train)
rf_smearing <- mean(exp(log_y_reg_train - rf_reg_train_log))
reg_pred_rf_validation <- pmax(0, exp(predict(rf_reg, x_reg_validation)) * rf_smearing - 1)
reg_pred_rf_test <- pmax(0, exp(predict(rf_reg, x_reg_test)) * rf_smearing - 1)

dreg_train <- xgb.DMatrix(x_reg_train, label = log_y_reg_train)
dreg_validation <- xgb.DMatrix(x_reg_validation, label = log1p(y_reg_validation))
dreg_test <- xgb.DMatrix(x_reg_test)

reg_grid <- list(
  list(max_depth = 3, eta = 0.04, min_child_weight = 5),
  list(max_depth = 5, eta = 0.03, min_child_weight = 8)
)

reg_xgb_candidates <- lapply(reg_grid, function(grid_row) {
  model <- xgb.train(
    params = c(
      list(
        objective = "reg:squarederror", eval_metric = "rmse",
        subsample = 0.85, colsample_bytree = 0.85
      ),
      grid_row
    ),
    data = dreg_train,
    nrounds = 1600,
    watchlist = list(validation = dreg_validation),
    early_stopping_rounds = 60,
    verbose = 0
  )
  train_log_prediction <- predict(model, dreg_train)
  smearing <- mean(exp(log_y_reg_train - train_log_prediction))
  validation_prediction <- pmax(
    0, exp(predict(model, dreg_validation)) * smearing - 1
  )
  list(
    model = model,
    smearing = smearing,
    validation_rmse = sqrt(mean((y_reg_validation - validation_prediction)^2))
  )
})

reg_xgb_choice <- which.min(vapply(
  reg_xgb_candidates, function(item) item$validation_rmse, numeric(1)
))
reg_xgb <- reg_xgb_candidates[[reg_xgb_choice]]
reg_pred_xgb_validation <- pmax(
  0, exp(predict(reg_xgb$model, dreg_validation)) * reg_xgb$smearing - 1
)
reg_pred_xgb_test <- pmax(
  0, exp(predict(reg_xgb$model, dreg_test)) * reg_xgb$smearing - 1
)

baseline_validation <- rep(median(y_reg_train), length(y_reg_validation))
baseline_test <- rep(median(y_reg_train), length(y_reg_test))

validation_rmse <- c(
  Baseline = metric_table(y_reg_validation, baseline_validation)$RMSE,
  LASSO = metric_table(y_reg_validation, reg_pred_lasso_validation)$RMSE,
  `Random forest` = metric_table(y_reg_validation, reg_pred_rf_validation)$RMSE,
  XGBoost = metric_table(y_reg_validation, reg_pred_xgb_validation)$RMSE
)
selected_regressor <- names(which.min(validation_rmse))

regression_results <- bind_rows(
  metric_table(y_reg_test, baseline_test) |> mutate(Model = "Median baseline"),
  metric_table(y_reg_test, reg_pred_lasso_test) |> mutate(Model = "LASSO"),
  metric_table(y_reg_test, reg_pred_rf_test) |> mutate(Model = "Random forest"),
  metric_table(y_reg_test, reg_pred_xgb_test) |> mutate(Model = "XGBoost")
) |>
  select(Model, everything())

cat("\nRegression validation RMSE (dollars)\n")
print(round(validation_rmse, 2))
cat("Selected regressor:", selected_regressor, "\n")
cat("Untouched test metrics (dollars except RMSLE)\n")
print(regression_results |> mutate(across(where(is.numeric), ~ round(.x, 3))))

cat("\nXGBoost regression feature importance\n")
print(head(xgb.importance(model = reg_xgb$model), 12))

cat("\nNotes\n")
cat("- Model selection used validation data; reported final metrics use untouched test data.\n")
cat("- All imputation, rare-level handling and dummy encoding were fitted on training data only.\n")
cat("- Feature importance and LASSO coefficients are associations, not causal effects.\n")
