###############################################################################
# Project: Inequality in Parental Investments (Analytical Framework)
# Purpose: Illustrate a "margins-style" workflow:
#          - Fit linear models with group-by-age interactions and controls
#          - Predict outcomes on a standardized grid (controls fixed at means)
#          - Plot predicted profiles with 95% confidence intervals
#
# Expected inputs:
#   - Outcomes:            outcome variables listed in `OUTCOME_MAP`
#   - Age bin factor:      `age_bin`
#   - Weight variable:     `w`
#   - Controls:            variables listed in `CONTROLS`
#   - Group variables:     variables listed in `GROUP_VARS`
#
# Outputs:
#   - A named list of ggplot objects in `plots`
###############################################################################

# -------------------------------#
# 0) Housekeeping & options
# -------------------------------#

options(stringsAsFactors = FALSE)
set.seed(1234)
if (!is.null(dev.list())) grDevices::dev.off()

# -------------------------------#
# 1) Dependencies
# -------------------------------#

suppressPackageStartupMessages({
  library(dplyr)
  library(broom)
  library(ggplot2)
  library(tidyr)
  library(haven)  # optional (for as_factor); safe even if unused
})

# -------------------------------#
# 2) Styling (optional)
# -------------------------------#

PALETTE <- c(
  primary   = "#005590",
  secondary = "#B60D45"
)

theme_journal <- function(base_size = 13) {
  theme_minimal(base_family = "Times New Roman", base_size = base_size) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
      axis.text.y = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      text = element_text(color = "black"),
      panel.grid.minor = element_blank()
    )
}

# -------------------------------#
# 3) REQUIRED: Provide `df`
# -------------------------------#
# This script intentionally does NOT read any file.
# You must have an object `df` in your environment.
#
# Example (not executed here):
#   df <- readRDS("your_data.rds")
#
stopifnot(exists("df"))
stopifnot(is.data.frame(df))

# -------------------------------#
# 4) Variable mapping (generic)
# -------------------------------#
# If your dataset uses different names, edit ONLY this section.

# Core variables
AGE_BIN <- "age_bin"     # factor with age categories (e.g., "0-1", "2-3", ...)
WGT     <- "w"           # sampling/analysis weights

# Controls used in every model (edit to match your dataset)
CONTROLS <- c("region_indicator", "female", "quarter", "n_children_u18")

# Grouping variables (each should be a factor or character; 2 levels recommended)
GROUP_VARS <- c("group_family", "group_migration", "group_education", "group_income")

# Define the level sets for each grouping variable (used to build prediction grid)
GROUP_LEVELS <- list(
  group_family    = c("Two-parent", "Single-parent"),
  group_income    = c("Lower-income", "Higher-income"),
  group_migration = c("No migration background", "Migration background"),
  group_education = c("Lower education household", "Higher education household")
)

# Outcomes to analyze (names in your dataset) + labels for plots
OUTCOME_MAP <- c(
  outcome_1 = "Spending category 1",
  outcome_2 = "Spending category 2",
  outcome_3 = "Spending category 3"
)

# -------------------------------#
# 5) Input checks
# -------------------------------#

required <- c(AGE_BIN, WGT, CONTROLS, GROUP_VARS, names(OUTCOME_MAP))
missing <- setdiff(required, names(df))
if (length(missing) > 0) {
  stop("Missing required columns in `df`: ", paste(missing, collapse = ", "))
}

# Ensure age bin is factor (safe conversion)
df <- df %>%
  mutate("{AGE_BIN}" := as.factor(.data[[AGE_BIN]]))

# Ensure grouping variables are factors with desired levels where provided
for (g in names(GROUP_LEVELS)) {
  if (g %in% names(df)) {
    df[[g]] <- factor(df[[g]], levels = GROUP_LEVELS[[g]])
  }
}

# -------------------------------#
# 6) Helper functions
# -------------------------------#

control_reference_values <- function(data, controls) {
  # Numeric: mean; Factor/character: most frequent (mode).
  out <- lapply(controls, function(v) {
    x <- data[[v]]
    if (is.numeric(x)) {
      mean(x, na.rm = TRUE)
    } else {
      # mode for categorical variables
      tab <- table(x, useNA = "no")
      if (length(tab) == 0) NA else names(tab)[which.max(tab)]
    }
  })
  tibble::as_tibble(setNames(out, controls))
}

fit_predict_plot <- function(data, outcome, group_var, age_bin, wgt, controls, group_levels,
                             ylab = "Predicted outcome", xlab = "Age category") {

  # Drop missing outcome, age, weights, group
  data_sub <- data %>%
    filter(
      !is.na(.data[[outcome]]),
      !is.na(.data[[age_bin]]),
      !is.na(.data[[wgt]]),
      !is.na(.data[[group_var]])
    )

  # If fewer than 2 group levels present, skip
  if (dplyr::n_distinct(data_sub[[group_var]], na.rm = TRUE) < 2) {
    return(NULL)
  }

  # Model: outcome ~ group*age + controls
  rhs <- paste(c(paste0(group_var, " * ", age_bin), controls), collapse = " + ")
  fml <- as.formula(paste(outcome, "~", rhs))
  model <- lm(fml, data = data_sub, weights = data_sub[[wgt]])

  # Controls set to reference values (means / modes)
  ctrl_ref <- control_reference_values(data_sub, controls)

  # Prediction grid: all group levels x all age levels
  grid <- expand.grid(
    group_value = group_levels,
    age_value   = levels(data_sub[[age_bin]]),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  # Assign into correct columns
  grid[[group_var]] <- factor(grid$group_value, levels = group_levels)
  grid[[age_bin]]   <- factor(grid$age_value, levels = levels(data_sub[[age_bin]]))
  grid$group_value <- NULL
  grid$age_value   <- NULL

  # Add controls (reference)
  grid <- dplyr::bind_cols(grid, ctrl_ref[rep(1, nrow(grid)), ])

  # Predict
  preds <- predict(model, newdata = grid, se.fit = TRUE)
  grid$fit <- as.numeric(preds$fit)
  grid$se  <- as.numeric(preds$se.fit)
  grid <- grid %>%
    mutate(
      ci_low  = fit - 1.96 * se,
      ci_high = fit + 1.96 * se
    )

  # Plot
  p <- ggplot(grid, aes(x = .data[[age_bin]], y = fit, color = .data[[group_var]], group = .data[[group_var]])) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
    labs(x = xlab, y = ylab) +
    scale_color_manual(values = c(PALETTE["primary"], PALETTE["secondary"])) +
    theme_journal()

  list(model = model, grid = grid, plot = p)
}

# -------------------------------#
# 7) Run framework
# -------------------------------#
# Returns objects instead of saving files (no paths).

results <- list()
plots   <- list()

for (outcome in names(OUTCOME_MAP)) {

  outcome_label <- OUTCOME_MAP[[outcome]]

  for (group_var in names(GROUP_LEVELS)) {

    res <- fit_predict_plot(
      data        = df,
      outcome     = outcome,
      group_var   = group_var,
      age_bin     = AGE_BIN,
      wgt         = WGT,
      controls    = CONTROLS,
      group_levels = GROUP_LEVELS[[group_var]],
      ylab        = "Predicted outcome (units as defined in data)",
      xlab        = "Age category"
    )

    if (is.null(res)) next

    key <- paste0(outcome, "__", group_var)
    results[[key]] <- res
    plots[[key]]   <- res$plot
  }
}

# -------------------------------#
# 8) Example: display one plot
# -------------------------------#
# To view a specific plot:
#   print(plots[["outcome_1__group_family"]])

message("Completed margins-style framework. Objects available: `results` and `plots`.")