###############################################################################
# Project: Inequality in Parental Investments
# Purpose: Produce descriptive expenditure profiles over the child life course
#          (bar charts, stacked bars, and line plots).
#
# Notes:
#   - This script illustrates an analytical workflow.
#   - No dataset is distributed with this repository.
#   - Users must supply their own appropriately structured survey dataset.
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
  library(haven)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# -------------------------------#
# 2) Paths
# -------------------------------#

PATH_DATA    <- "Data"
PATH_FIGURES <- "Figures"

dir.create(PATH_FIGURES, showWarnings = FALSE, recursive = TRUE)

DATA_FILE <- file.path(PATH_DATA, "survey_data.dta")
stopifnot(file.exists(DATA_FILE))

# -------------------------------#
# 3) Load dataset (generic)
# -------------------------------#

data <- read_dta(DATA_FILE)

# -------------------------------#
# 4) Variable definitions
# -------------------------------#

WEIGHT_VAR <- "weight_variable"

# Basic checks
required_vars <- c("age_child", WEIGHT_VAR)
missing_vars  <- setdiff(required_vars, names(data))
if (length(missing_vars) > 0) {
  stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
}

# Age bins: 0–1, 2–3, ..., 16–17
data <- data %>%
  mutate(
    age_bins = cut(
      age_child,
      breaks = seq(0, 18, by = 2),
      right  = FALSE,
      labels = c("0-1","2-3","4-5","6-7","8-9","10-11","12-13","14-15","16-17")
    )
  )

# Example expenditure variables (must exist in supplied dataset)
expenditure_vars <- c(
  "exp_toys",
  "exp_books",
  "exp_extracurricular",
  "exp_tutoring",
  "exp_clothing",
  "exp_childcare"
)

missing_outcomes <- setdiff(expenditure_vars, names(data))
if (length(missing_outcomes) > 0) {
  stop("Missing expenditure variables: ",
       paste(missing_outcomes, collapse = ", "))
}

# -------------------------------#
# 5) Helper: weighted mean
# -------------------------------#

wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  weighted.mean(x[ok], w[ok])
}

# -------------------------------#
# 6) Bar plots by age bins
# -------------------------------#

for (v in expenditure_vars) {

  df_plot <- data %>%
    filter(!is.na(.data[[v]]),
           !is.na(.data[[WEIGHT_VAR]]),
           !is.na(age_bins)) %>%
    group_by(age_bins) %>%
    summarise(
      mean_value = wmean(.data[[v]], .data[[WEIGHT_VAR]]),
      .groups = "drop"
    )

  p <- ggplot(df_plot, aes(x = age_bins, y = mean_value)) +
    geom_col(fill = "#003366") +
    theme_minimal(base_family = "Times New Roman") +
    labs(
      y = "Average expenditure",
      x = "Age of child (binned)"
    )

  ggsave(
    file.path(PATH_FIGURES,
              paste0("expenditure_profile_", v, ".png")),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300,
    bg = "white"
  )
}

message("Finished: lifecycle expenditure figures saved.")