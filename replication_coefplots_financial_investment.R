###############################################################################
# Project: Inequality in Parental Investments
# Purpose: Estimate weighted OLS models and create coefficient plots (EVS).
# Notes:   This script illustrates an analysis workflow and produces figures.
#          It assumes an input dataset in a local data directory.
###############################################################################

# -------------------------------#
# 0) Housekeeping & options
# -------------------------------#

# Reproducible defaults (avoid clearing the full workspace in shared sessions)
options(stringsAsFactors = FALSE)
set.seed(1234)

# Close open graphics devices (safe)
if (!is.null(dev.list())) grDevices::dev.off()

# -------------------------------#
# 1) Dependencies
# -------------------------------#

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(purrr)
  library(broom)
  library(ggplot2)
  library(readr)
})

# -------------------------------#
# 2) Paths & directories
# -------------------------------#

PATH_DATA    <- file.path("Data")
PATH_FIGURES <- file.path("Figures")

dir.create(PATH_FIGURES, showWarnings = FALSE, recursive = TRUE)

# -------------------------------#
# 3) Input
# -------------------------------#

DATA_FILE <- file.path(PATH_DATA, "EVS_ready.dta")
stopifnot(file.exists(DATA_FILE))

evs <- read_dta(DATA_FILE)

# -------------------------------#
# 4) Variables, labels, and settings
# -------------------------------#

# Age bins: 0–1, 2–3, ..., 16–17
evs <- evs %>%
  mutate(
    age_bins = cut(
      age_child,
      breaks = seq(0, 18, by = 2),
      right  = FALSE,
      labels = c("0-1","2-3","4-5","6-7","8-9","10-11","12-13","14-15","16-17")
    )
  )

# Outcomes (child-related expenditures; log(1+...) equivalised scale variables)
outcomes <- c(
  "ln1_escale_aus_spiel",
  "ln1_escale_aus_book",
  "ln1_escale_aus_unter",
  "ln1_escale_aus_nachhilfe",
  "ln1_escale_kids_clothes_shoes",
  "ln1_escale_aus_hort"
)

# Outcome labels for plotting
outcome_labels <- c(
  "ln1_escale_aus_spiel"          = "Toys",
  "ln1_escale_aus_book"           = "Books and school supplies",
  "ln1_escale_aus_unter"          = "Extracurricular lessons",
  "ln1_escale_aus_nachhilfe"      = "Private tutoring",
  "ln1_escale_kids_clothes_shoes" = "Clothing and footwear",
  "ln1_escale_aus_hort"           = "After-school care"
)

# Controls included in all models
controls <- c("state", "east", "weiblich", "quarter", "kinder")

# Weight variable (must exist in data)
WEIGHT_VAR <- "weights_hb"

# Basic checks (fail early)
required_vars <- c(outcomes, "age_bins", controls, WEIGHT_VAR)
missing_vars  <- setdiff(required_vars, names(evs))
if (length(missing_vars) > 0) {
  stop("Missing required variables in dataset: ", paste(missing_vars, collapse = ", "))
}

# -------------------------------#
# 5) Plot style (kept minimal & journal-like)
# -------------------------------#

theme_journal <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.title.y = element_blank()
    )
}

# -------------------------------#
# 6) Helper functions
# -------------------------------#

# Fit weighted OLS and return tidy coefficients for selected terms
fit_models_tidy <- function(data, outcomes, rhs_terms, controls, term_keep, outcome_labels) {
  map_dfr(outcomes, function(y) {
    fml <- as.formula(
      paste0(y, " ~ ", paste(rhs_terms, collapse = " + "),
             " + ", paste(controls, collapse = " + "),
             " + factor(age_bins)")
    )

    mod <- lm(fml, data = data, weights = data[[WEIGHT_VAR]])

    tidy(mod) %>%
      filter(term %in% term_keep) %>%
      mutate(
        outcome = y,
        activity = unname(outcome_labels[[y]])
      )
  })
}

# Generic coefficient plot with 95% CI
plot_coef <- function(df, file_out, xlab = "", legend_rows = 1, color_values = NULL) {
  p <- ggplot(df, aes(x = estimate, y = activity, color = group)) +
    geom_point(position = position_dodge(width = 0.6), size = 2.6) +
    geom_errorbarh(
      aes(
        xmin = estimate - 1.96 * std.error,
        xmax = estimate + 1.96 * std.error
      ),
      position = position_dodge(width = 0.6),
      height = 0.18
    ) +
    geom_vline(xintercept = 0, linetype = "solid") +
    labs(x = xlab) +
    scale_x_continuous() +
    guides(color = guide_legend(nrow = legend_rows, byrow = TRUE)) +
    theme_journal(base_size = 12)

  if (!is.null(color_values)) {
    p <- p + scale_color_manual(values = color_values)
  }

  ggsave(
    filename = file_out,
    plot = p,
    width = 4.9,
    height = 4.2,
    dpi = 600,
    bg = "white"
  )

  invisible(p)
}

# -------------------------------#
# 7) Analyses & figures
# -------------------------------#

# 7.1 Single parent
df_single <- fit_models_tidy(
  data = evs,
  outcomes = outcomes,
  rhs_terms = c("single_par"),
  controls = controls,
  term_keep = c("single_par"),
  outcome_labels = outcome_labels
) %>%
  mutate(group = "Single parent household")

plot_coef(
  df = df_single,
  file_out = file.path(PATH_FIGURES, "EVS_coefplot_singlepar.png"),
  legend_rows = 1,
  color_values = c("Single parent household" = "#B60D45")
)

# 7.2 Migration background
df_mig <- fit_models_tidy(
  data = evs,
  outcomes = outcomes,
  rhs_terms = c("migback"),
  controls = controls,
  term_keep = c("migback"),
  outcome_labels = outcome_labels
) %>%
  mutate(group = "Migration background")

plot_coef(
  df = df_mig,
  file_out = file.path(PATH_FIGURES, "EVS_coefplot_migback.png"),
  legend_rows = 1,
  color_values = c("Migration background" = "#B60D45")
)

# 7.3 Income groups: poor vs rich (two coefficients per model)
df_income <- fit_models_tidy(
  data = evs,
  outcomes = outcomes,
  rhs_terms = c("poor", "rich"),
  controls = controls,
  term_keep = c("poor", "rich"),
  outcome_labels = outcome_labels
) %>%
  mutate(
    group = dplyr::recode(
      term,
      "poor" = "Low-income household",
      "rich" = "High-income household"
    )
  )

plot_coef(
  df = df_income,
  file_out = file.path(PATH_FIGURES, "EVS_coefplot_income.png"),
  legend_rows = 2,
  color_values = c(
    "Low-income household"  = "#B60D45",
    "High-income household" = "#005590"
  )
)

# 7.4 Risk of poverty
df_poverty_risk <- fit_models_tidy(
  data = evs,
  outcomes = outcomes,
  rhs_terms = c("armutsgefährdet"),
  controls = controls,
  term_keep = c("armutsgefährdet"),
  outcome_labels = outcome_labels
) %>%
  mutate(group = "Risk of poverty")

plot_coef(
  df = df_poverty_risk,
  file_out = file.path(PATH_FIGURES, "EVS_coefplot_armutsgefaehrdet.png"),
  legend_rows = 1,
  color_values = c("Risk of poverty" = "#B60D45")
)

# 7.5 Education groups: low vs high
df_educ <- fit_models_tidy(
  data = evs,
  outcomes = outcomes,
  rhs_terms = c("low_educ", "high_educ"),
  controls = controls,
  term_keep = c("low_educ", "high_educ"),
  outcome_labels = outcome_labels
) %>%
  mutate(
    group = dplyr::recode(
      term,
      "low_educ"  = "Lower parental education",
      "high_educ" = "Higher parental education"
    )
  )

plot_coef(
  df = df_educ,
  file_out = file.path(PATH_FIGURES, "EVS_coefplot_education.png"),
  legend_rows = 2,
  color_values = c(
    "Lower parental education"  = "#B60D45",
    "Higher parental education" = "#005590"
  )
)

# 7.6 Low education only
df_loweduc <- fit_models_tidy(
  data = evs,
  outcomes = outcomes,
  rhs_terms = c("low_educ"),
  controls = controls,
  term_keep = c("low_educ"),
  outcome_labels = outcome_labels
) %>%
  mutate(group = "Lower parental education")

plot_coef(
  df = df_loweduc,
  file_out = file.path(PATH_FIGURES, "EVS_coefplot_low_educ.png"),
  legend_rows = 1,
  color_values = c("Lower parental education" = "#B60D45")
)

# -------------------------------#
# 8) Session info (optional)
# -------------------------------#

message("Finished. Figures saved to: ", PATH_FIGURES)