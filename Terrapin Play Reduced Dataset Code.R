# =========================================================
# TERRAPIN PLAY ANALYSES: REDUCED DATASET
#
# This script reruns the primary inferential analyses after
# excluding two temporally clustered AM sessions to confirm
# that the main results are robust to their removal.
#
# Only key models are included (no descriptive statistics or
# full plotting pipeline), as the goal is to verify that the
# main conclusions remain unchanged.
# =========================================================

# ---------------------------------------------------------
# 1. LOAD REQUIRED PACKAGES
# ---------------------------------------------------------
library(dplyr)
library(lme4)
library(glmmTMB)
library(MASS)
library(lmerTest)
library(performance)

# ---------------------------------------------------------
# 2. IMPORT DATA
# ---------------------------------------------------------
setwd("~/Downloads")
terrapin_data <- read.csv("Terrapin Play Reduced Dataset.csv")

# ---------------------------------------------------------
# 3. PREPARE VARIABLES
# ---------------------------------------------------------
terrapin_data <- terrapin_data %>%
  mutate(
    turtle_id      = as.factor(turtle_id),
    time_of_day    = as.factor(time_of_day),
    entry_position = as.factor(entry_position),
    entry_prox     = as.factor(entry_prox)
  )

# Quick check: sessions by time of day
session_check <- terrapin_data %>%
  distinct(session, time_of_day) %>%
  count(time_of_day)

print(session_check)

# =========================================================
# SECTION A. TIME OF DAY ANALYSIS
# =========================================================

# ---------------------------------------------------------
# A1. Individual-level summary for modeling
# ---------------------------------------------------------
individual_summary_tod <- terrapin_data %>%
  group_by(turtle_id, time_of_day) %>%
  summarise(play_events_count = n(), .groups = "drop")

# ---------------------------------------------------------
# A2. Compare candidate distributions
# ---------------------------------------------------------
m_gaussian <- lmer(
  play_events_count ~ time_of_day + (1 | turtle_id),
  data = individual_summary_tod,
  REML = FALSE
)

m_poisson <- glmer(
  play_events_count ~ time_of_day + (1 | turtle_id),
  data = individual_summary_tod,
  family = poisson(link = "log")
)

m_nb <- glmmTMB(
  play_events_count ~ time_of_day + (1 | turtle_id),
  data = individual_summary_tod,
  family = nbinom2
)

AIC(m_gaussian, m_poisson, m_nb)

# Check overdispersion in Poisson model
check_overdispersion(m_poisson)

# ---------------------------------------------------------
# A3. Selected model
# ---------------------------------------------------------
# Based on model comparison and dispersion diagnostics, the Poisson model
# provided the best fit for the reduced dataset. Although this differs from
# the distribution selected in the full dataset, the direction, magnitude,
# and significance of the time-of-day effect remained consistent across models.
summary(m_poisson)

# =========================================================
# SECTION B. BOUTS ANALYSIS
# =========================================================

# ---------------------------------------------------------
# B1. Compare Poisson vs. negative binomial
# ---------------------------------------------------------
poisson_model <- glm(
  bouts ~ 1,
  family = poisson,
  data = terrapin_data
)

nb_model <- MASS::glm.nb(
  bouts ~ 1,
  data = terrapin_data
)

AIC(poisson_model, nb_model)

# ---------------------------------------------------------
# B2. Negative binomial mixed model
# ---------------------------------------------------------
bouts_model <- glmmTMB(
  bouts ~ time_of_day + (1 | turtle_id),
  data = terrapin_data,
  family = nbinom2
)

summary(bouts_model)

# =========================================================
# SECTION C. HELPER FUNCTION
# =========================================================

run_binomial_glmer <- function(data, response_level, response_var, include_tod = FALSE) {
  formula_text <- if (include_tod) {
    paste0(response_var, ' == "', response_level, '" ~ time_of_day + (1 | turtle_id)')
  } else {
    paste0(response_var, ' == "', response_level, '" ~ (1 | turtle_id)')
  }
  
  glmer(
    as.formula(formula_text),
    data = data,
    family = binomial
  )
}

# =========================================================
# SECTION D. ENTRY POSITION COMPARISONS
# =========================================================

# Direct vs Under
direct_vs_under <- terrapin_data %>%
  filter(entry_position %in% c("direct", "under")) %>%
  mutate(entry_position = droplevels(entry_position))

model_direct_vs_under <- run_binomial_glmer(
  direct_vs_under, "direct", "entry_position", FALSE
)

model_direct_vs_under_tod <- run_binomial_glmer(
  direct_vs_under, "direct", "entry_position", TRUE
)

# Wall vs Under
wall_vs_under <- terrapin_data %>%
  filter(entry_position %in% c("wall", "under")) %>%
  mutate(entry_position = droplevels(entry_position))

model_wall_vs_under <- run_binomial_glmer(
  wall_vs_under, "wall", "entry_position", FALSE
)

model_wall_vs_under_tod <- run_binomial_glmer(
  wall_vs_under, "wall", "entry_position", TRUE
)

# Direct vs Wall
direct_vs_wall <- terrapin_data %>%
  filter(entry_position %in% c("direct", "wall")) %>%
  mutate(entry_position = droplevels(entry_position))

model_direct_vs_wall <- run_binomial_glmer(
  direct_vs_wall, "direct", "entry_position", FALSE
)

model_direct_vs_wall_tod <- run_binomial_glmer(
  direct_vs_wall, "direct", "entry_position", TRUE
)

# Summaries
summary(model_direct_vs_under)
summary(model_wall_vs_under)
summary(model_direct_vs_wall)

summary(model_direct_vs_under_tod)
summary(model_wall_vs_under_tod)
summary(model_direct_vs_wall_tod)

# =========================================================
# SECTION E. ENTRY PROXIMITY COMPARISONS
# =========================================================

# Close vs Medium
close_vs_medium <- terrapin_data %>%
  filter(entry_prox %in% c("close", "medium")) %>%
  mutate(entry_prox = droplevels(entry_prox))

model_close_vs_medium <- run_binomial_glmer(
  close_vs_medium, "close", "entry_prox", FALSE
)

model_close_vs_medium_tod <- run_binomial_glmer(
  close_vs_medium, "close", "entry_prox", TRUE
)

# Close vs Far
close_vs_far <- terrapin_data %>%
  filter(entry_prox %in% c("close", "far")) %>%
  mutate(entry_prox = droplevels(entry_prox))

model_close_vs_far <- run_binomial_glmer(
  close_vs_far, "close", "entry_prox", FALSE
)

model_close_vs_far_tod <- run_binomial_glmer(
  close_vs_far, "close", "entry_prox", TRUE
)

# Medium vs Far
medium_vs_far <- terrapin_data %>%
  filter(entry_prox %in% c("medium", "far")) %>%
  mutate(entry_prox = droplevels(entry_prox))

model_medium_vs_far <- run_binomial_glmer(
  medium_vs_far, "medium", "entry_prox", FALSE
)

model_medium_vs_far_tod <- run_binomial_glmer(
  medium_vs_far, "medium", "entry_prox", TRUE
)

# Summaries
summary(model_close_vs_medium)
summary(model_close_vs_far)
summary(model_medium_vs_far)

summary(model_close_vs_medium_tod)
summary(model_close_vs_far_tod)
summary(model_medium_vs_far_tod)

