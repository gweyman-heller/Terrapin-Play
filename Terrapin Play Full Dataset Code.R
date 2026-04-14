# =========================================================
# TERRAPIN PLAY BEHAVIOR FULL DATASET ANALYSES
# Each row in the dataset represents one play sequence
# =========================================================

# ---------------------------------------------------------
# 1. LOAD REQUIRED PACKAGES
# ---------------------------------------------------------
library(dplyr)
library(ggplot2)
library(stringr)
library(moments)
library(lme4)
library(glmmTMB)
library(MASS)
library(tidyr)
library(lmerTest)
library(emmeans)

# ---------------------------------------------------------
# 2. SET WORKING DIRECTORY AND IMPORT DATA
# ---------------------------------------------------------
setwd("~/Downloads")
terrapin_data <- read.csv("Terrapin Play Full Dataset.csv")

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

# Quick check: number of sessions by time of day
session_check <- terrapin_data %>%
  distinct(session, time_of_day) %>%
  count(time_of_day)

print(session_check)

# =========================================================
# SECTION A. PLAY SEQUENCES PER INDIVIDUAL
# =========================================================

# ---------------------------------------------------------
# A1. Summarize total play sequences per turtle
# ---------------------------------------------------------
turtle_play_summary <- terrapin_data %>%
  count(turtle_id, name = "play_sequences_count")

# ---------------------------------------------------------
# A2. Plot distribution of play sequences per individual
# ---------------------------------------------------------
ggplot(turtle_play_summary, aes(x = play_sequences_count)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(
    title = "Distribution of Play Sequences per Individual",
    x = "Number of Play Sequences",
    y = "Frequency"
  ) +
  theme_minimal()

# ---------------------------------------------------------
# A3. Descriptive statistics
# ---------------------------------------------------------
play_seq_range  <- range(turtle_play_summary$play_sequences_count)
play_seq_median <- median(turtle_play_summary$play_sequences_count)
play_seq_iqr    <- IQR(turtle_play_summary$play_sequences_count)
play_seq_mean   <- mean(turtle_play_summary$play_sequences_count)
play_seq_sd     <- sd(turtle_play_summary$play_sequences_count)
play_seq_skew   <- skewness(turtle_play_summary$play_sequences_count)

cat("Skewness:", play_seq_skew, "\n")
cat("Range:", play_seq_range[1], "to", play_seq_range[2], "\n")
cat("Median:", play_seq_median, "\n")
cat("IQR:", play_seq_iqr, "\n")
cat("Mean:", play_seq_mean, "\n")
cat("Standard deviation:", play_seq_sd, "\n")

summary_stats <- data.frame(
  Statistic = c(
    "Range (Min)", "Range (Max)", "Median", "IQR",
    "Mean", "Standard Deviation"
  ),
  Value = c(
    play_seq_range[1], play_seq_range[2], play_seq_median,
    play_seq_iqr, play_seq_mean, play_seq_sd
  )
)

print(summary_stats)

# =========================================================
# SECTION B. TIME OF DAY EFFECTS
# =========================================================

# ---------------------------------------------------------
# B1. Overall summary of play sequences by time of day
# ---------------------------------------------------------
time_of_day_summary <- terrapin_data %>%
  count(time_of_day, name = "play_events_count") %>%
  mutate(percentage = play_events_count / sum(play_events_count) * 100)

print(time_of_day_summary)

# ---------------------------------------------------------
# B2. Individual-level summary for modeling
# ---------------------------------------------------------
individual_summary_tod <- terrapin_data %>%
  group_by(turtle_id, time_of_day) %>%
  summarise(play_events_count = n(), .groups = "drop")

# ---------------------------------------------------------
# B3. Compare possible model distributions
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
##The difference between the AICs is not statistically significant because its less than two
#Poisson assumes strong right skew and neg. binomial assumes overdispersion
#Given the low skew, gaussion is likely the better fit, but check the residuals just in case 
# ---------------------------------------------------------
# B4. Residual diagnostics for Gaussian model
# ---------------------------------------------------------
par(mfrow = c(1, 2))

plot(
  resid(m_gaussian) ~ fitted(m_gaussian),
  xlab = "Fitted values",
  ylab = "Residuals"
)
abline(h = 0, lty = 2)

qqnorm(resid(m_gaussian))
qqline(resid(m_gaussian))
#Gaussion is best fit 
summary(m_gaussian)

# ---------------------------------------------------------
# B5. Plot play sequences by time of day
# ---------------------------------------------------------
ggplot(
  time_of_day_summary,
  aes(x = str_to_title(time_of_day), y = percentage, fill = time_of_day)
) +
  geom_col(width = 0.7, color = "black") +
  geom_text(
    aes(label = sprintf("%.1f%%", percentage)),
    vjust = -0.4,
    size = 4.2
  ) +
  scale_fill_manual(values = c("AM" = "#4A90E2", "PM" = "#9E9E9E")) +
  labs(
    title = "Play Events by Time of Day",
    x = "Time of Day",
    y = "Percentage (%)"
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, max(time_of_day_summary$percentage) * 1.1)) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.position = "none"
  )

# =========================================================
# SECTION C. BOUTS ANALYSIS
# =========================================================

# ---------------------------------------------------------
# C1. Descriptive statistics for bouts
# ---------------------------------------------------------
bouts_min    <- min(terrapin_data$bouts, na.rm = TRUE)
bouts_max    <- max(terrapin_data$bouts, na.rm = TRUE)
bouts_median <- median(terrapin_data$bouts, na.rm = TRUE)
bouts_iqr    <- IQR(terrapin_data$bouts, na.rm = TRUE)
bouts_mean   <- mean(terrapin_data$bouts, na.rm = TRUE)
bouts_sd     <- sd(terrapin_data$bouts, na.rm = TRUE)
bouts_skew   <- skewness(terrapin_data$bouts, na.rm = TRUE)
bouts_var    <- var(terrapin_data$bouts, na.rm = TRUE)

cat("Minimum:", bouts_min, "\n")
cat("Maximum:", bouts_max, "\n")
cat("Median:", bouts_median, "\n")
cat("IQR:", bouts_iqr, "\n")
cat("Mean:", bouts_mean, "\n")
cat("Standard Deviation:", bouts_sd, "\n")
cat("Skewness:", bouts_skew, "\n")
cat("Variance:", bouts_var, "Mean:", bouts_mean, "\n")

# ---------------------------------------------------------
# C2. Compare Poisson vs. negative binomial
# ---------------------------------------------------------
poisson_model <- glm(bouts ~ 1, family = poisson, data = terrapin_data)
nb_model <- glm.nb(bouts ~ 1, data = terrapin_data)

AIC(poisson_model, nb_model)
#Lower AIC means better fit, in this case it ends up being the negative binomial
summary(nb_model)
# ---------------------------------------------------------
# C3. Negative binomial mixed model for bouts
# ---------------------------------------------------------
bouts_model <- glmmTMB(
  bouts ~ time_of_day + (1 | turtle_id),
  data = terrapin_data,
  family = nbinom2
)

summary(bouts_model)

# ---------------------------------------------------------
# C4. Frequency tables
# ---------------------------------------------------------
print(table(terrapin_data$bouts))
print(table(terrapin_data$entry_position))
print(table(terrapin_data$entry_prox))

# =========================================================
# SECTION D. HELPER FUNCTIONS
# =========================================================

# ---------------------------------------------------------
# D1. Binomial GLMM helper for pairwise comparisons
# ---------------------------------------------------------
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

# ---------------------------------------------------------
# D2. Create summary table of percentages and SE
# ---------------------------------------------------------
make_percentage_summary <- function(data, category_var) {
  data %>%
    group_by(time_of_day, .data[[category_var]]) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(time_of_day) %>%
    mutate(
      total = sum(count),
      percentage = (count / total) * 100,
      se = sqrt((percentage / 100) * (1 - (percentage / 100)) / total)
    ) %>%
    ungroup()
}

# ---------------------------------------------------------
# D3. Create faceted percentage bar plots
# ---------------------------------------------------------
make_faceted_percentage_plot <- function(data, x_var, title_text, x_label, sig_df = NULL) {
  
  required_data_cols <- c(x_var, "percentage", "se", "time_of_day")
  missing_data_cols <- setdiff(required_data_cols, names(data))
  
  if (length(missing_data_cols) > 0) {
    stop(
      "The following required columns are missing from `data`: ",
      paste(missing_data_cols, collapse = ", ")
    )
  }
  
  if (!is.null(sig_df)) {
    required_sig_cols <- c(
      "time_of_day", "group1", "group2", "y",
      "line_drop", "text_offset", "x_mid", "label"
    )
    missing_sig_cols <- setdiff(required_sig_cols, names(sig_df))
    
    if (length(missing_sig_cols) > 0) {
      stop(
        "The following required columns are missing from `sig_df`: ",
        paste(missing_sig_cols, collapse = ", ")
      )
    }
  }
  
  y_upper <- max(data$percentage + data$se, na.rm = TRUE) * 1.2
  
  plot_obj <- ggplot(
    data,
    aes(
      x = str_to_title(.data[[x_var]]),
      y = percentage,
      fill = time_of_day
    )
  ) +
    geom_col(width = 0.7, color = "black") +
    geom_errorbar(
      aes(ymin = percentage - se, ymax = percentage + se),
      width = 0.15,
      linewidth = 0.6
    ) +
    geom_text(
      aes(label = sprintf("%.1f%%", percentage)),
      vjust = -0.4,
      size = 4.2
    ) +
    facet_wrap(
      ~time_of_day,
      labeller = labeller(time_of_day = c(AM = "Morning", PM = "Afternoon"))
    ) +
    scale_fill_manual(values = c("AM" = "#4A90E2", "PM" = "#9E9E9E")) +
    labs(
      title = title_text,
      x = x_label,
      y = "Percentage (%)"
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(ylim = c(0, y_upper)) +
    theme_classic(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = 12),
      legend.position = "none",
      axis.text = element_text(color = "black")
    )
  
  if (!is.null(sig_df)) {
    plot_obj <- plot_obj +
      geom_segment(
        data = sig_df,
        aes(x = group1, xend = group2, y = y, yend = y),
        inherit.aes = FALSE
      ) +
      geom_segment(
        data = sig_df,
        aes(x = group1, xend = group1, y = y - line_drop, yend = y),
        inherit.aes = FALSE
      ) +
      geom_segment(
        data = sig_df,
        aes(x = group2, xend = group2, y = y - line_drop, yend = y),
        inherit.aes = FALSE
      ) +
      geom_text(
        data = sig_df,
        aes(x = x_mid, y = y + text_offset, label = label),
        inherit.aes = FALSE,
        size = 5
      )
  }
  
  return(plot_obj)
}

# =========================================================
# SECTION E. ENTRY POSITION COMPARISONS
# =========================================================

# ---------------------------------------------------------
# E1. Pairwise binomial models: entry position
# ---------------------------------------------------------

# Direct vs. Under
direct_vs_under <- terrapin_data %>%
  filter(entry_position %in% c("direct", "under")) %>%
  mutate(entry_position = droplevels(entry_position))

model_direct_vs_under <- run_binomial_glmer(
  direct_vs_under, "direct", "entry_position", include_tod = FALSE
)

model_direct_vs_under_tod <- run_binomial_glmer(
  direct_vs_under, "direct", "entry_position", include_tod = TRUE
)

# Wall vs. Under
wall_vs_under <- terrapin_data %>%
  filter(entry_position %in% c("wall", "under")) %>%
  mutate(entry_position = droplevels(entry_position))

model_wall_vs_under <- run_binomial_glmer(
  wall_vs_under, "wall", "entry_position", include_tod = FALSE
)

model_wall_vs_under_tod <- run_binomial_glmer(
  wall_vs_under, "wall", "entry_position", include_tod = TRUE
)

# Direct vs. Wall
direct_vs_wall <- terrapin_data %>%
  filter(entry_position %in% c("direct", "wall")) %>%
  mutate(entry_position = droplevels(entry_position))

model_direct_vs_wall <- run_binomial_glmer(
  direct_vs_wall, "direct", "entry_position", include_tod = FALSE
)

model_direct_vs_wall_tod <- run_binomial_glmer(
  direct_vs_wall, "direct", "entry_position", include_tod = TRUE
)

summary(model_direct_vs_under)
summary(model_wall_vs_under)
summary(model_direct_vs_wall)

summary(model_direct_vs_under_tod)
summary(model_wall_vs_under_tod)
summary(model_direct_vs_wall_tod)

# =========================================================
# SECTION F. ENTRY PROXIMITY COMPARISONS
# =========================================================

# ---------------------------------------------------------
# F1. Pairwise binomial models: entry proximity
# ---------------------------------------------------------

# Close vs. Medium
close_vs_medium <- terrapin_data %>%
  filter(entry_prox %in% c("close", "medium")) %>%
  mutate(entry_prox = droplevels(entry_prox))

model_close_vs_medium <- run_binomial_glmer(
  close_vs_medium, "close", "entry_prox", include_tod = FALSE
)

model_close_vs_medium_tod <- run_binomial_glmer(
  close_vs_medium, "close", "entry_prox", include_tod = TRUE
)

# Close vs. Far
close_vs_far <- terrapin_data %>%
  filter(entry_prox %in% c("close", "far")) %>%
  mutate(entry_prox = droplevels(entry_prox))

model_close_vs_far <- run_binomial_glmer(
  close_vs_far, "close", "entry_prox", include_tod = FALSE
)

model_close_vs_far_tod <- run_binomial_glmer(
  close_vs_far, "close", "entry_prox", include_tod = TRUE
)

# Medium vs. Far
medium_vs_far <- terrapin_data %>%
  filter(entry_prox %in% c("medium", "far")) %>%
  mutate(entry_prox = droplevels(entry_prox))

model_medium_vs_far <- run_binomial_glmer(
  medium_vs_far, "medium", "entry_prox", include_tod = FALSE
)

model_medium_vs_far_tod <- run_binomial_glmer(
  medium_vs_far, "medium", "entry_prox", include_tod = TRUE
)

summary(model_close_vs_medium)
summary(model_close_vs_far)
summary(model_medium_vs_far)

summary(model_close_vs_medium_tod)
summary(model_close_vs_far_tod)
summary(model_medium_vs_far_tod)

# =========================================================
# SECTION G. ENTRY POSITION AND PROXIMITY PLOTS
# =========================================================

# ---------------------------------------------------------
# G1. Overall entry position plot
# ---------------------------------------------------------
entry_position_overall <- terrapin_data %>%
  group_by(entry_position) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    total = sum(count),
    percentage = (count / total) * 100,
    se = sqrt((percentage / 100) * (1 - (percentage / 100)) / total),
    entry_position = factor(
      str_to_title(entry_position),
      levels = c("Under", "Wall", "Direct")
    )
  )

y_max <- max(entry_position_overall$percentage + entry_position_overall$se, na.rm = TRUE)
y1 <- y_max * 1.10
y2 <- y_max * 1.20
y3 <- y_max * 1.30
bracket_drop <- 2
star_offset <- 2
y_upper <- y3 + star_offset + 3

entry_position_plot_overall <- ggplot(
  entry_position_overall,
  aes(x = entry_position, y = percentage)
) +
  geom_col(width = 0.7, fill = "#4A90E2", color = "black") +
  geom_errorbar(
    aes(ymin = percentage - se, ymax = percentage + se),
    width = 0.15,
    linewidth = 0.6
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", percentage)),
    vjust = -0.4,
    size = 4.2
  ) +
  geom_segment(aes(x = 1, xend = 3, y = y1, yend = y1), inherit.aes = FALSE) +
  geom_segment(aes(x = 1, xend = 1, y = y1 - bracket_drop, yend = y1), inherit.aes = FALSE) +
  geom_segment(aes(x = 3, xend = 3, y = y1 - bracket_drop, yend = y1), inherit.aes = FALSE) +
  annotate("text", x = 2, y = y1 + star_offset, label = "*", size = 5) +
  geom_segment(aes(x = 1, xend = 2, y = y2, yend = y2), inherit.aes = FALSE) +
  geom_segment(aes(x = 1, xend = 1, y = y2 - bracket_drop, yend = y2), inherit.aes = FALSE) +
  geom_segment(aes(x = 2, xend = 2, y = y2 - bracket_drop, yend = y2), inherit.aes = FALSE) +
  annotate("text", x = 1.5, y = y2 + star_offset, label = "*", size = 5) +
  geom_segment(aes(x = 2, xend = 3, y = y3, yend = y3), inherit.aes = FALSE) +
  geom_segment(aes(x = 2, xend = 2, y = y3 - bracket_drop, yend = y3), inherit.aes = FALSE) +
  geom_segment(aes(x = 3, xend = 3, y = y3 - bracket_drop, yend = y3), inherit.aes = FALSE) +
  annotate("text", x = 2.5, y = y3 + star_offset, label = "*", size = 5) +
  labs(
    title = "Entry Position Preferences",
    x = "Entry Position",
    y = "Percentage (%)"
  ) +
  coord_cartesian(ylim = c(0, y_upper), clip = "off") +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

entry_position_plot_overall

ggsave(
  filename = "entry_position_overall_plot.pdf",
  plot = entry_position_plot_overall,
  width = 8,
  height = 6
)

# ---------------------------------------------------------
# G2. Predicted probability plot for entry proximity models
# ---------------------------------------------------------
emm_cm <- emmeans(model_close_vs_medium_tod, ~ time_of_day, type = "response")
pred_cm <- as.data.frame(emm_cm) %>%
  mutate(comparison = "Close vs Medium")

emm_cf <- emmeans(model_close_vs_far_tod, ~ time_of_day, type = "response")
pred_cf <- as.data.frame(emm_cf) %>%
  mutate(comparison = "Close vs Far")

emm_mf <- emmeans(model_medium_vs_far_tod, ~ time_of_day, type = "response")
pred_mf <- as.data.frame(emm_mf) %>%
  mutate(comparison = "Medium vs Far")

pred_prox <- bind_rows(pred_cm, pred_cf, pred_mf)

sig_labels <- data.frame(
  comparison = c("Close vs Far", "Medium vs Far"),
  label = c("*", "*"),
  y = c(0.95, 0.95)
)

p_prox_pred <- ggplot(
  pred_prox,
  aes(x = time_of_day, y = response, color = time_of_day)
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.12,
    linewidth = 0.7
  ) +
  facet_wrap(~comparison) +
  scale_color_manual(
    values = c("AM" = "#4A90E2", "PM" = "#9E9E9E"),
    labels = c("AM" = "Morning", "PM" = "Afternoon")
  ) +
  scale_x_discrete(labels = c("AM" = "Morning", "PM" = "Afternoon")) +
  labs(
    title = "Predicted Probabilities for Entry Proximity Comparisons",
    x = "Time of Day",
    y = "Predicted Probability",
    color = "Time of Day"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.position = "none"
  ) +
  geom_text(
    data = sig_labels,
    aes(x = 1.5, y = y, label = label),
    inherit.aes = FALSE,
    size = 5
  )

p_prox_pred

ggsave(
  filename = "entry_proximity_predicted_probabilities.pdf",
  plot = p_prox_pred,
  width = 9,
  height = 4.5
)


# =========================================================
# SECTION H. CHANGE IN PLAY SEQUENCES ACROSS SESSIONS
# =========================================================

# ---------------------------------------------------------
# H1. Summarize play sequences per session
# ---------------------------------------------------------
session_counts <- terrapin_data %>%
  group_by(session, date, time_of_day) %>%
  summarise(play_sequences = n(), .groups = "drop") %>%
  arrange(session)

print(session_counts)

# ---------------------------------------------------------
# H2. Check for overdispersion in a Poisson model
# ---------------------------------------------------------
pois_model <- glm(
  play_sequences ~ session * time_of_day,
  family = poisson,
  data = session_counts
)

dispersion_ratio <- deviance(pois_model) / df.residual(pois_model)
mean_play_sequences <- mean(session_counts$play_sequences)
var_play_sequences  <- var(session_counts$play_sequences)

cat("Dispersion ratio:", dispersion_ratio, "\n")
cat("Mean play sequences:", mean_play_sequences, "\n")
cat("Variance play sequences:", var_play_sequences, "\n")

# ---------------------------------------------------------
# H3. Fit negative binomial model for temporal change
# Random intercept for date accounts for multiple sessions
# occurring on the same day
# ---------------------------------------------------------
time_change <- glmmTMB(
  play_sequences ~ session + time_of_day + (1 | date),
  data = session_counts,
  family = nbinom2
)

summary(time_change)

# ---------------------------------------------------------
# H4. Identify potential AM outlier using IQR rule
# ---------------------------------------------------------
am_sessions <- session_counts %>%
  filter(time_of_day == "AM") %>%
  arrange(desc(play_sequences))

Q1 <- quantile(am_sessions$play_sequences, 0.25, na.rm = TRUE)
Q3 <- quantile(am_sessions$play_sequences, 0.75, na.rm = TRUE)
IQR_val <- IQR(am_sessions$play_sequences, na.rm = TRUE)

upper_bound <- Q3 + 1.5 * IQR_val

am_sessions_outlier_check <- am_sessions %>%
  mutate(outlier = play_sequences > upper_bound)

print(am_sessions_outlier_check)

# Save the outlier session number(s)
outlier_sessions <- am_sessions_outlier_check %>%
  filter(outlier) %>%
  pull(session)

print(outlier_sessions)

# ---------------------------------------------------------
# H5. Remove outlier session(s) from the original dataset
# This is the critical fix: remove the session from
# terrapin_data before rebuilding summaries or rerunning models
# ---------------------------------------------------------
terrapin_data_no_outlier <- terrapin_data %>%
  filter(!session %in% outlier_sessions)

# Rebuild session-level counts without the outlier
session_counts_no_outlier <- terrapin_data_no_outlier %>%
  group_by(session, date, time_of_day) %>%
  summarise(play_sequences = n(), .groups = "drop") %>%
  arrange(session)

print(session_counts_no_outlier)

# ---------------------------------------------------------
# H6. Re-run temporal model without the outlier session
# ---------------------------------------------------------
time_change_no_outlier <- glmmTMB(
  play_sequences ~ session + time_of_day + (1 | date),
  data = session_counts_no_outlier,
  family = nbinom2
)

summary(time_change_no_outlier)

# ---------------------------------------------------------
# H7. Re-run individual time-of-day model without outlier
# This now correctly excludes the outlier session
# ---------------------------------------------------------
individual_summary_tod_no_outlier <- terrapin_data_no_outlier %>%
  group_by(turtle_id, time_of_day) %>%
  summarise(play_events_count = n(), .groups = "drop")

m_gaussian_no_outlier <- lmer(
  play_events_count ~ time_of_day + (1 | turtle_id),
  data = individual_summary_tod_no_outlier,
  REML = FALSE
)

summary(m_gaussian_no_outlier)

# ---------------------------------------------------------
# H8. Plot play sequences across sessions and mark outlier
# ---------------------------------------------------------
outlier_point <- session_counts %>%
  filter(session %in% outlier_sessions)

p_sessions <- ggplot(
  session_counts,
  aes(x = session, y = play_sequences, color = time_of_day)
) +
  geom_line(aes(group = time_of_day), linewidth = 0.6) +
  geom_point(size = 3) +
  geom_point(
    data = outlier_point,
    shape = 21,
    size = 5,
    stroke = 1.2,
    fill = NA,
    color = "black"
  ) +
  geom_text(
    data = outlier_point,
    aes(label = "Outlier"),
    vjust = -1,
    color = "black",
    size = 4
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linetype = "dashed",
    linewidth = 1.2
  ) +
  scale_color_manual(
    values = c("AM" = "#4A90E2", "PM" = "#9E9E9E"),
    labels = c("AM" = "Morning", "PM" = "Afternoon")
  ) +
  labs(
    title = "Current-Riding Sequences Across Sessions",
    x = "Session Number",
    y = "Number of Sequences",
    color = "Time of Day"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

p_sessions

ggsave(
  "play_sequences_sessions.pdf",
  plot = p_sessions,
  width = 8,
  height = 6
)