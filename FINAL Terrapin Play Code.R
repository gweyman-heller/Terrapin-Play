library(readxl)
directory_path <- file.path(Sys.getenv("HOME"), "Downloads", "Cleaned Terrapin Play Data.xlsx")
terrapin_data <- read_excel(directory_path)
#turlte_id needs to turned into a factor so its categorical instead of numeric 
terrapin_data$turtle_id <- as.factor(terrapin_data$turtle_id)
#make a table of how much each individual turtle played 
library(dplyr)
turtle_play_summary <- terrapin_data %>%
  count(turtle_id, name = "play_sequences_count")
#Graph distribution of play sequences per individual 
ggplot(turtle_play_summary, aes(x = play_sequences_count)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribution of Play Sequences",
       x = "Number of Play Sequences",
       y = "Frequency") +
  theme_minimal()

#skewness 
library(moments)
skewness_value <- skewness(turtle_play_summary$play_sequences_count)
cat("Skewness of play sequences:", skewness_value, "\n")
#Descriptive stats
range_values <- range(turtle_play_summary$play_sequences_count)
median_value <- median(turtle_play_summary$play_sequences_count)
iqr_value <- IQR(turtle_play_summary$play_sequences_count)
cat("Range of play sequences:", range_values[1], "to", range_values[2], "\n")
cat("Median of play sequences:", median_value, "\n")
cat("IQR of play sequences:", iqr_value, "\n")
mean_value <- mean(turtle_play_summary$play_sequences_count)
sd_value <- sd(turtle_play_summary$play_sequences_count)
cat("Mean of play sequences:", mean_value, "\n")
cat("Standard deviation of play sequences:", sd_value, "\n")
#Summary Table
summary_stats <- data.frame(
  Statistic = c("Range (Min)", "Range (Max)", "Median", "IQR", "Mean", "Standard Deviation"),
  Value = c(range_values[1], range_values[2], median_value, iqr_value, mean_value, sd_value)
)
print(summary_stats)

#Summary table to count the number of play events for each time of day (AM vs PM)
time_of_day_summary <- terrapin_data %>%
  group_by(time_of_day) %>%
  summarise(play_events_count = n())
#Calculate percentages
time_of_day_summary <- time_of_day_summary %>%
  mutate(percentage = play_events_count / sum(play_events_count) * 100)

#Create a summary table by individual showing the number of play events for AM and PM
individual_summary_tod <- terrapin_data %>%
  group_by(turtle_id, time_of_day) %>%
  summarize(
    play_events_count = n(),
  )
#is there a significant difference of play events between AM and PM while holding for individual
library(lme4)
#linear mixed-effects model
playTOD_model <- lmer(play_events_count ~ time_of_day + (1 | turtle_id), data = individual_summary_tod)

#Summary of the model
summary(playTOD_model)

#time of day bar graph for number of play events 
ggplot(time_of_day_summary, aes(x = time_of_day, y = percentage, fill = time_of_day)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(round(percentage), "%")), vjust = -0.5) +
  labs(title = "Percentage of Play Events by Time of Day",
       x = "Time of Day", y = "Percentage") +
  theme_minimal() +
  scale_fill_manual(values = c("AM" = "skyblue", "PM" = "orange")) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 12))
#Graph and descriptive stats for bouts 
if (!requireNamespace("moments", quietly = TRUE)) 

bouts_min <- min(terrapin_data$bouts, na.rm = TRUE)
bouts_max <- max(terrapin_data$bouts, na.rm = TRUE)
bouts_median <- median(terrapin_data$bouts, na.rm = TRUE)
bouts_iqr <- IQR(terrapin_data$bouts, na.rm = TRUE)
bouts_mean <- mean(terrapin_data$bouts, na.rm = TRUE)
bouts_sd <- sd(terrapin_data$bouts, na.rm = TRUE)
bouts_skew <- skewness(terrapin_data$bouts, na.rm = TRUE)

cat("Minimum:", bouts_min, "\n")
cat("Maximum:", bouts_max, "\n")
cat("Median:", bouts_median, "\n")
cat("IQR:", bouts_iqr, "\n")
cat("Mean:", bouts_mean, "\n")
cat("Standard Deviation:", bouts_sd, "\n")
cat("Skewness:", bouts_skew, "\n")

#Looking for overdispersion: If the variance is significantly larger than the mean, its overdispersed (in this case it is) 
var_bouts <- var(terrapin_data$bouts)
cat("Variance:", var_bouts, "Mean:", bouts_mean, "\n")
#run a negative binomial model to account for the overdispersion. Compare it to the poisson model
#lower AIC means better fit, in this case it ends up being the negative binomial
library(MASS)
nb_model <- glm.nb(bouts ~ 1, data = terrapin_data)
summary(nb_model)
poisson_model <- glm(bouts ~ 1, family = poisson, data = terrapin_data)
AIC(poisson_model, nb_model)

#negative binomial model with random intercepts for turtle_id
library(glmmTMB)
bouts_model <- glmmTMB(bouts ~ time_of_day + (1 | turtle_id), 
                       data = terrapin_data, 
                       family = nbinom2)
summary(bouts_model)

#general frequencies for bouts, entry positions, and entry proximities
bouts_freq <- table(terrapin_data$bouts)
print(bouts_freq)
pos_freq <- table(terrapin_data$entry_position)
print(pos_freq)
prox_freq <- table(terrapin_data$entry_prox)
print(prox_freq)

#Entry position stats:
#entry position needs to turned into a factor 
terrapin_data$entry_position <- as.factor(terrapin_data$entry_position)
#compare position categories 
model_direct_vs_under <- glmer(entry_position == "direct" ~ (1 | turtle_id), data = terrapin_data, family = binomial)
summary(model_direct_vs_under)
model_wall_vs_under <- glmer(entry_position == "wall" ~ (1 | turtle_id), data = terrapin_data, family = binomial)
summary(model_wall_vs_under)
model_direct_vs_wall <- glmer(entry_position == "direct" ~ (1 | turtle_id), data = subset(terrapin_data, entry_position %in% c("direct", "wall")), family = binomial)
summary(model_direct_vs_wall)
#see if time of day has an impact on these results 
model_direct_vs_under2 <- glmer(entry_position == "direct" ~ time_of_day + (1 | turtle_id), 
                                data = terrapin_data, family = binomial)
summary(model_direct_vs_under2)
model_wall_vs_under2 <- glmer(entry_position == "wall" ~ time_of_day + (1 | turtle_id), 
                              data = terrapin_data, family = binomial)
summary(model_wall_vs_under2)
model_direct_vs_wall2 <- glmer(entry_position == "direct" ~ time_of_day + (1 | turtle_id), 
                               data = subset(terrapin_data, entry_position %in% c("direct", "wall")), 
                               family = binomial)
summary(model_direct_vs_wall2)

#Plot entry postions
#Calculate percentages first
entry_position_summary <- terrapin_data %>%
  group_by(time_of_day, entry_position) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(time_of_day) %>%
  mutate(total = sum(count),
         percentage = (count / total) * 100,
         se = sqrt((percentage / 100) * (1 - (percentage / 100)) / total)) %>%
  ungroup()

#Check
head(entry_position_summary)
#Histogram
ggplot(entry_position_summary, aes(x = entry_position, y = percentage, fill = time_of_day)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = percentage - se, ymax = percentage + se), 
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,  
            size = 4) +  
  labs(title = "Entry Position Percentages by Time of Day",
       x = "Entry Position",
       y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("AM" = "skyblue", "PM" = "orange")) +
  theme(
    legend.position = "right",  
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12)
  )
#Entry Proximity Stats
#entry proximity needs to turned into a factor  
terrapin_data$entry_prox <- as.factor(terrapin_data$entry_prox)
#time of day needs to turned into a factor 
terrapin_data$time_of_day <- as.factor(terrapin_data$time_of_day)
#see if time of day has an impact on prox
#Entry Proximity Options
model_close_vs_medium <- glmer(entry_prox == "close" ~ (1 | turtle_id), 
                               data = terrapin_data, 
                               family = binomial)
summary(model_close_vs_medium)

model_close_vs_far <- glmer(entry_prox == "close" ~ (1 | turtle_id), 
                            data = terrapin_data, 
                            family = binomial)
summary(model_close_vs_far)

model_medium_vs_far <- glmer(entry_prox == "medium" ~ (1 | turtle_id), 
                             data = terrapin_data, 
                             family = binomial)
summary(model_medium_vs_far)

#Time of Day and Entry Proximity Preferences
model_close_time_of_day <- glmer(entry_prox == "close" ~ time_of_day + (1 | turtle_id), 
                                 data = terrapin_data, 
                                 family = binomial)
summary(model_close_time_of_day)

model_medium_time_of_day <- glmer(entry_prox == "medium" ~ time_of_day + (1 | turtle_id), 
                                  data = terrapin_data, 
                                  family = binomial)
summary(model_medium_time_of_day)

model_far_time_of_day <- glmer(entry_prox == "far" ~ time_of_day + (1 | turtle_id), 
                               data = terrapin_data, 
                               family = binomial)
summary(model_far_time_of_day)
#Graph Entry Proximity
#Calculate percentages 
entry_prox_summary <- terrapin_data %>%
  group_by(time_of_day, entry_prox) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(time_of_day) %>%
  mutate(total = sum(count),
         percentage = (count / total) * 100,
         se = sqrt((percentage / 100) * (1 - (percentage / 100)) / total)) %>%
  ungroup()

ggplot(entry_prox_summary, aes(x = entry_prox, y = percentage, fill = time_of_day)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = percentage - se, ymax = percentage + se), 
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 4) +  
  labs(title = "Entry Proximity Percentages by Time of Day",
       x = "Entry Proximity",
       y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("AM" = "skyblue", "PM" = "orange")) +
  theme(
    legend.position = "right",  
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12)
  )


