# March 4, 2026
# Applied Population Ecology
# Cleaning iNaturalist data

# Load packages
library(tidyverse)   
library(lubridate)   
library(sf)          
library(readr)
library(ggplot2)

# Import data
pronghorn <- read_csv("data/pronghorn_carrizo_observations.csv")
tule_elk <- read_csv("data/tuleelk_carrizo_observations.csv")

# Standardize date formats
# Remove weekday names (Mon, Tue, etc.)
pronghorn$observed_on_string <- gsub("^(Mon|Tue|Wed|Thu|Fri|Sat|Sun)\\s+", "", pronghorn$observed_on_string)

# Remove time, timezone, or anything starting with T
pronghorn$observed_on_string <- gsub("(\\s\\d{1,2}:\\d{2}.*$)|T.*$", "", pronghorn$observed_on_string)

# Run for loop
pronghorn$date <- NA
for (i in 1:nrow(pronghorn)) {
  
  parsed_date <- parse_date_time(
    pronghorn$observed_on_string[i],
    orders = c("ymd","mdy","B d Y","b d Y","Y/m/d"))
  
  pronghorn$date[i] <- format(as.Date(parsed_date), "%Y-%m-%d")}


# Clean data sets
pronghorn_clean <- pronghorn %>%
  select(user_id, date, latitude, longitude)

#_____________________________________________________________________


# Ensure date column is parsed with lubridate
# Adjust "observed_on" to your actual date column name
pronghorn_clean$date <- ymd(pronghorn_clean$date)
pronghorn_clean$year <- year(pronghorn_clean$date)

# --- Step 1: Get number of observations per unique observer per year ---

# Get all unique years
unique_years <- sort(unique(pronghorn_clean$year))

# --- Step 1: Get number of observations per unique observer per year ---
results <- data.frame(
  year = integer(),
  observer_id = character(),
  n_observations = integer(),
  stringsAsFactors = FALSE
)

for (yr in unique_years) {
  year_data <- pronghorn_clean[pronghorn_clean$year == yr, ]
  unique_observers <- unique(year_data$user_id)
  
  for (obs in unique_observers) {
    n_obs <- nrow(year_data[year_data$user_id == obs, ])
    results <- rbind(results, data.frame(
      year = yr,
      observer_id = obs,
      n_observations = n_obs,
      stringsAsFactors = FALSE
    ))
  }
}

# --- Step 2: Average observations per observer per year ---
yearly_summary <- results %>%
  group_by(year) %>%
  summarise(
    n_observers = n(),
    avg_obs_per_observer = mean(n_observations)
  )

for (yr in unique_years) {
  
  # Subset results for this year
  year_results <- results[results$year == yr, ]
  
  # Calculate metrics
  n_observers    <- nrow(year_results)
  avg_obs        <- mean(year_results$n_observations)
  
  yearly_summary <- rbind(yearly_summary, data.frame(
    year = yr,
    n_observers = n_observers,
    avg_obs_per_observer = avg_obs,
    stringsAsFactors = FALSE
  ))
}

# --- Preview results ---
print(results)         # Per observer per year

# Arrange yearly_summary by year ascending
yearly_summary <- yearly_summary %>%
  arrange(year)

print(yearly_summary)  # Averaged by year

# --- Step 3: Plot average observations per observer over time ---

pronghorn_observer_plot <- ggplot(yearly_summary, aes(x = year, y = avg_obs_per_observer)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(aes(size = n_observers), color = "steelblue") +
  scale_size_continuous(name = "Number of\nObservers") +
  labs(
    title = "Average Pronghorn Observations per Observer per Year",
    subtitle = "Point size reflects number of unique observers",
    x = "Year",
    y = "Avg. Observations per Observer"
  ) +
  theme_minimal()


################################################################################
# TULEEEEEEE

# Standardize date formats
# Remove weekday names (Mon, Tue, etc.)
tule_elk$observed_on_string <- gsub("^(Mon|Tue|Wed|Thu|Fri|Sat|Sun)\\s+", "", tule_elk$observed_on_string)

# Remove time, timezone, or anything starting with T
tule_elk$observed_on_string <- gsub("(\\s\\d{1,2}:\\d{2}.*$)|T.*$", "", tule_elk$observed_on_string)


tule_elk$date <- NA
for (i in 1:nrow(tule_elk)) {
  
  parsed_date <- parse_date_time(
    tule_elk$observed_on_string[i],
    orders = c("ymd","mdy","B d Y","b d Y","Y/m/d"))
  
  tule_elk$date[i] <- format(as.Date(parsed_date), "%Y-%m-%d")}

# Clean data sets
tule_elk_clean <- tule_elk %>%
  select(user_id, date, latitude, longitude)

#_____________________________________________________________________


# Ensure date column is parsed with lubridate
# Adjust "observed_on" to your actual date column name
tule_elk_clean$date <- ymd(tule_elk_clean$date)
tule_elk_clean$year <- year(tule_elk_clean$date)

# --- Step 1: Get number of observations per unique observer per year ---

# Get all unique years
unique_years_tule <- sort(unique(tule_elk_clean$year))

# --- Step 1: Get number of observations per unique observer per year ---
results_tule <- data.frame(
  year = integer(),
  observer_id = character(),
  n_observations = integer(),
  stringsAsFactors = FALSE
)

for (yr in unique_years_tule) {
  year_data_tule <- tule_elk_clean[tule_elk_clean$year == yr, ]
  unique_observers_tule<- unique(year_data_tule$user_id)
  
  for (obs in unique_observers_tule) {
    n_obs <- nrow(year_data_tule[year_data_tule$user_id == obs, ])
    results_tule <- rbind(results_tule, data.frame(
      year = yr,
      observer_id = obs,
      n_observations = n_obs,
      stringsAsFactors = FALSE
    ))
  }
}

# --- Step 2: Average observations per observer per year ---
yearly_summary_tule <- data.frame(
  year = integer(),
  n_observers = integer(),
  avg_obs_per_observer = numeric(),
  stringsAsFactors = FALSE
)
for (yr in unique_years_tule) {
  
  # Subset results for this year
  year_results_tule <- results_tule[results_tule$year == yr, ]
  
  # Calculate metrics
  n_observers    <- nrow(year_results_tule)
  avg_obs        <- mean(year_results_tule$n_observations)
  
  yearly_summary_tule <- rbind(yearly_summary_tule, data.frame(
    year = yr,
    n_observers = n_observers,
    avg_obs_per_observer = avg_obs,
    stringsAsFactors = FALSE
  ))
}

# --- Preview results ---
print(results_tule)         # Per observer per year

# Arrange yearly_summary by year ascending
yearly_summary_tule <- yearly_summary_tule %>%
  arrange(year)

print(yearly_summary_tule)  # Averaged by year

# --- Step 3: Plot average observations per observer over time ---

tule_observer_plot <- ggplot(yearly_summary_tule, aes(x = year, y = avg_obs_per_observer)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(aes(size = n_observers), color = "steelblue") +
  scale_size_continuous(name = "Number of\nObservers") +
  labs(
    title = "Average Tule elk Observations per Observer per Year",
    subtitle = "Point size reflects number of unique observers",
    x = "Year",
    y = "Avg. Observations per Observer"
  ) +
  theme_minimal()

# Print to toggle
print(tule_observer_plot)
print(pronghorn_observer_plot)

###################################################################
# Alter names
# --- Step 1: Summarize pronghorn per year ---
pronghorn_yearly <- pronghorn_clean %>%
  group_by(year) %>%
  summarise(
    n_observers_pronghorn = n_distinct(user_id),
    avg_obs_pronghorn = n() / n_distinct(user_id),
    .groups = "drop"
  )

# --- Step 2: Summarize tule elk per year ---
tule_yearly <- tule_elk_clean %>%
  group_by(year) %>%
  summarise(
    n_observers_tule = n_distinct(user_id),
    avg_obs_tule = n() / n_distinct(user_id),
    .groups = "drop"
  )

# --- Step 3: Merge into one row per year ---
combined_yearly <- full_join(pronghorn_yearly, tule_yearly, by = "year") %>%
  arrange(year)


# Bind rows
combined_summary <- bind_rows(pronghorn_summary, tule_summary)

# Pivot longer for plotting
combined_long <- combined_yearly %>%
  pivot_longer(
    cols = c(avg_obs_pronghorn, avg_obs_tule, n_observers_pronghorn, n_observers_tule),
    names_to = c(".value", "species"),
    names_pattern = "(.*)_(pronghorn|tule)"
  )

# Plot combined dataset
combined_plot <- ggplot(combined_long, aes(x = year, y = avg_obs, color = species, size = n_observers)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_color_manual(values = c("pronghorn" = "#22A884FF", "tule" = "#FDE725FF")) +
  scale_size_continuous(name = "Number of Observers") +
  labs(
    title = "Average community science sampling effort per year",
    x = "Year",
    y = "Average observations per observer",
    color = "Species"
  ) +
  theme_minimal()

# Save the plot
ggsave("sampling_effort_plot.png", 
       plot = combined_plot, 
       width = 8, 
       height = 5, 
       dpi = 300, 
       bg = "white")

# Bar chart of number of observers per year by species
observer_bar_plot <- ggplot(combined_long, aes(x = year, y = n_observers, fill = species)) +
  geom_col(position = "dodge") +   # use "dodge" to put bars side by side
  scale_fill_manual(values = c("pronghorn" = "#22A884FF", "tule" = "#FDE725FF")) +
  labs(
    title = "Number of unique observers per year",
    x = "Year",
    y = "Number of observers",
    fill = "Species"
  ) +
  theme_minimal()

# Save the plot
ggsave("observer_plot.png", 
       plot = observer_bar_plot, 
       width = 8, 
       height = 5, 
       dpi = 300, 
       bg = "white")





