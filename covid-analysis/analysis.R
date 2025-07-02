# clearing the environment
rm(list = ls())

# loading libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(tidyr)
library(purrr)

# load data
full_data <- readRDS("examdata.rds")

# check structure and summary to check dataset
str(full_data)
summary(full_data)

# Rename the states with full names
state_names <- c(vic = "Victoria", nsw = "New South Wales", qld = "Queensland", sa = "South Australia")
full_data$state <- state_names[full_data$state]

# Given population of each state to normalize values later
pop_data <- c(
  "New South Wales" = 8545100,
  "Victoria" = 7011100,
  "Queensland" = 5618800,
  "South Australia" = 1891700
)

# names(pop_data)

# arranging and adding cumulative and normalized columns
full_data <- full_data %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(
    total_cases = cumsum(replace_na(new_cases, 0)),
    total_deaths = cumsum(replace_na(new_deaths, 0)),
    population = pop_data[state],
    new_cases_per_100k = (new_cases / population) * 100000,
    new_deaths_per_100k = (new_deaths / population) * 100000,
    total_cases_per_100k = cumsum(replace_na(new_cases_per_100k, 0)),
    total_deaths_per_100k = cumsum(replace_na(new_deaths_per_100k, 0)),
    month = floor_date(date, unit = "month"),
    year = year(date)
  ) %>%
  ungroup()

#----------------------- summary of cases reported for intro-----------

# Summary of total cases per 100k for each state
total_cases_summary <- full_data %>%
  group_by(state) %>%
  summarise(
    start_date = min(date, na.rm = TRUE),
    end_date = max(date, na.rm = TRUE),
    min_total = round(min(total_cases_per_100k, na.rm = TRUE), 1),
    avg_daily_cases = mean(total_cases_per_100k, na.rm = TRUE),
    max_total = round(max(total_cases_per_100k, na.rm = TRUE), 1),
  )

print(total_cases_summary)


ggplot(full_data, aes(x = date, y = total_cases_per_100k, color = state)) +
  geom_smooth(linewidth  = 1) +
  scale_color_manual(values = c(
    "Victoria" = "#177e89",
    "New South Wales" = "#a4b8c4",
    "Queensland" = "#db3a34",
    "South Australia" = "#ffc857"
  )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    breaks = seq(0, 1500000, by = 10000),
    labels = scales::comma
  ) +
  labs(
    title = "Cumulative COVID-19 Cases per 100,000 (2020–mid 2025)",
    subtitle = "Total confirmed cases per 100,000 people, normalized by population",
    x = "Year",
    y = "Total Cases per 100,000",
    color = "State"
  ) +
  theme_minimal()

#-------- summary of death cases reported for intro-----------


# Summary of total cases per 100k across time for each state
total_death_summary <- full_data %>%
  group_by(state) %>%
  summarise(
    start_date = min(date, na.rm = TRUE),
    end_date = max(date, na.rm = TRUE),
    min_total = round(min(total_deaths_per_100k, na.rm = TRUE), 1),
    avg_daily_cases = mean(total_deaths_per_100k, na.rm = TRUE),
    max_total = round(max(total_deaths_per_100k, na.rm = TRUE), 1),
  )

print(total_death_summary)



ggplot(full_data, aes(x = date, y = total_deaths_per_100k, color = state)) +
  geom_smooth(linewidth  = 1) +
  scale_color_manual(values = c(
    "Victoria" = "#177e89",
    "New South Wales" = "#a4b8c4",
    "Queensland" = "#db3a34",
    "South Australia" = "#ffc857"
  )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    breaks = seq(0, 200, by = 10),
    labels = scales::comma
  ) +
  labs(
    title = "Cumulative COVID-19 Deaths per 100,000 (2020– mid 2025)",
    subtitle = "Total confirmed death cases per 100,000 people, normalized by population",
    x = "Year",
    y = "Total Deaths reported per 100,000",
    color = "State"
  ) +
  theme_minimal()

# ------------ ----------- last 24 months cases --------------------

# Filter for the last 24 months
last_24_data <- full_data %>%
  filter(date >= as.Date("2023-05-01"), !is.na(new_cases_per_100k), new_cases_per_100k >= 0)

# Calculate cumulative total cases again for 24 months
last_24_cumulative <- last_24_data %>%
  group_by(state) %>%
  arrange(date) %>%
  mutate(cumulative_cases_per_100k = cumsum(new_cases_per_100k)) %>%
  ungroup()

# Plot line graph 
ggplot(last_24_cumulative, aes(x = date, y = cumulative_cases_per_100k, color = state)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c(
    "Victoria" = "#177e89",
    "New South Wales" = "#a4b8c4",
    "Queensland" = "#db3a34",
    "South Australia" = "#ffc857"
  )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    breaks = seq(0, 15000, by = 1000),
    labels = scales::comma
  ) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y") +
  labs(
    title = "Cumulative COVID-19 Cases per 100,000 (May 2023 – June 2025)",
    subtitle = "Total confirmed cases per 100,000 people, by state",
    x = "Month",
    y = "Cumulative Cases per 100,000",
    color = "State"
  ) +
  theme_minimal()

# Get starting and ending values per state
growth_stats <- last_24_cumulative %>%
  group_by(state) %>%
  summarise(
    start_value = round(first(cumulative_cases_per_100k), 1),
    end_value = round(last(cumulative_cases_per_100k), 1),
    total_growth = round(end_value - start_value, 1)
  ) %>%
  arrange(desc(total_growth))

print(growth_stats)


#------------------------- monthly cases box plot-------------------

# monthly new cases calculation
monthly_cases <- last_24_data %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(state, month) %>%
  summarise(
    new_cases_per_100k = sum(new_cases_per_100k, na.rm = TRUE),
    .groups = "drop"
  )

# Boxplot of monthly new cases
ggplot(monthly_cases, aes(x = state, y = new_cases_per_100k, fill = state)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Victoria" = "#177e89",
    "New South Wales" = "#a4b8c4",
    "Queensland" = "#db3a34",
    "South Australia" = "#ffc857"
  )) +
  scale_y_continuous(breaks = seq(0, 1000, by = 100)) +
  labs(
    title = "Monthly New COVID-19 Cases (Per 100,000 Population)",
    subtitle = "Distribution from May 2023 – June 2025",
    x = "State",
    y = "Monthly New Cases per 100,000"
  ) +
  theme_minimal()

# Summary table of min, max, avg per state
state_summary <- monthly_cases %>%
  group_by(state) %>%
  summarise(
    min_per_100k = round(min(new_cases_per_100k), 1),
    max_per_100k = round(max(new_cases_per_100k), 1),
    avg_per_100k = round(mean(new_cases_per_100k), 1),
    .groups = "drop"
  )

print(state_summary)


# ------------------------- Death analysis -----------------

# filter data till Sep 30, 2023 
valid_death_data <- full_data %>%
  filter(date <= as.Date("2023-09-30")) %>%
  filter(!is.na(new_cases_per_100k), !is.na(new_deaths_per_100k)) %>%
  filter(new_cases_per_100k >= 0, new_deaths_per_100k >= 0)

# Yearly deaths distribution
yearly_death_stats <- valid_death_data %>%
  group_by(state, year) %>%
  summarise(
    total_deaths_per_100k = round(sum(new_deaths_per_100k, na.rm = TRUE), 1),
    .groups = "drop"
  )

ggplot(yearly_death_stats, aes(x = factor(year), y = total_deaths_per_100k, fill = state)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = round(total_deaths_per_100k, 1)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  scale_fill_manual(values = c(
    "Victoria" = "#177e89",
    "New South Wales" = "#a4b8c4",
    "Queensland" = "#db3a34",
    "South Australia" = "#ffc857"
  )) +
  labs(
    title = "Annual COVID-19 Deaths per 100,000 (Valid Data Only)",
    x = "Year",
    y = "Deaths per 100,000",
    fill = "State"
  ) +
  theme_minimal()


# For scatterplot
scatter_data <- full_data %>%
  filter(date < as.Date("2023-10-01")) %>%
  filter(!is.na(new_cases_per_100k), !is.na(new_deaths_per_100k)) %>%
  filter(new_cases_per_100k >= 0, new_deaths_per_100k >= 0)


# Plot: New cases vs deaths per 100k with regression line, faceted by state
ggplot(scatter_data, aes(x = new_cases_per_100k, y = new_deaths_per_100k)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = 0.8) +
  facet_wrap(~ state) +
  labs(
    title = "New Cases vs Deaths per 100,000 (Until Sep 2023)",
    x = "New Cases per 100,000",
    y = "New Deaths per 100,000"
  ) +
  theme_minimal()

# --------- everything is fine till here------------
# Filter valid data until Sep 30, 2023 and aggregate by year
aggregated_death_data <- full_data %>%
  filter(date < as.Date("2023-10-01")) %>%
  filter(!is.na(new_cases_per_100k), !is.na(new_deaths_per_100k)) %>%
  filter(new_cases_per_100k >= 0, new_deaths_per_100k >= 0) %>%
  group_by(state, year) %>%
  summarise(
    total_cases_per_100k = sum(new_cases_per_100k, na.rm = TRUE),
    total_deaths_per_100k = sum(new_deaths_per_100k, na.rm = TRUE),
    .groups = "drop"
  )


# Fit linear model per state on yearly totals
models_norm <- aggregated_death_data %>%
  group_by(state) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(total_deaths_per_100k ~ total_cases_per_100k, data = .x))) %>%
  select(state, model)

# Extract model stats for interpretation
model_stats <- models_norm %>%
  mutate(
    intercept = map_dbl(model, ~ coef(.x)[1]),
    slope = map_dbl(model, ~ coef(.x)[2]),
    r_squared = map_dbl(model, ~ summary(.x)$r.squared)
  ) %>%
  select(state, intercept, slope, r_squared)

print(model_stats)

# Prepare data for 2024 and 2025 predictions
predict_agg <- full_data %>%
  filter(year %in% c(2024, 2025)) %>%
  filter(!is.na(new_cases_per_100k), new_cases_per_100k >= 0) %>%
  group_by(state, year) %>%
  summarise(
    total_cases_per_100k = sum(new_cases_per_100k, na.rm = TRUE),
    .groups = "drop"
  )

# extract model coefficients
model_coeffs <- aggregated_death_data %>%
  group_by(state) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(total_deaths_per_100k ~ total_cases_per_100k, data = .x)),
    intercept = map_dbl(model, ~ coef(.x)[1]),
    slope = map_dbl(model, ~ coef(.x)[2])
  ) %>%
  select(state, intercept, slope)

# predicting total deaths per 100k using intercept and slope
predicted_summary_norm <- predict_agg %>%
  left_join(model_coeffs, by = "state") %>%
  mutate(
    predicted_total_deaths_per_100k = intercept + slope * total_cases_per_100k,
    population = pop_data[state],
    predicted_total_deaths = round((predicted_total_deaths_per_100k / 100000) * population)
  )

print(predicted_summary_norm)


# Plot predicted deaths
ggplot(predicted_summary_norm, aes(x = factor(year), y = predicted_total_deaths_per_100k, fill = state)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = round(predicted_total_deaths_per_100k, 1)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  scale_fill_manual(values = c(
    "Victoria" = "#177e89",
    "New South Wales" = "#a4b8c4",
    "Queensland" = "#db3a34",
    "South Australia" = "#ffc857"
  )) +
  labs(
    title = "Predicted COVID-19 Deaths per 100,000 (2024–2025)",
    x = "Year",
    y = "Predicted Deaths per 100,000",
    fill = "State"
  ) +
  theme_minimal()
