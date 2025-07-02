# clean up global environment
rm(list = ls())

# Load libraries
library(rvest)
library(dplyr)
library(lubridate)
library(janitor)
library(purrr)
library(tidyr)

# Define states and categories
states_list <- c("vic", "nsw", "qld", "sa")

base_url <- "https://covidlive.com.au/report/"
css_map <- c(
  cases = "table.DAILY-CASES",
  deaths = "table.DAILY-DEATHS"
)

# Cleaning functions which we will call later
clean_cases_or_deaths <- function(df, type) {
  df <- df %>%
    clean_names() %>%
    select(date, net) %>%
    filter(!is.na(net) & net != "", !is.na(date)) %>%
    mutate(
      date = dmy(date),
      net = as.integer(gsub(",", "", net))
    ) %>%
    filter(net > 0) %>%
    arrange(date)
  
  if (type == "cases") {
    df %>%
      mutate(
        new_cases = net
        ) %>%
      select(date, new_cases)
  } else {
    df %>%
      mutate(
        new_deaths = net
        ) %>%
      select(date, new_deaths)
  }
}


# Final list for all states
all_data <- list()

# going through each state
for (state in states_list) {
  message("Downloading data for state : ", toupper(state))
  
  urls <- list(
    cases  = paste0(base_url, "daily-cases/", state),
    deaths = paste0(base_url, "daily-deaths/", state)
  )
  
  table_cases  <- read_html(urls$cases)  %>% html_element(css_map["cases"])  %>% html_table(fill = TRUE); Sys.sleep(15)
  table_deaths <- read_html(urls$deaths) %>% html_element(css_map["deaths"]) %>% html_table(fill = TRUE); Sys.sleep(15)
  
  # Clean and tag state
  cleaned <- list(
    cases  = clean_cases_or_deaths(table_cases, "cases")     %>% mutate(state = state),
    deaths = clean_cases_or_deaths(table_deaths, "deaths")   %>% mutate(state = state)
  )
  
  all_data[[state]] <- cleaned
}

# combine all states and types into one table
all_cases  <- map_dfr(all_data, "cases")
all_deaths <- map_dfr(all_data, "deaths")

full_data <- reduce(
  list(all_cases, all_deaths),
  full_join,
  by = c("state", "date")
) %>%
  arrange(state, date)

# Save final data
saveRDS(full_data, file = "examdata.rds")
message(" Final data saved to examdata.rds")

#full_data %>% print(n=100)