# clearing the environment
rm(list = ls())

# Load necessary libraries
library(shiny)      
library(ggplot2)    
library(dplyr)      
library(lubridate)  
library(scales)     
library(tidyr)      
library(plotly)     
library(purrr)      

# Load the dataset
full_data <- readRDS("examdata.rds")

# Replace state with full names
state_names <- c(vic = "Victoria", nsw = "New South Wales", qld = "Queensland", sa = "South Australia")
full_data$state <- state_names[full_data$state]

# Define population 
pop_data <- c(
  "New South Wales" = 8545100,
  "Victoria" = 7011100,
  "Queensland" = 5618800,
  "South Australia" = 1891700
)

# Define color palette for all visualizations
state_colors <- c(
  "Victoria" = "#177e89",
  "New South Wales" = "#a4b8c4",
  "Queensland" = "#db3a34",
  "South Australia" = "#ffc857"
)

# Add normalised cases/deaths per 100k and cumulative versions
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

# ---------------- UI code----------------------------

ui <- fluidPage(
  titlePanel("COVID-19 Dashboard Australia"),
  
  sidebarLayout(
    sidebarPanel(
      # User can select states
      checkboxGroupInput("states", "Select States:",
                         choices = unique(full_data$state),
                         selected = unique(full_data$state)),
      # Date range filter
      dateRangeInput("date_range", "Select Date Range:",
                     start = min(full_data$date),
                     end = max(full_data$date),
                     min = min(full_data$date),
                     max = max(full_data$date))
    ),
    
    mainPanel(
      # tabs for all plots
      tabsetPanel(
        tabPanel("Cumulative Cases", plotlyOutput("plot_cases")),
        tabPanel("Cumulative Deaths", plotlyOutput("plot_deaths")),
        tabPanel("Last 24 Months Cases", plotlyOutput("plot_last24")),
        tabPanel("Monthly Boxplot", plotlyOutput("plot_box")),
        tabPanel("Annual Deaths", plotlyOutput("plot_annual_deaths")),
        tabPanel("Scatter: Cases vs Deaths", plotlyOutput("plot_scatter")),
        tabPanel("Predicted Deaths", plotlyOutput("plot_predicted"))
      )
    )
  )
)

# ---------- Server code--------------------

server <- function(input, output) {
  
  # Filter the full dataset reactively based on user selections
  filtered_data <- reactive({
    full_data %>%
      filter(state %in% input$states,
             date >= input$date_range[1],
             date <= input$date_range[2])
  })
  
  # Cumulative Cases Plot
  output$plot_cases <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = date, y = total_cases_per_100k, color = state)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = state_colors) +
      labs(
        title = "Cumulative COVID-19 Cases per 100,000",
        x = "Date", y = "Cases per 100,000"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Cumulative Deaths Plot
  output$plot_deaths <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = date, y = total_deaths_per_100k, color = state)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = state_colors) +
      labs(
        title = "Cumulative COVID-19 Deaths per 100,000",
        x = "Date", y = "Deaths per 100,000"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Cases in Last 24 Months Plot
  output$plot_last24 <- renderPlotly({
    last_24 <- filtered_data() %>%
      filter(date >= as.Date("2023-05-01")) %>%
      filter(!is.na(new_cases_per_100k)) %>%
      group_by(state) %>%
      arrange(date) %>%
      mutate(cumulative_cases_per_100k = cumsum(new_cases_per_100k)) %>%
      ungroup()
    
    p <- ggplot(last_24, aes(x = date, y = cumulative_cases_per_100k, color = state)) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = state_colors) +
      labs(
        title = "Cumulative COVID-19 Cases (Last 24 Months)",
        x = "Date", y = "Cumulative Cases per 100,000"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Monthly Boxplot of New Cases
  output$plot_box <- renderPlotly({
    box_data <- filtered_data() %>%
      filter(date >= as.Date("2023-05-01")) %>%
      mutate(month = floor_date(date, "month")) %>%
      group_by(state, month) %>%
      summarise(new_cases_per_100k = sum(new_cases_per_100k, na.rm = TRUE), .groups = "drop") %>%
      mutate(state_factor = factor(state, levels = names(state_colors)))
    
    p <- ggplot(box_data, aes(x = state_factor, y = new_cases_per_100k, fill = state_factor)) +
      geom_boxplot() +
      scale_fill_manual(values = state_colors) +
      labs(
        title = "Monthly New Cases per 100,000",
        x = "State", y = "Monthly Cases per 100,000"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Annual Deaths Summary Plot
  output$plot_annual_deaths <- renderPlotly({
    annual <- filtered_data() %>%
      filter(date <= as.Date("2023-09-30")) %>%
      filter(!is.na(new_deaths_per_100k)) %>%
      group_by(state, year) %>%
      summarise(total_deaths_per_100k = sum(new_deaths_per_100k, na.rm = TRUE), .groups = "drop") %>%
      mutate(state_factor = factor(state, levels = names(state_colors)))
    
    p <- ggplot(annual, aes(x = factor(year), y = total_deaths_per_100k, fill = state_factor)) +
      geom_col(position = position_dodge()) +
      scale_fill_manual(values = state_colors) +
      geom_text(aes(label = round(total_deaths_per_100k, 1)),
                position = position_dodge(0.9), vjust = -0.5) +
      labs(
        title = "Annual COVID-19 Deaths per 100,000 (till Sep 2023)",
        x = "Year", y = "Deaths per 100,000"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Scatterplot: New Cases vs Deaths per 100k
  output$plot_scatter <- renderPlotly({
    scatter_data <- filtered_data() %>%
      filter(date < as.Date("2023-10-01")) %>%
      filter(!is.na(new_cases_per_100k), !is.na(new_deaths_per_100k)) %>%
      filter(new_cases_per_100k >= 0, new_deaths_per_100k >= 0)
    
    p <- ggplot(scatter_data, aes(x = new_cases_per_100k, y = new_deaths_per_100k)) +
      geom_point(alpha = 0.3, color = "steelblue") +
      geom_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = 0.8) +
      facet_wrap(~ state) +
      labs(
        title = "New Cases vs Deaths per 100,000 (Until Sep 2023)",
        x = "New Cases per 100,000",
        y = "New Deaths per 100,000"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  # Predicted Deaths for 2024–2025 - linear regression
  output$plot_predicted <- renderPlotly({
    agg <- full_data %>%
      filter(date < as.Date("2023-10-01")) %>%
      group_by(state, year) %>%
      summarise(
        total_cases_per_100k = sum(new_cases_per_100k, na.rm = TRUE),
        total_deaths_per_100k = sum(new_deaths_per_100k, na.rm = TRUE),
        .groups = "drop")
    
    models <- agg %>%
      group_by(state) %>%
      nest() %>%
      mutate(
        intercept = map_dbl(data, ~ coef(lm(total_deaths_per_100k ~ total_cases_per_100k, data = .x))[1]),
        slope = map_dbl(data, ~ coef(lm(total_deaths_per_100k ~ total_cases_per_100k, data = .x))[2])
      ) %>% select(state, intercept, slope)
    
    pred_data <- full_data %>%
      filter(year %in% c(2024, 2025)) %>%
      group_by(state, year) %>%
      summarise(total_cases_per_100k = sum(new_cases_per_100k, na.rm = TRUE), .groups = "drop")
    
    pred_combined <- pred_data %>%
      left_join(models, by = "state") %>%
      mutate(
        pred_deaths = intercept + slope * total_cases_per_100k,
        state_factor = factor(state, levels = names(state_colors))
      )
    
    p <- ggplot(pred_combined, aes(x = factor(year), y = pred_deaths, fill = state_factor)) +
      geom_col(position = position_dodge()) +
      scale_fill_manual(values = state_colors) +
      geom_text(aes(label = round(pred_deaths, 1)), position = position_dodge(0.9), vjust = -0.5) +
      labs(
        title = "Predicted Deaths per 100,000 (2024–2025)",
        x = "Year", y = "Predicted Deaths per 100,000"
      ) +
      theme_minimal()
    ggplotly(p)
  })
}

# run the whole thing

shinyApp(ui = ui, server = server)
