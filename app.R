# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(rsconnect)

# Source the functions file
source("R/functions.R")

# Define the UI
ui <- fluidPage(
  titlePanel("Cause of Deaths Over Time"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      selectizeInput("state", "Select State(s):",
                     choices = NULL,
                     multiple = TRUE),
      selectizeInput("cause", "Select Cause(s) of Death:",
                     choices = NULL,
                     multiple = TRUE),
      dateRangeInput("daterange", "Select Date Range:",
                     start = NULL,
                     end = NULL,
                     format = "yyyy-mm-dd")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Time Series Analysis", plotOutput("deathPlot")),
        tabPanel("Univariate Analysis",
                 h4("Summary Statistics"),
                 verbatimTextOutput("summary"),
                 hr(),
                 h4("Deaths by Cause"),
                 plotOutput("deathsByCause"),
                 hr(),
                 h4("Deaths by State"),
                 plotOutput("deathsByState")),
        tabPanel("Bivariate Analysis",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("bivariate_state", "Select State:", choices = NULL),
                     selectInput("bivariate_cause", "Select Cause:", choices = NULL)
                   ),
                   mainPanel(
                     h4("Top Causes of Death in Selected State"),
                     plotOutput("topCausesInState"),
                     hr(),
                     h4("Top States for Selected Cause of Death"),
                     plotOutput("topStatesForCause"),
                     hr(),
                     h4("Heatmap of Deaths by State and Cause"),
                     plotOutput("heatmap")
                   )
                 ))
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Reactive expression to load data from file or use default
  data <- reactive({
    if (is.null(input$file1)) {
      # Load default data if no file is uploaded
      load_and_clean_data("Cleaned_Insecurity_Data.csv")
    } else {
      # Load uploaded data
      load_and_clean_data(input$file1$datapath)
    }
  })
  
  # Reactive expression to filter the data based on user selections
  filtered_main_data <- reactive({
    df <- data()
    
    # Filter by state
    if (!is.null(input$state) && !("All" %in% input$state)) {
      df <- df %>% filter(State %in% input$state)
    }
    
    # Filter by cause
    if (!is.null(input$cause) && !("All" %in% input$cause)) {
      df <- df %>% filter(deathclass %in% input$cause)
    }
    
    # Filter by date range
    if (!is.null(input$daterange)) {
      df <- df %>%
        filter(Start_date >= input$daterange[1] & Start_date <= input$daterange[2])
    }
    
    df
  })
  
  # Update UI elements based on loaded data
  observe({
    df <- data()
    updateSelectizeInput(session, "state", choices = c("All", sort(unique(df$State))), selected = "All")
    updateSelectizeInput(session, "cause", choices = c("All", sort(unique(df$deathclass))), selected = "All")
    updateDateRangeInput(session, "daterange",
                         start = min(df$Start_date, na.rm = TRUE),
                         end = max(df$End_date, na.rm = TRUE))
    updateSelectInput(session, "bivariate_state", choices = sort(unique(df$State)))
    updateSelectInput(session, "bivariate_cause", choices = sort(unique(df$deathclass)))
  })
  
  # Time Series Plot
  output$deathPlot <- renderPlot({
    plot_data <- filtered_main_data() %>%
      group_by(month_year, deathclass) %>%
      summarise(Deaths = sum(Number.._of_deaths, na.rm = TRUE)) %>%
      ungroup()
    
    title_text <- "Deaths Over Time"
    if (!is.null(input$state) && !("All" %in% input$state)) {
      title_text <- paste(title_text, "in", paste(input$state, collapse = ", "))
    }
    if (!is.null(input$cause) && !("All" %in% input$cause)) {
      title_text <- paste(title_text, "from", paste(input$cause, collapse = ", "))
    }
    
    ggplot(plot_data, aes(x = month_year, y = Deaths, color = deathclass, group = deathclass)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = title_text,
        x = "Month-Year",
        y = "Number of Deaths"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
  })
  
  # Univariate Analysis
  output$summary <- renderPrint({
    summary(filtered_main_data())
  })
  
  output$deathsByCause <- renderPlot({
    cause_data <- filtered_main_data() %>%
      group_by(deathclass) %>%
      summarise(TotalDeaths = sum(Number.._of_deaths, na.rm = TRUE))
    
    ggplot(cause_data, aes(x = reorder(deathclass, -TotalDeaths), y = TotalDeaths)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Total Deaths by Cause", x = "Cause of Death", y = "Total Deaths") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$deathsByState <- renderPlot({
    state_data <- filtered_main_data() %>%
      group_by(State) %>%
      summarise(TotalDeaths = sum(Number.._of_deaths, na.rm = TRUE))
    
    ggplot(state_data, aes(x = reorder(State, -TotalDeaths), y = TotalDeaths)) +
      geom_bar(stat = "identity", fill = "lightgreen") +
      labs(title = "Total Deaths by State", x = "State", y = "Total Deaths") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  })
  
  # Bivariate Analysis
  output$topCausesInState <- renderPlot({
    req(input$bivariate_state)
    state_cause_data <- data() %>%
      filter(State == input$bivariate_state) %>%
      group_by(deathclass) %>%
      summarise(TotalDeaths = sum(Number.._of_deaths, na.rm = TRUE)) %>%
      arrange(desc(TotalDeaths)) %>%
      top_n(10, TotalDeaths)
    
    ggplot(state_cause_data, aes(x = reorder(deathclass, -TotalDeaths), y = TotalDeaths)) +
      geom_bar(stat = "identity", fill = "salmon") +
      labs(title = paste("Top Causes of Death in", input$bivariate_state), x = "Cause of Death", y = "Total Deaths") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$topStatesForCause <- renderPlot({
    req(input$bivariate_cause)
    cause_state_data <- data() %>%
      filter(deathclass == input$bivariate_cause) %>%
      group_by(State) %>%
      summarise(TotalDeaths = sum(Number.._of_deaths, na.rm = TRUE)) %>%
      arrange(desc(TotalDeaths)) %>%
      top_n(10, TotalDeaths)
    
    ggplot(cause_state_data, aes(x = reorder(State, -TotalDeaths), y = TotalDeaths)) +
      geom_bar(stat = "identity", fill = "gold") +
      labs(title = paste("Top States for", input$bivariate_cause), x = "State", y = "Total Deaths") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$heatmap <- renderPlot({
    heatmap_data <- data() %>%
      group_by(State, deathclass) %>%
      summarise(TotalDeaths = sum(Number.._of_deaths, na.rm = TRUE)) %>%
      ungroup()
    
    ggplot(heatmap_data, aes(x = State, y = deathclass, fill = TotalDeaths)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +
      labs(title = "Heatmap of Deaths by State and Cause", x = "State", y = "Cause of Death") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
