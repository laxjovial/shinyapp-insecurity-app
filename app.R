library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(rsconnect)

# Read the data (make sure to load the data outside the app in your actual code)

library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

# Read the data (make sure to load the data outside the app in your actual code)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
# Read the data (make sure to load the data outside the app in your actual code)
insec <- read.csv("C:/Users/user/Documents/Data Analysis/insecurities", stringsAsFactors = FALSE)

insec <- insec %>%
  mutate(
    deathclass= case_when(
      # Murder-related
      str_detect(Insecurity, regex("shot|stabs?|kill|gunmen|beheaded|stray bullets?|extrajudicial|murder|lynch|mob attack|ritual|organ harvesting|witchcraft", ignore_case = TRUE)) ~ "Murder",
      
      # Suicide
      str_detect(Insecurity, regex("suicide|self harm", ignore_case = TRUE)) ~ "Suicide",
      
      # Terrorism-related
      str_detect(Insecurity, regex("terror|boko haram|bomb|insurgency|militia|herdsmen|bandits?|iswap|fulani|hoodlums?", ignore_case = TRUE)) ~ "Terrorism",
      
      # Road or vehicle-related accidents
      str_detect(Insecurity, regex("crash|auto crash|accident|collision|road mishap|vehicle|truck", ignore_case = TRUE)) ~ "Accident",
      
      # Explosion
      str_detect(Insecurity, regex("explosion|gas explosion|blast", ignore_case = TRUE)) ~ "Explosion",
      
      # Clashes
      str_detect(Insecurity, regex("clash|vs|conflict|chieftaincy|community|land dispute|cultist|cult war|fight", ignore_case = TRUE)) ~ "Clash",
      
      # Security-related incidents
      str_detect(Insecurity, regex("security forces|customs|military|soldier|airstrike|police|navy", ignore_case = TRUE)) ~ "Security Forces Incident",
      
      # Kidnapping
      str_detect(Insecurity, regex("kidnap|abduct|hostage", ignore_case = TRUE)) ~ "Kidnapping",
      
      # Natural disasters
      str_detect(Insecurity, regex("lightning|rainstorm|windstorm|flood|earthquake|landslide|thunder|heat wave", ignore_case = TRUE)) ~ "Natural Disaster",
      
      # Drowning or water-related
      str_detect(Insecurity, regex("drown|well|river|lake|boat mishap|capsize|sink|pool", ignore_case = TRUE)) ~ "Accident",
      
      # Electrocution, fumes, generator
      str_detect(Insecurity, regex("electrocute|fume|generator|electrical|light pole", ignore_case = TRUE)) ~ "Accident",
      
      # Robbery/theft/vandalism
      str_detect(Insecurity, regex("robbery|armed robbers?|thief|snatch|burglary|car snatcher|vandal", ignore_case = TRUE)) ~ "Crime",
      
      # Stampede
      str_detect(Insecurity, regex("stampede", ignore_case = TRUE)) ~ "Stampede",
      
      # Domestic and sexual violence
      str_detect(Insecurity, regex("rape|domestic violence|abuse", ignore_case = TRUE)) ~ "Domestic Violence",
      
      # Ambiguous but death-related
      str_detect(Insecurity, regex("found dead|dies|corpse|dead body|baby found|decomposing", ignore_case = TRUE)) ~ "Murder",
      
      # Students and youth
      str_detect(Insecurity, regex("student|undergraduate|school|nysc", ignore_case = TRUE)) ~ "Accident",
      
      # Default
      TRUE ~ "Other"
    )
  )
# Convert 'Start.date' and 'End.date' to Date format (from d/m/y)
insec$Start_date <- as.Date(insec$Start_date, format = "%d/%m/%Y")
insec$End_date <- as.Date(insec$End_date, format = "%d/%m/%Y")

# Extract Month and Year from 'Start.date' and create a Date object for it (e.g., first day of the month)
insec$month_year <- as.Date(format(insec$Start_date, "%Y-%m-01"))






library(shiny)
ui <- fluidPage(
  titlePanel("Cause of Deaths Over Time"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select State:", choices = unique(insec$State)),
      selectInput("cause", "Select Cause of Death:", choices = unique(insec$deathclass)),
      dateRangeInput("daterange", "Select Date Range:",
                     start = min(insec$Start_date, na.rm = TRUE),
                     end = max(insec$End_date, na.rm = TRUE),
                     format = "yyyy-mm-dd")
    ),
    mainPanel(
      plotOutput("deathPlot")
    )
  )
)







library(shiny)

server <- function(input, output) {
  filtered_data <- reactive({
    insec %>%
      filter(State == input$state,
             deathclass == input$cause,
             Start_date >= input$daterange[1],
             End_date <= input$daterange[2]) %>%
      group_by(month_year) %>%
      summarise(Deaths = sum(Number._of_deaths, na.rm = TRUE)) %>%
      ungroup()
  })
  
  output$deathPlot <- renderPlot({
    plot_data <- filtered_data()
    
    ggplot(plot_data, aes(x = month_year, y = Deaths)) +
      geom_line(color = "steelblue", size = 1.2) +  # Line plot
      geom_point(color = "darkred", size = 2) +  # Add dots if needed, optional
      labs(
        title = paste("Deaths from", input$cause, "in", input$state),
        x = "Month-Year",
        y = "Number of Deaths"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
  })
}
