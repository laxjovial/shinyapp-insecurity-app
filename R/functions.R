# Load necessary libraries
library(dplyr)
library(stringr)
library(lubridate)

#' Load and Clean Insecurity Data
#'
#' This function reads a CSV file, cleans column names,
#' classifies incidents based on keywords, and converts
#' date columns to the correct format.
#'
#' @param filepath A character string for the path to the CSV file.
#' @return A cleaned and prepared data frame.
load_and_clean_data <- function(filepath) {
  # Read the data from the specified filepath
  insec <- read.csv(filepath, stringsAsFactors = FALSE)

  # Explicitly rename columns to more R-friendly names before cleaning
  if ("Start.date" %in% names(insec) && "End.date" %in% names(insec)) {
    names(insec)[names(insec) == "Start.date"] <- "Start_date"
    names(insec)[names(insec) == "End.date"] <- "End_date"
  }
  
  # Clean column names
  names(insec) <- make.names(names(insec), unique = TRUE)

  # The rest of the data cleaning and preparation logic
  insec <- insec %>%
    mutate(
      deathclass = case_when(
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

  # Convert 'Start_date' and 'End_date' to Date format
  insec$Start_date <- as.Date(insec$Start_date, format = "%d/%m/%Y")
  insec$End_date <- as.Date(insec$End_date, format = "%d/%m/%Y")

  # Extract Month and Year from 'Start_date'
  insec$month_year <- as.Date(format(insec$Start_date, "%Y-%m-01"))

  return(insec)
}
