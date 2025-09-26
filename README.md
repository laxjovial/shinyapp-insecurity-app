Insecurity Data Analysis App
This is an R Shiny application designed to visualize and analyze insecurity data, focusing on cause of deaths, by state and over time.

File Structure
Cleaned_Insecurity_Data.csv: The primary data file.

R/functions.R: A script containing the load_and_clean_data function for data preparation.

app.R: The main Shiny application script that defines the user interface and server logic.

README.md: This file.

How to Use the App
Run the Application: Open app.R in RStudio and click "Run App".

Upload Your Data: The app will automatically load Cleaned_Insecurity_Data.csv by default. You can also upload your own CSV file using the "Upload CSV File" button.

Filter the Data: Use the sidebar controls to filter the data by:

State: Select one or more states to analyze.

Cause of Death: Choose specific causes of death.

Date Range: Use the date picker to narrow down the time period.

Explore the Tabs:

Time Series Analysis: Shows how deaths have changed over time.

Univariate Analysis: Provides a summary of the data and plots showing total deaths by cause and state.

Bivariate Analysis: Allows you to explore the relationship between cause of death and state with additional, independent filters.

## 🌐 Live App

[Click here to view the Shiny app](<https://osarietinelaho.shinyapps.io/insecurities/>)

To run locally, do this on R:
shiny::runGitHub("shinyapp-insecurity-app", "laxjovial")
