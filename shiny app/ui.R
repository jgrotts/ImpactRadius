#ui.r for shiny app
library(shiny)

# Define UI for dataset viewer application
# countries <- sort(as.character(unique(retail$Country)))

shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Summary statistics by country"),

  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    selectInput("Name", "Choose a country:", 
                choices = c("Australia",
                            "Austria",
                            "Bahrain",
                            "Belgium",
                            "Brazil",
                            "Canada",
                            "Channel Islands",
                            "Cyprus",
                            "Czech Republic",
                            "Denmark",
                            "EIRE",
                            "European Community",
                            "Finland",
                            "France",
                            "Germany",
                            "Greece",
                            "Hong Kong",
                            "Iceland",
                            "Israel",
                            "Italy",
                            "Japan",
                            "Lebanon",
                            "Lithuania",
                            "Malta",
                            "Netherlands",
                            "Norway",
                            "Poland",
                            "Portugal",
                            "RSA",
                            "Saudi Arabia",
                            "Singapore",
                            "Spain",
                            "Sweden",
                            "Switzerland",
                            "United Arab Emirates",
                            "United Kingdom",
                            "USA"))
  ),

  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
    mainPanel(
      
      tableOutput("table1"),
      tableOutput("table2"),
      plotOutput("dist_plot")
  )
))
