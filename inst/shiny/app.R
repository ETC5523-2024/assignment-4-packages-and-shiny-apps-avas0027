library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)

# Load the data
# Adjusted to match your package structure if this code is running in a package
data("nyc_data", package = "Shiny)")

# Convert DATE column to Date type for filtering
nyc_data$DATE <- as.Date(nyc_data$DATE, format = "%m/%d/%Y")

ui <- fluidPage(
  titlePanel("NYC Vehicle Collisions Explorer"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Date Range:",
                     start = min(nyc_data$DATE, na.rm = TRUE),
                     end = max(nyc_data$DATE, na.rm = TRUE)),
      selectInput("borough", "Select Borough:",
                  choices = unique(na.omit(nyc_data$BOROUGH)),
                  selected = "BROOKLYN")
    ),
    mainPanel(
      plotOutput("collision_plot"),
      tableOutput("collision_table")
    )
  )
)

server <- function(input, output) {
  # Reactive data filtered by user input
  filtered_data <- reactive({
    nyc_data %>%
      filter(DATE >= input$date_range[1], DATE <= input$date_range[2]) %>%
      filter(BOROUGH == input$borough)
  })

  # Render a histogram of collisions over time
  output$collision_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = DATE)) +
      geom_histogram(binwidth = 30, fill = "blue", color = "white") +
      labs(title = "Number of Collisions Over Time", x = "Date", y = "Collision Count")
  })

  # Display a sample of the filtered dataset
  output$collision_table <- renderTable({
    # Adjusted column names to use backticks if spaces exist
    head(filtered_data()[, c("DATE", "BOROUGH", "VEHICLE.1.TYPE", "VEHICLE.2.TYPE")])
  })
}

# Launch the Shiny app
shinyApp(ui, server)

#' Filter NYC Collision Data by Date Range and Borough
#'
#' @param data The NYC collisions dataset.
#' @param start_date The start date as "YYYY-MM-DD".
#' @param end_date The end date as "YYYY-MM-DD".
#' @param borough The borough to filter by.
#' @return A filtered dataframe.
#' @export
filter_collisions <- function(data, start_date, end_date, borough) {
  data %>%
    filter(DATE >= as.Date(start_date) & DATE <= as.Date(end_date)) %>%
    filter(BOROUGH == borough)
}





