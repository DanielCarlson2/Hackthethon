#install.packages("bslib")

library(shiny)
library(bslib)

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  # App title ----
  title = "Data Center GPU Health Dashboard",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    # Input: File upload for CSV ----
    fileInput(
      inputId = "csvFile",
      label = "Upload CSV File:",
      accept = c(".csv"),
      placeholder = "Choose a CSV file..."
    ),
    # Input: Slider for Maximum Temperature ----
    numericInput(
      inputId = "maxTemperature",
      label = "Maximum Temperature (Degrees Celsius):",
      value = 80,
      min = 0,
      max = 200
    ),
    # Input: Slider for Desired Temperature----
    numericInput(
      inputId = "desiredTemperature",
      label = "Desired Temperature (Degrees Celsius):",
      value = 70,
      min = 0,
      max = 200
    ),
    # Input: X-axis column selector ----
    selectInput(
      inputId = "xColumn",
      label = "Select X-axis Column:",
      choices = NULL,
      selected = NULL
    ),
    # Input: Y-axis column selector ----
    selectInput(
      inputId = "yColumn",
      label = "Select Y-axis Column:",
      choices = NULL,
      selected = NULL
    )
  ),
  # Main content area ----
  card(
    card_header("Data Preview"),
    tableOutput("dataPreview")
  ),
  card(
    card_header("Scatter Plot"),
    plotOutput(outputId = "scatterPlot")
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Reactive data from uploaded CSV
  data <- reactive({
    req(input$csvFile)
    
    # Read the CSV file
    df <- read.csv(input$csvFile$datapath)
    return(df)
  })
  
  # Update column choices when data changes
  observe({
    req(data())
    column_names <- names(data())
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      inputId = "xColumn",
      choices = column_names,
      selected = column_names[1]  # Select first column by default
    )
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      inputId = "yColumn",
      choices = column_names,
      selected = column_names[min(2, length(column_names))]  # Select second column by default, or first if only one column
    )
  })
  # Display data preview
  output$dataPreview <- renderTable({
    req(data())
    head(data(), 5)  # Show all rows
  })
  
  # Scatter plot of the uploaded data ----
  output$scatterPlot <- renderPlot({
    req(data(), input$xColumn, input$yColumn)
    
    # Get the selected column data
    x <- data()[[input$xColumn]]
    y <- data()[[input$yColumn]]
    
    # Check if both columns are numeric
    if (!is.numeric(x) || !is.numeric(y)) {
      plot.new()
      text(0.5, 0.5, "Please select numeric columns for both X and Y axes", 
           cex = 1.5, col = "red")
      return()
    }
    
    # Remove any NA values
    valid_data <- !is.na(x) & !is.na(y)
    x <- x[valid_data]
    y <- y[valid_data]
    
    if (length(x) == 0) {
      plot.new()
      text(0.5, 0.5, "No valid numeric data found", 
           cex = 1.5, col = "red")
      return()
    }
    
    # Create scatter plot
    plot(x, y, 
         col = "#007bc2", 
         pch = 19,
         xlab = input$xColumn,
         ylab = input$yColumn,
         main = paste("Scatter Plot:", input$xColumn, "vs", input$yColumn))
    
    # Add a trend line
    if (length(x) > 1) {
      abline(lm(y ~ x), col = "red", lwd = 2)
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)
