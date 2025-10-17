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
    # Input: Input box for Maximum Temperature ----
    numericInput(
      inputId = "maxTemperature",
      label = "Maximum Temperature (Degrees Celsius):",
      value = 80
    ),
    # Input: Input box for Desired Temperature----
    numericInput(
      inputId = "desiredTemperature",
      label = "Desired Temperature (Degrees Celsius):",
      value = 70
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
  # Main content area with tabs ----
  tabsetPanel(
    id = "mainTabs",
    # Tab 1: Data Analysis
    tabPanel(
      "Tab 1",
      card(
        card_header("Data Preview"),
        tableOutput("dataPreview")
      ),
      card(
        card_header("Scatter Plot"),
        plotOutput(outputId = "scatterPlot")
      )
    ),
    # Tab 2: Additional Analysis
    tabPanel(
      "Tab 2",
      card(
        card_header("Tab 2 Content"),
        p("This is Tab 2. You can add additional analysis or visualizations here."),
        plotOutput(outputId = "tab2Plot")
      )
    ),
    # Tab 3: Summary/Reports
    tabPanel(
      "Tab 3",
      card(
        card_header("Tab 3 Content"),
        p("This is Tab 3. You can add summary statistics or reports here."),
        verbatimTextOutput("summaryStats")
      )
    ),
    # Tab 4: How to Use
    tabPanel(
      "How to Use",
      card(
        card_header("Instructions"),
        h4("Getting Started"),
        p("Welcome to the Data Center GPU Health Dashboard! Follow these steps to analyze your data:"),
        
        h4("Step 1: Upload Your Data"),
        p("• Click 'Choose a CSV file...' to upload your dataset"),
        p("• Make sure your CSV file contains numeric columns for analysis"),
        p("• The app will automatically detect and display your data"),
        
        h4("Step 2: Set Temperature Thresholds"),
        p("• Enter your maximum temperature threshold (default: 80°C)"),
        p("• Enter your desired temperature threshold (default: 70°C)"),
        p("• These values help monitor GPU health and performance"),
        
        h4("Step 3: Select Analysis Columns"),
        p("• Choose a column for the X-axis of your scatter plot"),
        p("• Choose a column for the Y-axis of your scatter plot"),
        p("• Both columns must contain numeric data"),
        
        h4("Understanding the Tabs"),
        tags$ul(
          tags$li(tags$strong("Tab 1:"), "Contains your data preview and scatter plot analysis"),
          tags$li(tags$strong("Tab 2:"), "Shows a histogram of your X-axis data distribution"),
          tags$li(tags$strong("Tab 3:"), "Displays summary statistics about your dataset"),
          tags$li(tags$strong("How to Use:"), "This instruction panel")
        ),
        
        h4("Tips for Best Results"),
        p("• Use CSV files with clear column headers"),
        p("• Ensure numeric columns don't contain text or special characters"),
        p("• The scatter plot includes a trend line to show correlations"),
        p("• Check Tab 3 for data quality information"),
        
        h4("Troubleshooting"),
        p("• If plots don't appear, check that you've selected numeric columns"),
        p("• Error messages will guide you if data issues are detected"),
        p("• Make sure your CSV file is properly formatted")
      )
    )
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



  
  # Tab 2: Additional plot (example: histogram of X-axis data)
  output$tab2Plot <- renderPlot({
    req(data(), input$xColumn)
    
    x <- data()[[input$xColumn]]
    
    if (!is.numeric(x)) {
      plot.new()
      text(0.5, 0.5, "Please select a numeric column for X-axis", 
           cex = 1.5, col = "red")
      return()
    }
    
    x <- x[!is.na(x)]
    
    if (length(x) == 0) {
      plot.new()
      text(0.5, 0.5, "No valid numeric data found", 
           cex = 1.5, col = "red")
      return()
    }
    
    # Create histogram
    hist(x, col = "#007bc2", border = "white",
         xlab = input$xColumn,
         main = paste("Histogram of", input$xColumn))
  })
  





  # Tab 3: Summary statistics
  output$summaryStats <- renderText({
    req(data())
    
    df <- data()
    summary_text <- paste(
      "Dataset Summary:\n",
      "Number of rows:", nrow(df), "\n",
      "Number of columns:", ncol(df), "\n\n",
      "Column names:\n",
      paste(names(df), collapse = ", "), "\n\n",
      "Data types:\n"
    )
    
    for (col in names(df)) {
      summary_text <- paste(summary_text, col, ":", class(df[[col]]), "\n")
    }
    
    return(summary_text)
  })
}


# Run the application
shinyApp(ui = ui, server = server)
