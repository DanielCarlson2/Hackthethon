#install.packages("bslib")

library(shiny)
library(bslib)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # Custom theme with orange and purple colors ----
  theme = bs_theme(
    version = 5,
    primary = "#FF6B35",      # Orange primary color
    secondary = "#8B5CF6",    # Purple secondary color
    success = "#10B981",      # Green for success states
    info = "#3B82F6",         # Blue for info
    warning = "#F59E0B",      # Amber for warnings
    danger = "#EF4444",        # Red for danger
    light = "#F8FAFC",        # Light background
    dark = "#1E293B",          # Dark text
    bg = "#FFFFFF",            # Main background
    fg = "#1E293B",            # Main text color
    "font-family-base" = "'Inter', 'Segoe UI', sans-serif"
  ),
  # App title ----
  titlePanel("Data Center GPU Health Dashboard"),
  # Global CSV file input ----
  fluidRow(
    column(12,
      card(
        card_header("Data Upload"),
        fileInput(
          inputId = "csvFile",
          label = "Upload CSV File:",
          accept = c(".csv"),
          placeholder = "Choose a CSV file..."
        )
      )
    )
  ),
  # Main content area with tabs ----
  tabsetPanel(
    id = "mainTabs",
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
    ),
    # Tab 1: General Analysis
    tabPanel(
      "General",
      card(
        card_header("Data Preview"),
        tableOutput("dataPreview")
      )
    ),
    # Tab 2: Additional Analysis
    tabPanel(
      "By Rack",
      page_sidebar(
        title = "Distribution Analysis",
        sidebar = sidebar(
          # Input: Column selector for histogram ----
          selectInput(
            inputId = "histColumn",
            label = "Select Column for Histogram:",
            choices = NULL,
            selected = NULL
          ),
          # Input: Number of bins ----
          numericInput(
            inputId = "bins",
            label = "Number of Bins:",
            value = 100,
            min = 5,
            max = 1000
          ),
          # Input: Color selector ----
          selectInput(
            inputId = "histColor",
            label = "Histogram Color:",
            choices = c("Orange" = "#FF6B35", "Purple" = "#8B5CF6", "Blue" = "#3B82F6", "Green" = "#10B981"),
            selected = "#FF6B35"
          )
        ),
        card(
          card_header("Tab 2 Content"),
          p("This tab shows distribution analysis of your data."),
          plotOutput(outputId = "tab2Plot")
        )
      )
    ),
    # Tab 3: Summary/Reports
    tabPanel(
      "By GPU",
      page_sidebar(
        title = "Summary Statistics",
        sidebar = sidebar(
          # Input: Summary type selector ----
          selectInput(
            inputId = "summaryType",
            label = "Summary Type:",
            choices = c("Basic Statistics", "Data Types", "Missing Values", "All Information"),
            selected = "All Information"
          ),
          # Input: Decimal places ----
          numericInput(
            inputId = "decimalPlaces",
            label = "Decimal Places:",
            value = 2,
            min = 0,
            max = 10
          )
        ),
        card(
          card_header("Tab 3 Content"),
          p("This tab displays comprehensive summary statistics about your dataset."),
          verbatimTextOutput("summaryStats")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Single reactive data source for all tabs
  data <- reactive({
    req(input$csvFile)
    
    # Read the CSV file
    df <- read.csv(input$csvFile$datapath)
    return(df)
  })
 
  
  # Update column choices when data changes (Tab 2)
  observe({
    req(data())
    column_names <- names(data())
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      inputId = "histColumn",
      choices = column_names,
      selected = column_names[1]  # Select first column by default
    )
  })

  # Display data preview
  output$dataPreview <- renderTable({
    req(data())
    head(data(), 5)  # Show all rows
  })
  



  
  # Tab 2: Additional plot (histogram with custom settings)
  output$tab2Plot <- renderPlot({
    req(data(), input$histColumn)
    
    x <- data()[[input$histColumn]]
    
    if (!is.numeric(x)) {
      plot.new()
      text(0.5, 0.5, "Please select a numeric column", 
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
    
    # Create histogram with custom settings
    hist(x, 
         breaks = input$bins,
         col = input$histColor, 
         border = "white",
         xlab = input$histColumn,
         main = paste("Histogram of", input$histColumn),
         ylab = "Frequency")
  })
  





  # Tab 3: Summary statistics
  output$summaryStats <- renderText({
    req(data())
    
    df <- data()
    decimal_places <- input$decimalPlaces
    
    if (input$summaryType == "Basic Statistics") {
      summary_text <- paste(
        "Basic Statistics:\n",
        "Number of rows:", nrow(df), "\n",
        "Number of columns:", ncol(df), "\n\n"
      )
      
      # Add basic stats for numeric columns
      numeric_cols <- sapply(df, is.numeric)
      if (any(numeric_cols)) {
        summary_text <- paste(summary_text, "Numeric Columns Summary:\n")
        for (col in names(df)[numeric_cols]) {
          col_data <- df[[col]][!is.na(df[[col]])]
          if (length(col_data) > 0) {
            summary_text <- paste(summary_text, 
              col, ":\n",
              "  Mean:", round(mean(col_data), decimal_places), "\n",
              "  Median:", round(median(col_data), decimal_places), "\n",
              "  Min:", round(min(col_data), decimal_places), "\n",
              "  Max:", round(max(col_data), decimal_places), "\n\n")
          }
        }
      }
      
    } else if (input$summaryType == "Data Types") {
      summary_text <- paste(
        "Data Types:\n",
        "Column names and types:\n"
      )
      
      for (col in names(df)) {
        summary_text <- paste(summary_text, col, ":", class(df[[col]]), "\n")
      }
      
    } else if (input$summaryType == "Missing Values") {
      summary_text <- paste(
        "Missing Values Analysis:\n"
      )
      
      for (col in names(df)) {
        missing_count <- sum(is.na(df[[col]]))
        missing_pct <- round((missing_count / nrow(df)) * 100, decimal_places)
        summary_text <- paste(summary_text, 
          col, ": ", missing_count, " missing (", missing_pct, "%)\n")
      }
      
    } else { # All Information
      summary_text <- paste(
        "Complete Dataset Summary:\n",
        "Number of rows:", nrow(df), "\n",
        "Number of columns:", ncol(df), "\n\n",
        "Column names:\n",
        paste(names(df), collapse = ", "), "\n\n",
        "Data types:\n"
      )
      
      for (col in names(df)) {
        summary_text <- paste(summary_text, col, ":", class(df[[col]]), "\n")
      }
      
      summary_text <- paste(summary_text, "\nMissing Values:\n")
      for (col in names(df)) {
        missing_count <- sum(is.na(df[[col]]))
        missing_pct <- round((missing_count / nrow(df)) * 100, decimal_places)
        summary_text <- paste(summary_text, 
          col, ": ", missing_count, " missing (", missing_pct, "%)\n")
      }
    }
    
    return(summary_text)
  })
}


# Run the application
shinyApp(ui = ui, server = server)
