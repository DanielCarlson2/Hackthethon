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
        
        h4("Step 3: Analyze Your Data"),
        p("• View the data preview to see your uploaded dataset"),
        p("• Check the average statistics for columns 4 and beyond"),
        p("• Use the histogram dropdown to visualize data distribution"),
        p("• Adjust histogram settings (bins, color) as needed"),
        
        h4("Understanding the Tabs"),
        tags$ul(
          tags$li(tags$strong("General:"), "Contains your data preview, average statistics for columns 4+, and histogram analysis"),
          tags$li(tags$strong("By Rack:"), "Currently empty"),
          tags$li(tags$strong("By GPU:"), "Currently empty"),
          tags$li(tags$strong("How to Use:"), "This instruction panel")
        ),
        
        h4("Tips for Best Results"),
        p("• Use CSV files with clear column headers"),
        p("• Ensure numeric columns don't contain text or special characters"),
        p("• The histogram shows data distribution patterns"),
        p("• Statistics are calculated for columns 4 and beyond"),
        
        h4("Troubleshooting"),
        p("• If plots don't appear, check that you've selected numeric columns"),
        p("• Error messages will guide you if data issues are detected"),
        p("• Make sure your CSV file is properly formatted")
      )
    ),
    # Tab 1: General Analysis
    tabPanel(
      "General",
      page_sidebar(
        title = "General Analysis",
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
          card_header("Data Preview"),
          tableOutput("dataPreview")
        ),
        card(
          card_header("Average Statistics (Columns 4+)"),
          verbatimTextOutput("averageStats")
        ),
        card(
          card_header("Histogram"),
          plotOutput(outputId = "generalHistogram")
        )
      )
    ),
    # Tab 2: By Rack (Empty)
    tabPanel(
      "By Rack",
      card(
        card_header("By Rack Analysis"),
        p("This tab is currently empty.")
      )
    ),
    # Tab 3: By GPU (Empty)
    tabPanel(
      "By GPU",
      card(
        card_header("By GPU Analysis"),
        p("This tab is currently empty.")
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
  
  # Display average statistics for columns 4 through end
  output$averageStats <- renderText({
    req(data())
    
    df <- data()
    
    # Check if we have at least 4 columns
    if (ncol(df) < 4) {
      return("Dataset must have at least 4 columns to show statistics.")
    }
    
    # Get columns 4 through end
    cols_to_analyze <- names(df)[4:ncol(df)]
    
    summary_text <- paste("Average Statistics for Columns 4+:\n\n")
    
    for (col in cols_to_analyze) {
      col_data <- df[[col]]
      
      # Check if column is numeric
      if (is.numeric(col_data)) {
        col_data_clean <- col_data[!is.na(col_data)]
        
        if (length(col_data_clean) > 0) {
          summary_text <- paste(summary_text, 
            col, ":\n",
            "  Mean:", round(mean(col_data_clean), 2), "\n",
            "  Standard Deviation:", round(sd(col_data_clean), 2), "\n",
            "  Median:", round(median(col_data_clean), 2), "\n",
            "  Min:", round(min(col_data_clean), 2), "\n",
            "  Max:", round(max(col_data_clean), 2), "\n",
            "  Count:", length(col_data_clean), "\n\n")
        } else {
          summary_text <- paste(summary_text, col, ": No valid numeric data\n\n")
        }
      } else {
        summary_text <- paste(summary_text, col, ": Non-numeric column\n\n")
      }
    }
    
    return(summary_text)
  })
  
  # General tab histogram
  output$generalHistogram <- renderPlot({
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
}


# Run the application
shinyApp(ui = ui, server = server)
