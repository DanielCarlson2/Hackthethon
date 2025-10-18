#install.packages("bslib")

library(shiny)
library(bslib)
library(dplyr)

source("/cloud/project/User tool/User_tool_math.R")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    tags$style(HTML("
    .boxButton {
      background-color: #440154;
      color: white;
      padding: 20px;
      border-radius: 10px;
      text-align: center;
      cursor: pointer;
      font-size: 18px;
      width: fit-content;
      border: none;
      transition: all 0.3s ease;
    }
    .boxButton:hover {
      background-color: #31688E;
      transform: translateY(-2px);
      box-shadow: 0 4px 8px rgba(68, 1, 84, 0.3);
    }
  ")),
  # Custom Viridis-themed design ----
  theme = bs_theme(
    version = 5,
    primary = "#440154",      # Dark purple (viridis primary)
    secondary = "#31688E",    # Blue (viridis secondary)
    success = "#35B779",      # Green for success states
    info = "#1F9E89",         # Teal for info
    warning = "#FDE725",      # Yellow for warnings
    danger = "#F1605D",       # Red for danger
    light = "#F7F7F7",        # Light background
    dark = "#2D2D2D",         # Dark text
    bg = "#FFFFFF",           # Main background
    fg = "#2D2D2D",           # Main text color
    "font-family-base" = "'Inter', 'Segoe UI', sans-serif",
    "border-radius" = "0.5rem",
    "box-shadow" = "0 0.125rem 0.25rem rgba(68, 1, 84, 0.075)"
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
        p("Welcome to the Data Center GPU Health Dashboard! Follow these steps to analyze your GPU data:"),
        h4("Step 1: Upload Your Data"),
        p("• Click 'Choose a CSV file...' to upload your GPU dataset"),
        p("• Ensure your CSV has columns: GPU ID, GPU Rack, Hour, Average GPU Temp [C], Peak GPU Temp[C], Average GPU Power Usage [W], Peak GPU Power usage [W], Average GPU Memory Usage [%], Peak GPU memory Usage [%]"),
        p("• The app will automatically detect and display your data"),
        
        h4("Step 2: General Analysis"),
        p("• View the histogram at the top to see data distribution with 95% confidence intervals"),
        p("• Data outside 95% confidence will be highlighted in different colors"),
        p("• Check the data preview with updated column titles"),
        p("• Review average statistics for temperature, power, and memory usage"),
        
        h4("Step 3: Advanced Analysis"),
        p("• Use 'By Rack' tab to analyze specific GPU racks"),
        p("• Use 'By GPU' tab for failure detection and threshold analysis"),
        p("• Adjust time periods and sorting options as needed"),
        
        h4("Understanding the Tabs"),
        tags$ul(
          tags$li(tags$strong("General:"), "Histogram analysis with confidence intervals, data preview, and average statistics for key metrics"),
          tags$li(tags$strong("By Rack:"), "Rack-specific analysis with control charts and filtering"),
          tags$li(tags$strong("By GPU:"), "Individual GPU failure detection and threshold monitoring"),
          tags$li(tags$strong("How to Use:"), "This instruction panel")
        ),
        
        h4("Key Features"),
        p("• 95% confidence interval visualization in histograms"),
        p("• Automatic outlier detection with color coding"),
        p("• Focus on average metrics (temperature, power, memory)"),
        p("• Real-time failure detection and alerting"),
        
        h4("Troubleshooting"),
        p("• If plots don't appear, check that you've selected numeric columns"),
        p("• Error messages will guide you if data issues are detected"),
        p("• Make sure your CSV file follows the expected column format")
      )
    ),
    # Tab 1: General Analysis
    tabPanel(
      "General",
      page_sidebar(
        title = "General Analysis",
        sidebar = sidebar(
          # Input: Hour selector ----
          selectInput(
            inputId = "selectedHour",
            label = "Select Time Period:",
            choices = c("All Hours (Global)" = "all", "Hour 1" = "1", "Hour 2" = "2", "Hour 3" = "3", 
                       "Hour 4" = "4", "Hour 5" = "5", "Hour 6" = "6", "Hour 7" = "7", "Hour 8" = "8",
                       "Hour 9" = "9", "Hour 10" = "10", "Hour 11" = "11", "Hour 12" = "12",
                       "Hour 13" = "13", "Hour 14" = "14", "Hour 15" = "15", "Hour 16" = "16",
                       "Hour 17" = "17", "Hour 18" = "18", "Hour 19" = "19", "Hour 20" = "20",
                       "Hour 21" = "21", "Hour 22" = "22", "Hour 23" = "23", "Hour 24" = "24"),
            selected = "all"
          ),
          # Input: Column selector for histogram ----
          selectInput(
            inputId = "histColumn",
            label = "Select Column for Histogram:",
            choices = NULL,
            selected = NULL
          ),
          # Input: Number of data points to show ----
          selectInput(
            inputId = "dataPoints",
            label = "Number of Data Points:",
            choices = c("3" = "3", "5" = "5", "10" = "10", "15" = "15", "20" = "20", "25" = "25", "50" = "50"),
            selected = "3"
          ),
          # Input: Sorting option ----
          selectInput(
            inputId = "sortOption",
            label = "Sort Data By:",
            choices = c("All Hours" = "all_hours", "Time Period" = "time_period", "All Bins" = "all_bins", "Bin Number" = "bin_number"),
            selected = "all_hours"
          )
        ),
        card(
        card_header("Data Distribution"),
        p("Histogram showing the distribution of selected metrics with 95% confidence intervals. Data outside confidence intervals are highlighted in different colors."),
        plotOutput(outputId= "generalHistogram")
      ),
      card(
        card_header("Data Preview"),
        p("Shows data with customizable sorting and number of rows. Use the dropdowns above to control display options."),
        tableOutput("dataPreview")
      ),
      card(
        card_header("Statistics Analysis"),
        p("Statistical analysis for average metrics (temperature, power, memory) for the selected time period. Choose 'All Hours' for global statistics or select a specific hour."),
        verbatimTextOutput("averageStats")
      )
    ),
    ),
    
    # Tab 2: By Rack
    tabPanel(
      "By Rack",
      page_sidebar(
        title = "By Rack Filtering",
        sidebar = sidebar(
          # Input: Time period selector for rack analysis ----
          selectInput(
            inputId = "rackTimePeriod",
            label = "Select Time Period:",
            choices = c("All Hours (Global)" = "all", "Hour 1" = "1", "Hour 2" = "2", "Hour 3" = "3", 
                       "Hour 4" = "4", "Hour 5" = "5", "Hour 6" = "6", "Hour 7" = "7", "Hour 8" = "8",
                       "Hour 9" = "9", "Hour 10" = "10", "Hour 11" = "11", "Hour 12" = "12",
                       "Hour 13" = "13", "Hour 14" = "14", "Hour 15" = "15", "Hour 16" = "16",
                       "Hour 17" = "17", "Hour 18" = "18", "Hour 19" = "19", "Hour 20" = "20",
                       "Hour 21" = "21", "Hour 22" = "22", "Hour 23" = "23", "Hour 24" = "24"),
            selected = "all"
          ),
          numericInput(
            inputId = "rackTempMax",
            label = "Max Temperature (°C):",
            value = 90,
            min = 0,
            max = 200
          ),
          numericInput(
            inputId = "rackMemMax",
            label = "Max Memory Utilization (%):",
            value = 100,
            min = 0,
            max = 100
          ),
          numericInput(
            inputId = "rackMemMin",
            label = "Min Memory Utilization (%):",
            value = 0,
            min = 0,
            max = 100
          ),
          selectInput(
            inputId = "rackMetric",
            label = "Metric to Display:",
            choices = c(
              "Temperature" = "temperature",
              "Memory Utilization" = "memory",
              "Power Usage" = "power"
            ),
            selected = "temperature"
          )
        ),
      card(
        card_header("By Rack Analysis"),
        # 👉 Add dynamic rack buttons here
        uiOutput("rackButtons"),
      ),
       card(
         card_header("By Rack Plot"),
         plotOutput(outputId = "rackPlot")
       ),
       card(
         card_header("Rack Statistics"),
         p("Statistical summary for the selected rack and time period:"),
         verbatimTextOutput("rackStats")
       ),
       card(
         card_header("GPU Data for Selected Rack"),
         p("Detailed data for all GPUs in the selected rack:"),
         tableOutput("rackGPUTable")
       )
     )
    ),

    # Tab 3: By GPU - Failure Detection
    tabPanel(
      "By GPU",
      page_sidebar(
        title = "GPU Failure Detection",
        sidebar = sidebar(
          h4("Failure Thresholds"),
          # Temperature thresholds
          numericInput(
            inputId = "tempMax",
            label = "Temperature Max (°C):",
            value = 85,
            min = 0,
            max = 200
          ),
          numericInput(
            inputId = "tempMin",
            label = "Temperature Min (°C):",
            value = 30,
            min = 0,
            max = 200
          ),
          # Power usage thresholds
          numericInput(
            inputId = "powerMax",
            label = "Power Usage Max (W):",
            value = 250,
            min = 0,
            max = 1000
          ),
          numericInput(
            inputId = "powerMin",
            label = "Power Usage Min (W):",
            value = 50,
            min = 0,
            max = 1000
          ),
          # Memory usage thresholds
          numericInput(
            inputId = "memoryMax",
            label = "Memory Usage Max (%):",
            value = 95,
            min = 0,
            max = 100
          ),
          numericInput(
            inputId = "memoryMin",
            label = "Memory Usage Min (%):",
            value = 10,
            min = 0,
            max = 100
          ),
          # Action button to trigger analysis
          actionButton(
            inputId = "analyzeFailures",
            label = "Analyze GPU Failures",
            class = "btn-primary",
            style = "width: 100%; margin-top: 20px;"
          )
        ),
        # Main content area
        fluidRow(
          column(12,
            card(
              card_header("Failure Trend Analysis"),
              p("This plot shows the number of GPU failures over time periods, with a horizontal line indicating the mean failure count."),
              plotOutput("failureTrendPlot")
            )
          )
        ),
        fluidRow(
          column(12,
            card(
              card_header("GPU Failure Analysis Results"),
              uiOutput("failureResults")
            )
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
    # Create reactive value to store selected rack
    selectedRack <- reactiveVal(NULL)
    
  # Single reactive data source for all tabs
  data <- reactive({
    req(input$csvFile)
    
    # Read the CSV file
    df <- read.csv(input$csvFile$datapath)
    return(df)
  })
 
  
  # Update column choices when data changes (General tab histogram)
  observe({
    req(data())
    df <- data()
    
    # Check if we have at least 4 columns
    if (ncol(df) >= 4) {
      column_names <- names(df)[4:ncol(df)]
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = "histColumn",
        choices = column_names,
        selected = column_names[1]  # Select first column (column 4) by default
      )
    } else {
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = "histColumn",
        choices = "Dataset must have at least 4 columns",
        selected = "Dataset must have at least 4 columns"
      )
    }
  })

  # Display data preview with updated column names
  output$dataPreview <- renderTable({
    req(data())
    
    df <- data()
    
    # Update column names to the specified format
    if (ncol(df) >= 9) {
      colnames(df) <- c(
        "GPU ID",
        "GPU Rack", 
        "Hour",
        "Average GPU Temp [C]",
        "Peak GPU Temp[C]",
        "Average GPU Power Usage [W]",
        "Peak GPU Power usage [W]",
        "Average GPU Memory Usage [%]",
        "Peak GPU memory Usage [%]"
      )
    }
    
    # Apply sorting based on sortOption
    if (input$sortOption == "time_period") {
      # Sort by time period (column 3)
      time_col <- names(df)[3]
      df <- df[order(df[[time_col]]), ]
    } else if (input$sortOption == "bin_number") {
      # Sort by GPU cluster ID (column 2) as bin number
      cluster_col <- names(df)[2]
      df <- df[order(df[[cluster_col]]), ]
    } else if (input$sortOption == "all_bins") {
      # Sort by cluster ID then time period
      cluster_col <- names(df)[2]
      time_col <- names(df)[3]
      df <- df[order(df[[cluster_col]], df[[time_col]]), ]
    }
    # For "all_hours", no additional sorting needed
    
    # Filter data by selected hour if not "all"
    if (input$selectedHour != "all") {
      # Assuming Time_Period is column 3 (index 3)
      time_col <- names(df)[3]
      df <- df[df[[time_col]] == as.numeric(input$selectedHour), ]
    }
    
    # Show the specified number of data points
    num_points <- as.numeric(input$dataPoints)
    head(df, num_points)
  })
  
  
  # Display average statistics for columns 4, 6, and 8 only
  output$averageStats <- renderText({
    req(data())
    
    df <- data()
    
    # Check if we have at least 8 columns
    if (ncol(df) < 8) {
      return("Dataset must have at least 8 columns to show statistics.")
    }
    
    # Filter data by selected hour if not "all"
    if (input$selectedHour != "all") {
      # Assuming Time_Period is column 3 (index 3)
      time_col <- names(df)[3]
      df <- df[df[[time_col]] == as.numeric(input$selectedHour), ]
      
      if (nrow(df) == 0) {
        return(paste("No data found for Hour", input$selectedHour))
      }
    }
    
    # Get specific columns: 4 (Average GPU Temp), 6 (Average GPU Power), 8 (Average GPU Memory)
    cols_to_analyze <- names(df)[c(4, 6, 8)]
    
    # Create header based on selection
    if (input$selectedHour == "all") {
      summary_text <- paste("Average Statistics (All Hours):\n\n")
    } else {
      summary_text <- paste("Average Statistics for Hour", input$selectedHour, ":\n\n")
    }
    
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
            "  Count:", length(col_data_clean), "\n")
        } else {
          summary_text <- paste(summary_text, col, ": No valid numeric data\n\n")
        }
      } else {
        summary_text <- paste(summary_text, col, ": Non-numeric column\n\n")
      }
    }
    
    return(summary_text)
  })
  

  # Rack buttons
output$rackButtons <- renderUI({
  req(data())
  racks <- unique(data()$Rack_ID)
  
  # Define number of columns for the grid
  n_cols <- 4  # adjust as desired
  n_racks <- length(racks)
  n_rows <- ceiling(n_racks / n_cols)
  
  button_list <- lapply(racks, function(rack) {
    actionButton(
      inputId = paste0("rack_", rack),
      label = paste("Rack:", rack),
      class = "boxButton"
    )
  })
  
  # Arrange buttons into rows of columns
  grid <- tagList(
    lapply(1:n_rows, function(row) {
      fluidRow(
        lapply(1:n_cols, function(col) {
          btn_idx <- (row - 1) * n_cols + col
          if (btn_idx <= n_racks) {
            column(width = floor(12 / n_cols), button_list[[btn_idx]])
          } else {
            column(width = floor(12 / n_cols), "")
          }
        })
      )
    })
  )
  grid
})

# Handle dynamic button clicks
observe({
  req(data())
  racks <- unique(data()$Rack_ID)
  
  lapply(racks, function(rack) {
    observeEvent(input[[paste0("rack_", rack)]], {
      # Store which rack is selected in a reactive value
      selectedRack(rack)
    })
  })
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

  # Create reactive plot for selected rack
  output$rackPlot <- renderPlot({
    req(data(), selectedRack(), input$rackMetric)
    
    # Filter data for selected rack
    rack_data <- data()[data()$Rack_ID == selectedRack(), ]
    
    if (nrow(rack_data) == 0) {
      plot.new()
      text(0.5, 0.5, "No data found for selected rack", 
           cex = 1.5, col = "red")
      return()
    }
    
    # Get column names
    col_names <- names(rack_data)
    time_col <- col_names[3]  # Time_Period
    
    # Determine metric column based on selection
    metric_col <- switch(input$rackMetric,
      "temperature" = col_names[4],  # Average_GPU_Temperature
      "memory" = col_names[8],       # Average_GPU_Memory_Usage
      "power" = col_names[6]         # Average_GPU_Power_Usage
    )
    
    # Group by time period and calculate averages
    unique_time_periods <- sort(unique(rack_data[[time_col]]))
    
    # Check if we have valid time periods
    if (length(unique_time_periods) == 0) {
      plot.new()
      text(0.5, 0.5, "No time periods found in data", 
           cex = 1.5, col = "red")
      return()
    }
    
    metric_data <- numeric(length(unique_time_periods))
    
    for (i in seq_along(unique_time_periods)) {
      time_period <- unique_time_periods[i]
      time_data <- rack_data[rack_data[[time_col]] == time_period, ]
      
      # Check if we have data for this time period
      if (nrow(time_data) == 0) {
        metric_data[i] <- NA
      } else {
        metric_data[i] <- mean(time_data[[metric_col]], na.rm = TRUE)
      }
    }
    
    time_periods <- unique_time_periods
    
    if (length(metric_data) == 0 || all(is.na(metric_data))) {
      plot.new()
      text(0.5, 0.5, "No valid data for selected metric", 
           cex = 1.5, col = "red")
      return()
    }
    
    # Calculate control limits
    mean_val <- mean(metric_data, na.rm = TRUE)
    std_val <- sd(metric_data, na.rm = TRUE)
    upper_control <- mean_val + 3 * std_val
    lower_control <- mean_val - 3 * std_val
    
    # Get specification limits from inputs
    if (input$rackMetric == "temperature") {
      spec_upper <- input$rackTempMax
      spec_lower <- NA  # No lower temp limit specified
    } else if (input$rackMetric == "memory") {
      spec_upper <- input$rackMemMax
      spec_lower <- input$rackMemMin
    } else {  # power
      spec_upper <- NA  # No power limits specified in current inputs
      spec_lower <- NA
    }
    
    # Create the plot with error handling
    tryCatch({
      plot(time_periods, metric_data,
           type = "b",
           pch = 19,
           col = "#440154",
           lwd = 2,
           xlab = "Time Period",
           ylab = paste("Average", tools::toTitleCase(input$rackMetric)),
           main = paste("Rack", selectedRack(), "-", tools::toTitleCase(input$rackMetric), "Control Chart"),
           cex.lab = 1.2,
           cex.main = 1.3)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating plot:", e$message), 
           cex = 1.2, col = "red")
      return()
    })
    
    # Add control limits
    abline(h = mean_val, col = "#35B779", lwd = 2, lty = 1)  # Centerline
    abline(h = upper_control, col = "#FF6347", lwd = 2, lty = 2)  # Upper control limit
    abline(h = lower_control, col = "#FF6347", lwd = 2, lty = 2)  # Lower control limit
    
    # Add specification limits if available
    if (!is.na(spec_upper)) {
      abline(h = spec_upper, col = "#FFD700", lwd = 2, lty = 3)  # Upper spec limit
    }
    if (!is.na(spec_lower)) {
      abline(h = spec_lower, col = "#FFD700", lwd = 2, lty = 3)  # Lower spec limit
    }
    
    # Add legend
    legend_items <- c("Data Points", "Centerline", "Control Limits")
    legend_cols <- c("#440154", "#35B779", "#F1605D")
    legend_lty <- c(1, 1, 2)
    
    if (!is.na(spec_upper) || !is.na(spec_lower)) {
      legend_items <- c(legend_items, "Specification Limits")
      legend_cols <- c(legend_cols, "#FFD700")
      legend_lty <- c(legend_lty, 3)
    }
    
    legend("topright", 
           legend = legend_items,
           col = legend_cols,
           lty = legend_lty,
           lwd = 2,
           cex = 0.9)
    
     # Add grid
     grid()
   })
  
  # Rack Statistics
  output$rackStats <- renderText({
    req(data(), selectedRack(), input$rackMetric)
    
    # Filter data for selected rack
    rack_data <- data()[data()$Rack_ID == selectedRack(), ]
    
    # Filter by time period if not "all"
    if (input$rackTimePeriod != "all") {
      time_col <- names(rack_data)[3]  # Time_Period column
      rack_data <- rack_data[rack_data[[time_col]] == as.numeric(input$rackTimePeriod), ]
    }
    
    if (nrow(rack_data) == 0) {
      return("No data found for selected rack and time period.")
    }
    
    # Get column names
    col_names <- names(rack_data)
    time_col <- col_names[3]  # Time_Period
    
    # Determine metric column based on selection
    metric_col <- switch(input$rackMetric,
      "temperature" = col_names[4],  # Average_GPU_Temperature
      "memory" = col_names[8],       # Average_GPU_Memory_Usage
      "power" = col_names[6]         # Average_GPU_Power_Usage
    )
    
    # Get metric data
    metric_data <- rack_data[[metric_col]]
    metric_data <- as.numeric(metric_data[!is.na(metric_data)])
    
    if (length(metric_data) == 0) {
      return("No valid data for selected metric.")
    }
    
    # Create header based on selections
    if (input$rackTimePeriod == "all") {
      header_text <- paste("Rack", selectedRack(), "-", tools::toTitleCase(input$rackMetric), "Statistics (All Hours):\n\n")
    } else {
      header_text <- paste("Rack", selectedRack(), "-", tools::toTitleCase(input$rackMetric), "Statistics (Hour", input$rackTimePeriod, "):\n\n")
    }
    
    # Calculate statistics
    mean_val <- mean(metric_data)
    std_val <- sd(metric_data)
    median_val <- median(metric_data)
    min_val <- min(metric_data)
    max_val <- max(metric_data)
    count_val <- length(metric_data)
    
    # Calculate control limits
    upper_control <- mean_val + 3 * std_val
    lower_control <- mean_val - 3 * std_val
    
    # Create statistics text
    stats_text <- paste(
      header_text,
      "Basic Statistics:\n",
      "  Count:", count_val, "\n",
      "  Mean:", round(mean_val, 2), "\n",
      "  Standard Deviation:", round(std_val, 2), "\n",
      "  Median:", round(median_val, 2), "\n",
      "  Minimum:", round(min_val, 2), "\n",
      "  Maximum:", round(max_val, 2), "\n\n",
      "Control Limits:\n",
      "  Upper Control Limit (UCL):", round(upper_control, 2), "\n",
      "  Lower Control Limit (LCL):", round(lower_control, 2), "\n",
      "  Center Line:", round(mean_val, 2), "\n\n"
    )
    
    # Add specification limits if available
    if (input$rackMetric == "temperature") {
      spec_upper <- input$rackTempMax
      stats_text <- paste(stats_text,
        "Specification Limits:\n",
        "  Upper Spec Limit:", spec_upper, "°C\n\n")
    } else if (input$rackMetric == "memory") {
      spec_upper <- input$rackMemMax
      spec_lower <- input$rackMemMin
      stats_text <- paste(stats_text,
        "Specification Limits:\n",
        "  Upper Spec Limit:", spec_upper, "%\n",
        "  Lower Spec Limit:", spec_lower, "%\n\n")
    }
    
    return(stats_text)
  })
  
  # Rack GPU Data Table
  output$rackGPUTable <- renderTable({
    req(data(), selectedRack())
    
    # Filter data for selected rack
    rack_data <- data()[data()$Rack_ID == selectedRack(), ]
    
    # Filter by time period if not "all"
    if (input$rackTimePeriod != "all") {
      time_col <- names(rack_data)[3]  # Time_Period column
      rack_data <- rack_data[rack_data[[time_col]] == as.numeric(input$rackTimePeriod), ]
    }
    
    if (nrow(rack_data) == 0) {
      return(data.frame(Message = "No data found for selected rack and time period"))
    }
    
    # Sort by GPU ID and Time Period for better organization
    rack_data <- rack_data[order(rack_data[[names(rack_data)[1]]], rack_data[[names(rack_data)[3]]]), ]
    
    # Return the full dataset for the selected rack
    return(rack_data)
  }, 
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  spacing = "xs",
  width = "100%")
  
  # Tab 2: Additional plot (histogram with custom settings)
  output$tab2Plot <- renderPlot({
    req(data(), input$histColumn)
    
    df <- data()
    
    # Check if we have at least 4 columns
    if (ncol(df) < 4) {
      plot.new()
      text(0.5, 0.5, "Dataset must have at least 4 columns", 
           cex = 1.5, col = "red")
      return()
    }
    
    # Check if the selected column is the error message
    if (input$histColumn == "Dataset must have at least 4 columns") {
      plot.new()
      text(0.5, 0.5, "Please upload a dataset with at least 4 columns", 
           cex = 1.5, col = "red")
      return()
    }
    
    x <- df[[input$histColumn]]
    
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
    
    # Create histogram with fixed mint green color and 30 bins
    hist(x, 
         breaks = 30,
         col = "#440154",  # Viridis dark purple
         border = "white",
         xlab = input$histColumn,
         main = paste("Histogram of", input$histColumn),
         ylab = "Frequency")
  })

  # General histogram for the General tab with 95% confidence intervals
  output$generalHistogram <- renderPlot({
    req(data(), input$histColumn)
    
    df <- data()
    
    # Filter data by selected hour if not "all"
    if (input$selectedHour != "all") {
      # Assuming Time_Period is column 3 (index 3)
      time_col <- names(df)[3]
      df <- df[df[[time_col]] == as.numeric(input$selectedHour), ]
    }
    
    # Check if we have at least 4 columns
    if (ncol(df) < 4) {
      plot.new()
      text(0.5, 0.5, "Dataset must have at least 4 columns", 
           cex = 1.5, col = "red")
      return()
    }
    
    # Check if the selected column is the error message
    if (input$histColumn == "Dataset must have at least 4 columns") {
      plot.new()
      text(0.5, 0.5, "Please upload a dataset with at least 4 columns", 
           cex = 1.5, col = "red")
      return()
    }
    
    x <- df[[input$histColumn]]
    
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
    
    # Calculate 95% confidence interval
    mean_val <- mean(x)
    std_val <- sd(x)
    n <- length(x)
    se <- std_val / sqrt(n)
    
    # 95% confidence interval (using t-distribution)
    t_val <- qt(0.975, df = n - 1)
    ci_lower <- mean_val - t_val * se
    ci_upper <- mean_val + t_val * se
    
    # Create title based on hour selection
    if (input$selectedHour == "all") {
      plot_title <- paste("Histogram of", input$histColumn, "(All Hours)")
    } else {
      plot_title <- paste("Histogram of", input$histColumn, "(Hour", input$selectedHour, ")")
    }
    
    # Create histogram with viridis colors
    hist_result <- hist(x, 
         breaks = 30,
         plot = FALSE)
    
    # Determine colors based on 95% confidence interval
    colors <- ifelse(hist_result$mids >= ci_lower & hist_result$mids <= ci_upper, 
                     "#440154",  # Dark purple for within CI
                     "#FDE725")  # Yellow for outside CI
    
    # Plot histogram
    plot(hist_result,
         col = colors,
         border = "white",
         xlab = input$histColumn,
         main = plot_title,
         ylab = "Frequency")
    
    # Add confidence interval lines
    abline(v = ci_lower, col = "#35B779", lwd = 2, lty = 2)
    abline(v = ci_upper, col = "#35B779", lwd = 2, lty = 2)
    abline(v = mean_val, col = "#1F9E89", lwd = 2, lty = 1)
    
    # Add legend
    legend("topright", 
           legend = c("Within 95% CI", "Outside 95% CI", "Mean", "95% CI Bounds"),
           col = c("#440154", "#FDE725", "#1F9E89", "#35B779"),
           lty = c(1, 1, 1, 2),
           lwd = c(2, 2, 2, 2),
           cex = 0.8)
    
    # Add grid
    grid()
  })

  # Failure Trend Plot
  output$failureTrendPlot <- renderPlot({
    req(data())
    
    df <- data()
    
    # Check if we have enough columns
    if (ncol(df) < 8) {
      plot.new()
      text(0.5, 0.5, "Dataset must have at least 8 columns for failure analysis", 
           cex = 1.5, col = "red")
      return()
    }
    
    # Get column names
    col_names <- names(df)
    gpu_id_col <- col_names[1]      # GPU_Unique_ID
    cluster_col <- col_names[2]     # Rack_ID  
    time_col <- col_names[3]       # Time_Period
    temp_col <- col_names[4]       # Average_GPU_Temperature
    power_col <- col_names[6]      # Average_GPU_Power_Usage
    memory_col <- col_names[8]     # Average_GPU_Memory_Usage
    
    # Get unique time periods and sort them
    time_periods <- sort(unique(df[[time_col]]))
    
    # Initialize failure counts for each type and time period
    temp_failures <- numeric(length(time_periods))
    power_failures <- numeric(length(time_periods))
    memory_failures <- numeric(length(time_periods))
    
    # Count failures for each time period
    for (i in seq_along(time_periods)) {
      time_period <- time_periods[i]
      time_data <- df[df[[time_col]] == time_period, ]
      
      temp_count <- 0
      power_count <- 0
      memory_count <- 0
      
      # Check each row in this time period for failures
      for (j in seq_len(nrow(time_data))) {
        row <- time_data[j, ]
        
        # Check temperature failures
        temp_val <- as.numeric(row[[temp_col]])
        if (!is.na(temp_val)) {
          if (temp_val > input$tempMax || temp_val < input$tempMin) {
            temp_count <- temp_count + 1
          }
        }
        
        # Check power usage failures
        power_val <- as.numeric(row[[power_col]])
        if (!is.na(power_val)) {
          if (power_val > input$powerMax || power_val < input$powerMin) {
            power_count <- power_count + 1
          }
        }
        
        # Check memory usage failures
        memory_val <- as.numeric(row[[memory_col]])
        if (!is.na(memory_val)) {
          if (memory_val > input$memoryMax || memory_val < input$memoryMin) {
            memory_count <- memory_count + 1
          }
        }
      }
      
      temp_failures[i] <- temp_count
      power_failures[i] <- power_count
      memory_failures[i] <- memory_count
    }
    
    # Calculate means for each failure type
    mean_temp <- mean(temp_failures)
    mean_power <- mean(power_failures)
    mean_memory <- mean(memory_failures)
    
    # Find the maximum value for y-axis scaling
    max_failures <- max(temp_failures, power_failures, memory_failures)
    
    # Create the plot with proper scaling
    plot(time_periods, temp_failures,
         type = "b",  # Both points and lines
         pch = 19,    # Solid circles
         col = "#F1605D",  # Viridis red for temperature
         lwd = 2,     # Line width
         xlab = "Time Period",
         ylab = "Number of Failures",
         main = "GPU Failures Over Time by Type",
         cex.lab = 1.2,
         cex.main = 1.3,
         ylim = c(0, max_failures * 1.1))  # Add some padding at top
    
    # Add power failures line
    lines(time_periods, power_failures,
          type = "b",
          pch = 17,    # Triangle
          col = "#440154",  # Viridis dark purple for power
          lwd = 2)
    
    # Add memory failures line
    lines(time_periods, memory_failures,
          type = "b",
          pch = 15,    # Square
          col = "#1F9E89",  # Viridis teal for memory
          lwd = 2)
    
    # Add horizontal lines for means
    abline(h = mean_temp, 
           col = "#F1605D",  # Viridis red
           lwd = 1, 
           lty = 2)  # Dashed line
    
    abline(h = mean_power, 
           col = "#440154",  # Viridis dark purple
           lwd = 1, 
           lty = 2)  # Dashed line
    
    abline(h = mean_memory, 
           col = "#1F9E89",  # Viridis teal
           lwd = 1, 
           lty = 2)  # Dashed line
    
    # Add legend
    legend("topright", 
           legend = c("Temperature Failures", 
                     "Power Failures", 
                     "Memory Failures",
                     paste("Temp Mean:", round(mean_temp, 2)),
                     paste("Power Mean:", round(mean_power, 2)),
                     paste("Memory Mean:", round(mean_memory, 2))),
           col = c("#F1605D", "#440154", "#1F9E89", 
                  "#F1605D", "#440154", "#1F9E89"),
           lty = c(1, 1, 1, 2, 2, 2),
           lwd = c(2, 2, 2, 1, 1, 1),
           pch = c(19, 17, 15, NA, NA, NA),
           cex = 0.9)
    
    # Add grid for better readability
    grid()
  })

  # GPU Failure Detection Logic
  output$failureResults <- renderUI({
    req(data(), input$analyzeFailures)
    
    df <- data()
    
    # Check if we have enough columns
    if (ncol(df) < 8) {
      return(
        div(
          class = "alert alert-danger",
          h4("Error"),
          p("Dataset must have at least 8 columns for GPU failure analysis.")
        )
      )
    }
    
    # Get column names (assuming standard structure)
    col_names <- names(df)
    
    # Define column indices based on the dataset structure
    gpu_id_col <- col_names[1]      # GPU_Unique_ID
    cluster_col <- col_names[2]     # Rack_ID  
    time_col <- col_names[3]       # Time_Period
    temp_col <- col_names[4]       # Average_GPU_Temperature
    power_col <- col_names[6]      # Average_GPU_Power_Usage
    memory_col <- col_names[8]     # Average_GPU_Memory_Usage
    
    # Initialize failure tracking
    failures <- data.frame(
      GPU_ID = character(),
      Rack_ID = character(),
      Time_Period = character(),
      Failure_Type = character(),
      Value = numeric(),
      Threshold = numeric(),
      Row_Number = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Check each row for failures
    for (i in seq_len(nrow(df))) {
      row <- df[i, ]
      
      # Check temperature failures
      temp_val <- as.numeric(row[[temp_col]])
      if (!is.na(temp_val)) {
        if (temp_val > input$tempMax) {
          failures <- rbind(failures, data.frame(
            GPU_ID = as.character(row[[gpu_id_col]]),
            Rack_ID = as.character(row[[cluster_col]]),
            Time_Period = as.character(row[[time_col]]),
            Failure_Type = "Temperature Too High",
            Value = temp_val,
            Threshold = input$tempMax,
            Row_Number = i,
            stringsAsFactors = FALSE
          ))
        }
        if (temp_val < input$tempMin) {
          failures <- rbind(failures, data.frame(
            GPU_ID = as.character(row[[gpu_id_col]]),
            Rack_ID = as.character(row[[cluster_col]]),
            Time_Period = as.character(row[[time_col]]),
            Failure_Type = "Temperature Too Low",
            Value = temp_val,
            Threshold = input$tempMin,
            Row_Number = i,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Check power usage failures
      power_val <- as.numeric(row[[power_col]])
      if (!is.na(power_val)) {
        if (power_val > input$powerMax) {
          failures <- rbind(failures, data.frame(
            GPU_ID = as.character(row[[gpu_id_col]]),
            Rack_ID = as.character(row[[cluster_col]]),
            Time_Period = as.character(row[[time_col]]),
            Failure_Type = "Power Usage Too High",
            Value = power_val,
            Threshold = input$powerMax,
            Row_Number = i,
            stringsAsFactors = FALSE
          ))
        }
        if (power_val < input$powerMin) {
          failures <- rbind(failures, data.frame(
            GPU_ID = as.character(row[[gpu_id_col]]),
            Rack_ID = as.character(row[[cluster_col]]),
            Time_Period = as.character(row[[time_col]]),
            Failure_Type = "Power Usage Too Low",
            Value = power_val,
            Threshold = input$powerMin,
            Row_Number = i,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Check memory usage failures
      memory_val <- as.numeric(row[[memory_col]])
      if (!is.na(memory_val)) {
        if (memory_val > input$memoryMax) {
          failures <- rbind(failures, data.frame(
            GPU_ID = as.character(row[[gpu_id_col]]),
            Rack_ID = as.character(row[[cluster_col]]),
            Time_Period = as.character(row[[time_col]]),
            Failure_Type = "Memory Usage Too High",
            Value = memory_val,
            Threshold = input$memoryMax,
            Row_Number = i,
            stringsAsFactors = FALSE
          ))
        }
        if (memory_val < input$memoryMin) {
          failures <- rbind(failures, data.frame(
            GPU_ID = as.character(row[[gpu_id_col]]),
            Rack_ID = as.character(row[[cluster_col]]),
            Time_Period = as.character(row[[time_col]]),
            Failure_Type = "Memory Usage Too Low",
            Value = memory_val,
            Threshold = input$memoryMin,
            Row_Number = i,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    # Create UI based on results
    if (nrow(failures) == 0) {
      return(
        div(
          class = "alert alert-success",
          h4("✅ No Failures Detected"),
          p("All GPUs are operating within the specified thresholds.")
        )
      )
    } else {
      # Create failure boxes
      failure_boxes <- lapply(seq_len(nrow(failures)), function(i) {
        failure <- failures[i, ]
        
        # Determine alert class based on failure type
        alert_class <- if (grepl("Too High", failure$Failure_Type)) {
          "alert-danger"
        } else {
          "alert-warning"
        }
        
        div(
          class = paste("alert", alert_class),
          style = "margin-bottom: 15px;",
          h5(paste("🚨 GPU Failure Detected")),
          p(strong("GPU ID:"), failure$GPU_ID),
          p(strong("Rack:"), failure$Rack_ID),
          p(strong("Time Period:"), failure$Time_Period),
          p(strong("Failure Type:"), failure$Failure_Type),
          p(strong("Value:"), round(failure$Value, 2)),
          p(strong("Threshold:"), round(failure$Threshold, 2)),
          p(strong("Row Number:"), failure$Row_Number)
        )
      })
      
      return(
        div(
          div(
            class = "alert alert-info",
            h4(paste("⚠️", nrow(failures), "Failure(s) Detected")),
            p("The following GPUs have exceeded the specified thresholds:")
          ),
          failure_boxes
        )
      )
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)
