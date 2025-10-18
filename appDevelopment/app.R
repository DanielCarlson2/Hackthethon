
#install.packages("DT")

library(shiny)
library(bslib)
library(dplyr)
library(DT)


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
      transform: translateY(-2px);
      box-shadow: 0 4px 8px rgba(68, 1, 84, 0.3);
    }
    .boxButton.selected {
      border: 4px solid #FFFFFF !important;
      box-shadow: 0 0 0 2px #2D2D2D, 0 6px 12px rgba(0, 0, 0, 0.3) !important;
      transform: translateY(-2px);
      position: relative;
    }
    .boxButton.selected:hover {
      transform: translateY(-3px);
      box-shadow: 0 0 0 2px #2D2D2D, 0 8px 16px rgba(0, 0, 0, 0.4) !important;
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
          placeholder = "Click 'Browse' and select CSV..."
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
        
        h4("Step 2: Select Analysis Tab"),
        p("• Choose the appropriate tab for your analysis needs:"),
        tags$ul(
          tags$li(tags$strong("General:"), "View comprehensive statistics and data preview with customizable sorting and filtering"),
          tags$li(tags$strong("By Rack:"), "Analyze specific GPU racks with Ppk (Process Performance Index) and control charts"),
          tags$li(tags$strong("By GPU:"), "Detect GPU failures and monitor individual GPU performance against user identified thresholds")
        ),
        
        h4("Step 3: Configure Analysis Settings"),
        p("• Use the left sidebar to input your desired values:"),
        p("• Adjust time periods, thresholds, and filtering options"),
        p("• Click buttons or run analysis to view results"),
        
        h4("CSV File Organization"),
        p("For optimal results, organize your CSV file with columns in the following order:"),
        tags$ol(
          tags$li("GPU ID - Unique identifier for each GPU"),
          tags$li("GPU Rack - Rack location/identifier"),
          tags$li("Hour - Time period (1-24)"),
          tags$li("Average GPU Temp [C] - Average temperature in Celsius"),
          tags$li("Peak GPU Temp[C] - Peak temperature in Celsius"),
          tags$li("Average GPU Power Usage [W] - Average power consumption in Watts"),
          tags$li("Peak GPU Power usage [W] - Peak power consumption in Watts"),
          tags$li("Average GPU Memory Usage [%] - Average memory utilization percentage"),
          tags$li("Peak GPU memory Usage [%] - Peak memory utilization percentage")
        ),
        p("Ensure your data is numeric where appropriate and that column headers match the expected format for best results."),
        
        h4("Troubleshooting"),
        p("• If plots don't appear, check that you've selected numeric columns"),
        p("• Error messages will guide you if data issues are detected"),
        p("• Make sure your CSV file follows the expected column format"),
        
        h4("Authors"),
        p("This Data Center GPU Health Dashboard was developed by:"),
        tags$ul(
          tags$li("Daniel Carlson, M.Eng Aerospace Engineering, Cornell University"),
          tags$li("René Umeh, M.S. Mechanical Engineering, Cornell University"),
          tags$li("Nevin Motto, M.Eng Mechanical Engineering, Cornell University"),
          tags$li("Evelyne Morisseau, Ph.D. Mechanical Engineering, Cornell University"),
          tags$li("Charlie Gagliardo, M.Eng Mechanical Engineering, Cornell University")
        )
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
            choices = c("Time Period" = "time_period", "GPU Rack" = "gpu_rack", "GPU ID" = "gpu_id"),
            selected = "time_period"
          )
        ),
        # Helper Instructions
        div(
          style = "margin-bottom: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 5px; border-left: 4px solid #2196F3;",
          h6("📋 Instructions:", style = "margin-bottom: 8px; font-weight: bold; color: #1976D2;"),
          p("💡 Adjust the time period, data points, and sorting options on the left to customize your analysis.", 
            style = "margin: 0; font-size: 14px; color: #424242;")
        ),
        # Statistics Cards - Three side by side (moved to top)
        fluidRow(
          column(4,
            card(
              card_header("🌡️ Temperature Statistics", class = "bg-primary text-white"),
              p("Average GPU Temperature analysis (°C)"),
              verbatimTextOutput("tempStats"),
              class = "h-100"
            )
          ),
          column(4,
            card(
              card_header("⚡ Power Usage Statistics", class = "bg-success text-white"),
              p("Average GPU Power Usage analysis (W)"),
              verbatimTextOutput("powerStats"),
              class = "h-100"
            )
          ),
          column(4,
            card(
              card_header("💾 Memory Usage Statistics", class = "bg-info text-white"),
              p("Average GPU Memory Usage analysis (%)"),
              verbatimTextOutput("memoryStats"),
              class = "h-100"
            )
          )
        ),
        card(
          card_header("Data Preview"),
          p("Shows data with customizable sorting and number of rows. Use the dropdowns above to control display options."),
          tableOutput("dataPreview")
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
            value = 100,
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
          numericInput(
            inputId = "rackPowerMax",
            label = "Max Power Usage (W):",
            value = 300,
            min = 0,
            max = 1000
          ),
          numericInput(
            inputId = "rackPowerMin",
            label = "Min Power Usage (W):",
            value = 50,
            min = 0,
            max = 1000
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
        # Helper Instructions
        div(
          style = "margin-bottom: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 5px; border-left: 4px solid #2196F3;",
          h6("📋 Instructions:", style = "margin-bottom: 8px; font-weight: bold; color: #1976D2;"),
          p("💡 Input your desired values on the left and select a specific rack below for analysis.", 
            style = "margin: 0; font-size: 14px; color: #424242;"),
          p("📊 Ppk (Process Performance Index) measures how well your rack performs relative to specification limits. Higher Ppk values indicate better process capability and fewer quality issues.", 
            style = "margin: 5px 0 0 0; font-size: 13px; color: #666; font-style: italic;")
        ),
        # Ppk Color Legend
        div(
          style = "margin-bottom: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
          h6("Ppk Color Legend:", style = "margin-bottom: 10px; font-weight: bold;"),
          div(style = "display: flex; flex-wrap: wrap; gap: 10px;",
            span(style = "background-color: #35B779; color: white; padding: 5px 10px; border-radius: 3px; font-size: 12px;", "🟢 Ppk ≥ 1.67 (Excellent)"),
            span(style = "background-color: #1F9E89; color: white; padding: 5px 10px; border-radius: 3px; font-size: 12px;", "🔵 Ppk ≥ 1.33 (Good)"),
            span(style = "background-color: #DAA520; color: white; padding: 5px 10px; border-radius: 3px; font-size: 12px;", "🟡 Ppk ≥ 1.0 (Acceptable)"),
            span(style = "background-color: #F1605D; color: white; padding: 5px 10px; border-radius: 3px; font-size: 12px;", "🔴 Ppk ≥ 0.67 (Poor)"),
            span(style = "background-color: #8B0000; color: white; padding: 5px 10px; border-radius: 3px; font-size: 12px;", "🔴 Ppk < 0.67 (Very Poor)")
          )
        ),
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
         dataTableOutput("rackGPUTable")
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
            label = "Max Temperature (°C):",
            value = 85,
            min = 0,
            max = 200
          ),
          numericInput(
            inputId = "tempMin",
            label = "Min Temperature (°C):",
            value = 30,
            min = 0,
            max = 200
          ),
          # Power usage thresholds
          numericInput(
            inputId = "powerMax",
            label = "Max Power Usage (W):",
            value = 250,
            min = 0,
            max = 1000
          ),
          numericInput(
            inputId = "powerMin",
            label = "Min Power Usage (W):",
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
        # Helper Instructions
        div(
          style = "margin-bottom: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 5px; border-left: 4px solid #2196F3;",
          h6("📋 Instructions:", style = "margin-bottom: 8px; font-weight: bold; color: #1976D2;"),
          p("💡 Set your failure thresholds on the left and click 'Analyze GPU Failures' to detect problematic GPUs.", 
            style = "margin: 0; font-size: 14px; color: #424242;")
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
    } else if (input$sortOption == "gpu_rack") {
      # Sort by GPU rack (column 2)
      rack_col <- names(df)[2]
      df <- df[order(df[[rack_col]]), ]
    } else if (input$sortOption == "gpu_id") {
      # Sort by GPU ID (column 1)
      gpu_id_col <- names(df)[1]
      df <- df[order(df[[gpu_id_col]]), ]
    }
    
    # Filter data by selected hour if not "all"
    if (input$selectedHour != "all") {
      # Assuming Time_Period is column 3 (index 3)
      time_col <- names(df)[3]
      df <- df[df[[time_col]] == as.numeric(input$selectedHour), ]
    }
    
    # Hide the last column (Peak GPU memory Usage [%])
    df <- df[, -ncol(df)]
    
    # Show the specified number of data points
    num_points <- as.numeric(input$dataPoints)
    head(df, num_points)
  })
  
  
  # Helper function to calculate statistics for a specific column
  calculate_stats <- function(df, col_name, hour_selection) {
    # Filter data by selected hour if not "all"
    if (hour_selection != "all") {
      time_col <- names(df)[3]
      df <- df[df[[time_col]] == as.numeric(hour_selection), ]
      
      if (nrow(df) == 0) {
        return(paste("No data found for Hour", hour_selection))
      }
    }
    
    col_data <- df[[col_name]]
    
    # Check if column is numeric
    if (!is.numeric(col_data)) {
      return("Non-numeric column")
    }
    
    col_data_clean <- col_data[!is.na(col_data)]
    
    if (length(col_data_clean) == 0) {
      return("No valid numeric data")
    }
    
    # Calculate statistics
    stats_text <- paste(
      " Mean:", round(mean(col_data_clean), 2), "\n",
      " Std Dev:", round(sd(col_data_clean), 2), "\n",
      " Median:", round(median(col_data_clean), 2), "\n",
      " Min:", round(min(col_data_clean), 2), "\n",
      " Max:", round(max(col_data_clean), 2), "\n"
    )
    
    return(stats_text)
  }
  
  # Temperature Statistics
  output$tempStats <- renderText({
    req(data())
    
    df <- data()
    
    # Check if we have at least 8 columns
    if (ncol(df) < 8) {
      return("Dataset must have at least 8 columns to show statistics.")
    }
    
    # Get temperature column (column 4)
    temp_col <- names(df)[4]
    calculate_stats(df, temp_col, input$selectedHour)
  })
  
  # Power Usage Statistics
  output$powerStats <- renderText({
    req(data())
    
    df <- data()
    
    # Check if we have at least 8 columns
    if (ncol(df) < 8) {
      return("Dataset must have at least 8 columns to show statistics.")
    }
    
    # Get power column (column 6)
    power_col <- names(df)[6]
    calculate_stats(df, power_col, input$selectedHour)
  })
  
  # Memory Usage Statistics
  output$memoryStats <- renderText({
    req(data())
    
    df <- data()
    
    # Check if we have at least 8 columns
    if (ncol(df) < 8) {
      return("Dataset must have at least 8 columns to show statistics.")
    }
    
    # Get memory column (column 8)
    memory_col <- names(df)[8]
    calculate_stats(df, memory_col, input$selectedHour)
  })
  

  # Function to calculate Ppk for a rack
  calculate_ppk <- function(rack_data, metric_col, spec_upper, spec_lower) {
    if (nrow(rack_data) == 0) return(NA)
    
    metric_data <- as.numeric(rack_data[[metric_col]])
    metric_data <- metric_data[!is.na(metric_data)]
    
    if (length(metric_data) < 2) return(NA)
    
    mean_val <- mean(metric_data)
    std_val <- sd(metric_data)
    
    # Check for valid standard deviation
    if (is.na(std_val) || std_val == 0) return(NA)
    
    # Calculate Ppk
    if (!is.na(spec_upper) && !is.na(spec_lower)) {
      # Both limits available
      ppk_upper <- (spec_upper - mean_val) / (3 * std_val)
      ppk_lower <- (mean_val - spec_lower) / (3 * std_val)
      ppk <- min(ppk_upper, ppk_lower, na.rm = TRUE)
    } else if (!is.na(spec_upper)) {
      # Only upper limit
      ppk <- (spec_upper - mean_val) / (3 * std_val)
    } else if (!is.na(spec_lower)) {
      # Only lower limit
      ppk <- (mean_val - spec_lower) / (3 * std_val)
    } else {
      # No specification limits
      return(NA)
    }
    
    # Check for valid Ppk result
    if (is.na(ppk) || is.infinite(ppk)) return(NA)
    
    return(ppk)
  }
  
  # Function to get color based on Ppk value
  get_ppk_color <- function(ppk) {
    if (is.na(ppk)) return("#8B0000")  # Default dark red
    
    if (ppk >= 1.67) {
      return("#35B779")  # Green - Excellent (6-sigma)
    } else if (ppk >= 1.33) {
      return("#1F9E89")  # Teal - Good (5-sigma)
    } else if (ppk >= 1.0) {
      return("#DAA520")  # Dark yellow - Acceptable (3-sigma)
    } else if (ppk >= 0.67) {
      return("#F1605D")  # Red - Poor (2-sigma)
    } else {
      return("#8B0000")  # Dark red - Very poor
    }
  }
  
  # Rack buttons
output$rackButtons <- renderUI({
  req(data())
  racks <- unique(data()$Rack_ID)
  
  # Trigger re-render when selection changes
  selectedRack()
  
  # Define number of columns for the grid
  n_cols <- 4  # adjust as desired
  n_racks <- length(racks)
  n_rows <- ceiling(n_racks / n_cols)
  
  button_list <- lapply(racks, function(rack) {
    # Get rack data
    rack_data <- data()[data()$Rack_ID == rack, ]
    
    # Calculate Ppk based on selected metric
    metric_col <- switch(input$rackMetric,
      "temperature" = names(rack_data)[4],  # Average_GPU_Temperature
      "memory" = names(rack_data)[8],       # Average_GPU_Memory_Usage
      "power" = names(rack_data)[6],         # Average_GPU_Power_Usage
      names(rack_data)[4]  # Default to temperature
    )
    
    # Get specification limits
    spec_upper <- if (input$rackMetric == "temperature") input$rackTempMax
                  else if (input$rackMetric == "memory") input$rackMemMax
                  else if (input$rackMetric == "power") input$rackPowerMax
                  else NA
    
    spec_lower <- if (input$rackMetric == "memory") input$rackMemMin 
                  else if (input$rackMetric == "power") input$rackPowerMin
                  else NA
    
    # Calculate Ppk
    ppk <- calculate_ppk(rack_data, metric_col, spec_upper, spec_lower)
    
    # Get color based on Ppk
    ppk_color <- get_ppk_color(ppk)
    
    # Check if this rack is selected
    is_selected <- !is.null(selectedRack()) && selectedRack() == rack
    button_class <- if (is_selected) "boxButton selected" else "boxButton"
    
    # Create button with Ppk-based styling
    ppk_text <- if (!is.na(ppk)) paste0(" (Ppk: ", round(ppk, 2), ")") else " (No Spec)"
    
    actionButton(
      inputId = paste0("rack_", rack),
      label = paste("Rack:", rack, ppk_text),
      class = button_class,
      style = if (!is_selected) paste0("background-color: ", ppk_color, "; color: white;")
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
      "power" = col_names[6],         # Average_GPU_Power_Usage
      col_names[4]  # Default to temperature
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
    } else if (input$rackMetric == "power") {
      spec_upper <- input$rackPowerMax
      spec_lower <- input$rackPowerMin
    } else {
      spec_upper <- NA
      spec_lower <- NA
    }
    
    # Calculate y-axis limits to ensure control limits are visible
    data_range <- range(metric_data, na.rm = TRUE)
    control_range <- range(c(upper_control, lower_control), na.rm = TRUE)
    spec_range <- range(c(spec_upper, spec_lower), na.rm = TRUE)
    
    # Combine all ranges and add padding
    all_values <- c(data_range, control_range, spec_range)
    y_min <- min(all_values, na.rm = TRUE) * 0.9  # 10% padding below
    y_max <- max(all_values, na.rm = TRUE) * 1.1  # 10% padding above
    
    # Create the plot with error handling and proper y-axis scaling
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
           cex.main = 1.3,
           ylim = c(y_min, y_max))  # Set y-axis limits
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
      "power" = col_names[6],         # Average_GPU_Power_Usage
      col_names[4]  # Default to temperature
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
      spec_lower <- NA
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
    } else if (input$rackMetric == "power") {
      spec_upper <- input$rackPowerMax
      spec_lower <- input$rackPowerMin
      stats_text <- paste(stats_text,
        "Specification Limits:\n",
        "  Upper Spec Limit:", spec_upper, "W\n",
        "  Lower Spec Limit:", spec_lower, "W\n\n")
    }
    
    # Calculate and add Ppk
    ppk <- calculate_ppk(rack_data, metric_col, spec_upper, spec_lower)
    
    if (!is.na(ppk)) {
      # Determine Ppk interpretation
      if (ppk >= 1.67) {
        ppk_interpretation <- "Excellent (6-sigma process)"
      } else if (ppk >= 1.33) {
        ppk_interpretation <- "Good (5-sigma process)"
      } else if (ppk >= 1.0) {
        ppk_interpretation <- "Acceptable (3-sigma process)"
      } else if (ppk >= 0.67) {
        ppk_interpretation <- "Poor (2-sigma process)"
      } else {
        ppk_interpretation <- "Very Poor (unacceptable)"
      }
      
      stats_text <- paste(stats_text,
        "Process Performance Index (Ppk):\n",
        "  Ppk Value:", round(ppk, 3), "\n",
        "  Interpretation:", ppk_interpretation, "\n\n",
        "Ppk Description:\n",
        "  Ppk measures how well the process performs relative to specification limits.\n",
        "  Higher values indicate better process capability and fewer defects.\n",
        "  Ppk ≥ 1.67 = Excellent, Ppk ≥ 1.33 = Good, Ppk ≥ 1.0 = Acceptable,\n",
        "  Ppk ≥ 0.67 = Poor, Ppk < 0.67 = Very Poor\n")
    } else {
      stats_text <- paste(stats_text,
        "Process Performance Index (Ppk):\n",
        "  Ppk Value: Cannot calculate (insufficient data or no specification limits)\n\n",
        "Ppk Description:\n",
        "  Ppk measures how well the process performs relative to specification limits.\n",
        "  Higher values indicate better process capability and fewer defects.\n")
    }
    
    return(stats_text)
  })
  
  # Rack GPU Data Table with Pagination
  output$rackGPUTable <- DT::renderDT({
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
    
    # Return the dataset with DT formatting
    return(rack_data)
  }, 
  options = list(
    pageLength = 10,
    lengthMenu = c(5, 10, 25, 50, 100),
    searching = TRUE,
    ordering = TRUE,
    scrollX = TRUE,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
    columnDefs = list(
      list(className = 'dt-center', targets = '_all')
    )
  )
  )
  
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
          p(strong("Threshold:"), round(failure$Threshold, 2))
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
