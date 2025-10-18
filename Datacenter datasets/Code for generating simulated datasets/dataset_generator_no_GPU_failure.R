# Simulated GPU data set 
# Group members: Nevin Motto, Evelyne Morisseu, Daniel Carlson, Rene Umeh

# This script simulates GPU performance metrics over 
# multiple GPU racks and time periods. Each data point represents
# the average and peak operating conditions for a single GPU over
# a defined time period. 

# This data set was created to demonstrate our tools functionality and is one of 
# three test datasets. This particular dataset has average temperature,
# memory usage, and power usage values varying in a normal distribution
# around a set mean. 

# Each column in this dataset captures a different dimension of GPU behavior: 
# - GPU_Unique_ID: identifies each individual GPU
# - Rack_ID: groups GPUs by their physical rack
# - Time_Period: time intervals of measurement
# - Average_GPU_Temperature: typical temperature averages sustained (Celsius)
# - Peak_GPU_Temperature: maximum temperature spike in each GPU (Celsius)
# - Average_GPU_Power_Usage: average power draw (watts)
# - Peak_GPU_Power_Usage: Maximum power draw for each GPU (watts)
# - Average_GPU_Memory_Usage: average GPU memory utilization as % of memory
# - Peak_GPU_Memory_Usage: maximum memory usage as % of total memory

# Example dataset parameters:
set.seed(123)  # use for reproducibility of the random data

# CHANGE TO SPECIFY WHAT YOU WANT
size <-  5        # GPUs per rack
racks <- 10      # number of racks
time_periods <- 24  # number of time periods

# Total number of datapoints
n <- size * racks * time_periods

# Generate the simulated GPU performance dataset
one <- data.frame(
  
  # Unique ID for each GPU 
  GPU_Unique_ID = rep(1:(size * racks), each = time_periods),
  
  # Rack that each GPU belongs to
  Rack_ID = rep(rep(1:racks, each = size), each = time_periods),
  
  # Time intervals
  Time_Period = rep(1:time_periods, times = size * racks),
  
  # Average GPU temperature normally distributed around 75°C
  # Values are rounded to one decimal place with lower and upper limits
  # at 30°C and 90°C respectively
  Average_GPU_Temperature = pmin(pmax(round(rnorm(n, mean = 75, sd = 6), 1), 30), 90),
  
  # Peak GPU temperature (°C) representing short-term thermal spikes
  Peak_GPU_Temperature = pmin(pmax(round(rnorm(n, mean = 75, sd = 6), 1), 30), 90),
  
  # Average power draw (Watts) normally distributed around 180W with normal variation
  Average_GPU_Power_Usage = round(rnorm(n, mean = 180, sd = 20), 1),
  
  # Peak power draw (Watts)
  Peak_GPU_Power_Usage = round(rnorm(n, mean = 220, sd = 25), 1),
  
  # Average memory usage (% of total), normally distributed around 80%
  Average_GPU_Memory_Usage = pmin(pmax(round(rnorm(n, mean = 80, sd = 5), 1), 0), 100),
  
  # Peak memory usage (% of total), normally distributed around 90%
  Peak_GPU_Memory_Usage = pmin(pmax(round(rnorm(n, mean = 90, sd = 6), 1), 0), 100)
)

# Arrange the dataset in logical order
to_export <- one %>%
  arrange(Time_Period, Rack_ID, GPU_Unique_ID)

# Save to CSV
# CHANGE THE FINAL CSV NAME TO AVOID OVERWRITTING FILES AND MATCH YOUR FILE DIRECTORY TO STORE THE CSV FILE
write.csv(to_export, "/cloud/project/Datacenter_datasets/_2.csv", row.names = FALSE)
