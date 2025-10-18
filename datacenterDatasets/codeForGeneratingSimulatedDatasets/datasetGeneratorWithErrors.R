# Simulated GPU dataset with GPUs purposefully reaching the max failure limits for temperature, power usage, or memory usage to test app performance
# group members: Nevin Motto, Evelyne Morisseau, Daniel Carlson, Rene Umeh, Charlie Gagliardo
# What makes this data generator different is that we wanted to be able to simulate a datacenter where some racks are failing and others are doing fine. 
# To simulate this in a dataset, we first generate a csv of data where all the GPUs are operating at relatively normal metrics
# we then the randomly select 30% of GPUs to have metrics that are close to failing criteria. These will be then manually overwritten. The result is some specific GPUs and thus Racks that are running close to failure while others are normal


# this script simulates GPU performance data across three metrics: Temperature, Power usage, and memory usage
# Each datapoint is a row for a given GPU averaged over a given time period
# The columns are the following:
# - GPU Unique ID
# - GPU Cluster ID
# - Time Period
# - Average GPU Temperature in Celsius
# - Peak GPU Temperature in Celsius
# - GPU Power Usage in Watts
# - Peak GPU Power Usage in Watts
# - GPU Memory Usage in percentage of total memory
# - Peak GPU Memory Usage in percentage of total memory
# - Is Failing (this column is only used to help generate randomly GPUs that are failing, that way not all racks are failing and only some will fail like a real datacenter), in the real user provided CSV datasets this column would not exist

# load required libraries 
library(dplyr)

set.seed(123)  # reproducibility

# CHANGE TO SPECIFY WHAT YOU WANT
size <-  5        # GPUs per rack
racks <- 10       # number of racks
time_periods <- 24  # number of time periods (each digit is one hour of a 24 hour day)

# total datapoints
n <- size * racks * time_periods

# Randomly select 30% of racks to be "failing"
# CHANGE THE PERCENTAGE TO SPECIFY WHAT YOU WANT (IN ROUND)
failing_rack_ids <- sample(1:racks, size = round(0.3 * racks), replace = FALSE)

# Create base structure
one <- data.frame(
  GPU_Unique_ID = rep(1:(size * racks), each = time_periods),
  Rack_ID = rep(rep(1:racks, each = size), each = time_periods),
  Time_Period = rep(1:time_periods, times = size * racks)
)

# Add Failing Indicator
one <- one %>%
  mutate(
    Is_Failing = Rack_ID %in% failing_rack_ids
  )

# Generate metrics conditionally based on failure status
generate_values <- function(fail, normal_mean, normal_sd, fail_mean, fail_sd, min_val = -Inf, max_val = Inf) {
  vals <- ifelse(
    fail,
    rnorm(length(fail), mean = fail_mean, sd = fail_sd),
    rnorm(length(fail), mean = normal_mean, sd = normal_sd)
  )
  return(pmin(pmax(round(vals, 1), min_val), max_val))
}

# Add metric columns that indicate GPU is failing, and add them to the CSV dataset that were set as failing
one <- one %>%
  mutate(
    Average_GPU_Temperature = generate_values(Is_Failing, 75, 6, 85, 5, 30, 100),
    Peak_GPU_Temperature = generate_values(Is_Failing, 80, 6, 95, 4, 30, 100),
    Average_GPU_Power_Usage = generate_values(Is_Failing, 180, 20, 210, 25),
    Peak_GPU_Power_Usage = generate_values(Is_Failing, 220, 25, 260, 30),
    Average_GPU_Memory_Usage = generate_values(Is_Failing, 80, 5, 90, 5, 0, 100),
    Peak_GPU_Memory_Usage = generate_values(Is_Failing, 90, 6, 98, 3, 0, 100)
  )

# Optional: sort data for readability
to_export <- one %>%
  arrange(Time_Period, Rack_ID, GPU_Unique_ID) %>%
  select(-Is_Failing, everything(), Is_Failing)  # Move Is_Failing to end for UI development so it does not interfere with real metrics to be displayed to user


# Save to CSV
write.csv(to_export, "./datacenterDatasets/dataset_3.csv", row.names = FALSE)

# Print failing racks
cat("Failing Rack IDs:", failing_rack_ids, "\n")
# Move 'Is_Failing' column to the end
to_export <- to_export %>%
  select(-Is_Failing, everything(), Is_Failing)
