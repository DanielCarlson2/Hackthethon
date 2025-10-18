# Simulated GPU dataset with purposely "dead" GPUs to test app performance
# group members: Nevin Motto, Evelyne Morisseau, Daniel Carlson, Rene Umeh, Charlie Gagliardo

# this script simulates GPU performance data across three metrics: 
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

# load required libraries 
library(dplyr)

set.seed(123)  # Reproducibility

# CHANGE TO SPECIFY WHAT YOU WANT
size <- 5         # GPUs per rack
racks <- 5       # Number of racks
time_periods <- 12  # Number of time periods, 24 so there is 1 data pull per hour

# Total datapoints
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
  # Rounds values ensuring integer does not exceed min and max bounds
  return(pmin(pmax(round(vals, 1), min_val), max_val))
}

# Generate metrics 
one <- one %>%
  mutate(
    Average_GPU_Temperature = generate_values(Is_Failing, 75, 6, 85, 5, 30, 100),
    Peak_GPU_Temperature = generate_values(Is_Failing, 80, 6, 95, 4, 30, 100),
    Average_GPU_Power_Usage = generate_values(Is_Failing, 180, 20, 210, 25),
    Peak_GPU_Power_Usage = generate_values(Is_Failing, 220, 25, 260, 30),
    Average_GPU_Memory_Usage = generate_values(Is_Failing, 80, 5, 90, 5, 0, 100),
    Peak_GPU_Memory_Usage = generate_values(Is_Failing, 90, 6, 98, 3, 0, 100)
  )

# Hardcoding a few "dead" GPUs: both power and memory usage = 0
# Choose a few unique GPU IDs (ex 3 GPUs) to simulate dead GPUs
# YOU SPECIFY THE NUMBER OF GPUS YOU'D LIKE TO BE DEAD (CHANGE SIZE)
dead_gpu_ids <- sample(unique(one$GPU_Unique_ID), size = 3)

# ifelse statements to set GPU Power or Memory as 0 if GPU is dead
one <- one %>%
  mutate(
    Peak_GPU_Power_Usage = ifelse(GPU_Unique_ID %in% dead_gpu_ids, 0, Peak_GPU_Power_Usage),
    Peak_GPU_Memory_Usage = ifelse(GPU_Unique_ID %in% dead_gpu_ids, 0, Peak_GPU_Memory_Usage)
  )

# Sort data for readability
to_export <- one %>%
  arrange(Time_Period, Rack_ID, GPU_Unique_ID) %>%
  select(-Is_Failing, everything(), Is_Failing)  # Move Is_Failing to end

# Save to CSV
write.csv(to_export, "./datacenterDatasets/dataset_4.csv", row.names = FALSE)

# Print diagnostics
cat("Failing Rack IDs:", failing_rack_ids, "\n")
cat("Dead GPU IDs (power & memory = 0):", dead_gpu_ids, "\n")
