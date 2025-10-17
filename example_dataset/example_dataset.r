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


# Example dataset:
set.seed(123)  # reproducibility

size <- 10         # GPUs per cluster
clusters <- 3      # number of clusters
time_periods <- 3  # number of time periods

# total datapoints
n <- size * clusters * time_periods

example_dataset <- data.frame(
  GPU_Unique_ID = rep(1:(size*clusters), each = time_periods),
  GPU_Cluster_ID = rep(rep(1:clusters, each = size), each = time_periods),
  Time_Period = rep(1:time_periods, times = size*clusters),
  Average_GPU_Temperature = round(rnorm(n, mean = 70, sd = 5), 1),
  Peak_GPU_Temperature = round(rnorm(n, mean = 85, sd = 4), 1),
  Average_GPU_Power_Usage = round(rnorm(n, mean = 180, sd = 20), 1),
  Peak_GPU_Power_Usage = round(rnorm(n, mean = 220, sd = 25), 1),
  Average_GPU_Memory_Usage = pmin(pmax(round(rnorm(n, mean = 80, sd = 5), 1), 0), 100),
  Peak_GPU_Memory_Usage = pmin(pmax(round(rnorm(n, mean = 90, sd = 6), 1), 0), 100)
)

# Save to CSV
write.csv(example_dataset, "example_dataset.csv", row.names = FALSE)