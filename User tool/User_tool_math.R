# code by Group 9, Sigma Squad 

#loading all the libraries we need 
library(tidyverse)
library(ggplot2)
library(dplyr)

# user inputted data
#calculating sigma_short (the mean of each rack which is our subgroup)
get_chart_data <- function(GPU_data) {
  GPU_stats <- GPU_data %>%
    # Group by time period so we can compute subgroup means and SDs
    group_by(Time_Period) %>%
    summarize(
      num_GPU = n(),   # number of GPUs (sample size per subgroup)
      sd_avetemp = sd(Average_GPU_Temperature),   # subgroup SD of average temp
      mean_avetemp = mean(Average_GPU_Temperature), # subgroup mean of avg temp
      sd_avepow = sd(Average_GPU_Power_Usage),    # subgroup SD of power usage
      mean_avepow = mean(Average_GPU_Power_Usage), # subgroup mean of power usage
      sd_avemem = sd(Average_GPU_Memory_Usage),   # subgroup SD of memory usage
      mean_avemem = mean(Average_GPU_Memory_Usage) # subgroup mean of memory usage
    ) %>%
    ungroup() %>%
    mutate(
      # Grand means (x̄bar) across all subgroups
      xbbar_avetemp = mean(mean_avetemp),
      xbbar_avepow  = mean(mean_avepow),
      xbbar_avemem  = mean(mean_avemem),

      # Centerlines = overall averages (for control chart plotting)
      centerline_avetemp = xbbar_avetemp,
      centerline_avepow  = xbbar_avepow,
      centerline_avemem  = xbbar_avemem,

      # Pooled standard deviations (σ̂) across subgroups
      sigma_s_avetemp = sqrt(mean(sd_avetemp^2)),
      sigma_s_avepow  = sqrt(mean(sd_avepow^2)),
      sigma_s_avemem  = sqrt(mean(sd_avemem^2)),

      # Standard error of the mean for each metric
      # (uses average subgroup size rather than varying by period)
      se_avetemp = sigma_s_avetemp / sqrt(mean(num_GPU)),
      se_avepow  = sigma_s_avepow  / sqrt(mean(num_GPU)),
      se_avemem  = sigma_s_avemem  / sqrt(mean(num_GPU)),

      # Control limits (±3 standard errors around centerline)
      upper_avetemp = xbbar_avetemp + 3 * se_avetemp,
      lower_avetemp = xbbar_avetemp - 3 * se_avetemp,
      upper_avepow  = xbbar_avepow  + 3 * se_avepow,
      lower_avepow  = xbbar_avepow  - 3 * se_avepow,
      upper_avemem  = xbbar_avemem  + 3 * se_avemem,
      lower_avemem  = xbbar_avemem  - 3 * se_avemem
    )

  return(GPU_stats)
}
