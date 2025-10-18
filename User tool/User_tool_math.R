# code by Group 9, Sigma Squad 

#loading all the libraries we need 
library(tidyverse)
library(ggplot2)
library(dplyr)



# reading the given GPU dataset csv from the Dataset folder
# set the working directory 
dataset_num <- 1
filename <- paste("/cloud/project/Datacenter_datasets/", dataset_num, sep = "_")
filename <- paste0(filename, ".csv")

#pull the csv data and store it as GPU_data
GPU_data = read_csv(filename)

# user inputted data
#calculating sigma_short (the mean of each rack which is our subgroup)
get_chart_data <- function(GPU_data) {
GPU_stats = GPU_data %>% group_by(Time_Period) %>% summarize (
   num_GPU_rack = n(), sd_avetemp = sd(Average_GPU_Temperature), mean_avetemp = mean(Average_GPU_Temperature),
   sd_avepow = sd(Average_GPU_Power_Usage), mean_avepow = mean(Average_GPU_Power_Usage),
   sd_avemem = sd(Average_GPU_Memory_Usage), mean_avemem = mean(Average_GPU_Memory_Usage),
) %>% 
  # calculating xbbar
  mutate( xbbar_avetemp = mean(mean_avetemp), xbbar_avepow = mean(mean_avepow), xbbar_avemem = mean(mean_avemem), # to calculate the xbbar of each performance metric
         centerline_avetemp = xbbar_avetemp, centerline_avemem = xbbar_avemem, centerline_avepow = xbbar_avepow, # to set the centerline of each performance metric which is xbbar respectively 
         sigma_s_avetemp = sqrt(mean(sd_avetemp^2)), sigma_s_avepow = sqrt(mean(sd_avepow^2)), sigma_s_avemem = sqrt(mean(sd_avemem^2)),
         se_avetemp = sigma_s_avetemp / sqrt(num_GPU_rack), se_avepow = sigma_s_avepow / sqrt(num_GPU_rack), se_avemem = sigma_s_avemem / sqrt(num_GPU_rack),
         upper_avetemp = xbbar_avetemp + 3*se_avetemp, upper_avepow = xbbar_avepow + 3*se_avepow, upper_avemem = xbbar_avemem + 3*se_avemem,
         lower_avetemp = xbbar_avetemp - 3*se_avetemp, lower_avepow = xbbar_avepow - 3*se_avepow, lower_avemem = xbbar_avemem - 3*se_avemem )
 return(GPU_stats)
}


# # if statements checking the individual GPUs for failure
# max_temp = 70
# max_pow = 300
# max_mem = 300
# # user stats
# single_GPU_check <- function(GPU_data, max_temp, max_pow, max_mem) {
#   i <- 1
#   flagged_ids <- c()  # store flagged GPU IDs
#   corresponding_time <- c() #store the time it was flagged
#   
#   while (i <= nrow(GPU_data)) {
#     temp <- GPU_data$Peak_GPU_Temperature[i]
#     pow <- GPU_data$Peak_GPU_Power_Usage[i]
#     mem <- GPU_data$Peak_GPU_Memory_Usage[i]
#     
#     if (temp > max_temp || pow > max_pow || mem > max_mem || (pow == 0 && mem == 0)) {
#       flagged_ids <- c(flagged_ids, GPU_data$GPU_Unique_ID[i])
#       corresponding_time <- c(corresponding_time, GPU_data$Time_Period[i])
#     }
#     
#     i <- i + 1
#   }
#   
#   return(flagged_ids)
# }
