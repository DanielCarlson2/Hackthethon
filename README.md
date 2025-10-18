## SixSigmaHackathon: GPU Health Monitoring System
### Group: Daniel Carlson, Rene Umeh, Evelyne Morisseau, Charlie Gagliardo, and Nevin Motto

Project Prompt 1: Industrial Engineering

Data centers are key infrastructure in the AI economy, providing servers equipped with high numbers of GPUs needed for high speed model training and serving. Given the recent surge in GPU demand, data centers are struggling to keep up, and hardware failures need to be addressed swiftly. Develop a quality control system to track quality and/or failure in GPU usage across data centers for a system like Google Cloud or Amazon Web Services.

### Project goal

Create an intuitive Shinyapp dashboard tool to help users find existing hardware failures in GPU racks and individual GPUs based on trends in GPU temperature, powerdraw, and memory utilization collected on an hourly basis.

### Project Scope 

We have been contracted to develop a tool that will analyze GPU data for datacenters. The tool will be used to inform datacenter maintenance staff and onsite operators of possible individual GPU failures and GPU racks under high stress or full failure. High stress is defined as a GPU overheating, high power draw, and high memory usage. Maintenance staff will be able to enter desired upper and lower specification limits for GPU temperature, power consumption, and memory usage. This workspace will take .CSV format data with the following naming convention: <_daynumber>, where daynumber is the day of the month the corresponding data is from with the convention resetting each month. Each variable used is listed and described in the following section. 

Our scope includes GPUs organized into racks in the specified datacenter from our client. The scope does not include datacenters using different types of GPUs in the same datacenter (however this feature could be implemented in the future). Also, the scope does not take into account the cost of some GPUs being higher than others and does not have a system to prioritize specified GPUs over others. 

### How to Install the Tool:

- Have R and R Studio downloaded.
- Clone the github repository or download the ZIP file from github and open it in RStudio.
- Place your .csv files in the Dashboard.
- Input the following GPU specification limit information:
  - Max Temperature
  - Max Memory Utilization
  - Max Power Usage

### How to use:  

GIF OF USER USING 

### Codebook for CSV Data Inputs

| Variable                    | Type     | Description                                               | Unit        |
|-----------------------------|----------|-----------------------------------------------------------|-------------|
| GPU_Unique_ID              | Integer  | Unique identifier for each GPU in the datacenter          | ID number   |
| Rack_ID                    | Integer  | Unique ID locating GPUs in groups called “Racks”          | ID number   |
| Time_Period                | Integer  | Time of day data is pulled from the datacenter            | Hour        |
| Average_GPU_Temperature    | Numeric  | Average temperature of each GPU over the time period      | Celsius     |
| Peak_GPU_Temperature       | Numeric  | Highest temperature of a GPU over the time period         | Celsius     |
| Average_GPU_Power_Usage    | Numeric  | Average power usage of each GPU over the time period      | Watts       |
| Peak_GPU_Power_Usage       | Numeric  | Highest power usage of a GPU over the time period         | Watts       |
| Average_GPU_Memory_Usage   | Numeric  | Average memory usage of each GPU over the time period     | Percentage  |
| Peak_GPU_Memory_Usage      | Numeric  | Highest memory usage of a GPU over the time period        | Percentage  |


### Read Me for CSV Dataset Generators 
<details> 
<summary>Expand</summary>
  
#### General Overview/Purpose
There are three CSV Dataset Generators, each designed to simulate different GPU failure scenarios in a datacenter environment:

dataset_generator_no_GPU_failure.R
- All GPUs and racks are running at optimal, safe metrics.
- Represents a fully healthy datacenter with no hardware issues.

dataset_generator_with_errors.R
- Most GPUs operate at normal levels.
- A specified percentage of GPUs (default 30%) run near failure thresholds of overheating, high power draw, or high memory utilization.
- Simulates a datacenter with some GPUs under high stress requiring immediate maintenance or monitoring.

dataset_generator_with_dead_GPUs.R
- Most GPUs are healthy.
- A specified number of GPUs (default 3) are completely dead or shut down (no power draw or memory usage).
- Another specified percentage (default 30%) of GPUs are close to failure but still operational.
-Simulates a datacenter containing:
  -Healthy GPUs running safely,
  -Dead GPUs that need replacement, and
  -High-stress GPUs requiring maintenance intervention.

#### How to Install: 
- Download the .csv generator files from the github repository.
- Run in R or RStudio.

#### How to use .csv Generator:
- Open the .csv generator code that you just installed .
- With in the generator code modify the sections labelled with comments. The following can be changed:
  - The number of racks, GPUs, and time period
  - (If using a generator that adds GPUs close to failure or dead) The percentage of GPUs that will be close to fail and the number of GPUs that will be dead.
  - The filename of the outputted csv file (recommended to change per run so as to not overwrite any csv files accidentally) & the file directory for where you wish to store the csv files in your computer.
- Once you are happy with the specifications that you have chosen, run the code! 

#### Our Dataset Notes
For our generated simulated datasets we used the following inputs:
 - Number of Racks: 10
 - GPUs per Rack: 5
 - Time Periods: 24 (24 hour day, so data is collected per hour)
 - Total Data Points: 1200
 - Temperature Range: 30 ℃ to 90 ℃
</details>
