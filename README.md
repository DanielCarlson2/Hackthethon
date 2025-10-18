## SixSigmaHackathon: GPU Health Monitoring System
### Group: Daniel Carlson, Rene Umeh, Evelyne Morisseau, Charlie Gagliardo, and Nevin Motto

Project Prompt 1: Industrial Engineering

Data centers are key infrastructure in the AI economy, providing servers equipped with high numbers of GPUs needed for high speed model training and serving. Given the recent surge in GPU demand, data centers are struggling to keep up, and hardware failures need to be addressed swiftly. Develop a quality control system to track quality and/or failure in GPU usage across data centers for a system like Google Cloud or Amazon Web Services.

### Project Goal:

Create an intuitive Shinyapp dashboard tool to help users find existing hardware failures in GPU racks and individual GPUs based on trends in GPU temperature, powerdraw, and memory utilization collected on an hourly basis.

### Project Scope:

We have been contracted to develop a tool that will analyze GPU data for datacenters. The tool will be used to inform datacenter maintenance staff and onsite operators of possible individual GPU failures and GPU racks under high stress or full failure. High stress is defined as a GPU overheating, high power draw, and high memory usage. Maintenance staff will be able to enter desired upper and lower specification limits for GPU temperature, power consumption, and memory usage. This workspace will take CSV format data. The CSV must have the pictured column names in the same order. Each variable used is listed and described in the Codebook for CSV Data Inputs section. 

Example:

![CSV_column_example]()


Our scope includes GPUs organized into racks in the specified datacenter from our client. The scope does not include datacenters using different types of GPUs in the same datacenter (however this feature could be implemented in the future). Also, the scope does not take into account the cost of some GPUs being higher than others and does not have a system to prioritize specified GPUs over others. 

### How to Install the Tool:

- Have R and R Studio downloaded.
- Clone the GitHub repository or download the ZIP file from GitHub and open it in RStudio.
- Place your CSV files in the Dashboard.
- Input the following GPU specification limit information:
  - Max Temperature
  - Max Memory Utilization
  - Max Power Usage

### How to use:  

Prerequisite: Follow the CSV generator instructions first if you do not have a CSV file ready to upload.

Step 1: Open the Shiny App Tool located in the Github folder titled "AppDevelopment"
- Launch the Shiny application in RStudio or from the console.
- The interface will open automatically in your web browser.


Step 2: Upload Your CSV File
- Use the file upload box on the Home or Upload page.
- Select your desired CSV file.
- The app will automatically load your dataset and open the How To Use page.

Example:

![Step 1 example GIF](InstructionGIFs/UploadCSV.gif)

Step 3: Select a Focus Metric Tab
- Choose one of the metric tabs located along the top navigation bar:
  - General
  - Rack
  - Individual GPU

Step 3A: General Tab
- Provides an overview of all GPU performance metrics (Temperature, Power Usage, and Memory Usage).
- The top-level statistical summaries can be filtered by hour to focus on specific time periods.
- You can:
  - Preview the uploaded dataset.
  - Control how many data points are displayed.
  - Choose how data points are grouped (by rack, GPU, or time).

Example: 
![Step 3a example GIF](InstructionGIFs/GeneralTab.gif)

Step 3B: Rack Tab
- Displays GPU performance on a per-rack basis.
- Uses a performance index (Ppk) to determine which racks require attention.
- Racks are color-coded according to Ppk values for a visual representation of which racks need maintenance.

Example: 
![Step 3b example GIF](InstructionGIFs/ByRackTab.gif)

Step 3C: GPU Tab
- Allows users to set specification limits (upper and lower) for key metrics.
- Plots GPU failures over time to identify trends and timing.
- Displays detailed data showing where, when, and how each GPU failure occurred.

Example: 
![Step 3c example GIF](InstructionGIFs/ByGPUTab.gif)

### Codebook for CSV Data Inputs:

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
  
#### General Overview/Purpose:
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
- Simulates a datacenter containing:
  - Healthy GPUs running safely,
  - Dead GPUs that need replacement, and
  - High-stress GPUs requiring maintenance intervention.

#### How to Install: 
- Download the CSV generator files from the GitHub repository.
- Run in R or RStudio.

#### How to use CSV Generator:
- Open the CSV generator code that you just installed.
- With in the generator code modify the sections labelled with comments. The following can be changed:
  - The number of racks, GPUs, and time period
  - (If using a generator that adds GPUs close to failure or dead) The percentage of GPUs that will be close to fail and the number of GPUs that will be dead.
  - The filename of the outputted CSV file (recommended to change per run so as to not overwrite any CSV files accidentally) & the file directory for where you wish to store the CSV files in your computer.
- Once you are happy with the specifications that you have chosen, run the code! 

#### Dataset Notes:
For our generated simulated datasets we used the following inputs:
 - Number of Racks: 10
 - GPUs per Rack: 5
 - Time Periods: 24 (24 hour day, so data is collected per hour)
 - Total Data Points: 1200
 - Temperature Range: 30 ℃ to 90 ℃
</details>

### Thanks for reading! Happy GPU health monitoring! 
![Thanks!](InstructionGIFs/giphy.gif)
