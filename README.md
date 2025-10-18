## SixSigmaHackathon Team 9 Sigma Squad

Completing Prompt 1: Industrial Engineering
Data centers are key infrastructure in the AI economy, providing servers equipped with high numbers of GPUs needed for high speed model training and serving. Given the recent surge in GPU demand, data centers are struggling to keep up, and hardware failures need to be addressed swiftly. Develop a quality control system to track quality and/or failure in GPU usage across data centers for a system like Google Cloud or Amazon Web Services.

## Project goal

To create a Shinyapp dashboard tool to help users find existing and potential hardware failures based on trends in GPU temperature, powerdraw, and memory utilization over time. 

## Project Scope 

We have been contracted to develop a tool that will analyze GPU data for datacenters. The tool will be used to inform datacenter maintenance staff and onsite operators of possible individual GPU failures and GPU racks under high stress or full failure. High stress is defined as a GPU overheating, high power draw, and high memory usage. Maintenance staff will be able to enter desired upper and lower specification limits for GPU temperature, power consumption, and memory usage. This workspace will take .CSV format data with the following naming convention: <_daynumber>, where daynumber is the day of the month the corresponding data is from with the convention resetting each month. Each variable used is listed and described in the following section. 

Our scope includes GPUs organized into racks in the specified datacenter from our client. The scope does not include datacenters using different types of GPUs in the same datacenter (however this feature could be implemented in the future). Also, the scope does not take into account the cost of some GPUs being higher than others and does not have a system to prioritize specified GPUs over others. 

## How to Install the Tool

##
