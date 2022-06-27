### Deliverable 1: Linear Regression to Predict MPG

# Use the library() function to load the dplyr package
library(tidyverse)

# Import and read in the MechaCar_mpg.csv file as a dataframe.
MPG_data <- read.csv("MechaCar_mpg.csv", stringsAsFactors = F, check.names = F)

# Perform linear regression using the lm() function
MPG_linreg <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data = MPG_data)

# Using the summary() function, determine the p-value and r-squared of the linear regression module.
summary(MPG_linreg)


### Deliverable 2: Create Visualizations for the Trip Analysis

# Import and read in the Suspension_Coil.csv file as a table
Suspension_data <- read.csv("Suspension_Coil.csv", stringsAsFactors = F, check.names = F)

# Create a total_summary dataframe using the summarize() function to get the mean/median/variance/standard deviation of the suspension coil's PSI column
total_summary <- Suspension_data %>% 
  summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))

# Create a lot_summary dataframe using the group_by() and the summarize() functions to group each manufacturing lot by the mean/median/variance/and standard deviation of the suspension coil's PSI column.
lot_summary <- Suspension_data %>% group_by(Manufacturing_Lot) %>% 
  summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups='keep')


### Deliverable 3: T-Tests on Suspension Coils

# Use the t.test() function to determine if the PSI across all manufacturing lots is statistically different from the population mean of 1,500 pounds per square inch.
t.test(Suspension_data$PSI, mu = 1500)

# T-test on Lot 1
t.test(subset(Suspension_data, Manufacturing_Lot == "Lot1")$PSI, mu = 1500)

# T-test on Lot 2
t.test(subset(Suspension_data, Manufacturing_Lot == "Lot2")$PSI, mu = 1500)

# T-test on Lot 3
t.test(subset(Suspension_data, Manufacturing_Lot == "Lot3")$PSI, mu = 1500)

