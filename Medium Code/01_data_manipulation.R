# Medium Article
# Authors: Alejandra Cruces and Mario Tapia
# A Step-by-Step Guide to Real-World Data and Forecasting Techniques in R 
# Part 1: Getting Started with Data Manipulation and Trends 

## Step 1: Load the Necessary Libraries

# Install libraries if not already installed
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("zoo")) install.packages("zoo")
if (!require("ggplot2"))  install.packages("ggplot2")  
if (!require("forecast"))  install.packages("forecast") 

# Load libraries
library(openxlsx)
library(zoo)
library(ggplot2)
library(forecast)
library(DIMORA)

## Step 2: Import Lithium Export Data

base_url <- 'https://github.com/alecruces/SustainableLithium/raw/refs/heads/main/data_base/'


# Load Chilean lithium export data
lit.chl.exp <- read.xlsx(paste0(base_url, "litio_aus_chl.xlsx"), 
                         sheet = "lit_chl_exp")
lit.chl.exp$date <- as.Date(lit.chl.exp$date, origin = "1899-12-30")  # Fix the dates
lit.chl.exp$lit_chl_exp_kton_met <- lit.chl.exp$lit_chl_exp_klce / 1e6  # Convert to kilotons

# Load Australian lithium export data
lit.aus.exp <- read.xlsx(paste0(base_url, "litio_aus_chl.xlsx"), 
                         sheet = "lit_aus_exp")
lit.aus.exp$date <- as.Date(lit.aus.exp$date, origin = "1899-12-30")  # Fix the dates
lit.aus.exp$lit_aus_exp_kton_met <- lit.aus.exp$lit_aus_exp_kt_sp / 8  # Normalize the units

## Step 3: Transform the Data

# Generate the sequence of quarters
full_quarters <- seq(
  from = as.yearqtr(min(c(lit.chl.exp$date, lit.aus.exp$date))),
  to = as.yearqtr(max(c(lit.chl.exp$date, lit.aus.exp$date))),
  by = 0.25
)

# Create a data frame with quarters and their corresponding dates
full_quarters_df <- data.frame(
  date = as.Date(full_quarters, frac = 1),  # Convert to date
  quarter = as.yearqtr(full_quarters)    # Format as "YYYY Qn"
)
# Aggregate Chilean export data by quarter
lit.chl.exp$quarter <- as.yearqtr(lit.chl.exp$date)
chilean_exports <- aggregate(lit_chl_exp_kton_met ~ quarter, 
                             data = lit.chl.exp, sum)

# Aggregate Australian export data by quarter
lit.aus.exp$quarter <- as.yearqtr(lit.aus.exp$date)
australian_exports <- aggregate(lit_aus_exp_kton_met ~ quarter, 
                                data = lit.aus.exp, sum)

# Merge Chilean exports with full quarters
final_data <- merge(full_quarters_df, chilean_exports, by.x = "quarter", 
                    by.y = "quarter", all.x = TRUE)
names(final_data)[names(final_data) == "lit_chl_exp_kton_met"] <- "chilean_exports"

# Merge Australian exports with full quarters
final_data <- merge(final_data, australian_exports, by.x = "quarter", 
                    by.y = "quarter", all.x = TRUE)
names(final_data)[names(final_data) == "lit_aus_exp_kton_met"] <- "australian_exports"


## Step 4: Visualize the Lithium Export Data

# Plot the trends
ggplot(final_data, aes(x = date)) +
  geom_line(aes(y = chilean_exports, color = "Chile"), size = 1) +
  geom_line(aes(y = australian_exports, color = "Australia"), size = 1) +
  labs(title = "Lithium Exports Over Time",
       x = "Date",
       y = "Exports (kT)",
       color = "Country") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## Step 5: Analyze Trends and Seasonality

# Chilean Exports
# Extract the Chilean exports time series
chl_ts <- ts(na.omit(final_data$chilean_exports), start = c(2009, 1), 
             frequency = 4)

# Display the raw time series to observe the trend
tsdisplay(chl_ts, main = "Chilean Lithium Exports")

# Apply first differencing to remove the trend
chl_ts_diff <- diff(chl_ts, differences = 1)

# Check stationarity with time series diagnostics
tsdisplay(chl_ts_diff, main = "Stationarity Check: Chilean Exports")

## This is an extra plot to visualize how does the series looks like after differencing
# Plot the differenced series to confirm the trend has been removed
plot(chl_ts_diff, type = "l", col = "#00C3B1", lwd = 2,
     main = "Differenced Chilean Exports",
     xlab = "Quarter", ylab = "Differenced Exports")




## Extra: Answer to the Australian case

# Australian Exports
# Extract the Australian exports time series
aus_ts <- ts(final_data$australian_exports, start = c(2009, 1), frequency = 4)

# Display the raw time series to observe the trend
tsdisplay(aus_ts, main = "Australian Lithium Exports")

# Apply first differencing to remove the trend
# Differencing calculates the change between consecutive values, making the data stationary
aus_ts_diff <- diff(aus_ts, differences = 1)

# Plot the differenced series to confirm the trend has been removed
plot(aus_ts_diff, type = "l", col = "#00C3B1", lwd = 2,
     main = "Differenced Australian Exports",
     xlab = "Quarter", ylab = "Differenced Exports")

# Check stationarity with time series diagnostics
tsdisplay(aus_ts_diff, main = "Stationarity Check: Australian Exports")











