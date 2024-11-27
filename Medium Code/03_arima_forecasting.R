# Medium Article
# Authors: Alejandra Cruces and Mario Tapia
# A Step-by-Step Guide to Real-World Data and Forecasting Techniques in R 
# Part 3: Predicting the Future with ARIMA 


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
library(tseries)

## Step 2: Import Lithium Export Data

base_url <- 'https://github.com/alecruces/SustainableLithium/raw/refs/heads/main/data_base/'

# Load Chilean lithium export data
lit.chl.exp <- read.xlsx(paste0(base_url, "litio_aus_chl.xlsx"), sheet = "lit_chl_exp")
lit.chl.exp$date <- as.Date(lit.chl.exp$date, origin = "1899-12-30")  # Fix the dates
lit.chl.exp$lit_chl_exp_kton_met <- lit.chl.exp$lit_chl_exp_klce / 1e6  # Convert to kilotons

# Load Australian lithium export data
lit.aus.exp <- read.xlsx(paste0(base_url, "litio_aus_chl.xlsx"), sheet = "lit_aus_exp")
lit.aus.exp$date <- as.Date(lit.aus.exp$date, origin = "1899-12-30")  # Fix the dates
lit.aus.exp$lit_aus_exp_kton_met <- lit.aus.exp$lit_aus_exp_kt_sp / 8  # Normalize the units



## Step 1: Transforming Data into a Time Series Object
# Load the Library that allow us to work with ARIMA

library(tseries) 
# Load the data
chl_ts <- ts(na.omit(final_data$chilean_exports), frequency = 4, start = 2014)  # Quarterly time series from 2014
aus_ts <- ts(final_data$australian_exports, start = c(2010, 1), frequency = 4)


## Step 2: Applying Holt’s Exponential Smoothing
# Apply Holt’s exponential smoothing 

holt_model <- holt(aus_ts, h = 4)  # Forecast 4 quarters ahead 

# Plot the forecast 

plot(holt_model, main = "Holt’s Exponential Smoothing - Australian Lithium Exports", xlab = "Year", ylab = "Exports (kT)")

## Step 3:  Differencing to Achieve Stationarity

# Perform ADF test for stationarity 
adf.test(aus_ts, k =4)

# Differencing series to achieve stationarity
aus_ts_diff <- diff(aus_ts, differences = 1)

# Re-test stationarity 
adf.test(aus_ts_diff, k = 4)

## Step 4: Visualizing Stationarity

# Plot the original Chilean exports 
plot(aus_ts, type = "l", col = "blue", lwd = 2, main = "Australian Lithium Exports", xlab = "Year", ylab = "Exports (kT)") 

# Plot the differenced series 
plot(aus_ts_diff, type = "l", col = "blue", lwd = 2, main = "Differenced Australian Lithium Exports", xlab = "Year", ylab = "Exports (kT)") 

## Step 5: Selecting ARIMA Parameters

# Plot ACF and PACF
par(mfrow = c(1, 2))  # Side-by-side plots
acf(aus_ts, main = "ACF - Australian Exports")
pacf(aus_ts, main = "PACF - Australian Exports")

## Step 6: Fitting the ARIMA Model

# Fit ARIMA model (replace (1, 1, 0) with chosen parameters) 
arima_model <- Arima(aus_ts, order = c(1, 1, 0))  

# Summarize the model  
summary(arima_model)

for1_aus<- forecast(arima_model,h=4)

plot(for1_aus, type = "l", col = "black", lwd = 2,
     main = "Australia ARIMA (1,1,0) with drift", xlab = "Quarter", ylab = "kT",xaxt = "n")
lines(fitted(arima_model), col = "#00C3B1", lwd = 2, type = "l", lty = "dashed")
axis(1, at=seq(1, 60, by=4), labels=c("2010", "2011", "2012", "2013", "2014",
                                      "2015", "2016", "2017", "2018", "2019",
                                      "2020", "2021", "2022", "2023", "2024"))
abline(v=55, col="gray", lwd = 2, lty = 3)

## Step 7: Checking Residuals

# Check residuals
checkresiduals(arima_model)










