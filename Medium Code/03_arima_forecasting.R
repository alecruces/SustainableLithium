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

## Step 2: Import Lithium Export Data

base_url <- 'https://github.com/alecruces/SustainableLithium/raw/refs/heads/main/data_base2/'

# Load Chilean lithium export data
lit.chl.exp <- read.xlsx(paste0(base_url, "litio_aus_chl.xlsx"), sheet = "lit_chl_exp")
lit.chl.exp$date <- as.Date(lit.chl.exp$date, origin = "1899-12-30")  # Fix the dates
lit.chl.exp$lit_chl_exp_kton_met <- lit.chl.exp$lit_chl_exp_klce / 1e6  # Convert to kilotons

# Load Australian lithium export data
lit.aus.exp <- read.xlsx(paste0(base_url, "litio_aus_chl.xlsx"), sheet = "lit_aus_exp")
lit.aus.exp$date <- as.Date(lit.aus.exp$date, origin = "1899-12-30")  # Fix the dates
lit.aus.exp$lit_aus_exp_kton_met <- lit.aus.exp$lit_aus_exp_kt_sp / 8  # Normalize the units
