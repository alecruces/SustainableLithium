# Medium Article
# Authors: Alejandra Cruces and Mario Tapia
# A Step-by-Step Guide to Real-World Data and Forecasting Techniques in R 
# Part 2: Uncovering Growth Secrets with the Bass Model 

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

## Step 0: Import Lithium Export Data and Transform the Data

base_url <- 'https://github.com/alecruces/SustainableLithium/raw/refs/heads/main/data_base/'

# Load Chilean lithium export data
lit.chl.exp <- read.xlsx(paste0(base_url, "litio_aus_chl.xlsx"), sheet = "lit_chl_exp")
lit.chl.exp$date <- as.Date(lit.chl.exp$date, origin = "1899-12-30")  # Fix the dates
lit.chl.exp$lit_chl_exp_kton_met <- lit.chl.exp$lit_chl_exp_klce / 1e6  # Convert to kilotons

# Load Australian lithium export data
lit.aus.exp <- read.xlsx(paste0(base_url, "litio_aus_chl.xlsx"), sheet = "lit_aus_exp")
lit.aus.exp$date <- as.Date(lit.aus.exp$date, origin = "1899-12-30")  # Fix the dates
lit.aus.exp$lit_aus_exp_kton_met <- lit.aus.exp$lit_aus_exp_kt_sp / 8  # Normalize the units

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
chilean_exports <- aggregate(lit_chl_exp_kton_met ~ quarter, data = lit.chl.exp, sum)

# Aggregate Australian export data by quarter
lit.aus.exp$quarter <- as.yearqtr(lit.aus.exp$date)
australian_exports <- aggregate(lit_aus_exp_kton_met ~ quarter, data = lit.aus.exp, sum)

# Merge Chilean exports with full quarters
final_data <- merge(full_quarters_df, chilean_exports, by.x = "quarter", by.y = "quarter", all.x = TRUE)
names(final_data)[names(final_data) == "lit_chl_exp_kton_met"] <- "chilean_exports"

# Merge Australian exports with full quarters
final_data <- merge(final_data, australian_exports, by.x = "quarter", by.y = "quarter", all.x = TRUE)
names(final_data)[names(final_data) == "lit_aus_exp_kton_met"] <- "australian_exports"


## Step 1: Fitting the Bass Model


### Step 2: Visualizing Instantaneous Exports

# Prepare instantaneous fitted values
fitted.bm.inst <- make.instantaneous(fitted(bm.ch.exp))
fitted.bm.ts <- ts(fitted.bm.inst, frequency = 4, start = 2014)

# Plot instantaneous data
plot(chl_ts, type = "l", col = "black", lwd = 2,
     main = "Instantaneous Export Trends and Bass Model Predictions (Chile)", xlab = "Quarter", ylab = "kT")
lines(fitted.bm.ts, col = "#00C3B1", lwd = 2, type = "l", lty = "dashed")


## Step 3: Validating the Model with Residuals

# Check residuals of the Bass Model
checkresiduals(bm.ch.exp)

## Step 4: Forecasting Future Growth with the Bass Model


# Forecast exports
pred.years <- 1  # Forecast 1 year ahead
pred.x.len <- length(chl_ts) + pred.years * 4
pred.x.end <- 2014 + pred.x.len / 4

pred.bm.ch.exp <- predict(bm.ch.exp, newx = c(1:pred.x.len))
pred.inst.ch.exp <- make.instantaneous(pred.bm.ch.exp)
pred.bm.ts <- ts(pred.inst.ch.exp, frequency = 4, start = 2014)

# Plot forecast
par(bg = "white")
plot(chl_ts, ylab = "kT", main = "Instantaneous Export Trends and Bass Model Forecast (Chile)",
     xlab = "Quarter", xlim = c(2014, pred.x.end),
     lwd = 2, col = "black")
lines(pred.bm.ts, col = "#00C3B1", lwd = 2, type = "l", lty = "dashed")
abline(v = 2023.5, col = "gray", lwd = 2, lty = 3)

## Step 5: Incorporating Shocks with the Generalized Bass Model (GMB)

# Fit the Generalized Bass Model with mixed shocks
GBM_chl_exp_mix <- GBM(
  chl_ts, oos = 4, shock = "mixed", nshock = 2,
  prelimestimates = c(5.672961e+04, 2.953943e-04, 2.662497e-02, 25, 0.1, 0.1, 7, 14, 0.1)
) # # These preliminary estimates come from the Bass Model parameters and our visual interpretation of the shocks
summary(GBM_chl_exp_mix)
checkresiduals(GBM_chl_exp_mix)


# Plot Instantaneous data with GMB
plot(chl_ts, type = "l", col = "black", lwd = 2,
     main = "Instantaneous Export Trends with Shocks (GBM) - Chile", xlab = "Quarter", ylab = "kT", xlim = c(2014, 2024))
lines(
  ts(make.instantaneous(predict(GBM_chl_exp_mix, newx = c(1:(length(aus_ts_2010) + 4)))), frequency = 4, start = 2014), col = "#00C3B1", lwd = 2, type = "l", lty = "dashed")
abline(v = 2023.5, col = "gray", lwd = 2, lty = 3)



## Step 6: Exploring Chile vs. Australia with the Competition Model

# Transforming Australia exports into a time series object
aus_ts <- ts(final_data$australian_exports[17:55], start = c(2014, 1), frequency = 4) # Same time period as Chile
# Fit the Competition Model
bass_comp <- UCRCD(aus_ts, chl_ts, display = TRUE)
summary(bass_comp)

inst.comp.fit <-  fitted(bass_comp)
inst.comp.fit.chl <-  ts(make.instantaneous(inst.comp.fit[[2]]), start=2014, frequency = 4) 
inst.comp.fit.aus <-  ts(make.instantaneous(inst.comp.fit[[1]]), start=2014, frequency = 4) 

plot(aus_ts,  type = "l", col = "black", lwd = 2,
     main = "Instantaneous Export Trends - Competition Model", xlab = "Quarter", ylab = "kT")
lines(chl_ts,  type = "l", col = "grey", lwd = 2)
lines(inst.comp.fit.chl, col = "#00C3B1", lwd = 2, type = "l", lty = "dashed") 
lines(inst.comp.fit.aus , col = "purple", lwd = 2, type = "l", lty = "dashed")
legend("topleft", c("Chile", "Australia"),
       col = c("#00C3B1", "purple"), lty = c(1, 2))



## Homework: Modeling Australian Lithium Exports

## Step 1: Fitting the Bass Model

# Fit the Bass Model to Australian exports
aus_ts_2010 <- ts(final_data$australian_exports, start = c(2010, 1), frequency = 4)  # Remove NA values

bm.aus.exp <- BM(aus_ts_2010, display = TRUE)  # Fit the model
summary(bm.aus.exp)  # View model summary


# Prepare instantaneous fitted values
fitted.bm.inst <- make.instantaneous(fitted(bm.aus.exp))
fitted.bm.ts <- ts(fitted.bm.inst, frequency = 4, start = 2010)

# Plot instantaneous data
plot(
  aus_ts, type = "l", col = "black", lwd = 2,
  main = "Instantaneous Export Trends and Bass Model Predictions (Australia)", 
  xlab = "Quarter", ylab = "kT"
)
lines(
  fitted.bm.ts, col = "#00C3B1", lwd = 2, type = "l", lty = "dashed"
)


## Step 2: Forecasting with the Bass Model

# Forecast exports
pred.years <- 1  # Forecast 1 year ahead
pred.x.len <- length(aus_ts_2010) + pred.years * 4
pred.x.end <- 2010 + pred.x.len / 4

pred.bm.aus.exp <- predict(bm.aus.exp, newx = c(1:pred.x.len))
pred.inst.aus.exp <- make.instantaneous(pred.bm.aus.exp)
pred.bm.ts <- ts(pred.inst.aus.exp, frequency = 4, start = 2010)

# Plot forecast
par(bg = "white")
plot(
  aus_ts_2010, ylab = "kT", main = "Instantaneous Export Trends and Bass Model Forecast (Australia)", 
  xlab = "Quarter", xlim = c(2010, pred.x.end), 
  lwd = 2, col = "black"
)
lines(
  pred.bm.ts, col = "#00C3B1", lwd = 2, type = "l", lty = "dashed"
)
abline(v = 2023.5, col = "gray", lwd = 2, lty = 3)



## Step 3: Generalized Bass Model (GBM) with Double Rectangular Shock

# Fit the Generalized Bass Model with double rectangular shock
GBM_aus_exp_rett2 <- GBM(
  aus_ts_2010, 
  oos = 4, 
  shock = "rett", 
  nshock = 2, 
  prelimestimates = c(5.742860e+03, 1.144887e-03, 6.183144e-02, 29, 40, 0.1, 46, 48, -0.1)
)

# Display model summary
summary(GBM_aus_exp_rett2)

# Check residuals
checkresiduals(GBM_aus_exp_rett2)

# Plot instantaneous data with GBM
plot(
  aus_ts_2010,  
  type = "l", col = "black", lwd = 2,
  main = "Instantaneous Export Trends with Shocks (GBM) - Australia", 
  xlab = "Quarter", ylab = "kT", xlim = c(2010, 2024)
)

lines(
  ts(make.instantaneous(predict(GBM_aus_exp_rett2, newx = c(1:(length(aus_ts_2010) + 4)))), 
     frequency = 4, start = 2010), 
  col = "#00C3B1", lwd = 2, type = "l", lty = "dashed"
)
