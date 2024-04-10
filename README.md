# Powering the Future in a Sustainable way: Lithium Analysis and Forecasting

---

## Description
This work presents a comprehensive analysis of the global lithium market, focusing on demand factors, production trends, and forecasting models. It highlights the increasing demand for lithium due to the transition to electric transport and the expansion of clean technology. The study compares lithium exports from Chile and Australia, examines the impact of market events on prices and production, and utilizes various forecasting models to predict future trends. The findings suggest that the market potential for lithium has not yet been reached, with expectations of moderate to increasing demand. The work concludes with insights into the future of lithium extraction and market competition.

<p align="center">
<img src="https://github.com/alecruces/lithium_forecasting./assets/67338986/27f62b48-c5d0-4249-8773-fe1897fd3735" alt="litio1" style="width:400px;height:auto;"/>
</p>

## Keywords
Time Series Analysis, Sustainable Energy, Forecasting Models

## Data
The database is constructed with the following information:
- Exportation Lithium Data: Lithium in kilotons from Australia and Chile (Source: Australian Government’s Department of Industry, Science and Resources and National Customs Service of Chile).
- Economic data: GDP Australia and Chile (Source: Central Bank of Chile and Reserve Bank of Australia)
- Energy Related data: Electric vehicles Stock (China, Europe, USA, Total), Fast and Slow chargers (China, Europe, USA, Total), Solar investment (Source: IEA and Statista)
- Google Trends: E-cars, Lithium and Lithium Batteries from AUS, CHL, World (Source: Google Trends)
- Stock Market: Albemarle, Mineral Resources and SQM (Source: Yahoo! Finance).

## Methods
- Time series analysis
- Bass Model: Utilized to understand the general growth of lithium up to now.
- Generalized Bass Model (GBM): Shows how shocks impact the dynamics of lithium exports.
- Competition model: To compare the competition between Australia and Chile
- Holt's Exponential Smoothing with and without damping: Applied for forecasting, with a focus on the Mean Absolute Percentage Error (MAPE).
- K-Nearest Neighbors (KNN) Regression: Adapted for time series forecasting using lagged values.
- Autoregressive Integrated Moving Average (ARIMA): Used for time series forecasting with a drift component.
- Autoregressive Moving Average with eXogenous inputs (ARMAX): Incorporates explanatory variables like GDP, electric vehicle stock, and solar investment for forecasting.

## Software
R

## Files
* Code:
  
  Explanation about R files:
  
  - `load_data.R`: Downloads the data from a Github repository

  - `transform_data.R`: Manipulates data and performs all the transformations needed for the analysis

  - `models.R`: executes all the models except for ARIMA. It uses load_data and transform_data files

  - `arima_armax.R`: executes ARIMA/ARMAX models. It uses load_data and transform_data files


  How to run:

  1. Run `load_data.R` and `transform_data.R`
  2. Open `models.R` or `arima_armax.R`
  3. Run the selected R script
* Presentation

---
