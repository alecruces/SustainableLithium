# Powering the Future in a Sustainable Way: Lithium Analysis and Forecasting

> Comprehensive analysis and forecasting of the global lithium market, examining demand trends, export dynamics, and the impact of key market events on lithium prices and production.

<p align="center">
<img src="https://github.com/alecruces/lithium_forecasting./assets/67338986/27f62b48-c5d0-4249-8773-fe1897fd3735" alt="litio1" style="width:400px;height:auto;"/>
</p>

## Keywords
Time Series Analysis, Sustainable Energy, Forecasting Models

---

## Table of Contents

1. [About the Project](#about-the-project)
2. [Key Features](#key-features)
3. [Key Results](#key-results)
4. [Data Overview](#data-overview)
5. [Methodology](#methodology)
6. [Screenshots and Graphs](#screenshots-and-graphs)
7. [Technologies Used](#technologies-used)
8. [Setup & Installation](#setup--installation)
9. [Usage](#usage)
10. [Contributing](#contributing)
11. [License](#license)
12. [Contact](#contact)

---

### About the Project

This project provides a comprehensive analysis of the lithium market, focusing on demand and production trends, and uses various time series and forecasting models. The work examines lithium exports from Chile and Australia, factors affecting demand due to clean technology adoption, and future trends. Ultimately, it aims to provide insights into lithium’s role in sustainable energy and the market’s untapped potential.

### Key Features

- **Lithium Market Analysis**: Detailed study of lithium demand, production, and export patterns.
- **Forecasting Models**: Applied Holt’s Exponential Smoothing, ARIMA, ARMAX, and other advanced models to predict trends.
- **Comparison of Lithium Exports**: Focused on export dynamics of key producers, Chile and Australia.
- **Impact Assessment**: Examined how significant market events influence lithium prices and production.

### 3. Key Results

- **Forecasting Accuracy**:
  - For Australia: Holt’s Exponential Smoothing showed the best Mean Absolute Percentage Error (MAPE) at 13.64.
  - For Chile: ARMAX with explanatory variables (SQM, lithium battery trends, Mineral Resources stock) had the lowest MAPE.
- **Market Potential**: The Bass and Generalized Bass Models indicate the lithium market’s growth potential has not been fully realized, with future demand expected to increase moderately.
- **Influential Factors**: Key variables such as Google Trends for lithium batteries, Mineral Resources stock, and electric vehicle adoption significantly impact lithium demand.

### 4. Data Overview

The dataset includes the following sources and variables:
- **Lithium Export Data**: Kilotons of lithium exported from Australia and Chile.
- **Economic Indicators**: GDP data for Australia and Chile.
- **Energy Data**: Electric vehicle stock and charging infrastructure (global, by region).
- **Google Trends**: Search interest in e-cars, lithium, and batteries.
- **Stock Market Data**: Stock prices for companies like Albemarle, Mineral Resources, and SQM.

### 5. Methodology

The following methods and models were employed in this analysis:

- **Time Series Analysis**: Analyzed the trend and seasonality of lithium exports.
- **Bass and Generalized Bass Model (GBM)**: Estimated market potential and assessed shock impacts.
- **Competition Model**: Compared exports from Australia and Chile.
- **Forecasting Models**:
  - **Holt’s Exponential Smoothing**: Basic forecasting with emphasis on MAPE.
  - **KNN Regression**: Adapted for time series with lagged values.
  - **ARIMA and ARMAX**: Time series with drift and explanatory variables (like GDP and electric vehicle stock).

### 6. Screenshots and Graphs

Here are some key visualizations to consider including:
[Under Construction]

### 🛠️ Technologies Used

> Highlighting the primary tools and methods employed in this project.

- ![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white): Primary language for data processing, analysis, and forecasting.
- **📉 Time Series Analysis**: Used for trend and seasonality analysis in lithium exports.
- **🔮 Forecasting Models**: Implemented models including Holt’s Exponential Smoothing, ARIMA, ARMAX, and KNN Regression to predict lithium trends.

### 8. Setup & Installation

Clone the repository and follow these steps to run the analysis:

```bash
# Clone the repository
git clone https://github.com/username/lithium_forecasting.git

# Navigate to the project directory
cd lithium_forecasting

# Run the data preparation scripts
Rscript load_data.R
Rscript transform_data.R

# Run forecasting models
Rscript models.R     # Runs all models except ARIMA
Rscript arima_armax.R  # Runs ARIMA/ARMAX models
```
### 9. Usage

The repository includes the following R scripts:

- **`load_data.R`**: Downloads data from the GitHub repository.
- **`transform_data.R`**: Performs necessary data transformations.
- **`models.R`**: Executes forecasting models (excluding ARIMA).
- **`arima_armax.R`**: Runs ARIMA/ARMAX models.

To run the analysis, execute each script sequentially or as needed.

### 10. Contributing

Contributions are welcome! Please see the contributing guidelines for more details.



