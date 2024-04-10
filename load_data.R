########################################################################################
#################### LOADING DATA ###################################################
##########################################################################################

#Setting the path
#setwd("C:/Users/alecr/OneDrive/Escritorio/Master Data Science/Tercer Semestre/Financial Data/proyect")

###Libraries##################################
library(DIMORA)
library(dplyr)
library(forecast)
library(fpp2)
library(ggplot2)
library(hrbrthemes)
library(lmtest) 
library(lubridate)
library(openxlsx)
library(plotly)
library(readxl)
library(tidyverse)
library(tsibble)
library(zoo)
library(tsfknn)
library(FNN)
##############################################

# The url where the data sets are
base_url = 'https://raw.githubusercontent.com/jchaconm/UNIPD_BEFD_Lithium/main/'


# Fast and slow chargers (for 2022)
fast.slow.ch.22 <- read.xlsx(paste(base_url, file = "fast_slow_chargers_2022.xlsx", sep = ""))

# Fast chargers
fast.ch <- read.xlsx(paste(base_url, file = "fast_chargers.xlsx", sep = ""))
# Add another row with 2022 information from another dataset
fast.ch[nrow(fast.ch) + 1,] = c(2022, fast.slow.ch.22[1, "Fast"], fast.slow.ch.22[2, "Fast"], fast.slow.ch.22[3, "Fast"], fast.slow.ch.22[4, "Fast"])
par(mfrow=c(2,2))
plot(fast.ch$Year, fast.ch$China)
plot(fast.ch$Year, fast.ch$United_States)
plot(fast.ch$Year, fast.ch$Europe)
plot(fast.ch$Year, fast.ch$Other_countries)

# Slow chargers
slow.ch <- read.xlsx(paste(base_url, file = "slow_chargers.xlsx", sep = ""))
# Add another row with 2022 information from another dataset
slow.ch[nrow(slow.ch) + 1,] = c(2022, fast.slow.ch.22[1, "Slow"], fast.slow.ch.22[2, "Slow"], fast.slow.ch.22[3, "Slow"], fast.slow.ch.22[4, "Slow"])
par(mfrow=c(2,2))
plot(slow.ch$Year, slow.ch$China)
plot(slow.ch$Year, slow.ch$United_States)
plot(slow.ch$Year, slow.ch$Europe)
plot(slow.ch$Year, slow.ch$Other_countries)

# Global cumulative PV capacity (solar panels)
cum.pv.cap <- read.xlsx(paste(base_url, file = "cum_pv_capacity.xlsx", sep = ""))
plot(cum.pv.cap$Year, cum.pv.cap$MW)

# Global pv investment
pv.inv <- read.xlsx(paste(base_url, file = "pv_investment.xlsx", sep = ""))
plot(pv.inv$Year, pv.inv$Investment_billion_USD)

# Global Smart Devices sells  (no est? muy bonita esta data)
smart.dev.sales <- read.xlsx(paste(base_url, file = "smart_dev_sales.xlsx", sep = ""))
par(mfrow=c(1,3))
plot(smart.dev.sales$Year, smart.dev.sales$Smartphones)
plot(smart.dev.sales$Year, smart.dev.sales$Tablets)
plot(smart.dev.sales$Year, smart.dev.sales$VR_devices)



# Economic data

# DESCRIPTION
gdp_chl_current_prices <- read.xlsx(paste(base_url, file = "economic_data.xlsx", sep = ""), sheet = "gdp_chl_current_prices")
gdp_chl_current_prices$date <- as.Date(gdp_chl_current_prices$date, origin = "1899-12-30")
plot(gdp_chl_current_prices$date, gdp_chl_current_prices$gdp_chl_current_prices)

# DESCRIPTION
gdp_chl_chain <- read.xlsx(paste(base_url, file = "economic_data.xlsx", sep = ""), sheet = "gdp_chl_chain")
gdp_chl_chain$date <- as.Date(gdp_chl_chain$date, origin = "1899-12-30")
plot(gdp_chl_chain$date, gdp_chl_chain$gdp_chl_chain)

# DESCRIPTION
gdp_chl_desest <- read.xlsx(paste(base_url, file = "economic_data.xlsx", sep = ""), sheet = "gdp_chl_desest")
gdp_chl_desest$date <- as.Date(gdp_chl_desest$date, origin = "1899-12-30")
plot(gdp_chl_desest$date, gdp_chl_desest$gdp_chl_desest)

# DESCRIPTION
chl_ipc <- read.xlsx(paste(base_url, file = "economic_data.xlsx", sep = ""), sheet = "chl_ipc")
chl_ipc$date <- as.Date(chl_ipc$date, origin = "1899-12-30")
plot(chl_ipc$date, chl_ipc$chl_ipc)

# DESCRIPTION
chl_ipc_nv <- read.xlsx(paste(base_url, file = "economic_data.xlsx", sep = ""), sheet = "chl_ipc_nv")
chl_ipc_nv$date <- as.Date(chl_ipc_nv$date, origin = "1899-12-30")
plot(chl_ipc_nv$date, chl_ipc_nv$chl_ipc_nv)

# DESCRIPTION
chl_pop <- read.xlsx(paste(base_url, file = "economic_data.xlsx", sep = ""), sheet = "chl_pop")
chl_pop$date <- as.Date(chl_pop$date, origin = "1899-12-30")
plot(chl_pop$date, chl_pop$chl_pop)

# DESCRIPTION
chl_working_pop <- read.xlsx(paste(base_url, file = "economic_data.xlsx", sep = ""), sheet = "chl_working_pop")
chl_working_pop$date <- as.Date(chl_working_pop$date, origin = "1899-12-30")
plot(chl_working_pop$date, chl_working_pop$chl_working_pop)

# DESCRIPTION
aus_working_pop <- read.xlsx(paste(base_url, file = "economic_data.xlsx", sep = ""), sheet = "aus_working_pop")
aus_working_pop$date <- as.Date(aus_working_pop$date, origin = "1899-12-30")
plot(aus_working_pop$date, aus_working_pop$aus_working_pop)

# DESCRIPTION
gdp_aus_current_prices <- read.xlsx(paste(base_url, file = "economic_data.xlsx", sep = ""), sheet = "gdp_aus_current_prices")
gdp_aus_current_prices$date <- as.Date(gdp_aus_current_prices$date, origin = "1899-12-30")
plot(gdp_aus_current_prices$date, gdp_aus_current_prices$gdp_aus_current_prices)

# DESCRIPTION
gdp_aus_chain <- read.xlsx(paste(base_url, file = "economic_data.xlsx", sep = ""), sheet = "gdp_aus_chain")
gdp_aus_chain$date <- as.Date(gdp_aus_chain$date, origin = "1899-12-30")
plot(gdp_aus_chain$date, gdp_aus_chain$gdp_aus_chain)

# DESCRIPTION
aus_pop <- read.xlsx(paste(base_url, file = "economic_data.xlsx", sep = ""), sheet = "aus_pop")
plot(aus_pop$date, aus_pop$aus_pop)

#E-cars Google Trends interest
ecars.gtrends <- read.xlsx(paste(base_url, file = 'ecars_gtrends.xlsx', sep = ""))
ecars.gtrends$date <- as.Date(ecars.gtrends$month, origin = "1899-12-30")

#Lithium Google Trends interest
lithium.gtrends <- read.xlsx(paste(base_url, file = 'lithium_gtrends.xlsx', sep = ""))
lithium.gtrends$date <- as.Date(lithium.gtrends$month, origin = "1899-12-30")

#Lithium batteries Google Trends interest
li.batteries.gtrends <- read.xlsx(paste(base_url, file = 'li_batteries_gtrends.xlsx', sep = ""))
li.batteries.gtrends$date <- as.Date(li.batteries.gtrends$month, origin = "1899-12-30")


#Lithium prices
lithium.prices <- read.xlsx(paste(base_url, file = 'lithium_prices.xlsx', sep = ""))
lithium.prices$date <- as.Date(lithium.prices$date, origin = "1899-12-30")


#Stock prices
#Albermarle
albemarle.stock <- read.xlsx(paste(base_url, file = 'albemarle_stock.xlsx', sep = ""), sheet = "daily")
albemarle.stock$date <- as.Date(albemarle.stock$date, origin = "1899-12-30")

#BYD 
byd.stock <- read.xlsx(paste(base_url, file = 'byd_stock.xlsx', sep = ""), sheet = "daily")
byd.stock$date <- as.Date(byd.stock$date, origin = "1899-12-30")

#Ganfeng
ganfeng.stock <- read.xlsx(paste(base_url, file = 'ganfeng_stock.xlsx', sep = ""), sheet = "daily")
ganfeng.stock$date <- as.Date(ganfeng.stock$date, origin = "1899-12-30")

#SQM
sqm.stock <- read.xlsx(paste(base_url, file = 'sqm_stock.xlsx', sep = ""), sheet = "daily")
sqm.stock$date <- as.Date(sqm.stock$date, origin = "1899-12-30")

#Mineral Resources
mineral.resources.stock <- read.xlsx(paste(base_url, file = 'mineral_resources_stock.xlsx', sep = ""), sheet = "daily")
mineral.resources.stock$date <- as.Date(mineral.resources.stock$date, origin = "1899-12-30")



#Electric vehicles
ev.world.stock <- read.xlsx(paste(base_url, file = "ev.xlsx", sep = ""), sheet = "ev_world_stock")
ev.monthly <- read.xlsx(paste(base_url, file = "ev.xlsx", sep = ""), sheet = "ev_monthly")

#Lithium data
lit.chl.prod <- read.xlsx(paste(base_url, file = "litio_aus_chl.xlsx", sep = ""), sheet = "lit_chl_prod")
lit.chl.prod$lit_chl_prod_kton_met = lit.chl.prod$lit_chl_prod_ton_met/1000
plot(lit.chl.prod$date, lit.chl.prod$lit_chl_prod_kton_met)
# This first data does not consider production of lithium sulfate, which could be the reason that it does
# not match the exportation data from Aduana

lit.chl.exp.dolar <- read.xlsx(paste(base_url, file = "litio_aus_chl.xlsx", sep = ""), sheet = "lit_chl_exp_dolar")
lit.chl.exp.dolar$date <- as.Date(lit.chl.exp.dolar$date, origin = "1899-12-30")
plot(lit.chl.exp.dolar$date, lit.chl.exp.dolar$lit_chl_exp_dolar)

lit.chl.exp <- read.xlsx(paste(base_url, file = "litio_aus_chl.xlsx", sep = ""), sheet = "lit_chl_exp")
lit.chl.exp$lit_chl_exp_kton_met = lit.chl.exp$lit_chl_exp_klce/1000000
lit.chl.exp$date <- as.Date(lit.chl.exp$date, origin = "1899-12-30")
plot(lit.chl.exp$date, lit.chl.exp$lit_chl_exp_kton_met)

# For the lithium production, spodumene typically has a concentration of lithium carbonate LiO2
# between 6% and 7%. (Source: https://www.sgs.com/-/media/sgscorp/documents/corporate/brochures/sgs-min-wa109-hard-rock-lithium-processing-en.cdn.en.pdf)
lit.aus.prod <- read.xlsx(paste(base_url, file = "litio_aus_chl.xlsx", sep = ""), sheet = "lit_aus_prod")
lit.aus.prod$date <- as.Date(lit.aus.prod$date, origin = "1899-12-30")
lit.aus.prod$lit_aus_prod_kton_met = lit.aus.prod$lit_aus_prod_kt_sp / 8
plot(lit.aus.prod$date, lit.aus.prod$lit_aus_prod_kton_met)

lit.aus.exp <- read.xlsx(paste(base_url, file = "litio_aus_chl.xlsx", sep = ""), sheet = "lit_aus_exp")
lit.aus.exp$date <- as.Date(lit.aus.exp$date, origin = "1899-12-30")
lit.aus.exp$lit_aus_exp_kton_met = lit.aus.exp$lit_aus_exp_kt_sp / 8
plot(lit.aus.exp$date, lit.aus.exp$lit_aus_exp_kton_met)

lit.aus.exp.mdolar <- read.xlsx(paste(base_url, file = "litio_aus_chl.xlsx", sep = ""), sheet = "lit_aus_exp_mdolar")
lit.aus.exp.mdolar$date <- as.Date(lit.aus.exp.mdolar$date, origin = "1899-12-30")
plot(lit.aus.exp.mdolar$date, lit.aus.exp.mdolar$lit_aus_exp_mdolar)

par(mfrow = c(2,2))
plot(lit.chl.prod$date, lit.chl.prod$lit_chl_prod_kton_met, type = "b")
plot(lit.chl.exp$date, lit.chl.exp$lit_chl_exp_kton_met, type = "b")
plot(lit.aus.prod$date, lit.aus.prod$lit_aus_prod_kton_met, type = "b")
plot(lit.aus.exp$date, lit.aus.exp$lit_aus_exp_kton_met, type = "b")
