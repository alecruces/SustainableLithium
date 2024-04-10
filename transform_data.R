########################################################################################
#################### DATA TRANFORMATION ################################################
########################################################################################


###################################################################
# First: Create a Dataframe to store of Explanatory variables
# quarterly
###################################################################

quarter <- seq(from = min(as.yearqtr(as.Date(paste(2009, 1, 1,sep = "-")))), 
                to = max(as.yearqtr(as.Date(paste(2023, 12, 1, sep = "-")))), by = 0.25)

explanatory_variables <- as.data.frame(quarter)

###################################################################
# 1. Lithium
###################################################################

###############################################
### Exports: Quarterly ###
###############################################

# Exports from Chile to quarterly data
lit.chl.exp$quarter <- as.yearqtr(lit.chl.exp$date, format = "%Y-%m-%d", fiscal_start = 1)
lit.chl.exp.quarter <- lit.chl.exp %>% 
  group_by(quarter) %>% 
  summarise(lit_chl_exp_kton_met = sum(lit_chl_exp_kton_met)) %>% 
  as.data.frame()

lit.aus.exp$quarter <- as.yearqtr(lit.aus.exp$date, format = "%Y-%m-%d", fiscal_start = 1)

lit.total.exp.quarter <- merge(lit.chl.exp.quarter, lit.aus.exp, by = "quarter", all = TRUE)
lit.total.exp.quarter$lit.total.exp.kton <-  lit.total.exp.quarter$lit_chl_exp_kton_met + lit.total.exp.quarter$lit_aus_exp_kton_met
lit.total.exp.quarter <- subset(lit.total.exp.quarter, select = c(quarter, lit_chl_exp_kton_met, lit_aus_exp_kton_met, lit.total.exp.kton))

#Merge with data frame explanatory variables
explanatory_variables <- merge(explanatory_variables, lit.total.exp.quarter, by = "quarter", all = TRUE)

###########################################################################
## Taking the series
#Chile
lit.chl.exp.quarter.kton <- lit.chl.exp.quarter$lit_chl_exp_kton_met

#Australia
lit.aus.exp.quarter.kton <- lit.aus.exp$lit_aus_exp_kton_met
lit.aus.exp.mdolar$lit_aus_exp_price <- lit.aus.exp.mdolar$lit_aus_exp_mdolar/lit.aus.exp$lit_aus_exp_kton_met
lit.aus.exp.mdolar$quarter<- as.yearqtr(lit.aus.exp.mdolar$date, format = "%Y-%m-%d", fiscal_start = 1)

lit.aus.exp.mdolar <- lit.aus.exp.mdolar %>%
  mutate(lit_aus_exp_price_var = lag(lit_aus_exp_price,4)) %>%
  mutate(lit_aus_exp_price_var = (lit_aus_exp_price_var-lit_aus_exp_price )/lit_aus_exp_price ) %>%
  na.omit()


#Total
lit.total.exp <-  lit.total.exp.quarter$lit.total.exp.kton

###################################################################
# 2. GDP
###################################################################


###############################################
### GDP: CHILE ###
###############################################

# Transform GDP to real terms, look if we have to use gdp chain or current
# Create the quarter variable
gdp_chl_current_prices$quarter <- as.yearqtr(gdp_chl_current_prices$date, format = "%Y-%m-%d", fiscal_start = 1)
gdp_chl_chain$quarter <- as.yearqtr(gdp_chl_chain$date, format = "%Y-%m-%d", fiscal_start = 1)
gdp_chl_desest$quarter <- as.yearqtr(gdp_chl_desest$date, format = "%Y-%m-%d", fiscal_start = 1)


#We will keep only using gdp_chl_chain because is in real terms

chl_working_pop$quarter <- as.yearqtr(chl_working_pop$date, format = "%Y-%m-%d", fiscal_start = 1)
chl_working_pop <- subset(chl_working_pop, date != "2010-03-01")

chl.working.pop.quarter <- chl_working_pop%>% 
  group_by(quarter) %>% 
  summarise(chl_working_pop = sum(chl_working_pop)) %>% 
  as.data.frame()

gdp_chl_chain  <- merge(gdp_chl_chain, chl.working.pop.quarter, by = "quarter")
gdp_chl_chain <- merge(lit.total.exp.quarter, gdp_chl_chain, by = "quarter")

gdp_chl_chain$gdp_chl_pc = gdp_chl_chain$gdp_chl_chain / gdp_chl_chain$chl_working_pop


# Variation
gdp_chl_chain <- gdp_chl_chain %>%
  mutate(gdp_chl_var = lag(gdp_chl_chain,4)) %>%
  mutate(gdp_chl_pc_var = lag(gdp_chl_pc,4)) %>%
  mutate(gdp_chl_var = (gdp_chl_chain-gdp_chl_var)/gdp_chl_chain) %>%
  mutate(gdp_chl_pc_var = (gdp_chl_pc-gdp_chl_pc_var)/gdp_chl_pc)


#Merge with data frame explanatory variables
explanatory_variables <- merge(explanatory_variables, 
                               subset(gdp_chl_chain, select = c(quarter, gdp_chl_pc,gdp_chl_var,gdp_chl_pc_var)), 
                               by = "quarter", all = TRUE)


###############################################
### GDP: AUS ###
###############################################

gdp_aus_current_prices$quarter <- as.yearqtr(gdp_aus_current_prices$date, format = "%Y-%m-%d", fiscal_start = 1)
gdp_aus_chain$quarter <- as.yearqtr(gdp_aus_chain$date, format = "%Y-%m-%d", fiscal_start = 1)

gdp_aus_chain$quarter <- as.yearqtr(gdp_aus_chain$date, format = "%Y-%m-%d", fiscal_start = 1)
aus_working_pop$quarter <- as.yearqtr(aus_working_pop$date, format = "%Y-%m-%d", fiscal_start = 1)

gdp_aus_chain <- merge(gdp_aus_chain, aus_working_pop, by = "quarter")
gdp_aus_chain <- merge(lit.total.exp.quarter, gdp_aus_chain, by = "quarter")

gdp_aus_chain$gdp_aus_pc = gdp_aus_chain$gdp_aus_chain / gdp_aus_chain$aus_working_pop

plot(gdp_aus_chain$quarter,gdp_aus_chain$gdp_aus_pc, type = "l", col = "red")
acf(gdp_aus_chain$gdp_aus_pc)

# Variation
gdp_aus_chain <- gdp_aus_chain %>%
  mutate(gdp_aus_var = lag(gdp_aus_chain,4)) %>%
  mutate(gdp_aus_pc_var = lag(gdp_aus_pc,4)) %>%
  mutate(gdp_aus_var = (gdp_aus_chain-gdp_aus_var)/gdp_aus_chain) %>%
  mutate(gdp_aus_pc_var = (gdp_aus_pc-gdp_aus_var)/gdp_aus_chain)

#Merge with data frame explanatory variables
explanatory_variables <- merge(explanatory_variables, 
                               subset(gdp_aus_chain, select = c(quarter, gdp_aus_pc,gdp_aus_var,gdp_aus_pc_var)), 
                               by = "quarter", all = TRUE)


###################################################################
# 3. Electric Vehicles 
###################################################################

###############################################
### EV STOCK ###
###############################################

quarter <- seq(from = min(ev.world.stock$date), to = max(ev.world.stock$date), by = 0.25)

quarter <- as.data.frame(seq(from = min(as.yearqtr(as.Date(paste(ev.world.stock$date, 1, 1,sep = "-")))), 
                to = max(as.yearqtr(as.Date(paste(ev.world.stock$date, 12, 1, sep = "-")))), by = 0.25))

PHEV.stock <- cumsum(rep(c(ev.world.stock$PHEV[1], diff(ev.world.stock$PHEV))/4, each = 4))
BEV.stock <- cumsum(rep(c(ev.world.stock$BEV[1], diff(ev.world.stock$BEV))/4, each = 4))
ev.stock <- as.data.frame(cbind(quarter, PHEV.stock, BEV.stock))
colnames(ev.stock)[1] <- "quarter"
ev.stock <- merge(ev.stock, gdp_aus_chain, by ="quarter" )

#Merge with data frame explanatory variables
explanatory_variables <- merge(explanatory_variables, 
                               subset(ev.stock, select = c(quarter, PHEV.stock,BEV.stock)), 
                               by = "quarter", all = TRUE)


#EV monthly
quarter <- seq(from = min(as.yearqtr(as.Date(paste(2020, 10, 1,sep = "-")))), 
                to = max(as.yearqtr(as.Date(paste(2023, 4, 1, sep = "-")))), by = 1/12)
quarter <- subset(quarter, quarter != "2023 Q2")

ev.monthly <- as.data.frame(cbind(ev.monthly, quarter))

ev.quarterly <- ev.monthly %>% 
  group_by(quarter) %>% 
  summarise_at(c("China_BEV", "China_PHEV", "USA_BEV", "USA_PHEV", "EV_CHN", "EV_USA"), sum) %>% 
  as.data.frame()

#Merge with data frame explanatory variables
explanatory_variables <- merge(explanatory_variables,ev.quarterly,
                               by = "quarter", all = TRUE)

#################################################
### 4. Fast and slow chargers (for 2022) ###
##################################################

###############################################
### FAST CHARGERS ###
###############################################

# Add another row with 2022 information from another dataset
#fast.ch[nrow(fast.ch) + 1,] = c(2022, fast.slow.ch.22[1, "Fast"], fast.slow.ch.22[2, "Fast"], 
#                                fast.slow.ch.22[3, "Fast"], fast.slow.ch.22[4, "Fast"])

# Transforming to quarter

# Create a sequence of quarters for time variable and also for the values of the time series
# by considering the fourth part of the value for each quarter

quarter <- seq(from = min(as.yearqtr(as.Date(paste(fast.ch$Year, 1, 1,sep = "-")))), 
                to = max(as.yearqtr(as.Date(paste(fast.ch$Year, 12, 1, sep = "-")))), by = 0.25)

fast.ch.q.Ch <- cumsum(rep(c(fast.ch$China[1], diff(fast.ch$China))/4, each = 4))
fast.ch.q.EU <- cumsum(rep(c(fast.ch$Europe[1], diff(fast.ch$Europe))/4, each = 4))
fast.ch.q.US <- cumsum(rep(c(fast.ch$United_States[1], diff(fast.ch$United_States))/4, each = 4))
fast.ch.q.Ot <- cumsum(rep(c(fast.ch$Other_countries[1], diff(fast.ch$Other_countries))/4, each = 4))
fast.ch.q.total <-fast.ch.q.Ch + fast.ch.q.EU +fast.ch.q.US +fast.ch.q.Ot
fast_charger <- cbind(as.data.frame(quarter), fast.ch.q.Ch, fast.ch.q.EU, fast.ch.q.US, fast.ch.q.Ot, fast.ch.q.total)

#Merge with data frame explanatory variables
explanatory_variables <- merge(explanatory_variables,fast_charger,by = "quarter", all = TRUE)

###############################################
### SLOW CHARGERS ###
###############################################

# Add another row with 2022 information from another dataset
#slow.ch[nrow(slow.ch) + 1,] = c(2022, fast.slow.ch.22[1, "Slow"], fast.slow.ch.22[2, "Slow"], 
#                                fast.slow.ch.22[3, "Slow"], fast.slow.ch.22[4, "Slow"])

# Transforming to quarter

# Create a sequence of quarters for time variable and also for the values of the time series
# by considering the fourth part of the value for each quarter

quarter <- seq(from = min(as.yearqtr(as.Date(paste(slow.ch$Year, 1, 1,sep = "-")))), 
                to = max(as.yearqtr(as.Date(paste(slow.ch$Year, 12, 1, sep = "-")))), by = 0.25)

slow.ch.q.Ch <- cumsum(rep(c(slow.ch$China[1], diff(slow.ch$China))/4, each = 4))
slow.ch.q.EU <- cumsum(rep(c(slow.ch$Europe[1], diff(slow.ch$Europe))/4, each = 4))
slow.ch.q.US <- cumsum(rep(c(slow.ch$United_States[1], diff(slow.ch$United_States))/4, each = 4))
slow.ch.q.Ot <- cumsum(rep(c(slow.ch$Other_countries[1], diff(slow.ch$Other_countries))/4, each = 4))

slow_charger <- cbind(as.data.frame(quarter), slow.ch.q.Ch, slow.ch.q.EU, slow.ch.q.US, slow.ch.q.Ot)

#Merge with data frame explanatory variables
explanatory_variables <- merge(explanatory_variables,slow_charger,by = "quarter", all = TRUE)

###############################################
### Global cumulative PV capacity (solar panels) ###
###############################################

# Transforming to quarter

# Create a sequence of quarters for time variable and also for the values of the time series
# by considering the fourth part of the value for each quarter

quarter <- seq(from = min(as.yearqtr(as.Date(paste(cum.pv.cap$Year, 1, 1,sep = "-")))), 
                to = max(as.yearqtr(as.Date(paste(cum.pv.cap$Year, 12, 1, sep = "-")))), by = 0.25)

cum.pv.cap.q <- cumsum(rep(c(cum.pv.cap$MW[1], diff(cum.pv.cap$MW))/4, each = 4))

cum_pv_cap_q <- cbind(as.data.frame(quarter), cum.pv.cap.q)

#Merge with data frame explanatory variables
explanatory_variables <- merge(explanatory_variables,cum_pv_cap_q,by = "quarter", all = TRUE)


###############################################
### Global pv investment ###
###############################################

# Transforming to quarter

# Create a sequence of quarters for time variable and also for the values of the time series
# by considering the fourth part of the value for each quarter
quarter <- seq(from = min(as.yearqtr(as.Date(paste(pv.inv$Year, 1, 1,sep = "-")))), 
                to = max(as.yearqtr(as.Date(paste(pv.inv$Year, 12, 1, sep = "-")))), by = 0.25)

pv.inv.q <- cumsum(rep(c(pv.inv$Investment_billion_USD[1], diff(pv.inv$Investment_billion_USD))/4, each = 4))

pv_inv_q  <- cbind(as.data.frame(quarter), pv.inv.q)

#Merge with data frame explanatory variables
explanatory_variables <- merge(explanatory_variables, pv_inv_q, by = "quarter", all = TRUE)


#################################################
### Global Smart Devices sells  (no est? muy bonita esta data) ###
##################################################

# Transforming to quarter

# Create a sequence of quarters for time variable and also for the values of the time series
# by considering the fourth part of the value for each quarter
quarter <- seq(from = min(as.yearqtr(as.Date(paste(smart.dev.sales$Year, 1, 1,sep = "-")))), 
                to = max(as.yearqtr(as.Date(paste(smart.dev.sales$Year, 12, 1, sep = "-")))), by = 0.25)

smartphones.q <- cumsum(rep(c(smart.dev.sales$Smartphones[1], diff(smart.dev.sales$Smartphones))/4, each = 4))
tablets.q <- cumsum(rep(c(smart.dev.sales$Tablets[1], diff(smart.dev.sales$Tablets))/4, each = 4))

tablets_q  <- cbind(as.data.frame(quarter), tablets.q, smartphones.q)

#Merge with data frame explanatory variables
explanatory_variables <- merge(explanatory_variables, tablets_q, by = "quarter", all = TRUE)



###############################################
### Google Trends data ###
###############################################

#Transformation to quarter

#E-cars
ecars.gtrends$quarter <- as.yearqtr(ecars.gtrends$date, format = "%Y-%m-%d", fiscal_start = 1)
ecars.gtrends <- ecars.gtrends %>% 
  group_by(quarter) %>% 
  summarise_at(c("chile", "aus", "world"), mean) %>% 
  as.data.frame() %>% 
  rename(ecars.gtrends.chile = chile, ecars.gtrends.aus = aus, ecars.gtrends.world = world)


#Lithium
lithium.gtrends$quarter <- as.yearqtr(lithium.gtrends$date, format = "%Y-%m-%d", fiscal_start = 1)
lithium.gtrends <- lithium.gtrends %>% 
  group_by(quarter) %>% 
  summarise_at(c("chile", "aus", "world"), mean) %>% 
  as.data.frame()%>% 
  rename(lithium.gtrends.chile = chile, lithium.gtrends.aus = aus, lithium.gtrends.world = world)

#Li-ion batteries
li.batteries.gtrends$quarter <- as.yearqtr(li.batteries.gtrends$date, format = "%Y-%m-%d", fiscal_start = 1)
li.batteries.gtrends  <- li.batteries.gtrends  %>% 
  group_by(quarter) %>% 
  summarise_at(c("chile", "aus", "world"), mean) %>% 
  as.data.frame()%>% 
  rename(lit.bat.gtrends.chile = chile, lit.bat.gtrends.aus = aus, lit.bat.gtrends.world = world)


#Merge with data frame explanatory variables
explanatory_variables <- merge(explanatory_variables, ecars.gtrends,
                               by = "quarter", all = TRUE)
explanatory_variables <- merge(explanatory_variables, lithium.gtrends,
                               by = "quarter", all = TRUE)
explanatory_variables <- merge(explanatory_variables, li.batteries.gtrends,
                               by = "quarter", all = TRUE)


ecars.gtrends.chile <- ecars.gtrends %>%
    filter(quarter >= 2014 & quarter < "2023 Q4") %>%
    select(ecars.gtrends.chile)

lithium.gtrends.chile <- lithium.gtrends %>%
    filter(quarter >= 2014 & quarter < "2023 Q4") %>%
    select(lithium.gtrends.chile)

li.batteries.gtrends.chile <- li.batteries.gtrends %>%
    filter(quarter >= 2014 & quarter < "2023 Q4" ) %>%
    select(lit.bat.gtrends.chile)


#Variables for Chile (ready to plug it in a model)
#########
ecars.gtrends.chile <- ecars.gtrends.chile$chile
lithium.gtrends.chile <- lithium.gtrends.chile$chile
li.batteries.gtrends.chile <- li.batteries.gtrends.chile$chile
#########

ecars.gtrends.aus <- ecars.gtrends %>%
    filter(quarter >= 2010 & quarter < "2023 Q3") %>%
    select(ecars.gtrends.aus)

lithium.gtrends.aus <- lithium.gtrends %>%
    filter(quarter >= 2010 & quarter < "2023 Q3") %>%
    select(lithium.gtrends.aus)

li.batteries.gtrends.aus <- li.batteries.gtrends %>%
    filter(quarter >= 2010 & quarter < "2023 Q3" ) %>%
    select(lit.bat.gtrends.aus)

#Variables for Australia (ready to plug it in a model)
##########
ecars.gtrends.aus <- ecars.gtrends.aus$aus
lithium.gtrends.aus <- lithium.gtrends.aus$aus
li.batteries.gtrends.aus <- li.batteries.gtrends.aus$aus
##########

###############################################
### Lithium prices ###
###############################################

#Transform to quarter
lithium.prices$quarter <- as.yearqtr(lithium.prices$date, format = "%Y-%m-%d", fiscal_start = 1)

lithium.prices <- lithium.prices %>% 
  group_by(quarter) %>% 
  summarise(price = mean(price)) %>% 
  as.data.frame()


lithium.prices.ch <-  lithium.prices %>%
    filter(quarter < "2023 Q4") %>%
    select(price)

lithium.prices.aus <-  lithium.prices %>%
    filter(quarter < "2023 Q3") %>%
    select(price)

#Variables (ready to plug it in a model)

lithium.prices.ch  <- lithium.prices.ch$price
lithium.prices.aus  <- lithium.prices.aus$price

explanatory_variables <- merge(explanatory_variables, lithium.prices,
                               by = "quarter", all = TRUE)

###############################################
### Stock prices  ###
###############################################

#Transform to quarter

albemarle.stock$quarter <- as.yearqtr(albemarle.stock$date, format = "%Y-%m-%d", fiscal_start = 1)
albemarle.stock.quarter <- albemarle.stock %>% 
  group_by(quarter) %>% 
  summarise(albemarle.stock = mean(close)) %>% 
  as.data.frame()

ganfeng.stock$quarter <- as.yearqtr(ganfeng.stock$date, format = "%Y-%m-%d", fiscal_start = 1)
ganfeng.stock.quarter <- ganfeng.stock %>% 
  group_by(quarter) %>% 
  summarise(ganfeng.stock = mean(close)) %>% 
  as.data.frame()

sqm.stock$quarter <- as.yearqtr(sqm.stock$date, format = "%Y-%m-%d", fiscal_start = 1)
sqm.stock.quarter <- sqm.stock %>% 
  group_by(quarter) %>% 
  summarise(sqm.stock = mean(close)) %>% 
  as.data.frame()

mineral.resources.stock$quarter <- as.yearqtr(mineral.resources.stock$date, format = "%Y-%m-%d", fiscal_start = 1)
mineral.resources.stock.quarter <- mineral.resources.stock %>% 
  group_by(quarter) %>% 
  summarise(mineral.resources.stock = mean(close)) %>% 
  as.data.frame()

byd.stock$quarter <- as.yearqtr(byd.stock$date, format = "%Y-%m-%d", fiscal_start = 1)
byd.stock.quarter <-byd.stock %>% 
  group_by(quarter) %>% 
  summarise(byd.stock = mean(close)) %>% 
  as.data.frame()


explanatory_variables <- merge(explanatory_variables, albemarle.stock.quarter,
                               by = "quarter", all = TRUE)
explanatory_variables <- merge(explanatory_variables, ganfeng.stock.quarter,
                               by = "quarter", all = TRUE)
explanatory_variables <- merge(explanatory_variables,sqm.stock.quarter,
                               by = "quarter", all = TRUE)
explanatory_variables <- merge(explanatory_variables, mineral.resources.stock.quarter,
                               by = "quarter", all = TRUE)
explanatory_variables <- merge(explanatory_variables, byd.stock.quarter,
                               by = "quarter", all = TRUE)

explanatory_variables<- subset(explanatory_variables, quarter >= "2009 Q1")

# Exports Chile in kton met
lit.chl.exp.kton <-  na.omit(lit.chl.exp$lit_chl_exp_kton_met)
lit.chl.exp.quarter.kton <- na.omit(lit.chl.exp.quarter$lit_chl_exp_kton_met)

# Exports AUS in kton met
lit.aus.exp.kton <-  na.omit(lit.aus.exp$lit_aus_exp_kton_met)

# Total Exports
lit.total.exp <-  na.omit(lit.total.exp.quarter$lit.total.exp.kton)


##############################################################################
####################### Forecasting Explanatory Variables ###################
##############################################################################
## GDP: 2023 Q2
## PHEV BEV STOCK: 2022 Q4
## FAST/SLOW CHARGERS 2022 Q4
## CUMPV 2022 Q4
## TRENDS AND STOCk 2023 Q4
## Price 2017q1 to 2023Q4

df_base <- explanatory_variables[, grep("^(quarter|lit_|lit.total)", names(explanatory_variables), value = TRUE)]

########### 1. Chargers: Holt's winters ########################
#Ends on 2022 Q1 and prediction up to 2024 Q4
## Forecasting of chargers with Holt's winters because this series have tendency

quarter <- seq(from = min(as.yearqtr(as.Date(paste(2023, 1, 1,sep = "-")))), 
              to = max(as.yearqtr(as.Date(paste(2024, 12, 1, sep = "-")))), by = 0.25)
chargers_pred <- as.data.frame(quarter)

df_chargers <- explanatory_variables[, grep("^(slow|fast)", names(explanatory_variables), value = TRUE)] %>% 
  na.omit()

prediction <- data.frame(matrix(ncol = ncol(df_chargers), nrow = 8))

names(prediction) <- names(df_chargers)

for(i in 1:ncol(df_chargers)) {
  
  holt_model <- holt(df_chargers[,i], damped=T, h=8)
  var_forecast <- forecast(holt_model, h = 8)
  prediction[,i] <- var_forecast$mean
  
}

df_chargers <- explanatory_variables[, grep("^(quarter|slow|fast)", names(explanatory_variables), value = TRUE)] %>% 
  na.omit()

chargers_pred <- rbind(df_chargers,cbind(chargers_pred, prediction))
data_chargers <- merge(df_base, chargers_pred, by ="quarter", all = T)

rm(df_chargers,chargers_pred, prediction)

########### 2. EV stock: Holt's winters ########################
#Ends on 2022 Q1 and prediction up to 2024 Q4
## Forecasting of chargers with Holt's winters because this series have tendency
ev_pred <- as.data.frame(quarter)

df_ev <- explanatory_variables[, grep("^(PHEV.stock|BEV.stock)", names(explanatory_variables), value = TRUE)] %>% 
  na.omit()

prediction <- data.frame(matrix(ncol = ncol(df_ev), nrow = 8))

names(prediction) <- names(df_ev)

for(i in 1:ncol(df_ev)) {
  
  holt_model <- holt(df_ev[,i], damped=T, h=8)
  var_forecast <- forecast(holt_model, h = 8)
  prediction[,i] <- var_forecast$mean
  
}

df_ev <- explanatory_variables[, grep("^(quarter|PHEV.stock|BEV.stock)", names(explanatory_variables), value = TRUE)] %>% 
  na.omit()

ev_pred <- rbind(df_ev,cbind(ev_pred, prediction))
data_ev <- merge(df_base, ev_pred, by ="quarter", all = T)

rm(df_ev,ev_pred, prediction)

########### 3. Google Trends and Stock Market: Exponential Smoothing ########################
#Ends on 2023 Q4 and prediction up to 2024 Q4
## Forecasting of chargers with Exponential Smoothing

df_trends <- explanatory_variables[, grep("(^(albemarle|sqm|mineral|byd|ecars|lithium.gtrend|lit.bat))", names(explanatory_variables), value = TRUE)] %>% 
  na.omit()

quarter <- seq(from = min(as.yearqtr(as.Date(paste(2024, 1, 1,sep = "-")))), 
               to = max(as.yearqtr(as.Date(paste(2024, 12, 1, sep = "-")))), by = 0.25)
trends_pred <- as.data.frame(quarter)

df_trends <- explanatory_variables[, grep("^(albemarle|sqm|mineral|byd|ecars|lithium.gtrend|lit.bat)", names(explanatory_variables), value = TRUE)] %>% 
  na.omit()

prediction <- data.frame(matrix(ncol = ncol(df_trends), nrow = 4))

names(prediction) <- names(df_trends)

for(i in 1:ncol(df_trends)) {
  
  ses_model <- ses(df_trends[,i], h=4)
  var_forecast <- forecast(ses_model, h = 4)
  prediction[,i] <- var_forecast$mean
  
}

df_trends <- explanatory_variables[, grep("^(quarter|albemarle|sqm|mineral|byd|ecars|lithium.gtrend|lit.bat)", names(explanatory_variables), value = TRUE)] %>% 
  na.omit()

trends_pred <- rbind(df_trends,cbind(trends_pred, prediction))
data_trends <- merge(df_base, trends_pred, by ="quarter", all = T)

rm(df_trends,trends_pred, prediction)


########### 4. Price: Exponential Smoothing ########################
#price,2013 Q1 pv.inv

quarter <- seq(from = min(as.yearqtr(as.Date(paste(2023, 1, 1,sep = "-")))), 
               to = max(as.yearqtr(as.Date(paste(2024, 12, 4, sep = "-")))), by = 0.25)

price_pred <- as.data.frame(quarter)

df_price <- subset(explanatory_variables, select = c("quarter","price")) %>% 
  na.omit()

ses_model <- ses(df_price[,2], h=8)
var_forecast <- forecast(ses_model, h = 8)
price <-  var_forecast$mean

price_pred <- rbind(df_price,cbind(price_pred, price))
data_price <- merge(df_base, price_pred, by ="quarter", all = T)

rm(df_price,price_pred)

########### 4. Solar Investment : Exponential Smoothing ########################
#price,2013 Q1 pv.inv
quarter <- seq(from = min(as.yearqtr(as.Date(paste(2023, 1, 1,sep = "-")))), 
               to = max(as.yearqtr(as.Date(paste(2024, 12, 4, sep = "-")))), by = 0.25)

solar_pred <- as.data.frame(quarter)

df_solar <- subset(explanatory_variables, select = c("quarter","pv.inv.q")) %>% 
  na.omit()

holt_model <- holt(df_solar[,2], damped=T, h=8)
var_forecast <- forecast(holt_model, h = 8)
pv.inv.q <- var_forecast$mean

solar_pred <- rbind(df_solar,cbind(solar_pred, pv.inv.q))
data_solar <- merge(df_base, solar_pred, by ="quarter", all = T)

rm(df_solar,solar_pred)

########### 4. GDP: Exponential Smoothing ########################

quarter <- seq(from = min(as.yearqtr(as.Date(paste(2023, 9, 1,sep = "-")))), 
               to = max(as.yearqtr(as.Date(paste(2024, 12, 1, sep = "-")))), by = 0.25)
gdp_pred <- as.data.frame(quarter)

price_pred <- as.data.frame(quarter)

df_gdp <- subset(gdp_chl_chain, select = c(gdp_chl_chain, gdp_chl_pc)) %>% 
  na.omit()

prediction <- data.frame(matrix(ncol = ncol(df_gdp), nrow = 6))

names(prediction) <- names(df_gdp)

for(i in 1:ncol(df_gdp)) {
  
  holt_model <- holt(df_gdp[,i], h=6, damped = T)
  var_forecast <- forecast(holt_model, h = 6)
  prediction[,i] <- var_forecast$mean
  
}

df_gdp <- subset(gdp_chl_chain, select = c(quarter, gdp_chl_chain, gdp_chl_pc)) %>% 
  na.omit()

gdp_pred <- rbind(df_gdp,cbind(gdp_pred, prediction))
data_gdp_chl <- merge(df_base, gdp_pred, by ="quarter", all = T)

rm(df_gdp,gdp_pred, prediction)


data_gdp_chl <- data_gdp_chl %>%
  mutate(gdp_chl_var = lag(gdp_chl_chain,4)) %>%
  mutate(gdp_chl_pc_var = lag(gdp_chl_pc,4)) %>%
  mutate(gdp_chl_var = (gdp_chl_chain-gdp_chl_var)/gdp_chl_chain) %>%
  mutate(gdp_chl_pc_var = (gdp_chl_pc-gdp_chl_pc_var)/gdp_chl_pc)

########### 4. GDP: Exponential Smoothing ########################

quarter <- seq(from = min(as.yearqtr(as.Date(paste(2023, 9, 1,sep = "-")))), 
               to = max(as.yearqtr(as.Date(paste(2024, 12, 1, sep = "-")))), by = 0.25)
gdp_pred <- as.data.frame(quarter)

price_pred <- as.data.frame(quarter)

df_gdp <- subset(gdp_aus_chain, select = c(gdp_aus_chain, gdp_aus_pc)) %>% 
  na.omit()

prediction <- data.frame(matrix(ncol = ncol(df_gdp), nrow = 6))

names(prediction) <- names(df_gdp)

for(i in 1:ncol(df_gdp)) {
  
  holt_model <- holt(df_gdp[,i], h=6, damped = T)
  var_forecast <- forecast(holt_model, h = 6)
  prediction[,i] <- var_forecast$mean
  
}

df_gdp <- subset(gdp_aus_chain, select = c(quarter, gdp_aus_chain, gdp_aus_pc)) %>% 
  na.omit()

gdp_pred <- rbind(df_gdp,cbind(gdp_pred, prediction))
data_gdp_aus <- merge(df_base, gdp_pred, by ="quarter", all = T)

rm(df_gdp,gdp_pred, prediction)


data_gdp_aus <- data_gdp_aus %>%
  mutate(gdp_aus_var = lag(gdp_aus_chain,4)) %>%
  mutate(gdp_aus_pc_var = lag(gdp_aus_pc,4)) %>%
  mutate(gdp_aus_var = (gdp_aus_chain-gdp_aus_var)/gdp_aus_chain) %>%
  mutate(gdp_aus_pc_var = (gdp_aus_pc-gdp_aus_pc_var)/gdp_aus_pc)


#Data bases:
#data_gdp_chl
#data_gdp_aus
#data_solar
#data_price
#data_chargers
#data_ev
#data_trends
