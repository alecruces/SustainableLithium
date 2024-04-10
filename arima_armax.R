########################################################################################
#################### ARIMA/ARMAX ###################################################
##########################################################################################

#Setting the path
setwd("C:/Users/alecr/OneDrive/Escritorio/Master Data Science/Tercer Semestre/Financial Data/proyect")

source("C:/Users/alecr/OneDrive/Escritorio/Master Data Science/Tercer Semestre/Financial Data/proyect/load_data.R")
source("C:/Users/alecr/OneDrive/Escritorio/Master Data Science/Tercer Semestre/Financial Data/proyect/transform_data.R")

#######################################################################
## 1 AUSTRALIA EXPORTS
#######################################################################
# Exports AUS in kton met
tsdisplay(lit.aus.exp.kton)
# The series has trend

# First Difference
lit.aus.exp.kton.diff <- diff(lit.aus.exp.kton, differences = 1)

#The Difference is stationary
plot(lit.aus.exp.kton.diff, type = "l", col = "#00C3B1", lwd = 2,
     main = "Exports Australia", xlab = "Quarter", ylab = "Exports Australia")

tsdisplay(lit.aus.exp.kton.diff)

#######################################################################
## 1 CHILE EXPORTS
#######################################################################
# Exports CHL in kton met
tsdisplay(lit.chl.exp.quarter.kton)

# The series has trend

# First Difference
lit.chl.exp.quarter.kton.diff <- diff(lit.chl.exp.quarter.kton, differences = 1)


plot(lit.chl.exp.quarter.kton.diff, type = "l", col = "#00C3B1", lwd = 2,
     main = "Exports Chile", xlab = "Quarter", ylab = "Exports Chile")


tsdisplay(lit.chl.exp.quarter.kton.diff)


#######################################################################
## 1 ARIMA/SARIMAX/ARMAX: AUSTRALIA EXPORTS
#######################################################################

# First Difference
arima1_aus<- Arima(lit.aus.exp.kton, order=c(1,1,0))
summary(arima1_aus)

resid1_aus<- residuals(arima1_aus)
tsdisplay(resid1_aus)
checkresiduals(arima1_aus)

plot(lit.aus.exp.kton, type = "l")
lines(fitted(arima1_aus), col=2)

for1_aus<- forecast(arima1_aus)
plot(for1_aus)
lines(fitted(arima1_aus), col = "green")

##Auto Arima
arima_auto_aus<- auto.arima(lit.aus.exp.kton)
summary(arima_auto_aus)

resid1_aus<- residuals(arima_auto_aus)
tsdisplay(resid1_aus)
checkresiduals(arima1_aus)

autoplot(forecast(arima_auto_aus))
checkresiduals(arima_auto_aus)

for1_aus<- forecast(arima_auto_aus,h=4)

plot(for1_aus, type = "l", col = "black", lwd = 2,
     main = "Australia ARIMA (1,1,0) with drift", xlab = "Quarter", ylab = "kT",xaxt = "n")
lines(fitted(arima1_aus), col = "#00C3B1", lwd = 2, type = "l", lty = "dashed")
axis(1, at=seq(1, 60, by=4), labels=c("2010", "2011", "2012", "2013", "2014",
                                      "2015", "2016", "2017", "2018", "2019",
                                      "2020", "2021", "2022", "2023", "2024"))
abline(v=55, col="gray", lwd = 2, lty = 3)

#Series: lit.aus.exp.kton 
#ARIMA(1,1,0) with drift 

#Coefficients:
#  ar1   drift
#-0.2170  1.7632
#s.e.   0.1317  1.0038

#sigma^2 = 83.13:  log likelihood = -194.98
#AIC=395.96   AICc=396.44   BIC=401.92

Box.test(residuals(arima_auto_aus), lag=12,fitdf=6, type="Ljung")

################################################################
###SARIMA models
################################################################

# No seasonality, it does not make sense to use this model
armax1_aus<- Arima(lit.aus.exp.kton, order=c(1,1,0), seasonal=c(0,0,1))
summary(armax1_aus)

resid1_aus<- residuals(armax1_aus)
tsdisplay(resid1_aus)


plot(lit.aus.exp.kton, type = "l")
lines(fitted(arima1_aus), col=2)

################################################################
#############ARMAX models
################################################################

# First Difference
#armax1_aus<- Arima(lit.aus.exp.kton, xreg=explanatory_variables_aus$pv.inv.q, order=c(1,0,1))

#For australia

data_gdp_aus_aux <- subset(data_gdp_aus, quarter >= "2011 Q1" & quarter <="2023 Q3")
data_solar_aux <- subset(data_solar, quarter >= "2013 Q1" & quarter <="2023 Q3")
data_price_aux <- subset(data_price, quarter >= "2017 Q1" & quarter <="2023 Q3")
data_chargers_aux <- subset(data_chargers, quarter >= "2015 Q1" & quarter <="2023 Q3")
data_ev_aux <- subset(data_ev, quarter >= "2010 Q1" & quarter <="2023 Q3")
data_trends_aux <- subset(data_trends, quarter >= "2010 Q1" & quarter <="2023 Q3")

# For predict part
data_solar_pred <- subset(data_solar, quarter >= "2023 Q3" & quarter <="2024 Q4")
data_price_pred <- subset(data_price, quarter >= "2023 Q3" & quarter <="2024 Q4")
data_chargers_pred <- subset(data_chargers, quarter >= "2023 Q3" & quarter <="2024 Q4")
data_ev_pred <- subset(data_ev, quarter >= "2023 Q3" & quarter <="2024 Q4")
data_trends_pred <- subset(data_trends, quarter >= "2023 Q3" & quarter <="2024 Q4")


## ARIMAX with loop in explanatory variables
arimax_aus <- list()
arimax_aus_lag <- list()
arimax_aus_lag1 <- list()
best_model_arimax_aus <- list()
model_names_arimax_aus <- list()

# Loop

df <- data_gdp_aus_aux
for(i in 5:ncol(df)) {
  
  var_e <- (df[,i])
  var_lag <- lag((df[,i]),1)
  var_lag <- as.data.frame(cbind(var_e, var_lag))
  
  
  # To add more explanatory Variables
  xreg = var_e
  xreg2 = var_lag$var_lag
  xreg3 <- cbind(var_0 = var_lag$var_e,
                 var_1 = var_lag$var_lag)
  
  # Model ARMAX
  fit <- auto.arima(df$lit_aus_exp_kton_met, xreg = xreg)
  fit2 <- auto.arima(df$lit_aus_exp_kton_met, xreg = var_lag$var_lag)
  fit3 <- auto.arima(df$lit_aus_exp_kton_met, xreg = xreg3)
  
  # Save the models
  arimax_aus[[names(df)[i]]] <- fit
  arimax_aus_lag[[names(df)[i]]] <- fit2
  arimax_aus_lag1[[names(df)[i]]] <- fit3
  
  # Keep the model with the lowest AIC
  best_fit <- list(fit, fit2, fit3)[[which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))]]
  
  n_ <-which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))
  best_model_arimax_aus[[paste0(names(df)[i],n_)]] <- best_fit
  
}

df <- data_solar_aux
for(i in 5:ncol(df)) {
  
  var_e <- (df[,i])
  var_lag <- lag((df[,i]),1)
  var_lag <- as.data.frame(cbind(var_e, var_lag))
  
  
  # To add more explanatory Variables
  xreg = var_e
  xreg2 = var_lag$var_lag
  xreg3 <- cbind(var_0 = var_lag$var_e,
                 var_1 = var_lag$var_lag)
  
  # Model ARMAX
  fit <- auto.arima(df$lit_aus_exp_kton_met, xreg = xreg)
  fit2 <- auto.arima(df$lit_aus_exp_kton_met, xreg = var_lag$var_lag)
  fit3 <- auto.arima(df$lit_aus_exp_kton_met, xreg = xreg3)
  
  # Save the models
  arimax_aus[[names(df)[i]]] <- fit
  arimax_aus_lag[[names(df)[i]]] <- fit2
  arimax_aus_lag1[[names(df)[i]]] <- fit3
  
  # Keep the model with the lowest AIC
  best_fit <- list(fit, fit2, fit3)[[which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))]]

  n_ <-which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))
  best_model_arimax_aus[[paste0(names(df)[i],n_)]] <- best_fit
  
}

df <- data_ev_aux

for(i in 5:ncol(df)) {
  
  var_e <- (df[,i])
  var_lag <- lag((df[,i]),1)
  var_lag <- as.data.frame(cbind(var_e, var_lag))
  
  
  # To add more explanatory Variables
  xreg = var_e
  xreg2 = var_lag$var_lag
  xreg3 <- cbind(var_0 = var_lag$var_e,
                 var_1 = var_lag$var_lag)
  
  # Model ARMAX
  fit <- auto.arima(df$lit_aus_exp_kton_met, xreg = xreg)
  fit2 <- auto.arima(df$lit_aus_exp_kton_met, xreg = var_lag$var_lag)
  fit3 <- auto.arima(df$lit_aus_exp_kton_met, xreg = xreg3)
  
  # Save the models
  arimax_aus[[names(df)[i]]] <- fit
  arimax_aus_lag[[names(df)[i]]] <- fit2
  arimax_aus_lag1[[names(df)[i]]] <- fit3
  
  # Keep the model with the lowest AIC
  best_fit <- list(fit, fit2, fit3)[[which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))]]
  
  n_ <-which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))
  best_model_arimax_aus[[paste0(names(df)[i],n_)]] <- best_fit
  
}



df <- data_price_aux
for(i in 5:ncol(df)) {
  
  var_e <- (df[,i])
  var_lag <- lag((df[,i]),1)
  var_lag <- as.data.frame(cbind(var_e, var_lag))
  
  
  # To add more explanatory Variables
  xreg = var_e
  xreg2 = var_lag$var_lag
  xreg3 <- cbind(var_0 = var_lag$var_e,
                 var_1 = var_lag$var_lag)
  
  # Model ARMAX
  fit <- auto.arima(df$lit_aus_exp_kton_met, xreg = xreg)
  fit2 <- auto.arima(df$lit_aus_exp_kton_met, xreg = var_lag$var_lag)
  fit3 <- auto.arima(df$lit_aus_exp_kton_met, xreg = xreg3)
  
  # Save the models
  arimax_aus[[names(df)[i]]] <- fit
  arimax_aus_lag[[names(df)[i]]] <- fit2
  arimax_aus_lag1[[names(df)[i]]] <- fit3
  
  # Keep the model with the lowest AIC
  best_fit <- list(fit, fit2, fit3)[[which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))]]
  
  n_ <-which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))
  best_model_arimax_aus[[paste0(names(df)[i],n_)]] <- best_fit
  
}

df <- data_chargers_aux
for(i in 5:ncol(df)) {
  
  var_e <- (df[,i])
  var_lag <- lag((df[,i]),1)
  var_lag <- as.data.frame(cbind(var_e, var_lag))
  
  
  # To add more explanatory Variables
  xreg = var_e
  xreg2 = var_lag$var_lag
  xreg3 <- cbind(var_0 = var_lag$var_e,
                 var_1 = var_lag$var_lag)
  
  # Model ARMAX
  fit <- auto.arima(df$lit_aus_exp_kton_met, xreg = xreg)
  fit2 <- auto.arima(df$lit_aus_exp_kton_met, xreg = var_lag$var_lag)
  fit3 <- auto.arima(df$lit_aus_exp_kton_met, xreg = xreg3)
  
  # Save the models
  arimax_aus[[names(df)[i]]] <- fit
  arimax_aus_lag[[names(df)[i]]] <- fit2
  arimax_aus_lag1[[names(df)[i]]] <- fit3
  
  # Keep the model with the lowest AIC
  best_fit <- list(fit, fit2, fit3)[[which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))]]
  
  n_ <-which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))
  best_model_arimax_aus[[paste0(names(df)[i],n_)]] <- best_fit
  
}


df <- data_trends_aux
for(i in 5:ncol(df)) {
  
  var_e <- (df[,i])
  var_lag <- lag((df[,i]),1)
  var_lag <- as.data.frame(cbind(var_e, var_lag))
  
  
  # To add more explanatory Variables
  xreg = var_e
  xreg2 = var_lag$var_lag
  xreg3 <- cbind(var_0 = var_lag$var_e,
                 var_1 = var_lag$var_lag)
  
  # Model ARMAX
  fit <- auto.arima(df$lit_aus_exp_kton_met, xreg = xreg)
  fit2 <- auto.arima(df$lit_aus_exp_kton_met, xreg = var_lag$var_lag)
  fit3 <- auto.arima(df$lit_aus_exp_kton_met, xreg = xreg3)
  
  # Save the models
  arimax_aus[[names(df)[i]]] <- fit
  arimax_aus_lag[[names(df)[i]]] <- fit2
  arimax_aus_lag1[[names(df)[i]]] <- fit3
  
  # Keep the model with the lowest AIC
  best_fit <- list(fit, fit2, fit3)[[which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))]]
  
  n_ <-which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))
  best_model_arimax_aus[[paste0(names(df)[i],n_)]] <- best_fit
  
}


best_model_arimax_aus
aic_values <- sapply(best_model_arimax_aus, AIC)
models_orders <- best_model_arimax_aus[order(aic_values)]
models_orders

#Variables
#gdp_aus_var, price, fast.ch.q.total
# lit.bat.gtrends.world, lithium.gtrends.world
# albemarle.stock mineral.resources.stock

############### Best Models: ECONOMIC################
model <- arimax_aus_lag1[["gdp_aus_var"]]
summary(model)
# 364.88
resid1_aus<- residuals(model)
tsdisplay(resid1_aus)
checkresiduals(resid1_aus)

plot(data_gdp_aus_aux$lit_aus_exp_kton_met, type = "l", col = "black", lwd = 2,
     main = "GDP (t and t-1)", xlab = "Quarter", ylab = "kT",xaxt = "n")
lines(fitted(model), col = "#00C3B1", lwd = 2, type = "l", lty = "dashed")
axis(1, at=seq(1, 51, by=4), labels=c("2011", "2012", "2013", "2014",
                                      "2015", "2016", "2017", "2018", "2019",
                                      "2020", "2021", "2022", "2023"))

############### Best Models: Google Trends Lithium ################
model <- arimax_aus_lag[["lit.bat.gtrends.world"]]
summary(model)
resid1_aus<- residuals(model)
tsdisplay(resid1_aus)
checkresiduals(resid1_aus)

plot(data_trends_aux$lit_aus_exp_kton_met, type = "l", col = "black", lwd = 2,
     main = "Google Trends Lithium Batteries World (t-1) ", xlab = "Quarter", ylab = "kT",xaxt = "n")
lines(fitted(model), col = "#00C3B1", lwd = 2, type = "l", lty = "dashed")
axis(1, at=seq(1, 51, by=4), labels=c("2011", "2012", "2013", "2014",
                                      "2015", "2016", "2017", "2018", "2019",
                                      "2020", "2021", "2022", "2023"))

############ Stock Market Model Selection ##########################

model <- arimax_aus_lag1[["mineral.resources.stock"]]
summary(model)
#AIC=389.49
resid1_aus<- residuals(model)
tsdisplay(resid1_aus)
checkresiduals(resid1_aus)


plot(data_trends_aux$lit_aus_exp_kton_met, type = "l", col = "black", lwd = 2,
     main = "Mineral Resources (t and t-1) ", xlab = "Quarter", ylab = "kT",xaxt = "n")
lines(fitted(model), col = "#00C3B1", lwd = 2, type = "l", lty = "dashed")
axis(1, at=seq(1, 51, by=4), labels=c("2011", "2012", "2013", "2014",
                                      "2015", "2016", "2017", "2018", "2019",
                                      "2020", "2021", "2022", "2023"))

#############################
# ARMAX# WITH MORE THAN ONE VARIABLES#

var0<- data_trends_aux$mineral.resources.stock
var1<- lag(data_trends_aux$mineral.resources.stock,1)
var2<- lag(data_trends_aux$lit.bat.gtrends.world,1)
xreg <- cbind(var0 = var0,
              var1 = var1,
                 var2 = var2)

model_c <- auto.arima(data_trends_aux$lit_aus_exp_kton_met, xreg = xreg)
summary(model_c )
resid1_aus<- residuals(model_c)
tsdisplay(resid1_aus)
checkresiduals(resid1_aus)

###########################################
#### FORECASTING
############################################
# based on AIC we choose the second model

xreg=cbind(var0=data_trends_pred$mineral.resources.stock[2:5],
           var1=data_trends_pred$mineral.resources.stock[1:4], 
           var2 =data_trends_pred$lit.bat.gtrends.world[1:4])

f<-forecast(model_c, h=4, xreg=xreg)

plot(f, type = "l", col = "black", lwd = 2,
     main = " ", xlab = "Quarter", ylab = "kT",xaxt = "n")
lines(fitted(model_c), col = "#00C3B1", lwd = 2, type = "l", lty = "dashed")
axis(1, at=seq(1, 60, by=4), labels=c("2010","2011", "2012", "2013", "2014",
                                      "2015", "2016", "2017", "2018", "2019",
                                      "2020", "2021", "2022", "2023", "2024"))

abline(v=55, col="gray", lwd = 2, lty = 3)

#######################################################################
## 1 CHILE EXPORTS
#######################################################################

arima1<- Arima(lit.chl.exp.quarter.kton, order=c(0,1,4))
summary(arima1)

resid1<- residuals(arima1)
tsdisplay(resid1)

plot(lit.chl.exp.quarter.kton.diff)
lines(fitted(arima1), col=2)

for1<- forecast(arima1)
plot(for1)

auto.a.chl<- auto.arima(lit.chl.exp.quarter.kton)
summary(auto.a.chl)
# AIC=240.97
checkresiduals(auto.a.chl)

for1_chl<- forecast(auto.a.chl,h=4)
plot(for1_chl, type = "l", col = "black", lwd = 2,
     main = "Chile ARIMA (1,1,0) with drift", xlab = "Quarter", ylab = "kT",xaxt = "n")
lines(fitted(auto.a.chl), col = "#00C3B1", lwd = 2, type = "l", lty = "dashed")
axis(1, at=seq(1, 43, by=4), labels=c("2014","2015", "2016", "2017", "2018", "2019",
                                      "2020", "2021", "2022", "2023", "2024"))
abline(v=39, col="gray", lwd = 2, lty = 3)


################################################################
#############ARMAX models
################################################################

# First Difference
#armax1_chl<- Arima(lit.chl.exp.kton, xreg=explanatory_variables_chl$pv.inv.q, order=c(1,0,1))

#For Chile
data_gdp_chl_aux <- subset(data_gdp_chl, quarter >= "2014 Q1" & quarter <="2023 Q3")
data_solar_aux <- subset(data_solar, quarter >= "2014 Q1" & quarter <="2023 Q3")
data_price_aux <- subset(data_price, quarter >= "2017 Q1" & quarter <="2023 Q3")
data_chargers_aux <- subset(data_chargers, quarter >= "2015 Q1" & quarter <="2023 Q3")
data_ev_aux <- subset(data_ev, quarter >= "2014 Q1" & quarter <="2023 Q3")
data_trends_aux <- subset(data_trends, quarter >= "2014 Q1" & quarter <="2023 Q3")

## Arimax with loop
arimax_chl <- list()
arimax_chl_lag <- list()
arimax_chl_lag1 <- list()
best_model_arimax_chl <- list()
model_names_arimax_chl <- list()

# Loop a través de las columnas del DataFrame
df <- data_gdp_chl_aux
for(i in 5:ncol(df)) {
  
  var_e <- (df[,i])
  var_lag <- lag((df[,i]),1)
  var_lag <- as.data.frame(cbind(var_e, var_lag))
  
  
  # To add more explanatory Variables
  xreg = var_e
  xreg2 = var_lag$var_lag
  xreg3 <- cbind(var_0 = var_lag$var_e,
                 var_1 = var_lag$var_lag)
  
  # Model ARMAX
  fit <- auto.arima(df$lit_chl_exp_kton_met, xreg = xreg)
  fit2 <- auto.arima(df$lit_chl_exp_kton_met, xreg = var_lag$var_lag)
  fit3 <- auto.arima(df$lit_chl_exp_kton_met, xreg = xreg3)
  
  # Save the models
  arimax_chl[[names(df)[i]]] <- fit
  arimax_chl_lag[[names(df)[i]]] <- fit2
  arimax_chl_lag1[[names(df)[i]]] <- fit3
  
  # Keep the model with the lowest AIC
  best_fit <- list(fit, fit2, fit3)[[which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))]]
  
  n_ <-which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))
  best_model_arimax_chl[[paste0(names(df)[i],n_)]] <- best_fit
  
}

df <- data_solar_aux
for(i in 5:ncol(df)) {
  
  var_e <- (df[,i])
  var_lag <- lag((df[,i]),1)
  var_lag <- as.data.frame(cbind(var_e, var_lag))
  
  
  # To add more explanatory Variables
  xreg = var_e
  xreg2 = var_lag$var_lag
  xreg3 <- cbind(var_0 = var_lag$var_e,
                 var_1 = var_lag$var_lag)
  
  # Model ARMAX
  fit <- auto.arima(df$lit_chl_exp_kton_met, xreg = xreg)
  fit2 <- auto.arima(df$lit_chl_exp_kton_met, xreg = var_lag$var_lag)
  fit3 <- auto.arima(df$lit_chl_exp_kton_met, xreg = xreg3)
  
  # Save the models
  arimax_chl[[names(df)[i]]] <- fit
  arimax_chl_lag[[names(df)[i]]] <- fit2
  arimax_chl_lag1[[names(df)[i]]] <- fit3
  
  # Keep the model with the lowest AIC
  best_fit <- list(fit, fit2, fit3)[[which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))]]
  
  n_ <-which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))
  best_model_arimax_chl[[paste0(names(df)[i],n_)]] <- best_fit
  
}

df <- data_ev_aux

for(i in 5:ncol(df)) {
  
  var_e <- (df[,i])
  var_lag <- lag((df[,i]),1)
  var_lag <- as.data.frame(cbind(var_e, var_lag))
  
  
  # To add more explanatory Variables
  xreg = var_e
  xreg2 = var_lag$var_lag
  xreg3 <- cbind(var_0 = var_lag$var_e,
                 var_1 = var_lag$var_lag)
  
  # Model ARMAX
  fit <- auto.arima(df$lit_chl_exp_kton_met, xreg = xreg)
  fit2 <- auto.arima(df$lit_chl_exp_kton_met, xreg = var_lag$var_lag)
  fit3 <- auto.arima(df$lit_chl_exp_kton_met, xreg = xreg3)
  
  # Save the models
  arimax_chl[[names(df)[i]]] <- fit
  arimax_chl_lag[[names(df)[i]]] <- fit2
  arimax_chl_lag1[[names(df)[i]]] <- fit3
  
  # Keep the model with the lowest AIC
  best_fit <- list(fit, fit2, fit3)[[which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))]]
  
  n_ <-which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))
  best_model_arimax_chl[[paste0(names(df)[i],n_)]] <- best_fit
  
}



df <- data_price_aux
for(i in 5:ncol(df)) {
  
  var_e <- (df[,i])
  var_lag <- lag((df[,i]),1)
  var_lag <- as.data.frame(cbind(var_e, var_lag))
  
  
  # To add more explanatory Variables
  xreg = var_e
  xreg2 = var_lag$var_lag
  xreg3 <- cbind(var_0 = var_lag$var_e,
                 var_1 = var_lag$var_lag)
  
  # Model ARMAX
  fit <- auto.arima(df$lit_chl_exp_kton_met, xreg = xreg)
  fit2 <- auto.arima(df$lit_chl_exp_kton_met, xreg = var_lag$var_lag)
  fit3 <- auto.arima(df$lit_chl_exp_kton_met, xreg = xreg3)
  
  # Save the models
  arimax_chl[[names(df)[i]]] <- fit
  arimax_chl_lag[[names(df)[i]]] <- fit2
  arimax_chl_lag1[[names(df)[i]]] <- fit3
  
  # Keep the model with the lowest AIC
  best_fit <- list(fit, fit2, fit3)[[which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))]]
  
  n_ <-which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))
  best_model_arimax_chl[[paste0(names(df)[i],n_)]] <- best_fit
  
}

df <- data_chargers_aux
for(i in 5:ncol(df)) {
  
  var_e <- (df[,i])
  var_lag <- lag((df[,i]),1)
  var_lag <- as.data.frame(cbind(var_e, var_lag))
  
  
  # To add more explanatory Variables
  xreg = var_e
  xreg2 = var_lag$var_lag
  xreg3 <- cbind(var_0 = var_lag$var_e,
                 var_1 = var_lag$var_lag)
  
  # Model ARMAX
  fit <- auto.arima(df$lit_chl_exp_kton_met, xreg = xreg)
  fit2 <- auto.arima(df$lit_chl_exp_kton_met, xreg = var_lag$var_lag)
  fit3 <- auto.arima(df$lit_chl_exp_kton_met, xreg = xreg3)
  
  # Save the models
  arimax_chl[[names(df)[i]]] <- fit
  arimax_chl_lag[[names(df)[i]]] <- fit2
  arimax_chl_lag1[[names(df)[i]]] <- fit3
  
  # Keep the model with the lowest AIC
  best_fit <- list(fit, fit2, fit3)[[which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))]]
  
  n_ <-which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))
  best_model_arimax_chl[[paste0(names(df)[i],n_)]] <- best_fit
  
}


df <- data_trends_aux
for(i in 5:ncol(df)) {
  
  var_e <- (df[,i])
  var_lag <- lag((df[,i]),1)
  var_lag <- as.data.frame(cbind(var_e, var_lag))
  
  
  # To add more explanatory Variables
  xreg = var_e
  xreg2 = var_lag$var_lag
  xreg3 <- cbind(var_0 = var_lag$var_e,
                 var_1 = var_lag$var_lag)
  
  # Model ARMAX
  fit <- auto.arima(df$lit_chl_exp_kton_met, xreg = xreg)
  fit2 <- auto.arima(df$lit_chl_exp_kton_met, xreg = var_lag$var_lag)
  fit3 <- auto.arima(df$lit_chl_exp_kton_met, xreg = xreg3)
  
  # Save the models
  arimax_chl[[names(df)[i]]] <- fit
  arimax_chl_lag[[names(df)[i]]] <- fit2
  arimax_chl_lag1[[names(df)[i]]] <- fit3
  
  # Keep the model with the lowest AIC
  best_fit <- list(fit, fit2, fit3)[[which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))]]
  
  n_ <-which.min(c(AIC(fit), AIC(fit2), AIC(fit3)))
  best_model_arimax_chl[[paste0(names(df)[i],n_)]] <- best_fit
  
}


best_model_arimax_chl
aic_values <- sapply(best_model_arimax_chl, AIC)
models_orders <- best_model_arimax_chl[order(aic_values)]
models_orders

############### Best Models: Google Trends Lithium ################
model <- arimax_chl_lag[["lit.bat.gtrends.world"]]
summary(model)
#AIC=234.28
resid1_chl<- residuals(model)
tsdisplay(resid1_chl)
checkresiduals(resid1_chl)

plot(data_trends_aux$lit_chl_exp_kton_met, type = "l", col = "black", lwd = 2,
     main = "Google Trends Lithium Batteries World (t-1) ", xlab = "Quarter", ylab = "kT",xaxt = "n")
lines(fitted(model), col = "#00C3B1", lwd = 2, type = "l", lty = "dashed")
axis(1, at=seq(1, 43, by=4), labels=c("2014","2015", "2016", "2017", "2018", "2019",
                                      "2020", "2021", "2022", "2023", "2024"))


############ Stock Market Model Selection ##########################
#This one looks better
model <- arimax_chl_lag[["sqm.stock"]]
summary(model)
resid1_chl<- residuals(model)
tsdisplay(resid1_chl)
checkresiduals(resid1_chl)

plot(data_trends_aux$lit_chl_exp_kton_met, type = "l", col = "black", lwd = 2,
     main = "SQM (t-1) ", xlab = "Quarter", ylab = "kT",xaxt = "n")
lines(fitted(model), col = "#00C3B1", lwd = 2, type = "l", lty = "dashed")
axis(1, at=seq(1, 43, by=4), labels=c("2014","2015", "2016", "2017", "2018", "2019",
                                      "2020", "2021", "2022", "2023", "2024"))

model1 <- arimax_chl_lag1[["mineral.resources.stock"]]
summary(model1)
resid1_chl<- residuals(model1)
tsdisplay(resid1_chl)
checkresiduals(resid1_chl)

plot(data_trends_aux$lit_chl_exp_kton_met, type = "l", col = "black", lwd = 2,
     main = "Mineral Resoueces (t and t-1) ", xlab = "Quarter", ylab = "kT",xaxt = "n")
lines(fitted(model1), col = "#00C3B1", lwd = 2, type = "l", lty = "dashed")
axis(1, at=seq(1, 43, by=4), labels=c("2014","2015", "2016", "2017", "2018", "2019",
                                      "2020", "2021", "2022", "2023", "2024"))

#############################
#ARMAX with multiple variables

var0<- data_trends_aux$mineral.resources.stock
var1<- lag(data_trends_aux$mineral.resources.stock,1)
var2<- lag(data_trends_aux$lit.bat.gtrends.world,1)
var3<- lag(data_trends_aux$sqm.stock,1)
xreg2 <- cbind(var0 = var0,
               var1 = var1,
               var2 = var2,
               var3 = var3)

model_c1 <- Arima(data_trends_aux$lit_chl_exp_kton_met,order = c(1,1,0), xreg = xreg2)
summary(model_c1)
resid1_chl<- residuals(model_c1)
tsdisplay(resid1_chl)
checkresiduals(resid1_chl)


###########################################
#### FORECASTING
############################################

## Forecasting model 2
xreg2=cbind(var0=data_trends_pred$mineral.resources.stock[2:5],
           var1=data_trends_pred$mineral.resources.stock[1:4], 
           var2 =data_trends_pred$lit.bat.gtrends.world[1:4],
           var3 = data_trends_aux$sqm.stock[1:4])

##Forecasting model 2
f1<-forecast(model_c1, h=4, xreg=xreg2)
plot(f1, type = "l", col = "black", lwd = 2,
     main = "", xlab = "Quarter", ylab = "kT",xaxt = "n")
lines(fitted(model_c1), col = "#00C3B1", lwd = 2, type = "l", lty = "dashed")
axis(1, at=seq(1, 43, by=4), labels=c("2014","2015", "2016", "2017", "2018", "2019",
                                      "2020", "2021", "2022", "2023", "2024"))
abline(v=39, col="gray", lwd = 2, lty = 3)


