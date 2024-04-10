#Setting the path
setwd("C:/Users/alecr/OneDrive/Escritorio/Master Data Science/Tercer Semestre/Financial Data/proyect")

###Libraries##################################
library(openxlsx)
library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)
library(fpp2)
library(zoo)
library(dplyr)
library(ggplot2)

##############################################

source("C:/Users/alecr/OneDrive/Escritorio/Master Data Science/Tercer Semestre/Financial Data/proyect/transform_data.R")

#lit.total.exp.quarter
# Exports Chile in kton met
lit.chl.exp.quarter.kton <- lit.chl.exp.quarter$lit_chl_exp_kton_met
acf.chile <- acf(lit.chl.exp.quarter.kton, plot = F)
plot(acf.chile,     main = " ", xlab = "lag")

# Exports AUS in kton met
lit.aus.exp.kton <-  lit.aus.exp$lit_aus_exp_kton_met
acf.aus <- acf(lit.aus.exp.kton, plot = F)
plot(acf.aus,     main = " ", xlab = "lag")

# Clear tendency in both series


plot(lit.aus.exp$quarter, lit.aus.exp.quarter.kton, type = "l", col = "#00C3B1", lwd = 2, xlab = "Quarter", ylab = "kT", main = "Australia")
abline(v=2010, col="violet", lwd = 2, lty = 2)
abline(v=2015, col="violet", lwd = 2, lty = 2)
abline(v=2018, col="violet", lwd = 2, lty = 2)
abline(v=2020, col="violet", lwd = 2, lty = 2)

plot(lit.chl.exp.quarter$quarter, lit.chl.exp.quarter.kton, type = "l", col = "#00C3B1", lwd = 2, xlab = "Quarter", ylab = "kT", main = "Chile")
abline(v=2014, col="violet", lwd = 2, lty = 2)
abline(v=2016, col="violet", lwd = 2, lty = 2)
abline(v=2021, col="violet", lwd = 2, lty = 2)

ts.chl <- ts(lit.chl.exp.quarter.kton, start=2014, frequency = 4)
ts.aus <- ts(lit.aus.exp.kton, start=2010, frequency = 4)

plot(ts.aus,  type = "l", col = "#00C3B1", lwd = 2,
     main = "Exports", xlab = "Quarter", ylab = "kT")
lines(ts.chl,  type = "l", col = "violet", lwd = 2)
legend("topleft", c("Australia", "Chile"),
       col = c("#00C3B1", "purple"), lty = c(1, 2))

###############################################
### GBM AUSTRALIA ###
###############################################
# Double rectangular shock

GBM_aus_exp_rett2 <- GBM(lit.aus.exp.quarter.kton, oos = 4, shock = "rett", nshock = 2, prelimestimates = c(5.742860e+03, 1.144887e-03, 6.183144e-02, 29, 40, 0.1, 46, 48, -0.1))
summary(GBM_aus_exp_rett2)
checkresiduals(GBM_aus_exp_rett2)

# Plot
cum.aus.ex.pts <- ts(cumsum(lit.aus.exp.quarter.kton), start=2010, frequency = 4)
cum.gbm.aus.exp <-  ts(fitted(GBM_aus_exp_rett2), start=2010, frequency = 4) 
plot(cum.aus.ex.pts,  type = "l", col = "black", lwd = 2,
     main = "Cumulative", xlab = "Quarter", ylab = "kT")
lines(cum.gbm.aus.exp  , col = "#00C3B1", lwd = 2, type = "l", lty = "dashed") 

plot(lit.aus.exp$quarter, lit.aus.exp$lit_aus_exp_kton_met,  type = "l", col = "black", lwd = 2,
     main = "Instantaneous", xlab = "Quarter", ylab = "kT", xlim = c(2010, 2024))
lines(ts(make.instantaneous(predict(GBM_aus_exp_rett2, newx = c(1:(length(lit.aus.exp.quarter.kton)+4)))), frequency = 4, start = 2010, end = 2024), col = "#00C3B1", lwd = 2, type = "l", lty = "dashed")

###############################################
### GBM Chile ###
###############################################
# Mixed Shock
GBM_chl_exp_mix <- GBM(lit.chl.exp.quarter.kton, oos = 4, shock = "mixed", nshock = 2, prelimestimates = c(5.672961e+04, 2.953943e-04, 2.662497e-02, 25, 0.1, 0.1, 7, 14, 0.1))
summary(GBM_chl_exp_mix)
checkresiduals(GBM_chl_exp_mix)

# Plot
cum.chl.ex.pts <- ts(cumsum(lit.chl.exp.quarter.kton), start=2014, frequency = 4)
cum.gbm.chl.exp <-  ts(fitted(GBM_chl_exp_mix), start=2014, frequency = 4) 
plot(cum.chl.ex.pts,  type = "l", col = "black", lwd = 2,
     main = "Cumulative", xlab = "Quarter", ylab = "kT")
lines(cum.gbm.chl.exp  , col = "#00C3B1", lwd = 2, type = "l", lty = "dashed") 

plot(lit.chl.exp.quarter$quarter, lit.chl.exp.quarter$lit_chl_exp_kton_met,  type = "l", col = "black", lwd = 2,
     main = "Instantaneous", xlab = "Quarter", ylab = "kT", xlim = c(2014, 2024))
lines(ts(make.instantaneous(GBM_chl_exp_mix$fitted), frequency = 4, start=2014), col = "#00C3B1", lwd = 2, type = "l", lty = "dashed")
abline(v=2023.5, col="gray", lwd = 2, lty = 3)

###############################################
### GGM: EXPORTS ###
###############################################

## EXPORTATIONS ##
#Australia
## BM
bm_aus_exp <- BM(lit.aus.exp.quarter.kton, display = T, oos = 0)
summary(bm_aus_exp)

## GGM 
# preliminaries = m,a,b,p,q
m_aus_exp <- bm_aus_exp[["coefficients"]][["m"]]
p_aus_exp <- bm_aus_exp[["coefficients"]][["p"]]
q_aus_exp <- bm_aus_exp[["coefficients"]][["q"]]

GGM_aus_exp <- GGM(lit.aus.exp.quarter.kton, prelimestimates=c(m_aus_exp, 0.1, 0.06, p_aus_exp,q_aus_exp))
summary(GGM_aus_exp)


# Chile
# Quarter
## BM

bm_chl_exp <- BM(lit.chl.exp.quarter.kton, display = T, oos = 0)
summary(bm_chl_exp)

## GGM 
# preliminaries = m,a,b,p,q
m_chl_exp <- bm_chl_exp[["coefficients"]][["m"]]
p_chl_exp <- bm_chl_exp[["coefficients"]][["p"]]
q_chl_exp <- bm_chl_exp[["coefficients"]][["q"]]

GGM_chl_exp <- GGM(lit.chl.exp.quarter.kton, prelimestimates=c(m_chl_exp, 0.01, 0.08, p_chl_exp,q_chl_exp), oos = 0)
summary(GGM_chl_exp)

###############################################
### COMPETITION: EXPORTS ###
###############################################

lit.aus.exp.quarter.kton2 <- lit.aus.exp.quarter.kton[17:55]

bass_comp<- UCRCD(lit.aus.exp.quarter.kton2,lit.chl.exp.quarter.kton, display=T)
summary(bass_comp,)
coef(bass_comp)


### q1c is pos and q2-gamma in neg:  2 collaborates with 1 , 1 competes with 2
### only q1c and delta are significants
### 1 is Australia, 2 is Chile


#CUMULATIVE
cum.chl.comp <- ts(cumsum(lit.chl.exp.quarter.kton), start=2014, frequency = 4)
cum.aus.comp <- ts(cumsum(lit.aus.exp.quarter.kton2), start=2014, frequency = 4)
cum.comp.fit <-  fitted(bass_comp) 
cum.comp.fit.chl <-  ts(cum.comp.fit[[2]], start=2014, frequency = 4) 
cum.comp.fit.aus <-  ts(cum.comp.fit[[1]], start=2014, frequency = 4)  

plot(cum.aus.comp,  type = "l", col = "black", lwd = 2,
     main = "Exports - Cumulative", xlab = "Quarter", ylab = "kt")
lines(cum.chl.comp,  type = "l", col = "black", lwd = 2)
lines(cum.comp.fit.chl, col = "#00C3B1", lwd = 2, type = "l", lty = "dashed") 
lines(cum.comp.fit.aus , col = "purple", lwd = 2, type = "l", lty = "dashed")
legend("topleft", c("Chile", "Australia"),
       col = c("#00C3B1", "purple"), lty = c(1, 2))


#INSTANTANEUS
ts.chl.comp <- ts(lit.chl.exp.quarter.kton, start=2014, frequency = 4)
ts.aus.comp <- ts(lit.aus.exp.quarter.kton2, start=2014, frequency = 4)

inst.comp.fit <-  fitted(bass_comp)
inst.comp.fit.chl <-  ts(make.instantaneous(inst.comp.fit[[2]]), start=2014, frequency = 4) 
inst.comp.fit.aus <-  ts(make.instantaneous(inst.comp.fit[[1]]), start=2014, frequency = 4) 

plot(ts.aus.comp,  type = "l", col = "black", lwd = 2,
     main = "Exports - Instantaneous", xlab = "Quarter", ylab = "kt")
lines(ts.chl.comp,  type = "l", col = "grey", lwd = 2)
lines(inst.comp.fit.chl, col = "#00C3B1", lwd = 2, type = "l", lty = "dashed") 
lines(inst.comp.fit.aus , col = "purple", lwd = 2, type = "l", lty = "dashed")
legend("topleft", c("Chile", "Australia"),
       col = c("#00C3B1", "purple"), lty = c(1, 2))

##################################################
# Holt's exponential smoothing Chile
#################################################
fit1<- holt(lit.chl.exp.quarter.kton, h=4)
summary(fit1)
checkresiduals(fit1)

# PLOT
plot(forecast(fit1, h=4),  type = "l", col = "black", lwd = 2,
     main = "Cumulative", xlab = "Quarter", ylab = "kT", xaxt = "n")
lines(fitted(fit1)  , col = "#00C3B1", lwd = 2, type = "l", lty = "dashed") 
axis(1, at=seq(1, 43, by=4), labels=c("2014","2015", "2016", "2017", "2018", "2019",
                                      "2020", "2021", "2022", "2023", "2024"))
abline(v=39, col="gray", lwd = 2, lty = 3)


##################################################
# Holt's exponential smoothing Australia
#################################################
fit1<- holt(lit.aus.exp.quarter.kton, h=4)
summary(fit1)
checkresiduals(fit1)

# PLOT
plot(forecast(fit1, h=4),  type = "l", col = "black", lwd = 2,
     main = "", xlab = "Quarter", ylab = "kT", xaxt = "n")
lines(fitted(fit1)  , col = "#00C3B1", lwd = 2, type = "l", lty = "dashed") 
axis(1, at=seq(1, 59, by=4), labels=c("2010","2011","2012","2013","2014","2015", "2016", "2017", "2018", "2019",
                                      "2020", "2021", "2022", "2023", "2024"))
abline(v=55, col="gray", lwd = 2, lty = 3)
