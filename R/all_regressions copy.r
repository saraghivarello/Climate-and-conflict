library(stargazer)
library(pspatreg)
library(spatialreg)
library(spdep)
library(sf)
library(plm)
library(ggplot2)
library(dplyr)
library(splm)
library(rgdal)
library(car)

data_confl <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/df_confl.csv")


form_lmh <- Disp_log ~ x + TA_lag2 + PA_lag2 + DL_lag2 + inv_distance + conflicts + gdp_mean_origin  + gdp_mean_destination + population_density_origin + population_density_destination #+ accessibility_to_cities_mean_origin + accessibility_to_cities_mean_destination#+ PA_lag3 + DL_lag3 + inv_distance
form <- Disp_log ~ x + TA_lag2 + PA_lag2 + DL_lag2 + inv_distance + conflicts  #+gdp_mean_origin  + gdp_mean_destination 
form_1 <- Disp_log ~ x + TA_lag2 + PA_lag2 + DL_lag2 + inv_distance_2 + conflicts  +gdp_mean_origin + gdp_mean_destination 

#no fixed effects
lmh <- lm(form_lmh, data = data_confl)
#stargazer(lmh, type = "text", out = "lmh_log.txt")

lmh_log <- lm(form_lmh, data = data_confl)
lmh_log1 <- lm(form, data = data_confl)
lmh_log2 <- lm(form_1, data = data_confl)
stargazer(lmh_log, lmh_log1, lmh_log2, type = "text", out = "lmh_log_latex.tex")


#all regions log
log_ind <- plm(form, data = data_confl, model = "within", index = c("Current..Arrival..Region","month"), effect = "individual")
log_time <- plm(form, data = data_confl, model = "within", index = c("Current..Arrival..Region","month"), effect = "time")
log_two <- plm(form, data = data_confl, model = "within", index = c("Current..Arrival..Region","month"), effect = "twoways")
stargazer(log_ind, log_time, log_two, type = "text", out = "plm_log_latex.tex")
