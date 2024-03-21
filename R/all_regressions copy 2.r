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


data_d2 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_std_log_d_lag.csv")


form_lmh <- conflicts ~  TA + PA + DL + TA_lag1 + PA_lag1 + DL_lag1 + sum_disp + sum_disp # + population_density

lmh_log <- lm(form_lmh, data = data_d2)
#stargazer(lmh_log, lmh_log1, lmh_log2, type = "latex", out = "lmh_log_latex.tex")

log_two <- plm(form_lmh, data = data_d2, model = "within", index = c("admin1","month"), effect = "twoways")
stargazer(lmh_log, log_two, type = "text", out = "plm_conf_log_latex.tex")

