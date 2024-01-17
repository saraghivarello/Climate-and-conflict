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

data_confl <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_confl.csv")
data_d <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_norm6.csv")


#DROUGHT
form_d_1 <- Disp_log ~ x + conflicts  +gdp_mean_origin + gdp_mean_destination  + population_density_origin + population_density_destination #+ accessibility_to_cities_mean_origin + accessibility_to_cities_mean_destination#+ PA_lag3 + DL_lag3 + inv_distance
form_d_2 <- Disp_log ~ TA_lag6 + PA_lag6 + DL_lag6 + inv_distance_2 + conflicts  +gdp_mean_origin + gdp_mean_destination + population_density_origin + population_density_destination #+ accessibility_to_cities_mean_origin + accessibility_to_cities_mean_destination#+ PA_lag3 + DL_lag3 + inv_distance
form_d_3 <- Disp_log ~ x + TA_lag6 + PA_lag6 + DL_lag6 + conflicts +  inv_distance_2  +gdp_mean_origin + gdp_mean_destination  + population_density_origin + population_density_destination #+ accessibility_to_cities_mean_origin + accessibility_to_cities_mean_destination#+ PA_lag3 + DL_lag3 + inv_distance

lmh_df <- lm(form_d_1, data = data_d)
lmh_df1 <- lm(form_d_2, data = data_d)
lmh_df2 <- lm(form_d_3, data = data_d)
stargazer(lmh_df, lmh_df1, lmh_df2, type = "latex", out = "lmh_df_latex.tex")


#CONFLICTS
form_1 <- Disp_log ~ x  + gdp_mean_origin  + gdp_mean_destination + population_density_origin + population_density_destination #+ PA_lag3 + DL_lag3 + inv_distance
form_2 <- Disp_log ~  conflicts + inv_distance_2 + gdp_mean_origin  + gdp_mean_destination + population_density_origin + population_density_destination #+ accessibility_to_cities_mean_origin + accessibility_to_cities_mean_destination#+ PA_lag3 + DL_lag3 + inv_distance
form_3 <- Disp_log ~ x  + conflicts + inv_distance_2  + gdp_mean_origin + gdp_mean_destination + population_density_origin + population_density_destination
f_plm <- Disp_log ~ x #+ conflicts + inv_distance_2  


lmh_log <- lm(form_1, data = data_confl)
lmh_log1 <- lm(form_2, data = data_confl)
lmh_log2 <- lm(form_3, data = data_confl)
#stargazer(lmh_log, lmh_log1, lmh_log2, type = "text", out = "lmh_log_latex.tex")

