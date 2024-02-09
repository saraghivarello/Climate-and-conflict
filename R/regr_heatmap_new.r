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


data_d <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_new_all.csv")
data_c <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_new_all_c.csv")


#lags
form <- Disp_log ~ TA_dep + PA_dep + DL_dep + conflicts_dep + TA_arr + PA_arr + DL_arr + conflicts_arr + inv_distance_2 + gdp_mean_dep + gdp_mean_arr + population_density_dep + population_density_arr
form_lag1 <- Disp_log ~ TA_lag1_dep + PA_lag1_dep + DL_lag1_dep + conflicts_dep + TA_lag1_arr + PA_lag1_arr + DL_lag1_arr + conflicts_arr + inv_distance_2 + gdp_mean_dep + gdp_mean_arr + population_density_dep + population_density_arr
form_lag2 <- Disp_log ~ TA_lag2_dep + PA_lag2_dep + DL_lag2_dep + conflicts_dep + TA_lag2_arr + PA_lag2_arr + DL_lag2_arr + conflicts_arr + inv_distance_2 + gdp_mean_dep + gdp_mean_arr + population_density_dep + population_density_arr
form_lag3 <- Disp_log ~ TA_lag3_dep + PA_lag3_dep + DL_lag3_dep + conflicts_dep + TA_lag3_arr + PA_lag3_arr + DL_lag3_arr + conflicts_arr + inv_distance_2 + gdp_mean_dep + gdp_mean_arr + population_density_dep + population_density_arr
form_lag4 <- Disp_log ~ TA_lag4_dep + PA_lag4_dep + DL_lag4_dep + conflicts_dep + TA_lag4_arr + PA_lag4_arr + DL_lag4_arr + conflicts_arr + inv_distance_2 + gdp_mean_dep + gdp_mean_arr + population_density_dep + population_density_arr
form_lag5 <- Disp_log ~ TA_lag5_dep + PA_lag5_dep + DL_lag5_dep + conflicts_dep + TA_lag5_arr + PA_lag5_arr + DL_lag5_arr + conflicts_arr + inv_distance_2 + gdp_mean_dep + gdp_mean_arr + population_density_dep + population_density_arr
form_lag6 <- Disp_log ~ TA_lag6_dep + PA_lag6_dep + DL_lag6_dep + conflicts_dep + TA_lag6_arr + PA_lag6_arr + DL_lag6_arr + conflicts_arr + inv_distance_2 + gdp_mean_dep + gdp_mean_arr + population_density_dep + population_density_arr

# form <- Disp_log ~ TA_dep + PA_dep + DL_dep + conflicts_dep + inv_distance_2 + gdp_mean_dep + gdp_mean_arr + population_density_dep + population_density_arr
# form_lag1 <- Disp_log ~ TA_lag1_dep + PA_lag1_dep + DL_lag1_dep + conflicts_dep + inv_distance_2 + gdp_mean_dep + gdp_mean_arr + population_density_dep + population_density_arr
# form_lag2 <- Disp_log ~ TA_lag2_dep + PA_lag2_dep + DL_lag2_dep + conflicts_dep + inv_distance_2 + gdp_mean_dep + gdp_mean_arr + population_density_dep + population_density_arr
# form_lag3 <- Disp_log ~ TA_lag3_dep + PA_lag3_dep + DL_lag3_dep + conflicts_dep + inv_distance_2 + gdp_mean_dep + gdp_mean_arr + population_density_dep + population_density_arr
# form_lag4 <- Disp_log ~ TA_lag4_dep + PA_lag4_dep + DL_lag4_dep + conflicts_dep + inv_distance_2 + gdp_mean_dep + gdp_mean_arr + population_density_dep + population_density_arr
# form_lag5 <- Disp_log ~ TA_lag5_dep + PA_lag5_dep + DL_lag5_dep + conflicts_dep + inv_distance_2 + gdp_mean_dep + gdp_mean_arr + population_density_dep + population_density_arr
# form_lag6 <- Disp_log ~ TA_lag6_dep + PA_lag6_dep + DL_lag6_dep + conflicts_dep + inv_distance_2 + gdp_mean_dep + gdp_mean_arr + population_density_dep + population_density_arr

#all years
lm <- lm(form, data = data_d)
lm_lag1 <- lm(form_lag1, data = data_d)
lm_lag2 <- lm(form_lag2, data = data_d)
lm_lag3 <- lm(form_lag3, data = data_d)
lm_lag4 <- lm(form_lag4, data = data_d)
lm_lag5 <- lm(form_lag5, data = data_d)
lm_lag6 <- lm(form_lag6, data = data_d)
stargazer(lm, lm_lag1, lm_lag2, lm_lag3, lm_lag4, lm_lag5, lm_lag6, type = "text", out = "latex/lm_latex.tex")



#store regressoin coefficients in a dataframe
lm_all <- data.frame(lm$coefficients, lm_lag1$coefficients, lm_lag2$coefficients, lm_lag3$coefficients, lm_lag4$coefficients, lm_lag5$coefficients, lm_lag6$coefficients)
write.csv(lm_all, file = "latex/lm_all_new.csv")
# print lm r squared
lm_r_all <- data.frame(summary(lm)$adj.r.squared, summary(lm_lag1)$adj.r.squared, summary(lm_lag2)$adj.r.squared, summary(lm_lag3)$adj.r.squared, summary(lm_lag4)$adj.r.squared, summary(lm_lag5)$adj.r.squared, summary(lm_lag6)$adj.r.squared)
write.csv(lm_r_all, file = "latex/lm_r_all_new.csv")
# print lm p values
lm_p_all <- data.frame(summary(lm)$coefficients[,4], summary(lm_lag1)$coefficients[,4], summary(lm_lag2)$coefficients[,4], summary(lm_lag3)$coefficients[,4], summary(lm_lag4)$coefficients[,4], summary(lm_lag5)$coefficients[,4], summary(lm_lag6)$coefficients[,4])
write.csv(lm_p_all, file = "latex/lm_p_all_new.csv")
# print Observations with nobs()
lm_obs_all <- data.frame(nobs(lm))
write.csv(lm_obs_all, file = "latex/lm_obs_new.csv")




#conflict
#all years
lm <- lm(form, data = data_c)
lm_lag1 <- lm(form_lag1, data = data_c)
lm_lag2 <- lm(form_lag2, data = data_c)
lm_lag3 <- lm(form_lag3, data = data_c)
lm_lag4 <- lm(form_lag4, data = data_c)
lm_lag5 <- lm(form_lag5, data = data_c)
lm_lag6 <- lm(form_lag6, data = data_c)
stargazer(lm, lm_lag1, lm_lag2, lm_lag3, lm_lag4, lm_lag5, lm_lag6, type = "text", out = "lm_latex_confl.tex")

#store regression coefficients in a dataframe
lm_all <- data.frame(lm$coefficients, lm_lag1$coefficients, lm_lag2$coefficients, lm_lag3$coefficients, lm_lag4$coefficients, lm_lag5$coefficients, lm_lag6$coefficients)
write.csv(lm_all, file = "latex/lm_all_new_confl.csv")
# print lm r squared
lm_r_all <- data.frame(summary(lm)$adj.r.squared, summary(lm_lag1)$adj.r.squared, summary(lm_lag2)$adj.r.squared, summary(lm_lag3)$adj.r.squared, summary(lm_lag4)$adj.r.squared, summary(lm_lag5)$adj.r.squared, summary(lm_lag6)$adj.r.squared)
write.csv(lm_r_all, file = "latex/lm_r_all_new_confl.csv")
# print lm p values
lm_p_all <- data.frame(summary(lm)$coefficients[,4], summary(lm_lag1)$coefficients[,4], summary(lm_lag2)$coefficients[,4], summary(lm_lag3)$coefficients[,4], summary(lm_lag4)$coefficients[,4], summary(lm_lag5)$coefficients[,4], summary(lm_lag6)$coefficients[,4])
write.csv(lm_p_all, file = "latex/lm_p_all_new_confl.csv")
# print Observations with nobs()
lm_obs_all <- data.frame(nobs(lm))
write.csv(lm_obs_all, file = "latex/lm_obs_new_confl.csv")
