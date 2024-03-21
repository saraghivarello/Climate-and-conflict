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


data_d <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_norm_gravity_ml.csv")
data_c <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_norm_gravity_c_i.csv")


#lags
form <- Displacements_log ~ TA_dep + PA_dep + DL_dep + conflicts_dep + TA_arr + PA_arr + DL_arr + conflicts_arr + inv_distance_2 + gdp_mean_dep + gdp_mean_arr + population_density_dep + population_density_arr
form_lag1 <- Displacements_log ~ TA_lag1_dep + PA_lag1_dep + DL_lag1_dep + conflicts_lag1_dep + TA_lag1_arr + PA_lag1_arr + DL_lag1_arr + conflicts_lag1_arr + distance + gdp_mean_dep + gdp_mean_arr + pop_count_dep + pop_count_arr
form_lag2 <- Displacements_log ~ TA_lag2_dep + PA_lag2_dep + DL_lag2_dep + conflicts_lag2_dep + TA_lag2_arr + PA_lag2_arr + DL_lag2_arr + conflicts_lag2_arr + distance + gdp_mean_dep + gdp_mean_arr + pop_count_dep + pop_count_arr
form_lag3 <- Displacements_log ~ TA_lag3_dep + PA_lag3_dep + DL_lag3_dep + conflicts_lag3_dep + TA_lag3_arr + PA_lag3_arr + DL_lag3_arr + conflicts_lag3_arr + distance + gdp_mean_dep + gdp_mean_arr + pop_count_dep + pop_count_arr
form_lag4 <- Displacements_log ~ TA_lag4_dep + PA_lag4_dep + DL_lag4_dep + conflicts_lag4_dep + TA_lag4_arr + PA_lag4_arr + DL_lag4_arr + conflicts_lag4_arr + distance + gdp_mean_dep + gdp_mean_arr + pop_count_dep + pop_count_arr
form_lag5 <- Displacements_log ~ TA_lag5_dep + PA_lag5_dep + DL_lag5_dep + conflicts_lag5_dep + TA_lag5_arr + PA_lag5_arr + DL_lag5_arr + conflicts_lag5_arr + distance + gdp_mean_dep + gdp_mean_arr + pop_count_dep + pop_count_arr
form_lag6 <- Displacements_log ~ TA_lag6_dep + PA_lag6_dep + DL_lag6_dep + conflicts_lag6_dep + TA_lag6_arr + PA_lag6_arr + DL_lag6_arr + conflicts_lag6_arr + distance + gdp_mean_dep + gdp_mean_arr + pop_count_dep + pop_count_arr

form_log <- Displacements_log ~ TA_dep_log + PA_dep_log + DL_dep_log + conflicts_dep_log + TA_arr_log + PA_arr_log + DL_arr_log + conflicts_arr_log + distance_log + gdp_mean_dep_log + gdp_mean_arr_log + pop_count_dep_log + pop_count_arr_log
form_lag1_log <- Displacements_log ~ TA_lag1_dep_log + PA_lag1_dep_log + DL_lag1_dep_log + conflicts_lag1_dep_log + TA_lag1_arr_log + PA_lag1_arr_log + DL_lag1_arr_log + conflicts_lag1_arr_log + distance_log + gdp_mean_dep_log + gdp_mean_arr_log + pop_count_dep_log + pop_count_arr_log
form_lag2_log <- Displacements_log ~ TA_lag2_dep_log + PA_lag2_dep_log + DL_lag2_dep_log + conflicts_lag2_dep_log + TA_lag2_arr_log + PA_lag2_arr_log + DL_lag2_arr_log + conflicts_lag2_arr_log + distance_log + gdp_mean_dep_log + gdp_mean_arr_log + pop_count_dep_log + pop_count_arr_log
form_lag3_log <- Displacements_log ~ TA_lag3_dep_log + PA_lag3_dep_log + DL_lag3_dep_log + conflicts_lag3_dep_log + TA_lag3_arr_log + PA_lag3_arr_log + DL_lag3_arr_log + conflicts_lag3_arr_log + distance_log + gdp_mean_dep_log + gdp_mean_arr_log + pop_count_dep_log + pop_count_arr_log
form_lag4_log <- Displacements_log ~ TA_lag4_dep_log + PA_lag4_dep_log + DL_lag4_dep_log + conflicts_lag4_dep_log + TA_lag4_arr_log + PA_lag4_arr_log + DL_lag4_arr_log + conflicts_lag4_arr_log + distance_log + gdp_mean_dep_log + gdp_mean_arr_log + pop_count_dep_log + pop_count_arr_log
form_lag5_log <- Displacements_log ~ TA_lag5_dep_log + PA_lag5_dep_log + DL_lag5_dep_log + conflicts_lag5_dep_log + TA_lag5_arr_log + PA_lag5_arr_log + DL_lag5_arr_log + conflicts_lag5_arr_log + distance_log + gdp_mean_dep_log + gdp_mean_arr_log + pop_count_dep_log + pop_count_arr_log
form_lag6_log <- Displacements_log ~ TA_lag6_dep_log + PA_lag6_dep_log + DL_lag6_dep_log + conflicts_lag6_dep_log + TA_lag6_arr_log + PA_lag6_arr_log + DL_lag6_arr_log + conflicts_lag6_arr_log + distance_log + gdp_mean_dep_log + gdp_mean_arr_log + pop_count_dep_log + pop_count_arr_log

lm <- lm(form, data = data_d)
lm_lag1 <- lm(form_lag1, data = data_c)
lm_lag2 <- lm(form_lag2, data = data_c)
lm_lag3 <- lm(form_lag3, data = data_c)
lm_lag4 <- lm(form_lag4, data = data_c)
lm_lag5 <- lm(form_lag5, data = data_c)
lm_lag6 <- lm(form_lag6, data = data_c)

lm_log <- lm(form_log, data = data_c)
lm_lag1_log <- lm(form_lag1_log, data = data_c)
lm_lag2_log <- lm(form_lag2_log, data = data_c)
lm_lag3_log <- lm(form_lag3_log, data = data_c)
lm_lag4_log <- lm(form_lag4_log, data = data_c)
lm_lag5_log <- lm(form_lag5_log, data = data_c)
lm_lag6_log <- lm(form_lag6_log, data = data_c)

stargazer(lm, lm_lag4_log, type = "text", out = "lm_latex_gravity_log.tex")

#check if the errors have mean zero 
yhat <- predict(lm_lag5_log)
#plot(data_c$Displacements_log, yhat)
#abline(0,1)





# #store regressoin coefficients in a dataframe
# lm_all <- data.frame(lm$coefficients, lm_lag1$coefficients, lm_lag2$coefficients, lm_lag3$coefficients, lm_lag4$coefficients, lm_lag5$coefficients, lm_lag6$coefficients)
# write.csv(lm_all, file = "latex/lm_g.csv")
# # print lm r squared
# lm_r_all <- data.frame(summary(lm)$adj.r.squared, summary(lm_lag1)$adj.r.squared, summary(lm_lag2)$adj.r.squared, summary(lm_lag3)$adj.r.squared, summary(lm_lag4)$adj.r.squared, summary(lm_lag5)$adj.r.squared, summary(lm_lag6)$adj.r.squared)
# write.csv(lm_r_all, file = "latex/lm_r_g.csv")
# # print lm p values
# lm_p_all <- data.frame(summary(lm)$coefficients[,4], summary(lm_lag1)$coefficients[,4], summary(lm_lag2)$coefficients[,4], summary(lm_lag3)$coefficients[,4], summary(lm_lag4)$coefficients[,4], summary(lm_lag5)$coefficients[,4], summary(lm_lag6)$coefficients[,4])
# write.csv(lm_p_all, file = "latex/lm_p_g.csv")
# # print Observations with nobs()
# lm_obs_all <- data.frame(nobs(lm))
# write.csv(lm_obs_all, file = "latex/lm_obs_g.csv")


# lm_s_log <- data.frame(lm_log$coefficients, lm_lag1_log$coefficients, lm_lag2_log$coefficients, lm_lag3_log$coefficients, lm_lag4_log$coefficients, lm_lag5_log$coefficients, lm_lag6_log$coefficients)
# write.csv(lm_s_log, file = "latex/lm_log_g.csv")
# # print lm r squared
# lm_r_log <- data.frame(summary(lm_log)$adj.r.squared, summary(lm_lag1_log)$adj.r.squared, summary(lm_lag2_log)$adj.r.squared, summary(lm_lag3_log)$adj.r.squared, summary(lm_lag4_log)$adj.r.squared, summary(lm_lag5_log)$adj.r.squared, summary(lm_lag6_log)$adj.r.squared)
# write.csv(lm_r_log, file = "latex/lm_r_log_g.csv")
# # print lm p values
# lm_p_log <- data.frame(summary(lm_log)$coefficients[,4], summary(lm_lag1_log)$coefficients[,4], summary(lm_lag2_log)$coefficients[,4], summary(lm_lag3_log)$coefficients[,4], summary(lm_lag4_log)$coefficients[,4], summary(lm_lag5_log)$coefficients[,4], summary(lm_lag6_log)$coefficients[,4])
# write.csv(lm_p_log, file = "latex/lm_p_log_g.csv")
# # print Observations with nobs()
# lm_obs_log <- data.frame(nobs(lm_log))
# write.csv(lm_obs_log, file = "latex/lm_obs_log_g.csv")



