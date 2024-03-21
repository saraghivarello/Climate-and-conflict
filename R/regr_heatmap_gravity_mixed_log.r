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
data_c <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_norm_gravity_ml_c.csv")
data_z <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/gravity_zeros.csv")
data_z_c <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/gravity_zeros_c.csv")


#lags
form <- Displacements_log ~ TA_dep_lag_based_on_distance + PA_dep_lag_based_on_distance + DL_dep_lag_based_on_distance + conflicts_dep_lag_based_on_distance + TA_arr + PA_arr + DL_arr + conflicts_arr + distance + gdp_mean_dep + gdp_mean_arr + pop_count_dep + pop_count_arr
form1 <- Displacements_log ~ TA_lag_dep_based_on_distance + PA_lag_dep_based_on_distance + DL_lag_dep_based_on_distance + conflicts_lag_dep_based_on_distance + TA_lag1_arr + PA_lag1_arr + DL_lag1_arr + conflicts_lag1_arr + distance + gdp_mean_dep + gdp_mean_arr + pop_count_dep + pop_count_arr

#log
form_log <- Displacements_log ~ log(TA_dep_lag_based_on_distance) + log(PA_dep_lag_based_on_distance) + log(DL_dep_lag_based_on_distance) + log(conflicts_dep_lag_based_on_distance) + TA_arr_log + PA_arr_log + DL_arr_log + conflicts_arr_log + distance_log + gdp_mean_dep_log + gdp_mean_arr_log + pop_count_dep_log + pop_count_arr_log
form1_log <- Displacements_log ~ log(TA_lag_dep_based_on_distance) + log(PA_lag_dep_based_on_distance) + log(DL_lag_dep_based_on_distance) + log(conflicts_lag_dep_based_on_distance) + TA_lag1_arr_log + PA_lag1_arr_log + DL_lag1_arr_log + conflicts_lag1_arr_log + distance_log + gdp_mean_dep_log + gdp_mean_arr_log + pop_count_dep_log + pop_count_arr_log

#lag 
form_lag1 <- Displacements_log ~ TA_lag2_dep + PA_lag2_dep + DL_lag2_dep + conflicts_lag2_dep + TA_lag1_arr + PA_lag1_arr + DL_lag1_arr + conflicts_lag1_arr + distance + gdp_mean_dep + gdp_mean_arr + pop_count_dep + pop_count_arr
form_lag1_log <- Displacements_log ~ TA_lag2_dep_log + PA_lag2_dep_log + DL_lag2_dep_log + conflicts_lag2_dep_log + TA_lag1_arr_log + PA_lag1_arr_log + DL_lag1_arr_log + conflicts_lag1_arr_log + distance_log + gdp_mean_dep_log + gdp_mean_arr_log + pop_count_dep_log + pop_count_arr_log


lm <- lm(form, data = data_d)
lm1 <- lm(form, data = data_c)


lm_lag1 <- lm(form_lag1, data = data_d)
lm1_lag1 <- lm(form_lag1, data = data_c)

plm_lag1 <- plm(form_lag1, data = data_d, index = c("Previous..Departure..Region", "time"), model = "within", effect = "time")
plm1_lag1 <- plm(form_lag1, data = data_c, index = c("Previous..Departure..Region", "time"), model = "within", effect = "time")

lm_lag1_log <- lm(form_lag1_log, data = data_d)
lm1_lag1_log <- lm(form_lag1_log, data = data_c)

stargazer(lm_lag1_log, lm1_lag1_log, type = "latex", out = "lm_latex_gravity_log.tex")

#check if the errors have mean zero 
#yhat <- predict(lm_lag5_log)
#plot(data_c$Displacements_log, yhat)
#abline(0,1)

#compute vif
vif_values <- vif(lm_lag1_log)

#create bar chart to display each VIF value
#show all labels
#names(vif_values) <- names(lm_lag1_log$coefficients)

# Create a horizontal bar chart
barplot(vif_values, main="VIF Values", horiz=TRUE, col="skyblue", las=1, cex.names=1)

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



