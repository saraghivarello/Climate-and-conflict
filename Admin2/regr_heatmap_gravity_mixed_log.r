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


#data_d <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_norm_districts_dist.csv")
#data_c <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_norm_districts_c_dist.csv")
data_d <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_distr_std.csv") 
data_c <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_distr_std_c.csv")


#formulas 
form <- log(Displ_norm_origin) ~ TA_lag2_dep + PA_lag2_dep + DL_lag2_dep + conflicts_lag2_dep + TA_lag2_arr + PA_lag2_arr + DL_lag2_arr + conflicts_lag2_arr + dist_centroids + population_dep + population_arr + gdp_dep + gdp_arr
#form_log <- Displacements_log ~ TA_lag2_dep_log + PA_lag2_dep_log + DL_lag2_dep_log + conflicts_lag2_dep_log + TA_lag2_arr_log + PA_lag2_arr_log + DL_lag2_arr_log + conflicts_lag2_arr_log + dist_centroids_log + pop_count_dep_log + pop_count_arr_log + gdp_mean_dep_log + gdp_mean_arr_log


#drought

lm <- lm(form, data = data_d)
plm_i <- plm(form, data = data_d, index = c("Current..Arrival..District", "time"), model = "within", effect = "individual")
plm_t <- plm(form, data = data_d, index = c("Current..Arrival..District", "time"), model = "within", effect = "time")
plm_two <- plm(form, data = data_d, index = c("Current..Arrival..District", "time"), model = "within", effect = "twoways")

# lm_log <- lm(form_log, data = data_d)
# plm_i_log <- plm(form_log, data = data_d, index = c("Current..Arrival..District", "time"), model = "within", effect = "individual")
# plm_t_log <- plm(form_log, data = data_d, index = c("Current..Arrival..District", "time"), model = "within", effect = "time")
# plm_two_log <- plm(form_log, data = data_d, index = c("Current..Arrival..District", "time"), model = "within", effect = "twoways")

stargazer(lm, plm_two, type = "latex", out = "lm_districts.tex")
# #conflict

lm_c <- lm(form, data = data_c)
plm_i_c <- plm(form, data = data_c, index = c("Current..Arrival..District", "time"), model = "within", effect = "individual")
plm_t_c <- plm(form, data = data_c, index = c("Current..Arrival..District", "time"), model = "within", effect = "time")
plm_two_c <- plm(form, data = data_c, index = c("Current..Arrival..District", "time"), model = "within", effect = "twoways")

# lm_log_c <- lm(form_log, data = data_c)
# plm_i_log_c <- plm(form_log, data = data_c, index = c("Current..Arrival..District", "time"), model = "within", effect = "individual")
# plm_t_log_c <- plm(form_log, data = data_c, index = c("Current..Arrival..District", "time"), model = "within", effect = "time")
# plm_two_log_c <- plm(form_log, data = data_c, index = c("Current..Arrival..District", "time"), model = "within", effect = "twoways")

stargazer(lm_c, plm_two_c, type = "latex", out = "lm_districts.tex")


#compute vif
#vif_values <- vif(lm_lag1_log)
#barplot(vif_values, main="VIF Values", horiz=TRUE, col="skyblue", las=1, cex.names=1)




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



