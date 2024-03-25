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


data_d <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_norm_districts_c_dist.csv")
#data_d <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_districts.csv")

#lag 
form_lag1 <- Displacements_log ~ TA_lag2_dep + PA_lag2_dep + DL_lag2_dep + conflicts_lag2_dep + TA_lag2_arr + PA_lag2_arr + DL_lag2_arr + conflicts_lag2_arr + dist_centroids + pop_count_dep + pop_count_arr + gdp_mean_dep + gdp_mean_arr
form_lag1_log <- Displacements_log ~ TA_lag2_dep_log + PA_lag2_dep_log + DL_lag2_dep_log + conflicts_lag2_dep_log + TA_lag1_arr_log + PA_lag1_arr_log + DL_lag1_arr_log + conflicts_lag1_arr_log 


lm <- lm(form_lag1, data = data_d)
lm_lag1_log <- lm(form_lag1_log, data = data_d)

plm_lag1_i <- plm(form_lag1, data = data_d, index = c("Current..Arrival..District", "time"), model = "within", effect = "individual")
plm_lag1_t <- plm(form_lag1, data = data_d, index = c("Current..Arrival..District", "time"), model = "within", effect = "time")
plm_lag1_two <- plm(form_lag1, data = data_d, index = c("Current..Arrival..District", "time"), model = "within", effect = "twoways")


stargazer(lm, plm_lag1_two, type = "latex", out = "lm_districts.tex")


#compute vif
vif_values <- vif(lm_lag1_log)
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



