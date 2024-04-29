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


data <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_districts_all_r.csv")
data_n <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_districts_norm.csv")

#lag 
form <- conflicts ~ TA_lag1 + PA_lag1 + DL_lag1 + sum_disp_lag1 + gdp_mean + pop_count

lm <- lm(form, data = data_n)

plm_i <- plm(form, data = data_n, index = c("admin2"), model = "within", effect = "individual")
plm_t <- plm(form, data = data_n, index = c("time"), model = "within", effect = "time")
plm_two <- plm(form, data = data_n, index = c("admin2", "time"), model = "within", effect = "twoways")

stargazer(lm, plm_two, type = "text", out = "plm_districts.tex")


# Gravity model
form_log <- conflicts_log ~ TA_lag1_log + PA_lag1_log + DL_lag1_log + sum_disp_lag1_log + pop_count_log + gdp_mean_log

lm_log <- lm(form_log, data = data_n)

plm_i_log <- plm(form_log, data = data_n, index = c("admin2"), model = "within", effect = "individual")
plm_t_log <- plm(form_log, data = data_n, index = c("time"), model = "within", effect = "time")
plm_two_log <- plm(form_log, data = data_n, index = c("admin2", "time"), model = "within", effect = "twoways")

stargazer(lm_log, plm_two_log, type = "text", out = "plm_districts_log.tex")



#compute vif
vif_values <- vif(lm)
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



