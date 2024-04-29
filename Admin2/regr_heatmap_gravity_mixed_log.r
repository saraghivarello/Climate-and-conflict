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

data_r_d <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_reg_d_std.csv")
data_r_c <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_reg_c_std.csv")


#formulas 
form <- log(Displ_norm_origin) ~ TA_dep + PA_dep + DL_dep + conflicts_dep + TA_arr + PA_arr + DL_arr + conflicts_arr + distance + population_dep + population_arr + gdp_mean_dep + gdp_mean_arr
form_lag1 <- log(Displ_norm_origin) ~ TA_lag1_dep + PA_lag1_dep + DL_lag1_dep + conflicts_lag1_dep + TA_lag1_arr + PA_lag1_arr + DL_lag1_arr + conflicts_lag1_arr + distance + population_dep + population_arr + gdp_mean_dep + gdp_mean_arr
form_lag2 <- log(Displ_norm_origin) ~ TA_lag2_dep + PA_lag2_dep + DL_lag2_dep + conflicts_lag2_dep + TA_lag2_arr + PA_lag2_arr + DL_lag2_arr + conflicts_lag2_arr + distance + population_dep + population_arr + gdp_mean_dep + gdp_mean_arr
form_lag3 <- log(Displ_norm_origin) ~ TA_lag3_dep + PA_lag3_dep + DL_lag3_dep + conflicts_lag3_dep + TA_lag3_arr + PA_lag3_arr + DL_lag3_arr + conflicts_lag3_arr + distance + population_dep + population_arr + gdp_mean_dep + gdp_mean_arr
form_lag4 <- log(Displ_norm_origin) ~ TA_lag4_dep + PA_lag4_dep + DL_lag4_dep + conflicts_lag4_dep + TA_lag4_arr + PA_lag4_arr + DL_lag4_arr + conflicts_lag4_arr + distance + population_dep + population_arr + gdp_mean_dep + gdp_mean_arr
form_lag5 <- log(Displ_norm_origin) ~ TA_lag5_dep + PA_lag5_dep + DL_lag5_dep + conflicts_lag5_dep + TA_lag5_arr + PA_lag5_arr + DL_lag5_arr + conflicts_lag5_arr + distance + population_dep + population_arr + gdp_mean_dep + gdp_mean_arr
form_lag6 <- log(Displ_norm_origin) ~ TA_lag6_dep + PA_lag6_dep + DL_lag6_dep + conflicts_lag6_dep + TA_lag6_arr + PA_lag6_arr + DL_lag6_arr + conflicts_lag6_arr + distance + population_dep + population_arr + gdp_mean_dep + gdp_mean_arr

#form_log <- Displacements_log ~ TA_lag2_dep_log + PA_lag2_dep_log + DL_lag2_dep_log + conflicts_lag2_dep_log + TA_lag2_arr_log + PA_lag2_arr_log + DL_lag2_arr_log + conflicts_lag2_arr_log + dist_centroids_log + pop_count_dep_log + pop_count_arr_log + gdp_mean_dep_log + gdp_mean_arr_log


# #drought

lm <- lm(form, data = data_r_d)
lm_lag1 <- lm(form_lag1, data = data_r_d)
lm_lag2 <- lm(form_lag2, data = data_r_d)
lm_lag3 <- lm(form_lag3, data = data_r_d)
lm_lag4 <- lm(form_lag4, data = data_r_d)
lm_lag5 <- lm(form_lag5, data = data_r_d)
lm_lag6 <- lm(form_lag6, data = data_r_d)

# plm_i <- plm(form, data = data_d, index = c("Current..Arrival..District", "time"), model = "within", effect = "individual")
# plm_t <- plm(form, data = data_d, index = c("Current..Arrival..District", "time"), model = "within", effect = "time")
# plm_two <- plm(form, data = data_d, index = c("Current..Arrival..District", "time"), model = "within", effect = "twoways")

#stargazer(lm, plm_two, type = "text", out = "lm_districts.tex")
#conflict

lm_c <- lm(form, data = data_r_c)
lm_lag1_c <- lm(form_lag1, data = data_r_c)
lm_lag2_c <- lm(form_lag2, data = data_r_c)
lm_lag3_c <- lm(form_lag3, data = data_r_c)
lm_lag4_c <- lm(form_lag4, data = data_r_c)
lm_lag5_c <- lm(form_lag5, data = data_r_c)
lm_lag6_c <- lm(form_lag6, data = data_r_c)


# plm_i_c <- plm(form, data = data_c, index = c("Current..Arrival..District", "time"), model = "within", effect = "individual")
# plm_t_c <- plm(form, data = data_c, index = c("Current..Arrival..District", "time"), model = "within", effect = "time")
# plm_two_c <- plm(form, data = data_c, index = c("Current..Arrival..District", "time"), model = "within", effect = "twoways")


stargazer(lm_c, plm_two_c, type = "text", out = "lm_districts.tex")


#compute vif
#vif_values <- vif(lm_lag1)
#barplot(vif_values, main="VIF Values", horiz=TRUE, col="skyblue", las=1, cex.names=1)




#store regressoin coefficients in a dataframe
lm_all <- data.frame(lm$coefficients, lm_lag1$coefficients, lm_lag2$coefficients, lm_lag3$coefficients, lm_lag4$coefficients, lm_lag5$coefficients, lm_lag6$coefficients)
write.csv(lm_all, file = "latex/lmr_g.csv")
# print lm r squared
lm_r_all <- data.frame(summary(lm)$adj.r.squared, summary(lm_lag1)$adj.r.squared, summary(lm_lag2)$adj.r.squared, summary(lm_lag3)$adj.r.squared, summary(lm_lag4)$adj.r.squared, summary(lm_lag5)$adj.r.squared, summary(lm_lag6)$adj.r.squared)
write.csv(lm_r_all, file = "latex/lmr_r_g.csv")
# print lm p values
lm_p_all <- data.frame(summary(lm)$coefficients[,4], summary(lm_lag1)$coefficients[,4], summary(lm_lag2)$coefficients[,4], summary(lm_lag3)$coefficients[,4], summary(lm_lag4)$coefficients[,4], summary(lm_lag5)$coefficients[,4], summary(lm_lag6)$coefficients[,4])
write.csv(lm_p_all, file = "latex/lmr_p_g.csv")
# print Observations with nobs()
lm_obs_all <- data.frame(nobs(lm))
write.csv(lm_obs_all, file = "latex/lmr_obs_g.csv")


lm_all_c <- data.frame(lm_c$coefficients, lm_lag1_c$coefficients, lm_lag2_c$coefficients, lm_lag3_c$coefficients, lm_lag4_c$coefficients, lm_lag5_c$coefficients, lm_lag6_c$coefficients)
write.csv(lm_all_c, file = "latex/lmr_c_g.csv")
# print lm r squared
lm_c_r <- data.frame(summary(lm_c)$adj.r.squared, summary(lm_lag1_c)$adj.r.squared, summary(lm_lag2_c)$adj.r.squared, summary(lm_lag3_c)$adj.r.squared, summary(lm_lag4_c)$adj.r.squared, summary(lm_lag5_c)$adj.r.squared, summary(lm_lag6_c)$adj.r.squared)
write.csv(lm_c_r, file = "latex/lmr_r_c_g.csv")
# print lm p values
lm_c_p <- data.frame(summary(lm_c)$coefficients[,4], summary(lm_lag1_c)$coefficients[,4], summary(lm_lag2_c)$coefficients[,4], summary(lm_lag3_c)$coefficients[,4], summary(lm_lag4_c)$coefficients[,4], summary(lm_lag5_c)$coefficients[,4], summary(lm_lag6_c)$coefficients[,4])
write.csv(lm_c_p, file = "latex/lmr_p_c_g.csv")
# print Observations with nobs()
lm_c_obs <- data.frame(nobs(lm_c))
write.csv(lm_c_obs, file = "latex/lmr_obs_c_g.csv")



