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
data_d <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_norm_all_y.csv")
data_c <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_norm_all_y_confl.csv")

y_2016 <- data_d[data_d$year == 2016, ]
y_2017 <- data_d[data_d$year == 2017, ]
y_2018 <- data_d[data_d$year == 2018, ]
y_2019 <- data_d[data_d$year == 2019, ]
y_2020 <- data_d[data_d$year == 2020, ]
y_2021 <- data_d[data_d$year == 2021, ]
y_2022 <- data_d[data_d$year == 2022, ]

c_2016 <- data_c[data_c$year == 2016, ]
c_2017 <- data_c[data_c$year == 2017, ]
c_2018 <- data_c[data_c$year == 2018, ]
c_2019 <- data_c[data_c$year == 2019, ]
c_2020 <- data_c[data_c$year == 2020, ]
c_2021 <- data_c[data_c$year == 2021, ]
c_2022 <- data_c[data_c$year == 2022, ]

# individual and interaction terms
form_d_1 <- Disp_log ~ x_t + conflicts  +gdp_mean_origin + gdp_mean_destination  + population_density_origin + population_density_destination 
form_d_2 <- Disp_log ~ TA_lag2 + PA_lag2 + DL_lag2 + inv_distance_2 + conflicts + gdp_mean_origin + gdp_mean_destination + population_density_origin + population_density_destination 
form_d_3 <- Disp_log ~ x_t + TA_lag2 + PA_lag2 + DL_lag2 + conflicts +  inv_distance_2  +gdp_mean_origin + gdp_mean_destination  + population_density_origin + population_density_destination 

#lags
form <- Disp_log ~ TA + PA + DL + conflicts + inv_distance_2 + gdp_mean_origin + gdp_mean_destination + population_density_origin + population_density_destination 
form_lag1 <- Disp_log ~ TA_lag1 + PA_lag1 + DL_lag1 + conflicts_lag1 + inv_distance_2 + gdp_mean_origin + gdp_mean_destination + population_density_origin + population_density_destination 
form_lag2 <- Disp_log ~ TA_lag2 + PA_lag2 + DL_lag2 + conflicts_lag2 + inv_distance_2 + gdp_mean_origin + gdp_mean_destination + population_density_origin + population_density_destination
form_lag3 <- Disp_log ~ TA_lag3 + PA_lag3 + DL_lag3 + conflicts_lag3 + inv_distance_2 + gdp_mean_origin + gdp_mean_destination + population_density_origin + population_density_destination
form_lag4 <- Disp_log ~ TA_lag4 + PA_lag4 + DL_lag4 + conflicts_lag4 + inv_distance_2 + gdp_mean_origin + gdp_mean_destination + population_density_origin + population_density_destination
form_lag5 <- Disp_log ~ TA_lag5 + PA_lag5 + DL_lag5 + conflicts_lag5 + inv_distance_2 + gdp_mean_origin + gdp_mean_destination + population_density_origin + population_density_destination
form_lag6 <- Disp_log ~ TA_lag6 + PA_lag6 + DL_lag6 + conflicts_lag6 + inv_distance_2 + gdp_mean_origin + gdp_mean_destination + population_density_origin + population_density_destination

#all years
lm <- lm(form, data = data_d)
lm_lag1 <- lm(form_lag1, data = data_d)
lm_lag2 <- lm(form_lag2, data = data_d)
lm_lag3 <- lm(form_lag3, data = data_d)
lm_lag4 <- lm(form_lag4, data = data_d)
lm_lag5 <- lm(form_lag5, data = data_d)
lm_lag6 <- lm(form_lag6, data = data_d)
stargazer(lm, lm_lag1, lm_lag2, lm_lag3, lm_lag4, lm_lag5, lm_lag6, type = "text", out = "latex/lm_latex.tex")

#2016
lm_16 <- lm(form, data = y_2016)
lm1_16 <- lm(form_lag1, data = y_2016)
lm2_16 <- lm(form_lag2, data = y_2016)
lm3_16 <- lm(form_lag3, data = y_2016)
lm4_16 <- lm(form_lag4, data = y_2016)
lm5_16 <- lm(form_lag5, data = y_2016)
lm6_16 <- lm(form_lag6, data = y_2016)
#stargazer(lm_16, lm1_16, lm2_16, lm3_16, lm4_16, lm5_16, lm6_16, type = "text", out = "lm_16_latex.tex")

#2017
lm_17 <- lm(form, data = y_2017)
lm1_17 <- lm(form_lag1, data = y_2017)
lm2_17 <- lm(form_lag2, data = y_2017)
lm3_17 <- lm(form_lag3, data = y_2017)
lm4_17 <- lm(form_lag4, data = y_2017)
lm5_17 <- lm(form_lag5, data = y_2017)
lm6_17 <- lm(form_lag6, data = y_2017)
#stargazer(lm_17, lm1_17, lm2_17, lm3_17, lm4_17, lm5_17, lm6_17, type = "text", out = "lm_17_latex.tex")

#2018
lm_18 <- lm(form, data = y_2018)
lm1_18 <- lm(form_lag1, data = y_2018)
lm2_18 <- lm(form_lag2, data = y_2018)
lm3_18 <- lm(form_lag3, data = y_2018)
lm4_18 <- lm(form_lag4, data = y_2018)
lm5_18 <- lm(form_lag5, data = y_2018)
lm6_18 <- lm(form_lag6, data = y_2018)
#stargazer(lm_18, lm1_18, lm2_18, lm3_18, lm4_18, lm5_18, lm6_18, type = "text", out = "lm_18_latex.tex")

#2019
lm_19 <- lm(form, data = y_2019)
lm1_19 <- lm(form_lag1, data = y_2019)
lm2_19 <- lm(form_lag2, data = y_2019)
lm3_19 <- lm(form_lag3, data = y_2019)
lm4_19 <- lm(form_lag4, data = y_2019)
lm5_19 <- lm(form_lag5, data = y_2019)
lm6_19 <- lm(form_lag6, data = y_2019)
#stargazer(lm_19, lm1_19, lm2_19, lm3_19, lm4_19, lm5_19, lm6_19, type = "text", out = "lm_19_latex.tex")

#2020
lm_20 <- lm(form, data = y_2020)
lm1_20 <- lm(form_lag1, data = y_2020)
lm2_20 <- lm(form_lag2, data = y_2020)
lm3_20 <- lm(form_lag3, data = y_2020)
lm4_20 <- lm(form_lag4, data = y_2020)
lm5_20 <- lm(form_lag5, data = y_2020)
lm6_20 <- lm(form_lag6, data = y_2020)
#stargazer(lm_20, lm1_20, lm2_20, lm3_20, lm4_20, lm5_20, lm6_20, type = "text", out = "lm_20_latex.tex")

#2021
lm_21 <- lm(form, data = y_2021)
lm1_21 <- lm(form_lag1, data = y_2021)
lm2_21 <- lm(form_lag2, data = y_2021)
lm3_21 <- lm(form_lag3, data = y_2021)
lm4_21 <- lm(form_lag4, data = y_2021)
lm5_21 <- lm(form_lag5, data = y_2021)
lm6_21 <- lm(form_lag6, data = y_2021)
stargazer(lm_21, lm1_21, lm2_21, lm3_21, lm4_21, lm5_21, lm6_21, type = "text", out = "latex/lm_21_latex.tex")

#2022
lm_22 <- lm(form, data = y_2022)
lm1_22 <- lm(form_lag1, data = y_2022)
lm2_22 <- lm(form_lag2, data = y_2022)
lm3_22 <- lm(form_lag3, data = y_2022)
lm4_22 <- lm(form_lag4, data = y_2022)
lm5_22 <- lm(form_lag5, data = y_2022)
lm6_22 <- lm(form_lag6, data = y_2022)
stargazer(lm_22, lm1_22, lm2_22, lm3_22, lm4_22, lm5_22, lm6_22, type = "text", out = "latex/lm_22_latex.txt")

#store regressoin coefficients in a dataframe
lm_all <- data.frame(lm$coefficients, lm_lag1$coefficients, lm_lag2$coefficients, lm_lag3$coefficients, lm_lag4$coefficients, lm_lag5$coefficients, lm_lag6$coefficients)
write.csv(lm_all, file = "latex/lm_all.csv")
# print lm r squared
lm_r_all <- data.frame(summary(lm)$adj.r.squared, summary(lm_lag1)$adj.r.squared, summary(lm_lag2)$adj.r.squared, summary(lm_lag3)$adj.r.squared, summary(lm_lag4)$adj.r.squared, summary(lm_lag5)$adj.r.squared, summary(lm_lag6)$adj.r.squared)
write.csv(lm_r_all, file = "latex/lm_r_all.csv")
# print lm p values
lm_p_all <- data.frame(summary(lm)$coefficients[,4], summary(lm_lag1)$coefficients[,4], summary(lm_lag2)$coefficients[,4], summary(lm_lag3)$coefficients[,4], summary(lm_lag4)$coefficients[,4], summary(lm_lag5)$coefficients[,4], summary(lm_lag6)$coefficients[,4])
write.csv(lm_p_all, file = "latex/lm_p_all.csv")
# print Observations with nobs()
lm_obs_all <- data.frame(nobs(lm), nobs(lm_16), nobs(lm_17), nobs(lm_18), nobs(lm_19), nobs(lm_20), nobs(lm_21), nobs(lm_22))
write.csv(lm_obs_all, file = "latex/lm_obs.csv")


#store regression coefficients in a dataframe for 2016
lm_16_c <- data.frame(lm_16$coefficients, lm1_16$coefficients, lm2_16$coefficients, lm3_16$coefficients, lm4_16$coefficients, lm5_16$coefficients, lm6_16$coefficients)
write.csv(lm_16_c, file = "latex/lm_16_c.csv")
# print lm r squared
lm_r_16 <- data.frame(summary(lm_16)$adj.r.squared, summary(lm1_16)$adj.r.squared, summary(lm2_16)$adj.r.squared, summary(lm3_16)$adj.r.squared, summary(lm4_16)$adj.r.squared, summary(lm5_16)$adj.r.squared, summary(lm6_16)$adj.r.squared)
write.csv(lm_r_16, file = "latex/lm_r_16.csv")
# print lm p values
lm_p_16 <- data.frame(summary(lm_16)$coefficients[,4], summary(lm1_16)$coefficients[,4], summary(lm2_16)$coefficients[,4], summary(lm3_16)$coefficients[,4], summary(lm4_16)$coefficients[,4], summary(lm5_16)$coefficients[,4], summary(lm6_16)$coefficients[,4])
write.csv(lm_p_16, file = "latex/lm_p_16.csv")

#store regression coefficients in a dataframe for 2017
lm_17_c <- data.frame(lm_17$coefficients, lm1_17$coefficients, lm2_17$coefficients, lm3_17$coefficients, lm4_17$coefficients, lm5_17$coefficients, lm6_17$coefficients)
write.csv(lm_17_c, file = "latex/lm_17_c.csv")
# print lm r squared
lm_r_17 <- data.frame(summary(lm_17)$adj.r.squared, summary(lm1_17)$adj.r.squared, summary(lm2_17)$adj.r.squared, summary(lm3_17)$adj.r.squared, summary(lm4_17)$adj.r.squared, summary(lm5_17)$adj.r.squared, summary(lm6_17)$adj.r.squared)
write.csv(lm_r_17, file = "latex/lm_r_17.csv")
# print lm p values
lm_p_17 <- data.frame(summary(lm_17)$coefficients[,4], summary(lm1_17)$coefficients[,4], summary(lm2_17)$coefficients[,4], summary(lm3_17)$coefficients[,4], summary(lm4_17)$coefficients[,4], summary(lm5_17)$coefficients[,4], summary(lm6_17)$coefficients[,4])
write.csv(lm_p_17, file = "latex/lm_p_17.csv")

#store regression coefficients in a dataframe for 2018
lm_18_c <- data.frame(lm_18$coefficients, lm1_18$coefficients, lm2_18$coefficients, lm3_18$coefficients, lm4_18$coefficients, lm5_18$coefficients, lm6_18$coefficients)
write.csv(lm_18_c, file = "latex/lm_18_c.csv")
# print lm r squared
lm_r_18 <- data.frame(summary(lm_18)$adj.r.squared, summary(lm1_18)$adj.r.squared, summary(lm2_18)$adj.r.squared, summary(lm3_18)$adj.r.squared, summary(lm4_18)$adj.r.squared, summary(lm5_18)$adj.r.squared, summary(lm6_18)$adj.r.squared)
write.csv(lm_r_18, file = "latex/lm_r_18.csv")
# print lm p values
lm_p_18 <- data.frame(summary(lm_18)$coefficients[,4], summary(lm1_18)$coefficients[,4], summary(lm2_18)$coefficients[,4], summary(lm3_18)$coefficients[,4], summary(lm4_18)$coefficients[,4], summary(lm5_18)$coefficients[,4], summary(lm6_18)$coefficients[,4])
write.csv(lm_p_18, file = "latex/lm_p_18.csv")

#store regression coefficients in a dataframe for 2019
lm_19_c <- data.frame(lm_19$coefficients, lm1_19$coefficients, lm2_19$coefficients, lm3_19$coefficients, lm4_19$coefficients, lm5_19$coefficients, lm6_19$coefficients)
write.csv(lm_19_c, file = "latex/lm_19_c.csv")
# print lm r squared
lm_r_19 <- data.frame(summary(lm_19)$adj.r.squared, summary(lm1_19)$adj.r.squared, summary(lm2_19)$adj.r.squared, summary(lm3_19)$adj.r.squared, summary(lm4_19)$adj.r.squared, summary(lm5_19)$adj.r.squared, summary(lm6_19)$adj.r.squared)
write.csv(lm_r_19, file = "latex/lm_r_19.csv")
# print lm p values
lm_p_19 <- data.frame(summary(lm_19)$coefficients[,4], summary(lm1_19)$coefficients[,4], summary(lm2_19)$coefficients[,4], summary(lm3_19)$coefficients[,4], summary(lm4_19)$coefficients[,4], summary(lm5_19)$coefficients[,4], summary(lm6_19)$coefficients[,4])
write.csv(lm_p_19, file = "latex/lm_p_19.csv")

#store regression coefficients in a dataframe for 2020
lm_20_c <- data.frame(lm_20$coefficients, lm1_20$coefficients, lm2_20$coefficients, lm3_20$coefficients, lm4_20$coefficients, lm5_20$coefficients, lm6_20$coefficients)
write.csv(lm_20_c, file = "latex/lm_20_c.csv")
# print lm r squared
lm_r_20 <- data.frame(summary(lm_20)$adj.r.squared, summary(lm1_20)$adj.r.squared, summary(lm2_20)$adj.r.squared, summary(lm3_20)$adj.r.squared, summary(lm4_20)$adj.r.squared, summary(lm5_20)$adj.r.squared, summary(lm6_20)$adj.r.squared)
write.csv(lm_r_20, file = "latex/lm_r_20.csv")
# print lm p values
lm_p_20 <- data.frame(summary(lm_20)$coefficients[,4], summary(lm1_20)$coefficients[,4], summary(lm2_20)$coefficients[,4], summary(lm3_20)$coefficients[,4], summary(lm4_20)$coefficients[,4], summary(lm5_20)$coefficients[,4], summary(lm6_20)$coefficients[,4])
write.csv(lm_p_20, file = "latex/lm_p_20.csv")

#store regression coefficients in a dataframe for 2021
lm_21_c <- data.frame(lm_21$coefficients, lm1_21$coefficients, lm2_21$coefficients, lm3_21$coefficients, lm4_21$coefficients, lm5_21$coefficients, lm6_21$coefficients)
write.csv(lm_21_c, file = "latex/lm_21_c.csv")
# print lm r squared
lm_r_21 <- data.frame(summary(lm_21)$adj.r.squared, summary(lm1_21)$adj.r.squared, summary(lm2_21)$adj.r.squared, summary(lm3_21)$adj.r.squared, summary(lm4_21)$adj.r.squared, summary(lm5_21)$adj.r.squared, summary(lm6_21)$adj.r.squared)
write.csv(lm_r_21, file = "latex/lm_r_21.csv")
# print lm p values
lm_p_21 <- data.frame(summary(lm_21)$coefficients[,4], summary(lm1_21)$coefficients[,4], summary(lm2_21)$coefficients[,4], summary(lm3_21)$coefficients[,4], summary(lm4_21)$coefficients[,4], summary(lm5_21)$coefficients[,4], summary(lm6_21)$coefficients[,4])
write.csv(lm_p_21, file = "latex/lm_p_21.csv")

#store regression coefficients in a dataframe for 2022
lm_22_c <- data.frame(lm_22$coefficients, lm1_22$coefficients, lm2_22$coefficients, lm3_22$coefficients, lm4_22$coefficients, lm5_22$coefficients, lm6_22$coefficients)
write.csv(lm_22_c, file = "latex/lm_22_c.csv")
# print lm r squared
lm_r_22 <- data.frame(summary(lm_22)$adj.r.squared, summary(lm1_22)$adj.r.squared, summary(lm2_22)$adj.r.squared, summary(lm3_22)$adj.r.squared, summary(lm4_22)$adj.r.squared, summary(lm5_22)$adj.r.squared, summary(lm6_22)$adj.r.squared)
write.csv(lm_r_22, file = "latex/lm_r_22.csv")
# print lm p values
lm_p_22 <- data.frame(summary(lm_22)$coefficients[,4], summary(lm1_22)$coefficients[,4], summary(lm2_22)$coefficients[,4], summary(lm3_22)$coefficients[,4], summary(lm4_22)$coefficients[,4], summary(lm5_22)$coefficients[,4], summary(lm6_22)$coefficients[,4])
write.csv(lm_p_22, file = "latex/lm_p_22.csv")


#conflict
#all years
lm <- lm(form, data = data_c)
lm_lag1 <- lm(form_lag1, data = data_c)
lm_lag2 <- lm(form_lag2, data = data_c)
lm_lag3 <- lm(form_lag3, data = data_c)
lm_lag4 <- lm(form_lag4, data = data_c)
lm_lag5 <- lm(form_lag5, data = data_c)
lm_lag6 <- lm(form_lag6, data = data_c)
stargazer(lm, lm_lag1, lm_lag2, lm_lag3, lm_lag4, lm_lag5, lm_lag6, type = "text", out = "latex/lm_latex_confl.tex")

#2016
lm_16 <- lm(form, data = c_2016)
lm1_16 <- lm(form_lag1, data = c_2016)
lm2_16 <- lm(form_lag2, data = c_2016)
lm3_16 <- lm(form_lag3, data = c_2016)
lm4_16 <- lm(form_lag4, data = c_2016)
lm5_16 <- lm(form_lag5, data = c_2016)
lm6_16 <- lm(form_lag6, data = c_2016)

#2017
lm_17 <- lm(form, data = c_2017)
lm1_17 <- lm(form_lag1, data = c_2017)
lm2_17 <- lm(form_lag2, data = c_2017)
lm3_17 <- lm(form_lag3, data = c_2017)
lm4_17 <- lm(form_lag4, data = c_2017)
lm5_17 <- lm(form_lag5, data = c_2017)
lm6_17 <- lm(form_lag6, data = c_2017)

#2018
lm_18 <- lm(form, data = c_2018)
lm1_18 <- lm(form_lag1, data = c_2018)
lm2_18 <- lm(form_lag2, data = c_2018)
lm3_18 <- lm(form_lag3, data = c_2018)
lm4_18 <- lm(form_lag4, data = c_2018)
lm5_18 <- lm(form_lag5, data = c_2018)
lm6_18 <- lm(form_lag6, data = c_2018)

#2019
lm_19 <- lm(form, data = c_2019)
lm1_19 <- lm(form_lag1, data = c_2019)
lm2_19 <- lm(form_lag2, data = c_2019)
lm3_19 <- lm(form_lag3, data = c_2019)
lm4_19 <- lm(form_lag4, data = c_2019)
lm5_19 <- lm(form_lag5, data = c_2019)
lm6_19 <- lm(form_lag6, data = c_2019)

#2020
lm_20 <- lm(form, data = c_2020)
lm1_20 <- lm(form_lag1, data = c_2020)
lm2_20 <- lm(form_lag2, data = c_2020)
lm3_20 <- lm(form_lag3, data = c_2020)
lm4_20 <- lm(form_lag4, data = c_2020)
lm5_20 <- lm(form_lag5, data = c_2020)
lm6_20 <- lm(form_lag6, data = c_2020)

#2021
lm_21 <- lm(form, data = c_2021)
lm1_21 <- lm(form_lag1, data = c_2021)
lm2_21 <- lm(form_lag2, data = c_2021)
lm3_21 <- lm(form_lag3, data = c_2021)
lm4_21 <- lm(form_lag4, data = c_2021)
lm5_21 <- lm(form_lag5, data = c_2021)
lm6_21 <- lm(form_lag6, data = c_2021)

#2022
lm_22 <- lm(form, data = c_2022)
lm1_22 <- lm(form_lag1, data = c_2022)
lm2_22 <- lm(form_lag2, data = c_2022)
lm3_22 <- lm(form_lag3, data = c_2022)
lm4_22 <- lm(form_lag4, data = c_2022)
lm5_22 <- lm(form_lag5, data = c_2022)
lm6_22 <- lm(form_lag6, data = c_2022)
stargazer(lm_22, lm1_22, lm2_22, lm3_22, lm4_22, lm5_22, lm6_22, type = "text", out = "lm_22_latex_confl.txt")

#store regression coefficients in a dataframe
lm_all <- data.frame(lm$coefficients, lm_lag1$coefficients, lm_lag2$coefficients, lm_lag3$coefficients, lm_lag4$coefficients, lm_lag5$coefficients, lm_lag6$coefficients)
write.csv(lm_all, file = "latex/lm_all_confl.csv")
# print lm r squared
lm_r_all <- data.frame(summary(lm)$adj.r.squared, summary(lm_lag1)$adj.r.squared, summary(lm_lag2)$adj.r.squared, summary(lm_lag3)$adj.r.squared, summary(lm_lag4)$adj.r.squared, summary(lm_lag5)$adj.r.squared, summary(lm_lag6)$adj.r.squared)
write.csv(lm_r_all, file = "latex/lm_r_all_confl.csv")
# print lm p values
lm_p_all <- data.frame(summary(lm)$coefficients[,4], summary(lm_lag1)$coefficients[,4], summary(lm_lag2)$coefficients[,4], summary(lm_lag3)$coefficients[,4], summary(lm_lag4)$coefficients[,4], summary(lm_lag5)$coefficients[,4], summary(lm_lag6)$coefficients[,4])
write.csv(lm_p_all, file = "latex/lm_p_all_confl.csv")
# print Observations with nobs()
lm_obs_all <- data.frame(nobs(lm), nobs(lm_16), nobs(lm_17), nobs(lm_18), nobs(lm_19), nobs(lm_20), nobs(lm_21), nobs(lm_22))
write.csv(lm_obs_all, file = "latex/lm_obs_confl.csv")


#store regression coefficients in a dataframe for 2016
lm_16_c <- data.frame(lm_16$coefficients, lm1_16$coefficients, lm2_16$coefficients, lm3_16$coefficients, lm4_16$coefficients, lm5_16$coefficients, lm6_16$coefficients)
write.csv(lm_16_c, file = "latex/lm_16_c_confl.csv")
# print lm r squared
lm_r_16 <- data.frame(summary(lm_16)$adj.r.squared, summary(lm1_16)$adj.r.squared, summary(lm2_16)$adj.r.squared, summary(lm3_16)$adj.r.squared, summary(lm4_16)$adj.r.squared, summary(lm5_16)$adj.r.squared, summary(lm6_16)$adj.r.squared)
write.csv(lm_r_16, file = "latex/lm_r_16_confl.csv")
# print lm p values
lm_p_16 <- data.frame(summary(lm_16)$coefficients[,4], summary(lm1_16)$coefficients[,4], summary(lm2_16)$coefficients[,4], summary(lm3_16)$coefficients[,4], summary(lm4_16)$coefficients[,4], summary(lm5_16)$coefficients[,4], summary(lm6_16)$coefficients[,4])
write.csv(lm_p_16, file = "latex/lm_p_16_confl.csv")

#store regression coefficients in a dataframe for 2017
lm_17_c <- data.frame(lm_17$coefficients, lm1_17$coefficients, lm2_17$coefficients, lm3_17$coefficients, lm4_17$coefficients, lm5_17$coefficients, lm6_17$coefficients)
write.csv(lm_17_c, file = "latex/lm_17_c_confl.csv")
# print lm r squared
lm_r_17 <- data.frame(summary(lm_17)$adj.r.squared, summary(lm1_17)$adj.r.squared, summary(lm2_17)$adj.r.squared, summary(lm3_17)$adj.r.squared, summary(lm4_17)$adj.r.squared, summary(lm5_17)$adj.r.squared, summary(lm6_17)$adj.r.squared)
write.csv(lm_r_17, file = "latex/lm_r_17_confl.csv")
# print lm p values
lm_p_17 <- data.frame(summary(lm_17)$coefficients[,4], summary(lm1_17)$coefficients[,4], summary(lm2_17)$coefficients[,4], summary(lm3_17)$coefficients[,4], summary(lm4_17)$coefficients[,4], summary(lm5_17)$coefficients[,4], summary(lm6_17)$coefficients[,4])
write.csv(lm_p_17, file = "latex/lm_p_17_confl.csv")

#store regression coefficients in a dataframe for 2018
lm_18_c <- data.frame(lm_18$coefficients, lm1_18$coefficients, lm2_18$coefficients, lm3_18$coefficients, lm4_18$coefficients, lm5_18$coefficients, lm6_18$coefficients)
write.csv(lm_18_c, file = "latex/lm_18_c_confl.csv")
# print lm r squared
lm_r_18 <- data.frame(summary(lm_18)$adj.r.squared, summary(lm1_18)$adj.r.squared, summary(lm2_18)$adj.r.squared, summary(lm3_18)$adj.r.squared, summary(lm4_18)$adj.r.squared, summary(lm5_18)$adj.r.squared, summary(lm6_18)$adj.r.squared)
write.csv(lm_r_18, file = "latex/lm_r_18_confl.csv")
# print lm p values
lm_p_18 <- data.frame(summary(lm_18)$coefficients[,4], summary(lm1_18)$coefficients[,4], summary(lm2_18)$coefficients[,4], summary(lm3_18)$coefficients[,4], summary(lm4_18)$coefficients[,4], summary(lm5_18)$coefficients[,4], summary(lm6_18)$coefficients[,4])
write.csv(lm_p_18, file = "latex/lm_p_18_confl.csv")

#store regression coefficients in a dataframe for 2019
lm_19_c <- data.frame(lm_19$coefficients, lm1_19$coefficients, lm2_19$coefficients, lm3_19$coefficients, lm4_19$coefficients, lm5_19$coefficients, lm6_19$coefficients)
write.csv(lm_19_c, file = "latex/lm_19_c_confl.csv")
# print lm r squared
lm_r_19 <- data.frame(summary(lm_19)$adj.r.squared, summary(lm1_19)$adj.r.squared, summary(lm2_19)$adj.r.squared, summary(lm3_19)$adj.r.squared, summary(lm4_19)$adj.r.squared, summary(lm5_19)$adj.r.squared, summary(lm6_19)$adj.r.squared)
write.csv(lm_r_19, file = "latex/lm_r_19_confl.csv")
# print lm p values
lm_p_19 <- data.frame(summary(lm_19)$coefficients[,4], summary(lm1_19)$coefficients[,4], summary(lm2_19)$coefficients[,4], summary(lm3_19)$coefficients[,4], summary(lm4_19)$coefficients[,4], summary(lm5_19)$coefficients[,4], summary(lm6_19)$coefficients[,4])
write.csv(lm_p_19, file = "latex/lm_p_19_confl.csv")

#store regression coefficients in a dataframe for 2020
lm_20_c <- data.frame(lm_20$coefficients, lm1_20$coefficients, lm2_20$coefficients, lm3_20$coefficients, lm4_20$coefficients, lm5_20$coefficients, lm6_20$coefficients)
write.csv(lm_20_c, file = "latex/lm_20_c_confl.csv")
# print lm r squared
lm_r_20 <- data.frame(summary(lm_20)$adj.r.squared, summary(lm1_20)$adj.r.squared, summary(lm2_20)$adj.r.squared, summary(lm3_20)$adj.r.squared, summary(lm4_20)$adj.r.squared, summary(lm5_20)$adj.r.squared, summary(lm6_20)$adj.r.squared)
write.csv(lm_r_20, file = "latex/lm_r_20_confl.csv")
# print lm p values
lm_p_20 <- data.frame(summary(lm_20)$coefficients[,4], summary(lm1_20)$coefficients[,4], summary(lm2_20)$coefficients[,4], summary(lm3_20)$coefficients[,4], summary(lm4_20)$coefficients[,4], summary(lm5_20)$coefficients[,4], summary(lm6_20)$coefficients[,4])
write.csv(lm_p_20, file = "latex/lm_p_20_confl.csv")

#store regression coefficients in a dataframe for 2021
lm_21_c <- data.frame(lm_21$coefficients, lm1_21$coefficients, lm2_21$coefficients, lm3_21$coefficients, lm4_21$coefficients, lm5_21$coefficients, lm6_21$coefficients)
write.csv(lm_21_c, file = "latex/lm_21_c_confl.csv")
# print lm r squared
lm_r_21 <- data.frame(summary(lm_21)$adj.r.squared, summary(lm1_21)$adj.r.squared, summary(lm2_21)$adj.r.squared, summary(lm3_21)$adj.r.squared, summary(lm4_21)$adj.r.squared, summary(lm5_21)$adj.r.squared, summary(lm6_21)$adj.r.squared)
write.csv(lm_r_21, file = "latex/lm_r_21_confl.csv")
# print lm p values
lm_p_21 <- data.frame(summary(lm_21)$coefficients[,4], summary(lm1_21)$coefficients[,4], summary(lm2_21)$coefficients[,4], summary(lm3_21)$coefficients[,4], summary(lm4_21)$coefficients[,4], summary(lm5_21)$coefficients[,4], summary(lm6_21)$coefficients[,4])
write.csv(lm_p_21, file = "latex/lm_p_21_confl.csv")

#store regression coefficients in a dataframe for 2022
lm_22_c <- data.frame(lm_22$coefficients, lm1_22$coefficients, lm2_22$coefficients, lm3_22$coefficients, lm4_22$coefficients, lm5_22$coefficients, lm6_22$coefficients)
write.csv(lm_22_c, file = "latex/lm_22_c_confl.csv")
# print lm r squared
lm_r_22 <- data.frame(summary(lm_22)$adj.r.squared, summary(lm1_22)$adj.r.squared, summary(lm2_22)$adj.r.squared, summary(lm3_22)$adj.r.squared, summary(lm4_22)$adj.r.squared, summary(lm5_22)$adj.r.squared, summary(lm6_22)$adj.r.squared)
write.csv(lm_r_22, file = "latex/lm_r_22_confl.csv")
# print lm p values
lm_p_22 <- data.frame(summary(lm_22)$coefficients[,4], summary(lm1_22)$coefficients[,4], summary(lm2_22)$coefficients[,4], summary(lm3_22)$coefficients[,4], summary(lm4_22)$coefficients[,4], summary(lm5_22)$coefficients[,4], summary(lm6_22)$coefficients[,4])
write.csv(lm_p_22, file = "latex/lm_p_22_confl.csv")
