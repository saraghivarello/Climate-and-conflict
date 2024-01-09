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

data_ban_2 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/dd_ban_2.csv")
data_ban_3 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/dd_ban_3.csv")
data_ban_6 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/dd_ban_6.csv")
data_ban_d_2 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/dd_ban_d_2.csv")
data_gedo <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/dd_gedo.csv")
data_ban_20 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/dd_ban_20.csv")
data_flood_ban_20 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/dd_flood_ban_20.csv")
data_ban_all <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/dd_ban_all.csv")
data_ban_log <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/dd_ban_log.csv")

data_conc <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/df_conc.csv")
data_conc_3 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/df_conc_lag3.csv")
data_conc_6 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/df_conc_lag6.csv")
data_conc_dist <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/df_conc_2_dist.csv")
data_conc_gdp <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/df_conc_gdp.csv")
data_conc_log <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/df_conc_log.csv")
data_conc1 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/df_conc1.csv")

#data1 <- data1[data1$time <= 2022, ]
#data1 <- data1[data1$time >= 2021, ]

form_all <- Disp_log ~ x + TA_lag6 + inv_distance + conflicts #+ population_density_origin + population_density_destination#+ PA_lag3 + DL_lag3 + inv_distance
form_all_3 <- Disp_log ~ x + TA_lag3 + PA_lag3 + DL_lag3 + inv_distance + conflicts #+ population_density_destination#+ PA_lag3 + DL_lag3 + inv_distance
form_all_6 <- Disp_log ~ x + TA_lag6 + PA_lag6 + DL_lag6 + inv_distance + conflicts #+ population_density_destination#+ PA_lag3 + DL_lag3 + inv_distance
form_all_log_lmh <- Disp_log ~ x + TA_lag6 + PA_lag6 + DL_lag6 + inv_distance + conflicts + gdp_mean_origin  + gdp_mean_destination + population_density_origin + population_density_destination #+ accessibility_to_cities_mean_origin + accessibility_to_cities_mean_destination#+ PA_lag3 + DL_lag3 + inv_distance
form_all_log_lmh1 <- Disp_log ~ x + TA_lag6 + PA_lag6 + DL_lag6 + inv_distance_2 + conflicts + gdp_mean_origin  + gdp_mean_destination + population_density_origin + population_density_destination #+ accessibility_to_cities_mean_origin + accessibility_to_cities_mean_destination#+ PA_lag3 + DL_lag3 + inv_distance
form_all_log_lmh2 <- Disp_log ~ x + TA_lag6 + PA_lag6 + DL_lag6 + conflicts + gdp_mean_origin  + gdp_mean_destination + population_density_origin + population_density_destination #+ accessibility_to_cities_mean_origin + accessibility_to_cities_mean_destination#+ PA_lag3 + DL_lag3 + inv_distance
form_all_log <- Disp_log ~ x_dist + TA_lag2 + PA_lag2 + DL_lag2 + inv_distance + conflicts  #+gdp_mean_origin  + gdp_mean_destination 
form_all_log_1 <- Disp_log_1 ~ x + TA_lag2 + PA_lag2 + DL_lag2 + inv_distance + conflicts  +gdp_mean_origin + gdp_mean_destination 

#no fixed effects
lmh <- lm(form_all_gdp, data = data_conc_gdp)
#lmh_log <- lm(form_all_log_lmh, data = data_conc_log)
#lmh_log1 <- lm(form_all_log_lmh1, data = data_conc_log)
#lmh_log2 <- lm(form_all_log_lmh2, data = data_conc_log)
#stargazer(lmh_log, type = "text", out = "lmh_log.txt")

lmh_log <- lm(form_all_log_lmh, data = data_conc_6)
lmh_log1 <- lm(form_all_log_lmh1, data = data_conc_6)
lmh_log2 <- lm(form_all_log_lmh2, data = data_conc_6)
#stargazer(lmh_log, lmh_log1, lmh_log2, type = "latex", out = "lmh_log_latex.tex")


#all regions log
log_ind <- plm(form_all_log, data = data_conc_log, model = "within", index = c("Current..Arrival..Region","month"), effect = "individual")
log_time <- plm(form_all_log, data = data_conc_log, model = "within", index = c("Current..Arrival..Region","month"), effect = "time")
log_two <- plm(form_all_log, data = data_conc_log, model = "within", index = c("Current..Arrival..Region","month"), effect = "twoways")
#stargazer(log_ind, log_time, log_two, type = "latex", out = "plm_log_latex.tex")

#all regions together lag 3
f_all_ind_3 <- plm(form_all_3, data = data_conc_3, model = "within", index = c("Current..Arrival..Region","month"), effect = "individual")
f_all_time_3 <- plm(form_all_3, data = data_conc_3, model = "within", index = c("Current..Arrival..Region","month"), effect = "time")
f_all_two_3 <- plm(form_all_3, data = data_conc_3, model = "within", index = c("Current..Arrival..Region","month"), effect = "twoways")
#stargazer(f_all_ind_3, f_all_time_3, f_all_two_3, type = "latex", out = "plm_latex.tex")

#all regions together lag 6
f_all_ind_6 <- plm(form_all_6, data = data_conc_6, model = "within", index = c("Current..Arrival..Region","month"), effect = "individual")
f_all_time_6 <- plm(form_all_6, data = data_conc_6, model = "within", index = c("Current..Arrival..Region","month"), effect = "time")
f_all_two_6 <- plm(form_all_6, data = data_conc_6, model = "within", index = c("Current..Arrival..Region","month"), effect = "twoways")
stargazer(f_all_ind_6, f_all_time_6, f_all_two_6, type = "latex", out = "plm_latex.tex")



form_ban_2 <- Banadir ~ x + TA_lag2 + inv_distance + conflicts #+ population_density
form_ban_3 <- Banadir ~ x + TA_lag3 + inv_distance + conflicts #+ population_density
form_ban_6 <- Banadir ~ x + TA_lag6 + inv_distance + conflicts #+ population_density
form_ban_d_2 <- Banadir ~ TA_lag2 + inv_distance_2  #+ population_density
form_gedo <- Gedo ~ x + TA_lag2 + inv_distance + conflicts#summary() + PA_lag3 + DL_lag3 #+ inv_distance
form_ban_log <- log_disp_ban ~ x + TA_lag2 + PA_lag2 + DL_lag2 + inv_distance_2 + conflicts  + gdp_mean + population_density


#banadir in 2022
f_ind_b <- plm(form_ban_2, data = data_ban_2, model = "within", index = c("admin1","month"), effect = "individual")
f_time_b <- plm(form_ban_2, data = data_ban_2, model = "within", index = c("admin1","month"), effect = "time")
f_two_b <- plm(form_ban_2, data = data_ban_2, model = "within", index = c("admin1","month"), effect = "twoways")

#log ban 
lmh_b_log <- lm(form_ban_log, data = data_ban_log)
f_ind_b_log <- plm(form_ban_log, data = data_ban_log, model = "within", index = c("admin1","month"), effect = "individual")
f_time_b_log <- plm(form_ban_log, data = data_ban_log, model = "within", index = c("admin1","month"), effect = "time")
f_two_b_log <- plm(form_ban_log, data = data_ban_log, model = "within", index = c("admin1","month"), effect = "twoways")
#stargazer(lmh_b_log, f_ind_b_log, f_time_b_log, f_two_b_log, type = "latex", out = "plm_log_latex.tex")


#banadir in 2022 d^2
f_ind_b_d_2 <- plm(form_ban_d_2, data = data_ban_d_2, model = "within", index = c("admin1","month"), effect = "individual")
f_time_b_d_2 <- plm(form_ban_d_2, data = data_ban_d_2, model = "within", index = c("admin1","month"), effect = "time")
f_two_b_d_2 <- plm(form_ban_d_2, data = data_ban_d_2, model = "within", index = c("admin1","month"), effect = "twoways")

#banadir in 2022 lag=3
f_ind_b_3 <- plm(form_ban_3, data = data_ban_3, model = "within", index = c("admin1","month"), effect = "individual")
f_time_b_3 <- plm(form_ban_3, data = data_ban_3, model = "within", index = c("admin1","month"), effect = "time")
f_two_b_3 <- plm(form_ban_3, data = data_ban_3, model = "within", index = c("admin1","month"), effect = "twoways")

#banadir in 2022 lag=6
f_ind_b_6 <- plm(form_ban_6, data = data_ban_6, model = "within", index = c("admin1","month"), effect = "individual")
f_time_b_6 <- plm(form_ban_6, data = data_ban_6, model = "within", index = c("admin1","month"), effect = "time")
f_two_b_6 <- plm(form_ban_6, data = data_ban_6, model = "within", index = c("admin1","month"), effect = "twoways")

#all years together
f_ind_b_all <- plm(form_ban_2, data = data_ban_all, model = "within", index = c("admin1","time"), effect = "individual")
f_time_b_all <- plm(form_ban_2, data = data_ban_all, model = "within", index = c("admin1","time"), effect = "time")
f_two_b_all <- plm(form_ban_2, data = data_ban_all, model = "within", index = c("admin1","time"), effect = "twoways")

#find correlation between variables
#cor(data_ban_d_2[,c("x","inv_distance")])
#cor.test(data_ban_d_2$x, data_ban_d_2$inv_distance_2)
#create a vector of 204 random numbers
set.seed(40)
random <- runif(204, min = 0, max = 1)
cor.test(random, data_ban_2$Banadir)

#predict f_time_b
pred <- predict(f_time_b, newdata = data_ban_2)

#plot data_ban_log$log_disp_ban against data_ban_log$x
#plot(data_ban_log$x, data_ban_log$log_disp_ban)