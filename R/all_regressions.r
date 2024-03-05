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

data_ban_all <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/dd_ban_all.csv")
data_ban_log <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/dd_ban_log.csv")
data_ban_y <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/dd_ban_y.csv")
data_ban_log1 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/dd_ban_y_log1.csv")
data_ban_norm <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/dd_ban_norm.csv")

data_conc <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_conc.csv")
data_conc_3 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_conc_lag3.csv")
data_conc_6 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_conc_lag6.csv")
data_conc_dist <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_conc_2_dist.csv")
data_conc_gdp <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_conc_gdp.csv")
data_conc_log <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_conc_log.csv")
data_conc1 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_conc1.csv")
data_norm <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_norm_pa.csv")
data_fd <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_fd.csv")

#data_norm <- data_norm[data_norm$time <= 2023, ]
#data_norm <- data_norm[data_norm$time >= 2022, ]

form_all <- Disp_log ~ x + TA_lag6 + inv_distance + conflicts #+ population_density_origin + population_density_destination#+ PA_lag3 + DL_lag3 + inv_distance
form_all_3 <- Disp_log ~ x + TA_lag3 + PA_lag3 + DL_lag3 + inv_distance + conflicts #+ population_density_destination#+ PA_lag3 + DL_lag3 + inv_distance
form_all_6 <- Disp_log ~ x + TA_lag6 + PA_lag6 + DL_lag6 + inv_distance + conflicts #+ population_density_destination#+ PA_lag3 + DL_lag3 + inv_distance
form_all_log_lmh <- Disp_log ~ x + conflicts  +gdp_mean_origin + gdp_mean_destination  + population_density_origin + population_density_destination #+ accessibility_to_cities_mean_origin + accessibility_to_cities_mean_destination#+ PA_lag3 + DL_lag3 + inv_distance
form_all_log_lmh1 <- Disp_log ~ TA_lag2 + PA_lag2 + DL_lag2 + inv_distance_2 + conflicts  +gdp_mean_origin + gdp_mean_destination + population_density_origin + population_density_destination #+ accessibility_to_cities_mean_origin + accessibility_to_cities_mean_destination#+ PA_lag3 + DL_lag3 + inv_distance
form_all_log_lmh2 <- Disp_log ~ x + TA_lag2 + PA_lag2 + DL_lag2 + conflicts +  inv_distance_2  +gdp_mean_origin + gdp_mean_destination  + population_density_origin + population_density_destination #+ accessibility_to_cities_mean_origin + accessibility_to_cities_mean_destination#+ PA_lag3 + DL_lag3 + inv_distance
form_all_log <- Disp_log ~ TA_lag2 + PA_lag2 + DL_lag2 + inv_distance_2  +conflicts # +gdp_mean_origin  + gdp_mean_destination 
form_all_log_1 <- Disp_log_1 ~ x + TA_lag2 + PA_lag2 + DL_lag2 + inv_distance + conflicts  +gdp_mean_origin + gdp_mean_destination 

#no fixed effects
lmh_log <- lm(form_all_log_lmh, data = data_norm)
lmh_log1 <- lm(form_all_log_lmh1, data = data_norm)
lmh_log2 <- lm(form_all_log_lmh2, data = data_norm)
stargazer(lmh_log, lmh_log1, lmh_log2, type = "text", out = "lmh_log_latex.tex")


#all regions log
log_ind <- plm(form_all_log, data = data_norm, model = "within", index = c("Current..Arrival..Region"), effect = "individual")
log_time <- plm(form_all_log, data = data_norm, model = "within", index = c("month"), effect = "time")
log_two <- plm(form_all_log, data = data_norm, model = "within", index = c("Current..Arrival..Region","month"), effect = "twoways")
stargazer(log_ind, log_time, log_two, type = "text", out = "plm_log_latex.tex")


y_true <- data_norm$Disp_log
y_pred <- predict(log_ind)
y_mean <- mean(y_true)
TSS <- sum((y_true - y_mean)^2)
RSS <- sum((y_true - y_pred)^2)
r_squared <- 1 - (RSS / TSS)

print(paste("R-squared value is: ", r_squared))
print(summary(log_ind)$r.squared)
#print(r.squared(log_ind, model = NULL, type = "cor", dfcor = FALSE))

#fitted-vs-observed plot
plot(as.numeric(y_true - residuals(log_ind)), y_true, asp = 1)
abline(0, 1, col = 'red', lty = 'dashed', lwd = 2)

#all regions together lag 3
f_all_ind_3 <- plm(form_all_3, data = data_conc_3, model = "within", index = c("Current..Arrival..Region","month"), effect = "individual")
f_all_time_3 <- plm(form_all_3, data = data_conc_3, model = "within", index = c("Current..Arrival..Region","month"), effect = "time")
f_all_two_3 <- plm(form_all_3, data = data_conc_3, model = "within", index = c("Current..Arrival..Region","month"), effect = "twoways")
#stargazer(f_all_ind_3, f_all_time_3, f_all_two_3, type = "latex", out = "plm_latex.tex")

#all regions together lag 6
f_all_ind_6 <- plm(form_all_6, data = data_conc_6, model = "within", index = c("Current..Arrival..Region","month"), effect = "individual")
f_all_time_6 <- plm(form_all_6, data = data_conc_6, model = "within", index = c("Current..Arrival..Region","month"), effect = "time")
f_all_two_6 <- plm(form_all_6, data = data_conc_6, model = "within", index = c("Current..Arrival..Region","month"), effect = "twoways")
#stargazer(f_all_ind_6, f_all_time_6, f_all_two_6, type = "latex", out = "plm_latex.tex")


form_ban_log <- log_disp_ban ~ x + TA_lag2 + PA_lag2 + DL_lag2 + inv_distance_2 + conflicts  + gdp_mean + population_density
form_ban_log <- Disp_log ~ x + TA_lag2 + PA_lag2 + DL_lag2 + inv_distance_2 + conflicts  + gdp_mean + population_density
form_ban_y <- log_disp_ban ~ x + TA_lag2 + PA_lag2 + DL_lag2 + inv_distance_2 + conflicts + gdp_pp_destination + population_density

data_ban_y <- data_ban_y[data_ban_y$time <= 2023, ]
data_ban_y <- data_ban_y[data_ban_y$time >= 2016, ]

data_ban_log1 <- data_ban_log1[data_ban_log1$time <= 2022, ]
data_ban_log1 <- data_ban_log1[data_ban_log1$time >= 2021, ]


#log ban 
#lmh_b_log <- lm(form_ban_log1, data = data_ban_log)
#f_ind_b_log <- plm(form_ban_log1, data = data_ban_log, model = "within", index = c("admin1","month"), effect = "individual")
#f_time_b_log <- plm(form_ban_log1, data = data_ban_log, model = "within", index = c("admin1","month"), effect = "time")
#f_two_b_log <- plm(form_ban_log1, data = data_ban_log, model = "within", index = c("admin1","month"), effect = "twoways")
#stargazer(lmh_b_log, f_ind_b_log, f_time_b_log, f_two_b_log, type = "latex", out = "plm_log_latex.tex")

#log ban 
lmh_b_log1 <- lm(form_ban_log, data = data_ban_norm)
f_ind_b_log1 <- plm(form_ban_log, data = data_ban_norm, model = "within", index = c("admin1","month"), effect = "individual")
f_time_b_log1 <- plm(form_ban_log, data = data_ban_norm, model = "within", index = c("admin1","month"), effect = "time")
f_two_b_log1 <- plm(form_ban_log, data = data_ban_norm, model = "within", index = c("admin1","month"), effect = "twoways")
#stargazer(lmh_b_log1, f_ind_b_log1, f_time_b_log1, f_two_b_log1, type = "text", out = "plm_log_latex.tex")

lmh_b_y <- lm(form_ban_y, data = data_ban_y)
f_ind_b_y <- plm(form_ban_y, data = data_ban_y, model = "within", index = c("admin1","month"), effect = "individual")
f_time_b_y <- plm(form_ban_y, data = data_ban_y, model = "within", index = c("admin1","month"), effect = "time")
f_two_b_y <- plm(form_ban_y, data = data_ban_y, model = "within", index = c("admin1","month"), effect = "twoways")
#stargazer(lmh_b_y, f_ind_b_y, f_time_b_y, f_two_b_y, type = "text", out = "plm_log_latex.tex")



#find correlation between variables
#cor(data_ban_d_2[,c("x","inv_distance")])
#cor.test(data_ban_d_2$x, data_ban_d_2$inv_distance_2)
#create a vector of 204 random numbers
#set.seed(40)
#random <- runif(204, min = 0, max = 1)
#cor.test(random, data_ban_2$Banadir)

#predict f_time_b
#pred <- predict(f_time_b, newdata = data_ban_2)

#plot data_ban_log$log_disp_ban against data_ban_log$x
#plot(data_ban_log$x, data_ban_log$log_disp_ban)