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


data_d <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lags_norm_regr.csv")
data_d1 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lags_norm_regr_log.csv")
data_d2 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_norm_log_d.csv")
data1 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lag1_2016_n4_disp_pop.csv")
#data1 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lags_2016_n4_disp_d_pop_norm.csv")

form <- conflicts ~ TA + PA + DL #+ sum_disp #+ population_density
form_1 <- conflicts ~ TA_lag1 + PA_lag1 + DL_lag1 #+ population_density
form_2 <- conflicts ~ TA_lag2 + PA_lag2 + DL_lag2 #+ population_density
form_3 <- conflicts ~ TA_lag3 + PA_lag3 + DL_lag3 #+ population_density
form_4 <- conflicts ~ TA_lag4 + PA_lag4 + DL_lag4 #+ population_density
form_5 <- conflicts ~ TA_lag5 + PA_lag5 + DL_lag5 #+ population_density
form_6 <- conflicts ~ TA_lag6 + PA_lag6 + DL_lag6 #+ population_density

fe <- plm(form, 
              data = data_d2, 
              model = "within", 
              index = c("admin1","time"), 
              effect = "twoways")

slx <- pspatfit(formula = form,
                    data = data_d2, 
                    listw = lwsp_inv, 
                    demean = TRUE,
                    eff_demean = "twoways",
                    method = "eigen",
                    type = "slx",
                    index = c("admin1", "time"))

slx_1 <- pspatfit(formula = form_1,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "slx",
                        index = c("admin1", "time"))

slx_1 <- pspatfit(formula = form_1,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "slx",
                        index = c("admin1", "time"))

slx_2 <- pspatfit(formula = form_2,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "slx",
                        index = c("admin1", "time"))

slx_3 <- pspatfit(formula = form_3,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "slx",
                        index = c("admin1", "time"))

slx_4 <- pspatfit(formula = form_4,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "slx",
                        index = c("admin1", "time"))

slx_5 <- pspatfit(formula = form_5,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "slx",
                        index = c("admin1", "time"))

slx_6 <- pspatfit(formula = form_6,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "slx",
                        index = c("admin1", "time"))                        

#stargazer(fe, type = "text", out = "latex/lm_latex.tex")

# coefficients
slx_all <- data.frame(coef(slx), coef(slx_1), coef(slx_2), coef(slx_3), coef(slx_4), coef(slx_5), coef(slx_6))
write.csv(slx_all, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/slx_all.csv")

# AIC
slx_r_all <- data.frame(summary(slx)$aic, summary(slx_1)$aic, summary(slx_2)$aic, summary(slx_3)$aic, summary(slx_4)$aic, summary(slx_5)$aic, summary(slx_6)$aic)
write.csv(slx_r_all, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/slx_r_all.csv")

# p values
slx_p_all <- data.frame(summary(slx)$coef[,4], summary(slx_1)$coef[,4], summary(slx_2)$coef[,4], summary(slx_3)$coef[,4], summary(slx_4)$coef[,4], summary(slx_5)$coef[,4], summary(slx_6)$coef[,4])
write.csv(slx_p_all, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/slx_p_all.csv")

#extract aic of all models and put it in a dataframe




map_it <- st_read("/home/sara/Documenti/GitHub/Climate-and-conflict/Datasets/som_adm_ocha_itos_20230308_shp/som_admbnda_adm1_ocha_20230308.shp") # nolint: line_length_linter.
adj_m <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/adj_som.csv", header = FALSE)
adj_m <- adj_m[-1, -1]
adj <- matrix(unlist(adj_m), nrow = 18, ncol = 18)
lwsp_it <- spdep::mat2listw(adj, style = "W")


dist <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/dist_som.csv", header = FALSE)
dist <- dist[-1, ]
dist <- matrix(unlist(dist), nrow = 18, ncol = 18)
dist <- apply(dist, 2, function(x) as.numeric(as.character(x)))

W <- DistWMat(
  dist,
  distCutOff = NULL,
  type = "inverse",
  alpha = NULL,
  mevn = FALSE
)
lwsp_inv <- spdep::mat2listw(W, style = "W")



