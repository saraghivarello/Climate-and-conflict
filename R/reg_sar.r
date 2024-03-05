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
library(SDPDmod)


data_d2 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_norm_log_d.csv")
data1 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lag1_2016_n4_disp_pop.csv")
data_d2 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lags_2016_n4_disp_d_pop_norm.csv")

form <- conflicts ~ TA + PA + DL #+ sum_disp #+ population_density
form_1 <- conflicts ~ TA_lag1 + PA_lag1 + DL_lag1 #+ sum_disp #+ population_density
form_2 <- conflicts ~ TA_lag2 + PA_lag2 + DL_lag2 #+ population_density
form_3 <- conflicts ~ TA_lag3 + PA_lag3 + DL_lag3 #+ population_density
form_4 <- conflicts ~ TA_lag4 + PA_lag4 + DL_lag4 #+ population_density
form_5 <- conflicts ~ TA_lag5 + PA_lag5 + DL_lag5 #+ population_density
form_6 <- conflicts ~ TA_lag6 + PA_lag6 + DL_lag6 #+ population_density


map_it <- st_read("/home/sara/Documenti/GitHub/Climate-and-conflict/Datasets/som_adm_ocha_itos_20230308_shp/som_admbnda_adm1_ocha_20230308.shp") # nolint: line_length_linter.
adj_m <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/adj_som.csv", header = FALSE)
adj_m <- adj_m[-1, -1]
adj <- matrix(unlist(adj_m), nrow = 18, ncol = 18)
lwsp_it <- spdep::mat2listw(adj, style = "W")


dist <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/dist_som_2.csv", header = FALSE)
dist <- dist[-1, -1]

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


sar <- pspatfit(formula = form,
                    data = data_d2, 
                    listw = lwsp_inv, 
                    demean = TRUE,
                    eff_demean = "twoways",
                    method = "eigen",
                    type = "sar",
                    index = c("admin1", "time"))

sar_1 <- pspatfit(formula = form_1,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sar",
                        index = c("admin1", "time"))

sar_2 <- pspatfit(formula = form_2,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sar",
                        index = c("admin1", "time"))

sar_3 <- pspatfit(formula = form_3,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sar",
                        index = c("admin1", "time"))

sar_4 <- pspatfit(formula = form_4,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sar",
                        index = c("admin1", "time"))

sar_5 <- pspatfit(formula = form_5,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sar",
                        index = c("admin1", "time"))

sar_6 <- pspatfit(formula = form_6,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sar",
                        index = c("admin1", "time"))  



