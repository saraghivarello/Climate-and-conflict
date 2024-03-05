#setwd("C:/Users/PcLaptop/Documents/GitHub/Climate-and-conflict/R")
#setwd("/home/sara/R/x86_64-pc-linux-gnu-library/4.3warnings()")
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
library(xtable)

data1 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lag1_2016_n4_disp_pop.csv")
data1 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lags_2016_n4_disp_d_pop_norm.csv")
map_it <- st_read("/home/sara/Documenti/GitHub/Climate-and-conflict/Datasets/som_adm_ocha_itos_20230308_shp/som_admbnda_adm1_ocha_20230308.shp") # nolint: line_length_linter.
adj_m <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/adj_som.csv", header = FALSE)
adj_m <- adj_m[-1, -1]
adj <- matrix(unlist(adj_m), nrow = 18, ncol = 18)
lwsp_it <- spdep::mat2listw(adj, style = "W")

# disp_flows <- read.csv("weights_normalized.csv", header = FALSE)
# disp_flows <- disp_flows[-1, ]
# disp_flows <- matrix(unlist(disp_flows), nrow = 18, ncol = 18)
# #disp_flows <- as.matrix(disp_flows)
# lwsp_disp_flows <- spdep::mat2listw(disp_flows, style = "W")

dist <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/dist_som_2.csv", header = FALSE)
dist <- dist[-1,-1]
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

data1 <- data1[data1$time >= 2016, ]
#data1 <- data1[data1$time <= 2018, ]

formlin <- conflicts ~ TA_lag1 + PA_lag1 + DL_lag1 + sum_disp #+ population_density

reg <- lm(formlin, data = data1)

fe <- plm(formlin, 
              data = data1, 
              model = "within", 
              index = c("admin1","time"), 
              effect = "twoways")


re <- plm(formlin, 
              data = data1, 
              model = "random", 
              index = c("admin1","time"), 
              effect = "twoways")

phtest(fe, re)

sar <- pspatfit(formula = formlin,
                        data = data1, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sar",
                        index = c("admin1", "time"))

sar_1 <- spml(formlin,
               data = data1, 
               index=c("admin1", "time"),
               listw = lwsp_inv,
               model="within",
               effect = "twoways",
               spatial.error="none", 
               lag=TRUE, 
               Hess = FALSE)

sarar <- pspatfit(formula = formlin,
                        data = data1, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sarar",
                        index = c("admin1", "time"))

sarar_1 <- spml(formlin,
               data = data1, 
               index=c("admin1", "time"),
               listw = lwsp_inv,
               model="within",
               effect = "twoways",
               spatial.error="b", 
               lag=TRUE, 
               method = "eigen", 
               na.action = na.fail, 
               quiet = TRUE, 
               zero.policy = NULL,
               control = list(), 
               legacy = FALSE)


sem <- pspatfit(formula = formlin,
                        data = data1, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sem",
                        index = c("admin1", "time"))
