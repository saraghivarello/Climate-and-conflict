#setwd("C:/Users/PcLaptop/Documents/GitHub/Climate-and-conflict/R")
#setwd("/home/sara/R/x86_64-pc-linux-gnu-library/4.3warnings()")

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

data1 <- read.csv("csv/df_lag1_disp_pop.csv")
map_it <- st_read("Datasets/som_adm_ocha_itos_20230308_shp/som_admbnda_adm1_ocha_20230308.shp") # nolint: line_length_linter.
adj_m <- read.csv("csv/adj_som.csv", header = FALSE)
adj_m <- adj_m[-1, -1]
adj <- matrix(unlist(adj_m), nrow = 18, ncol = 18)
lwsp_it <- spdep::mat2listw(adj, style = "W")

# disp_flows <- read.csv("weights_normalized.csv", header = FALSE)
# disp_flows <- disp_flows[-1, ]
# disp_flows <- matrix(unlist(disp_flows), nrow = 18, ncol = 18)
# #disp_flows <- as.matrix(disp_flows)
# lwsp_disp_flows <- spdep::mat2listw(disp_flows, style = "W")

dist <- read.csv("csv/dist_som.csv", header = FALSE)
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

data1 <- data1[data1$time >= 2016, ]

# map_it$ADM1_EN <- gsub(" ", "_", map_it$ADM1_EN)
# unemp_it_sf <- st_as_sf(dplyr::left_join(data1, map_it, by = c("admin1" = "ADM1_EN"))) # nolint: line_length_linter.

formlin <- conflicts_pro_capite ~ TA_lag1 + PA_lag1 + DL_lag1 + sum_disp #+ population_density
#formlin_d <- conflicts ~ TA + PA + DL + factor(admin1) + factor(month) #+ factor(admin1):factor(month)

fixed <- plm(formlin, data = data1, model = "within", index = c("admin1","time"), effect = "twoways")

sar <- pspatfit(formula = formlin,
                        data = data1, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sar",
                        index = c("admin1", "time"))


sem <- pspatfit(formula = formlin,
                        data = data1, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sem",
                        index = c("admin1", "time"))
#summary(sar)

slx <- pspatfit(formula = formlin,
                        data = data1, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "slx",
                        index = c("admin1", "time"))


sdm <- pspatfit(formula = formlin,
                        data = data1, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sdm",
                        index = c("admin1", "time"))
                        


sarar <- pspatfit(formula = formlin,
                        data = data1, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sarar",
                        index = c("admin1", "time"))


sdem <- pspatfit(formula = formlin,
                        data = data1, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sdem",
                        index = c("admin1", "time"))

# summary(gns)