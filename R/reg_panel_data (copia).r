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



data1 <- read.csv("df_lag1.csv")
map_it <- st_read("Datasets/som_adm_ocha_itos_20230308_shp/som_admbnda_adm1_ocha_20230308.shp") # nolint: line_length_linter.
adj_m <- read.csv("adj_som.csv", header = FALSE)
adj_m <- adj_m[-1, -1]
adj <- matrix(unlist(adj_m), nrow = 18, ncol = 18)


# dist <- read.csv("dist_som.csv", header = FALSE)
# dist <- dist[-1, ]
# dist <- matrix(unlist(dist), nrow = 18, ncol = 18)
# dist <- apply(dist, 2, function(x) as.numeric(as.character(x)))

# inv_dist <- 100 / (as.matrix(dist)+0.0001)
# lwsp_inv <- spdep::mat2listw(inv_dist, style = "W")

#data1 <- data1[data1$time >= 2016, ]

map_it$ADM1_EN <- gsub(" ", "_", map_it$ADM1_EN)

unemp_it_sf <- st_as_sf(dplyr::left_join(data1, map_it, by = c("admin1" = "ADM1_EN"))) # nolint: line_length_linter.
lwsp_it <- spdep::mat2listw(adj, style = "W")

formlin <- conflicts ~ TA_lag1 + PA_lag1 + DL_lag1
#formlin_d <- conflicts ~ TA + PA + DL + factor(admin1) + factor(month) #+ factor(admin1):factor(month)

fixed <- plm(formlin, data = data1, model = "within", index = c("admin1","month"), effect = "twoways")

L_sar_REML <- pspatfit(formula = formlin,
                        data = data1, 
                        listw = lwsp_it, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sar",
                        index = c("admin1", "month"),
                        )

summary(L_sar_REML)




