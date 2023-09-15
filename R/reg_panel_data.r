setwd("C:/Users/PcLaptop/Documents/GitHub/Climate-and-conflict/R")
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

data1 <- read.csv("df_with_dummies.csv")
map_it <- st_read("som_adm_ocha_itos_20230308_shp/som_admbnda_adm1_ocha_20230308.shp") # nolint: line_length_linter.
adj_m <- read.csv("adj_som.csv", header = FALSE)
adj_m <- adj_m[-1, -1]
adj <- matrix(unlist(adj_m), nrow = 18, ncol = 18)

dist <- read.csv("dist_som.csv", header = FALSE)
dist <- dist[-1, ]
dist <- matrix(unlist(dist), nrow = 18, ncol = 18)
dist <- apply(dist, 2, function(x) as.numeric(as.character(x)))

inv_dist <- 100 / (as.matrix(dist)+0.0001)
lwsp_inv <- spdep::mat2listw(inv_dist, style = "W")

#data1 <- data1[data1$time <= 2010, ]
map_it$ADM1_EN <- gsub(" ", "_", map_it$ADM1_EN)

unemp_it_sf <- st_as_sf(dplyr::left_join(data1, map_it, by = c("admin1" = "ADM1_EN"))) # nolint: line_length_linter.
lwsp_it <- spdep::mat2listw(adj, style = "W")
#

formlin <- conflicts ~ TA + PA + DL
formlin_d <- conflicts ~ TA + PA + DL + factor(admin1) + factor(month) + factor(admin1):factor(month)

splm_model <- spml(formlin,
               data = data1, 
               index=c("admin1","month"),
               listw = lwsp_it,
               model="within",
               effect = "twoways",
               spatial.error="none", 
               lag=TRUE, 
               Hess = FALSE)

L_sar_REML <- pspatfit(formula = formlin,
                        data = data1, 
                        listw = lwsp_inv, 
                        #demean = TRUE,
                        #eff_demean = "twoways",
                        #method = "eigen",
                        type = "sar",
                        index = c("admin1", "month"),
                        )

L_sem_REML <- pspatfit(formlin,
                        data = data1, 
                        listw = lwsp_it,
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sem", 
                        index = c("admin1", "month")
                        )

lmh <- lm(formlin, data = data1) 
lmh_d <- lm(formlin_d , data = data1)

#pooling <- plm(formlin_d, data = data1, model = "pooling", index = c("admin1","month"), effect = "twoways")
fixed <- plm(formlin, data = data1, model = "within", index = c("admin1","month"), effect = "twoways")
#random <- plm(formlin_d, data = data1, model = "random", index = c("admin1","month"), effect = "twoways")

#compute AIC
logLik.plm <- function(object){
  out <- -plm::nobs(object) * log(2 * var(object$residuals) * pi)/2 - deviance(object)/(2 * var(object$residuals))
  
  attr(out,"df") <- nobs(object) - object$df.residual
  attr(out,"nobs") <- plm::nobs(object)
  return(out)
}

#pred_effs <- as.numeric(fixef(gtw_u,"twoways"))
#eff_id_level <- as.numeric(fixef(fixed,"individual"))[ii] 
#eff_ti_level <- as.numeric(fixef(fixed,"time"))[it]
summary <- summary(fixed)

mod.pois <- glm(
    formlin,
    data = data1, family = poisson
  )