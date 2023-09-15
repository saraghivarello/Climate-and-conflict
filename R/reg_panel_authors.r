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
library(haven)

data1 <- haven::read_dta("C:/Users/PcLaptop/Documents/GitHub/Climate-and-conflict/AJAE_MaystadtEcker.dta") # nolint
map_it <- st_read("som_adm_ocha_itos_20230308_shp/som_admbnda_adm1_ocha_20230308.shp") # nolint: line_length_linter.
adj_m <- read.csv("adj_som.csv", header = FALSE)
adj_m <- adj_m[-1, -1]
adj <- matrix(unlist(adj_m), nrow = 18, ncol = 18)
lwsp_it <- spdep::mat2listw(adj, style = "W")

formlin <- tot_violent_mth ~ TA3_m80 + PA3_83 + DL_TA3_m80
#formlin_d <- conflicts ~ TA + PA + DL + factor(admin1):factor(month)

# splm_model <- spml(formlin,
#                data = data1, 
#                index=c("new_province_id","newident_yrmth"),
#                listw = lwsp_it,
#                model="within",
#                effect = "twoways",
#                spatial.error="none", 
#                lag=TRUE, 
#                Hess = FALSE)

# L_sar_REML <- pspatfit(formula = formlin,
#                         data = data1, 
#                         listw = lwsp_inv, 
#                         #demean = TRUE,
#                         #eff_demean = "twoways",
#                         #method = "eigen",
#                         type = "sar",
#                         index = c("new_province_id","newident_yrmth"),
#                         )

# L_sem_REML <- pspatfit(formlin,
#                         data = data1, 
#                         listw = lwsp_it,
#                         demean = TRUE,
#                         eff_demean = "twoways",
#                         method = "eigen",
#                         type = "sem", 
#                         index = c("new_province_id","newident_yrmth")
#                         )

lmh <- lm(formlin, data = data1) 
#lmh_d <- lm(formlin_d , data = data1)

#pooling <- plm(formlin_d, data = data1, model = "pooling", index = c("admin1","month"), effect = "twoways")
fixed <- plm(formlin, data = data1, model = "within", index = c("new_province_id","newident_yrmth"), effect = "twoways")
#random <- plm(formlin_d, data = data1, model = "random", index = c("admin1","month"), effect = "twoways")

#compute AIC of fixed
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
#summary(mod.pois)
# compute r^2
1 - mod.pois$deviance / mod.pois$null.deviance

