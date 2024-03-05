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
library(pscl)
library(stats)
library(lmtest)
library(lme4)
#install.packages("")

data_d2 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_std_1997.csv")
#select data from 2016 onward data_d2[data_d2$time >= "2016-01"]
data_d2 <- data_d2[data_d2$time >= "2016-01",]


form <- conflicts ~ TA + PA + DL #+ sum_disp #+ population_density
form_1 <- conflicts ~ TA_lag1 + PA_lag1 + DL_lag1 #+ sum_disp# + population_density
form_2 <- conflicts ~ TA_lag2 + PA_lag2 + DL_lag2 #+ sum_disp_lag2#+ population_density
form_3 <- conflicts ~ TA_lag3 + PA_lag3 + DL_lag3 #+ sum_disp_lag3 #+ population_density
form_4 <- conflicts ~ TA_lag4 + PA_lag4 + DL_lag4 #+ sum_disp_lag4 #+ population_density
form_5 <- conflicts ~ TA_lag5 + PA_lag5 + DL_lag5 #+ sum_disp_lag5 #+ population_density
form_6 <- conflicts ~ TA_lag6 + PA_lag6 + DL_lag6 #+ sum_disp_lag6 #+ population_density

form_3d <- conflicts ~ TA + PA + DL + sum_disp + pspt(long, lat, time,
                                                      nknots = c(18, 18, 8),
                                                      psanova = TRUE,
                                                      nest_sp1 = c(1, 2, 3),
                                                      nest_sp2 = c(1, 2, 3),
                                                      nest_time = c(1, 2, 2),
                                                      f1t = FALSE, f2t = FALSE)

map_it <- st_read("/home/sara/Documenti/GitHub/Climate-and-conflict/Datasets/map_sm.shp")
data_sf <- st_as_sf(dplyr::left_join(data_d2, map_it, by = c("admin1" = "ADM1_EN")))


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

#linear model
lin <- lm(form, data = data_d2)

fe <- plm(form_2, 
              data = data_d2, 
              model = "within", 
              index = c("admin1","time"), 
              effect = "twoways")

#lmtest for spatial dependence
slmtest(fe, lwsp_inv, model="pooling", test=c("lme","lml","rlme","rlml"), index=NULL)


fe_ps <- pspatfit(form_2, data = data_d2,
                      demean = TRUE,
                      index = c("admin1","time"),
                      eff_demean = "twoways" )        

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
                        type = "sdm",
                        index = c("admin1", "time"))

slx_2 <- pspatfit(formula = form_2,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sdm",
                        index = c("admin1", "time"))

slx_3 <- pspatfit(formula = form_3,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sdm",
                        index = c("admin1", "time"))

slx_4 <- pspatfit(formula = form_4,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sdm",
                        index = c("admin1", "time"))      

slx_5 <- pspatfit(formula = form_5,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sdm",
                        index = c("admin1", "time"))          

slx_6 <- pspatfit(formula = form_6,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sdm",
                        index = c("admin1", "time"))        


# plot_sptime(slx, 
#              data = data_sf, 
#              time_var = "time", 
#              reg_var = "admin1")

# plot_sp3d(fe_ps, data = data_sf, 
#            time_var = "time", 
#            time_index = c("2016-01"),
#            addmain = TRUE, addint = TRUE)