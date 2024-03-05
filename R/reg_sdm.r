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


data_d <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lags_norm_regr.csv")
data_d1 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lags_norm_regr_log.csv")
data_d2 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_norm_log_d.csv")
data1 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lag1_2016_n4_disp_pop.csv")
#data1 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lags_2016_n4_disp_d_pop_norm.csv")
data_d2 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_std_1997.csv")

form <- conflicts ~ TA + PA + DL #+ sum_disp #+ population_density
form_1 <- conflicts ~ TA_lag1 + PA_lag1 + DL_lag1 #+ population_density
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


fe <- plm(form, 
              data = data_d2, 
              model = "within", 
              index = c("admin1","time"), 
              effect = "twoways")

sdm <- pspatfit(formula = form,
                    data = data_d2, 
                    listw = lwsp_inv, 
                    demean = TRUE,
                    eff_demean = "twoways",
                    method = "eigen",
                    type = "sdm",
                    index = c("admin1", "time"))

sdm_1 <- pspatfit(formula = form_1,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sdm",
                        index = c("admin1", "time"))

sdm_2 <- pspatfit(formula = form_2,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sdm",
                        index = c("admin1", "time"))

sdm_3 <- pspatfit(formula = form_3,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sdm",
                        index = c("admin1", "time"))

sdm_4 <- pspatfit(formula = form_4,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sdm",
                        index = c("admin1", "time"))

sdm_5 <- pspatfit(formula = form_5,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sdm",
                        index = c("admin1", "time"))

sdm_6 <- pspatfit(formula = form_6,
                        data = data_d2, 
                        listw = lwsp_inv, 
                        demean = TRUE,
                        eff_demean = "twoways",
                        method = "eigen",
                        type = "sdm",
                        index = c("admin1", "time"))  


y_true <- data_d2$conflicts
y_pred <- as.numeric(data_d2$conflicts - residuals(sdm))
y_mean <- mean(y_true)
RSS <- sum((y_true - y_pred)^2)
TSS <- sum((y_true - y_mean)^2)
r_squared <- 1 - (RSS / TSS)

#print(paste("R-squared value is: ", r_squared))
#print(r.squared(sdm, model = NULL, type = "cor", dfcor = FALSE))

#fitted-vs-observed plot
#plot larger ticks and labels
par(cex = 2.5)
y_t_r <- as.numeric(data_d2$conflicts - residuals(sdm))
y_f <- fitted(sdm)
y_r <- residuals(sdm)
y_0 <-  y_true - y_f - y_r


# plot(y_true, 
# y_pred, 
# xlab = "true values",
# ylab = 'true values - residuals',
# type = "p", cex.lab = 1.3,
# cex.main = 1.3,
# main = "SDM model")
# print(all.equal((y_f), y_pred))

#plot(y_f, y_r)


#plot(as.numeric(data_d2$conflicts - residuals(sdm)), data_d2$conflicts, asp = 1)
#abline(0, 1, col = 'red', lty = 'dashed', lwd = 2)                      
#plot best fit line as dotted blue line

# plot(data_d2$conflicts,
# sdm$fitted.values,
# xlab = "true values",
# ylab = 'fitted values',
# type = "p", cex.lab = 1.3,
# cex.main = 1.3,
# main = "Fitted Values SDM model")

#plot(sdm$fitted.values, sdm$residuals,
#xlab = 'fitted values', ylab = "residuals",
#type = "p", cex.lab = 1.3, cex.main=1.3,
#main = "Residuals SDM model")

# coefficients
sdm_all <- data.frame(coef(sdm), coef(sdm_1), coef(sdm_2), coef(sdm_3), coef(sdm_4), coef(sdm_5), coef(sdm_6))
write.csv(sdm_all, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/sdm_all_1997.csv")

# AIC
sdm_r_all <- data.frame(summary(sdm)$aic, summary(sdm_1)$aic, summary(sdm_2)$aic, summary(sdm_3)$aic, summary(sdm_4)$aic, summary(sdm_5)$aic, summary(sdm_6)$aic)
write.csv(sdm_r_all, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/sdm_r_all_1997.csv")

# p values
sdm_p_all <- data.frame(summary(sdm)$coef[,4], summary(sdm_1)$coef[,4], summary(sdm_2)$coef[,4], summary(sdm_3)$coef[,4], summary(sdm_4)$coef[,4], summary(sdm_5)$coef[,4], summary(sdm_6)$coef[,4])
write.csv(sdm_p_all, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/sdm_p_all_1997.csv")

# log likelihood
sdm_ll_all <- data.frame(logLik(sdm), logLik(sdm_1), logLik(sdm_2), logLik(sdm_3), logLik(sdm_4), logLik(sdm_5), logLik(sdm_6))
write.csv(sdm_ll_all, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/sdm_ll_all_1997.csv")




