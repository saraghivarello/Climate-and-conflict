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

data_d <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lags_norm_regr.csv")
data_d1 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lags_norm_regr_log.csv")
data_d2 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_std_log_d_lag.csv")
data1 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lag1_2016_n4_disp_pop.csv")
#data1 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lags_2016_n4_disp_d_pop_norm.csv")
data_d2 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_std_1997.csv")

form <- conflicts ~ TA + PA + DL #+ sum_disp #+ population_density
form_1 <- conflicts ~ TA_lag1 + PA_lag1 + DL_lag1 #+ sum_disp_lag1#+ population_density
form_2 <- conflicts ~ TA_lag2 + PA_lag2 + DL_lag2 #+ sum_disp_lag2#+ population_density
form_3 <- conflicts ~ TA_lag3 + PA_lag3 + DL_lag3 #+ sum_disp_lag3 #+ population_density
form_4 <- conflicts ~ TA_lag4 + PA_lag4 + DL_lag4 #+ sum_disp_lag4 #+ population_density
form_5 <- conflicts ~ TA_lag5 + PA_lag5 + DL_lag5 #+ sum_disp_lag5 #+ population_density
form_6 <- conflicts ~ TA_lag6 + PA_lag6 + DL_lag6 #+ sum_disp_lag6 #+ population_density

map_it <- st_read("/home/sara/Documenti/GitHub/Climate-and-conflict/Datasets/map_sm.shp")
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

#linear model
lin <- lm(form, data = data_d2)

fe <- plm(form, 
              data = data_d2, 
              model = "within", 
              index = c("admin1","time"), 
              effect = "twoways")

fe_lag1 <- plm(form_1, 
              data = data_d2, 
              model = "within", 
              index = c("admin1","time"), 
              effect = "twoways")

form_m <- as.formula('conflicts ~ TA + PA + DL + sum_disp + (1|admin1) + (1|time)')
form_m_lag1  <- as.formula('conflicts ~ TA_lag1 + PA_lag1 + DL_lag1 + sum_disp + (1|admin1) + (1|time)')
#do the same with lme
mle_fe <- lmer(form_m, data = data_d2)
mle_fe_lag1 <- lmer(form_m_lag1, data = data_d2)
x <- summary(mle_fe_lag1)
print(x, signif.stars = getOption("show.signif.stars"))

fe_ps <- pspatfit(form, data = data_d2,
                      demean = TRUE,
                      index = c("admin1","time"),
                      eff_demean = "twoways" )

fe_ps_lag1 <- pspatfit(form_1, data = data_d2,
                      demean = TRUE,
                      index = c("admin1","time"),
                      eff_demean = "twoways" )  
                    
fe_ps_lag2 <- pspatfit(form_2, data = data_d2,
                      demean = TRUE,
                      index = c("admin1","time"),
                      eff_demean = "twoways" )

fe_ps_lag3 <- pspatfit(form_3, data = data_d2,                          
                      demean = TRUE,
                      index = c("admin1","time"),
                      eff_demean = "twoways" )              

fe_ps_lag4 <- pspatfit(form_4, data = data_d2,
                      demean = TRUE,
                      index = c("admin1","time"),
                      eff_demean = "twoways" )              

fe_ps_lag5 <- pspatfit(form_5, data = data_d2,
                      demean = TRUE,
                      index = c("admin1","time"),
                      eff_demean = "twoways" )              

fe_ps_lag6 <- pspatfit(form_6, data = data_d2,                  
                      demean = TRUE,
                      index = c("admin1","time"),
                      eff_demean = "twoways" )          

#plot the fitted values versus the true values
plot(data_d2$conflicts, as.numeric(data_d2$conflicts - residuals(fe_ps_lag5)), asp = 1)

                                                    

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

sdm <- pspatfit(formula = form,
                    data = data_d2, 
                    listw = lwsp_inv, 
                    demean = TRUE,
                    eff_demean = "twoways",
                    method = "eigen",
                    type = "sdm",
                    index = c("admin1", "time"))    

sar <- pspatfit(formula = form,
                    data = data_d2, 
                    listw = lwsp_inv, 
                    demean = TRUE,
                    eff_demean = "twoways",
                    method = "eigen",
                    type = "sar",
                    index = c("admin1", "time"))

sem <- pspatfit(formula = form,
                    data = data_d2, 
                    listw = lwsp_inv, 
                    demean = TRUE,
                    eff_demean = "twoways",
                    method = "eigen",
                    type = "sem",
                    index = c("admin1", "time"))                    

#perform a likelihood ratio test
lrtest(slx, sar)

# FE

# # coefficients fe
# fe_ml <- data.frame(coef(fe_ps), coef(fe_ps_lag1), coef(fe_ps_lag2), coef(fe_ps_lag3), coef(fe_ps_lag4), coef(fe_ps_lag5), coef(fe_ps_lag6))
# write.csv(fe_ml, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/fe_ml.csv")
# # p values
# slx_p_all <- data.frame(summary(fe_ps)$coef[,4], summary(fe_ps_lag1)$coef[,4], summary(fe_ps_lag2)$coef[,4], summary(fe_ps_lag3)$coef[,4], summary(fe_ps_lag4)$coef[,4], summary(fe_ps_lag5)$coef[,4], summary(fe_ps_lag6)$coef[,4])
# write.csv(slx_p_all, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/fe_ml_p.csv")
# # log likelihood fe
# fe_ml_ll <- data.frame(logLik(fe_ps), logLik(fe_ps_lag1), logLik(fe_ps_lag2), logLik(fe_ps_lag3), logLik(fe_ps_lag4), logLik(fe_ps_lag5), logLik(fe_ps_lag6))
# write.csv(fe_ml_ll, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/fe_ml_ll.csv")
# # AIC
# fe_ml_aic <- data.frame(AIC(fe_ps), AIC(fe_ps_lag1), AIC(fe_ps_lag2), AIC(fe_ps_lag3), AIC(fe_ps_lag4), AIC(fe_ps_lag5), AIC(fe_ps_lag6))
# write.csv(fe_ml_aic, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/fe_ml_aic.csv")



# SLX

# coefficients
slx_all <- data.frame(coef(slx), coef(slx_1), coef(slx_2), coef(slx_3), coef(slx_4), coef(slx_5), coef(slx_6))
write.csv(slx_all, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/slx_std_1997.csv")
# AIC
slx_r_all <- data.frame(summary(slx)$aic, summary(slx_1)$aic, summary(slx_2)$aic, summary(slx_3)$aic, summary(slx_4)$aic, summary(slx_5)$aic, summary(slx_6)$aic)
write.csv(slx_r_all, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/slx_r_std_1997.csv")
# p values
slx_p_all <- data.frame(summary(slx)$coef[,4], summary(slx_1)$coef[,4], summary(slx_2)$coef[,4], summary(slx_3)$coef[,4], summary(slx_4)$coef[,4], summary(slx_5)$coef[,4], summary(slx_6)$coef[,4])
write.csv(slx_p_all, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/slx_p_std_1997.csv")
# log likelihood
slx_ll_all <- data.frame(logLik(slx), logLik(slx_1), logLik(slx_2), logLik(slx_3), logLik(slx_4), logLik(slx_5), logLik(slx_6))
write.csv(slx_ll_all, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/slx_ll_std_1997.csv")


#compute the y predicted for slx model
y_true <- data_d2$conflicts
y_pred <- fitted(slx_1)

plot(as.numeric(data_d2$conflicts - residuals(fe)), data_d2$conflicts, asp = 1)
plot(as.numeric(predict(fe)), data_d2$conflicts, asp = 1)
cex.main = 3.3
y_pred <- fitted(fe_ps)
y_pred <- as.numeric(y_pred)
#plot larger ticks and labels
#par(cex = 2)
#plot(y_true, y_pred, asp = 1)
#plot(as.numeric(predict(mle_fe)), data_d2$conflicts, asp = 1)
#abline(0, 1, col = 'red', lty = 'dashed', lwd = 2)

#access first element of coef(fe_ps)
#compare two arrays y_pred and pred
#Wlag_TA = W %*% data_d2$TA

yhat <- as.numeric(data_d2$conflicts - fe_ps$residuals) # reference
f_pred <- as.numeric(fitted(fe_ps)) # fitted values
#compute manually the fitted values
pred <- coef(fe_ps)[1] * data_d2$TA  + coef(fe_ps)[2] * data_d2$PA + coef(fe_ps)[3] * data_d2$DL
pred_beta <- as.numeric(tcrossprod(coef(fe_ps), as.matrix(data_d2[, c("TA", "PA", "DL")])))
pred_e <- as.numeric(fe_ps$bfixed) # sum of ind and time effects

all.equal(pred + pred_beta, yhat)
#plot(pred_e + pred_beta, yhat)

#plm
yhat <- as.numeric(fe$model[ , 1] - fe$residuals) # reference
pred_beta <- as.numeric(tcrossprod(coef(fe), as.matrix(fe$model[ , -1])))
pred_effs <- as.numeric(fixef(fe, "twoways")) # sum of ind and time effects
#all.equal(pred_effs + pred_beta, yhat) # TRUE
#plot(data_d2$conflicts , yhat)

plot_sptime(fe_ps, 
             data = data_d2, 
             time_var = "time", 
             reg_var = "admin1")

plot_sp3d(fe_ps, data = data_d2, 
           time_var = "time", 
           time_index = c(2016-01),
           addmain = TRUE, addint = TRUE)