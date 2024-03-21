library(stargazer)
library(plm)

data_d <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lags_norm_regr.csv")
data_d1 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lags_norm_regr_log.csv")
data_d2a <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_norm_log_d_lag.csv")
data_d2 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_std_log_d_lag.csv")
data1 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lag1_2016_n4_disp_pop.csv")
#data1 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_lags_2016_n4_disp_d_pop_norm.csv")

form <- conflicts ~  TA + PA + DL #+ sum_disp #+ population_density
form_1 <- conflicts ~ TA_lag1 + PA_lag1 + DL_lag1 + sum_disp_lag1 #+ population_density
form_2 <- conflicts ~ TA_lag2 + PA_lag2 + DL_lag2 + sum_disp_lag2 #+ population_density
form_3 <- conflicts ~ TA_lag3 + PA_lag3 + DL_lag3 + sum_disp_lag3 #+ population_density
form_4 <- conflicts ~ TA_lag4 + PA_lag4 + DL_lag4 + sum_disp_lag4 #+ population_density
form_5 <- conflicts ~ TA_lag5 + PA_lag5 + DL_lag5 + sum_disp_lag5 #+ population_density
form_6 <- conflicts ~ TA_lag6 + PA_lag6 + DL_lag6 + sum_disp_lag6 #+ population_density

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

fe_lag2 <- plm(form_2,
              data = data_d2, 
              model = "within", 
              index = c("admin1","time"), 
              effect = "twoways")

fe_lag3 <- plm(form_3,      
              data = data_d2, 
              model = "within", 
              index = c("admin1","time"), 
              effect = "twoways")

fe_lag4 <- plm(form_4,
              data = data_d2, 
              model = "within", 
              index = c("admin1","time"), 
              effect = "twoways")

fe_lag5 <- plm(form_5,
              data = data_d2, 
              model = "within", 
              index = c("admin1","time"), 
              effect = "twoways")

fe_lag6 <- plm(form_6,
              data = data_d2, 
              model = "within", 
              index = c("admin1","time"), 
              effect = "twoways")


fixefs <- fixef(fe, "twoways")
fitted_by_hand <- fixefs + fe$coefficients["TA"] * fe$model$TA +fe$coefficients["PA"] * fe$model$PA + fe$coefficients["DL"] * fe$model$DL

try <- plm(conflicts ~  as.numeric(fitted_by_hand), 
              data = data_d2, 
              model = "within", 
              index = c("admin1","time"), 
              effect = "twoways")

yhat <- as.numeric(fe$model[ , 1] - fe$residuals) # reference
yhat1 <- as.numeric(predict(fe)) # fitted values
pred_beta <- as.numeric(tcrossprod(coef(fe), as.matrix(fe$model[ , -1])))
pred_effs <- as.numeric(fixef(fe, "twoways")) # sum of ind and time effects

# ok they are all the same
plot(data_d2$conflicts, yhat, asp = 1)
all.equal(yhat1, yhat)

stargazer(fe, fe_lag1, fe_lag2, fe_lag3, type = "text")

# coefficients
# fe_all <- data.frame(coef(fe), coef(fe_lag1), coef(fe_lag2), coef(fe_lag3), coef(fe_lag4), coef(fe_lag5), coef(fe_lag6))
# write.csv(fe_all, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/fe_all.csv")

# # r squared
# fe_r_all <- data.frame(summary(fe)$r.squared, summary(fe_lag1)$r.squared, summary(fe_lag2)$r.squared, summary(fe_lag3)$r.squared, summary(fe_lag4)$r.squared, summary(fe_lag5)$r.squared, summary(fe_lag6)$r.squared)
# write.csv(fe_r_all, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/fe_r_all.csv")

# # p values
# fe_p_all <- data.frame(summary(fe)$coef[,4], summary(fe_lag1)$coef[,4], summary(fe_lag2)$coef[,4], summary(fe_lag3)$coef[,4], summary(fe_lag4)$coef[,4], summary(fe_lag5)$coef[,4], summary(fe_lag6)$coef[,4])
# write.csv(fe_p_all, file = "/home/sara/Documenti/GitHub/Climate-and-conflict/latex/fe_p_all.csv")
#plot(as.numeric(fe$model[ , 1]), as.numeric(fe$model[ , 1] - residuals(fe)), asp = 1)
