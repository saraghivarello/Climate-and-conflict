library(stargazer)
library(plm)
library(ggplot2)

data_d2 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_std_log_d_lag.csv")
data_d2 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/csv/df_std_log_c.csv")


form <- conflicts_log ~  TA_lag1 + PA_lag1 + DL_lag1 + sum_disp #+ population_density

fe <- plm(form, 
              data = data_d2, 
              model = "within", 
              index = c("admin1","time"), 
              effect = "twoways")

fe_ps <- pspatfit(form, data = data_d2,
        demean = TRUE,
        index = c("admin1","time"),
        eff_demean = "twoways" )  


fixefs <- fixef(fe, "twoways")
fitted_by_hand <- fixefs + fe$coefficients["TA_lag1"] * fe$model$TA_lag1 +fe$coefficients["PA_lag1"] * fe$model$PA_lag1 + fe$coefficients["DL_lag1"] * fe$model$DL_lag1 + fe$coefficients["sum_disp"] * fe$model$sum_disp

yhat <- fe$fitted
#plot(yhat~data_d2$conflicts|data_d2$admin1, boxplots=FALSE, xlab="x1", ylab="yhat",smooth=FALSE)

yhat <- as.numeric(fe$model[ , 1] - fe$residuals) # reference
yhat1 <- as.numeric(predict(fe)) # fitted values
pred_beta <- as.numeric(tcrossprod(coef(fe), as.matrix(fe$model[ , -1])))
pred_effs <- as.numeric(fixef(fe, "twoways")) # sum of ind and time effects
#change size of x and y labels



plot(data_d2$conflicts, data_d2$sum_disp, asp = 1)

ggplot(data_d2, aes(x=data_d2$conflicts, y=yhat, color=admin1), cex=15.5) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "green") +
  labs(x="True  values", y="Predicted values", title="Predicted vs true values") +
  theme(legend.position="bottom", text = element_text(size = 14)) 



try <- plm(conflicts ~  as.numeric(yhat1), 
              data = data_d2, 
              model = "within", 
              index = c("admin1","time"), 
              effect = "twoways")



# ok they are all the same
#plot(data_d2$conflicts, yhat, asp = 1)
all.equal(yhat1, yhat)

stargazer(fe, type = "latex")

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
