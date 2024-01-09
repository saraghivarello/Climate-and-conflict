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

data_ban_2 <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/dd_ban_2.csv")
data_conc <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/df_conc.csv")
data_ban_all <- read.csv("/home/sara/Documenti/GitHub/Climate-and-conflict/dd_ban_all.csv")


#data1 <- data1[data1$time <= 2022, ]
#data1 <- data1[data1$time >= 2021, ]

form_ban_2 <- Banadir ~ x + inv_distance #+ conflicts + population_density
form_ban_d_2 <- Banadir ~ TA_lag2 + inv_distance_2  #+ population_density
form_all <- Displacements ~ x + TA_lag2 + inv_distance + conflicts #+ population_density_destination#+ PA_lag3 + DL_lag3 + inv_distance


#banadir in 2022
f_ind_b <- plm(form_ban_2, data = data_ban_2, model = "within", index = c("admin1","month"), effect = "individual")
f_time_b <- plm(form_ban_2, data = data_ban_2, model = "within", index = c("admin1","month"), effect = "time")
f_two_b <- plm(form_ban_2, data = data_ban_2, model = "within", index = c("admin1","month"), effect = "twoways")

set.seed(42L)

new.x2   <- runif(16, min = min(data_ban_2$x),   max = max(data_ban_2$x))
new.inv_dist2 <- runif(16, min = min(data_ban_2$inv_distance), max = max(data_ban_2$inv_distance))

newdata <- data.frame(admin1 = c(rep('Awdal', 5), rep('Banadir', 7), rep('Bakool', 4)),
                      month = c(1:(1+4), 1:(1+6), 1:(1+3)),
                      x = new.x2, inv_distance = new.inv_dist2)
# make pdata.frame
newdata.p <- pdata.frame(newdata, index = c("admin1", "month"))

## predict from fixed effect model with new data as pdata.frame
predict(f_time_b, newdata = newdata.p) # has NA values for the 11'th firm

## set na.fill = TRUE to have the weighted mean used to for fixed effects -> no NA values
predict(f_time_b, newdata = newdata.p, na.fill = TRUE)


#predict f_time_b
pred <- predict(f_time_b, newdata = data_ban_2)

#all regions together
f_all_ind <- plm(form_all, data = data_conc, model = "within", index = c("Current..Arrival..Region","month"), effect = "individual")
f_all_time <- plm(form_all, data = data_conc, model = "within", index = c("Current..Arrival..Region","month"), effect = "time")
f_all_two <- plm(form_all, data = data_conc, model = "within", index = c("Current..Arrival..Region","month"), effect = "twoways")

#find correlation between variables
#cor(data_ban_d_2[,c("x","inv_distance")])
#cor.test(data_ban_d_2$x, data_ban_d_2$inv_distance_2)
#create a vector of 204 random numbers
set.seed(40)
random <- runif(204, min = 0, max = 1)
cor.test(random, data_ban_2$Banadir)

data_ban_2$pred <- plm:::predict.plm(f_time_b, data_ban_2)
# From examining the source code, predict.plm does not incorporate 
# the random effects, so you do not get appropriate predictions. 
# You just get the FE predictions.

ggplot(data_ban_2, aes(x=x, y=pred)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
#color according to region
ggplot(data_ban_2, aes(x=x, y=pred, color=admin1)) + geom_point() + geom_smooth(method = "lm", se = FALSE) 
#color according to inv_distance
ggplot(data_ban_2, aes(x=x, y=pred, color=inv_distance)) + geom_point() + geom_smooth(method = "lm", se = FALSE)