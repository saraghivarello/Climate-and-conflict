library(pspatreg)
library(plm)
library(ggplot2)
library(dplyr)
library(rgdal)
library(car)

data1 <- read.csv("/home/sara/big_df.csv")

#drop rows with nan
#data1 <- data1[complete.cases(data1), ]

data1 <- data1[data1$date >= "2020-04-05", ]
data1 <- data1[data1$date <= "2020-09-19", ]

#f_jacopo_fede <- external_mobility_total_diff ~ SI+anger_continuous+anticipation_continuous+disgust_continuous+fear_continuous+joy_continuous+sadness_continuous+surprise_continuous+trust_continuous+distress+count_+distress_continuous+norm_anger_continuous+norm_anticipation_continuous+norm_disgust_continuous+norm_fear_continuous+norm_joy_continuous+anxiety+depression+daily_deaths+daily_cases
#formlin <- internal_mobility_diff ~ SI+sadness_continuous+trust_continuous+count_+norm_joy_continuous+norm_sadness_continuous+anxiety+depression
#f_jacopo <- external_mobility_total_diff ~ SI+sadness_continuous+trust_continuous+count_+norm_joy_continuous+norm_sadness_continuous+daily_deaths+daily_cases
#f_jacopo1 <- external_mobility_total_diff ~ SI+anger_continuous+anticipation_continuous+disgust_continuous+fear_continuous+joy_continuous+sadness_continuous+surprise_continuous+trust_continuous+distress+count_+distress_continuous+norm_anger_continuous+norm_anticipation_continuous+norm_disgust_continuous+norm_fear_continuous+norm_joy_continuous+daily_deaths+daily_cases

no_f_eff <- depression ~ date+ active_mobility + SI+tot_votes_gop_dem+rate_votes_gop+rate_votes_dem+governor_in_2020+FIPS+State+Life_Expectancy+Perc_Frequent_Mental_Distress+Median_Household_Income+Population+Perc_less_than_18_years_of_age+Perc_65_and_over+Average_Reading_Grade_Performance+Average_Math_Grade_Performance+Perc_Rural+daily_deaths+daily_cases
#formlin <- norm_distress_continuous ~ external_mobility_total_diff +internal_mobility_diff + SI + +count_ #+anxiety+depression
#formlin <- external_mobility_total_diff ~ SI+fear+anxiety+depression
depr_surv <- depression ~ SI + active_mobility + daily_cases + daily_deaths
anx_surv <- anxiety ~ SI + active_mobility + daily_cases + daily_deaths
dist_tweet <- distress_continuous ~ SI + active_mobility + daily_cases + daily_deaths

mob_depr_surv <- active_mobility ~ SI + depression + daily_cases + daily_deaths
mob_anx_surv <- active_mobility ~ SI + anxiety  + daily_cases + daily_deaths
mob_dist_tweet <- active_mobility ~ SI + distress_continuous + daily_cases + daily_deaths


reg <- lm(no_f_eff, data = data1)

fe <- plm(mob_dist_tweet, 
              data = data1, 
              model = "within", 
              index = c("stusab","date"), 
              effect = "twoways")

summary(fe)