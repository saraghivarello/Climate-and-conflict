*/ *********************************************************************************************
*/ *********************************************************************************************
*/ Do.file replicating results presented in the paper published in AJAE
*/ "Extreme Weather and Civil War: Does Drought Fuel Conflict in Somalia through Livestock Price Shocks?"
*/ Authors: Jean-Francois Maystadt and Olivier Ecker
*/ *********************************************************************************************
*/ *********************************************************************************************

clear

set matsize 8000

*/ *********************************************************************************************
*/ Replication will require the command created by Solomon Hsiang and available at
*/ "http://www.fight-entropy.com/2010/06/standard-error-adjustment-ols-for.html" 
*/ *********************************************************************************************


sysdir set PLUS  "\\lancs\homes\54\maystadt\My Documents\Stata13updates\ado\plus\"


use "AJAE_MaystadtEcker.dta"
xtset new_province_id newident_yrmth

*/ To ease all the estimations, we partial out (removing region and time fiwed effects) all the variables
xi: xtreg tot_violent_mth i.newident_yrmth, fe 
predict res2_tot_violent_mth, e
xi: xtreg TA3_m80 i.newident_yrmth, fe 
predict res2_TA3_m80, e
xi: xtreg DL_TA3_m80 i.newident_yrmth, fe 
predict res2_DL_TA3_m80, e
xi: xtreg tot_conflict_mth i.newident_yrmth, fe 
predict res2_tot_conflict_mth, e
xi: xtreg tot_ucdp_mth i.newident_yrmth, fe 
predict res2_tot_ucdp_mth, e
gen interTDI=TA3_m80*DL_TA3_m80
xi: xtreg interTDI i.newident_yrmth, fe 
predict res2_interTDI, e
xi: xtreg PA3_83 i.newident_yrmth, fe 
predict res2_PA3_83, e
xi: xtreg DL_PA3_83 i.newident_yrmth, fe 
predict res2_DL_PA3_83, e

*/ Different normal conditions
xi: xtreg TA3_m50 i.newident_yrmth, fe 
predict res2_TA3_m50, e
xi: xtreg DL_TA3_m50 i.newident_yrmth, fe 
predict res2_DL_TA3_m50, e
xi: xtreg TA3_a97 i.newident_yrmth, fe 
predict res2_TA3_a97, e
xi: xtreg DL_TA3_a97 i.newident_yrmth, fe 
predict res2_DL_TA3_a97, e

*/ Average instead of maximum
xi: xtreg TA3_a80 i.newident_yrmth, fe 
predict res2_TA3_a80, e
xi: xtreg DL_TA3_a80 i.newident_yrmth, fe 
predict res2_DL_TA3_a80, e

*/ Different time periods
xi: xtreg TA1_m80 i.newident_yrmth, fe 
predict res2_TA1_m80, e
xi: xtreg DL_TA1_m80 i.newident_yrmth, fe 
predict res2_DL_TA1_m80, e
xi: xtreg TA2_m80 i.newident_yrmth, fe 
predict res2_TA2_m80, e
xi: xtreg DL_TA2_m80 i.newident_yrmth, fe 
predict res2_DL_TA2_m80, e
xi: xtreg TA4_m80 i.newident_yrmth, fe 
predict res2_TA4_m80, e
xi: xtreg DL_TA4_m80 i.newident_yrmth, fe 
predict res2_DL_TA4_m80, e
xi: xtreg TA5_m80 i.newident_yrmth, fe 
predict res2_TA5_m80, e
xi: xtreg DL_TA5_m80 i.newident_yrmth, fe 
predict res2_DL_TA5_m80, e
xi: xtreg TA6_m80 i.newident_yrmth, fe 
predict res2_TA6_m80, e
xi: xtreg DL_TA6_m80 i.newident_yrmth, fe 
predict res2_DL_TA6_m80, e

xi: xtreg PA1_83 i.newident_yrmth, fe 
predict res2_PA1_83, e
xi: xtreg DL_PA1_83 i.newident_yrmth, fe 
predict res2_DL_PA1_83, e
xi: xtreg PA2_83 i.newident_yrmth, fe 
predict res2_PA2_83, e
xi: xtreg DL_PA2_83 i.newident_yrmth, fe 
predict res2_DL_PA2_83, e
xi: xtreg PA4_83 i.newident_yrmth, fe 
predict res2_PA4_83, e
xi: xtreg DL_PA4_83 i.newident_yrmth, fe 
predict res2_DL_PA4_83, e
xi: xtreg PA5_83 i.newident_yrmth, fe 
predict res2_PA5_83, e
xi: xtreg DL_PA5_83 i.newident_yrmth, fe 
predict res2_DL_PA5_83, e
xi: xtreg PA6_83 i.newident_yrmth, fe 
predict res2_PA6_83, e
xi: xtreg DL_PA6_83 i.newident_yrmth, fe 
predict res2_DL_PA6_83, e

xi: xtreg PA3_97 i.newident_yrmth, fe 
predict res2_PA3_97, e
xi: xtreg DL_PA3_97 i.newident_yrmth, fe 
predict res2_DL_PA3_97, e

*/ Jointly determined

xi: xtreg DL_TAPA3_m83 i.newident_yrmth, fe 
predict res2_DL_TAPA3_m83, e

*/ Other variables
xi: xtreg lr_rat_price_catex_petro i.newident_yrmth, fe 
predict res2_lr_rat_price_catex_petro, e
*/ Defining a dummy variable for missing value
gen lr_rat_price_catex_missing=0
replace lr_rat_price_catex_missing=1 if lr_rat_price_catex_petro==.
sort new_province_id yr_id
xi: xtreg lr_rat_price_catex_missing i.newident_yrmth, fe 
predict res2_lr_rat_price_catex_missing, e

xtset new_province_id newident_yrmth

*/ Global for region-specific month-fixed effects
global interDum Interdum1_1 Interdum2_1 Interdum3_1 Interdum4_1 Interdum5_1 Interdum6_1 Interdum7_1 Interdum8_1 Interdum9_1 Interdum10_1 Interdum11_1 Interdum12_1 Interdum13_1 Interdum14_1 Interdum15_1 Interdum16_1 Interdum17_1 Interdum18_1 Interdum1_2 Interdum2_2 Interdum3_2 Interdum4_2 Interdum5_2 Interdum6_2 Interdum7_2 Interdum8_2 Interdum9_2 Interdum10_2 Interdum11_2 Interdum12_2 Interdum13_2 Interdum14_2 Interdum15_2 Interdum16_2 Interdum17_2 Interdum18_2 Interdum1_3 Interdum2_3 Interdum3_3 Interdum4_3 Interdum5_3 Interdum6_3 Interdum7_3 Interdum8_3 Interdum9_3 Interdum10_3 Interdum11_3 Interdum12_3 Interdum13_3 Interdum14_3 Interdum15_3 Interdum16_3 Interdum17_3 Interdum18_3 Interdum1_4 Interdum2_4 Interdum3_4 Interdum4_4 Interdum5_4 Interdum6_4 Interdum7_4 Interdum8_4 Interdum9_4 Interdum10_4 Interdum11_4 Interdum12_4 Interdum13_4 Interdum14_4 Interdum15_4 Interdum16_4 Interdum17_4 Interdum18_4 Interdum1_5 Interdum2_5 Interdum3_5 Interdum4_5 Interdum5_5 Interdum6_5 Interdum7_5 Interdum8_5 Interdum9_5 Interdum10_5 Interdum11_5 Interdum12_5 Interdum13_5 Interdum14_5 Interdum15_5 Interdum16_5 Interdum17_5 Interdum18_5 Interdum1_6 Interdum2_6 Interdum3_6 Interdum4_6 Interdum5_6 Interdum6_6 Interdum7_6 Interdum8_6 Interdum9_6 Interdum10_6 Interdum11_6 Interdum12_6 Interdum13_6 Interdum14_6 Interdum15_6 Interdum16_6 Interdum17_6 Interdum18_6 Interdum1_7 Interdum2_7 Interdum3_7 Interdum4_7 Interdum5_7 Interdum6_7 Interdum7_7 Interdum8_7 Interdum9_7 Interdum10_7 Interdum11_7 Interdum12_7 Interdum13_7 Interdum14_7 Interdum15_7 Interdum16_7 Interdum17_7 Interdum18_7 Interdum1_8 Interdum2_8 Interdum3_8 Interdum4_8 Interdum5_8 Interdum6_8 Interdum7_8 Interdum8_8 Interdum9_8 Interdum10_8 Interdum11_8 Interdum12_8 Interdum13_8 Interdum14_8 Interdum15_8 Interdum16_8 Interdum17_8 Interdum18_8 Interdum1_9 Interdum2_9 Interdum3_9 Interdum4_9 Interdum5_9 Interdum6_9 Interdum7_9 Interdum8_9 Interdum9_9 Interdum10_9 Interdum11_9 Interdum12_9 Interdum13_9 Interdum14_9 Interdum15_9 Interdum16_9 Interdum17_9 Interdum18_9 Interdum1_10 Interdum2_10 Interdum3_10 Interdum4_10 Interdum5_10 Interdum6_10 Interdum7_10 Interdum8_10 Interdum9_10 Interdum10_10 Interdum11_10 Interdum12_10 Interdum13_10 Interdum14_10 Interdum15_10 Interdum16_10 Interdum17_10 Interdum18_10 Interdum1_11 Interdum2_11 Interdum3_11 Interdum4_11 Interdum5_11 Interdum6_11 Interdum7_11 Interdum8_11 Interdum9_11 Interdum10_11 Interdum11_11 Interdum12_11 Interdum13_11 Interdum14_11 Interdum15_11 Interdum16_11 Interdum17_11 Interdum18_11 Interdum1_12 Interdum2_12 Interdum3_12 Interdum4_12 Interdum5_12 Interdum6_12 Interdum7_12 Interdum8_12 Interdum9_12 Interdum10_12 Interdum11_12 Interdum12_12 Interdum13_12 Interdum14_12 Interdum15_12 Interdum16_12 Interdum17_12 Interdum18_12

*/ ********************************************************************************
*/ Table 2 : Descriptive statistics
*/ ********************************************************************************
sum tot_violent_mth TA3_m80 PA3_83 DL_TA3_m80 lr_rat_price_catex_petro 

xtfisher tot_violent_mth if yr_id>1996, lag(1)
xtfisher TA3_m80 if yr_id>1996, lag(1)
xtfisher PA3_83 if yr_id>1996, lag(1)
xtfisher DL_TA3_m80 if yr_id>1996, lag(1)
xtfisher lr_rat_price_catex_petro if yr_id>1996, lag(1)

*/ *******************************************************************************
*/ Table 3: Estimation Results of the Preferred Model Specification
*/ ********************************************************************************
*/ Second-stage is runned twice to obtain standard errors
ols_spatial_HAC res2_tot_violent_mth res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehatDL
ols_spatial_HAC res2_tot_violent_mth pricehatDL res2_PA3_83 $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

*/ ********************************************************************************
*/ Table 4 based on results of Tables 2 and 3 
*/ ********************************************************************************

*/ ********************************************************************************
*/ Table 5 based on results of Tables 2, 3, 5, 6, 7, 8 and 9 of the Supplementary Appendix (see below) 
*/ ********************************************************************************

*/ ********************************************************************************
*/ Table 6 based on results of Table 14 of the Supplementary Appendix (see below) 
*/ ********************************************************************************


*/ ********************************************************************************
*/ *******************************************************************************
*/ Supplementary Appendix
*/ ********************************************************************************
*/ *******************************************************************************

*/ ********************************************************************************
*/ Table SA: Table 2. District-level Estimation (see end of do.file)
*/ ********************************************************************************

*/ ********************************************************************************
*/ Table SA: Table 3. Alternative Conflict Data
*/ ********************************************************************************
*/ Second-stage is runned twice to obtain standard errors

*/ ACLED (all conflict types)
drop pricehatDL
ols_spatial_HAC res2_tot_conflict_mth res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehatDL
ols_spatial_HAC res2_tot_conflict_mth pricehatDL res2_PA3_83 $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

*/ UCDP (violent conflicts)
drop pricehatDL
ols_spatial_HAC res2_tot_ucdp_mth res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80  $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehatDL
ols_spatial_HAC res2_tot_ucdp_mth pricehatDL res2_PA3_83 $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)


*/ ********************************************************************************
*/ Table SA:  Table 4. Modifications of Time Period Over Which Anomalies Are Averaged 
*/ *******************************************************************************
*/ Second-stage is runned twice to obtain standard errors

*/ Reduced form
drop pricehatDL

local var "TA1_m80"
foreach v of local var {
ols_spatial_HAC res2_tot_violent_mth res2_`v' res2_DL_`v' res2_PA1_83  $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_PA1_83  res2_`v' res2_DL_`v' $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehat_`v'
ols_spatial_HAC res2_tot_violent_mth pricehat_`v' res2_PA1_83  $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
ols_spatial_HAC res2_tot_violent_mth pricehat_`v' res2_PA1_83  $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
}


local var "TA2_m80"
foreach v of local var {
ols_spatial_HAC res2_tot_violent_mth res2_`v' res2_DL_`v' res2_PA2_83  $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_PA2_83  res2_`v' res2_DL_`v' $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehat_`v'
ols_spatial_HAC res2_tot_violent_mth pricehat_`v' res2_PA2_83  $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
ols_spatial_HAC res2_tot_violent_mth pricehat_`v' res2_PA2_83  $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
}


local var "TA4_m80"
foreach v of local var {
ols_spatial_HAC res2_tot_violent_mth res2_`v' res2_DL_`v' res2_PA4_83  $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_PA4_83  res2_`v' res2_DL_`v' $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehat_`v'
ols_spatial_HAC res2_tot_violent_mth pricehat_`v' res2_PA4_83  $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
ols_spatial_HAC res2_tot_violent_mth pricehat_`v' res2_PA4_83 $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
}

local var "TA5_m80"
foreach v of local var {
ols_spatial_HAC res2_tot_violent_mth res2_`v' res2_DL_`v' res2_PA5_83  $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_PA5_83  res2_`v' res2_DL_`v' $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehat_`v'
ols_spatial_HAC res2_tot_violent_mth pricehat_`v' res2_PA5_83  $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
ols_spatial_HAC res2_tot_violent_mth pricehat_`v' res2_PA5_83  $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
}

local var "TA6_m80"
foreach v of local var {
ols_spatial_HAC res2_tot_violent_mth res2_`v' res2_DL_`v' res2_PA6_83  $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_PA6_83  res2_`v' res2_DL_`v' $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehat_`v'
ols_spatial_HAC res2_tot_violent_mth pricehat_`v' res2_PA6_83  $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
ols_spatial_HAC res2_tot_violent_mth pricehat_`v' res2_PA6_83  $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
}

*/ ********************************************************************************
*/ Table SA:  Table 5. Alternative Temperature Data
*/ *******************************************************************************
*/ Second-stage is runned twice to obtain standard errors


local var "TA3_m50"
foreach v of local var {
ols_spatial_HAC res2_tot_violent_mth res2_`v' res2_DL_`v' res2_PA3_83  $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_PA3_83  res2_`v' res2_DL_`v' $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehat_`v'
ols_spatial_HAC res2_tot_violent_mth pricehat_`v' res2_PA3_83  $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
ols_spatial_HAC res2_tot_violent_mth pricehat_`v' res2_PA3_83  $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
}

local var "TA3_a80"
foreach v of local var {
ols_spatial_HAC res2_tot_violent_mth res2_`v' res2_DL_`v' res2_PA3_83  $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_PA3_83  res2_`v' res2_DL_`v' $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehat_`v'
ols_spatial_HAC res2_tot_violent_mth pricehat_`v' res2_PA3_83  $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
ols_spatial_HAC res2_tot_violent_mth pricehat_`v' res2_PA3_83  $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
}

local var "TA3_a97"
foreach v of local var {
ols_spatial_HAC res2_tot_violent_mth res2_`v' res2_DL_`v' res2_PA3_83 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_PA3_83  res2_`v' res2_DL_`v' $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(3)
predict pricehat_`v'
ols_spatial_HAC res2_tot_violent_mth pricehat_`v' res2_PA3_83  $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(3)
ols_spatial_HAC res2_tot_violent_mth pricehat_`v' res2_PA3_83  $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(3)
}

*/ ********************************************************************************
*/ Table SA:  Table 6. Alternative Definition of Drought Length
*/ *******************************************************************************
*/ Second-stage is runned twice to obtain standard errors

drop pricehat_TA1_m80 pricehat_TA2_m80 pricehat_TA4_m80 pricehat_TA5_m80 pricehat_TA6_m80 pricehat_TA3_m50 pricehat_TA3_a97 pricehat_TA3_a80 touse _est_OLS _est_spatial _est_spatHAC

* preci and temp ano with drought length defined by rainfall
ols_spatial_HAC res2_tot_violent_mth res2_TA3_m80 res2_PA3_83 res2_DL_PA3_83 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_TA3_m80 res2_PA3_83 res2_DL_PA3_83 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricePAhat
ols_spatial_HAC res2_tot_violent_mth pricePAhat res2_PA3_83 $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

* preci and temp ano with drought length defined by both
ols_spatial_HAC res2_tot_violent_mth res2_TA3_m80 res2_PA3_83 res2_DL_TAPA3_m83 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_TA3_m80 res2_PA3_83 res2_DL_TAPA3_m83 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehatDL
ols_spatial_HAC res2_tot_violent_mth pricehatDL res2_PA3_83 $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)


*/ ********************************************************************************
*/ Table SA:  Table 7. Alternative Livestock Price Data in the Two-Stage Regression
*/ *******************************************************************************
*/ Second-stage is runned twice to obtain standard errors
*/ Partial out (time-demeaned) alternative price
xi: xtreg lr_rat_price_catlo_petro i.newident_yrmth, fe 
predict res2_lr_rat_price_catlo_petro, e
xi: xtreg lr_rat_price_goaex_petro i.newident_yrmth, fe 
predict res2_lr_rat_price_goaex_petro, e
xi: xtreg lr_rat_price_sheex_petro i.newident_yrmth, fe 
predict res2_lr_rat_price_sheex_petro, e
xi: xtreg lr_rat_price_camlo_petro i.newident_yrmth, fe 
predict res2_lr_rat_price_camlo_petro, e

*/ *******************************************************************************
* Cattle price for local quality
ols_spatial_HAC res2_lr_rat_price_catlo_petro  res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict newprice_catlo
ols_spatial_HAC res2_tot_violent_mth newprice_catlo res2_PA3_83 $interDum  if lratio_catlo_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

* Goat price for export quality
ols_spatial_HAC res2_lr_rat_price_goaex_petro res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict newprice_goaex
ols_spatial_HAC res2_tot_violent_mth newprice_goaex res2_PA3_83 $interDum if lratio_goaex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

* Sheep price for export quality
ols_spatial_HAC res2_lr_rat_price_sheex_petro res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict newprice_sheex
ols_spatial_HAC res2_tot_violent_mth newprice_sheex res2_PA3_83 $interDum if lratio_sheex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

* Camel price for local quality
ols_spatial_HAC res2_lr_rat_price_camlo_petro  res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80  $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict newprice_camlo
ols_spatial_HAC res2_tot_violent_mth newprice_camlo res2_PA3_83 $interDum  if lratio_camlo_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

*/ ********************************************************************************
*/ Table SA:  Table 8. Alternative Normalizations of Cattle Price in Two-Stage Regression
*/ *******************************************************************************
*/ Second-stage is runned twice to obtain standard errors
*/ Computing the time-demeaned alternative price
sum  lr_rat_price_catex_sugar lr_rat_price_catex_rrice
xi: xtreg  lr_rat_price_catex_sugar i.newident_yrmth, fe 
predict res2_lr_rat_price_catex_sugar, e
xi: xtreg  lr_rat_price_catex_rrice i.newident_yrmth, fe 
predict res2_lr_rat_price_catex_rrice, e

* Normalizing with Sugar price
ols_spatial_HAC res2_lr_rat_price_catex_sugar res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict newprice_sugar
ols_spatial_HAC res2_tot_violent_mth newprice_sugar res2_PA3_83 $interDum if lratio_sugar_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

* Normalizing with Red Rice price
ols_spatial_HAC res2_lr_rat_price_catex_rrice res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict newprice_rice
ols_spatial_HAC res2_tot_violent_mth newprice_rice res2_PA3_83 $interDum if lratio_rrice_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

*/ ********************************************************************************
*/ Table SA:  Table 9. Alternative Function Form Controlling for Seasonamlity Effects
*/ *******************************************************************************
*/ Second-stage is runned twice to obtain standard errors
gen TA3_m80_rainy=TA3_m80*rainy
gen DL_TA3_m80_rainy=DL_TA3_m80*rainy
xi: xtreg TA3_m80_rainy i.newident_yrmth, fe 
predict res2_TA3_m80_rainy, e
xi: xtreg DL_TA3_m80_rainy i.newident_yrmth, fe 
predict res2_DL_TA3_m80_rainy, e

ols_spatial_HAC res2_tot_violent_mth res2_TA3_m80 res2_TA3_m80_rainy res2_PA3_83 res2_DL_TA3_m80 res2_DL_TA3_m80_rainy $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_TA3_m80 res2_TA3_m80_rainy res2_PA3_83 res2_DL_TA3_m80 res2_DL_TA3_m80_rainy  $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehatDLrain
ols_spatial_HAC res2_tot_violent_mth pricehatDLrain res2_PA3_83 $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)


*/ ********************************************************************************
*/ Table SA:  Table 10. Alternative Function Form Controlling for Time Lags
*/ *******************************************************************************
*/ Second-stage is runned twice to obtain standard errors
xtset new_province_id newident_yrmth

gen lag1_res2_TA3_m80=l.res2_TA3_m80
gen lag2_res2_TA3_m80=ll.res2_TA3_m80
gen lag3_res2_TA3_m80=lll.res2_TA3_m80

gen lag1_res2_DL_TA3_m80=l.res2_DL_TA3_m80
gen lag2_res2_DL_TA3_m80=ll.res2_DL_TA3_m80
gen lag3_res2_DL_TA3_m80=lll.res2_DL_TA3_m80

drop pricehatDL 

* including One-Month lag
ols_spatial_HAC res2_tot_violent_mth res2_TA3_m80 lag1_res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 lag1_res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_TA3_m80 lag1_res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 lag1_res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehatlag1DL
ols_spatial_HAC res2_tot_violent_mth pricehatlag1DL res2_PA3_83 $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

* including Two-Month lag
ols_spatial_HAC res2_tot_violent_mth res2_TA3_m80 lag1_res2_TA3_m80 lag2_res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 lag1_res2_DL_TA3_m80 lag2_res2_DL_TA3_m80  $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_TA3_m80 lag1_res2_TA3_m80  lag2_res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 lag1_res2_DL_TA3_m80  lag2_res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehatlag2DL
ols_spatial_HAC res2_tot_violent_mth pricehatlag2DL res2_PA3_83 $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

* including Three-Month lag
ols_spatial_HAC res2_tot_violent_mth res2_TA3_m80 lag1_res2_TA3_m80 lag2_res2_TA3_m80 lag3_res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 lag1_res2_DL_TA3_m80  lag2_res2_DL_TA3_m80 lag3_res2_DL_TA3_m80  $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_TA3_m80 lag1_res2_TA3_m80  lag2_res2_TA3_m80 lag3_res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 lag1_res2_DL_TA3_m80  lag2_res2_DL_TA3_m80 lag3_res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehatlag3DL
ols_spatial_HAC res2_tot_violent_mth pricehatlag3DL res2_PA3_83 $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

*/ ********************************************************************************
*/ Table SA:  Table 11. Alternative Function Form Controlling for Spatial Spillover Effects
*/ *******************************************************************************
*/ Second-stage is runned twice to obtain standard errors
xtset new_province_id newident_yrmth

xi: xtreg w2x_TA3_m80 i.newident_yrmth, fe 
predict res2_w2x_TA3_m80, e
xi: xtreg w2x_DL_TA3_m80 i.newident_yrmth, fe 
predict res2_w2x_DL_TA3_m80, e

ols_spatial_HAC res2_tot_violent_mth res2_TA3_m80 w2x_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 w2x_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_TA3_m80 w2x_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 w2x_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehatw2xDL
ols_spatial_HAC res2_tot_violent_mth pricehatw2xDL res2_PA3_83 $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

*/ ********************************************************************************
*/ Table SA:  Table 12. Alternative Functional Forms With modified Drought Definitions
*/ *******************************************************************************
*/ Second-stage is runned twice to obtain standard errors
xtset new_province_id newident_yrmth

drop pricehatDL
* No drought length
ols_spatial_HAC res2_tot_violent_mth res2_TA3_m80 res2_PA3_83  $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_TA3_m80 res2_PA3_83  $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehatDL
ols_spatial_HAC res2_tot_violent_mth pricehatDL res2_PA3_83 $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

* No rainfall
drop pricehatDL
ols_spatial_HAC res2_tot_violent_mth res2_TA3_m80 res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_TA3_m80  res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehatDL
ols_spatial_HAC res2_tot_violent_mth pricehatDL  $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

* Interaction
drop pricehatDL
ols_spatial_HAC res2_tot_violent_mth res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 res2_interTDI $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 res2_interTDI $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehatDL
ols_spatial_HAC res2_tot_violent_mth pricehatDL res2_PA3_83 $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

*/ ********************************************************************************
*/ Table SA:  Table 13. Alternative definition of drought
*/ *******************************************************************************
*/ Second-stage is runned twice to obtain standard errors
xtset new_province_id newident_yrmth

gen TA3_m80_sq=TA3_m80^2
gen DL_TA3_m80_sq=DL_TA3_m80^2

xi: xtreg TA3_m80_sq i.newident_yrmth, fe 
predict res2_TA3_m80_sq, e
xi: xtreg DL_TA3_m80_sq i.newident_yrmth, fe 
predict res2_DL_TA3_m80_sq, e

drop pricehatDL

ols_spatial_HAC res2_tot_violent_mth res2_TA3_m80 res2_TA3_m80_sq res2_PA3_83 res2_DL_TA3_m80 res2_DL_TA3_m80_sq $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_TA3_m80 res2_TA3_m80_sq res2_PA3_83 res2_DL_TA3_m80 res2_DL_TA3_m80_sq  $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehatDLsq
ols_spatial_HAC res2_tot_violent_mth pricehatDLsq res2_PA3_83 $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

*/ ********************************************************************************
*/ Table SA:  Table 14. Estimation Results of the Preferred Model Specifications Based on Livelihood System Subsamples
*/ *******************************************************************************
*/ Second-stage is runned twice to obtain standard errors
xtset new_province_id newident_yrmth

global nobanadirbis_inter Interdum1_* Interdum2_* Interdum3_* Interdum4_* Interdum5_* Interdum6_* Interdum7_* Interdum8_* Interdum10_* Interdum11_* Interdum12_* Interdum13_* Interdum14_* Interdum15_* Interdum16_* Interdum17_* Interdum18_* 

* No Banadir
ols_spatial_HAC res2_tot_violent_mth res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $nobanadirbis_inter if new_province_id!=9, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $nobanadirbis_inter  if new_province_id!=9, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehatnoban
ols_spatial_HAC res2_tot_violent_mth pricehatnoban res2_PA3_83  $nobanadirbis_inter if lr_rat_price_catex_missing==0   & new_province_id!=9, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

* Pastoralism
global med_pasto_inter Interdum12_* Interdum17_* Interdum2_* Interdum3_* Interdum4_* Interdum5_* Interdum6_* Interdum11_* Interdum18_*
ols_spatial_HAC res2_tot_violent_mth res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $med_pasto_inter if med_pastoralist==1, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $med_pasto_inter if med_pastoralist==1, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehatmedpasto
ols_spatial_HAC res2_tot_violent_mth pricehatmedpasto res2_PA3_83 $med_pasto_inter if lr_rat_price_catex_missing==0  & med_pastoralist==1, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

* Agro-pastoralism
global med_agro_inter Interdum1_* Interdum11_* Interdum12_* Interdum8_* Interdum10_* Interdum13_* Interdum15_* Interdum14_* Interdum16_*
ols_spatial_HAC res2_tot_violent_mth res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $med_agro_inter if med_agro==1, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro  res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $med_agro_inter if med_agro==1, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehatmedagro
ols_spatial_HAC res2_tot_violent_mth pricehatmedagro  res2_PA3_83 $med_agro_inter if lr_rat_price_catex_missing==0  & med_agro==1, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

*Agriculture
global farming_inter Interdum13_* Interdum15_* Interdum17_* Interdum12_* Interdum16_* Interdum14_*
ols_spatial_HAC res2_tot_violent_mth res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $farming_inter if farming==1, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $farming_inter if farming==1, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehatfarm
ols_spatial_HAC res2_tot_violent_mth pricehatfarm  res2_PA3_83 $farming_inter if farming==1 & lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

*No Agriculture
global noagri_inter  Interdum1_* Interdum2_* Interdum3_* Interdum4_* Interdum5_* Interdum6_* Interdum7_* Interdum8_*  Interdum10_*  Interdum11_*  Interdum18_* 
ols_spatial_HAC res2_tot_violent_mth res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $noagri_inter if farming==0 & new_province_id!=9, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $noagri_inter if farming==0 & new_province_id!=9, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict pricehatnofarm
ols_spatial_HAC res2_tot_violent_mth pricehatnofarm  res2_PA3_83 $noagri_inter if farming==0 & lr_rat_price_catex_missing==0 & new_province_id!=9, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

*/ ********************************************************************************
*/ Table SA:  Table 15. Estimation Results of the Two-Stage Regression Controlling for a Potential Labor Income Channel 
*/ *******************************************************************************
*/ Second-stage is runned twice to obtain standard errors
xi: xtreg lwage i.newident_yrmth, fe 
predict res2_lwage, e
gen res2lag_lwage=l.res2_lwage
ols_spatial_HAC res2_lr_rat_price_catex_petro res2lag_lwage res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80  $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict newprice_wage_catex
ols_spatial_HAC res2_tot_violent_mth newprice_wage_catex res2lag_lwage res2_PA3_83  $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)


*/ ********************************************************************************
*/ Table SA:  Table 16. Estimation Results of the Two-Stage Regression Controlling for Potential Food Consumption Channels 
*/ *******************************************************************************
*/ Second-stage is runned twice to obtain standard errors

xi: xtreg lprice_rice i.newident_yrmth, fe 
predict res2_lprice_rice, e
xi: xtreg lprice_sorghum i.newident_yrmth, fe 
predict res2_lprice_sorghum, e
xi: xtreg lprice_whmaize i.newident_yrmth, fe 
predict res2_lprice_whmaize, e
gen res2lag_lprice_whmaize=l.res2_lprice_whmaize
gen res2lag_lprice_sorghum=l.res2_lprice_sorghum
gen res2lag_lprice_rice=l.res2_lprice_rice  

global maize_inter Interdum1_*  Interdum2_* Interdum3_*  Interdum5_* Interdum6_* Interdum7_* Interdum8_* Interdum9_* Interdum10_* Interdum11_*  Interdum12_* Interdum13_* Interdum14_* Interdum15_* Interdum16_*    Interdum17_*   Interdum18_*

*Maize Price
ols_spatial_HAC res2_lr_rat_price_catex_petro res2lag_lprice_whmaize res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80  $maize_inter, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict newprice_maize
ols_spatial_HAC res2_tot_violent_mth newprice_maize res2lag_lprice_whmaize res2_PA3_83  $maize_inter if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

*Sorghum Price
global sorghum_inter Interdum1_*  Interdum2_* Interdum3_*  Interdum5_* Interdum6_* Interdum7_* Interdum8_* Interdum9_* Interdum10_* Interdum11_*  Interdum12_* Interdum13_* Interdum14_* Interdum15_* Interdum16_*    Interdum17_*   Interdum18_*
ols_spatial_HAC res2_lr_rat_price_catex_petro res2lag_lprice_sorghum res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80  $sorghum_inter, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict newprice_sorghum
ols_spatial_HAC res2_tot_violent_mth newprice_sorghum res2lag_lprice_sorghum res2_PA3_83  $sorghum_inter if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)

* Rice Price
ols_spatial_HAC res2_lr_rat_price_catex_petro res2lag_lprice_rice res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80  $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)
predict newprice_lagrice
ols_spatial_HAC res2_tot_violent_mth newprice_lagrice res2lag_lprice_rice res2_PA3_83  $interDum if lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id)  dist(263) lag(4)


*/ ********************************************************************************
*/ Table SA:  Table 17. Estimation Results of the Relationship Between weather Variables and Wages and Staple Food prices
*/ *******************************************************************************
*Causal Labor rate (log)
ols_spatial_HAC res2_lwage res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
*Maize Price (log)
ols_spatial_HAC res2_lprice_whmaize res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
*Red Sorghum Price (log)
ols_spatial_HAC res2_lprice_sorghum res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)
*Red Rice price (log)
ols_spatial_HAC res2_lprice_rice res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $interDum, lat(x_coord) lon(y_coord) t(ident_yrmth) p(new_province_id) dist(263) lag(4)


*/ *******************************************************************************
*/ Table SA- Table 2: See below district regression
*/ ********************************************************************************
use "C:\Users\n12072\Dropbox\AJAE_Revision\Drafting of paper and reply\For resubmission\Accepted\AJAE_MaystadtEcker_district.dta", clear

xtset id_2 newident_yrmth

xi: xtreg tot_violent_mth i.newident_yrmth, fe 
predict res2_tot_violent_mth, e
xi: xtreg TA3_m80 i.newident_yrmth, fe 
predict res2_TA3_m80, e
xi: xtreg DL_TA3_m80 i.newident_yrmth, fe 
predict res2_DL_TA3_m80, e
xi: xtreg PA3_83 i.newident_yrmth, fe 
predict res2_PA3_83, e
xi: xtreg lr_rat_price_catex_petro i.newident_yrmth, fe 
predict res2_lr_rat_price_catex_petro, e
gen res2_lr_rat_price_catex_missing=0
replace res2_lr_rat_price_catex_missing=1 if res2_lr_rat_price_catex_petro==.

*/ Need to exlude from regression Interdum11* Interdum49*, because missing values
global interReg Interreg1_1 Interreg2_1 Interreg3_1 Interreg4_1 Interreg5_1 Interreg6_1 Interreg7_1 Interreg8_1 Interreg9_1 Interreg10_1 Interreg11_1 Interreg12_1 Interreg13_1 Interreg14_1 Interreg15_1 Interreg16_1 Interreg17_1 Interreg18_1 Interreg1_2 Interreg2_2 Interreg3_2 Interreg4_2 Interreg5_2 Interreg6_2 Interreg7_2 Interreg8_2 Interreg9_2 Interreg10_2 Interreg11_2 Interreg12_2 Interreg13_2 Interreg14_2 Interreg15_2 Interreg16_2 Interreg17_2 Interreg18_2 Interreg1_3 Interreg2_3 Interreg3_3 Interreg4_3 Interreg5_3 Interreg6_3 Interreg7_3 Interreg8_3 Interreg9_3 Interreg10_3 Interreg11_3 Interreg12_3 Interreg13_3 Interreg14_3 Interreg15_3 Interreg16_3 Interreg17_3 Interreg18_3 Interreg1_4 Interreg2_4 Interreg3_4 Interreg4_4 Interreg5_4 Interreg6_4 Interreg7_4 Interreg8_4 Interreg9_4 Interreg10_4 Interreg11_4 Interreg12_4 Interreg13_4 Interreg14_4 Interreg15_4 Interreg16_4 Interreg17_4 Interreg18_4 Interreg1_5 Interreg2_5 Interreg3_5 Interreg4_5 Interreg5_5 Interreg6_5 Interreg7_5 Interreg8_5 Interreg9_5 Interreg10_5 Interreg11_5 Interreg12_5 Interreg13_5 Interreg14_5 Interreg15_5 Interreg16_5 Interreg17_5 Interreg18_5 Interreg1_6 Interreg2_6 Interreg3_6 Interreg4_6 Interreg5_6 Interreg6_6 Interreg7_6 Interreg8_6 Interreg9_6 Interreg10_6 Interreg11_6 Interreg12_6 Interreg13_6 Interreg14_6 Interreg15_6 Interreg16_6 Interreg17_6 Interreg18_6 Interreg1_7 Interreg2_7 Interreg3_7 Interreg4_7 Interreg5_7 Interreg6_7 Interreg7_7 Interreg8_7 Interreg9_7 Interreg10_7 Interreg11_7 Interreg12_7 Interreg13_7 Interreg14_7 Interreg15_7 Interreg16_7 Interreg17_7 Interreg18_7 Interreg1_8 Interreg2_8 Interreg3_8 Interreg4_8 Interreg5_8 Interreg6_8 Interreg7_8 Interreg8_8 Interreg9_8 Interreg10_8 Interreg11_8 Interreg12_8 Interreg13_8 Interreg14_8 Interreg15_8 Interreg16_8 Interreg17_8 Interreg18_8 Interreg1_9 Interreg2_9 Interreg3_9 Interreg4_9 Interreg5_9 Interreg6_9 Interreg7_9 Interreg8_9 Interreg9_9 Interreg10_9 Interreg11_9 Interreg12_9 Interreg13_9 Interreg14_9 Interreg15_9 Interreg16_9 Interreg17_9 Interreg18_9 Interreg1_10 Interreg2_10 Interreg3_10 Interreg4_10 Interreg5_10 Interreg6_10 Interreg7_10 Interreg8_10 Interreg9_10 Interreg10_10 Interreg11_10 Interreg12_10 Interreg13_10 Interreg14_10 Interreg15_10 Interreg16_10 Interreg17_10 Interreg18_10 Interreg1_11 Interreg2_11 Interreg3_11 Interreg4_11 Interreg5_11 Interreg6_11 Interreg7_11 Interreg8_11 Interreg9_11 Interreg10_11 Interreg11_11 Interreg12_11 Interreg13_11 Interreg14_11 Interreg15_11 Interreg16_11 Interreg17_11 Interreg18_11 Interreg1_12 Interreg2_12 Interreg3_12 Interreg4_12 Interreg5_12 Interreg6_12 Interreg7_12 Interreg8_12 Interreg9_12 Interreg10_12 Interreg11_12 Interreg12_12 Interreg13_12 Interreg14_12 Interreg15_12 Interreg16_12 Interreg17_12 Interreg18_12

*/ ******************************************************************************************************************************************

ols_spatial_HAC res2_tot_violent_mth res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $interReg, lat(x_coord) lon(y_coord) t(newident_yrmth) p(id_2) dist(123) lag(4)
ols_spatial_HAC res2_lr_rat_price_catex_petro res2_TA3_m80 res2_PA3_83 res2_DL_TA3_m80 $interReg, lat(x_coord) lon(y_coord) t(newident_yrmth) p(id_2)  dist(123) lag(4)
predict pricehatdisDL
ols_spatial_HAC res2_tot_violent_mth pricehatdisDL res2_PA3_83 $interReg if res2_lr_rat_price_catex_missing==0, lat(x_coord) lon(y_coord) t(newident_yrmth) p(id_2)  dist(123) lag(4)



