encode state, gen(state1)
xtset state1 year, yearly

 *EXPLORING PANEL DATASET
xtline Vp, overlay
xtline G, overlay
xtline P, overlay
xtline Z, overlay

drop if year==1976


*label variables
label variable state "State Code"
lab var year "Election year"
lab var Vp "Dem share of presidential vote"
lab var I "1 [-1] if Dem [Rep] presidential incumbent"
lab var DPER "1 [-1] {0} Dem [Rep] presidential incumbent {not} running again"
lab var DUR "Duration party in charge"
lab var G "per capita real GDP growth rate Q1-Q3 election year"
lab var P "Inflation rate (first 15Q of admin)"
lab var Z"No. quarters GDP growt>3.2% during (first 15Q of admin)"
label variable state1 "State"


*EXPLORING FIXED EFFECTS: HETEROGENEITY ACROSS STATES
bysort state1: egen Vp_mean=mean(Vp)
twoway scatter Vp state1, msymbol(circle_hollow) || connected Vp_mean state1, msymbol(diamond)
*MULTIPLE REGRESSION
sort state1 year
generate g_i=G*I
generate p_i=P*I
generate z_i=Z*I
label var g_i "Original Equation G"
label var p_i "Original Equation P"
label var z_i "Original Equation Z"
regress Vp DPER DUR I g_i p_i z_i
estimates store ols
predict vp_ols
label variable vp_ols "Fitted Values OLS Regression"


*FIXED EFFECTS USING LEAST SQUARE DUMMY VARIABLE MODEL (LSDV)
xi: regress Vp DPER DUR I g_i p_i z_i i.state1
estimates store ols_fe
estimates table ols ols_fe, star stats(N)
predict vp_fe
label variable vp_fe "Fitted Values of vp using FE model"
rvfplot, yline(0)
predict residuals_fe, residuals
histogram residuals_fe, kdensity normal
separate Vp, by(state1)
separate vp_fe, by(state1)
 *plotting fixed effects multiple regression
twoway scatter Vp vp_fe, mlabel(state1) || lfitci Vp vp_fe, clstyle(p2) 

*CHECK JOINT SIGNFICANCE OF ECONOMIC VARIABLES WITH F-TESTS
correlate DPER DUR I g_i p_i z_i
*UNRESTRICTED REGRESSION
reg Vp DPER DUR I g_i p_i z_i i.state1
*RESTRICTED REGRESSION 
reg Vp DPER DUR I i.state1

*F-TEST
di (   1.78742533    -  1.48944586 ) / 56
di   1.48944586  / (504)
di .00532106/.00295525
* if it is smaller than model approved
di invFtail(56, 504, 0.05)
* if p value is < 0.5 than model approved
di Ftail(56, 504, 1.8005448)
*poolability test
testparm i.state1
*reject null hypothesis. State-Fixed Effect Model Approved!


*ASSIGN STATE TO PARTY
generate assigned_party = "Republican"
label variable assigned_party "Assigned State based on vphat_fixed"
replace assigned_party = "Democratic" if vp_fe > .5

*ASSIGN ELECTORAL VOTES TO STATES AND PREDICT ELECTIONS
generate electoral_votes = 9
label variable electoral_votes "Electoral votes allocation based on 2010 census"
replace electoral_votes = 3 if state == "AK"
replace electoral_votes = 3 if state == "ND"
replace electoral_votes = 3 if state == "SD"
replace electoral_votes = 3 if state == "MT"
replace electoral_votes = 3 if state == "DE"
replace electoral_votes = 3 if state == "DC"
replace electoral_votes = 3 if state == "VT"
replace electoral_votes = 3 if state == "WY"
replace electoral_votes = 4 if state == "HI"
replace electoral_votes = 4 if state == "ID"
replace electoral_votes = 4 if state == "ME"
replace electoral_votes = 4 if state == "NH"
replace electoral_votes = 4 if state == "RI"
replace electoral_votes = 5 if state == "NE"
replace electoral_votes = 5 if state == "NM"
replace electoral_votes = 5 if state == "WV"
replace electoral_votes = 6 if state == "AR"
replace electoral_votes = 6 if state == "IA"
replace electoral_votes = 6 if state == "KS"
replace electoral_votes = 6 if state == "MS"
replace electoral_votes = 6 if state == "NV"
replace electoral_votes = 6 if state == "UT"
replace electoral_votes = 7 if state == "CT"
replace electoral_votes = 7 if state == "OK"
replace electoral_votes = 7 if state == "OR"
replace electoral_votes = 8 if state == "KY"
replace electoral_votes = 8 if state == "LA"
replace electoral_votes = 10 if state == "MD"
replace electoral_votes = 10 if state == "MN"
replace electoral_votes = 10 if state == "MO"
replace electoral_votes = 10 if state == "WI"
replace electoral_votes = 11 if state == "AZ"
replace electoral_votes = 11 if state == "IN"
replace electoral_votes = 11 if state == "MA"
replace electoral_votes = 11 if state == "TN"
replace electoral_votes = 12 if state == "WA"
replace electoral_votes = 13 if state == "VA"
replace electoral_votes = 14 if state == "NJ"
replace electoral_votes = 15 if state == "NC"
replace electoral_votes = 16 if state == "GA"
replace electoral_votes = 16 if state == "MI"
replace electoral_votes = 18 if state == "OH"
replace electoral_votes = 20 if state == "IL"
replace electoral_votes = 20 if state == "PA"
replace electoral_votes = 29 if state == "FL"
replace electoral_votes = 29 if state == "NY"
replace electoral_votes = 38 if state == "TX"
replace electoral_votes = 55 if state == "CA"
generate total_electoral_votes = electoral_votes if assigned_party == "Democratic"
label variable total_electoral_votes "Electoral Votes for Democratic by year"
separate  total_electoral_votes, by(year)
*use function total () to compute the total electoral_votes Democratic gained in that year
*if the total is greater than 270 the party won, otherwise lost

*DATASET WITH ADDITIONAL VARIABLES
label variable UR  "Unemployment Growth Rate"
label variable PCT "Percentage of personal current taxes"
label variable SIT "States income taxes"
label variable PT "Property taxes"
label variable COVID "year with Covid"
generate p_i2 = p_i^2
generate UR2 = UR^2
generate PCT2 = PCT^2
generate SIT2=SIT^2
generate PT2=PT^2
label variable p_i2 "Squared p_i"
label variable UR2 "Squared UR"
label variable PCT2 "Squared PCT"
label variable SIT2 "Squared SIT"
label variable PT "Squared PT"
global X_original p_i z_i DPER DUR I
global X_additional UR PCT SIT PT  p_i2 UR2 PCT2 SIT2 PT2 COVID
global X_effects _Istate1_2-_Istate1_51

*USE LASSO POST-SELECTION TO SELECT MEANINGFUL VARIABLES
*exclusion of g_i and fixed effects from lasso penalty using adaptive selection
lasso linear Vp (g_i $X_effects) $X_original $X_additional, selection(adaptive)
ereturn list
lassocoef
*regress y on variables selected by the lasso
reg `e(post_sel_vars)' 
predict vp_ps
predict residuals_ps, residuals
histogram residuals_ps ,kdensity normal
estimates store lasso_ps

*DOUBLE SELECTION LASSO
lasso linear Vp ($X_effects) $X_original $X_additional, selection(adaptive)
local Xy = "`e(allvars_sel)'"
lasso linear g_i ($X_effects) $X_original $X_additional, selection(adaptive)
local Xd = "`e(allvars_sel)'"
reg Vp g_i `Xy' `Xd'
predict vp_ds
predict residuals_ds, residuals
histogram residuals_ds, kdensity normal
estimates store lasso_ds

*MODEL COMPARISON
ssc install estout, replace
estout ols ols_fe lasso_ps lasso_ds
lassogof ols ols_fe lasso_ps lasso_ds


