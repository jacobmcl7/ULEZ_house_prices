*this do-file does the (postcode-sector-level) near-far analysis

*-------------------------------------------*
*PREPARE THE ANALYSIS
*-------------------------------------------*
*set the cd
cd "C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data"

*make a global macro for the controls, to insert into regressions
global controls "detached semidetached terraced new leasehold"
*-------------------------------------------*

*-------------------------------------------*
*DEFINE PROGRAMS FOR USE IN THE ANALYSIS
*-------------------------------------------*
*define a program to make the event study graphs once a regression has been run
cap prog drop graphprep
program def graphprep
args mod start end base treat
	quietly{
		cap drop `mod'*
		gen `mod' = .
		gen `mod'_up = .
		gen `mod'_lo = .
		gen `mod'_t = .
		local n = 1
		forvalues i = `start'/`end' {
			if `i'!=`base' {
			replace `mod' = _b[et`i'] in `n'
			replace `mod'_up = _b[et`i']+invttail(e(df_r),0.5*(1-c(level)/100))*_se[et`i'] in `n'
			replace `mod'_lo = _b[et`i']-invttail(e(df_r),0.5*(1-c(level)/100))*_se[et`i'] in `n'
			}
			else replace `mod' = 0 in `n'
			replace `mod'_t = `i'-`treat' in `n'
			local n = `n'+1
		}
	}
end



*------------------------------------------*
*OUTLINE
*------------------------------------------*
*we take three different approaches to the problem:
* 1) use coarsened exact matching to refine the groups of treated and control postcode sectors, and then run DiDs on those
* 2) make a synthetic control for each treated unit, and aggregate the post-treatment behaviour of all of the treated units
* 3) use a modified k-means clustering algorithm to group postcode sectors into multiple clusters, and then compare within clusters


*------------------------------------------*
*1) COARSENED EXACT MATCHING
*------------------------------------------*

*we do this with only a few and then many controls, and in each case, separately for 2021 and 2023 regions

******** 1a) basic controls


******** 1ai) 2021

*load in the cross-sectional postcode-sector data from control_merging.do
use "Temp\pp_ready_collapsed_xsec.dta", clear

*first generate a unique cross-sectional identifier (i.e. the pseudo-postcode sector)
decode in_ULEZ, gen(in_ULEZ_decoded)
gen pcsect = pcsect_original + " " + in_ULEZ_decoded

*now generate a treatment variable and keep only the relevant pcsects: either ones in the 2021 ULEZ not too close to 2019, or those at least 50km from the outer border
gen treated = in_ULEZ_decoded == "In 2021 ULEZ"
keep if (treated == 1 & pcsect_dist_from_2019_ULEZ > 5) | pcsect_dist_from_2023_ULEZ > 50

*remove all observations with missings (in the controls)
missings tag, gen(tag)
drop if tag > 0
drop tag

*filter the data to remove all pcsects in which class D LEZs or ZEZs have been implemented (Birmingham (B), Bristol (BS) and Oxford (OX))
keep if !(regexm(pcsect, "^BS[0-9]+") | regexm(pcsect, "^B[0-9]+") | regexm(pcsect, "^OX[0-9]+"))

*do the CEM matching (with k2k, to avoid having to use weights)
set seed 12345
cem pcsect_share_flat (#5) pcsect_share_detached (#5) pcsect_share_semi (#5) pcsect_share_new (#5) pcsect_share_leasehold (#5), treatment(treated) showbreaks k2k

*get matched pcsects
keep if cem_matched == 1
keep pcsect


*TO BE REMOVED WHEN DONE PROPERLY (MAYBE): --------
*replace matched pcsects to match with how they are formatted in the pp_ready file
replace pcsect = subinstr(pcsect, " Not in ULEZ", "", .)
replace pcsect = subinstr(pcsect, "In 2021 ULEZ", "2021", .)
save "Temp\matched_pcsects_2021.dta", replace

*now briefly prepare for visualisation
preserve
gen pcsect_original = subinstr(pcsect, " 2021", "", .)
gen tag = 1
*export as an excel file
export excel using "Temp\2021_pcsects_vis.xlsx", firstrow(variables) replace
restore
*--------

*load in the data
use "Temp\pp_ready_all.dta", clear

*work out which postcode sectors have matched: merge in the postcode matched data
merge m:1 pcsect using "Temp\matched_pcsects_2021.dta" 
keep if _merge == 3
drop _merge

*summarise
tab pcsect


*now do a dynamic DiD (like (5) in the do-file before, but at the saleyear level)

preserve

gen cohort = 0
replace cohort = 2021 if in_2021_ULEZ == 1

gen Stime = cohort if cohort>0
gen etime = saleyear-Stime

tab etime, gen(et)
*there are 11 et dummies, with etime = 0 now being at et7

foreach var of varlist et1-et11 {
	replace `var' = 0 if cohort==0
}

*again rebase to two quarters prior to implementation - we see the same sharp drop before this point as in 2023 (giving more support to the claim that it comes from anticipation rather than randomness)
global leads "et1-et5"
global lags "et7-et11"

*regress and prepare the coefficients for graphing
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) vce(cluster pcsect)
graphprep mod1 1 11 6 7

*plot the graphs
twoway (rcap mod1_up mod1_lo mod1_t, lc("216 17 89")) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (years after first treatment, j)") xlabel(-6(1)4) xline(-0.5, lc(black*.80%50) lp(dash)) title("2021 Dynamic DiD") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

*looks good - confirms original findings! If anything, the original result was an underestimate
*need to check control regions - is this a sensible comparison?

restore




******** 1aii) 2023

*reload original data
use "Temp\pp_ready_collapsed_xsec.dta", clear

*pre-process it in the same way
decode in_ULEZ, gen(in_ULEZ_decoded)
gen pcsect = pcsect_original + " " + in_ULEZ_decoded

gen treated = in_ULEZ_decoded == "In 2023 ULEZ"
keep if (treated == 1 & pcsect_dist_from_2021_ULEZ > 5) | pcsect_dist_from_2023_ULEZ > 50

*remove all observations with missings
missings tag, gen(tag)
drop if tag > 0
drop tag

*filter the data to remove all pcsects in which class D LEZs or ZEZs have been implemented (Birmingham (B), Bristol (BS) and Oxford (OX))
keep if !(regexm(pcsect, "^BS[0-9]+") | regexm(pcsect, "^B[0-9]+") | regexm(pcsect, "^OX[0-9]+"))

*do the CEM matching (with k2k, to avoid having to use weights)
set seed 12345
cem pcsect_share_flat (#5) pcsect_share_detached (#5) pcsect_share_semi (#5) pcsect_share_new (#5) pcsect_share_leasehold (#5), treatment(treated) showbreaks k2k

*get matched pcsects
keep if cem_matched == 1
keep pcsect


*TO BE REMOVED WHEN DONE PROPERLY: --------
*replace matched pcsects to match with how they are formatted in the pp_ready file
replace pcsect = subinstr(pcsect, " Not in ULEZ", "", .)
replace pcsect = subinstr(pcsect, "In 2023 ULEZ", "2023", .)
save "Temp\matched_pcsects_2023.dta", replace

*now briefly prepare for visualisation
preserve
gen pcsect_original = subinstr(pcsect, " 2023", "", .)
gen tag = 1
*export as an excel file
export excel using "Temp\2023_pcsects_vis.xlsx", firstrow(variables) replace
restore
*--------

*load in the data
use "Temp\pp_ready_all.dta", clear

*work out which postcode sectors have matched: merge in the postcode matched data
merge m:1 pcsect using "Temp\matched_pcsects_2023.dta" 
keep if _merge == 3
drop _merge

*summarise
tab pcsect


*now do a dynamic DiD (like (5) in the do-file before, but at the saleyear level)

preserve

gen cohort = 0
replace cohort = 2023 if in_2023_ULEZ == 1

gen Stime = cohort if cohort>0
gen etime = saleyear-Stime

tab etime, gen(et)
*there are 11 et dummies, with etime = 0 now being at et7

foreach var of varlist et1-et11 {
	replace `var' = 0 if cohort==0
}

*again rebase to two quarters prior to implementation - we see the same sharp drop before this point as in 2023 (giving more support to the claim that it comes from anticipation rather than randomness)
global leads "et1-et7"
global lags "et9-et11"

*regress and prepare the coefficients for graphing
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) vce(cluster pcsect)
graphprep mod1 1 11 8 9

*plot the graphs
twoway (rcap mod1_up mod1_lo mod1_t, lc("143 45 86"%50)) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-8(1)2) xline(-0.5, lc(black*.80%50) lp(dash)) title("2023 Dynamic DiD") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

*very poor pre-trends

restore





******** 1b) more controls

******** 1bi) 2021

*load in the cross-sectional postcode-sector data from control_merging.do
use "Temp\pp_ready_collapsed_xsec.dta", clear

*first generate a unique cross-sectional identifier (i.e. the pseudo-postcode sector)
decode in_ULEZ, gen(in_ULEZ_decoded)
gen pcsect = pcsect_original + " " + in_ULEZ_decoded

*now generate a treatment variable and keep only the relevant pcsects: either ones in the 2021 ULEZ not too close to 2019, or those at least 50km from the outer border
gen treated = in_ULEZ_decoded == "In 2021 ULEZ"
keep if (treated == 1 & pcsect_dist_from_2019_ULEZ > 5) | pcsect_dist_from_2023_ULEZ > 50

*remove all observations with missings (in the controls)
missings tag, gen(tag)
drop if tag > 0
drop tag

*filter the data to remove all pcsects in which class D LEZs or ZEZs have been implemented (Birmingham (B), Bristol (BS) and Oxford (OX))
keep if !(regexm(pcsect, "^BS[0-9]+") | regexm(pcsect, "^B[0-9]+") | regexm(pcsect, "^OX[0-9]+"))

*do the CEM matching (with k2k, to avoid having to use weights)
*USE PRICE DIFFERENCE? I.E. MATCH ON PRE-TRENDS?
set seed 12345
cem pcsect_share_flat (#5) pcsect_share_detached (#5) pcsect_share_semi (#5) pcsect_share_new (#5) pcsect_share_leasehold (#5) pcsect_pop_density (#5) pcsect_imd (#5) pcdist_share_flat (#5) pcdist_share_detached (#5) pcdist_share_semi (#5) pcdist_share_new (#5) pcdist_share_leasehold (#5) pcdist_pop_density (#5) pcdist_imd (#5), treatment(treated) showbreaks k2k

*get matched pcsects
keep if cem_matched == 1
keep pcsect


*TO BE REMOVED WHEN DONE PROPERLY (MAYBE): --------
*replace matched pcsects to match with how they are formatted in the pp_ready file
replace pcsect = subinstr(pcsect, " Not in ULEZ", "", .)
replace pcsect = subinstr(pcsect, "In 2021 ULEZ", "2021", .)
save "Temp\matched_pcsects_2021.dta", replace

*now briefly prepare for visualisation
preserve
gen pcsect_original = subinstr(pcsect, " 2021", "", .)
gen tag = 1
*export as an excel file
export excel using "Temp\2021_pcsects_vis.xlsx", firstrow(variables) replace
restore
*--------

*load in the data
use "Temp\pp_ready_all.dta", clear

*work out which postcode sectors have matched: merge in the postcode matched data
merge m:1 pcsect using "Temp\matched_pcsects_2021.dta" 
keep if _merge == 3
drop _merge

*summarise
tab pcsect


*now do a dynamic DiD (like (5) in the do-file before, but at the saleyear level)

preserve

gen cohort = 0
replace cohort = 2021 if in_2021_ULEZ == 1

gen Stime = cohort if cohort>0
gen etime = saleyear-Stime

tab etime, gen(et)
*there are 11 et dummies, with etime = 0 now being at et7

foreach var of varlist et1-et11 {
	replace `var' = 0 if cohort==0
}

*again rebase to two quarters prior to implementation - we see the same sharp drop before this point as in 2023 (giving more support to the claim that it comes from anticipation rather than randomness)
global leads "et1-et5"
global lags "et7-et11"

*regress and prepare the coefficients for graphing
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) vce(cluster pcsect)
graphprep mod1 1 11 6 7

*plot the graphs
twoway (rcap mod1_up mod1_lo mod1_t, lc("216 17 89")) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (years after first treatment, j)") xlabel(-6(1)4) xline(-0.5, lc(black*.80%50) lp(dash)) title("2021 Dynamic DiD") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

*not bad - similar to the one before

restore



******** 1bii) 2023

*reload original data
use "Temp\pp_ready_collapsed_xsec.dta", clear

*pre-process it in the same way
decode in_ULEZ, gen(in_ULEZ_decoded)
gen pcsect = pcsect_original + " " + in_ULEZ_decoded

gen treated = in_ULEZ_decoded == "In 2023 ULEZ"
keep if (treated == 1 & pcsect_dist_from_2021_ULEZ > 5) | pcsect_dist_from_2023_ULEZ > 50

*remove all observations with missings
missings tag, gen(tag)
drop if tag > 0
drop tag

*filter the data to remove all pcsects in which class D LEZs or ZEZs have been implemented (Birmingham (B), Bristol (BS) and Oxford (OX))
keep if !(regexm(pcsect, "^BS[0-9]+") | regexm(pcsect, "^B[0-9]+") | regexm(pcsect, "^OX[0-9]+"))

*do the CEM matching (with k2k, to avoid having to use weights)
*USE PRICE DIFFERENCE? I.E. MATCH ON PRE-TRENDS?
set seed 12345
cem pcsect_share_flat (#5) pcsect_share_detached (#5) pcsect_share_semi (#5) pcsect_share_new (#5) pcsect_share_leasehold (#5) pcsect_pop_density (#5) pcsect_imd (#5) pcsect_sale_count (#5) pcsect_price_diff_22_15 (#5) pcsect_price_diff_22_17 (#5) pcsect_price_diff_22_19 (#5) pcsect_price_diff_22_21 (#5), treatment(treated) showbreaks k2k

*get matched pcsects
keep if cem_matched == 1
keep pcsect


*TO BE REMOVED WHEN DONE PROPERLY: --------
*replace matched pcsects to match with how they are formatted in the pp_ready file
replace pcsect = subinstr(pcsect, " Not in ULEZ", "", .)
replace pcsect = subinstr(pcsect, "In 2023 ULEZ", "2023", .)
save "Temp\matched_pcsects_2023.dta", replace

*now briefly prepare for visualisation
preserve
gen pcsect_original = subinstr(pcsect, " 2023", "", .)
gen tag = 1
*export as an excel file
export excel using "Temp\2023_pcsects_vis.xlsx", firstrow(variables) replace
restore
*--------

*load in the data
use "Temp\pp_ready_all.dta", clear

*work out which postcode sectors have matched: merge in the postcode matched data
merge m:1 pcsect using "Temp\matched_pcsects_2023.dta" 
keep if _merge == 3
drop _merge

*summarise
tab pcsect


*now do a dynamic DiD (like (5) in the do-file before, but at the saleyear level)

preserve

gen cohort = 0
replace cohort = 2023 if in_2023_ULEZ == 1

gen Stime = cohort if cohort>0
gen etime = saleyear-Stime

tab etime, gen(et)
*there are 11 et dummies, with etime = 0 now being at et7

foreach var of varlist et1-et11 {
	replace `var' = 0 if cohort==0
}

*again rebase to two quarters prior to implementation - we see the same sharp drop before this point as in 2023 (giving more support to the claim that it comes from anticipation rather than randomness)
global leads "et1-et7"
global lags "et9-et11"

*regress and prepare the coefficients for graphing
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) vce(cluster pcsect)
graphprep mod1 1 11 8 9

*plot the graphs
twoway (rcap mod1_up mod1_lo mod1_t, lc("143 45 86"%50)) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-8(1)2) xline(-0.5, lc(black*.80%50) lp(dash)) title("2023 Dynamic DiD") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

*just as bad as before

restore




*the matching is good for 2021, and supports original conclusions in this case, but is poor for 2023





*NEXT STEPS: 
* - justify the number of groupings chosen in the matching
* - maybe try with weighting (or other forms of matching)
* - maybe write a program that does this to make the code read more nicely
* - address the fact that when we match on price trends, we need similar sizes as well if the matches are to carry through to the house-level DiD
	* - maybe don't match on price trends
* - plan how to use this. Maybe do it in different degrees of controls, and plot them all on the same graph






*---------------------------------------*
*2) SYNTHETIC CONTROL AND AGGREGATION
*---------------------------------------*

*this is all done in R - see this file







*---------------------------------------*
*3) K-MEANS CLUSTERING
*---------------------------------------*

*as before, we do this separately for 2021 and 2023

******** 3a) 2021

*the first part of this is done in R - see this file

*now load in the processed excel data from R, and save as dta (could do this in R)
import excel "Temp\matched_pcsects_2021_kmeans.xlsx", clear firstrow
save "Temp\matched_pcsects_2021_kmeans.dta", replace

*load in the original data for merging
use "Temp\pp_ready_all.dta", clear

*work out which postcode sectors have matched: merge in the postcode matched data, and keep only observations from matched postcode sectors
merge m:1 pcsect using "Temp\matched_pcsects_2021_kmeans.dta" 
keep if _merge == 3
drop _merge


*now try a dynamic DiD (like 5) in the one before)
preserve

gen cohort = 0
replace cohort = 2021 if in_2021_ULEZ == 1

gen Stime = cohort if cohort>0
gen etime = saleyear-Stime

tab etime, gen(et)
*there are 11 et dummies, with etime = 0 now being at et7

foreach var of varlist et1-et11 {
	replace `var' = 0 if cohort==0
}

*again rebase to two quarters prior to implementation - we see the same sharp drop before this point as in 2023 (giving more support to the claim that it comes from anticipation rather than randomness)
global leads "et1-et5"
global lags "et7-et11"

*regress and prepare the coefficients for graphing
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) vce(cluster pcsect)
graphprep mod1 1 11 6 7

*plot the graphs
twoway (rcap mod1_up mod1_lo mod1_t, lc("216 17 89")) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (years after first treatment, j)") xlabel(-6(1)4) xline(-0.5, lc(black*.80%50) lp(dash)) title("2021 Dynamic DiD") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, unit of time = quarter, and 95% CIs shown", span)

*save the graph
graph export "Output\Results\kmeans_dynamic_DiD_2021.pdf", as(pdf) replace

restore




******** 3b) 2023


*now do this for 2023

*the first part of this is done in R - see this file

*now load in the processed excel data from R, and save as dta (could do this in R)
import excel "Temp\matched_pcsects_2023_kmeans.xlsx", clear firstrow
save "Temp\matched_pcsects_2023_kmeans.dta", replace

*load in the original data
use "Temp\pp_ready_all.dta", clear

*work out which postcode sectors have matched: merge in the postcode matched data
merge m:1 pcsect using "Temp\matched_pcsects_2023_kmeans.dta" 
keep if _merge == 3
drop _merge


*now do the dynamic DiD

preserve

gen cohort = 0
replace cohort = 2023 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0

*generate event time variable
gen Stime = cohort if cohort>0
gen etime = saleyear-Stime

*generate the event time dummies
tab etime, gen(et)
*there are 11 et dummies, with etime = 0 being at et9

*ensure the event time dummies are always 0 for the never treated group
foreach var of varlist et1-et11  {
	replace `var' = 0 if cohort==0
}

*define a set of leads and lags
*the graph will show a quarter of what looks like anticipatory behaviour - to deal with this, set the base period to be two quarters before the policy, rather than one
global leads "et1-et7"
global lags "et9-et11"

*run the regressions, and use the graphprep program written at the start to prepare the coefficients for graphing
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) vce(cluster pcsect)
*this corresponds to first dummy = 1, last dummy = 11, baseline dummy = 8, first treatment dummy = 9
graphprep mod1 1 11 8 9

*plot the graph of the coefficients
twoway (rcap mod1_up mod1_lo mod1_t, lc("143 45 86"%50)) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (years after first treatment, j)") xlabel(-8(1)2) xline(-0.5, lc(black*.80%50) lp(dash)) title("2023 Dynamic DiD") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, unit of time = year, and 95% CIs shown", span)

*save the graph
graph export "Output\Results\kmeans_dynamic_DiD_2023.pdf", as(pdf) replace

restore


*NEXT STEPS:
* - do this with different sets of controls
* - try with a different distance metric
* - do with different controls
* - do with different numbers of clusters
* - aggregate over different initial randomisations
* - look for a way to do this to get pre-trends to hold (SDID-type designs, but that can account for repeated cross-sectional data)





*---------------------------------------*
*4) SDID on the refined data from 3
*---------------------------------------*

*we do this separately for 2021 and 2023, and use the Morin (2024) approach to get the weights

**** 4a) 2021

*load in the collapsed pcsect-level panel data
use "Temp\pp_ready_collapsed_panel.dta", clear

*first generate pcsect, the unique cross-sectional identifier (i.e. the pseudo-postcode sector)
decode in_ULEZ, gen(in_ULEZ_decoded)
gen pcsect = pcsect_original
replace pcsect = pcsect + " 2021" if in_ULEZ_decoded == "In 2021 ULEZ"
replace pcsect = pcsect + " 2023" if in_ULEZ_decoded == "In 2023 ULEZ"
replace pcsect = pcsect + " 2019" if in_ULEZ_decoded == "In 2019 ULEZ"

*remove all observations with missings (in the controls), then balance the panel (required)
missings tag, gen(tag)
drop if tag > 0
drop tag

bys pcsect: gen n = _N
drop if n < 11
drop n

*xtset the data - string variables not allowed, so first destring
encode pcsect, gen(pcsect_num)
xtset pcsect_num saleyear

*merge with the postcode sector data to keep only the matched pcsects
merge m:1 pcsect using "Temp\matched_pcsects_2021_kmeans.dta"
keep if _merge == 3
drop _merge

*generate a treatment variable
gen treatment = in_ULEZ_decoded == "In 2021 ULEZ" & saleyear >= 2021

*do SDiD to get the first set of weights, as suggested by Morin (2024)
sdid avg_log_price pcsect_num saleyear treatment, vce(noinference) returnweights

*save the weights as a temporary file to be merged into the larger dataset
keep pcsect saleyear lambda2021 omega2021 
save "Temp\sdid_weights_2021.dta", replace

*now load in the original data
use "Temp\pp_ready_all.dta", clear
*merge in the weights
merge m:1 pcsect saleyear using "Temp\sdid_weights_2021.dta"
*lots are unmatched from the original dataset, but this is because we cut a large region out
keep if _merge == 3
drop _merge

*now get the second set of weights
bys pcsect saleyear: gen morin_weight = 1/_N

*now get the full weights
gen weight = lambda2021 * omega2021 * morin_weight


*now do the weighted DiD

preserve

gen cohort = 0
replace cohort = 2021 if in_2021_ULEZ == 1

gen Stime = cohort if cohort>0
gen etime = saleyear-Stime

tab etime, gen(et)
*there are 11 et dummies, with etime = 0 now being at et7

foreach var of varlist et1-et11 {
	replace `var' = 0 if cohort==0
}

*again rebase to two quarters prior to implementation - we see the same sharp drop before this point as in 2023 (giving more support to the claim that it comes from anticipation rather than randomness)
global leads "et1-et5"
global lags "et7-et11"

*regress and prepare the coefficients for graphing
reghdfe log_price $leads $lags $controls [aweight=weight], absorb(quarter pcsect) vce(cluster pcsect)
graphprep mod1 1 11 6 7

*where is the multicollinearity coming from?? Only happens when we use weights

*plot the graphs
twoway (rcap mod1_up mod1_lo mod1_t, lc("216 17 89")) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-6(1)4) xline(-0.5, lc(black*.80%50) lp(dash)) title("2021 Dynamic DiD") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

restore





**** 4b) 2023

*do this - work out where multicollinearity is coming from first though



















******************************************************************
******************************************************************
******************************************************************
******************************************************************
*ARCHIVE
******************************************************************
******************************************************************
******************************************************************
******************************************************************



*0) SYNTHETIC DID

*first for 2021

*load in the data
use "Temp\pp_ready_collapsed_panel.dta", clear

*first generate a unique cross-sectional identifier (i.e. the pseudo-postcode sector)
decode in_ULEZ, gen(in_ULEZ_decoded)
gen pcsect = pcsect_original
replace pcsect = pcsect + " 2021" if in_ULEZ_decoded == "In 2021 ULEZ"
replace pcsect = pcsect + " 2023" if in_ULEZ_decoded == "In 2023 ULEZ"
replace pcsect = pcsect + " 2019" if in_ULEZ_decoded == "In 2019 ULEZ"

*now generate a treated and a treatment variable and keep only the relevant pcsects: either ones in the 2021 ULEZ not too close to 2019, or those at least 50km from the outer border
gen treated = in_ULEZ_decoded == "In 2021 ULEZ"
gen treatment = in_ULEZ_decoded == "In 2021 ULEZ" & saleyear >= 2021
keep if (treated == 1 & pcsect_dist_from_2019_ULEZ > 5) | pcsect_dist_from_2023_ULEZ > 50

*remove all observations with missings (in the controls), then balance the panel (required)
missings tag, gen(tag)
drop if tag > 0
drop tag

bys pcsect: gen n = _N
drop if n < 11
drop n


*filter the data to remove all pcsects in which class D LEZs or ZEZs have been implemented (Birmingham (B), Bristol (BS) and Oxford (OX))
keep if !(regexm(pcsect, "^BS[0-9]+") | regexm(pcsect, "^B[0-9]+") | regexm(pcsect, "^OX[0-9]+"))

*xtset it - string variables not allowed, so first destring
encode pcsect, gen(pcsect_num)
xtset pcsect_num saleyear


*now do sdid to get the first set of weights, as suggested by Morin (2024)
sdid avg_log_price pcsect_num saleyear treatment, covariates(pcsect_share_semi pcsect_share_detached pcsect_share_flat pcsect_share_new pcsect_share_leasehold pcsect_imd pcsect_pop_density) vce(noinference) returnweights

*save the weights as a temporary file to be merged into the larger dataset
keep pcsect saleyear lambda2021 omega2021 
save "Temp\sdid_weights_2021.dta", replace

*now load in the original data
use "Temp\pp_ready_all.dta", clear
*merge in the weights
merge m:1 pcsect saleyear using "Temp\sdid_weights_2021.dta"
*lots are unmatched from the original dataset, but this is because we cut a large region out
keep if _merge == 3
drop _merge

*now get the second set of weights
bys pcsect saleyear: gen morin_weight = 1/_N

*now get the full weights
gen weight = lambda2021 * omega2021 * morin_weight


*now do the weighted DiD

preserve

gen cohort = 0
replace cohort = 2021 if in_2021_ULEZ == 1

gen Stime = cohort if cohort>0
gen etime = saleyear-Stime

tab etime, gen(et)
*there are 11 et dummies, with etime = 0 now being at et7

foreach var of varlist et1-et11 {
	replace `var' = 0 if cohort==0
}

*again rebase to two quarters prior to implementation - we see the same sharp drop before this point as in 2023 (giving more support to the claim that it comes from anticipation rather than randomness)
global leads "et1-et5"
global lags "et7-et11"

*regress and prepare the coefficients for graphing
reghdfe log_price $leads $lags $controls [aweight=weight], absorb(quarter pcsect) vce(cluster pcsect)
graphprep mod1 1 11 6 7

*plot the graphs
twoway (rcap mod1_up mod1_lo mod1_t, lc("216 17 89")) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-6(1)4) xline(-0.5, lc(black*.80%50) lp(dash)) title("2021 Dynamic DiD") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

restore


preserve
keep if in_2023_ULEZ == 0
gen sdid_wt = omega2021*lambda2021
bys pcdist: egen av_wt = mean(sdid_wt)
collapse (mean) av_wt = sdid_wt, by(pcdist saleyear)
gsort -av_wt
keep if saleyear == 2019
restore

*the above suggests the weights are not working well - why are so many random regions getting high weights?



*try now for 2023


*load in the data
use "Temp\pp_ready_collapsed_panel.dta", clear

*first generate a unique cross-sectional identifier (i.e. the pseudo-postcode sector)
decode in_ULEZ, gen(in_ULEZ_decoded)
gen pcsect = pcsect_original
replace pcsect = pcsect + " 2021" if in_ULEZ_decoded == "In 2021 ULEZ"
replace pcsect = pcsect + " 2023" if in_ULEZ_decoded == "In 2023 ULEZ"
replace pcsect = pcsect + " 2019" if in_ULEZ_decoded == "In 2019 ULEZ"

*now generate a treated and a treatment variable and keep only the relevant pcsects: either ones in the 2023 ULEZ not too close to 2021, or those at least 50km from the outer border
gen treated = in_ULEZ_decoded == "In 2023 ULEZ"
gen treatment = in_ULEZ_decoded == "In 2023 ULEZ" & saleyear >= 2023
keep if (treated == 1 & pcsect_dist_from_2021_ULEZ > 5) | pcsect_dist_from_2023_ULEZ > 50

*remove all observations with missings (in the controls), then balance the panel (required)
missings tag, gen(tag)
drop if tag > 0
drop tag

bys pcsect: gen n = _N
drop if n < 11
drop n


*filter the data to remove all pcsects in which class D LEZs or ZEZs have been implemented (Birmingham (B), Bristol (BS) and Oxford (OX))
keep if !(regexm(pcsect, "^BS[0-9]+") | regexm(pcsect, "^B[0-9]+") | regexm(pcsect, "^OX[0-9]+"))

*xtset it - string variables not allowed, so first destring
encode pcsect, gen(pcsect_num)
xtset pcsect_num saleyear

*create a line plot for average price evolution over time for each pcsect, on the same plot
twoway (line avg_log_price saleyear if treated == 0,  lcolor(black%30) lwidth(thin) connect(L)) (line avg_log_price saleyear if treated == 1,  lcolor(red%10) lwidth(vthin) connect(L))
	*we see that mean price is very different for the two groups, which makes synthetic control very difficult (but should be fine for SDID as it can get around differences in levels - not clear why this doesn't work)

*now do sdid to get the first set of weights, as suggested by Morin (2024)
sdid avg_log_price pcsect_num saleyear treatment, covariates(pcsect_share_semi pcsect_share_detached pcsect_share_flat pcsect_share_new pcsect_share_leasehold pcsect_imd pcsect_pop_density) vce(noinference) returnweights

*save the weights as a temporary file to be merged into the larger dataset
keep pcsect saleyear lambda2023 omega2023
save "Temp\sdid_weights_2023.dta", replace

*now load in the original data
use "Temp\pp_ready_all.dta", clear
*merge in the weights
merge m:1 pcsect saleyear using "Temp\sdid_weights_2023.dta"
*lots are unmatched from the original dataset, but this is because we cut a large region out
keep if _merge == 3
drop _merge

*now get the second set of weights
bys pcsect saleyear: gen morin_weight = 1/_N

*now get the full weights
gen weight = lambda2023 * omega2023 * morin_weight

*now do the DiD
preserve

gen cohort = 0
replace cohort = 2023 if in_2023_ULEZ == 1

gen Stime = cohort if cohort>0
gen etime = saleyear-Stime

tab etime, gen(et)
*there are 11 et dummies, with etime = 0 now being at et7

foreach var of varlist et1-et11 {
	replace `var' = 0 if cohort==0
}

*again rebase to two quarters prior to implementation - we see the same sharp drop before this point as in 2023 (giving more support to the claim that it comes from anticipation rather than randomness)
global leads "et1-et7"
global lags "et9-et11"

*regress and prepare the coefficients for graphing
reghdfe log_price $leads $lags $controls [aweight=weight], absorb(quarter pcsect) vce(cluster pcsect)
graphprep mod1 1 11 8 9

*plot the graphs
twoway (rcap mod1_up mod1_lo mod1_t, lc("143 45 86"%50)) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-8(1)2) xline(-0.5, lc(black*.80%50) lp(dash)) title("2023 Dynamic DiD") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

*not working???? Why?
*the original pcsect-level SDID analysis also has this feature - why? Can it just not match them well enough?

*seems like the synthetic control part, regardless of whether it is being used with something else, struggles to find a control group with suitable pre-trends for the 2023 zone

restore























*1) COARSENED EXACT MATCHING

*the first part of this analysis is done in R

*TO BE REMOVED WHEN DONE PROPERLY: replace matched pcsects to match with how they are formatted in the pp_ready file
preserve
use "Temp\matched_pcsects_2021.dta", clear
replace pcsect = subinstr(pcsect, " Not in ULEZ", "", .)
replace pcsect = subinstr(pcsect, "In 2021 ULEZ", "2021", .)
replace pcsect = subinstr(pcsect, "In 2023 ULEZ", "2023", .)
save "Temp\matched_pcsects_2021.dta", replace

*now prepare for visualisation
gen pcsect_original = subinstr(pcsect, " 2021", "", .)
replace pcsect_original = subinstr(pcsect_original, " 2023", "", .)
gen tag = 1
*export as an excel file
export excel using "Temp\2021_pcsects_vis.xlsx", firstrow(variables) replace
restore

*load in the data
use "Temp\pp_ready_all.dta", clear

*work out which postcode sectors have matched: merge in the postcode matched data
merge m:1 pcsect using "Temp\matched_pcsects_2021.dta" 
keep if _merge == 3
drop _merge

*summarise
tab pcsect

*now do basic static DiD (liek 2) in the origninal file)

preserve

*generate a ULEZ dummy: recall here that quarter = number of quarters since Jan 1 2000, so 2021 Q4 is quarter = 2021-2000)*4 + 4 = 21*4 + 4
gen ULEZ = in_2021_ULEZ & quarter >= 21*4 + 4

*do the simple static DiD regression
reghdfe log_price ULEZ $controls, absorb(quarter pcsect) vce(cluster pcsect)

*negative but not significant (low power probably)

restore


*now try a dynamic one (like 5) in the one before)
preserve

gen cohort = 0
replace cohort = 2021 if in_2021_ULEZ == 1

gen Stime = cohort if cohort>0
gen etime = saleyear-Stime

tab etime, gen(et)
*there are 11 et dummies, with etime = 0 now being at et7

foreach var of varlist et1-et11 {
	replace `var' = 0 if cohort==0
}

*again rebase to two quarters prior to implementation - we see the same sharp drop before this point as in 2023 (giving more support to the claim that it comes from anticipation rather than randomness)
global leads "et1-et5"
global lags "et7-et11"

*regress and prepare the coefficients for graphing
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) vce(cluster pcsect)
graphprep mod1 1 11 6 7

*plot the graphs
twoway (rcap mod1_up mod1_lo mod1_t, lc("216 17 89")) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-6(1)4) xline(-0.5, lc(black*.80%50) lp(dash)) title("2021 Dynamic DiD") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

restore





* now do the same for the 2023 ULEZ

preserve
use "Temp\matched_pcsects_2023.dta", clear
replace pcsect = subinstr(pcsect, " Not in ULEZ", "", .)
replace pcsect = subinstr(pcsect, "In 2021 ULEZ", "2021", .)
replace pcsect = subinstr(pcsect, "In 2023 ULEZ", "2023", .)
save "Temp\matched_pcsects_2023.dta", replace

*now prepare for visualisation
gen pcsect_original = subinstr(pcsect, " 2021", "", .)
replace pcsect_original = subinstr(pcsect_original, " 2023", "", .)
gen tag = 1
*export as an excel file
export excel using "Temp\2023_pcsects_vis.xlsx", firstrow(variables) replace
restore

*load in the data
use "Temp\pp_ready_all.dta", clear

*work out which postcode sectors have matched: merge in the postcode matched data
merge m:1 pcsect using "Temp\matched_pcsects_2023.dta" 
keep if _merge == 3
drop _merge

*summarise
tab pcsect

*now do basic static DiD (liek 2) in the origninal file)
preserve

gen ULEZ = in_2023_ULEZ & quarter >= 23*4 + 3
reghdfe log_price ULEZ $controls, absorb(quarter pcsect) vce(cluster pcsect)

restore


* now dynamic

preserve

gen cohort = 0
replace cohort = 2023 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0

*generate event time variable
gen Stime = cohort if cohort>0
gen etime = saleyear-Stime

*generate the event time dummies
tab etime, gen(et)
*there are 11 et dummies, with etime = 0 being at et9

*ensure the event time dummies are always 0 for the never treated group
foreach var of varlist et1-et11  {
	replace `var' = 0 if cohort==0
}

*define a set of leads and lags
*the graph will show a quarter of what looks like anticipatory behaviour - to deal with this, set the base period to be two quarters before the policy, rather than one
global leads "et1-et7"
global lags "et9-et11"

*run the regressions, and use the graphprep program written at the start to prepare the coefficients for graphing
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) vce(cluster pcsect)
*this corresponds to first dummy = 1, last dummy = 11, baseline dummy = 8, first treatment dummy = 9
graphprep mod1 1 11 8 9

*plot the graph of the coefficients
twoway (rcap mod1_up mod1_lo mod1_t, lc("143 45 86"%50)) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-8(1)2) xline(-0.5, lc(black*.80%50) lp(dash)) title("2023 Dynamic DiD") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

restore



*plot the control areas!!







**** K-MEANS CLUSTERING

*load in excel data on pcsect and save as dta (do this in R)
import excel "Temp\matched_pcsects_2021_kmeans.xlsx", clear firstrow
save "Temp\matched_pcsects_2021_kmeans.dta", replace


*load in the data
use "Temp\pp_ready_all.dta", clear

*work out which postcode sectors have matched: merge in the postcode matched data
merge m:1 pcsect using "Temp\matched_pcsects_2021_kmeans.dta" 
keep if _merge == 3
drop _merge


*now try a dynamic DiD (like 5) in the one before)
preserve

gen cohort = 0
replace cohort = 2021 if in_2021_ULEZ == 1

gen Stime = cohort if cohort>0
gen etime = saleyear-Stime

tab etime, gen(et)
*there are 11 et dummies, with etime = 0 now being at et7

foreach var of varlist et1-et11 {
	replace `var' = 0 if cohort==0
}

*again rebase to two quarters prior to implementation - we see the same sharp drop before this point as in 2023 (giving more support to the claim that it comes from anticipation rather than randomness)
global leads "et1-et5"
global lags "et7-et11"

*regress and prepare the coefficients for graphing
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) vce(cluster pcsect)
graphprep mod1 1 11 6 7

*plot the graphs
twoway (rcap mod1_up mod1_lo mod1_t, lc("216 17 89")) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-6(1)4) xline(-0.5, lc(black*.80%50) lp(dash)) title("2021 Dynamic DiD") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

restore




*now do this for 2023

*load in excel data on pcsect and save as dta (do this in R)
import excel "Temp\matched_pcsects_2023_kmeans.xlsx", clear firstrow
save "Temp\matched_pcsects_2023_kmeans.dta", replace

*load in the data
use "Temp\pp_ready_all.dta", clear

*work out which postcode sectors have matched: merge in the postcode matched data
merge m:1 pcsect using "Temp\matched_pcsects_2023_kmeans.dta" 
keep if _merge == 3
drop _merge

* now dynamic

preserve

gen cohort = 0
replace cohort = 2023 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0

*generate event time variable
gen Stime = cohort if cohort>0
gen etime = saleyear-Stime

*generate the event time dummies
tab etime, gen(et)
*there are 11 et dummies, with etime = 0 being at et9

*ensure the event time dummies are always 0 for the never treated group
foreach var of varlist et1-et11  {
	replace `var' = 0 if cohort==0
}

*define a set of leads and lags
*the graph will show a quarter of what looks like anticipatory behaviour - to deal with this, set the base period to be two quarters before the policy, rather than one
global leads "et1-et7"
global lags "et9-et11"

*run the regressions, and use the graphprep program written at the start to prepare the coefficients for graphing
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) vce(cluster pcsect)
*this corresponds to first dummy = 1, last dummy = 11, baseline dummy = 8, first treatment dummy = 9
graphprep mod1 1 11 8 9

*plot the graph of the coefficients
twoway (rcap mod1_up mod1_lo mod1_t, lc("143 45 86"%50)) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-8(1)2) xline(-0.5, lc(black*.80%50) lp(dash)) title("2023 Dynamic DiD") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

restore










**** PROPENSITY SCORE MATCHING

gen close = pcsect_dist_from_2023_ULEZ < 20
gen in_2023_ULEZ = in_ULEZ > 1

reg in_2023_ULEZ pcsect_avg_log_price-pcsect_share_freehold pcsect_no22019-pcdist_pop_density if close == 1
*poor - large std errors
*should do this with e.g. neural network

predict fitted_values if close == 0, xb

histogram fitted_values



**** NN/DISTANCE MATCHING

drop nn*

gen nn1 = ""
gen nn2 = ""
decode in_ULEZ, gen(in_ULEZ_decoded)
gen pcsect = pcsect_original + " " + in_ULEZ_decoded
recast str100 pcsect
recast str100 nn1
recast str100 nn2

keep if close == 0 | in_ULEZ == 1

mahapick pcsect_avg_log_price-pcsect_share_freehold pcsect_no22019-pcdist_pop_density, idvar(pcsect) treated(close) pickids(nn1 nn2)
*try this with genfile - might give more informative info on distances etc 
*we could try to do this like synthetic control, getting a weighting for each control unit

*extract the pcsects, then merge in w monthly/annual pcsect-level time series and compare evolution over time

compress





**** SYNTHETIC CONTROL

*this does the synthetic control analysis for the 2021 zone

use "Temp\pp_ready_collapsed_panel.dta", clear


*now loop through each of these, and get the synthetic control for each, using the synth package

*first collect up all treated units
gen treatment = in_ULEZ == 2

*collect up all treated units
decode in_ULEZ, gen(in_ULEZ_decoded)
gen pcsect = pcsect_original + " " + in_ULEZ_decoded

*remove all observations with missings, then balance the data (required for synth)
missings tag, gen(tag)
drop if tag > 0
drop tag

bys pcsect: gen n = _N
drop if n < 11
drop n

*keep only pcsects in the 2021 expansion zone or at least 50km from the outer 2023 zone (to address spillover issues)
keep if in_ULEZ == 2 | pcsect_dist_from_2023_ULEZ > 50

*convert the postcode sector to a unique numeric identifier, with the treated ones first (which means we can't use encode)
gsort -treatment pcsect_original saleyear
gen pcsect_numeric = _n / 11 if mod(_n, 11) == 0

*make this value the same for all observations in the same pcsect
bysort pcsect (saleyear): replace pcsect_numeric = pcsect_numeric[11] if missing(pcsect_numeric)

*tsset the data
xtset pcsect_numeric saleyear


*count how many pcsects are treated, so we can then loop over them
*save a local macro with the number of treated units
preserve
keep if treatment == 1
sort pcsect_numeric
local n_treated = pcsect_numeric[_N]
restore

*display the number of treated units
display "`n_treated' treated units"

*now loop over these treated units
forvalues i = 1/`n_treated' {

    *keep only untreated units and the current treated unit
    preserve
    keep if pcsect_numeric == `i' | treatment == 0

    *do the synthetic control
    synth avg_log_price avg_log_price(2015(1)2020), trunit(`i') trperiod(2021) genvars

    *save the results somewhere

    restore
}


*numlist has too many elements - why? Too many controls to choose from? How to fix?








*prepare for synth_runner
gen treated = treatment * (saleyear >= 2021)

*now do the SC
synth_runner avg_log_price avg_log_price(2015(1)2020), d(treated) genvars

*too many units?? I think this is why it doesn't work
*not just too high dimensional feature space
*can we increase permitted number of treated units/control units in synth?








*************************************** try CEM in Stata

*set the cd and load in the data
cd "C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data"
use "Temp\pp_ready_collapsed_xsec.dta", clear

*first do 2021

*first generate a unique cross-sectional identifier (i.e. the pseudo-postcode sector)
decode in_ULEZ, gen(in_ULEZ_decoded)
gen pcsect = pcsect_original + " " + in_ULEZ_decoded

*now generate a treatment variable and keep only the relevant pcsects
gen treated = in_ULEZ_decoded == "In 2021 ULEZ"
keep if (treated == 1 & pcsect_dist_from_2019_ULEZ > 5) | pcsect_dist_from_2023_ULEZ > 50

*remove all observations with missings
missings tag, gen(tag)
drop if tag > 0
drop tag

*filter the data to remove all pcsects in which class D LEZs or ZEZs have been implemented (Birmingham (B), Bristol (BS) and Oxford (OX))
keep if !(regexm(pcsect, "^BS[0-9]+") | regexm(pcsect, "^B[0-9]+") | regexm(pcsect, "^OX[0-9]+"))

*do the CEM matching (with k2k, to avoid having to use weights)
set seed 12345
cem pcsect_imd (#5) pcsect_share_flat (#5) pcsect_share_detached (#5) pcsect_share_semi (#5) pcsect_share_new (#5) pcsect_share_leasehold (#5) pcdist_imd (#5) pcdist_share_flat (#5) pcdist_share_detached (#5) pcdist_share_semi (#5) pcdist_share_new (#5) pcdist_share_leasehold (#5), treatment(treated) showbreaks k2k

*get matched pcsects
keep if cem_matched == 1
keep pcsect


*TO BE REMOVED WHEN DONE PROPERLY: --------
*replace matched pcsects to match with how they are formatted in the pp_ready file
replace pcsect = subinstr(pcsect, " Not in ULEZ", "", .)
replace pcsect = subinstr(pcsect, "In 2021 ULEZ", "2021", .)
save "Temp\matched_pcsects_2021.dta", replace

*now briefly prepare for visualisation
preserve
gen pcsect_original = subinstr(pcsect, " 2021", "", .)
gen tag = 1
*export as an excel file
export excel using "Temp\2021_pcsects_vis.xlsx", firstrow(variables) replace
restore
*--------

*load in the data
use "Temp\pp_ready_all.dta", clear

*work out which postcode sectors have matched: merge in the postcode matched data
merge m:1 pcsect using "Temp\matched_pcsects_2021.dta" 
keep if _merge == 3
drop _merge

*summarise
tab pcsect


*now do a dynamic DiD (like (5) in the do-file before, but at the saleyear level)

preserve

gen cohort = 0
replace cohort = 2021 if in_2021_ULEZ == 1

gen Stime = cohort if cohort>0
gen etime = saleyear-Stime

tab etime, gen(et)
*there are 11 et dummies, with etime = 0 now being at et7

foreach var of varlist et1-et11 {
	replace `var' = 0 if cohort==0
}

*again rebase to two quarters prior to implementation - we see the same sharp drop before this point as in 2023 (giving more support to the claim that it comes from anticipation rather than randomness)
global leads "et1-et5"
global lags "et7-et11"

*regress and prepare the coefficients for graphing
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) vce(cluster pcsect)
graphprep mod1 1 11 6 7

*plot the graphs
twoway (rcap mod1_up mod1_lo mod1_t, lc("216 17 89")) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (years after first treatment, j)") xlabel(-6(1)4) xline(-0.5, lc(black*.80%50) lp(dash)) title("2021 Dynamic DiD") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

*looks good - confirms original findings!

restore







*now do 2023

*reload original data
use "Temp\pp_ready_collapsed_xsec.dta", clear

*pre-process it in the same way
decode in_ULEZ, gen(in_ULEZ_decoded)
gen pcsect = pcsect_original + " " + in_ULEZ_decoded

gen treated = in_ULEZ_decoded == "In 2023 ULEZ"
keep if (treated == 1 & pcsect_dist_from_2021_ULEZ > 5) | pcsect_dist_from_2023_ULEZ > 50

*remove all observations with missings
missings tag, gen(tag)
drop if tag > 0
drop tag

*filter the data to remove all pcsects in which class D LEZs or ZEZs have been implemented (Birmingham (B), Bristol (BS) and Oxford (OX))
keep if !(regexm(pcsect, "^BS[0-9]+") | regexm(pcsect, "^B[0-9]+") | regexm(pcsect, "^OX[0-9]+"))

*do the CEM matching (with k2k, to avoid having to use weights)
set seed 12345
cem pcsect_imd (#5) pcsect_pop_density (#5) pcsect_no22019 (#5) pcsect_share_flat (#5) pcsect_share_detached (#5) pcsect_share_semi (#5) pcsect_share_new (#5) pcsect_share_leasehold (#5) pcdist_imd (#5) pcdist_pop_density (#5) pcdist_no22019 (#5) pcdist_share_flat (#5) pcdist_share_detached (#5) pcdist_share_semi (#5) pcdist_share_new (#5) pcdist_share_leasehold (#5), treatment(treated) showbreaks k2k

*get matched pcsects
keep if cem_matched == 1
keep pcsect


*TO BE REMOVED WHEN DONE PROPERLY: --------
*replace matched pcsects to match with how they are formatted in the pp_ready file
replace pcsect = subinstr(pcsect, " Not in ULEZ", "", .)
replace pcsect = subinstr(pcsect, "In 2023 ULEZ", "2023", .)
save "Temp\matched_pcsects_2023.dta", replace

*now briefly prepare for visualisation
preserve
gen pcsect_original = subinstr(pcsect, " 2023", "", .)
gen tag = 1
*export as an excel file
export excel using "Temp\2023_pcsects_vis.xlsx", firstrow(variables) replace
restore
*--------

*load in the data
use "Temp\pp_ready_all.dta", clear

*work out which postcode sectors have matched: merge in the postcode matched data
merge m:1 pcsect using "Temp\matched_pcsects_2023.dta" 
keep if _merge == 3
drop _merge

*summarise
tab pcsect


*now do a dynamic DiD (like (5) in the do-file before, but at the saleyear level)

preserve

gen cohort = 0
replace cohort = 2023 if in_2023_ULEZ == 1

gen Stime = cohort if cohort>0
gen etime = saleyear-Stime

tab etime, gen(et)
*there are 11 et dummies, with etime = 0 now being at et7

foreach var of varlist et1-et11 {
	replace `var' = 0 if cohort==0
}

*again rebase to two quarters prior to implementation - we see the same sharp drop before this point as in 2023 (giving more support to the claim that it comes from anticipation rather than randomness)
global leads "et1-et7"
global lags "et9-et11"

*regress and prepare the coefficients for graphing
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) vce(cluster pcsect)
graphprep mod1 1 11 8 9

*plot the graphs
twoway (rcap mod1_up mod1_lo mod1_t, lc("143 45 86"%50)) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-8(1)2) xline(-0.5, lc(black*.80%50) lp(dash)) title("2023 Dynamic DiD") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

restore






*now try all together

*note this may be bad because we can't guarantee the same proportion of matched units in 2021 vs 2023 as in the overall sample (so this may be misleading) (but then they all have this problem)

*reload original data
use "Temp\pp_ready_collapsed_xsec.dta", clear

*pre-process it in the same way
decode in_ULEZ, gen(in_ULEZ_decoded)
gen pcsect = pcsect_original + " " + in_ULEZ_decoded

gen treated = in_ULEZ_decoded == "In 2023 ULEZ" | in_ULEZ_decoded == "In 2021 ULEZ"
keep if (in_ULEZ_decoded == "In 2023 ULEZ" & pcsect_dist_from_2021_ULEZ > 5) | (in_ULEZ_decoded == "In 2021 ULEZ" & pcsect_dist_from_2019_ULEZ > 5) | pcsect_dist_from_2023_ULEZ > 50

missings tag, gen(tag)
drop if tag > 0
drop tag

keep if !(regexm(pcsect, "^BS[0-9]+") | regexm(pcsect, "^B[0-9]+") | regexm(pcsect, "^OX[0-9]+"))

*do the CEM matching (with k2k, to avoid having to use weights)
set seed 12345
cem pcsect_imd (#5) pcsect_share_flat (#5) pcsect_share_detached (#5) pcsect_share_semi (#5) pcsect_share_new (#5) pcsect_share_leasehold (#5) pcdist_imd (#5) pcdist_share_flat (#5) pcdist_share_detached (#5) pcdist_share_semi (#5) pcdist_share_new (#5) pcdist_share_leasehold (#5), treatment(treated) showbreaks k2k

*get matched pcsects
keep if cem_matched == 1
keep pcsect

*TO BE REMOVED WHEN DONE PROPERLY: --------
*replace matched pcsects to match with how they are formatted in the pp_ready file
replace pcsect = subinstr(pcsect, " Not in ULEZ", "", .)
replace pcsect = subinstr(pcsect, "In 2023 ULEZ", "2023", .)
replace pcsect = subinstr(pcsect, "In 2021 ULEZ", "2021", .)
save "Temp\matched_pcsects_both.dta", replace

*now briefly prepare for visualisation
preserve
gen pcsect_original = subinstr(pcsect, " 2023", "", .)
replace pcsect_original = subinstr(pcsect_original, " 2021", "", .)
gen tag = 1
*export as an excel file
export excel using "Temp\both_pcsects_vis.xlsx", firstrow(variables) replace
restore
*--------

*load in the data
use "Temp\pp_ready_all.dta", clear

*work out which postcode sectors have matched: merge in the postcode matched data
merge m:1 pcsect using "Temp\matched_pcsects_both.dta" 
keep if _merge == 3
drop _merge

*summarise
tab pcsect


*now do Abraham and Sun event study

preserve 

gen cohort = 0
replace cohort = 2023 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0
replace cohort = 2021 if in_2021_ULEZ == 1

gen Stime = cohort if cohort != 0
gen etime = saleyear - Stime
tab etime, gen(et)

foreach var of varlist et1-et13  {
replace `var' = 0 if cohort==0
}

*make the necessary corrections and variables to use the eventstudyinteract package (provided by the authors)
replace cohort = . if cohort == 0
gen never_treated = cohort == .
egen long pcsect_numeric = group(pcsect)

*define lags to account for anticipation, as usual
global leads "et1-et7"
global lags "et9-et13"

*note the new more general program for plotting graphs after regressions with eventstudyinteract
eventstudyinteract log_price $leads $lags, control_cohort(never_treated) cohort(cohort) absorb(quarter pcsect_numeric) covariates($controls) vce(cluster pcsect_numeric)
graphprepgeneral mod1 1 13 8 9 e(b_iw) e(V_iw)

*plot the graph
twoway (rcap mod1_up mod1_lo mod1_t, lc("33 131 128")) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6)) yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-8(1)4) xline(-0.5, lc(black*.80%50) lp(dash)) title("Staggered DiD, using Abraham and Sun (2021)") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = year", span)

restore

















**************************************************************************************
**************************************************************************************
*TEMPORARY CODE FROM THE MAIN ANALYSIS DO-FILE
**************************************************************************************
**************************************************************************************


*****************************************************************************
*TEMPORARY
*****************************************************************************


*do 12) with time trends


preserve

* Load in the UK-wide data and merge with matched postcode sectors for 2023
use "Temp\pp_ready_all.dta", clear
merge m:1 pcsect using "Temp\matched_pcsects_2023_kmeans.dta" 
keep if _merge == 3
drop _merge

* Generate cohort and event time variables as before
gen cohort = 0
replace cohort = 23*4 + 3 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0
gen Stime = cohort if cohort>0
gen etime = quarter-Stime
tab etime, gen(et)
foreach var of varlist et1-et40  {
	replace `var' = 0 if cohort==0
}

* Define leads and lags
global leads "et1-et33"
global lags "et35-et40"

* create a numeric pcdist variable
encode pcdist, gen(pcdist_numeric)

* Run the regression with interacted FEs
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect i.pcdist_numeric#i.quarter) vce(cluster pcsect)

*collinear! because postcode district and quarter determine treatment status together

* try an alternative
reghdfe log_price $leads $lags $controls c.quarter#in_2023_ULEZ, absorb(quarter pcsect) vce(cluster pcsect)

*still collinear! WHY? because your quarter and whether you are ever treated completely determines your treatment status

*another option - cruder time FEs
reghdfe log_price $leads $lags $controls c.saleyear#in_2023_ULEZ, absorb(quarter pcsect) vce(cluster pcsect)

*still collinear - why????

*new option: crude location time trends (which is the set of characters up to the first number)
*first get the pcarea - the first part of the postcode, up to the first number
gen pcarea = regexs(1) if regexm(postcode, "^([A-Za-z]+)")
encode pcarea, gen(pcarea_numeric)
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect i.quarter#i.pcarea_numeric) vce(cluster pcsect)

* Prepare and plot as before
graphprep mod1 1 40 34 35
twoway (rcap mod1_up mod1_lo mod1_t, lc("143 45 86"%50)) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-30(10)0) xtick(-34(1)5, tlength(relative0p6)) xline(-0.5, lc(black*.80%50) lp(dash)) title("2023 Dynamic DiD with pcdist-specific trends") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

*no longer collinear, but not good

restore







*6b) do the same regression as in 6), but use multiway clustering (quarter x pcsect)

preserve

*keep the two analysis regions and the control region
keep if (inrange(dist_from_2023_ULEZ, 5, 10)) | (dist_from_2021_ULEZ > 5 & in_2023_ULEZ == 1) | (dist_from_2019_ULEZ > 5 & in_2021_ULEZ == 1)

*generate a cohort variable, giving the quarter in which each house was first treated
gen cohort = 0
replace cohort = 23*4 + 3 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0
replace cohort = 21*4 + 4 if in_2021_ULEZ == 1

*generate an event time variable (i.e. number of quarters before/after first treatment)
gen Stime = cohort if cohort>0
gen etime = quarter-Stime

*use it to create a set of event time dummies
tab etime, gen(et)
*there are 47 et dummies, with etime = 0 being at et35

*set each event time dummy to 0 for the never treated group
foreach var of varlist et1-et47  {
	replace `var' = 0 if cohort==0
}

*again define leads and lags to account for the anticipatory period, as before
global leads "et1-et32"
global lags "et34-et47"

*generate a grouped variable to cluster on
egen double_cluster = group(pcsect quarter)

*run the regression and prepare the graphs for plotting with the graphprep function
*reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) vce(cluster double_cluster)
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) cluster(pcsect quarter)
graphprep mod1 1 47 33 35

*plot the graphs
twoway (rcap mod1_up mod1_lo mod1_t, lc("33 131 128")) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6)) yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-30(10)10) xtick(-34(1)12, tlength(relative0p6)) xline(-0.5, lc(black*.80%50) lp(dash)) title("Staggered DiD for 2021 and 2023") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

*save the graph
graph export "Output\Results\ES_a_10b5s_formatted_multiway_clust.pdf", as(pdf) replace





