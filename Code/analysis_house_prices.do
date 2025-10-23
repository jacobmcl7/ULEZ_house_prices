*-------------------------------------------*
*DESCRIPTION
*-------------------------------------------*
*this do-file runs some regression analysis for the main question - the effect of ULEZ on house prices
*it does so in a series of preserve-restore sections, so each component of the analysis can be run in isolation after having loaded in the cleaned data from the 'Temp' folder
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

*make a flexible version of this program, that allows for any named matrices - this will be used with the packages for other estimators
cap prog drop graphprepgeneral
program def graphprepgeneral
args mod start end base treat b V
	quietly {
        cap drop `mod'*
		gen `mod' = .
		gen `mod'_up = .
		gen `mod'_lo = .
		gen `mod'_t = .
		local n = 1
		forvalues i = `start'/`end' {
			display `i'
			if `i'<`base' {
				replace `mod' = `b'[1, `i'] in `n'
				replace `mod'_up = `b'[1, `i']+1.96*sqrt(`V'[`i', `i']) in `n'
				replace `mod'_lo = `b'[1, `i']-1.96*sqrt(`V'[`i', `i']) in `n'
			}
			if `i'>`base' {
				replace `mod' = `b'[1, `i'-1] in `n'
				replace `mod'_up = `b'[1, `i'-1]+1.96*sqrt(`V'[`i'-1, `i'-1]) in `n'
				replace `mod'_lo = `b'[1, `i'-1]-1.96*sqrt(`V'[`i'-1, `i'-1]) in `n'
			}
			if `i' == `base' {
				display "base"
				replace `mod' = 0 in `n'
			}
			replace `mod'_t = `i'-`treat' in `n'
			local n = `n'+1
		}
    }
end
*-------------------------------------------*



*-------------------------------------------*
*PREPARE DATA
*-------------------------------------------*
*set the cd and load in the data
cd "C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data"
use "Temp\pp_ready.dta", clear

*make a global macro for the controls, to insert into regressions
global controls "detached semidetached terraced new leasehold"
*-------------------------------------------*



*-------------------------------------------*
*INDEX
*-------------------------------------------*
*1) a static staggered DiD 				*PRODUCES THE RESULTS FOR SPECIFICATIONS 1 AND 2 IN TABLE 3*
	*1a) disaggregate by groups 		*PRODUCES TABLE 4 IN APPENDIX A.3*
	*1b) vary the assumed distances 	*PRODUCES TABLE 5 IN APPENDIX A.3*
*2) a static DiD for 2023 				*PRODUCES THE RESULT FOR SPECIFICATION 4 IN TABLE 3*
*3) a static DiD for 2021 				*PRODUCES THE RESULT FOR SPECIFICATION 3 IN TABLE 3*
*4) a dynamic DiD for 2023 				*PRODUCES THE RESULT IN FIGURE 9 IN APPENDIX A.6*
*5) a dynamic DiD for 2021				*PRODUCES THE RESULT IN FIGURE 8 IN APPENDIX A.6*
*6) an event study (i.e. staggered DiD) for both regions combined						*PRODUCES THE TWFE RESULT IN FIGURE 4 AND APPENDIX A.4*
	*6a) check robustness of parallel trends, using honestdid							*PRODUCES THE TWFE PARALLEL TRENDS ANALYSIS IN FIGURE 6 AND TABLE 4*
*7) event study, now using Abraham and Sun (2021) interacted-weighted ATT estimator		*PRODUCES THE ABRAHAM AND SUN (2021) RESULT IN FIGURE 4 AND APPENDIX A.4*
	*7a) check robustness of parallel trends, using honestdid							*PRODUCES THE ABRAHAM AND SUN (2021) PARALLEL TRENDS ANALYSIS IN FIGURE 6 AND TABLE 4*
*8) event study, using Gardner (2021) did2s estimator									*PRODUCES THE GARDNER (2021) RESULT IN FIGURE 4 AND APPENDIX A.4*
	*8a) check robustness of parallel trends, using honestdid							*PRODUCES THE GARDNER (2021) PARALLEL TRENDS ANALYSIS IN FIGURE 6 AND TABLE 4*
*9) a collection of 6), 7) and 8) onto one graph										*PRODUCES FIGURE 4*
*10) event study, using the two stage estimator from Butts (2024)						*PRODUCES FIGURE 5 AND THE BUTTS (2024) RESULT IN APPENDIX A.4*
	*10a) check robustness of parallel trends, using honestdid							*PRODUCES THE BUTTS (2024) PARALLEL TRENDS ANALYSIS IN FIGURE 6 AND TABLE 4*
*11) a dynamic DiD for 2021, using long-distance controls                               *PRODUCES PANEL A) OF FIGURE 8
*12) a dynamic DiD for 2023, using long-distance controls                               *PRODUCES PANEL B) OF FIGURE 8
*13)-16) robustness checks on the parameters of the long-distance analysis              *PRODUCES FIGURES 20 AND 21 IN APPENDIX B
*-------------------------------------------*



*-------------------------------------------*
*LIST OF FIGURES AND TABLES PRODUCED
*-------------------------------------------*
*1)
	*"Temp\reg1.doc": this is (formatted into) table 3
*1a)
	*"Temp\reg1_het.doc": this is (formatted into) the first half of table 4
	*"Temp\reg1_het2.doc": this is (formatted into) the second half of table 4
*1b)
	*"Temp\reg1_het3.doc": this is (formatted into) table 5
*2)
	*"Temp\reg1.doc": this is (formatted into) table 3
*3)
	*"Temp\reg1.doc": this is (formatted into) table 3
*4)
	*"Output\Results\DiD_2023_a_10b5s_formatted.pdf": this is figure 9
*5)
	*"Output\Results\DiD_2021_a_10b5s_formatted.pdf": this is figure 8
*6)
	*"Output\Results\ES_a_10b5s_formatted.pdf": this is unreported (but is contained in figure 4)
*6a)
	*"Output\Results\ES_a_10b5s_yearly_formatted.pdf": this is unreported
	*the results from the sensitivity testing are used in table 7
*7)
	*"Output\Results\ES_a_10b5s_AbrahamSun_formatted.pdf": this is unreported (but is contained in figure 4)
*7a)
	*"Output\Results\ES_a_10b5s_AbrahamSun_yearly_formatted.pdf": this is unreported
	*the results from the sensitivity testing are used in table 7
*8)
	*"Output\Results\ES_a_10b5s_Gardner_formatted.pdf": this is unreported (but is contained in figure 4)
*8a)
	*"Output\Results\ES_a_10b5s_Gardner_yearly_formatted.pdf": this is unreported
	*the results from the sensitivity testing are used in table 7
*9)
	*"Output\Results\ES_a_10b5s_combined_formatted.pdf": this is figure 4
*10)
	*"Output\Results\ES_a_10b5s_Butts_formatted.pdf": this is figure 5
*10a)
	*"Output\Results\ES_a_10b5s_Butts_yearly_formatted.pdf": this is unreported
	*the results from the sensitivity testing are used in table 7
	*"Output\Results\RambachanRoth_CIs_formatted.pdf": this is figure 10

*continue this for the rest!!
*also update individual labels of each section in the code below to match the index above

*-------------------------------------------*






********************************************************************************************

*1) a static staggered DiD

*THIS SECTION PRODUCES THE RESULTS FOR SPECIFICATIONS 1 AND 2 IN TABLE 3

preserve

keep if inrange(dist_from_2023_ULEZ, 5, 10) | (dist_from_2021_ULEZ > 5 & in_2023_ULEZ == 1) | (dist_from_2019_ULEZ > 5 & in_2021_ULEZ == 1)

*generate the ULEZ variable to reflect staggered treatment
*recall here that quarter = number of quarters since Jan 1 2000, so for example 2021 Q4 is quarter = 2021-2000)*4 + 4 = 21*4 + 4
gen ULEZ = 0
replace ULEZ = 1 if in_2021_ULEZ & quarter >= 21*4 + 4
replace ULEZ = 1 if (in_2023_ULEZ & !in_2021_ULEZ) & quarter >= 23*4 + 3

reghdfe log_price ULEZ, absorb(quarter pcsect) vce(cluster pcsect)

outreg2 using "Temp\reg1.doc", replace addtext(Treated Group?, 2021 & 2023, Postcode Sector FEs?, Yes, Quarter FEs?, Yes, Controls?, No)

reghdfe log_price ULEZ $controls, absorb(quarter pcsect) vce(cluster pcsect)

outreg2 using "Temp\reg1.doc", append addtext(Treated Group?, 2021 & 2023, Postcode Sector FEs?, Yes, Quarter FEs?, Yes, Controls?, Yes)

restore





*1a) static staggered DiD for various disaggregations

*THIS SECTION PRODUCES TABLE 4 IN APPENDIX 2

*first by house type

preserve

*standard setup - keep the three analysis regions
keep if inrange(dist_from_2023_ULEZ, 5, 10) | (dist_from_2021_ULEZ > 5 & in_2023_ULEZ == 1) | (dist_from_2019_ULEZ > 5 & in_2021_ULEZ == 1)

*generate a ULEZ dummy for when exposed to the ULEZ
gen ULEZ = 0
replace ULEZ = 1 if in_2021_ULEZ & quarter >= 21*4 + 4
replace ULEZ = 1 if (in_2023_ULEZ & !in_2021_ULEZ) & quarter >= 23*4 + 3

*generate interactions of this dummy with the house type
gen ULEZ_detached = ULEZ*detached
gen ULEZ_semidetached = ULEZ*semidetached
gen ULEZ_terraced = ULEZ*terraced
gen ULEZ_flat = ULEZ*flat

*run the regression, and make the table
reghdfe log_price ULEZ_detached ULEZ_semidetached ULEZ_terraced ULEZ_flat $controls, absorb(quarter pcsect) vce(cluster pcsect)
outreg2 using "Temp\reg1_het.doc", replace addtext(Treated Group?, 2021 & 2023, Postcode Sector FEs?, Yes, Quarter FEs?, Yes, Controls?, Yes)

restore



*now by price quartile

preserve

*same as 1a)
keep if inrange(dist_from_2023_ULEZ, 5, 10) | (dist_from_2021_ULEZ > 5 & in_2023_ULEZ == 1) | (dist_from_2019_ULEZ > 5 & in_2021_ULEZ == 1)

*same as 1a)
gen ULEZ = 0
replace ULEZ = 1 if in_2021_ULEZ & quarter >= 21*4 + 4
replace ULEZ = 1 if (in_2023_ULEZ & !in_2021_ULEZ) & quarter >= 23*4 + 3

*generate an estimation of the cdf for each quarter
bys quarter: cumul log_price, generate(price_cdf)

*classify houses into quartiles based on the value taken by their quarterly cdf estimation (i.e. compare them to other houses sold in the quarter)
gen price_quartile1 = (price_cdf < 0.25)
gen price_quartile2 = (inrange(price_cdf, 0.25, 0.5))
gen price_quartile3 = (inrange(price_cdf, 0.5, 0.75))
gen price_quartile4 = (price_cdf > 0.75)

*generate interactions of the ULEZ dummy with the price quartile dummies
gen ULEZ_quartile1 = ULEZ*price_quartile1
gen ULEZ_quartile2 = ULEZ*price_quartile2
gen ULEZ_quartile3 = ULEZ*price_quartile3
gen ULEZ_quartile4 = ULEZ*price_quartile4

*run the regression, and make the table
reghdfe log_price ULEZ_quartile1 ULEZ_quartile2 ULEZ_quartile3 ULEZ_quartile4 price_quartile1 price_quartile2 price_quartile3 price_quartile4 $controls, absorb(quarter pcsect) vce(cluster pcsect)
outreg2 using "Temp\reg1_het2.doc", replace addtext(Treated Group?, 2021 & 2023, Postcode Sector FEs?, Yes, Quarter FEs?, Yes, Controls?, Yes)

restore




*1b) static staggered DiD for various assumed distances

*THIS SECTION PRODUCES TABLE 5 IN APPENDIX 2

*I run the same regression as above, but suppose parallel trends holds out to 12.5km and 7.5km as well as 10km, and suppose that spillovers end at 7.5km and 2.5km as well as 5km (i.e. vary both by 2.5km either side)

*first look at varying the parallel trends distance
preserve

*generate the ULEZ dummy
gen ULEZ = 0
replace ULEZ = 1 if in_2021_ULEZ & quarter >= 21*4 + 4
replace ULEZ = 1 if (in_2023_ULEZ & !in_2021_ULEZ) & quarter >= 23*4 + 3

*pick the sample for PT ending at 12.5km
keep if inrange(dist_from_2023_ULEZ, 5, 12.5) | (dist_from_2021_ULEZ > 5 & in_2023_ULEZ == 1) | (dist_from_2019_ULEZ > 5 & in_2021_ULEZ == 1)

*do the regression and make the table
reghdfe log_price ULEZ $controls, absorb(quarter pcsect) vce(cluster pcsect)
outreg2 using "Temp\reg1_het3.doc", replace addtext(Parallel Trends Distance, 12.5km, Spillover Distance, 5km, Postcode Sector FEs?, Yes, Quarter FEs?, Yes, Controls?, Yes)

*refine to 7.5km
keep if inrange(dist_from_2023_ULEZ, 5, 7.5) | (dist_from_2021_ULEZ > 5 & in_2023_ULEZ == 1) | (dist_from_2019_ULEZ > 5 & in_2021_ULEZ == 1)

*same again
reghdfe log_price ULEZ $controls, absorb(quarter pcsect) vce(cluster pcsect)
outreg2 using "Temp\reg1_het3.doc", append addtext(Parallel Trends Distance, 7.5km, Spillover Distance, 5km, Postcode Sector FEs?, Yes, Quarter FEs?, Yes, Controls?, Yes)

restore


*now vary the spillover distance
preserve

*make the ULEZ dummy
gen ULEZ = 0
replace ULEZ = 1 if in_2021_ULEZ & quarter >= 21*4 + 4
replace ULEZ = 1 if (in_2023_ULEZ & !in_2021_ULEZ) & quarter >= 23*4 + 3

*pick the sample for spillovers ending at 2.5km
keep if inrange(dist_from_2023_ULEZ, 2.5, 10) | (dist_from_2021_ULEZ > 2.5 & in_2023_ULEZ == 1) | (dist_from_2019_ULEZ > 2.5 & in_2021_ULEZ == 1)

*regress and collect results
reghdfe log_price ULEZ $controls, absorb(quarter pcsect) vce(cluster pcsect)
outreg2 using "Temp\reg1_het3.doc", append addtext(Parallel Trends Distance, 10km, Spillover Distance, 2.5km, Postcode Sector FEs?, Yes, Quarter FEs?, Yes, Controls?, Yes)

*refine to 7.5km
keep if inrange(dist_from_2023_ULEZ, 7.5, 10) | (dist_from_2021_ULEZ > 7.5 & in_2023_ULEZ == 1) | (dist_from_2019_ULEZ > 7.5 & in_2021_ULEZ == 1)

*regress and collect results
reghdfe log_price ULEZ $controls, absorb(quarter pcsect) vce(cluster pcsect)
outreg2 using "Temp\reg1_het3.doc", append addtext(Parallel Trends Distance, 10km, Spillover Distance, 7.5km, Postcode Sector FEs?, Yes, Quarter FEs?, Yes, Controls?, Yes)

restore





********************************************************************************************





*2) a static DiD for 2021

*THIS ANALYSIS PRODUCES THE RESULT FOR SPECIFICATION 4 IN TABLE 3

preserve

*keep only the 2021 analysis zone and the control zone
keep if inrange(dist_from_2023_ULEZ, 5, 10) | (dist_from_2019_ULEZ > 5 & in_2021_ULEZ == 1)

*generate a ULEZ dummy: recall here that quarter = number of quarters since Jan 1 2000, so 2021 Q4 is quarter = 2021-2000)*4 + 4 = 21*4 + 4
gen ULEZ = in_2021_ULEZ & quarter >= 21*4 + 4

*do the simple static DiD regression
reghdfe log_price ULEZ $controls, absorb(quarter pcsect) vce(cluster pcsect)

*collect the results
outreg2 using "Temp\reg1.doc", append addtext(Treated Group?, 2021, Postcode Sector FEs?, Yes, Quarter FEs?, Yes, Controls?, Yes)

restore
*very negative and significant




********************************************************************************************





*3) a static DiD for 2023

*THIS ANALYSIS PRODUCES THE RESULT FOR SPECIFICATION 3 IN TABLE 3

*analogous procedure as above
preserve

keep if inrange(dist_from_2023_ULEZ, 5, 10) | (dist_from_2021_ULEZ > 5 & in_2023_ULEZ == 1)

gen ULEZ = in_2023_ULEZ & quarter >= 23*4 + 3

reghdfe log_price ULEZ $controls, absorb(quarter pcsect) vce(cluster pcsect)

outreg2 using "Temp\reg1.doc", append addtext(Treated Group?, 2023, Postcode Sector FEs?, Yes, Quarter FEs?, Yes, Controls?, Yes)

restore
*weakly significant, and negative





********************************************************************************************






*4) a dynamic DiD for 2023

*THIS ANALYSIS PRODUCES THE RESULT IN FIGURE 9 IN APPENDIX 5

preserve

*keep the 2023 analysis zone and the control zone
keep if inrange(dist_from_2023_ULEZ, 5, 10) | (dist_from_2021_ULEZ > 5 & in_2023_ULEZ == 1)

*generate cohort variable giving quarter of first treatment
gen cohort = 0
replace cohort = 23*4 + 3 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0

*generate event time variable
gen Stime = cohort if cohort>0
gen etime = quarter-Stime

*generate the event time dummies
tab etime, gen(et)
*there are 40 et dummies, with etime = 0 being at et35

*ensure the event time dummies are always 0 for the never treated group
foreach var of varlist et1-et40  {
	replace `var' = 0 if cohort==0
}

*define a set of leads and lags
*the graph will show a quarter of what looks like anticipatory behaviour - to deal with this, set the base period to be two quarters before the policy, rather than one
global leads "et1-et32"
global lags "et34-et40"

*run the regressions, and use the graphprep program written at the start to prepare the coefficients for graphing
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) vce(cluster pcsect)
*this corresponds to first dummy = 1, last dummy = 40, baseline dummy = 33, first treatment dummy = 35
graphprep mod1 1 40 33 35

*plot the graph of the coefficients
twoway (rcap mod1_up mod1_lo mod1_t, lc("143 45 86"%50)) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-30(10)0) xtick(-34(1)5, tlength(relative0p6)) xline(-0.5, lc(black*.80%50) lp(dash)) title("2023 Dynamic DiD") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

*save the graph
graph export "Output\Results\DiD_2023_a_10b5s_formatted.pdf", as(pdf) replace

restore




********************************************************************************************






*5) a dynamic DiD for 2021 

*THIS ANALYSIS PRODUCES THE RESULT IN FIGURE 8 IN APPENDIX 5

*the process is exactly analogous to 4) - refer to this for details
preserve

*generate new analysis sample
keep if inrange(dist_from_2023_ULEZ, 5, 10) | (dist_from_2019_ULEZ > 5 & in_2021_ULEZ == 1)

gen cohort = 0
replace cohort = 21*4 + 4 if in_2021_ULEZ == 1

gen Stime = cohort if cohort>0
gen etime = quarter-Stime

tab etime, gen(et)
*there are 40 et dummies, with etime = 0 now being at et28

foreach var of varlist et1-et40  {
	replace `var' = 0 if cohort==0
}

*again rebase to two quarters prior to implementation - we see the same sharp drop before this point as in 2023 (giving more support to the claim that it comes from anticipation rather than randomness)
global leads "et1-et25"
global lags "et27-et40"

*regress and prepare the coefficients for graphing
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) vce(cluster pcsect)
graphprep mod1 1 40 26 28

*plot and the graphs
twoway (rcap mod1_up mod1_lo mod1_t, lc("216 17 89")) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-20(10)10) xtick(-27(1)12, tlength(relative0p6)) xline(-0.5, lc(black*.80%50) lp(dash)) title("2021 Dynamic DiD") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

graph export "Output\Results\DiD_2021_a_10b5s_formatted.pdf", as(pdf) replace

restore




********************************************************************************************





*6) TWFE event study for both regions combined

*THIS ANALYSIS PRODUCES THE TWFE RESULT IN FIGURE 4 AND APPENDIX 3

*the procedure is very similar to 4) and 5)
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

*run the regression and prepare the graphs for plotting with the graphprep function
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) vce(cluster pcsect)
graphprep mod1 1 47 33 35

*plot the graphs
twoway (rcap mod1_up mod1_lo mod1_t, lc("33 131 128")) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6)) yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-30(10)10) xtick(-34(1)12, tlength(relative0p6)) xline(-0.5, lc(black*.80%50) lp(dash)) title("Staggered DiD for 2021 and 2023") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

*save the graph
graph export "Output\Results\ES_a_10b5s_formatted.pdf", as(pdf) replace

*save regression table for selected coefficients
outreg2 using "Temp\reg2.doc", replace addtext(Estimand, Treatment Effect, Estimator, TWFE OLS, Controls?, Yes) keep(et30 et31 et32 et34 et35 et36 et37 et38 et39 et40)





*6a) check robustness of parallel trends, using honestdid, having aggregated the periods as suggested in the paper

*THIS ANALYSIS PRODUCES THE TWFE PARALLEL TRENDS ANALYSIS IN APPENDIX 6

*first rerun the regression using yearly time periods rather than quarterly
drop cohort
drop et*
drop Stime

*make a new yearly cohort variable
gen cohort = 0
replace cohort = 2023 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0
replace cohort = 2021 if in_2021_ULEZ == 1

*make a yearly event time variable
gen Stime = cohort if cohort>0
gen etime = saleyear-Stime

*use it to create a set of event time dummies (et9 corresponds to the first year of treatment)
tab etime, gen(et)

*set the event time dummies to 0 for the never treated group
foreach var of varlist et1-et12  {
	replace `var' = 0 if cohort==0
}

*define leads and lags for the regression
global leads "et1-et7"
global lags "et9-et12"

*run the regression, and prepare the coefficients for plotting
reghdfe log_price $leads $lags $controls, absorb(saleyear pcsect) vce(cluster pcsect)
graphprep mod1 1 12 8 9

*plot the graph
twoway (rcap mod1_up mod1_lo mod1_t, lc("33 131 128")) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.1(.1).1, format(%3.2f) angle(0)) ytick(-.1(0.025).1, tlength(relative0p6)) yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (years after first treatment, j)") xlabel(-8(1)3) xline(-0.5, lc(black*.80%50) lp(dash)) title("Staggered DiD for 2021 and 2023") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = year", span)

*save the graph
graph export "Output\Results\ES_a_10b5s_yearly_formatted.pdf", as(pdf) replace

*collect the correct matrices from the regression output, containing just the beta coefficient estimates and their covariances
mat b = e(b)[1, 1..11]
mat v = e(V)[1..11, 1..11]

*now use relative magnitude restrictions from Rambachan and Roth (2023), using the last four pre-treatment coefficients (as we only have four post-treatment coefficients)

*in the following, I loop through the post-treatment coefficients, and use the honestdid package provided by the authors to calculate, for M = 1, the confidence interval for the coefficient estimate accounting for the weakening of the parallel trends assumption that M=1 implies
*please refer to the paper for full details of this method

*create a matrix to store the results
mat def CIs = J(4,3,0)

*set up the loop through the post-treatment coefficients
forvalues i = 1/4 {

	mat CIs[`i', 1] = `i' - 1

	*set up the correct input vector for the honestdid package, depending on the value of i
	if `i' == 1 {
		matrix def l_vec = 1 \ 0 \ 0 \ 0
	}
	if `i' == 2 {
		matrix def l_vec = 0 \ 1 \ 0 \ 0
	}
	if `i' == 3 {
		matrix def l_vec = 0 \ 0 \ 1 \ 0
	}
	if `i' == 4 {
		matrix def l_vec = 0 \ 0 \ 0 \ 1
	}

	*run the honestdid analysis, with the equal amounts of pre- and post-treatment coefficients, the vector of coefficient estimates and covariance matrix saved previously, the input vector specified above, and M = 1
	honestdid, pre(4 5 6 7) post(8 9 10 11) b(b) vcov(v) l_vec(l_vec) mvec(1)

	*get the upper and lower bounds from the corresponding mata object
	mata: st_numscalar("lb", `s(HonestEventStudy)'.CI[2,2])
	mata: st_numscalar("ub", `s(HonestEventStudy)'.CI[2,3])

	*save the scalars to the matrix set up at the start
	mat CIs[`i', 2] = lb
	mat CIs[`i', 3] = ub

}

*list the matrix, and save its columns as variables
mat list CIs
svmat CIs

*plot the confidence intervals for each coefficient, and save it so the graphs for all estimators can all be plotted together
twoway (rcap CIs2 CIs3 CIs1, lc("33 131 128") lwidth(0.5) msize(large)), ytitle("95% CI for coefficient estimate") ylabel(-.15(.05).05, format(%3.2f) angle(0)) ytick(-.15(0.025).05, tlength(relative0p6)) yline(0, lc(black%50)) xtitle("Years since first treatment") title("TWFE OLS") legend(order(1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) saving("Temp\TWFEOLS", replace)


*now conduct sensitivity analysis for j = 1 and j = 2 - report the maximal M such that the coefficients are still significant
matrix def l_vec = 0 \ 1 \ 0 \ 0
honestdid, pre(4 5 6 7) post(8 9 10 11) b(b) vcov(v) l_vec(l_vec) mvec(0.75(0.05)1)
*greatest M s.t. significance is retained at 5% = 0.85 (nearest 0.05)

matrix def l_vec = 0 \ 0 \ 1 \ 0
honestdid, pre(4 5 6 7) post(8 9 10 11) b(b) vcov(v) l_vec(l_vec) mvec(0.65(0.05)0.9)
*greatest M s.t. significance is retained at 5% = 0.75 (nearest 0.05)

restore







********************************************************************************************





*7) event study, now using Abraham and Sun (2021) interacted-weighted ATT estimator and the eventstudyinteract package

*THIS ANALYSIS PRODUCES THE ABRAHAM AND SUN (2021) RESULT IN FIGURE 4 AND APPENDIX 3

*the procedure is very similar to 6) - refer to this for details (any differences are explained here)
preserve 

keep if (inrange(dist_from_2023_ULEZ, 5, 10)) | (dist_from_2021_ULEZ > 5 & in_2023_ULEZ == 1) | (dist_from_2019_ULEZ > 5 & in_2021_ULEZ == 1)

gen cohort = 0
replace cohort = 23*4 + 3 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0
replace cohort = 21*4 + 4 if in_2021_ULEZ == 1

gen Stime = cohort if cohort != 0
gen etime = quarter - Stime
tab etime, gen(et)

foreach var of varlist et1-et47  {
replace `var' = 0 if cohort==0
}

*make the necessary corrections and variables to use the eventstudyinteract package (provided by the authors)
replace cohort = . if cohort == 0
gen never_treated = cohort == .
egen long pcsect_numeric = group(pcsect)

*define lags to account for anticipation, as usual
global leads "et1-et32"
global lags "et34-et47"

*note the new more general program for plotting graphs after regressions with eventstudyinteract
eventstudyinteract log_price $leads $lags, control_cohort(never_treated) cohort(cohort) absorb(quarter pcsect_numeric) covariates($controls) vce(cluster pcsect_numeric)
graphprepgeneral mod1 1 47 33 35 e(b_iw) e(V_iw)

*plot the graph
twoway (rcap mod1_up mod1_lo mod1_t, lc("33 131 128")) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6)) yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-30(10)10) xtick(-34(1)12, tlength(relative0p6)) xline(-0.5, lc(black*.80%50) lp(dash)) title("Staggered DiD, using Abraham and Sun (2021)") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

*save the graph
graph export "Output\Results\ES_AbrahamSun_a_10b5s_formatted.pdf", as(pdf) replace

*save regression table
outreg2 using "Temp\reg2.doc", append addtext(Estimand, Treatment Effect, Estimator, Abraham and Sun (2021), Controls?, Yes) keep(et30 et31 et32 et34 et35 et36 et37 et38 et39 et40)





*7a) check robustness of parallel trends, using honestdid, having aggregated the periods as suggested in the paper

*THIS ANALYSIS PRODUCES THE ABRAHAM AND SUN (2021) PARALLEL TRENDS ANALYSIS IN APPENDIX 6

*the procedure here is very similar to 6a) - please refer to this for details (any differences are annotated here)

*first rerun the regression on yearly time periods, rather than quarterly
drop cohort
drop Stime et*

gen cohort = 0
replace cohort = 2023 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0
replace cohort = 2021 if in_2021_ULEZ == 1

gen Stime = cohort if cohort != 0
gen etime = saleyear - Stime
tab etime, gen(et)

foreach var of varlist et1-et12  {
replace `var' = 0 if cohort==0
}

*make the necessary corrections and variables to use eventstudyinteract
replace cohort = . if cohort == 0
gen never_treated = cohort == .
egen long pcsect_numeric = group(pcsect)

global leads "et1-et7"
global lags "et9-et12"

eventstudyinteract log_price $leads $lags, control_cohort(never_treated) cohort(cohort) absorb(quarter pcsect_numeric) covariates($controls) vce(cluster pcsect_numeric)
graphprepgeneral mod1 1 12 8 9 e(b_iw) e(V_iw)

twoway (rcap mod1_up mod1_lo mod1_t, lc("33 131 128")) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.1(.1).1, format(%3.2f) angle(0)) ytick(-.1(0.025).1, tlength(relative0p6)) yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (years after first treatment, j)") xlabel(-8(1)3) xline(-0.5, lc(black*.80%50) lp(dash)) title("Staggered DiD, using Abraham and Sun (2021)") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = year", span)

graph export "Output\Results\ES_AbrahamSun_a_10b5s_yearly_formatted.pdf", as(pdf) replace

*now use relative magnitude restrictions from Rambachan and Roth (2023), using the last four pre-treatment coefficients (as we only have four post-treatment coefficients

*do it at M=1, save the results, and make a graph, as before

mat drop CIs
drop CIs*

mat def CIs = J(4,3,0)

forvalues i = 1/4 {

	mat CIs[`i', 1] = `i' - 1

	if `i' == 1 {
		matrix def l_vec = 1 \ 0 \ 0 \ 0
	}
	if `i' == 2 {
		matrix def l_vec = 0 \ 1 \ 0 \ 0
	}
	if `i' == 3 {
		matrix def l_vec = 0 \ 0 \ 1 \ 0
	}
	if `i' == 4 {
		matrix def l_vec = 0 \ 0 \ 0 \ 1
	}

	honestdid, pre(4 5 6 7) post(8 9 10 11) b(e(b_iw)) vcov(e(V_iw)) l_vec(l_vec) mvec(1)

	mata: st_numscalar("lb", `s(HonestEventStudy)'.CI[2,2])
	mata: st_numscalar("ub", `s(HonestEventStudy)'.CI[2,3])

	mat CIs[`i', 2] = lb
	mat CIs[`i', 3] = ub

}

mat list CIs
svmat CIs

twoway (rcap CIs2 CIs3 CIs1, lc("33 131 128") lwidth(0.5) msize(large)), ytitle("95% CI for coefficient estimate") ylabel(-.15(.05).05, format(%3.2f) angle(0)) ytick(-.15(0.025).05, tlength(relative0p6)) yline(0, lc(black%50)) xtitle("Years since first treatment") title("Abraham and Sun (2021)") legend(order(1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) saving("Temp\AbrahamSun", replace)


*also conduct sensitivity analysis for j = 1 and j = 2, as before
matrix def l_vec = 0 \ 1 \ 0 \ 0
honestdid, pre(4 5 6 7) post(8 9 10 11) b(e(b_iw)) vcov(e(V_iw)) l_vec(l_vec) mvec(0.65(0.05)0.9)
*greatest M s.t. significance is retained at 5% = 0.75 (nearest 0.05)

matrix def l_vec = 0 \ 0 \ 1 \ 0
honestdid, pre(4 5 6 7) post(8 9 10 11) b(e(b_iw)) vcov(e(V_iw)) l_vec(l_vec) mvec(1(0.05)1.25)
*greatest M s.t. significance is retained at 5% = 1 (nearest 0.05)

restore





********************************************************************************************





*8) event study, using Gardner (2021) did2s estimator

*THIS ANALYSIS PRODUCES THE GARDNER (2021) RESULT IN FIGURE 4 AND APPENDIX 3

*the proceduce is again similar to 6) - refer to this for details (any differences are annotated here)

preserve

keep if (inrange(dist_from_2023_ULEZ, 5, 10)) | (dist_from_2021_ULEZ > 5 & in_2023_ULEZ == 1) | (dist_from_2019_ULEZ > 5 & in_2021_ULEZ == 1)

*define a variable saying when each region is being treated
gen treatment = 0
replace treatment = 1 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0 & quarter >= 23*4 + 2
replace treatment = 1 if in_2021_ULEZ == 1 & quarter >= 21*4 + 3

*make the pcsect variable numeric
egen long pcsect_numeric = group(pcsect)

*make the event time dummies through the cohort variable, as usual
gen cohort = 0
replace cohort = 23*4 + 3 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0
replace cohort = 21*4 + 4 if in_2021_ULEZ == 1

gen Stime = cohort if cohort != 0
gen etime = quarter - Stime
tab etime, gen(et)

foreach var of varlist et1-et47  {
replace `var' = 0 if cohort==0
}

*define the leads and lags, accounting for anticipation
global leads "et1-et32"
global lags "et34-et47"

*run the regression, using the package provided by the author (and Kyle Butts)
did2s log_price, first_stage($controls i.pcsect_numeric i.quarter) second_stage($leads $lags) treatment(treatment) cluster(pcsect_numeric)
graphprepgeneral mod1 1 47 33 35 e(b) e(V)

*plot the graph
twoway (rcap mod1_up mod1_lo mod1_t, lc("33 131 128")) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6)) yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (j)") xlabel(-30(10)10) xtick(-34(1)12, tlength(relative0p6)) xline(-0.5, lc(black*.80%50) lp(dash)) title("Staggered DiD, using Gardner (2021)") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

graph export "Output\Results\ES_Gardner_10b5s_formatted.pdf", as(pdf) replace

*save regression table
outreg2 using "Temp\reg2.doc", append addtext(Estimand, Treatment Effect, Estimator, Gardner (2021), Controls?, Yes) keep(et30 et31 et32 et34 et35 et36 et37 et38 et39 et40)





*8a) again check robustness of parallel trends, using honestdid, having aggregated the periods as suggested in the paper

*THIS ANALYSIS PRODUCES THE GARDNER (2021) PARALLEL TRENDS ANALYSIS IN APPENDIX 6

*same prodecure as 6a) - refer to this for details

*frst rerun the regression on yearly time periods, rather than quarterly
drop cohort
drop treatment
drop Stime et*

*define a variable saying when each region is being treated
gen treatment = 0
replace treatment = 1 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0 & saleyear >= 2023
replace treatment = 1 if in_2021_ULEZ == 1 & saleyear >= 2021

gen cohort = 0
replace cohort = 2023 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0
replace cohort = 2021 if in_2021_ULEZ == 1

gen Stime = cohort if cohort != 0
gen etime = saleyear - Stime
tab etime, gen(et)

foreach var of varlist et1-et12  {
replace `var' = 0 if cohort==0
}

egen long pcsect_numeric = group(pcsect)

global leads "et1-et7"
global lags "et9-et12"

did2s log_price, first_stage($controls i.pcsect_numeric i.quarter) second_stage($leads $lags) treatment(treatment) cluster(pcsect_numeric)
graphprepgeneral mod1 1 12 8 9 e(b) e(V)

twoway (rcap mod1_up mod1_lo mod1_t, lc("33 131 128")) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.1(.1).1, format(%3.2f) angle(0)) ytick(-.1(0.025).1, tlength(relative0p6)) yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (years after first treatment, j)") xlabel(-8(1)3) xline(-0.5, lc(black*.80%50) lp(dash)) title("Staggered DiD, using Gardner (2021)") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = year", span)

graph export "Output\Results\ES_Gardner_10b5s_yearly_formatted.pdf", as(pdf) replace


*now use relative magnitude restrictions from Rambachan and Roth (2023), using the last four pre-treatment coefficients (as we only have four post-treatment coefficients

*do it at M=1, save the results, and make a graph

*refer to 6a) for details

mat drop CIs
drop CIs*

mat def CIs = J(4,3,0)

forvalues i = 1/4 {

	mat CIs[`i', 1] = `i' - 1

	if `i' == 1 {
		matrix def l_vec = 1 \ 0 \ 0 \ 0
	}
	if `i' == 2 {
		matrix def l_vec = 0 \ 1 \ 0 \ 0
	}
	if `i' == 3 {
		matrix def l_vec = 0 \ 0 \ 1 \ 0
	}
	if `i' == 4 {
		matrix def l_vec = 0 \ 0 \ 0 \ 1
	}

	honestdid, pre(4 5 6 7) post(8 9 10 11) b(e(b)) vcov(e(V)) l_vec(l_vec) mvec(1)

	mata: st_numscalar("lb", `s(HonestEventStudy)'.CI[2,2])
	mata: st_numscalar("ub", `s(HonestEventStudy)'.CI[2,3])

	mat CIs[`i', 2] = lb
	mat CIs[`i', 3] = ub

}

mat list CIs
svmat CIs

twoway (rcap CIs2 CIs3 CIs1, lc("33 131 128") lwidth(0.5) msize(large)), ytitle("95% CI for coefficient estimate") ylabel(-.15(.05).05, format(%3.2f) angle(0)) ytick(-.15(0.025).05, tlength(relative0p6)) yline(0, lc(black%50)) xtitle("Years since first treatment") title("Gardner (2021)") legend(order(1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) saving("Temp\Gardner", replace)


*also conduct sensitivity analysis for j = 1 and j = 2
matrix def l_vec = 0 \ 1 \ 0 \ 0
honestdid, pre(4 5 6 7) post(8 9 10 11) b(e(b)) vcov(e(V)) l_vec(l_vec) mvec(1(0.05)1.25)
*greatest M s.t. significance is retained at 5% = 1 (nearest 0.05)

matrix def l_vec = 0 \ 0 \ 1 \ 0
honestdid, pre(4 5 6 7) post(8 9 10 11) b(e(b)) vcov(e(V)) l_vec(l_vec) mvec(1.1(0.05)1.35)
*greatest M s.t. significance is retained at 5% = 1.15 (nearest 0.05)

restore





********************************************************************************************





*9) now plot all three of the above results together on one graph

*THIS ANALYSIS PRODUCES FIGURE 4

*this code is just a repeat of 6), 7) and 8), used to produce figure 4
preserve

keep if (inrange(dist_from_2023_ULEZ, 5, 10)) | (dist_from_2021_ULEZ > 5 & in_2023_ULEZ == 1) | (dist_from_2019_ULEZ > 5 & in_2021_ULEZ == 1)

gen cohort = 0
replace cohort = 23*4 + 3 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0
replace cohort = 21*4 + 4 if in_2021_ULEZ == 1

gen Stime = cohort if cohort>0
gen etime = quarter-Stime

tab etime, gen(et)
*there are 47 et dummies, with etime = 0 being at et35

foreach var of varlist et1-et47  {
	replace `var' = 0 if cohort==0
}

*again just define leads and lags to account for the anticipatory period, as before
global leads "et1-et32"
global lags "et34-et47"

reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) vce(cluster pcsect)
graphprep mod1 1 47 33 35


*extra stuff for A+S

replace cohort = . if cohort == 0
gen never_treated = cohort == .

egen long pcsect_numeric = group(pcsect)

eventstudyinteract log_price $leads $lags, control_cohort(never_treated) cohort(cohort) absorb(quarter pcsect_numeric) covariates($controls) vce(cluster pcsect_numeric)
graphprepgeneral mod2 1 47 33 35 e(b_iw) e(V_iw)


*extra stuff for Gardner

gen treatment = 0
replace treatment = 1 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0 & quarter >= 23*4 + 2
replace treatment = 1 if in_2021_ULEZ == 1 & quarter >= 21*4 + 3

did2s log_price, first_stage($controls i.pcsect_numeric i.quarter) second_stage($leads $lags) treatment(treatment) cluster(pcsect_numeric)
graphprepgeneral mod3 1 47 33 35 e(b) e(V)


*separate the points on the graphs slightly, to make them more readable
replace mod1_t = mod1_t-0.075 if mod1_t!=-2
replace mod2_t = mod2_t+0.075 if mod2_t!=-2

*now plot the graphs
twoway (rcap mod1_up mod1_lo mod1_t, lc("33 131 128%20")) (connected mod1 mod1_t, lc("33 131 128") mc("33 131 128") ms(o) msize(vsmall)) (rcap mod2_up mod2_lo mod2_t, lc("143 45 86%20")) (connected mod2 mod2_t, lc("143 45 86") mc("143 45 86") ms(o) msize(vsmall)) (rcap mod3_up mod3_lo mod3_t, lc("216 17 89%20")) (connected mod3 mod3_t, lc("216 17 89") mc("216 17 89") ms(o) msize(vsmall)), ytitle("Estimate of {&beta}{subscript:j}") ylabel(-.1(.05).1, format(%3.2f) angle(0)) ytick(-.1(0.025).1, tlength(relative0p6)) yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-30(10)10) xtick(-34(1)12, tlength(relative0p6)) xline(-0.5, lc(black*.80%50) lp(dash)) title("Staggered DiD for 2021 and 2023") legend(order(2 "Standard TWFE" 4 "Abraham and Sun (2021)" 6 "Gardner (2021)")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, bars represent 95% CIs, and unit of time = quarter", span)

graph export "Output\Results\ES_all_10b5s_formatted.pdf", as(pdf) replace

restore





********************************************************************************************





*10) now do Butts (2024), with 10km spillovers and 15km outer distance, as justified in the paper

*THIS ANALYSIS PRODUCES FIGURE 5 AND THE BUTTS (2024) RESULT IN APPENDIX 3

*the code is based on the Butts (2024) paper and the replication file for his community health centre example, available at https://github.com/kylebutts/Spatial-Spillover/blob/master/code/CHC/analysis.R - please see these materials for details

preserve

*I keep a new larger sample than before, as no rings need to be cut out: I am dealing with spillovers more explicitly through the estimation
*we still omit houses near the 2019 zone because this zone had ongoing treatment before that point
keep if dist_from_2023_ULEZ <= 15 & dist_from_2019_ULEZ > 5

*define a variable saying when each region is being treated
gen treatment = 0
replace treatment = 1 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0 & quarter >= 23*4 + 3
replace treatment = 1 if in_2021_ULEZ == 1 & quarter >= 21*4 + 4

*define a variable saying when each region is being subject to spillovers (under the assumption that spillovers end at 5km) - i.e. is it within 5km of an active ULEZ?
gen exposed = 0
replace exposed = 1 if dist_from_2021_ULEZ < 10 & quarter >= 21*4 + 4
replace exposed = 1 if dist_from_2023_ULEZ < 10 & quarter >= 23*4 + 3

*generate a variable giving when exposed but not directly treated (i.e. when exposed = 1 and treatment = 0)
gen spill = exposed * (1 - treatment)

*make the pcsect variable numeric
egen long pcsect_numeric = group(pcsect)

*now we have to construct the event time dummies differently: we have to do them separately for regions subject to spillovers
*make a slight amendment in how they are coded, as Butts (2024) does
gen cohort = 0
replace cohort = 23*4 + 3 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0
replace cohort = 21*4 + 4 if in_2021_ULEZ == 1

*generate event time dummies for each j (these are the D_it^k as referenced in the paper)
gen Stime = cohort if cohort != 0
gen etime = quarter - Stime
tab etime, gen(et)

*set each to zero for never treated observations
foreach var of varlist et1-et47  {
replace `var' = 0 if cohort==0
}

*now define spillover event-time dummies in the same way, for all houses who are exposed to spillovers but not direct treatment
*we specify that treatment = 0 so that we don't get overlap between D_it and S_it (as Butts does)
gen spill_cohort = 0
replace spill_cohort = 23*4 + 3 if dist_from_2023_ULEZ < 10 & treatment == 0
replace spill_cohort = 21*4 + 4 if dist_from_2021_ULEZ < 10 & treatment == 0

*generate spillover event time dummies for each j (these are the S_it^k)
gen spill_Stime = spill_cohort if spill_cohort != 0
gen spill_etime = quarter - spill_Stime
tab spill_etime, gen(spill_et)

*set each to zero for never treated observations
foreach var of varlist spill_et1-spill_et47  {
replace `var' = 0 if spill_cohort==0
}

*define the leads and lags, leaving a base category at -2 (like Butts)
global leads "et1-et32"
global lags "et34-et47"

*define spillover lags and leads, leaving no base category (i.e. those who were never exposed to spillovers) like Butts does
global spill_leads "spill_et1-spill_et33"
global spill_lags "spill_et34-spill_et47"

*run the regression - note that our treatment variable is exposed, giving whether subject to spillovers or direct treatment (as Butts does)
did2s log_price, first_stage($controls i.pcsect_numeric i.quarter) second_stage($leads $lags $spill_leads $spill_lags) treatment(exposed) cluster(pcsect_numeric)

*save matrices to input into the general plotting function
*in particular, keep only spillovers up to 6 periods after treatment - this is because past that point we have very few observations (i.e. less than 5km from 2021, but not in 2023)
mat def etcoefs = e(b)[1,1..46]
mat def etse = e(V)[1..46,1..46]
mat def spill_etcoefs = e(b)[1,47..87]
mat def spill_etse = e(V)[47..87,47..87]

graphprepgeneral mod1 1 47 33 35 etcoefs etse
graphprepgeneral mod2 1 41 100 35 spill_etcoefs spill_etse
*note that in the graphprepgeneral program I wrote, if there is no base (like in the spillover case here), we can just set the 'base' value really high - this is clear from how the program works

*plot the graph, and save it
twoway (rcap mod1_up mod1_lo mod1_t, lc("33 131 128%20")) (connected mod1 mod1_t, lc("33 131 128") mc("33 131 128") ms(o) msize(vsmall)) (rcap mod2_up mod2_lo mod2_t, lc("143 45 86%20")) (connected mod2 mod2_t, lc("143 45 86") mc("143 45 86") ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.1(.05).1, format(%3.2f) angle(0)) ytick(-.1(0.05).1, tlength(relative0p6)) yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (j)") xlabel(-30(10)10) xtick(-34(1)12, tlength(relative0p6)) xline(-0.5, lc(black*.80%50) lp(dash)) title("Staggered DiD, using Butts (2024)") legend(order(2 "Treatment" 4 "Spillover")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, bars represent 95% CIs, and unit of time = quarter", span)

graph export "Output\Results\ES_Butts_15b10s_formatted.pdf", as(pdf) replace

*save regression table
outreg2 using "Temp\reg2.doc", append addtext(Estimand, Treatment Effect, Estimator, Butts (2024), Controls?, Yes) keep(et30 et31 et32 et34 et35 et36 et37 et38 et39 et40)
outreg2 using "Temp\reg2.doc", append addtext(Estimand, Spillover Effect, Estimator, Butts (2024), Controls?, Yes) keep(spill_et30 spill_et31 spill_et32 spill_et34 spill_et35 spill_et36 spill_et37 spill_et38 spill_et39 spill_et40)






*10a) again check robustness of parallel trends, using honestdid, having aggregated the periods as suggested in the paper

*THIS ANALYSIS PRODUCES THE BUTTS (2024) PARALLEL TRENDS ANALYSIS IN APPENDIX 6

*now do the same as in 10), but yearly - exactly in the same style as 6a). See the notes in 10) and 6a) for details

*make the yearly treatment variable
drop treatment
gen treatment = 0
replace treatment = 1 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0 & saleyear >= 2023
replace treatment = 1 if in_2021_ULEZ == 1 & saleyear >= 2021

*make the yearly exposed variable
drop exposed
gen exposed = 0
replace exposed = 1 if dist_from_2021_ULEZ < 10 & saleyear >= 2021
replace exposed = 1 if dist_from_2023_ULEZ < 10 & saleyear >= 2023

*make the yearly spillover variable
drop spill
gen spill = exposed * (1 - treatment)

*make the yearly cohort variable
drop cohort
gen cohort = 0
replace cohort = 2023 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0
replace cohort = 2021 if in_2021_ULEZ == 1

*make the yearly event time dummies
drop Stime
drop et*
gen Stime = cohort if cohort != 0
gen etime = saleyear - Stime
tab etime, gen(et)

*set to 0 for not treated
foreach var of varlist et1-et12  {
replace `var' = 0 if cohort==0
}

*make the yearly spillover cohort variable
drop spill_cohort
gen spill_cohort = 0
replace spill_cohort = 2023 if dist_from_2023_ULEZ < 10 & treatment == 0
replace spill_cohort = 2021 if dist_from_2021_ULEZ < 10 & treatment == 0

*make the yearly spillover event time dummies
drop spill_Stime
drop spill_et*
gen spill_Stime = spill_cohort if spill_cohort != 0
gen spill_etime = saleyear - spill_Stime
tab spill_etime, gen(spill_et)

*set to zero for non treated
foreach var of varlist spill_et1-spill_et12  {
replace `var' = 0 if spill_cohort==0
}

*define the yearly direct treatment and spillover lags and leads
global leads "et1-et7"
global lags "et9-et12"

*for spillover, we don't include a base period, as in the paper and replication files
global spill_leads "spill_et1-spill_et8"
global spill_lags "spill_et9-spill_et12"

*run the regression
did2s log_price, first_stage($controls i.pcsect_numeric i.quarter) second_stage($leads $lags $spill_leads $spill_lags) treatment(exposed) cluster(pcsect_numeric)

*define matrices containing the direct and spillover coefficients and covariances
mat def etcoefs = e(b)[1,1..11]
mat def etse = e(V)[1..11,1..11]
mat def spill_etcoefs = e(b)[1,12..21]
mat def spill_etse = e(V)[12..21,12..21]

*prepare them for plotting
graphprepgeneral mod1 1 12 8 9 etcoefs etse
graphprepgeneral mod2 1 10 100 9 spill_etcoefs spill_etse

*plot the graph, and save it
twoway (rcap mod1_up mod1_lo mod1_t, lc("33 131 128%20")) (connected mod1 mod1_t, lc("33 131 128") mc("33 131 128") ms(o) msize(vsmall)) (rcap mod2_up mod2_lo mod2_t, lc("143 45 86%20")) (connected mod2 mod2_t, lc("143 45 86") mc("143 45 86") ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.1(.05).1, format(%3.2f) angle(0)) ytick(-.1(0.05).1, tlength(relative0p6)) yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (j)") xlabel(-8(1)3) xline(-0.5, lc(black*.80%50) lp(dash)) title("Staggered DiD, using Butts (2024)") legend(order(2 "Treatment" 4 "Spillover")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = year", span)

graph export "Output\Results\ES_Butts_15b10s_yearly_formatted.pdf", as(pdf) replace


*now use relative magnitude restrictions from Rambachan and Roth (2023), using the last four pre-treatment coefficients (as we only have four post-treatment coefficients
*do it at M=1, save the results, and make a graph

*exactly like 6a) from here

mat drop CIs
drop CIs*

mat def CIs = J(4,3,0)

forvalues i = 1/4 {

	mat CIs[`i', 1] = `i' - 1

	if `i' == 1 {
		matrix def l_vec = 1 \ 0 \ 0 \ 0
	}
	if `i' == 2 {
		matrix def l_vec = 0 \ 1 \ 0 \ 0
	}
	if `i' == 3 {
		matrix def l_vec = 0 \ 0 \ 1 \ 0
	}
	if `i' == 4 {
		matrix def l_vec = 0 \ 0 \ 0 \ 1
	}

	honestdid, pre(4 5 6 7) post(8 9 10 11) b(etcoefs) vcov(etse) l_vec(l_vec) mvec(1)

	mata: st_numscalar("lb", `s(HonestEventStudy)'.CI[2,2])
	mata: st_numscalar("ub", `s(HonestEventStudy)'.CI[2,3])

	mat CIs[`i', 2] = lb
	mat CIs[`i', 3] = ub

}

mat list CIs
svmat CIs

twoway (rcap CIs2 CIs3 CIs1, lc("33 131 128") lwidth(0.5) msize(large)), ytitle("95% CI for coefficient estimate") ylabel(-.15(.05).05, format(%3.2f) angle(0)) ytick(-.15(0.025).05, tlength(relative0p6)) yline(0, lc(black%50)) xtitle("Years since first treatment") title("Butts (2024)") legend(order(1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) saving("Temp\Butts", replace)

*also conduct sensitivity analysis for j = 1 and j = 2
matrix def l_vec = 0 \ 1 \ 0 \ 0
honestdid, pre(4 5 6 7) post(8 9 10 11) b(etcoefs) vcov(etse) l_vec(l_vec) mvec(1(0.05)1.25)
*greatest M s.t. significance is retained at 5% = 1.15 (nearest 0.05)

matrix def l_vec = 0 \ 0 \ 1 \ 0
honestdid, pre(4 5 6 7) post(8 9 10 11) b(etcoefs) vcov(etse) l_vec(l_vec) mvec(1(0.05)1.25)
*greatest M s.t. significance is retained at 5% = 1.15 (nearest 0.05)


*finally, since all the four Rambachan and Roth (2023) graphs from 6a), 7a), 8a) and 10a) are done, we can now combine them into one graph and save it!

graph combine "Temp\TWFEOLS" "Temp\AbrahamSun" "Temp\Gardner" "Temp\Butts", xsize(8) ysize(6) graphregion(fcolor(white) ifcolor(white)) title("95% Confidence Intervals from Rambachan and Roth (2023)", size(*0.9)) note("Models reestimated at the yearly level, with equal number of pre- and post-treatment coefficients", size(vsmall) span)

graph export "Output\Results\RambachanRoth_CIs_formatted.pdf", as(pdf) replace

restore





********************************************************************************************





*11) now do the confirmatory analysis using control regions around the UK - first for 2021

*THIS ANALYSIS PRODUCES PANEL A) OF FIGURE 8

*the k-means matching is done in R - see the kmeans_matching.R file

preserve

*load in the processed excel data from R, and save as dta, so it can be merged
import excel "Temp\matched_pcsects_2021_kmeans.xlsx", clear firstrow
save "Temp\matched_pcsects_2021_kmeans.dta", replace

*load in the UK-wide data for merging
use "Temp\pp_ready_all.dta", clear

*work out which postcode sectors have matched: merge in the postcode matched data, and keep only observations from matched postcode sectors
merge m:1 pcsect using "Temp\matched_pcsects_2021_kmeans.dta" 
keep if _merge == 3
drop _merge


*now do the dynamic DiD, just as in 5)

gen cohort = 0
replace cohort = 21*4 + 4 if in_2021_ULEZ == 1

gen Stime = cohort if cohort>0
gen etime = quarter-Stime

tab etime, gen(et)
*there are 40 et dummies, with etime = 0 now being at et28

foreach var of varlist et1-et40  {
	replace `var' = 0 if cohort==0
}

*set the lags: in this case, pre-emption appears to begin four quarters before the ULEZ is actually implemented, so we rebase to five quarters before implementation (rather than two, as in 5))
global leads "et1-et22"
global lags "et24-et40"

*regress and prepare the coefficients for graphing
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) vce(cluster pcsect)
graphprep mod1 1 40 23 28

*plot the graphs
twoway (rcap mod1_up mod1_lo mod1_t, lc("216 17 89")) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-20(10)10) xtick(-27(1)12, tlength(relative0p6)) xline(-0.5, lc(black*.80%50) lp(dash)) title("2021 Dynamic DiD") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

*save the graph
graph export "Output\Results\kmeans_dynamic_DiD_2021.pdf", as(pdf) replace

restore





********************************************************************************************





*12) now do the same long-distance analysis for the 2023 zone

*THIS ANALYSIS PRODUCES PANEL B) OF FIGURE 8

*the matching is done in R - see kmeans_matching.R

preserve

*now load in the processed excel data from R, and save as dta
import excel "Temp\matched_pcsects_2023_kmeans.xlsx", clear firstrow
save "Temp\matched_pcsects_2023_kmeans.dta", replace

*load in the UK-wide data
use "Temp\pp_ready_all.dta", clear

*work out which postcode sectors have matched: merge in the postcode matched data
merge m:1 pcsect using "Temp\matched_pcsects_2023_kmeans.dta" 
keep if _merge == 3
drop _merge


*now do the dynamic DiD, just as in 4)

gen cohort = 0
replace cohort = 23*4 + 3 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0

*generate event time variable
gen Stime = cohort if cohort>0
gen etime = quarter-Stime

*generate the event time dummies
tab etime, gen(et)
*there are 40 et dummies, with etime = 0 being at et35

*ensure the event time dummies are always 0 for the never treated group
foreach var of varlist et1-et40  {
	replace `var' = 0 if cohort==0
}

*define leads and lags - here the presence of pre-emptive behaviour is unclear, so we just normalise at the quarter before treatment as is standard
global leads "et1-et33"
global lags "et35-et40"

*run the regressions, and use the graphprep program written at the start to prepare the coefficients for graphing
reghdfe log_price $leads $lags $controls, absorb(quarter pcsect) vce(cluster pcsect)
*this corresponds to first dummy = 1, last dummy = 11, baseline dummy = 8, first treatment dummy = 9
graphprep mod1 1 40 34 35

*plot the graph of the coefficients
twoway (rcap mod1_up mod1_lo mod1_t, lc("143 45 86"%50)) (connected mod1 mod1_t, lc(black) mc(black) ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-30(10)0) xtick(-34(1)5, tlength(relative0p6)) xline(-0.5, lc(black*.80%50) lp(dash)) title("2023 Dynamic DiD") legend(order(2 "Estimate of {&beta}{subscript:j}" 1 "95% CI")) graphregion(fcolor(white) ifcolor(white)) note("SEs clustered at the postcode sector level, and unit of time = quarter", span)

*save the graph
graph export "Output\Results\kmeans_dynamic_DiD_2023.pdf", as(pdf) replace

restore





********************************************************************************************





*13) robustness checks for k-means: vary the k. First for 2021


use "Temp\pp_ready_all.dta", clear

*loop over the different values of k, from 10 to 20 in steps of 2

forvalues k = 10(2)20 {

	preserve

	*import Temp\matched_pcsects_2021_kmeans_k.xlsx, for the specified k
	import excel "Temp\matched_pcsects_2021_kmeans_`k'.xlsx", clear firstrow
	save "Temp\matched_pcsects_2021_kmeans.dta", replace

	restore

	*work out which postcode sectors have matched: merge in the postcode matched data, and keep only observations from matched postcode sectors
	merge m:1 pcsect using "Temp\matched_pcsects_2021_kmeans.dta" 
	*generate a variable k_match, with the k from the value in the loop, if _merge = 3
	gen k`k'_match = (_merge==3)
	drop _merge

}

*now do the dynamic DiDs for each, just as in 5)

gen cohort = 0
replace cohort = 21*4 + 4 if in_2021_ULEZ == 1

gen Stime = cohort if cohort>0
gen etime = quarter-Stime

tab etime, gen(et)
*there are 40 et dummies, with etime = 0 now being at et28

foreach var of varlist et1-et40  {
	replace `var' = 0 if cohort==0
}

*set the lags: in this case, pre-emption appears to begin four quarters before the ULEZ is actually implemented, so we rebase to five quarters before implementation (rather than two, as in 5))
global leads "et1-et22"
global lags "et24-et40"

*regress and prepare the coefficients for graphing, for each k
forvalues k = 10(2)20 {
	reghdfe log_price $leads $lags $controls if k`k'_match == 1, absorb(quarter pcsect) vce(cluster pcsect)
	graphprep mod`k' 1 40 23 28
}

*plot the graphs (without standard errors)
twoway (connected mod10 mod10_t, lc(black) mc(black) ms(o) msize(vsmall)) (connected mod12 mod12_t, lc("33 131 128") mc("33 131 128") ms(o) msize(vsmall)) (connected mod14 mod14_t, lc("143 45 86") mc("143 45 86") ms(o) msize(vsmall)) (connected mod16 mod16_t, lc("216 17 89") mc("216 17 89") ms(o) msize(vsmall)) (connected mod18 mod18_t, lc("255 127 0") mc("255 127 0") ms(o) msize(vsmall)) (connected mod20 mod20_t, lc("0 128 0") mc("0 128 0") ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-20(10)10) xtick(-27(1)12, tlength(relative0p6)) xline(-0.5, lc(black*.80%50) lp(dash)) title("2021 Dynamic DiD - robustness (varying k)") legend(order(1 "k = 10" 2 "k = 12" 3 "k = 14" 4 "k = 16" 5 "k = 18" 6 "k = 20")) graphregion(fcolor(white) ifcolor(white)) note("SEs omitted for clarity", span)

*save the graph
graph export "Output\Results\kmeans_dynamic_DiD_2021_robustness.pdf", as(pdf) replace






***************************************************************************





*14) do the same as above, for 2023


use "Temp\pp_ready_all.dta", clear

*loop over the different values of k, from 10 to 20 in steps of 2

forvalues k = 10(2)20 {

	preserve

	*import Temp\matched_pcsects_2023_kmeans_k.xlsx, for the specified k
	import excel "Temp\matched_pcsects_2023_kmeans_`k'.xlsx", clear firstrow
	save "Temp\matched_pcsects_2023_kmeans.dta", replace

	restore

	*work out which postcode sectors have matched: merge in the postcode matched data, and keep only observations from matched postcode sectors
	merge m:1 pcsect using "Temp\matched_pcsects_2023_kmeans.dta" 
	*generate a variable k_match, with the k from the value in the loop, if _merge = 3
	gen k`k'_match = (_merge==3)
	drop _merge

}

*now do the dynamic DiD, just as in 4)

gen cohort = 0
replace cohort = 23*4 + 3 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0

*generate event time variable
gen Stime = cohort if cohort>0
gen etime = quarter-Stime

*generate the event time dummies
tab etime, gen(et)
*there are 40 et dummies, with etime = 0 being at et35

*ensure the event time dummies are always 0 for the never treated group
foreach var of varlist et1-et40  {
	replace `var' = 0 if cohort==0
}

*define leads and lags - here the presence of pre-emptive behaviour is unclear, so we just normalise at the quarter before treatment as is standard
global leads "et1-et33"
global lags "et35-et40"

*do the regressions
forvalues k = 10(2)20 {
	reghdfe log_price $leads $lags $controls if k`k'_match == 1, absorb(quarter pcsect) vce(cluster pcsect)
	graphprep mod`k' 1 40 34 35
}

*plot the graphs (without standard errors)
twoway (connected mod10 mod10_t, lc(black) mc(black) ms(o) msize(vsmall)) (connected mod12 mod12_t, lc("33 131 128") mc("33 131 128") ms(o) msize(vsmall)) (connected mod14 mod14_t, lc("143 45 86") mc("143 45 86") ms(o) msize(vsmall)) (connected mod16 mod16_t, lc("216 17 89") mc("216 17 89") ms(o) msize(vsmall)) (connected mod18 mod18_t, lc("255 127 0") mc("255 127 0") ms(o) msize(vsmall)) (connected mod20 mod20_t, lc("0 128 0") mc("0 128 0") ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-30(10)0) xtick(-34(1)5, tlength(relative0p6)) xline(-0.5, lc(black*.80%50) lp(dash)) title("2023 Dynamic DiD - robustness (varying k)") legend(order(1 "k = 10" 2 "k = 12" 3 "k = 14" 4 "k = 16" 5 "k = 18" 6 "k = 20")) graphregion(fcolor(white) ifcolor(white)) note("SEs omitted for clarity", span)

*save the graph
graph export "Output\Results\kmeans_dynamic_DiD_2023_robustness.pdf", as(pdf) replace






******************************************************************************




*15) more robustness checks for k-means, varying the seed - first 2021



use "Temp\pp_ready_all.dta", clear

*loop over the different values of the seed, set in R

forvalues seed = 1000(1000)5000 {

	preserve

	*import Temp\matched_pcsects_2021_kmeans_k.xlsx, for the specified k
	import excel "Temp\matched_pcsects_2021_kmeans_seed_`seed'.xlsx", clear firstrow
	save "Temp\matched_pcsects_2021_kmeans.dta", replace

	restore

	*work out which postcode sectors have matched: merge in the postcode matched data, and keep only observations from matched postcode sectors
	merge m:1 pcsect using "Temp\matched_pcsects_2021_kmeans.dta" 
	*generate a variable k_match, with the k from the value in the loop, if _merge = 3
	gen seed`seed'_match = (_merge==3)
	drop _merge

}

*now do the dynamic DiDs for each, just as in 5)

gen cohort = 0
replace cohort = 21*4 + 4 if in_2021_ULEZ == 1

gen Stime = cohort if cohort>0
gen etime = quarter-Stime

tab etime, gen(et)
*there are 40 et dummies, with etime = 0 now being at et28

foreach var of varlist et1-et40  {
	replace `var' = 0 if cohort==0
}

*set the lags: in this case, pre-emption appears to begin four quarters before the ULEZ is actually implemented, so we rebase to five quarters before implementation (rather than two, as in 5))
global leads "et1-et22"
global lags "et24-et40"

*regress and prepare the coefficients for graphing, for each k
forvalues seed = 1000(1000)5000 {
	reghdfe log_price $leads $lags $controls if seed`seed'_match == 1, absorb(quarter pcsect) vce(cluster pcsect)
	graphprep mod`seed' 1 40 23 28
}

*plot the graphs (without standard errors)
twoway (connected mod1000 mod1000_t, lc(black) mc(black) ms(o) msize(vsmall)) (connected mod2000 mod2000_t, lc("33 131 128") mc("33 131 128") ms(o) msize(vsmall)) (connected mod3000 mod3000_t, lc("143 45 86") mc("143 45 86") ms(o) msize(vsmall)) (connected mod4000 mod4000_t, lc("216 17 89") mc("216 17 89") ms(o) msize(vsmall)) (connected mod5000 mod5000_t, lc("255 127 0") mc("255 127 0") ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-20(10)10) xtick(-27(1)12, tlength(relative0p6)) xline(-0.5, lc(black*.80%50) lp(dash)) title("2021 Dynamic DiD - robustness (varying seed)") legend(order(1 "seed = 1000" 2 "seed = 2000" 3 "seed = 3000" 4 "seed = 4000" 5 "seed = 5000")) graphregion(fcolor(white) ifcolor(white)) note("SEs omitted for clarity", span)

*save the graph
graph export "Output\Results\kmeans_dynamic_DiD_2021_robustness_seed.pdf", as(pdf) replace





***************************************************************************




*16) same, but now with 2023


use "Temp\pp_ready_all.dta", clear

*loop over the different values of k, from 10 to 20 in steps of 2

forvalues seed = 1000(1000)5000 {

	preserve

	*import Temp\matched_pcsects_2023_kmeans_k.xlsx, for the specified k
	import excel "Temp\matched_pcsects_2023_kmeans_seed_`seed'.xlsx", clear firstrow
	save "Temp\matched_pcsects_2023_kmeans.dta", replace

	restore

	*work out which postcode sectors have matched: merge in the postcode matched data, and keep only observations from matched postcode sectors
	merge m:1 pcsect using "Temp\matched_pcsects_2023_kmeans.dta" 
	*generate a variable k_match, with the k from the value in the loop, if _merge = 3
	gen seed`seed'_match = (_merge==3)
	drop _merge

}

*now do the dynamic DiD, just as in 4)

gen cohort = 0
replace cohort = 23*4 + 3 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0

*generate event time variable
gen Stime = cohort if cohort>0
gen etime = quarter-Stime

*generate the event time dummies
tab etime, gen(et)
*there are 40 et dummies, with etime = 0 being at et35

*ensure the event time dummies are always 0 for the never treated group
foreach var of varlist et1-et40  {
	replace `var' = 0 if cohort==0
}

*define leads and lags - here the presence of pre-emptive behaviour is unclear, so we just normalise at the quarter before treatment as is standard
global leads "et1-et33"
global lags "et35-et40"

*do the regressions
forvalues seed = 1000(1000)5000 {
	reghdfe log_price $leads $lags $controls if seed`seed'_match == 1, absorb(quarter pcsect) vce(cluster pcsect)
	graphprep mod`seed' 1 40 34 35
}

*plot the graphs (without standard errors)
twoway (connected mod1000 mod1000_t, lc(black) mc(black) ms(o) msize(vsmall)) (connected mod2000 mod2000_t, lc("33 131 128") mc("33 131 128") ms(o) msize(vsmall)) (connected mod3000 mod3000_t, lc("143 45 86") mc("143 45 86") ms(o) msize(vsmall)) (connected mod4000 mod4000_t, lc("216 17 89") mc("216 17 89") ms(o) msize(vsmall)) (connected mod5000 mod5000_t, lc("255 127 0") mc("255 127 0") ms(o) msize(vsmall)), ytitle("Coefficient estimate") ylabel(-.2(.1).2, format(%3.2f) angle(0)) ytick(-.2(0.025).2, tlength(relative0p6))  yline(0, lc(black*.80%50) lp(solid)) xtitle("Event-time (quarters after first treatment, j)") xlabel(-30(10)0) xtick(-34(1)5, tlength(relative0p6)) xline(-0.5, lc(black*.80%50) lp(dash)) title("2023 Dynamic DiD - robustness (varying seed)") legend(order(1 "seed = 1000" 2 "seed = 2000" 3 "seed = 3000" 4 "seed = 4000" 5 "seed = 5000")) graphregion(fcolor(white) ifcolor(white)) note("SEs omitted for clarity", span)

*save the graph
graph export "Output\Results\kmeans_dynamic_DiD_2023_robustness_seed.pdf", as(pdf) replace