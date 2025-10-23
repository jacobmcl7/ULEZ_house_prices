*-------------------------------------------*
*DESCRIPTION
*-------------------------------------------*
*this do-file makes some graphs and runs any other descriptive analysis 
*like the 'analysis_house_prices.do' file, this do-file takes the form of a series of preserve-restore loops, so that each component can be run in isolation after loading in the data
*a lot of these figures and tables were not included in the eventual write-up: I have left them in, as they helped me to understand the data and to guide my analysis
*the first 6 sections use the cleaned price paid data, but in the last one, I analyse the Eurostat data instead (to make figure 6) 
*-------------------------------------------*


*-------------------------------------------*
*PREPARE THE PRICE PAID DATA
*-------------------------------------------*
*set the cd and load in the data
cd "C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data"
use "Temp\pp_ready.dta", clear

*make a global macro for the controls, to insert into regressions
global controls "detached semidetached terraced new leasehold"

*generate a time variable, giving the midpoint of the relevant quarter
gen time = saleyear + salequarter/4 - 0.125
*-------------------------------------------*



*-------------------------------------------*
*INDEX:
*-------------------------------------------*
*1) time series of mean log prices for each analysis region                                     *NOT DIRECTLY USED IN ANALYSIS*
    *1a) adjusted to all go through the same point at 2015 Q1                                   *NOT DIRECTLY USED IN ANALYSIS*
*2) similar time series, but now adjusting the means for fixed effects and covariates           *PRODUCES FIGURE 10 IN APPENDIX A.5*
    *2a) same as 2) but yearly                                                                  *NOT DIRECTLY USED IN ANALYSIS*
    *2b) same as 2) but including the 2019 zone                                                 *NOT DIRECTLY USED IN ANALYSIS*
*3) distributional graphs of house prices in the ULEZ zones, compared to controls               *PRODUCES FIGURES 3A, 3B*
*4) histograms of distances from the 2021 and 2023 ULEZ borders                                 *NOT DIRECTLY USED IN ANALYSIS*
*5) graphs of total and proportional sale counts by region                                      *NOT DIRECTLY USED IN ANALYSIS*
*6) summary statistics for the nearby region                                                    *PRODUCES TABLE 1, PANEL B*
    *6a) same as 6), but for the whole set of geocoded observations                             *PRODUCES TABLE 1, PANEL A
*7) counts of observations in each analysis zone and the control zone                           *PRODUCES TABLE 2
    *7a) average sale price of houses in the analysis zone in 2019                              *USED TO CONTEXTUALISE THE ANALYSIS*

*NOW EUROSTAT DATA
*8) histograms for different transport statistics across European cities                        *PRODUCES FIGURE 9*
*-------------------------------------------*


*-------------------------------------------*
*LIST OF FIGURES AND TABLES PRODUCED
*-------------------------------------------*
*1)
    *"Output\Figures\price_series.pdf": this is unreported
*1a)
    *"Output\Figures\price_series_a.pdf": this is unreported
*2)
    *"Output\Figures\price_series_controls.pdf": this is unreported
    *"Output\Figures\price_series_controls_a.pdf": this is Figure 7 in Appendix A.5
*2a)
    *"Output\Figures\price_series_controls_yearly.pdf": this is unreported
*2b)
    *"Output\Figures\price_series_controls_a_2019.pdf": this is unreported
*3)
    *"Output\Figures\2021_histograms.pdf": this is unreported
    *"Output\Figures\2023_histograms.pdf": this is unreported
    *"Output\Figures\2021_kdensities.pdf": this is Figure 3A
    *"Output\Figures\2023_kdensities.pdf": this is Figure 3B
    *"Output\Figures\2021_cdfs.pdf": this is unreported
    *"Output\Figures\2023_cdfs.pdf": this is unreported
*4)
    *"Output\Figures\hist_dist_ulez.pdf": this is unreported
*5)
    *"Output\Figures\sale_counts.pdf": this is unreported
    *"Output\Figures\sale_proportions.pdf": this is unreported
*6)
    *"Temp\temp.doc": this is (formatted into) Table 1
    *"Temp\temp_2021.doc": this is unreported
    *"Temp\temp_2023.doc": this is unreported
    *"Temp\temp_control.doc": this is unreported
*7)
    *none, but the info is used to make Table 2
*8)
    *"Output\Figures\transport_stats.pdf": this is Figure 9

*adjust the labels for each section below to match the index above
*-------------------------------------------*





*1) generate a time series of mean log house prices for each of the analysis regions

preserve

*get the two analysis regions and the control region
keep if inrange(dist_from_2023_ULEZ, 5, 10) | (in_2023_ULEZ == 1 & dist_from_2021_ULEZ > 5) | (in_2021_ULEZ == 1 & dist_from_2019_ULEZ > 5)

*create dummies for which region each observation is in
gen in_ULEZ = 0
replace in_ULEZ = 1 if in_2023_ULEZ == 1
replace in_ULEZ = 2 if in_2021_ULEZ == 1

*take region-quarter means of price
collapse (mean) meansaleprice = log_price, by(in_ULEZ time)

*plot the evolution of these means, and save the graph
twoway (connected meansaleprice time if in_ULEZ == 0, lcolor("33 131 128"%50) mcolor("33 131 128"%50) msize(vsmall)) (connected meansaleprice time if in_ULEZ == 1, lcolor("143 45 86"%50) mcolor("143 45 86"%50) msize(vsmall)) (connected meansaleprice time if in_ULEZ == 2, lcolor("216 17 89"%50) mcolor("216 17 89"%50) msize(vsmall)), xlabel(2015(1)2024) ylabel(12.5(.25)13.5, format(%3.1f) angle(0) labsize(small)) xline(2021.875, lc(black*.80%50) lp(dash)) xline(2023.625, lc(black*.80%50) lp(dash)) legend(order(1 "Control Zone" 2 "2023 Analysis Zone" 3 "2021 Analysis Zone")) xtick(2015(0.25)2024.5, tlength(relative0p6)) ytick(12.5(0.125)13.5, tlength(relative0p6)) ytitle("Mean log(house sale price)") xtitle("Year") title("Mean log(house sale price) evolution") graphregion(fcolor(white) ifcolor(white)) 

graph export "Output\Figures\price_series.pdf", as(pdf) replace

restore



*1a) do the same but so that they all go through the same point at 2015 Q1

preserve

keep if inrange(dist_from_2023_ULEZ, 5, 10) | (in_2023_ULEZ == 1 & dist_from_2021_ULEZ > 5) | (in_2021_ULEZ == 1 & dist_from_2019_ULEZ > 5)

gen in_ULEZ = 0
replace in_ULEZ = 1 if in_2023_ULEZ == 1
replace in_ULEZ = 2 if in_2021_ULEZ == 1

collapse (mean) meansaleprice = log_price, by(in_ULEZ time)

*get the gaps between the control zone and the other two regions at the start of the sample
local gapcontrol = meansaleprice[81] - meansaleprice[1]
display `gapcontrol'
local gap23 = meansaleprice[81] - meansaleprice[41]
display `gap23'

*adjust the means so that they all go through the same point at 2015 Q1
replace meansaleprice = meansaleprice + `gapcontrol' if in_ULEZ == 0
replace meansaleprice = meansaleprice + `gap23' if in_ULEZ == 1

*plot the graph, and save it
twoway (connected meansaleprice time if in_ULEZ == 0, lcolor("33 131 128"%50) mcolor("33 131 128"%50) msize(vsmall)) (connected meansaleprice time if in_ULEZ == 1, lcolor("143 45 86"%50) mcolor("143 45 86"%50) msize(vsmall)) (connected meansaleprice time if in_ULEZ == 2, lcolor("216 17 89"%50) mcolor("216 17 89"%50) msize(vsmall)), xlabel(2015(1)2024) ylabel(12.5(.25)13.5, format(%3.1f) angle(0) labsize(small)) xline(2021.875, lc(black*.80%50) lp(dash)) xline(2023.625, lc(black*.80%50) lp(dash)) yline(0, lc(black.80%50)) legend(order(1 "Control Zone" 2 "2023 Analysis Zone" 3 "2021 Analysis Zone")) xtick(2015(0.25)2024.5, tlength(relative0p6)) ytick(12.5(0.125)13.5, tlength(relative0p6)) ytitle("Mean log(house sale price)") xtitle("Year") title("Mean log(house sale price) evolution") graphregion(fcolor(white) ifcolor(white)) note("Adjusted so each series goes through the same point at 2015 Q1", span)

graph export "Output\Figures\price_series_a.pdf", as(pdf) replace

restore





***************************************************************





*2) generate a time series of mean log house prices, having first removed fixed effects and controls

*remove the influence of controls and FEs

preserve

*get the two analysis zones and the control zone
keep if inrange(dist_from_2023_ULEZ, 5, 10) | (in_2023_ULEZ == 1 & dist_from_2021_ULEZ > 5) | (in_2021_ULEZ == 1 & dist_from_2019_ULEZ > 5)

*convert the postcode sector into a numeric variable, to use it in the regression
egen long pcsect_numeric = group(pcsect)

*create a variable that gives the region each observation is in (0 = control, 1 = 2023, 2 = 2021)
gen in_ULEZ = 0
replace in_ULEZ = 1 if in_2023_ULEZ == 1
replace in_ULEZ = 2 if in_2021_ULEZ == 1

*do the regression
reg log_price i.quarter##i.in_ULEZ i.pcsect_numeric $controls

*now process and plot the data

*it doesn't matter what the base level of pcsect or controls are, because we will shift them anyway to make them comparable
*as such, just take them to be the omitted categories from the above regression, and so calculate adjusted means by just summing the coefficients of the relevant dummies
*note: a postcode sector from each region will be omitted to prevent multicollinearity - this is why two extra postcode sectors are reported as omitted in the regression output
*this is not an issue, for the reasons above

*fill a matrix with the appropriate controls
*first define a matrix with the constant in all entries
matrix def adj_means = J(40, 4, _b[_cons])

*now fill column 1 (for control zone) with the correct values, by adding the coefficient on the quarter dummies
forvalues i = 62/100 {
    *keeps (1,1) as the constant, then sets (1,2) to constant + coefficient on quarter 62 (i.e. 2015 Q2), etc
    mat adj_means[`i' - 60, 1] = adj_means[`i' - 60, 1] + _b[`i'.quarter]
}

*now fill column 2 (for 2023 zone) with the correct values, by adding the coefficient on the quarter*1.in_ULEZ dummies
*sets (1,2) to constant + coefficient on in_ULEZ = 1 manually
*then set (2,2) to constant + coefficient on in_ULEZ == 1 dummy + coefficient on quarter 62 + coefficient on quarter 62 interacted with in_ULEZ = 1, and so on, in the loop
mat adj_means[1, 2] = adj_means[1, 2] + _b[1.in_ULEZ]
forvalues i = 62/100 {
    *replace the value with constant + coeff on in_ULEZ == 1 dummy + coeff on quarter dummy + coeff on quarter dummy interacted with in_ULEZ = 1
    mat adj_means[`i' - 60, 2] = adj_means[`i' - 60, 2] + _b[1.in_ULEZ] + _b[`i'.quarter] + _b[`i'.quarter#1.in_ULEZ]
}

*now fill column 3 (for the 2021 zone) in the same way
*do the first row manually, as before
mat adj_means[1, 3] = adj_means[1, 3] + _b[2.in_ULEZ]
forvalues i = 62/100 {
    *replace the value with constant + coeff on in_ULEZ == 2 dummy + coeff on quarter dummy + coeff on quarter dummy interacted with in_ULEZ = 2
    mat adj_means[`i' - 60, 3] = adj_means[`i' - 60, 3] + _b[2.in_ULEZ] + _b[`i'.quarter] + _b[`i'.quarter#2.in_ULEZ]
}

*finally, fill column 4 with the time (i.e. midpoint of the quarter)
forvalues i = 1/40 {
    mat adj_means[`i', 4] = 2015 + `i'/4 - 0.125
}

*save the matrices as variables
mat list adj_means
svmat adj_means

*now plot the evolution
twoway (connected adj_means1 adj_means4, lcolor("33 131 128"%50) mcolor("33 131 128"%50) msize(vsmall)) (connected adj_means2 adj_means4, lcolor("143 45 86"%50) mcolor("143 45 86"%50) msize(vsmall)) (connected adj_means3 adj_means4, lcolor("216 17 89"%50) mcolor("216 17 89"%50) msize(vsmall)), xlabel(2015(1)2024) ylabel(, format(%4.1f) angle(0) labsize(small)) xline(2021.875, lc("216 17 89"%50) lp(dash)) xline(2023.625, lc("143 45 86"*.80%50) lp(dash)) legend(order(1 "Control Zone" 2 "2023 Analysis Zone" 3 "2021 Analysis Zone")) xtick(2015(0.25)2024.75, tlength(relative0p6)) ytitle("Control-adjusted means of log(house sale price)") xtitle("Year") title("Control-adjusted means of log(house sale price)") graphregion(fcolor(white) ifcolor(white))

graph export "Output\Figures\price_series_controls.pdf", as(pdf) replace


*now adjust them so they all go through the same point at Q1 2020 (i.e. mid-way through the sample)

replace adj_means1 = adj_means1 + (adj_means[21, 3] - adj_means[21, 1])
replace adj_means2 = adj_means2 + (adj_means[21, 3] - adj_means[21, 2])

*now plot them
twoway (connected adj_means1 adj_means4, lcolor("33 131 128"%50) mcolor("33 131 128"%50) msize(vsmall)) (connected adj_means2 adj_means4, lcolor("143 45 86"%50) mcolor("143 45 86"%50) msize(vsmall)) (connected adj_means3 adj_means4, lcolor("216 17 89"%50) mcolor("216 17 89"%50) msize(vsmall)), xlabel(2015(1)2024, labsize(small)) ylabel(, format(%4.1f) angle(0) labsize(small)) xline(2021.875, lc("216 17 89"%50) lp(dash)) xline(2023.625, lc("143 45 86"%50) lp(dash)) legend(order(1 "Control Zone" 2 "2023 Analysis Zone" 3 "2021 Analysis Zone")) xtick(2015(0.25)2024.75, tlength(relative0p6)) ytitle("log(house sale price)") xtitle("Year") title("Control-adjusted means of log(house sale price)") graphregion(fcolor(white) ifcolor(white)) note("Shifted to be equal at 2020 Q1", span)

graph export "Output\Figures\price_series_controls_a.pdf", as(pdf) replace

restore



*2a) do the same but yearly

*refer to the above for details

preserve

keep if inrange(dist_from_2023_ULEZ, 5, 10) | (in_2023_ULEZ == 1 & dist_from_2021_ULEZ > 5) | (in_2021_ULEZ == 1 & dist_from_2019_ULEZ > 5)

egen long pcsect_numeric = group(pcsect)

gen in_ULEZ = 0
replace in_ULEZ = 1 if in_2023_ULEZ == 1
replace in_ULEZ = 2 if in_2021_ULEZ == 1

reg log_price i.saleyear##i.in_ULEZ i.pcsect_numeric $controls

matrix def adj_means = J(10, 4, _b[_cons])

forvalues i = 2016/2024 {
    mat adj_means[`i' - 2014, 1] = adj_means[`i' - 2014, 1] + _b[`i'.saleyear]
}

mat adj_means[1, 2] = adj_means[1, 2] + _b[1.in_ULEZ]
forvalues i = 2016/2024 {
    mat adj_means[`i' - 2014, 2] = adj_means[`i' - 2014, 2] + _b[1.in_ULEZ] + _b[`i'.saleyear] + _b[`i'.saleyear#1.in_ULEZ]
}

mat adj_means[1, 3] = adj_means[1, 3] + _b[2.in_ULEZ]
forvalues i = 2016/2024 {
    mat adj_means[`i' - 2014, 3] = adj_means[`i' - 2014, 3] + _b[2.in_ULEZ] + _b[`i'.saleyear] + _b[`i'.saleyear#2.in_ULEZ]
}

forvalues i = 2015/2024 {
    mat adj_means[`i' - 2014, 4] = `i' + 0.5
}

mat list adj_means
svmat adj_means

twoway (connected adj_means1 adj_means4, lcolor("33 131 128"%50) mcolor("33 131 128"%50) msize(vsmall)) (connected adj_means2 adj_means4, lcolor("143 45 86"%50) mcolor("143 45 86"%50) msize(vsmall)) (connected adj_means3 adj_means4, lcolor("216 17 89"%50) mcolor("216 17 89"%50) msize(vsmall)), xlabel(2015(1)2024) ylabel(, format(%4.1f) angle(0) labsize(small)) xline(2021.875, lc("216 17 89"%50) lp(dash)) xline(2023.625, lc("143 45 86"*.80%50) lp(dash)) legend(order(1 "Control Zone" 2 "2023 Analysis Zone" 3 "2021 Analysis Zone")) xtick(2015(0.25)2024.75, tlength(relative0p6)) ytitle("Control-adjusted means of log(house sale price)") xtitle("Year") title("Control-adjusted means of log(house sale price)") graphregion(fcolor(white) ifcolor(white))

replace adj_means1 = adj_means1 + (adj_means[5, 3] - adj_means[5, 1])
replace adj_means2 = adj_means2 + (adj_means[5, 3] - adj_means[5, 2])

twoway (connected adj_means1 adj_means4, lcolor("33 131 128"%50) mcolor("33 131 128"%50) msize(vsmall)) (connected adj_means2 adj_means4, lcolor("143 45 86"%50) mcolor("143 45 86"%50) msize(vsmall)) (connected adj_means3 adj_means4, lcolor("216 17 89"%50) mcolor("216 17 89"%50) msize(vsmall)), xlabel(2015(1)2024) ylabel(, format(%4.1f) angle(0) labsize(small)) xline(2021.875, lc("216 17 89"%50) lp(dash)) xline(2023.625, lc("143 45 86"%50) lp(dash)) legend(order(1 "Control Zone" 2 "2023 Analysis Zone" 3 "2021 Analysis Zone")) xtick(2015(0.25)2024.75, tlength(relative0p6)) ytitle("Control-adjusted means of log(house sale price)") xtitle("Year") title("Control-adjusted means of log(house sale price)") graphregion(fcolor(white) ifcolor(white)) note("Shifted to all be equal at 2019", span)

restore



*2b) do the same but including the 2019 zone

*again refer to the above for details

preserve

keep if inrange(dist_from_2023_ULEZ, 5, 10) | (in_2023_ULEZ == 1 & dist_from_2021_ULEZ > 5) | (in_2021_ULEZ == 1 & dist_from_2019_ULEZ > 5) | (in_2019_ULEZ == 1)

egen long pcsect_numeric = group(pcsect)

gen in_ULEZ = 0
replace in_ULEZ = 1 if in_2023_ULEZ == 1
replace in_ULEZ = 2 if in_2021_ULEZ == 1
replace in_ULEZ = 3 if in_2019_ULEZ == 1

reg log_price i.quarter##i.in_ULEZ i.pcsect_numeric $controls

*fill a matrix with the appropriate controls
*first define a matrix with the constant in all entries
matrix def adj_means = J(40, 5, _b[_cons])

*now fill column 1 (for control zone) with the correct values, by adding the coefficient on the quarter dummies
forvalues i = 62/100 {
    *keeps (1,1) as the constant, then sets (1,2) to constant + coefficient on quarter 62 (i.e. 2015 Q2), etc
    mat adj_means[`i' - 60, 1] = adj_means[`i' - 60, 1] + _b[`i'.quarter]
}

*now fill column 2 (for 2023 zone) with the correct values, by adding the coefficient on the quarter*1.in_ULEZ dummies
*sets (1,2) to constant + coefficient on in_ULEZ = 1 manually
*then set (2,2) to constant + coefficient on in_ULEZ == 1 dummy + coefficient on quarter 62 + coefficient on quarter 62 interacted with in_ULEZ = 1, and so on, in the loop
mat adj_means[1, 2] = adj_means[1, 2] + _b[1.in_ULEZ]
forvalues i = 62/100 {
    *replace the value with constant + coeff on in_ULEZ == 1 dummy + coeff on quarter dummy + coeff on quarter dummy interacted with in_ULEZ = 1
    mat adj_means[`i' - 60, 2] = adj_means[`i' - 60, 2] + _b[1.in_ULEZ] + _b[`i'.quarter] + _b[`i'.quarter#1.in_ULEZ]
}

*now fill column 3 (for the 2021 zone) in the same way
*do the first row manually, as before
mat adj_means[1, 3] = adj_means[1, 3] + _b[2.in_ULEZ]
forvalues i = 62/100 {
    *replace the value with constant + coeff on in_ULEZ == 2 dummy + coeff on quarter dummy + coeff on quarter dummy interacted with in_ULEZ = 2
    mat adj_means[`i' - 60, 3] = adj_means[`i' - 60, 3] + _b[2.in_ULEZ] + _b[`i'.quarter] + _b[`i'.quarter#2.in_ULEZ]
}

*now fill column 4 (for the 2019 zone) in the same way
*do the first row manually, as before
mat adj_means[1, 4] = adj_means[1, 4] + _b[3.in_ULEZ]
forvalues i = 62/100 {
    *replace the value with constant + coeff on in_ULEZ == 3 dummy + coeff on quarter dummy + coeff on quarter dummy interacted with in_ULEZ = 3
    mat adj_means[`i' - 60, 4] = adj_means[`i' - 60, 4] + _b[3.in_ULEZ] + _b[`i'.quarter] + _b[`i'.quarter#3.in_ULEZ]
}

*finally, fill column 5 with the time (i.e. midpoint of the quarter)
forvalues i = 1/40 {
    mat adj_means[`i', 5] = 2015 + `i'/4 - 0.125
}

mat list adj_means
svmat adj_means

*now plot the evolution
twoway (connected adj_means1 adj_means5, lcolor("33 131 128"%50) mcolor("33 131 128"%50) msize(vsmall)) (connected adj_means2 adj_means5, lcolor("143 45 86"%50) mcolor("143 45 86"%50) msize(vsmall)) (connected adj_means3 adj_means5, lcolor("216 17 89"%50) mcolor("216 17 89"%50) msize(vsmall)) (connected adj_means4 adj_means5, lcolor("240 120 120"%50) mcolor("240 120 120"%50) msize(vsmall)), xlabel(2015(1)2024) ylabel(, format(%4.1f) angle(0) labsize(small)) xline(2019.375, lc("240 120 120"%50) lp(dash)) xline(2021.875, lc("216 17 89"%50) lp(dash)) xline(2023.625, lc("143 45 86"*.80%50) lp(dash)) legend(order(1 "Control Zone" 2 "2023 Analysis Zone" 3 "2021 Analysis Zone" 4 "2019 Zone")) xtick(2015(0.25)2024.75, tlength(relative0p6)) ytitle("Control-adjusted means of log(house sale price)") xtitle("Year") title("Control-adjusted means of log(house sale price)") graphregion(fcolor(white) ifcolor(white))

graph export "Output\Figures\price_series_controls_2019.pdf", as(pdf) replace


*now adjust them so they all go through the same point at Q1 2018

replace adj_means1 = adj_means1 + (adj_means[13, 3] - adj_means[13, 1])
replace adj_means2 = adj_means2 + (adj_means[13, 3] - adj_means[13, 2])
replace adj_means4 = adj_means4 + (adj_means[13, 3] - adj_means[13, 4])

*now plot them
twoway (connected adj_means1 adj_means5, lcolor("33 131 128"%50) mcolor("33 131 128"%50) msize(vsmall)) (connected adj_means2 adj_means5, lcolor("143 45 86"%50) mcolor("143 45 86"%50) msize(vsmall)) (connected adj_means3 adj_means5, lcolor("216 17 89"%50) mcolor("216 17 89"%50) msize(vsmall)) (connected adj_means4 adj_means5, lcolor("240 120 120"%50) mcolor("240 120 120"%50) msize(vsmall)), xlabel(2015(1)2024) ylabel(, format(%4.1f) angle(0) labsize(small)) xline(2019.375, lc("240 120 120"%50) lp(dash)) xline(2021.875, lc("216 17 89"%50) lp(dash)) xline(2023.625, lc("143 45 86"*.80%50) lp(dash)) legend(order(1 "Control Zone" 2 "2023 Analysis Zone" 3 "2021 Analysis Zone" 4 "2019 Zone")) xtick(2015(0.25)2024.75, tlength(relative0p6)) ytitle("Control-adjusted means of log(house sale price)") xtitle("Year") title("Control-adjusted means of log(house sale price)") graphregion(fcolor(white) ifcolor(white)) note("Shifted to be equal at 2018 Q1", span)

graph export "Output\Figures\price_series_controls_a_2019.pdf", as(pdf) replace

restore





***************************************************************






*3) generate a series of distributional graphs to show how house prices in each sample looked before and after ULEZ

preserve 

*get the two analysis zones and the control zone
keep if inrange(dist_from_2023_ULEZ, 5, 10) | (in_2023_ULEZ == 1 & dist_from_2021_ULEZ > 5) | (in_2021_ULEZ == 1 & dist_from_2019_ULEZ > 5)

*first 2021, which we compare to the control group

*make a variable indicating whether before or after 2021 expansion
gen post = 0 
replace post = 1 if quarter >= 21*4 + 4

*first make histograms, and save them to be combined later: one for the 2021 zone, one for the control zone, both with the distribution before implementation and after implementation overlaid on each other
*first 2021 zone
twoway (histogram log_price if post==0 & log_price>11 & log_price<16 & in_2021_ULEZ == 1, start(11) width(0.1) fcolor(black%5) lcolor(black)) (histogram log_price if post==1 & log_price>11 & log_price<16 & in_2021_ULEZ == 1, start(11) width(0.1) fcolor("216 17 89%5") lcolor("216 17 89")), legend(size(*0.75) order(1 "Pre 2021 ULEZ Expansion" 2 "Post 2021 ULEZ Expansion")) xlabel(11(1)16) xtitle("Log house sale price") ylabel(, format(%3.1f) angle(0)) title("House prices in the 2021 analysis zone") graphregion(fcolor(white) ifcolor(white)) saving("Temp\hist2021", replace)

*now control zone
twoway (histogram log_price if post==0 & log_price>11 & log_price<16 & in_2023_ULEZ == 0, start(11) width(0.1) fcolor(black%5) lcolor(black)) (histogram log_price if post==1 & log_price>11 & log_price<16 & in_2023_ULEZ == 0, start(11) width(0.1) fcolor("33 131 128%5") lcolor("33 131 128")), legend(order(1 "Pre 2021 ULEZ Expansion" 2 "Post 2021 ULEZ Expansion")) xlabel(11(1)16) xtitle("Log house sale price") ylabel(, format(%3.1f) angle(0)) title("House prices in the control zone") graphregion(fcolor(white) ifcolor(white)) saving("Temp\hist2021control", replace)

*now kdensity: same as above
twoway (kdensity log_price if post==0 & log_price>11 & log_price<16 & in_2021_ULEZ == 1, lcolor(black)) (kdensity log_price if post==1 & log_price>11 & log_price<16 & in_2021_ULEZ == 1, lcolor("216 17 89")), legend(size(*0.75) order(1 "Pre 2021 ULEZ Expansion" 2 "Post 2021 ULEZ Expansion")) xlabel(11(1)16) xtitle("Log house sale price") ylabel(, format(%3.1f) angle(0)) title("House prices in the 2021 analysis zone") graphregion(fcolor(white) ifcolor(white)) saving("Temp\kdensity2021", replace)

twoway (kdensity log_price if post==0 & log_price>11 & log_price<16 & in_2023_ULEZ == 0, lcolor(black)) (kdensity log_price if post==1 & log_price>11 & log_price<16 & in_2023_ULEZ == 0, lcolor("33 131 128")), legend(size(*0.75) order(1 "Pre 2021 ULEZ Expansion" 2 "Post 2021 ULEZ Expansion")) xlabel(11(1)16) xtitle("Log house sale price") ylabel(, format(%3.1f) angle(0)) title("House prices in the control zone") graphregion(fcolor(white) ifcolor(white)) saving("Temp\kdensity2021control", replace)

*now cdfs: same as above (having first used the cumul function to get an empirical cdf)
cumul log_price if post==0 & in_2021_ULEZ == 1, gen(cumul0)
cumul log_price if post==1 & in_2021_ULEZ == 1, gen(cumul1)
sort cumul0 cumul1
twoway (line cumul0 log_price if log_price>11 & log_price<16 & in_2021_ULEZ == 1, lcolor(black)) (line cumul1 log_price if log_price>11 & log_price<16 & in_2021_ULEZ == 1, lcolor("216 17 89")), legend(order(1 "Pre 2021 ULEZ Expansion" 2 "Post 2021 ULEZ Expansion")) xlabel(11(1)16) xtitle("Log house sale price") ylabel(, format(%3.1f) angle(0)) ytitle("Cumulative density") title("CDF of house prices in the 2021 analysis zone") graphregion(fcolor(white) ifcolor(white)) saving("Temp\cumul2021", replace)

drop cumul0 cumul1
cumul log_price if post==0 & in_2023_ULEZ == 0, gen(cumul0)
cumul log_price if post==1 & in_2023_ULEZ == 0, gen(cumul1)
sort cumul0 cumul1
twoway (line cumul0 log_price if log_price>11 & log_price<16 & in_2023_ULEZ == 0, lcolor(black)) (line cumul1 log_price if log_price>11 & log_price<16 & in_2023_ULEZ == 0, lcolor("33 131 128")), legend(order(1 "Pre 2021 ULEZ Expansion" 2 "Post 2021 ULEZ Expansion")) xlabel(11(1)16) xtitle("Log house sale price") ylabel(, format(%3.1f) angle(0)) ytitle("Cumulative density") title("CDF of house prices in the control zone") graphregion(fcolor(white) ifcolor(white)) saving("Temp\cumul2021control", replace)


*now 2023: exact same as in 2021, but with the 2023 zone instead of the 2021 zone, and the post variable indicating before/after the 2023 expansion

drop post 
gen post = 0
replace post = 1 if quarter >= 23*4 + 3

*first histograms 
twoway (histogram log_price if post==0 & log_price>11 & log_price<16 & in_2023_ULEZ == 1 & in_2021_ULEZ == 0, start(11) width(0.1) fcolor(black%5) lcolor(black)) (histogram log_price if post==1 & log_price>11 & log_price<16 & in_2023_ULEZ == 1 & in_2021_ULEZ == 0, start(11) width(0.1) fcolor("143 45 86%5") lcolor("143 45 86")), legend(order(1 "Pre 2023 ULEZ Expansion" 2 "Post 2023 ULEZ Expansion")) xlabel(11(1)16) xtitle("Log house sale price") ylabel(, format(%3.1f) angle(0)) title("House prices in the 2023 analysis zone") graphregion(fcolor(white) ifcolor(white)) saving("Temp\hist2023", replace)

twoway (histogram log_price if post==0 & log_price>11 & log_price<16 & in_2023_ULEZ == 0, start(11) width(0.1) fcolor(black%5) lcolor(black)) (histogram log_price if post==1 & log_price>11 & log_price<16 & in_2023_ULEZ == 0, start(11) width(0.1) fcolor("33 131 128%5") lcolor("33 131 128")), legend(order(1 "Pre 2023 ULEZ Expansion" 2 "Post 2023 ULEZ Expansion")) xlabel(11(1)16) xtitle("Log house sale price") ylabel(, format(%3.1f) angle(0)) title("House prices in the control zone") graphregion(fcolor(white) ifcolor(white)) saving("Temp\hist2023control", replace)

*now kdensity
twoway (kdensity log_price if post==0 & log_price>11 & log_price<16 & in_2023_ULEZ == 1 & in_2021_ULEZ == 0, lcolor(black)) (kdensity log_price if post==1 & log_price>11 & log_price<16 & in_2023_ULEZ == 1 & in_2021_ULEZ == 0, lcolor("143 45 86")), legend(size(*0.75) order(1 "Pre 2023 ULEZ Expansion" 2 "Post 2023 ULEZ Expansion")) xlabel(11(1)16) xtitle("Log house sale price") ylabel(, format(%3.1f) angle(0)) title("House prices in the 2023 analysis zone") graphregion(fcolor(white) ifcolor(white)) saving("Temp\kdensity2023", replace)

twoway (kdensity log_price if post==0 & log_price>11 & log_price<16 & in_2023_ULEZ == 0, lcolor(black)) (kdensity log_price if post==1 & log_price>11 & log_price<16 & in_2023_ULEZ == 0, lcolor("33 131 128")), legend(size(*0.75) order(1 "Pre 2023 ULEZ Expansion" 2 "Post 2023 ULEZ Expansion")) xlabel(11(1)16) xtitle("Log house sale price") ylabel(, format(%3.1f) angle(0)) title("House prices in the control zone") graphregion(fcolor(white) ifcolor(white)) saving("Temp\kdensity2023control", replace)

*now cdfs
drop cumul0 cumul1
cumul log_price if post==0 & in_2023_ULEZ == 1 & in_2021_ULEZ == 0, gen(cumul0)
cumul log_price if post==1 & in_2023_ULEZ == 1 & in_2021_ULEZ == 0, gen(cumul1)
sort cumul0 cumul1
twoway (line cumul0 log_price if log_price>11 & log_price<16 & in_2023_ULEZ == 1 & in_2021_ULEZ == 0, lcolor(black)) (line cumul1 log_price if log_price>11 & log_price<16 & in_2023_ULEZ == 1 & in_2021_ULEZ == 0, lcolor("143 45 86")), legend(order(1 "Pre 2023 ULEZ Expansion" 2 "Post 2023 ULEZ Expansion")) xlabel(11(1)16) xtitle("Log house sale price") ylabel(, format(%3.1f) angle(0)) ytitle("Cumulative density") title("CDF of house prices in the 2023 analysis zone") graphregion(fcolor(white) ifcolor(white)) saving("Temp\cumul2023", replace)

drop cumul0 cumul1
cumul log_price if post==0 & in_2023_ULEZ == 0, gen(cumul0)
cumul log_price if post==1 & in_2023_ULEZ == 0, gen(cumul1)
sort cumul0 cumul1
twoway (line cumul0 log_price if log_price>11 & log_price<16 & in_2023_ULEZ == 0, lcolor(black)) (line cumul1 log_price if log_price>11 & log_price<16 & in_2023_ULEZ == 0, lcolor("33 131 128")), legend(order(1 "Pre 2023 ULEZ Expansion" 2 "Post 2023 ULEZ Expansion")) xlabel(11(1)16) xtitle("Log house sale price") ylabel(, format(%3.1f) angle(0)) ytitle("Cumulative density") title("CDF of house prices in the control zone") graphregion(fcolor(white) ifcolor(white)) saving("Temp\cumul2023control", replace)


*now create the graphs that combine the saved graphs, and save them as new graphs

graph combine Temp\hist2021.gph Temp\hist2021control.gph, graphregion(fcolor(white) ifcolor(white)) ysize(2.5) xsize(5)
graph export "Output\Figures\2021_histograms.pdf", as(pdf) replace

graph combine Temp\hist2023.gph Temp\hist2023control.gph, graphregion(fcolor(white) ifcolor(white)) ysize(2.5) xsize(5)
graph export "Output\Figures\2023_histograms.pdf", as(pdf) replace

graph combine Temp\kdensity2021.gph Temp\kdensity2021control.gph, graphregion(fcolor(white) ifcolor(white)) ysize(2.5) xsize(5)
graph export "Output\Figures\2021_kdensities.pdf", as(pdf) replace

graph combine Temp\kdensity2023.gph Temp\kdensity2023control.gph, graphregion(fcolor(white) ifcolor(white)) ysize(2.5) xsize(5)
graph export "Output\Figures\2023_kdensities.pdf", as(pdf) replace

graph combine Temp\cumul2021.gph Temp\cumul2021control.gph, graphregion(fcolor(white) ifcolor(white)) ysize(2.5) xsize(5)
graph export "Output\Figures\2021_cdfs.pdf", as(pdf) replace

graph combine Temp\cumul2023.gph Temp\cumul2023control.gph, graphregion(fcolor(white) ifcolor(white)) ysize(2.5) xsize(5)
graph export "Output\Figures\2023_cdfs.pdf", as(pdf) replace

restore





***************************************************************





*4) plot some basic histograms of distances to the different ULEZ borders, for the whole sample of houses less than 20km from the 2023 ULEZ border

preserve

*fairly straightforward histograms of the distance variables, saving them so they can be combined
twoway (histogram dist_from_2021_ULEZ, width(0.25) fcolor("216 17 89%75") lcolor(black%5)), xtitle("Distance from the 2021 ULEZ border") ylabel(, format(%3.2f) angle(0)) graphregion(fcolor(white) ifcolor(white)) saving("Temp\dist2021", replace)
twoway (histogram dist_from_2023_ULEZ, width(0.25) fcolor("143 45 86%75") lcolor(black%5)), xtitle("Distance from the 2023 ULEZ border") ylabel(, format(%3.2f) angle(0)) graphregion(fcolor(white) ifcolor(white)) saving("Temp\dist2023", replace)

*combine them and save the combined graph
graph combine Temp\dist2021.gph Temp\dist2023.gph, graphregion(fcolor(white) ifcolor(white)) ysize(1) xsize(3) title("Distribution of house distances from ULEZ borders")
graph export "Output\Figures\hist_dist_ulez.pdf", as(pdf) replace

restore

*note - the borders of greater london (i.e. the 2023 border) were redrawn to include London's suburbs in 1965, so not a surprise that we see a sharp drop-off in density at the border




***************************************************************





*5) create a graph of sale counts by region

preserve

*get the two analysis zones and the control zone
keep if inrange(dist_from_2023_ULEZ, 5, 10) | (in_2023_ULEZ == 1 & dist_from_2021_ULEZ > 5) | (in_2021_ULEZ == 1 & dist_from_2019_ULEZ > 5)

*make a variable giving which zone each observation is in
gen in_ULEZ = 0
replace in_ULEZ = 1 if in_2023_ULEZ == 1 & in_2021_ULEZ == 0
replace in_ULEZ = 2 if in_2021_ULEZ == 1

*count the number of sales in each quarter in each zone
collapse (count) count = log_price, by(in_ULEZ time)

*plot this evolution, and save the graph
twoway (connected count time if in_ULEZ == 0, lcolor("33 131 128%75") mcolor("33 131 128%75") msize(vsmall)) (connected count time if in_ULEZ == 1, lcolor("143 45 86%75") mcolor("143 45 86%75") msize(vsmall)) (connected count time if in_ULEZ == 2, lcolor("216 17 89%75") mcolor("216 17 89%75") msize(vsmall)), xline(2021.875, lc("216 17 89%50") lp(dash)) xline(2023.625, lc("143 45 86%50") lp(dash)) legend(order(1 "Control Zone" 2 "2023 Analysis Zone" 3 "2021 Analysis Zone")) xtitle("Year") ytitle("Number of sales") title("Number of sales by region") graphregion(fcolor(white) ifcolor(white))

graph export "Output\Figures\sale_counts.pdf", as(pdf) replace


*now do it as a proportion of the total number of sales in the sample

*get the total number of sales in the control zone, in the local macro `a'
egen total_count = total(count) if in_ULEZ == 0
sort total_count
local a = total_count[1]
drop total_count

*now for the 2023 analysis zone, in `b'
egen total_count = total(count) if in_ULEZ == 1
sort total_count
local b = total_count[1]
drop total_count

*now for the 2021 analysis zone, in `c'
egen total_count = total(count) if in_ULEZ == 2
sort total_count
local c = total_count[1]
drop total_count

*get the zone-wise proportions
replace count = count/`a' if in_ULEZ == 0
replace count = count/`b' if in_ULEZ == 1
replace count = count/`c' if in_ULEZ == 2

*plot this evolution, and save the graph
sort in_ULEZ time

twoway (connected count time if in_ULEZ == 0, lcolor("33 131 128%75") mcolor("33 131 128%75") msize(vsmall)) (connected count time if in_ULEZ == 1, lcolor("143 45 86%75") mcolor("143 45 86%75") msize(vsmall)) (connected count time if in_ULEZ == 2, lcolor("216 17 89%75") mcolor("216 17 89%75") msize(vsmall)), xline(2021.875, lc("216 17 89%50") lp(dash)) xline(2023.625, lc("143 45 86%50") lp(dash)) legend(order(1 "Control Zone" 2 "2023 Analysis Zone" 3 "2021 Analysis Zone")) xtitle("Year") ytitle("Proportion of sales") title("Proportion of total sales in the sample period by region") graphregion(fcolor(white) ifcolor(white))

graph export "Output\Figures\sale_proportions.pdf", as(pdf) replace

restore

*note: these help support the parallel trends assumption




***************************************************************




*6) collect a table of summary statistics for key variables in the total sample, and then disaggregate by region

preserve

*fairly straightforward summary tables using outreg2
outreg2 using "Temp\temp.doc", replace sum(log) keep(log_price detached semidetached terraced flat new leasehold in_2019_ULEZ dist_from_2019_ULEZ in_2021_ULEZ dist_from_2021_ULEZ in_2023_ULEZ dist_from_2023_ULEZ)

outreg2 using "Temp\temp_2021.doc" if in_2021_ULEZ & dist_from_2019_ULEZ > 5, replace sum(log) keep(log_price detached semidetached terraced flat new leasehold dist_from_2019_ULEZ dist_from_2021_ULEZ dist_from_2023_ULEZ)
outreg2 using "Temp\temp_2023.doc" if in_2023_ULEZ & dist_from_2021_ULEZ > 5, replace sum(log) keep(log_price detached semidetached terraced flat new leasehold dist_from_2019_ULEZ dist_from_2021_ULEZ dist_from_2023_ULEZ)
outreg2 using "Temp\temp_control.doc" if inrange(dist_from_2023_ULEZ, 5, 10), replace sum(log) keep(log_price detached semidetached terraced flat new leasehold dist_from_2019_ULEZ dist_from_2021_ULEZ dist_from_2023_ULEZ)

restore

*these are then manually converted to tables in LaTeX




*6a) now do this for the whole dataset, with all geocoded observations

preserve 

*get all the geocoded house data in (i.e. not just the region in and around London), then do the same as above
use "Temp\pp_ready_all.dta", clear

outreg2 using "Temp\temp_all.doc", replace sum(log) keep(log_price detached semidetached terraced flat new leasehold in_2019_ULEZ dist_from_2019_ULEZ in_2021_ULEZ dist_from_2021_ULEZ in_2023_ULEZ dist_from_2023_ULEZ)

restore

*again this is then manually converted to a LaTeX table




***************************************************************




*7) counts of sales by region

preserve

*get the analysis zones and the control zone
keep if inrange(dist_from_2023_ULEZ, 5, 10) | (in_2023_ULEZ == 1 & dist_from_2021_ULEZ > 5) | (in_2021_ULEZ == 1 & dist_from_2019_ULEZ > 5)

*count the number of sales in each zone
count if !in_2023_ULEZ
    *117,463
count if in_2023_ULEZ & !in_2021_ULEZ
    *249,725
count if in_2021_ULEZ
    *127,380

restore



*7a) another brief calculation: average sale price in the analysis regions before ULEZ

preserve

*get the analysis zones and the control zone
keep if (in_2023_ULEZ == 1 & dist_from_2021_ULEZ > 5) | (in_2021_ULEZ == 1 & dist_from_2019_ULEZ > 5)

*get the average sale price since 2021, and calculate average price
keep if saleyear >= 2021
summarize price
    *average price = £572,919

restore







*############################ NOW EUROSTAT DATA ############################*



*8) histograms for different transport statistics

*first the number of cars per 1000 people

*load in the data
import excel "Input\Eurostat\cars_per_1000.xlsx", sheet("Sheet 1") cellrange(A8:BU896) clear

*London last observed in 2018 - keep this column, and reorganise the data so that it is in a more usable format
keep A BH
drop if inrange(_n, 1, 2)
drop if inrange(_n, _N - 7, _N)
rename A city
rename BH cars_per_1000_2018

*drop missings, and convert to numeric
drop if cars_per_1000_2018 == ":"
destring cars_per_1000_2018, replace

*count how many cities
display _N
    *662 cities

*get London's value
list if city == "London"
    *London's value is 282.79

*drop major outliers
drop if cars_per_1000_2018 > 1000

*histogram plot
local london_value = 282.79
graph twoway (histogram cars_per_1000, width(10) fcolor(blue%40) lcolor(black%50) lwidth(vvthin)), xline(`london_value', lc(black) lwidth(medium) lpattern(dash)) xtitle("Cars per 1000 people") ytitle("Density") xlabel(200(100)800, labsize(medsmall)) ylabel(, format(%4.3f) labsize(medsmall)) title("Registered cars per 1000 inhabitants") graphregion(fcolor(white) ifcolor(white)) text(0.0056 290 "London (= 282.79)", place(e) color(black) size(medium)) note("Data from 2018, covering 662 European cities", span size(small)) saving("Temp\cars_per_1000", replace)


*now the proportion of work journeys by car

import excel "Input\Eurostat\journeys_to_work_by_car.xlsx", sheet("Sheet 1") cellrange(A8:BU896) clear

*London last observed in 2011 - same as above
keep A AT
drop if inrange(_n, 1, 2)
drop if inrange(_n, _N - 7, _N)
rename A city
rename AT prop_work_journeys_car_2011

*drop missings, and convert to numeric
drop if prop_work_journeys_car_2011 == ":"
destring prop_work_journeys_car_2011, replace

*count how many cities
display _N
    *344 cities

*make into a proportion
replace prop_work_journeys_car_2011 = prop_work_journeys_car_2011/100

*get London's value
list if city == "London"
    *London's value is 0.319

local london_value = 0.319
graph twoway (histogram prop_work_journeys_car_2011, width(0.02) fcolor(blue%40) lcolor(black%50) lwidth(vvthin)), xline(`london_value', lc(black) lwidth(medium) lpattern(dash)) xtitle("Proportion of journeys") ytitle("Density") xlabel(0.2(0.1)0.9, labsize(medsmall) format(%2.1f)) ylabel(, labsize(medsmall)) title("Proportion of work journeys by car") graphregion(fcolor(white) ifcolor(white)) text(5.5 0.33 "London (= 0.319)", place(e) color(black) size(medium)) note("Data from 2011, covering 344 European cities", span size(small)) saving("Temp\prop_journeys_car", replace)




*now the proportion of work journeys by public transport

import excel "Input\Eurostat\journeys_to_work_by_public_transport.xlsx", sheet("Sheet 1") cellrange(A8:BU896) clear

*London last observed in 2011 - same as above
keep A AT
drop if inrange(_n, 1, 2)
drop if inrange(_n, _N - 7, _N)
rename A city
rename AT prop_work_journeys_pt_2011

*drop missings, and convert to numeric
drop if prop_work_journeys_pt_2011 == ":"
destring prop_work_journeys_pt_2011, replace

*count how many cities
display _N
    *347 cities

*make into a proportion
replace prop_work_journeys_pt_2011 = prop_work_journeys_pt_2011/100

*get London's value
list if city == "London"
    *London's value is 0.526

local london_value = 0.526
graph twoway (histogram prop_work_journeys_pt_2011, width(0.02) fcolor(blue%40) lcolor(black%50) lwidth(vvthin)), xline(`london_value', lc(black) lwidth(medium) lpattern(dash)) xtitle("Proportion of journeys") ytitle("Density") xlabel(0(0.1)0.5, labsize(small) format(%2.1f)) ylabel(, labsize(small)) title("Proportion of work journeys by public transport") graphregion(fcolor(white) ifcolor(white)) text(7 0.52 "London (= 0.526)", place(w) color(black) size(small)) note("Data from 2011, covering 347 European cities", span) saving("Temp\prop_journeys_pt", replace)


*now combine the graphs together

*for presentation, I only plot the first two, otherwise they would have looked weird/ugly on the page as a row of three, and there is no nicer way of presenting them. I would have included the last in the appendix, but I have no room 
graph combine "Temp\cars_per_1000.gph" "Temp\prop_journeys_car.gph", graphregion(fcolor(white) ifcolor(white)) ysize(1) xsize(2.5) rows(1)
graph export "Output\Figures\transport_stats.pdf", as(pdf) replace