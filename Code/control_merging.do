*-------------------------------------------*
*OUTLINE
*-------------------------------------------*
*this do-file takes the pp_ready dataset and merges in more controls
*this dataset is then collapsed to a cross-sectional postcode-sector-level dataset to allow for the matching required for the long-distance analysis
*-------------------------------------------*

*NEXT STEPS:
*get IMD constituents in the postcode sector (in 2019)
*do a DETAILED ANNOTATION AND FULL EXPLANATION of the code
*label final variables (and intermediary ones)
*explain the averaging at the end!

*-------------------------------------------*
*DATA PREPARATION
*-------------------------------------------*
*set the cd and load in the data
cd "C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data"
use "Temp\pp_ready_all.dta", clear

*we use postcode sectors for the data merging (because it is a lot easier), but actual matching will be on pseudo-postcode sectors
*as such, get the postcode sector back from the pseudo-postcode sector
gen pcsect_original = substr(postcode, 1, length(postcode) - 2)
*-------------------------------------------*



*-------------------------------------------*
*PROCESS AND MERGE IN THE CONTROLS
*-------------------------------------------*

*1) GET THE LSOA CODES

*we start by getting the nearest LSOA to all postcodes, using the ONS mapping in the input data
*this is required so that we can merge in controls given at the LSOA level

preserve

import delimited "Input\Extra controls\PCD_OA_LSOA_MSOA_LAD_NOV22_UK_LU.csv", clear

*pcd7, pcd8, pcds are all the same up to addition of blank spaces, so keep one (keep pcds, as the postcode formatting is the same as in the house price data), and remove all other irrelevant variables
keep pcds lsoa11cd lsoa11nm
rename pcds postcode

*save as a temp file
save "Temp\lsoa_postcode_mapping.dta", replace

*now merge this with the pp_ready data
restore

merge m:1 postcode using "Temp\lsoa_postcode_mapping.dta"
drop if _merge == 2
drop _merge
*around 0.2% of postcodes failed to match
*the reason for this is that the mapping file records all postcodes from 2022, and some new ones have been made since then. These unmatched sales are (almost) all from after that period: see 'tab saleyear if _merge == 1'
*we can't use any newer version, because they use updated 2021 LSOAs, and then our data for other variables would have to be from after 2021 which therefore can respond to treatment, so we must use 2011 LSOAs (the most recent before 2021) and thus we are forced to use a postcode mapping a bit earlier than we would like (as the most recent postcode mappings map to 2021 LSOAs)
*we have to just accept that we will lose sales in postcodes created after the mapping was made (but at least there aren't many)




*2) MERGE IN THE IMD

*we now start to merge in the LSOA data: first the index of multiple deprivation

*start by cleaning it and saving it as a dta file
preserve
import excel "Input\Extra controls\IMD2019_Index_of_Multiple_Deprivation.xlsx", sheet("IMD2019") firstrow clear

*keep the LSOA and the IMD value, then save
keep LSOAcode2011 IndexofMultipleDeprivationI
rename LSOAcode2011 lsoa11cd
rename IndexofMultipleDeprivationI imd

save "Temp\imd_lsoa.dta", replace
restore

*now merge this with the pp_ready data
merge m:1 lsoa11cd using "Temp\imd_lsoa.dta"
drop if _merge == 2
drop _merge
*quite a few houses haven't matched - this is because we don't have IMD data for Wales, and also 0.2% of postcodes are missing an LSOA map
*for this reason, Welsh cities can't be used in the analysis

*now get the postcode-sector and postcode-district averages
bys pcsect_original: egen pcsect_imd = mean(imd)
bys pcdist: egen pcdist_imd = mean(imd)





*3) MERGE IN THE POPULATION DENSITY DATA

*now the population density data, also at the LSOA level

*start by cleaning it and saving it as a dta file
preserve
import excel "Input\Extra controls\SAPE22DT11-mid-2019-lsoa-population-density.xlsx", sheet("Mid-2019 Population Density") cellrange(A5:N34758) firstrow clear

keep LSOACode PeopleperSqKm
rename LSOACode lsoa11cd

save "Temp\pop_density_lsoa.dta", replace
restore

*now merge this with the pp_ready data
merge m:1 lsoa11cd using "C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data\Temp\pop_density_lsoa.dta"
drop if _merge == 2
drop _merge
*same merging issues as above - issues coming from postcodes missing an LSOA mapping

*generate a postcode-sector and postcode-district average
bys pcsect_original: egen pcsect_pop_density = mean(PeopleperSqKm)
bys pcdist: egen pcdist_pop_density = mean(PeopleperSqKm)





*4) CREATE MORE VARIABLES

*we now create postcode-sector and postcode-district averages for house-level variables available in the data


*first get the sale count data, for all houses sold before 2020
bys pcsect_original: egen pcsect_sale_count = total(saleyear <= 2019)
bys pcdist: egen pcdist_sale_count = total(saleyear <= 2019)

*now get the counts of each type of house: first postcode sector
bys pcsect_original: egen count_detached = total(detached == 1 & saleyear <= 2019)
bys pcsect_original: egen count_semi = total(semi == 1 & saleyear <= 2019)
bys pcsect_original: egen count_terraced = total(terraced == 1 & saleyear <= 2019)
bys pcsect_original: egen count_flat = total(flat == 1 & saleyear <= 2019)
bys pcsect_original: egen count_new = total(new == 1 & saleyear <= 2019)
bys pcsect_original: egen count_old = total(old == 1 & saleyear <= 2019)
bys pcsect_original: egen count_leasehold = total(leasehold == 1 & saleyear <= 2019)
bys pcsect_original: egen count_freehold = total(freehold == 1 & saleyear <= 2019)

*now postcode district
bys pcdist: egen pcdist_count_detached = total(detached == 1 & saleyear <= 2019)
bys pcdist: egen pcdist_count_semi = total(semi == 1 & saleyear <= 2019)
bys pcdist: egen pcdist_count_terraced = total(terraced == 1 & saleyear <= 2019)
bys pcdist: egen pcdist_count_flat = total(flat == 1 & saleyear <= 2019)
bys pcdist: egen pcdist_count_new = total(new == 1 & saleyear <= 2019)
bys pcdist: egen pcdist_count_old = total(old == 1 & saleyear <= 2019)
bys pcdist: egen pcdist_count_leasehold = total(leasehold == 1 & saleyear <= 2019)
bys pcdist: egen pcdist_count_freehold = total(freehold == 1 & saleyear <= 2019)

*a few missings in the next bits, because of 0 counts in the above

*now get shares of each type of house, from the above
gen pcsect_share_detached = count_detached / pcsect_sale_count
gen pcsect_share_semi = count_semi / pcsect_sale_count
gen pcsect_share_terraced = count_terraced / pcsect_sale_count
gen pcsect_share_flat = count_flat / pcsect_sale_count
gen pcsect_share_new = count_new / pcsect_sale_count
gen pcsect_share_old = count_old / pcsect_sale_count
gen pcsect_share_leasehold = count_leasehold / pcsect_sale_count
gen pcsect_share_freehold = count_freehold / pcsect_sale_count

*now get the shares for postcode district
gen pcdist_share_detached = pcdist_count_detached / pcdist_sale_count
gen pcdist_share_semi = pcdist_count_semi / pcdist_sale_count
gen pcdist_share_terraced = pcdist_count_terraced / pcdist_sale_count
gen pcdist_share_flat = pcdist_count_flat / pcdist_sale_count
gen pcdist_share_new = pcdist_count_new / pcdist_sale_count
gen pcdist_share_old = pcdist_count_old / pcdist_sale_count
gen pcdist_share_leasehold = pcdist_count_leasehold / pcdist_sale_count
gen pcdist_share_freehold = pcdist_count_freehold / pcdist_sale_count




*5) GET ULEZ VARIABLES

*we need to know the zone each house is in (so we can collapse by postcode sector and ULEZ zone combined, i.e. the pseudo-postcode sector)

*create an indicator for which ULEZ zone each house is in
gen in_ULEZ = 0
replace in_ULEZ = 1 if in_2023_ULEZ == 1
replace in_ULEZ = 2 if in_2021_ULEZ == 1
replace in_ULEZ = 3 if in_2019_ULEZ == 1

lab def in_ULEZ 0 "Not in ULEZ" 1 "In 2023 ULEZ" 2 "In 2021 ULEZ" 3 "In 2019 ULEZ"
lab val in_ULEZ in_ULEZ

*we also need to know the average distance from houses in the postcode sector to each of the ULEZ borders, so we can drop postcode sectors too close

*create a variable with the average distance from the ULEZ borders for each postcode sector (so we know whether to drop them or not)
bys pcsect_original: egen pcsect_dist_from_2019_ULEZ = mean(dist_from_2019_ULEZ)
bys pcsect_original: egen pcsect_dist_from_2021_ULEZ = mean(dist_from_2021_ULEZ)
bys pcsect_original: egen pcsect_dist_from_2023_ULEZ = mean(dist_from_2023_ULEZ)




*6) NOW MAKE THE CROSS-SECTIONAL DATASET FOR THE MATCHING

*since the postcode-sector and postcode-district averages are constant at the pseudo-postcode sector level, we can just take pseudo-postcode sector averages of each of these variables, as they will be exactly these constant values
collapse (mean) pcsect_sale_count (mean) pcsect_share_detached (mean) pcsect_share_semi (mean) pcsect_share_terraced (mean) pcsect_share_flat (mean) pcsect_share_new (mean) pcsect_share_old (mean) pcsect_share_leasehold (mean) pcsect_share_freehold (mean) pcsect_dist_from_2019_ULEZ (mean) pcsect_dist_from_2021_ULEZ (mean) pcsect_dist_from_2023_ULEZ (mean) pcsect_imd (mean) pcsect_pop_density (mean) pcdist_sale_count (mean) pcdist_share_detached (mean) pcdist_share_semi (mean) pcdist_share_terraced (mean) pcdist_share_flat (mean) pcdist_share_new (mean) pcdist_share_old (mean) pcdist_share_leasehold (mean) pcdist_share_freehold pcdist_imd (mean) pcdist_pop_density, by(in_ULEZ pcsect_original)
*note: this collapses at the pseudo-postcode sector level (which I think we need to do, to isolate the treated units)

*save as an excel file, to be used for matching in R
export excel using "Temp\pp_ready_collapsed_xsec.xlsx", firstrow(variables) replace