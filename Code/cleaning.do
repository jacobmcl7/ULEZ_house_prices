*this do-file cleans up all the data from 2015 onwards

cd "C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data"

foreach year in "2024" "2023" "2022" "2021" "2020" "2019" "2018" "2017" "2016" "2015" {

	import delimited "Temp\pp_`year'_processed.csv", clear
	
	*keep if the geolocation match was good
	keep if score == 100

	*drop the irrevelant variables
	keep user_field* dist_2023 dist_2021 dist_2019 join_count_2023 join_count_2021 join_count_2019

	*extract the postcode district and sector, and drop if no postcode is available
	gen pcdist = substr(user_field4, 1, strpos(user_field4, " ") - 1)
	drop if pcdist == ""
	gen pcsect = substr(user_field4, 1, strpos(user_field4, " ") + 1)
	drop if pcsect == ""

	*generate a year and month variable
	gen saleyear = substr(user_field3, 7, 4)
	destring saleyear, replace
	gen salemonth = substr(user_field3, 4, 2)
	destring salemonth, replace

	*take log of house prices
	gen log_price = log(user_field2)

	*sign the distances to the ULEZ borders according to whether the houses are in the respective zones (i.e. negative if they are)
	replace dist_2023 = -dist_2023 if join_count_2023 == 1
	replace dist_2021 = -dist_2021 if join_count_2021 == 1
	replace dist_2019 = -dist_2019 if join_count_2019 == 1

	*rename variables
	rename join_count_2019 in_2019_ULEZ
	rename join_count_2021 in_2021_ULEZ
	rename join_count_2023 in_2023_ULEZ
	rename user_field1 transaction_id
	rename user_field2 price
	rename user_field3 sale_date
	rename user_field4 postcode
	rename user_field5 property_type
	rename user_field6 old_or_new
	rename user_field7 tenure_duration
	rename user_field8 PAON
	rename user_field9 SAON
	rename user_field10 street
	rename user_field11 locality
	rename user_field12 town_city
	rename user_field13 district
	rename user_field14 county
	rename user_field15 transaction_type
	rename user_field16 record_status
	rename dist_2023 dist_from_2023_ULEZ
	rename dist_2021 dist_from_2021_ULEZ
	rename dist_2019 dist_from_2019_ULEZ

	*generate the house-specific controls
	gen detached = (property_type == "D")
	gen semidetached = (property_type == "S")
	gen terraced = (property_type == "T")
	gen flat = (property_type == "F")
	gen new = (old_or_new == "Y")
	gen old = (old_or_new == "N")
	gen leasehold = (tenure_duration == "L")
	gen freehold = (tenure_duration == "F")
	gen stdentry = (transaction_type == "A")

	*drop some more irrelevant variables
	drop old_or_new property_type tenure_duration transaction_type record_status sale_date
	
	*save the file
	save "Temp\pp_`year'_ready.dta", replace
	
	display "`year' done"
	
}

*append them all together
append using "Temp\pp_2016_ready.dta"
append using "Temp\pp_2017_ready.dta"
append using "Temp\pp_2018_ready.dta"
append using "Temp\pp_2019_ready.dta"
append using "Temp\pp_2020_ready.dta"
append using "Temp\pp_2021_ready.dta"
append using "Temp\pp_2022_ready.dta"
append using "Temp\pp_2023_ready.dta"
append using "Temp\pp_2024_ready.dta"

*drop any duplicate observations which have different IDs but the same everything else - there are a few of these double-entries
order transaction_id
bys in_2019_ULEZ-stdentry: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup

*generate variables for the quarter of the year of sale, and then the number of quarters since 2000
gen salequarter = 1 if inrange(salemonth, 1, 3)
replace salequarter = 2 if inrange(salemonth, 4, 6)
replace salequarter = 3 if inrange(salemonth, 7, 9)
replace salequarter = 4 if inrange(salemonth, 10, 12)

gen quarter = 4*(saleyear - 2000) + salequarter

*keep only standard entries (like Schneebacher et al (2024))
keep if stdentry == 1

*adjust postcode sectors so that they are completely contained within one ULEZ zone - i.e. convert postcodes to 'pseudo-postcodes'
replace pcsect = pcsect + " 2019" if in_2019_ULEZ == 1
replace pcsect = pcsect + " 2021" if in_2021_ULEZ == 1 & in_2019_ULEZ == 0
replace pcsect = pcsect + " 2023" if in_2023_ULEZ == 1 & in_2021_ULEZ == 0

*order the variables
order transaction_id saleyear salemonth salequarter quarter price log_price PAON SAON street locality town_city district county postcode pcdist pcsect detached semidetached terraced flat new old leasehold freehold stdentry in_2019_ULEZ dist_from_2019_ULEZ in_2021_ULEZ dist_from_2021_ULEZ in_2023_ULEZ dist_from_2023_ULEZ

*label the variables
label variable in_2019_ULEZ "Is house in 2019 ULEZ?"
label variable in_2021_ULEZ "Is house in 2021 ULEZ?"
label variable in_2023_ULEZ "Is house in 2023 ULEZ?"
label variable transaction_id "Transaction ID"
label variable price "Sale price"
label variable log_price "Logarithm of sale price"
label variable saleyear "Year of sale"
label variable salemonth "Month of sale"
label variable salequarter "Quarter of sale"
label variable quarter "Number of quarters since 1 Jan 2000"
label variable PAON "Address - PAON"
label variable SAON "Address - SAON"
label variable street "Address - Street"
label variable locality "Address - Locality"
label variable town_city "Address - Town/City"
label variable district "Address - District"
label variable county "Address - County"
label variable postcode "Address - Postcode"
label variable pcdist "Postcode district of house"
label variable pcsect "Postcode sector of house"
label variable detached "Is the house detached?"
label variable semidetached "Is the house semi-detached?"
label variable terraced "Is the house terraced?"
label variable flat "Is the house a flat/apartment?"
label variable new "Is the house newly built?"
label variable old "Is the house an established residential building (not newly built)"
label variable leasehold "Is the tenure leasehold?"
label variable freehold "Is the tenure freehold?"
label variable stdentry "Is the house recorded as a 'standard entry' in the PPD?'"
label variable dist_from_2019_ULEZ "Distance from house to 2019 ULEZ border (km)"
label variable dist_from_2021_ULEZ "Distance from house to 2021 ULEZ border (km)"
label variable dist_from_2023_ULEZ "Distance from house to 2023 ULEZ border (km)"

*sort and compress the data
sort saleyear salemonth pcsect
compress

*save one that contains all entries, not just London
save "Temp\pp_ready_all.dta", replace

*keep only entries sufficiently close to London to be useful for the initial (near-near) analysis
keep if dist_from_2023_ULEZ <= 20

*save the data
save "Temp\pp_ready.dta", replace