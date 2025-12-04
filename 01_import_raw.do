* Do file purpose: read in monthly data, HDI, KPIs, other outcomes.
* Created by: Herman Lange
* Last Modified: 12/02/2025




*-------------------------------------------------------------------------------*
*					Import and Process Pre-Reform KPI Data 2020Q2-2021Q3		*
*-------------------------------------------------------------------------------*
* Import Raw Data, through loop since each indicator is in separate excel tab:
foreach kpi in diabetes prenatal Std Dental Cyrvical Vaccines hypertension {

import excel "${data}/Combined_PreReformData.xlsx", sheet("`kpi'") firstrow clear
rename (Uf IBGE Municipio) (state_abbr municipality_id municipality_name)

* A few empty rows in the end:
drop if municipality_id==""

* Reshape Long
reshape long Q, i(state_abbr municipality_id municipality_name) j(period) string

* Name and Label KPI
rename Q ratio_`kpi' 
destring ratio_`kpi', replace
destring municipality_id, replace
replace ratio_`kpi' = ratio_`kpi'/100
label var ratio_`kpi' "Ratio `kpi' in %"

* Fix Date Format to Subsequent Merge with Post-Reform KPI
	gen yy = substr(period,5,2 )
	gen qq    = substr(period,1,1 )
	drop period 
	gen period = yy + "_" + qq
	label var period "yy_trimester"
	order state_abbr municipality_id municipality_name period ratio_`kpi'
	keep state_abbr municipality_id municipality_name period ratio_`kpi'

* Save Interim Dataset
save "${working}/`kpi'.dta", replace
}


* Merge together pre-data 
use "${working}/diabetes.dta", clear
foreach kpi in prenatal Std Dental Cyrvical Vaccines hypertension {
	qui merge 1:1 state_abbr municipality_id municipality_name period using  "${working}/`kpi'.dta"
	tab _merge
	drop _merge
}

* Rename to intuitive names 
rename (ratio_Vaccines ratio_Std ratio_Dental ratio_prenatal ratio_Cyrvical) (ratio_vaccine ratio_stdTest ratio_dental ratio_prenatalVisit ratio_papsmear)

save "${working}/KPI_pre.dta", replace



*-------------------------------------------------------------------------------*
*					Import and Process Post-Reform KPI Data 					*
*-------------------------------------------------------------------------------*
* Import Raw Data
import delimited "${data}/brazil_primarycare_kpis.csv", clear


* Set to missing if numerator > denominator, or denomitor is zero, or either is negative: No observations changed
foreach data in numerator denominator_used {
	replace `data' = . if numerator>denominator_used
	replace `data' = . if numerator<0
	replace `data' = . if denominator_used<0
}

* Define Date in YY_trimester format 
gen double _date = daily(period_start_date, "YMD")   // "2022-01-01" → %td
format _date %tdCCYY-NN-DD
assert !missing(_date)

	* Overwrite original
	drop period_start_date
	rename _date period_start_date

	* Create a string version that has yy_trimester format
	gen y    = year(period_start_date)
	gen m    = month(period_start_date)
	gen byte quad = ceil(m/4)
	tostring y, replace
	tostring quad, replace
	replace y = substr(y,3,2)
	gen period = y + "_" + quad
	drop y m quad
	label var period "yy_trimester"
	order state_abbr municipality_id municipality_name period period_start_date
	
*Define Ratio 
gen ratio = numerator/denominator_used	
label var ratio "Value of numerator to denominator"
rename denominator_used wt
label var wt "Weight: denominator used" 

* Define weights to allow for later aggregation 
preserve 
	rename registered_population wt_reg 
	label var wt_reg "Weight: registered population" 
	rename resident_population wt_res 
	label var wt_res "Weight: resident population" 
	keep municipality_id period wt_*
	sort municipality_id period 
	duplicates drop municipality_id period , force
	save "${working}/temp.dta", replace
restore
	
* Convert to wide
encode indicator, gen(indicator_id)
tab indicator_id
//1 1-year vaccine, 2) diabetes, 3) hypertension, 4) pregnant tested, 5) pregnant received, 6) pregnant women with at least, 7) cervical
keep state_abbr municipality_id indicator_id municipality_name period period_start_date wt ratio


reshape wide ratio wt, i(state_abbr municipality_id municipality_name period period_start_date) j(indicator_id)

	* Properly label and name all variables
	foreach var in ratio wt {
	rename `var'1 `var'_vaccine
	label var `var'_vaccine "Proportion of 1-year-old children vaccinated in primary care..."
	rename `var'2 `var'_diabetes
	label var `var'_diabetes "Proportion of people with diabetes who..."
	rename `var'3 `var'_hypertension
	label var `var'_hypertension "Proportion of people with hypertension who had a visit..."
	rename `var'4 `var'_stdTest
	label var `var'_stdTest "Proportion of pregnant women tested for syphilis and HIV"
	rename `var'5 `var'_dental
	label var `var'_dental "Proportion of pregnant women who received dental care"
	rename `var'6 `var'_prenatalVisit
	label var `var'_prenatalVisit "Proportion of pregnant women with at least 6 prenatal visits..."
	rename `var'7 `var'_papsmear
	label var `var'_papsmear "Proportion of women with cervical cytology (Pap smear)..."
	}

* Drop indicator weights, did not end up using
drop wt_*
	
* Merge in population weights for subsequents aggregation 
merge m:1 municipality_id period using "temp.dta", keep(3) nogen


save "${working}/KPI_post.dta", replace


*-------------------------------------------------------------------------------*
*					Import and Process Provided Monthly Data					*
*-------------------------------------------------------------------------------*
pq use "${data}/health_outcomes_monthly.parquet", clear


* Define Data in yy_trimester format and a proper data format 
gen period_date = dofc(period_start_date)
format period_date %tdCCYY-NN-DD

	gen byte quad = ceil(month(period_date)/4)
	gen str2 yy   = substr(string(year(period_date),"%04.0f"),3,2)   // "2020" -> "20"
	gen str6 period = yy + "_" + string(quad)                        // e.g., "20_3"
	label var period "yy_trimester (yy_q; 1=Jan–Apr, 2=May–Aug, 3=Sep–Dec)"
	drop quad period_start_date yy
	rename period_date period_start_date
	order municipality_id period_start_date period

* Clean Data ~22k/~4 million
	* Drop Actual Duplicates // 8,831 / 3.95 million
	duplicates drop municipality_id period_start_date numerator denominator outcome, force
	
	* Aggregate Double Reporting that's non duplicates 200k / 3.9 million obs (one indicator deaths per hospital is reported multiple times likely at municipality-quadrimester-hospital level)
	collapse (sum) numerator (sum) denominator, by(municipality_id period_start_date period outcome)
	
	* Set to Missing Impossible Observations 4,740/3.7 million
	foreach data in numerator denominator {
		replace `data' = . if numerator>denominator
		replace `data' = . if numerator<0
		replace `data' = . if denominator<0
	}
	

* Aggregate to Trimester Level
collapse (sum) numerator (sum) denominator, by(municipality_id period outcome)

* Define ratio & weight
gen ratio = numerator/denominator	
label var ratio "Value of numerator to denominator"
rename denominator wt
label var wt "Weight: denominator" 
drop numerator


* Reshape wide (first figure out which id matches which indicator)
encode outcome, gen(outcome_id)
tab outcome_id
drop outcome
//1) 1_3_consult
//2) 4_6_consult
//3) 7_consult
//4) anomalies
//5) low_weight
//6) no_consult
//7) vaccinated
//8) radiotherapy
//9) fetal_deaths
//10) hospital_cytology
//11) diabetes_hospitalized
//12) hypertension_hospitalized
//13) maternal_death
//14) neonatal_death
//15) neo_hosp_mortality
//16) outpatient_cytology
//17) diabetes_primary
//18) diabetes_specialized
//19) hypertension_primary
//20) hypertension_specialized
//21) premature

reshape wide ratio wt, i(municipality_id period) j(outcome_id)

* Properly label and name all variables
	foreach var in ratio wt {
	rename `var'1 `var'1_3_consult
	label var `var'1_3_consult "births_with_1_to_3_prenatal_consultations"
	rename `var'2 `var'4_6_consult
	label var `var'4_6_consult "births_with_4_to_6_prenatal_consultations"
	rename `var'3 `var'7_consult
	label var `var'7_consult "births_with_7_or_more_prenatal_consultations"
	rename `var'4 `var'anomalies
	label var `var'anomalies "births_with_congenital_anomalies"
	rename `var'5 `var'low_weight
	label var `var'low_weight "births_with_low_birth_weight"
	rename `var'6 `var'no_consult
	label var `var'no_consult "births_with_no_prenatal_consultations"
	rename `var'7 `var'vaccinated
	label var `var'vaccinated "children_up_to_one_year_vaccinated_polio_pentavalent"
	rename `var'8 `var'radiotherapy
	label var `var'radiotherapy "cytology_radiotherapy"
	rename `var'9 `var'fetal_deaths
	label var `var'fetal_deaths "fetal_deaths"
	rename `var'10 `var'hospital_cytology
	label var `var'hospital_cytology "hospital_cytology_procedures"
	rename `var'11 `var'diabetes_hospitalized
	label var `var'diabetes_hospitalized "hospitalizations_due_to_diabetes"
	rename `var'12 `var'hypertension_hospitalized
	label var `var'hypertension_hospitalized "hospitalizations_due_to_hypertension"
	rename `var'13 `var'maternal_death
	label var `var'maternal_death "maternal_deaths"
	rename `var'14 `var'neonatal_death
	label var `var'neonatal_death "neonatal_deaths"
	rename `var'15 `var'neo_hosp_mortality
	label var `var'neo_hosp_mortality "neonatal_hospital_mortality"
	rename `var'16 `var'outpatient_cytology
	label var `var'outpatient_cytology "outpatient_cytology_procedures"
	rename `var'17 `var'diabetes_primary
	label var `var'diabetes_primary "people_with_diabetes_primary_care_consultation_and_glycated_hemoglobin_request_last_6_months"
	rename `var'18 `var'diabetes_specialized
	label var `var'diabetes_specialized "people_with_diabetes_specialized_care_consultations"
	rename `var'19 `var'hypertension_primary
	label var `var'hypertension_primary "people_with_hypertension_primary_care_consultation_and_bp_measurement_last_6_months"
	rename `var'20 `var'hypertension_specialized
	label var `var'hypertension_specialized "people_with_hypertension_specialized_care_consultations"
	rename `var'21 `var'premature
	label var `var'premature "premature_births"
	}

* Drop indicator weights, did not end up using
drop wt*

* Convert municipality id to long 
destring municipality_id, replace	
save "${working}\parquet.dta", replace



*-------------------------------------------------------------------------------*
*					Import and Process HDI Data									*
*-------------------------------------------------------------------------------*
* Import Raw Data
import excel "${data}/HDI.xls", sheet("Séries") firstrow clear

* Rename to match other data
rename (Sigla Codigo Município) (state_abbr municipality_id municipality_name)

* Reshape Long 
reshape long Q, i(state_abbr municipality_id municipality_name) j(period) string
 
* Rename Outcome Variable
rename Q hdi 
label var hdi "Fijan HDI"

* Fix Dates
	gen yy = substr(period,3,2 )
	order state_abbr municipality_id municipality_name hdi yy
	keep state_abbr municipality_id municipality_name hdi yy 

* Destring municipality ID and remove last digit
replace municipality_id = substr(municipality_id, 1,6)
destring municipality_id, replace 

/* Fuzzy Merge no longer needed, issue resolved
	****************************************************
	* Municipality ID's differ, so Fuzzy Merge Below   *
	****************************************************
	
	* Replace Municipality Names as upper & Save Data
	replace municipality_name = upper(municipality_name)
	save "${working}/hdi.dta", replace 

	* Save seperate version for fuzzy merge Index
	rename (state_abbr municipality_id municipality_name) (state_hdi id_hdi name_hdi)
		* Keep one obs per municipality
		duplicates drop id_hdi, force
		save "${working}/hdi_match.dta", replace 




	* Load data with KPI's municipality ID Structure
	use "${working}/KPI_post.dta", replace
	duplicates drop municipality_id, force
	save "${working}/combinedData_match.dta", replace

	matchit municipality_id municipality_name using "${working}/hdi_match.dta", idusing(id_hdi) txtusing(name_hdi)

	* Merge in state from both to ensure they match
	merge m:1 id_hdi using "${working}/hdi_match.dta", nogen 
	merge m:1 municipality_id using "${working}/combinedData_match.dta", nogen 
	drop hdi yy
	drop if state_hdi != state_abbr
	keep if similscore>0.9 //manually reviewed all non-perfect matches
	egen maxScore = max(similscore), by(id_hdi)
	keep if similscore>maxScore-0.001 //rounding prevents equality here
	gsort -similscore
	keep id_hdi state_abbr municipality_id
	save "${working}hdi_kpi_map.dta", replace

	* Process and save 
	use "${working}/hdi.dta", clear 
	rename (municipality_id municipality_name) (id_hdi name_hdi)
	merge m:1 id_hdi using "${working}hdi_kpi_map.dta"
	
	*Compare state_distribution with merged state distribution to ensure we don't cause geographic bias //seems random
	tab state_abbr if _merge==1
	tab state_abbr if _merge ==3 //merged state distribution
	keep if _merge ==3
	drop _merge 
	
	drop id_hdi name_hdi
*/
save "${working}/hdi.dta", replace 






*-------------------------------------------------------------------------------*
*				Erase Intermediate Files to Avoid Clutter						*
*-------------------------------------------------------------------------------*
foreach kpi in diabetes prenatal Std Dental Cyrvical Vaccines hypertension {
	erase "${working}/`kpi'.dta"
}
erase  "${working}/temp.dta"
//erase "${working}/hdi_match.dta" 
//erase "${working}/combinedData_match.dta"
//erase "${working}hdi_kpi_map.dta"
