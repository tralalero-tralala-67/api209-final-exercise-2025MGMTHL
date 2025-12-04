* Do file purpose: construct KPIs, health index (if any), distance-to-target, etc.
* Created by: Herman Lange
* Last Modified: 12/02/2025


*------------------------------------------------------------------------------*
*						Define KPIs								    		   *
*------------------------------------------------------------------------------*
use "${working}/combined.dta", clear 





*------------------------------------------------------------------------------*
*						Define Composite KPI					    		   *
*------------------------------------------------------------------------------*
* Define Bounded Composite KPI 
preserve 
	keep municipality_id period ratio*	

	gen frac_prenatal     = ratio_prenatalVisit/0.45
	gen frac_stdTest      = ratio_stdTest/0.6
	gen frac_dental       = ratio_dental/0.6
	gen frac_papsmear     = ratio_papsmear/0.4
	gen frac_vaccine      = ratio_vaccine/0.95
	gen frac_hypertension = ratio_hypertension/0.5
	gen frac_diabetes     = ratio_diabetes/0.5

	* Cap each at 1 (100% of target) 
	foreach v in frac_prenatal frac_stdTest frac_dental frac_papsmear ///
				frac_vaccine frac_hypertension frac_diabetes {
		replace `v' = 1 if `v' > 1 & !missing(`v')
	}

	* Final Synthetic Indicator (0–10), using official weights
	gen ratioCombined = 1*frac_prenatal     ///
        + 1*frac_stdTest      ///
        + 2*frac_dental       ///
        + 1*frac_papsmear     ///
        + 2*frac_vaccine      ///
        + 2*frac_hypertension ///
        + 1*frac_diabetes
	
	* Re-Scale to 0-1:
	replace ratioCombined = ratioCombined/10
	label var ratioCombined "Composite KPI (std)"

	keep municipality_id period ratioCombined
	tempfile temp 
	save `temp'
restore 
merge 1:1 municipality_id period using `temp', keep(3) nogen

* Define unbounded KPI
gen ratioCombined_full = ratio_prenatalVisit/0.45 + /// 
				ratio_stdTest/0.6 + /// 
				2*ratio_dental/0.6 + /// 
				ratio_papsmear/0.4 + /// 
				2*ratio_vaccine/0.95 + /// 
				2*ratio_hypertension/0.5 + /// 
				ratio_diabetes/0.5 
				
* Re-Scale to 0-1:
replace ratioCombined_full = ratioCombined_full/10
label var ratioCombined_full "Composite KPI"



*------------------------------------------------------------------------------*
*						Define Distance-to-Target							   *
*------------------------------------------------------------------------------*
* Define Time Dimension for regressions
gen time_id = .
replace time_id = 1 if period == "20_2"
replace time_id = 2 if period == "20_3"
replace time_id = 3 if period == "21_1"
replace time_id = 4 if period == "21_2"
replace time_id = 5 if period == "21_3"
replace time_id = 6 if period == "22_1"
replace time_id = 7 if period == "22_2"
replace time_id = 8 if period == "22_3"
replace time_id = 9 if period == "23_1"
replace time_id = 10 if period == "23_2"
replace time_id = 11 if period == "23_3"
replace time_id = 12 if period == "24_1"
replace time_id = 13 if period == "24_2"
replace time_id = 14 if period == "24_3"
replace time_id = 15 if period == "25_1"

xtset municipality_id time_id


* Define post 
gen post = (time_id>5)

* Define Composite KPI distance
	* Continous
	gen dist = 1 - ratioCombined if period == "21_3"
	egen dist_comp = max(dist), by(municipality_id)
	label var dist_comp "Distance to Target 21_3 (continous)"
	drop dist 
	
	* Dummy
	sum dist_comp, d
	gen dist_comp_d = (dist_comp>.4727632)
	label var dist_comp_d "Distance to Target 21_3 (High)"

* Define Individual KPI Distances:
preserve 
keep if time_id==5
keep municipality_id ratio* 	
    * Fractions of target
    gen dist_prenatal     = ratio_prenatalVisit / 0.45   // KPI 1
    gen dist_stdTest      = ratio_stdTest       / 0.60   // KPI 2
    gen dist_dental       = ratio_dental        / 0.60   // KPI 3
    gen dist_papsmear     = ratio_papsmear      / 0.40   // KPI 4
    gen dist_vaccine      = ratio_vaccine       / 0.95   // KPI 5
    gen dist_hypertension = ratio_hypertension  / 0.50   // KPI 6
    gen dist_diabetes     = ratio_diabetes      / 0.50   // KPI 7
	
	* Cap distance at 1
	foreach kpi in dist_prenatal dist_stdTest dist_dental dist_papsmear dist_vaccine dist_hypertension dist_diabetes {
		replace `kpi' = 1 if `kpi' >1 & `kpi' !=.
		replace `kpi' = 1-`kpi'
		label var `kpi' "Initial Distance to Target `kpi'"
	}

	keep municipality_id dist*
	tempfile kpiDist 
	save `kpiDist'
restore 
joinby municipality_id using `kpiDist', unmatched(master)


*------------------------------------------------------------------------------*
*						Define Instrument (Post x Target)					   *
*------------------------------------------------------------------------------*
gen post_dist = post*dist_comp
label var post_dist "Post x Distance (Cont)"
gen post_dist_d = post*dist_comp_d
label var post_dist_d "Post x Distance (Dummy)"

foreach kpi in dist_prenatal dist_stdTest dist_dental dist_papsmear dist_vaccine dist_hypertension dist_diabetes {
	gen post_`kpi' = `kpi' *post 
	label var post_`kpi' "Post x Distance to `kpi'"
}


*------------------------------------------------------------------------------*
*						Define Health Index	(3 versions)		    		   *
*------------------------------------------------------------------------------*

	* Maternal & perinatal outcomes (all are bad when high)
	local direct_maternal ///
		ratioanomalies ///
		ratiolow_weight ///
		ratiofetal_deaths ///
		ratiomaternal_death ///
		rationeonatal_death ///
		rationeo_hosp_mortality
	corr ratioanomalies ratiolow_weight ratiofetal_deaths ratiomaternal_death rationeonatal_death rationeo_hosp_mortality
	
	* Chronic disease outcomes (hospitalization rates)
	local direct_chronic ///
		ratiodiabetes_hospitalized ///
		ratiohypertension_hospitalized
	corr ratiodiabetes_hospitalized ratiohypertension_hospitalized
	 
	* (If later you want "indirect" outcomes like prenatal visit distribution,
	*  vaccination coverage, etc., you can define another local list here.)

	*------------------------------------------------------
	* 2. Flip sign so that higher values = better health
	*    (all listed variables are "bad" when high)
	*------------------------------------------------------

	foreach v of local direct_maternal {
		gen `v'_n = -1 * `v'
		label var `v'_n "`: var label `v'' (flipped: higher=better)"
	}

	foreach v of local direct_chronic {
		gen `v'_n = -1 * `v'
		label var `v'_n "`: var label `v'' (flipped: higher=better)"
	}

	*------------------------------------------------------
	* 3. Standardize each component (z-scores)
	*    Here: standardized over the full sample.
	*    If you want pre-reform-only standardization later, we can tweak.
	*------------------------------------------------------

	foreach v of local direct_maternal {
		egen z_`v' = std(`v'_n)
		label var z_`v' "Z: `: var label `v'' (higher=better)"
	}

	foreach v of local direct_chronic {
		egen z_`v' = std(`v'_n)
		label var z_`v' "Z: `: var label `v'' (higher=better)"
	}

	*------------------------------------------------------
	* 4. Build composite indices
	*    - index_maternal_perinatal: maternal & perinatal outcomes
	*    - index_chronic: chronic disease complications
	*    - index_health_overall: average of the two
	*------------------------------------------------------

	* Collect all z-variables into locals for rowmean()
	local z_maternal
	foreach v of local direct_maternal {
		local z_maternal `z_maternal' z_`v'
	}

	local z_chronic
	foreach v of local direct_chronic {
		local z_chronic `z_chronic' z_`v'
	}

	* Maternal & perinatal index
	egen index_maternal_perinatal = rowmean(`z_maternal')
	egen miss_maternal = rowmiss(`z_maternal')
	replace index_maternal_perinatal = . if miss_maternal > 0
	drop miss_maternal
	label var index_maternal_perinatal ///
		"Composite index: maternal & perinatal health (higher=better)"

	* Chronic disease index
	egen index_chronic = rowmean(`z_chronic')
	egen miss_chronic = rowmiss(`z_chronic')
	replace index_chronic = . if miss_chronic > 0
	drop miss_chronic
	label var index_chronic ///
		"Composite index: chronic disease complications (higher=better)"

	* Optional: overall health index = average of the two indices
	egen index_health_overall = rowmean(index_maternal_perinatal index_chronic)
	egen miss_overall = rowmiss(`z_maternal' `z_chronic')
	replace index_health_overall = . if miss_overall
	drop miss_overall
	label var index_health_overall ///
		"Overall health index (higher=better)"


	* Drop intermediate variables
	foreach v of local direct_maternal {
		drop `v'_n z_`v'
	}

	foreach v of local direct_chronic {
		drop `v'_n z_`v'
	}
	

	
	
*------------------------------------------------------------------------------*
*						Define HDI as Log					    		   *
*------------------------------------------------------------------------------*
gen ln_hdi = ln(hdi)
label var ln_hdi "HDI (Log)"

*------------------------------------------------------------------------------*
*						Define Fixed Effects					    		   *
*------------------------------------------------------------------------------*
* State×time group for col (7)
egen state_time = group(state_abbr time_id), label

save "${working}/combined_regressionReady.dta", replace 
