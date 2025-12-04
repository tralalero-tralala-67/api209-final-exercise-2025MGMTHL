* Do file purpose: main regressions (FE, IV, reduced form)
* Created by: Herman Lange
* Last Modified: 12/02/2025



*------------------------------------------------------------------------------*
*								Table 1: Composite KPI & HDI Across FE		   *
*------------------------------------------------------------------------------*
* Load Data	
clear 
eststo clear
use "${working}/combined_regressionReady.dta", clear
keep if hdi!=.
			 
* HDI
	eststo clear

	* (1) No FE
	eststo m1: regress ln_hdi ratioCombined ///
		, cluster(municipality_id)

	* (2) Municipality FE only
	eststo m2: reghdfe ln_hdi ratioCombined ///
		, absorb(municipality_id) cluster(municipality_id)

	* (3) State FE only
	eststo m3: reghdfe ln_hdi ratioCombined ///
		, absorb(state_abbr) cluster(municipality_id)

	* (4) Time FE only
	eststo m4: reghdfe ln_hdi ratioCombined ///
		, absorb(period) cluster(municipality_id)

	* (5) State + time FE
	eststo m5: reghdfe ln_hdi ratioCombined ///
		, absorb(state_abbr period) cluster(municipality_id)

	* (6) Municipality + time FE
	eststo m6: reghdfe ln_hdi ratioCombined ///
		, absorb(municipality_id period) cluster(municipality_id)

	* (7) State×time FE
	eststo m7: reghdfe ln_hdi ratioCombined ///
		, absorb(state_time) cluster(municipality_id)

	*---------------------------------------------------------*
	*  Export LaTeX table 
	*---------------------------------------------------------*
	esttab m1 m2 m3 m4 m5 m6 m7 using "${output}/table2_fe_variants_index_hdi.tex", ///
		replace se star(* 0.1 ** 0.05 *** 0.01) ///
    b(3) se(3) ///
    label ///
    mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)") ///
    nonumbers ///
    mgroups("HDI (Log)", ///
            pattern(1 0 0 0 0 0 0) span ///
            prefix(\multicolumn{@span}{c}{) ///
            suffix(}) ) ///
    keep(ratioCombined) order(ratioCombined) ///
    stats(N r2, fmt(%9.0fc %9.5f) labels("Observations" "R^2")) ///
    title("Fixed-Effects Variants for the Baseline KPI--Outcome Association") ///
	nonotes

	
*------------------------------------------------------------------------------*
*						Table 2: Individual + Composite KPI & HDI  		       *
*------------------------------------------------------------------------------*
* List of KPI components
local kpis ratio_vaccine ratio_diabetes ratio_hypertension ///
           ratio_stdTest ratio_dental ratio_prenatalVisit ///
           ratio_papsmear ratioCombined

eststo clear
local j = 1

foreach x of local kpis {

    capture drop kpi_tmp
    gen double kpi_tmp = `x'

    quietly reghdfe ln_hdi kpi_tmp, absorb(municipality_id period) ///
        cluster(municipality_id)

    eststo m`j'
    local ++j
}

*---------------------------------------------*
*  Export LaTeX table: 8 columns (7 KPIs + composite)
*---------------------------------------------*
esttab m1 m2 m3 m4 m5 m6 m7 m8 using "${output}/table_kpi_components_hdi.tex", ///
    replace se star(* 0.1 ** 0.05 *** 0.01) ///
    b(3) se(3) ///
    label ///
    keep(kpi_tmp) ///
    coeflabels(kpi_tmp "KPI") ///
    mtitles("KPI 1" "KPI 2" "KPI 3" "KPI 4" "KPI 5" "KPI 6" "KPI 7" "Composite") ///
    nonumbers ///
    mgroups("HDI (Log)", ///
        pattern(1 0 0 0 0 0 0 0) span ///
        prefix(\multicolumn{@span}{c}{) ///
        suffix(}) ) ///
    stats(N r2, fmt(%9.0fc %9.5f) labels("Observations" "R^2")) ///
    nonotes
		
			
	
*------------------------------------------------------------------------------*
*						Table 3: First-Stage Estimates 			 		       *
*------------------------------------------------------------------------------*	

* 1. List of KPI outcomes (continuous variables)
local kpis  ratio_vaccine ratio_diabetes ratio_hypertension ///
            ratio_stdTest ratio_dental ratio_prenatalVisit ///
            ratio_papsmear ratioCombined_full

* 2. Matching list of Post × initial-distance variables
*    (adjust names here if your actual variables differ)
local posts post_dist_vaccine post_dist_diabetes post_dist_hypertension ///
           post_dist_stdTest post_dist_dental post_dist_prenatal ///
           post_dist_papsmear post_dist

eststo clear

local i = 1
foreach y of local kpis {

    * Pick the matching post_dist variable for this KPI
    local x : word `i' of `posts'

    quietly reghdfe `y' `x', ///
        absorb(municipality_id period) ///
        cluster(municipality_id)

    eststo m`i'
    local ++i
}

*----------------------------------------------------*
* Export LaTeX table: 8 columns (7 KPIs + composite)
*----------------------------------------------------*
esttab m1 m2 m3 m4 m5 m6 m7 m8 using "${output}/table_kpi_did_components.tex", ///
    replace se star(* 0.1 ** 0.05 *** 0.01) ///
    b(3) se(3) ///
    label ///
    keep(post_dist_vaccine post_dist_diabetes post_dist_hypertension ///
         post_dist_stdTest post_dist_dental post_dist_prenatal ///
         post_dist_papsmear post_dist) ///
    coeflabels( ///
        post_dist_vaccine     "Post × Dist (Vaccination)" ///
        post_dist_diabetes    "Post × Dist (Diabetes)" ///
        post_dist_hypertension "Post × Dist (hypertension)" ///
        post_dist_stdTest     "Post × Dist (HIV/syphilis)" ///
        post_dist_dental      "Post × Dist (Prenatal)" ///
        post_dist_papsmear    "Post × Dist (Pap smear)" ///
        post_dist    "Post × Dist (composite)" ///
    ) ///
    mtitles("KPI 1" "KPI 2" "KPI 3" "KPI 4" "KPI 5" "KPI 6" "KPI 7" "Composite") ///
    nonumbers ///
    mgroups("KPI outcomes (levels)", ///
        pattern(1 0 0 0 0 0 0 0) span ///
        prefix(\multicolumn{@span}{c}{) ///
        suffix(}) ) ///
    stats(N r2, fmt(%9.0fc %9.5f) labels("Observations" "R^2")) ///
    nonotes	
	
	
*------------------------------------------------------------------------------*
*						Table 4: Reduced-Form Estimates 			 		   *
*------------------------------------------------------------------------------*		

* List of Post × distance variables (the RF regressors)
local posts post_dist_vaccine post_dist_diabetes post_dist_hypertension ///
           post_dist_stdTest post_dist_dental post_dist_prenatal ///
           post_dist_papsmear post_dist

eststo clear
local j = 1

foreach x of local posts {

    * Use a common name so table has one row of coefficients
    capture drop post_tmp
    gen double post_tmp = `x'

    quietly reghdfe ln_hdi post_tmp, ///
        absorb(municipality_id period) ///
        cluster(municipality_id)

    eststo m`j'
    local ++j
}

*----------------------------------------------------*
* Export LaTeX table: 8 columns (7 KPIs + composite)
*----------------------------------------------------*
esttab m1 m2 m3 m4 m5 m6 m7 m8 using "${output}/table_kpi_reducedform_hdi.tex", ///
    replace se star(* 0.1 ** 0.05 *** 0.01) ///
    b(3) se(3) ///
    label ///
    keep(post_tmp) ///
    coeflabels(post_tmp "Post × initial distance") ///
    mtitles("KPI 1" "KPI 2" "KPI 3" "KPI 4" "KPI 5" "KPI 6" "KPI 7" "Composite") ///
    nonumbers ///
    mgroups("Log HDI (reduced form)", ///
        pattern(1 0 0 0 0 0 0 0) span ///
        prefix(\multicolumn{@span}{c}{) ///
        suffix(}) ) ///
    stats(N r2, fmt(%9.0fc %9.5f) labels("Observations" "R^2")) ///
    nonotes	
	
	
*------------------------------------------------------------------------------*
*						Table 5: IV Estimates	 					 		   *
*------------------------------------------------------------------------------*		

* 1. List of KPI variables (endogenous regressors)
local kpis  ratio_vaccine ratio_diabetes ratio_hypertension ///
            ratio_stdTest ratio_dental ratio_prenatalVisit ///
            ratio_papsmear ratioCombined_full

* 2. Matching list of Post × distance instruments
local posts post_dist_vaccine post_dist_diabetes post_dist_hypertension ///
           post_dist_stdTest post_dist_dental post_dist_prenatal ///
           post_dist_papsmear post_dist   

eststo clear
local j = 1

foreach k of local kpis {

    * Matching instrument for this KPI
    local z : word `j' of `posts'

    capture drop kpi_tmp post_tmp
    gen double kpi_tmp  = `k'
    gen double post_tmp = `z'

    quietly ivreghdfe ln_hdi (kpi_tmp = post_tmp), ///
        absorb(municipality_id period) ///
        cluster(municipality_id)

    eststo m`j'
    local ++j
}

*----------------------------------------------------*
* Export LaTeX table: 8 columns (7 KPIs + composite)
*----------------------------------------------------*
esttab m1 m2 m3 m4 m5 m6 m7 m8 using "${output}/table_kpi_iv_hdi.tex", ///
    replace se star(* 0.1 ** 0.05 *** 0.01) ///
    b(3) se(3) ///
    label ///
    keep(kpi_tmp) ///
    coeflabels(kpi_tmp "KPI (2SLS)") ///
    mtitles("KPI 1" "KPI 2" "KPI 3" "KPI 4" "KPI 5" "KPI 6" "KPI 7" "Composite") ///
    nonumbers ///
    mgroups("Log HDI (IV)", ///
        pattern(1 0 0 0 0 0 0 0) span ///
        prefix(\multicolumn{@span}{c}{) ///
        suffix(}) ) ///
    stats(N r2, fmt(%9.0fc %9.5f) labels("Observations" "R^2")) ///
    nonotes
			
	
	
	
*------------------------------------------------------------------------------*	
*			Coefficient Plot (CHAT GPT Helped Bring this Vision to Life)	   *
*------------------------------------------------------------------------------*		
*============================================================*
* 0. Define lists (KPI vars and matching Post×dist vars)
*============================================================*
local kpis  ratio_vaccine ratio_diabetes ratio_hypertension ///
            ratio_stdTest ratio_dental ratio_prenatalVisit ///
            ratio_papsmear ratioCombined_full

local posts post_dist_vaccine post_dist_diabetes post_dist_hypertension ///
           post_dist_stdTest post_dist_dental post_dist_prenatal ///
           post_dist_papsmear post_dist     // composite

*============================================================*
* 1. Reduced-form regressions: ln(HDI) on Post×distance_k
*    Store as rf1–rf8
*============================================================*
eststo clear
local j = 1

foreach z of local posts {

    capture drop post_tmp
    gen double post_tmp = `z'

    quietly reghdfe ln_hdi post_tmp, ///
        absorb(municipality_id period) ///
        cluster(municipality_id)

    eststo rf`j'
    local ++j
}

*============================================================*
* 2. IV regressions: ln(HDI) on KPI_k, instrumented by Post×distance_k
*    Store as iv1–iv8 (do NOT clear the rf estimates!)
*============================================================*
local j = 1

foreach k of local kpis {

    local z : word `j' of `posts'

    capture drop kpi_tmp post_tmp
    gen double kpi_tmp  = `k'
    gen double post_tmp = `z'

    quietly ivreghdfe ln_hdi (kpi_tmp = post_tmp), ///
        absorb(municipality_id period) ///
        cluster(municipality_id)

    eststo iv`j'
    local ++j
}

*============================================================*
* 3. Build a dataset with coefficients and 95% CIs
*    16 rows: 8 KPIs × (Reduced form, 2SLS)
*============================================================*
preserve
clear
set obs 16

gen kpi_id = .          // 1–8
gen spec   = ""         // "Reduced form" or "2SLS"
gen coef   = .
gen se     = .

local row = 1
forvalues j = 1/8 {

    * ---- Reduced form for KPI j ----
    est restore rf`j'
    matrix b = e(b)
    matrix V = e(V)
    scalar b_rf  = b[1,1]
    scalar se_rf = sqrt(V[1,1])

    replace kpi_id = `j'           in `row'
    replace spec   = "Reduced form" in `row'
    replace coef   = b_rf          in `row'
    replace se     = se_rf         in `row'
    local ++row

    * ---- IV (2SLS) for KPI j ----
    est restore iv`j'
    matrix b = e(b)
    matrix V = e(V)
    scalar b_iv  = b[1,1]
    scalar se_iv = sqrt(V[1,1])

    replace kpi_id = `j'   in `row'
    replace spec   = "2SLS" in `row'
    replace coef   = b_iv  in `row'
    replace se     = se_iv in `row'
    local ++row
}


* 90% confidence intervals
scalar z90 = invnormal(0.95)   // ≈ 1.645
gen ub = coef + z90*se
gen lb = coef - z90*se

* Convert to % change in HDI **per 10 p.p. change in regressor**
foreach v in coef se lb ub {
    replace `v' = 100*`v' / 10   // 100 for % in HDI, /10 for 0.10 change in X
}


* Drop KPI 7 (Pap smear) from BOTH specs in the plot
drop if kpi_id == 7

* Renumber KPIs so y-axis is contiguous again
egen kpi_id_new = group(kpi_id)
drop kpi_id
rename kpi_id_new kpi_id


* Label KPIs on the y-axis (KPI 7 now = composite)
local kpi_ylabs ///
    1 "KPI 1: Vaccination" ///
    2 "KPI 2: Diabetes" ///
    3 "KPI 3: Hypertension" ///
    4 "KPI 4: HIV/syphilis" ///
    5 "KPI 5: Dental (preg.)" ///
    6 "KPI 6: Prenatal visits" ///
    7 "Composite KPI"



*============================================================*
* Coefficient plots in two panels: Reduced form vs 2SLS
*============================================================*
twoway ///
    (rcap lb ub kpi_id if spec=="Reduced form", ///
        horizontal lwidth(medthick) lcolor("107 174 214")) ///
    (scatter kpi_id coef if spec=="Reduced form", ///
        msymbol(O) msize(medlarge) mcolor("8 48 107")), ///
    xline(0, lpattern(shortdash)) ///
    ylabel(`kpi_ylabs', angle(0)) ///
    ytitle("") ///
	xtitle("Change in HDI (%) for 10 pp increase in incentive intensity", size(vsmall)) ///
    xscale(range(-0.5 0.8)) ///
    title("Reduced form") ///
    legend(off) ///
    name(gr_rf, replace) ///
    scheme(s1color)

twoway ///
    (rcap lb ub kpi_id if spec=="2SLS", ///
        horizontal lwidth(medthick) lcolor("107 174 214")) ///
    (scatter kpi_id coef if spec=="2SLS", ///
        msymbol(D) msize(medlarge) mcolor("8 48 107")), ///
    xline(0, lpattern(shortdash)) ///
    ylabel(`kpi_ylabs', angle(0)) ///
    ytitle("") ///
	xtitle("Change in HDI (%) for 10 pp increase in KPI", size(vsmall)) ///
    xscale(range(-15 5)) ///
    title("2SLS") ///
    legend(off) ///
    name(gr_iv, replace) ///
    scheme(s1color)


	
graph combine gr_rf gr_iv, cols(2)
   // note("Bars show 95% confidence intervals. Pap smear KPI omitted from figure due to weak first stage.")
	
graph export "${output}/coefficientPlot.png", replace
	
	
	
	
	
	
	
	