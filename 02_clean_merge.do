* Do file purpose: harmonize IDs, reshape, merge to municipality–time panel
* Created by: Herman Lange
* Last Modified: 12/02/2025

*------------------------------------------------------------------------------*
*							Append KPI Data									   *
*------------------------------------------------------------------------------*
use "${working}/KPI_pre.dta", clear
keep state* muni* period* ratio*
append using "${working}/KPI_post.dta"

*------------------------------------------------------------------------------*
*							Merge in Parquet Data							   *
*------------------------------------------------------------------------------*
merge 1:1 municipality_id period using "${working}\parquet.dta", keep(3) nogen //merges to 83,400/83,402 municipality_quadrimesters

*------------------------------------------------------------------------------*
*							Merge in HDI Data								   *
*------------------------------------------------------------------------------*
* Generate annual measure to merge into 
gen yy = substr(period, 1,2)
merge m:1 state_abbr municipality_id yy using "${working}/hdi.dta", keep(1|3) nogen //keeping broader set (24 and 25) without HDI for trends

save "${working}/combined.dta", replace 


*------------------------------------------------------------------------------*
*							Erase Intermeiate Files							   *
*------------------------------------------------------------------------------*
erase "${working}/KPI_post.dta"
erase "${working}/KPI_pre.dta"
erase "${working}\parquet.dta"
erase "${working}/hdi.dta"
