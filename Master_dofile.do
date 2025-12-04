* Do file purpose: One Click replication, taking attached raw-data to generate all associated tables used in memo for API209/DEV401 final exercise for Herman, Max, and Maxine.
* Created by: Herman Lange
* Last Modified: 12/02/2025


* Required User Input: Update Global root below to reflect filepath in which Project_root is saved on your local pc.


*------------------------------------------------------------------------------*
*							Define Globals									   *
*------------------------------------------------------------------------------*
clear
global root = "/Users/hermanlange/Desktop/Project_Root"
global data = "${root}/Raw_data"
global working = "${root}/Working"
global dofiles = "${root}/Do_files"
global output = "${root}/Output"

*------------------------------------------------------------------------------*
*						Install Necessary Programs							   *
*------------------------------------------------------------------------------*
ssc install pq
ssc install matchit
ssc install freqindex
ssc reghdfe
 

*------------------------------------------------------------------------------*
*		1. Read in HDI, KPIs, other outcomes						   		   *
*------------------------------------------------------------------------------*
do "${root}/01_import_raw.do"

*------------------------------------------------------------------------------*
*		2. Harmonize IDs, reshape, merge to municipality–time panel    		   *
*------------------------------------------------------------------------------*
do "${root}/02_clean_merge.do"


*------------------------------------------------------------------------------*
*		3. construct KPIs, health index (if any), distance-to-target, etc	   *
*------------------------------------------------------------------------------*
do "${root}/03_construct_vars.do"

*------------------------------------------------------------------------------*
*		4. Main regressions (FE, IV, reduced form)							   *
*------------------------------------------------------------------------------*
do "${root}/04_analysis_main.do"


