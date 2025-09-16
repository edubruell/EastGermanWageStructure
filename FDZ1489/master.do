/**************************************************************************
 * Nutzer:        Eduard Bruell
 * Datum:         26.03.2025
 * SIAB-Version:  R 7521 V1
 * Project:       FDZ 1489
 *************************************************************************/

/*------------------------------------------------------------------------
                         MASTER DO DATEI SIAB PANEL
------------------------------------------------------------------------*/

version 17
clear
set more off
set matsize 1000
set linesize 255
// adopath ++ "$prog"
global prepare_data = 1

/*--------------------------------*
 *    PROGRAMME AUFRUFEN          *
 *--------------------------------*/

// Load custom programs (Do file diagnostics)
do ${prog}\eb00_programs.do

********************************************************************************
* Slightly modified Dauth et al. (2023) data preparation routine               *
********************************************************************************
// OBSERVATION PERIOD
global minYear 1995    // Begin of your observation period (default: 1975)
global maxYear 2021    // End of your observation period (default: 2017)
	
if "$prepare_data" == "1" {
	display "Runing prepare pass"
	/*
    // 00) Load SIAB for the first time and generate year and age
    do ${prog}\00_first_load.do

    // 01a) Split episodes that span over one year
    do ${prog}\01_split_episodes.do

    // 01b) Generate biographical variables (tage_erw, tage_bet, ...)
    /* Uses a slightly modified version of '01_SIAB_bio.do' from FDZ-Methodenreport
       06/2017 (Johanna Eberle & Alexandra Schmucker). The first and last lines 
       of the original file have been commented out to ensure compatibility. */
    do ${prog}\01_SIAB_bio_MODIFIED.do

    // 02) Merge BHP data and switch the labels from German to English
    do ${prog}\02_merge_BHP.do

    // 03a) Map 3-digit industries to 1-digit industries (based on Statistisches Bundesamt)
    do ${prog}\03a_industries_1digit_destatis.do

    // 03b) Map 3-digit industries to 1-digit industries (based on IAB establishment panel)
    do ${prog}\03b_industries_1digit_iab.do

    // 04) Add Blossfeld occupations
    do ${prog}\04_occ_blossfeld.do

    // 05) Create broader education groups
    do ${prog}\05_educ_broad.do

    // 06) Add the contribution assessment ceiling
    do ${prog}\06_wages_assessment_ceiling.do

    // 07) Add the marginal part-time income threshold and flag marginal wages
    do ${prog}\07_wages_marginal.do

    // 08) Deflate wages, marginal part-time income threshold, and contribution assessment ceiling
    do ${prog}\08_wages_deflation.do

    // Restrict the data set to selected years
    keep if jahr >= $minYear & jahr <= $maxYear

    // 09) Impute wages (2-step procedure based on Dustmann et al. (2009) and Card et al. (2013))
    do ${prog}\09_wages_imputation.do

	//Start from saved intermediate data here!*/
    use ${data}\siab_intermediate.dta, clear

    // 10) Treat parallel episodes:
    /* Generate information on parallel episodes and keep only the 'main' episode */
    do ${prog}\10_parallel_episodes.do

    // 11) Transfer data set into yearly panel
    do ${prog}\11_yearly_panel.do

    // 12) Restrict the data to certain groups
    do ${prog}\12_restrictions.do

    // 13) Clean up
    //do ${prog}\13_clean_up.do

    // Erase intermediate version of SIAB (optional)
    //erase ${data}\siab_intermediate.dta
	
	//Save the dataset
	local l_date = c(current_date)
	local l_time = c(current_time)
	label data "FDZ1489 SIAB Panel - `l_date' - `l_time'"

    // Save final data
    save ${data}\siab_panel.dta, replace
} 

********************************************************************************
* Main project code                                                            *
********************************************************************************

do ${prog}\eb01_overview.do
//do ${prog}\eb02_bug_check.do

/*-------------------------------------------------------
 *    AUSGABE EINER DATEILISTE ALS LOG-DATEI
 *-------------------------------------------------------*/
capture log close
log using "${log}\dateiliste.log", replace
dir $prog\*
dir $log\*
dir $data\*
dir $orig\*
log close
