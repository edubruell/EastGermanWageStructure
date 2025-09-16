cap log close
log using ${log}\02_merge_BHP.log, replace


/*
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	SIAB Preparation
	
	Merge the Establishment History Panel (BHP) to the SIAB	
	
	Author(s): Wolfgang Dauth, Johann Eppelsheimer
	
	Version: 1.0
	Created: 2020-03-23
	
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/



********************************************************************************
* Merge basic BHP information to individual data
********************************************************************************

* merge BHP
merge m:1 betnr jahr using ${orig}\SIAB_7521_v1_bhp_basis_v1.dta, keep(master match)

* inspect merge
tab erwstat _merge  // not matched individuals are in almost all cases non-workers
drop _merge


* rename variable to BHP_***
*foreach var of varlist w73_3 w93_5 w93_3 w03_5 w03_3 w08_5 w08_3 w73_3_gen group_w73_3 w93_3_gen group_w93_3 w08_3_gen group_w08_3 grd_jahr grd_dat lzt_jahr lzt_dat az_ges az_vz az_gf te_imp_mw ao_kreis ao_bula {
*	rename `var' BHP_`var'
*}


cap log close
