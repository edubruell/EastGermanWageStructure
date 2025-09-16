cap log close
log using ${log}\05_educ_broad.log, replace


/*
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	SIAB Preparation
	
	Create broader education groups based on imputed education ("ausbildung_imp")
	
		Generates the variables:
		- educ: Education (university and university of applied science combined), imputed based on Fitzenberger, Osikominu & Voelter (2008)
	
	Author(s): Wolfgang Dauth, Johann Eppelsheimer
	
	Version: 1.0
	Created: 2020-03-23
	
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/

********************************************************************************
* Generate a more general education variable based on imputed education from the FDZ
********************************************************************************

* generate broader education classes
gen educ = .
replace educ = 3 if inlist(ausbildung_imp, 5, 6)	// degree from an university or university of applied science (Uni or FH)
replace educ = 2 if inlist(ausbildung_imp, 2, 4)	// vocational training (Ausbildung)
replace educ = 1 if inlist(ausbildung_imp, 1, 3)	// neither vocational training or degree from universtiy (of applied science)

* define and set labels
label variable educ "Education, imputed based on Fitzenberger, Osikominu & Voelter (2008)"

label define lblValEduc 1 "1 no vocational training" 2 "2 vocational training" 3 "3 university or university of applied science"
label values educ lblValEduc


********************************************************************************
* Comparision
********************************************************************************
tab ausbildung_imp educ, m



log close
