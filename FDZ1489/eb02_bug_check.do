cap log close
log using "$log/eb02_bug_check.log", replace

/*
This Do-File creates most basic statistics and decomopositions
shown in "Evolution of the East German Wage Structure".

Note that most of the outputs is post-processed in R to create
graphs for RIF- and DFL-Decompositions. 

TABLE OF CONTENTS:
------------------
*/

//==============================================================================
// 1. Lohn-Perzentile und Lohn-Komponenten-Perzentile nach Ost/West x Jahr
//==============================================================================

//Load Data that will be needed
use ${data}\siab_panel.dta, clear

describe

//generate counter variables for shares
drop if east==.
drop n_persnr
drop n_betr
egen n_betr   = tag(year east betnr)
egen n_persnr = tag(year east persnr)


preserve
	keep if year==1999
	gcollapse (p15) p15 = mwage (p50) p50 = mwage  (p85) p85 = mwage (sum) n_persnr n_betr, by(east year)
	mkmat east year p15 p50 p85 n_persnr n_betr, mat(m_pct)
	matrix list m_pct
restore


gdistinct persnr if year==1999
gdistinct betnr if year==1999

// Load SIAB, restrict sources and generate variables 'jahr' and age
*******************************************************************
use ${orig}\siab_7521_v1.dta, clear

describe

// restrict sources to employment and benefit recipience spells
tab quelle, m
keep if inlist(quelle,1,2,3)

// generate jahr (= year)
gen year = year(begepi)

// generate age
gen age = jahr - gebjahr
label variable age "age (in years)"

gdistinct persnr if year==1999
gdistinct betnr if year==1999

egen n_betr   = tag(year east betnr)
egen n_persnr = tag(year east persnr)

preserve
	keep if year==1999
	gcollapse (sum) n_persnr n_betr, by(east year)
	mkmat east year p15 p50 p85 n_persnr n_betr, mat(m_pct)
	matrix list m_pct
restore

cap log close
