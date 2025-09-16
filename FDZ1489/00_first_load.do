cap log close
log using ${log}\00_first_load.log, replace


// Load SIAB, restrict sources and generate variables 'jahr' and age
*******************************************************************
use ${orig}\siab_7521_v1.dta

// restrict sources to employment and benefit recipience spells
tab quelle, m
keep if inlist(quelle,1,2,3)

// generate jahr (= year)
gen jahr = year(begepi)
label variable jahr "year"

// generate age
gen age = jahr - gebjahr
label variable age "age (in years)"

cap log close
