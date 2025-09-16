cap log close
log using ${log}\11_yearly_panel.log, replace


/*
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	SIAB Preparation
	
	Transfer data set into yearly panel (using one specific cutoff date)
	 - retain information on total employment / unemployment durations and earnings per year
	
	Generates the variables:
		- year_days_emp: total days employed per calendar year
		- year_days_benefits: total days benefit recipience per calendar year
		- year_labor_earn: total labor earnings per calendar year
		
	Drops the variables:
		- nspell
		- begorig
		- endorig
		- begepi
		- endepi
		- begepi_orig
		- endepi_orig

		
	Author(s): Wolfgang Dauth, Johann Eppelsheimer

	Version: 1.0
	Created: 2020-03-23
	
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*/



********************************************************************************
* Set cutoff date
********************************************************************************
global cutoffMonth	06		// cutoff month
global cutoffDay 	30		// cutoff day


/*
	Example:
	
	To set the cutoff date to June 30 assign 
		06 to cutoffMonth and 
		30 to cutoffDay.
*/



*------------------------------------------------------------------------------*
*  Aggregate employment outcomes: days employed, labor earnings 
*------------------------------------------------------------------------------*

*Durations of episodes
sort persnr jahr begepi endepi quelle
by persnr jahr begepi endepi: gen dur_emp = endepi-begepi + 1 if quelle == 1
by persnr jahr begepi endepi: gen dur_benefits = endepi-begepi + 1 if inlist(quelle, 2, 16) | parallel_benefits == 1

*Total time working, total time receiving UI benefits
by persnr jahr: egen year_days_emp=total(dur_emp)
by persnr jahr: egen year_days_benefits=total(dur_benefits)

*Earnings=wage*duration
gen help_labor_earn=parallel_wage_imp*dur_emp
by persnr jahr: egen year_labor_earn=total(help_labor_earn)
drop dur_emp dur_benefits help_labor_earn

lab var year_days_emp "Total days employed per calendar year"
lab var year_days_benefits "Total days benefit recipience per calendar year"
lab var year_labor_earn "Total labor earnings per calendar year"


********************************************************************************
* Transfer dates into strictly ascending numbers
********************************************************************************

* cutoff date
gen cutoff_num = 100 * $cutoffMonth + $cutoffDay

* begin of episodes
gen begepi_num = 100 * month(begepi) + day(begepi)

* end of episodes
gen endepi_num = 100 * month(endepi) + day(endepi)


********************************************************************************
* Flag episodes that include the cutoff date
********************************************************************************
gen cutoff_incl = 0
replace cutoff_incl = 1 if begepi_num <= cutoff_num & cutoff_num <= endepi_num

sum cutoff_incl


********************************************************************************
* Keep only episodes that include the cutoff date
********************************************************************************
keep if cutoff_incl == 1


********************************************************************************
* Adjust durations to end at cutoff date
********************************************************************************
foreach var of varlist tage_bet tage_job { 
	replace `var' = `var' - (endepi - mdy($cutoffMonth, $cutoffDay, jahr)) if quelle == 1
}

replace tage_erw = tage_erw - (endepi - mdy($cutoffMonth, $cutoffDay, jahr)) if quelle == 1 & !inlist(erwstat, 102, 121, 122, 141, 144)		// excluding Azubis
replace tage_lst = tage_lst - (endepi - mdy($cutoffMonth, $cutoffDay, jahr)) if inlist(quelle, 2, 16) | parallel_benefits == 1

		
********************************************************************************
* Clean up
********************************************************************************
drop cutoff_num begepi_num endepi_num cutoff_incl ///
	 nspell begorig endorig begepi endepi begepi_orig endepi_orig

log close
