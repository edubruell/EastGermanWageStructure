cap log close
log using ${log}\10_parallel_episodes.log, replace


/*
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	SIAB Preparation
	
	Treat parallel episodes: 
		- generate some information on parallel episodes
		- keep only 'main' episode
		
	Generates the variables:
		- nspell: non-parallel spell counter
		- parallel_jobs: number of parallel jobs
		- parallel_wage: total wage of all parallel employment spells
		- parallel_wage_imp: total imputed wage of all parallel employment spells
		- parallel_benefits: indicator for recipience of UI benefits
		
	Drops the variables:
		- spell
		- level1
		- level2
		
	
	Author(s): Wolfgang Dauth, Johann Eppelsheimer

	Version: 1.0
	Created: 2020-03-23
	
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*/



********************************************************************************
* Identify main episode (by sorting data accordingly)
********************************************************************************

/*
	Comment: 
	
	There is no common rule of identifying the main episode. Usually the job
	with the longest duration or the highest wage is defined as the main episode.
	However, it is left to the user to decide which characteristic defines the
	main episode.
	
*/

gsort persnr begepi quelle -tage_bet -wage_imp		// define job with longest tenure as main episode
* gsort persnr begepi quelle -wage_imp -tage_bet	// define job with highest wage as main episode



********************************************************************************
* Generate information on parallel jobs (parallel_jobs, parallel_wage, parallel_wage_imp)
********************************************************************************
gen tmp_jobs = quelle == 1
gen tmp_wage = tentgelt if quelle == 1
gen tmp_wage_imp = wage_imp if quelle == 1 

by persnr begepi: egen parallel_jobs = total(tmp_jobs)			// count parallel employment episodes
by persnr begepi: egen parallel_wage = total(tmp_wage)			// total wage of all parallel employment episodes
by persnr begepi: egen parallel_wage_imp = total(tmp_wage_imp)	// total (imputed) wage of all parallel employment episodes

label variable parallel_jobs "Number of parallel jobs"
label variable parallel_wage "Total wage per episode"
label variable parallel_wage_imp "Total (imputed) wage per episode"


********************************************************************************
* Generate information of possibly parallel UI benefits (parallel_benefits)
********************************************************************************
gen tmp_benefits = quelle == 2 | quelle == 16						// indicator for recipience of UI benefits
by persnr begepi: egen parallel_benefits = max(tmp_benefits)		// attach indicator for recipience of UI benefits to all parallel episodes
label variable parallel_benefits "Total UI benefits per episode" 


********************************************************************************
* Only keep the main episode
********************************************************************************
by persnr begepi: keep if _n == 1 


********************************************************************************
* Generate non-parallel spell counter
********************************************************************************
by persnr: gen nspell = _n
label variable nspell "Non-parallel spell counter" 


********************************************************************************
* Clean up
********************************************************************************
sort persnr nspell
drop spell level1 level2 tmp_jobs tmp_wage tmp_wage_imp tmp_benefits

log close
