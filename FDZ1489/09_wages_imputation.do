cap log close
log using ${log}\09_wages_imputation.log, replace


/*
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	SIAB Preparation
	
	Imputation of right-censored wages
	
	2-Step imputation procedure, similar to Dustmann et al. (2009) and Card et al. (2013):
		- Step 1: based on Gartner (2005)
		- Step 2: imputation models including leave-one-out means
		
	Generates the variables:
		- cens: 1 if right-censored/imputed wage, 0 otherwise; (4 EUR below assessment ceiling)
		- wage: daily wage, not imputed, top-coded wages replaced by assessment ceiling (-4 EUR), deflated (2015)
		- wage_imp: imputed daily wage, deflated (2015)
	

	Author(s): Wolfgang Dauth, Johann Eppelsheimer
	The authors thank Johannes Ludsteck for his valuable suggestions!
	
	Version: 1.0
	Created: 2020-03-23
	
	
	References:
	
	Card, D., J. Heining, and P. Kline (2013). Workplace heterogeneity and the rise of West German wage inequality. The Quarterly Journal of Economics 128 (3), 967-1015
	Dustmann, C., J. Ludsteck, and U. Schönberg (2009). Revisiting the German wage structure. The Quarterly Journal of Economics 124 (2), 843-881.
	Gartner, H. (2005).  The imputation of wages above the contribution limit with the German IAB employment sample. FDZ-Methodenreport 02/2005: http://doku.iab.de/fdz/reporte/2005/MR_2.pdf
	
	https://twitter.com/marxmatt/status/1104570847648456704?s=20
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/



********************************************************************************
* Modify assessment limit and flag censored wages
********************************************************************************

* substracte 4 EUR from exact assessment limit (to make sure all cencored wages are covered by the imputation)
gen limit_assess4 = limit_assess_defl - (100 * 4/cpi)		// ... use deflated 4 Euros
gen ln_limit_assess4 = log(limit_assess4)
label variable limit_assess4 "Contribution assessment ceiling minus 4 EUR"
label variable ln_limit_assess4 "Log of contribution assessment ceiling minus 4 EUR"

* flag censored wages:
gen cens = 0
replace cens = 1 if wage_defl > limit_assess4 & quelle == 1		// only flag BeH spells (quelle == 1)
label variable cens "1 if right-censored/imputed wage, 0 otherwise; (4 EUR below assessment ceiling)"


********************************************************************************
* Overview of censored wages
********************************************************************************

* overall censoring
tab cens if quelle == 1, m

* censoring by education
tab cens educ if quelle == 1, m
tab cens if educ == 3 & quelle == 1		// a huge share of high-skilled workers' wages are censored!

* censorig of high-skilled workers by age groups
forvalues a = 25(5)60 {
	disp "subsample high-skilled: age < `a':"
	tab cens if educ == 3 & age > 18 & age < `a' & quelle == 1
}

* censoring by fulltime/part-time
tab cens teilzeit if quelle == 1

* censoring by gender
tab cens frau if quelle == 1

* censoring for men in fulltime employment
tab cens if frau == 0 & teilzeit == 0 & quelle == 1

* censoring for high-skilled men in regular fulltime employment
tab cens if frau == 0 & teilzeit == 0 & quelle == 1 & erwstat == 101 & marginal == 0 & educ == 3

* censoring for high-skilled men above 40 in regular fulltime employment
tab cens if frau == 0 & teilzeit == 0 & quelle == 1 & erwstat == 101 & marginal == 0 & educ == 3 & age > 40



********************************************************************************
* Prepare dependent variable: wage
********************************************************************************

* wage
gen wage = wage_defl if quelle == 1
replace wage = limit_assess4 if quelle == 1 & wage_defl > limit_assess4
label variable wage "Daily wage, not imputed, top-coded wages replaced by assessment ceiling (-4 EUR)"

* log-wage
gen ln_wage = log(wage)

* log-wages with censored wages reported as missing
gen ln_wage_cens = ln_wage if cens == 0


********************************************************************************
* Prepare control variables for imputation
********************************************************************************
gen age_sq = (age/10)^2 			// express age squared in 100 years (to have readable coefficients in the regressions)
gen old  = age > 40					// dummy for "older" people
gen age_old = age * old 			// different age profiles for young and old workers
gen age_sq_old = age_sq * old		// age_old squared
gen tenure_sq = (tage_job/10)^2		// tenure squared

* control variables for imputation:
global controls frau old age age_sq age_old age_sq_old tage_job tenure_sq

* check for missings in control variables
foreach var of varlist $controls {
	disp "missings in `var':"
	count if missing(`var') & quelle == 1
}


/*
	Comment:
	
	In principle all variables from the actual analyses should be in the 
	imputation as well. Ommiting variables from the scientific analysis
	in the imputation might result in biased estimates. This is case 
	whether wage is the depenent or an independent variable.
	
	It is left to the user to complete or modify the list of control variables.
	
	Below we run separat regression for the selected groups. Hence, it might also 
	be reasonable to modify these groups.
*/



********************************************************************************
* Prepare groups; by default: year, skill group and East/West (by default we assign Berlin to East Germany)
********************************************************************************

* start and end year
sum jahr
global minYear = r(min)
global maxYear = r(max)

* education groups (for the imputation regard missings as low-skilled)
gen educ_tmp = educ
replace educ_tmp = 1 if missing(educ)
label values educ_tmp lblValEduc

sum educ_tmp 
global minEduc = r(min)		// minimum level of education
global maxEduc = r(max)		// maximum level of education



********************************************************************************
* Step 1: imputation with observable characteristics (for details refer to Gartner (2005))
********************************************************************************

* sort and set seed
sort persnr spell
set seed 123

* empty variable for (imputed) log-wages
gen ln_wage_imp = .

* create temporal copy of the dataset on the computer's C-drive. Loading data from there is considerably faster than loading data from the NAS.
* note: when working with large datasets (e.g., 30% sample of the IEB) you might exceed the local storage limits. In such cases, you need to manually assign storage locations on your network drive.
tempfile	temp_beforeimp
save		"`temp_beforeimp'", replace

/* Imputation will not be carried out for observations with missing east/west info:
   - Non-BEH Spells
   - Some establishments have missing locational info
   --> Save those in separate file but do not impute.
   */  
keep if missing(east)
replace ln_wage_imp = ln_wage if quelle==1
save ${data}\temp_0.dta, replace   
   
* separately run Tobit-wage-regressions by all groups
local c=0								// Running variable that will index temporary datasets
forvalues y = $minYear/$maxYear {		// loop over years

	display "$S_DATE $S_TIME: Step 1, processing year `y' ..."		// show current status of computations

	forvalues ed = $minEduc/$maxEduc {	// loop over education groups
		forvalues ew = 0/1 {			// loop over East/West
			use "`temp_beforeimp'", clear
			display "`c'. Year: `y' - Education: `ed' - East: `ew'"
			keep if jahr == `y' & educ_tmp == `ed' & east == `ew'   // Run tobit regression on small datasets rather than using if-condition. This reduces computation times tremendously!
			quietly{
				intreg ln_wage ln_wage_cens $controls if marginal == 0 	// Tobit estimates with right-censored data (since ln_wage_cens has missings) (excluding marginal wages)
			
				global sdi = e(sigma)									// save variance of residual from Tobit
				predict xbn if e(sample), xb							// predict xb from observables
			
				gen eta = (ln_limit_assess4 - xbn) / $sdi if e(sample)	// eta is a residual with standard normal distribution
			
				gen ln_wage_tmp = ln_wage if cens == 0					// note: do not use if e(sample) here; otherwise uncensored wages of obs. with missing covariables get dropped
				replace ln_wage_tmp = xbn + ///												// take xb from regression and add normally distributed random term
						$sdi * invnorm(normal(eta) + uniform()*(1-normal(eta))) ///		// with s.d. = sigma, that must be larger than social security contribution ceiling
						if e(sample) & cens == 1
			
				replace ln_wage_imp = ln_wage_tmp
			
				count if ln_wage_imp < ln_wage & e(sample)				// make sure the imputation produces no values below social security contribution ceiling
				if r(N) > 0 disp in red r(N) " imputed wage(s) below censoring limit in year " `y' " and education group " `ed'  " and east = " `ew'
			
				drop xbn eta ln_wage_tmp	// drop temporary variables
			}
			local c=`c'+1
			save ${data}\temp_`c'.dta, replace						// save data for year/education/east-combination temporarily on NAS
		}
	}
}

*Append individual parts
quietly{
	local files=`c'
	use ${data}\temp_0.dta
	forvalues c=1/`files' {
		append using ${data}\temp_`c'.dta
		erase ${data}\temp_`c'.dta
		}
	erase ${data}\temp_0.dta
}
********************************************************************************
* Intermediate step: obtain LEAVE-ONE-OUT means of imputed wages 
* (for workers and plants; something like worker or plant fixed effects)
********************************************************************************

* helper variable containing only ones
gen ones = 1

* worker mean-wages
bysort persnr quelle (spell): egen obs = total(ones)			// number of observations per worker and quelle (source)
by persnr quelle: egen ln_wage_sum = total(ln_wage_imp)			// sum wage over all employment episodes for each worker
gen ln_wage_mean_worker = (ln_wage_sum - ln_wage_imp)/(obs-1)	// subtract wage of respective episode (leave-one-out) and divide by number of other episodes
drop obs ln_wage_sum

gen only_one_obs = missing(ln_wage_mean_worker)							// dummy if worker only observed once (and denominator above (obs-1) = 0)
sum ln_wage_imp
replace ln_wage_mean_worker = r(mean) if missing(ln_wage_mean_worker)	// set mean to sample mean if worker only observed once
replace ln_wage_mean_worker = . if quelle != 1							// set non-BEH-spells to missing

* plant mean-wages
bysort jahr betnr (spell): egen obs = total(ones)				// number of worker episodes per plant and year
by jahr betnr: egen ln_wage_sum = total(ln_wage_imp)			// sum wage over all employment episodes per plant and year
gen ln_wage_mean_firm = (ln_wage_sum - ln_wage_imp)/(obs-1)		// subtract wage of respective worker (leave-one-out) and divide by number of other worker episodes
drop obs ln_wage_sum

gen only_one_worker = missing(ln_wage_mean_firm)							// dummy if worker is firm's only employee
by jahr: egen ln_wage_sample = mean(ln_wage_imp)
replace ln_wage_mean_firm = ln_wage_sample if missing(ln_wage_mean_firm)	// set mean to sample mean if worker is firm's only employee
replace ln_wage_mean_firm = . if quelle != 1								// set non-BEH-spells to missing

drop ln_wage_sample ones

	

********************************************************************************
* Step 2: extended imputation models including the leave-one-out means
********************************************************************************

* empty variable for (imputed) log-wages
gen ln_wage_imp2 = .

* create temporal copies of the dataset
tempfile	temp_beforeimp
save		"`temp_beforeimp'", replace

*Save observations that will be ignored in imputation 
keep if missing(east)
replace ln_wage_imp = ln_wage if quelle==1
save ${data}\temp_0.dta, replace   

* separately run Tobit-wage-regressions by all groups (including leave-one-out means)
local c=0							    	// Running variable that will index temporary datasets
forvalues y = $minYear/$maxYear {			// loop over years

	display "$S_DATE $S_TIME: Step 2, processing year `y' ..."

	forvalues ed = $minEduc/$maxEduc {		// loop over education groups
		forvalues ew = 0/1 {		
			// loop over East/West
			use "`temp_beforeimp'", clear
			keep if jahr == `y' & educ_tmp == `ed' & east == `ew'
			display "`c'. Year: `y' - Education: `ed' - East: `ew'"
			quietly{

				intreg ln_wage ln_wage_cens ln_wage_mean_worker only_one_obs ln_wage_mean_firm only_one_worker $controls if marginal == 0
			
				global sdi = e(sigma)									// save variance of residual from Tobit
				predict xbn if e(sample), xb							// predict xb from observables
			
				gen eta = (ln_limit_assess4 - xbn) / $sdi if e(sample)	// eta is a residual with s.d. = sigma
			
				gen ln_wage_tmp = ln_wage if cens == 0					// note: do not use if e(sample) here; otherwise uncensored wages of obs. with missing covariables get dropped
				replace ln_wage_tmp = xbn + ///  											// take xb from regression and add normally distributed random term
						$sdi * invnorm(normal(eta) + uniform()*(1-normal(eta))) /// 		// with s.d. = sigma, that must be larger than social security contribution ceiling
						if e(sample) & cens==1
					
				replace ln_wage_imp2 = ln_wage_tmp if jahr == `y' & educ_tmp == `ed' & east == `ew'
			
				count if ln_wage_imp2 < ln_wage & e(sample)				// make sure imputation produces no values below social security contribution ceiling
			}
			if r(N) > 0 disp in red r(N) " imputed wage(s) below censoring limit in year " `y' " and education group " `ed'  " and east = " `ew'
			drop xbn eta ln_wage_tmp		// drop temporal variables
			local c=`c'+1
			save ${data}\temp_`c'.dta, replace						// save data for year/education/east-combination temporarily on NAS
		}
	}
}

*Append individual parts
local files=`c'
use ${data}\temp_0.dta
forvalues c=1/`files' {
	append using ${data}\temp_`c'.dta
	erase ${data}\temp_`c'.dta
	}
erase ${data}\temp_0.dta



********************************************************************************
* Imputed wages in levels
********************************************************************************
gen wage_imp_int = exp(ln_wage_imp)		// intermediate wages from step 1 (for comparison))
gen wage_imp = exp(ln_wage_imp2)		// final version of imputed daily wages

label variable wage_imp_int "Intermediate version of imputed daily wage"
label variable wage_imp 	"Daily wage, imputed"


********************************************************************************
* Minor adjustments:
* 	- Limit imputed wages at 10 * 99th percentile (in extremely rare cases imputed wages could by chance be inplausible high)
*	- Replace missing wages from the second stage by imputed wages from the first stage (in the very rare case that plant-means of wages are extremely low, invnorm(...) returns missing values due to a numeric overflow)
********************************************************************************
sum wage_imp, d
global maxWage = 10 * r(p99)
replace wage_imp_int = $maxWage if wage_imp_int > $maxWage & !missing(wage_imp_int)
replace wage_imp 	 = $maxWage if wage_imp > $maxWage & !missing(wage_imp)

replace wage_imp = wage_imp_int if missing(wage_imp)

********************************************************************************
* Overview/Comparison of wages and imputed wages
********************************************************************************

* summary statistics
sum wage*
sum wage* if cens == 1
sum wage* if jahr == $maxYear
sum wage* if cens == 1 & jahr == $maxYear

* correlations
corr wage*
corr wage* if cens == 1
corr wage* if cens == 0
/*
* graphical comparison: histogram of wages and imputed wages (last year, 5 years prior, 10 years prior, 15 years prior, ...)
forvalues y = $maxYear (-5) $minYear {
	count if jahr == `y'
	global N_year = r(N)

	twoway__histogram_gen wage_imp_int 	if jahr == `y' , start(0) width(5) gen(temp_h1 temp_x1)
	twoway__histogram_gen wage_imp 		if jahr == `y' , start(0) width(5) gen(temp_h2 temp_x2)
	twoway__histogram_gen wage 			if jahr == `y' & !missing(wage_imp), start(0) width(5) gen(temp_h3 temp_x3)

	twoway ///
		(bar temp_h1 temp_x1 if inrange(temp_x1,0,300), barwidth(5) fcolor(orange%40) lcolor(none) lwidth(0)) 			///
		(bar temp_h2 temp_x2 if inrange(temp_x2,0,300), barwidth(5) fcolor(white%0) lcolor(red) lwidth(0.25)) 				///
		(bar temp_h3 temp_x3 if inrange(temp_x3,0,300), barwidth(5) fcolor(black%70) lcolor(none) lwidth(0)) 				///
		, legend(rows(1) order(3 "Raw" 1 "Imputed, step 1" 2 "Imputed, step 2")) ///
		xlabel(0(50)300) ///
		title("Distribution of wages in `y'") ///
		note("Number of observations = $N_year") ///
		scheme(s2mono)
		
	graph export ${graphs}\wage_imputation_`y'.png, replace width(1280)
	
	drop temp_x1 temp_h1 temp_x2 temp_h2 temp_x3 temp_h3  
}
*/


gen lwage_imp = ln(wage_imp)
********************************************************************************
* Clean up
********************************************************************************
drop 	educ_tmp old age_sq age_old age_sq_old tenure_sq						///
		ln_wage_mean_worker only_one_obs ln_wage_mean_firm only_one_worker		///
		limit_assess4 ln_limit_assess4 ln_wage_cens wage_imp_int ln_wage_imp 

tab jahr

// Save intermediate version to disk
save ${data}\siab_intermediate.dta, replace

log close
