cap log close
log using ${log}\12_restrictions.log, replace



/*
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	SIAB Preparation
	
	Restricts the data to certain groups
	
	On default most commands are commented out. Users should uncomment and adjust selected lines.
	
	
	Author(s): Wolfgang Dauth, Johann Eppelsheimer

	Version: 1.0
	Created: 2020-03-23
	
	
	Comments: 
	Users can also run this do-file on the finished clean_siab.dta.
	
	Note, that this do-file can be very handy if you have several related research projects. 
	For instance, you could generate one base version of siab_clean.dta	and then create several
	'offsprings' by running 12_restrictions.do with different settings.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		
*/

********************************************************************************
* Age limits
********************************************************************************
sum age

drop if age < 20
drop if age > 62

********************************************************************************
* Short hand and helper variables
********************************************************************************
rename jahr year

//What is the last year someone is in our data
sort persnr year
by persnr: gegen last_year_in_data = max(year)
//What is the first year someone is there
by persnr: gegen first_year_in_data = min(year)

//Indicator for leaving our dataset in a given year
gen leaves = 0
by persnr: replace leaves = 1 if  last_year_in_data == year

rename frau female
label define fem 0 "Male" 1 "Female"
label values female fem

//Define Labels
cap label define reglab 0 "West" 1	"East"
cap label define femlab 0 "Men" 1	"Women"
cap label define agelab 1  "20 to 27" 2  "28 to 31" 3  "32 to 36" 4  "37 to 40" 5  "41 to 44" 6  "45 to 49" 7  "50 to 54" 8  "55 to 62"

//Generate 8 quantiles of age and label them
xtile age_q = age, nq(8)
label values age_q agelab
tab age_q

//Monthly wage
gen mwage = wage_imp * 30

//Compute residuals for the monthly wage
gen mresid = .
forvalues f=0/1{
	forvalues e=0/1{
		reg mwage i.age_q i.educ i.educ#i.age_q if east==`e' & female==`f', robust
		predict resid_e`e'_f`f' if east==`e' & female==`f', r
		quietly sum mwage if east==`e' & female==`f'
		replace mresid = resid_e`e'_f`f' + r(mean) if east==`e' & female==`f'
	}
}

drop resid*

gen lwresid = .
//Compute residuals for the log wage
forvalues f=0/1{
	forvalues e=0/1{
		reg lwage_imp i.age_q i.educ i.educ#i.age_q if east==`e' & female==`f', robust
		predict resid_e`e'_f`f' if east==`e' & female==`f', r
		replace lwresid = resid_e`e'_f`f'  if east==`e' & female==`f'
	}
}


//Firm size
gen firm_size = .
replace firm_size = 1 if  az_ges>=1 & az_ges<=10
replace firm_size = 2 if  az_ges>10 & az_ges<=25
replace firm_size = 3 if  az_ges>25 & az_ges<=100
replace firm_size = 4 if  az_ges>100 & az_ges<=500
replace firm_size = 5 if  az_ges>500
#delimit ;
label define fsize_lab 1 "1-10 Employees"
					   2 "10-25 Employees"
					   3 "25-100 Employees"
					   4 "100-500 Employees"
					   5 "More than 500 Employees";
#delimit cr

label values firm_size fsize_lab

//Firm Age
gen firm_age = year - grd_jahr

//Quantiles of firm age
xtile firm_age_q = firm_age, nq(4)
cap label define fageq_lab 1  " 0 to 8" 2  "9 to 18" 3  "19 to 25" 4  "26 and older"
label values firm_age_q fageq_lab
tab firm_age_q

********************************************************************************
* Data source
********************************************************************************
tab quelle

keep if quelle == 1		// BeH  (Employee History)
* keep if quelle == 2		// LEH  (Benefit Recipient History)
* keep if quelle == 3		// LHG  (Unemployment Benefit II Recipient History)
* keep if quelle == 4		// MTH  (Participants in Measure History)
* keep if quelle == 5		// XMTH (Participants in Measure History)
* keep if quelle == 6		// ASU  (Jobseeker History)
* keep if quelle == 7		// XASU (Jobseeker History)


********************************************************************************
* Years
********************************************************************************
tab year

* keep if jahr >= 1975
* keep if jahr <= 2017

********************************************************************************
* Male/female
********************************************************************************
tab female, m

* drop if female == 0	    // keep women only
drop if female == 1			// keep men only
drop if missing(female)		// drop missings


********************************************************************************
* Occupational status
********************************************************************************
tab erwstat, m

keep if erwstat == 101							// only keep employees liable to social security ("Sozialversicherungspflichtig Beschaeftigte")

* drop if erwstat < 100								// drop if not in employment

* drop if missing(erwstat)							// drop if missing employment status

* drop if inlist(erwstat, 102, 121, 122, 141)		// drop trainees
* drop if inlist(erwstat, 109, 209)					// drop marginal part-time workers
* drop if inlist(erwstat, 105, 106)					// drop interns/trainees
* drop if inlist(erwstat, 140, 141, 142, 143)		// drop sailors
* drop if inlist(erwstat, 123)						// drop voluntary social/ecological year, ...
* drop if inlist(erwstat, 103, 119)					// drop partial retirement
* drop if inlist(erwstat, 112)						// drop family member workers in agriculture
* drop if inlist(erwstat, 203)						// drop artists
* drop if inlist(erwstat, 118, 205)					// drop casual workers
* drop if inlist(erwstat, 201)						// drop employees registered within households
* drop if inlist(erwstat, 104, 120, 124, 142)		// drop other groups with less than 100 workers
* drop if inlist(erwstat, 599)						// drop others



********************************************************************************
* Occupations
********************************************************************************
tab beruf, m
tab beruf2010_3, m

* drop ...


********************************************************************************
* Full-time
********************************************************************************
tab teilzeit, m

keep if teilzeit == 0		// keep only full-time workers



********************************************************************************
* Wages below marginal part-time income threshold ("Geringfuegigkeitsgrenze")
********************************************************************************
sum tentgelt
tab marginal

* drop if marginal == 1 & quelle == 1	// drop workers below marginal part-time income threshold



********************************************************************************
* Missing establishment identifier
********************************************************************************
count if missing(betnr) & quelle == 1

* drop if missing(betnr) & quelle == 1

/*
	Comment:
	
	The variables betnr refers to the workplace of individuals.
	Hence, people not in employment have missings in this variable!
*/


********************************************************************************
* Missing regional information of WORKPLACE
********************************************************************************
count if missing(ao_kreis) & quelle == 1

* drop if missing(ao_bula) & quelle == 1

/*
	Comment:
	
	The variables ao_bula refers to the workplace of individuals.
	Hence, people not in employment have missings in this variable!
*/


********************************************************************************
* WORKPLACE in East/West
********************************************************************************
tab state east, m

* keep if east == 0									// workplace in West (excl. Berlin) (drops all non-working individuals!)
* keep if east == 1 								// workplace in East (incl. Berlin) (drops all non-working individuals!)
* keep if ao_bula <= 11								// workplace in West (incl. Berlin) (drops all non-working individuals!)
* keep if inlist(ao_bula, 11, 12, 13, 14, 15, 16)	// workplace in East (incl. Berlin) (drops all non-working individuals!)

/* 
	Comment:
	
	east = 1: workplace in East Germany
	east = 0: workplace in West Germany
	Berlin is assigned to East
	
	For further details see ao_bula (from BHP).
	
	The variables east and ao_bula refer to the workplace of individuals.
	Hence, people not in employment have missings in these variables!
*/


********************************************************************************
* Other restrictions and variable definitions
********************************************************************************

//Industry restrictions and definitions
drop if industry1_destatis==1 | industry1_destatis==2 | industry1_destatis==16

  
gen sector = .
replace sector = 1 if industry1_destatis == 3          //Mining
replace sector = 2 if inrange(industry1_destatis,4,5)   //Manufacturing
replace sector = 3 if industry1_destatis == 6           //Construction
replace sector = 4 if inrange(industry1_destatis,7,15)  //Services

//Idea: Public vs. Private sector
gen svbroad = sector
replace svbroad = 4 if inrange(industry1_destatis,12,14) //Eduaction, Pub. Admin, Health
replace svbroad = 5 if inrange(industry1_destatis,7,11) | industry1_destatis==15 //All other service industries

//merge AKM
merge m:1 persnr using "${orig}/SIAB_7521_v1_akm_pers.dta"
drop if _merge ==2
drop _merge

merge m:1 betnr using "${orig}/SIAB_7521_v1_akm_estab.dta"
drop if _merge==2
drop _merge


//Service Industry dummy
gen d_services = (w93_3_gen>=501)	

//Other sector defintion
gen sector_d = .
replace sector_d = 1 if inrange(w93_3_gen,11,145)                //Agriculture/Mining
replace sector_d = 2 if inrange(w93_3_gen,151,372)               //Manufacturing
replace sector_d = 3 if inrange(w93_3_gen,451,454)               //Construction
replace sector_d = 4 if inrange(w93_3_gen,501,954)               //Services
replace sector_d = 5 if inrange(w93_3_gen,401,410)               //Utilites

// Contemporaneous Fixed-Effects using AKM intervals from 1995 onward
gen contemp_ffe = .
replace contemp_ffe = feff_1993_1999   if year >= 1995 & year < 2000
replace contemp_ffe = feff_2000_2006   if year >= 2000 & year < 2007
replace contemp_ffe = feff_2007_2013   if year >= 2007 & year < 2014
replace contemp_ffe = feff_2014_2021   if year >= 2014 & year <= 2021

gen contemp_pfe = .
replace contemp_pfe = peff_1993_1999   if year >= 1995 & year < 2000
replace contemp_pfe = peff_2000_2006   if year >= 2000 & year < 2007
replace contemp_pfe = peff_2007_2013   if year >= 2007 & year < 2014
replace contemp_pfe = peff_2014_2021   if year >= 2014 & year <= 2021

// Quartiles of contemporaneous fixed effects (by region and year)
gquantiles q_firm = contemp_ffe, xtile nq(4) by(east year)
gquantiles q_pers = contemp_pfe, xtile nq(4) by(east year)
tab q_firm east,m
tab q_pers east,m

//firm fixed effect classes within east and year
gquantiles wr  = contemp_ffe, xtile nq(20) by(east year)

//Firm and non-firm wage components
quietly: reg mwage i.wr##i.east##i.year
predict firm_mwage 
gen l_fw = log(firm_mwage)
predict non_firm_wage, r

//Use w93_3_gen to create 2-digit industries
gen ind2d = floor(w93_3_gen/10)

gen construction = sector_d == 3

//Firm size quartiles
sum az_ges, detail
gquantiles fs_quant = az_ges, xtile nq(4) by(year)
replace    fs_quant = 9999 if fs_quant == .

//Bet and Persnr for the collapse counters
/*
gsort betnr 
by betnr: gen n_betr = _n==_N
gsort persnr 
by persnr: gen n_persnr = _n==_N
*/
egen n_betr   = tag(year east betnr)
egen n_persnr = tag(year east persnr)

//Compute RIFs for our wage percentiles
gegen ey = group(east year)
gsort ey 
foreach p in "15" "50" "85"{
	display "Compute RIF for `p'-pct."
	by ey: egen rif_`p'=rifvar(mwage),  q(`p') 
}

by ey: egen w_85_50 = rifvar(lwage_imp), iqr(50 85)
by ey: egen w_50_15 = rifvar(lwage_imp), iqr(15 50)

tab educ, gen(e)
gen occ3 = beruf

gen sub40 = (age<=40)

//Interacted effects
gegen age_educ = group(sub40 educ)
gegen demo_detail = group(age_q educ)
gegen fctrl = group(educ sub40 industry1_destatis)
gegen sec_educ = group(educ sector)

log close


