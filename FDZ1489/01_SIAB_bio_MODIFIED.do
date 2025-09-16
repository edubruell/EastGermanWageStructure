cap log close
log using ${log}\02_EberleSchmucker.log, replace

/*************************************************************************\
|    Authors:         Johanna Eberle, Alexandra Schmucker                 |
|    Date:            June 2017                                           |
|    SIAB version:    7514 v1                                             |
|    Purpose:         Generation of cross sectional data and              |
|                     biographical variables                              |
|    Do-file version: v0                                                  |
|                                                                         |
|    List of changes                                                      |
|    --------------------                                                 |
|    - commented out lines 66 - 76 and 296 - 301 (Wolfgang Dauth, Johann Eppelsheimer)    |
|                                                                         |
\*************************************************************************/

/*------------------------------------------------------------------------------------------------*\
  GENERATE ADDITIONAL BIOGRAPHIC VARIABLES FROM LONGITUDINAL DATA

	First day in employment (ein_erw):
		- Date of entry into first employment
		- Periods of vocational training are ignored (erwstat = 102, 121, 122, 141, 144)
		  --> Missing for persons continuously in vocational training in SIAB
		- Entry into first employment can be later than entry in first establishment or job, since the latter variables include periods of vocational training

	Number of days in employment (tage_erw):
		- Total number of days a person was employed until the end of the observation or until cutoff date 
		- Periods of vocational training are ignored (erwstat = 102,121,122,141,144)
		  --> Value 0 for persons who are in vocational training throughout
		  
	First day in establishment (ein_bet):
		- Date of entry into establishment
		- Includes periods of vocational training
		- Not affected by interruptions of employment
		  --> unique for every combination of person and establishment

	Number of days in establishment (tage_bet):
		- Number of days in establishment until the end of the observation or until cutoff date
		- Includes periods of vocational training 
		- Gaps are subtracted

	First day in job (ein_job):
		- Start date of current job
		- Periods of vocational training are considered as separate jobs (even if there is no time lag between training and job)
		- Re-employment after interruptions in the same establishment are considered as new jobs if:
		  a) the reason of notification to social security agency implies end of employment: (grund = 30, 34, 40, 49) 
			 + Time gap > 92 days
		  b) an other reason of notification exists
			 + Time gap > 366 days

	Number of days in job (tage_job):
		- Number of days in current job until the end of the observation or until cutoff date
		- see ein_job

	Number of benefit receipts (anz_lst):
		- Number of benefit receipts accoring to SGB II or SGB III
		- Sources LeH LHG
		- Time gaps are ignored if interruption < 10 days
		- Change of benefit type doesn't count as new benefit receipt

	Number of days with benefit receipt (tage_lst):
		- Duration of benefit receipts until the end of the observation or until cutoff date
		- see anz_lst
		- Gaps are subtracted

\*------------------------------------------------------------------------------------------------*/

/*

<<<<< THE FOLLOWING BLOCK OF CODE HAS BEEN COMMENTED OUT IN ORDER TO MAKE THE DO-FILE COMPATIBLE WITH THE CODE FROM 
Dauth/Eppelsheimer (2020):
 "Preparing the Sample of Integrated Labour Market Biographies (SIAB) for Scientific Analysis: A Guide" >>>>>

cap log close
log using ${log}/01_${logname}_bio.log, replace

* LOAD SIAB DATA
use ${orig}/${dataname}.dta, clear

* DROP VARIABLES NOT NEEDED FOR YOUR ANALYSIS
keep persnr betnr spell quelle begorig endorig begepi endepi tentgelt teilzeit erwstat grund // Add more variables if necessary for your analysis

/*frau gebjahr nation nation_gr famst kind ausbildung schule beruf beruf2010_3 beruf2010_4 niveau gleitz leih befrist 
  estatvor estatnach profil art_kuend arbzeit restanspruch traeger alo_beg alo_dau wo_kreis wo_bula wo_aa wo_rd */ 

<<<<< END OF COMMENTED SECTION >>>>>
 */

*---------------------------------------*
* OBSERVATION COUNTER                   *
*---------------------------------------*

* OBSERVATION COUNTER PER EPISODE AND SOURCE
bysort persnr begepi quelle (spell): gen byte level1 = _n-1

* OBSERVATION COUNTER PER EPISODE
bysort persnr begepi (spell): gen byte level2 = _n-1

assert !missing(level1, level2)

*---------------------------------------*
* FIRST DAY IN EMPLOYMENT  (ein_erw)    *
*---------------------------------------*

sort persnr spell

* TAG VOCATIONAL TRAINING
gen byte azubi = inlist(erwstat, 102, 121, 122, 141, 144)

* TAG EMPLOYMENTS (WITHOUT VOCATIONAL TRAINING)
gen byte emp = 1 if azubi != 1 & quelle == 1

* TAG DATE OF FIRST EMPLOYMENT
bysort persnr emp (begorig): gen int h = begorig[1] if emp ==1

* ASSIGN DATE OF FIRST EMPLOYMENT TO ALL OBSERVATIONS
bysort persnr: egen int ein_erw = max(h)
replace ein_erw = .n if ein_erw ==.
format ein_erw %tdDD_Mon_CCYY

drop h


*-----------------------------------------*
* NUMBER OF DAYS IN EMPLOYMENT (tage_erw) *
*-----------------------------------------*

* COUNTER OF EMPLOYMENT OBSERVATIONS PER EPISODE (WITHOUT VOCATIONAL TRAINING)
bysort persnr emp begepi (spell): gen byte nrE = _n if emp == 1

* AUXILIARY VARIABLE FOR EMPLOYMENT DURATIONS (EXCLUDING PARALLEL EPISODES)
gen int d = endepi - begepi +1 if nrE == 1

* RUNNING TOTAL OF JOB DURATIONS
bysort persnr (begepi nrE): gen int tage_erw = sum(d)
sort persnr spell
drop d emp nrE


*--------------------------------------*
* FIRST DAY IN ESTABLISHMENT (ein_bet) *
*--------------------------------------*

bysort persnr betnr: egen int ein_bet = min(begepi) if !missing(betnr)
format ein_bet %tdDD_Mon_CCYY


*--------------------------------------------*
* NUMBER OF DAYS IN ESTABLISHMENT (tage_bet) *
*--------------------------------------------*

* AUXILIARY VARIABLE MARKS DUPLICATE OBSERVATIONS PER ESTABLISHMENT AND EPISODE
bysort persnr betnr begepi endepi (spell): gen byte nrB = _n if !missing(betnr)

* CALCULATION OF THE DURATION
gen int dauer = endepi - begepi +1 if nrB == 1

* RUNNING TOTAL OF DAYS IN ESTABLISHMENT
bysort persnr betnr (spell): gen int tage_bet = sum(dauer) if !missing(betnr)
drop dauer


*----------------------------*
* FIRST DAY IN JOB (ein_job) *
*----------------------------*


* SORT OBSERVATIONS BY PERSON, ESTABLISHMENT (VOCATIONAL TRAINING SEPARATE) AND EPISODE
sort persnr azubi betnr spell

* MARK SUBSEQUENT EPISODES OF JOB
* FIRST OBSERVATION IS SET TO MISSING
gen byte job = 1 if persnr == persnr[_n-1] & betnr == betnr[_n-1] & azubi == azubi[_n-1] & !missing(betnr)

* CONSIDER ENDING NOTIFICATION OF PREVIOUS MAIN EMPLOYMENT IN CASE OF GAPS
bysort persnr azubi betnr begepi (spell): gen byte end = 1 if inlist(grund[1], 30, 34, 40, 49)

* COUNT AS NEW JOB IF EMPLOYER REPORTED END OF EMPLOYMENT AND GAP > 92 DAYS
* COUNT AS NEW JOB IF GAP > 366 DAYS
gen int gap = begepi - endepi[_n-1] -1 if job == 1 
replace job = . if end[_n-1] == 1 & gap > 92
replace job = . if gap > 366

* GENERATE START DATE OF JOB, THEN COPY IT TO SUBSEQUENT OBSERVATIONS OF SAME JOB
gen int ein_job = begepi if !missing(betnr)
replace ein_job = ein_job[_n-1] if job ==1
format ein_job %tdDD_Mon_CCYY
drop end gap


*----------------------------------*
* NUMBER OF DAYS IN JOB (tage_job) *
*----------------------------------*

* AUXILIARY VARIABLE FOR NUMBER OF PARALLEL EPISODES PER JOB
* VOCATIONAL TRAINING SEPARATE
bysort persnr azubi betnr begepi (spell): gen byte nrA = _n if !missing(betnr)

gen int jobdauer = endepi - begepi +1 if !missing(betnr)
gen int jobdauer_kum = jobdauer
gen int jobdauer_dup = 0

* AUXILIARY VARIABLE FOR DURATION OF PARALLEL EPISODES IN SAME ESTABLISHMENT (VOCATIONAL TRAINING SEPARATE)
replace jobdauer_dup = jobdauer if nrA != 1

* RUNNING TOTAL OF JOB DURATION (MINUS DURATION OF PARALLEL EPISODES)
replace jobdauer_kum = jobdauer_kum[_n-1] + jobdauer - jobdauer_dup if job ==1
gen int tage_job = jobdauer_kum

drop jobdauer jobdauer_kum job jobdauer_dup nrB nrA


*--------------------------------------*
* NUMBER OF BENEFIT RECEIPTS (anz_lst) *
*--------------------------------------*

* MARK EPISODE OF BENEFIT RECEIPT (DATA SOURCES LeH, LHG)
gen byte quelleL = inlist(quelle, 2, 16)

* COUNTER OF BENEFIT RECEIPTS WITHIN EPISODE
bysort persnr begepi quelleL (quelle spell): gen byte nrL  = _n if quelleL

* COPY END DATE OF LAST BENEFIT RECEIPT TO SUBSEQUENT OBSERVATIONS
sort persnr spell
gen int ende_vor = .
replace ende_vor = endepi[_n-1] if quelleL[_n-1] & persnr == persnr[_n-1]
replace ende_vor = ende_vor[_n-1] if missing(ende_vor) & persnr == persnr[_n-1]
format ende_vor %td

* MARK OBSERVATIONS THAT COUNT AS SEPARATE BENEFIT RECEIPTS
* Only 1 per episode, separate benefit receipt if gap > 10 days 
gen byte lst = quelleL & nrL == 1 & (begepi - ende_vor > 10)

* RUNNING TOTAL OF BENEFIT RECEIPTS
gsort persnr begepi -lst
gen int anz_lst = lst
replace anz_lst = anz_lst[_n-1] + lst if persnr == persnr[_n-1]
drop ende_vor lst


*------------------------------------------------*
* NUMBER OF DAYS WITH BENEFIT RECEIPT (tage_lst) *
*------------------------------------------------*

* DURATION OF BENEFIT RECEIPT (WITHOUT DURATION OF PARALLEL OBSERVATIONS)
gen int lstdauer = endepi - begepi +1 if quelleL ==1 & nrL ==1 

* RUNNING TOTAL OF DAYS IN BENEFIT RECEIPT
bysort persnr (spell): gen int tage_lst = sum(lstdauer) if nrL ==1	| quelleL ==0

* FILL VARIABLE FOR PARALLEL OBSERVATIONS
bysort persnr begepi (nrL): replace tage_lst = tage_lst[1]

drop lstdauer quelleL nrL 


*-----------------------*
* ADJUST MISSING VALUES *
*-----------------------*

foreach var of varlist ein_bet tage_bet ein_job tage_job {
	replace `var' = .n if quelle != 1
	replace `var' = .z if quelle ==1 & missing(`var')
}
replace ein_erw = .z if missing(ein_erw)


*--------------------------------------------------*
* LABEL THE GENERATED VARIABLES                    *
*--------------------------------------------------*

label language en
label variable ein_erw    "First day in employment"
label variable tage_erw   "Number of days in employment"
label variable ein_bet    "First day in establishment"
label variable tage_bet   "Number of days in establishment"
label variable ein_job    "First day in job"
label variable tage_job   "Number of days in job"
label variable anz_lst    "Number of benefit receipts"
label variable tage_lst   "Number of days with benefit receipt"

label language de
label variable ein_erw    "Eintrittsdatum Erwerbsleben"
label variable tage_erw   "Dauer Erwerbsleben"
label variable ein_bet    "Eintrittsdatum Betrieb"
label variable tage_bet   "Dauer Betrieb"
label variable ein_job    "Eintrittsdatum Job"
label variable tage_job   "Dauer Job"
label variable anz_lst    "Anzahl bisheriger Leistungsbezuege"
label variable tage_lst   "Dauer bisheriger Leistungsbezuege"


foreach var of varlist ein_erw ein_bet tage_bet ein_job tage_job {

	label language en
	lab def `var'_en .n "n/a" .z "no entry", add modify
	lab val `var' `var'_en
	numlabel `var'_en, add mask("#  ")

	label language de
	lab def `var'_de .n "fehlt syst." .z "k. Angabe", add modify
	lab val `var' `var'_de
	numlabel `var'_de, add mask("#  ")

}

/*

<<<<< THE FOLLOWING BLOCK OF CODE HAS BEEN COMMENTED OUT IN ORDER TO MAKE THE DO-FILE COMPATIBLE WITH THE CODE FROM 
Dauth/Eppelsheimer (2020):
 "Preparing the Sample of Integrated Labour Market Biographies (SIAB) for Scientific Analysis: A Guide" >>>>>

compress
sort persnr spell
save ${data}/${dataname}_bio.dta, replace
d
log close
exit

*/


cap log close
