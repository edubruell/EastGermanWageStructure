cap log close
log using "$log/eb01_overview.log", replace

/*
This Do-File creates most basic statistics and decomopositions
shown in "Evolution of the East German Wage Structure".

Note that most of the outputs is post-processed in R to create
graphs for RIF- and DFL-Decompositions. 

TABLE OF CONTENTS:
------------------
 1. Lohn-Perzentile und Lohn-Komponenten-Perzentile nach Ost/West x Jahr
 2. Lohnverteilung nach Alter X Bildungsgruppen  in Ost/West
 3. Kontrafaktische Verteilungen nach Imputationen von Leaver/Entrant Löhnen
	3a. Imputation von Leavern
	3b. Imputation von 
	3c. Entrant and Leaver Characteristics
 4. Standard-Abweichungen von Löhnen und Residual-Löhnen nach Ost/West x Jahr
 5. DFL- und RIF-Dekompositionen
	5a. DFL-Dekomposition
	5b. RIF-Dekomposition
*/

//==============================================================================
// 1. Lohn-Perzentile und Lohn-Komponenten-Perzentile nach Ost/West x Jahr
//==============================================================================

//Load Data that will be needed
use ${data}\siab_panel.dta, clear

//generate counter variables for shares
bys year east: egen total_obs=count(mwage!=.) 
gen sum_pct = 1/total_obs
drop if east==.

preserve
	gcollapse (p15) p15 = mwage (p50) p50 = mwage  (p85) p85 = mwage (sum) n_persnr n_betr, by(east year)
	mkmat east year p15 p50 p85 n_persnr n_betr, mat(m_pct)
restore

//Compute percentiles of AKM firm fixed-effects 
preserve
	gcollapse (p15) p15 = contemp_ffe (p50) p50 = contemp_ffe  (p85) p85 = contemp_ffe (sum) n_persnr n_betr, by(east year)
	mkmat east year p15 p50 p85 n_persnr n_betr, mat(m_pct_akm)
restore

//Compute percentiles of non-firm wage
preserve
	gcollapse (p15) p15 = non_firm_wage (p50) p50 = non_firm_wage  (p85) p85 = non_firm_wage (sum) n_persnr n_betr, by(east year)
	mkmat east year p15 p50 p85 n_persnr n_betr, mat(m_pct_nfw)
restore

//Compute percentiles of firm-specific wage components
preserve
	gcollapse (p15) p15 = firm_mwage (p50) p50 = firm_mwage  (p85) p85 = firm_mwage (sum) n_persnr n_betr, by(east year)
	mkmat east year p15 p50 p85 n_persnr n_betr, mat(m_pct_fw)
restore

/*Ab hier erfolgt noch eine Ausgabe von Perzentilen der Lohnverteilung und Perzentilen von Lohn-Komponenten.


 Struktur der Tabllen
 1. east       = "Indikator fuer Ost/Westdeutschland"
 2. year       = "Jahr"
 3. p15        = "15-Perzentil der Lohnverteilung/Lohn-Komponenten-Verteilung"
 4. p50        = "Median der Lohnverteilung/Lohn-Komponenten-Verteilung"
 5. p85        = "85-Perzentil der Lohnverteilung/Lohn-Komponenten-Verteilung"
 6. n_pers     = "Fuer die Berechnung verwendete Anzahl an Personen-Beobachtungen"
 7. n_Betr     = "Fuer die Berechnung verwendete Anzahl an Betrieben"
 
 Diese Ausgaben basieren aber immer auf einer kompletten Ost/West-Jahreszelle und erfüllen damit immer 
 die Mindestbeobachtungskriterien
 
 */

display "Ausgabe der Perzentile in Ost/West x Jahres-zellen ohne Gewichtung"
matrix list m_pct
display "Ausgabe der Perzentile der AKM-Firmeneffekt"
matrix list m_pct_akm 
display "Ausgabe der Perzentile der nicht firmen-spezifischen Lohnanteile"
matrix list m_pct_nfw
display "Ausgabe der Perzentile der firmen-spezifischen Lohnanteile"
matrix list m_pct_fw

//==============================================================================
// 2. Lohnverteilung nach Alter X Bildungsgruppen 
//==============================================================================

/*
//UNCOMMENTED SINCE IT HAS A PROBLEM WITH OBSERVATION COUNTS IN THE LOWEST EDUCATION CATEGORY!
use ${data}\siab_panel.dta, clear

keep if inlist(year, 1995, 2004,2009,2021) & lwage_imp < .
drop if east==.
bys year east: egen total_obs=count(mwage!=.) 

// Define age groups
gen age_group = .
replace age_group = 1 if age >= 20 & age <= 36
replace age_group = 2 if age >= 37 & age <= 47
replace age_group = 3 if age >= 48 & age <= 62
label define agegrp 1 "20-36" 2 "37-47" 3 "48-62"
label values age_group agegrp

//generate counter variables for shares
gen emp_share = 1/total_obs

preserve 
	#delimit ;
		collapse (p15) p15=lwage_imp 
				 (p50) p50=lwage_imp 
				 (p85) p85=lwage_imp 
				 (p85) p85_cens=cens 
				 (sum) emp_share 
				 (sum) n_persnr 
				 (sum) n_betr, by(year east educ age_group);
		
	#delimit cr
	// Wage gaps
	gen wg_50_15 = p50 - p15
	gen wg_85_50 = p85 - p50
	
	keep if educ!=.
	keep if east!=.
	keep if age_group!=.

	mkmat year east educ age_group wg_50_15 wg_85_50 p85_cens emp_share n_persnr n_betr, mat(m_wg_educ)
	/*
	Dieser Abschnitt analysiert die Lohnverteilung nach Ost/West Altersgruppen und Bildungsniveau  Deutschlands
	für ausgewählte Jahre (1995, 2004, 2009, 2021).

	Beschreibung der Struktur der Ausgaben:
	---------------------------------------
	1. year       = "Jahr der Beobachtung"
	2. east       = "Indikator für Ostdeutschland"
	3. educ       = "Bildungsabschluss in 3 Abstufung (niedrig/mittel/hoch)"
	4. age_group  = "Altersgruppe: 20–36, 37–47, 48–62"
	5. wg_50_15   = "Abstand zwischen Median und 15-Perzentil"
	6. wg_85_50   = "Abstand zwischen 85-Perzentil und Median"
	7. p85_cens   = "Indikator ob das 85-Perzentil auf imputierten Löhnen beruht"
	8. emp_share  = "Anteil der Beobachtungen an der jeweiligen Ost-Jahres-Zelle"
	9. n_persnr   = "Anzahl der Personen-Beobachtungen"
	10. n_betr     = "Anzahl der Betriebe"
	
	Alle Auswertungen beruhen auf vollständigen Jahres-Alters-Bildungsgruppen-Zellen, 
	die die Mindestanzahl an Beobachtungen erfüllen.
	*/
	matrix list m_wg_educ
restore
*/

//==============================================================================
// 	3. Kontrafaktische Verteilungen nach Imputationen von Leaver/Entrant Löhnen 
//==============================================================================


// 	3a. Imputationen für Labour Market Leaver
//------------------------------------------------------------------------------

use ${data}\siab_panel.dta, clear
sort persnr year
bys year east: egen total_obs=count(mwage!=.) 

//Fill the dataset
tsset persnr year 
tsfill, full
drop leaves

//Is there a Wage observation
gen has_wage = (mwage!=.)

//Indicator for being missing next year, but not in the current year
gen leaves=0
by persnr: replace leaves=1 if has_wage==1 & has_wage[_n+1]==0 
//movers who directly have a job in the other part of Germany are also leavers
by persnr: replace leaves=1 if east!=east[_n+1] & (has_wage==1 & has_wage[_n+1]==1) 
replace leaves = . if year==2021

//Indicator for having left last year
gen left_last_year = 0
by persnr: replace left_last_year = 1 if leaves[_n-1]==1 

//Fill in east information for people who left the labor market last year
by persnr:  replace east = east[_n-1] if  left_last_year == 1  & east==.

//Imputation Method Nr. 1 - Carry forward the old wage
gen cf_mwage = mwage
by persnr:  replace cf_mwage = mwage[_n-1] if  left_last_year == 1  

//Imputation Method Nr. 2 - Imputation on observables
//Predict mwage based on observables
gen mw_obs =.
gen mw_imp_obs = mwage
quietly{
	levelsof year, local(obs_years)
	foreach y of local obs_years {
		forvalues e=0/1{
			tempvar prediction
			reg mwage i.age_q i.educ i.educ#i.age_q if east==`e' & year == `y', robust
			predict `prediction' if east==`e'& year == `y'  
			replace mw_obs = `prediction' if east==`e' & year == `y' 
		}
	}
}
by persnr:  replace mw_imp_obs = mw_obs[_n-1] if  left_last_year == 1  

//Imputation Method Nr. 3 - Specific wages for people who left last year (0, p15, p50)
preserve 
	collapse (p15) p15_e = mwage  (p50) p50_e = mwage, by(east year) 
	drop if east==.
	reshape wide p15_e p50_e, i(year) j(east) 
	mkmat year p15_e0 p15_e1 p50_e0 p50_e1, matrix(m_pct)
restore 
gen mw_left_0   = mwage
gen mw_left_p15 = mwage
gen mw_left_p50 = mwage
replace mw_left_0 = 0  if  left_last_year == 1
replace mw_left_p15 = m_pct[year-1994,2+east]  if  left_last_year == 1
replace mw_left_p50 = m_pct[year-1994,4+east]  if  left_last_year == 1


keep if (year ==1996 | year==2004 | year==2017)
drop if east==.

#delimit ;
collapse
		(p15) p15_imp0 = mwage
		(p50) p50_imp0 = mwage
		(p85) p85_imp0 = mwage
		(p15) p15_imp1 = cf_mwage
		(p50) p50_imp1 = cf_mwage
		(p85) p85_imp1 = cf_mwage
		(p15) p15_imp2 = mw_imp_obs
		(p50) p50_imp2 = mw_imp_obs
		(p85) p85_imp2 = mw_imp_obs
		(p15) p15_imp3 = mw_left_p50
		(p50) p50_imp3 = mw_left_p50
		(p85) p85_imp3 = mw_left_p50
		(p15) p15_imp4 = mw_left_p15
		(p50) p50_imp4 = mw_left_p15
		(p85) p85_imp4 = mw_left_p15
		(sum) n_persnr 
		(sum) n_betr
		,by(east year) ;
#delimit cr
		
gen id = _n  // create an ID for reshaping
reshape long p15_imp p50_imp p85_imp, i(id) j(imputation)
rename (p15_imp p50_imp p85_imp) (p15 p50 p85)


/*
Dieser Abschnitt gibt kontrafaktische Lohnverteilungen aus unter der Annahme das Personen,
die den Arbeitsmarkt im Vorjahr verlassen haben(Labour Market Leaver) mit einem imputierten 
Lohn in die Lohnverteilung mit einbezogen werden. 

Beschreibung der Struktur der Ausgaben:
---------------------------------------
 1. year         = "Jahr der Beobachtung"
 2. east         = "Indikator für Ost-/Westdeutschland"
 3. imputation   = "Index der Imputationsmethode (0–4, siehe unten)"
 4. p15          = "15-Perzentil der kontrafaktischen Lohnverteilung"
 5. p50          = "Median der kontrafaktischen Lohnverteilung"
 6. p85          = "85-Perzentil der kontrafaktischen Lohnverteilung"
 7. n_persnr     = "Anzahl der Personen-Beobachtungen in der Ost/West-Jahreszelle"
 8. n_betr       = "Anzahl der Betriebe"

Verwendete Imputationsmethoden:
-------------------------------
 0 = Keine Imputation (nur beobachtete Löhne)
 1 = Carry-forward des letzten beobachteten Lohns für Leaever
 2 = Vorhersage des Lohnes anhand beobachtbarer Merkmale (Alter x Bildung x Jahr)
 3 = Imputation mit dem Median-Lohn der Vorjahresgruppe
 4 = Imputation mit dem 15 Perzentil der Vorjahresgruppe

Alle Auswertungen basieren auf vollständigen Jahres-Ost/West-Zellen (1996, 2004, 2017)
und erfüllen Mindestanforderungen an Beobachtungsumfang.
*/

mkmat year east imputation p15 p50 p85  n_persnr n_betr, mat(m_imputation_leavers)
matrix list m_imputation_leavers

// 	3b. Imputationen für Labour Market Entrants
//------------------------------------------------------------------------------

use ${data}\siab_panel.dta, clear
sort persnr year
bys year east: egen total_obs=count(mwage!=.) 

//Fill the dataset
tsset persnr year 
tsfill, full
drop leaves

//Is there a Wage observation
gen has_wage = (mwage!=.)

//Indicator for being missing last year, but not in the current year
gen entered=0
bys persnr: replace entered=1 if has_wage==1 & has_wage[_n-1]==0
replace entered = . if year==1995 

//Indicator for having left last year
gen enter_next_year = 0
by persnr: replace enter_next_year = 1 if entered[_n+1]==1 

//Fill in east information for people who left the labor market last year
by persnr:  replace east = east[_n+1] if  enter_next_year == 1  & east==.


//Imputation Method Nr. 1 - Carry backward the old wage
gen cf_mwage = mwage
by persnr:  replace cf_mwage = mwage[_n+1] if  enter_next_year == 1  

//Imputation Method Nr. 2 - Imputation on observables
//Predict mwage based on observables
gen mw_obs =.
gen mw_imp_obs = mwage
quietly{
	levelsof year, local(obs_years)
	foreach y of local obs_years {
		forvalues e=0/1{
			tempvar prediction
			reg mwage i.age_q i.educ i.educ#i.age_q if east==`e' & year == `y', robust
			predict `prediction' if east==`e'& year == `y'  
			replace mw_obs = `prediction' if east==`e' & year == `y' 
		}
	}
}
by persnr:  replace mw_imp_obs = mw_obs[_n+1] if  enter_next_year == 1  

//Imputation Method Nr. 3 - Specific wages for people who enter next year (0, p15, p50)
preserve 
	collapse (p15) p15_e = mwage  (p50) p50_e = mwage, by(east year) 
	drop if east==.
	reshape wide p15_e p50_e, i(year) j(east) 
	mkmat year p15_e0 p15_e1 p50_e0 p50_e1, matrix(m_pct)
restore 
gen mw_left_0   = mwage
gen mw_left_p15 = mwage
gen mw_left_p50 = mwage
replace mw_left_0 = 0  if  enter_next_year == 1
replace mw_left_p15 = m_pct[year-1994,2+east]  if  enter_next_year == 1
replace mw_left_p50 = m_pct[year-1994,4+east]  if  enter_next_year == 1


keep if (year ==1995 | year==2004 | year == 2009| year==2020)
drop if east==.

#delimit ;
collapse
		(p15) p15_imp0 = mwage
		(p50) p50_imp0 = mwage
		(p85) p85_imp0 = mwage
		(p15) p15_imp1 = cf_mwage
		(p50) p50_imp1 = cf_mwage
		(p85) p85_imp1 = cf_mwage
		(p15) p15_imp2 = mw_imp_obs
		(p50) p50_imp2 = mw_imp_obs
		(p85) p85_imp2 = mw_imp_obs
		(p15) p15_imp3 = mw_left_p50
		(p50) p50_imp3 = mw_left_p50
		(p85) p85_imp3 = mw_left_p50
		(p15) p15_imp4 = mw_left_p15
		(p50) p50_imp4 = mw_left_p15
		(p85) p85_imp4 = mw_left_p15
		(sum) n_persnr 
		(sum) n_betr
		,by(east year) ;
#delimit cr

gen id = _n  // create an ID for reshaping
reshape long p15_imp p50_imp p85_imp, i(id) j(imputation)
rename (p15_imp p50_imp p85_imp) (p15 p50 p85)
mkmat year east imputation p15 p50 p85  n_persnr n_betr, mat(m_imputation_entrants)

/*
Dieser Abschnitt gibt kontrafaktische Lohnverteilungen aus unter der Annahme, 
dass Personen, die im Folgejahr in den Arbeitsmarkt eintreten (Labour Market Entrants), 
mit einem imputierten Lohn bereits im aktuellen Jahr in die Lohnverteilung einbezogen werden.

Beschreibung der Struktur der Ausgaben:
---------------------------------------
 1. year         = "Jahr der Beobachtung"
 2. east         = "Indikator für Ost-/Westdeutschland"
 3. imputation   = "Index der Imputationsmethode (0–4, siehe unten)"
 4. p15          = "15-Perzentil der kontrafaktischen Lohnverteilung"
 5. p50          = "Median der kontrafaktischen Lohnverteilung"
 6. p85          = "85-Perzentil der kontrafaktischen Lohnverteilung"
 7. n_persnr     = "Anzahl der Personen-Beobachtungen in der Ost/West-Jahreszelle"
 8. n_betr       = "Anzahl der Betriebe"

Verwendete Imputationsmethoden:
-------------------------------
 0 = Keine Imputation (nur beobachtete Löhne)
 1 = Carry-backward des später beobachteten Lohns für Entrants
 2 = Vorhersage des Lohnes anhand beobachtbarer Merkmale (Alter x Bildung x Jahr)
 3 = Imputation mit dem Median-Lohn der Folgejahresgruppe
 4 = Imputation mit dem 15-Perzentil der Folgejahresgruppe

Alle Auswertungen basieren auf vollständigen Jahres-Ost/West-Zellen 
(1995, 2004, 2009, 2020) und erfüllen Mindestanforderungen an Beobachtungsumfang.
*/

matrix list m_imputation_entrants


// 	3c. Entrant and Leaver Characteristics
//---------------------------------------------------------------------------

//use ${data}\siab_panel.dta, clear

//Generate descriptives for everyone by percentile class first
//-------------------------------------------------------------

/*
//keep what is needed for the table
keep if year == 1995 | year == 2004 | year == 2017

 //Generate wage percentiles
sort east year
foreach p in "15" "50" "85"{
	bys year: egen mwage_p`p' = pctile(mwage), p(`p')
}

//How do stayers in specific pecentile categories look like
gen pct_class = .
replace pct_class = 1 if mwage > mwage_p15 & mwage <= mwage_p50
replace pct_class = 2 if mwage == mwage_p50

preserve
drop if pct_class>2
	#delimit ; 
	collapse (mean) mean_w  = mwage 
			(sd)  sd_mw =  mwage 
			(mean) mean_mresid  = mresid 
			(sd)  sd_mresid =  mresid 
			(mean) low_skilled = e1
			(mean) medium_skilled = e2
			(mean) high_skilled = e3
			(mean) mean_age = age
			(sd) sd_age = age
			(sum) n_pers
			(sum) n_betr
			, by(east year pct_class) ;	 
	#delimit cr
	drop if east ==.
	
	mkmat east year pct_class mean_w sd_mw mean_mresid sd_mresid n_pers n_betr, mat(m_all_wages)
	mkmat east year pct_class low_skilled medium_skilled high_skilled mean_age sd_age n_pers n_betr, mat(m_all_demo)

restore

matrix list m_all_wages
matrix list m_all_demo
*/

//Generate the leaver table entries
//-------------------------------------------------------------

use ${data}\siab_panel.dta, clear
drop if east==.
sort persnr year
keep persnr year east
reshape wide east, i(persnr) j(year)

//Compute for each year when the next year that someone moves takes place
quietly{
	forvalues a=1995/2021{
		gen nmove_after_`a' = .
		forvalues y=1996/2021{
			replace nmove_after_`a' = `y' if east`y'!=east`a' & east`y'!=. & nmove_after_`a'==. & east`a' !=. & `y'>`a'
		}
	}
}
reshape long east nmove_after_, i(persnr) j(year)

drop east 
merge 1:1 persnr year using ${data}\siab_panel.dta
drop _merge
cap drop leaves
cap drop has_wage

//Is there a Wage observation
gen has_wage = (mwage!=.)

//Indicator for being missing next year, but not in the current year
sort persnr year
gen leaves=0
by persnr: replace leaves=1 if has_wage==1 & has_wage[_n+1]==0
replace leaves = . if year==2021

//Indicator for being missing next year, but reappearing in another region in a later year
gen leaves_but_moves=0 
replace leaves_but_moves =1 if nmove_after_>=year & nmove_after_ !=. & leaves==1

//Variable for collapsing this thing
gen leav_cat = 0
replace leav_cat = 1 if leaves ==1 
replace leav_cat = 2 if leaves_but_moves ==1 

keep if year==1995 | year == 2004 | year==2009 | year == 2019
keep if east!=.
preserve
	#delimit ; 
	collapse (mean) mean_w  = mwage 
			(sd)   sd_mw =  mwage 
			(mean) mean_mresid  = mresid 
			(sd)  sd_mresid =  mresid 
			(mean) e1
			(mean) e2
			(mean) e3
			(mean) mean_age = age
			(sd) sd_age = age
			(sum) n_persnr
			(sum) n_betr
			, by(east year leav_cat) ;	
	#delimit cr
	mkmat east year leav_cat mean_w sd_mw mean_mresid sd_mresid n_persnr n_betr, mat(m_leave_wages)
	mkmat east year leav_cat e1 e2 e3 mean_age sd_age n_persnr n_betr, mat(m_leave_demo)
restore

/*
Beschreibung der Struktur der Ausgaben:
---------------------------------------

Matrix m_leave_wages:
---------------------
 1. east         = "Indikator für Ost-/Westdeutschland im Jahr der Beobachtung"
 2. year         = "Jahr der Beobachtung"
 3. leav_cat     = "Leaver-Kategorie:
                     0 = Stayer (bleibt im Panel),
                     1 = Leaver (verlässt den Arbeitsmarkt vollständig),
                     2 = Leaver mit späterer Rückkehr in anderer Region"
 4. mean_w       = "Durchschnittlicher Bruttolohn (mwage)"
 5. sd_mw        = "Standardabweichung des Bruttolohns"
 6. mean_mresid  = "Durchschnittlicher Residual-Lohn (mresid)"
 7. sd_mresid    = "Standardabweichung des Residual-Lohns"
 8. n_persnr     = "Anzahl der Personen in der jeweiligen Gruppe"
 9. n_betr       = "Anzahl der Betriebe"

Matrix m_leave_demo:
---------------------
 1. east         = "Indikator für Ost-/Westdeutschland im Jahr der Beobachtung"
 2. year         = "Jahr der Beobachtung"
 3. leav_cat     = "Leaver-Kategorie (siehe oben)"
 4. e1           = "Anteil geringqualifizierter Personen"
 5. e2           = "Anteil mittelqualifizierter Personen"
 6. e3           = "Anteil hochqualifizierter Personen"
 7. mean_age     = "Durchschnittsalter"
 8. sd_age       = "Standardabweichung des Alters"
 9. n_persnr     = "Anzahl der Personen in der jeweiligen Gruppe"
10. n_betr       = "Anzahl der Betriebe"
*/

matrix list m_leave_wages
matrix list m_leave_demo

//Generate the entrant table entries
//-------------------------------------------------------------

use ${data}\siab_panel.dta, clear
drop if east==.

//Fill the dataset
tsset persnr year 
tsfill, full

//Is there a Wage observation
gen has_wage = (mwage!=.)

//Indicator for being missing last year, but not in the current year
gen entered=0
bys persnr: replace entered=1 if has_wage==1 & has_wage[_n-1]==0
replace entered = . if year==1995

//First obsertion in the data is this year
gen first_year_obs = (year==first_year_in_data)
tab entered first_year_obs

//Generate an entry indicator for people, who are not in their first year in the data
gen first_entrant = entered * first_year_obs
gen re_entrant = entered *  !first_year_obs
tab entered first_entrant

//keep what is needed for the table
keep if year == 2004 | year==2009| year == 2021

//only look at the east
keep if east == 1


//Variable for collapsing this thing
gen ent_cat = 0
replace ent_cat = 1 if entered ==1 
replace ent_cat = 2 if first_entrant ==1 

preserve
	#delimit ; 
	collapse (mean) mean_w  = mwage 
			(sd)  sd_mw =  mwage 
			(mean) mean_mresid  = mresid 
			(sd)  sd_mresid =  mresid 
			(mean) e1
			(mean) e2
			(mean) e3
			(mean) mean_age = age
			(sd) sd_age = age
			(sum) n_persnr
			(sum) n_betr
			, by(east year ent_cat) ;	 
	#delimit cr
	drop if east ==.
	
	mkmat east year ent_cat mean_w sd_mw mean_mresid sd_mresid n_persnr n_betr, mat(m_enter_wages)
	mkmat east year ent_cat e1 e2 e3 mean_age sd_age n_persnr n_betr, mat(m_enter_demo)
restore

/*Beschreibung der Struktur der Ausgaben:
---------------------------------------

Matrix m_enter_wages:
---------------------
 1. east         = "Indikator für Ostdeutschland (immer 1 in dieser Auswertung)"
 2. year         = "Jahr der Beobachtung (2004, 2009, 2021)"
 3. ent_cat      = "Entrant-Kategorie:
                     0 = Nicht-Entrants (bereits vorher im Panel),
                     1 = Entrants insgesamt (inkl. Erst- und Wiedereintritt),
                     2 = Erst-Eintritte (Personen, deren erste Beobachtung im aktuellen Jahr erfolgt)"
 4. mean_w       = "Durchschnittlicher Bruttolohn (mwage)"
 5. sd_mw        = "Standardabweichung des Bruttolohns"
 6. mean_mresid  = "Durchschnittlicher Residual-Lohn (mresid)"
 7. sd_mresid    = "Standardabweichung des Residual-Lohns"
 8. n_persnr     = "Anzahl der Personen in der jeweiligen Gruppe"
 9. n_betr       = "Anzahl der Betriebe"

Matrix m_enter_demo:
---------------------
 1. east         = "Indikator für Ostdeutschland (immer 1)"
 2. year         = "Jahr der Beobachtung"
 3. ent_cat      = "Entrant-Kategorie (siehe oben)"
 4. e1           = "Anteil geringqualifizierter Personen"
 5. e2           = "Anteil mittelqualifizierter Personen"
 6. e3           = "Anteil hochqualifizierter Personen"
 7. mean_age     = "Durchschnittsalter"
 8. sd_age       = "Standardabweichung des Alters"
 9. n_persnr     = "Anzahl der Personen in der jeweiligen Gruppe"
10. n_betr       = "Anzahl der Betriebe"
*/

matrix list m_enter_wages
matrix list m_enter_demo

//==============================================================================
//	4. Standard-Abweichungen von Löhnen und Residual-Löhnen nach Ost/West x Jahr
//==============================================================================
use ${data}\siab_panel.dta, clear
drop if east==.

//Compute standard deviations of wages
resid_sd i.educ i.age_q, tag(educ)
resid_sd i.educ i.age_q i.ind2d, tag(ind2d)
resid_sd i.educ i.age_q i.ind2d i.occ3, tag(occ3)
resid_sd i.educ i.age_q i.ind2d i.occ3 contemp_ffe, tag(firm_fe)

/*Hier werden fünf verschiedene Tabellen mit Standardabweichungen der Log-Löhne 
und Standard-Abweichungen von Log-Lohnresidualen für Ost/West X Jahres-zellen 
für unterschiedliche Mincer-Gleichungen ausgegeben. Jede Tabelle entspricht 
einer der Spezifikationen des Programms resid_sd weiter oben. 
Es werden immer alle Beobachtungen in einer Ost/West X Jahres-zelle genutzt 
und die Beobachtungszahlen für Personen und Betriebe sind in den letzten 
beiden Spalten angegeben. Da es nur Ost/West Jahres-Splits sind, sind die
Beibachtungskriterien für die Ausgabe immer erfüllt.
 */

matrix list m_sd_educ
matrix list m_sd_ind2d
matrix list m_sd_occ3
matrix list m_sd_firm_fe

//==============================================================================
//	5. RIF- und DFL-Decompositions
//==============================================================================

//5a. DFL-Dekomposition
//------------------------------------------------------------------------------
rename demo_detail demo
dfl_pct  demo occ3 svbroad

/*
In diesem Abschnitt werden mit dem DFL-Verfahren (DiNardo-Fortin-Lemieux) 
gewichtete kontrafaktische Lohn-Verteilungen berechnet, bei denen die Verteilung 
verschiedener Variablen wie in 1995 gewichtet wird. Ausgaben aus ungewichteten
Verteilungen sind in Abschnitt 1.

Die Analyse fokussiert sich auf die Dekomposition der Lohnverteilung entlang 
verschiedener Kovariaten:

 - demo     = "Demographische Merkmale wie Alter und Bildung bei 1995er Werten"
 - occ3     = "Beruf in 3-stelliger Klassifikation bei 1995er-Werten"
 - svbroad  = "Sektorale Zuordnung bei 1995er-Werten"

Für jede dieser Variablen werden die kontrafaktischen Verteilungen per Jahr 
und Region (Ost/West) mit dem `dfl_yearly`-Befehl berechnet. Aus diesen 
Verteilungen werden die Perzentile p15, p50 und p85 extrahiert. 

Die resultierenden Matrizen (m_p_dfl_*) enthalten:
 1. east         = "Indikator für Ost-/Westdeutschland"
 2. year         = "Jahr der Beobachtung"
 3. p15          = "15-Perzentil der kontrafaktischen Verteilung"
 4. p50          = "Median der kontrafaktischen Verteilung"
 5. p85          = "85-Perzentil der kontrafaktischen Verteilung"
 6. n_persnr     = "Anzahl Personen in der Zelle"
 7. n_betr       = "Anzahl Betriebe in der Zelle"

*/
matrix list m_p_dfl_demo
matrix list m_p_dfl_occ3
matrix list m_p_dfl_svbroad


//5b. RIF-Dekomposition
//------------------------------------------------------------------------------

/*
In diesem Abschnitt werden RIF-Decompositionen durchgeführt, bei denen die 
Einflussfunktionen ("Recentered Influence Functions") der Perzentile (p15, p50, p85)
und der Log-Lohnlücken (85-50, 50-15) auf Gruppenebene (z.B. Berufsgruppen, Alters-Bildung,
Sektoren, Firmen-FE-Quartile) berechnet werden
Eine Beschreibung des Vorgehens und der Tabellen ist weiter unten über den Tabellenausgaben enthalten.

Die RIF-Statistiken werden erzeugt durch:
 - `rifvar()` aus dem **rif**-Package für Stata
 - Gewichtungen aus `dfl_yearly`, basierend auf einem Basisjahr (1995)
 
Für jede der benannten Variablen (z.B. `age_educ`, `svbroad`, `q_firm`) werden:
 - Mittelwerte der Einflussfunktionen für p15, p50, p85 berechnet
 - RIFs der Log-Lohnlücken (85-50, 50-15) berechnet
 - Gruppenanteile und Beobachtungszahlen ausgegeben
 
HINWEIS: Bei den RIFs handelt es sich um umsklarierte Lohn-Beobachtungen selbst,
von dennen Mittelwerte ausgegeben werden, nicht um Perzentile. Für eine RIF-Skalierung
einer Lohnvariable gibt jede einzelne Variable den Beitrag zu einem Perzentil an. 

Die Ergebnisse werden in zwei Matrizen pro Variable gespeichert:
 - `r_<var>_dfl`: Perzentile (RIF15, RIF50, RIF85)
 - `s_<var>_dfl`: Lohnlücken (85-50, 50-15) und Gruppenanteile
 
r-Tabellen: 
 1. east        = "Indikator für Ost-/Westdeutschland"
 2. year        = "Jahr der Beobachtung"
 3. <var>       = "Gruppenvariable (z.B. q_firm)"
 4. ffl         = Gewichtung (0 = ungewichtet, 1 = DFL)
 5. rif_15      = "Mittelwert der Einflussfunktion für das 15. Perzentil"
 6. rif_50      = "Mittelwert der Einflussfunktion für das Median-Perzentil"
 7. rif_85      = "Mittelwert der Einflussfunktion für das 85. Perzentil"
 8. n_persnr     = "Anzahl Personen in der Zelle"
 9. n_betr       = "Anzahl Betriebe in der Zelle"
 
s-Tabellen:  
 1. east        = "Indikator für Ost-/Westdeutschland"
 2. year        = "Jahr der Beobachtung"
 3. <var>       = "Gruppenvariable (z.B. q_firm)"
 4. ffl         = Gewichtung (0 = ungewichtet, 1 = DFL)
 5. w_85_50     = Mittelwert der Einflussfunktion der oberen Lohnlücke
 6. w_50_15     = Mittelwert der Einflussfunktion der unteren Lohnlücke
 7. share       = Anteil der Gruppe innerhalb Ost/West × Jahr
 8. n_persnr     = "Anzahl Personen in der Zelle"
 9. n_betr       = "Anzahl Betriebe in der Zelle"
*/

otf_rifs age_educ q_firm

preserve
	//mining will cause problems here
	drop if svbroad == 1
	otf_rifs svbroad
restore

output_mtx age_educ svbroad q_firm


matrix dir 

cap log close
