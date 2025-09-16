cap log close
log using ${log}\03b_industries_1digit_iab.log, replace


/*
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	SIAB Preparation
	
	Generate 1-digit industry variable based on 3-digit industries (w93_3_gen)
	Mapping based IAB establishment panel
	w93_3_gen is already coded time-consistent (see Eberle et al. 2014)
	
	Generates the variable:
		- industry1_estpanel: Industry; 1-digit; IAB establishment panel; based on w93_3_gen
	
	Author(s): Wolfgang Dauth, Johann Eppelsheimer
	
	Version: 1.0
	Created: 2020-03-23
	
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	
	Reference:
		
	Eberle et al., 2014:
		Eberle, J., P. Jacobebbinghaus, J. Ludsteck, and J. Witter (2014).
		Generation of timeconsistent industry codes in the face of classification changes
		Institute of Employment Research, Nuremberg. FDZ-Methodenreport 05/2011
	
*/


********************************************************************************
* Map 3-digit industries to 1-digit industries
********************************************************************************
gen industry1_estpanel=.


*** Group 1 ***
* Agriculture, hunting and forestry, fishing
* Land- und Forstwirtschaft, Fischerei
replace industry1_estpanel=1 if w93_3_gen>=11 & w93_3_gen<=50
* Mining and quarrying, electricity, gas and water supply, recycling
* Bergbau und Gewinnung von Steinen und Erden, Energie und Wasserversorgung; Abwasser- und Abfallentsorgung
replace industry1_estpanel=1 if (w93_3_gen>=101 & w93_3_gen<=145) | (w93_3_gen>=371 & w93_3_gen<=410) | w93_3_gen==900

*** Group 2 ***
* Manufacture of food products beverages and tobacco
* Nahrungs- und Genussmittel
replace industry1_estpanel=2 if w93_3_gen>=151 & w93_3_gen<=160

*** Group 3 ***
* Manufacture of consumer products (not including manufacture of wood products)
* Verbrauchsgueter
replace industry1_estpanel=3 if (w93_3_gen>=171 & w93_3_gen<=193) | (w93_3_gen>=221 & w93_3_gen<=223) | (w93_3_gen>=361 & w93_3_gen<=366)

*** Group 4 ***
* Manufacture of industrial goods 
* Produktionsgueter
replace industry1_estpanel=4 if (w93_3_gen>=201 & w93_3_gen<=212) | (w93_3_gen>=231 & w93_3_gen<=287)

*** Group 5 ***
* Manufacture of capital and consumer goods
* Investitions- und Gebrauchsgueter
replace industry1_estpanel=5 if w93_3_gen>=291 & w93_3_gen<=355

*** Group 6 ***
* Construction
* Baugewerbe
replace industry1_estpanel=6 if w93_3_gen>=451 & w93_3_gen<=455

*** Group 7 ***
* Hotels and restaurants
* Gastgewerbe
replace industry1_estpanel=7 if w93_3_gen>=551 & w93_3_gen<=555
* Other services
* Sonstige Dienstleistungen - personennah	
replace industry1_estpanel=7 if w93_3_gen>=921 & w93_3_gen<=930
* Trade, maintenance and repair of motor vehicles, motor
* Handel, Instandhaltung und Reparatur von Kraftfahrzeugen
replace industry1_estpanel=7 if w93_3_gen>=501 & w93_3_gen<=527

*** Group 8 ***
* Transport, storage
* Verkehr und Lagerei
replace industry1_estpanel=8 if w93_3_gen>=601 & w93_3_gen<=634
* Information and communication
* Information und Kommunikation
replace industry1_estpanel=8 if w93_3_gen>=641 & w93_3_gen<=642
* Financial intermediation, insurance
* Finanz- und Versicherungsdienstleistungen
replace industry1_estpanel=8 if w93_3_gen>=651 & w93_3_gen<=672
* Real estate
* Grundstücke
replace industry1_estpanel=8 if w93_3_gen>=701 & w93_3_gen<=703
* Renting
* Vermietung
replace industry1_estpanel=8 if w93_3_gen>=711 & w93_3_gen<=714
* IT
replace industry1_estpanel=8 if w93_3_gen>=721 & w93_3_gen<=726
* Liberal professions
* wirtschaftliche, wissenschaftliche und freiberufliche Dienstleistungen
replace industry1_estpanel=8 if w93_3_gen>=731 & w93_3_gen<=744
* Other services
* Sonstige Dienstleistungen - unternehmensnah
replace industry1_estpanel=8 if w93_3_gen>=745 & w93_3_gen<=748

*** Group 9 ***
* Education
* Erziehung und Unterricht
replace industry1_estpanel=9 if w93_3_gen>=801 & w93_3_gen<=804
* Health and social work
* Gesundheits- und Sozialwesen
replace industry1_estpanel=9 if w93_3_gen>=851 & w93_3_gen<=853
* Non-industrial organizations
* Interessenvertretungen
replace industry1_estpanel=9 if w93_3_gen>=911 & w93_3_gen<=913
* Public administration
* Öffentliche Verwaltung, Verteidigung, Sozialversicherung
replace industry1_estpanel=9 if w93_3_gen>=751 & w93_3_gen<=753



********************************************************************************
* Label
********************************************************************************
label define lblIndEstPanel ///
	 1 "1 Land- und Forstwirtschaft, Fischerei" ///
	 2 "2 Nahrungs- und Genussmittel" ///
	 3 "3 Verbrauchsgueter" /// 
	 4 "4 Produktionsgueter" ///
	 5 "5 Investitions- und Gebrauchsgueter" ///
	 6 "6 Baugewerbe" /// 
	 7 "7 Gastgewerbe" ///
	 8 "8 Verkehr und Lagerei" ///
	 9 "9 Erziehung und Unterricht" ///

label values industry1_estpanel lblIndEstPanel

label variable industry1_estpanel "Industry; 1-digit; IAB establishment panel; based on w93_3_gen"



********************************************************************************
* Overview of mapping
********************************************************************************
tab w93_3_gen industry1_estpanel



log close
