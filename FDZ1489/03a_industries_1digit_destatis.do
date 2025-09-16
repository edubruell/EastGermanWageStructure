cap log close
log using ${log}\03a_industries_1digits_destatis.log, replace


/*
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	SIAB Preparation
	
	Generate 1-digit industry variable based on 3-digit industries (w93_3_gen)
	Mapping based on Statistisches Bundesamt (2002)
	w93_3_gen is already coded time-consistent (see Eberle et al. 2014)
	
	Generates the variable:
		- industry1_destatis: Industry; 1-digit; Statistisches Bundesamt; based on w93_3_gen
	
	Author(s): Wolfgang Dauth, Johann Eppelsheimer
	
	Version: 1.0
	Created: 2020-03-23
	
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	
	References:
		
	Eberle et al., 2014:
		Eberle, J., P. Jacobebbinghaus, J. Ludsteck, and J. Witter (2014).
		Generation of timeconsistent industry codes in the face of classification changes
		Institute of Employment Research, Nuremberg. FDZ-Methodenreport 05/2011
		
	Statistisches Bundesamt, 2002:
		Klassifikation der Wirtschaftszweige, Ausgabe 1993
		https://www.destatis.de/DE/Methoden/Klassifikationen/GueterWirtschaftklassifikationen/Content75/KlassifikationWZ93.html
	
*/


********************************************************************************
* Map 3-digit industries to 1-digit industries
********************************************************************************
gen industry1_destatis=.

replace industry1_destatis =  1 if w93_3_gen >= 011 & w93_3_gen <= 020
replace industry1_destatis =  2 if w93_3_gen >= 050 & w93_3_gen <= 050
replace industry1_destatis =  3 if w93_3_gen >= 101 & w93_3_gen <= 145
replace industry1_destatis =  4 if w93_3_gen >= 151 & w93_3_gen <= 372
replace industry1_destatis =  5 if w93_3_gen >= 401 & w93_3_gen <= 410
replace industry1_destatis =  6 if w93_3_gen >= 451 & w93_3_gen <= 455
replace industry1_destatis =  7 if w93_3_gen >= 501 & w93_3_gen <= 527
replace industry1_destatis =  8 if w93_3_gen >= 551 & w93_3_gen <= 555
replace industry1_destatis =  9 if w93_3_gen >= 601 & w93_3_gen <= 642
replace industry1_destatis = 10 if w93_3_gen >= 651 & w93_3_gen <= 672
replace industry1_destatis = 11 if w93_3_gen >= 701 & w93_3_gen <= 748
replace industry1_destatis = 12 if w93_3_gen >= 751 & w93_3_gen <= 753
replace industry1_destatis = 13 if w93_3_gen >= 801 & w93_3_gen <= 804
replace industry1_destatis = 14 if w93_3_gen >= 851 & w93_3_gen <= 853
replace industry1_destatis = 15 if w93_3_gen >= 900 & w93_3_gen <= 930
replace industry1_destatis = 16 if w93_3_gen >= 950 & w93_3_gen <= 990



********************************************************************************
* Labels
********************************************************************************
label define lblIndDestatis ///
	 1 "1 Land- u. Forstwirtschaft" ///
	 2 "2 Fischerei u. Fischzucht" ///
	 3 "3 Bergbau u. Gewinnung v. Steinen u. Erden" /// 
	 4 "4 Verarbeitendes Gewerbe" ///
	 5 "5 Energie- u. Wasserversorgung" ///
	 6 "6 Baugewerbe" /// 
	 7 "7 Handel; Instandhaltung u. Reparatur v. KFZ u. Geb.guetern" ///
	 8 "8 Gastgewerbe" ///
	 9 "9 Verkehr und Nachrichtenuebermittlung" ///
	10 "10 Kredit- u. Vers.gewerbe" ///
	11 "11 Grunds.- u. Wohn.wesen, Vermietung bewgl. Sachen, Dienstl. f. Untern." ///
	12 "12 Oeff. Verw., Verteid., Soz.vers." ///
	13 "13 Erziehung u. Unterricht" ///
	14 "14 Gesundsh.-, Vet.- u. Soz.wesen" ///
	15 "15 Sonstige Dienstl." ///
	16 "16 Private Haushalte"

label values industry1_destatis lblIndDestatis
label variable industry1_destatis "Industry; 1-digit; Statistisches Bundesamt; based on w93_3_gen"



********************************************************************************
* Overview of mapping
********************************************************************************
tab w93_3_gen industry1_destatis




log close
