cap log close
log using ${log}\04_occ_blossfeld.log, replace


/*
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	SIAB Preparation
	
	Generate Blossfeld-Occupations
	
	Generates the variable:
		- occ_blo: Blossfeld Occupations
	
	Author(s): Wolfgang Dauth, Johann Eppelsheimer
	
	Version: 1.0
	Created: 2020-03-23
	
	Based on:
		Schimpl-Neimanns, B. (2003)
		Mikrodaten-Tools: Umsetzung der Berufsklassifikation von Blossfeld auf die Mikrozensen 1973-1998
		ZUMA, Mannheim, ZUMA-Methodenbericht 2003/10
	
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		
*/



********************************************************************************
* Recode occupations
********************************************************************************

* copy occupations (KldB88)
gen occ_blo = beruf

* recode occupational cods to match Blossfeld's occupations (see Schimpl-Neimanns, 2003)
recode occ_blo 										///
        (11/22 41/51 53/62               = 1)		///
													///
        (071/133 135/141 143 151/162				///
        164 176/193 203/213 222/244					///
        252 263 301 313 321/323 332/				///
        346 352/371 373 375/377 402/				///
        403 412 423/433 442 452/463					///
        465/472 482 486 504 512/531					///
        543/549                          = 2)		///
													///
        (134 142 144 163 171/175 					///
		201/202 221 251 261/262 270/291				///
        302 305/312 314/315 331 351					///
        372 374 378/401 411 421/422					///
        441 451 464 481 483/485 					///
		491/503 511 541/542              = 3)		///
													///
        (303 304 621/635 721/722 733				///
        857                              = 4)		///
													///
        (032 052 601/612 726 883         = 5)		///
													///
        (685/686 688 706 713/716 					///
		723/725 741/744 791/794 805 				///
		838 911/913 923/937              = 6)		///
													///
        (684 704/705 711/712 801/804				///
        812 814 831 832/836 837 					///
		851/852 854/856 892/902 921/922  = 7)		///
													///
        (821/823 853 861/864 873/877     = 8)		///
													///
        (811 813 841/844 871/872 					///
		881/882 891                      = 9)		///
													///
        (682 687 731/732 734 782/784				///
        773                              = 10)		///
													///
        (031 681 683 691/703 771/772				///
        774/781                          = 11)		///
													///
        (751/763                         = 12)		///
													///
		(else = 99)


********************************************************************************
* Labels
********************************************************************************
label define lblBlo  1 "AGR-Agrarberufe" ///
					 2 "EMB-Einfache manuelle Berufe" ///
					 3 "QMB-Qual. manuelle Berufe" ///
					 4 "TEC-Techniker" ///
					 5 "ING-Ingenieure" ///
					 6 "EDI-Einfache Dienste" ///
					 7 "QDI-Qual. Dienste" ///
					 8 "SEMI-Semiprofessionen" ///
					 9 "PROF-Professionen" ///
					10 "EVB-Einfache kaufm. u. Verwaltungsberufe" ///
					11 "QVB-Qual. kaufm. u. Verwaltungsberufe" ///
					12 "MAN-Manager" ///
					99 "NO-Nicht zuordenbar"

label values occ_blo lblBlo
label variable occ_blo "Blossfeld occupations"


********************************************************************************
* Overview
********************************************************************************
tab beruf occ_blo
tab occ_blo




log close
