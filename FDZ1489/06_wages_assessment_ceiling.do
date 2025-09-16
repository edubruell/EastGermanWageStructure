cap log close
log using ${log}\06_wages_assessment_ceiling.log, replace


/****************************************************************************************************************************************
|    Authors:    Kevin Ruf, Alexandra Schmucker, Stefanie Wolter (modifications by Eduard Brüll) 										|
|																																		|
|    Date:            Jan 2022                                      																	|
|																																		|
|    Purpose:         Generation of variables:																							|
|						1. Contribution Assessment Ceiling																				|
|																																		|
|	 Notes:			  This Stata do-file is based on the do-files of Dauth & Eppelsheimer and updated for the years 2018-2023  			|
|					  Please copy the parts of the do-file that you need into your own do-files. 										|
|					  																													|
|	 References:	  https://de.wikipedia.org/wiki/Beitragsbemessungsgrenze															|
|					  https://de.wikipedia.org/wiki/Geringf%C3%BCgige_Besch%C3%A4ftigung												|
|					  https://www.destatis.de/DE/Themen/Wirtschaft/Preise/Verbraucherpreisindex/_inhalt.html							|
|																																		|
|                                                                       																|
|    List of changes                                                    																|
|    --------------------                                               																|
|    -                v2: added years 2021-2023 (Philipp vom Berge; Mar 2023)                                                  			|
|    -				  own_modifications: data spread out across the original do-files;                                                  |    
|										 added new state indicator calculated from ao_kreis                                             |
\***************************************************************************************************************************************/

********************************************************************************
* Generate east dummy (1 if WORKPLACE is in the eastern part of Germany; 0 if it is in the western part)
********************************************************************************

//generate a state indicator
gen state = floor(ao_kreis/1000)
//rename wo_bula state
lab def bula_lab  1 "Schleswig-Holstein" 2 "Hamburg" 3 "Niedersachsen" 4 "Bremen" 5 "Nordrhein-Westfalen" 6 "Hessen" 7 "Rheinland-Pfalz" 8 "Baden-Wuerttemberg" 9 "Bayern" 10 "Saarland" 11 "Berlin" 12 "Brandenburg" 13 "Mecklenburg-Vorpommern" 14 "Sachsen" 15 "Sachsen-Anhalt" 16 "Thueringen"
lab val state bula_lab
//Generate an east-germany dummy variable
gen east = 0 if state>=1 & state<=10
replace east = 1 if state>=11 & state<=16
lab var east "east"
lab def east_lab 0 "west" 1 "east"
lab val east east_lab

tab state east

********************************************************************************
* 1. Contribution Assessment Ceiling
********************************************************************************
* Note: For the flagging of right-censored wages, it is recommended to assume all daily wages that amount to at least 98% of the contribution assessment ceiling as censored.

* The following variables are required: 
* jahr: year of earning
* east: dummy for East Germany (see Dauth and Eppelsheimer 2020 for the generation of 'east' using the SIAB)

* Limits for the years 1975 - 2001 are converted from DM to EUR
global exch = 1/1.95583		// exchange rate (EUR/DM)

* Set exact assessment ceiling
cap drop limit_assess
gen limit_assess = .
label variable limit_assess "Contribution assessment ceiling"
* West 1975 - 1991:
replace limit_assess =  92.05*$exch if jahr == 1975
replace limit_assess = 101.64*$exch if jahr == 1976
replace limit_assess = 111.78*$exch if jahr == 1977
replace limit_assess = 121.64*$exch if jahr == 1978
replace limit_assess = 131.51*$exch if jahr == 1979
replace limit_assess = 137.70*$exch if jahr == 1980
replace limit_assess = 144.66*$exch if jahr == 1981
replace limit_assess = 154.52*$exch if jahr == 1982
replace limit_assess = 164.38*$exch if jahr == 1983
replace limit_assess = 170.49*$exch if jahr == 1984
replace limit_assess = 177.53*$exch if jahr == 1985
replace limit_assess = 184.11*$exch if jahr == 1986
replace limit_assess = 187.40*$exch if jahr == 1987
replace limit_assess = 196.72*$exch if jahr == 1988
replace limit_assess = 200.55*$exch if jahr == 1989
replace limit_assess = 207.12*$exch if jahr == 1990
replace limit_assess = 213.70*$exch if jahr == 1991

* West 1992 - 2001:
replace limit_assess = 222.95*$exch if east == 0 & jahr == 1992
replace limit_assess = 236.71*$exch if east == 0 & jahr == 1993
replace limit_assess = 249.86*$exch if east == 0 & jahr == 1994
replace limit_assess = 256.44*$exch if east == 0 & jahr == 1995
replace limit_assess = 262.30*$exch if east == 0 & jahr == 1996
replace limit_assess = 269.59*$exch if east == 0 & jahr == 1997
replace limit_assess = 276.16*$exch if east == 0 & jahr == 1998
replace limit_assess = 279.45*$exch if east == 0 & jahr == 1999
replace limit_assess = 281.97*$exch if east == 0 & jahr == 2000
replace limit_assess = 286.03*$exch if east == 0 & jahr == 2001

* West 2002 - 2023:
replace limit_assess = 147.95 if east == 0 & jahr == 2002
replace limit_assess = 167.67 if east == 0 & jahr == 2003
replace limit_assess = 168.85 if east == 0 & jahr == 2004
replace limit_assess = 170.96 if east == 0 & jahr == 2005
replace limit_assess = 172.60 if east == 0 & jahr == 2006
replace limit_assess = 172.60 if east == 0 & jahr == 2007
replace limit_assess = 173.77 if east == 0 & jahr == 2008
replace limit_assess = 177.53 if east == 0 & jahr == 2009
replace limit_assess = 180.82 if east == 0 & jahr == 2010
replace limit_assess = 180.82 if east == 0 & jahr == 2011
replace limit_assess = 183.61 if east == 0 & jahr == 2012
replace limit_assess = 190.68 if east == 0 & jahr == 2013
replace limit_assess = 195.62 if east == 0 & jahr == 2014
replace limit_assess = 198.90 if east == 0 & jahr == 2015
replace limit_assess = 203.28 if east == 0 & jahr == 2016
replace limit_assess = 208.77 if east == 0 & jahr == 2017
replace limit_assess = 213.70 if east == 0 & jahr == 2018
replace limit_assess = 220.27 if east == 0 & jahr == 2019
replace limit_assess = 226.23 if east == 0 & jahr == 2020
replace limit_assess = 233.42 if east == 0 & jahr == 2021
replace limit_assess = 231.78 if east == 0 & jahr == 2022
replace limit_assess = 240.00 if east == 0 & jahr == 2023

* East 1992 - 2001:
replace limit_assess = 157.38*$exch if east == 1 & jahr == 1992
replace limit_assess = 174.25*$exch if east == 1 & jahr == 1993
replace limit_assess = 193.97*$exch if east == 1 & jahr == 1994
replace limit_assess = 210.41*$exch if east == 1 & jahr == 1995
replace limit_assess = 222.95*$exch if east == 1 & jahr == 1996
replace limit_assess = 233.42*$exch if east == 1 & jahr == 1997
replace limit_assess = 230.14*$exch if east == 1 & jahr == 1998
replace limit_assess = 236.71*$exch if east == 1 & jahr == 1999
replace limit_assess = 232.79*$exch if east == 1 & jahr == 2000
replace limit_assess = 240.00*$exch if east == 1 & jahr == 2001

* East 2002 - 2023:
replace limit_assess = 123.29 if east == 1 & jahr == 2002
replace limit_assess = 139.73 if east == 1 & jahr == 2003
replace limit_assess = 142.62 if east == 1 & jahr == 2004
replace limit_assess = 144.66 if east == 1 & jahr == 2005
replace limit_assess = 144.66 if east == 1 & jahr == 2006
replace limit_assess = 149.59 if east == 1 & jahr == 2007
replace limit_assess = 147.54 if east == 1 & jahr == 2008
replace limit_assess = 149.59 if east == 1 & jahr == 2009
replace limit_assess = 152.88 if east == 1 & jahr == 2010
replace limit_assess = 157.80 if east == 1 & jahr == 2011
replace limit_assess = 157.38 if east == 1 & jahr == 2012
replace limit_assess = 161.10 if east == 1 & jahr == 2013
replace limit_assess = 164.38 if east == 1 & jahr == 2014
replace limit_assess = 170.96 if east == 1 & jahr == 2015
replace limit_assess = 177.05 if east == 1 & jahr == 2016
replace limit_assess = 187.40 if east == 1 & jahr == 2017
replace limit_assess = 190.68 if east == 1 & jahr == 2018
replace limit_assess = 202.19 if east == 1 & jahr == 2019
replace limit_assess = 211.48 if east == 1 & jahr == 2020
replace limit_assess = 220.27 if east == 1 & jahr == 2021
replace limit_assess = 221.92 if east == 1 & jahr == 2022
replace limit_assess = 233.42 if east == 1 & jahr == 2023






log close
