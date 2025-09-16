cap log close
log using ${log}\07_wages_marginal.log, replace

/*
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	SIAB Preparation
	
	Add Marginal Part-Time Income Threshold and flag affected records (1975 - 2014)
	
	Generates the variables:
		- limit_marginal: Marginal part-time income threshold
		- marginal: 1 if marginal wage, 0 otherwise
	
	Note: Limits for the years 1975 - 2001 are converted from DM to EUR
	Based on FDZ Arbeitshilfe (http://doku.iab.de/fdz/Bemessungsgrenzen_de_en.xls)
	
	
	Author(s): Wolfgang Dauth, Johann Eppelsheimer
	
	Version: 1.0
	Created: 2020-03-23
	
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		
*/

********************************************************************************
* Marginal Part-Time Income Threshold   
********************************************************************************
* The following variables are required: 
* jahr: year of earning
* east: dummy for East Germany (see Dauth and Eppelsheimer 2020 for the generation of 'east' using the SIAB)

* Limits for the years 1975 - 2001 are converted from DM to EUR
global exch = 1/1.95583		// exchange rate (EUR/DM)

* Set limit for marginal wages
cap drop limit_marginal
gen limit_marginal = .

* West 1975 - 1991:
replace limit_marginal = 11.51*$exch if jahr == 1975
replace limit_marginal = 12.70*$exch if jahr == 1976
replace limit_marginal = 13.97*$exch if jahr == 1977
replace limit_marginal = 12.82*$exch if jahr == 1978
replace limit_marginal = 12.82*$exch if jahr == 1979
replace limit_marginal = 12.79*$exch if jahr == 1980
replace limit_marginal = 12.82*$exch if jahr == 1981
replace limit_marginal = 12.82*$exch if jahr == 1982
replace limit_marginal = 12.82*$exch if jahr == 1983
replace limit_marginal = 12.79*$exch if jahr == 1984
replace limit_marginal = 13.15*$exch if jahr == 1985
replace limit_marginal = 13.48*$exch if jahr == 1986
replace limit_marginal = 14.14*$exch if jahr == 1987
replace limit_marginal = 14.43*$exch if jahr == 1988
replace limit_marginal = 14.79*$exch if jahr == 1989
replace limit_marginal = 15.45*$exch if jahr == 1990
replace limit_marginal = 15.78*$exch if jahr == 1991

* West 1992 - 2001:
replace limit_marginal = 16.39*$exch if east == 0 & jahr == 1992
replace limit_marginal = 17.42*$exch if east == 0 & jahr == 1993
replace limit_marginal = 18.41*$exch if east == 0 & jahr == 1994
replace limit_marginal = 19.07*$exch if east == 0 & jahr == 1995
replace limit_marginal = 19.34*$exch if east == 0 & jahr == 1996
replace limit_marginal = 20.05*$exch if east == 0 & jahr == 1997
replace limit_marginal = 20.38*$exch if east == 0 & jahr == 1998
replace limit_marginal = 20.71*$exch if east == 0 & jahr == 1999
replace limit_marginal = 20.66*$exch if east == 0 & jahr == 2000
replace limit_marginal = 20.71*$exch if east == 0 & jahr == 2001

* West 2002 - 2023:
replace limit_marginal = 10.68 if east == 0 & jahr == 2002
replace limit_marginal = 13.15 if east == 0 & jahr == 2003
replace limit_marginal = 13.11 if east == 0 & jahr == 2004
replace limit_marginal = 13.15 if east == 0 & jahr == 2005
replace limit_marginal = 13.15 if east == 0 & jahr == 2006
replace limit_marginal = 13.15 if east == 0 & jahr == 2007
replace limit_marginal = 13.11 if east == 0 & jahr == 2008
replace limit_marginal = 13.15 if east == 0 & jahr == 2009
replace limit_marginal = 13.15 if east == 0 & jahr == 2010
replace limit_marginal = 13.15 if east == 0 & jahr == 2011
replace limit_marginal = 13.11 if east == 0 & jahr == 2012
replace limit_marginal = 14.79 if east == 0 & jahr == 2013
replace limit_marginal = 14.79 if east == 0 & jahr == 2014
replace limit_marginal = 14.79 if east == 0 & jahr == 2015
replace limit_marginal = 14.75 if east == 0 & jahr == 2016
replace limit_marginal = 14.79 if east == 0 & jahr == 2017
replace limit_marginal = 14.79 if east == 0 & jahr == 2018
replace limit_marginal = 14.79 if east == 0 & jahr == 2019
replace limit_marginal = 14.75 if east == 0 & jahr == 2020
replace limit_marginal = 14.79 if east == 0 & jahr == 2021
replace limit_marginal = 17.10 if east == 0 & jahr == 2022
replace limit_marginal = 17.10 if east == 0 & jahr == 2023

* East 1992 - 2001:
replace limit_marginal = 09.84*$exch if east == 1 & jahr == 1992
replace limit_marginal = 12.82*$exch if east == 1 & jahr == 1993
replace limit_marginal = 14.47*$exch if east == 1 & jahr == 1994
replace limit_marginal = 15.45*$exch if east == 1 & jahr == 1995
replace limit_marginal = 16.39*$exch if east == 1 & jahr == 1996
replace limit_marginal = 17.10*$exch if east == 1 & jahr == 1997
replace limit_marginal = 17.10*$exch if east == 1 & jahr == 1998
replace limit_marginal = 20.71*$exch if east == 1 & jahr == 1999
replace limit_marginal = 20.66*$exch if east == 1 & jahr == 2000
replace limit_marginal = 20.71*$exch if east == 1 & jahr == 2001

* East 2002 - 2023:
replace limit_marginal = 10.68 if east == 1 & jahr == 2002
replace limit_marginal = 13.15 if east == 1 & jahr == 2003
replace limit_marginal = 13.11 if east == 1 & jahr == 2004
replace limit_marginal = 13.15 if east == 1 & jahr == 2005
replace limit_marginal = 13.15 if east == 1 & jahr == 2006
replace limit_marginal = 13.15 if east == 1 & jahr == 2007
replace limit_marginal = 13.11 if east == 1 & jahr == 2008
replace limit_marginal = 13.15 if east == 1 & jahr == 2009
replace limit_marginal = 13.15 if east == 1 & jahr == 2010
replace limit_marginal = 13.15 if east == 1 & jahr == 2011
replace limit_marginal = 13.11 if east == 1 & jahr == 2012
replace limit_marginal = 14.79 if east == 1 & jahr == 2013
replace limit_marginal = 14.79 if east == 1 & jahr == 2014
replace limit_marginal = 14.79 if east == 1 & jahr == 2015
replace limit_marginal = 14.75 if east == 1 & jahr == 2016
replace limit_marginal = 14.79 if east == 1 & jahr == 2017
replace limit_marginal = 14.79 if east == 1 & jahr == 2018
replace limit_marginal = 14.79 if east == 1 & jahr == 2019
replace limit_marginal = 14.75 if east == 1 & jahr == 2020
replace limit_marginal = 14.79 if east == 1 & jahr == 2021
replace limit_marginal = 17.10 if east == 1 & jahr == 2022
replace limit_marginal = 17.10 if east == 1 & jahr == 2023


********************************************************************************
* Flag marginal wages
********************************************************************************
gen marginal = 0
replace marginal = 1 if tentgelt <= limit_marginal



********************************************************************************
* Labels
********************************************************************************
label variable limit_marginal "Marginal part-time income threshold"
label variable marginal "1 if marginal wage, 0 otherwise"



log close
