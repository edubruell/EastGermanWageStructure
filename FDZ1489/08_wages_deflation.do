cap log close
log using ${log}\08_wages_deflation.log, replace



/*
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	SIAB Preparation
	
	Deflate wages, marginal part-time income threshold and contribution assessment ceiling
	
	Consumer Price Index:
		Statistisches Bundesamt (2019)
		Preise - Verbraucherpreisindizes fuer Deutschland (Lange Reihe ab 1948)
		https://www.destatis.de/DE/Themen/Wirtschaft/Preise/Verbraucherpreisindex/_inhalt.html

		
	Generates the variables:
		- wage_defl: daily wage, deflated
		- limit_marginal_defl: marginal part-time income threshold, deflated
		- limit_assess_defl: contribution assessment ceiling, deflated
	
	
	Author(s): Wolfgang Dauth, Johann Eppelsheimer
	
	Version: 1.0
	Created: 2020-03-23
	
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		
*/



********************************************************************************
* Consumer Price Index
********************************************************************************

cap drop cpi
gen cpi = .
label variable cpi "Consumer Price Index"

replace cpi=    39.9    if jahr == 1975
replace cpi=    41.6    if jahr == 1976
replace cpi=    43.2    if jahr == 1977
replace cpi=    44.4    if jahr == 1978
replace cpi=    46.2    if jahr == 1979
replace cpi=    48.7    if jahr == 1980
replace cpi=    51.8    if jahr == 1981
replace cpi=    54.4    if jahr == 1982
replace cpi=    56.3    if jahr == 1983
replace cpi=    57.6    if jahr == 1984
replace cpi= 	58.8	if jahr == 1985
replace cpi=    58.7    if jahr == 1986
replace cpi=    58.9    if jahr == 1987
replace cpi=    59.6    if jahr == 1988	
replace cpi=	61.3	if jahr == 1989
replace cpi=	62.9	if jahr == 1990
replace cpi=	65.5	if jahr == 1991
replace cpi=	68.8	if jahr == 1992
replace cpi=	71.9	if jahr == 1993
replace cpi=	73.8	if jahr == 1994
replace cpi=	75.1	if jahr == 1995
replace cpi=	76.2	if jahr == 1996
replace cpi=	77.6	if jahr == 1997
replace cpi=	78.3	if jahr == 1998
replace cpi=	78.8	if jahr == 1999
replace cpi=	79.9 	if jahr == 2000
replace cpi=	81.5	if jahr == 2001
replace cpi=	82.6	if jahr == 2002
replace cpi=	83.5	if jahr == 2003
replace cpi=	84.9	if jahr == 2004
replace cpi=	86.2	if jahr == 2005
replace cpi=	87.6	if jahr == 2006
replace cpi=	89.6	if jahr == 2007
replace cpi=	91.9	if jahr == 2008
replace cpi=	92.2	if jahr == 2009
replace cpi=	93.2	if jahr == 2010
replace cpi=	95.2	if jahr == 2011
replace cpi=	97.1	if jahr == 2012
replace cpi=	98.5	if jahr == 2013
replace cpi=	99.5	if jahr == 2014
replace cpi=	100.0	if jahr == 2015
replace cpi=	100.5	if jahr == 2016
replace cpi=	102.0	if jahr == 2017
replace cpi=	103.8	if jahr == 2018
replace cpi=	105.3	if jahr == 2019
replace cpi=	105.8	if jahr == 2020
replace cpi=	109.1	if jahr == 2021
replace cpi=	117.7	if jahr == 2022

*********************************************************************************************************
* Deflate wages, marginal part-time income threshold and contribution assessment ceiling (base year 2015)
*********************************************************************************************************
gen wage_defl           = 100 * tentgelt / cpi
gen limit_marginal_defl = 100 * limit_marginal / cpi
gen limit_assess_defl   = 100 * limit_assess / cpi

label variable wage_defl "Daily wage, deflated (2015)"
label variable limit_marginal_defl "Marginal part-time income threshold, deflated (2015)"
label variable limit_assess_defl "Contribution assessment ceiling, deflated (2015)"




log close
