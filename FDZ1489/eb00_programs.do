//--------------------------------------------------------------
// Software dependencies
// Installs the rif and gtools packages from SSC
//--------------------------------------------------------------
fdzinstall rif
fdzinstall gtools


//--------------------------------------------------------------
// Program: displaycond
// Purpose: Conditionally display a string if binary flag == 1.
// Usage:   displaycond "Hello" , b(1)
//--------------------------------------------------------------
cap program drop displaycond
program displaycond 
    syntax anything, b(integer)
    if `b' == 1 {
        display "`1'"
    }
end

//--------------------------------------------------------------
// Program: fileDiagnostics
// Purpose: Loop over all .dta files in a folder, open each file,
//          and run basic diagnostics (describe, summarize).
//--------------------------------------------------------------
cap program drop fileDiagnostics 
program define fileDiagnostics
    version 14
    // Use syntax to parse options: folder is required; pattern is optional.
    syntax , folder(string) [pattern(string)]
    
    // If the pattern is not provided, default to "*.dta"
    if "`pattern'" == "" {
        local pattern "*.dta"
    }
    
    // Get list of files in the specified folder matching the pattern.
    local files: dir "`folder'" files "`pattern'"
    
    
    // Loop over each file in the folder.
    foreach file of local files {
        di "----------------------------------------------"
        di "Processing file: `folder'/`file'"
        
        // Open the file.
        use "`folder'/`file'", clear
        
        // Display file information and basic diagnostics.
        describe, short
        codebook _all, compact
		
    }
end

//--------------------------------------------------------------
// Program: dfl_yearly
// Purpose: Compute year-on-year DFL weights by comparing a base
//          year (cyear) to other years in the data.
// Usage:   dfl_yearly x1 x2 x3, cyear(1995) gen(dfl_wt)
//--------------------------------------------------------------
cap program drop dfl_yearly
program dfl_yearly 
    version 13
    syntax varlist(fv ts) [, cyear(int 1995)] gen(string)

    // Get all years in data and subtract cyear
    quietly levelsof year, local(years_in_data)
    quietly local years_in_data: list years_in_data - cyear

    // Initialize the output variable
    gen `gen' = .

    // Loop over all years other than the base year
    foreach y of local years_in_data {
        quietly {
            tempvar period prob1 p_e0 p_e1
            display "-> Computing weights for year `y'"

            // Create treatment indicator (base year = 1)
            gen `period' = .
            gen `prob1'  = .
            replace `period' = 1 if year == `cyear'
            replace `period' = 0 if year == `y'

            // Run logit separately by region (east==0/1)
            forvalues e = 0/1 {
                logit `period' `varlist' if east == `e'
                predict `p_e`e'' if east == `e', p
                replace `prob1' = `p_e`e'' if east == `e'
                replace `gen'  = (`prob1') / (1 - `prob1') if year == `y' & east == `e'
            }
        }
    }

    // Set weight to 1 in base year
    replace `gen' = 1 if year == `cyear'
end


//--------------------------------------------------------------
// Program: resid_sd
// Purpose: Estimate wage regression residuals and compute
//          standard deviations by year and region.
// Usage:   resid_sd x1 x2 x3, yvar(lwage_imp) tag(mytag)
// Output:  Matrix m_sd_<tag> with sd(lwage_imp), sd(residuals),
//          and sample sizes by east × year.
//--------------------------------------------------------------
cap program drop resid_sd
program resid_sd 
    version 13
    syntax varlist(fv ts) [, v yvar(string)] tag(string)

    // Default dependent variable if none provided
    if "`yvar'" == "" {
        local yvar = "lwage_imp"
    }

    quietly levelsof year, local(ys)
    display "Computing `yvar' residuals for varlist: `varlist'"

    tempvar resid loop_temp
    quietly gen `resid' = .

    // Loop over years and regions
    foreach y of local ys {
        forvalues e = 0/1 {
            quietly reg `yvar' `varlist' if east == `e' & year == `y'
            quietly predict `loop_temp', residuals
            quietly replace `resid' = `loop_temp' if east == `e' & year == `y'
            quietly drop `loop_temp'
        }
    }

    preserve
        gen resid_sd = `resid'
        collapse (sd) sd = `yvar' resid_sd (sum) n_persnr n_betr, by(east year)
        mkmat east year sd resid_sd n_persnr n_betr, mat(m_sd_`tag')
    restore
end


//--------------------------------------------------------------
// Program: otf_rifs
// Purpose: Compute on-the-fly RIF decompositions for a list of 
//          categorical variables using DFL reweighting.
// Usage:   otf_rifs x1 x2 x3, byear(1995) mwage(mwage) lwage(lwage_imp) name_tag(_dfl)
// Inputs:  
//   - namelist: List of categorical variables
//   - byear:    Reference year for DFL weights (default: 1995)
//   - mwage:    Variable for unconditional RIFs (default: mwage)
//   - lwage:    Variable for conditional IQR RIFs (default: lwage_imp)
//   - name_tag: Suffix for output matrix names
// Output:
//   - Matrices m_<var><name_tag>, r_<var><name_tag>, s_<var><name_tag>
//     with group-level RIF means (percentiles and wage gaps), shares,
//     and counts by year, region, and category level.
// Notes:
//   - Computes both unweighted and DFL-weighted RIFs
//   - Uses rifvar() function from the rif package
//   - Relies on dfl_yearly for DFL weights
//--------------------------------------------------------------
cap program drop otf_rifs
program otf_rifs
	syntax namelist[, v byear(int 1995) mwage(string) lwage(string) name_tag(string)]
	
	//Set program inputs
	local b_verbose = 0
	if "`v'" == "v"{
		local b_verbose = 1
	}
	if "`mwage'"==""{
		local mwage = "mwage"
	}
	if "`lwage'"==""{
		local lwage = "lwage_imp"
	}
	
	//Start main program loop
	foreach vname in `namelist'{
		tempname m_unweighted m_weighted 	
		local var = "`vname'"	
		forvalues i = 0/1{
			//Lookup labels
			tempname n_lab w_lab 
			label define `n_lab' 0 "unweighted" 1 "weighted"
			label define `w_lab' 0 "" 1 "[pw=otf_dfl]"
			//Loaded
			local name:  label `n_lab' `i'
			local wght:  label `w_lab' `i'
			//Output if verbose run
			preserve 
				keep if year>=`byear'
				//Only work with explicit missings (9999 should indicate missing in output)
				keep if `var' !=.
				if `i'==0 & "`mwage'"!="mwage"{
					drop rif_85 rif_50 rif_15 w_85_50 w_50_15
					gsort ey 
					displaycond "Compute base RIFs for `var' for `mwage'",b(`b_verbose')
					foreach p in "15" "50" "85"{
						displaycond "Compute OTF-RIF for `p'-pct.",b(`b_verbose')							
						quietly by ey: egen rif_`p'=rifvar(`mwage'),  q(`p')
					}
					displaycond "Compute OTF-RIF for wage-gaps for `mwage'",b(`b_verbose')
					quietly by ey: egen w_85_50 = rifvar(`lwage'), iqr(50 85)
					quietly by ey: egen w_50_15 = rifvar(`lwage'), iqr(15 50)
				}
				if `i'==1{
					displaycond "Compute  dfl weights for `var'",b(`b_verbose')
					quietly dfl_yearly i.`var', cyear(`byear') gen("otf_dfl")
					drop rif_85 rif_50 rif_15 w_85_50 w_50_15
					gsort ey 
					displaycond "Compute  on-the-fly weighted RIFs for `var'",b(`b_verbose')
					foreach p in "15" "50" "85"{
						displaycond "Compute OTF-RIF for `p'-pct.",b(`b_verbose')							
						quietly by ey: egen rif_`p'=rifvar(`mwage'),  q(`p') weight(otf_dfl) 
					}
					displaycond "Compute OTF-RIF for wage-gaps",b(`b_verbose')
					quietly by ey: egen w_85_50 = rifvar(`lwage'), iqr(50 85) weight(otf_dfl) 
					quietly by ey: egen w_50_15 = rifvar(`lwage'), iqr(15 50) weight(otf_dfl) 
				}
				//Collapse rif means and count persons
				quietly gen n_obs = !missing(`var')
				gcollapse (mean) rif_85 rif_50 rif_15  w_85_50 w_50_15 (sum) n_obs n_betr `wght', by(`var' east year)
				sort east year `var' 
				by east year: gegen total_p = total(n_obs)
				gen double share = n_obs/total_p
				rename n_obs n_persnr
				gen ffl = `i'
				mkmat east year `var' ffl rif_85 rif_50 rif_15 w_85_50 w_50_15 share n_persnr n_betr, mat(`m_`name'')
			restore

		}
	//Export to matrix
	matrix m_`vname'`name_tag' = `m_unweighted' \ `m_weighted' 
	//Correct output of persnr for FFL 
	preserve
		local varnames: colfullnames  m_`vname'`name_tag'
		drop _all
		quietly{
			svmat m_`vname'`name_tag', names(col)
			replace n_persnr=. if ffl==1
			replace n_betr=. if ffl==1
			bys east year `var': gegen vtemp = min(n_persnr)
		    bys east year `var': gegen btemp = min(n_betr)
			replace n_betr   = btemp  
			replace n_persnr = vtemp
			sort  east year `var' ffl
		}
		sort  east year `var' ffl
		mkmat east year `var' ffl rif_85 rif_50 rif_15 n_persnr n_betr, mat(r_`vname'`name_tag')
	    mkmat east year `var' ffl w_85_50 w_50_15 share n_persnr n_betr, mat(s_`vname'`name_tag')
		mkmat `varnames', mat(m_`vname'`name_tag')
	restore
	}	
end


//--------------------------------------------------------------
// Program: dfl_pct
// Purpose: Compute DFL-reweighted wage percentiles (15/50/85)
//          by region and year for a list of categorical vars.
// Usage:   dfl_pct x1 x2 x3
// Output:  Matrix m_p_dfl_<var> with percentiles by east × year.
// Notes:
//   - Recomputes DFL weights internally using dfl_yearly
//   - Very slow: re-estimates weights and does not cache them
//   - Uses gcollapse with percentile syntax and [pw=otf_dfl]
//--------------------------------------------------------------
cap program drop dfl_pct
program dfl_pct
	syntax namelist
	foreach vname in `namelist'{	
	display "DFL-weighted percentiles for `vname'"
	tempfile pers_sums
	
		// Save unweighted sums before applying weights
		preserve
			collapse (sum) n_persnr = n_persnr (sum) n_betr = n_betr, by(east year)
			tempfile sums
			save `pers_sums', replace
		restore
		
		//Run the yearly DFL and save its output to a matrix
		preserve
			dfl_yearly i.`vname', cyear(1995) gen("otf_dfl")
			gcollapse (p15) p15 = mwage (p50) p50 = mwage  (p85) p85 = mwage [pw=otf_dfl], by(east year)
			merge 1:1 east year using `pers_sums', nogen
			mkmat east year p15 p50 p85 n_persnr n_betr, mat(m_p_dfl_`vname')
		restore
	}
end

//--------------------------------------------------------------
// Program: output_mtx
// Purpose: Display matrices with RIF decomposition results 
//          (percentiles, wage gaps, and group shares).
// Usage:   output_mtx x1 x2 x3
// Output:  Lists of r_<name> and s_<name> matrices in Results window.
// Notes:
//   - r_<name>: Contains percentiles (rif_15, rif_50, rif_85)
//   - s_<name>: Contains wage gaps (w_85_50, w_50_15) and group shares
//--------------------------------------------------------------
cap program drop output_mtx
program output_mtx
	syntax namelist
	foreach name in `namelist'{
		display _newline
		display "Ausgabe der RIF-Werte für Perzentile mit und ohne Gewichtung"
		display ". matrix list r_`name'"
		matrix list r_`name'
		display _newline
		display "Ausgabe der RIF-Werte für Lohnluecken und Gruppen-Anteile mit und ohne Gewichtung"
		display ". matrix list s_`name'"
		matrix list s_`name'
	}
end
