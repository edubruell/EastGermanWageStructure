
cap log close
log using "$log/eb01_file_diag.log", replace


// Run diagnostics on files in the orig folder
fileDiagnostics, folder("$orig") pattern("*.dta")
fileDiagnostics, folder("$data") pattern("*.dta")

cap log close



