/* 
DE LOECKER - EECKHOUT - UNGER
The rise of market power and the macroeconomic implications
Quarterly Journal of Economics 2020

USER INFO 
Create folder structure as in zip file:
/code/ 
/data/
/data/PF/
/temp/
/output/tables
/output/figures
/output/figures/BW
*/	
// qui {
if "`c(username)'"=="" {
	local dropbox ""
	local dump "" 
	}
set more off
global dropbox  = "`c(pwd)'/.."
cd "$dropbox"
// shell "C:/PROGRA~1/R/R-4.0.3/bin/x64/Rscript.exe" --vanilla code/RunDeLoeckerEtAlCompustatQuery.R
* Create dataset from latest download (with # trimmed percentages)
do "code/Create_Data.do"
* Create temp file with main variables for analysis
cd "$dropbox"
do "code/Create_Temp.do"
* Create output compustat (main): tables and figures
cd "$dropbox"
do "code/Create_Output.do"
* erase all files in temp file
cd "$dropbox"
/*cd "temp/"
local list : dir . files "*.dta"
foreach f of local list {
    erase "`f'"
}
* Dropping black-white figures
cd "$dropbox/output/figures/BW/"
locale list: dir . files "*.eps"
foreach f of local list{
	erase "`f'"
	}*/
// }
