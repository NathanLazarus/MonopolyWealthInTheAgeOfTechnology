/* DE LOECKER - EECKHOUT - UNGER
The rise of market power and the macroeconomic implications
Quarterly Journal of Economics
* US compustat - history 
*/

/* Use stata file downloaded Compustat using protocol:
Access compustat (WRDS KU Leuven FEB)
> Compustat - Capital IQ from Standard & Poor's
> NORTH AMERICA
> FUNDAMENTALS ANNUAL
> DATE RANGE 1955 - 2016
> GVKEY CODE - search the entire database
> CONSOLIDATED ACCOUNTS, FORMAT INDL AND FS (BELOW DROP FS IF REPORTED BOTH), DOMESTIC POPULATION SOURCE, DATA FORMAT STD, 
>> DATA SELECTED:
	SALE, COGS, XLR, XSGA, PPEGT, PPENT, INTAN, XRD, XAD, EMP, MKVALT, DVT, INDUSTRY INFO (NAICS)
	additional data for robustness: foreign incorp code, company name, 
	
	* external datasets:
		1. interest_rate.dta
		2. us_gdpdeflator.dta
*/
cd "data/"
* insert your data file here:
 use "datafile.dta", clear
// qui {
sort gvkey fyear
rename fyear year
bysort gvkey year : gen nrobs = _N
* Keep only observation for one industry (some firms are in several industries) 
drop if (nrobs == 2 | nrobs == 3) & indfmt == "FS"
sort gvkey year
drop if gvkey==gvkey[_n-1] & year==year[_n-1]

* Drop firms without industry information
keep if naics~=""
* Take into account obs with industry code obs for which only d-1 digits in the d-category!!!
forvalues i =2/4 {
gen ind`i'd 								= substr(naics,1,`i')
destring ind`i'd, replace
egen nrind`i' = group(ind`i'd)
}

* write code to put all $ vars into comparable units!!!! 
gen newmk2 = prcc_f * csho
label var newmk2 " fiscal year market value prior 1998
replace mkvalt  = newmk2 if mkvalt==.

* use following variables:
keep gvkey year naics ind* sale cogs xsga xlr xrd xad dvt ppegt intan emp mkvalt oibdp
replace sale	= sale*1000
replace xlr		= xlr*1000
replace oibdp	=oibdp*1000
replace cogs	= cogs*1000
replace xsga 	= xsga*1000
replace mkvalt 	= mkvalt*1000
replace dvt 	= dvt*1000
replace ppegt	= ppegt*1000
*replace ppent 	= ppent*1000
replace intan	= intan*1000

/* Macro vars: - Merge in Usercost and US GDP deflator
- deflator: use US-wide for main specification, industry specific deflators dating back to 1955 scattered across industry classification changes
 comment: no impact for markup measure, up to estimation of output elasticity! Robustness deflators see appendix.
- User cost of capital computed using FRED nominal interest rate, inflation and calibrated depreciation (See text)
*/
sort year
merge year using "macro_vars.dta", _merge(macro)
keep if macro==3
* Deflated values
gen sale_D		= (sale/USGDP)*100
gen cogs_D 		= (cogs/USGDP)*100
gen xsga_D 		= (xsga/USGDP)*100
gen mkvalt_D 	= (mkvalt/USGDP)*100
gen dividend_D	= (dvt/USGDP)*100
gen capital_D   = (ppegt/USGDP)*100
*gen capital2_D  = (ppent/USGDP)*100
gen intan_D		= (intan/USGDP)*100
gen xlr_D 		= (xlr/USGDP)*100
gen kexp		= (usercost*capital_D)
gen mat1 		= ((sale-xlr-oibdp)/USGDP)*100
* materials is generated from sales, wagebill and operating income bdp, as in Keller and Yeaple (Restat)

* TRIM : no negative values
drop if sale_D<0 
drop if cogs_D<0
drop if xsga<0
* trim on sales-cogs ratio as mu_0 is simply 0.85*sales/cogs
gen s_g = sale/cogs
keep if s_g>0
gen trim=0
keep if year>1949

* save files to temp directory
cd "$dropbox/temp/"
* main results for 1% trim (below)
* robustness for appendix: change to p(x) p(y) with x=2-5 and y=95-98
* robustness 2% and 5%
forvalues t=1/5 {
bysort year: egen s_g_p_`t'  = pctile(s_g), p(`t')
}
forvalues s=95/99 {
bysort year: egen s_g_p_`s'  = pctile(s_g), p(`s')
}
* label vars:
label var cogs "Costs directly allocated to production, such as material, labor and overhead.
label var emp "Nr people employed by the company and its consolidated subsidiaries in thousands
foreach var of varlist sale cogs xsga mkvalt intan xlr {
label var `var'_D "Deflated `x'
}
label var dividend_D "Deflated Dvt
label var capital_D "Deflated cap
label var kexp "real capital expenditure

label var ind2d "2 digit NAICS
label var ind3d "3 digit NAICS
label var ind4d "4 digit NAICS
label var usercost "usercost (i-delf+deprc)
label var kexp "capital expenses
label var mat1 "material cost imputed
label var s_g "sale-cogs ratio
sort gvkey year

preserve
keep if s_g> s_g_p_1 & s_g< s_g_p_99
replace trim = 1
drop s_g_p* macro trim indfmt
save data_main_upd_trim_1.dta, replace
restore

* switch on for higher trim
forvalues t=2/5 {
preserve
keep if s_g> s_g_p_2 & s_g< s_g_p_98
replace trim = `t'
save data_main_upd_trim_`t'.dta, replace
drop s_g_p* macro trim 
restore
}
// }
*--------------------------------------------------*
* data created 
