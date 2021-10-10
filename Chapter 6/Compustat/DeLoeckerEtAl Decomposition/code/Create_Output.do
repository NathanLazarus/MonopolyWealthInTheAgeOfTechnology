/*
RISE OF MARKET POWER -- VERSION NOV 2019 [QJE] 
 MAKE RESULTS: PULLS IN TEMP FILE FROM CREATE_TEMP_FILE
*/
use "temp/temp_file.dta", clear
cd "output/"
// qui {
*---------------------------------------------------------------------------------------------------------------------------*
* TABLES
*---------------------------------------------------------------------------------------------------------------------------*
* Table 1: *- DECOMPOSITIONS: INDUSTRY WIDE OVER TIME
preserve
forvalues d=2/4{
use "tables/Table1_data_spec1_digit_`d'.dta", clear
log using "tables/table1", replace
tabstat MARKUP_spec1 DMARKUP_spec1 WITHIN1_IND`d'_st BETWEEN1_IND`d'_st REALLOC1_IND`d'_st , by(year) stat(mean) format(%5.3f) notot
log close
}
restore
*---------------------------------------------------------------------------------------------------------------------------*
* Table 2: Markups, market value and dividens
preserve
gen lmktv 	= ln(mkvalt_D)
gen ldiv	= ln(dividend_D)
gen lmktv_s = ln(mkvalt_D/sale_D)
gen ldiv_s	= ln(dividend_D/sale_D)
gen lsales 	= ln(sale_D)

forvalues r= 1/2 {
foreach x of varlist lmktv_s ldiv_s  {
reg `x' lmu`r' , cluster(id)
estimates store `x'_mu`r'_A
reg `x' lmu`r' i.year, cluster(id)
estimates store `x'_mu`r'_B
areg `x' lmu`r' i.year, a(ind2d)
estimates store `x'_mu`r'_C
areg `x' lmu`r' i.year, a(id) 
estimates store `x'_mu`r'_D
}
}
forvalues r= 1/2 {
foreach x of varlist lmktv ldiv {
reg `x' lmu`r' lsales, cluster(id)
estimates store `x'_mu`r'_As
reg `x' lmu`r' lsales i.year, cluster(id)
estimates store `x'_mu`r'_Bs
areg `x' lmu`r' lsales i.year, a(ind2d) 
estimates store `x'_mu`r'_Cs
areg `x' lmu`r' lsales i.year, a(id) 
estimates store `x'_mu`r'_Ds
}
}
log using "tables/table2", replace
estimates table lmktv* , keep(lmu1 lsales) stat(r2) b(%4.2f) se(%4.2f) modelwidth(15)
estimates table ldiv*, keep(lmu1 lsales) stat(r2) b(%4.2f) se(%4.2f) modelwidth(15)
log close
restore
*---------------------------------------------------------------------------------------------------------------------------*
* Table 3: Markups and R&D and Advertising
preserve
sort gvkey year 
gen RD = xrd*1000
gen RD_D = (RD/USGDP)*100
gen rd_s = RD/sale
gen AD = xad*1000
gen AD_D = (AD/USGDP)*100
gen ad_s = AD/sale
gen lnrd_s = ln(rd_s)
gen lnad_s = ln(ad_s)
bysort year: egen TRD = sum(RD_D)
bysort year: egen TAD = sum(AD_D)
gen RD_s = TRD/TOTSALES
gen AD_s = TAD/TOTSALES
gen ln_pi 		= ln(pi_rate2)
gen lnsga_s 	= ln(xsga_D/sale_D)
gen AD_dum		= 1 if AD~=. & AD>0
replace AD_dum 	= 0 if AD_dum==.
gen RD_dum		= 1 if RD~=. & RD>0
replace RD_dum	= 0 if RD_dum==.
areg lmu1 RD_dum AD_dum i.year, a(ind2d) cluster(id)
estimate store extensive
keep if xsga_D~=. 
keep if xrd~=.
keep if xad~=.
keep if xrd>0
keep if xad>0
keep if pi_rate2>0
areg lmu1 lnsga_s, a(ind2d) cluster(id)
estimates store lmu1_sga
areg lmu1 lnrd_s lnad_s, a(ind2d) cluster(id)
estimates store lmu1_rdad
areg ln_pi lnrd_s lnad_s, a(ind2d) cluster(id)
estimates store lnpi_rdad
log using "tables/table3", replace
estimates table extensive lmu1_sga lmu1_rdad lnpi_rdad, stat(r2  N) b(%4.2f) se(%4.2f) keep(RD_dum AD_dum lnsga_s lnrd_s lnad_s)
log close
restore
*---------------------------------------------------------------------------------------------------------------------------*
* Table 4:  Aggregation and Profits
* see output Figure 11
*---------------------------------------------------------------------------------------------------------------------------*
* Table 5: Labor share and markups
preserve
gen wagebill1 = xlr_D
gen labshare = wagebill1/sale_D
gen csl = xlr_D/totcost1 
gen csl2 = xlr_D/totcost2
drop if wagebill1 ==.
drop if labshare>1
drop if labshare<=0.025
drop if csl >1
drop if csl<=0.025
gen lnls = ln(wagebill1 ) - ln(sale_D)
gen lmu_spec1 = ln(mu_spec1)
gen lmu_spec2 = ln(mu_spec2)
gen lcsl = ln(csl)
gen lcsl2 = ln(csl2)
keep if labshare<2 & labshare>0 
reg lnls lmu_spec1, cluster(id)
estimates store mu_ls0 
reg lnls lmu_spec1 i.year, cluster(id)
estimates store mu_ls1
areg lnls lmu_spec1 i.year, a(ind2d) cluster(id)
estimates store mu_ls2 
areg lnls lmu_spec1 i.year, a(id) cluster(id)
estimates store mu_ls3 
areg lnls lmu_spec1 lcsl i.year, a(ind2d) cluster(id)
estimates store mu_ls4 
areg lnls lmu_spec1 lcsl i.year, a(id) cluster(id)
estimates store mu_ls5
areg lnls lmu_spec2 lcsl2 i.year, a(ind2d) cluster(id)
estimates store mu_ls6 
areg lnls lmu_spec2 lcsl2 i.year, a(id) cluster(id)
estimates store mu_ls7
log using "tables/table5", replace
estimates table mu_ls*, keep(lmu_spec1 lmu_spec2  lcsl lcsl2) stat(r2  N) b(%4.2f) se(%4.2f)
log close 
restore 
*---------------------------------------------------------------------------------------------------------------------------*
* Table 6: Capital share and markups
* 6. capital share analysis
preserve
gen lmu_spec1 = ln(mu_spec1)
gen lmu_spec2 = ln(mu_spec2)
gen capshare = kexp/sale_D
gen csk = kexp/totcost1
gen lck = ln(csk)
gen csk2 = kexp/totcost2
gen lck2 = ln(csk2)
drop if capshare<0
egen cap_p1 = pctile(capshare), p(1)
egen cap_p99 = pctile(capshare), p(99) 
drop if capshare<cap_p1
drop if capshare>cap_p99
gen lnks = ln(capshare)
reg lnks lmu_spec1, cluster(id)
estimates store mu_ks0 
reg lnks lmu_spec1 i.year, cluster(id)
estimates store mu_ks1
areg lnks lmu_spec1 i.year, a(ind2d) cluster(id)
estimates store mu_ks2 
areg lnks lmu_spec1 i.year, a(id) cluster(id)
estimates store mu_ks3 
areg lnks lmu_spec1 lck i.year, a(ind2d) cluster(id)
estimates store mu_ks4 
areg lnks lmu_spec1 lck i.year, a(id) cluster(id)
estimates store mu_ks5
areg lnks lmu_spec2 lck2 i.year, a(ind2d) cluster(id)
estimates store mu_ks6 
areg lnks lmu_spec2 lck2 i.year, a(id) cluster(id)
estimates store mu_ks7
log using "tables/table6", replace
estimates table mu_ks*, keep(lmu_spec1 lmu_spec2 lck lck2) stat(r2 N) b(%4.2f) se(%4.2f)
log close 
restore
*---------------------------------------------------------------------------------------------------------------------------*
* FIGURES
cd "figures/"
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 1. 	MAIN FACT - Weighted aggregate Markup
preserve
sort year
drop if year==year[_n-1]
sort year
scatter MARKUP_spec1 year, c(l) lcolor(red ) lpattern(solid) symbol(none) lwidth(thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(5) ) 
graph export Fig1.pdf, replace
scatter MARKUP_spec1 year, c(l) lcolor(black ) lpattern(solid) symbol(none) lwidth(thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(5) ) 
graph export BW/Fig1.pdf, replace
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 2a and 2b. Aggregate Markup
scatter MARKUP_spec1 MARKUP0_AGG year, c(l l) lcolor(red green) lpattern(solid dash) msymbol(none none) lwidth(thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(11) ) 
graph export Fig2a.pdf, replace
scatter MARKUP_spec1 MARKUP0_AGG year, c(l l) lcolor(black black) lpattern(solid dash) msymbol(none none) lwidth(thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(11) ) 
graph export BW/Fig2a.pdf, replace
scatter MARKUP_spec1  MARKUP10_AGG_w2 year, c(l l l) lcolor(red green)  lpattern(solid dash) msymbol(none none) lwidth(thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(11) ) 
graph export Fig2b.pdf, replace
scatter MARKUP_spec1  MARKUP10_AGG_w2 year, c(l l l) lcolor(black black)  lpattern(solid dash) msymbol(none none) lwidth(thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(11) ) 
graph export BW/Fig2b.pdf, replace
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 3a and 3b Dsitribution of markups
label var mu_1_ms90 "P90
label var mu_1_ms50 "P50
label var mu_1_ms75 "P75
label var MARKUP10_AGG "Average
scatter  MARKUP10_AGG mu_1_ms90 mu_1_ms75 mu_1_ms50 year , connect(l l l l )  msymbol(none none none none ) color(red red red red) lpattern(solid dash shortdash longdash_dot) lwidth(thick thick thick thick) xtitle("") xlabel(1960 1970 1980 1990 2000 2010) ylabel(1 1.5 2 2.5) legend(ring(0)  pos(10) )   sort
graph export Fig3b.pdf, replace
scatter  MARKUP10_AGG mu_1_ms90 mu_1_ms75 mu_1_ms50 year , connect(l l l l )  msymbol(none none none none ) color(black black black black) lpattern(solid dash shortdash longdash_dot) lwidth(thick thick thick thick) xtitle("") xlabel(1960 1970 1980 1990 2000 2010) ylabel(1 1.5 2 2.5) legend(ring(0)  pos(10) )   sort
graph export BW/Fig3b.pdf, replace
restore
preserve
drop if mu_1<0.5
drop if mu_1> 3.5
twoway (kdensity mu_1 if (year==2016 ) , kernel(gaussian) xlabel(1 2 3) lcolor(red) lwidth(thick) ) (kdensity mu_1 if (year==1980 ),  kernel(gaussian)  lcolor(red) ytitle("") xtitle("") clpattern(dash) lwidth(thick) graphregion(color(white) )  legend(ring(0)  col(1) pos(2) label(1 "2016") label(2 "1980")  ) )
graph export Fig3a.pdf, replace
twoway (kdensity mu_1 if (year==2016 ) , kernel(gaussian) xlabel(1 2 3) lcolor(black) lwidth(thick) ) (kdensity mu_1 if (year==1980 ),  kernel(gaussian)  lcolor(black) ytitle("") xtitle("") clpattern(dash) lwidth(thick) graphregion(color(white) )  legend(ring(0)  col(1) pos(2) label(1 "2016") label(2 "1980")  ) )
graph export BW/Fig3a.pdf, replace
restore
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 4 Decomposition firm-level
preserve
* cutt-off year
keep if year>1979 & year<=2016
drop if ind2d <9 
drop if ind2d>99
bysort year: egen ts = sum(sale_D)
gen msagg = sale_D/ts

xtset id year, yearly
foreach x of varlist mu_spec1 {
bysort year: egen MU_agg 		= sum(msagg*`x')
xtset id year, yearly
gen d`x' 	= D.`x'
gen Lms 	= L.msagg
gen demean`x'= `x'-MU_agg
gen L`x' 	= L.demean`x'
gen Lmsagg	= L.msagg
gen dmsagg  = D.msagg
bysort year: egen Dagg`x' 		= sum(d`x'*Lmsagg)
bysort year: egen Daggms`x'		= sum(dmsagg*L`x')
bysort year: egen Cross_agg`x' 	= sum(dmsagg*d`x')
bysort ind2d year: egen D`x' 	= sum(d`x'*Lms)
bysort ind2d year: egen Dms`x' 	= sum(dms*L`x')
bysort ind2d year: egen Cross`x' 	= sum(dms*d`x')
}
* aggregate 
sort year
drop if year==year[_n-1]
gen DMU_agg = MU_agg -MU_agg[_n-1]
gen net_entry = DMU_agg - Daggmu_spec - Daggms -Cross_agg
gen REALL = net_entry + Daggms +Cross_agg
gen reall_inc = Daggms + Cross_agg
*initialise at 1980 so keep from 1980
gen Dagg_sum 	= sum(Daggmu)
gen Daggms_sum 	= sum(Daggms)
gen Cross_sum		= sum(Cross_agg)
gen Net_sum		= sum(net_entry)
gen Reall_sum	= sum(REALL)	
gen reall_inc_s	= sum(reall_inc)
gen a	=	1 if year==1980
egen a_m = mean(MU_agg) if a==1
egen a_mu = mean(a_m)
foreach x of varlist Dagg_sum Daggms_s Cross_s Net_s Reall_s reall_inc_s {
replace `x' = `x'+a_mu
}
label var MU_agg "Markup (benchmark)
label var Dagg_sum "Within 
label var Net_s "Net Entry 
label var reall_inc_s "Reallocation
scatter MU_agg Dagg_sum  reall_inc_s Net_s   year , c(l l l l) lwidth(thick thick thick thick) lpattern(solid dash shortdash longdash_dot) lcolor(red blue black green) xlabel(1980 1990 2000 2010) msymbol(none none none none none) sort xtitle("") legend(ring(0)  pos(11) ) 
graph export Fig4.pdf, replace
scatter MU_agg Dagg_sum  reall_inc_s Net_s   year , c(l l l l) lwidth(thick thick thick thick) lpattern(solid dash shortdash longdash_dot) lcolor(black black black black ) xlabel(1980 1990 2000 2010) msymbol(none none none none none) sort xtitle("") legend(ring(0)  pos(11) ) 
graph export BW/Fig4.pdf, replace
restore
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 5 : Using industry and econonmy-wide averages
bysort year: egen Tcogs = sum(cogs_D)
bysort year: egen TSales = sum(sale_D)
bysort ind2d year: egen tcogs2d = sum(cogs_D)
bysort ind2d year: egen ts2d = sum(sale_D)
sort ind2d year
preserve
drop if ind2d==ind2d[_n-1] & year==year[_n-1]
gen ms2d = ts2d/TSales
bysort year: egen CS_Agg2d = sum(ms2d*0.85*(ts2d/tcogs2d))
bysort year: egen CS_Agg2d_theta = sum(ms2d*theta_WI1_ct*(ts2d/tcogs2d))
gen CS_Agg = .85* TSales/Tcogs
label var CS_Agg "Averages (Economy-wide)
label var CS_Agg2d "Averages (By Industry)
label var CS_Agg2d_theta "Averages (By Industry, time-varying thetas)
label var MARKUP_spec1 "Benchmark Aggregate Markup
scatter MARKUP_spec1 CS_Agg2d_theta CS_Agg2d  CS_Agg year, c(l l l l) lcolor(red black black blue) lpattern(solid dash shortdash longdash_dot) msymbol(none none none none)  lwidth(thick thick thick thick thick) xlabel( 1960 1970 1980 1990 2000 2010) xtitle("")  legend(ring(0)  pos(11)  )
graph export Fig5.pdf, replace
scatter MARKUP_spec1 CS_Agg2d_theta CS_Agg2d  CS_Agg year, c(l l l l) lcolor(black black black black) lpattern(solid dash shortdash longdash_dot) msymbol(none none none none)  lwidth(thick thick thick thick thick) xlabel( 1960 1970 1980 1990 2000 2010) xtitle("")  legend(ring(0)  pos(11)  )
graph export BW/Fig5.pdf, replace
restore
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 6: census (see end do-file using disclosed census output)
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 7:  Aggregate overhead and capital cost shares of total cost
sort year
preserve
drop if year==year[_n-1]
label var cs_blue_rk_tot "Cost Share Capital
label var cs_blue_x_tot "Cost Share Overhead
scatter cs_blue_rk_tot cs_blue_x_tot year, c(l l l l )  lcolor(green blue) lpattern(dash solid) msymbol(none none none none) lwidth(thick thick thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0) pos(11) col(1)) 
graph export Fig7.pdf, replace
scatter cs_blue_rk_tot cs_blue_x_tot year, c(l l l l )  lcolor(black black) lpattern(dash solid) msymbol(none none none none) lwidth(thick thick thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0) pos(11) col(1)) 
graph export BW/Fig7.pdf, replace
restore
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 8: Average Profit Rate and Profit Rate Distribution 
preserve 
sort year
drop if year==year[_n-1]
scatter PR_1_AGG  year if year>1980, c(l l) lcolor(red red) msymbol(none none) ytitle("") lwidth(thick) xlabel(1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(5) ) 
graph export Fig8a.pdf, replace
scatter PR_1_AGG  year if year>1980, c(l l) lcolor(black black) msymbol(none none) ytitle("") lwidth(thick) xlabel(1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(5) ) 
graph export BW/Fig8a.pdf, replace
restore
preserve
drop if pi_rate2<-.15
drop if pi_rate2> .55
twoway (kdensity pi_rate2 if (year==2016 ) , kernel(gaussian)  lcolor(red) xlabel(0 .2 .4) lwidth(thick) ) (kdensity pi_rate2 if (year==1980 ),  kernel(gaussian)  lcolor(red) ytitle("") xtitle("") clpattern(dash) lwidth(thick) graphregion(color(white) )  legend(ring(0)  col(1) pos(2) label(1 "2016") label(2 "1980")  ) )
graph export Fig8b.pdf, replace
twoway (kdensity pi_rate2 if (year==2016 ) , kernel(gaussian)  lcolor(black) xlabel(0 .2 .4) lwidth(thick) ) (kdensity pi_rate2 if (year==1980 ),  kernel(gaussian)  lcolor(black) ytitle("") xtitle("") clpattern(dash) lwidth(thick) graphregion(color(white) )  legend(ring(0)  col(1) pos(2) label(1 "2016") label(2 "1980")  ) )
graph export BW/Fig8b.pdf, replace
restore
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 9: Market Value and Dividends
preserve
sort year
drop if year==year[_n-1]
keep if year>1961
label var MARKUP_spec1 "Markup
label var MKVAL_AGG "Market Value Share
label var DIV_AGG "Dividend Share
twoway (scatter MARKUP_spec1 year , c(l) yaxis(1) msymbol(none) color(red) lpattern(solid ) xtitle("")  lwidth(thick)) || (scatter MKVAL_AGG year, c(l) lpattern(dash) yaxis(2) msymbol(none) color(green) lwidth(thick) xtitle("") ), xlabel(1960 1970 1980 1990 2000 2010) ylabel(.5 1 1.5, axis(2)) legend(ring(0)  pos(10)) 
graph export Fig09a.pdf, replace
twoway (scatter MARKUP_spec1 year , c(l) yaxis(1) msymbol(none) color(black) lpattern(solid ) xtitle("")  lwidth(thick)) || (scatter MKVAL_AGG year, c(l) lpattern(dash) yaxis(2) msymbol(none) color(black) lwidth(thick) xtitle("") ), xlabel(1960 1970 1980 1990 2000 2010) ylabel(.5 1 1.5, axis(2)) legend(ring(0)  pos(10)) 
graph export BW/Fig09a.pdf, replace
twoway (scatter MARKUP_spec1 year , c(l) yaxis(1) msymbol(none) color(red) lpattern(solid ) xtitle("") ylabel(1.2 1.3 1.4 1.5 1.6) lwidth(thick)) || (scatter DIV_AGG year, c(l) lpattern(dash) yaxis(2) msymbol(none) color(green)  lwidth(thick)), xlabel(1960 1970 1980 1990 2000 2010) legend(ring(0)  pos(10) ) 
graph export Fig9b.pdf, replace
twoway (scatter MARKUP_spec1 year , c(l) yaxis(1) msymbol(none) color(black) lpattern(solid ) xtitle("") ylabel(1.2 1.3 1.4 1.5 1.6) lwidth(thick)) || (scatter DIV_AGG year, c(l)  lpattern(dash) yaxis(2) msymbol(none) color(black)  lwidth(thick)), xlabel(1960 1970 1980 1990 2000 2010) legend(ring(0)  pos(10) ) 
graph export BW/Fig9b.pdf, replace
restore
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 10: Markup, excess markup and SG&A share
preserve
gen cs = cogs_D /sale_D
gen ss = xsga_D /sale_D 
gen ks = kexp/sale_D
keep if ss>0 
drop if ss==.
egen ssp99 = pctile(ss), p(99)
egen ssp01 = pctile(ss), p(1)
egen ksp99 = pctile(ks), p(99)
egen ksp01 = pctile(ks), p(1)
drop if ss< ssp01 | ss> ssp99
drop if ks< ksp01 | ks> ksp99
gen m1_st = thetaW1_c/(1-ss -ks)
gen m2_st = thetaW2_c/(1-ss -ks)
gen m1 = mu_spec1
gen m2 = mu_spec2
gen year1 = "1980" if year==1980
replace year1 = "1990"  if year==1990
replace year1 = "2016" if year==2016
replace year1 = "2000" if year==2000
forvalues x=1/2 { 
gen m`x'd = m`x'-m`x'_st
bysort year: egen m`x'p20 = pctile(m`x'), p(20)
bysort year: egen m`x'p30 = pctile(m`x'), p(30)
bysort year: egen m`x'p45 = pctile(m`x'), p(45)
bysort year: egen m`x'p55 = pctile(m`x'), p(55)
bysort year: egen m`x'p70 = pctile(m`x'), p(70)
bysort year: egen m`x'p80 = pctile(m`x'), p(80)
bysort year: gen bin25_`x'  = 1 if m`x'<= m`x'p30 & m`x'>=m`x'p20
bysort year: gen bin75_`x'  = 1 if m`x'>= m`x'p70 & m`x'<=m`x'p80
bysort year: gen bin50_`x'  = 1 if m`x'>m`x'p45 & m`x'<=m`x'p55
bysort year: egen bin25_s`x' = sum(sale_D) if bin25_`x'==1
bysort year: egen mup25_s`x' = sum((sale_D/bin25_s`x')*m1) if bin25_`x'==1
bysort year: egen ssp25_s`x' = sum((sale_D/bin25_s`x')*ss) if bin25_`x'==1
bysort year: egen mdp25_s`x' = sum((sale_D/bin25_s`x')*m`x'd) if bin25_`x'==1
bysort year: egen bin75_s`x' = sum(sale_D) if bin75_`x'==1
bysort year: egen mup75_s`x' = sum((sale_D/bin75_s`x')*m1) if bin75_`x'==1
bysort year: egen ssp75_s`x' = sum((sale_D/bin75_s`x')*ss) if bin75_`x'==1
bysort year: egen mdp75_s`x' = sum((sale_D/bin75_s`x')*m`x'd) if bin75_`x'==1
bysort year: egen bin50_s`x' = sum(sale_D) if bin50_`x'==1
bysort year: egen mup50_s`x' = sum((sale_D/bin50_s`x')*m1) if bin50_`x'==1
bysort year: egen ssp50_s`x' = sum((sale_D/bin50_s`x')*ss) if bin50_`x'==1
bysort year: egen mdp50_s`x' = sum((sale_D/bin50_s`x')*m`x'd) if bin50_`x'==1

foreach x of varlist mup25_s`x' ssp25_s`x' mdp25_s`x' mup50_s`x' mup75_s`x' ssp50_s`x' ssp75_s`x' mdp50_s`x' mdp75_s`x' {
bysort year: egen K`x' = mean(`x')
}
}
* only for PF1 (trad. prod. technology)
forvalues x=1/1 {
sort year
drop if year==year[_n-1]
label var Kmup25_s`x' "P25
label var Kmup50_s`x' "P50
label var Kmup75_s`x' "P75
twoway (scatter Kmup25_s`x' Kssp25_s`x', mlabel(year1) mfcolor(none) msymbol(+) mcolor(blue)) || ///
(scatter Kmup50_s`x' Kssp50_s`x', mlabel(year1) mfcolor(none)  msymbol(O) mcolor(green)) ||                  ///
(scatter Kmup75_s`x' Kssp75_s`x', mlabel(year1) mfcolor(none)  msymbol(x) mcolor(red))                       ///
, xlabel(.1 .2 .3) legend(ring(0) pos(11) col(1))
graph export Fig10a.pdf, replace
twoway (scatter Kmup25_s`x' Kssp25_s`x', mlabel(year1) mfcolor(none) msymbol(+) mcolor(black)) || ///
(scatter Kmup50_s`x' Kssp50_s`x', mlabel(year1) mfcolor(none)  msymbol(O) mcolor(black)) ||                  ///
(scatter Kmup75_s`x' Kssp75_s`x', mlabel(year1) mfcolor(none)  msymbol(x) mcolor(black))                       ///
, xlabel(.1 .2 .3) legend(ring(0) pos(11) col(1))
graph export BW/Fig10a.pdf, replace

label var Kmdp25_s`x' "P25
label var Kmdp50_s`x' "P50
label var Kmdp75_s`x' "P75
twoway (scatter Kmdp25_s`x' Kssp25_s`x', mlabel(year1) mfcolor(none) msymbol(+) mcolor(blue)) || ///
(scatter Kmdp50_s`x' Kssp50_s`x' if Kmdp50_s`x'>-0.5, mlabel(year1) mfcolor(none)  msymbol(O)mcolor(green)) ||                  ///
(scatter Kmdp75_s`x' Kssp75_s`x' if Kmdp75_s`x'<1 & Kmdp75_s`x'>-0.5, mlabel(year1) mfcolor(none) msymbol(x) mcolor(red))                       ///
, xlabel(.1 .2 .3) legend(ring(0) pos(11) col(1))
graph export Fig10b.pdf, replace
twoway (scatter Kmdp25_s`x' Kssp25_s`x', mlabel(year1) mfcolor(none) msymbol(+) mcolor(black)) || ///
(scatter Kmdp50_s`x' Kssp50_s`x' if Kmdp50_s`x'>-0.5, mlabel(year1) mfcolor(none)  msymbol(O)mcolor(black)) ||                  ///
(scatter Kmdp75_s`x' Kssp75_s`x' if Kmdp75_s`x'<1 & Kmdp75_s`x'>-0.5, mlabel(year1) mfcolor(none) msymbol(x) mcolor(black))                       ///
, xlabel(.1 .2 .3) legend(ring(0) pos(11) col(1))
graph export BW/Fig10b.pdf, replace
}
restore
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 11: Decomposition of equation (15): overhead and aggregation
preserve
forvalues j=1/2 {
bysort year: egen a`j' = sum(share_firm_agg/mu_spec`j')
bysort year: egen ss`j' = sum(share_firm_agg*(xsga_D/sale_D))
bysort year: egen a`j'_F = sum(share_firm_agg*((1/mu_spec`j')-(xsga_D/sale_D)))
replace ss`j' = 0 if `j'==2
gen A`j' = 1-a`j'-ss`j'
gen A`j'_F = 1- a`j'_F 
}
bysort year: egen a1RTS1 = sum(share_firm_agg*(gamma_RTS1/mu_spec1))
bysort year: egen a1RTS2 = sum(share_firm_agg*(rts_w1/mu_spec1))
bysort year: egen a2RTS1 = sum(share_firm_agg*(gamma_RTS2/mu_spec2))
bysort year: egen a2RTS2 = sum(share_firm_agg*(rts_w2/mu_spec2))
forvalues j=1/2 {
gen A`j'RTS1 = 1-a`j'RTS1
gen A`j'RTS2 = 1-a`j'RTS2
}
forvalues j=1/2 {
gen SB`j' = 1-1/MARKUP_spec`j'
gen SB_F`j'= SB`j' - ss1
gen SB_A`j' = 1-a`j'
}
label var SB1 "SB (red)
label var SB2 "SB (blue)
label var SB_F1 " SB Fixed cost (red)
label var SB_A1 "Aggregation (red)
label var SB_A2 "Aggregation (blue)
label var A1 "DLE (red)
label var A2 "DLE (blue)
label var A2RTS1 "DLE RTS (blue)
sort year
drop if year==year[_n-1]
label var SB1 "Avg, No FC
label var SB_F1 "Avg, FC
label var SB_A1 "Aggr, No FC
label var A1 "Aggr, FC
label var SB2 "Avg, No RTS
label var SB_A2 "Aggr, No RTS
label var A2RTS1 "Aggr, RTS
label var A1 "Aggr, FC (PF1)
label var PR_1_AGG "Profit Rate
scatter SB1 SB_F1 SB_A1 A1 PR_1_AGG year if year>1979, c(l l l l l) lpattern(solid dash dot shortdash longdash_dot) lcolor(blue green black red purple) msymbol(none none none none none none none) lwidth(thick thick thick thick thick thick thick) xlabel(  1980 1990 2000 2010) xtitle("") legend(ring(0) pos(11)) 
graph export Fig11.pdf, replace
scatter SB1 SB_F1 SB_A1 A1 PR_1_AGG year if year>1979, c(l l l l l) lpattern(solid dash dot shortdash longdash_dot) lcolor(black black black black black) msymbol(none none none none none none none) lwidth(thick thick thick thick thick thick thick) xlabel(  1980 1990 2000 2010) xtitle("") legend(ring(0) pos(11)) 
graph export BW/Fig11.pdf, replace
restore
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 12: Cost-share based aggregated markups and technology (a) and (b)
sort year
preserve
drop if year==year[_n-1]
scatter MARKUP3_AGG year, c(l) lcolor(red ) lpattern(solid) symbol(none) lwidth(thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(5) ) 
graph export Fig12a.pdf, replace
scatter MARKUP3_AGG year, c(l) lcolor(black ) lpattern(solid) symbol(none) lwidth(thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(5) ) 
graph export BW/Fig12a.pdf, replace
label var thetaW1_c "Elasticity V
label var COSTSHARE1 "Cost Share V
scatter thetaW1_c COSTSHARE1 year, c(l l )  lcolor(red red) lpattern(solid dash ) msymbol( none none ) lwidth(thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0) pos(8) col(1)) 
graph export Fig12b.pdf, replace
scatter thetaW1_c COSTSHARE1 year, c(l l )  lcolor(black black) lpattern(solid dash ) msymbol( none none ) lwidth(thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0) pos(8) col(1)) 
graph export BW/Fig12b.pdf, replace
restore
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 13: Average Markups, elasticities and cost shares from PF2 (blue)
sort year
preserve
drop if year==year[_n-1]
scatter MARKUP_spec2 year , c(l) lcolor(blue ) lpattern(solid) symbol(none) lwidth(thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(5) ) 
graph export Fig13a.pdf, replace
scatter MARKUP_spec2 year , c(l) lcolor(black ) lpattern(solid) symbol(none) lwidth(thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(5) ) 
graph export BW/Fig13a.pdf, replace
label var thetaW2_c "Elasticity V
label var COSTSHARE2 "Cost Share V
label var thetaW2_x "Elasticity X
label var COSTSHARE3 "Cost Share X
scatter thetaW2_c thetaW2_x  COSTSHARE2 COSTSHARE3	year, c(l l l l )  lcolor(blue green blue green) lpattern(solid dash dot shortdash) msymbol(none none none none) lwidth(thick thick thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0) pos(5) col(2)) 
graph export Fig13b.pdf, replace
scatter thetaW2_c thetaW2_x  COSTSHARE2 COSTSHARE3	year, c(l l l l )  lcolor(black black black black) lpattern(solid dash dot shortdash) msymbol(none none none none) lwidth(thick thick thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0) pos(5) col(2)) 
graph export BW/Fig13b.pdf, replace
restore
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 14: Returns to scale 
preserve
sort year
drop if year==year[_n-1]
label var RTS_W1 "PF1
label var RTS_W2 "PF2
scatter RTS_W1 RTS_W2 year  if year>1970, c(l l) lcolor(red blue) ysc(r(.95 1.2)) lpattern(solid dash) yline(1) msymbol(none none) lwidth(thick thick) ytitle("") xlabel( 1970 1980 1990 2000 2010) xtitle("") legend(ring(0) col(1) pos(11) ) 
graph export Fig14a.pdf, replace
scatter RTS_W1 RTS_W2 year  if year>1970, c(l l) lcolor(black black) ysc(r(.95 1.2)) lpattern(solid dash) yline(1) msymbol(none none) lwidth(thick thick) ytitle("") xlabel( 1970 1980 1990 2000 2010) xtitle("") legend(ring(0) col(1) pos(11) ) 
graph export BW/Fig14a.pdf, replace
label var gamma_RTS1 "CS (firm)
label var gamma_RTS2 "CS (industry mean)
scatter gamma_RTS1 gamma_RTS2 year , c(l l) lcolor(green green) ysc(r(.95 1.1)) lpattern(solid dash) yline(1) msymbol(none none) lwidth(thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0) col(1) pos(11) ) 
graph export Fig14b.pdf, replace
scatter gamma_RTS1 gamma_RTS2 year , c(l l) lcolor(black black) ysc(r(.95 1.1)) lpattern(solid dash) yline(1) msymbol(none none) lwidth(thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0) col(1) pos(11) ) 
graph export BW/Fig14b.pdf, replace
restore
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 15: Markups and RTS (PF2)
preserve
sort year
drop if year==year[_n-1]
scatter MARKUP6_AGG year, c(l) lcolor(blue ) lpattern(solid) symbol(none) lwidth(thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(5) ) 
graph export Fig15a.pdf, replace
scatter MARKUP6_AGG year, c(l) lcolor(black ) lpattern(solid) symbol(none) lwidth(thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(5) ) 
graph export BW/Fig15a.pdf, replace
forvalues s=1/2 {
gen MARKUP6_AGG_g`s'_AGG = MARKUP6_AGG*gamma_RTS`s' 
}
label var MARKUP_spec2 "PF2
label var MARKUP6_AGG_g1 "CS Returns to Scale
scatter MARKUP_spec2 MARKUP6_AGG_g1 year, c(l l) lcolor(blue blue) lpattern(solid dash) symbol(none none) lwidth(thick thick) ylabel() xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(11) ) 
graph export Fig15b.pdf, replace 
scatter MARKUP_spec2 MARKUP6_AGG_g1 year, c(l l) lcolor(black black) lpattern(solid dash) symbol(none none) lwidth(thick thick) ylabel() xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(11) ) 
graph export BW/Fig15b.pdf, replace 
restore
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 16: Markups with input weights
preserve
sort year
drop if year==year[_n-1]
scatter MARKUP_spec1 MARKUP1_AGG_w4  year, c(l l l) lcolor(red green)  lpattern(solid dash) msymbol(none none) lwidth(thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(11) ) 
graph export Fig16a.pdf, replace
scatter MARKUP_spec1 MARKUP1_AGG_w4  year, c(l l l) lcolor(black black)  lpattern(solid dash) msymbol(none none) lwidth(thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(11) ) 
graph export BW/Fig16a.pdf, replace
scatter MARKUP_spec2 MARKUP11_AGG_w4 year, c(l l) lcolor(red green) lpattern(solid dash) msymbol(none none) lwidth(thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(11) ) 
graph export Fig16b.pdf, replace
scatter MARKUP_spec2 MARKUP11_AGG_w4 year, c(l l) lcolor(black black) lpattern(solid dash) msymbol(none none) lwidth(thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(11) ) 
graph export BW/Fig16b.pdf, replace
scatter  MARKUP_spec1 MARKUP10_AGG_w3 year, c(l l) lcolor(red green) lpattern(solid dash) msymbol(none none) lwidth(thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(11) ) 
graph export Fig16c.pdf, replace
scatter  MARKUP_spec1 MARKUP10_AGG_w3 year, c(l l) lcolor(black black) lpattern(solid dash) msymbol(none none) lwidth(thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(11) ) 
graph export BW/Fig16c.pdf, replace
scatter MARKUP_spec2 MARKUP11_AGG_w3 year, c(l l) lcolor(blue green) lpattern(solid dash) msymbol(none none) lwidth(thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(11) ) 
graph export Fig16d.pdf, replace
scatter MARKUP_spec2 MARKUP11_AGG_w3 year, c(l l) lcolor(black black) lpattern(solid dash) msymbol(none none) lwidth(thick thick) ytitle("") xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring(0)  pos(11) ) 
graph export BW/Fig16d.pdf, replace
restore
*---------------------------------------------------------------------------------------------------------------------------*
* Fig 17: Decomposition with input weights
preserve
* cutt-off year
keep if year>1979 & year<=2016
drop if ind2d <9 
drop if ind2d>99
bysort year: egen tcogs = sum(cogs_D)
gen msagg = cogs_D/tcogs
xtset id year, yearly
foreach x of varlist mu_spec1 {
bysort year: egen MU_agg 		= sum(msagg*`x')
xtset id year, yearly
gen d`x' 	= D.`x'
gen Lms 	= L.msagg
gen demean`x'= `x'-MU_agg
gen L`x' 	= L.demean`x'
gen Lmsagg	= L.msagg
gen dmsagg  = D.msagg
bysort year: egen Dagg`x' 		= sum(d`x'*Lmsagg)
bysort year: egen Daggms`x'		= sum(dmsagg*L`x')
bysort year: egen Cross_agg`x' 	= sum(dmsagg*d`x')
bysort ind2d year: egen D`x' 	= sum(d`x'*Lms)
bysort ind2d year: egen Dms`x' 	= sum(dms*L`x')
bysort ind2d year: egen Cross`x' 	= sum(dms*d`x')
}
* aggregate 
sort year
drop if year==year[_n-1]
gen DMU_agg = MU_agg -MU_agg[_n-1]
gen net_entry = DMU_agg - Daggmu_spec - Daggms -Cross_agg
gen REALL = net_entry + Daggms +Cross_agg
gen reall_inc = Daggms + Cross_agg
*initialise at 1980 so keep from 1980
gen Dagg_sum 	= sum(Daggmu)
gen Daggms_sum 	= sum(Daggms)
gen Cross_sum		= sum(Cross_agg)
gen Net_sum		= sum(net_entry)
gen Reall_sum	= sum(REALL)	
gen reall_inc_s	= sum(reall_inc)
gen a	=	1 if year==1980
egen a_m = mean(MU_agg) if a==1
egen a_mu = mean(a_m)
foreach x of varlist Dagg_sum Daggms_s Cross_s Net_s Reall_s reall_inc_s {
replace `x' = `x'+a_mu
}
label var MU_agg "Markup (COGS weighted)
label var Dagg_sum "Within 
label var Net_s "Net Entry 
label var reall_inc_s "Reallocation
scatter MU_agg Dagg_sum  reall_inc_s Net_s   year , c(l l l l) lpattern(solid dash shortdash longdash_dot) lwidth(thick thick thick thick) lcolor(red blue black green) xlabel(1980 1990 2000 2010) msymbol(none none none none none) sort xtitle("") legend(ring(0)  pos(11) )  ylabel(1 1.1 1.2 1.3 1.4)
graph export Fig17a.pdf, replace
scatter MU_agg Dagg_sum  reall_inc_s Net_s   year , c(l l l l) lpattern(solid dash shortdash longdash_dot) lwidth(thick thick thick thick) lcolor(black black black black) xlabel(1980 1990 2000 2010) msymbol(none none none none none) sort xtitle("") legend(ring(0)  pos(11) )  ylabel(1 1.1 1.2 1.3 1.4)
graph export BW/Fig17a.pdf, replace
restore
* again for empl.
preserve
* cutt-off year
keep if year>1979 & year<=2016
drop if ind2d <9 
drop if ind2d>99
bysort year: egen ts = sum(sale_D)
bysort year: egen tl = sum(emp)
gen msagg = emp/tl
xtset id year, yearly
foreach x of varlist mu_spec1 {
bysort year: egen MU_agg 		= sum(msagg*`x')
xtset id year, yearly
gen d`x' 	= D.`x'
gen Lms 	= L.msagg
gen demean`x'= `x'-MU_agg
gen L`x' 	= L.demean`x'
gen Lmsagg	= L.msagg
gen dmsagg  = D.msagg
bysort year: egen Dagg`x' 		= sum(d`x'*Lmsagg)
bysort year: egen Daggms`x'		= sum(dmsagg*L`x')
bysort year: egen Cross_agg`x' 	= sum(dmsagg*d`x')
bysort ind2d year: egen D`x' 	= sum(d`x'*Lms)
bysort ind2d year: egen Dms`x' 	= sum(dms*L`x')
bysort ind2d year: egen Cross`x' 	= sum(dms*d`x')
}
* aggregate 
sort year
drop if year==year[_n-1]
gen DMU_agg = MU_agg -MU_agg[_n-1]
gen net_entry = DMU_agg - Daggmu_spec - Daggms -Cross_agg
gen REALL = net_entry + Daggms +Cross_agg
gen reall_inc = Daggms + Cross_agg
*initialise at 1980 so keep from 1980
gen Dagg_sum 	= sum(Daggmu)
gen Daggms_sum 	= sum(Daggms)
gen Cross_sum		= sum(Cross_agg)
gen Net_sum		= sum(net_entry)
gen Reall_sum	= sum(REALL)	
gen reall_inc_s	= sum(reall_inc)
gen a	=	1 if year==1980
egen a_m = mean(MU_agg) if a==1
egen a_mu = mean(a_m)
foreach x of varlist Dagg_sum Daggms_s Cross_s Net_s Reall_s reall_inc_s {
replace `x' = `x'+a_mu
}
label var MU_agg "Markup (employment weight)
label var Dagg_sum "Within 
label var Net_s "Net Entry 
label var reall_inc_s "Reallocation
scatter MU_agg Dagg_sum  reall_inc_s Net_s   year , c(l l l l) lwidth(thick thick thick thick) lpattern(solid dash shortdash longdash_dot)  lcolor(red blue black green) xlabel(1980 1990 2000 2010) msymbol(none none none none none) sort xtitle("") legend(ring(0)  pos(11) ) 
graph export Fig17b.pdf, replace
scatter MU_agg Dagg_sum  reall_inc_s Net_s   year , c(l l l l) lwidth(thick thick thick thick) lpattern(solid dash shortdash longdash_dot)  lcolor(black black black black) xlabel(1980 1990 2000 2010) msymbol(none none none none none) sort xtitle("") legend(ring(0)  pos(11) ) 
graph export BW/Fig17b.pdf, replace
restore
*-------------------------------------------------------------------------------------------------------------------------*
* APPENDIX
cd "$dropbox"
use "temp/temp_file.dta", clear
cd "output/figures/"
*-------------------------------------------------------------------------------------------------------------------------*
* Appendix A
* Fig A.1 Output elasticities:
xtset id year, yearly
gen lns = ln(sale_D)
gen v = ln(cogs_D)
gen lnk = ln(capital_D)
gen Investment = capital_D-.9*L.capital_D
gen lni = ln(Investment)
gen lnk2 = lnk^2
gen lni2 = lni^2
gen lnk3 = lnk^3
gen lni3 = lni^3
gen lnki = lnk*lni
gen theta_op 	= .
egen nrind = group(ind2d)
forvalues t=1972/2016 {
forvalue  s = 1/23 {
reg lns v lni lni2 lni3 lnk lnk2 lnk3 lnki share_firm share_ind_4 if year==`t' & nrind==`s'
replace theta_op = _b[v] if year==`t' & nrind==`s'
}
}	
* before 1972 pool // nr obs with investment limited in certain sectors (alt. rolling windows)
forvalue  s = 1/23 {
reg lns v lni lni2 lni3 lnk lnk2 lnk3 lnki share_firm share_ind_4 if year<1972 & nrind==`s'
replace theta_op = _b[v] if year<1972 & nrind==`s'
}

gen mu_op = theta_op*sale_D/cogs_D
bysort year: egen Mop = sum(share_firm_agg*mu_op)
bysort year: egen Theta_OP_st = sum(share_firm_agg*theta_op)
bysort year: egen Theta_ACF_St = sum(share_firm_agg*theta_WI1_ct)
preserve
sort year
drop if year==year[_n-1]
label var Theta_OP_st "OP
label var Theta_ACF_St "ACF
scatter Theta_OP_st  Theta_ACF_St year if year>1959, c(l l) lcolor(red red) lpattern(solid dash) msymbol(none none) legend(pos(6) col(2)) lwidth(thick thick) xtitle("") ytitle("") xlabel(1960 1970 1980 1990 2000 2010) legend(ring(0) col(1) pos(7) ) 
graph export FigA1.pdf, replace
scatter Theta_OP_st  Theta_ACF_St year if year>1959, c(l l) lcolor(black black) lpattern(solid dash) msymbol(none none) legend(pos(6) col(2)) lwidth(thick thick) xtitle("") ytitle("") xlabel(1960 1970 1980 1990 2000 2010) legend(ring(0) col(1) pos(7) ) 
graph export BW/FigA1.pdf, replace
restore
*-------------------------------------------------------------------------------------------------------------------------*
* Appendix C
* Fig C.1 Distribution and Kernel under PF2
preserve
drop if mu_2<0.7
drop if mu_2> 1.7
twoway (kdensity mu_2 if (year==2016 ) , kernel(gaussian) xlabel(.8 1 1.2 1.4 1.6) lcolor(blue) lwidth(thick)) (kdensity mu_2 if (year==1980 ),  kernel(gaussian)  lcolor(blue) ytitle("") xtitle("") clpattern(dash) lwidth(thick) graphregion(color(white) )  legend(ring(0)  col(1) pos(2) label(1 "2016") label(2 "1980")  ) )
graph export FigC1a.pdf, replace
twoway (kdensity mu_2 if (year==2016 ) , kernel(gaussian) xlabel(.8 1 1.2 1.4 1.6) lcolor(black) lwidth(thick)) (kdensity mu_2 if (year==1980 ),  kernel(gaussian)  lcolor(black) ytitle("") xtitle("") clpattern(dash) lwidth(thick) graphregion(color(white) )  legend(ring(0)  col(1) pos(2) label(1 "2016") label(2 "1980")  ) )
graph export BW/FigC1a.pdf, replace
sort year
drop if year==year[_n-1]
label var mu_2_ms90 "P90
label var mu_2_ms50 "P50
label var mu_2_ms75 "P75
label var MARKUP11_AGG "Average
scatter  MARKUP11_AGG mu_2_ms90 mu_2_ms75 mu_2_ms50  year , connect(l l l l )  msymbol(none none none none ) color(blue blue blue blue) lpattern(solid dash shortdash longdash_dot)  lwidth(thick thick thick thick) xtitle("") xlabel(1960 1970 1980 1990 2000 2010) legend(ring(0)  pos(10) )  sort
graph export FigC1b.pdf, replace
scatter  MARKUP11_AGG mu_2_ms90 mu_2_ms75 mu_2_ms50  year , connect(l l l l )  msymbol(none none none none ) color(black black black black) lpattern(solid dash shortdash longdash_dot)  lwidth(thick thick thick thick) xtitle("") xlabel(1960 1970 1980 1990 2000 2010) legend(ring(0)  pos(10) )  sort
graph export BW/FigC1b.pdf, replace
restore
*-------------------------------------------------------------------------------------------------------------------------*
* Appendix D
* Fig D.1 Industry specific average markups using labor cost / Leontief using L
* Estimate Leontief production a la De Loecker-Scott 	- by sector - by year
preserve
gen k	=	ln(capital_D)
gen k2	=	k^2
xtset id year, yearly
gen K			= exp(k)
gen INV 		= K - (1-.1)*L.K
gen i			= ln(INV)
gen i2			= i^2
gen lnXLR 		= ln(xlr_D)
* subsample with wage data
drop if xlr_D==.
drop if xlr_D<0
drop if emp==0
gen M = cogs_D-xlr_D
gen m = ln(M)
gen M_s = M/sale_D
gen sga_l = xsga_D - xlr_D
gen lsga_xlr = ln(sga_l)
drop if M_s>1 & M_s<0
xtset id year, yearly
gen ind2d_fix = ind2d
replace ind2d_fix = 48 if ind2d==49 
* transportation and warehousing put together, too few numbers in ind 49
drop nrind*
* make sure all previous code on nrind is gone
egen nrind2 = group(ind2d_fix)
gen th_l_s = . 
forvalues s = 1/22 {
ivregress gmm y (lnXLR = L.lnXLR)  k L.k  L.k2 L.i L.i2  if `s'==nrind2
replace th_l_s = _b[lnXLR] if `s'==nrind2
}
gen th_l_t = . 
forvalues t = 1960/2016 {
ivregress gmm y (lnXLR = L.lnXLR)  k L.k  L.k2 L.i L.i2  i.ind2d if year==`t'
replace th_l_t = _b[lnXLR] if year==`t'
}
* Cobb-Douglas (L, M, K)
gen th_l_cd = .
forvalues t=1960/2016 {
ivregress gmm y (lnXLR m = L.lnXLR L.m)  k lsga_xlr L.k L.lsga_xlr L.k2 L.i L.i2  if year==`t'
replace th_l_cd = _b[lnXLR] if year==`t'
}
gen muls = th_l_s*(sale_D/xlr_D)
gen mus_lab = 1/ ((1/muls)+M_s)
gen mult = th_l_t*(sale_D/xlr_D)
gen mut_lab = 1/ ((1/mult)+M_s)
gen mu_l_cd = th_l_cd*(sale_D/xlr_D)
keep if mu_l_cd<20
keep if mut_lab<20
keep if mus_lab<20
bysort year: egen ts = sum(sale_D)
gen mssub = sale_D/ ts
bysort year: egen MAGG_LABs = sum(mssub*mus_lab)
bysort year: egen MAGG_LABt = sum(mssub*mut_lab)
bysort year: egen MAGG_spec1 = sum(mssub*mu_spec1)
bysort year: egen MARKUP_AGG_L_CD = sum(mssub*mu_l_cd) 
sort year
drop if year==year[_n-1]
keep if year>1959
label var MAGG_LABs "Leontief (sector specific)
label var MAGG_LABt "Leontief (time varying)
label var MAGG_spec1 "Baseline (PF1)
scatter MAGG_LABs MAGG_LABt MAGG_spec1 year, c(l l l  l) lcolor(green green red) msymbol( none none none none) lwidth(thick thick thick) lpattern(dot dash solid) xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring (0) pos(10))
graph export FigD1.pdf, replace
scatter MAGG_LABs MAGG_LABt MAGG_spec1 year, c(l l l  l) lcolor(black black black) msymbol( none none none none) lwidth(thick thick thick) lpattern(dot dash solid) xlabel(1960 1970 1980 1990 2000 2010) xtitle("") legend(ring (0) pos(10))
graph export BW/FigD1.pdf, replace
restore
*-------------------------------------------------------------------------------------------------------------------------*
* Appendix E
* Fig E.1 Evolution of st. dev of markup, sales and empl.
preserve
label var sd_mu_ "sd markup
label var sd_s "sd sales
label var sd_l "sd employment
sort year
drop if year==year[_n-1]
scatter sd_mu_ sd_s sd_l year, c( l l l ) lcolor(red green green) lpattern(solid dash shortdash) msymbol(none none none) lwidth(thick thick thick) xtitle("") xlabel(1960 1970 1980 1990 2000 2010) legend(ring(0)  pos(10) )
graph export FigE1.pdf, replace
scatter sd_mu_ sd_s sd_l year, c( l l l ) lcolor(black black black) lpattern(solid dash shortdash) msymbol(none none none) lwidth(thick thick thick) xtitle("") xlabel(1960 1970 1980 1990 2000 2010) legend(ring(0)  pos(10) )
graph export BW/FigE1.pdf, replace
restore
*-------------------------------------------------------------------------------------------------------------------------*
* Appendix F 
* Fig F.1 Markups vs Profit Rate
preserve
gen s_pi = (sale_D-cogs_D-xsga_D-kexp)/sale_D
gen s_pik = (sale_D-cogs_D-xsga_D)/sale_D
gen markup_T = .95*(sale_D/(cogs_D+xsga_D))
bysort year: egen M_T = sum(share_firm*markup_T)
bysort year: egen PIrate	= sum(share_firm*(s_pi))
bysort year: egen PIrate_k 	= sum(share_firm*s_pik)
sort year
drop if year==year[_n-1]
label var M_T "tau
label var PIrate "profit rate
label var PIrate_k "profit rate (without Capital)
twoway (scatter M_T  year, yaxis(1) c(l ) lcolor(green) lpattern(dash) lwidth(thick thick) msymbol(none)) || (scatter PIrate year,c(l) lwidth(thick thick) lpattern(solid) xlabel( 1960 1970 1980 1990 2000 2010) xtitle("") lcolor(red) msymbol(none) yaxis(2)) , legend(ring(0)  pos(5)  )
graph export FigF1a.pdf, replace
twoway (scatter M_T  year, yaxis(1) c(l ) lcolor(black) lpattern(dash) lwidth(thick thick) msymbol(none)) || (scatter PIrate year,c(l) lwidth(thick thick) lpattern(solid) xlabel( 1960 1970 1980 1990 2000 2010) xtitle("") lcolor(black) msymbol(none) yaxis(2)) , legend(ring(0)  pos(5)  )
graph export BW/FigF1a.pdf, replace
twoway (scatter M_T  year, yaxis(1) c(l ) lcolor(green) lpattern(dash) lwidth(thick thick) msymbol(none)) || (scatter PIrate_k year,c(l) lwidth(thick thick) xlabel( 1960 1970 1980 1990 2000 2010) xtitle("") lcolor(blue) msymbol(none) yaxis(2)) , legend(ring(0)  pos(5)  )
graph export FigF1b.pdf, replace
twoway (scatter M_T  year, yaxis(1) c(l ) lcolor(black) lpattern(dash) lwidth(thick thick) msymbol(none)) || (scatter PIrate_k year,c(l) lwidth(thick thick) xlabel( 1960 1970 1980 1990 2000 2010) xtitle("") lcolor(black) msymbol(none) yaxis(2)) , legend(ring(0)  pos(5)  )
graph export BW/FigF1b.pdf, replace
restore
*-------------------------------------------------------------------------------------------------------------------------*
* create Computstat output for census-merge
cd "$dropbox"
use "temp/temp_file", clear
gen sector =1 if ind2d>30 & ind2d <34
* manuf
replace sector = 2 if ind2d==42
* wholesale
replace sector = 3 if ind2d==44 | ind2d==45
*retail
replace sector = 4 if ind2d==52 | ind2d==53
* FIRE
replace sector = 5 if ind2d==51 | ind2d > 53
* services
replace sector = 6 if ind2d==11
* agr
replace sector = 7 if ind2d==21
* mining
replace sector = 8 if ind2d==22
* utilities 


forvalues s = 1/8 {
keep if mu_spec1~=.
preserve
keep if sector==`s' 
bysort year: egen ts = sum(sale_D)
gen ms = sale_D/ts
bysort year: egen MU_sector = sum(ms*mu_spec1)
bysort year: egen tcogs = sum(cogs_D)
gen MUagg_sector = .85* ts/tcogs
sort year
drop if year==year[_n-1]
keep year MU_sector MUagg_sector ind2d
sort year
save "Compustat_`s'.dta", replace
restore
}
use Compustat_1, clear
gen sector="manufacturing"
sort year
save, replace
use Compustat_2, clear
gen sector="wholesale"
sort year
save, replace
use Compustat_3, clear
gen sector="retail"
sort year
save, replace
use Compustat_1, clear
append using Compustat_2
append using Compustat_3
sort year sector
save "data/Compustat_out.dta", replace
forvalues s=1/8 {
erase "Compustat_`s'.dta"
}
*-------------------------------------------------------------------------------------------------------------------------*
use "data/census_final.dta", clear
cd "output/figures/"
* Main paper Figure 6 Aggregate Markup
*1. manufacturing
scatter Mw year , sort c(l) lcolor(red) msymbol(none) lwidth(thick) xtitle("") ytitle("") xlabel(1972 1977 1982 1987 1992 1997 2002 2007 2012) 
graph export Fig6a.pdf, replace
scatter Mw year , sort c(l) lcolor(black) msymbol(none) lwidth(thick) xtitle("") ytitle("") xlabel(1972 1977 1982 1987 1992 1997 2002 2007 2012) 
graph export BW/Fig6a.pdf, replace
scatter Mw90 Mw75 Mw50 year, sort c(l l l) lcolor(red red red) lpattern(dash dot dashdot) msymbol(none none none) lpattern( dash shortdash longdash_dot) lwidth(thick thick thick) xtitle("")  xlabel(1972 1977 1982 1987 1992 1997 2002 2007 2012) legend(ring(0)  pos(10) ) 
graph export Fig6b.pdf, replace
scatter Mw90 Mw75 Mw50 year, sort c(l l l) lcolor(black black black) lpattern(dash dot dashdot) msymbol(none none none) lpattern( dash shortdash longdash_dot) lwidth(thick thick thick) xtitle("")  xlabel(1972 1977 1982 1987 1992 1997 2002 2007 2012) legend(ring(0)  pos(10) ) 
graph export BW/Fig6b.pdf, replace
*2. retail
scatter Rw year , sort c(l) lcolor(red) msymbol(none)  lwidth(thick) xtitle("") ytitle("") xlabel(1972 1977 1982 1987 1992 1997 2002 2007 2012) 
graph export Fig6c.pdf, replace
scatter Rw year , sort c(l) lcolor(black) msymbol(none)  lwidth(thick) xtitle("") ytitle("") xlabel(1972 1977 1982 1987 1992 1997 2002 2007 2012) 
graph export BW/Fig6c.pdf, replace
scatter Rw90 Rw75 Rw50 year , sort c(l l l) lcolor(red red red) lpattern(dash dot dashdot) msymbol(none none none) lpattern( dash shortdash longdash_dot) lwidth(thick thick thick) xtitle("")  xlabel(1972 1977 1982 1987 1992 1997 2002 2007 2012) legend(ring(0)  pos(10) ) 
graph export Fig6d.pdf, replace
scatter Rw90 Rw75 Rw50 year , sort c(l l l) lcolor(black black black) lpattern(dash dot dashdot) msymbol(none none none) lpattern( dash shortdash longdash_dot) lwidth(thick thick thick) xtitle("")  xlabel(1972 1977 1982 1987 1992 1997 2002 2007 2012) legend(ring(0)  pos(10) ) 
graph export BW/Fig6d.pdf, replace
*3. wholesale
scatter Ww year , sort c(l) lcolor(red) msymbol(none)  lwidth(thick) xtitle("") ytitle("") xlabel(1972 1977 1982 1987 1992 1997 2002 2007 2012) 
graph export Fig6e.pdf, replace
scatter Ww year , sort c(l) lcolor(black) msymbol(none)  lwidth(thick) xtitle("") ytitle("") xlabel(1972 1977 1982 1987 1992 1997 2002 2007 2012) 
graph export BW/Fig6e.pdf, replace
scatter Ww90 Ww75 Ww50 year , sort c(l l l) lcolor(red red red) lpattern(dash dot dashdot) msymbol(none none none) lpattern( dash shortdash longdash_dot) lwidth(thick thick thick) xtitle("")  xlabel(1972 1977 1982 1987 1992 1997 2002 2007 2012) legend(ring(0)  pos(10) ) 
graph export Fig6f.pdf, replace
scatter Ww90 Ww75 Ww50 year , sort c(l l l) lcolor(black black black) lpattern(dash dot dashdot) msymbol(none none none) lpattern( dash shortdash longdash_dot) lwidth(thick thick thick) xtitle("")  xlabel(1972 1977 1982 1987 1992 1997 2002 2007 2012) legend(ring(0)  pos(10) ) 
graph export BW/Fig6f.pdf, replace
*
*-------------------------------------------------------------------------------------------------------------------------*
// }
*-------------------------------------------------------------------------------------------------------------------------*
/* Output created: Main text and Appendix
1. / Tables
2. / Figures
*-------------------------------------------------------------------------------------------------------------------------*
