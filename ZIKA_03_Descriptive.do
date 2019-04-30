********************************************************************************
* Zika and fertiliy in Colombia ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* July 2017
* @authors: Luis F. Gamboa and Paul Rodriguez
* @description: quarterly analysis of areas with and without Zika incidence
*               This file is JUST for getting an idea, but does not consider
*               "selection" into Zika
* @ Requires:   municipalityZikaMonth.dta, birthRegMuni.dta
* Last update: 
*	@WHO:  Paul Rodríguez
*	@WHEN: 2018.02.19
*	@WHAT: Added 2016 final birth-alive info
********************************************************************************

set more off

clear all
set matsize 1000
set maxvar 4100

glo mainFolder="C:\Users\andro\Dropbox\Salud Colombia\ZIka-embarazo\share"

glo tables = "$mainFolder/outcomes/tables/"
glo images = "$mainFolder/outcomes/images/" 

glo genOpts="  graphregion(color(white) lwidth(medium)) "


////////////////////////////////////////////////////////////////////////////////
// Descriptives: histogram and data for map
////////////////////////////////////////////////////////////////////////////////
if 1==1 {

	use "$mainFolder/data/mainData/municipalityZikaMonth.dta", clear
	drop if dm==.
	drop if year<2008     // No births data prior to this date
	drop if dm>m(2016m12) // No data after it

	merge 1:m codigomunicipio dm using "$mainFolder\data\mainData\birthRegMuni.dta", nogen
	
	preserve
		keep if year==2016
		collapse (sum) zika (mean) poblacintotal altitud , by(codigomunicipio codigodepartamento)

		gen zikaEv1516100000h= zika/ (poblacintotal/100000)
		label var zikaEv1516100000h  "Cumulative Zika cases 2016 per 100.000h"
		
		*******
		hist zikaEv1516100000h if zikaEv1516100000h>0 & zikaEv1516100000h<200 , scheme(plotplainblind) caption(Municipalities with at least 1 case of Zika) ///
			xline(15 110)
		
		*******
		tw 	(lpoly zikaEv1516100000h altitud , lwidth(thick) ) ///
			if altitud<3000, ///
			ytitle(Zika cases 2016 per 100.000h) xline(1800 , lwidth(thick)) xtitle(Meters above sea level) $genOpts scheme(plottig) name(a1, replace)
			
		graph export "$mainFolder/outcomes/images/descrip_altitude.png", as(png) replace		

		*******
		export delimited using "$mainFolder\outcomes\mapa\zikaYearly.csv", replace
	restore
	
}



////////////////////////////////////////////////////////////////////////////////
// Descriptives: ZIKV and Google trends by altitude
////////////////////////////////////////////////////////////////////////////////
if 1==1 {

use "$mainFolder/data/mainData/municipalityZikaMonth.dta", clear
drop if dm==.
drop if year<2008     // No births data prior to this date
drop if dm>m(2016m12) // No data after it

*gen logpop=log(poblacintotal)


gen below1800= altitud<1800 if altitud!=.
label var below1800 "Located below 1800 masl"

gen urb=poblacioncabecera/poblacintotal

tab NBIcat, gen(dNBI_)

	collapse (mean) Zikap100000h zikagoogle /// // Abortop100000h Atenciop100000h
			 , by(dm year month below1800)

	twoway 	(line Zikap100000h dm if below1800==1, lwidth(thick)  ) ///
			(line Zikap100000h dm if below1800==0, lwidth(thick) lpattern(dash) ) ///
			(line zikagoogle dm , lwidth(thick) yaxis(2) ) ///
			if year>=2015 , scheme(plottig) ytitle("New cases per 100.000 inhabitants") ytitle("Google search index" , axis(2)) ///
			xtitle("") xline(665 670 , lwidth(thick)) ///
				plotregion(fcolor(gs16) ifcolor(gs15)) xlabel(660(4)681  , angle(forty_five)) ///
			legend(order(1 "Mean ZIKV incidence, below 100 masl" 2 "Mean ZIKV incidence, above 1800 masl" 3 "Google search index") position(6) cols(2)) ///
			caption("Línea Vertical:" "June 2015: Ministry of health alerts of the arrival of ZIKV to Colombia" ///
									  "November 2015: first news regarding the potential link between ZIKV and microcephaly") $genOpts

	graph export "$mainFolder/outcomes\images\zikaTrends.png", as(png) replace
	

}

////////////////////////////////////////////////////////////////////////////////
use "$mainFolder/data/mainData/municipalityZikaMonth.dta", clear
drop if dm==.
drop if year<2008     // No births data prior to this date
drop if dm>m(2016m12) // No data after it
merge 1:m codigomunicipio dm using "$mainFolder/data/mainData/birthRegMuni.dta", nogen


gen urb=poblacioncabecera/poblacintotal

tab NBIcat, gen(dNBI_)

gen qtr=qofd(dofm(dm))
gen Q=quarter(dofm(dm))
collapse (sum) births chinkungunya zika dengueClas (mean) rainfall discap capital dismer road_densi aguam  poblacintotal urb NBI dNBI_2 dNBI_3 dNBI_4 dNBI_5 poblacintotalm acueductoPer numconsul apgar2 altitud year , by(qtr Q codigomunicipio)
format qtr %tq


////////////////////////////////////////////////////////////////////////////////

gen logpop=log(poblacintotal)

gen births1000h= births/ (poblacintotal/1000)
label var births1000h "Total births per 1.000h, Quarter"

gen below1800= altitud<1800 if altitud!=.
label var below1800 "Located below 1800 masl"

bys year codigomunicipio : egen yChin = total(chinkungunya)
replace yChin=0 if year!=2015
bys codigomunicipio : egen yChinpop = max(yChin)
drop yChin
replace yChinpop= yChinpop/ (poblacintotal/100000)
label var yChinpop  "Cumulative Chinkun cases 2015 per 100.000h"

bys year codigomunicipio : egen yDen = total(dengueClas)
replace yDen=0 if year!=2014
bys codigomunicipio : egen yDenpop = max(yDen)
drop yDen
replace yDenpop= yDenpop/ (poblacintotal/100000)
label var yDenpop  "Cumulative Dengue cases 2014 per 100.000h"


bys year codigomunicipio : egen yChin = total(zika)
replace yChin=0 if year!=2016
bys codigomunicipio : egen yZikapop = max(yChin)
drop yChin
gen zikaEv1516100000h= yZikapop/ (poblacintotal/100000)
label var zikaEv1516100000h  "Cumulative Zika cases 2016 per 100.000h"

* anual rates differences
xtset codigomunicipio qtr
gen numconsulRyeard= numconsul-L4.numconsul
gen apgar2Ryeard= apgar2-L4.apgar2
gen birthsRyeard= births1000h-L4.births1000h

sum altitud if qtr==tq(2016q1), d // 1SD917 meters!
gen altitudStd=(altitud-1800)/r(sd)


////////////////////////////////////////////////////////////////////////////////
// Descriptives: Altitude counter
////////////////////////////////////////////////////////////////////////////////
cap drop catG
glo Cat =3
gen     catG=0 if zikaEv1516100000h==0
replace catG=1 if zikaEv1516100000h>0 & zikaEv1516100000h<=25
replace catG=2 if zikaEv1516100000h>25 & zikaEv1516100000h<=100
replace catG=$Cat if zikaEv1516100000h>100 & zikaEv1516100000h!=.
tab catG if qtr==tq(2015q4) & catG>0 & altitud<1800

tab catG if qtr==tq(2015q4) & altitud>=1800 // 179 municipalities


	gen ZikaHist = zikaEv1516100000h>0 if zikaEv1516100000h!=.

////////////////////////////////////////////////////////////////////////////////
// 1. Compare municipalities above and below 1800 masl, quarterly

if 1==1 {

	preserve
	collapse (mean)  births1000h birthsRyeard  ///
			, by(qtr year below1800)

	loc l1=tq(2016q1)
	twoway 	(line births1000h qtr if below1800==0 , lwidth(thick)  ) ///
			(line births1000h qtr if below1800==1 , lwidth(thick) lpattern(dash) ) ///
			if year>2010 , ytitle("Birth rate ") xtitle("") ///
			plotregion(fcolor(gs16) ifcolor(gs15)) xlabel(206(4)226  , angle(forty_five)) ylabel(, format(%2.1f))	 ///
			xline( `l1', lwidth(thick) ) ///
			legend(order(1 "Above 1800 masl" 2 "Below 1800 masl"  )) scheme(plottig) name(a1, replace)		
			
	twoway 	(line birthsRyeard qtr if below1800==0 , lwidth(thick)  ) ///
			(line birthsRyeard qtr if below1800==1 , lwidth(thick) lpattern(dash) ) ///
			if year>2010 , ytitle("Birth rate yearly variation") xtitle("")  ///
			plotregion(fcolor(gs16) ifcolor(gs15)) xlabel(206(4)226  , angle(forty_five)) ylabel(, format(%2.1f))	 ///
			 xline( `l1', lwidth(thick) ) yline( 0 , lwidth(thick)) ///
			legend(order(1 "Above 1800 masl" 2 "Below 1800 masl"  )) scheme(plottig) name(a2, replace) 
			
	grc1leg a1 a2 , scheme(plottig) scale(1.5) xsize(7) ysize(4)
	graph export "$images/trendsALL_Altitude.png", as(png) replace
			
	restore

}

////////////////////////////////////////////////////////////////////////////////
// 2. Compare municipalities with and without Zika, quarterly

if 1==1 {

	preserve
	collapse (mean)  births1000h birthsRyeard  ///
			, by(qtr year ZikaHist)

	loc l1=tq(2016q1)
	twoway 	(line births1000h qtr if ZikaHist==0 , lwidth(thick)  ) ///
			(line births1000h qtr if ZikaHist==1 , lwidth(thick) lpattern(dash) ) ///
			if year>2010 , ytitle("Birth rate ") xtitle("") ///
			plotregion(fcolor(gs16) ifcolor(gs15)) xlabel(206(4)226  , angle(forty_five)) ylabel(, format(%2.1f))	 ///
			 xline( `l1', lwidth(thick) )  ///
			legend(order(1 "No Zika" 2 "Zika"  )) scheme(plottig) name(a1, replace) xline( `l1' )	ylabel(, format(%2.1f))		
			
	twoway 	(line birthsRyeard qtr if ZikaHist==0 , lwidth(thick)  ) ///
			(line birthsRyeard qtr if ZikaHist==1 , lwidth(thick) lpattern(dash) ) ///
			if year>2010 , ytitle("Birth rate yearly variation") xtitle("") ///
			plotregion(fcolor(gs16) ifcolor(gs15)) xlabel(206(4)226  , angle(forty_five)) ylabel(, format(%2.1f))	 ///
			 xline( `l1', lwidth(thick) ) yline( 0 , lwidth(thick)) ///
			legend(order(1 "No Zika" 2 "Zika"  )) scheme(plottig) name(a2, replace) xline( `l1' )	ylabel(, format(%2.1f))	
			
	grc1leg a1 a2 , scheme(plottig) scale(1.5) xsize(7) ysize(4)
	graph export "$images/trendsALL_ZikaStatus.png", as(png) replace
			
	restore

}


////////////////////////////////////////////////////////////////////////////////
// 3. Intensity analysis
if 1==1 {

	if 1==1 { // Graph *************************************************************
		preserve
		collapse (mean)  births1000h birthsRyeard  ///
				, by(qtr year catG)

		loc l1=tq(2016q1)
		twoway 	(line birthsRyeard qtr if catG==0 , lwidth(thick)  ) ///
				(line birthsRyeard qtr if catG==1 , lwidth(thick) lpattern(dash) ) ///
				(line birthsRyeard qtr if catG==2 , lwidth(thick) lpattern(dash) ) ///
				(line birthsRyeard qtr if catG==3 , lwidth(thick) lpattern(dash) ) ///
				if year>2010 , ytitle("Birth rate yearly variation") xtitle("") xline( `l1' )  ylabel(, format(%2.1f)) ///
				legend(order(1 "No Zika" ///
							 2 "Zika Low" ///
							 3 "Zika Medium"  ///
							 4 "Zika High" ///
					   )) $genOpts scheme(plottig)
		graph export "$images/trendsALL_Intensity.png", as(png) replace
		restore
	}

}
 
