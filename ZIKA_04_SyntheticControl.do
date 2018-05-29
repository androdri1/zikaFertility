********************************************************************************
* March 26 2018
* @authors: Luis F. Gamboa and Paul Rodriguez
* @description: quarterly analysis of areas with and without Zika incidence
*               uysing matching (psmatch2) THIS IS THE PAPER VERSION
********************************************************************************

set more off

clear all
set matsize 1000
set maxvar 4100

global mainFolder "D:/Mis Documentos/git/zikaFertility"

glo tables = "$mainFolder/outcomes/tables/"
glo images = "$mainFolder/outcomes/images" 

glo genOpts="  graphregion(color(white) lwidth(medium)) "

////////////////////////////////////////////////////////////////////////////////
use "$mainFolder/data/mainData/municipalityZikaMonth.dta", clear
drop if dm==.
drop if year<2008     // No births data prior to this date
drop if dm>m(2016m12) // No data after it

*gen logpop=log(poblacintotal)


gen below1800= altitud<1800 if altitud!=.
label var below1800 "Located below 1800 masl"

gen urb=poblacioncabecera/poblacintotal

tab NBIcat, gen(dNBI_)

////////////////////////////////////////////////////////////////////////////////
// Descriptives: ZIKV over time by altitude
////////////////////////////////////////////////////////////////////////////////
if 1==0 {
preserve
	collapse (sum)  zika dengueClas chinkungunya parto  /// // abortion
			 (mean) clasp100000h Zikap100000h Chinkp100000h Partop100000h  zikagoogle /// // Abortop100000h Atenciop100000h
			 , by(dm year month below1800)
	replace zika=. if year<2016 // There is no data prior to it (it seems! has to be confirmed)
	replace chinkungunya=. if dm<=mofd(dofw(weekly("2014w22","YW")))  // There is no data prior to it (it seems! has to be confirmed)
	replace parto=. if dm>mofd(dofw(weekly("2016w35","YW")))  // There is no data prior to it (it seems! has to be confirmed)
	*replace abortion=. if dm>mofd(dofw(weekly("2016w35","YW")))  // There is no data prior to it (it seems! has to be confirmed)
	replace Partop100000h=. if dm>mofd(dofw(weekly("2016w35","YW")))  // There is no data prior to it (it seems! has to be confirmed)
	*replace Abortop100000h=. if dm>mofd(dofw(weekly("2016w35","YW")))  // There is no data prior to it (it seems! has to be confirmed)


	twoway 	(line Zikap100000h dm if below1800==1, lwidth(thick)  ) ///
			(line Zikap100000h dm if below1800==0, lwidth(thick) lpattern(dash) ) ///
			(line zikagoogle dm , lwidth(thick) yaxis(2) ) ///
			if year>=2015 , scheme(plottig) ytitle("New cases per 100.000 inhabitants") ytitle("Google search index" , axis(2)) ///
			xtitle("") xline(665 670 , lwidth(thick)) ///
			legend(order(1 "ZIKV incidence below 1800 masl" 2 "ZIKV incidence, above 1800 masl" 3 "Google search index") position(6) cols(2)) ///
			caption("Vertical lines:" "June 2015: Ministry of health alerts of the arrival of ZIKV to Colombia)" ///
										"November 2015: first news regarding the potential link between ZIKV and microcephaly") $genOpts

	graph export "$mainFolder/outcomes\images\zikaTrends.png", as(png) replace
	
	/*		
	twoway 	(line Zikap100000h dm if below1800==1, lwidth(thick) lpattern(dash) ) ///
			(line clasp100000h dm if below1800==1, lwidth(thick) lpattern(dash) ) ///
			(connected Partop100000h dm if below1800==1, lwidth(thick)) ///
			(connected Partop100000h dm if below1800==0, lwidth(thick)) ///
			if year>=2014 & dm <mofd(dofw(weekly("2016w35","YW"))) , scheme(plottig) ytitle("Casos mensuales por 100.000h") xtitle("") xline(665 670) ///
			legend(order(1 "Nuevos casos de Zika" 2 "Nuevos casos de Dengue" 3 "Nacimientos, debajo de 1800 msnm" 4 "Nacimientos, encima de 1800 msnm") position(6) cols(2)) ///
			caption("Vertical lines:" "Junio 2015: El ministerio de salud alerta sobre la llegada del Zika a Colombia)" "Noviembre 2015: primeras noticas de Zika y microcefalia") $genOpts
	graph export "$mainFolder/outcomes\images\zika1617.png", as(png) replace		
	*/
	
restore
}

////////////////////////////////////////////////////////////////////////////////
// Add birth-registry data

merge 1:m codigomunicipio dm using "$mainFolder\data\mainData\birthRegMuni.dta", nogen


////////////////////////////////////////////////////////////////////////////////
// Descriptives: histogram and data for map
////////////////////////////////////////////////////////////////////////////////
if 1==0 {
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
// Collapse the data by quarters


gen qtr=qofd(dofm(dm))
gen Q=quarter(dofm(dm))
collapse (sum) births chinkungunya zika dengueClas (mean) rainfall discap capital logpop dismer road_densi aguam  poblacintotal urb NBI dNBI_2 dNBI_3 dNBI_4 dNBI_5 poblacintotalm acueductoPer numconsul apgar2 altitud year , by(qtr Q codigomunicipio)
format qtr %tq

gen births1000h= births/ (poblacintotal/1000)
label var births1000h "Total births per 1.000h, Quarter"
////////////////////////////////////////////////////////////////////////////////


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

	
label var birthsRyeard    "Birth Rate "
label var numconsulRyeard "Health Care"

sum altitud if qtr==tq(2016q1), d // 1SD917 meters!
gen altitudStd=(altitud-1800)/r(sd)

gen ZikaHist = zikaEv1516100000h>0 if zikaEv1516100000h!=.

////////////////////////////////////////////////////////////////////////////////
// Descriptives: birth-rate an yearly birth-rate variation trends
////////////////////////////////////////////////////////////////////////////////
if 1==0 {

	preserve
	collapse (mean)  births1000h birthsRyeard  ///
			, by(qtr year ZikaHist)

	loc l1=tq(2016q1)
			
	twoway 	(line births1000h qtr if ZikaHist==0 , lwidth(thick) ylabel(, format(%2.1f))  ) ///
			(line births1000h qtr if ZikaHist==1 , lwidth(thick) lpattern(dash) ylabel(, format(%2.1f)) ) ///
			 if year>2010 , ytitle("Births per 1.000 inhabitants") xtitle("") xline( `l1' , lwidth(thick)) ///
			legend(order(1 "No Zika" ///
						 2 "Zika"  ///
				   ) position(6) cols(2) ) name(a1a, replace) scheme(plottig)  title("A. Birth rate" "per quarter") xlabel(#3)			
			
		
	twoway 	(line birthsRyeard qtr if ZikaHist==0 , lwidth(thick) ylabel(, format(%2.1f))  ) ///
			(line birthsRyeard qtr if ZikaHist==1 , lwidth(thick) lpattern(dash) ylabel(, format(%2.1f)) ) ///
			 if year>2010 , ytitle("Births per 1.000 inhabitants") xtitle("") xline( `l1' , lwidth(thick)) ///
			legend(order(1 "No Zika" ///
						 2 "Zika"  ///
				   ) position(6) cols(2) ) name(a1b, replace) scheme(plottig)  title("B. Birth-rate" "yearly variation") xlabel(#3)			
			
	grc1leg a1a a1b , scheme(plottig) scale(1.5)
	graph export "$images/trendsALL_main.png", as(png) replace
			
	restore

}


*********************************************************************************
/////////////////////////////////////////////////////////////////////////////////
// Do the matching at municipality level
/////////////////////////////////////////////////////////////////////////////////
// Get trend values prior to 2015

* These ones yearly
foreach varDep in apgar2 numconsul {
	forval y=2013(1)2015 {
		qui{
			local labelo : variable label `varDep'
			gen `varDep'y`y'=`varDep'*(year==`y')
			bys codigomunicipio: egen `varDep'Y`y' = mean(`varDep'y`y')
			drop `varDep'y`y'
			label var `varDep'Y`y' "`labelo': `y'"
		}
	}
}

* And this one quarterly
foreach varDep in births birthsRyeard  {
	forval y=2008(1)2015 {
		forval q=1(1)4 {
			qui{
				local labelo : variable label `varDep'
				gen `varDep'y`y'q`q'=`varDep'*(qtr==tq(`y'q`q'))
				bys codigomunicipio: egen `varDep'Y`y'q`q' = mean(`varDep'y`y'q`q')
				drop `varDep'y`y'q`q'
				label var `varDep'Y`y'q`q' "`labelo': `y', Q`q'"
			}
		}
	}
}

***************
* Who had Zika cases in 2010? At any moment prior to 2010?
glo Cat =3
gen     catG=0 if zikaEv1516100000h==0
replace catG=1 if zikaEv1516100000h>0 & zikaEv1516100000h<=25
replace catG=2 if zikaEv1516100000h>25 & zikaEv1516100000h<=100
replace catG=$Cat if zikaEv1516100000h>100 & zikaEv1516100000h!=.
tab catG if qtr==tq(2015q4)

* Our "treatment" is to be affected by Zika, if you are below the transmition area
gen evZik=zikaEv1516100000h>0 if zikaEv1516100000h!=.
replace evZik=. if altitud<1800 & evZik==0 // But our control is above 1500masl
replace evZik=. if altitud>1800 & evZik==1 // And no treatment units come from such area

label var discap  "Distance to the capital"
label var capital "Capital"
label var road_densi "Road density"
label var aguam "Road density"
label var poblacintotalm "Total population"
label var logpop "LOG Total population"
label var urb "Urban population ratio"
label var NBI  "Poverty index "
label var dNBI_2 "Poverty index (50,01 to 70)"
label var dNBI_3 "Poverty index (70,01 to 80)"
label var dNBI_4 "Poverty index (less than 30)"
label var dNBI_5 "Poverty index (more than 80)"
label var acueductoPer "Piped water coverage"

forval y=2008(1)2015 {
	forvalues c=1/4{
		label var birthsY`y'q`c'       "Births in `y', Q`c'"
		label var birthsRyeardY`y'q`c' "Anual Diff. Births in `y', Q`c'"
	}
}

glo controlmatch discap capital road_densi poblacintotalm urb NBI dNBI_2 dNBI_3 dNBI_4 dNBI_5 acueductoPer  /// // 
				 birthsRyeardY201?q? // numconsulY201? apgar2Y201?  birthsY201?q? 

unab controlmatch : $controlmatch
forval sch=1(1)$Cat {
	psmatch2 evZik `controlmatch' if qtr==tq(2015q4) & (catG==0 | catG==`sch') , kernel out(births1000h) common caliper(0.025) // trim(15)
	egen wei_`sch'=max(_weight) , by( codigomunicipio)
	rename _pscore pscore_`sch'
}

* Get wages using weighting from all groups
egen tW=rowtotal(wei_*)
replace tW=. if tW==0

*********************************************************************************
/////////////////////////////////////////////////////////////////////////////////
// BALANCE TABLE
/////////////////////////////////////////////////////////////////////////////////
cd "$mainFolder/outcomes/tables"

// ==================================
// Balance table per sub-group
if 1==0 {
	glo numC= 2*$Cat+2
	glo cat2= 2*$Cat
	qui {
		texdoc init MATCH_balance , replace force
		tex {
		tex \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}		
		tex \begin{table}[H]
		tex \centering
		tex \scriptsize		
		tex \caption{Matching: Balance Table \label{MATCH_balance}}
		tex \begin{tabular}{l*{$numC}{c}}			
		tex \toprule
			* Group titles
			tex  & \multicolumn{$cat2}{c}{Municipality average} \\
			tex  & 	
			forval sch=1(1)$Cat {
				tex & \multicolumn{2}{c}{Group `sch'}		
			}
			tex \\	
						
			* Small titles
			tex Variable & C 	
			forval sch=1(1)$Cat {
				tex & T & MC		
			}
			tex \\
		
			* Lines
			tex \cmidrule(l){2-2}
			local i=3
			forval sch=1(1)$Cat {
				local i1=`i'+1
				tex \cmidrule(l){`i'-`i1'}		
				local i=`i1'+1
			}	
	}

	* Balance table
	foreach v of varlist $controlmatch {
		local labelo : variable label `v'

		qui sum `v' if catG==0 & qtr==tq(2015q4)
		scalar m0u = r(mean)
		scalar v0u = r(Var)
		local m0u : di %7.4f m0u

		glo linBa = "`labelo' & `m0u'"
		forval sch=1(1)$Cat {
		
			qui sum `v' if catG==`sch' & qtr==tq(2015q4)
			scalar m`sch'u = r(mean)
			scalar v`sch'u = r(Var)
			local m`sch'u : di %7.4f m`sch'u

			* Before matching *****************************************************		
			* standardised % bias before matching
			local bias`sch' = 100*(m`sch'u - m0u)/sqrt((v`sch'u + v0u)/2)
			local bias`sch' : di %7.4f `bias`sch''	
			
			* t-tests before matching
			qui regress `v' evZik if qtr==tq(2015q4) & (catG==0 | catG==`sch'), cluster(codigomunicipio)
			local tbef = _b[evZik]/_se[evZik]
			local tbef : di %7.4f `tbef'
			local pbef = 2*ttail(e(df_r),abs(`tbef'))		
			
			local star`sch'u = ""
			if ((`pbef' < 0.1) )  local star`sch'u = "^{*}" 
			if ((`pbef' < 0.05) ) local star`sch'u = "^{**}" 
			if ((`pbef' < 0.01) ) local star`sch'u = "^{***}" 		
			
			* After matching *****************************************************
			qui sum `v' [aw=wei_`sch'] if catG==0 & qtr==tq(2015q4)
			scalar m`sch'm = r(mean)
			scalar v`sch'm = r(Var)
			local m`sch'm : di %7.4f m`sch'm

			* standardised % bias after matching
			local bias`sch'm = 100*(m`sch'u - m`sch'm)/sqrt((v`sch'u + v`sch'm)/2)
			local bias`sch'm : di %7.4f `bias`sch'm'			
			
			* t-tests after matching
			qui regress `v' evZik if qtr==tq(2015q4) & (catG==0 | catG==`sch') [pw=wei_`sch'], cluster(codigomunicipio)
			local tbef = _b[evZik]/_se[evZik]
			local tbef : di %7.4f `tbef'
			local pbef = 2*ttail(e(df_r),abs(`tbef'))		
			
			local star`sch'm = ""
			if ((`pbef' < 0.1) )  local star`sch'm = "^{*}" 
			if ((`pbef' < 0.05) ) local star`sch'm = "^{**}" 
			if ((`pbef' < 0.01) ) local star`sch'm = "^{***}" 
			
			glo linBa = "$linBa & $ `m`sch'u'`star`sch'u' $ & $ `m`sch'm'`star`sch'm' $ "		
		}
					
		disp "$linBa \\"
		tex $linBa  \\
	}

	* Some stats
		* Lines
		local i=3
		tex \cmidrule(l){2-2}
		forval sch=1(1)$Cat {
			local i1=`i'+1
			tex \cmidrule(l){`i'-`i1'}		
			local i=`i1'+1
		}			
		
		* Incidence
		tex  Zika 2016 Incidence & 	
		forval sch=1(1)$Cat {
			sum zikaEv1516100000h if catG==`sch'
			local mini : disp %4.2f r(min)
			local maxi : disp %4.2f r(max)
			tex & \multicolumn{2}{c}{`mini' to `maxi'}		
		}
		tex \\		

		* Number of Munici
		sum zikaEv1516100000h if catG==0 & qtr==tq(2015q4)
		local conto : disp %4.0f r(N)	
		tex  No. Municipalities & `conto' 	
		forval sch=1(1)$Cat {
			sum zikaEv1516100000h if catG==`sch' & qtr==tq(2015q4)
			local conto : disp %4.0f r(N)
			tex & \multicolumn{2}{c}{`conto'}		
		}
		tex \\		
		
		* Number of Munici included
		sum zikaEv1516100000h if catG==0 & qtr==tq(2015q4) & tW!=. & tW>0
		local conto : disp %4.0f r(N)		
		tex  No. Municipalities Common S & `conto'	
		forval sch=1(1)$Cat {
			sum zikaEv1516100000h if catG==`sch' & qtr==tq(2015q4) & wei_`sch'!=.
			local conto : disp %4.0f r(N)
			tex & \multicolumn{2}{c}{`conto'}		
		}
		tex \\		
			
	qui{
		tex \bottomrule
		tex \multicolumn{$numC}{l}{\parbox[l]{16cm}{Municipalities were matched using Kernel Propensity Score matching (bandwidth for the kernel: 0.06). ///
									T: municipalities with positive Zika incidence in 2016. C: municipalities with ///
									zero Zika incidence in 2010. MC: re-weighted average of group C. The stars show ///
									the significance of a t-test of difference of means: In column T the test is between groups T and C, ///
									and in column MC, between groups T and C but after matching. ///
								}} \\	
		tex \multicolumn{$numC}{l}{Significance: * 10\%, ** 5\%, *** 1\%.  } \\
		tex \end{tabular}
		tex \end{table}
		tex }
		texdoc close
	}

}


// ==================================
// Balance table overall
if 1==1 {
	glo numC= 5
	qui {
		texdoc init MATCH_balance , replace force
		tex {
		tex \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}		
		tex \begin{table}[H]
		tex \centering
		tex \scriptsize		
		tex \caption{Matching: Balance Table \label{MATCH_balance}}
		tex \begin{tabular}{l*{$numC}{c}}			
		tex \toprule
			* Group titles
			tex  & \multicolumn{2}{c}{Full Sample} & \multicolumn{2}{c}{Matched Sample}  \\				
			tex \cmidrule(l){2-3}  \cmidrule(l){4-5}
			tex Variable & T & C & T & C \\ 	
			
			tex \midrule

	}

	* Balance table
	foreach v of varlist $controlmatch {
		local labelo : variable label `v'

		
		* Treatment before matching..................................
		qui sum `v' if catG>0 & catG!=. & qtr==tq(2015q4)
		scalar mu = r(mean)
		scalar vu = r(Var)
		local mu : di %7.4f mu

		glo linBa = "`labelo' & `mu' "		
		
		* Control before matching..................................
		qui sum `v' if catG==0 & qtr==tq(2015q4)
		scalar m0u = r(mean)
		scalar v0u = r(Var)
		local m0u : di %7.4f m0u


			* Before matching *****************************************************		
			
			* t-tests before matching
			qui regress `v' evZik if qtr==tq(2015q4) & (catG==0 | (catG>0 & catG!=.) ), cluster(codigomunicipio)
			local tbef = _b[evZik]/_se[evZik]
			local tbef : di %7.4f `tbef'
			local pbef = 2*ttail(e(df_r),abs(`tbef'))		
			
			local staru = ""
			if ((`pbef' < 0.1) )  local staru = "^{*}" 
			if ((`pbef' < 0.05) ) local staru = "^{**}" 
			if ((`pbef' < 0.01) ) local staru = "^{***}" 		
			
			glo linBa = "$linBa & $ `m0u'`staru' $  "
			
		* Treatment After matching..................................
		qui sum `v' [aw=tW] if catG>0 & catG!=. & qtr==tq(2015q4)
		scalar mm = r(mean)
		scalar vm = r(Var)
		local mm : di %7.4f mm
		
		* Control After matching..................................
		qui sum `v' [aw=tW] if catG==0 & qtr==tq(2015q4)
		scalar m0m = r(mean)
		scalar v0m = r(Var)
		local m0m : di %7.4f m0m
			
			* After matching *****************************************************
			
			* t-tests after matching
			qui regress `v' evZik if qtr==tq(2015q4) & (catG==0 | catG>0 & catG!=. ) [pw=tW], cluster(codigomunicipio)
			local tbef = _b[evZik]/_se[evZik]
			local tbef : di %7.4f `tbef'
			local pbef = 2*ttail(e(df_r),abs(`tbef'))		
			
			local starm = ""
			if ((`pbef' < 0.1) )  local starm = "^{*}" 
			if ((`pbef' < 0.05) ) local starm = "^{**}" 
			if ((`pbef' < 0.01) ) local starm = "^{***}" 
			
			glo linBa = "$linBa & `mm'  & $ `m0m'`starm' $ "		
		
					
		disp "$linBa \\"
		tex $linBa  \\
	}

	* Some stats
		* Lines
		tex \cmidrule(l){2-3}  \cmidrule(l){4-5}
		
		* Incidence
		tex  Zika 2016 Incidence 	
			sum zikaEv1516100000h if catG==0
			local meano : disp %4.2f r(mean)
			tex & `meano'		
		
			sum zikaEv1516100000h if catG>0 & catG!=.
			local meano : disp %4.2f r(mean)
			tex & `meano'	
			
			sum zikaEv1516100000h [aw=tW] if catG==0
			local meano : disp %4.2f r(mean)
			tex & `meano'		
		
			sum zikaEv1516100000h [aw=tW] if catG>0 & catG!=.
			local meano : disp %4.2f r(mean)
			tex & `meano'				
		tex \\		

		* Number of Munici
		tex  No. Municipalities  	
			sum zikaEv1516100000h if catG==0 & qtr==tq(2015q4)
			local conto : disp %4.0f r(N)
			tex & `conto'
			
			sum zikaEv1516100000h if catG>0 & catG!=. & qtr==tq(2015q4)
			local conto : disp %4.0f r(N)
			tex & `conto'
			
			sum zikaEv1516100000h [aw=tW] if catG==0 & qtr==tq(2015q4)
			local conto : disp %4.0f r(N)
			tex & `conto'
			
			sum zikaEv1516100000h [aw=tW] if catG>0 & catG!=. & qtr==tq(2015q4)
			local conto : disp %4.0f r(N)
			tex & `conto'			
		tex \\		
			
	qui{
		tex \bottomrule
		tex \multicolumn{$numC}{l}{\parbox[l]{11cm}{Municipalities were matched using Kernel Propensity Score matching (bandwidth for the kernel: 0.06). ///
									T: municipalities with positive Zika incidence in 2016. C: municipalities with ///
									zero Zika incidence in 2010. The stars in columns C show ///
									the significance of a t-test of difference of means between groups T and C within the relevant sample. ///
								}} \\	
		tex \multicolumn{$numC}{l}{Significance: * 10\%, ** 5\%, *** 1\%.  } \\
		tex \end{tabular}
		tex \end{table}
		tex }
		texdoc close
	}

}


// ==================================
// Pscores graphs for the appendix
if 1==0 {

	tw 	(kdensity pscore_1 if catG==1) (kdensity pscore_1 if catG==0, lpattern(dash)) ///
		(kdensity pscore_1 if catG==1 [aw=wei_1] , lwidth(thick) ) (kdensity pscore_1 if catG==0 [aw=wei_1] , lwidth(thick) lpattern(dash)) ///
		if qtr==tq(2015q4) , name(a1, replace) legend(order(1 "Zika" 2 "No Zika" 3 "Zika Matched" 4 "No Zika, Matched" )) scheme(plottig) title("Low Intensity") ytitle("Propensity score G1")
	tw 	(kdensity pscore_2 if catG==2) (kdensity pscore_2 if catG==0, lpattern(dash)) ///
		(kdensity pscore_2 if catG==2 [aw=wei_2] , lwidth(thick) ) (kdensity pscore_2 if catG==0 [aw=wei_2] , lwidth(thick) lpattern(dash)) ///
		if qtr==tq(2015q4) , name(a2, replace) legend(order(1 "Zika" 2 "No Zika" 3 "Zika Matched" 4 "No Zika, Matched" )) scheme(plottig) title("Medium Intensity") ytitle("Propensity score G2")
	tw 	(kdensity pscore_3 if catG==3) (kdensity pscore_3 if catG==0, lpattern(dash)) ///
		(kdensity pscore_3 if catG==3 [aw=wei_3] , lwidth(thick) ) (kdensity pscore_3 if catG==0 [aw=wei_3] , lwidth(thick) lpattern(dash)) ///
		if qtr==tq(2015q4) , name(a3, replace) legend(order(1 "Zika" 2 "No Zika" 3 "Zika Matched" 4 "No Zika, Matched" )) scheme(plottig) title("High Intensity") ytitle("Propensity score G3")
	window manage close graph a1 
	window manage close graph a2 
	window manage close graph a3
	grc1leg a1 a2 a3 , name(pscore , replace)  $genOpts
}
*********************************************************************************
/////////////////////////////////////////////////////////////////////////////////
// RUN EXERCISES
/////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// 1. Compare municipalities with and without Zika, quarterly

forval y=2015(1)2016 {
	forval qt=1(1)4 {
		gen zinter`y'q`qt'_2=ZikaHist*(qtr==tq(`y'q`qt'))
	}
}

forvalues i=5/6 {
	forvalues c=1/4{
		label var zinter201`i'q`c'_2       "Zikv cases in 201`i', Q`c'"
	}
}

if 1==1 {
	_eststo clear
	eststo: areg birthsRyeard zinter2015q* zinter2016q* ZikaHist c.ZikaHist#i.Q [aw=tW], a(qtr) cluster(codigomunicipio)
	eststo: areg numconsulRyeard zinter2015q* zinter2016q* ZikaHist c.ZikaHist#i.Q [aw=tW], a(qtr) cluster(codigomunicipio)

	eststo: areg birthsRyeard zinter2015q* zinter2016q* ZikaHist c.ZikaHist#i.Q , a(qtr) cluster(codigomunicipio)
	eststo: areg numconsulRyeard zinter2015q* zinter2016q* ZikaHist c.ZikaHist#i.Q , a(qtr) cluster(codigomunicipio)
	
	
	* Nice results here!!!

	* Latex table **************************************************************
	cd "$tables"
	esttab est1 est2  using outcomesMATCH_main.tex , se(3) star(* .1 ** .05 *** .01) keep( zinter2015q* zinter2016q*) booktabs label replace ///
	addnotes("Data after matching." "Treatment is defined for municipalities with zikv cases and with altitude below 1800msnm.")
	if 1==1 { 
	
	// Graph NOT-MATCHED (repeats one created above) *************************************************************
		preserve
		collapse (mean)  births1000h birthsRyeard  ///
				, by(qtr year ZikaHist)

		loc l1=tq(2016q1)
		/* twoway 	(line births1000h qtr if ZikaHist==0 , lwidth(thick)  ) ///
				(line births1000h qtr if ZikaHist==1 , lwidth(thick) lpattern(dash) ) ///
				 if year>2010 , ytitle("Birth rate") xtitle("") xline( `l1' ) ///
				legend(order(1 "No Zika, matched control" ///
							 2 "Zika"  ///
					   )) name(a1, replace) scheme(plottig) 
		*/
		twoway 	(line birthsRyeard qtr if ZikaHist==0 , lwidth(thick) ylabel(, format(%2.1f))  ) ///
				(line birthsRyeard qtr if ZikaHist==1 , lwidth(thick) lpattern(dash) ylabel(, format(%2.1f)) ) ///
				 if year>2010 , ytitle("Birth rate yearly variation") xtitle("") xline( `l1' , lwidth(thick)) ///
				legend(order(1 "No Zika" ///
							 2 "Zika"  ///
					   ) position(6) cols(2) ) name(a2a, replace) scheme(plottig)  title("A. Full sample") xlabel(#3)
		restore		   
		// Graph MATCHED *************************************************************
	
		preserve
		collapse (mean)  births1000h birthsRyeard  ///
				[aw=tW] , by(qtr year ZikaHist)

		loc l1=tq(2016q1)
		/* twoway 	(line births1000h qtr if ZikaHist==0 , lwidth(thick)  ) ///
				(line births1000h qtr if ZikaHist==1 , lwidth(thick) lpattern(dash) ) ///
				 if year>2010 , ytitle("Birth rate") xtitle("") xline( `l1' ) ///
				legend(order(1 "No Zika, matched control" ///
							 2 "Zika"  ///
					   )) name(a1, replace) scheme(plottig) 
		*/
		twoway 	(line birthsRyeard qtr if ZikaHist==0 , lwidth(thick) ylabel(, format(%2.1f))  ) ///
				(line birthsRyeard qtr if ZikaHist==1 , lwidth(thick) lpattern(dash) ylabel(, format(%2.1f)) ) ///
				 if year>2010 , ytitle("Birth rate yearly variation") xtitle("") xline( `l1', lwidth(thick) ) ///
				legend(order(1 "No Zika" ///
							 2 "Zika"  ///
					   ) position(6) cols(2) ) name(a2b, replace) scheme(plottig) title("B. Matched sample") xlabel(#3)				   
		graph export "$images/trendsMATCH_main.png", as(png) replace
		restore
		
		* .........
		grc1leg a2a a2b , scheme(plottig) scale(1.5)		
		
		graph export "$images/trendsMATCH_main2.png", as(png) replace		
	}	
	esttab est3 est4 est1 est2 , se(3) star(* .1 ** .05 *** .01) keep(zinter2015q* zinter2016q*) stats(N N_clust r2_a)
	tab ZikaHist if  qtr==tq(2016q4)

}

