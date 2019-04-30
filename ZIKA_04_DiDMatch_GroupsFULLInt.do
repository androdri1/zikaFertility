********************************************************************************
* January 2019
* @authors: Luis F. Gamboa and Paul Rodriguez
* @description: quarterly analysis of areas with and without Zika incidence
*               but controlling for selection into Zika using a synthetic 
*               control approach
*               Here we split the sample by mother characteristics, but this 
*               makes it hard to obtain a proper matched sample
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
// Step 1: Produce the matched dataset
////////////////////////////////////////////////////////////////////////////////
if 1==0 {

	clear all
	gen a=0
	tempfile misDatos
	save `misDatos'
	
	foreach parti in "ALL" "EdadMae0" "EdadMae1" "EdadMae2" "EducMae1" "EducMae2" {
		glo parti="`parti'"
		
		use "$mainFolder/data/mainData/municipalityZikaMonth.dta", clear
		drop if dm==.
		drop if year<2008     // No births data prior to this date
		drop if dm>m(2016m12) // No data after it

		gen urb=poblacioncabecera/poblacintotal
		tab NBIcat, gen(dNBI_)
		
		gen qtr=qofd(dofm(dm))
		gen Q=quarter(dofm(dm))
		collapse (sum) chinkungunya zika dengueClas (mean) rainfall discap capital dismer road_densi aguam  poblacintotal urb NBI dNBI_2 dNBI_3 dNBI_4 dNBI_5 poblacintotalm acueductoPer altitud year , by(qtr Q codigomunicipio)
		format qtr %tq	
		
		bys codigomunicipio : gen cuanto=_N
		keep if cuanto==36 // We should have 36 periods (2008Q1 to 2016Q4). This removes 72 municipalities out of 1081
		drop cuanto
		
		if "$parti"=="ALL" {
			merge 1:1 codigomunicipio qtr using "$mainFolder/data/mainData/birthRegMuniQTR.dta", keep(match) nogen
		}
		if "$parti"=="EdadMae0" {
			merge 1:m codigomunicipio qtr using "$mainFolder/data/mainData/birthRegMuniEdadMaeQTR.dta", keep(match) nogen
			keep if edad_madre3==0
		}
		if "$parti"=="EdadMae1" {
			merge 1:m codigomunicipio qtr using "$mainFolder/data/mainData/birthRegMuniEdadMaeQTR.dta", keep(match) nogen
			keep if edad_madre3==1
		}
		if "$parti"=="EdadMae2" {
			merge 1:m codigomunicipio qtr using "$mainFolder/data/mainData/birthRegMuniEdadMaeQTR.dta", keep(match) nogen
			keep if edad_madre3==2
		}
		if "$parti"=="EducMae1" {
			merge 1:m codigomunicipio qtr using "$mainFolder/data/mainData/birthRegMuniEducMaeQTR.dta", keep(match) nogen
			keep if educamae==1
		}
		if "$parti"=="EducMae2" {
			merge 1:m codigomunicipio qtr using "$mainFolder/data/mainData/birthRegMuniEducMaeQTR.dta", keep(match) nogen
			keep if educamae==2
		}
		
		////////////////////////////////////////////////////////////////////////////////

		foreach varDep in rainfall discap capital dismer road_densi aguam  poblacintotal urb NBI dNBI_2 dNBI_3 dNBI_4 dNBI_5 poblacintotalm acueductoPer altitud year {
			bys codigomunicipio: ipolate `varDep' qtr, epolate gen(xx)
			replace `varDep'=xx
			drop xx
		}
		replace altitud=30  if codigomunicipio==13062
		replace altitud=200 if codigomunicipio==13490
		replace altitud=700 if codigomunicipio==17495
		replace altitud=1710 if codigomunicipio==17665		
		replace altitud=852 if codigomunicipio==19300		
		replace altitud=1140 if codigomunicipio==19785		
		replace altitud=982 if codigomunicipio==19845		
		replace altitud=1200 if codigomunicipio==20570		
		replace altitud=55 if codigomunicipio==23682		
		replace altitud=5 if codigomunicipio==23815		
		replace altitud=50 if codigomunicipio==27150		
		replace altitud=43 if codigomunicipio==27160		
		replace altitud=50 if codigomunicipio==27425		
		replace altitud=50 if codigomunicipio==27430		
		replace altitud=50 if codigomunicipio==27450		
		replace altitud=50 if codigomunicipio==27580		
		replace altitud=50 if codigomunicipio==27600		
		replace altitud=119 if codigomunicipio==27810		
		replace altitud=320 if codigomunicipio==44035		
		replace altitud=223 if codigomunicipio==44420		
		replace altitud=24 if codigomunicipio==47030		
		replace altitud=15 if codigomunicipio==47205		
		replace altitud=40 if codigomunicipio==47460		
		replace altitud=75 if codigomunicipio==47660		
		replace altitud=28 if codigomunicipio==47720		
		replace altitud=30 if codigomunicipio==47960		
		replace altitud=30 if codigomunicipio==47980		
		replace altitud=2200 if codigomunicipio==52254		
		replace altitud=2467 if codigomunicipio==52480		
		replace altitud=60 if codigomunicipio==54553		
		replace altitud=1 if codigomunicipio==70221		
		replace altitud=135 if codigomunicipio==70233		
		replace altitud=107 if codigomunicipio==91430		
		replace altitud=97 if codigomunicipio==94663		
				
		// Time-varying variables are not interpolated
		
		xtset codigomunicipio qtr
		xtdescribe

		/* For glo parti="ALL"

		codigomunicipio:  5001, 5002, ..., 99773                     n =       1052
			 qtr:  2008q1, 2008q2, ..., 2016q4                       T =         36
				   Delta(qtr) = 1 quarter
				   Span(qtr)  = 36 periods
				   (codigomunicipio*qtr uniquely identifies each observation)

		Distribution of T_i:   min      5%     25%       50%       75%     95%     max
								36      36      36        36        36      36      36

			 Freq.  Percent    Cum. |  Pattern
		 ---------------------------+--------------------------------------
			 1052    100.00  100.00 |  111111111111111111111111111111111111
		 ---------------------------+--------------------------------------
			 1052    100.00         |  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/
		
		
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


		bys year codigomunicipio : egen yZikv = total(zika)
		replace yZikv=0 if year!=2016
		bys codigomunicipio : egen yZikapop = max(yZikv)
		drop yZikv
		gen zikaEv1516100000h= yZikapop/ (poblacintotal/100000)
		label var zikaEv1516100000h  "Cumulative Zika cases 2016 per 100.000h"

		* anual rates differences
		xtset codigomunicipio qtr
		gen numconsulRyeard= numconsul-L4.numconsul
		gen apgar2Ryeard= apgar2-L4.apgar2
		gen birthsRyeard= births1000h-L4.births1000h

		sum altitud if qtr==tq(2016q1), d // 1SD917 meters!
		gen altitudStd=(altitud-1800)/r(sd)

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
					}
				}
			}
		}

		***************
		* Who had Zika cases in 2010? At any moment prior to 2010?
		glo Cat =3
		gen     catG=0 if altitud>1800 
		replace catG=1 if zikaEv1516100000h>0 & zikaEv1516100000h<=25 & altitud<=1800
		replace catG=2 if zikaEv1516100000h>25 & zikaEv1516100000h<=100 & altitud<=1800
		replace catG=$Cat if zikaEv1516100000h>100 & zikaEv1516100000h!=. & altitud<=1800
		tab catG if qtr==tq(2015q4)

		* Our "treatment" is to be affected by Zika, if you are below the transmition area
		gen evZik=zikaEv1516100000h>0 if zikaEv1516100000h!=.
		replace evZik=. if altitud<1800 & evZik==0 // But our control is above 1500masl
		replace evZik=. if altitud>1800 & evZik==1 // And no treatment units come from such area

		label var dismer  "Distance to the main market"
		label var capital "Capital"
		label var poblacintotalm "Total population"
		label var urb "Urban population ratio"
		label var NBI  "Poverty index "
		label var acueductoPer "Piped water coverage"

		forval y=2008(1)2015 {
			forvalues q=1/4{
				label var birthsY`y'q`q'       "Births in `y', Q`q'"
				label var birthsRyeardY`y'q`q' "Anual Diff. Births in `y', Q`q'"
			}
		}

		glo controlmatch dismer poblacintotalm urb NBI acueductoPer  /// // 
						 birthsRyeardY201?q?

		unab controlmatch : $controlmatch
		forval sch=1(1)$Cat {
			*psmatch2 evZik `controlmatch' if qtr==tq(2015q4) & (catG==0 | catG==`sch') , kernel out(births1000h) common trim(5)
			psmatch2 evZik `controlmatch' if qtr==tq(2015q4) & (catG==0 | catG==`sch') , radius  out(births1000h) common trim(5) caliper(0.01)  // <<< Preferred
			
			
			egen wei_`sch'=max(_weight) , by( codigomunicipio)
			rename _pscore pscore_`sch'
		}
		
		disp in red "Match done!"

		* Get wages using weighting from all groups
		egen tW=rowtotal(wei_*)
		replace tW=. if tW==0

		*********************************************************************************
		/////////////////////////////////////////////////////////////////////////////////
		// BALANCE TABLE
		/////////////////////////////////////////////////////////////////////////////////
		cd "$mainFolder/outcomes/tables"

		// ==================================
		// Balance table overall
		if 1==1 {
			glo numC= 5
			qui {
				texdoc init MATCH_balance_`parti' , replace force
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
				tex \multicolumn{$numC}{l}{\parbox[l]{11cm}{Municipalities were matched using Radius Propensity Score matching (bandwidth for the kernel: 0.06, caliper=0.01, 5\% lower tail of the control was trimmed). ///
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
		if 1==1 {

			tw 	(kdensity pscore_1 if catG==1)                               (kdensity pscore_1 if catG==0, lpattern(dash)) ///
				(kdensity pscore_1 if catG==1   [aw=wei_1] , lwidth(thick) ) (kdensity pscore_1 if catG==0 [aw=wei_1] , lwidth(thick) lpattern(dash)) ///
				if qtr==tq(2015q4) , name(a1, replace) legend(order(1 "Zika" 2 "No Zika" 3 "Zika Matched" 4 "No Zika, Matched" )) scheme(plottig) title("Low Intensity") xtitle("Propensity score G1") ytitle("density")
			tw 	(kdensity pscore_2 if catG==2)                               (kdensity pscore_2 if catG==0, lpattern(dash)) ///
				(kdensity pscore_2 if catG==2 [aw=wei_2] , lwidth(thick) )   (kdensity pscore_2 if catG==0 [aw=wei_2] , lwidth(thick) lpattern(dash)) ///
				if qtr==tq(2015q4) , name(a2, replace) legend(order(1 "Zika" 2 "No Zika" 3 "Zika Matched" 4 "No Zika, Matched" )) scheme(plottig) title("Medium Intensity") xtitle("Propensity score G2") ytitle("density")
			tw 	(kdensity pscore_3 if catG==3)                               (kdensity pscore_3 if catG==0, lpattern(dash)) ///
				(kdensity pscore_3 if catG==3 [aw=wei_3] , lwidth(thick) )   (kdensity pscore_3 if catG==0 [aw=wei_3] , lwidth(thick) lpattern(dash)) ///
				if qtr==tq(2015q4) , name(a3, replace) legend(order(1 "Zika" 2 "No Zika" 3 "Zika Matched" 4 "No Zika, Matched" )) scheme(plottig) title("High Intensity") xtitle("Propensity score G3") ytitle("density")
			window manage close graph a1 
			window manage close graph a2 
			window manage close graph a3
			grc1leg a1 a2 a3 , name(pscore`parti' , replace)  $genOpts
			
			
			graph export "$images/MATCH_balance_`parti'.png", as(png) replace	
		}
		// ==================================		
		
		* Add data identifier
		
		gen typeData="$parti"
		append using `misDatos'	
		save `misDatos', replace
		
		*********************************************************************************
	}
	use `misDatos', clear
	cap drop a

	tab typeData
	save "$mainFolder/data/mainData/02_matchedData.dta", replace

		
}
*********************************************************************************
/////////////////////////////////////////////////////////////////////////////////
// Step 2: MAIN EXERCISES
if 1==1 {
// ALL (Individual regressions) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	foreach parti in  "ALL" "EdadMae0" "EdadMae1" "EdadMae2" "EducMae1" "EducMae2" { // 

		glo parti="`parti'"
		
		use "$mainFolder/data/mainData/02_matchedData.dta", clear

		keep if typeData=="$parti"
		////////////////////////////////////////////////////////////////////////////
		// 1. Compare municipalities with and without Zika, quarterly
			gen ZikaHist = zikaEv1516100000h>0 if zikaEv1516100000h!=.
			
			glo testA=""
			glo testB=""			
			forval y=2015(1)2016 {
				forval qt=1(1)4 {
					gen zinter`y'q`qt'_2=ZikaHist*(qtr==tq(`y'q`qt'))
					
					if (`y'==2015 & `qt'<4) | (`y'<2015)  { // Test for all pre-intervention interactions
						glo testA="$testA (zinter`y'q`qt'_2=0) "
					}
					else if (`y'==2015 & `qt'==4) | (`y'==2016)  {  // Test for all post-intervention interactions
						glo testB="$testB (zinter`y'q`qt'_2=0) "
					}						
				}
			}
			forvalues i=5/6 {
				forvalues c=1/4{
					label var zinter201`i'q`c'_2       "Zikv cases in 201`i', Q`c'"
				}
			}
			set matsize 5000
			
			* a) DiD: Zika vs No Zika ...............................................
			areg birthsRyeard    zinter2015q* zinter2016q* i.qtr , a(codigomunicipio) cluster(codigomunicipio)
			qui tab codigomunicipio if e(sample)==1
			estadd scalar eneM= r(r)
			qui tab codigomunicipio if catG>0 &  e(sample)==1
			estadd scalar eneMT=r(r)
			test $testA
			estadd scalar Fbefore_F  =r(F)
			estadd scalar Fbefore_dfr=r(df_r)
			estadd scalar Fbefore_df =r(df)
			estadd scalar Fbefore_p  =r(p)
			test $testB
			estadd scalar Fafter_F  =r(F)
			estadd scalar Fafter_dfr=r(df_r)
			estadd scalar Fafter_df =r(df)
			estadd scalar Fafter_p  =r(p)
			sum births1000h if qtr==tq(2015q4) & ZikaHist==1 &  e(sample)==1
			estadd scalar ycontMean  =r(mean)
			estadd scalar ycontSD  =r(sd)
			est store r0${parti}		
			
			* c) DiD: Zika below 1800 vs Zika above 1800 ...........................				
			forval y=2015(1)2016 {
				forval qt=1(1)4 {
					replace zinter`y'q`qt'_2=. if ZikaHist==0 & altitud<1800
					replace zinter`y'q`qt'_2=. if ZikaHist==1 & altitud>=1800
				}
			}		
			
			areg birthsRyeard    zinter2015q* zinter2016q* i.qtr , a(codigomunicipio) cluster(codigomunicipio)
			qui tab codigomunicipio if e(sample)==1
			estadd scalar eneM= r(r)
			qui tab codigomunicipio if catG>0 &  e(sample)==1
			estadd scalar eneMT=r(r)	
			test $testA
			estadd scalar Fbefore_F  =r(F)
			estadd scalar Fbefore_dfr=r(df_r)
			estadd scalar Fbefore_df =r(df)
			estadd scalar Fbefore_p  =r(p)
			test $testB
			estadd scalar Fafter_F  =r(F)
			estadd scalar Fafter_dfr=r(df_r)
			estadd scalar Fafter_df =r(df)
			estadd scalar Fafter_p  =r(p)
			sum births1000h if qtr==tq(2015q4) & ZikaHist==1 &  e(sample)==1
			estadd scalar ycontMean  =r(mean)
			estadd scalar ycontSD  =r(sd)			
			est store r2${parti}			
			
			* d) DiD-Match: Zika below 1800 vs Zika above 1800 .....................
			areg birthsRyeard    zinter2015q* zinter2016q* i.qtr [aw=tW], a(codigomunicipio) cluster(codigomunicipio)
			qui tab codigomunicipio if e(sample)==1
			estadd scalar eneM= r(r)
			qui tab codigomunicipio if catG>0 &  e(sample)==1
			estadd scalar eneMT=r(r)		
			test $testA
			estadd scalar Fbefore_F  =r(F)
			estadd scalar Fbefore_dfr=r(df_r)
			estadd scalar Fbefore_df =r(df)
			estadd scalar Fbefore_p  =r(p)
			test $testB
			estadd scalar Fafter_F  =r(F)
			estadd scalar Fafter_dfr=r(df_r)
			estadd scalar Fafter_df =r(df)
			estadd scalar Fafter_p  =r(p)	
			sum births1000h [aw=tW] if qtr==tq(2015q4) & ZikaHist==1 &  e(sample)==1
			estadd scalar ycontMean  =r(mean)
			estadd scalar ycontSD  =r(sd)				
			est store r3${parti}
			
			* b) DiD: Above and Below 1800 ..........................................
				
			forval y=2015(1)2016 {
				forval qt=1(1)4 {
					drop zinter`y'q`qt'_2
					gen zinter`y'q`qt'_2=below1800*(qtr==tq(`y'q`qt'))
				}
			}		
			
			areg birthsRyeard    zinter2015q* zinter2016q* i.qtr , a(codigomunicipio) cluster(codigomunicipio)
			qui tab codigomunicipio if e(sample)==1
			estadd scalar eneM= r(r)
			qui tab codigomunicipio if below1800==1 &  e(sample)==1
			estadd scalar eneMT=r(r)		
			test $testA
			estadd scalar Fbefore_F  =r(F)
			estadd scalar Fbefore_dfr=r(df_r)
			estadd scalar Fbefore_df =r(df)
			estadd scalar Fbefore_p  =r(p)
			test $testB
			estadd scalar Fafter_F  =r(F)
			estadd scalar Fafter_dfr=r(df_r)
			estadd scalar Fafter_df =r(df)
			estadd scalar Fafter_p  =r(p)
			sum births1000h if qtr==tq(2015q4) & below1800==1 &  e(sample)==1
			estadd scalar ycontMean  =r(mean)
			estadd scalar ycontSD  =r(sd)				
			est store r1${parti}			

			*disp in red "$parti"
			*esttab r0${parti} r1${parti} r2${parti} r3${parti} , se(3) star(* .1 ** .05 *** .01) keep(zinter2015q* zinter2016q*) stats(N eneM eneMT r2_a Fbefore_F Fbefore_dfr Fbefore_df Fbefore_p Fafter_F Fafter_dfr Fafter_df Fafter_p)

			if 1==1 { 
			
			// Graph NOT-MATCHED *************************************************************
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
				twoway 	(line birthsRyeard qtr if ZikaHist==0 , lwidth(thick)  ) ///
						(line birthsRyeard qtr if ZikaHist==1 , lwidth(thick) lpattern(dash) ) ///
						 if year>2010 , ytitle("Birth rate yearly variation") xtitle("") xline( `l1' , lwidth(thick)) yline( 0 , lwidth(thick)) ///
						legend(order(1 "No Zika" ///
									 2 "Zika"  ///
							   ) position(6) ) ///
						plotregion(fcolor(gs16) ifcolor(gs15)) xlabel(206(4)226  , angle(forty_five)) ///
						name(a2a, replace) scheme(plottig)  title("A. All data")  ylabel(, format(%2.1f))
				restore		   
				// Graph MATCHED *************************************************************
			
				preserve
				collapse (mean)  births1000h birthsRyeard  ///
						[aw=tW] , by(qtr year ZikaHist)

				loc l1=tq(2015q4)
				/* twoway 	(line births1000h qtr if ZikaHist==0 , lwidth(thick)  ) ///
						(line births1000h qtr if ZikaHist==1 , lwidth(thick) lpattern(dash) ) ///
						 if year>2010 , ytitle("Birth rate") xtitle("") xline( `l1' ) ///
						legend(order(1 "No Zika, matched control" ///
									 2 "Zika"  ///
							   )) name(a1, replace) scheme(plottig) 
				*/
				twoway 	(line birthsRyeard qtr if ZikaHist==0 , lwidth(thick)  ) ///
						(line birthsRyeard qtr if ZikaHist==1 , lwidth(thick) lpattern(dash) ) ///
						 if year>2010 , ytitle("Birth rate yearly variation") xtitle("") xline( `l1', lwidth(thick) ) yline( 0 , lwidth(thick)) ///
						legend(order(1 "No Zika, matched" ///
									 2 "Zika, matched"  ///
							   ) position(6) ) ///
						plotregion(fcolor(gs16) ifcolor(gs15)) xlabel(206(4)226  , angle(forty_five)) ///
						name(a2b, replace) scheme(plottig) title("B. Matched sample") ylabel(, format(%2.1f))			   
				graph export "$images/trendsMATCH_G_$parti.png", as(png) replace
				restore
				
				* .........
				
				graph combine a2a a2b , scheme(plottig) scale(1.5) xsize(7) ysize(4)
				graph export "$images/trendsMATCH_$parti.png", as(png) replace		
			}	

			* Continuous variable; for DiD above below 1800 masl ******************
			sum zika if zikaEv1516100000h>0, d
			*gen ZikInt= zikaEv1516100000h/284.8721  // In terms of standard deviations of total Zika cases during 2016
			gen ZikInt= zika/268.6305  // In terms of standard deviations of monthly incidence; not such a good idea
			
			forval y=2015(1)2016 {
				forval qt=1(1)4 {
					gen zinterC`y'q`qt'   =ZikInt*(qtr==tq(`y'q`qt'))
					label var zinterC`y'q`qt'   "Zikv intensity (SD) in 201`i', Q`c'"
				}
			}		
			
			forval y=2015(1)2016 {
				forval qt=1(1)4 {
					replace zinterC`y'q`qt'=. if ZikaHist==0 & altitud<1800
					replace zinterC`y'q`qt'=. if ZikaHist==1 & altitud>=1800		
				}
			}
			
			areg birthsRyeard  zinter2015q* zinter2016q*  zinterC2015q* zinterC2016q* i.qtr [aw=tW], a(codigomunicipio) cluster(codigomunicipio)
			qui tab codigomunicipio if e(sample)==1
			estadd scalar eneM= r(r)
			qui tab codigomunicipio if catG>0 &  e(sample)==1
			estadd scalar eneMT=r(r)		
			est store r4${parti}	
			
			areg birthsRyeard  zinterC2015q* zinterC2016q* i.qtr [aw=tW], a(codigomunicipio) cluster(codigomunicipio)
			qui tab codigomunicipio if e(sample)==1
			estadd scalar eneM= r(r)
			qui tab codigomunicipio if catG>0 &  e(sample)==1
			estadd scalar eneMT=r(r)		
			est store r5${parti}				
		
			
			disp in red "$parti"
			esttab r3${parti} r4${parti}  r5${parti} , se(3) star(* .1 ** .05 *** .01) keep(zinter2015q* zinter2016q* zinterC2015q* zinterC2016q*) stats(N eneM eneMT r2_a)
			*/	
	}		
	
	
	if 1==1 { // Testing differences between groups ............................
	
		// Education ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		use "$mainFolder/data/mainData/02_matchedData.dta", clear

		keep if typeData=="EducMae1" | typeData=="EducMae2"
		encode typeData, gen(edc)
		////////////////////////////////////////////////////////////////////////////
		// 1. Compare municipalities with and without Zika, quarterly

		gen ZikaHist = zikaEv1516100000h>0 if zikaEv1516100000h!=.
		forval y=2015(1)2016 {
			forval qt=1(1)4 {
				cap drop zinter`y'q`qt'_2
				gen zinter`y'q`qt'_2=ZikaHist*(qtr==tq(`y'q`qt'))
				forval ed=1(1)2 {
					gen zinterEd`ed'_`y'q`qt'_2=ZikaHist*(qtr==tq(`y'q`qt'))*(edc==`ed')
					gen qtrEd`ed'_`y'q`qt'=(qtr==tq(`y'q`qt'))*(edc==`ed')
				}
			}
		}
		forvalues i=5/6 {
			forvalues c=1/4{
				forval ed=1(1)2 {
					label var zinterEd`ed'_201`i'q`c'_2       "Ed`ed': Zikv cases in 201`i', Q`c'"
				}
			}
		}
		set matsize 5000
		
		* a) DiD: Zika vs No Zika ...............................................
		areg birthsRyeard    zinter2015q* zinter2016q* zinterEd2_2015q* zinterEd2_2016q* i.qtr i.edc qtrEd* , a(codigomunicipio) cluster(codigomunicipio)
		qui tab codigomunicipio if e(sample)==1
		estadd scalar eneM= r(r)
		qui tab codigomunicipio if catG>0 &  e(sample)==1
		estadd scalar eneMT=r(r)

		est store r0Educ
		
		* c) DiD: Zika below 1800 vs Zika above 1800 ...........................				
		forval y=2015(1)2016 {
			forval qt=1(1)4 {
				replace zinter`y'q`qt'_2=. if ZikaHist==0 & altitud<1800
				replace zinter`y'q`qt'_2=. if ZikaHist==1 & altitud>=1800		
				forval ed=1(1)2 {
					replace zinterEd`ed'_`y'q`qt'_2=. if ZikaHist==0 & altitud<1800
					replace zinterEd`ed'_`y'q`qt'_2=. if ZikaHist==1 & altitud>=1800
				}
			}
		}		
		
		areg birthsRyeard    zinter2015q* zinter2016q* zinterEd2_2015q* zinterEd2_2016q* i.qtr i.edc qtrEd* , a(codigomunicipio) cluster(codigomunicipio)
		qui tab codigomunicipio if e(sample)==1
		estadd scalar eneM= r(r)
		qui tab codigomunicipio if catG>0 &  e(sample)==1
		estadd scalar eneMT=r(r)		
		est store r2Educ			
		
		* d) DiD-Match: Zika below 1800 vs Zika above 1800 .....................
		areg birthsRyeard    zinter2015q* zinter2016q* zinterEd2_2015q* zinterEd2_2016q* i.qtr i.edc qtrEd* [aw=tW], a(codigomunicipio) cluster(codigomunicipio)
		qui tab codigomunicipio if e(sample)==1
		estadd scalar eneM= r(r)
		qui tab codigomunicipio if catG>0 &  e(sample)==1
		estadd scalar eneMT=r(r)		
		est store r3Educ

		
		* b) DiD: Above and Below 1800 ..........................................
			
		forval y=2015(1)2016 {
			forval qt=1(1)4 {
				drop zinter`y'q`qt'_2
				gen zinter`y'q`qt'_2=below1800*(qtr==tq(`y'q`qt'))		
				forval ed=1(1)2 {
					drop zinterEd`ed'_`y'q`qt'_2
					gen zinterEd`ed'_`y'q`qt'_2=below1800*(qtr==tq(`y'q`qt'))*(edc==`ed')
				}
			}
		}		
		
		areg birthsRyeard    zinter2015q* zinter2016q* zinterEd2_2015q* zinterEd2_2016q* i.qtr i.edc qtrEd* , a(codigomunicipio) cluster(codigomunicipio)
		qui tab codigomunicipio if e(sample)==1
		estadd scalar eneM= r(r)
		qui tab codigomunicipio if below1800==1 &  e(sample)==1
		estadd scalar eneMT=r(r)		
		est store r1Educ		
		
		*esttab r0Educ r1Educ r2Educ r3Educ , se(3) star(* .1 ** .05 *** .01) keep(zinterEd1_2015q* zinterEd1_2016q* zinterEd2_2015q* zinterEd2_2016q*) stats(N eneM eneMT r2_a)

																				// Age  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		use "$mainFolder/data/mainData/02_matchedData.dta", clear

		keep if typeData=="EdadMae0" | typeData=="EdadMae1" | typeData=="EdadMae2" 
		encode typeData, gen(age)
		////////////////////////////////////////////////////////////////////////////
		// 1. Compare municipalities with and without Zika, quarterly

		gen ZikaHist = zikaEv1516100000h>0 if zikaEv1516100000h!=.
		forval y=2015(1)2016 {
			forval qt=1(1)4 {
				cap drop zinter`y'q`qt'_2
				gen zinter`y'q`qt'_2=ZikaHist*(qtr==tq(`y'q`qt'))			
				forval age=1(1)3 {
					gen zinterAg`age'_`y'q`qt'_2=ZikaHist*(qtr==tq(`y'q`qt'))*(age==`age')
					gen qtrAg`age'_`y'q`qt'=(qtr==tq(`y'q`qt'))*(age==`age')
				}
			}
		}
		forvalues i=5/6 {
			forvalues c=1/4{
				forval age=1(1)3 {
					label var zinterAg`age'_201`i'q`c'_2       "Ag`age': Zikv cases in 201`i', Q`c'"
				}
			}
		}
		set matsize 5000
		
		* a) DiD: Zika vs No Zika ...............................................
		areg birthsRyeard    zinter2015q* zinter2016q* zinterAg2_2015q* zinterAg2_2016q* zinterAg3_2015q* zinterAg3_2016q* i.qtr i.age qtrAg* , a(codigomunicipio) cluster(codigomunicipio)
		qui tab codigomunicipio if e(sample)==1
		estadd scalar eneM= r(r)
		qui tab codigomunicipio if catG>0 &  e(sample)==1
		estadd scalar eneMT=r(r)

		est store r0Age		
		
		* c) DiD: Zika below 1800 vs Zika above 1800 ...........................				
		forval y=2015(1)2016 {
			forval qt=1(1)4 {
				cap drop zinter`y'q`qt'_2
				gen zinter`y'q`qt'_2=ZikaHist*(qtr==tq(`y'q`qt'))			
				forval age=1(1)3 {
					replace zinterAg`age'_`y'q`qt'_2=. if ZikaHist==0 & altitud<1800
					replace zinterAg`age'_`y'q`qt'_2=. if ZikaHist==1 & altitud>=1800
				}
			}
		}		
		
		areg birthsRyeard   zinter2015q* zinter2016q* zinterAg2_2015q* zinterAg2_2016q* zinterAg3_2015q* zinterAg3_2016q* i.qtr i.age qtrAg* , a(codigomunicipio) cluster(codigomunicipio)
		qui tab codigomunicipio if e(sample)==1
		estadd scalar eneM= r(r)
		qui tab codigomunicipio if catG>0 &  e(sample)==1
		estadd scalar eneMT=r(r)		
		est store r2Age			
		
		* d) DiD-Match: Zika below 1800 vs Zika above 1800 .....................
		areg birthsRyeard   zinter2015q* zinter2016q* zinterAg2_2015q* zinterAg2_2016q* zinterAg3_2015q* zinterAg3_2016q* i.qtr i.age qtrAg* [aw=tW], a(codigomunicipio) cluster(codigomunicipio)
		qui tab codigomunicipio if e(sample)==1
		estadd scalar eneM= r(r)
		qui tab codigomunicipio if catG>0 &  e(sample)==1
		estadd scalar eneMT=r(r)		
		est store r3Age
		tab codigomunicipio ZikaHist if e(sample)==1

		
		* b) DiD: Above and Below 1800 ..........................................
			
		forval y=2015(1)2016 {
			forval qt=1(1)4 {
				cap drop zinter`y'q`qt'_2
				gen zinter`y'q`qt'_2=ZikaHist*(qtr==tq(`y'q`qt'))			
				forval age=1(1)3 {
					drop zinterAg`age'_`y'q`qt'_2
					gen zinterAg`age'_`y'q`qt'_2=below1800*(qtr==tq(`y'q`qt'))*(age==`age')
				}
			}
		}		
		
		areg birthsRyeard    zinter2015q* zinter2016q* zinterAg2_2015q* zinterAg2_2016q* zinterAg3_2015q* zinterAg3_2016q* i.qtr i.age qtrAg* , a(codigomunicipio) cluster(codigomunicipio)
		qui tab codigomunicipio if e(sample)==1
		estadd scalar eneM= r(r)
		qui tab codigomunicipio if below1800==1 &  e(sample)==1
		estadd scalar eneMT=r(r)		
		est store r1Age		
		
		*esttab r0Age r1Age r2Age r3Age , se(3) star(* .1 ** .05 *** .01) keep(zinterAg1_2015q* zinterAg1_2016q* zinterAg2_2015q* zinterAg2_2016q*  zinterAg3_2015q* zinterAg3_2016q* ) stats(N eneM eneMT r2_a)


	}
		
	if 1==1 { // Produce Tables (requires testing diff between groups ~~~~~~~~~~~

		esttab r0ALL r1ALL r2ALL r3ALL , se(3) star(* .1 ** .05 *** .01) keep(zinter2015q* zinter2016q* ) stats(N eneM eneMT r2_a  Fbefore_F Fbefore_dfr Fbefore_df Fbefore_p Fafter_F Fafter_dfr Fafter_df Fafter_p ycontMean ycontSD)
		esttab r0ALL r1ALL r2ALL r3ALL using "$tables/resultsALL.csv" , se(3) star(* .1 ** .05 *** .01) keep(zinter2015q* zinter2016q*) scalars(N eneM eneMT Fbefore_F Fbefore_dfr Fbefore_df Fbefore_p Fafter_F Fafter_dfr Fafter_df Fafter_p ycontMean ycontSD) ///
				label csv plain replace b("%8.3f")

		esttab r3EducMae1 r3EducMae2 , se(3) star(* .1 ** .05 *** .01) keep(zinter2015q* zinter2016q*) stats(N eneM eneMT r2_a Fbefore_F Fbefore_dfr Fbefore_df Fbefore_p Fafter_F Fafter_dfr Fafter_df Fafter_p ycontMean ycontSD)
		esttab r3Educ , se(3) star(* .1 ** .05 *** .01) keep(zinter2015q* zinter2016q* zinterEd2_2015q* zinterEd2_2016q* ) stats(N eneM eneMT r2_a)
		esttab r3Educ r3EducMae1 r3EducMae2 using "$tables/resultsEduc.csv" , se(3) star(* .1 ** .05 *** .01) keep(zinter2015q* zinter2016q* zinterEd2_2015q* zinterEd2_2016q*   ) scalars(N eneM eneMT Fbefore_F Fbefore_dfr Fbefore_df Fbefore_p Fafter_F Fafter_dfr Fafter_df Fafter_p ycontMean ycontSD) ///
				label csv plain replace b("%8.3f")
				
		esttab r3EdadMae0 r3EdadMae1 r3EdadMae2 , se(3) star(* .1 ** .05 *** .01) keep(zinter2015q* zinter2016q*) stats(N eneM eneMT r2_a Fbefore_F Fbefore_dfr Fbefore_df Fbefore_p Fafter_F Fafter_dfr Fafter_df Fafter_p ycontMean ycontSD)
		esttab r3Age , se(3) star(* .1 ** .05 *** .01) keep(zinter2015q* zinter2016q* zinterAg2_2015q* zinterAg2_2016q*  zinterAg3_2015q* zinterAg3_2016q* ) stats(N eneM eneMT r2_a)
		esttab r3Age r3EdadMae0 r3EdadMae1 r3EdadMae2 using "$tables/resultsAge.csv" , se(3) star(* .1 ** .05 *** .01) keep(zinter2015q* zinter2016q* zinterAg2_2015q* zinterAg2_2016q*  zinterAg3_2015q* zinterAg3_2016q* ) scalars(N eneM eneMT Fbefore_F Fbefore_dfr Fbefore_df Fbefore_p Fafter_F Fafter_dfr Fafter_df Fafter_p ycontMean ycontSD) ///
				label csv plain replace b("%8.3f")
				
		esttab r4ALL r5ALL  , se(3) star(* .1 ** .05 *** .01) keep(zinter2015q* zinter2016q* zinterC2015q* zinterC2016q*) stats(N eneM eneMT r2_a )
		esttab r4ALL r5ALL using "$tables/resultsALLcontinuous.csv" , se(3) star(* .1 ** .05 *** .01) keep(zinter2015q* zinter2016q* zinterC2015q* zinterC2016q*) scalars(N eneM eneMT ) ///
				label csv plain replace b("%8.3f")			
			
	}
}

/////////////////////////////////////////////////////////////////////////////////
// Step 3: ALL QUARTERS REGS
if 1==0 {
// ALL (Individual regressions) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	foreach parti in  "ALL" "EdadMae0" "EdadMae1" "EdadMae2" "EducMae1" "EducMae2" { // 

		glo parti="`parti'"
		
		use "$mainFolder/data/mainData/02_matchedData.dta", clear

		keep if typeData=="$parti"
		////////////////////////////////////////////////////////////////////////////
		// 1. Compare municipalities with and without Zika, quarterly
			*qui {
				glo testA=""
				glo testB=""
				gen ZikaHist = zikaEv1516100000h>0 if zikaEv1516100000h!=.
				forval y=2009(1)2016 {
					loc low=1
					if `y'==2009 loc low=3
					forval qt=`low'(1)4 {
						gen zinter`y'q`qt'_2=ZikaHist*(qtr==tq(`y'q`qt'))
						
						if (`y'==2015 & `qt'<4) | (`y'<2015)  { // Test for all pre-intervention interactions
							glo testA="$testA (zinter`y'q`qt'_2=0) "
						}
						else if (`y'==2015 & `qt'==4) | (`y'==2016)  {  // Test for all post-intervention interactions
							glo testB="$testB (zinter`y'q`qt'_2=0) "
						}						
						
					}
				}
				forvalues i=5/6 {
					forvalues c=1/4{
						label var zinter201`i'q`c'_2       "Zikv cases in 201`i', Q`c'"
					}
				}
				set matsize 5000
				
				* a) DiD: Zika vs No Zika ...............................................
				areg birthsRyeard    zinter20*q* i.qtr , a(codigomunicipio) cluster(codigomunicipio)
				qui tab codigomunicipio if e(sample)==1
				estadd scalar eneM= r(r)
				qui tab codigomunicipio if catG>0 &  e(sample)==1
				estadd scalar eneMT=r(r)
				test $testA
				estadd scalar Fbefore_F  =r(F)
				estadd scalar Fbefore_dfr=r(df_r)
				estadd scalar Fbefore_df =r(df)
				estadd scalar Fbefore_p  =r(p)
				test $testB
				estadd scalar Fafter_F  =r(F)
				estadd scalar Fafter_dfr=r(df_r)
				estadd scalar Fafter_df =r(df)
				estadd scalar Fafter_p  =r(p)
				est store r0${parti}		
				
				* c) DiD: Zika below 1800 vs Zika above 1800 ...........................				
				forval y=2009(1)2016 {
					loc low=1
					if `y'==2009 loc low=3
					forval qt=`low'(1)4 {
						replace zinter`y'q`qt'_2=. if ZikaHist==0 & altitud<1800
						replace zinter`y'q`qt'_2=. if ZikaHist==1 & altitud>=1800
					}
				}		
				
				areg birthsRyeard    zinter20*q* i.qtr , a(codigomunicipio) cluster(codigomunicipio)
				qui tab codigomunicipio if e(sample)==1
				estadd scalar eneM= r(r)
				qui tab codigomunicipio if catG>0 &  e(sample)==1
				estadd scalar eneMT=r(r)		
				test $testA
				estadd scalar Fbefore_F  =r(F)
				estadd scalar Fbefore_dfr=r(df_r)
				estadd scalar Fbefore_df =r(df)
				estadd scalar Fbefore_p  =r(p)
				test $testB
				estadd scalar Fafter_F  =r(F)
				estadd scalar Fafter_dfr=r(df_r)
				estadd scalar Fafter_df =r(df)
				estadd scalar Fafter_p  =r(p)				
				est store r2${parti}			
				
				* d) DiD-Match: Zika below 1800 vs Zika above 1800 .....................
				areg birthsRyeard    zinter20*q* i.qtr [aw=tW], a(codigomunicipio) cluster(codigomunicipio)
				qui tab codigomunicipio if e(sample)==1
				estadd scalar eneM= r(r)
				qui tab codigomunicipio if catG>0 &  e(sample)==1
				estadd scalar eneMT=r(r)	
				test $testA
				estadd scalar Fbefore_F  =r(F)
				estadd scalar Fbefore_dfr=r(df_r)
				estadd scalar Fbefore_df =r(df)
				estadd scalar Fbefore_p  =r(p)
				test $testB
				estadd scalar Fafter_F  =r(F)
				estadd scalar Fafter_dfr=r(df_r)
				estadd scalar Fafter_df =r(df)
				estadd scalar Fafter_p  =r(p)				
				est store r3${parti}

				
				* b) DiD: Above and Below 1800 ..........................................
					
				forval y=2009(1)2016 {
					loc low=1
					if `y'==2009 loc low=3
					forval qt=`low'(1)4 {				
						drop zinter`y'q`qt'_2
						gen zinter`y'q`qt'_2=below1800*(qtr==tq(`y'q`qt'))
					}
				}		
				
				areg birthsRyeard    zinter20*q* i.qtr , a(codigomunicipio) cluster(codigomunicipio)
				qui tab codigomunicipio if e(sample)==1
				estadd scalar eneM= r(r)
				qui tab codigomunicipio if below1800==1 &  e(sample)==1
				estadd scalar eneMT=r(r)	
				test $testA
				estadd scalar Fbefore_F  =r(F)
				estadd scalar Fbefore_dfr=r(df_r)
				estadd scalar Fbefore_df =r(df)
				estadd scalar Fbefore_p  =r(p)
				test $testB
				estadd scalar Fafter_F  =r(F)
				estadd scalar Fafter_dfr=r(df_r)
				estadd scalar Fafter_df =r(df)
				estadd scalar Fafter_p  =r(p)				
				est store r1${parti}			
			*}	
			disp in red "`parti'"
			esttab r0${parti} r1${parti} r2${parti} r3${parti} , se(3) star(* .1 ** .05 *** .01) keep(zinter20*q*) stats(N eneM eneMT r2_a Fbefore_F Fbefore_dfr Fbefore_df Fbefore_p Fafter_F Fafter_dfr Fafter_df Fafter_p)
			
			/* Continuous variable; for above below 1800 masl ******************
			sum zika if zikaEv1516100000h>0, d
			*gen ZikInt= zikaEv1516100000h/284.8721  // In terms of standard deviations of total Zika cases during 2016
			gen ZikInt= zika/268.6305  // In terms of standard deviations of monthly incidence; not such a good idea
			
			forval y=2015(1)2016 {
				forval qt=1(1)4 {
					gen zinterC`y'q`qt'   =ZikInt*(qtr==tq(`y'q`qt'))
					label var zinterC`y'q`qt'   "Zikv intensity (SD) in 201`i', Q`c'"
				}
			}		
			
			forval y=2015(1)2016 {
				forval qt=1(1)4 {
					replace zinterC`y'q`qt'=. if ZikaHist==0 & altitud<1800
					replace zinterC`y'q`qt'=. if ZikaHist==1 & altitud>=1800		
				}
			}
			
			areg birthsRyeard  zinter2015q* zinter2016q*  zinterC2015q* zinterC2016q* i.qtr [aw=tW], a(codigomunicipio) cluster(codigomunicipio)
			qui tab codigomunicipio if e(sample)==1
			estadd scalar eneM= r(r)
			qui tab codigomunicipio if catG>0 &  e(sample)==1
			estadd scalar eneMT=r(r)		
			est store r4${parti}		
			
			esttab r3${parti} r4${parti} , se(3) star(* .1 ** .05 *** .01) keep(zinter2015q* zinter2016q* zinterG?2015q* zinterG?2016q* zinterC2015q* zinterC2016q*) stats(N eneM eneMT r2_a)
			*/	
	}		
	
	// Produce Tables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	esttab r3ALL r3EducMae1 r3EducMae2 r3EdadMae0 r3EdadMae1 r3EdadMae2 , se(3) star(* .1 ** .05 *** .01) keep(zinter20*q* ) stats(N eneM eneMT r2_a  Fbefore_F Fbefore_dfr Fbefore_df Fbefore_p Fafter_F Fafter_dfr Fafter_df Fafter_p)
	esttab r3ALL r3EducMae1 r3EducMae2 r3EdadMae0 r3EdadMae1 r3EdadMae2 using "$tables/allQuartersTable.csv" , se(3) star(* .1 ** .05 *** .01) keep(zinter20*q*) scalars(N eneM eneMT  Fbefore_F Fbefore_dfr Fbefore_df Fbefore_p Fafter_F Fafter_dfr Fafter_df Fafter_p) ///
			label csv plain replace b("%8.3f")
}
