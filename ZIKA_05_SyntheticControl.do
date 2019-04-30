********************************************************************************
* January 2019
* @authors: Luis F. Gamboa and Paul Rodriguez
* @description: quarterly analysis of areas with and without Zika incidence
*               but controlling for selection into Zika using a synthetic 
*               control approach
*               Here we split the sample by mother characteristics, but this 
*               makes it hard to obtain a proper matched sample
*              This version uses synth command, and synth_runner
********************************************************************************

set more off

clear all
set matsize 799

*parallel setclusters 3


glo mainFolder="C:\Users\andro\Dropbox\Salud Colombia\ZIka-embarazo\share"
		
glo tables    = "$mainFolder/outcomes/tables/"
glo images    = "$mainFolder/outcomes/images/" 
glo logs      = "$mainFolder/outcomes/logs/" 
glo matchData = "$mainFolder/outcomes/matchData/" 

glo genOpts="  graphregion(color(white) lwidth(medium)) "

mat define OP = [100,0.25,0.1]


foreach parti in "EdadMae0" "EdadMae1" "EdadMae2" "EducMae1" "EducMae2" { //  "ALL" 

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
	// Time-varying variables are not interpolated
	
	xtset codigomunicipio qtr
	xtdescribe

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

	*********************************************************************************
	/////////////////////////////////////////////////////////////////////////////////
	// Do the matching at municipality level
	/////////////////////////////////////////////////////////////////////////////////

	glo contSetTit1 = "Municipalities without reported cases of Zika"
	glo contSetTit2 = "Municipalities above 1800 masl"

	glo folder1 = "zeroCont"
	glo folder2 = "1800_above"

	tempfile bigFilo
	save `bigFilo', replace

	foreach contSet in 2  { // 1 2
		use `bigFilo', clear
		***************
		* Who had Zika cases in 2010? At any moment prior to 2010?
		glo Cat =3
		
		if `contSet'==1 gen     catG=0 if zikaEv1516100000h==0
		if `contSet'==2 gen     catG=0 if altitud>1800 
		
		replace catG=1 if zikaEv1516100000h>0 & zikaEv1516100000h<=25        & altitud<=1800
		replace catG=2 if zikaEv1516100000h>25 & zikaEv1516100000h<=100      & altitud<=1800
		replace catG=$Cat if zikaEv1516100000h>100 & zikaEv1516100000h!=.    & altitud<=1800

		drop if catG==. // Not above 1800 masl where no Zika cases were reported

		tab catG if qtr==tq(2015q4)


		glo optiono_birthsRyeard = "ylabel(-.35(0.05).35) ytitle(Birth rate yearly variation) "
		*glo optiono_births1000h  = "ylabel(1.2(0.5)2.2)   ytitle(Quarterly birth rate) "
		glo optiono_births1000h  = "  ytitle(Quarterly birth rate) " // ylabel(1.2(0.5)2.2) 

		* =============================================================================
		glo iniP = tq(2011q1) //2009q1
		glo iniPx= tq(2013q1) //2009q1
		glo medP = tq(2015q3)
		glo treP = tq(2015q4)


		drop if codigomunicipio== 25488	// Le falta el último periodo
		drop if qtr<$iniP
		
		disp in red "Aqui"
		bys codigomunicipio: gen ene=_N
		tab ene
		drop if ene<24		
		
		foreach varDep in birthsRyeard dismer poblacintotalm urb NBI acueductoPer {
			gen issue = `varDep'==.
			bys codigomunicipio : egen maxo=max(issue)
			drop if maxo==1
			drop  issue maxo
		}			 
		* ==============================================================================					
		

		recode catG (0=0) (1 2 3 = 1), gen(zikV)
		replace zikV=0 if qtr<=$medP
		bysort codigomunicipio: egen TREATED = max(zikV)
				
		foreach varo in birthsRyeard  {	// births1000h   --- numconsul numconsulRyeard : cannot find good match for numconsul...
			forval mr=1(1)3 { // Level of RMSPE which is going to be used as common support
				forval capI=1(1)1 { //3 No sirve para nada ya
					preserve	
					
					if `mr'!=1 { // Remove according to RMSPE original match
						merge 1:1 codigomunicipio qtr using "$matchData/matchData_CS`contSet'_${parti}_`varo'_`capI'_MR1.dta", nogen
						drop if pre_rmspe>OP[1,`mr'] & TREATED==1 // Drop treated units which does not match OK
						drop post_rmspe lead effect `varo'_synth
						rename pre_rmspe pre_rmspeORI
					}
					
					loc addVaro=""
					forval i= $iniP(1)$medP {
						loc addVaro="`addVaro' `varo'(`i') "
					}
					disp in red "Cosa log_CS`contSet'_${parti}_`varo'_`capI'_MR`mr'.txt"
					*
					cap log close
					log using "$logs/log_CS`contSet'_${parti}_`varo'_`capI'_MR`mr'.txt", text replace
					disp "HERE: control set: `contSet' , partition: ${parti},  outcome:`varo' ,  capI:`capI'  " 
					synth_runner `varo' `addVaro' dismer poblacintotalm urb NBI acueductoPer ///
						, d(zikV) gen_vars	 //	parallel: nunca pude usarlo!!!           synthsettings(nested): dio igual...
					
					disp e(pval_joint_post)		// The proportion of effects from control units that have 
												// post-treatment RMSPE at least as great as the treated unit
					disp e(pval_joint_post_t)   // The same, but scales all values by the relevant pre-treatment RMSPE.
					disp e(avg_pre_rmspe_p)		// Proportion of placebos that have a pre-tret RMSPE at least as
												// large as the average of the treated units.

					cap drop contSet varo parti capI MR today t
					cap drop SCdiff SCpval SCpvalR
												
					mat CO= e(b) \ e(pvals_std) \  e(pvals)		
					mat CO=CO'
					mat colnames CO = SCdiff SCpval SCpvalR
					svmat CO, names(col)  

					gen contSet=`contSet'
					gen varo=`varo'
					gen parti = "${parti}"
					gen capI=`capI'
					gen MR=`mr'		
					gen today=c(current_date)
					gen t=_n if SCdiff!=.												
												
					log close

					save "$matchData/matchData_CS`contSet'_${parti}_`varo'_`capI'_MR`mr'.dta", replace
					restore

				}
			}
		}
	}
}

