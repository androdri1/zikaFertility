********************************************************************************
* September 2018
* @authors: Luis F. Gamboa and Paul Rodriguez
* @description: Produces the graphs after the estimation
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

cd "$mainFolder\outcomes\matchData"

mat define OP = [100,0.25,0.1]
loc Tipt_MR1 = "C. Syntethic Control Version "
loc Tipt_MR2 = "C. Syntethic Control Version [RMSPE below 0.25]"
loc Tipt_MR3 = "C. Syntethic Control Version [RMSPE below 0.1] "


// ****************************************************************************
// What do you want to do?

* CS2: this is the exercise with controls only from above 1800 masl

loc varo birthsRyeard
// ****************************************************************************
clear
set obs 2
gen uno=1
tempfile filo
save `filo'
foreach parti in  "ALL" "EdadMae0" "EdadMae1" "EdadMae2" "EducMae1" "EducMae2" { // Choose the general exercise that you want
	forval ii=1(1)3 { // Choose a restriction over the RMSPE

		local Tip="_MR`ii'"
		use matchData_CS2_`parti'_`varo'_1`Tip'.dta, clear

		disp in red "Number of units: total and treated `parti'_`varo'_1`Tip'"
		qui tab codigomunicipio
		disp as text r(r)

		qui tab codigomunicipio if catG>0
		disp r(r)
		
		sum births1000h if qtr==tq(2015q4) & TREATED==1

		loc io=1

		if "`Tip'"=="_MR1" { 	// If we have the main estimation (without restrictions on RMSPE),
							// this will generate a histogram, count sample sizes, and
							// acumulate all possible partitions in the memory
			hist pre_rmspe , scheme(plottig) xline( 0.1 0.25 , lwidth(thick))
			graph export "$images\histogramRMSPE_`parti'_`varo'.png" , as(png) replace
			* graph save Graph "D:\Paul.Rodriguez\Dropbox\Salud Colombia\ZIka-embarazo\outcomes\images\histogramRMSPE.gph"
			* The graph was edited by hand
		}

		collapse (mean) `varo' `varo'_synth if  catG>0 & catG!=. , by(qtr) 
		rename `varo' `varo'1
		rename `varo'_synth `varo'_synth1
		gen effect1=`varo'1- `varo'_synth1


		loc varo birthsRyeard
		loc l1=tq(2015q4)
		twoway 	(line `varo'_synth1 qtr , lwidth(thick)  ) ///
				(line `varo'1       qtr , lwidth(vthick) lpattern(dash) ) ///		
				, ${optiono_`varo'} ytitle("Birth rate yearly variation") xtitle("") xlabel(206(4)226  , angle(forty_five)) ///				
				xline( `l1', lwidth(thick) ) yline( 0 , lwidth(thick)) ///
				legend(order(1 "Synthetic Control Group" ///
							 2 "Zika"  ///
					   ) position(6) ) name(a2`ii', replace) scheme(plottig)	///
				title(`Tipt`Tip'') ylabel(, format(%2.1f))

		graph export "$images\synth_CS2_`parti'_`varo'_1`Tip'.png" , as(png) replace
		
		keep if effect1!=.
		list qtr effect1 , clean noobs
		gen tip="`Tip'"
		gen parti="`parti'"
		append using `filo'	
		save `filo'	, replace
	}
}
drop if uno==1
drop uno
