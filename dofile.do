clear
set more off
cd "/Users/preciousallor/Desktop/innovation paper/access"
 use datafinal, replace
 merge 1:1 phid using distance
 drop if _merge==1|_merge==2
 rename cs2q19 dist
 rename cs2q18 finance
 save final1, replace
 drop _merge
 merge m:m clust using weight
 drop if _merge==1|_merge==2
 rename WTA_S wht
 drop if _merge==1|_merge==2
  drop _merge
  save final, replace
 use final,replace
 
  #delimit ;
 keep phid clust nh pid access loan hid region REGION SEX RELAT MARSTAT wht dist finance clusterno supid male single reg10 reg9 reg8 reg7 reg6 reg5 reg4 reg3 reg2 reg1  ebanking ezwich atm check spous_present hsize age tertiary secondary primary education traditional islam christian widowed divorced married notpoor headmale welfare labforce hhsize
 ;
 #delimit cr
 
 save final2,replace
 
use final2, replace
 merge m:m phid using employ
 drop if _merge==1|_merge==2
 drop _merge

  merge m:m phid using income
 drop if _merge==1|_merge==2
 drop _merge
 
 
 merge m:m phid using momo
 drop if _merge==1|_merge==2
 drop _merge
 
 save final4,replace


 drop education spous_present
 global xlist notpoor access age agesq male education ///
		spous_present  married  christian islam ///
		traditional hsize labforce  region dist finance wht
keep $xlist		
save final,replace
		
global reg reg1 reg2 reg4 reg5 reg6 reg7 reg9 reg8 reg10

global  zlist age agesq male education ///
		spous_present  married  christian islam ///
		traditional hsize labforce accesseduc 


 sum notpoor $xlist
 esttab		using summary12.rtf,replace cells ("mean(fmt(3)) sd (fmt(3)) min(fmt(4)) max(fmt(4)) count") title("Summary statistics")

 estpost sum $reg reg3
  esttab		using summary11.rtf,replace cells ("mean(fmt(3)) sd (fmt(3)) min(fmt(4)) max(fmt(4)) count") title("Summary statistics")
  
  
 biprobit (notpoor=  access male age  primary secondary tertiary married christian /// 
  islam traditional labforce hsize reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8 reg9 reg10) ///
  ( access = male age  primary secondary tertiary  married christian /// 
  islam traditional labforce hsize momo reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8 reg9 reg10), robust

 
  probit notpoor access education age agesq spous_present  married christian islam ///
		traditional hsize labforce  i.region, robust
 
 
 probit notpoor  i.access##c.education i.region
 biprobit notpoor access education age agesq spous_present  married christian islam ///
		traditional hsize labforce finance  i.region
 
biprobit (notpoor= acces education age agesq spous_present  married christian islam ///
		traditional hsize labforce  i.region) (access = age agesq spous_present education  married christian islam ///
		traditional hsize labforce finance  i.region ), robust
 
 outreg2 using results,word addstat (e(rho)) replace 
 margins,dydx(_all) post force
outreg2 using results,word append

qui biprobit (notpoor= i.access##c.education age agesq spous_present  married christian islam ///
		traditional hsize labforce  i.region) (access = age agesq spous_present education  married christian islam ///
		traditional hsize labforce finance  i.region )
margins access, at(education=(0 (1) 19)) post force
marginsplot, recastci(rspike) title  ("Moderation Effect")

*****roadmap
/*
the goal now is to determine the effect of Access to finance on poverty using the presence of a bank as the instrument
then we determine if education plays a significant role in moderating this relationship.
Remember you are using a seemingly unrelated binomial probit
