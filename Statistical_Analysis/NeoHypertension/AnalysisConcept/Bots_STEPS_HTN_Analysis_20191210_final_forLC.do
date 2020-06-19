******************************************************************************************************************************************************
* ANALYSIS code for Botswana STEPS 2014 survey data manuscript 'Determining prevalence and predictors of undiagnosed, untreated and uncontrolled hypertension'
* final analysis!
* Neo M. Tapela
* 10th December 2019 
* File name: Bots_STEPS_HTN_Analysis_20191210_NT.do
******************************************************************************************************************************************************


clear
cls
use "/Users/ntapela/Documents/WORK/RESEARCH/STEPS 2014 data from WHO/Final data/Botswana_STEPS2014_data_4-may-16 cleaned 20191210 deID.dta"


*-------------------------------- DECLARING MULTI-STAGE SURVEY DESIGN --------------------------------*

*metadata was reviewed and referencing STEPS Manual. Some clarifications were made via email communcication with WHO Geneva statistician contact (Dr Stefan Savin). Notes below. 
svyset psu [pw=wstep1], strata(stratum) // code from Stefan...reran entire analysis with this and results are essentially unchanged //
svydes 
* svyset  psu [pw=wstep1], strata(district_code) // nb code previously used, district_code = i1 renamed //


/* DESIGN NOTES:
* this was multistage sampling (PSU = enumeration areas, SSU = household). No stratification, simply PPS sampling within each district to arrive at target 300 PSUs: in the end only 25 districts and 227 enumeration areas enrolled. SSU not incorporated code as would make minimal difference in Cis

** Email responses from Stefan Dec 10 2019: 
The survey has a multiple stage sampling design. The first stage was to select 300 Enumeration Areas from 27 districts based on PPS, second stage 
was the level of household selection. a number of HHs were selected in each EA. And the last stage was to randomly select one individual from each 
of the selected HHs. In the dataset the i1 variable corresponds to districts and the PSU - to EAs. The Individual weights (wstep1, wstep2 and wstep3) 
were calculated for each of the three steps by using probability of selection at each of the above sample stages and population distribution weights.

All values in stratum variable are 1 becase the selection was not stratified and this variable is used for complex sample analysis.

Yes, household are second selection units, but I think you don’t have household variable in the dataset...Theoretically you could use SSU in the 
analysis but this will have a minor impact on CIs of the results. And we don’t have the SSU variable so you may need to look into raw data and 
try to import it. We usually use only PSU to describe the complex sample design for STEPS data analysis in EpiInfo.

Dr Stefan Savin
Technical Officer
HQ/NMH/PND/SPP
World Health Organization
Geneva, CH
Office: +41227914243
Email: savins@who.int

**Details from WHO STEPS protocol manual (page 4-1-5) regarding weighting: 
--- sampling weight: inverse of the probability of selection. For multi-stage sampling designs, this means calculating the probability of selection at every stage of selection and multiplying them all together... For the probability of selection within the household (the final stage of sampling in most STEPS survey), the STEPS app automatically includes household size in the dataset.
--- non response weight: The non-response weight is calculated by taking the inverse of the response rate either for the overall survey or, more often, for each subset of the survey (e.g. EA which can yield info regarding rural vs urban).
--- population weight: adjusts for age-sex differences between the sample population and target population
--- given possible variation in non-response and population weights between each step (interview, anthropometric measurement and blood test) separate weights are provided for each step. weights for step 1 and 2 are identical as steps were conducted in the same visit/same consent for given individual. 
*/


*-------------------------------------------------------------------------------------------------------
* Figure 1. Flowchart illustrating analystic sample selection 
*-------------------------------------------------------------------------------------------------------

tab gender, missing
* 0 missing

*** missing education
tab ses_highestedu2, missing
* 2 missing 
tab ses_highestedu2 gender, missing


*** missing BP history
tab bp_measuredever if ses_highestedu2 !=., missing
* 14 missing 
tab bp_measuredever gender if ses_highestedu2 !=., missing
* 10 F, 4 M

tab bp_diagnosedever if bp_measuredever == 1 & ses_highestedu2 !=., missing
* 0 missing 
tab bp_diagnosedever if bp_measuredever == 0 & ses_highestedu2 !=., missing
* 0 missing

tab bp_onmeds_pmh if bp_diagnosedever == 1 & ses_highestedu2 !=., missing
* 0 missing 
tab bp_onmeds_pmh if bp_diagnosedever == 0 & ses_highestedu2 !=., missing
* 0 missing

tab bp_onmeds_pmh if ses_highestedu2 !=., missing
* 14 missing (all from missing bp_measuredever)


*** missing BP measured values
tab bp_cat, missing
* 61 missing
tab bp_cat if ses_highestedu2 !=. & bp_measuredever !=., missing
* 47 missing among those who have non-missing education and BP history values
tab bp_cat gender if ses_highestedu2 !=. & bp_measuredever !=., missing
* 32F, 15M 

tab htn_analysis, missing
* 4,007
tab htn_analysis gender, missing
* 2751 F, 1317 M
tab htn_analysis gender, missing
* 2709 F, 1298 M


*** interrogating missing BP measurements (although not included in Fig 1)
tab bp_cat if bp_onmeds_pmh == 1 & ses_highestedu2 !=. & bp_measuredever !=., missing
* 2 missing (out of total 451 on BP meds; 0.4% )
tab bp_cat if bp_onmeds_pmh == 0 & ses_highestedu2 !=. & bp_measuredever !=., missing
* 45 missing (out of 3603 not on BP meds; 0.1%... may be problematic as suggests more missing BP measurements for those not on meds)

tab bp_cat if bp_onmeds_pmh == . & ses_highestedu2 !=. & bp_measuredever !=., missing
* 0, ie. none of the 14 with missing bp_onmeds_pmh have non-missing ses_highestedu2 and non-missing bp_measuredever
tab bp_cat bp_measuredever if bp_onmeds_pmh == . & ses_highestedu2 !=., missing
* 13 (out of the 14) missing bp_onmeds_pmh are also missing bp_measuredever
tab bp_cat ses_highestedu2 if bp_onmeds_pmh == ., missing
* 1 (out of the 14) missing bp_onmeds_pmh are also missing ses_highestedu2
tab bp_cat bp_onmeds_pmh, missing
* 14 are missing bp_cat (BP measurement) and bp_onmeds_pmh (all of whom are among/the same as 14 missing bp_measuredever); an additional 1 is missing bp_onmeds_pmh while has documented bp_cat (is missing education)
list pid if bp_cat !=. & bp_onmeds_pmh == . // pid 30098113 is the one participant out of the 14 missing bp_measuredever had BP checked //
list pid bp1* bp2* bp3* if bp_onmeds_pmh == .


*** confirming number of observations with complete documentation of core variables, and which are missing
* core variables are: age gender ses_highestedu2 district_code psu sbp_mean dbp_mean htn bp_measuredever
tab htn_analysis, missing 
* 63 incomplete (includes 61  missing BP values,  2 missing education values and 15 missing bp_measuredever...one of the 15  missing bp_measuredever is also missing bp_measuredever... NB total not 63 as some obs are missing more than one core variable) 
sort bp_measuredever
list pid ses_highestedu2 bp_cat bp_measuredever if htn_analysis == 0 



*-------------------------------------------------------------------------------------------------------
* Table 1. Sociodemographic characteristics of survey participants (unweighted), by gender 
*-------------------------------------------------------------------------------------------------------

*overall:
sum age bmi if htn_analysis ==1, detail
tab1 male agerange_rev2 ses_marital2 ses_race ses_highestedu2 ses_employment2 ses_incomeannual_usdquint bin_distr_rural if htn_analysis == 1, missing

* by gender:
sum age bmi if htn_analysis == 1 & male == 1 , detail
tab1 male agerange_rev2 ses_marital2 ses_race ses_highestedu2 ses_employment2 ses_incomeannual_usdquint bin_distr_rural if htn_analysis == 1 & male == 1, missing

sum age bmi if htn_analysis ==1 &  male == 0, detail
tab1 male agerange_rev2 ses_marital2 ses_race ses_highestedu2 ses_employment2 ses_incomeannual_usdquint bin_distr_rural if htn_analysis == 1 & male == 0, missing
  

*-------------------------------------------------------------------------------------------------------
*Table 2. Clinical and behavioral characteristics of survey participants (unweighted), by gender. 
* ---> MAY NEED TO REPLACE WITH WEIGHTED, PERHAPS MERGING WITH TABLE 3 OR INTO TABLE 5 (SEE AGYEMANG ET AL)
*-------------------------------------------------------------------------------------------------------

*overall:
tab1  bmi_cat cvd_anyhx diabetic_known comorbidity_other  bmi_cat  smoke_current  alc_binge  noexercise_vigmod fruitsveggies_lowintake  salt_some_bin counseling_any edu_lifestyle_any if htn_analysis == 1, missing
sum sedentary_time if htn_analysis ==1, detail

*by gender: 
tab1  bmi_cat cvd_anyhx diabetic_known comorbidity_other  bmi_cat  smoke_current  alc_binge  noexercise_vigmod fruitsveggies_lowintake salt_some_bin  counseling_any edu_lifestyle_any if htn_analysis == 1 & male == 1, missing
sum sedentary_time if htn_analysis ==1 & male == 1, detail

tab1  bmi_cat cvd_anyhx diabetic_known comorbidity_other  bmi_cat  smoke_current  alc_binge  noexercise_vigmod fruitsveggies_lowintake salt_some_bin counseling_any edu_lifestyle_any if htn_analysis == 1 & male == 0, missing
sum sedentary_time if htn_analysis ==1 & male == 0, detail


*-------------------------------------------------------------------------------------------------------
*Figure 2. Age-standardized prevalence of hypertension, awareness, treatment and control
*-------------------------------------------------------------------------------------------------------
*NB. Tantamount to prevalence of unscreened, undiagnosed, untreated and uncontrolled hypertension (as a proportion of each successive subset of hypertensives)

* hypertensive (regardless of whether previously screened or not), out of all participants 
svy, subpop(if htn_analysis ==1): tab htn2,  cell obs ci deff
svy, subpop(if htn_analysis ==1): tab htn2,  count format(%14.3gc) obs ci deff 

* aware, out of hypertensives
svy, subpop(if htn_analysis ==1 & htn2 == 1): tab bp_diagnosedever,  cell obs ci deff
svy, subpop(if htn_analysis ==1 & htn2 ==1): tab bp_diagnosedever,  count format(%14.3gc) obs ci deff 

* on meds, out of aware hypertensives
svy, subpop(if htn_analysis ==1 & htn2 ==1 & bp_diagnosedever ==1): tab  bp_onmeds_pmh,  cell obs ci deff
svy, subpop(if htn_analysis ==1 & htn2 ==1 & bp_diagnosedever == 1): tab  bp_onmeds_pmh,  count format(%14.3gc) obs ci deff

* controlled, out of those treated with meds
svy, subpop(if htn_analysis ==1 & htn ==1 & bp_diagnosedever == 1 & bp_onmeds_pmh ==1): tab controlled,  cell obs ci deff 
svy, subpop(if htn_analysis ==1 & htn ==1 & bp_diagnosedever == 1 & bp_onmeds_pmh ==1): tab controlled,  count format(%14.3gc) obs ci deff 



*** interrogation of whether classified correctly those reporting prior htn but not on meds and with nl BP correctly as normotensive (not included in Figure 2)
sum dbp_mean if normotensive == 1 & htn_analysis == 1 
sum dbp_mean if aware_nomeds_bpnl == 1 & htn_analysis == 1 
sum dbp_mean if untreated ==1 & bp_diagnosedever ==1 & bp_elev == 1 & htn_analysis == 1 

svy, subpop(if normotensive == 1 & htn_analysis ==1): mean sbp_mean 
svy, subpop(if aware_nomeds_bpnl == 1 & htn_analysis ==1): mean sbp_mean
svy, subpop(if untreated ==1 & bp_diagnosedever ==1 & bp_elev == 1  & htn_analysis ==1): mean sbp_mean 
* above confirms that classification is appropriate, with normotensive and aware_nomeds_bpnl having similar BP. 



*-------------------------------------------------------------------------------------------------------
* Figure 3. Age-standardized proportions of undiagnosed, untreated and sub-optimally controlled blood pressure among hypertensives. (as a proportion of common denominator of ALL hypertensives)
*-------------------------------------------------------------------------------------------------------
* NB. only the overall pie chart is included in tables and figures, not the disaggreated analysis (sex, age group)

* overall
svy, subpop(if htn_analysis ==1 & htn2==1): tab htn_status2,  cell obs ci deff
svy, subpop(if htn_analysis ==1 & htn2==1): tab htn_status2,  count format(%14.3gc) obs ci deff // deriving pppulation represented by the sample//

*by gender (not included in Figure 3)
svy, subpop(if htn_analysis ==1 & htn2 ==1 & male == 1): tab htn_status2,  cell obs ci deff
svy, subpop(if htn_analysis ==1 & htn2 ==1 & male == 1): tab htn_status2,  count format(%14.3gc) obs ci deff 
svy, subpop(if htn_analysis ==1 & htn2 ==1 & male == 0): tab htn_status2,  cell obs ci deff
svy, subpop(if htn_analysis ==1 & htn2 ==1 & male == 0): tab htn_status2,  count format(%14.3gc) obs ci deff 

*by age group (not included in Figure 3)
svy, subpop(if htn_analysis ==1 & htn2 ==1 & age15_29 == 1): tab htn_status2,  cell obs ci deff
svy, subpop(if htn_analysis ==1 & htn2 ==1 & age15_29 == 1): tab htn_status2,  count format(%14.3gc) obs ci deff 
svy, subpop(if htn_analysis ==1 & htn2 ==1 & age30_49 == 1): tab htn_status2,  cell obs ci deff
svy, subpop(if htn_analysis ==1 & htn2 ==1 & age30_49 == 1): tab htn_status2,  count format(%14.3gc) obs ci deff 
svy, subpop(if htn_analysis ==1 & htn2 ==1 & age50_69 == 1): tab htn_status2,  cell obs ci deff
svy, subpop(if htn_analysis ==1 & htn2 ==1 & age50_69 == 1): tab htn_status2,  count format(%14.3gc) obs ci deff 


*-------------------------------------------------------------------------------------------------------
* Table 3. Distribution of severity of BP elevation among uncontrolled hypertensives (those who are unaware, untreated, and sub-optimally treated).
*-------------------------------------------------------------------------------------------------------
svy, subpop(if htn_analysis ==1 & htn2 ==1): prop bp_cat if htn_status2 == 1 // undiagnosed ie. unaware//
svy, subpop(if htn_analysis ==1 & htn2 ==1): prop bp_cat if htn_status2 == 2 // untreated //
svy, subpop(if htn_analysis ==1 & htn2 ==1): prop bp_cat if htn_status2 == 3 // uncontrolled ie. sub-optimally treated//
* while above analysis had previously generated  95% CI for code line above (sub-optimally treated), this is not happening on repeat analysis... CI too wide? N too small



*-------------------------------------------------------------------------------------------------------
* Table 4. Unadjusted odds ratios (with 95% CI) of factors associated with hypertension, awareness, treatment and control. 
*-------------------------------------------------------------------------------------------------------

/* variables considered for inclusion were: 
patient level SES: 
	age (cont)
	sex 
	education (4 levels) 
	residence type (rural)
	+/- marital status (bin)

patient level lifestyle behaviour: 
	smoking
	alcohol use
	salt
	fruits and vegetable intake
	adiposity (BMI > WC, bmi is in 3 levels

patient level clinical and physiogic: 
	known diabetes
	other comorbidity (asthma, HIV*, cancer, mental illness)
	
health system factors: 
	advice on lifestyle risk factors	
*/

*-- initial steps
* data visualized to check for outliers 
* checked for collinearity, including diabetes and other comorbidity (r < 0.5)


*-- running model
logistic htn2 age [pweight=wstep1] if htn_analysis ==1, robust
svy, subpop(if htn_analysis ==1): logistic htn2 age
* above are essentially equivalent, however latter produces in its output represented pop and no. of obs
* had initially used the former as we needed to do model validation (testparm, ic stat) which svy did not allow. now that we are pursuing fully-adjusted modeling, rather than best fit model selection, will go with latter. 


*** a) univariable logistic regression for DV = hypertension (htn2) in entire survey sample
* nb. excluding continuous IV from this table, as interested in proportions and unadjusted ORs of subgroups/categories -*

svy, subpop(if htn_analysis ==1): logistic htn2 age

svy, subpop(if htn_analysis ==1): logistic htn2 ib1.agerange_rev2
svy, subpop(if htn_analysis ==1): tab agerange_rev2, obs
svy, subpop(if htn_analysis ==1): tab agerange_rev2 htn2, row obs
svy, subpop(if htn_analysis ==1 & agerange_rev2 == 3): prop htn2 // to derive % htn2 and 95% CI for age 50-69 years//

svy, subpop(if htn_analysis ==1): logistic htn2 male
svy, subpop(if htn_analysis ==1): tab male, obs
svy, subpop(if htn_analysis ==1): tab male htn2, row obs

svy, subpop(if htn_analysis ==1): logistic htn2 edu_secorhigher
svy, subpop(if htn_analysis ==1): tab edu_secorhigher, obs
svy, subpop(if htn_analysis ==1): tab edu_secorhigher htn2, row obs

svy, subpop(if htn_analysis ==1): logistic htn2 ib1.ses_highestedu2
svy, subpop(if htn_analysis ==1): tab ses_highestedu2, obs
svy, subpop(if htn_analysis ==1): tab ses_highestedu2 htn2, row obs

svy, subpop(if htn_analysis ==1): logistic htn2 bin_distr_rural
svy, subpop(if htn_analysis ==1): tab bin_distr_rural, obs
svy, subpop(if htn_analysis ==1): tab bin_distr_rural htn2, row obs

svy, subpop(if htn_analysis ==1): logistic htn2 smoke_current
svy, subpop(if htn_analysis ==1): tab smoke_current, obs
svy, subpop(if htn_analysis ==1): tab smoke_current htn2, row obs

svy, subpop(if htn_analysis ==1): logistic htn2 alc_binge
svy, subpop(if htn_analysis ==1): tab alc_binge, obs
svy, subpop(if htn_analysis ==1): tab alc_binge htn2, row obs

svy, subpop(if htn_analysis ==1): logistic htn2 salt_some_bin
svy, subpop(if htn_analysis ==1): tab salt_some_bin, obs
svy, subpop(if htn_analysis ==1): tab salt_some_bin htn2, row obs

svy, subpop(if htn_analysis ==1): logistic htn2 fruitsveggies_lowintake
svy, subpop(if htn_analysis ==1): tab fruitsveggies_lowintake, obs
svy, subpop(if htn_analysis ==1): tab fruitsveggies_lowintake htn2, row obs

svy, subpop(if htn_analysis ==1): logistic htn2 ib1.bmi_cat2
svy, subpop(if htn_analysis ==1): tab bmi_cat2, obs
svy, subpop(if htn_analysis ==1): tab bmi_cat2 htn2, row obs

svy, subpop(if htn_analysis ==1): logistic htn2 diabetic_known
svy, subpop(if htn_analysis ==1): tab diabetic_known, obs
svy, subpop(if htn_analysis ==1): tab diabetic_known htn2, row obs
svy, subpop(if htn_analysis ==1 & diabetic_known == 1): prop htn2 // to derive % htn2 and 95% CI for age 50-69 years//

svy, subpop(if htn_analysis ==1): logistic htn2 comorbidity_other
svy, subpop(if htn_analysis ==1): tab comorbidity_other, obs
svy, subpop(if htn_analysis ==1): tab comorbidity_other htn2, row obs

svy, subpop(if htn_analysis ==1): logistic htn2 counseling_any
svy, subpop(if htn_analysis ==1): tab counseling_any, obs
svy, subpop(if htn_analysis ==1): tab counseling_any htn2, row obs
svy, subpop(if htn_analysis ==1): tab counseling_any, obs ci // to derive % and 95% CI for for those who received lifestyle advice//


svy, subpop(if htn_analysis ==1): tab smoke_current htn2, obs row
svy, subpop(if htn_analysis ==1): tab smoke_current agerange_rev2, obs row // analysis indicating that younger people smoke more//


*** same code as above, using foreach command instead (factor variables not allowed for foreach command, so manually including these below)
foreach v of varlist male ///
edu_secorhigher ///
bin_distr_rural  ///
smoke_current ///
alc_binge ///
salt_some_bin ///
fruitsveggies_lowintake  ///
diabetic_known ///
comorbidity_other ///
counseling_any { 
svy, subpop(if htn_analysis ==1): logistic htn2 `v'
svy, subpop(if htn_analysis ==1): tab `v', obs
svy, subpop(if htn_analysis ==1): tab `v' htn2, row obs
}

svy, subpop(if htn_analysis ==1): logistic htn2 ib1.agerange_rev2
svy, subpop(if htn_analysis ==1): tab agerange_rev2, obs
svy, subpop(if htn_analysis ==1): tab agerange_rev2 htn2, row obs

svy, subpop(if htn_analysis ==1): logistic htn2 ib1.ses_highestedu2
svy, subpop(if htn_analysis ==1): tab ses_highestedu2, obs
svy, subpop(if htn_analysis ==1): tab ses_highestedu2 htn2, row obs

svy, subpop(if htn_analysis ==1): logistic htn2 ib1.bmi_cat2
svy, subpop(if htn_analysis ==1): tab bmi_cat2, obs
svy, subpop(if htn_analysis ==1): tab bmi_cat2 htn2, row obs



*** b) univariable logistic regression for DV = awareness (bp_diagnosedever) among hypertensives, continuing with the shorter foreach code.
foreach v of varlist male ///
edu_secorhigher ///
bin_distr_rural  ///
smoke_current ///
alc_binge ///
salt_some_bin ///
fruitsveggies_lowintake  ///
diabetic_known ///
comorbidity_other ///
counseling_any { 
svy, subpop(if htn_analysis ==1 & htn2== 1): logistic bp_diagnosedever `v'
svy, subpop(if htn_analysis ==1 & htn2== 1): tab `v', obs
svy, subpop(if htn_analysis ==1 & htn2== 1): tab `v' bp_diagnosedever, row obs
}

svy, subpop(if htn_analysis ==1 & htn2== 1): logistic bp_diagnosedever ib1.agerange_rev2
svy, subpop(if htn_analysis ==1 & htn2== 1): tab agerange_rev2, obs
svy, subpop(if htn_analysis ==1 & htn2== 1): tab agerange_rev2 bp_diagnosedever, row obs

svy, subpop(if htn_analysis ==1 & htn2== 1): logistic bp_diagnosedever ib1.ses_highestedu2
svy, subpop(if htn_analysis ==1 & htn2== 1): tab ses_highestedu2, obs
svy, subpop(if htn_analysis ==1 & htn2== 1): tab ses_highestedu2 bp_diagnosedever, row obs

svy, subpop(if htn_analysis ==1 & htn2== 1): logistic bp_diagnosedever ib1.bmi_cat2
svy, subpop(if htn_analysis ==1 & htn2== 1): tab bmi_cat2, obs
svy, subpop(if htn_analysis ==1 & htn2== 1): tab bmi_cat2 bp_diagnosedever, row obs



*** c) univariable logistic regression for DV = treatment (bp_onmeds_pmh) among diagnosed/aware hypertensives
foreach v of varlist male ///
edu_secorhigher ///
bin_distr_rural  ///
smoke_current ///
alc_binge ///
salt_some_bin ///
fruitsveggies_lowintake  ///
diabetic_known ///
comorbidity_other ///
counseling_any { 
svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1): logistic bp_onmeds_pmh `v'
svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1): tab `v', obs
svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1): tab `v' bp_onmeds_pmh, row obs
}

svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1): logistic bp_onmeds_pmh ib1.agerange_rev2
svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1): tab agerange_rev2, obs
svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1): tab agerange_rev2 bp_onmeds_pmh, row obs

svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1): logistic bp_onmeds_pmh ib1.ses_highestedu2
svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1): tab ses_highestedu2, obs
svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1): tab ses_highestedu2 bp_onmeds_pmh, row obs

svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1): logistic bp_onmeds_pmh ib1.bmi_cat2
svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1): tab bmi_cat2, obs
svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1): tab bmi_cat2 bp_onmeds_pmh, row obs



*** d) univariable logistic regression for DV = control (controlled) among treated hypertensives
foreach v of varlist male ///
edu_secorhigher ///
bin_distr_rural  ///
smoke_current ///
alc_binge ///
salt_some_bin ///
fruitsveggies_lowintake  ///
diabetic_known ///
comorbidity_other ///
counseling_any { 
svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1 & bp_onmeds_pmh == 1): logistic controlled `v'
svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1 & bp_onmeds_pmh == 1): tab `v', obs
svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1 & bp_onmeds_pmh == 1): tab `v' controlled, row obs
}

svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1 & bp_onmeds_pmh == 1): logistic controlled ib1.agerange_rev2
svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1 & bp_onmeds_pmh == 1): tab agerange_rev2, obs
svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1 & bp_onmeds_pmh == 1): tab agerange_rev2 controlled, row obs

svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1 & bp_onmeds_pmh == 1): logistic controlled ib1.ses_highestedu2
svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1 & bp_onmeds_pmh == 1): tab ses_highestedu2, obs
svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1 & bp_onmeds_pmh == 1): tab ses_highestedu2 controlled, row obs

svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1 & bp_onmeds_pmh == 1): logistic controlled ib1.bmi_cat2
svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1 & bp_onmeds_pmh == 1): tab bmi_cat2, obs
svy, subpop(if htn_analysis ==1 & htn2== 1 & bp_diagnosedever == 1 & bp_onmeds_pmh == 1): tab bmi_cat2 controlled, row obs


*-------------------------------------------------------------------------------------------------------
* Table 5. Determinants of hypertension, awareness, treatment and control  following fully-adjusted multivariable logistic regression.
*-------------------------------------------------------------------------------------------------------
* note that: opting to use binary edu_noneorincompleteprimary instead of 4-level ses_highestedu2
* nb kept age as continuous not ib1.agerange_rev2 (to allow sufficient adjustment), kept fruitsveggies_lowintake, 
* kept counseling_any  only for dv = control and dv = treatment (because this is really part of treatment, assess as predictor of control among those on treatment; does not seem to be appropriate to test whether advice is a predictor of hypertension occuring;

*** a) predictors of hypertension, htn2 - fully adjusted model
svy, subpop(if htn_analysis ==1): logistic htn2 male /// 
age ///
edu_secorhigher ///
bin_distr_rural  ///
smoke_current ///
alc_binge ///
salt_some_bin ///
fruitsveggies_lowintake ///
ib1.bmi_cat2  ///
diabetic_known ///
comorbidity_other  

estimates store prevalence1, title(full model prevalence)
estimates dir


*** b) predictors of awareness - fully adjusted model
svy, subpop(if htn_analysis ==1 & htn2 == 1): logistic bp_diagnosedever male /// 
age ///
edu_secorhigher ///
bin_distr_rural  ///
smoke_current ///
alc_binge ///
salt_some_bin ///
fruitsveggies_lowintake ///
ib1.bmi_cat2  ///
diabetic_known ///
comorbidity_other

estimates store awareness1, title(full model awareness)
estimates dir


*** c) predictors of treatment - fully adjusted model
svy, subpop(if htn_analysis ==1 & htn2 == 1 & bp_diagnosedever ==1 ): logistic bp_onmeds_pmh male /// 
age ///
edu_secorhigher ///
bin_distr_rural  ///
smoke_current ///
alc_binge ///
salt_some_bin ///
fruitsveggies_lowintake ///
ib1.bmi_cat2  ///
diabetic_known ///
comorbidity_other ///
counseling_any 

estimates store treatment1, title(full model treatment)
estimates dir


*** d) predictors of control - fully adjusted model
svy, subpop(if htn_analysis ==1 & htn2 == 1 & bp_diagnosedever ==1 & bp_onmeds_pmh == 1):logistic controlled male /// 
age ///
edu_secorhigher ///
bin_distr_rural  ///
smoke_current ///
alc_binge ///
salt_some_bin ///
fruitsveggies_lowintake ///
ib1.bmi_cat2  ///
diabetic_known ///
comorbidity_other ///
counseling_any 

estimates store control1, title(full model control)
estimates dir

*** END














