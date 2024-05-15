cd "/Users/farahbeche/Downloads"

use "/Users/farahbeche/Downloads/Epi Analysis Project Items/NHANES_dataset for AP.dta"

merge 1:1 seqn using "/Users/farahbeche/Downloads/Analysis Project Items/Depression_data for AP.dta"

*Target Population
keep if ridageyr>=20

tab dmdeduc2, missing 

tab dmdeduc2

tab indfmpir

* Recode income variable
gen income = (indfmpir >= 1)

gen indfmpir1 = .

replace indfmpir1 = 1 if indfmpir >= 0 & indfmpir <= 1.99
replace indfmpir1 = 2 if indfmpir >= 2 & indfmpir <= 3.99
replace indfmpir1 = 3 if indfmpir >= 4

list indfmpir indfmpir1

* Define labels for variables

label define Education_Level 1 "Less than 9th grade" ///
                            2 "9-11th grade (Includes 12th grade with no diploma)" ///
                            3 "High school graduate/GED or equivalent" ///
                            4 "Some college or AA degree" ///
                            5 "College graduate or above" ///
                            7 "Refused" ///
                            9 "Don't Know"

label define Race_Ethnicity 1 "Mexican American" ///
                            2 "Other Hispanic" ///
                            3 "Non-Hispanic White" ///
                            4 "Non-Hispanic Black" ///
                            6 "Non-Hispanic Asian" ///
                            7 "Other Race - Including Multi-Racial"

label define Gender 1 "Male" ///
                    2 "Female"

label define Income 1 "Low income" ///
                    2 "Mid income" ///
					3 "High income"
					

label define Health Insurance 1 "Yes" ///
                              2 "No" ///
                              7 "Refused" ///
                              9 "Don't Know"


* Apply labels to variables
label values  Education_Level  dmdeduc2
label values ridreth3 Race_Ethnicity
label values riagendr Gender
label values indfmpir1 Income
label values dmdmartz Marital Status
label values hiq011 Health Insurance


gen dep_cat = 0 
replace dep_cat = 1 if dpq010 + dpq020 + dpq030 + dpq040 + dpq050 + dpq060 + dpq070 + dpq080 + dpq090 < 10
replace dep_cat = 2 if dpq010 + dpq020 + dpq030 + dpq040 + dpq050 + dpq060 + dpq070 + dpq080 + dpq090 >= 10
label define dep_cat_lab 1 "Not Depressed(<10)" 2 "Depressed(>=10)" 
label values dep_cat dep_cat_lab
tab dep_cat, miss

* Depression (continuous)
egen dep_cont = rowtotal(dpq010 dpq020 dpq030 dpq040 dpq050 dpq060 dpq070 dpq080 dpq090)
summarize dep_cont

****************************************************************************

* Univariate analysis for Education Level
tab dmdeduc2

* Univariate analysis for Race/Ethnicity
tab ridreth3

* Univariate analysis for Gender
tab riagendr

* Univariate analysis for Income
tab indfmpir1

* Univariate analysis for Depression Category
tab dep_cat

* Univariate analysis for Depression (continuous)
summarize dep_cont, detail
histogram dep_cont, bin(10) title("Distribution of Depression Scores")
* Handle outliers 
summarize dep_cont, detail
local p5 = r(p5)
local p95 = r(p95)
replace dep_cont = `p5' if dep_cont < `p5'
replace dep_cont = `p95' if dep_cont > `p95'
****************************************************************************
* Bivariable associations between education level and depression score/category
* Table 2

recode dmdeduc2 (7 = .) (9 = .)
recode dep_cont (7 = .) (9 = .)
recode dep_cat (7 = .) (9 = .)
recode hiq011   (7 = .) (9 = .)

* Race/ethnicity by education level
tab ridreth3 dmdeduc2, chi2 row

* Gender by education level
tab riagendr dmdeduc2, chi2 row

* Income-to-poverty ratio by education level
tab indfmpir1 dmdeduc2, chi2 row

* Covered by Health Insurance ratio by education level
tab hiq011 dmdeduc2, chi2 row

* Age ratio by education level
anova ridageyr dmdeduc2
tabstat ridageyr, by(dmdeduc2) stats(mean sd count)

* Depression score (continuous) by education level
anova dep_cont dmdeduc2
tabstat dep_cont, by(dmdeduc2) stats(mean sd count)

* Depression category by education level
tab dep_cat dmdeduc2, chi2 row
					 

* Bivariable associations between depression (category) and other variables
* Table 3a

* Race/ethnicity by depression category
tab ridreth3 dep_cat, chi2 row

* Gender by depression category
tab riagendr dep_cat, chi2 row

* Income-to-poverty ratio by depression category
tab indfmpir1 dep_cat, chi2 row

* Education level ratio by depression category
tab dmdeduc2 dep_cat, chi2 row

* Health insurance ratio by depression category
tab hiq011 dep_cat, chi2 row

* Age ratio by depression category
anova ridageyr dep_cat
tabstat ridageyr, by(dep_cat) stats(mean sd count)


* Bivariable associations between depression (continuous) and other variables  
* Table 3b

* Depression score by race/ethnicity
anova dep_cont ridreth3
tabstat dep_cont, by(ridreth3) stats(mean sd count)

* Depression score by gender
ttest dep_cont, by(riagendr) 

* Depression score by income-to-poverty ratio
anova dep_cont indfmpir1
tabstat dep_cont, by(indfmpir1) stats(mean sd count)

* Depression score by education level
anova dep_cont dmdeduc2
tabstat dep_cont, by(dmdeduc2) stats(mean sd count)

* Depression score by covered by health insurance
ttest dep_cont, by(hiq011)

* Depression score by covered by age
pwcorr dep_cont ridageyr, sig 
tabstat dep_cont, by(ridageyr) stats(mean sd count)

****************************************************************************

* Multivariable associations between depression (continuous) and other variables  

*Table 4a Logistic regression  
*Outcome (dependent variable) = depression score 					  
*Exposure (independent variable) = education level
*Confounder (independent variable) = race/ethnicity, gender, health insurance coverage, age, income to poverty ratio

tab dep_cat
gen binary_depression = dep_cat
replace binary_depression = 0 if dep_cat == 1
replace binary_depression = 1 if dep_cat == 2
tab binary_depression

logistic binary_depression i.dmdeduc2
logistic binary_depression ib3.ridreth3
logistic binary_depression i.riagendr
logistic binary_depression i.indfmpir1
logistic binary_depression ridageyr

logistic binary_depression i.dmdeduc2 ib3.ridreth3 i.riagendr ridageyr i.indfmpir1

logistic binary_depression ib3.ridreth3 i.riagendr ridageyr i.dmdeduc2##i.indfmpir1

*Table 4b Linear regression  
*Outcome (dependent variable) = depression score 					           
*Exposure (independent variable) = education level
*Confounder (independent variable) = race/ethnicity, gender, health insurance coverage, age, income to poverty ratio

*Unajduted model:
regress dep_cont i.dmdeduc2
regress dep_cont ib3.ridreth3
regress dep_cont i.riagendr
regress dep_cont i.indfmpir1
regress dep_cont ridageyr

*Full model 1:
regress dep_cont i.dmdeduc2 ib3.ridreth3 i.riagendr i.indfmpir1 ridageyr
																		   
*Full model 2:
regress dep_cont ib3.ridreth3 i.riagendr ridageyr i.dmdeduc2##i.indfmpir1
