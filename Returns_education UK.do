*returnseduk.do
*Estimating returns to education in UK
*input:  a_indresp.dta
*output: returnseduk.log

*The dataset is a cross sectional data from the understanding society UK Household
*longitudinal study: data of responding adults (16+) using WAVE 1
*pid  unique identifiers of individuals
*wave 1: data collected from 2009 to 2011.

*******************************************************************RETURNS TO EDUCATION******************************************************************************
**SUBJECT: PROJECT

* Tell Stata to close any log file that may already be open:
capture log close

* This opens the dataset you want to use
use "C:\Users\USER\Documents\MASTER ECONOMICS AND PUBLIC POLICY\2 TERM\Project\MICRO TOPIC\DO FILES\a_indresp.dta"

// SVYSET
		 svyset a_hidp [pweight=a_indpxus_xw], vce(linearized)

**CLEANING THE DATA 
*** Commonly used commands to modify and sort the data file ***
// To recode missing values (which are given SPECIFIC NEGATIVE VALUES -9 -8 -7 -2 -1) to missing values that Stata can recognise as missing value [.]:
recode a_hiqual_dv (-9/-1=.)
recode a_mlstat (-9/-1=.)
recode a_ukborn (-9/-1=.)
recode a_nqfhigh_dv (-9/-1=.)
recode a_scend (-9/-1=.)
recode a_jbhrs (-9/-1=.)
recode a_jbstat (-9/-1=.)
recode a_jbterm1 (-9/-1=.)
recode a_nchresp (-9/-1=.)
recode a_nchunder16 (-9/-1=.)
recode a_paedqf (-9/-1=.)
recode a_maedqf (-9/-1=.)
recode a_feend (-9/-1=.) 

        ///SAMPLE SELECTION 
**Eliminating non-response values: do not work, do not response or do not know, losing values, refusal.
drop if (a_jbsemp==-8)
recode a_jbsemp  (-9=.)
recode a_jbsemp  (-2=.)
recode a_jbsemp  (-1=.)


//Eliminating monthly labour income negative or cero values

drop if (a_fimnlabgrs_dv==0) | (a_fimnlabgrs_dv<400) | (a_fimnlabgrs_dv>11000)


**RENAMING VARIABLES
rename a_sex sex
rename a_dvage age
rename a_mlstat married
rename a_ukborn born
rename a_nqfhigh leveledu
rename a_scend ageleav
rename a_jbhrs hoursweek
rename a_jbstat ecoact
rename a_jbterm1 cujob
rename a_nchresp child
rename a_nchunder16 child2
rename a_paedqf fateduc
rename a_maedqf moeduc
rename a_fimnlabgrs_dv wagemonth
rename a_urban_dv urban
rename a_feend futleave

**CREATE NEW VARIABLES
gen age2 = age^2
gen wageanual = 12*wagemonth

**CREATING PROXY OF EXPERIENCE IN UK
gen expe = (age-ageleav)-5

      //Cleaning negative values transforming to missing values
recode expe  (-1=.)
recode expe  (-2=.)
recode expe  (-3=.)
recode expe  (-4=.)
recode expe  (-5=.)
      //generating experience2
gen expesq = expe^2

**CREATE DUMMYS: creating dummy for married,born in uk,level of qualification individual, education parents,
gen Dmarried= (married==2) if married~=.

gen Dbornuk= (born==1)| (born==2) | (born==3) | (born==4) if born~=.
      //variables for levels of education
gen Dhigher= (leveledu==1) if leveledu~=.
gen DFirstDeg= (leveledu==2) if leveledu~=.
gen DAlevel= (leveledu==7) if leveledu~=.
gen Dgcse= (leveledu==13) | (leveledu==11) if leveledu~=.
gen Durban= (urban==1) if urban~=.
gen Dchild= (child2>0) if child2~=.
gen Dfemale= (sex==2) if sex~=.

//Recode variables: 

recode hoursweek (75=.)
recode hoursweek (77=.)
recode hoursweek (78=.)
recode hoursweek (80=.)
recode hoursweek (84=.)
recode hoursweek (85=.)
recode hoursweek (90=.)
recode hoursweek (96=.)
recode hoursweek (97=.)

//dummy for parents education: 
gen Dfedu= (fateduc==3) | (fateduc==4) | (fateduc==5) if fateduc~=.
gen Dmedu= (moeduc==3) | (moeduc==4) | (moeduc==5) if moeduc~=.

gen Dfaedu= (fateduc==4) | (fateduc==5) if fateduc~=.
gen Dmoedu= (moeduc==4) | (moeduc==5) if moeduc~=.

//log of monthly wage 
gen logwagemonth = log(wagemonth) 
//construct logwageanual
gen logwageanual= log(wageanual)

*********1) SUMMARIZE STATISTICS
summarize logwageanual logwagemonth age age2 expe2 expesq2 hoursweek Dhigher DFirstDeg DAlevel Dgcse schooling Durban Dfemale Dmarried Dbornuk Dchild child2

//1.1) GENERATING REGRESSIONS 
svy: regress logwageanual age age2 expe2 expesq2 hoursweek Dhigher DFirstDeg DAlevel Dbornuk Dfemale Dmarried Dchild


*********2)to estimate the difference in returns to education if male or female 
svy: regress logwageanual age age2 expe2 expesq2 hoursweek Dhigher DFirstDeg DAlevel Dbornuk Dmarried Dchild if sex==2
svy: regress logwageanual age age2 expe2 expesq2 hoursweek Dhigher DFirstDeg DAlevel Dbornuk Dmarried Dchild if sex==1


**FIRST REGRESSION: 
reg logwageanual age age2 expe2 expesq2 hoursweek Dhigher DFirstDeg DAlevel Dfemale

reg logwageanual age age2 expe2 expesq2 hoursweek Dhigher DFirstDeg DAlevel Dfemale Dbornuk child2
reg logwageanual age age2 expe2 expesq2 hoursweek Dhigher DFirstDeg DAlevel Dfemale Dchild
reg logwageanual age age2 expe2 expesq2 hoursweek Dhigher DFirstDeg DAlevel Dfemale Dbornuk


//TRANSFORMING DUMMYS FOR YEARS OF SCHOOLING 
**1. generate years of schooling by proxi o higher level of qualification: 
generate schooling= leveledu
recode schooling (1=22)  
recode schooling (2=18)
recode schooling (3=17)
recode schooling (4=17)
recode schooling (5=16)
recode schooling (7=13) 
recode schooling (8=13)
recode schooling (9=13)
recode schooling (10=12)
recode schooling (11=12)
recode schooling (12=12) 
recode schooling (13=12)
recode schooling (14=14)
recode schooling (15=6)
recode schooling (16=6)
recode schooling (96=6)

recode schooling (1=22)  
recode schooling (2=18)
recode schooling (3=18)
recode schooling (4=17)
recode schooling (5=16)
recode schooling (7=13) 
recode schooling (8=13)
recode schooling (9=13)
recode schooling (10=13)
recode schooling (11=12)
recode schooling (12=12) 
recode schooling (13=12)
recode schooling (14=12)
recode schooling (15=6)
recode schooling (16=6)
recode schooling (96=6)


//1) OLS regression with schooling variable 
*Opcion1
svy: regress logwageanual age age2 expe2 expesq2 hoursweek schooling Dbornuk Dfemale Dmarried Dchild
*Opcion2
svy: regress logwageanual age age2 expe2 expesq2 hoursweek schooling Dfemale Dchild Dmarried


//INSTRUMENTAL VARIABLE

**first stage: you will have the predictions of the endogenous variable
reg schooling Dmoedu age age2 expe2 expesq2 hoursweek Dfemale Dbornuk Dmarried Dchild
local FS=_b[Dmoedu] 
**reduce form
reg logwageanual Dmoedu age age2 expe2 expesq2 hoursweek Dfaedu Dfemale Dbornuk Dmarried Dchild
local RF=_b[Dmoedu] 


di 'RF'/'FS'

* As a Regression: of IV with yearschool using leaving age of educa:no inclui child ni married 

 

///LAST STEP: RUN AFFTER THE EQUATION YOU SELECTED 
**DURBIN-W Hausman test on endogeneity: to prove that because is endogenous we need IV
estat endog

//to see if instruments are week
estat firststage

//just if have  2 instruments and 1 endogenous variable. 
estat overid


