********************************************************************************************************************************;
********************************************************************************************************************************;
/*

This file implements a hotdecking procedure to impute missing values for all variables. Most count/number variables are not
imputed directly. Rather a percentage variable is imputed which is later converted to a count. This is done to make
sure that imputed values for variables with missing values are consistent with other variables, such as district enrollment
or number of schools in the district.

*/
********************************************************************************************************************************;
********************************************************************************************************************************;

%let inlib    = D:\TRANSLATE\sas_data;
%let outlib   = D:\TRANSLATE\sas_data;
%let macrolib = D:\TRANSLATE\final_sas;

* A. LIBNAMES, OPTIONS, INCLUDES & FORMATS;

%include "&macrolib.\McrSerpentineSort.sas";
%include "&macrolib.\McrHotdeckImputation.sas";

options nocenter ls=170 mprint compress=yes nolabel;

libname in  "&inlib";
libname out "&outlib";

proc format;
     value $charmiss " "   = "missing"
                     OTHER = "non-missing";

     value nummiss   . - .Z = "missing"
                     OTHER  = "non-missing";
run;

* B. BATCH LISTS FOR HOT DECK IMPUTATION;

%LET batch1 = member numsch pmigrant pell staff1 staff2 staff3 tottch;

%LET batch2 = ExpFoodTot ExpBusTot ExpFoodLabor ExpBusLabor;

%LET batch3 = psch_level1 psch_level2 psch_level3 psch_level4 psch_level5 plevel3 plevel2 plevel1 plevel5 plevel4
              psch_charter pchart pst1 pt1 psch_st1 psch_t1 pFRP pFree pRP psch_local1 psch_local4 psch_local5
              psch_local2 psch_local3 psch_local6 plocal1 plocal2 plocal3 plocal4 plocal5 plocal6 race1 race2 race3 race4 race5;

%LET batch4 = unemp TYPEOFVERIF NUMNSLPSCHOOLS NUMPROVISIONSCHOOLS ENROLLMENT ENROLLMENTPROVISION PUBORPRIV FREEELIGTOT FREEELIGCAT
              FREEELIGINCOME FREEELIGNOTVERIFIED FREEREPORTEDPROVISION freecatappstu freeincappstu rpelig rpappstu rpprovstu randverif;

%LET batch5 = A1 A2 A3 A4 A5 A6 A7 A8 A9 A10
              B1 B2 B3 B4 B5 B6 B7 B8 B9 B10
              C1 C2 C3 C4 C5 C6 C7 C8 C9 C10;

%LET batches_14 = &batch1 &batch2 &batch3 &batch4;

* C. Additional Variable Creation Pre-Imputation;

data vsr_imputation (rename=(_matchid2=matchid2 _year=year)) vsr_nomatchid (rename=(_matchid2=matchid2 _year=year));
 length matchid2 $8. id $25.;
 set in.vsr_varprep (drop=id);

 p_free_red = .;

 if (enrollment>0) & (not missing(freeeligtot)) & (not missing(rpelig)) then p_free_red = (freeeligtot+rpelig)/enrollment;
 if p_free_red>1 then p_free_red=1;

 p_non_income = .;

 if (enrollment>0) & (not missing(freeelignotverified)) & (not missing(freeeligcat)) then p_non_income = (freeelignotverified+freeeligcat)/enrollment;
 if p_non_income>1 then p_non_income=1;

 /* Modify the MATCHID of a few districts to ensure unique records */

 if      matchid=60426  AND upcase(sfaname)="LOS ANGELES CO OFFICE OF EDUCATION"                then matchid2="60426-1";
 else if matchid=60426  AND upcase(sfaname)="LOS ANGELES COUNTY OFFICE OF EDUCATION"            then matchid2="60426-2";
 else if matchid=60426  AND upcase(sfaname)="LOS ANGELES UNIFIED SCHOOL DISTRICT FOOD SERVICES" then matchid2="60426-3";

 else if matchid=170616 AND upcase(sfaname)="PRAIRIEVIEW COMM CONS SCH DIST 192"                then matchid2="170616-1";
 else if matchid=170616 AND upcase(sfaname)="OGDEN COMM CONS SCH DIST 212"                      then matchid2="170616-2";

 else if matchid=460062 AND upcase(sfaname)="GEDDES COMMUNITY"                                  then matchid2="460062-1";
 else if matchid=460062 AND upcase(sfaname)="PLATTE  SCHOOL"                                    then matchid2="460062-2";

 else if matchid=460084 AND upcase(sfaname)="WAKONDA"                                           then matchid2="460084-1";
 else if matchid=460084 AND upcase(sfaname)="IRENE"                                             then matchid2="460084-2";

 else if matchid=500164 AND upcase(sfaname)="SOUTHWEST VERMONT SUPERVISORY UNION"               then matchid2="500164-1";
 else if matchid=500164 AND upcase(sfaname)="POWNAL SCHOOL DISTRICT"                            then matchid2="500164-2";
 else if matchid=500164 AND upcase(sfaname)="SHAFTSBURY ELEMENTARY"                             then matchid2="500164-3";

 else matchid2 = strip(put(matchid,6.));

 /* Create new Identifier Variable (MATCHID2 + YEAR + STATE) */

 id = catx("_",strip(put(matchid2,8.)),strip(put(year,4.)),strip(state));

 mark=1; /* Single Class Variable */

 /* Missing Flags for Sort Variables */

 imp_enrollment   = (missing(enrollment));
 imp_p_free_red   = (missing(p_free_red));
 imp_p_non_income = (missing(p_non_income));

 /* These are defined as character variables in later analyses */

 _year    = put(year,4.);
 _matchid2 = put(matchid2,8.);

 drop year matchid2;

 rename Applications_Subject_to_Verifica = Apps_Subject_to_Verifica;

 /* Split off the records without MATCHIDs before imputation */

 if missing(matchid) then output vsr_nomatchid;
 else                     output vsr_imputation;

run;

* Confirm Uniqueness of sort identifier;

proc sort data=vsr_imputation nodupkey;
 by id;
run;

proc print data=vsr_imputation (obs=40);
 var id matchid2 matchid year state sfaname;
 where index(matchid2,"-")>0;
 title1; title2 "CHECK ON ID CREATION"; title3;
run;

proc freq data=vsr_imputation;
 tables _CHARACTER_ / list missing;
 format _CHARACTER_ $charmiss.;
 title1; title2 "FREQUENCY OF CHARACTER VARIABLES IN FULL DATA SET (MISSING/NONMISSING)"; title3;
run;

/*****************************************************/
/*---------------------------------------------------*/
/*--- 2. Preparing the Input Data for Macro Calls ---*/
/*---------------------------------------------------*/
/*****************************************************/

/*--- A. Mean Imputation of Sorting Variables ---*/

%let col1 = enrollment;
%let col2 = p_free_red;
%let col3 = p_non_income;

%macro mean_impute(class,dataset);

  proc sql;
    CREATE TABLE temp AS SELECT *,
                                mean(&col1) as mean_&col1,
                                mean(&col2) as mean_&col2,
                                mean(&col3) as mean_&col3
                         FROM &dataset
                         GROUP BY &class
                         ORDER BY &class;
  quit;

  data &dataset;
    set temp;
    if missing(&col1) then &col1 = mean_&col1;
    if missing(&col2) then &col2 = mean_&col2;
    if missing(&col3) then &col3 = mean_&col3;
    drop mean_&col1 mean_&col2 mean_&col3;
  run;

%mend;

proc means data=vsr_imputation N NMISS MEAN MIN MAX;
 var enrollment p_free_red p_non_income;
 title1; title2 "BEFORE IMPUTATION"; title3;
run;

/* Step 1. Impute Missing Values with District Means */

%mean_impute(matchid2,vsr_imputation)

proc means data=vsr_imputation N NMISS MEAN MIN MAX;
 var enrollment p_free_red p_non_income;
 title1; title2 "DISTRICT-IMPUTED VALUES"; title3;
run;

/* Step 2. Impute Missing Values with State Means */

%mean_impute(state,vsr_imputation)

proc means data=vsr_imputation N NMISS MEAN MIN MAX;
 var enrollment p_free_red p_non_income;
 title1; title2 "STATE-IMPUTED VALUES"; title3;
run;

/*--- B. Creating Categorical Class Variables ---*/

data vsr_imputation;
 set vsr_imputation;

 /* Enrollment */

 enrollment_cat=.;

 if      enrollment<=1000  then enrollment_cat = 1;
 if 1000<enrollment<=5000  then enrollment_cat = 2;
 if 5000<enrollment<=10000 then enrollment_cat = 3;
 if      enrollment>10000  then enrollment_cat = 4;

 /* Free/Reduced Price */

 p_free_red_cat = .;

 if     p_free_red<=.25 then p_free_red_cat=1;
 if .25<p_free_red<=.50 then p_free_red_cat=2;
 if .50<p_free_red<=.75 then p_free_red_cat=3;
 if     p_free_red>.75  then p_free_red_cat=4;

run;

proc freq data=vsr_imputation;
 tables enrollment_cat p_free_red_cat randverif / list missing;
 title1; title2 "CATEGORICAL CLASS VARIABLES (FREQS)"; title3;
run;

proc means data=vsr_imputation N NMISS MEAN MIN MAX;
 var enrollment enrollment_cat p_free_red p_free_red_cat randverif;
 title1; title2 "CATEGORICAL CLASS VARIABLES (MEANS)"; title3;
run;

/****************************************************************/
/*--------------------------------------------------------------*/
/*--- 3. SERPENTINE SORT AND HOT DECK IMPUTATION MACRO CALLS ---*/
/*--------------------------------------------------------------*/
/****************************************************************/

/*--- A. Running Serpentine Sort - BATCHES 1-4 ---*/

%LET ORIGFILE = vsr_imputation;                              /*input filename */
%LET OUTFILE  = vsr_imputation_serp;                         /*output filename */
%LET IDVARS   = id;                                          /*identifying variable(s) */
%LET CLASS    = mark;                                        /*classing variable(s) */
%LET VARS     = enrollment_cat p_free_red_cat p_non_income;  /*sorting variables, listed in desired sort order */
%LET MAXV     = 4 4 1;                                       /*maximum value for each sorting variable */
%LET MISSVAR  = &batches_14;                                 /* the set which is a union of all batch imputations */

/*******/
%serpsort
/*******/

/*--- B. Running Hot Deck Imputation - BATCHES 1-4 ---*/

%LET INFILE=vsr_imputation_serp;
%LET SORTVAR=sortvar;
%LET MVALUE=(.,.M,.D,.R,.N);
%LET DIRECTN=both;
%LET ID=ID;
%LET CLASS=mark;
%LET LSTCLASS=mark;

%macro loop_hotdeck;

  %do i = 1 %to 4;

    %let missvar = &&batch&i;
    %let hotfile = hot_&i;

    %HOTDECK(printfreq=0);

  %end;

%mend;

%loop_hotdeck

data imputed_14;
  merge hot_1 hot_2 hot_3 hot_4;
  by id;
run;

/*--- C. Merging the Imputed Version of RANDVERIF back onto COMBINED_REVISED ---*/

proc sort data=imputed_14;     by id; run;
proc sort data=vsr_imputation; by id; run;

data batch5;
 merge vsr_imputation (in=a) imputed_14 (in=b);
 by id;
 keep id comb imp enrollment_cat p_free_red_cat p_non_income randverif_h &batch5;
 rename randverif_h = randverif;
run;

/*--- D. Running Serpentine Sort - BATCH 5 ---*/

%LET ORIGFILE = batch5;                                      /*input filename */
%LET OUTFILE  = batch5_serp;                                 /*output filename */
%LET IDVARS   = id;                                          /*identifying variable(s) */
%LET CLASS    = randverif;                                   /*classing variable(s) */
%LET VARS     = enrollment_cat p_free_red_cat p_non_income;  /*sorting variables, listed in desired sort order */
%LET MAXV     = 4 4 1;                                       /*maximum value for each sorting variable */
%LET MISSVAR  = &batch5;                                     /*the set which is a union of all batch imputations */

/*******/
%serpsort
/*******/

/*--- E. Running Hot Deck Imputation - BATCH 5 ---*/

%LET INFILE=batch5_serp;
%LET SORTVAR=sortvar;
%LET MVALUE=(.,.M,.D,.R,.N);
%LET DIRECTN=both;
%LET ID=ID;
%LET CLASS=randverif;
%LET LSTCLASS=randverif;
%LET MISSVAR=&batch5;
%LET HOTFILE=hot_5;

%HOTDECK(printfreq=0);

/*--- F. Merging Imputed Variables from Batches 1-4 and Batch 5 Together ---*/

data imputed;
 merge hot_1 hot_2 hot_3 hot_4 hot_5;
 by id;
run;

/*--- G. Merging Imputed Variables back with Original Variables ---*/

data vsr_imputation;
 merge imputed vsr_imputation;
 by id;
run;

/*--- H. Bringing Back in Missing MATCHID Records ---*/

data out.vsr_imputation;
 set vsr_imputation
     vsr_nomatchid;
run;

proc means data=out.vsr_imputation N NMISS MEAN;
run;

proc contents data=out.vsr_imputation;
run;

ENDSAS;














