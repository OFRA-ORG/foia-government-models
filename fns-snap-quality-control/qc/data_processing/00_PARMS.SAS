
*** PARMS.SAS ***;
*** Parameter file for IQCS file development **;


*** Define current year to be processed ***;
%LET YEAR4=2023;
%LET YEAR2=23;

*** Define previous year ***;
%LET PREYR4=2022;
%LET PREYR2=22;

***DEFINE RANGE OF MONTHS TO KEEP***;
%LET FIRST_MO = 202210;
%LET LAST_MO = 202309;

*** Define maximum number of persons per household ***;
%LET maxpers=17;

OPTIONS /*SYMBOLGEN MPRINT*/ COMPRESS=YES;
OPTIONS FORMCHAR='B3C4DAC2BFC3C5B4C0C1D9'X ;
