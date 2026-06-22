* Read-SAIPE.sas;
* Import SAIPE excel files & check correspondence of CCD-US and SAIPE;


proc import replace out=saipe04 datafile='C:\Work\NSLPmodel\Data\SAIPE\SaipeAllYrs.xls'; sheet='USSD04';
proc import replace out=saipe05 datafile='C:\Work\NSLPmodel\Data\SAIPE\SaipeAllYrs.xls'; sheet='USSD05';
proc import replace out=saipe06 datafile='C:\Work\NSLPmodel\Data\SAIPE\SaipeAllYrs.xls'; sheet='USSD06';
proc import replace out=saipe07 datafile='C:\Work\NSLPmodel\Data\SAIPE\SaipeAllYrs.xls'; sheet='USSD07';
proc import replace out=saipe08 datafile='C:\Work\NSLPmodel\Data\SAIPE\SaipeAllYrs.xls'; sheet='USSD08';

%macro fixit;
 leaid=compress(fips||distnum);

  drop distnum;
  format _all_; informat _all_;

  label
    LEAID   ='LEA ID'
    NAME    ='SAIPE: LEA name'
    pop     ='SAIPE: Total population'
    pop5to17='SAIPE: Population age 5-17'
    pov5to17='SAIPE: Number in poverty, age 5-17';

 proc sort; by leaid;
%mend;


data ccd.saipe04; set saipe04; %fixit
data ccd.saipe05; set saipe05; %fixit
data ccd.saipe06; set saipe06; %fixit
data ccd.saipe07; set saipe07; %fixit
data ccd.saipe08; set saipe08; %fixit proc contents;
run;



%macro doit(ccd,saipe);
  data ccd; set &ccd; if bound='1';
   proc sort; by leaid;

  data all; merge ccd (in=a) saipe (in=b); by leaid; if a;
   if a then ccd=1;
   if b then saipe=1;

  proc freq;
   tables ccd*saipe / missing list;
   tables type*saipe / missing nopercent nocol;
   title &ccd and &saipe: Match of CCD type=1,2 and Bound=1 to SAIPE;
  run;
%mend;

/*
data ccd04; set ccd.ag041c (keep=leaid agchrt04 bound04 type04 lstate04 name04
                               rename=(agchrt04=charter bound04=bound type04=type lstate04=state name04=ccdname));
 if type='7' & charter^='1' then type='8';
data ccd05; set ccd.ag051a (keep=leaid agchrt05 bound05 type05 lstate05 name05
                               rename=(agchrt05=charter bound05=bound type05=type lstate05=state name05=ccdname));
 if type='7' & charter^='1' then type='8';
data ccd06; set ccd.ag061c (keep=leaid agchrt06 bound06 type06 lstate06 name06
                               rename=(agchrt06=charter bound06=bound type06=type lstate06=state name06=ccdname));
 if type='7' & charter^='1' then type='8';
data ccd08; set ccd.ag08prelim (keep=leaid bound08 type08 lstate08 name08
                                   rename=(bound08=bound type08=type lstate08=state name08=ccdname)); charter=.;

%doit(ccd04,saipe04)
%doit(ccd05,saipe05)
%doit(ccd06,saipe06)
%doit(ccd08,saipe08)
*/

data ccd07; set ccd.ag071a (keep=leaid agchrt07 bound07 type07 lstate07 name07
                               rename=(agchrt07=charter bound07=bound type07=type lstate07=lstate name07=ccdname));

%doit(ccd07,saipe07)
run;


/*
data check2; set all;
 if ^(ccd & saipe);

proc freq;
 tables ccd*saipe / missing list;
 tables bound06 type06 agchrt06 / missprint;
 tables bound06*type06 / missprint;

proc sort; by lstate06 name06;
proc print;
 by lstate06; id lstate06;
 var name06 bound06 type06;

proc print data=check2; where saipe=1;
 var state leaid name pop pop5to17 pov5to17;
*/
run;
