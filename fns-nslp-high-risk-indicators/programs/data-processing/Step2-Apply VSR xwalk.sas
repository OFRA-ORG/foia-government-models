* Step2-Apply VSR xwalk.sas;
* VSRxwalk is the results of final review/cleaning in Excel, converted to SAS by DBMSCopy;
* This program matches the crosswalk to the VERIF "A" files and output Verif "B" files with MatchID;
* MatchID provides a link to match public SFAs across years of VSR data;
* -------------------------------------------------------------------------------------------------;
libname x 'C:\Data\NSLPmodel\xwalk';

data x.VSRxwalk; set x.VSRxwalk;
 *California used new SFAIDs in SY2008-09. The prior year SFAIDs (transformed) matched CCD;
 * reassign MATCHID for CA so it matches CCD;

if state='CA' and length(sfaid)=4 then do;
    if sfaid4>'' and mod(sfaid4*1.0,1000000000)=1 then sfaid=left(int(sfaid4*1.0/1000000000)); else
    if sfaid3>'' and mod(sfaid3*1.0,1000000000)=1 then sfaid=left(int(sfaid3*1.0/1000000000)); else
    if sfaid2>'' and mod(sfaid2*1.0,1000000000)=1 then sfaid=left(int(sfaid2*1.0/1000000000)); else
    if sfaid1>'' and mod(sfaid1*1.0,1000000000)=1 then sfaid=left(int(sfaid1*1.0/1000000000)); end;
run;

data xwalk; set x.vsrxwalk (keep=state matchid enrproby sfaid1-sfaid5 id1-id5);

%macro doit;
 %do k=1 %to 5;
   data x&k; set xwalk (keep=state matchid enrproby sfaid&k id&k);
    rename sfaid&k=sfaid;
    if sfaid&k>' ' then output;
   proc sort; by state sfaid;
 %end;
%mend;

%doit

 proc sort data=t.verif0405a; by state sfaid;
 proc sort data=t.verif0506a; by state sfaid;
 proc sort data=t.verif0607a; by state sfaid;
 proc sort data=t.verif0708a; by state sfaid;
 proc sort data=t.verif0809a; by state sfaid;

%macro labelit(k);

   inxwalk=(matchid>" ");
   err=(id^=id&k)*inxwalk;

  if &k=3 & err=1 & state in('ME','WA') then do;
    matchid='';enrproby=.;sfaid3=''; id3=''; inxwalk=0; err=0;
  end;

   label state='State'
           matchid ='SFAID, to match across VSR yrs'
           SFAID   ='SFAID, original'
           ID      ='Record ID'
           enrproby='VSR Yr with enrollment variance';
  proc freq;
       tables inxwalk err / missing;
       tables inxwalk*puborpriv / missing nopercent;
 run;
%mend;

data t.verif0405b; merge t.verif0405a (in=a) x1; by state sfaid;if a;  title verif0405; %labelit(1);
data t.verif0506b; merge t.verif0506a (in=a) x2; by state sfaid;if a;  title verif0506; %labelit(2);
data t.verif0607b; merge t.verif0607a (in=a) x3; by state sfaid;if a;  title verif0607; %labelit(3);
data t.verif0708b; merge t.verif0708a (in=a) x4; by state sfaid;if a;  title verif0708; %labelit(4);
data t.verif0809b; merge t.verif0809a (in=a) x5; by state sfaid;if a;  title verif0809; %labelit(5);
run;

title err=1 if id does not match original;
proc print data=t.verif0405b; where err=1; var state sfaid matchid id id1 sfaname; run;
proc print data=t.verif0506b; where err=1; var state sfaid matchid id id2 sfaname; run;
proc print data=t.verif0607b; where err=1; var state sfaid matchid id id3 sfaname; run;
proc print data=t.verif0708b; where err=1; var state sfaid matchid id id4 sfaname; run;
proc print data=t.verif0809b; where err=1; var state sfaid matchid id id5 sfaname; run;
