*Read-SAIPE counties.sas - need county data for LEAIDs that do not match the SAIPE district file;

%let in04="C:\Work\NSLPmodel\Data\SAIPE\est04ALL.xls";
%let in05="C:\Work\NSLPmodel\Data\SAIPE\est05ALL.xls";
%let in06="C:\Work\NSLPmodel\Data\SAIPE\est06ALL.xls";
%let in07="C:\Work\NSLPmodel\Data\SAIPE\est07ALL.xls";
%let in08="C:\Work\NSLPmodel\Data\SAIPE\est08ALL.xls";


%macro doit(yr);
  PROC IMPORT
       DATAFILE= &&in&yr
       OUT= WORK.SAIPEc&yr
       DBMS=EXCEL REPLACE;
       GETNAMES=NO; MIXED=YES; SCANTEXT=YES; USEDATE=YES; SCANTIME=YES;
  run;

  data ccd.saipec&yr; set saipec&yr; format _all_; informat _all_;

    if _n_<4 or f1*1.0<1 or f2='000' then delete;

    conum=compress(f1||f2);

    pov5to17= compress(translate(f17,'',','))*1.0;
    pop5to17= round(pov5to17/(f20/100));

    keep conum pop5to17 pov5to17;

    label conum    ='FIPS state&county'
          pov5to17 ='SAIPE: pop age 5 to 17 below poverty'
          pop5to17 ='SAIPE: pop age 5 to 17';

    proc sort; by conum;

    proc print data=ccd.saipec&yr (obs=10); title year &yr;
  run;
%mend;

%doit(04)
%doit(05)
%doit(06)
%doit(07)
%doit(08)
run;
