* Step4-Read CCD-US-FS.sas - Read CCD-Universe Survey & CCD-Finance Survey;
* Save intermediate files with subsets of variables to C:\Work\NSLPmodel\Data\FinalDat (libname=FD);

libname fd 'C:\Work\NSLPmodel\Data\FinalDat';


*------------------------------------------------------------------------------------------;
** Read CCD-Universe Survey ******************;
*------------------------------------------------------------------------------------------;
%macro readus(yr,filenam);
  data ccdus&yr; set ccd.&filenam;
   keep leaid fipst ccdus year NAME&yr CONAME&yr LATCOD&yr LONCOD&yr
        BOUND&yr CONUM&yr  GSHI&yr GSLO&yr METMIC&yr MSC&yr TYPE&yr
        ELL&yr   MEMBER&yr MIGRNT&yr PK12&yr SCH&yr TOTTCH&yr LEAADM&yr LEASUP&yr SCHSUP&yr;

    ccdus=1;
    year=2000 + &yr*1.0;
  rename
     NAME&yr  = LEANAME
     CONAME&yr= CONAME
     LATCOD&yr= LATCOD
     LONCOD&yr= LONCOD
     BOUND&yr = BOUND
     CONUM&yr = CONUM
     GSHI&yr  = GSHI
     GSLO&yr  = GSLO
     METMIC&yr= METMIC
     MSC&yr   = MSC
     TYPE&yr  = TYPE
     ELL&yr   = ELL
     MEMBER&yr= MEMBER
     MIGRNT&yr= MIGRNT
     PK12&yr  = PK12
     SCH&yr   = SCH
     TOTTCH&yr= TOTTCH
     LEAADM&yr= LEAADM
     LEASUP&yr= LEASUP
     SCHSUP&yr= SCHSUP;

  data fd.ccdus&yr; set ccdus&yr;
    array allvars ELL MEMBER MIGRNT PK12 SCH TOTTCH LEAADM LEASUP SCHSUP;
      do over allvars;
        if allvars=-1 then allvars=.M; else
        if allvars=-2 then allvars=.N;
      end;
    if union='000' then union='';
     label
        LEANAME ='CCD:US: LEA name'
        CONAME  ='CCD:US: County'
        LATCOD  ='CCD:US: Latitude'
        LONCOD  ='CCD:US: Longitude'
        CCDUS   ='CCD:US: 1 if LEA in CCD-US'
        BOUND   ='CCD-US: OPERATIONAL STATUS CODE'
        CONUM   ='CCD-US: FIPS COUNTY NUMBER (FIPS ST+COUNTY)'
        ELL     ='CCD-US: ENGLISH LANGUAGE LEARNER STUDENTS'
        GSHI    ='CCD-US: HIGH GRADE OFFERED'
        GSLO    ='CCD-US: LOW GRADE OFFERED'
        MEMBER  ='CCD-US: TOTAL CALCULATED STUDENTS'
        METMIC  ='CCD-US: METRO/MICRO CODE'
        MIGRNT  ='CCD-US: MIGRANT STUDENTS'
        MSC     ='CCD-US: METRO STATUS CODE'
        PK12    ='CCD-US: TOTAL PK THRU 12 STUDENTS'
        SCH     ='CCD-US: NUMBER OF SCHOOLS (SCHOOL UNIV)'
        TOTTCH  ='CCD-US: TOTAL FTE TEACHERS (no implied decimal)'
        TYPE    ='CCD-US: AGENCY TYPE CODE'
        UNION   ='CCD-US: SUPERVISORY UNION NUMBER'
        LEAADM  ='CCD-US: LEA ADMINISTRATORS'
        LEASUP  ='CCD-US: LEA ADMINISTRATORS SUPPORT STAFF'
        SCHSUP  ='CCD-US: SCHOOL ADMINISTRATORS SUPPORT STAFF';

    proc sort; by leaid;
    proc means; title CCD-US data;
    proc freq; tables bound type  / missing;
  run;
%mend;
                   * CCD-US file name corresponds to first year of SY;
%readus(04,ag041c)
%readus(05,ag051a)
%readus(06,ag061c)
%readus(07,ag071a)
%readus(08,ag081a)

*------------------------------------------------------------------------------------------;
** Read CCD-Finance Survey (file name corresponds to last year of SY)    ******************;
*------------------------------------------------------------------------------------------;
%macro readfs(yr,filenam);
  data fd.ccdfs&yr; set ccd.&filenam (keep=LEAID FIPST NAME E11 V29 V30 V37 V38 V90);

   year=2000 + &yr*1.0;
   ccdfs=1;
   rename name=leaname;
    label
      FIPST  = 'CCD-FS: FIPS state code'
      E11    = 'CCD-FS: CURRENT EXP - FOOD SERVICES'
      V29    = 'CCD-FS: SALARIES - FOOD SERVICE'
      V30    = 'CCD-FS: EMPL BENEFITS - FOOD SERVICES'
      V37    = 'CCD-FS: SALARIES - SUPPORT SERVICES - BUSINESS/CENTRAL/OTHER'
      V38    = 'CCD-FS: EMPL BENEFITS - SUPPORT SERVICES - BUSINESS/CENTRAL/OTHER'
      V90    = 'CCD-FS: CURRENT EXP - SUPPORT SERVICES - BUSINESS/CENTRAL/OTHER'
      ccdfs  = 'CCD-FS: 1 if LEA in CCD-FS'
      ;
    proc means; title CCD-FS data;
    proc sort; by leaid;
   run;
%mend;

%readfs(04,sdf041b)
%readfs(05,sdf051c)
%readfs(06,sdf061a)
%readfs(07,sdf071a)
%readfs(08,sdf081a)
run;

*-------------------------------------------------------------------------------------------------------------------------;
