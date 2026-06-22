* Step3-ReadCCDschools.sas - read school data & aggregate by LEAID;
* Input:  CCD SUS data for SY2004-05, SY2005-06, SY2006-07;
* Output: ccdsus04, ccdsus05, ccdsus06;
*-----------------------------------------------------------------;
libname fd 'C:\Work\NSLPmodel\Data\FinalDat';
options obs=max;

****** Read CCD-Schools Data & Aggregate by LEA - file name corresponds to first year of SY ******************;

%let sumvars=race1-race5 toteth frelch redlch totfrl migrnt member
             frpmiss1 frpmiss2 level1-level5   local1-local6 charter t1 st1
                              levele1-levele5 locale1-locale6 charte t1e st1e;
%macro readsus (yr);
   data sch&yr; set sch&yr;

    keep LEAID FIPST LEANM&yr AM&yr ASIAN&yr BLACK&yr HISP&yr WHITE&yr FRELCH&yr MIGRNT&yr REDLCH&yr TOTETH&yr
         TOTFRL&yr MEMBER&yr CHARTR&yr LEVEL&yr STATUS&yr STITLI&yr TITLEI&yr
         LOCALE&yr ULOCAL&yr year;

      year=2000+ &yr*1.0;
      rename
       AM&yr    = race1
       ASIAN&yr = race2
       BLACK&yr = race3
       HISP&yr  = race4
       WHITE&yr = race5
       LEANM&yr = LEANAME
       FRELCH&yr= FRELCH
       MIGRNT&yr= MIGRNT
       LOCALE&yr= LOCAL
       ULOCAL&yr= ULOCAL
       REDLCH&yr= REDLCH
       TOTETH&yr= TOTETH
       TOTFRL&yr= TOTFRL
       MEMBER&yr= MEMBER
       CHARTR&yr= CHARTR
       LEVEL&yr = LEVEL
       STATUS&yr= STATUS
       STITLI&yr= STITLI
       TITLEI&yr= TITLEI;

   data sch&yr; set sch&yr;
    array allvars race1-race5 TOTETH FRELCH REDLCH TOTFRL MIGRNT MEMBER;

      do over allvars;
         if allvars=-1 then allvars=.M; else
         if allvars=-2 then allvars=.N; end;

      frpmiss1=(TOTFRL=.M);
      frpmiss2=(FRELCH=.M or REDLCH=.M);

      level1 = (level='1');
      level2 = (level='2');
      level3 = (level='3');
      level4 = (level='4');
      level5 = (level='N');

      if  local ^in('N','M') then do;  loc=local*1.0;  end;
      if ulocal ^in('N','M') then do; uloc=ulocal*1.0; end;

      loc=max(loc,uloc);

      local1=(loc=1 or loc=11);             * large city (pop> 250,000);
      local2=(loc=2 or loc=12 or loc=13);   * midsize city (pop<250,000);
      local3=(3<=loc<=4 or 21<=loc=23);     * suburb;
      local4=(5<=loc<=6 or 31<=loc<=33);    * town outside metro area;
      local5=(7<=loc<=8 or 41<=loc<=43);    * rural;
      local6=(local='N' or local='M');      * Not reported / missing;

      charter=(CHARTR='1');
      t1     =(TITLEI='1');
      st1    =(STITLI='1');        drop uloc local ulocal loc level;

       * weight by enrollment so that after aggregating by LEA, we can get percent of enrollment in these categories;
       array orig level1-level5   local1-local6   charter t1 st1;
       array new  levele1-levele5 locale1-locale6 charte t1e st1e;
          do over new; new=orig*member; end;

    proc freq;
          tables status frpmiss1 frpmiss2;
          tables chartr*charter / missing list;
          tables frpmiss1*frpmiss2 / missing list;
      title CCD_SUS &yr - check status before aggreagating schools for LEA;
    proc means;
    proc sort; by leaid;

    proc means noprint data=sch&yr; by leaid; id year fipst leaname; var &sumvars;
      output out=bylea&yr n=nschs sum=&sumvars;


  data fd.ccdsus&yr; set bylea&yr; drop _type_ _freq_;
   ccdsus=1;
   label
     CCDSUS      ='CCD-SUS: 1 if LEA in CCD-SUS'
     race1       ='CCD-SUS: # American Indian students'
     race2       ='CCD-SUS: # Asian students'
     race3       ='CCD-SUS: # Black students'
     race4       ='CCD-SUS: # Hispanic students'
     race5       ='CCD-SUS: # White students'
     toteth      ='CCD-SUS: # students w/ethnicity reported'
     frelch      ='CCD-SUS: # NSLP-free students'
     redlch      ='CCD-SUS: # NSLP-RP students'
     totfrl      ='CCD-SUS: # Total FRP students'
     nschs       ='CCD-SUS: # schools'
     frpmiss1    ='CCD-SUS: Total FRP missing'
     frpmiss2    ='CCD-SUS: Free or RP counts missing'
     level1      ='CCD-SUS: # Primary schools (LEVEL=1)'
     level2      ='CCD-SUS: # Middle schools (LEVEL=2)'
     level3      ='CCD-SUS: # High schools  (LEVEL=3)'
     level4      ='CCD-SUS: # Other schools (LEVEL=4)'
     level5      ='CCD-SUS: # Unknown schools (LEVEL=N)'
     local1      ='CCD-SUS: # Schools in large city (pop>250,000)'
     local2      ='CCD-SUS: # Schools in small city (pop<250,000)'
     local3      ='CCD-SUS: # Schools in suburbs'
     local4      ='CCD-SUS: # Schools in towns'
     local5      ='CCD-SUS: # Schools in rural areas'
     local6      ='CCD-SUS: # Schools in unknown areas'
     charter     ='CCD-SUS: # charter schools'
     t1          ='CCD-SUS: # Title 1 schools'
     st1         ='CCD-SUS: # Schoolwide Title 1 schools'
     levele1     ='CCD-SUS: # students in Primary schools (LEVEL=1)'
     levele2     ='CCD-SUS: # students in Middle schools (LEVEL=2)'
     levele3     ='CCD-SUS: # students in High schools  (LEVEL=3)'
     levele4     ='CCD-SUS: # students in Other schools (LEVEL=4)'
     levele5     ='CCD-SUS: # students in Unknown schools (LEVEL=N)'
     locale1     ='CCD-SUS: # students in large city schools (pop>250,000)'
     locale2     ='CCD-SUS: # students in small city schools (pop<250,000)'
     locale3     ='CCD-SUS: # students in suburbs schools'
     locale4     ='CCD-SUS: # students in towns schools'
     locale5     ='CCD-SUS: # students in rural areas schools'
     locale6     ='CCD-SUS: # students in in unknown areas schools'
     charte      ='CCD-SUS: # students in charter schools'
     t1e         ='CCD-SUS: # students in Title 1 schools'
     st1e        ='CCD-SUS: # students in Schoolwide Title 1 schools';

    proc sort; by leaid;
   proc means; title CCD-SUS data;
  run;
%mend;

 data sch04; set ccdsus.sc041bai ccdsus.sc041bkn ccdsus.sc041bow; %readsus(04)
 data sch05; set ccdsus.sc051aai ccdsus.sc051akn ccdsus.sc051aow; %readsus(05)
 data sch06; set ccdsus.sc061cai ccdsus.sc061ckn ccdsus.sc061cow; %readsus(06)
 data sch07; set ccdsus.sc071aai ccdsus.sc071akn ccdsus.sc071aow; %readsus(07)
 data sch08; set ccdsus.sc081b;                                   %readsus(08)
run;
