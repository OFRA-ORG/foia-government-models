* Step7-Merge LAUS.sas;
libname fd 'C:\Work\NSLPmodel\Data\FinalDat';
options compress=yes;

**** Merge LAUS to final VSR-CCD files and construct variables ************;

%macro doit(yr);  *** READ LAUS;

 data laus&yr (keep=conum unemp); set ccd.laus;
  if year-2000=&yr;
  proc sort; by conum;

 data laus&yr; set laus&yr; by conum; if first.conum;
  proc print data=laus&yr (obs=10); title year &yr;

 data vsrccd; set fd.vsrccd&yr;
   proc sort; by conum;

 data vsrccd; merge vsrccd (in=a) laus&yr; by conum; if a;
   proc means; var unemp matchid; title &yr;
   proc print; where matchid>0 & unemp<0;
    var leaid leaname conum unemp;


 data fd.final&yr; set vsrccd;
   format _all_; informat _all_; drop nfile;

   * CONSTRUCT MEASURES AS PERCENT OF ENROLLMENT OR PER STUDENT;
   array race race1-race5;
     if toteth>0 then do over race;
       race=race/toteth*100;              ** denominator=#students with ethnicity indicated;
     end;

   array vlst frelch redlch totfrl migrnt  levele1-levele5 locale1-locale6 charte t1e st1e;
   array pct  pFree  pRP    pFRP  pMigrant plevel1-plevel5 plocal1-plocal6 pchart pt1 pst1;

       if member>0 then do over vlst; pct=vlst/member*100; end;

   if pop5to17>0 then pov5to17=pov5to17/pop5to17*100; else pov5to17=0;


   label
     pov5to17 ='SAIPE: % kids age 5-17 in poverty'
     year     ='Year (beg of SY)'
     saipe    ='Source of SAIPE: County, LEA, missing'
     race1    ='CCD-SUS: % American Indian students'
     race2    ='CCD-SUS: % Asian students'
     race3    ='CCD-SUS: % Black students'
     race4    ='CCD-SUS: % Hispanic students'
     race5    ='CCD-SUS: % White students'
     pFree    ='CCD-SUS: % enrolled in NSLP-free'
     pRP      ='CCD-SUS: % enrolled in NSLP-RP'
     pFRP     ='CCD-SUS: % enrolled in NSLP-Free and RP'
     pMigrant ='CCD-SUS: % migrant students'
     plevel1  ='CCD-SUS: % enrollment in Primary schools (LEVEL=1)'
     plevel2  ='CCD-SUS: % enrollment in Middle schools (LEVEL=2)'
     plevel3  ='CCD-SUS: % enrollment in High schools  (LEVEL=3)'
     plevel4  ='CCD-SUS: % enrollment in Other schools (LEVEL=4)'
     plevel5  ='CCD-SUS: % enrollment in Unknown schools (LEVEL=N)'
     plocal1  ='CCD-SUS: % enrollment in large city (pop>250,000)'
     plocal2  ='CCD-SUS: % enrollment in small city (pop<250,000)'
     plocal3  ='CCD-SUS: % enrollment in suburbs'
     plocal4  ='CCD-SUS: % enrollment in towns'
     plocal5  ='CCD-SUS: % enrollment in rural areas'
     plocal6  ='CCD-SUS: % enrollment in unknown areas'
     pchart   ='CCD-SUS: % enrollment in charter schools'
     pt1      ='CCD-SUS: % enrollment in Title 1 schools'
     pst1     ='CCD-SUS: % enrollment in Schoolwide Title 1 schools';

    ** these vars are from CCD-FS;
   if member>0 then do;
      ExpFoodTot   = E11/member;
      ExpBusTot    = V90/member;
      ExpFoodLabor = sum(V29,V30)/member;
      ExpBusLabor  = sum(V37,V38)/member;
      Staff1       = LeaAdm/member;
      Staff2       = LeaSup/member;
      Staff3       = SchSup/member;
   end;
                  drop e11 v90 v29 v30 v37 v38 leaadm leasup schsup levele1-levele5 locale1-locale6 charte t1e st1e;
  label
     id          ='ID (record id) from VSR'
     id2         ='ID (record id) from VSR, if aggregated SFAs'
     id3         ='ID (record id) from VSR, if aggregated SFAs'
     matchid     ='VSR-CCD match ID'
     AggCCD      ='1 if aggregate of mult CCD records'
     AggVSR      ='1 if aggregate of mult VSR records'
     ExpFoodTot  ='CCD-FS: Current Exp - Food Services (E11), per student'
     ExpBusTot   ='CCD-FS: Current Exp - Support Services - Bus/Central/Oth (V90), per student'
     ExpFoodLabor='CCD-FS: Labor cost- Salaries (V29) & Bnfts (V30) -Food Services, per student'
     ExpBusLabor ='CCD-FS: Labor cost- Salaries (V37) & Bnfts (V38) -Bus/Central/Oth, per student'
     Staff1      ='CCD-US: LEA Administrators per student'
     Staff2      ='CCD-US: LEA Administrators Support Staff per student'
     Staff3      ='CCD-US: School Administrators Support Staff per student';

  * LABEL VSR DATA *;
   drop FIRSTYEAR_REPORTED;
   label
     A1   ='VSR: Free cat apps- no change'
     A2   ='VSR: Free cat kids- no change'
     A3   ='VSR: Free cat apps- change to RP'
     A4   ='VSR: Free cat kids- change to RP'
     A5   ='VSR: Free cat apps- change to paid'
     A6   ='VSR: Free cat kids- change to paid'
     A7   ='VSR: Free cat apps- no response'
     A8   ='VSR: Free cat kids- no response'
     A9   ='VSR: Free cat apps- reapproved by 2/15'
     A10  ='VSR: Free cat kids- reapproved by 2/15'
     B1   ='VSR: Free inc apps- no change'
     B2   ='VSR: Free inc kids- no change'
     B3   ='VSR: Free inc apps- change to RP'
     B4   ='VSR: Free inc kids- change to RP'
     B5   ='VSR: Free inc apps- change to paid'
     B6   ='VSR: Free inc kids- change to paid'
     B7   ='VSR: Free inc apps- no response'
     B8   ='VSR: Free inc kids- no response'
     B9   ='VSR: Free inc apps- reapproved by 2/15'
     B10  ='VSR: Free inc kids- reapproved by 2/15'
     C1   ='VSR: RP apps- no change'
     C2   ='VSR: RP kids- no change'
     C3   ='VSR: RP apps- change to free'
     C4   ='VSR: RP kids- change to free'
     C5   ='VSR: RP apps- change to paid'
     C6   ='VSR: RP kids- change to paid'
     C7   ='VSR: RP apps- no response'
     C8   ='VSR: RP kids- no response'
     C9   ='VSR: RP apps- reapproved by 2/15'
     C10  ='VSR: RP kids- reapproved by 2/15'
     ENROLLMENT           ='VSR: Enrollment w/access to NSLP'
     ENROLLMENTPROVISION  ='VSR: Enrollment w/access to NSLP-prov 2/3 schools'
     FREEELIGTOT          ='VSR: # Total free eligible students'
     FREEELIGCAT          ='VSR: # free elig kids based on categ apps'
     FREEELIGINCOME       ='VSR: # free elig kids based on income'
     FREEELIGNOTVERIFIED  ='VSR: # free elig kids not verified'
     FREEREPORTEDPROVISION='VSR: # free elig kids reportrd for provision 2/3'
     FREECATELIGAPPS      ='VSR: # Categ apps'
     FREEINCOMEAPPS       ='VSR: # Income apps'
     NUMNSLPSCHOOLS       ='VSR: # NSLP schools'
     NUMPROVISIONSCHOOLS  ='VSR: # Provision 2/3 schools'
     PUBORPRIV            ='VSR: Public (1), Private (2)'
     REPORTBEGYEAR        ='VSR: Beg of SY'
     REPORTENDYEAR        ='VSR: End of SY'
     RPAPPS               ='VSR: # RP eligible students'
     RPELIG               ='VSR: # RP applications'
     RPREPORTEDPROVISION  ='VSR: # RP kids reported for provision 2/3 schools'
     SFANAME              ='VSR: SFA name'
     TYPEOFAPP            ='VSR: Type of application used'
     TYPEOFVERIF          ='VSR: Type of verification';
  proc contents;
  proc means;
 run;
%mend;

%doit(08)
%doit(07)
%doit(06)
%doit(05)
%doit(04)
run;
