* Step6-Merge VSR-CCD.sas - DO NOT AGGREGATE VSR DATA - this produced the June 14 version of VSRCCDyr;
libname fd 'C:\Work\NSLPmodel\Data\FinalDat';

%let sumccd=charter charte frelch frpmiss1 frpmiss2 level1-level5 levele1-levele5
            local1-local6 locale1-locale6 member migrnt nschs PK12 race1-race5 redlch st1
            st1e t1 t1e toteth totfrl E11 ELL LEAADM LEASUP SCH SCHSUP TOTTCH V29 V30 V37 V38 V90
            pop5to17 pov5to17;

%let sumvsr=a1-a10 b1-b10 c1-c10 enrollment enrollmentprovision
            freecateligapps freeeligcat freeeligincome freeelignotverified freeeligtot
            freeincomeapps freereportedprovision numnslpschools numprovisionschools
            puborpriv rpapps rpelig rpreportedprovision;

%macro doit(yr,vfile);
    *------------------------------------------------------------------------;
    ** READ CCD DATA, MERGE 3 CCD FILES WITH LINKFILE AND AGGREGATE AS NEEDED;
    *------------------------------------------------------------------------;
   data ccdlink; set fd.ccdlink; if matchid>0; proc sort; by leaid;

   data leas;    set fd.ccdus&yr;   rename member=member1 migrnt=migrnt1;
   data schools; set fd.ccdsus&yr;  rename member=member2 migrnt=migrnt2; ccdsch=1;
   data finance; set fd.ccdfs&yr;
   data saipe;   set ccd.saipe&yr;  keep leaid pop5to17 pov5to17;

   * merge with LEAs first so that variables common to all three files (fipst, leanmae, year) are taken from first file;
   data all; merge leas (in=a) schools finance ccdlink; by leaid; if a;

   data all needsaipe (drop=pop5to17 pov5to17); merge all (in=a) saipe (in=b); by leaid;
     if a & b  then output all;
     if a & ^b then output needsaipe;

     proc sort data=needsaipe; by conum;

    * use SAIPE data for the county if data are not available for the school district area;
   data needsaipe; merge needsaipe (in=a) ccd.saipec&yr (in=b); by conum; if a;
     if b then saipe='County'; else saipe='--';

   data all; set all needsaipe;
     if saipe='' then saipe='LEA';

     * these vars needed for diagnostics only;
     ccdlink=(matchid>0);
     if ccdlink=1 & aggCCD<1 then aggCCD=0;

    state =fipstate(fipst);
    member=max(member1,member2);
    migrnt=max(migrnt1,migrnt2);
                                    drop member1 member2 migrnt1 migrnt2;
    label
      MEMBER='CCD: TOTAL STUDENTS (max of CCD-US&CCD-SUS)'
      MIGRNT='CCD: MIGRANT STUDENTS (max of CCD-US&CCD-SUS)';

    if state in('--','AS','GU','PQ','PR','VI') then delete;

    nfile=sum(ccdus,ccdsch,ccdfs);

    format member 9.;

    proc freq; tables saipe / missing; title source of SAIPE data - &yr;

   /*
    *----------------------------------------------;
    **** PRINT DIAGNOSTICS *******;
    *----------------------------------------------;
    proc freq;
     tables nfile ccdlink;
     tables nfile*ccdlink nfile*aggccd ccdus*ccdsch*ccdfs / missing list;
     title &yr -- Match across 3 CCD files and to CCDlink (excludes territories);

    proc freq; weight member1;
     tables nfile ccdlink;
     tables nfile*ccdlink nfile*aggccd ccdus*ccdsch*ccdfs / missing list;
     title &yr -- Weighted match across 3 CCD files and to CCDlink (excludes territories);

    *----------------------------------------------;
    * LEAs in all 3 CCD files but NOT linked to VSR;
    *----------------------------------------------;
    data check; set all; if nfile=3 & ccdlink<1;
    proc sort; by state leaid;
    proc print; by state; id state;
     var leaid leaname ccdus ccdsch ccdfs member e11 matchid aggccd;
     title &yr -- LEAs in all 3 CCD files but NOT linked to VSR;

    *----------------------------------------------;
    * LEAs in 1 CCD file and linked to VSR;
    *----------------------------------------------;
    data check; set all; if nfile=1 & ccdlink=1;
    proc sort; by state leaid;
    proc print; by state; id state;
     var leaid leaname ccdus ccdsch ccdfs member e11 matchid aggccd;
     title &yr -- LEAs in 1 CCD file andlinked to VSR;
   */

   data all; set all; drop ccdsch ccdus ccdfs; if matchid>0;

  ** aggregate records as indicated in CCDlink file before merge with VSR;

   data agg noagg; set all;
     if aggCCD=1 then output agg; else output noagg;

     proc sort data=agg; by matchid;


     * do not keep leaid, leaname because we get the name from VSR file;
     proc means noprint data=agg; by matchid;
      id conum coname state year aggCCD;
      var &sumccd;
      output out=aggx sum=&sumccd;

   data ccd; set noagg aggx;
   run;

    *-------------------------------------------------------------------;
    ** READ VSR DATA, MERGE WITH LINKFILE AND do not AGGREGATE          ;
    ** For all years after 2008 - use sfaid08 - and check the nonmatches;
    *-------------------------------------------------------------------;
   data vsrlink; set fd.vsrlink (keep=state sfaname sfaid&yr matchid aggVSR);
     if matchid>0;
     rename sfaid&yr=sfaid;
    proc sort; by state sfaid;

   data vsr; set vsr.&vfile;
      drop MODIFIED_BY_USER_ID REGIONNAME REGIONNUM SANAME STATUS_ID;

     * must account for possible duplicate SFAIDs - sort by id and PUBLIC and drop private SFA in case of dups;

     public=(PUBORPRIV=1);

    proc sort; by state sfaid descending public;

   data vsr; set vsr; by state sfaid; if last.sfaid;

   data vsr; merge vsr (in=a) vsrlink; by state sfaid; if a;
     linked=(matchid>0);

    ** DIAGNOSTICS **;
    proc freq; weight enrollment;
      tables linked;
      title weighted by enrollment;
   run;

  /*
   data vsr nomatch; set vsr (drop=linked);
     if matchid>0 then output vsr; else output nomatch;  ***** examine nomatches for years after SY08-09 *******;

   data agg noagg; set vsr;
    if aggvsr=1 then output agg; else output noagg;

     proc sort data=agg; by matchid;

         ** NO AGGREGATION OCCURS IN 2008 - THIS ONLY APPLIES TO EARLIER YEARS WHICH ARE USED FOR TESTING ONLY - SO ID SFANAMES, ETC ;
     proc means noprint data=agg; by matchid;
      id state sfaid sfaname reportbegyear reportendyear typeofapp typeofverif aggVSR;
      var &sumvsr;
      output out=aggx sum=&sumvsr;

     proc transpose data=agg prefix=id out=list; by matchid; var id;

        proc print data=list;

     data aggx; merge aggx list; by matchid; drop _name_ _label_;

    data vsr; set noagg aggx;
  */
     proc sort data=ccd; by matchid;
     proc sort data=vsr; by matchid;

   data alldat; merge vsr (in=a) ccd (in=b); by matchid;
      if a;
      if b then matched=1; else matched=0;

   proc freq; weight enrollment; tables matched; title Percent of VSR enrollment in match file for yr &yr;
   proc freq; weight member;     tables matched; title Percent of CCD enrollment in match file for yr &yr;

   data fd.vsrccd&yr; set alldat; format _all_;
            drop matched _freq_ _type_ ccdlink;
    proc contents;
   run;
%mend;

%doit(08,verif0809b)
%doit(07,verif0708b)
%doit(06,verif0607b)
%doit(05,verif0506b)
%doit(04,verif0405b)
run;
