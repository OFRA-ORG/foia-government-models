**********************************************************************************************************************

**********************************************************************************************************************

**********************************************************************************************************************
20May09 When subdividing activity codes into pseudo codes to the CARMM program, these are the places where they also
                will need to be added:
                        Below where %addActvCodeToMxMail is being called in (appends the psuedo code to the mix mail map)
                        Below where Proc format is formatting all the activity codes
                        In the excel file (ActivityCodesMaster 03AUG09.xls)
                        In the sas code of "mapclassRecast.sas"  NOTE not needed for delivery cost model, as not
                using DMM field.
            In the mixed mail/direct actv code match file "MxMailCode.csv" - add new activity codes for Std Mail
            Be sure to use the correct field in ActivityCodeMaster - NewCRAProd
                                select KL.*, ac.NewCRAProd as class2
*********************************************************************************************************************
REMOVE COMMENT ON SELECT CASING DEFINITION SECTION TO USE FOR CASING ONLY!!!
*********************************************************************************************************************;
resetline;
options MPRINT SYMBOLGEN;

libname IOCS  'D:\ExternalSSD\Folder 19\FY25\SAS\SASLib';
%let pathProg = D:\ExternalSSD\Folder 19\FY25\SAS\SASInputFiles;  
%let pathOut =  D:\ExternalSSD\Folder 19\FY25\SAS\KLTables;  
%let FY = 25;  
%let FYData = 25;

%macro assignRouteGroup();
    ***  The following statement assigns route code groups
    ***  so that reports can be created separately for
    ***  these groups.;

        *set rgroup for special runs, for delivery cost model (3 values);
        *Put Letter Route types in RGroup 1, SPR in RGroup 2, 99 in RGroup 3;

    if '71' <= route <= '83' then RGroup=1;
    else if '84' <= route <= '98' then RGroup=2;
    else RGroup=3;

%mend;


 data b;
 set IOCS.PRCPubCL25; 
 title1 'FY 20&FY. Qtr A CARMM System';
  /*********** The following variables are updated using Cluster dataset  ***********************************/
 /****************  Updated by Dan Zhang on Nov 16, 2020  **************************************************/
 	dollar = READCOST;   /*  Cost (in dollar) for each carrier reading based on cluster sampling method  */  
           act1st = ' ';
       act234 = '   ';
	F9252 = CRAFTGRP;    /*  Craft Subscript: 5=Full-Time Carrier; 6=Part-Time Carrier  */  
	f264 = WEIGHTCAG;    /*  CAG Group used in the Control Total Dollar: A-H  */ 
/***********************************************************************************************************/


   ***  This secion is used for Folder 19, Delivery Cost Model                                                  ****;
   ***  Subdivide ECR HD/SAT into DAL and non-DAL for use in Folder 19 Delivery Cost Model**;
   IF F262 in ('1317', '2317', '3317', '4317') then do;
      act1st = substr(F262,1,1);
      act234 = substr(F262,2,3);
      if  Q23E10    = 'N'  then act234 = '319'; ** Parent pc w/ no DAL;
         else
      if  (Q23J02    = 'B'  or
          Q23H03    = 'H') then do; ** WSH or MMS=EH marking eff FY25Q1;
		  /*(Q23J02    = 'A'  or
          Q23H03    = 'H') then do; ** WSH or MMS=EH marking; */

          if (q23B01    = 'Y' and substr(F262,1,1) ne '1') 
			 then act234 = '315'; ** DAL, for non-letters;
          else act234 = '317';
          end;
         else
      if (Q23J02    = 'A' or 
          Q23J02    = 'C' or
          Q23H03    = 'I') then do; **WSS or MMS=ES marking  eff FY25Q1;
          /*(Q23J02    = 'B'  or
          Q23H03    = 'I') then do; **WSS or MMS=ES marking; */

          if (Q23B01    = 'Y'  and substr(F262,1,1) ne '1') 
		  then act234 = '316'; ** DAL, for non-letters;
          else act234 = '318';
          end;
      F262 = act1st||act234;
   END;

        ** CARMM Edits;
        if f264='K' then f9252='9';
        if f261 ne '4';
        if dollar=0 and q01=' ' then delete;
        else do;
              if f260 in ('84', '89') then f260='87';
        end;
        if f9252 in ('5','6'); **carrier craft;
 run;

*Read in file that maps activity codes into mixed mail codes,
        and add all new activity codes added above;
%include "&pathProg\macMxMail_AMS.sas" / source2; /*GET MORE DETAILS IN THE LOG FOR THIS INCLUDE STATEMENT*/
%readMxMail("&pathProg\MxMailCodeFY25.csv",MxMailMap);  *for domestic CRA;



%addActvCodeToMxMail(MxMailMap,'1317','1315');
%addActvCodeToMxMail(MxMailMap,'1317','1316');
%addActvCodeToMxMail(MxMailMap,'1317','1318');
%addActvCodeToMxMail(MxMailMap,'1317','1319');
%addActvCodeToMxMail(MxMailMap,'2317','2315');
%addActvCodeToMxMail(MxMailMap,'2317','2316');
%addActvCodeToMxMail(MxMailMap,'2317','2318');
%addActvCodeToMxMail(MxMailMap,'2317','2319');
%addActvCodeToMxMail(MxMailMap,'3317','3315');
%addActvCodeToMxMail(MxMailMap,'3317','3316');
%addActvCodeToMxMail(MxMailMap,'3317','3318');
%addActvCodeToMxMail(MxMailMap,'3317','3319');
%addActvCodeToMxMail(MxMailMap,'4317','4315');
%addActvCodeToMxMail(MxMailMap,'4317','4316');
%addActvCodeToMxMail(MxMailMap,'4317','4318');
%addActvCodeToMxMail(MxMailMap,'4317','4319');


proc sort data=MxMailMap nodup;
        by dircode mixcode;
run;

data carr; set b;
  Craft=F9252;
  Actv=F262;
  *Actv=F9806; *mcz 10Aug08 use F9806 to distribute Int'l mixed mail;
  Route=F260;
  BF=F261;
  Fin=F263;
  Cag=F264;
  Cagfin=cag !! fin;

  ****  SELECT CASING DEFINITION *** ;
  *To restrict to subset that is casing remove comment from next line;
 if Q16F03A in ('A','B','C');  ** General casing (3 options) -- use for Delivery Cost Model inputs;

   if actv in ('5740', '5745', '5741') then actv = '5750';  /*as per Audrey Hu, 17Nov23*/
   if actv <= '0900' and actv not in ('0350','0560') then actgrp=1;
     else if '1020' <= actv <= '4910' or actv in ('0350','0560') then actgrp=2;
     else if '5300' <= actv <= '5750' then actgrp=3;
     else if '5040' <= actv <= '6720' then actgrp=4;
  TITLE1 "FY 20&fy. Qtr A CARMM SYSTEM";
run;

proc sql;
  create table smy as
  select cag, fin, actv, bf, sum(dollar) as dollar
  from carr
  group by cag, fin, actv, bf;
quit;

 data smyx;
    array bfx{4} bf1 - bf4;
    do i=1 to 4;
     set smy;
     by cag fin actv bf;
      if i=1 and bf='2' then i=i+1;
      if i=1 and bf='3' then i=i+2;
      if i=1 and bf='5' then i=i+3;
      if i=2 and bf='3' then i=i+1;
      if i=2 and bf='5' then i=i+2;
      if i=3 and bf='5' then i=i+1;
      bfx{i}=dollar;
      if last.actv then i=4;
    end;
     if bf1=. then bf1=0;
     if bf2=. then bf2=0;
     if bf3=. then bf3=0;
     if bf4=. then bf4=0;
     total=sum(of bf1-bf4);
     cagfin=cag !! fin;
     if actv <= '0900' and actv not in ('0350','0560') then actgrp=1;
       else if '1020' <= actv <= '4910' or actv in ('0350','0560') then actgrp=2;
       else if '5300' <= actv <= '5750' then actgrp=3;
       else if '5040' <= actv <= '6720' then actgrp=4;
        output;
 run;


 proc sort data=smyx;
       by cagfin actgrp actv;
 run;
    *----------------------------------------------------------;
    *                                                          ;
    * Dollar tally summary by cag fin route actv bf.  This     ;
    * summary file is used in next step for distributing       ;
    * mixed-mail.                                             ;
    *                                                          ;
    *----------------------------------------------------------;

proc sql;
  create table smy2 as
  select cag, fin, route, actv, bf, sum(dollar) as dollar
  from carr
  group by cag, fin, route, actv, bf;
quit;


 data smy2;
    set smy2;

*Use macro to assign route group;
        %assignRouteGroup;
    cagfin=cag !! fin;
 run;

 data out;
        array bff{4} dollar1-dollar4;
        do i=1 to 4;
         set smy2;
         by cag fin route actv bf;
          if i=1 and bf='2' then i=i+1;
          if i=1 and bf='3' then i=i+2;
          if i=1 and bf='5' then i=i+3;
          if i=2 and bf='3' then i=i+1;
          if i=2 and bf='5' then i=i+2;
          if i=3 and bf='5' then i=i+1;
          bff{i}=dollar;
          if last.actv then i=4;
        end;
         if dollar1=. then dollar1=0;
         if dollar2=. then dollar2=0;
         if dollar3=. then dollar3=0;
         if dollar4=. then dollar4=0;
         drop i cag fin _type_ _freq_ dollar bf;
        output;
 run;


   ***************************************************************;
   ****Note: this 2nd   part of the program is analogous to the **;
   ****      old program ALA860C0, which performed distribution **;
   ****      and produced various summary reports.              **;
   ***************************************************************;

 data out;
      set out;
      if '5300' <= actv <= '5750' then mixmail=1;
      else if '1000' <= actv <= '5000' or actv in ('0350','0560') then mixmail=0;
      dollart=sum(of dollar1 - dollar4);
 run;

    *--------------------------------------------------------------;
    * The following proc SUMMARY gets the preliminary results for  ;
    * Report 7  This corresponds to the old LIOCATT report ALA860P7;
    *--------------------------------------------------------------;
 proc sort data=out out=out7;
       by rgroup route actv;
       where mixmail ne 1;
 run;

 proc  summary data=out7 noprint;
       var dollar1 dollar2 dollar3 dollar4;
       output out=rpt7 sum=;
       by rgroup route actv;
     /****uncomment print if report is needed***
     proc print data=rpt7;
       id rgroup route ;
     * sum dollar1 dollar2 dollar3 dollar4;
       by rgroup route;
       title2 'Report 7';
      ******************************************/
 run;

    ***************************************************************;
    * Distribute mixed mail costs to direct mail activity codes.   ;
    ***************************************************************;

    *--------------------------------------------------------------;
    * Read the mixed-mail to direct mail code conversion file.     ;
    * This file tells which direct mail activity codes to associate;
    * with each mixed-mail activity code.                          ;
    *--------------------------------------------------------------;
        ;
Data mxmail;
set MxMailMap;
run;

 proc sort data=mxmail; by dircode;
 run;

 data dirmail (drop= i mixcode);
       array dir(8) $mix1 -mix8;
       do i=1 to 8;
          set mxmail;
          by dircode;
          actv=dircode;
          dir(i)=mixcode;
          if last.dircode then i=8;
       end;
       output;
 run;
    *--------------------------------------------------------------;
    * Direct mail cost summary by cagfin, route and activity       ;
    *--------------------------------------------------------------;
 proc sort data=out out=dist2;
         by cagfin route actv;
         where mixmail=0; * Contains only direct mail;
 run;

 proc summary data=dist2 noprint;
        var dollar1 dollar2 dollar3 dollar4 dollart;
        output out=dirdol
               sum=dollar1d dollar2d dollar3d dollar4d dollartd;
         by cagfin route actv;
        title2 'dirdol';
    * proc print data=dirdol;
 run;

    *--------------------------------------------------------------;
    * Merge direct mail cost with the conversion table to associate;
    * each direct mail activity code with mixed mail code that will;
    * get distribued cost.                                         ;
    *--------------------------------------------------------------;
 proc sort data=dirdol;
        by actv;
                run;
 proc sort data=dirmail;
        by actv;
 run;

 data dircode1 (drop=mix1-mix8 i);
         merge dirdol  (in=a)
               dirmail (in=b keep=actv mix1-mix8);
         by actv;
         source=a+2*b;
         if source=2 then delete; * delete activity codes that contain
                                    no cost;
         array dir(8) mix1-mix8;
         do i=1 to 8;
           mixcode=dir(i);
           if mixcode ne ' ' then output;
           if mixcode= ' ' then i=8;
          end;
    *proc print n;
 run;

    *--------------------------------------------------------------;
    * Mixed mail cost summary by cagfin, route and actv            ;
    *--------------------------------------------------------------;

 proc sort data=out out=dist1;
         by cagfin route actv;
         where mixmail=1; * Contains only mixed mails;
                 run;

 proc summary data=dist1 noprint;
        var dollar1 dollar2 dollar3 dollar4 dollart;
        output out=mxdol
               sum=dollar1i dollar2i dollar3i dollar4i dollarti;
         by cagfin route actv;
        title2 'mxdol';
        run;

    *--------------------------------------------------------------;
    * Attach summary mixed mail cost to the direct mail activity   ;
    * code.                                                        ;
    *--------------------------------------------------------------;

 data mxdol;
        set mxdol;
        rename actv=mixcode;
     *proc print data=mxdol;
         run;

 proc sort data=dircode1;
         by cagfin route mixcode;
                 run;
 proc sort data=mxdol;
         by cagfin route mixcode;
                 run;

 data final ;
         merge dircode1 (in=a) mxdol (in=b);
         by cagfin route mixcode;
         source=a+2*b;

     * proc print;
     * title2 'final';
       run;

    *--------------------------------------------------------------;
    * Calculate the total direct mail cost associated with         ;
    * each mixed mail code.                                        ;
    *--------------------------------------------------------------;

 proc sort data=final;
         by cagfin route mixcode;
                 run;

  proc summary data=final noprint;
        var dollar1d dollar2d dollar3d dollar4d dollartd;
         output out=total1
                sum=total1d total2d total3d total4d totaltd;
         by cagfin route mixcode;
         run;

    *--------------------------------------------------------------;
    * Distribute the mixed mail cost to direct mail activity codes ;
    *--------------------------------------------------------------;
       data distfile (drop=_type_ _freq_ source);
          merge final (in=a) total1 (in=b);
             by cagfin route mixcode;

          if total1d=0 then ratio1=.;
             else ratio1= dollar1d/total1d;
          dist_1=dollar1i *ratio1;

          if total2d=0 then ratio2=.;
             else ratio2= dollar2d/total2d;
          dist_2=dollar2i *ratio2;

          if total3d=0 then ratio3=.;
             else ratio3= dollar3d/total3d;
          dist_3=dollar3i *ratio3;

   ***  The following statements add the undistributed dollar
   ***  for basic functions 1 - 3 to last basic function;

          if (total1d =0 and dollar1i > 0)
             then dollar4i=dollar4i +dollar1i;
          if (total2d =0 and dollar2i > 0)
             then dollar4i=dollar4i +dollar2i;
          if (total3d =0 and dollar3i > 0)
             then dollar4i=dollar4i +dollar3i;

   ***  Note that the last basic function distribution is based on
   ***  the total cost (sum of four basic functions cost);

          if totaltd=0 then ratiot=.;
          else ratiot= dollartd/totaltd;
          dist_4=dollar4i *ratiot;

   ***  The following code assigns the undistributed mixed
   ***  dollars to the dist_0 cagegory;

       if actv=' ' then
           do;
              actv=mixcode;
              dist_0=dollarti;
           end;

   *Use macro to assign route group;
   %assignRouteGroup;


   * proc print data=distfile n;
run;

      *-----------------------------------------------------------;
      * Combine schedule K and L to produce K and L report        ;
      * (report 13). Schedule K is report 7 and schedule L is     ;
      * summary of the mixed mail distribution result (finout).  ;
      *-----------------------------------------------------------;

  proc sort data=distfile;
         by  rgroup route actv;
                 run;

  proc summary data=distfile noprint;
        var  dist_0 dist_1 dist_2 dist_3 dist_4;
        output out=finoutl sum=dollar0 dollar1 dollar2 dollar3 dollar4;
         by  rgroup route actv;
        run;

 *-----------------------------------------------------------;
 *  The following proc format classifies activity codes into ;
 *  categories for subsequent reports.                       ;
 *-----------------------------------------------------------;
 proc format ;
 value $actv
 /*SPECIAL SERVICES  */
         '0060'       ='055 Registered  '
         '0050'       ='051 Certified   '
         '0080'       ='054 Insured     '
         '0030'       ='052 COD         '
	
 /* OTHSERV INCL. BUSRPLY, RETRCPT, ADDRCORR, FORMS3547&3579 */
         '6020','6030'='074 PO Box    '
         '0090','0190',
         '0300','0100',
	     '0110','0140'				='058 Other Services'
 
 /* FIRST CLASS   */
         '1060'                     ='003 FC SP Letters  '
         '1020'                     ='004 FC SP Cards    '
         '1080','1081','1085','1086'='008 FC Prst Letters'
         '1022','1035','1040','1045'='009 FC Prst Cards  '
         '2060'						='014 FC SP Flats    '
         '2080','2081','2086'		='017 FC Prst Flats  '
         '3060','4060'				='143 FC Package Svc '
		 /*'3080','4080'				='020 FC Prst Parcels'*/
		 '3080','4080'				='143 FC Package Svc '
			

 /*PRIORITY MAIL  */
         '1160','2160','3160','4160'='148 Priority       '

 /* EXPRESS MAIL    */
            '1110','2110','3110','4110' ='140 Express    '

 /* PERIODICALS       */
            '1211','2211','3211','4211'='031 PER In County     '
            '1212','2212','3212','4212'='032 PER Outside County'

 /* STANDARD          */
            '1317'                     ='021 MM ECR HD Letters         '
            '2317','3317','4317'       ='022 MM ECR HD Flats/Parcels   '
			'1318'                     ='021 MM ECR SAT Letters        '
            '2318','3318','4318'       ='022 MM ECR SAT Flats/Parcels  '
            '1312','2312','3312','4312'='023 MM ECR Carrier Route      '
			'1320','2320','3320','4320'='024 MM EDDM - Retail          '
            '1340','1341','1345'       ='025 MM REG Letters            '
            '2340','2341','2345'       ='026 MM REG Flats              '
            '3340','4340'              ='027 MM REG Parcel             '            
            '3341','4341'              ='027 MM REG Parcel             '
			'1315'                     ='021 DAL Ltr WSH                '
			'2315','3315','4315'       ='021 DAL Flt/IPP/PAR WSH        '
			'1316'                     ='022 DAL Ltr WSS                '
			'2316','3316','4316'       ='022 DAL Flt/IPP/PAR WSS        '
			'1319','2319','3319','4319'='021 Parent No Dal HD/SAT       '

 /* PACKAGE          */
           '2402','3402','4402'        ='157 Ground Advantage HW 1-3lb '  
		   '2403','3403','4403'        ='158 Ground Advantage HW over3lb '
           '2405','3405','4405'        ='156 Ground Advantage LW        '
		   '2410','3410','4410'        ='145 PKG Retail Ground          '
           '2460','2465','2480','2495' ='042 PKG BPM Flats              ' 
           '3460','3465','3480','3495', 
           '4460','4465','4480','4495' ='043 PKG BPM Parcels            '
           '1420','1425','1430',		   
           '2420','2425','2430',		   
           '3420','3425','3430',		   
           '4420','4425','4430'        ='044 PKG Media                  '

 /* PARCEL SELECT and PS Lightweight        */
		   '2360','3360','4360',
           '2493','3493','4493'        ='151 PKG Parcel Select          '

 /* PARCEL RETURN SERVICE (PRS)          */
           '2475','3475','4475'        ='154 PKG Parcel Return (PRS)    '

/* PREMIUM FORWARDING SERVICE (PFS) 	*/
			'0350','1170','2170','3170','4170'='170 Prem. Forw. Svc'


/* GOVERNMENT MAIL   */
            '0560',
            '1510','2510','3510','4510'='085 USPS       '

 /* FREE              */
            '1910','2910','3910','4910'='086 Free       '

 /* INTERNATIONAL OUTBOUND AND INBOUND  */
            '1780','2780','3780','4780',
            '5081','6081','5082','6082',
            '0752','0757','0762','0767',
            '0770','0860','0862','0867'              ='185 International'

 /* OTHER CODES  */
            OTHER = '999 OTHER';
run;


 data KL (drop=_type_ _freq_);
         set rpt7 (in=a) finoutl (in=b);
         if a then source='K';
         if b then source='L';
         if dollar0 > 0 then
            do;   source='U';
                  dollar4=dollar0; *move the undistributed mixed mail
                                    cost to basic function OTHER;
            end;

         if dollar0=. and dollar1=. and dollar2=. and dollar3=. and
          dollar4=. then delete;

         total= sum (of dollar1 - dollar4);
                 if actv <= '0900' and actv not in ('0350','0560') then actgrp=1;
                else if '1020' <= actv <= '4910' or actv in ('0350','0560') then actgrp=2;
            else if '5300' <= actv <= '5750' then actgrp=3;
            else if '5020' <= actv <= '6720' then actgrp=4;**bls;
         **use format to classify activity to classes***;
         class=put(actv,$actv.);

         **assign labels for use in reports**;
         label
            dollar1 = 'OUTGOING'
            dollar2 = 'INCOMING'
            dollar3 = 'TRANSIT '
            dollar4 = 'OTHER   ';

                        **restrict to letter routes only**;
                        *if rgroup=1;
 run;


*Generate class variable from lookup table;
*import craft,activity code combinations used in delivery cost model;
PROC IMPORT OUT= WORK.ActivityCodesTable
            DATAFILE= "&pathprog\ActivityCodesMaster_FilingFY25.xlsx" 
            DBMS=XLSX REPLACE;
RUN;
proc sql;
create table KL2 as
  select KL.*, ac.NewCRAProd as class2
  from KL left join ActivityCodesTable as ac
  on KL.actv = ac.ActivityCode;
quit;

data checkClass; set KL2;
where class ne class2;
run;

***zzz;

data KL; set KL2;
if class2 = "" then class2 = "999 OTHER";
if substr(class2,1,3) = "185" then class = "185 International";
        else class = class2;
drop class2;
run;

proc sort data=kl;
by rgroup route actv;
run;

*Export KL table by route, for delivery cost model;
proc export data=kl
    outfile = "&pathOut\KLTable_Casing&FYData..xlsx"
        dbms = xlsx replace;
run;
