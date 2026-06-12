options ps=79 ls=132;
***********************************************************************************************
* sas2bin.SAS - CREATE STANDARD BINARY FILE FROM 20xx QC SAS FILE.
*
*
* Notes:    The header file MATHPC.hdr, which is to be updated after this program is run, 
*           must match the names and sequence of variables written here.
*
* INFILE:   QCFY20xx.SAS7BDAT
* OUTFILE:  MATHPC.BIN
**********************************************************************************************;

*OPTIONS OBS = 100;

LIBNAME IN  '...\400_QC_File\Data';
  
%INCLUDE "../../data_processing/00_PARMS.SAS"  /SOURCE2;

proc freq data= IN.QCFY&year4 (keep=SEX1 sex2);
tables SEX1 sex2/missprint list;
run;

proc contents data= IN.QCFY&year4 varnum;
run;


DATA _NULL_;
   LENGTH DEFAULT=3;

   HHBYTES=300;   **** Bytes output per Household ****;
   PPBYTES=180;   **** Bytes output per Person    ****;

   SET IN.QCFY&year4 END=EOF;

   /** Set 3 dummy variables to make compatable with minimodel **/
   CASHOT=0;
   FTSTUD=0;
   FSPART=1;

   /*** THE FOLLOWING ARRAYS CONTAIN PERSON LEVEL VARIABLES ***/

   ARRAY AGE    (&maxpers.) AGE1-AGE&maxpers.;
   ARRAY CTZN   (&maxpers.) CTZN1-CTZN&maxpers.;
   ARRAY EMPRG  (&maxpers.) EMPRG1-EMPRG&maxpers.;
   ARRAY FSAFIL (&maxpers.) FSAFIL1-FSAFIL&maxpers.;
   ARRAY RACETH (&maxpers.) RACETH1-RACETH&maxpers.;
   ARRAY SEX    (&maxpers.) SEX1-SEX&maxpers.;
   ARRAY REL    (&maxpers.) REL1-REL&maxpers.;
   ARRAY YRSED  (&maxpers.) YRSED1-YRSED&maxpers.;
   ARRAY TANF   (&maxpers.) TANF1-TANF&maxpers.;

   ARRAY CONT   (&maxpers.) CONT1-CONT&maxpers.;
   ARRAY DEEM   (&maxpers.) DEEM1-DEEM&maxpers.;
   ARRAY GA     (&maxpers.) GA1-GA&maxpers.;
   ARRAY EDLOAN (&maxpers.) EDLOAN1-EDLOAN&maxpers.;
   ARRAY OTHERN (&maxpers.) OTHERN1-OTHERN&maxpers.;
   ARRAY OTHGOV (&maxpers.) OTHGOV1-OTHGOV&maxpers.;
   ARRAY OTHUN  (&maxpers.) OTHUN1-OTHUN&maxpers.;
   ARRAY SOCSEC (&maxpers.) SOCSEC1-SOCSEC&maxpers.;
   ARRAY SLFEMP (&maxpers.) SLFEMP1-SLFEMP&maxpers.;

   ARRAY SSI    (&maxpers.) SSI1-SSI&maxpers.;
   ARRAY CSUPRT (&maxpers.) CSUPRT1-CSUPRT&maxpers.;
   ARRAY UNEMP  (&maxpers.) UNEMP1-UNEMP&maxpers.;
   ARRAY VET    (&maxpers.) VET1-VET&maxpers.;
   ARRAY WAGES  (&maxpers.) WAGES1-WAGES&maxpers.;
   ARRAY WCOMP  (&maxpers.) WCOMP1-WCOMP&maxpers.;
   ARRAY DIVER  (&maxpers.) DIVER1-DIVER&maxpers.;
   ARRAY ENERGY (&maxpers.) ENERGY1-ENERGY&maxpers.;
   ARRAY EITC   (&maxpers.) EITC1-EITC&maxpers.;       
   ARRAY WGESUP (&maxpers.) WGESUP1-WGESUP&maxpers.;   
   ARRAY FSUN   (&maxpers.) FSUN1-FSUN&maxpers.;

   ARRAY WRKREG (&maxpers.) WRKREG1-WRKREG&maxpers.;   
   ARRAY WRKFAR (&maxpers.) WRKFAR1-WRKFAR&maxpers.;   
   ARRAY ABWDST (&maxpers.) ABWDST1-ABWDST&maxpers.;   
   ARRAY DPCOST (&maxpers.) DPCOST1-DPCOST&maxpers.;   
   ARRAY EMPSTA (&maxpers.) EMPSTA1-EMPSTA&maxpers.;   
   ARRAY EMPSTB (&maxpers.) EMPSTB1-EMPSTB&maxpers.;   

   ARRAY NDISCA (&maxpers.) NDISCA1-NDISCA&maxpers.;   
   ARRAY DIS    (&maxpers.) DIS1-DIS&maxpers.;         
   ARRAY FOSTER (&maxpers.) FOSTER1-FOSTER&maxpers.;   
   ARRAY WORK   (&maxpers.) WORK1-WORK&maxpers.;       


  /*** RECODE ANY SAS MISSINGS SO WE CAN USE IB1 FORMAT ***/
  ARRAY M _NUMERIC_ ;
  DO OVER M ;
    IF       M EQ .   THEN M = -1;
    ELSE IF  M EQ .A  THEN M = -2;
    ELSE IF  M EQ .B  THEN M = -3;
    ELSE IF  M EQ .C  THEN M = -4;
    ELSE IF  M EQ .D  THEN M = -5;
    ELSE IF  M EQ .E  THEN M = -6;


    ELSE IF  M <= .Z  THEN DO;
       PUT 'INVALID MISSING CODE AT ' HHLDNO=;
       STOP;
    END;

  END ;

  *** THE FOLLOWING MISSINGS ARE SET TO ZERO (ALL BUT MFIP & CAP HOUSEHOLDS)***;
    IF MN_FIP = 0 AND SSI_CAP in (0, 4) THEN DO;  
    IF FSDEPDED < 0 THEN FSDEPDED = 0;
    IF FSMEDEXP < 0 THEN FSMEDEXP = 0;
    IF FSCSDED < 0 THEN FSCSDED = 0;
  END;

  FILE 'MATHPC.BIN' RECFM=N;

  FILLER = 0 ;
  CTFMHH = 0;  /* DUMMY VARIABLE FOR STANDARD MATH HEADER COMPATIBILITY */
  RECTYP = 1;  /* HOUSEHOLD RECORD */

  /*** WRITE OUT BINARY HH/PER BINARY FILE - PAD   ***/
  /*** RECORDS OUT TO 4 BYTE BOUNDARIES USING FILLERS ***/

  PUT

     /*** FIXED HH VARS ***/
     RECTYP        1.
     CTFMHH        IB1.
     CTPRHH        IB1.
     FILLER        IB1.

     /*** 4-byte real HH VARS ***/
     ERN_INC_DED_PCT  FLOAT4.  
     FYWGT            FLOAT4.  
	 HWGT             FLOAT4.
     FSBENSUPP        FLOAT4.  
     /*** 8-byte integer HH VARS ***/
     HHLDNO        IB8.
     /*** 4-byte integer HH VARS ***/
     AMTADJ        IB4.  
     AMTERR        IB4.
     ASSLIM        IB4.  
     BENMAX        IB4.
     COUNTYCD      IB4.  
     BENFIX        IB4.  
     EXCL_FSCSDED  IB4.  
     FSCONT        IB4.
     FSCSDED       IB4.  
     FSCSEXP       IB4.
     FSCSUPRT      IB4.
     FSDEEM        IB4.
     FSDEPDED      IB4.  
     FSDEPDE2      IB4.  
     FSDIVER       IB4.  
     FSEDLOAN      IB4.
     FSENERGY      IB4.  
     FSERNDE2      IB4.  
     FSMEDDE2      IB4. 
     FSMEDEXP      IB4.
     FSOTHERN      IB4.
     FSOTHGOV      IB4.
     FSOTHUN       IB4.
     FSSLFEMP      IB4.
     FSSLTDE2      IB4.  
     FSSLTEXP      IB4.
     FSSOCSEC      IB4.
     FSSTDDE2      IB4.  
     FSTOTDE2      IB4.  
     FSUNEARN      IB4. 
     FSUNEMP       IB4.
     FSVEHAST      IB4.
     FSVET         IB4.
     FSWAGES       IB4.
     FSWCOMP       IB4.
     FSWGESUP      IB4.  
     GROSSCRN      IB4.  
     HOMELESS_DED  IB4.  
     LIQRESOR      IB4.
     LOCALCOD      IB4.
     MINIMUM_BEN   IB4.
     NETSCRN       IB4.
     OTHNLRES      IB4.
     RAWBEN        IB4.
     RAWERND       IB4.
     RAWGROSS      IB4.
     RAWLQRES      IB4.
     RAWNET        IB4.
     RCNTACTN      IB4.
     REALPROP      IB4.
     RENT          IB4.  
     REVNUM        IB4.
     SHELCAP       IB4.
     SHELDED       IB4.  
     TPOV          IB4.
     UTIL          IB4.  
     YRMONTH       IB4.
     FSFOSTER      IB4.  

     /*** 1-byte integer HH VARS ***/
     ACTNTYPE      IB1.
     AK_AREA       IB1.  
     ALLADJ        IB1.  
     AUTHREP       IB1.
     CASE          IB1.
     CAT_ELIG      IB1.
     CERTHHSZ      IB1.
     CERTMTH       IB1.
     EXPEDSER      IB1.
     FSNONCIT      IB1.  
     HOMEDED       IB1.  
     LASTCERT      IB1.
     MN_FIP        IB1.
     PURE_PA       IB1.  
     RAWHSIZE      IB1.
     REGION        IB1.
     REGIONCD      IB1.
     REP_SYS       IB1.  
     SSI_CAP       IB1.
     STATE         IB1.
     STATUS        IB1.
     STRATUM       IB1.
     SUA1          IB1.  
     SUA2          IB1.  
     TANF_IND      IB1.
     URBRUR        IB1.
     VEHICLEA      IB1.  
     VEHICLEB      IB1.  
     WRK_POOR      IB1.
     MED_DED_DEMO  IB1.
     FSNDISCA      IB1.  

     COMPOSITION   IB1.  
     NONCIT_HEAD   IB1.  
     FSNDIS        IB1.  

     FSDIS         IB1.  
     FSELDER       IB1.  
     FSKID         IB1.  
     SUPP_BEN      IB1.  

     /* Adjust number of 1-byte household-level fillers to make total length a multiple of 4 */
     FILLER        IB1.
     FILLER        IB1.
     ;


   OUTHH +1;
   RECTYP = 3;     /*** PERSON RECORD ***/

   DO I = 1 TO CTPRHH;

      SEEDP = ROUND(RANUNI(740)*10000000, 1);


     /*** WRITE OUT ALL PERSONS IN THE HOUSEHOLD ***/
       OUTPER + 1;
       PUT

       /*** FIXED PER VARS ***/
       RECTYP       1.
       FILLER       IB1.
       FILLER       IB1.
       FILLER       IB1.


       /*** 4-byte integer PER VARS ***/
       CONT   (I)       IB4.
       CSUPRT (I)       IB4.
       DEEM   (I)       IB4.
       DIVER  (I)       IB4.      
       DPCOST (I)       IB4.      
       EDLOAN (I)       IB4.
       EITC   (I)       IB4.
       ENERGY (I)       IB4.      
       FOSTER (I)       IB4.      
       GA     (I)       IB4.
       OTHERN (I)       IB4.
       OTHGOV (I)       IB4.
       OTHUN  (I)       IB4.
       SEEDP            IB4.      
       SLFEMP (I)       IB4.
       SOCSEC (I)       IB4.
       SSI    (I)       IB4.
       TANF   (I)       IB4.
       UNEMP  (I)       IB4.
       VET    (I)       IB4.
       WAGES  (I)       IB4.
       WCOMP  (I)       IB4.
       WGESUP (I)       IB4.     
       ;

       IF I = FSUN(I) THEN PUT

          FSASSET       IB4.
          FSBEN         IB4.
          FSEARN        IB4.
          FSERNDED      IB4.
          FSGA          IB4.
          FSGRINC       IB4.
          FSMEDDED      IB4.
          FSNETINC      IB4.
          FSSLTDED      IB4.
          FSSTDDED      IB4.
          FSSSI         IB4.
          FSTANF        IB4.
          FSTOTDED      IB4.
          ;

       ELSE  PUT

          FILLER        IB4.
          FILLER        IB4.
          FILLER        IB4.
          FILLER        IB4.
          FILLER        IB4.
          FILLER        IB4.
          FILLER        IB4.
          FILLER        IB4.
          FILLER        IB4.
          FILLER        IB4.
          FILLER        IB4.
          FILLER        IB4.
          FILLER        IB4.
          ;

       /*** 1-byte PER VARS ***/
       PUT

       ABWDST (I)       IB1.    
       AGE    (I)       IB1.
       CTZN   (I)       IB1.
       EMPRG  (I)       IB1.
       EMPSTA (I)       IB1.   
       EMPSTB (I)       IB1.   
       FSAFIL (I)       IB1.   
       FSUN   (I)       IB1.
       RACETH (I)       IB1.
       REL    (I)       IB1.
       SEX    (I)       IB1.
       WRKREG (I)       IB1.   
       YRSED  (I)       IB1.
       NDISCA (I)       IB1.   
       DIS    (I)       IB1.  
       WORK   (I)       IB1.  
       ;

       IF I = FSUN(I) THEN PUT

          CASHOT        IB1.  
          FSASTEST      IB1.  
          FSGRTEST      IB1.  
          FSMINBEN      IB1.
          FSNELDER      IB1.
          FSNETEST      IB1.  
          FSNGMOM       IB1.
          FSNK0T4       IB1.
          FSNK5T17      IB1.
          FSNKID        IB1.
          FSPART        IB1.  
          FSUSIZE       IB1.
          FTSTUD        IB1.  
      ;

       ELSE PUT
          FILLER        IB1.
          FILLER        IB1.
          FILLER        IB1.
          FILLER        IB1.
          FILLER        IB1.
          FILLER        IB1.
          FILLER        IB1.
          FILLER        IB1.
          FILLER        IB1.
          FILLER        IB1.
          FILLER        IB1.
          FILLER        IB1.
          FILLER        IB1.
          ;

      /* Adjust number of 1-byte person-level fillers to make total length a multiple of 4 */
       PUT
          FILLER       IB1.
          FILLER       IB1.
          FILLER       IB1.
          ;


   END;      /* END OF DO LOOP */

   IF EOF THEN DO;
      FILE LOG;
      PUT ' ';
      PUT ' ';
      PUT ' ';
      PUT 'AT EOF:          _N_       = ' _N_ '.';
      PUT '  HOUSEHOLDS WRITTEN       = ' OUTHH '.';
      PUT '  PERSONS WRITTEN          = ' OUTPER '.';


   ALLBYTES=(OUTHH*HHBYTES)+(OUTPER*PPBYTES);

      PUT;
      PUT "File MATHPC.BIN should be " ALLBYTES comma12. " bytes";
      PUT;


   END;

RUN;
