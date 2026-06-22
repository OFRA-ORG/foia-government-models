*======================================================================;
* Hotdeck Macro - Performs Hotdeck Imputation
*======================================================================;
*
* Originally Developed by Barbara Lepidus Carlson and Linda S. Bandeh
* Mathematica Policy Research, Inc., Princeton, New Jersey
*                     February 1995
* Modified:  9/13/2004 by Chris Rankin
*            added imputation for multiple items,
*            code to ensure that no donor is used more
*            than three times
*Modified: Mar/02/05 by Xiaojing Lin for HCSDB project.
*
* A Bloomenthal:  Oct 2010
        /*   fixed syntax errors in BOTH section - still not tested
             added &printfreq parameter to deactivate Proc Freq (to not have so much output if macro is invoked repeatedly)
             removed the single reference to &ORIGVAR
             made loop parameters consistent in renaming section */
*
* Parameter Description:
*   INFILE= input filename   - resultant file from serpentine sort macro
*  HOTFILE= output filename  - always hot for nsrcg
*       ID= identifying variable(s)
*    CLASS= classing variable(s)
* LSTCLASS= last classing variable
*  SORTVAR= sorting variable - always sortvar for nsrcg
*  MISSVAR= variable(s) to be imputed
*  ORIGVAR= original names of vars to be imputed (when preliminary
*           imputation done prior to hot deck imputation)
*   MVALUE= value considered to be missing for all MISSVAR variables
*           - can be set as a global value (.,.M) for nsrcg)
*
*  DIRECTN= user option:  select PREV, NEXT, or BOTH method
*           always prev for nsrcg
*======================================================================;

/*--- set input parameter macro variable values here ---*/

%MACRO HOTDECK ( printfreq = 1 ) ;

%LET INFILE=&OUTFILE; /* set the infile to the outfile from the serpentine sort */

%LET STOP=0;  /* macro variable for whether to iterate again. set to 0 for 1st iteration, set to 1 when no donor used more than 3 times */

%LET II=0;    /* macro variable for iteration number */

%DO %UNTIL (&STOP=1);

   /*test code */

   %IF &II=50 %THEN %LET STOP=1; /* stop iterating when you've gotten to such a high count (50) */

   /* test code */

   %IF %UPCASE(&DIRECTN)=BOTH OR %UPCASE(&DIRECTN)=PREV
   %THEN %LET START=1;
   %ELSE %LET START=2;

   %IF %UPCASE(&DIRECTN)=BOTH OR %UPCASE(&DIRECTN)=NEXT
   %THEN %LET XEND=2;
   %ELSE %LET XEND=1;

   %LOCAL J;

   %DO J=&START %TO &XEND;

      %IF &II=0 %THEN %DO;

         PROC SORT
            %IF (%UPCASE(&DIRECTN)=BOTH AND &J=2) %THEN %DO; DATA=HOTDECK1
            %END;
            %ELSE %DO; DATA=&INFILE (KEEP=&ID &CLASS &MISSVAR &SORTVAR)
            %END;
            OUT=SORTFILE;
            BY &CLASS %IF &J=2 %THEN %DO; DESCENDING %END; &SORTVAR;
         RUN;

      %END;
      %ELSE %DO;

         PROC SORT
            %IF (%UPCASE(&DIRECTN)=BOTH AND &J=2) %THEN %DO; DATA=HOTDECK1
            %END;
            %ELSE %DO; DATA=NEWDONOR (KEEP=&ID &CLASS &MISSVAR &SORTVAR DONOR_COUNT)
            %END;
            OUT=SORTFILE;
            BY &CLASS %IF &J=2 %THEN %DO; DESCENDING %END; &SORTVAR;
         RUN;


      %END;

      /* The following steps will allow you to find the first or last respondent */
      /* to all questions to be imputed within a sorted imputation class         */


      /* get the number of variables to be imputed */

      DATA _NULL_;
        ARRAY MISSVAR &MISSVAR;
        CALL SYMPUT('NMISSV',LEFT(TRIM(PUT(DIM(MISSVAR),8.))));
      RUN;

      /* assign respind to 1 for the first(last) respondent within a class, */
      /* 9 to all others                                                    */

      DATA RESPIND&J (DROP=RESPFLAG XABORT RESPCOUNT);
        SET SORTFILE END=EOF;
        BY &CLASS;
        RETAIN RESPFLAG XABORT;
        RESPIND&J=9;

    ARRAY MISSVAR &MISSVAR;

    IF FIRST.&LSTCLASS THEN RESPFLAG=0;
        IF RESPFLAG EQ 0 THEN DO;
           RESPCOUNT=0;
           DO OVER MISSVAR;
              IF MISSVAR NOT IN &MVALUE THEN RESPCOUNT=RESPCOUNT+1;
           END;
       IF RESPCOUNT=%EVAL(&NMISSV) THEN DO;
          RESPIND&J=1;
          RESPFLAG=1;
           END;
        END;
        IF LAST.&LSTCLASS AND RESPFLAG EQ 0 THEN DO;
           ERROR "ERROR: AT LEAST ONE IMPUTATION CLASS WITH NO DONOR"
           &LSTCLASS=;
           XABORT+1;
        END;
        IF EOF & XABORT THEN ABORT;
      RUN;

      PROC SORT DATA=RESPIND&J;
         BY &CLASS  RESPIND&J %IF &J=2 %THEN %DO; DESCENDING %END; &SORTVAR;
      RUN;

      DATA HOTDECK&J (DROP=PRVR&J.R1-PRVR&J.R&NMISSV /*DONORID*/ NMISS);
        SET RESPIND&J;
        BY &CLASS;
        RETAIN PRVR&J.R1-PRVR&J.R&NMISSV DONORID;
        ARRAY MISSVAR  &MISSVAR;
        ARRAY IMPVAR&J IMPV&J.R1-IMPV&J.R&NMISSV;
        ARRAY PREVAR&J PRVR&J.R1-PRVR&J.R&NMISSV;
        IF FIRST.&LSTCLASS THEN DO;
           DO OVER PREVAR&J;
              PREVAR&J=.;
           END;
           DONORID="";
        END;
        NMISS=0;
        DO OVER MISSVAR;
           IF MISSVAR IN &MVALUE THEN NMISS+1;
        END;
        IF NMISS=0 THEN DO;   /* note: potential donors must have */
       DO OVER MISSVAR;       /* responses for all variables      */
              IMPVAR&J=MISSVAR;
              PREVAR&J=MISSVAR;
           END;
           DONORID=&ID;
           DONOR&J="";
        END;
        ELSE DO;
       DO OVER MISSVAR;
              IMPVAR&J=PREVAR&J;  /* note: retains all response values from    */
           END;                   /* potential donors, but only missing values */
           DONOR&J=DONORID;       /* will be imputed in next step */
        END;
      RUN;


   %END;  ** the J loop ;

   /** changed so that a single donor cannot be used more than 3 times **/

   DATA &HOTFILE._&II(DROP= IMPV1R1-IMPV1R&NMISSV
                       IMPV2R1-IMPV2R&NMISSV);

     %IF %UPCASE(&DIRECTN)=BOTH %THEN %DO;
       SET HOTDECK2;
     %END;
     %ELSE %DO;
       SET HOTDECK&XEND;
     %END;

     RETAIN COUNT;

     ARRAY IMPVAR1 IMPV1R1-IMPV1R&NMISSV;
     ARRAY IMPVAR2 IMPV2R1-IMPV2R&NMISSV;
     ARRAY MISSVAR &MISSVAR.;
     ARRAY IR_VAR  IR_VAR1-IR_VAR&NMISSV;
     ARRAY I_FLAG  I_VAR1-I_VAR&NMISSV;

     /* set imputation flags */
     DO OVER I_FLAG;
       I_FLAG=0;
           IF MISSVAR IN &MVALUE. THEN I_FLAG=1;
     END;

     /* set imputed values to original values */

     DO OVER IR_VAR;
       IR_VAR=MISSVAR;
     END;

     %IF %UPCASE(&DIRECTN)=PREV %THEN %DO;
        DO OVER MISSVAR;
        /* only impute items w/ missing values */
           IF MISSVAR IN &MVALUE. THEN IR_VAR=IMPVAR1;
    END;
        DONOR=DONOR1;
        CHOSE='PREV';
     %END;
     %ELSE %IF %UPCASE(&DIRECTN)=NEXT %THEN %DO;
        DO OVER MISSVAR;
        /* only impute items w/ missing values */
           IF MISSVAR IN &MVALUE. THEN IR_VAR=IMPVAR2;
        END;
        DONOR=DONOR2;
        CHOSE='NEXT';
     %END;

     %ELSE %IF %UPCASE(&DIRECTN)=BOTH %THEN %DO;
        RAN=MOD(INT(RANUNI(1)*10),2)+1;
        IF RAN EQ 1 THEN DO;
           DO OVER MISSVAR;
              IF MISSVAR IN &MVALUE. THEN IR_VAR=IMPVAR1;    * changed to " IN &MVALUE." ;
           END;
           DONOR=DONOR1;
           CHOSE='PREV';
        END;
        ELSE IF RAN EQ 2 THEN DO;
           DO OVER MISSVAR;
              IF MISSVAR IN &MVALUE. THEN IR_VAR=IMPVAR2;     * "THEN " was missing ;
           END;
           DONOR=DONOR2;
           CHOSE='NEXT';
        END;
     %END;

     %IF &II=0 %THEN %DO;
        IF DONOR="" THEN COUNT=0;
        ELSE COUNT=COUNT+1;
     %END;
     %ELSE %DO;
        IF DONOR="" AND DONOR_COUNT=. THEN COUNT=0;
        /* # of times donor used in */
        /* prior iterations         */
        ELSE IF DONOR="" THEN COUNT=DONOR_COUNT;
    IF DONOR NE . THEN COUNT=COUNT+1;
     %END;

     %IF %UPCASE(&DIRECTN)=BOTH OR %UPCASE(&DIRECTN)=PREV %THEN %DO;
         DROP DONOR1;
     %END;
     %IF %UPCASE(&DIRECTN)=BOTH OR %UPCASE(&DIRECTN)=NEXT %THEN %DO;
         DROP DONOR2;
     %END;

   RUN;

   /* The next section identifies donors used more than three times */
   /* and removes them from the potential donor pool */


   PROC MEANS NWAY MAX NOPRINT DATA=&HOTFILE._&II.;
      VAR COUNT;
      OUTPUT OUT=CHECKMAX(DROP=_TYPE_ _FREQ_)
      MAX=MAX_COUNT ;
   RUN;

   DATA _NULL_;
     SET CHECKMAX;

     /* define a macro variable for whether */
     /* any donors were used > 3 times */

     CALL SYMPUT('MAX',LEFT(TRIM(PUT(MAX_COUNT,8.))));

   RUN;


   %IF %EVAL(&MAX) < 4 %THEN %LET STOP=1;

   /* if a donor has been used more than three times, prepare */
   /* data for next iteration                                 */

   %IF &STOP=0 %THEN %DO;

      PROC FREQ NOPRINT DATA=&HOTFILE._&II.;
         TABLES DONOR/OUT=COUNT_DONOR(RENAME=(DONOR=&ID COUNT=DONOR_COUNT)
                                          DROP=PERCENT);
      RUN;
     /* DATA COUNT_DONOR;
        LENGTH &ID. $8.;
        SET COUNT_DONOR;
    &ID=LEFT(PUT(DONOR, 8.));
      RUN;*/

      PROC SORT DATA=&HOTFILE._&II;
         BY &ID;
      RUN;

      DATA &HOTFILE._&II NEWDONOR;
        MERGE &HOTFILE._&II(IN=IN_1) COUNT_DONOR;
        BY &ID;
    IF IN_1;

        ** output potential donor records used less than three times
        ** as well as records for which count is > 3 to the newdonor
        ** dataset ;

        IF (DONOR="" AND DONOR_COUNT < 3) OR COUNT > 3 THEN OUTPUT NEWDONOR;
        ELSE OUTPUT &HOTFILE._&II;
      RUN;


   %END;
   %ELSE %DO;

   /* No donors used more than three times, so output  */


      /* rename imputed and flag variables back to      */
      /* the original names, with appropriate prefixes  */

      %DO VN = 1 %TO &NMISSV;
          /* %LET VAR&VN.=%TRIM(%SCAN(&ORIGVAR.,&VN.)); /**/
        /**/  %LET VAR&VN.=%TRIM(%SCAN(&missVAR.,&VN.));  /**/
          %LET VAR_REV&VN.=%TRIM(%SCAN(&MISSVAR.,&VN.));
      %END;

      DATA &HOTFILE (RENAME=(
        %DO VN = 1 %TO &NMISSV;
            /*IR_VAR&VN=IR_&&VAR&VN*/
            IR_VAR&VN = &&VAR&VN.._H
            I_VAR&VN =I_&&VAR&VN
        %END; ));

        SET %DO M=0 %TO &II;
               &HOTFILE._&M(KEEP= &ID &MISSVAR
                        %DO VN = 1 %TO &NMISSV;
                                IR_VAR&VN I_VAR&VN
                            %END;  )

            %END;;
      RUN;


      /* check the frequencies of original values vs. imputed values */;

      %DO VN=1 %TO &NMISSV;

     %IF ( "&printfreq" = "1" ) /*(&&VAR&VN. NE B33) AND (&&VAR&VN. NE B38)*/ %THEN %DO;

         PROC FREQ DATA=&HOTFILE;
           where ( I_&&VAR&VN.= 1 ) ;                                           * amb ;
           *TABLES I_&&VAR&VN.*&&VAR_REV&VN.*IR_&&VAR&VN./MISSING LIST;
           TABLES I_&&VAR&VN. * &&VAR_REV&VN. * &&VAR&VN.._H/*IR_&&VAR&VN.*/
              /MISSING LIST;
           TITLE5 "QA Imputed Values";
            RUN;

     %END;

      %END;

      /* sort and create final output dataset */

      PROC SORT DATA=&HOTFILE OUT=&HOTFILE(DROP=&MISSVAR);
         BY &ID;
      RUN;


   %END;

   %LET II=%EVAL(&II+1);

%END; ** II loop ;

%MEND HOTDECK;






