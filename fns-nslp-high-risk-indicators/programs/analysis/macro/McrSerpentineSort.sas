/*Exhibit 2.  Serpentine Sort*/
/**********************************************************/
/*Developed by Barbara Lepidus Carlson and Linda S. Bandeh*/
/*Mathematica Policy Research, Inc., Princeton, New Jersey*/
/*                     February 1995                      */
/*
/* C. Rankin:  10/19/2004 added code to generate
/*             maxv variable
/*
/* A Bloomenthal:  Oct 2010 removed "commenting" of "(DROP=FRWDRVRS)"
/*             when this is inactivated, SORTVAR is not created properly
/*
/**********************************************************/

*OPTIONS PAGESIZE=60 LINESIZE=165 NOCENTER MPRINT;

LIBNAME LIB '.';

*%LET ORIGFILE = filename1;               /*input filename */
*%LET OUTFILE  = filename2;               /*output filename */
*%LET IDVARS   = idvar1 idvar2 ...;       /*identifying variable(s) */
*%LET CLASS    = classvar1 classvar2 ...; /*classing variable(s) */
*%LET VARS     = sortvar1 sortvar2...;    /*sorting variables, listed in desired sort order */
*%LET MAXV     = #        #       ...;    /*maximum value for each sorting variable */
                                          /*(over-estimating is fine, under-estimating is not)*/

%MACRO SERPSORT;

   %IF &VARS = DUMMY %THEN %DO;
      DATA &OUTFILE;
         SET &ORIGFILE;
         SORTVAR = 1;
      RUN;
   %END;
   %ELSE %DO;

      * get the maximum values for the variables used to sort;
      * first, get the number of sort variables ;

      DATA _NULL_;
        ARRAY SORTVARS &VARS;
        CALL SYMPUT('N_MAX',LEFT(TRIM(PUT(DIM(SORTVARS),8.))));
      RUN;

      * next, create a macro variable with the maximum values for all sort variables;

      PROC MEANS NOPRINT MAX DATA=&ORIGFILE;
         VAR &VARS;
         OUTPUT OUT=MAXSORT(DROP=_TYPE_ _FREQ_) MAX=MAX1-MAX&N_MAX.;
      RUN;

      DATA MAXVALS(KEEP=MAXVAL);
        SET MAXSORT;
        ARRAY MAXVARS(*) MAX1-MAX&N_MAX;
        DO I=1 TO DIM(MAXVARS);
           MAXVAL=MAXVARS(I);
           OUTPUT;
        END;
      RUN;

      PROC SQL NOPRINT;
         SELECT(MAXVAL) INTO :MAXV SEPARATED BY ' '
         FROM MAXVALS;
      RUN;

      %PUT "Maxv = &MAXV ";

      DATA DROPVARS;
         SET &ORIGFILE (KEEP=&IDVARS &CLASS &VARS &MISSVAR);
         ARRAY X &VARS;
         CALL SYMPUT('NUMVARS',LEFT(TRIM(PUT(DIM(X),8.))));
         SORTVAR1 = X{1};
      RUN;
      /*If necessary, use this DATA step to recode any sort variables to nonmissing, */
      /*positive integers or to create a unique identifier variable from multiple identifiers*/

      PROC SORT DATA=DROPVARS OUT=FILE1;
         BY &CLASS SORTVAR1;
      RUN;

      %LOCAL H I;

      %DO I=2 %TO &NUMVARS;
         %LET H=%EVAL(&I-1);

         DATA FILE&I /**/(DROP=FRWDRVRS)/**/;   /* AMB: this was commented out, but if NOT dropped then SORTVAR is not right*/
            SET FILE&H;
            BY &CLASS SORTVAR&H;
            RETAIN FRWDRVRS;

            ARRAY MARR MAX1-MAX&NUMVARS (&MAXV);
            ARRAY VARR &VARS;

            IF _N_=1 THEN FRWDRVRS=-1;
            file print ;
            IF FIRST.SORTVAR&H THEN FRWDRVRS=-1*FRWDRVRS;
            IF FRWDRVRS=-1 THEN RVAR=MARR{&I}-VARR{&I};
            ELSE RVAR=VARR{&I};
            SORTVAR&I=(SORTVAR&H*(10**(LENGTH(LEFT(COMPRESS(PUT(MARR{&I},8.)))))) + RVAR);

            *IF FIRST.SORTVAR&H THEN
              put // ( _n_ FIRST.SORTVAR&H &IDVARS &CLASS SORTVAR&H ) ( = )  +20 ( &VARS) ( = )
                  / ( SORTVAR&H FRWDRVRS  VARR{&I} RVAR  SORTVAR&I ) ( = ) ;
            *IF laST.SORTVAR&H THEN
              put / ( _n_ laST.SORTVAR&H &IDVARS &CLASS SORTVAR&H ) ( = )  +20 ( &VARS) ( = )
                  / ( SORTVAR&H FRWDRVRS  VARR{&I} RVAR  SORTVAR&I ) ( = ) ;

            IF &I=&NUMVARS THEN SORTVAR=SORTVAR&I;
         RUN;

         PROC SORT DATA=FILE&I;
            BY &CLASS SORTVAR&I;
         RUN;
      %END;

      /*
      DATA &OUTFILE
      (DROP=MAX1-MAX&NUMVARS RVAR SORTVAR1-SORTVAR&NUMVARS);
      SET FILE&NUMVARS;
      RUN;
      */

      /*Your file will now have a variable called SORTVAR, which can be used to do a serpentine*/
      /*sort.  To merge back with the rest of the data file, you must re-sort by the identifying*/
      /*variable(s) first.*/

      PROC SORT DATA=FILE&NUMVARS OUT=OUTFILE(KEEP=&IDVARS SORTVAR);
         BY &IDVARS;
      RUN;
      PROC SORT DATA=DROPVARS;
         BY &IDVARS;
      RUN;
      DATA &OUTFILE;
         MERGE DROPVARS OUTFILE;
         BY &IDVARS;
      RUN;

   %END;

%MEND SERPSORT;



