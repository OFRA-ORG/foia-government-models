!**************************************************************************************************
! Source File:  FStab_gl.F90                
! Called By:    FS_TABLES                   
!
! TABLE 6 -  Gainer/Loser Tables
!
!**************************************************************************************************
SUBROUTINE FS_TAB_gainer_loser (        &
     BASE_HAS_DIS,     &   !    HOUSEHOLD HAS DISABLED
     BASE_HAS_EARN,    &   !    HOUSEHOLD HAS EARNERS
     BASE_HAS_ELDER,   &   !    HOUSEHOLD HAS ELDERLY
     BASE_HAS_KIDS,    &   !    HOUSEHOLD HAS KIDS
     BASE_HAS_NONCIT,  &   !    HOUSEHOLD HAS NONCITIZENS
     BASE_HAS_ABAWD,   &   !    HOUSEHOLD HAS ABAWDS
     BASE_FSUSIZE,     &   !    FOOD STAMP UNIT SIZE
     BASE_POVRAT,      &   !    POVERTY RATE
     BASE_FSBEN,       &   !    BASELAW BENEFIT AMOUNT
     BASE_PARTIC,      &   !    LOGICAL FOR PARTICIPATION IN BASELAW
     FSBEN,            &   !    PLAN BENEFIT AMOUNT
     PARTIC,           &   !    LOGICAL FOR PARTICIPATION
     KTH,              &   !    PLAN NUMBER
     REGION,           &   !    REGION OF COUNTRY
     WGT               &   !    WEIGHT
     )   

  USE GLOBAL
  USE FSWORK , ONLY: SHOW_ELIG, PLANNAME_TABLE, PLANNBR_TABLE, tab_gl_stats, tab_gl_stats_ben, create_table_extracts
  USE FSPARM , ONLY: GLUNIT, GLTABS, JSON_FILE, dostats
  USE FSSIZES, ONLY: MAX_NTH
  use Utils
  
  IMPLICIT NONE
  !---- Declare parameters from calling program
  INTEGER, intent(in)  ::         BASE_FSBEN
  LOGICAL, intent(in)  ::         BASE_PARTIC
  INTEGER, intent(in)  ::         FSBEN
  LOGICAL, intent(in)  ::         BASE_HAS_DIS
  REAL(8), intent(in)  ::         BASE_POVRAT
  LOGICAL, intent(in)  ::         BASE_HAS_EARN
  LOGICAL, intent(in)  ::         BASE_HAS_ELDER
  INTEGER, intent(in)  ::         BASE_FSUSIZE
  LOGICAL, intent(in)  ::         BASE_HAS_KIDS
  INTEGER, intent(in)  ::         KTH
  LOGICAL, intent(in)  ::         PARTIC
  INTEGER, intent(in)  ::         REGION
  LOGICAL, intent(in)  ::         BASE_HAS_NONCIT
  LOGICAL, intent(in)  ::         BASE_HAS_ABAWD
  REAL, intent(in)     ::         WGT

  !---- Variables for tables
  INTEGER  ::   IPOV, I, J, ITH, NBR_OF_KTHS =0,  &
       TAB,     COL, ROW, SUMROW,   IB1, IB2,  ICAT,  BEN_DIF

  INTEGER, PARAMETER :: NWAFERS=7
  LOGICAL  ::  BAD_DATA, IN_WAFER(NWAFERS)

  REAL (8) :: AMT,  AMTB, PEOPLE

  !---- Row and column labels
  CHARACTER(26) :: ROWLAB1(11) = (/    &
       ' Baselaw: Y        N/A    ',  &
       ' Baselaw: Y        N/A    ',  &
       ' Baselaw: Y        Loss of',  &
       ' Baselaw: Y        Loss of',  &
       ' Baselaw: Y        Loss of',  &
       ' Baselaw: Y        No     ',  &
       ' Baselaw: N        N/A    ',  &
       ' Baselaw: N        N/A    ',  &
       ' Baselaw: Y        Gain of',  &
       ' Baselaw: Y        Gain of',  &
       ' Baselaw: Y        Gain of'/)

  CHARACTER(26) :: ROWLAB2(11) = (/    &
       ' Reform:  N               ',  &
       ' Reform:  N               ',  &
       ' Reform:  Y        $51+   ',  &
       ' Reform:  Y        $21-50 ',  &
       ' Reform:  Y        $1-20  ',  &
       ' Reform:  Y        Change ',  &
       ' (inelig)                 ',  &
       ' (elig)                   ',  &
       ' Reform:  Y        $1-20  ',  &
       ' Reform:  Y        $21-50 ',  &
       ' Reform:  Y        $51+   '/)

  CHARACTER(26) :: ROWLAB3(11)=(/      &
       ' (inelig)                 ',  &
       ' (elig)                   ',  &
       '                          ',  &
       '                          ',  &
       '                          ',  &
       '                          ',  &
       ' Reform:  Y               ',  &
       ' Reform:  Y               ',  &
       '                          ',  &
       '                          ',  &
       '                          '/)

  INTEGER ::   LAB_LENGTH(NWAFERS) = 0

  CHARACTER (60) ::  TYPLAB(NWAFERS)= (/                          &
       'ALL SNAP UNITS                                              ' &
       ,'SNAP UNITS WITH EARNINGS                                    ' &
       ,'SNAP UNITS WITH ELDERLY                                     ' &
       ,'SNAP UNITS WITH DISABLED                                    ' &
       ,'SNAP UNITS WITH KIDS                                        ' &
       ,'SNAP UNITS WITH NONCITIZENS                                 ' &
       ,'SNAP UNITS WITH NONELDERLY NONDISABLED CHILDLESS ADULTS     ' /)


  INTEGER :: LISTLAB_LENGTH = 30

  CHARACTER(30) :: LISTLAB(NWAFERS)= (/    &
       'ALL SNAP UNITS                '   &
       ,'UNITS W/ EARNINGS             '   &
       ,'UNITS W/ ELDERLY              '   &
       ,'UNITS W/ DISABLED             '   &
       ,'UNITS W/ KIDS                 '   &
       ,'UNITS W/ NONCITIZENS          '   &
       ,'UNITS W/ ABAWDS               '  /)


  CHARACTER(12) :: COLLAB1(6) = (/               &
       '          0%','       1-50%','     51-100%',  &
       '    101-130%','       131+%','       Total'/)

  CHARACTER(12) :: COLLAB2(5) = (/ '   Northeast', '     Midwest',  '       South', &
       '        West', '       Total'/)

  CHARACTER( 80) :: PLANTEMP
  CHARACTER( 80) :: TYPETEMP
  CHARACTER(100) :: END_LABEL

  !  The benefit values in the table reflect the change in average benefits,
  !  but the benefit statistics are based on the change in per unit benefits.




  !---- Table variables used in previous GL table routines
  !---- ** Demographic tables **
  !---- First subscript:  which plan
  !---- Second subscript: which row (GL category)
  !---- Third subscript:  which column (income class)
  !---- Fourth subscript: which table (ALL, HHWKIDS, etc.)
  REAL(8) :: TABHHLD(max_nth+1,11,6,NWAFERS) = 0.0
  REAL(8) :: TABPER (max_nth+1,11,6,NWAFERS) = 0.0
  REAL(8) :: TABAMT (max_nth+1,11,6,NWAFERS) = 0.0
  REAL(8) :: TABAMTB(max_nth+1,11  ,NWAFERS) = 0.0 ! baselaw benefits

  !---- Aggregates of households used for the summary tables
  !---- First subscript: which row (on summary table)
  INTEGER, PARAMETER :: NSUMROWS=10
  REAL(8)  ::   SUMHH  (NSUMROWS)  = 0.0
  REAL(8)  ::   SUMPER (NSUMROWS)  = 0.0
  REAL(8)  ::   SUMAMT (NSUMROWS)  = 0.0
  REAL(8)  ::   SUMAMTB(NSUMROWS)  = 0.0

  !---- ** Region tables **
  !---- First subscript:  which plan
  !---- Second subscript: which row (GL category)
  !---- Third subscript:  which column (region class)
  REAL(8)  ::   REGHH (max_nth+1,11,5)  = 0.0D0
  REAL(8)  ::   REGAMT(max_nth+1,11,5)  = 0.0D0
  REAL(8)  ::   REGPER(max_nth+1,11,5)  = 0.0D0

  !---- Percent change variables
  !---- First subscript:  which row (GL category)
  !---- Second subscript: which column (income class)
  REAL(8)  ::   PCTAMT(11,6)    = 0.0
  REAL(8)  ::   PCTHH (11,6)    = 0.0
  REAL(8)  ::   PCTPER(11,6)    = 0.0

  !---- Percent change variables for summary tables
  !---- First subscript:  which row (GL category)

  REAL(8)  ::   SUMPCTAMT(NSUMROWS)   = 0.0
  REAL(8)  ::   SUMPCTHH (NSUMROWS)   = 0.0
  REAL(8)  ::   SUMPCTPER(NSUMROWS)   = 0.0
  REAL(8)  ::   SUMAMTCHG(NSUMROWS)   = 0.0

  INTEGER, PARAMETER :: NINC=6 , NREG=5, NROWS=11 , BASETOT=1

  !---- Comma display fields
  CHARACTER(15) COMMA_FIELD (3)
  INTEGER , PARAMETER ::  FIELD_LEN = 15

  CHARACTER(len=40), DIMENSION(nsumrows) :: rowlab = (/ &
       'No change                               '   &
       ,'Change                                  '   &
       ,'No longer eligible                      '   &
       ,'Still eligible but not participating    '   &
       ,'Still participating with lower benefits '   &
       ,'Total losers                            '   &
       ,'Newly eligible and participating        '   &
       ,'Still eligible but newly participating  '   &
       ,'Still Participating with higher benefits'   &
       ,'Total gainers                           '   &
       /)

  ! prefix for global table footer key names
  CHARACTER(len=5) :: tab_prefix



  !---- By-pass calculations if print requested
  IF (KEOF == 3) GOTO 900
  !****************************************************************
  !     Perform table calculations
  !****************************************************************

  !---- Keep track of highest KTH
  IF (KTH > NBR_OF_KTHS)  NBR_OF_KTHS = KTH

  !---- Poverty rate recode
  IF (BASE_POVRAT <= 0.00) THEN
     IPOV = 1
  ELSE IF (BASE_POVRAT <= 0.50) THEN
     IPOV = 2
  ELSE IF (BASE_POVRAT <= 1.00) THEN
     IPOV = 3
  ELSE IF (BASE_POVRAT <= 1.30) THEN
     IPOV = 4
  ELSE
     IPOV = 5
  ENDIF

  IF (BASE_PARTIC) THEN
     AMTB = WGT*BASE_FSBEN
  ELSE
     AMTB = 0.0
  ENDIF

  IB1 = 0
  IB2 = 0
  IF (BASE_PARTIC) IB1 = BASE_FSBEN
  IF (PARTIC) IB2 = FSBEN
  BEN_DIF = IB2 - IB1


  IF (KTH == 1) THEN
     ICAT = BASETOT          !!  Baselaw totals accumulated in row 1
     AMT = 0.0
     IF (BASE_PARTIC) AMT = WGT*BASE_FSBEN
  ELSE
     AMT  =  WGT*FLOAT(BEN_DIF)

     !------ Set the row index to the tables using PARTIC and BEN_DIF
     !------ Skip persons who did not participate in either baselaw or reform
     IF (.NOT. BASE_PARTIC .AND. .NOT. PARTIC) RETURN

     IF (.NOT. BASE_PARTIC) THEN             !! Baselaw: N,  Reform: Y
        IF (BASE_FSBEN <= 0.0) THEN
           ICAT = 7                            !! Ineligible for baselaw
        ELSE
           ICAT = 8                            !! Eligible for baselaw
        ENDIF
     ELSE IF (.NOT. PARTIC) THEN             !! Baselaw: Y,  Reform: N
        IF (FSBEN <= 0.0) THEN
           ICAT = 1                            !! Ineligible for reform
        ELSE
           ICAT = 2                            !! Eligible for reform
        ENDIF
     ELSE                                    !! Baselaw: Y,  Reform Y
        SELECT CASE (BEN_DIF)
        CASE (:-51)
           ICAT = 3                          !! Loss of $51 or more
        CASE (-50:-21)
           ICAT = 4                          !! Loss of $21-50
        CASE (-20:-1)
           ICAT = 5                          !! Loss of $1-20
        CASE (0)
           ICAT = 6                          !! No change
        CASE (1:20)
           ICAT = 9                          !! Gain of $1-20
        CASE (21:50)
           ICAT = 10                         !! Gain of $21-50
        CASE (51:)
           ICAT = 11                         !! Gain of $51+
        END SELECT
     ENDIF
  END IF

  PEOPLE = WGT*FLOAT(BASE_FSUSIZE)

  !----- *** Check for bad data ***
  BAD_DATA = .FALSE.
  IF (   KTH    < 1 .OR. KTH    >  max_nth+1  &
       .OR. ICAT   < 1 .OR. ICAT   > 11          &
       .OR. REGION < 1 .OR. REGION >  4          &
       .OR. IPOV   < 1 .OR. IPOV   >  5 ) BAD_DATA = .TRUE.

  IF (BAD_DATA) THEN
     WRITE(PRFILE,2020)ICAT,REGION,KTH,IPOV
     RETURN
  ENDIF

  DO I = 1, NWAFERS
     IN_WAFER(I) = .FALSE.
  ENDDO

  !---- All food stamp units
  IN_WAFER(1) = .TRUE.

  !---- Food stamp units with earnings
  IF (BASE_HAS_EARN)    IN_WAFER(2) = .TRUE.

  !---- Food stamp units with elderly
  IF (BASE_HAS_ELDER)   IN_WAFER(3) = .TRUE.

  !---- Food stamp units with disabled
  IF (BASE_HAS_DIS)     IN_WAFER(4) = .TRUE.

  !---- Food stamp units with kids
  IF (BASE_HAS_KIDS)    IN_WAFER(5) = .TRUE.

  !---- Food stamp units with NONCITIZENS
  IF (BASE_HAS_NONCIT)    IN_WAFER(6) = .TRUE.

  !---- Food stamp units with ABAWD
  IF (BASE_HAS_ABAWD)    IN_WAFER(7) = .TRUE.


  DO I = 1, NWAFERS
     IF (IN_WAFER(I)) THEN
        TABHHLD(KTH,ICAT,IPOV,I) = TABHHLD(KTH,ICAT,IPOV,I) + WGT
        TABPER (KTH,ICAT,IPOV,I) = TABPER (KTH,ICAT,IPOV,I) + PEOPLE
        TABAMT (KTH,ICAT,IPOV,I) = TABAMT (KTH,ICAT,IPOV,I) + AMT
        TABAMTB(KTH,ICAT     ,I) = TABAMTB(KTH,ICAT     ,I) + AMTB
     ENDIF
  ENDDO

  !---- Regional tables
  REGHH (KTH,ICAT,REGION) = REGHH (KTH,ICAT,REGION) + WGT
  REGAMT(KTH,ICAT,REGION) = REGAMT(KTH,ICAT,REGION) + AMT
  REGPER(KTH,ICAT,REGION) = REGPER(KTH,ICAT,REGION) + PEOPLE


  !-- cal state g/ tabs, skip for baselaw call
  IF (kth > 1) &
       CALL FS_TAB_state_gainer_loser ( &
       base_has_earn     &
       ,base_has_elder    &
       ,base_has_dis      &
       ,base_has_kids     &
       ,base_has_noncit   &
       ,base_has_abawd    &
       ,kth               &
       ,wgt               &
       ,BASE_PARTIC       &
       ,PARTIC            &
       ,BASE_FSBEN        &
       ,FSBEN             &
       ,BASE_FSUSIZE      &
       )



  RETURN


900 CONTINUE
  !******************************************************************
  !     Print the table
  !******************************************************************

  !--- Calculate the length of the wafer labels:
  DO J = 1,NWAFERS
     DO I = LISTLAB_LENGTH,1,-1
        IF (LISTLAB(J)(I:I) .NE. ' ') GO TO 910
     ENDDO
910  CONTINUE
     LAB_LENGTH(J) = I
  ENDDO


  !---- Calculate baselaw totals for tables
  DO TAB = 1,NWAFERS
     DO COL = 1,NINC-1
        TABHHLD(1,BASETOT,NINC,TAB) = TABHHLD(1,BASETOT,NINC,TAB)+TABHHLD(1,BASETOT,COL,TAB)
        TABPER (1,BASETOT,NINC,TAB) = TABPER (1,BASETOT,NINC,TAB)+TABPER (1,BASETOT,COL,TAB)
        TABAMT (1,BASETOT,NINC,TAB) = TABAMT (1,BASETOT,NINC,TAB)+TABAMT (1,BASETOT,COL,TAB)
     ENDDO
  ENDDO

  !---- Calculate baselaw totals for regional tables
  DO COL = 1,NREG-1
     REGHH (1,BASETOT,NREG) =  REGHH (1,BASETOT,NREG)+REGHH (1,BASETOT,COL)
     REGPER(1,BASETOT,NREG) =  REGPER(1,BASETOT,NREG)+REGPER(1,BASETOT,COL)
     REGAMT(1,BASETOT,NREG) =  REGAMT(1,BASETOT,NREG)+REGAMT(1,BASETOT,COL)
  ENDDO

  !---- Do calculations and print tables over all KTH
  DO ITH = 2, NBR_OF_KTHS

     IF (.NOT. GLTABS(ITH-1) ) CYCLE

     !------ Form totals for tables
     DO TAB = 1,NWAFERS

        DO ROW = 1,NSUMROWS
           SUMHH  (ROW) = 0.
           SUMPER (ROW) = 0.
           SUMAMT (ROW) = 0.
           SUMAMTB(ROW) = 0.
        ENDDO

        DO ROW = 1,NROWS

           !-------- Total income level columns into TOTAL column
           DO COL = 1,NINC-1
              TABHHLD(ITH,ROW,NINC,TAB) = TABHHLD(ITH,ROW,NINC,TAB) + TABHHLD(ITH,ROW,COL,TAB)
              TABPER (ITH,ROW,NINC,TAB) = TABPER (ITH,ROW,NINC,TAB) + TABPER (ITH,ROW,COL,TAB)
              TABAMT (ITH,ROW,NINC,TAB) = TABAMT (ITH,ROW,NINC,TAB) + TABAMT (ITH,ROW,COL,TAB)
           ENDDO    !! End of loop over COL

           SUMROW = 0
           SELECT CASE (ROW)
           CASE (1)            !! Losers;  Baselaw: Y  Reform: N; Inelig.
              SUMROW = 3
           CASE (2)            !! Losers;  Baselaw: Y  Reform: N; Elig.
              SUMROW = 4
           CASE (3:5)          !! Losers;  Baselaw: Y  Reform: Y
              SUMROW = 5
           CASE (6)            !! No change
              SUMROW = 1
           CASE (7)            !! Gainers; Baselaw: N  Reform: Y; Inelig.
              SUMROW = 7
           CASE (8)            !! Gainers; Baselaw: N  Reform: Y; Elig.
              SUMROW = 8
           CASE (9:11)         !! Gainers; Baselaw: Y  Reform: Y
              SUMROW = 9
           END SELECT

           !-------- Aggregate households into summary participation category
           SUMHH  (SUMROW) = SUMHH  (SUMROW)+TABHHLD(ITH,ROW,NINC,TAB)
           SUMPER (SUMROW) = SUMPER (SUMROW)+TABPER (ITH,ROW,NINC,TAB)
           SUMAMT (SUMROW) = SUMAMT (SUMROW)+TABAMT (ITH,ROW,NINC,TAB)
           SUMAMTB(SUMROW) = SUMAMTB(SUMROW)+TABAMTB(ITH,ROW     ,TAB)

        ENDDO  !!  End of loop over ROW

        !-------- Create summary totals
        SUMHH  ( 6) = SUMHH  (3) + SUMHH  (4) + SUMHH  (5)
        SUMHH  (10) = SUMHH  (9) + SUMHH  (8) + SUMHH  (7)
        SUMHH  ( 2) = SUMHH (10) + SUMHH  (6)

        SUMPER ( 6) = SUMPER (3) + SUMPER (4) + SUMPER (5)
        SUMPER (10) = SUMPER (9) + SUMPER (8) + SUMPER (7)
        SUMPER ( 2) = SUMPER(10) + SUMPER (6)

        SUMAMT ( 6) = SUMAMT (3) + SUMAMT (4) + SUMAMT (5)
        SUMAMT (10) = SUMAMT (9) + SUMAMT (8) + SUMAMT (7)
        SUMAMT ( 2) = SUMAMT(10) + SUMAMT (6)

        SUMAMTB( 6) = SUMAMTB(3) + SUMAMTB(4) + SUMAMTB(5)
        SUMAMTB(10) = SUMAMTB(9) + SUMAMTB(8) + SUMAMTB(7)
        SUMAMTB( 2) = SUMAMTB(10)+ SUMAMTB(6)

        DO ROW = 1,NROWS

           !---------  Calculate printed table cell values for Part B of table
           DO COL = 1,NINC
              PCTHH(ROW,COL) = 0.0
              PCTPER(ROW,COL) = 0.0
              PCTAMT(ROW,COL) = 0.0
              IF (TABHHLD(1,BASETOT,COL,TAB) > 0.0) &
                   PCTHH(ROW,COL) = 100.0*TABHHLD(ITH,ROW,COL,TAB) / TABHHLD(1,BASETOT,COL,TAB)
              IF (TABPER(1,BASETOT,COL,TAB) > 0.0)  &
                   PCTPER(ROW,COL) = 100.0*TABPER(ITH,ROW,COL,TAB) / TABPER(1,BASETOT,COL,TAB)
              IF (TABAMT(1,BASETOT,COL,TAB) > 0.0)  &
                   PCTAMT(ROW,COL) = 100.0*TABAMT(ITH,ROW,COL,TAB) / TABAMT(1,BASETOT,COL,TAB)
           ENDDO    !!  End of loop over COL
        ENDDO      !!  End of loop over ROW

        !-------- Calculations for the summary table
        DO ROW = 1,NSUMROWS
           SUMPCTHH(ROW) = 0.0
           SUMPCTPER(ROW) = 0.0
           SUMPCTAMT(ROW) = 0.0
           SUMAMTCHG(ROW) = 0.0
           IF (TABHHLD(1,BASETOT,NINC,TAB) > 0.0) &
                SUMPCTHH (ROW) = 100.0*SUMHH (ROW)  / TABHHLD(1,BASETOT,NINC,TAB)
           IF (TABPER(1,BASETOT,NINC,TAB) > 0.0) &
                SUMPCTPER(ROW) = 100.0*SUMPER(ROW)  / TABPER(1,BASETOT,NINC,TAB)
           IF (TABAMT(1,BASETOT,NINC,TAB) > 0.0) &
                SUMPCTAMT(ROW) = 100.0*SUMAMT(ROW)  / TABAMT(1,BASETOT,NINC,TAB)
           IF (SUMHH(ROW) > 0.0) THEN
              SUMAMTCHG (ROW) = SUMAMT (ROW)/SUMHH(ROW)
              SUMAMTB   (ROW) = SUMAMTB(ROW)/SUMHH(ROW)
           ENDIF
        ENDDO    !!  End of loop over ROW

        !-------  Page 1 of each table (Summary)

        CALL ISNEWPG(TABFILE, PAGE_BREAK_NUMLINES + 1)

        PLANTEMP = 'PLAN ' //PLANNBR_TABLE(ITH-1) // ': '//PLANNAME_TABLE(ITH-1)
        TYPETEMP = 'UNIVERSE: ' // TYPLAB(TAB)

        CALL CENTER_TEXT(PLANTEMP,80)
        CALL CENTER_TEXT(TYPETEMP,70)


        WRITE(TABFILE,3000) TYPETEMP , PLANTEMP

3000    FORMAT( &
             T42,'                          '//                               &
             T42,'         SUMMARY GAINER/LOSER TABLE'/                       &
             T31,A70//                                                        &
             T26,A80//                                                        &
             1X,131('-')/                                                     &
             T58,'Percent of Baselaw Totals'/                                 &
             T45,54('-'), T107,'Average',                    T121,'Average'/  &
             T45,'     Units              Persons',                           &
             T84,'   Benefits',  T106,'$ Change',T121,'Baselaw'/  &
             T45,'Gaining/Losing      Gaining/Losing' ,                       &
             T84,'  Gained/Lost',T106,'Per Unit',T121,'Benefit'/  &
             1X,131('-')/)



        WRITE (TABFILE,3010)               &
             (SUMPCTHH(ROW)                &
             ,tab_gl_stats(1, ith-1, row, tab)  &
             ,SUMPCTPER(ROW)               &
             ,tab_gl_stats(2, ith-1, row, tab)  &
             ,SUMPCTAMT(ROW)               &
             ,tab_gl_stats_ben(ith-1, row,tab) &
             ,SUMAMTCHG(ROW)               &
             ,SUMAMTB  (ROW), ROW = 1, NSUMROWS)
             
      ! Open table (array of arrays) in JSON file with correct name
      if (tab == 1) then
        WRITE (JSON_FILE, *) '"Table 2": ['
      else
        WRITE (JSON_FILE, *) '"Table 2', CHAR(tab + 63), '": ['
      end if

      ! Write row by row

      do row=1, nsumrows
        IF (dostats(nth)) THEN
          IF (row == 1) THEN 
            WRITE (JSON_FILE, 1221, ADVANCE='no') TRIM(rowlab(row)), &
              SUMPCTHH(ROW), TRIM(tab_gl_stats(1, ith-1, row, tab)), &
              SUMPCTPER(ROW), TRIM(tab_gl_stats(2, ith-1, row, tab)), &
              SUMAMTB(ROW)
          ELSE
            WRITE (JSON_FILE, 1121, ADVANCE='no') TRIM(rowlab(row)), &
              SUMPCTHH(ROW), TRIM(tab_gl_stats(1, ith-1, row, tab)), &
              SUMPCTPER(ROW), TRIM(tab_gl_stats(2, ith-1, row, tab)), &
              SUMPCTAMT(ROW), TRIM(tab_gl_stats_ben(ith-1, row, tab)), &
              SUMAMTCHG(ROW), &
              SUMAMTB(ROW)
          END IF
        ELSE
          IF (row == 1) THEN 
            WRITE (JSON_FILE, 1211, ADVANCE='no') TRIM(rowlab(row)), &
              SUMPCTHH(ROW), &
              SUMPCTPER(ROW), &
              SUMAMTB(ROW)
          ELSE
            WRITE (JSON_FILE, 1111, ADVANCE='no') TRIM(rowlab(row)), &
              SUMPCTHH(ROW), &
              SUMPCTPER(ROW), &
              SUMPCTAMT(ROW), &
              SUMAMTCHG(ROW), &
              SUMAMTB(ROW)
          END IF
        END IF
        IF (row /= nsumrows) WRITE (JSON_FILE, *) ','
      end do

      ! Close table, get ready for the next one
      WRITE (JSON_FILE, *) ! newline
      WRITE (JSON_FILE, *) '],'

      tab_prefix = 'tab2'
      if (tab /= 1) tab_prefix = TRIM(tab_prefix) // CHAR(tab + 63)
      WRITE(JSON_FILE, '(A,A,A,I12)') ' "', TRIM(tab_prefix), 'units": ', NINT(tabhhld(1, basetot, ninc, tab),   &
            SELECTED_INT_KIND(12)), ','
      WRITE(JSON_FILE, '(A,A,A,I12)') ' "', TRIM(tab_prefix), 'persons": ', NINT(tabper(1, basetot, ninc, tab),  &
            SELECTED_INT_KIND(12)), ','
      WRITE(JSON_FILE, '(A,A,A,I12)') ' "', TRIM(tab_prefix), 'benefits": ', NINT(tabamt(1, basetot, ninc, tab), &
            SELECTED_INT_KIND(12)), ','

! JSON format for each row of table
1111 FORMAT('["', A, '",', 4(F8.2, ','), F8.2, ']') ! Weighted
1211 FORMAT('["', A, '",', 2(F8.2, ','), '"n.a.","n.a.",', F8.2, ']') ! Weighted, first row 
1121 FORMAT('["', A, '",', 3(F8.2, ',"', A, '",'), F8.2, ',', F8.2, ']') ! Weighted, with stat sig
1221 FORMAT('["', A, '",', 2(F8.2, ',"', A, '",'), '"n.a.","","n.a.",', F8.2, ']') ! Weighted, with stat sig, first row 


3010    FORMAT( &
             4X,'NO CHANGE'                       ,T47,F8.2,a1,T67,F8.2,a1,T86,F8.2,a1,T105,F8.2,T120,F8.2  &
             //, 4X,'CHANGE'                          ,T47,F8.2,a1,T67,F8.2,a1,T86,F8.2,a1,T105,F8.2,T120,F8.2  &
             //, 7X,'LOSERS'                                     &
             //, 7X,'   No Longer Eligible           ',T47,F8.2,a1,T67,F8.2,a1,T86,F8.2,a1,T105,F8.2,T120,F8.2  &
             //, 7X,'   Still Eligible but           '           &
             /, 7X,'   Not Participating in Reform  ',T47,F8.2,a1,T67,F8.2,a1,T86,F8.2,a1,T105,F8.2,T120,F8.2  &
             //, 7X,'   Still Participating with     '          &
             /, 7X,'   Lower Benefits               ',T47,F8.2,a1,T67,F8.2,a1,T86,F8.2,a1,T105,F8.2,T120,F8.2  &
             //, 7X,'   Total Losers                 ',T47,F8.2,a1,T67,F8.2,a1,T86,F8.2,a1,T105,F8.2,T120,F8.2  &
             //, 7X,'GAINERS'                                   &
             //, 7X,'   Newly Eligible and           '          &
             /, 7X,'   Participating in Reform      ',T47,F8.2,a1,T67,F8.2,a1,T86,F8.2,a1,T105,F8.2,T120,F8.2  &
             //, 7X,'   Still Eligible but           '          &
             /, 7X,'   Newly Participating in Reform',T47,F8.2,a1,T67,F8.2,a1,T86,F8.2,a1,T105,F8.2,T120,F8.2  &
             //, 7X,'   Still Participating with'               &
             /, 7X,'   Higher Benefits              ',T47,F8.2,a1,T67,F8.2,a1,T86,F8.2,a1,T105,F8.2,T120,F8.2  &
             //, 7X,'   Total Gainers                ',T47,F8.2,a1,T67,F8.2,a1,T86,F8.2,a1,T105,F8.2,T120,F8.2  &
             //)


        !-------  Write out baselaw totals
        COMMA_field(1) = COMMA8(TABHHLD(1,BASETOT,NINC,TAB),FIELD_LEN)
        COMMA_field(2) = COMMA8(TABPER (1,BASETOT,NINC,TAB),FIELD_LEN)
        COMMA_field(3) = COMMA8(TABAMT (1,BASETOT,NINC,TAB),FIELD_LEN)
        
        
        WRITE(TABFILE,6000) COMMA_FIELD


        IF (create_table_extracts) THEN

           DO row = 1, nsumrows
              WRITE (36, 3601)                    &
                   "t_gl        "                   &
                   ,ith                              &
                   ,ADJUSTL(PLANNAME_TABLE(ITH-1))   &
                   ,listlab(tab)(1:20)               &
                   ,rowlab(row)                      &
                   ,SUMPCTHH(ROW)                    &
                   ,tab_gl_stats(1, ith-1, row, tab) &
                   ,SUMPCTPER(ROW)                   &
                   ,tab_gl_stats(2, ith-1, row, tab) &
                   ,SUMPCTAMT(ROW)                   &
                   ,tab_gl_stats_ben(ith-1, row,tab) &
                   ,SUMAMTCHG(ROW)                   &
                   ,SUMAMTB  (ROW)                   &
                   ,TABHHLD(1,BASETOT,NINC,TAB)      &
                   ,TABPER (1,BASETOT,NINC,TAB)      &
                   ,TABAMT (1,BASETOT,NINC,TAB)
           END DO
        END IF

3601    FORMAT(a12, i3, 2x, a40, 2x, a20, 2x, a40, 2x, 3(f10.4, a2), 2f10.2, 3f12.0)



        !-----    Write footer
        SELECT CASE(GLUNIT)
        CASE(1)
           WRITE (TABFILE,7010)
        CASE(2)
           WRITE (TABFILE,7020)
        END SELECT
        WRITE (TABFILE,7000)
        WRITE (TABFILE,1290)


        END_LABEL =                                                &
             '<<## END OF FSTAMP TABLE   : GAINER/LOSER ' //   &
             'PLAN ' //plannbr_table(ith-1) // ': ' //         &
             LISTLAB(TAB)(1:LAB_LENGTH(TAB)) //               &
             ' SUMMARY ##>>'


        !-------- Page 2 of each table (Detailed)

        CALL ISNEWPG(TABFILE, PAGE_BREAK_NUMLINES + 1)

        WRITE(TABFILE,4000)  TYPETEMP, PLANTEMP, (COLLAB1(COL),COL=1,NINC)

        IF (SHOW_ELIG) THEN
           WRITE(TABFILE,4200)                                &
                (ROWLAB1(ROW), (PCTHH (ROW,COL),COL=1,NINC),  &
                ROWLAB2(ROW), (PCTPER(ROW,COL),COL=1,NINC),  &
                ROWLAB3(ROW), (PCTAMT(ROW,COL),COL=1,NINC),ROW=1,NROWS)

        ELSE   ! don't show rows with non-participant data
           WRITE(TABFILE,4210)                                        &
                (ROWLAB1(ROW), (PCTHH (ROW,COL),COL=1,NINC),          &
                ROWLAB2(ROW), (PCTPER(ROW,COL),COL=1,NINC),          &
                ROWLAB3(ROW), (PCTAMT(ROW,COL),COL=1,NINC),ROW=1,1), &
                (ROWLAB1(ROW), (PCTHH(ROW,COL),COL=1,NINC),           &
                ROWLAB2(ROW), (PCTPER(ROW,COL),COL=1,NINC),          &
                ROWLAB3(ROW), (PCTAMT(ROW,COL),COL=1,NINC),ROW=3,6), &
                (ROWLAB1(ROW), (PCTHH(ROW,COL),COL=1,NINC),           &
                ROWLAB2(ROW), (PCTPER(ROW,COL),COL=1,NINC),          &
                ROWLAB3(ROW), (PCTAMT(ROW,COL),COL=1,NINC),ROW=9,11)
        END IF

        !-------  Write out baselaw totals
        WRITE(TABFILE,6000) COMMA_FIELD

        !----   Write footer
        SELECT CASE(GLUNIT)
        CASE(1)
           WRITE (TABFILE,7010)
        CASE(2)
           WRITE (TABFILE,7020)
        END SELECT
        WRITE (TABFILE,7000)


        END_LABEL =                                               &
             '<<## END OF FSTAMP TABLE 2B: GAINER/LOSER ' //  &
             'PLAN ' //plannbr_table(ith-1) // ': ' //        &
             LISTLAB(TAB)(1:LAB_LENGTH(TAB)) //              &
             ' DETAIL ##>>'



     ENDDO    !!  End of loop over TAB

     !-------------------------------------------------------------------
     !------------------------- Regional Table --------------------------
     !-------------------------------------------------------------------

     DO ROW = 1,NROWS

        !------ Total region level columns into TOTAL column
        DO COL = 1,NREG-1
           REGHH (ITH,ROW,NREG) = REGHH (ITH,ROW,NREG) + REGHH (ITH,ROW,COL)
           REGPER(ITH,ROW,NREG) = REGPER(ITH,ROW,NREG) + REGPER(ITH,ROW,COL)
           REGAMT(ITH,ROW,NREG) = REGAMT(ITH,ROW,NREG) + REGAMT(ITH,ROW,COL)
        ENDDO    !! End of loop over COL
     ENDDO  !!  End of loop over ROW

     DO ROW = 1,NROWS

        !-------  Calculate printed table cell values
        DO COL = 1,NREG
           PCTHH (ROW,COL) = 0.0
           PCTPER(ROW,COL) = 0.0
           PCTAMT(ROW,COL) = 0.0
           IF (REGHH(1,BASETOT,COL) > 0.0) &
                PCTHH(ROW,COL) = 100.0* REGHH(ITH,ROW,COL) / REGHH(1,BASETOT,COL)
           IF (REGPER(1,BASETOT,COL) > 0.0) &
                PCTPER(ROW,COL) = 100.0*REGPER(ITH,ROW,COL) / REGPER(1,BASETOT,COL)
           IF (REGAMT(1,BASETOT,COL) > 0.0) &
                PCTAMT(ROW,COL) = 100.0*REGAMT(ITH,ROW,COL) / REGAMT(1,BASETOT,COL)
        ENDDO    !!  End of loop over COL
     ENDDO      !!  End of loop over ROW

     CALL ISNEWPG(TABFILE, PAGE_BREAK_NUMLINES + 1)

     TYPETEMP = 'UNIVERSE: ' // TYPLAB(1)
     CALL CENTER_TEXT(TYPETEMP,70)

     WRITE(TABFILE,5000) TYPETEMP , PLANTEMP, (COLLAB2(COL),COL=1,NREG)

     IF (SHOW_ELIG) THEN
        WRITE(TABFILE,5200)                                &
             (ROWLAB1(ROW), (PCTHH (ROW,COL),COL=1,NREG),  &
             ROWLAB2(ROW), (PCTPER(ROW,COL),COL=1,NREG),  &
             ROWLAB3(ROW), (PCTAMT(ROW,COL),COL=1,NREG),ROW=1,NROWS)

     ELSE   ! don't show rows with non-participant data
        WRITE(TABFILE,5210)                                       &
             (ROWLAB1(ROW), (PCTHH (ROW,COL),COL=1,NREG),           &
             ROWLAB2(ROW), (PCTPER(ROW,COL),COL=1,NREG),           &
             ROWLAB3(ROW), (PCTAMT(ROW,COL),COL=1,NREG),ROW=1,1),  &
             (ROWLAB1(ROW), (PCTHH (ROW,COL),COL=1,NREG),           &
             ROWLAB2(ROW), (PCTPER(ROW,COL),COL=1,NREG),           &
             ROWLAB3(ROW), (PCTAMT(ROW,COL),COL=1,NREG),ROW=3,6),  &
             (ROWLAB1(ROW), (PCTHH (ROW,COL),COL=1,NREG),           &
             ROWLAB2(ROW), (PCTPER(ROW,COL),COL=1,NREG),           &
             ROWLAB3(ROW), (PCTAMT(ROW,COL),COL=1,NREG),ROW=9,11)
     END IF

     !------ Write out baselaw totals
     COMMA_field(1) = COMMA8(REGHH (1,BASETOT,NREG),FIELD_LEN)
     COMMA_field(2) = COMMA8(REGPER(1,BASETOT,NREG),FIELD_LEN)
     COMMA_field(3) = COMMA8(REGAMT(1,BASETOT,NREG),FIELD_LEN)
     

     WRITE(TABFILE,6000)  COMMA_FIELD

     !----   Write footer
     SELECT CASE(GLUNIT)
     CASE(1)
        WRITE (TABFILE,7010)
     CASE(2)
        WRITE (TABFILE,7020)
     END SELECT
     WRITE (TABFILE,7000)

  ENDDO           

  CALL FS_TAB_state_gainer_loser( &
       .FALSE.           &
       ,.FALSE.           &
       ,.FALSE.           &
       ,.FALSE.           &
       ,.FALSE.           &
       ,.FALSE.           &
       ,0                 &
       ,0.0               &
       ,.FALSE.           &
       ,.FALSE.           &
       ,0                 &
       ,0                 &
       ,0                 &
       )


  RETURN

  !----------------------------------------------------------------------
  ! FORMAT STATEMENTS
  !----------------------------------------------------------------------
2020 FORMAT( &
       ' *** ERROR IN TABLE 2 (GAINER/LOSER) SUBSCRIPTS ***',  &
       '  ICAT = ',I5,                                         &
       '  REGION = ',I5,                                       &
       '  KTH = ',I2,                                          &
       '  IPOV = ',I5)


  !------------------------
  !---- Detailed table
  !------------------------
4000 FORMAT( &
       T42,'                          '//                        &
       T42,'         DETAILED GAINER/LOSER TABLE'/               &
       T31,A70//                                                 &
       T26,A80//                                                 &
       1X,131('-')/                                              &
       T69,'         Percent of Baselaw Totals by           '/   &
       5X,' Participation     Amount of',                          &
       T69,'Gross Income as a Percentage of the Poverty Line'/   &
       5X,' Status (Y/N)      Benefit  ',                          &
       T58,68('-')/                                              &
       5X,'                   Gain/Loss',T52,6A12/               &
       1X,131('-')/)

  !---- SIPP/CPS ROWS
4200 FORMAT( 6X,   'LOSERS'//                            &
       5(5X,A26,7X,'Units Losing:   ',1X,F8.2, 5(F12.2)/  &
       5X,A26,7X,'Persons Losing: ',1X,F8.2, 5(F12.2)/  &
       5X,A26,7X,'Benefits Lost:  ',1X,F8.2, 5(F12.2)//),&
       6X,   'NO CHANGE'//                                &
       5X,A26,7X,'Units:          ',1X,F8.2, 5(F12.2)/  &
       5X,A26,7X,'Persons:        ',1X,F8.2, 5(F12.2)/  &
       5X,A26,7X,'Benefits:       ',1X,F8.2, 5(F12.2)// &
       6X,   'GAINERS  '//                                &
       5(5X,A26,7X,'Units Gaining:  ',1X,F8.2, 5(F12.2)/  &
       5X,A26,7X,'Persons Gaining:',1X,F8.2, 5(F12.2)/  &
       5X,A26,7X,'Benefits Gained:',1X,F8.2, 5(F12.2)//))

  !---- QCMINI ROWS (because QC can't simulate expansive reforms)
4210 FORMAT( 6X,   'LOSERS'//                                &
       4(5X,A26,7X,'Units Losing:   ',1X,F8.2, 5(F12.2)/      &
       5X,A26,7X,'Persons Losing: ',1X,F8.2, 5(F12.2)/      &
       5X,A26,7X,'Benefits Lost:  ',1X,F8.2, 5(F12.2)//),   &
       6X,   'NO CHANGE'//                                    &
       5X,A26,7X,'Units:          ',1X,F8.2, 5(F12.2)/      &
       5X,A26,7X,'Persons:        ',1X,F8.2, 5(F12.2)/      &
       5X,A26,7X,'Benefits:       ',1X,F8.2, 5(F12.2)//     &
       6X,   'GAINERS  '//                                    &
       3(5X,A26,7X,'Units Gaining:  ',1X,F8.2, 5(F12.2)/      &
       5X,A26,7X,'Persons Gaining:',1X,F8.2, 5(F12.2)/      &
       5X,A26,7X,'Benefits Gained:',1X,F8.2, 5(F12.2)//))

  !---- Regional G/L table's format is different from the other wafers
5000 FORMAT(                                                  &
       T42,'                          '//                     &
       T42,'    DETAILED GAINER/LOSER TABLE BY REGION'/       &
       T31,A70//                                              &
       T26,A80//                                              &
       1X,131('-')/                                           &
       5X,' Participation     Amount of',                     &
       T69,'     Percent Change from Baselaw by Region      '/&
       5X,' Status (Y/N)      Benefit  ',                     &
       T56,70('-')/                                           &
       5X,'                   Gain/Loss',T55,5(A12,2X)/       &
       1X,131('-')/)

  !---- SIPP/CPS ROWS
  !---- Regional G/L table's format is different from the other wafers
5200 FORMAT( 6X,   'LOSERS'//                                      &
       5(5X,A26,7X,'Units Losing:    ',3X,F8.2,2X, 4(F12.2,2X)/    &
       5X,A26,7X,'Persons Losing:  ',3X,F8.2,2X, 4(F12.2,2X)/    &
       5X,A26,7X,'Benefits Lost:   ',3X,F8.2,2X, 4(F12.2,2X)//), &
       6X,   'NO CHANGE'//                                         &
       5X,A26,7X,'Units:           ',3X,F8.2,2X, 4(F12.2,2X)/    &
       5X,A26,7X,'Persons:         ',3X,F8.2,2X, 4(F12.2,2X)/    &
       5X,A26,7X,'Benefits:        ',3X,F8.2,2X, 4(F12.2,2X)//   &
       6X,   'GAINERS  '//                                         &
       5(5X,A26,7X,'Units Gaining:   ',3X,F8.2,2X, 4(F12.2,2X)/    &
       5X,A26,7X,'Persons Gaining: ',3X,F8.2,2X, 4(F12.2,2X)/    &
       5X,A26,7X,'Benefits Gained: ',3X,F8.2,2X, 4(F12.2,2X)//))

  !---- QCMINI ROWS (because QC can't simulate expansive reforms)
  !---- Regional G/L table's format is different from the other wafers
5210 FORMAT( 6X,   'LOSERS'//                                      &
       4(5X,A26,7X,'Units Losing:    ',3X,F8.2,2X, 4(F12.2,2X)/   &
       5X,A26,7X,'Persons Losing:  ',3X,F8.2,2X, 4(F12.2,2X)/   &
       5X,A26,7X,'Benefits Lost:   ',3X,F8.2,2X, 4(F12.2,2X)//),&
       6X,   'NO CHANGE'//                                        &
       5X,A26,7X,'Units:           ',3X,F8.2,2X, 4(F12.2,2X)/   &
       5X,A26,7X,'Persons:         ',3X,F8.2,2X, 4(F12.2,2X)/   &
       5X,A26,7X,'Benefits:        ',3X,F8.2,2X, 4(F12.2,2X)//  &
       6X,   'GAINERS  '//                                        &
       3(5X,A26,7X,'Units Gaining:   ',3X,F8.2,2X, 4(F12.2,2X)/   &
       5X,A26,7X,'Persons Gaining: ',3X,F8.2,2X, 4(F12.2,2X)/   &
       5X,A26,7X,'Benefits Gained: ',3X,F8.2,2X, 4(F12.2,2X)//))
  !------------------------
  !---- Footnotes
  !------------------------
6000 FORMAT(1X,131('-')//           &
       1X,'Baselaw Units:    ',A15/ &
       1X,'Baselaw Persons:  ',A15/ &
       1X,'Baselaw Benefits: ',A15)

7000 FORMAT(                                                           &
       /  1X, 'Total gainers are equal to the sum of (1) baselaw ' &
       , 'participants who gain benefits under reform, and '    &
       , '(2) baselaw nonparticipants'                          &
       /  1X, '(elig. or inelig.) who participate under reform. '  &
       , 'Since it is possible that the sum of total gainers '  &
       , 'can exceed the total number'                          &
       /  1X, 'of baselaw participants, the number of gainers '    &
       , 'relative to the number of baselaw participants may '  &
       , 'exceed 100 percent.'                                  &
       )
7010 FORMAT(/1X, 'Gainer/loser table displays the effect of the '     &
       , 'reform on the household, which means that '         &
       , 'all SNAP units within the household'                &
       /1X, 'are combined into one unit (GLUNIT = 1).'    )

7020 FORMAT(/1X, 'Gainer/loser table displays the effect of the '     &
       , 'reform on the baselaw unit, which means that '      &
       , 'reform SNAP units are combined '                    &
       /1X, 'into a unit that conforms to the baselaw '          &
       , 'unit (GLUNIT = 2).' )

1290 FORMAT (&
       /,1x,'Statistics key:' &
       ,/,1x,'* Change is statistically different from zero at a 90% level of significance.' &
       )



END SUBROUTINE FS_TAB_gainer_loser
