!**************************************************************************************************
! Source File:  FStab_protect_gl.F90                
! Called By:    FS_TABLES                  
!
! TABLE 6 -  Gainer/Loser Tables
!
!**************************************************************************************************
    SUBROUTINE FS_tab_protected_gainer_loser(        &
                KTH               &   !    PLAN NUMBER
               ,FSUSIZE           &   !    FOOD STAMP UNIT SIZE
               ,FSNELDER          &
               ,FSNKID            &
               ,FSNDIS            &
               ,FSNFEMALE         &
               ,FSNMALE           &
               ,FSHRACE           &
               ,FSHETHNIC         &
               ,FSHORIGIN         &
               ,BASE_FSBEN        &   !    BASELAW BENEFIT AMOUNT
               ,BASE_PARTIC       &   !    LOGICAL FOR PARTICIPATION IN BASELAW
               ,FSBEN             &   !    PLAN BENEFIT AMOUNT
               ,PARTIC            &   !    LOGICAL FOR PARTICIPATION
               ,WGT               &    !    WEIGHT
               ,jkist             &
               ,junit             &
               )


    USE GLOBAL
    USE GLOBPARM, ONLY: MODEL_LABEL

    USE FSWORK,  ONLY:      &
       PLANNAME_TABLE       &
      ,PLANNBR_TABLE        &
      ,USE_HEAD_RACE        &
      ,AREA_OF_ORIGIN       &
      ,PERSON_LEVEL_DISAB   &
      ,tab_protect_gl_stats          &
      ,var_protect_gl_stats          &
      ,create_table_extracts

    USE FSPARM , ONLY: GLUNIT, GLTABS, prlevel, JSON_FILE, dostats
    use Utils
    
    IMPLICIT NONE

!---Declare parameters from calling program

    INTEGER, intent(in) :: KTH
    INTEGER, intent(in) :: FSUSIZE
    INTEGER, intent(in) :: FSNELDER
    INTEGER, intent(in) :: FSNKID
    INTEGER, intent(in) :: FSNDIS
    INTEGER, intent(in) :: FSNFEMALE
    INTEGER, intent(in) :: FSNMALE
    INTEGER, intent(in) :: FSHRACE
    INTEGER, intent(in) :: FSHETHNIC
    INTEGER, intent(in) :: FSHORIGIN
    INTEGER, intent(in) :: BASE_FSBEN
    LOGICAL, intent(in) :: BASE_PARTIC
    INTEGER, intent(in out) :: FSBEN
    LOGICAL, intent(in)  :: PARTIC
    REAL   , intent(in)  :: WGT

    INTEGER, intent(in out) :: jkist
    INTEGER, intent(in out) :: junit

!---Variables for tables
    INTEGER  ::   NBR_OF_KTHS = 0
    INTEGER  ::   I, J, K, L, M, II, ICAT, IB1, IB2, BEN_DIF, AMT, icat2

    CHARACTER( 80) :: PLANTEMP
    CHARACTER( 80) :: PLANTEMP2

    INTEGER :: NUM_ADULTS, NUM_NOT_DISAB, ORIGIN_IDX, RACE_IDX

    integer, PARAMETER :: NROWS = 30
    INTEGER, PARAMETER :: NCAT  = 9
    INTEGER, parameter :: MAX_KTH = 10


!---- Number of units by characteristic and KTH (cells)
      REAL (8) ::     PART_VALUE(NROWS, NCAT, MAX_KTH + 1) = 0.0
      REAL (8) :: WGT_PART_VALUE(NROWS, NCAT, MAX_KTH + 1) = 0.0


!---- Percent change by characteristic and KTH (cells)
      REAL (8)  ::     PART_PCT(NROWS, NCAT, MAX_KTH + 1) = 0.0
      REAL (8)  :: WGT_PART_PCT(NROWS, NCAT, MAX_KTH + 1) = 0.0



!---- BENEFITS
      REAL :: per_cap_ben

      REAL (8) ::     PART_BEN(NROWS, NCAT, MAX_KTH + 1) = 0.0
      REAL (8) :: WGT_PART_BEN(NROWS, NCAT, MAX_KTH + 1) = 0.0

      REAL (8) ::     AVG_BEN_GAIN (NROWS, MAX_KTH + 1) = 0.0
      REAL (8) :: WGT_AVG_BEN_GAIN (NROWS, MAX_KTH + 1) = 0.0
      REAL (8) ::     AVG_BEN_LOSS (NROWS, MAX_KTH + 1) = 0.0
      REAL (8) :: WGT_AVG_BEN_LOSS (NROWS, MAX_KTH + 1) = 0.0


!--- Table variables
    REAL(8) :: PCT_FIELD (NROWS, NCAT, MAX_KTH+1, 2) = 0.0
    REAL(8) :: AVG_GAIN  (NROWS, MAX_KTH+1, 2)       = 0.0
    REAL(8) :: AVG_LOSS  (NROWS, MAX_KTH+1, 2)       = 0.0

!---Comma display fields
    INTEGER, PARAMETER ::  FIELD_LEN = 10
    INTEGER, PARAMETER ::  FIELD_LEN2 = 8
    CHARACTER(LEN=FIELD_LEN) :: COMMA_FIELD (NROWS,NCAT,MAX_KTH+1)  = " "
    CHARACTER(LEN=FIELD_LEN2) :: COMMA_FIELD2 (NROWS,NCAT,MAX_KTH+1)  = " "

!--- EXCEL OUTPUT
    REAL(8) :: XL_FIELD (NROWS,NCAT,MAX_KTH+1)  = 0.0
    REAL(8) :: XL_FIELD2 (NROWS,NCAT,MAX_KTH+1)  = 0.0


!---- Row labels for both tables
      integer, parameter :: empty_rows = 20
      integer, dimension(empty_rows) :: skip_row = &
         (/2,3, 7,8, 11, 14, 17,18, 21,22,  25,26, 35,36, 38,39, 43,44, 47,48/)
      CHARACTER(len=18) :: ROWLAB(nrows+empty_rows)= (/  &
      'Total Units       '  &  !  1
     ,'                  '  &  !  2
     ,'Units with:       '  &  !  3
     ,' Children (<18)   '  &  !  4
     ,' Adults   (18-59) '  &  !  5
     ,' Elderly  (60+)   '  &  !  6
     ,'                  '  &  !  7
     ,'Race of head:     '  &  !  8
     ,' White only       '  &  !  9
     ,' Black only       '  &  ! 10
     ,'                  '  &  ! 11
     ,'                  '  &  ! 12
     ,' Asian Only       '  &  ! 13
     ,'                  '  &  ! 14
     ,' All Others       '  &  ! 15
     ,' Unknown          '  &  ! 16
     ,'                  '  &  ! 17
     ,'Ethnicity of head:'  &  ! 18
     ,' Hispanic         '  &  ! 19
     ,' Not Hispanic     '  &  ! 20
     ,'                  '  &  ! 21
     ,'Units w/ Disabled:'  &  ! 22
     ,' Yes              '  &  ! 23
     ,' No               '  &  ! 24
     ,'                  '  &  ! 25
     ,'Origin of head    '  &  ! 26
     ,' North America    '  &  ! 27
     ,' Europe           '  &  ! 28
     ,' Asia/Pacific Is. '  &  ! 29
     ,' Middle East      '  &  ! 30
     ,' Central/S.America'  &  ! 31
     ,' Africa           '  &  ! 32
     ,' Elsewhere        '  &  ! 33
     ,' Unknown          '  &  ! 34
     ,'                  '  &  ! 35
     ,'                  '  &  ! 36
     ,'Total Persons     '  &  ! 37
     ,'                  '  &  ! 38
     ,'Age:              '  &  ! 39
     ,' Children (<18)   '  &  ! 40
     ,' Adults   (18-59) '  &  ! 41
     ,' Elderly  (60+)   '  &  ! 42
     ,'                  '  &  ! 43
     ,'Sex:              '  &  ! 44
     ,' Male             '  &  ! 45
     ,' Female           '  &  ! 46
     ,'                  '  &  ! 47
     ,'Disability:       '  &  ! 48
     ,' Yes              '  &  ! 49
     ,' No               '  &  ! 50
     /)



     character(41) :: json_rowlab1(NROWS) = (/&
      "Total units                              ", &
      "Units with:                              ", &
      "                                         ", &
      "                                         ", &
      "Race of head:                            ", &
      "                                         ", &
      "                                         ", &
      "                                         ", &
      "                                         ", &
      "                                         ", &
      "Ethnicity of head:                       ", &
      "                                         ", &
      "Units with individuals with disabilities:", &
      "                                         ", &
      "Area of origin of head:                  ", &
      "                                         ", &
      "                                         ", &
      "                                         ", &
      "                                         ", &
      "                                         ", &
      "                                         ", &
      "                                         ", &
      "Total persons                            ", &
      "Age:                                     ", &
      "                                         ", &
      "                                         ", &
      "Sex:                                     ", &
      "                                         ", &
      "Disability:                              ", &
      "                                         " &
     /)

     character(38) :: json_rowlab2(NROWS) = (/&
      "                                      ", &
      "Children (less than 18 years)         ", &
      "Adults (18 to 59 years)               ", &
      "Elderly individuals (60 years or more)", &
      "White alone, not of Hispanic origin   ", &
      "Black alone, not of Hispanic origin   ", &
      "Hispanic (see below)                  ", &
      "Asian alone                           ", &
      "Other                                 ", &
      "Unknown                               ", &
      "Hispanic                              ", &
      "Not Hispanic                          ", &
      "Yes                                   ", &
      "No                                    ", &
      "North America                         ", &
      "Europe                                ", &
      "Asia/Pacific Islands                  ", &
      "Middle East                           ", &
      "Central/South America                 ", &
      "Africa                                ", &
      "Elsewhere                             ", &
      "Unknown                               ", &
      "                                      ", &
      "Children (less than 18 years)         ", &
      "Adults (18 to 59 years)               ", &
      "Elderly (60 years or more)            ", &
      "Male                                  ", &
      "Female                                ", &
      "Yes                                   ", &
      "No                                    " &
     /)

!---By-pass calculations if print requested
    IF (KEOF == 3) GOTO 900

!**************************************************************
!   Perform table calculations
!**************************************************************
    IF (FSNELDER > FSUSIZE .OR. &
        FSNKID   > FSUSIZE .OR. &
        FSNDIS   > FSUSIZE) THEN
       CALL DEBUG_MSG("INVALID TAB 10 PARM", 1)
    END IF



    !!  testing
    if (kfreq > 0 .and. prlevel(nth) >= 9) then
       CALL ISNEWPG (PRFILE,18)
       write(prfile, 2020 ) &
                FSUSIZE           &
               ,FSNELDER          &
               ,FSNKID            &
               ,FSNDIS            &
               ,FSNFEMALE         &
               ,FSNMALE           &
               ,FSHRACE           &
               ,FSHETHNIC         &
               ,FSHORIGIN         &
               ,BASE_FSBEN        &
               ,BASE_PARTIC       &
               ,FSBEN             &
               ,PARTIC            &
               ,KTH               &
               ,WGT

2020  format(/, t2, "Table 70 test"  &
         ,/,t2,"FSUSIZE     :", i10  &
         ,/,t2,"FSNELDER    :", i10  &
         ,/,t2,"FSNKID      :", i10  &
         ,/,t2,"FSNDIS      :", i10  &
         ,/,t2,"FSNFEMALE   :", i10  &
         ,/,t2,"FSNMALE     :", i10  &
         ,/,t2,"FSHRACE     :", i10  &
         ,/,t2,"FSHETHNIC   :", i10  &
         ,/,t2,"FSHORIGIN   :", i10  &
         ,/,t2,"BASE_FSBEN  :", i10  &
         ,/,t2,"BASE_PARTIC :", L10  &
         ,/,t2,"FSBEN       :", i10  &
         ,/,t2,"PARTIC      :", L10  &
         ,/,t2,"KTH         :", i10  &
         ,/,t2,"WGT         :", f10.4 )


    end if




!---Keep track of highest KTH
    IF (KTH > NBR_OF_KTHS)  NBR_OF_KTHS = KTH


    IB1 = 0
    IB2 = 0
    IF (BASE_PARTIC) IB1 = BASE_FSBEN
    IF (PARTIC) IB2 = FSBEN
    BEN_DIF = IB2 - IB1

    !!  BASELAW

    IF (KTH == 1) THEN
       ICAT = 1    !! BASETOT -  Baselaw totals accumulated in row 1
       AMT = 0
       IF (BASE_PARTIC) AMT = BASE_FSBEN
    ELSE
       AMT  =  BEN_DIF

!------ Set the row index to the tables using PARTIC and BEN_DIF
!------ Skip persons who did not participate in either baselaw or reform
       IF (.NOT. BASE_PARTIC .AND. .NOT. PARTIC) then
          ICAT = 0
          goto 101
       end if


       IF (.NOT. BASE_PARTIC) THEN      !! Baselaw: N,  Reform: Y
          IF (BASE_FSBEN <= 0) THEN
             ICAT = 5                   !! Ineligible for baselaw
          ELSE
             ICAT = 6                   !! Eligible for baselaw
         ENDIF

       ELSE IF (.NOT. PARTIC) THEN      !! Baselaw: Y,  Reform: N
          IF (FSBEN <= 0) THEN
             ICAT = 1                   !! Ineligible for reform
          ELSE
             ICAT = 2                   !! Eligible for reform
          ENDIF
       ELSE                             !! Baselaw: Y,  Reform Y
          SELECT CASE (BEN_DIF)
             CASE (:-1)
                ICAT = 3                !! Loser
             CASE (0)
                ICAT = 4                !! No change
            CASE (1:)
                ICAT = 7                !! Gainer
          END SELECT
       ENDIF
    ENDIF

    IF (KTH == 1) FSBEN = BASE_FSBEN

    IF (KTH == 1 .and. .not. base_partic) goto 101




!------------------------------------------------------------------------------

!--- 1 Total units
    PART_VALUE (1, ICAT, KTH) = PART_VALUE (1, ICAT, KTH) + 1
    WGT_PART_VALUE (1, ICAT, KTH) = WGT_PART_VALUE (1, ICAT, KTH) + WGT

!--- 2 Units with kids
    IF (FSNKID > 0) THEN
       PART_VALUE (2, ICAT, KTH) = PART_VALUE (2, ICAT, KTH) + 1
       WGT_PART_VALUE (2, ICAT, KTH) = WGT_PART_VALUE (2, ICAT, KTH) + WGT
    END IF

!--- 3 Units with adults
    NUM_ADULTS = FSUSIZE - FSNELDER - FSNKID
    IF (NUM_ADULTS > 0) THEN
       PART_VALUE (3, ICAT, KTH) = PART_VALUE (3, ICAT, KTH) + 1
       WGT_PART_VALUE (3, ICAT, KTH) = WGT_PART_VALUE (3, ICAT, KTH) + WGT
    END IF

!-- 4 Units with elderly
    IF (FSNELDER > 0) THEN
       PART_VALUE (4, ICAT, KTH) = PART_VALUE (4, ICAT, KTH) + 1
       WGT_PART_VALUE (4, ICAT, KTH) = WGT_PART_VALUE (4, ICAT, KTH) + WGT
    END IF


!!   5-10: RACE OF HEAD
    IF (USE_HEAD_RACE) THEN

       RACE_IDX = fshrace + 4

       PART_VALUE (RACE_IDX, ICAT, KTH) = PART_VALUE (RACE_IDX, ICAT, KTH) + 1
       WGT_PART_VALUE (RACE_IDX, ICAT, KTH) = WGT_PART_VALUE (RACE_IDX, ICAT, KTH) + WGT

    END IF

!!  11-12: ETHNICITY OF HEAD

    IF (FSHETHNIC == 1) THEN
       PART_VALUE (11, ICAT, KTH) = PART_VALUE (11, ICAT, KTH) + 1
       WGT_PART_VALUE (11, ICAT, KTH) = WGT_PART_VALUE (11, ICAT, KTH) + WGT
    ELSE
       PART_VALUE (12, ICAT, KTH) = PART_VALUE (12, ICAT, KTH) + 1
       WGT_PART_VALUE (12, ICAT, KTH) = WGT_PART_VALUE (12, ICAT, KTH) + WGT
    END IF

!!  13-14: UNITS WITH DISABLED

    IF (FSNDIS > 0) THEN
       PART_VALUE (13, ICAT, KTH) = PART_VALUE (13, ICAT, KTH) + 1
       WGT_PART_VALUE (13, ICAT, KTH) = WGT_PART_VALUE (13, ICAT, KTH) + WGT
    ELSE
       PART_VALUE (14, ICAT, KTH) = PART_VALUE (14, ICAT, KTH) + 1
       WGT_PART_VALUE (14, ICAT, KTH) = WGT_PART_VALUE (14, ICAT, KTH) + WGT
    END IF

!!  15-22: AREA OF ORIGIN OF HEAD
    IF (AREA_OF_ORIGIN) THEN
       ORIGIN_IDX = fshORIGIN + 14

       PART_VALUE (ORIGIN_IDX, ICAT, KTH) = PART_VALUE (ORIGIN_IDX, ICAT, KTH) + 1
       WGT_PART_VALUE (ORIGIN_IDX, ICAT, KTH) = WGT_PART_VALUE (ORIGIN_IDX, ICAT, KTH) + WGT
    END IF

    !!  PERSON-LEVEL SECTION

!--- 23 Total persons
    PART_VALUE (23, ICAT, KTH) = PART_VALUE (23, ICAT, KTH) + FSUSIZE
    WGT_PART_VALUE (23, ICAT, KTH) = WGT_PART_VALUE (23, ICAT, KTH) + WGT * FSUSIZE

!--- 24 Kids
    PART_VALUE (24, ICAT, KTH) = PART_VALUE (24, ICAT, KTH) + FSNKID
    WGT_PART_VALUE (24, ICAT, KTH) = WGT_PART_VALUE (24, ICAT, KTH) + WGT * FSNKID

!--- 25 Adults
    PART_VALUE (25, ICAT, KTH) = PART_VALUE (25, ICAT, KTH) + NUM_ADULTS
    WGT_PART_VALUE (25, ICAT, KTH) = WGT_PART_VALUE (25, ICAT, KTH) + WGT * NUM_ADULTS

!-- 26 Elderly
    PART_VALUE (26, ICAT, KTH) = PART_VALUE (26, ICAT, KTH) + FSNELDER
    WGT_PART_VALUE (26, ICAT, KTH) = WGT_PART_VALUE (26, ICAT, KTH) + WGT * FSNELDER

!-- 27 Males
    PART_VALUE (27, ICAT, KTH) = PART_VALUE (27, ICAT, KTH) + FSNMALE
    WGT_PART_VALUE (27, ICAT, KTH) = WGT_PART_VALUE (27, ICAT, KTH) + WGT * FSNMALE

!-- 28 Females
    PART_VALUE (28, ICAT, KTH) = PART_VALUE (28, ICAT, KTH) + FSNFEMALE
    WGT_PART_VALUE (28, ICAT, KTH) = WGT_PART_VALUE (28, ICAT, KTH) + WGT * FSNFEMALE


    IF (PERSON_LEVEL_DISAB) THEN

!-- 29 Disabled
       PART_VALUE (29, ICAT, KTH) = PART_VALUE (29, ICAT, KTH) + FSNDIS
       WGT_PART_VALUE (29, ICAT, KTH) = WGT_PART_VALUE (29, ICAT, KTH) + WGT * FSNDIS

!-- 30 Not Disabled
       num_not_disab = fsusize - fsndis
       PART_VALUE (30, ICAT, KTH) = PART_VALUE (30, ICAT, KTH) + NUM_NOT_DISAB
       WGT_PART_VALUE (30, ICAT, KTH) = WGT_PART_VALUE (30, ICAT, KTH) + WGT * NUM_NOT_DISAB

    END IF

!-------------------------------------------------------------------------------
!    BENEFITS
!-------------------------------------------------------------------------------

!--- 1 Total units
    PART_BEN (1, ICAT, KTH) = PART_BEN (1, ICAT, KTH) + AMT
    WGT_PART_BEN (1, ICAT, KTH) = WGT_PART_BEN (1, ICAT, KTH) + WGT * AMT

!--- 2 Units with kids
    IF (FSNKID > 0) THEN
       PART_BEN (2, ICAT, KTH) = PART_BEN (2, ICAT, KTH) + AMT
       WGT_PART_BEN (2, ICAT, KTH) = WGT_PART_BEN (2, ICAT, KTH) + WGT * AMT
    END IF

!--- 3 Units with adults
    IF (NUM_ADULTS > 0) THEN
       PART_BEN (3, ICAT, KTH) = PART_BEN (3, ICAT, KTH) + AMT
       WGT_PART_BEN (3, ICAT, KTH) = WGT_PART_BEN (3, ICAT, KTH) + WGT * AMT
    END IF

!-- 4 Units with elderly
    IF (FSNELDER > 0) THEN
       PART_BEN (4, ICAT, KTH) = PART_BEN (4, ICAT, KTH) + AMT
       WGT_PART_BEN (4, ICAT, KTH) = WGT_PART_BEN (4, ICAT, KTH) + WGT * AMT
    END IF


!!   5-10: RACE OF HEAD
    IF (USE_HEAD_RACE) THEN
       RACE_IDX = fshrace + 4
       PART_BEN (RACE_IDX, ICAT, KTH) = PART_BEN (RACE_IDX, ICAT, KTH) + AMT
       WGT_PART_BEN (RACE_IDX, ICAT, KTH) = WGT_PART_BEN (RACE_IDX, ICAT, KTH) + WGT * AMT
    END IF

!!  11-12: ETHNICITY OF HEAD

    IF (FSHETHNIC == 1) THEN
       PART_BEN (11, ICAT, KTH) = PART_BEN (11, ICAT, KTH) + AMT
       WGT_PART_BEN (11, ICAT, KTH) = WGT_PART_BEN (11, ICAT, KTH) + WGT * AMT
    ELSE
       PART_BEN (12, ICAT, KTH) = PART_BEN (12, ICAT, KTH) + AMT
       WGT_PART_BEN (12, ICAT, KTH) = WGT_PART_BEN (12, ICAT, KTH) + WGT * AMT
    END IF

!!  13-14: UNITS WITH DISABLED

    IF (FSNDIS > 0) THEN
       PART_BEN (13, ICAT, KTH) = PART_BEN (13, ICAT, KTH) + AMT
       WGT_PART_BEN (13, ICAT, KTH) = WGT_PART_BEN (13, ICAT, KTH) + WGT * AMT
    ELSE
       PART_BEN (14, ICAT, KTH) = PART_BEN (14, ICAT, KTH) + AMT
       WGT_PART_BEN (14, ICAT, KTH) = WGT_PART_BEN (14, ICAT, KTH) + WGT * AMT
    END IF

!!  15-22: AREA OF ORIGIN OF HEAD
    IF (AREA_OF_ORIGIN) THEN
       ORIGIN_IDX = fshORIGIN + 14
       PART_BEN (ORIGIN_IDX, ICAT, KTH) = PART_BEN (ORIGIN_IDX, ICAT, KTH) + AMT
       WGT_PART_BEN (ORIGIN_IDX, ICAT, KTH) = WGT_PART_BEN (ORIGIN_IDX, ICAT, KTH) + WGT * AMT
    END IF

    !!  PERSON-LEVEL SECTION

    PER_CAP_BEN = REAL(AMT) / FSUSIZE

!--- 23 Total persons
    PART_BEN (23, ICAT, KTH) = PART_BEN (23, ICAT, KTH) + AMT
    WGT_PART_BEN (23, ICAT, KTH) = WGT_PART_BEN (23, ICAT, KTH) + WGT * AMT

!--- 24 Kids
    PART_BEN (24, ICAT, KTH) = PART_BEN (24, ICAT, KTH)         +  PER_CAP_BEN * FSNKID
    WGT_PART_BEN (24, ICAT, KTH) = WGT_PART_BEN (24, ICAT, KTH) +  PER_CAP_BEN * FSNKID * WGT

!--- 25 Adults
    PART_BEN (25, ICAT, KTH) = PART_BEN (25, ICAT, KTH)         + PER_CAP_BEN * NUM_ADULTS
    WGT_PART_BEN (25, ICAT, KTH) = WGT_PART_BEN (25, ICAT, KTH) + PER_CAP_BEN * NUM_ADULTS * WGT

!-- 26 Elderly
    PART_BEN (26, ICAT, KTH) = PART_BEN (26, ICAT, KTH)         + PER_CAP_BEN * FSNELDER
    WGT_PART_BEN (26, ICAT, KTH) = WGT_PART_BEN (26, ICAT, KTH) + PER_CAP_BEN * FSNELDER * WGT

!-- 27 Males
    PART_BEN (27, ICAT, KTH) = PART_BEN (27, ICAT, KTH)         + PER_CAP_BEN * FSNMALE
    WGT_PART_BEN (27, ICAT, KTH) = WGT_PART_BEN (27, ICAT, KTH) + PER_CAP_BEN * FSNMALE * WGT

!-- 28 Females
    PART_BEN (28, ICAT, KTH) = PART_BEN (28, ICAT, KTH)         + PER_CAP_BEN * FSNFEMALE
    WGT_PART_BEN (28, ICAT, KTH) = WGT_PART_BEN (28, ICAT, KTH) + PER_CAP_BEN * FSNFEMALE * WGT


    IF (PERSON_LEVEL_DISAB) THEN

!-- 29 Disabled
       PART_BEN (29, ICAT, KTH) = PART_BEN (29, ICAT, KTH)         + PER_CAP_BEN * FSNDIS
       WGT_PART_BEN (29, ICAT, KTH) = WGT_PART_BEN (29, ICAT, KTH) + PER_CAP_BEN * FSNDIS * WGT

!-- 30 Not Disabled
       num_not_disab = fsusize - fsndis
       PART_BEN (30, ICAT, KTH) = PART_BEN (30, ICAT, KTH)         + PER_CAP_BEN * NUM_NOT_DISAB
       WGT_PART_BEN (30, ICAT, KTH) = WGT_PART_BEN (30, ICAT, KTH) + PER_CAP_BEN * NUM_NOT_DISAB * WGT

    END IF


101  CONTINUE

    !! 1st call
    CALL FS_STATS_GL(       &
         BASE_FSBEN   &  !
        ,FSBEN        &  !
        ,FSUSIZE      &  !
        ,fsndis       &  !
        ,fsnelder     &  !
        ,NUM_ADULTS   &  !
        ,fsnkid       &  !
        ,fsnmale      &  !
        ,fsnfemale    &  !
        ,fshrace      &  !
        ,fshethnic    &  !
        ,fshorigin    &  !
        ,KTH          &  ! = 1 for baselaw, > 1 for reforms
        ,BASE_PARTIC  &  !
        ,PARTIC       &  !
        ,WGT          &  !
        ,icat         &  !
        ,jkist        &
        ,junit        &
    )


    icat2 = 0
    select case (icat)
        case (1:3)
           icat2 = 9
        case (5:7)
           icat2 = 8
    end select

    !!  2nd call for total gain/loss
    if (kth >  1 .and. icat2 > 0) then
      CALL FS_STATS_GL(   &
         BASE_FSBEN   &  !
        ,FSBEN        &  !
        ,FSUSIZE      &  !
        ,fsndis       &  !
        ,fsnelder     &  !
        ,NUM_ADULTS   &  !
        ,fsnkid       &  !
        ,fsnmale      &  !
        ,fsnfemale    &  !
        ,fshrace      &  !
        ,fshethnic    &  !
        ,fshorigin    &  !
        ,KTH          &  ! = 1 for baselaw, > 1 for reforms
        ,BASE_PARTIC  &  !
        ,PARTIC       &  !
        ,WGT          &  !
        ,icat2        &  !
        ,jkist        &
        ,junit        &
        )

    end if

    !!  3rd call for total chg
    if (kth >  1 .and. icat > 0 .and. icat /= 4) then
      icat2 = 10
      CALL FS_STATS_GL(   &
         BASE_FSBEN   &  !
        ,FSBEN        &  !
        ,FSUSIZE      &  !
        ,fsndis       &  !
        ,fsnelder     &  !
        ,NUM_ADULTS   &  !
        ,fsnkid       &  !
        ,fsnmale      &  !
        ,fsnfemale    &  !
        ,fshrace      &  !
        ,fshethnic    &  !
        ,fshorigin    &  !
        ,KTH          &  ! = 1 for baselaw, > 1 for reforms
        ,BASE_PARTIC  &  !
        ,PARTIC       &  !
        ,WGT          &  !
        ,icat2        &
        ,jkist        &
        ,junit        &
         )
    end if


    RETURN


 900 CONTINUE
!******************************************************************
!     Print the table
!******************************************************************
    if (KTH == 0) then
       GOTO  999
    end if


    !! CALL STATS ROUTINE:
    CALL FS_STATS_GL(   &
                0            &  !
               ,0            &  !
               ,0            &  !
               ,0            &  !
               ,0            &  !
               ,0            &  !
               ,0            &  !
               ,0            &  !
               ,0            &  !
               ,0            &  !
               ,0            &  !
               ,0            &  !
               ,0            &  !
               ,.FALSE.      &  !
               ,.FALSE.      &  !
               ,0.0          &  !
               ,0            &  !
               ,0            &  !
               ,0            &  !
               )


    tab_protect_gl_stats = var_protect_gl_stats

    !! PUT ANY DATABASE-SPECIFIC LABELS HERE:
    ROWLAB(12) = ' Hispanic         '
    IF (MODEL_LABEL == "   QC MINI" ) THEN
       ROWLAB( 9) = ' White, not Hisp. '
       ROWLAB(10) = ' Black, not Hisp. '
       ROWLAB(11) = '                  '
       ROWLAB(13) = ' Asian/Pacific Is.'
       ROWLAB(14) = ' American Indian/ '
       ROWLAB(15) = ' Alaskan Native   '
       ROWLAB(16) = ' Unknown          '

    elseIF (MODEL_LABEL == " MATH SIPP") THEN
       ROWLAB( 9) = ' White only,nonHis'
       ROWLAB(10) = ' Black only,nonHis'
       ROWLAB(11) = '                  '
       ROWLAB(13) = ' Asian only       '
       ROWLAB(14) = '                  '
       ROWLAB(15) = ' Other            '
       ROWLAB(16) = ' Unknown          '

    END IF




    !! MOVE BASELAW TOTS TO CHAR FIELDS
    DO I = 1, NROWS
       comma_field(i,1,1)  = COMMA8(WGT_PART_VALUE(I,1,1), FIELD_LEN)
       comma_field2(i,1,1) = COMMA8(PART_VALUE(I,1,1), FIELD_LEN2)

       XL_FIELD  (I,1,1) = WGT_PART_VALUE(I,1,1)
       XL_FIELD2 (I,1,1) =     PART_VALUE(I,1,1)

    END DO


    !! REFORM PCT OF TOTAL - UNITS
    !! SUM UP GAINERS & LOSERS

    DO J = 2, NBR_OF_KTHS
       DO I = 1, NROWS
          !! SUM UP GAINERS/LOSERS
          !! TOT GAIN CAT 8 = CAT 5+6+7
          PART_VALUE(I,8,J) = PART_VALUE(I,5,J) + PART_VALUE(I,6,J) + PART_VALUE(I,7,J)
          WGT_PART_VALUE(I,8,J) = WGT_PART_VALUE(I,5,J) + WGT_PART_VALUE(I,6,J) + WGT_PART_VALUE(I,7,J)

          PART_BEN  (I,8,J) = PART_BEN  (I,5,J) + PART_BEN(I,6,J) + PART_BEN(I,7,J)
          WGT_PART_BEN  (I,8,J) = WGT_PART_BEN  (I,5,J) + WGT_PART_BEN(I,6,J) + WGT_PART_BEN(I,7,J)

          !! TOT LOSS CAT 9 = CAT 1+2+3
          PART_VALUE(I,9,J) = PART_VALUE(I,1,J) + PART_VALUE(I,2,J) + PART_VALUE(I,3,J)
          WGT_PART_VALUE(I,9,J) = WGT_PART_VALUE(I,1,J) + WGT_PART_VALUE(I,2,J) + WGT_PART_VALUE(I,3,J)

          PART_BEN  (I,9,J) = PART_BEN  (I,1,J) + PART_BEN(I,2,J) + PART_BEN(I,3,J)
          WGT_PART_BEN  (I,9,J) = WGT_PART_BEN  (I,1,J) + WGT_PART_BEN(I,2,J) + WGT_PART_BEN(I,3,J)


       END DO


       !!  COMPUTE PCTS & AVGS:
       DO I = 1, NROWS

          IF (abs(PART_VALUE(I,8,J)) > 0) THEN
             AVG_BEN_GAIN (I,J) = PART_BEN  (I,8,J) / PART_VALUE(I,8,J)
             WGT_AVG_BEN_GAIN (I,J) = WGT_PART_BEN (I,8,J) / WGT_PART_VALUE(I,8,J)
          END IF

          IF (abs(PART_VALUE(I,9,J)) > 0) THEN
             AVG_BEN_LOSS (I,J) = PART_BEN  (I,9,J) / PART_VALUE(I,9,J)
             WGT_AVG_BEN_LOSS (I,J) = WGT_PART_BEN (I,9,J) / WGT_PART_VALUE(I,9,J)
          END IF

          DO K = 1, NCAT
             IF(abs(PART_VALUE(I,1,1)) > 0) THEN
                PART_PCT     (I,K,J) =     PART_VALUE(I,K,J) /     PART_VALUE(I,1,1) * 100.0
                WGT_PART_PCT (I,K,J) = WGT_PART_VALUE(I,K,J) / WGT_PART_VALUE(I,1,1) * 100.0
             END IF
          END DO

       END DO


      DO I = 1, NROWS
         AVG_GAIN (I,J,1) = WGT_AVG_BEN_GAIN (I, J)
         AVG_GAIN (I,J,2) =     AVG_BEN_GAIN (I, J)
         AVG_LOSS (I,J,1) = WGT_AVG_BEN_LOSS (I, J)
         AVG_LOSS (I,J,2) =     AVG_BEN_LOSS (I, J)

         DO K = 1,NCAT
            comma_field(i,k,j)  = COMMA8(WGT_PART_VALUE(I,K,J), FIELD_LEN)  !! REF
            comma_field2(i,k,j) = COMMA8(PART_VALUE(I,K,J), FIELD_LEN2)
            PCT_FIELD  (I,K,J,1) = WGT_PART_PCT (I,K,J)
            PCT_FIELD  (I,K,J,2) = PART_PCT     (I,K,J)

            XL_FIELD  (I,K,J) = WGT_PART_VALUE(I,K,J)  !! REF
            XL_FIELD2 (I,K,J) =     PART_VALUE(I,K,J)

         END DO
      END DO

   END DO


    return       !! end of keof=3, pass 1

 999  continue   !! keof=3, pass 2

!-------------------------------------------------------------------
!   PRINT 2 PAGES PER PLAN:
!   BY WEIGHTED & UNWEIGHTED
!-------------------------------------------------------------------

  ! Print JSON tables
  WRITE (JSON_FILE, *) '"Table 7": [' ! open table (weighted gainers)

  DO i=1,NROWS
    IF (dostats(nth)) THEN
      WRITE (JSON_FILE, 1121, ADVANCE='no') TRIM(json_rowlab1(i)), TRIM(json_rowlab2(i)), &
        NINT(WGT_PART_VALUE(i,1,1), SELECTED_INT_KIND(12)), &
        (WGT_PART_PCT(i,k,2), TRIM(tab_protect_gl_stats(i,2,1,1,k)), k=5,8) ! stats indexed by characteristic (i), participation (1=eligible, 2=participant), unknown (?), reform (1=first reform), g/l category (5-8 gainers, 1-3 & 9 losers)
    ELSE
      WRITE (JSON_FILE, 1111, ADVANCE='no') TRIM(json_rowlab1(i)), TRIM(json_rowlab2(i)), &
        NINT(WGT_PART_VALUE(i,1,1), SELECTED_INT_KIND(12)), &
        (WGT_PART_PCT(i,k,2), k=5,8)
    END IF

    ! Fill in the benefits per unit, if possible
    IF (abs(WGT_PART_VALUE(i,1,1)) > 0) THEN
      WRITE (JSON_FILE, '(F7.2,"]")', ADVANCE='no') WGT_AVG_BEN_GAIN(i,2)
    ELSE
      WRITE (JSON_FILE, '(A,"]")', ADVANCE='no') '"n.a."'
    END IF

    if (i /= NROWS) WRITE (JSON_FILE, *) ',' ! not the last...
  end do

  WRITE (JSON_FILE, *) ! newline
  WRITE (JSON_FILE, *) '],' ! close table (array of arrays, not the last one)

  WRITE (JSON_FILE, *) '"Table 7A": [' ! open table (weighted losers)

  DO i=1,NROWS
    IF (dostats(nth)) THEN
      WRITE (JSON_FILE, 1121, ADVANCE='no') TRIM(json_rowlab1(i)), TRIM(json_rowlab2(i)), &
        NINT(WGT_PART_VALUE(i,1,1), SELECTED_INT_KIND(12)), &
        (WGT_PART_PCT(i,k,2), TRIM(tab_protect_gl_stats(i,2,1,1,k)), k=1,3), &
        WGT_PART_PCT(i,9,2), TRIM(tab_protect_gl_stats(i,2,1,1,9)) 
    ELSE
      WRITE (JSON_FILE, 1111, ADVANCE='no') TRIM(json_rowlab1(i)), TRIM(json_rowlab2(i)), &
        NINT(WGT_PART_VALUE(i,1,1), SELECTED_INT_KIND(12)), &
        (WGT_PART_PCT(i,k,2), k=1,3), &
        WGT_PART_PCT(i,9,2) 
    END IF

    ! Fill in the benefits per unit, if possible
    IF (abs(WGT_PART_VALUE(i,1,1)) > 0) THEN
      WRITE (JSON_FILE, '(F7.2,"]")', ADVANCE='no') WGT_AVG_BEN_LOSS(i,2)
    ELSE
      WRITE (JSON_FILE, '(A,"]")', ADVANCE='no') '"n.a."'
    END IF

    if (i /= NROWS) WRITE (JSON_FILE, *) ',' ! not the last...
  end do

  WRITE (JSON_FILE, *) ! newline
  WRITE (JSON_FILE, *) '],' ! close table (array of arrays, not the last one)

  WRITE (JSON_FILE, *) '"Table 7B": [' ! open table (unweighted gainers)

  DO i=1,NROWS
    WRITE (JSON_FILE, 1112, ADVANCE='no') TRIM(json_rowlab1(i)), TRIM(json_rowlab2(i)), &
      NINT(PART_VALUE(i,1,1), SELECTED_INT_KIND(12)), &
      (NINT(PART_VALUE(i,k,2), SELECTED_INT_KIND(12)), k=5,8)

    ! Fill in the benefits per unit, if possible
    IF (abs(PART_VALUE(i,1,1)) > 0) THEN
      WRITE (JSON_FILE, '(F7.2,"]")', ADVANCE='no') AVG_BEN_GAIN(i,2)
    ELSE
      WRITE (JSON_FILE, '(A,"]")', ADVANCE='no') '"n.a."'
    END IF

    if (i /= NROWS) WRITE (JSON_FILE, *) ',' ! not the last...
  end do

  WRITE (JSON_FILE, *) ! newline
  WRITE (JSON_FILE, *) '],' ! close table (array of arrays, not the last one)

  WRITE (JSON_FILE, *) '"Table 7C": [' ! open table (unweighted losers)

  DO i=1,NROWS
    WRITE (JSON_FILE, 1112, ADVANCE='no') TRIM(json_rowlab1(i)), TRIM(json_rowlab2(i)), &
      NINT(PART_VALUE(i,1,1), SELECTED_INT_KIND(12)), &
      (NINT(PART_VALUE(i,k,2), SELECTED_INT_KIND(12)), k=1,3), &
      NINT(PART_VALUE(i,9,2), SELECTED_INT_KIND(12)) 

    ! Fill in the benefits per unit, if possible
    IF (abs(PART_VALUE(i,1,1)) > 0) THEN
      WRITE (JSON_FILE, '(F7.2,"]")', ADVANCE='no') AVG_BEN_LOSS(i,2)
    ELSE
      WRITE (JSON_FILE, '(A,"]")', ADVANCE='no') '"n.a."'
    END IF

    if (i /= NROWS) WRITE (JSON_FILE, *) ',' ! not the last...
  end do

  WRITE (JSON_FILE, *) ! newline
  WRITE (JSON_FILE, *) '],' ! close table (array of arrays, not the last one)

    DO J = 2, NBR_OF_KTHS    !! Loop over all plans
       IF (.NOT. GLTABS(J-1) ) CYCLE

       PLANTEMP = 'PLAN ' //PLANNBR_TABLE(J-1) // ': '//PLANNAME_TABLE(J-1)
       CALL CENTER_TEXT(PLANTEMP,80)
       PLANTEMP2 = PLANNAME_TABLE(J-1)

       DO L = 1, 2             !! WEIGHTED/UNWEIGHTED

          !----  Print table
          CALL ISNEWPG(TABFILE, PAGE_BREAK_NUMLINES + 1)  ! +1 lets ISNEWPG know
                                                         ! lines are coming - but
                                                          ! not exactly how many.
          WRITE (TABFILE,1100)  PLANTEMP


          IF (L == 1) THEN
             WRITE (TABFILE,1150)  ! WGT title
             WRITE (TABFILE,1201)
          ELSE
             WRITE (TABFILE,1151)  ! UNWGT title
             WRITE (TABFILE,1202)
          ENDIF

          WRITE (TABFILE,1210)  ! underline

          I = 0
          DO II = 1, NROWS+EMPTY_ROWS
             DO M = 1, EMPTY_ROWS
                IF (II == SKIP_ROW(M)) THEN
                   WRITE(TABFILE, 1214)ROWLAB(II)
                   GOTO 201
                END IF
             END DO

             I = I + 1
             !!  BASELAW PLUS KTH REFORM
             IF (L == 1) THEN   !! WEIGHTED  (PCTS)

                WRITE(TABFILE, 1216)                 &
                   ROWLAB(II)                        &
                 , COMMA_FIELD (I,1,1)               &
                 ,(PCT_FIELD   (I,K,J,L), tab_protect_gl_STATs(I,2,1,J-1,k), K = 5, 8)  &
                 , AVG_GAIN    (I,  J,L)                                       &
                 ,(PCT_FIELD   (I,K,J,L), tab_protect_gl_STATs(I,2,1,J-1,k), K = 1, 3)  &
                 , PCT_FIELD   (i,9,J,L), tab_protect_gl_STATs(I,2,1,J-1,9)             &
                 , AVG_LOSS    (I,  J,L)

 1216 format (T2, A, T22, A, T33, 4(2X,F7.2,A),   2X,F7.2 ,  4(2X,F7.2,A),   2X,F7.2  )

             ELSE               !! UNWEIGHTED (COUNTS)
                WRITE(TABFILE, 1217)                 &
                  ROWLAB(II)                         &
                 ,COMMA_FIELD2  (I,1,1)              &
                 ,(COMMA_FIELD2 (I,K,J), K = 5, 8)   &
                 ,AVG_GAIN     (I,J,L)               &
                 ,(COMMA_FIELD2 (I,K,J), K = 1, 3)   &
                 , COMMA_FIELD2 (I,9,J)              &
                 ,AVG_LOSS     (I,J,L)

             END IF

             if (create_table_extracts) then

              IF (L == 1) THEN   !!   weighted
                WRITE (40, 4001)                &
                   "t_prot_gl   "               &
                  ,j                            &
                  ,adjustl(plantemp2)           &
                  ,"W"                          &
                  ,rowlab(ii)                   &
                  , XL_FIELD (I,1,1)            &
                  ,(PCT_FIELD   (I,K,J,L), tab_protect_gl_STATs(I,2,1,J-1,k), K = 5, 8)  &
                  , AVG_GAIN    (I,  J,L)                                       &
                  ,(PCT_FIELD   (I,K,J,L), tab_protect_gl_STATs(I,2,1,J-1,k), K = 1, 3)  &
                  , PCT_FIELD   (i,9,J,L), tab_protect_gl_STATs(I,2,1,J-1,9)             &
                  , AVG_LOSS    (I,  J,L)

              else
                WRITE (40, 4002)                &
                   "t_prot_gl   "               &
                  ,j                            &
                  ,adjustl(plantemp2)           &
                  ,"U"                          &
                  ,rowlab(ii)                   &
                  ,XL_FIELD2  (I,1,1)              &
                  ,(XL_FIELD2 (I,K,J), K = 5, 8)   &
                  ,AVG_GAIN     (I,J,L)            &
                  ,(XL_FIELD2 (I,K,J), K = 1, 3)   &
                  , XL_FIELD2 (I,9,J)              &
                  ,AVG_LOSS     (I,J,L)

               end if

              end if

 4001         format(1x,a12, 2x,i2, 2x,a40, 2x,a1, 2x,a42, 2x,F10.0, 4(2X,F8.2, 2x,A), 2X,F7.2, 4(2X,F8.2, 2x,A), 2X,F7.2  )
 4002         format(1x,a12, 2x,i2, 2x,a40, 2x,a1, 2x,a42, 4x,F8.0,  4(2X,F8.0, 3x  ), 2X,F7.2, 4(2X,F8.0, 3x  ), 2X,F7.2  )

 201         CONTINUE
          END DO

          WRITE (TABFILE,1210)


   !----  Print plan descriptions below tables
   !-----    Write footer
          SELECT CASE(GLUNIT)
            CASE(1)
              WRITE (TABFILE,7010)
            CASE(2)
              WRITE (TABFILE,7020)
          END SELECT
          WRITE (TABFILE,7000)

          IF (L == 1) WRITE (TABFILE,1290)


     END DO ! end of WEIGHTED/UNWEIGHTED
    END DO ! end of plan




    RETURN
!----------------------------------------------------------------------
! FORMAT STATEMENTS
!----------------------------------------------------------------------
1100 format( T37,'                                  ' //                         &
             T37,'       PROTECTED CLASS IMPACTS ON GAINER/LOSER SNAP UNITS' /   &
             T37,'    UNIVERSE: PARTICIPATING SNAP UNITS IN BASELAW OR REFORM' / &
             T25,  A80                                                       )


1150 FORMAT(T5, "WEIGHTED")
1151 FORMAT(T5, "UNWEIGHTED")



1201 format( 1X, 131('-')                                 &
 ,/, t43, 'Units/Persons Gaining ', t94, 'Units/Persons Losing'           &
 ,/, t35, 37('-'), t84, 37('-')                           &
 ,/, t41, 'Percent of Baselaw Totals', t90, 'Percent of Baselaw Totals'       &
 ,/, t35, 37('-'), t84, 37('-')                           &
 ,/, t37, 'Newly', t47,  'Still',   t57, 'Still' ,  t96 , 'Still',     t106, 'Still'                 &
 ,/, t35, 'Eligible', t45, 'Eligible', t56, 'Partic',  T87, 'No', t094, 'Eligible',  t105, 'Partic'  &
 ,/, t25, 'Baselaw     and       Newly   w/Higher   Total      Avg $   Longer      Not     w/Lower    Total     Avg $'&
 ,/, t2,  'Characteristic'                                                                                            &
   ,t26,  'Units     Partic    Partic    Benefit  Gainers     Gain   Eligible   Partic    Benefit   Losers     Loss'  &
   )

1202 format( 1X, 131('-')                                 &
 ,/, t49, 'Gaining ', t99, 'Losing'                       &
 ,/, t35, 37('-'), t84, 37('-')                           &
 ,/, t41, ' Number of Units/Persons ', t90, ' Number of Units/Persons '       &
 ,/, t35, 37('-'), t84, 37('-')                           &
 ,/, t47,  'Still',   t57, 'Still' ,  t96 , 'Still',     t106, 'Still'               &
 ,/, t45, 'Eligible', t56, 'Partic',  T87, 'No', t094, 'Eligible',  t105, 'Partic'              &
 ,/, t25, 'Baselaw     Newly     Newly   w/Higher   Total      Avg $   Longer      Not     w/Lower    Total     Avg $'&
 ,/, t2,  'Characteristic'                                                                                            &
   ,t26,  'Units    Eligible   Partic    Benefit  Gainers     Gain   Eligible   Partic    Benefit   Losers     Loss'  &
   )

1210 format(1X,131('-'))

1214 format (T2,A)


1217 format (T2, A, T22, A, T33, 4(2X,A8    ), 2X,F7.2,  4(2X,A8    ), 2X,F7.2 )

! JSON format of row, including 2 label lines
1111 format('["', A, '", "', A, '",', I12, ',', 4(F7.2, ',')) ! weighted
1121 format('["', A, '", "', A, '",', I12, ',', 4(F7.2, ',"', A, '",')) ! weighted with stats
1112 format('["', A, '", "', A, '",', I12, ',', 4(I12, ',')) ! unweighted


1290 format (&
  /,1x,'Statistics key:' &
 ,/,1x,'* Change is statistically different from zero at a 90% level of significance' &
 ,/,1x,'+ Change for subgroup is statistically different from the overall change at a 90% level of significance' &
 ,/,1x,'# Both conditions are met' &

)

7000 FORMAT(                                                           &
         /  1X, 'Total gainers are equal to the sum of (1) baselaw '   &
              , 'participants who gain benefits under reform, and '    &
              , '(2) baselaw nonparticipants'                          &
         /  1X, '(elig. or inelig.) who participate under reform. '    &
              , 'Since it is possible that the sum of total gainers '  &
              , 'can exceed the total number'                          &
         /  1X, 'of baselaw participants, the number of gainers '      &
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

      END
