!**************************************************************************************************
! Source File:  FStab_protect_ben.F90                 
! Called By:    FS_TABLES                   
!
! TABLE 8 - Protected Class Impacts in Eligible and Participating Food Stamp Units
!
!**************************************************************************************************
      SUBROUTINE FS_Tab_protected_benefits( &
                KTH          &  ! = 1 for baselaw, > 1 for reforms
               ,FSBEN        &  !
               ,FSUSIZE      &  !
               ,fsndis       &  !
               ,fsnelder     &  !
               ,fsnadult     &  !
               ,fsnkid       &  !
               ,fsnmale      &  !
               ,fsnfemale    &  !
               ,fshrace      &  !
               ,fshethnic    &  !
               ,fshorigin    &  !
               ,PARTIC       &  !
               ,WGT          )  !

      USE GLOBAL
      USE GLOBPARM, ONLY: MODEL_LABEL


      USE FSWORK, ONLY:     &
       SHOW_ELIG            &
      ,PLANNAME_TABLE       &
      ,PLANNBR_TABLE        &
      ,USE_HEAD_RACE        &
      ,AREA_OF_ORIGIN       &
      ,PERSON_LEVEL_DISAB   &
      ,tab_protect_ben_stats           &
      ,var_protect_ben_stats           &
      ,create_table_extracts

      USE FSPARM, ONLY: JSON_FILE, dostats
      use utils


      IMPLICIT NONE


      INTEGER, parameter ::  MAX_KTH = 10

!---- Declare parameters from calling program
      INTEGER, intent(in)   ::  KTH
      INTEGER, intent(in)   ::  FSBEN
      INTEGER, intent(in)   ::  FSUSIZE
      INTEGER, intent(in)   ::  fsndis
      INTEGER, intent(in)   ::  fsnelder
      INTEGER, intent(in)   ::  fsnadult
      INTEGER, intent(in)   ::  fsnkid
      INTEGER, intent(in)   ::  fsnmale
      INTEGER, intent(in)   ::  fsnfemale
      INTEGER, intent(in)   ::  fshrace
      INTEGER, intent(in)   ::  fshethnic
      INTEGER, intent(in)   ::  fshorigin
      LOGICAL, intent(in)   ::  PARTIC
      REAL   , intent(in)   ::  WGT


!---- Variables for tables
      REAL (8)          ::  WGT_FSBEN
      CHARACTER(len=80) ::  PLANTEMP
      CHARACTER(len=80) ::  PLANTEMP2

      INTEGER :: I, J, K, L, PASS, II, istart
      INTEGER :: NBR_OF_KTHS = 0

      INTEGER :: NUM_ADULTS, NUM_NOT_DISAB, ORIGIN_IDX, RACE_IDX

      integer, PARAMETER  :: NROWS = 30

!---- Number of units by characteristic and KTH (cells)
      REAL (8) :: ELIG_VALUE(NROWS,MAX_KTH + 1) = 0
      REAL (8) :: PART_VALUE(NROWS,MAX_KTH + 1) = 0
      REAL (8) :: WGT_ELIG_VALUE(NROWS,MAX_KTH + 1) = 0.0
      REAL (8) :: WGT_PART_VALUE(NROWS,MAX_KTH + 1) = 0.0

!---- BENEFITS
      REAL :: per_cap_ben

      REAL (8) :: ELIG_BEN(NROWS,MAX_KTH + 1) = 0
      REAL (8) :: PART_BEN(NROWS,MAX_KTH + 1) = 0
      REAL (8) :: WGT_ELIG_BEN(NROWS,MAX_KTH + 1) = 0.0
      REAL (8) :: WGT_PART_BEN(NROWS,MAX_KTH + 1) = 0.0

      REAL (8) :: AVG_ELIG_BEN     (NROWS,MAX_KTH + 1) = 0.0
      REAL (8) :: WGT_AVG_ELIG_BEN (NROWS,MAX_KTH + 1) = 0.0
      REAL (8) :: AVG_PART_BEN     (NROWS,MAX_KTH + 1) = 0.0
      REAL (8) :: WGT_AVG_PART_BEN (NROWS,MAX_KTH + 1) = 0.0

      REAL (8) :: ELIG_BEN_DIFF    (NROWS,MAX_KTH + 1) = 0.0
      REAL (8) :: WGT_ELIG_BEN_DIFF(NROWS,MAX_KTH + 1) = 0.0
      REAL (8) :: PART_BEN_DIFF    (NROWS,MAX_KTH + 1) = 0.0
      REAL (8) :: WGT_PART_BEN_DIFF(NROWS,MAX_KTH + 1) = 0.0

      REAL (8) :: BEN_FIELD (NROWS, MAX_KTH + 1, 2, 2) = 0.0
      REAL (8) :: BEN_DIFF  (NROWS, MAX_KTH + 1, 2, 2) = 0.0


!---- Row labels for both tables
      integer, parameter :: empty_rows = 17
      integer, dimension(empty_rows) :: skip_row = (/2,3, 7,8, 15,16, 19,20,  23,24, 33, 35,36, 40,41, 44,45/)
      CHARACTER(len=42) :: ROWLAB(nrows+empty_rows)= (/  &
      'Total Units                               '  &  !  1
     ,'                                          '  &  !  2
     ,'Units with:                               '  &  !  3
     ,'  Children (<18)                          '  &  !  4
     ,'  Adults   (18-59)                        '  &  !  5
     ,'  Elderly  (60+)                          '  &  !  6
     ,'                                          '  &  !  7
     ,'Race of head:                             '  &  !  8
     ,'  White only                              '  &  !  9
     ,'  Black only                              '  &  ! 10
     ,' (Hispanic: see below)                    '  &  ! 13
     ,'  Asian Only                              '  &  ! 11
     ,'  All Others                              '  &  ! 12
     ,'  Unknown                                 '  &  ! 14
     ,'                                          '  &  ! 15
     ,'Ethnicity of head:                        '  &  ! 16
     ,'  Hispanic                                '  &  ! 17
     ,'  Not Hispanic                            '  &  ! 18
     ,'                                          '  &  ! 19
     ,'Units with Disabled:                      '  &  ! 20
     ,'  Yes                                     '  &  ! 21
     ,'  No                                      '  &  ! 22
     ,'                                          '  &  ! 23
     ,'Area of Origin of head:                   '  &  ! 24
     ,'  North America                           '  &  ! 25
     ,'  Europe                                  '  &  ! 26
     ,'  Asia/Pacific Islands                    '  &  ! 27
     ,'  Middle East                             '  &  ! 28
     ,'  Central/South America                   '  &  ! 29
     ,'  Africa                                  '  &  ! 30
     ,'  Elsewhere                               '  &  ! 31
     ,'  Unknown                                 '  &  ! 32
     ,'                                          '  &  ! 33
     ,'Total Persons                             '  &  ! 34
     ,'                                          '  &  ! 35
     ,'Age:                                      '  &  ! 36
     ,'  Children (<18)                          '  &  ! 37
     ,'  Adults   (18-59)                        '  &  ! 38
     ,'  Elderly  (60+)                          '  &  ! 39
     ,'                                          '  &  ! 40
     ,'Sex:                                      '  &  ! 41
     ,'  Male                                    '  &  ! 42
     ,'  Female                                  '  &  ! 43
     ,'                                          '  &  ! 44
     ,'Disability:                               '  &  ! 45
     ,'  Yes                                     '  &  ! 46
     ,'  No                                      '  &  ! 47
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

     CHARACTER(LEN=1) :: temp_stat

    character(len=4) :: displ_pass
    character(len=1) :: displ_wgt



!---- By-pass calculations if print requested

    IF (KEOF == 3) GOTO 900
!****************************************************************
!     Perform table calculations
!****************************************************************

    WGT_FSBEN = FSBEN * WGT

!-- Keep track of highest KTH computed
    IF (KTH > MAX_KTH + 1) RETURN 
    IF (KTH > NBR_OF_KTHS) NBR_OF_KTHS = KTH

!------------------------------------------------------------------------------

!--- 1 Total units
    IF (FSBEN > 0) THEN
       ELIG_VALUE (1,KTH) = ELIG_VALUE (1,KTH) + 1
       WGT_ELIG_VALUE (1,KTH) = WGT_ELIG_VALUE (1,KTH) + WGT
       IF (PARTIC) THEN
          PART_VALUE (1,KTH) = PART_VALUE (1,KTH) + 1
          WGT_PART_VALUE (1,KTH) = WGT_PART_VALUE (1,KTH) + WGT
       END IF
!   END IF

!--- 2 Units with kids
    IF (FSNKID > 0) THEN
       ELIG_VALUE (2,KTH) = ELIG_VALUE (2,KTH) + 1
       WGT_ELIG_VALUE (2,KTH) = WGT_ELIG_VALUE (2,KTH) + WGT
       IF (PARTIC) THEN
          PART_VALUE (2,KTH) = PART_VALUE (2,KTH) + 1
          WGT_PART_VALUE (2,KTH) = WGT_PART_VALUE (2,KTH) + WGT
       END IF
    END IF

!--- 3 Units with adults
    NUM_ADULTS = FSNADULT - FSNELDER
    IF (NUM_ADULTS > 0) THEN
       ELIG_VALUE (3,KTH) = ELIG_VALUE (3,KTH) + 1
       WGT_ELIG_VALUE (3,KTH) = WGT_ELIG_VALUE (3,KTH) + WGT
       IF (PARTIC) THEN
          PART_VALUE (3,KTH) = PART_VALUE (3,KTH) + 1
          WGT_PART_VALUE (3,KTH) = WGT_PART_VALUE (3,KTH) + WGT
       END IF
    END IF

!-- 4 Units with elderly
    IF (FSNELDER > 0) THEN
       ELIG_VALUE (4,KTH) = ELIG_VALUE (4,KTH) + 1
       WGT_ELIG_VALUE (4,KTH) = WGT_ELIG_VALUE (4,KTH) + WGT
       IF (PARTIC) THEN
          PART_VALUE (4,KTH) = PART_VALUE (4,KTH) + 1
          WGT_PART_VALUE (4,KTH) = WGT_PART_VALUE (4,KTH) + WGT
       END IF
    END IF


!!   5-10: RACE OF HEAD

    IF (USE_HEAD_RACE) THEN
       RACE_IDX = fshrace + 4

       ELIG_VALUE (RACE_IDX,KTH) = ELIG_VALUE (RACE_IDX,KTH) + 1
       WGT_ELIG_VALUE (RACE_IDX,KTH) = WGT_ELIG_VALUE (RACE_IDX,KTH) + WGT
       IF (PARTIC) THEN
          PART_VALUE (RACE_IDX,KTH) = PART_VALUE (RACE_IDX,KTH) + 1
          WGT_PART_VALUE (RACE_IDX,KTH) = WGT_PART_VALUE (RACE_IDX,KTH) + WGT
       END IF
    END IF

!!  11-12: ETHNICITY OF HEAD

    IF (FSHETHNIC == 1) THEN
       ELIG_VALUE (11,KTH) = ELIG_VALUE (11,KTH) + 1
       WGT_ELIG_VALUE (11,KTH) = WGT_ELIG_VALUE (11,KTH) + WGT
       IF (PARTIC) THEN
          PART_VALUE (11,KTH) = PART_VALUE (11,KTH) + 1
          WGT_PART_VALUE (11,KTH) = WGT_PART_VALUE (11,KTH) + WGT
       END IF
    ELSE
       ELIG_VALUE (12,KTH) = ELIG_VALUE (12,KTH) + 1
       WGT_ELIG_VALUE (12,KTH) = WGT_ELIG_VALUE (12,KTH) + WGT
       IF (PARTIC) THEN
          PART_VALUE (12,KTH) = PART_VALUE (12,KTH) + 1
          WGT_PART_VALUE (12,KTH) = WGT_PART_VALUE (12,KTH) + WGT
       END IF

    END IF

!!  13-14: UNITS WITH DISABLED

    IF (FSNDIS > 0) THEN
       ELIG_VALUE (13,KTH) = ELIG_VALUE (13,KTH) + 1
       WGT_ELIG_VALUE (13,KTH) = WGT_ELIG_VALUE (13,KTH) + WGT
       IF (PARTIC) THEN
          PART_VALUE (13,KTH) = PART_VALUE (13,KTH) + 1
          WGT_PART_VALUE (13,KTH) = WGT_PART_VALUE (13,KTH) + WGT
       END IF
    ELSE
       ELIG_VALUE (14,KTH) = ELIG_VALUE (14,KTH) + 1
       WGT_ELIG_VALUE (14,KTH) = WGT_ELIG_VALUE (14,KTH) + WGT
       IF (PARTIC) THEN
          PART_VALUE (14,KTH) = PART_VALUE (14,KTH) + 1
          WGT_PART_VALUE (14,KTH) = WGT_PART_VALUE (14,KTH) + WGT
       END IF

    END IF

!!  15-22: AREA OF ORIGIN OF HEAD
    IF (AREA_OF_ORIGIN) THEN
       ORIGIN_IDX = fshORIGIN + 14

       ELIG_VALUE (ORIGIN_IDX,KTH) = ELIG_VALUE (ORIGIN_IDX,KTH) + 1
       WGT_ELIG_VALUE (ORIGIN_IDX,KTH) = WGT_ELIG_VALUE (ORIGIN_IDX,KTH) + WGT
       IF (PARTIC) THEN
          PART_VALUE (ORIGIN_IDX,KTH) = PART_VALUE (ORIGIN_IDX,KTH) + 1
          WGT_PART_VALUE (ORIGIN_IDX,KTH) = WGT_PART_VALUE (ORIGIN_IDX,KTH) + WGT
       END IF
    END IF


    !!  PERSON-LEVEL SECTION

!--- 23 Total persons
    ELIG_VALUE (23,KTH) = ELIG_VALUE (23,KTH) + FSUSIZE
    WGT_ELIG_VALUE (23,KTH) = WGT_ELIG_VALUE (23,KTH) + WGT * FSUSIZE
    IF (PARTIC) THEN
       PART_VALUE (23,KTH) = PART_VALUE (23,KTH) + FSUSIZE
       WGT_PART_VALUE (23,KTH) = WGT_PART_VALUE (23,KTH) + WGT * FSUSIZE
    END IF

!--- 24 Kids
    ELIG_VALUE (24,KTH) = ELIG_VALUE (24,KTH) + FSNKID
    WGT_ELIG_VALUE (24,KTH) = WGT_ELIG_VALUE (24,KTH) + WGT * FSNKID
    IF (PARTIC) THEN
       PART_VALUE (24,KTH) = PART_VALUE (24,KTH) + FSNKID
       WGT_PART_VALUE (24,KTH) = WGT_PART_VALUE (24,KTH) + WGT * FSNKID
    END IF

!--- 25 Adults
    ELIG_VALUE (25,KTH) = ELIG_VALUE (25,KTH) + NUM_ADULTS
    WGT_ELIG_VALUE (25,KTH) = WGT_ELIG_VALUE (25,KTH) + WGT * NUM_ADULTS
    IF (PARTIC) THEN
       PART_VALUE (25,KTH) = PART_VALUE (25,KTH) + NUM_ADULTS
       WGT_PART_VALUE (25,KTH) = WGT_PART_VALUE (25,KTH) + WGT * NUM_ADULTS
    END IF

!-- 26 Elderly
    ELIG_VALUE (26,KTH) = ELIG_VALUE (26,KTH) + FSNELDER
    WGT_ELIG_VALUE (26,KTH) = WGT_ELIG_VALUE (26,KTH) + WGT * FSNELDER
    IF (PARTIC) THEN
       PART_VALUE (26,KTH) = PART_VALUE (26,KTH) + FSNELDER
       WGT_PART_VALUE (26,KTH) = WGT_PART_VALUE (26,KTH) + WGT * FSNELDER
    END IF

!-- 27 Males
    ELIG_VALUE (27,KTH) = ELIG_VALUE (27,KTH) + FSNMALE
    WGT_ELIG_VALUE (27,KTH) = WGT_ELIG_VALUE (27,KTH) + WGT * FSNMALE
    IF (PARTIC) THEN
       PART_VALUE (27,KTH) = PART_VALUE (27,KTH) + FSNMALE
       WGT_PART_VALUE (27,KTH) = WGT_PART_VALUE (27,KTH) + WGT * FSNMALE
    END IF

!-- 28 Females
    ELIG_VALUE (28,KTH) = ELIG_VALUE (28,KTH) + FSNFEMALE
    WGT_ELIG_VALUE (28,KTH) = WGT_ELIG_VALUE (28,KTH) + WGT * FSNFEMALE
    IF (PARTIC) THEN
       PART_VALUE (28,KTH) = PART_VALUE (28,KTH) + FSNFEMALE
       WGT_PART_VALUE (28,KTH) = WGT_PART_VALUE (28,KTH) + WGT * FSNFEMALE
    END IF


    IF (PERSON_LEVEL_DISAB) THEN

!-- 29 Disabled
       ELIG_VALUE (29,KTH) = ELIG_VALUE (29,KTH) + FSNDIS
       WGT_ELIG_VALUE (29,KTH) = WGT_ELIG_VALUE (29,KTH) + WGT * FSNDIS
       IF (PARTIC) THEN
          PART_VALUE (29,KTH) = PART_VALUE (29,KTH) + FSNDIS
          WGT_PART_VALUE (29,KTH) = WGT_PART_VALUE (29,KTH) + WGT * FSNDIS
       END IF

!-- 30 Not Disabled
       num_not_disab = fsusize - fsndis
       ELIG_VALUE (30,KTH) = ELIG_VALUE (30,KTH) + NUM_NOT_DISAB
       WGT_ELIG_VALUE (30,KTH) = WGT_ELIG_VALUE (30,KTH) + WGT * NUM_NOT_DISAB
       IF (PARTIC) THEN
          PART_VALUE (30,KTH) = PART_VALUE (30,KTH) + NUM_NOT_DISAB
          WGT_PART_VALUE (30,KTH) = WGT_PART_VALUE (30,KTH) + WGT * NUM_NOT_DISAB
       END IF

    END IF

!-------------------------------------------------------------------------------
!    BENEFITS
!-------------------------------------------------------------------------------

!--- 1 Total units
       ELIG_BEN (1,KTH) = ELIG_BEN (1,KTH) + FSBEN
       WGT_ELIG_BEN (1,KTH) = WGT_ELIG_BEN (1,KTH) + WGT * FSBEN
       IF (PARTIC) THEN
          PART_BEN (1,KTH) = PART_BEN (1,KTH) + FSBEN
          WGT_PART_BEN (1,KTH) = WGT_PART_BEN (1,KTH) + WGT * FSBEN
       END IF

!--- 2 Units with kids
    IF (FSNKID > 0) THEN
       ELIG_BEN (2,KTH) = ELIG_BEN (2,KTH) + FSBEN
       WGT_ELIG_BEN (2,KTH) = WGT_ELIG_BEN (2,KTH) + WGT * FSBEN
       IF (PARTIC) THEN
          PART_BEN (2,KTH) = PART_BEN (2,KTH) + FSBEN
          WGT_PART_BEN (2,KTH) = WGT_PART_BEN (2,KTH) + WGT * FSBEN
       END IF
    END IF

!--- 3 Units with adults
    NUM_ADULTS = FSNADULT - FSNELDER
    IF (NUM_ADULTS > 0) THEN
       ELIG_BEN (3,KTH) = ELIG_BEN (3,KTH) + FSBEN
       WGT_ELIG_BEN (3,KTH) = WGT_ELIG_BEN (3,KTH) + WGT * FSBEN
       IF (PARTIC) THEN
          PART_BEN (3,KTH) = PART_BEN (3,KTH) + FSBEN
          WGT_PART_BEN (3,KTH) = WGT_PART_BEN (3,KTH) + WGT * FSBEN
       END IF
    END IF

!-- 4 Units with elderly
    IF (FSNELDER > 0) THEN
       ELIG_BEN (4,KTH) = ELIG_BEN (4,KTH) + FSBEN
       WGT_ELIG_BEN (4,KTH) = WGT_ELIG_BEN (4,KTH) + WGT * FSBEN
       IF (PARTIC) THEN
          PART_BEN (4,KTH) = PART_BEN (4,KTH) + FSBEN
          WGT_PART_BEN (4,KTH) = WGT_PART_BEN (4,KTH) + WGT * FSBEN
       END IF
    END IF


!!   5-10: RACE OF HEAD
    IF (USE_HEAD_RACE) THEN

       RACE_IDX = fshrace + 4

       if (fshrace /= 3) THEN   !! skip hispanic
          ELIG_BEN (RACE_IDX,KTH) = ELIG_BEN (RACE_IDX,KTH) + FSBEN
          WGT_ELIG_BEN (RACE_IDX,KTH) = WGT_ELIG_BEN (RACE_IDX,KTH) + WGT * FSBEN
          IF (PARTIC) THEN
             PART_BEN (RACE_IDX,KTH) = PART_BEN (RACE_IDX,KTH) + FSBEN
             WGT_PART_BEN (RACE_IDX,KTH) = WGT_PART_BEN (RACE_IDX,KTH) + WGT * FSBEN
          END IF
       END IF

    END IF

!!  11-12: ETHNICITY OF HEAD

    IF (FSHETHNIC == 1) THEN
       ELIG_BEN (11,KTH) = ELIG_BEN (11,KTH) + FSBEN
       WGT_ELIG_BEN (11,KTH) = WGT_ELIG_BEN (11,KTH) + WGT * FSBEN
       IF (PARTIC) THEN
          PART_BEN (11,KTH) = PART_BEN (11,KTH) + FSBEN
          WGT_PART_BEN (11,KTH) = WGT_PART_BEN (11,KTH) + WGT * FSBEN
       END IF
    ELSE
       ELIG_BEN (12,KTH) = ELIG_BEN (12,KTH) + FSBEN
       WGT_ELIG_BEN (12,KTH) = WGT_ELIG_BEN (12,KTH) + WGT * FSBEN
       IF (PARTIC) THEN
          PART_BEN (12,KTH) = PART_BEN (12,KTH) + FSBEN
          WGT_PART_BEN (12,KTH) = WGT_PART_BEN (12,KTH) + WGT * FSBEN
       END IF

    END IF

!!  13-14: UNITS WITH DISABLED

    IF (FSNDIS > 0) THEN
       ELIG_BEN (13,KTH) = ELIG_BEN (13,KTH) + FSBEN
       WGT_ELIG_BEN (13,KTH) = WGT_ELIG_BEN (13,KTH) + WGT * FSBEN
       IF (PARTIC) THEN
          PART_BEN (13,KTH) = PART_BEN (13,KTH) + FSBEN
          WGT_PART_BEN (13,KTH) = WGT_PART_BEN (13,KTH) + WGT * FSBEN
       END IF
    ELSE
       ELIG_BEN (14,KTH) = ELIG_BEN (14,KTH) + FSBEN
       WGT_ELIG_BEN (14,KTH) = WGT_ELIG_BEN (14,KTH) + WGT * FSBEN
       IF (PARTIC) THEN
          PART_BEN (14,KTH) = PART_BEN (14,KTH) + FSBEN
          WGT_PART_BEN (14,KTH) = WGT_PART_BEN (14,KTH) + WGT * FSBEN
       END IF

    END IF

!!  15-22: AREA OF ORIGIN OF HEAD
    IF (AREA_OF_ORIGIN) THEN
       ORIGIN_IDX = fshORIGIN + 14

       ELIG_BEN (ORIGIN_IDX,KTH) = ELIG_BEN (ORIGIN_IDX,KTH) + FSBEN
       WGT_ELIG_BEN (ORIGIN_IDX,KTH) = WGT_ELIG_BEN (ORIGIN_IDX,KTH) + WGT * FSBEN
       IF (PARTIC) THEN
          PART_BEN (ORIGIN_IDX,KTH) = PART_BEN (ORIGIN_IDX,KTH) + FSBEN
          WGT_PART_BEN (ORIGIN_IDX,KTH) = WGT_PART_BEN (ORIGIN_IDX,KTH) + WGT * FSBEN
       END IF
    END IF


    !!  PERSON-LEVEL SECTION

    PER_CAP_BEN = REAL(FSBEN) / FSUSIZE

!--- 23 Total persons
    ELIG_BEN (23,KTH) = ELIG_BEN (23,KTH) + FSBEN
    WGT_ELIG_BEN (23,KTH) = WGT_ELIG_BEN (23,KTH) + WGT * FSBEN
    IF (PARTIC) THEN
       PART_BEN (23,KTH) = PART_BEN (23,KTH) + FSBEN
       WGT_PART_BEN (23,KTH) = WGT_PART_BEN (23,KTH) + WGT * FSBEN
    END IF

!--- 24 Kids
    ELIG_BEN (24,KTH) = ELIG_BEN (24,KTH)         +  PER_CAP_BEN * FSNKID
    WGT_ELIG_BEN (24,KTH) = WGT_ELIG_BEN (24,KTH) +  PER_CAP_BEN * FSNKID * WGT
    IF (PARTIC) THEN
       PART_BEN (24,KTH) = PART_BEN (24,KTH)         + PER_CAP_BEN * FSNKID
       WGT_PART_BEN (24,KTH) = WGT_PART_BEN (24,KTH) + PER_CAP_BEN * FSNKID * WGT
    END IF

!--- 25 Adults
    ELIG_BEN (25,KTH) = ELIG_BEN (25,KTH)         + PER_CAP_BEN * NUM_ADULTS
    WGT_ELIG_BEN (25,KTH) = WGT_ELIG_BEN (25,KTH) + PER_CAP_BEN * NUM_ADULTS * WGT
    IF (PARTIC) THEN
       PART_BEN (25,KTH) = PART_BEN (25,KTH)         + PER_CAP_BEN * NUM_ADULTS
       WGT_PART_BEN (25,KTH) = WGT_PART_BEN (25,KTH) + PER_CAP_BEN * NUM_ADULTS * WGT
    END IF

!-- 26 Elderly
    ELIG_BEN (26,KTH) = ELIG_BEN (26,KTH)         + PER_CAP_BEN * FSNELDER
    WGT_ELIG_BEN (26,KTH) = WGT_ELIG_BEN (26,KTH) + PER_CAP_BEN * FSNELDER * WGT
    IF (PARTIC) THEN
       PART_BEN (26,KTH) = PART_BEN (26,KTH)         + PER_CAP_BEN * FSNELDER
       WGT_PART_BEN (26,KTH) = WGT_PART_BEN (26,KTH) + PER_CAP_BEN * FSNELDER * WGT
    END IF

!-- 27 Males
    ELIG_BEN (27,KTH) = ELIG_BEN (27,KTH)         + PER_CAP_BEN * FSNMALE
    WGT_ELIG_BEN (27,KTH) = WGT_ELIG_BEN (27,KTH) + PER_CAP_BEN * FSNMALE * WGT
    IF (PARTIC) THEN
       PART_BEN (27,KTH) = PART_BEN (27,KTH)         + PER_CAP_BEN * FSNMALE
       WGT_PART_BEN (27,KTH) = WGT_PART_BEN (27,KTH) + PER_CAP_BEN * FSNMALE * WGT
    END IF

!-- 28 Females
    ELIG_BEN (28,KTH) = ELIG_BEN (28,KTH)         + PER_CAP_BEN * FSNFEMALE
    WGT_ELIG_BEN (28,KTH) = WGT_ELIG_BEN (28,KTH) + PER_CAP_BEN * FSNFEMALE * WGT
    IF (PARTIC) THEN
       PART_BEN (28,KTH) = PART_BEN (28,KTH)         + PER_CAP_BEN * FSNFEMALE
       WGT_PART_BEN (28,KTH) = WGT_PART_BEN (28,KTH) + PER_CAP_BEN * FSNFEMALE * WGT
    END IF


    IF (PERSON_LEVEL_DISAB) THEN

!-- 29 Disabled
       ELIG_BEN (29,KTH) = ELIG_BEN (29,KTH)         + PER_CAP_BEN * FSNDIS
       WGT_ELIG_BEN (29,KTH) = WGT_ELIG_BEN (29,KTH) + PER_CAP_BEN * FSNDIS * WGT
       IF (PARTIC) THEN
          PART_BEN (29,KTH) = PART_BEN (29,KTH)         + PER_CAP_BEN * FSNDIS
          WGT_PART_BEN (29,KTH) = WGT_PART_BEN (29,KTH) + PER_CAP_BEN * FSNDIS * WGT
       END IF

!-- 30 Not Disabled
       num_not_disab = fsusize - fsndis
       ELIG_BEN (30,KTH) = ELIG_BEN (30,KTH)         + PER_CAP_BEN * NUM_NOT_DISAB
       WGT_ELIG_BEN (30,KTH) = WGT_ELIG_BEN (30,KTH) + PER_CAP_BEN * NUM_NOT_DISAB * WGT
       IF (PARTIC) THEN
          PART_BEN (30,KTH) = PART_BEN (30,KTH)         + PER_CAP_BEN * NUM_NOT_DISAB
          WGT_PART_BEN (30,KTH) = WGT_PART_BEN (30,KTH) + PER_CAP_BEN * NUM_NOT_DISAB * WGT
       END IF

    END IF

    END IF



    return



!******************************************************************
!     Print the tables
!******************************************************************
900 CONTINUE


    !! PUT ANY DATABASE-SPECIFIC LABELS HERE:

    tab_protect_ben_stats = var_protect_ben_stats
    IF (MODEL_LABEL == "   QC MINI") THEN
       ROWLAB( 9) = '  White, not of Hispanic Origin           '
       ROWLAB(10) = '  Black, not of Hispanic Origin           '
       ROWLAB(11) = '  Hispanic                                '
       ROWLAB(12) = '  Asian or Pacific Islander               '
       ROWLAB(13) = '  American Indian or Alaskan Native       '
       ROWLAB(14) = '  Unknown                                 '

    ELSEIF (MODEL_LABEL == " MATH SIPP" ) THEN
       ROWLAB( 9) = '  White alone, not of Hispanic Origin     '
       ROWLAB(10) = '  Black alone, not of Hispanic Origin     '
       ROWLAB(11) = '  Hispanic (see below)                    '
       ROWLAB(12) = '  Asian alone                             '
       ROWLAB(13) = '  Other                                   '
       ROWLAB(14) = '  Unknown                                 '

    END IF




!---- Calculate percent change of plans from baseline

    !!  REFORM PCT OF TOTAL
    DO J = 1, NBR_OF_KTHS

       IF (J > 1 .AND. NBR_OF_KTHS == 1) EXIT

       DO I = 1, NROWS
          IF(abs(ELIG_VALUE(I,J)) > 0.0)     AVG_ELIG_BEN(I,J) = ELIG_BEN(I,J) / ELIG_VALUE(I,J)
          IF(abs(WGT_ELIG_VALUE(I,J)) > 0.0) WGT_AVG_ELIG_BEN(I,J) = WGT_ELIG_BEN(I,J) / WGT_ELIG_VALUE(I,J)
          IF(abs(PART_VALUE(I,J)) > 0.0)     AVG_PART_BEN(I,J) = PART_BEN(I,J) / PART_VALUE(I,J)
          IF(abs(WGT_PART_VALUE(I,J)) > 0.0) WGT_AVG_PART_BEN(I,J) = WGT_PART_BEN(I,J) / WGT_PART_VALUE(I,J)
       END DO



       !!  CHANGE FROM BASELAW

       !!  DIFFERENCE (REFORM - BASSELAW):
       DO I = 1, NROWS
          ELIG_BEN_DIFF    (I,J) = AVG_ELIG_BEN    (I,J) - AVG_ELIG_BEN    (I,1)
          WGT_ELIG_BEN_DIFF(I,J) = WGT_AVG_ELIG_BEN(I,J) - WGT_AVG_ELIG_BEN(I,1)
          PART_BEN_DIFF    (I,J) = AVG_PART_BEN    (I,J) - AVG_PART_BEN    (I,1)
          WGT_PART_BEN_DIFF(I,J) = WGT_AVG_PART_BEN(I,J) - WGT_AVG_PART_BEN(I,1)
       END DO


      !!  COMPUTE CHARACTER (COMMA8) FIELDS:
       DO I = 1, NROWS
          !! UNWGT
          BEN_FIELD(I,J,1,2) = AVG_ELIG_BEN (I,J)
          BEN_FIELD(I,J,2,2) = AVG_PART_BEN (I,J)
          BEN_DIFF (I,J,1,2) = ELIG_BEN_DIFF(I,J)
          BEN_DIFF (I,J,2,2) = PART_BEN_DIFF(I,J)


          !!  WGT
          BEN_FIELD(I,J,1,1) = WGT_AVG_ELIG_BEN (I,J)
          BEN_FIELD(I,J,2,1) = WGT_AVG_PART_BEN (I,J)
          BEN_DIFF (I,J,1,1) = WGT_ELIG_BEN_DIFF(I,J)
          BEN_DIFF (I,J,2,1) = WGT_PART_BEN_DIFF(I,J)

       END DO

    END DO   !!  REFORM LOOP





!-------------------------------------------------------------------
!   PRINT UP TO 4 PAGES PER PLAN:
!   BY WEIGHTED & UNWEIGHTED
!   BY ELIGIBLE (MATH SIPP & MATH CPS ONLY) & PARTICIPATING
!-------------------------------------------------------------------
    istart = 2
    if (nbr_of_kths == 1) istart = 1

    IF (model_code /= "QCMM") THEN
      ! Print JSON table
      WRITE (JSON_FILE, *) '"Table 6": [' ! open table (array of arrays)

      DO i=1,NROWS
        IF (dostats(nth)) THEN
          WRITE (JSON_FILE, 1121, ADVANCE='no') TRIM(json_rowlab1(i)), TRIM(json_rowlab2(i)), &
            WGT_AVG_ELIG_BEN(i,1), &
            WGT_AVG_ELIG_BEN(i,2), &
            WGT_ELIG_BEN_DIFF(i,2), &
            TRIM(tab_protect_ben_stats(i,2,1,1)) ! Indexed by characteristic (=row=i), participation (1=eligibles, 2=participants), unknown (?), and reform number (1=first reform?)
        ELSE
          WRITE (JSON_FILE, 1111, ADVANCE='no') TRIM(json_rowlab1(i)), TRIM(json_rowlab2(i)), &
            WGT_AVG_ELIG_BEN(i,1), &
            WGT_AVG_ELIG_BEN(i,2), &
            WGT_ELIG_BEN_DIFF(i,2)
        ENDIF
        if (i /= NROWS) WRITE (JSON_FILE, *) ',' ! not the last...
      end do

      WRITE (JSON_FILE, *) ! newline
      WRITE (JSON_FILE, *) '],' ! close table (array of arrays, not the last one)
    END IF

    ! Print JSON table
    WRITE (JSON_FILE, *) '"Table 6A": [' ! open table (array of arrays)

    DO i=1,NROWS
      IF (dostats(nth)) THEN
        WRITE (JSON_FILE, 1121, ADVANCE='no') TRIM(json_rowlab1(i)), TRIM(json_rowlab2(i)), &
          WGT_AVG_PART_BEN(i,1), &
          WGT_AVG_PART_BEN(i,2), &
          WGT_PART_BEN_DIFF(i,2), &
          TRIM(tab_protect_ben_stats(i,2,1,1)) ! Indexed by characteristic (=row=i), participation (1=eligibles, 2=participants), unknown (?), and reform number (1=first reform?)
      ELSE
        WRITE (JSON_FILE, 1111, ADVANCE='no') TRIM(json_rowlab1(i)), TRIM(json_rowlab2(i)), &
          WGT_AVG_PART_BEN(i,1), &
          WGT_AVG_PART_BEN(i,2), &
          WGT_PART_BEN_DIFF(i,2)
      ENDIF
      if (i /= NROWS) WRITE (JSON_FILE, *) ',' ! not the last...
    end do

    WRITE (JSON_FILE, *) ! newline
    WRITE (JSON_FILE, *) '],' ! close table (array of arrays, not the last one)

    IF (model_code /= "QCMM") THEN
      ! Print JSON table
      WRITE (JSON_FILE, *) '"Table 6B": [' ! open table (array of arrays)

      DO i=1,NROWS
        WRITE (JSON_FILE, 1111, ADVANCE='no') TRIM(json_rowlab1(i)), TRIM(json_rowlab2(i)), &
        AVG_ELIG_BEN(i,1), &
        AVG_ELIG_BEN(i,2), &
        ELIG_BEN_DIFF(i,2)
        ! No significance testing for counts
        if (i /= NROWS) WRITE (JSON_FILE, *) ',' ! not the last...
      end do

      WRITE (JSON_FILE, *) ! newline
      WRITE (JSON_FILE, *) '],' ! close table (array of arrays, not the last one)
    END IF

    ! Print JSON table
    WRITE (JSON_FILE, *) '"Table 6C": [' ! open table (array of arrays)

    DO i=1,NROWS
      WRITE (JSON_FILE, 1111, ADVANCE='no') TRIM(json_rowlab1(i)), TRIM(json_rowlab2(i)), &
      AVG_PART_BEN(i,1), &
      AVG_PART_BEN(i,2), &
      PART_BEN_DIFF(i,2)
      ! No significance testing for counts
      if (i /= NROWS) WRITE (JSON_FILE, *) ',' ! not the last...
    end do

    WRITE (JSON_FILE, *) ! newline
    WRITE (JSON_FILE, *) '],' ! close table (array of arrays, not the last one)


    DO J = istart, NBR_OF_KTHS    !! Loop over all plans

     PLANTEMP = 'PLAN ' //PLANNBR_TABLE(J-1) // ': '//PLANNAME_TABLE(J-1)
     CALL CENTER_TEXT(PLANTEMP,80)
     PLANTEMP2 = PLANNAME_TABLE(J-1)

     DO K = 1, 2                  !! WEIGHTED/UNWEIGHTED


       !-- Pass = 1 for Eligibles table
       !-- Pass = 2 for Participants table
       DO PASS = 1,2

          IF (PASS == 1 .AND. .NOT. SHOW_ELIG  ) CYCLE

          !----  Print table
          CALL ISNEWPG(TABFILE, PAGE_BREAK_NUMLINES + 1)  ! +1 lets ISNEWPG know
                                                          ! lines are coming - but
                                                          ! not exactly how many.

          IF (PASS == 1) THEN
             WRITE (TABFILE,1100) plantemp ! E title
          ELSE
             WRITE (TABFILE,1101) plantemp ! P title
          ENDIF


          IF (K == 1) THEN
             WRITE (TABFILE,1150)  ! WGT title
          ELSE
             WRITE (TABFILE,1151)  ! UNWGT title
          ENDIF

          WRITE (TABFILE,1201)

          WRITE (TABFILE,1210)  ! underline


          I = 0
          DO II = 1, NROWS+EMPTY_ROWS
             DO L = 1, EMPTY_ROWS
                IF (II == SKIP_ROW(L)) THEN
                   WRITE(TABFILE, 1214)ROWLAB(II)
                   GOTO 201
                END IF
             END DO

             I = I + 1
             IF (NBR_OF_KTHS == 1) THEN
               !!  BASELAW ONLY
               WRITE(TABFILE, 1215)           &
                 ROWLAB(II)                   &
                ,BEN_FIELD (I,1,PASS,K)
             ELSE
               temp_stat = " "
               if (k == 1) temp_stat = tab_protect_ben_stats(I,PASS,1,J-1)
               !!  BASELAW PLUS KTH REFORM
               WRITE(TABFILE, 1216)         &
                 ROWLAB(II)                 &
                ,BEN_FIELD (I,1,PASS,K)     &
                ,BEN_FIELD (I,J,PASS,K)     &
                ,BEN_DIFF  (I,J,PASS,K),  temp_stat
             END IF

             if (create_table_extracts) then

              if (pass == 1) then
                displ_pass = "ELIG"
              else
                displ_pass = "PART"
              end if

              if (k == 1) then
                displ_wgt = "W"
              else
                displ_wgt = "U"
              end if

              IF (NBR_OF_KTHS == 1) THEN
                WRITE (39, 3901)                &
                   "t_prot_ben  "               &
                  ,j                            &
                  ,adjustl(plantemp2)           &
                  ,displ_wgt                    &
                  ,displ_pass                   &
                  ,rowlab(ii)                   &
                 ,BEN_FIELD (I,1,PASS,K)        &
                  , 0.0                         &
                  , 0.0                         &
                  , " "
              else
                WRITE (39, 3901)                &
                   "t_prot_ben  "               &
                  ,j                            &
                  ,adjustl(plantemp2)           &
                  ,displ_wgt                    &
                  ,displ_pass                   &
                  ,rowlab(ii)                   &
                  ,BEN_FIELD (I,1,PASS,K)       &
                  ,BEN_FIELD (I,J,PASS,K)       &
                  ,BEN_DIFF  (I,J,PASS,K)       &
                  ,temp_stat
               end if

              end if

 201         CONTINUE
          END DO

 3901     format(1x,a12, 2x,i3, 2x,a40, 2x,a1, 2x,a4, 2x,a42, 3(2x,f10.2), 2x,a1)

          WRITE (TABFILE,1210)
          if (k == 1) WRITE (TABFILE,1290)  !! show key for weighted tables only



       END DO ! end of pass loop

     END DO ! end of WEIGHTED/UNWEIGHTED
    END DO ! end of plan


    RETURN
!----------------------------------------------------------------------
! FORMAT STATEMENTS
!----------------------------------------------------------------------
1100 format( T37,'                              ' //                        &
             T37,'  PROTECTED CLASS IMPACTS ON SNAP UNIT AVERAGE BENEFITS' / &
             T37,'          UNIVERSE: ELIGIBLE SNAP UNITS' //         &
             t24, a80 )

1101 format( T37,'                              ' //                        &
             T37,'  PROTECTED CLASS IMPACTS ON SNAP UNIT AVERAGE BENEFITS' / &
             T37,'         UNIVERSE: PARTICIPATING SNAP UNITS' //       &
             t24, a80 )


1150 FORMAT(T5, "WEIGHTED")
1151 FORMAT(T5, "UNWEIGHTED")

1201 format(/ 1X, 131('-')                                                        &
 ,//,                       t50, 'Baselaw', t66, 'Reform ', t82, 'Difference'     &
  ,/,                       t50, 'Average', t66, 'Average', t82, 'From Baselaw'   &
  ,/, t2, 'Characteristic', t50, 'Benefit', T66, 'Benefit', t82, 'Average'        &
   )

1210 format(1X,131('-'))

1214 format (T2,A)

1215 format (T2, A, T49, F8.2)
1216 format (T2, A, T49, F8.2, T65, F8.2, T81, F8.2, A)

! JSON format of row, including 2 label lines
1111 format('["', A, '", "', A, '",', F8.2, ',', F8.2, ',', F8.2, ']') ! Both weighted and unweighted
1121 FORMAT('["', A, '", "', A, '",', F8.2, ',', F8.2, ',', F8.2, ',"', A,'"]') ! With significance test


!1270 format(/ 1X,'PLAN ',A,':  ',A70)

1290 format (&
  /,1x,'Statistics key:' &
 ,/,1x,'* Percentage change is statistically different from zero at a 90% level of significance' &
 ,/,1x,'+ Percentage change for subgroup is statistically different from the overall change at a 90% level of significance' &
 ,/,1x,'# Both conditions are met' &
,//,1x, 'The benefit values in the table reflect the change in average benefits, but the benefit statistics' &
 ,/,1x, 'are based on the change in per unit benefits.'  &
  )

end subroutine FS_Tab_protected_benefits