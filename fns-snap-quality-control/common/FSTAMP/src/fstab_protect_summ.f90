!**************************************************************************************************
! Source File:  FStab_protect_summ.F90                  
! Called By:    FS_TABLES                   
!
! TABLE 8 - Protected Class Impacts in Eligible and Participating Food Stamp Units
!
!**************************************************************************************************

      SUBROUTINE FS_Tab_protected_summary( &
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


    USE FSWORK, ONLY:       &
       SHOW_ELIG            &
      ,PLANNAME_TABLE       &
      ,PLANNBR_TABLE        &
      ,USE_HEAD_RACE        &
      ,AREA_OF_ORIGIN       &
      ,PERSON_LEVEL_DISAB   &
      ,tab_protect_summ_stats           &
      ,var_protect_summ_stats           &
      ,create_table_extracts

    USE FSPARM, ONLY: JSON_FILE, dostats
    use Utils


      IMPLICIT NONE


      INTEGER, parameter ::  MAX_KTH = 5

!---- Declare parameters from calling program
      INTEGER, intent(in)  ::  KTH
      INTEGER, intent(in)  ::  FSBEN
      INTEGER, intent(in)  ::  FSUSIZE
      INTEGER, intent(in)  ::  fsndis
      INTEGER, intent(in)  ::  fsnelder
      INTEGER, intent(in)  ::  fsnadult
      INTEGER, intent(in)  ::  fsnkid
      INTEGER, intent(in)  ::  fsnmale
      INTEGER, intent(in)  ::  fsnfemale
      INTEGER, intent(in)  ::  fshrace
      INTEGER, intent(in)  ::  fshethnic
      INTEGER, intent(in)  ::  fshorigin
      LOGICAL, intent(in)  ::  PARTIC
      REAL   , intent(in)  ::  WGT


!---- Variables for tables
      CHARACTER(len=80) ::  PLANTEMP
      CHARACTER(len=80) ::  PLANTEMP2

      INTEGER :: I, J, K, L, PASS, II, istart
      INTEGER :: NBR_OF_KTHS = 0

      INTEGER :: NUM_ADULTS, NUM_NOT_DISAB, ORIGIN_IDX, RACE_IDX

      INTEGER, PARAMETER  :: UNIT_ROW = 1
      INTEGER, PARAMETER  :: PERS_ROW = 23
      integer, PARAMETER  :: NROWS = 30

!---- Number of units by characteristic and KTH (cells)
      REAL (8) :: ELIG_VALUE(NROWS,MAX_KTH + 1) = 0
      REAL (8) :: PART_VALUE(NROWS,MAX_KTH + 1) = 0
      REAL (8) :: WGT_ELIG_VALUE(NROWS,MAX_KTH + 1) = 0.0
      REAL (8) :: WGT_PART_VALUE(NROWS,MAX_KTH + 1) = 0.0

!---- Number of units by characteristic and KTH (cells)
      REAL (8) :: ELIG_DIFF (NROWS,MAX_KTH + 1) = 0
      REAL (8) :: PART_DIFF (NROWS,MAX_KTH + 1) = 0
      REAL (8) :: WGT_ELIG_DIFF (NROWS,MAX_KTH + 1) = 0.0
      REAL (8) :: WGT_PART_DIFF (NROWS,MAX_KTH + 1) = 0.0

!---- Percent change by characteristic and KTH (cells)
      REAL (8)  :: ELIG_PCT(NROWS,MAX_KTH + 1) = 0.0
      REAL (8)  :: PART_PCT(NROWS,MAX_KTH + 1) = 0.0
      REAL (8)  :: WGT_ELIG_PCT(NROWS,MAX_KTH + 1) = 0.0
      REAL (8)  :: WGT_PART_PCT(NROWS,MAX_KTH + 1) = 0.0

!---- Percent change by characteristic and KTH (cells)
      REAL (8)  :: PCT_FIELD (NROWS, MAX_KTH + 1, 2, 2) = 0.0
      REAL (8)  :: PCT_DIFF  (NROWS, MAX_KTH + 1, 2, 2) = 0.0

      REAL (8)  :: ELIG_DIFF_PCT     (NROWS, MAX_KTH + 1) = 0.0
      REAL (8)  :: PART_DIFF_PCT     (NROWS, MAX_KTH + 1) = 0.0
      REAL (8)  :: WGT_ELIG_DIFF_PCT (NROWS, MAX_KTH + 1) = 0.0
      REAL (8)  :: WGT_PART_DIFF_PCT (NROWS, MAX_KTH + 1) = 0.0



!---- Comma display fields
      INTEGER, parameter :: FIELD_LEN = 14
      CHARACTER (LEN=FIELD_LEN ) :: COMMA_FIELD (NROWS, MAX_KTH + 1, 2, 2) = " "
      CHARACTER (LEN=FIELD_LEN ) :: COMMA_DIFF  (NROWS, MAX_KTH + 1, 2, 2) = " "

!---- for excel tabs:
      real(8) :: XL_FIELD (NROWS, MAX_KTH + 1, 2, 2) = 0.0
      real(8) :: XL_DIFF  (NROWS, MAX_KTH + 1, 2, 2) = 0.0



!---- Row labels for both tables
      integer, parameter :: empty_rows = 17
      integer, dimension(empty_rows) :: skip_row = (/2,3, 7,8, 15,16, 19,20,  23,24, 33, 35,36, 40,41, 44,45/)
      CHARACTER(len=8) :: ROWLABa(nrows+empty_rows)= (/  &
      'Units   '  &  !  1
     ,'Units   '  &  !  2
     ,'Units   '  &  !  3
     ,'Units   '  &  !  4
     ,'Units   '  &  !  5
     ,'Units   '  &  !  6
     ,'Units   '  &  !  7
     ,'Units   '  &  !  8
     ,'Units   '  &  !  9
     ,'Units   '  &  ! 10
     ,'Units   '  &  ! 13
     ,'Units   '  &  ! 11
     ,'Units   '  &  ! 12
     ,'Units   '  &  ! 14
     ,'Units   '  &  ! 15
     ,'Units   '  &  ! 16
     ,'Units   '  &  ! 17
     ,'Units   '  &  ! 18
     ,'Units   '  &  ! 19
     ,'Units   '  &  ! 20
     ,'Units   '  &  ! 21
     ,'Units   '  &  ! 22
     ,'Units   '  &  ! 23
     ,'Units   '  &  ! 24
     ,'Units   '  &  ! 25
     ,'Units   '  &  ! 26
     ,'Units   '  &  ! 27
     ,'Units   '  &  ! 28
     ,'Units   '  &  ! 29
     ,'Units   '  &  ! 30
     ,'Units   '  &  ! 31
     ,'Units   '  &  ! 32
     ,'Units   '  &  ! 33
     ,'Persons '  &  ! 34
     ,'Persons '  &  ! 35
     ,'Persons '  &  ! 36
     ,'Persons '  &  ! 37
     ,'Persons '  &  ! 38
     ,'Persons '  &  ! 39
     ,'Persons '  &  ! 40
     ,'Persons '  &  ! 41
     ,'Persons '  &  ! 42
     ,'Persons '  &  ! 43
     ,'Persons '  &  ! 44
     ,'Persons '  &  ! 45
     ,'Persons '  &  ! 46
     ,'Persons '  &  ! 47
     /)

      CHARACTER(len=24) :: ROWLABb(nrows+empty_rows)= (/  &
      'Total Units             '  &  !  1
     ,'Units with              '  &  !  2
     ,'Units with              '  &  !  3
     ,'Units with              '  &  !  4
     ,'Units with              '  &  !  5
     ,'Units with              '  &  !  6
     ,'Head                    '  &  !  7
     ,'Race of head            '  &  !  8
     ,'Race of head            '  &  !  9
     ,'Race of head            '  &  ! 10
     ,'Race of head            '  &  ! 13
     ,'Race of head            '  &  ! 11
     ,'Race of head            '  &  ! 12
     ,'Race of head            '  &  ! 14
     ,'Head                    '  &  ! 15
     ,'Ethnicity of head       '  &  ! 16
     ,'Ethnicity of head       '  &  ! 17
     ,'Ethnicity of head       '  &  ! 18
     ,'Head                    '  &  ! 19
     ,'Units with Disabled     '  &  ! 20
     ,'Units with Disabled     '  &  ! 21
     ,'Units with Disabled     '  &  ! 22
     ,'Head                    '  &  ! 23
     ,'Area of Origin of head  '  &  ! 24
     ,'Area of Origin of head  '  &  ! 25
     ,'Area of Origin of head  '  &  ! 26
     ,'Area of Origin of head  '  &  ! 27
     ,'Area of Origin of head  '  &  ! 28
     ,'Area of Origin of head  '  &  ! 29
     ,'Area of Origin of head  '  &  ! 30
     ,'Area of Origin of head  '  &  ! 31
     ,'Area of Origin of head  '  &  ! 32
     ,'Persons                 '  &  ! 33
     ,'Total Persons           '  &  ! 34
     ,'Persons                 '  &  ! 35
     ,'Age                     '  &  ! 36
     ,'Age                     '  &  ! 37
     ,'Age                     '  &  ! 38
     ,'Age                     '  &  ! 39
     ,'Persons                 '  &  ! 40
     ,'Sex                     '  &  ! 41
     ,'Sex                     '  &  ! 42
     ,'Sex                     '  &  ! 43
     ,'Persons                 '  &  ! 44
     ,'Disability              '  &  ! 45
     ,'Disability              '  &  ! 46
     ,'Disability              '  &  ! 47
     /)

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

    character(len=1) :: temp_stat

    character(len=4) :: displ_pass
    character(len=1) :: displ_wgt

!---- By-pass calculations if print requested

    IF (KEOF == 3) GOTO 900
!****************************************************************
!     Perform table calculations
!****************************************************************


!-- Keep track of highest KTH computed
    IF (KTH > MAX_KTH + 1) RETURN !--- TABLE3 CAN ONLY HANDLE 5 REFORMS
    IF (KTH > NBR_OF_KTHS) NBR_OF_KTHS = KTH

!--- 1 Total units
    IF (FSBEN > 0) THEN
       ELIG_VALUE (1,KTH) = ELIG_VALUE (1,KTH) + 1
       WGT_ELIG_VALUE (1,KTH) = WGT_ELIG_VALUE (1,KTH) + WGT
       IF (PARTIC) THEN
          PART_VALUE (1,KTH) = PART_VALUE (1,KTH) + 1
          WGT_PART_VALUE (1,KTH) = WGT_PART_VALUE (1,KTH) + WGT
       END IF

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

    END IF






    return


!******************************************************************
!     Print the tables
!******************************************************************
900 CONTINUE

     write(prfile, *) " "
     write(prfile, *) " use_head_race = ", use_head_race
     write(prfile, *) " "


    !! PUT ANY DATABASE-SPECIFIC LABELS HERE:
    tab_protect_summ_stats = var_protect_summ_stats
    IF (MODEL_LABEL == "   QC MINI" ) THEN
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


    write(prfile, 5000) " STATS    ", (i, i = 1,30)
    write(prfile, 5100) " var_protect_summ_stats", (var_protect_summ_stats(I,2,1,1), I = 1,30)
    write(prfile, 5100) " tab_protect_summ_stats ", (tab_protect_summ_stats (I,2,1,1), I = 1,30)

5000  format(T2, A, t12, 30i3)
5100  format(T2, A, T12, 30A3)



!---- Calculate percent change of plans from baseline

    !!  REFORM PCT OF TOTAL
    DO J = 1, NBR_OF_KTHS

       IF (J > 1 .AND. NBR_OF_KTHS == 1) EXIT

       DO I = 1, PERS_ROW-1
          IF(abs(ELIG_VALUE(UNIT_ROW,J)) > 0.0)     ELIG_PCT(I,J) = ELIG_VALUE(I,J) / ELIG_VALUE(UNIT_ROW,J) * 100.0
          IF(abs(WGT_ELIG_VALUE(UNIT_ROW,J)) > 0.0) WGT_ELIG_PCT(I,J) = WGT_ELIG_VALUE(I,J) / WGT_ELIG_VALUE(UNIT_ROW,J) * 100.0
          IF(abs(PART_VALUE(UNIT_ROW,J)) > 0.0)     PART_PCT(I,J) = PART_VALUE(I,J) / PART_VALUE(UNIT_ROW,J) * 100.0
          IF(abs(WGT_PART_VALUE(UNIT_ROW,J)) > 0.0) WGT_PART_PCT(I,J) = WGT_PART_VALUE(I,J) / WGT_PART_VALUE(UNIT_ROW,J) * 100.0
       END DO

       DO I = PERS_ROW, NROWS
          IF(abs(ELIG_VALUE(PERS_ROW,J)) > 0.0)     ELIG_PCT(I,J) = ELIG_VALUE(I,J) / ELIG_VALUE(PERS_ROW,J) * 100.0
          IF(abs(WGT_ELIG_VALUE(PERS_ROW,J)) > 0.0) WGT_ELIG_PCT(I,J) = WGT_ELIG_VALUE(I,J) / WGT_ELIG_VALUE(PERS_ROW,J) * 100.0
          IF(abs(PART_VALUE(PERS_ROW,J)) > 0.0)     PART_PCT(I,J) = PART_VALUE(I,J) / PART_VALUE(PERS_ROW,J) * 100.0
          IF(abs(WGT_PART_VALUE(PERS_ROW,J)) > 0.0) WGT_PART_PCT(I,J) = WGT_PART_VALUE(I,J) / WGT_PART_VALUE(PERS_ROW,J) * 100.0
       END DO



       !!  CHANGE FROM BASELAW

       !!  DIFFERENCE (REFORM - BASSELAW):
       DO I = 1, NROWS
          ELIG_DIFF    (I,J) = ELIG_VALUE    (I,J) - ELIG_VALUE    (I,1)
          WGT_ELIG_DIFF(I,J) = WGT_ELIG_VALUE(I,J) - WGT_ELIG_VALUE(I,1)
          PART_DIFF    (I,J) = PART_VALUE    (I,J) - PART_VALUE    (I,1)
          WGT_PART_DIFF(I,J) = WGT_PART_VALUE(I,J) - WGT_PART_VALUE(I,1)
       END DO

       !! PCT CHANGE:
       DO I = 1, NROWS
          IF (abs(ELIG_VALUE(I,J)) > 0.0)     ELIG_DIFF_PCT    (I,J) = ELIG_DIFF     (I,J) / ELIG_VALUE    (I,1) * 100.0
          IF (abs(WGT_ELIG_VALUE(I,J)) > 0.0) WGT_ELIG_DIFF_PCT(I,J) = WGT_ELIG_DIFF (I,J) / WGT_ELIG_VALUE(I,1) * 100.0
          IF (abs(PART_VALUE(I,J)) > 0.0)     PART_DIFF_PCT    (I,J) = PART_DIFF     (I,J) / PART_VALUE    (I,1) * 100.0
          IF (abs(WGT_PART_VALUE(I,J)) > 0.0) WGT_PART_DIFF_PCT(I,J) = WGT_PART_DIFF (I,J) / WGT_PART_VALUE(I,1) * 100.0
       END DO

      !!  COMPUTE CHARACTER (COMMA8) FIELDS:
       DO I = 1, NROWS
          !! UNWGT
          comma_field(i,j,1,2) = COMMA8(ELIG_VALUE(I,J),FIELD_LEN)
          comma_field(i,j,2,2) = COMMA8(PART_VALUE(I,J),FIELD_LEN)
          comma_diff(i,j,1,2) = COMMA8(ELIG_DIFF (I,J),FIELD_LEN)
          comma_diff(i,j,2,2) = COMMA8(PART_DIFF (I,J),FIELD_LEN)
          PCT_FIELD  (I,J,1,2) =             ELIG_PCT  (I,J)
          PCT_FIELD  (I,J,2,2) =             PART_PCT  (I,J)
          PCT_DIFF   (I,J,1,2) =        ELIG_DIFF_PCT  (I,J)
          PCT_DIFF   (I,J,2,2) =        PART_DIFF_PCT  (I,J)

          XL_FIELD(I,J,1,2) = ELIG_VALUE(I,J)
          XL_FIELD(I,J,2,2) = PART_VALUE(I,J)
          XL_DIFF (I,J,1,2) = ELIG_DIFF (I,J)
          XL_DIFF (I,J,2,2) = PART_DIFF (I,J)


          !!  WGT
          COMMA_FIELD(I,J,1,1) = COMMA8(WGT_ELIG_VALUE(I,J),FIELD_LEN)
          COMMA_FIELD(I,J,2,1) = COMMA8(WGT_PART_VALUE(I,J),FIELD_LEN)
          COMMA_DIFF (I,J,1,1) = COMMA8(WGT_ELIG_DIFF (I,J),FIELD_LEN)
          COMMA_DIFF (I,J,2,1) = COMMA8(WGT_PART_DIFF (I,J),FIELD_LEN)
          PCT_FIELD  (I,J,1,1) =        WGT_ELIG_PCT  (I,J)
          PCT_FIELD  (I,J,2,1) =        WGT_PART_PCT  (I,J)
          PCT_DIFF   (I,J,1,1) =   WGT_ELIG_DIFF_PCT  (I,J)
          PCT_DIFF   (I,J,2,1) =   WGT_PART_DIFF_PCT  (I,J)

          XL_FIELD(I,J,1,1) = WGT_ELIG_VALUE(I,J)
          XL_FIELD(I,J,2,1) = WGT_PART_VALUE(I,J)
          XL_DIFF (I,J,1,1) = WGT_ELIG_DIFF (I,J)
          XL_DIFF (I,J,2,1) = WGT_PART_DIFF (I,J)

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
      ! Print JSON table (PASS=1 for eligible units, K=(1,2) for (weighted,unweighted), I=row)
      WRITE (JSON_FILE, *) '"Table 5": [' ! open table (array of arrays)

      DO i=1,NROWS
        IF (dostats(nth)) THEN
          ! stats is indexed by row, pass (1=eligible, 2=participant), weight (1=weighted, 2=unweighted), and plan number (skipping baselaw, so 1=first reform)
          WRITE (JSON_FILE, 1112, ADVANCE='no') TRIM(json_rowlab1(i)), TRIM(json_rowlab2(i)), &
            NINT(WGT_ELIG_VALUE(i,1), SELECTED_INT_KIND(12)), WGT_ELIG_PCT(i,1), &
            NINT(WGT_ELIG_VALUE(i,2), SELECTED_INT_KIND(12)), WGT_ELIG_PCT(i,2), &
            NINT(WGT_ELIG_DIFF(i,2), SELECTED_INT_KIND(12)), WGT_ELIG_DIFF_PCT(i,2), tab_protect_summ_stats(i,1,1,1)
        ELSE
          WRITE (JSON_FILE, 1111, ADVANCE='no') TRIM(json_rowlab1(i)), TRIM(json_rowlab2(i)), &
            NINT(WGT_ELIG_VALUE(i,1), SELECTED_INT_KIND(12)), WGT_ELIG_PCT(i,1), &
            NINT(WGT_ELIG_VALUE(i,2), SELECTED_INT_KIND(12)), WGT_ELIG_PCT(i,2), &
            NINT(WGT_ELIG_DIFF(i,2), SELECTED_INT_KIND(12)), WGT_ELIG_DIFF_PCT(i,2)
        END IF
        if (i /= NROWS) WRITE (JSON_FILE, *) ',' ! not the last...
      end do

      WRITE (JSON_FILE, *) ! newline
      WRITE (JSON_FILE, *) '],' ! close table (array of arrays, not the last one)
    END IF

    ! Print JSON table (PASS=2 for participating units, K=(1,2) for (weighted,unweighted), I=row)
    WRITE (JSON_FILE, *) '"Table 5A": [' ! open table (array of arrays)

    DO i=1,NROWS
      IF (dostats(nth)) THEN
        ! stats is indexed by row, pass (1=eligible, 2=participant), weight (1=weighted, 2=unweighted), and plan number (skipping baselaw, so 1=first reform)
        WRITE (JSON_FILE, 1112, ADVANCE='no') TRIM(json_rowlab1(i)), TRIM(json_rowlab2(i)), &
          NINT(WGT_PART_VALUE(i,1), SELECTED_INT_KIND(12)), WGT_PART_PCT(i,1), &
          NINT(WGT_PART_VALUE(i,2), SELECTED_INT_KIND(12)), WGT_PART_PCT(i,2), &
          NINT(WGT_PART_DIFF(i,2), SELECTED_INT_KIND(12)), WGT_PART_DIFF_PCT(i,2), tab_protect_summ_stats(i,2,1,1)
      ELSE
        WRITE (JSON_FILE, 1111, ADVANCE='no') TRIM(json_rowlab1(i)), TRIM(json_rowlab2(i)), &
          NINT(WGT_PART_VALUE(i,1), SELECTED_INT_KIND(12)), WGT_PART_PCT(i,1), &
          NINT(WGT_PART_VALUE(i,2), SELECTED_INT_KIND(12)), WGT_PART_PCT(i,2), &
          NINT(WGT_PART_DIFF(i,2), SELECTED_INT_KIND(12)), WGT_PART_DIFF_PCT(i,2)
      END IF
      if (i /= NROWS) WRITE (JSON_FILE, *) ',' ! not the last...
    end do

    WRITE (JSON_FILE, *) ! newline
    WRITE (JSON_FILE, *) '],' ! close table (array of arrays, not the last one)

    IF (model_code /= "QCMM") THEN
      ! Print JSON table (PASS=1 for eligible units, K=(1,2) for (weighted,unweighted), I=row)
      WRITE (JSON_FILE, *) '"Table 5B": [' ! open table (array of arrays)

      DO i=1,NROWS
        WRITE (JSON_FILE, 1111, ADVANCE='no') TRIM(json_rowlab1(i)), TRIM(json_rowlab2(i)), &
          NINT(ELIG_VALUE(i,1), SELECTED_INT_KIND(12)), ELIG_PCT(i,1), &
          NINT(ELIG_VALUE(i,2), SELECTED_INT_KIND(12)), ELIG_PCT(i,2), &
          NINT(ELIG_DIFF(i,2), SELECTED_INT_KIND(12)), ELIG_DIFF_PCT(i,2)
        if (i /= NROWS) WRITE (JSON_FILE, *) ',' ! not the last...
      end do

      WRITE (JSON_FILE, *) ! newline
      WRITE (JSON_FILE, *) '],' ! close table (array of arrays, not the last one)
    END IF

    ! Print JSON table (PASS=2 for participating units, K=(1,2) for (weighted,unweighted), I=row)
    WRITE (JSON_FILE, *) '"Table 5C": [' ! open table (array of arrays)

    DO i=1,NROWS
      WRITE (JSON_FILE, 1111, ADVANCE='no') TRIM(json_rowlab1(i)), TRIM(json_rowlab2(i)), &
        NINT(PART_VALUE(i,1), SELECTED_INT_KIND(12)), PART_PCT(i,1), &
        NINT(PART_VALUE(i,2), SELECTED_INT_KIND(12)), PART_PCT(i,2), &
        NINT(PART_DIFF(i,2), SELECTED_INT_KIND(12)), PART_DIFF_PCT(i,2)
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

          if (istart == 1) then
             WRITE (TABFILE,1201) "Baselaw"
          else
             WRITE (TABFILE,1201) plannbr_table(j-1) !
          end if

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
             temp_stat = " "
             if (k == 1) temp_stat = tab_protect_summ_stats(I,PASS,1,J-1)

             IF (NBR_OF_KTHS == 1) THEN
               !!  BASELAW ONLY
               WRITE(TABFILE, 1215)           &
                 ROWLAB(II)                   &
                ,COMMA_FIELD (I,1,PASS,K)     &
                ,PCT_FIELD   (I,1,PASS,K)
             ELSE
               !!  BASELAW PLUS KTH REFORM
               WRITE(TABFILE, 1216)           &
                 ROWLAB(II)                   &
                ,COMMA_FIELD (I,1,PASS,K)     &
                ,PCT_FIELD   (I,1,PASS,K)     &
                ,COMMA_FIELD (I,J,PASS,K)     &
                ,PCT_FIELD   (I,J,PASS,K)     &
                ,COMMA_DIFF  (I,J,PASS,K)     &
                ,PCT_DIFF    (I,J,PASS,K)     &
                ,temp_stat
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
                WRITE (38, 3801)                &
                   "t_prot_summ "               &
                  ,j                            &
                  ,adjustl(plantemp2)            &
                  ,displ_wgt                    &
                  ,displ_pass                   &
                  ,rowlaba(ii)                   &
                  ,rowlabb(ii)                   &
                  ,rowlab(ii)                   &
                  ,XL_FIELD (I,1,PASS,K)     &
                  ,PCT_FIELD   (I,1,PASS,K)     &
                  , 0.0                         &
                  , 0.0                         &
                  , 0.0                         &
                  , 0.0                         &
                  , " "
              else
                WRITE (38, 3801)                &
                   "t_prot_summ "               &
                  ,j                            &
                  ,adjustl(plantemp2)            &
                  ,displ_wgt                    &
                  ,displ_pass                   &
                  ,rowlaba(ii)                   &
                  ,rowlabb(ii)                   &
                  ,rowlab(ii)                   &
                  ,XL_FIELD (I,1,PASS,K)     &
                  ,PCT_FIELD   (I,1,PASS,K)     &
                  ,XL_FIELD (I,J,PASS,K)     &
                  ,PCT_FIELD   (I,J,PASS,K)     &
                  ,XL_DIFF  (I,J,PASS,K)     &
                  ,PCT_DIFF    (I,J,PASS,K)     &
                  ,temp_stat
               end if
              end if

 201         CONTINUE
          END DO

 3801     format(1x,a12, 2x,i3, 2x,a40, 2x,a1, 2x,a4, 2x,a8, 2x,a24, 2x,a42, 3(2x,F14.0, 2x,f8.2), 2x,a1)

          WRITE (TABFILE,1210)
          if (k == 1) WRITE (TABFILE,1290)  !! show key for weighted tables only





       END DO ! end of pass loop

     END DO ! end of WEIGHTED/UNWEIGHTED
    END DO ! end of plan



    RETURN
!----------------------------------------------------------------------
! FORMAT STATEMENTS
!----------------------------------------------------------------------
1100 format( T37,'                              ' //                   &
             T37,'                PROTECTED CLASS IMPACTS' /           &
             T37,'            UNIVERSE: ELIGIBLE SNAP UNITS' //        &
             t24, a80)

1101 format( T37,'                              ' //                   &
             T37,'                PROTECTED CLASS IMPACTS' /           &
             T37,'           UNIVERSE: PARTICIPATING SNAP UNITS' //  &
             t24, a80)


1150 FORMAT(T5, "WEIGHTED")
1151 FORMAT(T5, "UNWEIGHTED")

1201 format(/ 1X, 131('-')                                       &
 ,//, t57,  'Baselaw', t80, 'Reform Plan ',a, t112, 'Difference'   &
 ,//, t2, 'Characteristic', t51,'Number',t62,'% of Total',t79,'Number',t90,'% of Total',t108,'Number',t119,'% Change' &
   )

1210 format(1X,131('-'))

1214 format (T2,A)

1215 format (T2, A, T45, A, T63, F8.2)
1216 format (T2, A, T45, A, T63, F8.2, T73, A, t91, F8.2, T101, A, t119, F8.2, A)

! JSON format of row, including 2 label lines
1111 format('["', A, '", "', A, '",', I12, ',', F8.2, ',', I12, ',', F8.2, ',', I12, ',', F8.2, ']') ! Unweighted and (rounded) weighted
1112 format('["', A, '","', A, '",', I12, ',', F8.2, ',', I12, ',', F8.2, ',', I12, ',', F8.2, ',"', A, '"]') ! Unweighted and (rounded) weighted with stats for diff

1290 format (&
  /,1x,'Statistics key:' &
 ,/,1x,'* Change is statistically different from zero at a 90% level of significance' &
 ,/,1x,'+ Change for subgroup is statistically different from the overall change at a 90% level of significance' &
 ,/,1x,'# Both conditions are met' &
  )

    end
