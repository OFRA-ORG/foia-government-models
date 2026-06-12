!**************************************************************************************************
! Source File:  FStab_gl_st.F90
! Called By:    FS_TABLES
!
! STATE-LEVEL Gainer/Loser Tables
!
!**************************************************************************************************

MODULE fstab_gl_st_mod
  INTERFACE fraction_or_na
    ! Interface used for overloaded function definition.
    ! Calling fraction_or_na will select one of the functions listed below
    ! depending on the supplied arguments.
    MODULE PROCEDURE fraction_or_na_real, fraction_or_na_int
  END INTERFACE fraction_or_na

  CONTAINS

  SUBROUTINE fraction_or_na_real(file_handle, numerator, denominator, scale)
    ! Print either a fraction or the text `"n.a."` to a file handle.
    ! Inputs:
    !   file_handle:  Open file handle where to print the result (integer).
    !   numerator:    Fraction numerator (real(8)).
    !   denominator:  Fraction denominator (real(8)).
    !   scale:        (Optional) Scale to multiply the resulting fraction--
    !                 useful for displaying percentages (real(8)).
    !
    ! Outputs:  none
    ! Note:  Always prints fractions with format (F10.2).
    IMPLICIT NONE
    REAL(8), INTENT(in)           :: numerator, denominator
    INTEGER, INTENT(in)           :: file_handle
    REAL(8), OPTIONAL, INTENT(in) :: scale
    REAL(8) :: true_scale
    true_scale = 1.
    IF (PRESENT(scale)) true_scale = scale
    IF (ABS(denominator) < ABS(EPSILON(denominator)) * 2.) THEN
      WRITE(file_handle, '(a)', ADVANCE='no') '"n.a."'
    ELSE
      WRITE(file_handle, '(F10.2)', ADVANCE='no') numerator * true_scale / denominator
    ENDIF
  END SUBROUTINE fraction_or_na_real

  SUBROUTINE fraction_or_na_int(file_handle, numerator, denominator, scale)
    ! Print either a fraction or the text `"n.a."` to a file handle.
    ! Inputs:
    !   file_handle:  Open file handle where to print the result (integer).
    !   numerator:    Fraction numerator (integer(8)).
    !   denominator:  Fraction denominator (integer(8)).
    !   scale:        (Optional) Scale to multiply the resulting fraction--
    !                 useful for displaying percentages (real(8)).
    !
    ! Outputs:  none
    ! Note:  Always prints fractions with format (F10.2).
    IMPLICIT NONE
    INTEGER(8), INTENT(in)        :: numerator, denominator
    INTEGER, INTENT(in)           :: file_handle
    REAL(8), OPTIONAL, INTENT(in) :: scale
    REAL(8) :: true_scale
    true_scale = 1.
    IF (PRESENT(scale)) true_scale = scale
    IF (denominator == 0) THEN
      WRITE(file_handle, '(a)', ADVANCE='no') '"n.a."'
    ELSE
      WRITE(file_handle, '(F10.2)', ADVANCE='no') REAL(numerator, 8) * true_scale / REAL(denominator, 8)
    ENDIF
  END SUBROUTINE fraction_or_na_int
END MODULE fstab_gl_st_mod

SUBROUTINE FS_TAB_state_gainer_loser (        &
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
     ,in_BASE_FSBEN     &
     ,in_FSBEN          &
     ,FSUSIZE           &   ! Food stamp unit size
     )

  USE GLOBAL
  use fstab_gl_st_mod, only: fraction_or_na
  USE USERPARM, ONLY : dostate
  USE States, only : nstates, state_name, state_order, state_idx, TOTSTATES, US_POS
  USE FSWORK, ONLY : fstate, PLANNBR_TABLE, PLANNAME_TABLE, create_table_extracts
  USE FSPARM, ONLY:  aprocsta, JSON_FILE
  USE FSSIZES, ONLY: MAX_NTH
  use Utils
  IMPLICIT NONE

  !---- Declare parameters from calling program
  INTEGER, INTENT(in)  :: kth, in_fsben, in_base_fsben, fsusize
  LOGICAL, INTENT(in)  :: BASE_HAS_EARN, BASE_HAS_ELDER, BASE_HAS_DIS, BASE_HAS_KIDS, BASE_HAS_NONCIT, BASE_HAS_ABAWD
  LOGICAL, INTENT(in)  :: partic, base_partic
  REAL, INTENT(in)     :: WGT

  !---- Variables for tables
  INTEGER :: NBR_OF_kths = 0
  INTEGER :: I, J, ITH, ist, stateidx, icat, IB1, IB2
  INTEGER :: k, i_start, i_end

  INTEGER, PARAMETER :: NWAFERS = 7
  INTEGER, PARAMETER :: Ncat = 9
  INTEGER, PARAMETER :: dp = selected_real_kind(15, 307) ! 15 digits, 1-^-307 to 10^307-1
  INTEGER, PARAMETER :: i8 = selected_int_kind(18)
  
  LOGICAL, DIMENSION(NWAFERS) :: IN_WAFER(NWAFERS)
  logical :: first_state = .TRUE.   
  INTEGER :: base_fsben, fsben, ben_dif

  CHARACTER(LEN=28), DIMENSION(0:Nstates) :: st_name

  CHARACTER (len=60), DIMENSION(nwafers) ::  subgroup_label = (/  &
       'ALL SNAP UNITS                                              ' &
       ,'SNAP UNITS WITH EARNINGS                                    ' &
       ,'SNAP UNITS WITH ELDERLY                                     ' &
       ,'SNAP UNITS WITH DISABLED                                    ' &
       ,'SNAP UNITS WITH KIDS                                        ' &
       ,'SNAP UNITS WITH NONCITIZENS                                 ' &
       ,'SNAP UNITS WITH NONELDERLY NONDISABLED CHILDLESS ADULTS     ' &
       /)

  CHARACTER(len=80) :: PLANTEMP
  CHARACTER(len=80) :: TYPETEMP

  INTEGER(8), ALLOCATABLE, SAVE :: tot_base_ben(:, :)
  REAL(8), ALLOCATABLE, SAVE :: wgt_tot_base_ben(:, :)
  INTEGER(8), ALLOCATABLE, SAVE :: tot_base_cat(:, :)
  REAL(8), ALLOCATABLE, SAVE :: wgt_tot_base_cat(:, :)
  INTEGER(8), ALLOCATABLE, SAVE :: tot_base_pers(:, :)
  REAL(8), ALLOCATABLE, SAVE :: wgt_tot_base_pers(:, :)

  INTEGER(8), ALLOCATABLE, SAVE :: nbr_cat(:, :, :, :)
  REAL(8), ALLOCATABLE, SAVE :: wgt_nbr_cat(:, :, :, :)

  INTEGER(8), ALLOCATABLE, SAVE :: nbr_pers(:, :, :, :)
  REAL(8), ALLOCATABLE, SAVE :: wgt_nbr_pers(:, :, :, :)

  INTEGER(8), ALLOCATABLE, SAVE :: delta_ben(:, :, :, :)
  REAL(8), ALLOCATABLE, SAVE :: wgt_delta_ben(:, :, :, :)
  
  INTEGER(8), ALLOCATABLE, SAVE :: total_ben(:, :, :, :)
  REAL(8), ALLOCATABLE, SAVE :: wgt_total_ben(:, :, :, :)

  REAL(8), ALLOCATABLE, SAVE :: avg_delta(:, :, :, :)
  REAL(8), ALLOCATABLE, SAVE :: wgt_avg_delta(:, :, :, :)

  REAL(8), ALLOCATABLE, SAVE :: pct_ben_chg(:, :, :, :)
  REAL(8), ALLOCATABLE, SAVE :: wgt_pct_ben_chg(:, :, :, :)

  !---- Comma display fields
  CHARACTER(len=8) tot_name

  CHARACTER(len=24), DIMENSION(ncat) :: gl_cat_label1 = (/ &
       '                        '   &   ! 1
       ,'                        '   &   ! 2
       ,'Still Eligible          '   &   ! 3
       ,'Still Participating     '   &   ! 4
       ,'                        '   &   ! 5
       ,'Newly Eligible and      '   &   ! 6
       ,'Still Eligible, Newly   '   &   ! 7
       ,'Still Participating     '   &   ! 8
       ,'                        '   &   ! 9
       /)

  CHARACTER(len=24), DIMENSION(ncat) :: gl_cat_label2 = (/ &
       'No Change               '   &   ! 1
       ,'No Longer Eligible      '   &   ! 2
       ,'Not Partic in Reform    '   &   ! 3
       ,'Lower Benefits          '   &   ! 4
       ,'Total Losers            '   &   ! 5
       ,'Participating in Reform '   &   ! 6
       ,'Participating in Reform '   &   ! 7
       ,'Higher Benefits         '   &   ! 8
       ,'Total Gainers           '   &   ! 9
       /)

  !---- Allocate arrays to avoid stack size limitations
  if (.NOT. ALLOCATED(tot_base_ben)) THEN
     ALLOCATE(tot_base_ben(0:totstates, nwafers), source=0_i8)
     ALLOCATE(wgt_tot_base_ben(0:totstates, nwafers), source=0.0_dp)
     ALLOCATE(tot_base_cat(0:totstates, nwafers), source=0_i8)
     ALLOCATE(wgt_tot_base_cat(0:totstates, nwafers), source=0.0_dp)
     ALLOCATE(tot_base_pers(0:totstates, nwafers), source=0_i8)
     ALLOCATE(wgt_tot_base_pers(0:totstates, nwafers), source=0.0_dp)
     
     ALLOCATE(nbr_cat(0:totstates, ncat, nwafers, max_nth), source=0_i8)
     ALLOCATE(wgt_nbr_cat(0:totstates, ncat, nwafers, max_nth), source=0.0_dp)
     
     ALLOCATE(nbr_pers(0:totstates, ncat, nwafers, max_nth), source=0_i8)
     ALLOCATE(wgt_nbr_pers(0:totstates, ncat, nwafers, max_nth), source=0.0_dp)
     
     ALLOCATE(delta_ben(0:totstates, ncat, nwafers, max_nth), source=0_i8)
     ALLOCATE(wgt_delta_ben(0:totstates, ncat, nwafers, max_nth), source=0.0_dp)
     
     ALLOCATE(total_ben(0:totstates, ncat, nwafers, max_nth), source=0_i8)
     ALLOCATE(wgt_total_ben(0:totstates, ncat, nwafers, max_nth), source=0.0_dp)
     
     ALLOCATE(avg_delta(0:totstates, ncat, nwafers, max_nth), source=0.0_dp)
     ALLOCATE(wgt_avg_delta(0:totstates, ncat, nwafers, max_nth), source=0.0_dp)
     
     ALLOCATE(pct_ben_chg(0:totstates, ncat, nwafers, max_nth), source=0.0_dp)
     ALLOCATE(wgt_pct_ben_chg(0:totstates, ncat, nwafers, max_nth), source=0.0_dp)
  ENDIF
  
  !---- By-pass calculations if print requested
  IF (KEOF == 3) GOTO 900

  !****************************************************************
  !     Perform table calculations
  !****************************************************************

  !---- Keep track of highest ith
  IF (kth> NBR_OF_kthS)  NBR_OF_kthS = kth


  BASE_FSBEN = in_base_fsben
  FSBEN = in_fsben

  IB1 = 0
  IB2 = 0
  IF (BASE_PARTIC) IB1 = BASE_FSBEN
  IF (PARTIC) IB2 = FSBEN
  BEN_DIF = IB2 - IB1


  !! category translation:
  IF (.NOT. BASE_PARTIC .AND. .NOT. PARTIC) RETURN

  IF (.NOT. BASE_PARTIC) THEN              !! Baselaw: N,  Reform: Y
     IF (BASE_FSBEN <= 0) THEN
        ICAT = 6                            !! Ineligible for baselaw
     ELSE
        ICAT = 7                            !! Eligible for baselaw
     ENDIF
  ELSE IF (.NOT. PARTIC) THEN             !! Baselaw: Y,  Reform: N
     IF (FSBEN <= 0) THEN
        ICAT = 2                            !! Ineligible for reform
     ELSE
        ICAT = 3                            !! Eligible for reform
     ENDIF
  ELSE                                    !! Baselaw: Y,  Reform Y
     SELECT CASE (BEN_DIF)
     CASE (:-1)
        ICAT = 4                          !! Loss of $51 or more
     CASE (0)
        ICAT = 1                          !! No change
     CASE (1:)
        ICAT = 8                          !! Gain of $51+
     END SELECT
  ENDIF

  !! Calculate actual benefits
  IF (BASE_PARTIC) THEN
     BASE_FSBEN = in_base_fsben
  ELSE
     base_fsben = 0
  ENDIF

  IF (PARTIC) THEN
     FSBEN = in_fsben
  ELSE
     fsben = 0
  ENDIF


  !!  subgroup assignment:
  DO I = 1, NWAFERS
     IN_WAFER(I) = .FALSE.
  ENDDO

  !---- All units
  IN_WAFER(1) = .TRUE.

  !---- Units with earnings
  IF (BASE_HAS_EARN)    IN_WAFER(2) = .TRUE.

  !---- Units with elderly
  IF (BASE_HAS_ELDER)   IN_WAFER(3) = .TRUE.

  !---- Units with disabled
  IF (BASE_HAS_DIS)     IN_WAFER(4) = .TRUE.

  !---- Units with kids
  IF (BASE_HAS_KIDS)    IN_WAFER(5) = .TRUE.

  !---- Units with NONCITIZENS
  IF (BASE_HAS_NONCIT)    IN_WAFER(6) = .TRUE.

  !---- Units with ABAWDs
  IF (BASE_HAS_ABAWD)    IN_WAFER(7) = .TRUE.


  !-- state row:
  ist = state_idx(fstate)

  ! set ist differently for MATHSIPP models
  IF (model_code == "MSIP" .AND. dostate > 1) ist = kist


  ! Sum values for each protected class type
  DO j = 1, nwafers
     IF (.NOT. in_wafer(j)) CYCLE
     IF (dostate == 3 .AND. .NOT. aprocsta(ist, kth-1)) CYCLE ! don't look at skipped states

     !!  units
     nbr_cat(ist, icat, j, kth) =     nbr_cat(ist, icat, j, kth) + 1
     wgt_nbr_cat(ist, icat, j, kth) = wgt_nbr_cat(ist, icat, j, kth) + wgt

     nbr_cat(US_POS, icat, j, kth) =     nbr_cat(US_POS, icat, j, kth) + 1
     wgt_nbr_cat(US_POS, icat, j, kth) = wgt_nbr_cat(US_POS, icat, j, kth) + wgt

     !!  persons
     nbr_pers(ist, icat, j, kth) =     nbr_pers(ist, icat, j, kth) + fsusize
     wgt_nbr_pers(ist, icat, j, kth) = wgt_nbr_pers(ist, icat, j, kth) + fsusize * wgt

     nbr_pers(US_POS, icat, j, kth) =     nbr_pers(US_POS, icat, j, kth) + fsusize
     wgt_nbr_pers(US_POS, icat, j, kth) = wgt_nbr_pers(US_POS, icat, j, kth) + fsusize * wgt

     !!  benefit (unit-level)
     delta_ben(ist, icat, j, kth) =     delta_ben(ist, icat, j, kth) + ben_dif
     wgt_delta_ben(ist, icat, j, kth) = wgt_delta_ben(ist, icat, j, kth) + ben_dif * wgt

     delta_ben(US_POS, icat, j, kth) =     delta_ben(US_POS, icat, j, kth) + ben_dif
     wgt_delta_ben(US_POS, icat, j, kth) = wgt_delta_ben(US_POS, icat, j, kth) + ben_dif * wgt
     
     total_ben(ist, icat, j, kth) =     total_ben(ist, icat, j, kth) + IB2 ! IB2 is fsben corrected for eligibility
     wgt_total_ben(ist, icat, j, kth) = wgt_total_ben(ist, icat, j, kth) + IB2 * wgt

     total_ben(US_POS, icat, j, kth) =     total_ben(US_POS, icat, j, kth) + IB2
     wgt_total_ben(US_POS, icat, j, kth) = wgt_total_ben(US_POS, icat, j, kth) + IB2 * wgt

     !!  baselaw benefits (only count once, kth=2 is the first cal here)
     IF (kth == 2 .AND. BASE_PARTIC) THEN
        tot_base_ben(ist, j) =     tot_base_ben(ist, j) + IB1 ! IB1 is base_fsben corrected for eligibility
        wgt_tot_base_ben(ist, j) = wgt_tot_base_ben(ist, j) + IB1 * wgt

        tot_base_ben(US_POS, j) =     tot_base_ben(US_POS, j) + IB1
        wgt_tot_base_ben(US_POS, j) = wgt_tot_base_ben(US_POS, j) + IB1 * wgt

        tot_base_cat(ist, j) =     tot_base_cat(ist, j) + 1
        wgt_tot_base_cat(ist, j) = wgt_tot_base_cat(ist, j) + wgt

        tot_base_cat(US_POS, j) =     tot_base_cat(US_POS, j) + 1
        wgt_tot_base_cat(US_POS, j) = wgt_tot_base_cat(US_POS, j) + wgt

        tot_base_pers(ist, j) =     tot_base_pers(ist, j) + fsusize
        wgt_tot_base_pers(ist, j) = wgt_tot_base_pers(ist, j) + fsusize * wgt

        tot_base_pers(US_POS, j) =     tot_base_pers(US_POS, j) + fsusize
        wgt_tot_base_pers(US_POS, j) = wgt_tot_base_pers(US_POS, j) + fsusize * wgt

     END IF

  END DO

  RETURN


900 CONTINUE   ! Print

  IF (dostate == 3) THEN
     tot_name = "Total   "
  ELSE
     tot_name = "U.S.    "
  END IF

  ! Loop through states
  DO ist = 0, nstates

     IF (ist == US_POS .AND. dostate == 3)  THEN
        st_name(ist) = "Sum of States"
     ELSE
        st_name(ist) = state_name(ist)
     END IF

     !! create g/l aggregate totals (category type 5 for losers, 9 for gainers)
     DO j = 1, nwafers
        DO ith = 2, nbr_of_kths

           !! losers:
           nbr_cat(ist, 5, j, ith) = SUM(nbr_cat(ist, 2:4, j, ith))
           wgt_nbr_cat(ist, 5, j, ith) = SUM(wgt_nbr_cat(ist, 2:4, j, ith))
           delta_ben(ist, 5, j, ith) = SUM(delta_ben(ist, 2:4, j, ith))
           wgt_delta_ben(ist, 5, j, ith) = SUM(wgt_delta_ben(ist, 2:4, j, ith))
           total_ben(ist, 5, j, ith) = SUM(total_ben(ist, 2:4, j, ith))
           wgt_total_ben(ist, 5, j, ith) = SUM(wgt_total_ben(ist, 2:4, j, ith))
           nbr_pers(ist, 5, j, ith) = SUM(nbr_pers(ist, 2:4, j, ith))
           wgt_nbr_pers(ist, 5, j, ith) = SUM(wgt_nbr_pers(ist, 2:4, j, ith))

           !! gainers:
           nbr_cat(ist, 9, j, ith) = SUM(nbr_cat(ist, 6:8, j, ith))
           wgt_nbr_cat(ist, 9, j, ith) = SUM(wgt_nbr_cat(ist, 6:8, j, ith))
           delta_ben(ist, 9, j, ith) = SUM(delta_ben(ist, 6:8, j, ith))
           wgt_delta_ben(ist, 9, j, ith) = SUM(wgt_delta_ben(ist, 6:8, j, ith))
           total_ben(ist, 9, j, ith) = SUM(total_ben(ist, 6:8, j, ith))
           wgt_total_ben(ist, 9, j, ith) = SUM(wgt_total_ben(ist, 6:8, j, ith))
           nbr_pers(ist, 9, j, ith) = SUM(nbr_pers(ist, 6:8, j, ith))
           wgt_nbr_pers(ist, 9, j, ith) = SUM(wgt_nbr_pers(ist, 6:8, j, ith))

        END DO

     END DO

     DO i = 1, nCAT
        DO j = 1, nwafers
           DO ith= 2, nbr_of_kths

              !! avg ben per unit by cat
              IF (nbr_cat(ist, i, j,ith) /= 0) &
                   avg_delta(ist, i, j,ith) = REAL(delta_ben(ist, i, j,ith),8)  / nbr_cat(ist, i, j, ith)

              IF (abs(wgt_nbr_cat(ist, i, j, ith)) > 0.0) &
                   wgt_avg_delta(ist, i, j, ith) = wgt_delta_ben(ist, i, j, ith)  / wgt_nbr_cat(ist, i, j, ith)

              IF (tot_base_ben (ist, j) /= 0)  &
                   pct_ben_chg (ist, i, j, ith) =  REAL(delta_ben(ist, i, j, ith),8) / tot_base_ben (ist, j) * 100.0

              IF (abs(wgt_tot_base_ben(ist, j)) > 0.0)  &
                   wgt_pct_ben_chg (ist, i, j, ith) =  wgt_delta_ben(ist, i, j, ith) / wgt_tot_base_ben (ist, j) * 100.0


           END DO !! iths
        END DO   !! subgr
     END DO     !! g/l cat
  END DO      !! states


DO j=1, nwafers
     ! Table 9: weighted
     IF (j == 1) THEN
        WRITE (JSON_FILE, *) '"Table 9": ['
     ELSE
        WRITE (JSON_FILE, *) '"Table 9', CHAR(j + 63), '": ['
     END IF

     first_state = .TRUE.
     DO stateidx = 0, Nstates
        ist = state_order(stateidx)
        ! State-level tables are nonsensical for national-level models, so skip if that is the case.
        IF (ist /= US_POS) THEN
            IF ((model_code == "MSIP" .OR. model_code == "MCPS") .AND. dostate == 1) CYCLE
            IF (dostate == 3 .AND. .NOT. aprocsta(ist, 1)) CYCLE
        END IF

        IF (.NOT. first_state) THEN
          WRITE (JSON_FILE, *) "," ! ready for next row
        ELSE
          first_state = .FALSE.
        END IF

        ! Note: all numbers are weighted
        WRITE (JSON_FILE, '("[""",A,""",")', ADVANCE='no') TRIM(st_name(ist)) ! name of the state
        WRITE (JSON_FILE, '(I12,",")', ADVANCE='no') NINT(wgt_tot_base_cat(ist, j), SELECTED_INT_KIND(12)) ! no change units (baselaw)
        DO i=1,9
          ! In order:
          ! 1: no change units pct of baselaw
          ! 2-5: loser units (4 types) pct of baselaw
          ! 6-9: gainer units (4 types) pct of baselaw
          CALL fraction_or_na(JSON_FILE, wgt_nbr_cat(ist, i, j, 2), wgt_tot_base_cat(ist, j), 100.d0)
          WRITE (JSON_FILE, '(A)', ADVANCE='no') ','
        END DO
        WRITE (JSON_FILE, '(I12,",")', ADVANCE='no') NINT(wgt_tot_base_pers(ist, j), SELECTED_INT_KIND(12)) ! no change persons (baselaw)
        DO i=1,9
          ! In order:
          ! 1: no change persons pct of baselaw
          ! 2-5: loser persons (4 types) pct of baselaw
          ! 6-9: gainer persons (4 types) pct of baselaw
          CALL fraction_or_na(JSON_FILE, wgt_nbr_pers(ist, i, j, 2), wgt_tot_base_pers(ist, j), 100.d0)
          WRITE (JSON_FILE, '(A)', ADVANCE='no') ','
        END DO
        WRITE (JSON_FILE, '(I12,",")', ADVANCE='no') NINT(wgt_tot_base_ben(ist, j), SELECTED_INT_KIND(12)) ! no change benefits (baselaw)
        DO i=1,9
          ! In order:
          ! 1: no change benefits pct of baselaw
          ! 2-5: loser benefits (4 types) pct of baselaw
          ! 6-9: gainer benefits (4 types) pct of baselaw
          CALL fraction_or_na(JSON_FILE, ABS(wgt_delta_ben(ist, i, j, 2)), wgt_tot_base_ben(ist, j), 100.d0)
          WRITE (JSON_FILE, '(A)', ADVANCE='no') ','
        END DO
        CALL fraction_or_na(JSON_FILE, wgt_tot_base_ben(ist, j), wgt_tot_base_cat(ist, j)) ! no change benefits per unit (baselaw)
        WRITE (JSON_FILE, '(A)', ADVANCE='no') ','
        CALL fraction_or_na(JSON_FILE, wgt_total_ben(ist, 1, j, 2), wgt_nbr_cat(ist, 1, j, 2)) ! Average benefit for units with no change in benefits
        WRITE (JSON_FILE, '(A)', ADVANCE='no') ','
        DO i=2,9 ! 4x losers, 4x gainers delta change per unit, pct. change in benefits per unit
          CALL fraction_or_na(JSON_FILE, ABS(wgt_delta_ben(ist, i, j, 2)), wgt_nbr_cat(ist, i, j, 2))
          IF (i == 9) THEN
            WRITE(JSON_FILE, '(A)', ADVANCE='no') ']'
          ELSE
            WRITE(JSON_FILE, '(A)', ADVANCE='no') ','
          END IF
        END DO

     END DO

     WRITE (JSON_FILE, *) ! newline
     WRITE (JSON_FILE, *) '],' ! Still not the last table...
     first_state = .TRUE.

     ! Table 10: unweighted
     IF (j == 1) THEN
        WRITE (JSON_FILE, *) '"Table 10": ['
     ELSE
        WRITE (JSON_FILE, *) '"Table 10', CHAR(j + 63), '": ['
     END IF

     DO stateidx = 0, Nstates
        ist = state_order(stateidx)
        ! State-level tables are nonsensical for national-level models, so skip if that is the case.
        IF (ist /= US_POS) THEN
            IF ((model_code == "MSIP" .OR. model_code == "MCPS") .AND. dostate == 1) CYCLE
            IF (dostate == 3 .AND. .NOT. aprocsta(ist, 1)) CYCLE
        END IF

        IF (.NOT. first_state) THEN
          WRITE (JSON_FILE, *) "," ! ready for next row
        ELSE
          first_state = .FALSE.
        END IF

        ! Note: all numbers are unweighted
        WRITE (JSON_FILE, '("[""",A,""",")', ADVANCE='no') TRIM(st_name(ist)) ! name of the state
        WRITE (JSON_FILE, '(I12,",")', ADVANCE='no') tot_base_cat(ist, j) ! no change units (baselaw)
        DO i=1,9
          ! In order:
          ! 1: no change units pct of baselaw
          ! 2-5: loser units (4 types) pct of baselaw
          ! 6-9: gainer units (4 types) pct of baselaw
          CALL fraction_or_na(JSON_FILE, nbr_cat(ist, i, j, 2), tot_base_cat(ist, j), 100.d0)
          WRITE (JSON_FILE, '(A)', ADVANCE='no') ','
        END DO
        WRITE (JSON_FILE, '(I12,",")', ADVANCE='no') tot_base_pers(ist, j) ! no change persons (baselaw)
        DO i=1,9
          ! In order:
          ! 1: no change persons pct of baselaw
          ! 2-5: loser persons (4 types) pct of baselaw
          ! 6-9: gainer persons (4 types) pct of baselaw
          CALL fraction_or_na(JSON_FILE, nbr_pers(ist, i, j, 2), tot_base_pers(ist, j), 100.d0)
          WRITE (JSON_FILE, '(A)', ADVANCE='no') ','
        END DO
        WRITE (JSON_FILE, '(I12,",")', ADVANCE='no') tot_base_ben(ist, j) ! no change benefits (baselaw)
        DO i=1,9
          ! In order:
          ! 1: no change benefits pct of baselaw
          ! 2-5: loser benefits (4 types) pct of baselaw
          ! 6-9: gainer benefits (4 types) pct of baselaw
          CALL fraction_or_na(JSON_FILE, ABS(delta_ben(ist, i, j, 2)), tot_base_ben(ist, j), 100.d0)
          WRITE (JSON_FILE, '(A)', ADVANCE='no') ','
        END DO
        CALL fraction_or_na(JSON_FILE, tot_base_ben(ist, j), tot_base_cat(ist, j)) ! no change benefits per unit (baselaw)
        WRITE (JSON_FILE, '(A)', ADVANCE='no') ','
        CALL fraction_or_na(JSON_FILE, total_ben(ist, 1, j, 2), nbr_cat(ist, 1, j, 2)) ! Average benefit for units with no change in benefits
        WRITE (JSON_FILE, '(A)', ADVANCE='no') ','
        DO i=2,9 ! 4x losers, 4x gainers delta change per unit, pct. change in benefits per unit
          CALL fraction_or_na(JSON_FILE, ABS(delta_ben(ist, i, j, 2)), nbr_cat(ist, i, j, 2))
          IF (i == 9) THEN
            WRITE(JSON_FILE, '(A)', ADVANCE='no') ']'
          ELSE
            WRITE(JSON_FILE, '(A)', ADVANCE='no') ','
          END IF
        END DO

     END DO

     WRITE (JSON_FILE, *) ! newline
     WRITE (JSON_FILE, *) '],' ! Still not the last table...

  END DO


  !------------------------------------------------
  ! Print weighted tables
  !------------------------------------------------
  DO ith = 2, nbr_of_kths
     DO j = 1, nwafers

        DO k = 1, 3

           i_start = (k-1)*3 + 1
           i_end = i_start + 2

           CALL ISNEWPG(TABFILE, PAGE_BREAK_NUMLINES + 1)

           PLANTEMP = 'PLAN ' //PLANNBR_TABLE(ith-1) // ': '//PLANNAME_TABLE(ith-1)
           TYPETEMP = 'UNIVERSE: ' // subgroup_label(j)

           WRITE(TABFILE,3000) TYPETEMP , PLANTEMP, k, "Weighted" &
                ,(gl_cat_label1(i), i = i_start, i_end) &
                ,(gl_cat_label2(i), i = i_start, i_end)
           
           DO stateidx = 0, nstates 
              ist = state_order(stateidx)
              IF (dostate == 3 .AND. .NOT. aprocsta(ist, ith-1)) CYCLE

              WRITE(tabfile, 3020) &
                   st_name (ist)    &
                   ,(wgt_nbr_cat(ist, i, j, ith), wgt_avg_delta (ist, i, j, ith), wgt_pct_ben_chg(ist, i, j, ith) &
                   ,i = i_start, i_end) !!, nbr_units(ist)
           END DO

        END DO
     END DO
  END DO


  !------------------------------------------------
  ! Print unweighted tables
  !------------------------------------------------
  DO ith = 2, nbr_of_kths
     DO j = 1, nwafers

        DO k = 1, 3

           i_start = (k-1)*3 + 1
           i_end = i_start + 2

           CALL ISNEWPG(TABFILE, PAGE_BREAK_NUMLINES + 1)

           PLANTEMP = 'PLAN ' //PLANNBR_TABLE(ith-1) // ': '//PLANNAME_TABLE(ith-1)
           TYPETEMP = 'UNIVERSE: ' // subgroup_label(j)


           WRITE(TABFILE,3000) TYPETEMP , PLANTEMP, k, "Unweighted" &
                ,(gl_cat_label1(i), i = i_start, i_end) &
                ,(gl_cat_label2(i), i = i_start, i_end)
           
           do stateidx = 0, nstates
              ist = state_order(stateidx)
              IF (dostate == 3 .AND. .NOT. aprocsta(ist, ith-1)) CYCLE

              WRITE(tabfile, 3010) &
                   st_name (ist)    &
                   ,(nbr_cat(ist, i, j, ith), avg_delta (ist, i, j, ith), pct_ben_chg(ist, i, j, ith) &
                   ,i = i_start, i_end) !!, nbr_units(ist)
           END DO

        END DO
     END DO
  END DO


3000 FORMAT (//, t2, "GAINER/LOSER TABLE BY STATE" &
       //,t2, a  &  !! universe
       /,t2, a  &  !! plan
       /,t2, "Page ", i1, " of 3" &  !! page
       //,t2, a  &  !! wgt/unwgt
       ,/,t2, 130("-")    &
       ,/,t29, 3(2x,a24,12x)  &
       ,/,t29, 3(2x,a24,12x)  &
       ,/,t2, 130("-")    &
       ,/,t2,"     ",t18, 3(11x,"   ", 3x,"Avg      ",3x,"Pct      ") &
       ,/ ,t2,"State",t18, 3(11x,"Nbr", 3x,"Ben Chg  ",3x,"Ben Chg  ") &
       ,/ ,t2,"-----",t18, 3(11x,"---", 3x,"---------",3x,"---------") &
       )



3010 FORMAT(t2, a, t18, 3(4x,i10,   2x,f10.2, 2x,f10.3))
3020 FORMAT(t2, a, t18, 3(4x,f10.0, 2x,f10.2, 2x,f10.3))




  !--- extract weighted
  IF (create_table_extracts) THEN
     DO ith = 2, nbr_of_kths
        DO j = 1, nwafers
           do stateidx = 0, nstates
              ist = state_order(stateidx)
              DO i = 1, ncat
                 WRITE (41, 4101)                    &
                      "t_gl_st     "                   &
                      ,"W"                             &
                      ,ith                             &
                      ,ADJUSTL(PLANNAME_TABLE(ITH-1)(1:40))  &
                      ,j                               &
                      ,ist                             &
                      ,i                               &
                      ,wgt_nbr_cat(ist, i, j, ith)     &
                      ,wgt_avg_delta (ist, i, j, ith)  &
                      ,wgt_pct_ben_chg(ist, i, j, ith) &
                      ,wgt_nbr_pers(ist, i, j, ith)    &
                      ,wgt_total_ben(ist, i, j, ith)   &
                      ,wgt_delta_ben(ist, i, j, ith)   &
                      ,wgt_tot_base_cat(ist, j)        &
                      ,wgt_tot_base_pers(ist, j)       &
                      ,wgt_tot_base_ben(ist, j)

              END DO   !! g/l cat
           END DO      !! state
        END DO         !! subgroup
     END DO            !! plan
  END IF

  !--- extract unweighted
  IF (create_table_extracts) THEN
     DO ith = 2, nbr_of_kths
        DO j = 1, nwafers
            do stateidx = 0, nstates
              ist = state_order(stateidx)
              DO i = 1, ncat
                 WRITE (41, 4102)                    &
                      "t_gl_st     "                   &
                      ,"U"                             &
                      ,ith                             &
                      ,ADJUSTL(PLANNAME_TABLE(ITH-1)(1:40) )  &
                      ,j                               &
                      ,ist                             &
                      ,i                               &
                      ,nbr_cat(ist, i, j, ith)         &
                      ,avg_delta (ist, i, j, ith)      &
                      ,pct_ben_chg(ist, i, j, ith)     &
                      ,nbr_pers(ist, i, j, ith)        &
                      ,total_ben(ist, i, j, ith)       &
                      ,delta_ben(ist, i, j, ith)       &
                      ,tot_base_cat(ist, j)            &
                      ,tot_base_pers(ist, j)           &
                      ,tot_base_ben(ist, j)

              END DO   !! g/l cat
           END DO      !! state
        END DO         !! subgroup
     END DO            !! plan
  END IF

4101 FORMAT(a12, 2x,a1, 2x,i3, 2x,a40, 3(2x,i3), 2x,f10.0, 2x,f10.2, 2x,f10.3, 6(2x,f20.3)) !! weighted extract
4102 FORMAT(a12, 2x,a1, 2x,i3, 2x,a40, 3(2x,i3), 2x,i10,   2x,f10.2, 2x,f10.3, 6(2x,i20)) !! unweighted extract

  RETURN
END SUBROUTINE FS_TAB_state_gainer_loser
