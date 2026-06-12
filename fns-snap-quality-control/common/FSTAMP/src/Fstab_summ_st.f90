!**************************************************************************************************
! Source File:  FSsumm.F90                  
! Called By:    FS_TABLES                   
!
! TABLE 1 - Summary Comparisons of Impacts on Food Stamp Program
!
! Produces an estimate of the standard sampling error of the percent change
! estimates.  It uses a drop-one-record jackknife estimate.
!
!**************************************************************************************************
SUBROUTINE fs_tab_summary_state()
  USE global
  USE userparm
  USE states, ONLY : nstates, state_name, state_order, state_idx, US_POS
  USE fssizes
  USE fswork
  USE fslocs
  USE fsparm
  use Utils

  IMPLICIT NONE

  INTEGER, PARAMETER :: num_summ_pcts = 5  ! number of pct change estimates in Table 1

  INTEGER :: i, j, num_plans = 0
  INTEGER :: diff  (num_summ_pcts)
  INTEGER ::  idx

  REAL(8):: x
  REAL(8), DIMENSION(num_summ_pcts) :: pct
  REAL(8) :: wtotal(num_summ_pcts, 0:max_nth) = 0.0
  REAL(8) :: utotal(num_summ_pcts, 0:max_nth) = 0.0


  !-- for state estimates:
  INTEGER :: isx, stateidx

  REAL(8), allocatable, save, DIMENSION (:, :, :) :: total_s
  INTEGER, allocatable, save, DIMENSION (:, :, :) :: utotal_s

  CHARACTER (131) :: title_line (max_nth)
  CHARACTER (20) :: rowlab
  CHARACTER (14)     :: temp1 (num_summ_pcts)

  CHARACTER(len=07) :: archive_num
  CHARACTER(len=40) :: extract_label
  CHARACTER(len=20) :: state_label
  
  logical :: first_call = .TRUE.

  IF (keof==2) GOTO 200
  IF (keof==3) GOTO 300

  !------------------------------------------------------------------------------
  !------------------------------------------------------------------------------

  num_plans = num_plans + 1
  if (first_call) then
     allocate(total_s(num_summ_pcts, 0:max_nth, 0:nstates))
     allocate(utotal_s(num_summ_pcts, 0:max_nth, 0:nstates))
     first_call = .FALSE.
  end if

  total_s = 0.0
  utotal_s = 0

  RETURN

  !------------------------------------------------------------------------------
200 CONTINUE     !---- phase 2 processing -------------------------------------
  !------------------------------------------------------------------------------

  !--------------------------------------------------------------------------
  !------- count up esimates for baselaw plan--------------------------------
  IF (nth == 1) THEN

     DO kist = start_kist, end_kist

        IF (model_code == "MSIP" .OR. model_code == "MCPS") THEN
           j = kist
           !!  select baselaw data to tabulate
           SELECT CASE (dostate)
              !!  only tab national results
           CASE (1)
              CYCLE
              !!  only tab 51 simstates
           CASE (2)
              IF (kist == US_POS) CYCLE
              !!  only tab SELECTED simstates
           CASE default
              IF (kist == US_POS .OR. .NOT. aprocsta(kist, nth)) CYCLE
           END SELECT

           wtotal   (:,0)     = wtotal  (:,0)      + s_baselaw_est(:, j) * wgt1(j)
           total_s (:,0,j)    = total_s (:,0,j)    + s_baselaw_est(:, j) * wgt1(j)

           utotal  (:,0)      = utotal  (:,0)      + REAL(s_baselaw_est(:, j))
           utotal_s(:,0,j)    = utotal_s(:,0,j)    +      s_baselaw_est(:, j)

        END IF

        IF (model_code == "QCMM") THEN
           j = state_idx(fstate)


           DO i = 1, num_summ_pcts

              wtotal   (i,0)     = wtotal  (i,0)      + s_baselaw_est(i, 1) * wgt1(1)
              total_s (i,0,j)    = total_s (i,0,j)    + s_baselaw_est(i, 1) * wgt1(1)

              utotal  (i,0)      = utotal  (i,0)      + REAL(s_baselaw_est(i, 1))
              utotal_s(i,0,j)    = utotal_s(i,0,j)    +      s_baselaw_est(i, 1)

           END DO

        END IF

     END DO
  ENDIF


  !----------------------------------------------------------------
  !-------- count up estimates for NTH plan -----------------------
  idx = nth

  DO kist = start_kist, end_kist
     j = kist
     IF (model_code == "MSIP" .OR. model_code == "MCPS") THEN
        !!  select reform data to tabulate
        SELECT CASE (dostate)
           !!  only tab national results
        CASE (1)
           CYCLE
           !!  only tab 51 simstates
        CASE (2)
           IF (kist == US_POS) CYCLE
           !!  only tab SELECTED simstates
        CASE default
           IF (kist == US_POS .OR. .NOT. aprocsta(kist, nth)) CYCLE
        END SELECT

        wtotal  (:,idx)      = wtotal  (:,idx)      + s_reform_est(:, j) * wgt1(j)
        total_s (:,idx,j)    = total_s (:,idx,j)    + s_reform_est(:, j) * wgt1(j)

        utotal(:,idx)        = utotal (:,idx)       + REAL(s_reform_est(:, j))
        utotal_s(:,idx,j)    = utotal_s(:,idx,j)    +      s_reform_est(:, j)

     END IF

     IF (model_code == "QCMM") THEN
        j = state_idx(fstate)

        wtotal  (:,idx)      = wtotal  (:,idx)      + s_reform_est(:, 1) * wgt1(1)
        total_s (:,idx,j)    = total_s (:,idx,j)    + s_reform_est(:, 1) * wgt1(1)

        utotal(:,idx)        = utotal (:,idx)       + REAL(s_reform_est(:, 1))
        utotal_s(:,idx,j)    = utotal_s(:,idx,j)    +      s_reform_est(:, 1)

     END IF

  END DO

  RETURN
  !------------------------------------------------------------------------------
300 CONTINUE     !---- phase 3 processing -------------------------------------
  !------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------
  !-------------- Print the Table (Weighted) ---------------------------------------------

  DO i = 1, num_plans

     CALL isnewpg(tabfile, page_break_numlines + 1 )         ! +1 lets ISNEWPG know
     ! lines are coming - but
     WRITE (tabfile,1100) ! title                            ! not exactly how many.

     title_line(i) = "Plan " // plannbr(i) // ": " // planname(i)
     CALL center_text(title_line(i), 131)

     WRITE (tabfile,1132) title_line(i)
     WRITE (tabfile,1105) ! column headings

     rowlab = 'Baselaw   '
     DO j = 1, num_summ_pcts
        temp1(j) = COMMA8(wtotal(j,0), 14)
     ENDDO
     WRITE (tabfile,1110) rowlab, (temp1(j), j=1,num_summ_pcts)  !! baselaw line

     !! write 5 baselaw estimates to extract
     IF (create_table_extracts) THEN

        extract_label = 'Baselaw'
        state_label = 'Sum of states'
        WRITE(32, 3201) 't_summ_st   ', 1, ADJUSTL(extract_label), ADJUSTL(state_label),'W', 'Elig', 'Unit', wtotal(1,0), 0.0
        WRITE(32, 3201) 't_summ_st   ', 1, ADJUSTL(extract_label), ADJUSTL(state_label),'W', 'Elig', 'Pers', wtotal(2,0), 0.0
        WRITE(32, 3201) 't_summ_st   ', 1, ADJUSTL(extract_label), ADJUSTL(state_label),'W', 'Part', 'Unit', wtotal(3,0), 0.0
        WRITE(32, 3201) 't_summ_st   ', 1, ADJUSTL(extract_label), ADJUSTL(state_label),'W', 'Part', 'Pers', wtotal(4,0), 0.0
        WRITE(32, 3201) 't_summ_st   ', 1, ADJUSTL(extract_label), ADJUSTL(state_label),'W', 'Part', 'Ben ', wtotal(5,0), 0.0
     END IF

3201 FORMAT (1x,a12, i3, 1x,a40, 1x,a16, 1x,a1, 1x,a4, 1x,a6, f20.0, 1x, f20.4)






     ! convert totals to alpha
     DO j = 1, num_summ_pcts
        temp1(j) = COMMA8(wtotal(j,i), 14)
     ENDDO

     rowlab = 'Plan ' // plannbr(i)

     ! compute national percent changes:
     DO j = 1, num_summ_pcts
        IF (abs(wtotal(j,0)) > 0.0) THEN
           pct(j) = 100.0 *  wtotal(j,i) / wtotal(j,0) - 100.0
        ELSE
           pct(j) = 0.0
        ENDIF


     ENDDO

     ! write line to table
     WRITE (tabfile,1120) rowlab, (temp1(j), pct(j), j=1,num_summ_pcts)

     ! Begin writing table as a named array of arrays to JSON
     WRITE (JSON_FILE, *) '"Table 3": ['
     WRITE (JSON_FILE, 1111, ADVANCE="no") "Sum of states",  &
                            (NINT(wtotal(j,i), SELECTED_INT_KIND(12)), pct(j), j=1, num_summ_pcts) 

     !! write 5 reform estimates to extract
     IF (create_table_extracts) THEN
        archive_num = planname(i)(1:7)
        extract_label = planname(i)(:40)
        state_label = 'Sum of states'
        WRITE(32, 3201) 't_summ_st   ', i+1, ADJUSTL(extract_label), ADJUSTL(state_label), 'W', 'Elig', 'Unit', &
                        wtotal(1,i), pct(1)
        WRITE(32, 3201) 't_summ_st   ', i+1, ADJUSTL(extract_label), ADJUSTL(state_label), 'W', 'Elig', 'Pers', &
                        wtotal(2,i), pct(2)
        WRITE(32, 3201) 't_summ_st   ', i+1, ADJUSTL(extract_label), ADJUSTL(state_label), 'W', 'Part', 'Unit', &
                        wtotal(3,i), pct(3)
        WRITE(32, 3201) 't_summ_st   ', i+1, ADJUSTL(extract_label), ADJUSTL(state_label), 'W', 'Part', 'Pers', &
                        wtotal(4,i), pct(4)
        WRITE(32, 3201) 't_summ_st   ', i+1, ADJUSTL(extract_label), ADJUSTL(state_label), 'W', 'Part', 'Ben ', &
                        wtotal(5,i), pct(5)
     END IF



     DO stateidx = 0, nstates ! include US

        isx = state_order(stateidx)

        IF (utotal_s(1,i,isx) == 0 .AND. utotal_s(1,0,isx) == 0) CYCLE !! state not in sample

        rowlab = state_name(isx)

        DO j = 1, num_summ_pcts
           IF (abs(total_s(j,0,isx)) > 0.0) THEN
              pct(j) = 100.0 *  total_s(j,i,isx) / total_s(j,0,isx) - 100.0
           ELSE
              pct(j) = 0.0
           ENDIF

           temp1(j) = COMMA8(total_s(j,i,isx), 14)
        ENDDO

        WRITE (tabfile,1121) rowlab, (temp1(j), pct(j), j=1,num_summ_pcts)
        WRITE (JSON_FILE, *) ',' ! Ready for next row
        WRITE (JSON_FILE, 1111, ADVANCE="no") TRIM(state_name(isx)), &
              (NINT(total_s(j,i,isx), SELECTED_INT_KIND(12)), pct(j), j=1,num_summ_pcts) 

        IF (create_table_extracts) THEN
           archive_num = planname(i)(1:7)
           extract_label = planname(i)(:40)
           state_label = rowlab
           WRITE(32, 3201) 't_summ_st   ', i+1, ADJUSTL(extract_label ), ADJUSTL(state_label),  'W', 'Elig', 'Unit', &
                           total_s(1,i,isx), pct(1)
           WRITE(32, 3201) 't_summ_st   ', i+1, ADJUSTL(extract_label ), ADJUSTL(state_label),  'W', 'Elig', 'Pers', &
                           total_s(2,i,isx), pct(2)
           WRITE(32, 3201) 't_summ_st   ', i+1, ADJUSTL(extract_label ), ADJUSTL(state_label),  'W', 'Part', 'Unit', &
                           total_s(3,i,isx), pct(3)
           WRITE(32, 3201) 't_summ_st   ', i+1, ADJUSTL(extract_label ), ADJUSTL(state_label),  'W', 'Part', 'Pers', &
                           total_s(4,i,isx), pct(4)
           WRITE(32, 3201) 't_summ_st   ', i+1, ADJUSTL(extract_label ), ADJUSTL(state_label),  'W', 'Part', 'Ben ', &
                           total_s(5,i,isx), pct(5)
        END IF

     END DO

     WRITE(JSON_FILE, *) ! newline
     WRITE(JSON_FILE, *) "]," ! close table (array of arrays), get ready for next table


     !---------------------------------------------------------------------------------------
     !-------------- Print the Table (Unweighted) ---------------------------------------------

     CALL isnewpg(tabfile, page_break_numlines + 1 )

     WRITE (tabfile,1132) title_line(i)

     WRITE (tabfile,2105) ! column headings

     DO j = 1, num_summ_pcts
        x = utotal(j,0)
        temp1(j) = COMMA8 (x, 14)
     ENDDO

     rowlab = 'Baselaw   '
     WRITE (tabfile,2110) rowlab,  (temp1(j), j=1,num_summ_pcts)

     !! write 5 baselaw estimates to extract
     IF (create_table_extracts) THEN
        extract_label = 'Baselaw'
        state_label =  'Sum of states'
        WRITE(32, 3202) 't_summ_st   ', 1, ADJUSTL(extract_label), ADJUSTL(state_label),  'U', 'Elig', 'Unit', Utotal(1,0), 0
        WRITE(32, 3202) 't_summ_st   ', 1, ADJUSTL(extract_label), ADJUSTL(state_label),  'U', 'Elig', 'Pers', Utotal(2,0), 0
        WRITE(32, 3202) 't_summ_st   ', 1, ADJUSTL(extract_label), ADJUSTL(state_label),  'U', 'Part', 'Unit', Utotal(3,0), 0
        WRITE(32, 3202) 't_summ_st   ', 1, ADJUSTL(extract_label), ADJUSTL(state_label),  'U', 'Part', 'Pers', Utotal(4,0), 0
        WRITE(32, 3202) 't_summ_st   ', 1, ADJUSTL(extract_label), ADJUSTL(state_label),  'U', 'Part', 'Ben ', Utotal(5,0), 0
     END IF

3202 FORMAT (1x,a12, i3, 1x,a40, 1x,a16, 1x,a1, 1x,a4, 1x,a6, f20.0, 1x, i20)

     DO j = 1, num_summ_pcts
        x = utotal(j,i)
        temp1(j) = COMMA8(x, 14)
        diff(j) = NINT(utotal(j,i) - utotal(j,0))
     ENDDO

     WRITE (JSON_FILE, *) '"Table 3A": [' ! Summary Effects of Simulated Policy Change by State (Unweighted)
     WRITE (JSON_FILE, 1112, ADVANCE="no") "Sum of states", (NINT(utotal(j,i), SELECTED_INT_KIND(12)), diff(j), j=1, num_summ_pcts) 

     !! write 5 reform estimates to extract
     IF (create_table_extracts) THEN
        extract_label = planname(i)(:40)
        state_label =  'Sum of states'
        WRITE(32, 3202) 't_summ_st   ', i+1, ADJUSTL(extract_label), ADJUSTL(state_label), 'U', 'Elig', 'Unit', Utotal(1,i), diff(1)
        WRITE(32, 3202) 't_summ_st   ', i+1, ADJUSTL(extract_label), ADJUSTL(state_label), 'U', 'Elig', 'Pers', Utotal(2,i), diff(2)
        WRITE(32, 3202) 't_summ_st   ', i+1, ADJUSTL(extract_label), ADJUSTL(state_label), 'U', 'Part', 'Unit', Utotal(3,i), diff(3)
        WRITE(32, 3202) 't_summ_st   ', i+1, ADJUSTL(extract_label), ADJUSTL(state_label), 'U', 'Part', 'Pers', Utotal(4,i), diff(4)
        WRITE(32, 3202) 't_summ_st   ', i+1, ADJUSTL(extract_label), ADJUSTL(state_label), 'U', 'Part', 'Ben ', Utotal(5,i), diff(5)
     END IF



     rowlab = 'Plan ' // plannbr(i)
     WRITE (tabfile,2120) rowlab, (temp1(j), diff(j), j=1,num_summ_pcts)

     DO stateidx = 0, nstates
        isx = state_order(stateidx)
        IF (utotal_s(1,i,isx) == 0 .AND. utotal_s(1,0,isx) == 0) CYCLE !! state not in sample

        rowlab = state_name(isx)

        DO j = 1, num_summ_pcts
           x = REAL(utotal_s(j,i,isx))
           temp1(j) = COMMA8 (x, 14)
           diff(j) = utotal_s(j,i,isx) - utotal_s(j,0,isx)
        ENDDO

        WRITE (tabfile,2121) rowlab, (temp1(j), diff(j), j=1,num_summ_pcts)
        WRITE (JSON_FILE, *) ',' ! ready for next row
        WRITE (JSON_FILE, 1112, ADVANCE="no") TRIM(rowlab), (utotal_s(j,i,isx), diff(j), j=1, num_summ_pcts) 

        IF (create_table_extracts) THEN
           archive_num = planname(i)(1:7)
           extract_label = planname(i)(:40)
           state_label = rowlab
           WRITE(32, 3202) 't_summ_st   ', i+1, ADJUSTL(extract_label), ADJUSTL(state_label), 'U', 'Elig', 'Unit', &
                           REAL(utotal_s(1,i,isx)), diff(1)
           WRITE(32, 3202) 't_summ_st   ', i+1, ADJUSTL(extract_label), ADJUSTL(state_label), 'U', 'Elig', 'Pers', &
                           REAL(utotal_s(2,i,isx)), diff(2)
           WRITE(32, 3202) 't_summ_st   ', i+1, ADJUSTL(extract_label), ADJUSTL(state_label), 'U', 'Part', 'Unit', &
                           REAL(utotal_s(3,i,isx)), diff(3)
           WRITE(32, 3202) 't_summ_st   ', i+1, ADJUSTL(extract_label), ADJUSTL(state_label), 'U', 'Part', 'Pers', &
                           REAL(utotal_s(4,i,isx)), diff(4)
           WRITE(32, 3202) 't_summ_st   ', i+1, ADJUSTL(extract_label), ADJUSTL(state_label), 'U', 'Part', 'Ben ', &
                           REAL(utotal_s(5,i,isx)), diff(5)

        END IF


     END DO

     WRITE(JSON_FILE, *) ! newline
     WRITE(JSON_FILE, *) "]," ! close table (array of arrays), get ready for next table

  ENDDO  !--- loop over number of suffix sets

  !---------------------------------------------------------------------------------------
  !-------------- Print plan descriptions below the tables ---------------------------------


     deALLOCATE (total_s)
     deallocate (utotal_s)


  RETURN

  !------------------------------------------------------------------------------------------------------
  !----------- Format Statements ------------------------------------------------------------------------
1100 FORMAT(             &
       T64, '        '  &
       // T40, 'SUMMARY COMPARISONS OF IMPACTS ON SNAP BY STATE' //)

1105 FORMAT(      &
       /1x, 'Weighted'   &
       /1X, 131('-')     &
       /T35,'Eligibles ',T79,'Participants',T119,'Benefits' &
       /T17,42('-'),4X,42('-'),4X,24('-')                   &
       /t2,'State', t27,'Units  % Chg', t47,'Persons  % Chg', t71,'Units  % Chg', t91,'Persons  % Chg', t115,'Dollars    % Chg'&
       /1X,131('-')  / )

1110 FORMAT (1x,A14, t17, 4(1X,A14,5X,'NA'), 3X, A14, 7X, 'NA' /1X,66('- ')/ )

  ! JSON output of rows (non-statistical)
1111 FORMAT('["', A, '",', 4(I12, ',', F7.2, ','), I12, ',', F7.2, ']') ! Weighted values and diffs (floats)
1112 FORMAT('["', A, '",', 4(I12, ',', I12, ','), I12, ',', I12, ']') ! Unweighted values and diffs (counts=ints)

1120 FORMAT (1X,A14, t17, 4(1X, A14,F7.3  ), 3X, A14, F9.3 /1X,66('- ')/ )

1121 FORMAT (1X,A14, t17, 4(1X, A14,F7.3  ), 3X, A14, F9.3 )

1132 FORMAT (1x, a)

2105 FORMAT(///  &
       /1x, 'Unweighted' &
       /1X, 131('-')     &
       /T35,'Eligibles ',T79,'Participants',T119,'Benefits'  &
       /T15,42('-'),4X,42('-'),4X,24('-')                    &
       /t2,'State', t27,'Units    Chg', t47,'Persons    Chg', t71,'Units    Chg', t91,'Persons    Chg', t115,'Dollars      Chg'&
       /1X,131('-')  / )
2110 FORMAT (1X,A, t17, 4(1X,A14,5X,'NA') , 3X,  A14, 7X, 'NA'  /1X,66('- ')/ )
2120 FORMAT (1X,A, t17, 4(1X, A14, I7 )   , 3X,  A14, I9 /1X,66('- ')/  )
2121 FORMAT (1X,A, t17, 4(1X, A14, I7 )   , 3X,  A14, I9   )

END SUBROUTINE fs_tab_summary_state
