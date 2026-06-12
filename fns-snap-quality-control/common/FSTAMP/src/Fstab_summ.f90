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
SUBROUTINE fs_tab_summary()
  USE global
  USE userparm
  USE states
  USE fssizes
  USE fswork
  USE fslocs
  USE fsparm
  use Utils

  IMPLICIT NONE

  INTEGER, PARAMETER :: num_summ_pcts = 5  ! number of pct change estimates in Table 1

  INTEGER :: i, ip, j, k, num_plans = 0
  INTEGER :: diff  (num_summ_pcts)
  INTEGER ::  idx, ikist
  INTEGER, DIMENSION(num_summ_pcts) :: baselaw_est,  reform_est
  REAL(8), DIMENSION(num_summ_pcts) :: pct
  REAL(8):: x

  REAL(8) :: wtotal(num_summ_pcts, 0:max_nth) = 0.0
  REAL(8) :: utotal(num_summ_pcts, 0:max_nth) = 0.0

  INTEGER ::         &
       num_elig_units  &
       ,num_part_units  &
       ,max_elig_units  &
       ,max_part_units  &
       ,num_elig_states &
       ,num_part_states &
       ,sum_elig_pers   &
       ,sum_part_pers   &
       ,sum_ben

  REAL :: avg_pers, avg_ben

  !!  table key
  CHARACTER(LEN=92), DIMENSION(3) :: table_key = (/  &
       "These are results from a national simulation.                                              "  &
       ,"These are results from a state simulation. Changes over all states are tabulated.          "  &
       ,"These are results from a state simulation. Changes over selected states only are tabulated."  &
       /)


  CHARACTER(len=14) :: temp3(num_summ_pcts)
  CHARACTER(LEN=14) :: temp1 (num_summ_pcts), temp2 (num_summ_pcts)
  CHARACTER(len=14) :: csumm_sd
  CHARACTER(LEN=10) :: rowlab
  CHARACTER(len=07) :: rowlab2 = 'Std Err'
  CHARACTER(len=05) :: temp3a(num_summ_pcts)
  CHARACTER(len=07) :: archive_num
  CHARACTER(len=40) :: extract_label
  INTEGER :: zero_int = 0


  IF (keof==2) GOTO 200
  IF (keof==3) GOTO 300

  !------------------------------------------------------------------------------
  !------------------------------------------------------------------------------
  num_plans = num_plans + 1



  RETURN

  !------------------------------------------------------------------------------
200 CONTINUE     !---- phase 2 processing -------------------------------------
  !------------------------------------------------------------------------------

  !--------------------------------------------------------------------------
  !------- count up esimates for baselaw plan--------------------------------
  IF (nth == 1) THEN

     s_baselaw_est = 0

     max_elig_units = 0
     max_part_units = 0
     num_elig_states = 0
     num_part_states = 0
     sum_elig_pers = 0
     sum_part_pers = 0
     sum_ben = 0
     x0 = 0.0

     DO ikist = start_kist, end_kist
        kist = state_order(ikist)

        IF (model_code == "MSIP") THEN
           !!  select baselaw data to tabulate
           SELECT CASE (dostate)
              !!  only tab national results
           CASE (1)
              IF (kist /= US_POS) CYCLE
              !!  only tab 51 simstates
           CASE (2)
              IF (kist == US_POS) CYCLE
              !!  only tab SELECTED simstates
           CASE default
              IF ( .NOT. aprocsta(kist, nth) .OR. kist == US_POS) CYCLE
           END SELECT
        END IF

        baselaw_est = 0

        num_elig_units = 0
        num_part_units = 0

        DO ip = 1, ctprhh
           IF (l_fsben(1, KIST)%iper(ip) > 0) THEN
              baselaw_est(1) = baselaw_est(1) + 1
              baselaw_est(2) = baselaw_est(2) + l_fsusize(1, KIST)%iper(ip)

              num_elig_units = num_elig_units + 1
              num_elig_states = num_elig_states + 1
              sum_elig_pers = sum_elig_pers + l_fsusize(1, KIST)%iper(ip)

              IF (l_fspart(1, KIST)%iper(ip) > 0) THEN
                 baselaw_est(3) = baselaw_est(3) + 1
                 baselaw_est(4) = baselaw_est(4) + l_fsusize(1, KIST)%iper(ip)
                 baselaw_est(5) = baselaw_est(5) + l_fsben  (1, KIST)%iper(ip)

                 num_part_units = num_part_units + 1
                 num_part_states = num_part_states + 1
                 sum_part_pers = sum_part_pers + l_fsusize(1, KIST)%iper(ip)
                 sum_ben = sum_ben + l_fsben(1, KIST)%iper(ip)
              ENDIF
           ENDIF
        ENDDO

        max_elig_units = MAX(max_elig_units, num_elig_units)
        max_part_units = MAX(max_part_units, num_part_units)

        wtotal (:,0) = wtotal (:,0) + baselaw_est * wgt1(kist)

        s_baselaw_est(:, kist) = baselaw_est

     END DO

     !! unweighted elig counts
     IF (num_elig_states > 0) THEN
        avg_pers = REAL(sum_elig_pers) / num_elig_states
        x0(1) = REAL(max_elig_units)
        x0(2) = avg_pers
     END IF

     !! unweighted part counts
     IF (num_part_states > 0) THEN
        avg_pers = REAL(sum_part_pers) / num_part_states
        avg_ben = REAL(sum_ben) / num_part_states
        x0(3) = REAL(max_part_units)
        x0(4) = avg_pers
        x0(5) = avg_ben
     END IF


     IF (dostate == 1) THEN
        utotal (:,0) = utotal (:,0) + baselaw_est
     ELSE
        utotal (:,0) = utotal (:,0) + x0
     END IF

  ENDIF   !! end of baselaw (nth=1) code


  !----------------------------------------------------------------
  !-------- count up estimates for NTH plan -----------------------
  s_reform_est = 0

  max_elig_units = 0
  max_part_units = 0
  num_elig_states = 0
  num_part_states = 0
  sum_elig_pers = 0
  sum_part_pers = 0
  sum_ben = 0
  x1 = 0.0

  idx = nth + 1
  DO ikist = start_kist, end_kist
     kist = state_order(ikist)

     IF (model_code == "MSIP") THEN
        !!  select reform data to tabulate
        SELECT CASE (dostate)
           !!  only tab national results
        CASE (1)
           IF (kist /= US_POS) CYCLE
           !!  only tab 51 simstates
        CASE (2)
           IF (kist == US_POS) CYCLE
           !!  only tab SELECTED simstates
        CASE default
           IF (kist == US_POS .OR. .NOT. aprocsta(kist, nth)) CYCLE
        END SELECT
     END IF


     reform_est = 0
     num_elig_units = 0
     num_part_units = 0

     DO ip = 1, ctprhh
        IF (l_fsben(idx, KIST)%iper(ip) > 0) THEN
           reform_est(1) = reform_est(1) + 1
           reform_est(2) = reform_est(2) + l_fsusize(idx, KIST)%iper(ip)

           num_elig_units = num_elig_units + 1
           num_elig_states = num_elig_states + 1
           sum_elig_pers = sum_elig_pers + l_fsusize(idx, KIST)%iper(ip)

           IF (l_fspart(idx, KIST)%iper(ip) > 0) THEN
              reform_est(3) = reform_est(3) + 1
              reform_est(4) = reform_est(4) + l_fsusize(idx, KIST)%iper(ip)
              reform_est(5) = reform_est(5) + l_fsben  (idx, KIST)%iper(ip)

              num_part_units = num_part_units + 1
              num_part_states = num_part_states + 1
              sum_part_pers = sum_part_pers + l_fsusize(idx, KIST)%iper(ip)
              sum_ben = sum_ben + l_fsben(idx, KIST)%iper(ip)

           ENDIF
        ENDIF
     END DO

     max_elig_units = MAX(max_elig_units, num_elig_units)
     max_part_units = MAX(max_part_units, num_part_units)

     wtotal (:,nth) = wtotal (:,nth) + reform_est * wgt1(kist)

     s_reform_est(:, kist) = reform_est

  END DO

  !! unweighted elig counts
  IF (num_elig_states > 0) THEN
     avg_pers = REAL(sum_elig_pers) / num_elig_states
     x1(1) = REAL(max_elig_units)
     x1(2) = avg_pers
  END IF

  !! unweighted part counts
  IF (num_part_states > 0) THEN
     avg_pers = REAL(sum_part_pers) / num_part_states
     avg_ben = REAL(sum_ben) / num_part_states
     x1(3) = REAL(max_part_units)
     x1(4) = avg_pers
     x1(5) = avg_ben
  END IF

  IF (dostate == 1) THEN
     utotal (:,nth) = utotal (:,nth) + reform_est
  ELSE
     utotal (:,nth) = utotal (:,nth) + x1
  END IF



  IF (model_code == "MSIP") THEN
     CALL fs_table1_x()
  END IF

  RETURN

  !------------------------------------------------------------------------------
300 CONTINUE     !---- phase 3 processing -------------------------------------
  !------------------------------------------------------------------------------

  !-------------- Print the Table (Weighted) ---------------------------------------------
  CALL isnewpg(tabfile, page_break_numlines + 1 )         ! +1 lets ISNEWPG know
  ! lines are coming - but
  WRITE (tabfile,1100) ! title                            ! not exactly how many.
  WRITE (tabfile,1105) ! column headings
      
  ! Begin writing table as a named array of arrays to JSON
  write (JSON_FILE, *) '"Table 1": ['

  DO i = 0, num_plans
     DO j = 1, num_summ_pcts
        temp1(j) = COMMA8(wtotal(j,i), 14)
     ENDDO

     IF (i == 0) THEN
        rowlab = 'Baselaw   '
        WRITE (tabfile,1110) rowlab,  (temp1(j), j=1,num_summ_pcts)
        IF (dostats(nth)) THEN
           WRITE (JSON_FILE, 1221, ADVANCE="no") TRIM(rowlab), (NINT(wtotal(j,i), SELECTED_INT_KIND(12)), j=1, num_summ_pcts) ! no sig testing for baselaw
        ELSE
           WRITE (JSON_FILE, 1211, ADVANCE="no") TRIM(rowlab), (NINT(wtotal(j,i), SELECTED_INT_KIND(12)), j=1, num_summ_pcts)
        END IF
        extract_label = 'Baselaw'
        !! write 5 baselaw estimates to extract
        IF (create_table_extracts) THEN
           WRITE(31, 3101) 't_summ      ', i+1, ADJUSTL(extract_label), 'W', 'Elig', 'Unit', wtotal(1,i), 0.0, 0.0, " ", 0.0
           WRITE(31, 3101) 't_summ      ', i+1, ADJUSTL(extract_label), 'W', 'Elig', 'Pers', wtotal(2,i), 0.0, 0.0, " ", 0.0
           WRITE(31, 3101) 't_summ      ', i+1, ADJUSTL(extract_label), 'W', 'Part', 'Unit', wtotal(3,i), 0.0, 0.0, " ", 0.0
           WRITE(31, 3101) 't_summ      ', i+1, ADJUSTL(extract_label), 'W', 'Part', 'Pers', wtotal(4,i), 0.0, 0.0, " ", 0.0
           WRITE(31, 3101) 't_summ      ', i+1, ADJUSTL(extract_label), 'W', 'Part', 'Ben ', wtotal(5,i), 0.0, 0.0, " ", 0.0
        END IF

3101    FORMAT (1x,a12, i3, 1x, a40, a2, a5, a20, f20.4, 2f10.4, a5, f20.4)

     ELSE
        rowlab = 'Plan ' // plannbr(i)
        DO j = 1, num_summ_pcts
           IF (abs(wtotal(j,0)) > 0.0) THEN
              pct(j) = 100.0 *  wtotal(j,i) / wtotal(j,0) - 100.0
           ELSE
              pct(j) = 0.0
           ENDIF
           IF (j < num_summ_pcts) THEN
              csumm_sd = comma8(tab_summ_sd(j,i), 14)
              temp3(j) = csumm_sd
              temp3a(j) = "(" // ADJUSTL(tab_summ_stats(j,i)) // ")"

           ELSE
              csumm_sd = comma8(tab_summ_sd_ben(i), 14)
              temp3(j) = csumm_sd
              temp3a(j) = "(" // ADJUSTL(tab_summ_stats_ben(i)) // ")"

           END IF

           temp2(j) = " "
           DO k = 14, 1, -1                       !-------------------
              IF ( temp2(j)(k:k) == ' ') THEN      !-- add a left
                 temp2(j)(k:k) = '('                !-- parenthesis
                 EXIT                               !-- to the standard
              ENDIF                                !-- error
           ENDDO                                  !-------------------
        ENDDO
        WRITE (tabfile,1120) rowlab, (temp1(j), pct(j), j=1,num_summ_pcts) &
             ,rowlab2, (temp3(j), temp3a(j),  j=1,num_summ_pcts)
        IF (dostats(nth)) THEN
           WRITE (JSON_FILE, 1121, ADVANCE="no") TRIM(ADJUSTL(planname(i))),   &
                  (NINT(wtotal(j,i), SELECTED_INT_KIND(12)), pct(j), TRIM(tab_summ_stats(i,j)), j=1, num_summ_pcts - 1),  &
                  NINT(wtotal(num_summ_pcts,i), SELECTED_INT_KIND(12)), pct(num_summ_pcts), TRIM(tab_summ_stats_ben(i)) 
        ELSE
           WRITE (JSON_FILE, 1111, ADVANCE="no") TRIM(ADJUSTL(planname(i))),   &
                  (NINT(wtotal(j,i), SELECTED_INT_KIND(12)), pct(j), j=1, num_summ_pcts)
        END IF
        !!  write 5 reform estimates to extract
        IF (create_table_extracts) THEN
           archive_num = planname(i)(1:7)
           WRITE(31, 3101) 't_summ      ', i+1, ADJUSTL(planname(i)), 'W', 'Elig', 'Unit', wtotal(1,i), 0.0, pct(1), &
                           tab_summ_stats(1,i), tab_summ_sd(1,i)
           WRITE(31, 3101) 't_summ      ', i+1, ADJUSTL(planname(i)), 'W', 'Elig', 'Pers', wtotal(2,i), 0.0, pct(2), &
                           tab_summ_stats(2,i), tab_summ_sd(2,i)
           WRITE(31, 3101) 't_summ      ', i+1, ADJUSTL(planname(i)), 'W', 'Part', 'Unit', wtotal(3,i), 0.0, pct(3), &
                           tab_summ_stats(3,i), tab_summ_sd(3,i)
           WRITE(31, 3101) 't_summ      ', i+1, ADJUSTL(planname(i)), 'W', 'Part', 'Pers', wtotal(4,i), 0.0, pct(4), &
                           tab_summ_stats(4,i), tab_summ_sd(4,i)
           WRITE(31, 3101) 't_summ      ', i+1, ADJUSTL(planname(i)), 'W', 'Part', 'Ben ', wtotal(5,i), 0.0, pct(5), &
                           tab_summ_stats_ben(i), tab_summ_sd_ben(i)
        END IF

     ENDIF

     WRITE (tabfile,*) " "
     if (i /= num_plans) write (JSON_FILE, *) ","    ! comma between rows (except for the last row)

  ENDDO   !--- loop over number of suffix sets

  WRITE (tabfile,1125)  ! underline
  write(JSON_FILE, *)    ! newline
  write(JSON_FILE, *) "],"    ! close table (array of arrays), get ready for next table


  !---------------------------------------------------------------------------------------
  !-------------- Print the Table (Unweighted) ---------------------------------------------
  !---------------------------------------------------------------------------------------

  !--- skip for MS+ state runs
  IF (model_code == "MSIP" .AND. dostate > 1) GOTO 301

  WRITE (tabfile,2105) ! column headings
  write (JSON_FILE, *) '"Table 1B": ['

  DO i = 0, num_plans
     DO j = 1, num_summ_pcts
        x = utotal(j,i)
        temp1(j) = COMMA8(x, 14)
     ENDDO

     IF (i == 0) THEN
        rowlab = 'Baselaw   '
        WRITE (tabfile,2110) rowlab,  (temp1(j), j=1,num_summ_pcts)
        ! no stats testing for counts:
        WRITE (JSON_FILE, 1211, ADVANCE="no") TRIM(rowlab), (NINT(utotal(j,i), SELECTED_INT_KIND(12)), j=1, num_summ_pcts) ! no stats testing for counts

        IF (create_table_extracts) THEN
           extract_label = 'Baselaw'
           WRITE(31, 3102) 't_summ      ', i+1, ADJUSTL(extract_label), 'U', 'Elig', 'Unit', NINT(Utotal(1,i)), zero_int
           WRITE(31, 3102) 't_summ      ', i+1, ADJUSTL(extract_label), 'U', 'Elig', 'Pers', NINT(Utotal(2,i)), zero_int
           WRITE(31, 3102) 't_summ      ', i+1, ADJUSTL(extract_label), 'U', 'Part', 'Unit', NINT(Utotal(3,i)), zero_int
           WRITE(31, 3102) 't_summ      ', i+1, ADJUSTL(extract_label), 'U', 'Part', 'Pers', NINT(Utotal(4,i)), zero_int
           WRITE(31, 3102) 't_summ      ', i+1, ADJUSTL(extract_label), 'U', 'Part', 'Ben ', NINT(Utotal(5,i)), zero_int
        END IF
     ELSE
        rowlab = 'Plan ' // plannbr(i)
        DO j = 1, num_summ_pcts
           diff(j) = NINT(utotal(j,i) - utotal(j,0))
        ENDDO
        WRITE (tabfile,2120) rowlab, (temp1(j), diff(j), j=1,num_summ_pcts)
        WRITE (JSON_FILE, 1112, ADVANCE="no") TRIM(ADJUSTL(planname(i))),    &
               (NINT(utotal(j,i), SELECTED_INT_KIND(12)), diff(j), j=1, num_summ_pcts) ! no stats testing for counts

        IF (create_table_extracts) THEN
           archive_num = planname(i)(1:7)
           WRITE(31, 3102) 't_summ      ', i+1, ADJUSTL(planname(i)), 'U', 'Elig', 'Unit', NINT(utotal(1,i)),  diff(1)
           WRITE(31, 3102) 't_summ      ', i+1, ADJUSTL(planname(i)), 'U', 'Elig', 'Pers', NINT(utotal(2,i)),  diff(2)
           WRITE(31, 3102) 't_summ      ', i+1, ADJUSTL(planname(i)), 'U', 'Part', 'Unit', NINT(utotal(3,i)),  diff(3)
           WRITE(31, 3102) 't_summ      ', i+1, ADJUSTL(planname(i)), 'U', 'Part', 'Pers', NINT(utotal(4,i)),  diff(4)
           WRITE(31, 3102) 't_summ      ', i+1, ADJUSTL(planname(i)), 'U', 'Part', 'Ben ', NINT(utotal(5,i)),  diff(5)
        END IF

     ENDIF
     
     if (i /= num_plans) write (JSON_FILE, *) ","    ! comma between rows (except for the last row)
     
  ENDDO   !--- loop over number of suffix sets

3102 FORMAT (1x,a12, i3, 1x, a40, a2, a5, a20, 2i20)


  WRITE (tabfile,1125)   ! underline
  write(JSON_FILE, *)    ! newline
  write(JSON_FILE, *) "],"    ! close table (array of arrays), get ready for next table


301 CONTINUE

  !---------------------------------------------------------------------------------------
  !-------------- Print plan descriptions below the tables ---------------------------------
  DO i = 1, num_plans
     WRITE (tabfile,1130) plannbr(i), planname(i)
  ENDDO

  WRITE(tabfile, 1280) table_key(dostate)
  ! Write the key as a footnote for all tables.
  WRITE(JSON_FILE, *) ' "fnote": "', TRIM(table_key(dostate)), '",'

  WRITE(tabfile, 1290) !------- stats key


  IF (model_code == "MSIP") THEN
     CALL fs_table1_x()
  END IF


  RETURN

  !------------------------------------------------------------------------------------------------------
  !----------- Format Statements ------------------------------------------------------------------------
1100 FORMAT(             &
       T64, '       '   &
       // T50, 'SUMMARY COMPARISONS OF IMPACTS ON SNAP' //)
1105 FORMAT(       &
       /1x, 'Weighted'   &
       /1X, 131('-')     &
       /T33,'Eligibles ',T77,'Participants',T117,'Benefits' &
       /T15,42('-'),4X,42('-'),4X,24('-')                   &
       /1X,' Plan No.            Units  % Chg         Persons  % Chg           Units  % Chg         Persons  % Chg', &
       '           Dollars    % Chg' &
       /1X,131('-')  / )
1110 FORMAT (2X,A9, 4(2X,A14,5X,'NA') , 4X,  A14, 7X, 'NA'  &
       /1X,66('- ')/ )

! JSON output of rows (non-statistical)
! 1111 Weighted values and diffs (counts=rounded floats, diffs=pcts)
1111 FORMAT('["', A, '",', 4(I12, ',', F7.2, ','), I12, ',', F9.2, ']') 
! 1211 Weighted and unweighted values, baselaw (with NAs for diffs) (counts=rounded floats, diffs=pcts)
1211 FORMAT('["', A, '",', 4(I12, ',"n.a.",'), I12, ',"n.a."]') 
! 1112 Unweighted values and diffs (counts=ints, diffs=ints)
1112 FORMAT('["', A, '",', 4(I12, ',', I12, ','), I12, ',', I12, ']') 

! JSON output of rows (statistical tests)
! 1121 Weighted values and diffs (floats) with statistical sig level (string)
1121 FORMAT('["', A, '",', 4(I12, ',', F7.2, ',"', A, '",'), I12, ',', F9.2, ',"', A, '"]') 
! 1221 Weighted values, baselaw (with NAs for diffs and blanks for stat checks)
1221 FORMAT('["', A, '",', 4(I12, ',"n.a.","",'), I12, ',"n.a.",""]') 

1120 FORMAT (2X,A9, 4(2X, A14,F7.3  ) , 4X,  A14, F9.3  &
       ,/, 2x,a7,2x, 4(2x, a, a, 2x  ) , 4x,  a, a      )

1125 FORMAT (1X,131('-')/)   ! underline
1130 FORMAT (2X,'Plan ',A,':  ',A70)
2105 FORMAT(///  &
       /1x, 'Unweighted' &
       /1X, 131('-')     &
       /T33,'Eligibles ',T77,'Participants',T117,'Benefits'  &
       /T15,42('-'),4X,42('-'),4X,24('-')                    &
       /1X,' Plan No.            Units    Chg         Persons    Chg           Units    Chg         Persons    Chg', &
       '           Dollars      Chg' &
       /1X,131('-')  / )
2110 FORMAT (2X,A9, 4(2X,A14,5X,'NA') , 4X,  A14, 7X, 'NA' &
       /1X,66('- ')/ )
2120 FORMAT (2X,A9, 4(2X, A14, I7 ) , 4X,  A14, I9   /  )

1280 FORMAT (/,1x,"Note: ", a)

1290 FORMAT (&
       /,1x,'Statistics key:' &
       ,/,1x,'(*  ) Change is statistically different from zero at a 90% level of significance' &
       ,/,1x,'(** ) Change is statistically different from zero at a 95% level of significance' &
       ,/,1x,'(***) Change is statistically different from zero at a 99% level of significance' &
       ,/,1x,'(   ) None of the above conditions are met' &
       ,/,1x,'(N/A) No statistics selected for this run'  &
       /)


END SUBROUTINE fs_tab_summary
