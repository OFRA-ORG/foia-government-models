!**************************************************************************************************
! Source File:  FSTAB7.F90                  
! Called By:    FS_TABLES                   
!
! TABLE 7 -  Person-level Gainer/Loser Tables
!
!**************************************************************************************************
    SUBROUTINE FS_TAB_gainer_loser_pers()

    USE GLOBAL

    USE FSWORK
    USE FSPARM
    USE FSSIZES, ONLY: MAX_NTH
    USE fslocs
    use Utils
    use states, only: TOTSTATES

    IMPLICIT NONE

    INTEGER, parameter :: num_part_categ = 5
    INTEGER :: ip, iunit, i, tab_index,  num_plans = 0

    !!  baselaw
    integer(8), DIMENSION(max_persons, 0:TOTSTATES) :: base_part
    INTEGER(8), DIMENSION(max_persons, 0:TOTSTATES) :: base_fsun, base_fsusize, base_fsben
    real,    DIMENSION(max_persons, 0:TOTSTATES) :: base_per_capita_fsben

    !!  reform
    integer(8), DIMENSION(max_persons) :: part
    real,    DIMENSION(max_persons) :: per_capita_fsben, benefit_change

!-- Comma display fields
    CHARACTER(100) :: end_label
    CHARACTER(132), DIMENSION(num_part_categ) :: title_line

    ! baselaw counts
    INTEGER(8), DIMENSION(num_part_categ, max_nth) :: nbr_base_part = 0
    REAL(8), DIMENSION(num_part_categ, max_nth) :: &
         wgt_nbr_base_part       = 0.0             &
       , tot_base_per_capita     = 0.0             &
       , wgt_tot_base_per_capita = 0.0

    INTEGER(8), DIMENSION(num_part_categ+1, max_nth) :: tot_base_ben = 0
    REAL(8), DIMENSION(num_part_categ+1, max_nth) :: wgt_tot_base_ben = 0.0

    ! reform counts
    INTEGER(8), DIMENSION(num_part_categ, max_nth) :: nbr_part = 0
    REAL(8), DIMENSION(num_part_categ, max_nth) :: &
         wgt_nbr_part       = 0.0                  &
        ,tot_per_capita     = 0.0                  &
        ,wgt_tot_per_capita = 0.0                  &
        ,tot_change         = 0.0                  &
        ,wgt_tot_change     = 0.0

    ! baselaw avgs
    REAL(8), DIMENSION(num_part_categ, max_nth) :: avg_base_per_capita, wgt_avg_base_per_capita

    ! reform avgs
    REAL(8), DIMENSION(num_part_categ, max_nth) :: avg_per_capita, wgt_avg_per_capita,  &
                                                avg_change, wgt_avg_change,          &
                                                pct_base_ben, wgt_pct_base_ben


    CHARACTER(14), DIMENSION(num_part_categ) :: awgt_nbr_part, awgt_tot_change

    character(len=40), dimension(num_part_categ) :: rowlab = (/  &
         'Base Participant, No Chg In Per-Cap-Ben'  &
        ,'Base Participant, No Longer Partic     '  &
        ,'Base Participant, Decr In Per-Cap-Ben  '  &
        ,'Base Participant, Incr In Per-Cap-Ben  '  &
        ,'New Participant                        '  &
        /)

    character(55) :: json_rowlab(num_part_categ) = (/&
      "Baselaw participant, no change in per-capita benefits  ", &
      "Baselaw participant, no longer participating           ", &
      "Baselaw participant, decrease in per-capita benefits   ", &
      "Baselaw participant, increase in per-capita benefits   ", &
      "Baselaw non-participant, participating under simulation" &
      /)

    CHARACTER(132) :: extr_title
!************************************************************************************************

    if (keof==2) goto 200
    if (keof==3) goto 300

!****************************************************************
!   KEOF=1
!****************************************************************
    num_plans = num_plans + 1
    return


!****************************************************************
!   KEOF=2
!****************************************************************
200 CONTINUE

    do ip = 1,ctprhh
       per_capita_fsben(ip) = 0.0
       part(ip) = 0
    end do


!---baselaw calculations:
    if (nth == 1) then

       do ip = 1, ctprhh
          base_fsben(ip, kist)  = l_fsben(1, KIST)%iper(ip)
          base_part(ip, kist)   = l_fspart(1, KIST)%iper(ip)
          base_fsun(ip, kist)   = l_fsun(1, KIST)%iper(ip)
          base_fsusize(ip, kist)  = l_fsusize(1, KIST)%iper(ip)
          base_per_capita_fsben(ip, kist) = 0.0
       end do

       do iunit = 1, ctprhh
          IF (base_fsun(iunit, kist) /= iunit) cycle
          if (base_part(iunit, kist) == 0) cycle  !! baselaw part
          do ip = 1,ctprhh
             if (base_fsun(ip, kist) /= iunit) cycle
             base_part(ip, kist) = 1
             base_per_capita_fsben(ip, kist) = &
                REAL(base_fsben(iunit, kist)) / REAL(base_fsusize(iunit, kist))
          end do
       end do

    endif


!---reform calculations:
     do iunit = 1, ctprhh
       IF(fsun(iunit) /= iunit) cycle
       if (fspart(iunit) == 0) cycle
       do ip = 1,ctprhh
          if (fsun(ip) /= iunit) cycle
          part(ip) = 1
          per_capita_fsben(ip) = REAL(fsben(iunit)) / REAL(fsusize(iunit))
       end do
    end do

    do ip = 1, ctprhh
       tab_index = 0
       benefit_change(ip) = per_capita_fsben(ip) - base_per_capita_fsben(ip, kist)

       if (base_part(ip, kist) == 1) then
          select case (part(ip))
             case (0)
                tab_index = 2               !! no longer part
             CASE (1)
                if ( .not. abs(benefit_change(ip)) > 0.0) then
                   tab_index = 1           !! no change in per-capita-benefit
                ELSEIF(benefit_change(ip) < 0.0) then
                   tab_index = 3           !! decrease in per-capita-benefit
                else
                   tab_index = 4           !! increase in per-capita-benefit
                end if
          end select

       else
          if (part(ip) == 1) tab_index = 5    !! newly part
       end if

       if (tab_index == 0) cycle         !! not part in both baselaw and reform

       if (tab_index < 5) then

          nbr_base_part(tab_index, nth) = nbr_base_part(tab_index, nth) + 1
          wgt_nbr_base_part(tab_index, nth) = wgt_nbr_base_part(tab_index, nth) + wgt

          tot_base_ben(tab_index, nth) = tot_base_ben(tab_index, nth) + base_fsben(ip, kist)
          ! cast wgt as a real 8 in this next line because base_fsben is now int 8
          wgt_tot_base_ben(tab_index, nth) = wgt_tot_base_ben(tab_index, nth) + base_fsben(ip, kist) * real(wgt,8)   

          tot_base_ben(num_part_categ+1, nth) = tot_base_ben(num_part_categ+1, nth) + base_fsben(ip, kist)
          ! cast wgt as a real 8 in this next line because base_fsben is now int 8
          wgt_tot_base_ben(num_part_categ+1, nth) = wgt_tot_base_ben(num_part_categ+1, nth) + base_fsben(ip, kist) * real(wgt,8)

          tot_base_per_capita(tab_index, nth) = tot_base_per_capita(tab_index, nth) + base_per_capita_fsben(ip, kist)
          wgt_tot_base_per_capita(tab_index, nth) = wgt_tot_base_per_capita(tab_index, nth) + base_per_capita_fsben(ip, kist) * wgt

       end if

       nbr_part(tab_index,nth) = nbr_part(tab_index,nth) + 1
       wgt_nbr_part(tab_index,nth) = wgt_nbr_part(tab_index,nth) + wgt

       tot_per_capita(tab_index,nth) = tot_per_capita(tab_index,nth) + per_capita_fsben(ip)
       wgt_tot_per_capita(tab_index,nth) = wgt_tot_per_capita(tab_index,nth) + per_capita_fsben(ip)*wgt

       tot_change(tab_index,nth) = tot_change(tab_index,nth) + benefit_change(ip)
       wgt_tot_change(tab_index,nth) = wgt_tot_change(tab_index,nth) + benefit_change(ip)*wgt


    end do

    RETURN

!****************************************************************
!   KEOF=3
!****************************************************************
300 continue


    ! compute avgs for all plans
    do i = 1, num_plans

       do tab_index = 1, num_part_categ

          if ( nbr_base_part(tab_index, i) /= 0) THEN
             avg_base_per_capita(tab_index, i) = REAL(tot_base_per_capita(tab_index, i)) / REAL(nbr_base_part(tab_index, i))
          else
             avg_base_per_capita(tab_index, i) = 0.0
          endif

          IF ( abs(wgt_nbr_base_part(tab_index, i)) > 0.0) THEN
             wgt_avg_base_per_capita(tab_index, i) = wgt_tot_base_per_capita(tab_index, i) / wgt_nbr_base_part(tab_index, i)
          else
             wgt_avg_base_per_capita(tab_index, i) = 0.0
          ENDIF

          if (nbr_part(tab_index,i) /= 0) then
             avg_per_capita(tab_index,i) = REAL(tot_per_capita(tab_index,i)) / REAL(nbr_part(tab_index,i))
             avg_change(tab_index,i) = REAL(tot_change(tab_index,i)) / REAL(nbr_part(tab_index,i))
          else
             avg_per_capita(tab_index,i) = 0.0
             avg_change    (tab_index,i) = 0.0
          endif

          if (abs(wgt_nbr_part(tab_index,i)) > 0.0) then
             wgt_avg_per_capita(tab_index,i) = wgt_tot_per_capita(tab_index,i) / wgt_nbr_part(tab_index,i)
             wgt_avg_change(tab_index,i) = wgt_tot_change(tab_index,i) / wgt_nbr_part(tab_index,i)
          else
             wgt_avg_per_capita(tab_index,i) = 0.0
             wgt_avg_change(tab_index,i) = 0.0
          endif

          if (tot_base_ben(num_part_categ+1, i) /= 0) then
             pct_base_ben(tab_index,i) = &
                REAL(tot_change(tab_index,i)) / REAL(tot_base_ben(num_part_categ+1, i)) * 100.0
          else
             pct_base_ben(tab_index,i) = 0.0
          endif

          if (abs(wgt_tot_base_ben(num_part_categ+1, i)) > 0.0) then
             wgt_pct_base_ben(tab_index,i) = &
                wgt_tot_change(tab_index,i) / wgt_tot_base_ben(num_part_categ+1, i) * 100.0
          else
             wgt_pct_base_ben(tab_index,i) = 0.0
          endif

       end do
    end do

    ! Write JSON table
    WRITE (JSON_FILE, *) '"Table 8": [' ! open table (per-capita and total benefits)
    do tab_index = 1, num_part_categ
      ! Note: no baselaw
      WRITE(JSON_FILE, 1111, ADVANCE='no') TRIM(json_rowlab(tab_index)), &
        NINT(wgt_nbr_part(tab_index, 1), SELECTED_INT_KIND(12)), &
        wgt_avg_base_per_capita(tab_index, 1), &
        wgt_avg_per_capita(tab_index, 1), &
        wgt_avg_change(tab_index, 1), &
        NINT(wgt_tot_change(tab_index, 1), SELECTED_INT_KIND(12)), &
        wgt_pct_base_ben(tab_index, 1)

      if (tab_index /= num_part_categ) WRITE(JSON_FILE, *) "," ! not the last row
    end do

    WRITE(JSON_FILE, *) ']' ! done with this table.  this is actually the last table!

    !  write the table(s):
    do i = 1, num_plans

       call isnewpg(tabfile, page_break_numlines + 1 )
       title_line(i) = "Plan " // plannbr(i) // ": " // planname(i)
       call center_text(title_line(i), 131)

       WRITE(tabfile, 3000)                     !! title
       WRITE(tabfile, 3010) title_line(i)
       WRITE(tabfile, 3125)                     !! underline
       WRITE(tabfile, 3020)                     !! column headings
       WRITE(tabfile, 3125)                     !! underline

       do tab_index = 1, num_part_categ
          awgt_nbr_part(tab_index) = comma8(wgt_nbr_part(tab_index, i), 14)
          awgt_tot_change(tab_index) = comma8(wgt_tot_change(tab_index,i), 14)
       end do

       WRITE(tabfile,3030)                    &
          (awgt_nbr_part          (tab_index)     &
          ,wgt_avg_base_per_capita(tab_index, i)  &
          ,wgt_avg_per_capita     (tab_index, i)  &
          ,wgt_avg_change         (tab_index, i)  &
          ,awgt_tot_change        (tab_index)     &
          ,wgt_pct_base_ben       (tab_index, i), tab_index = 1, num_part_categ)

      if (create_table_extracts) then
        do tab_index = 1, num_part_categ
           extr_title = adjustl(planname(i))
           write(37, 3701)  &
             't_gl_pers   ' &
            ,i+1            &
            ,extr_title(1:56)     &
            ,rowlab(tab_index)    &
            ,wgt_nbr_part           (tab_index, i)  &
            ,wgt_avg_base_per_capita(tab_index, i)  &
            ,wgt_avg_per_capita     (tab_index, i)  &
            ,wgt_avg_change         (tab_index, i)  &
            ,wgt_tot_change         (tab_index, i)  &
            ,wgt_pct_base_ben       (tab_index, i)

        end do
      end if

    end do

 3701 format(1x,a12, 1x,i3, 1x,a56, 1x,a40, f15.0, 3(1x,f12.2), 1x,f15.0, 1x,f12.4)


3000  FORMAT( &
       T42,'                         '//                              &
       T42,'  ANALYSIS OF SNAP UNIT COMPOSITION CHANGES'                 )

3010  FORMAT(/ a /)

3020  format (    T52, "Average", t69, "Average", t86, "Average", t103, "Total Change in Benefits" &
              ,/, T52, "Per-Capita", t69, "Per-Capita", t86, "Change In", t100, 31("-")           &
              ,/, T52, "Benefit",    t69, "Benefit",    t86, "Per-Capita", t118, "% of Total"     &
              ,/, t32, "Participants"                                                             &
                , T52, "In Baselaw", t69, "In Reform ", t86, "Benefit",  t103, "Dollars"          &
                ,t115, "Baselaw Benefits")

! JSON table row format
1111 format('["', A, '",', I12, ',', 3(f12.2, ','), I12, ',', f7.2, ']')

3030  FORMAT(//, t2, "Baselaw Participant,"  &
             ,/, t7, "No Change In"          &
             ,/, t7, "Per-Capita-Benefits", t30, a, t45, 3(5x,f12.2), t100, a, t120, f7.2  &
             //, t2, "Baselaw Participant,"  &
             ,/, t7, "No Longer   "          &
             ,/, t7, "Participating      ", t30, a, t45, 3(5x,f12.2), t100, a, t120, f7.2  &
            ,//, t2, "Baselaw Participant,"  &
             ,/, t7, "Decrease In "          &
             ,/, t7, "Per-Capita-Benefits", t30, a, t45, 3(5x,f12.2), t100, a, t120, f7.2  &
            ,//, t2, "Baselaw Participant,"  &
             ,/, t7, "Increase In "          &
             ,/, t7, "Per-Capita-Benefits", t30, a, t45, 3(5x,f12.2), t100, a, t120, f7.2  &
            ,//, t2, "Baselaw Non-Participant,"  &
             ,/, t7, "Participating"             &
             ,/, t7, "Under Reform       ", t30, a, t45, 3(5x,f12.2), t100, a, t120, f7.2  )

    END_LABEL =  '<<## END OF FSTAMP TABLE  : ANALYSIS OF UNIT COMPOSITION CHANGES ##>>'


    RETURN


3125  FORMAT (1X,131('-')/)   ! underline

    END
