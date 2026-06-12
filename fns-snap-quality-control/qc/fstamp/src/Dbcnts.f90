!**************************************************************************************************
! Source File:  DBCNTS.F90                  
! Called By:    FSTAMP2                     
!
! Increments the database-specific debug counts (keof=2).
! Prints totals for the database-specific debug counts (keof=3).
!
!**************************************************************************************************
    subroutine db_fs_counts

    use global
    use states
    use fssizes
    use fsparm
    use fslocs
    use fswork
    use fs_dbwork
    use fs_dblocs
    use fs_dbdefine
    use utils
    implicit none

    integer, parameter :: num_medimp_cases = 4

    integer ::                          &
       nbr_pure_pa_diff(max_nth) = 0


    integer ::                          &
       nbr_head_lt_age15_u (max_nth)= 0 &
     , nbr_kid_only_u      (max_nth)= 0 &
     , nbr_unknown_age_u   (max_nth)= 0 &
     , nbr_weird_depded_u  (max_nth)= 0 &
     , nbr_failed_astest_u (max_nth)= 0 &
     , nbr_failed_grtest_u (max_nth)= 0 &
     , nbr_failed_netest_u (max_nth)= 0 &
     , nbr_zero_benefit_u  (max_nth)= 0 &

     , medexp_impute_u(num_medimp_cases) = 0 

    real (8) ::                              &
       nbr_head_lt_age15_w (max_nth) = 0.0   &
     , nbr_kid_only_w      (max_nth) = 0.0   &
     , nbr_unknown_age_w   (max_nth) = 0.0   &
     , nbr_weird_depded_w  (max_nth) = 0.0   &
     , nbr_failed_astest_w (max_nth) = 0.0   &
     , nbr_failed_grtest_w (max_nth) = 0.0   &
     , nbr_failed_netest_w (max_nth) = 0.0   &
     , nbr_zero_benefit_w  (max_nth) = 0.0   &
                                           
     , medexp_impute_w(num_medimp_cases) = 0.0 


    integer ::  iunit, ip, i, icase

    logical ::  has_unknown_age

    INTEGER :: nbr_guam(max_nth) = 0
    INTEGER :: nbr_vi(max_nth) = 0
    INTEGER :: nbr_hi(max_nth) = 0
    INTEGER :: nbr_ak(max_nth) = 0
    INTEGER :: nbr_ca(max_nth) = 0
    INTEGER :: nbr_fl(max_nth) = 0
    INTEGER :: nbr_mn(max_nth) = 0
    INTEGER :: nbr_ms(max_nth) = 0
    INTEGER :: nbr_ny(max_nth) = 0
    INTEGER :: nbr_pa(max_nth) = 0
    INTEGER :: nbr_mn_fip(max_nth) = 0
    integer :: nbr_exfscsded(max_nth) = 0 
    integer :: nbr_fscspded(max_nth) = 0 
    integer :: nbr_fscspded_passes(max_nth) = 0 
    integer :: nbr_sheltzero(max_nth) = 0                                      
                                             
    
    if (keof== 3) goto 300

    !-----------------------------------------------------------------------------------------
    !----- KEOF = 2 
    !-----------------------------------------------------------------------------------------

    i = state_idx(l_state%ihhld)
    if (l_state%ihhld==66) i=52  !! guam
    if (l_state%ihhld==78) i=53  !! VI


    if (i == 52) then
       nbr_guam(nth) = nbr_guam(nth)  + 1
       if (nbr_guam(nth) <= debugnbr .AND. prlevel(nth) >= no_loop_prlevel)  then
          call debug_msg("Guam unit", nbr_guam(nth) )
       end if
    elseif (i == 53) then
       nbr_vi(nth)  = nbr_vi(nth)  + 1
       if (nbr_vi(nth) <= debugnbr .AND. prlevel(nth) >= no_loop_prlevel) then
          call debug_msg("Virgin Is. unit", nbr_vi(nth) )
       end if
    elseif (i == 15) then
       nbr_hi(nth)  = nbr_hi(nth)  + 1
       if (nbr_hi(nth) <= debugnbr .AND. prlevel(nth) >= no_loop_prlevel) then
          call debug_msg("Hawaii unit", nbr_hi(nth) )
       end if
    elseif (i == 02) then
       nbr_ak(nth)  = nbr_ak(nth)  + 1
       if (nbr_ak(nth) <= debugnbr .AND. prlevel(nth) >= no_loop_prlevel) then
          call debug_msg("Alaska unit", nbr_ak(nth) )
       end if 
       
    elseif (i == 06) then
       nbr_ca(nth)  = nbr_ca(nth)  + 1
       if (nbr_ca(nth) <= debugnbr .AND. prlevel(nth) >= no_loop_prlevel) then
          call debug_msg("California unit", nbr_ca(nth) )
       end if 
       
    elseif (i == 12) then
       nbr_fl(nth)  = nbr_fl(nth)  + 1
       if (nbr_fl(nth) <= debugnbr .AND. prlevel(nth) >= no_loop_prlevel) then
          call debug_msg("Florida unit", nbr_fl(nth) )
       end if 
       
    elseif (i == 27) then
       nbr_mn(nth)  = nbr_mn(nth)  + 1
       if (nbr_mn(nth) <= debugnbr .AND. prlevel(nth) >= no_loop_prlevel) then
          call debug_msg("Minnesota unit", nbr_mn(nth) )
       end if 
       
    elseif (i == 28) then
       nbr_ms(nth)  = nbr_ms(nth)  + 1
       if (nbr_ms(nth) <= debugnbr .AND. prlevel(nth) >= no_loop_prlevel) then
          call debug_msg("Mississippi unit", nbr_ms(nth) )
       end if 
       
    elseif (i == 42) then
       nbr_pa(nth)  = nbr_pa(nth)  + 1
       if (nbr_pa(nth) <= debugnbr .AND. prlevel(nth) >= no_loop_prlevel) then
          call debug_msg("Pennsylvania unit", nbr_pa(nth) )
       end if 
       
    elseif (i == 36) then
       nbr_ny(nth)  = nbr_ny(nth)  + 1
       if (nbr_ny(nth) <= debugnbr .AND. prlevel(nth) >= no_loop_prlevel) then
          call debug_msg("New York unit", nbr_ny(nth) )
       end if 
    end if

    if (l_mn_fip%iHHLD == 1) then
       nbr_mn_fip(nth) = nbr_mn_fip(nth)  + 1
       if (nbr_mn_fip(nth) <= debugnbr .AND. prlevel(nth) >= no_loop_prlevel)  then
          call debug_msg("MN FIP unit", nbr_mn_fip(nth) )
       end if
    end if


    !! Pure pa:
    if (l_pure_pa%iHHLD /= pure_pa_flag) then
       nbr_pure_pa_diff(nth) = nbr_pure_pa_diff(nth) + 1
       if (nbr_pure_pa_diff(nth) <= debugnbr .AND. prlevel(nth) >= no_loop_prlevel) then
          call debug_msg("PURE_PA diff", nbr_pure_pa_diff(nth))
       end if
    end if
        
    !! CSP expenses
    if (l_exfscsded%ihhld > 0) then
       nbr_exfscsded(nth)  = nbr_exfscsded(nth) + 1
       if (nbr_exfscsded(nth) <= debugnbr .AND. prlevel(nth) >= no_loop_prlevel) then
          call debug_msg("Unit with EXFSCSDED", nbr_exfscsded(nth))     
       end if    
    end if    
    
       

    !-------------------------------------------------------------------------
    !---- Other debug counts over FSUs of this NTH
    !-------------------------------------------------------------------------

    do iunit = 1, ctprhh
 
        if (fsun(iunit) /= iunit) cycle  ! unit does not exist
 
        has_unknown_age = .false.
 
        do ip = 1, ctprhh
          if (fsun(ip) /= iunit) cycle
 
          if (l_age%iper(ip) <  0) has_unknown_age = .true.
        enddo
 
        select case (l_age%iper(iunit))
          case(0:15)
            nbr_head_lt_age15_u(nth) = nbr_head_lt_age15_u(nth) + 1
            nbr_head_lt_age15_w(nth) = nbr_head_lt_age15_w(nth) + wgt
        end select
 
        if (fsnkid(iunit) == fsusize(iunit) ) then
           nbr_kid_only_u(nth) = nbr_kid_only_u(nth) + 1
           nbr_kid_only_w(nth) = nbr_kid_only_w(nth) + wgt
        endif
 
        if (has_unknown_age) then
           nbr_unknown_age_u(nth) = nbr_unknown_age_u(nth) + 1
           nbr_unknown_age_w(nth) = nbr_unknown_age_w(nth) + wgt
        endif

        if (ctprhh == fsnkid(iunit) .and. orig_fsdepded > 0) then  
           nbr_weird_depded_u(nth) = nbr_weird_depded_u(nth) + 1
           nbr_weird_depded_w(nth) = nbr_weird_depded_w(nth) + wgt
        endif
 
        if (fsastest(iunit) == 0) then
           nbr_failed_astest_u(nth) = nbr_failed_astest_u(nth) + 1
           nbr_failed_astest_w(nth) = nbr_failed_astest_w(nth) + wgt
        endif
 
        if (fsgrtest(iunit) == 0) then
           nbr_failed_grtest_u(nth) = nbr_failed_grtest_u(nth) + 1
           nbr_failed_grtest_w(nth) = nbr_failed_grtest_w(nth) + wgt
        endif
 
        if (fsnetest(iunit) == 0) then
           nbr_failed_netest_u(nth) = nbr_failed_netest_u(nth) + 1
           nbr_failed_netest_w(nth) = nbr_failed_netest_w(nth) + wgt
        endif

        if (fsben(iunit) == 0) then
           nbr_zero_benefit_u(nth) = nbr_zero_benefit_u(nth) + 1
           nbr_zero_benefit_w(nth) = nbr_zero_benefit_w(nth) + wgt
        endif
 
       if (fscspded(iunit) > 0) then
          nbr_fscspded(nth)  = nbr_fscspded(nth) + 1
          if (nbr_fscspded(nth) <= debugnbr .AND. prlevel(nth) >= no_loop_prlevel) then
             call debug_msg("Unit w/ FSCSPDED", nbr_fscspded(nth))     
          end if
       end if 
       
       if (fssltexp(iunit) == 0) then
          nbr_sheltzero(nth)  = nbr_sheltzero(nth) + 1
          if (nbr_sheltzero(nth) <= debugnbr .AND. prlevel(nth) >= no_loop_prlevel) then
             call debug_msg("Unit with Zero Shelter Expense", nbr_sheltzero(nth))     
          end if       
          
       if (fsgrinc(iunit) > gross_screen(iunit) .and. fsgrtest(iunit) == 1) then
             nbr_fscspded_passes(nth)  = nbr_fscspded_passes(nth) + 1
             if (nbr_fscspded_passes(nth) <= debugnbr .AND. prlevel(nth) >= no_loop_prlevel) then
                call debug_msg("Unit w/ FSCSPDED passes gross test", nbr_fscspded_passes(nth))     
             end if 
          end if  
       end if

    enddo


    !---------------------------------------------------------------------------
    !---- Counts describing imputation of dependent care and medical expenses
    !---- when unit composition is not the original FSU composition.
    !---------------------------------------------------------------------------
    if (nth > 1) return
 
    !---- Medical expenses imputation
 
    if (orig_fsmedexp <= 0 ) then
      icase = 1
    else if (orig_fsnelder > 0) then
      icase = 2
    else if (orig_fsndis   > 0) then
      icase = 3
    else 
      icase = 4
    endif
 
    medexp_impute_u(icase) = medexp_impute_u(icase) + 1
    medexp_impute_w(icase) = medexp_impute_w(icase) + wgt


    return


    !-----------------------------------------------------------------------------------------
    !----- KEOF = 3 
    !-----------------------------------------------------------------------------------------
300 continue

    if (nth == 1) then
        call isnewpg(prfile, page_break_numlines + 11)  
        write(prfile,1000)                           &
       (medexp_impute_u(i)                           &
      , medexp_impute_w(i), i = 1, num_medimp_cases) 


    endif
 

 
    !---------------------------------------------------------------------
    !---- Print db-specific debug counts for this NTH
    !---------------------------------------------------------------------
    call isnewpg(prfile, page_break_numlines + 23)
    write(prfile,1200)           &
      nth                        &
     ,nbr_head_lt_age15_u (nth)  &
     ,nbr_head_lt_age15_w (nth)  &
     ,nbr_kid_only_u      (nth)  &
     ,nbr_kid_only_w      (nth)  &
     ,nbr_unknown_age_u   (nth)  &
     ,nbr_unknown_age_w   (nth)  &
     ,nbr_weird_depded_u  (nth)  &
     ,nbr_weird_depded_w  (nth)  &
     ,nbr_failed_astest_u (nth)  &
     ,nbr_failed_astest_w (nth)  &
     ,nbr_failed_grtest_u (nth)  &
     ,nbr_failed_grtest_w (nth)  &
     ,nbr_failed_netest_u (nth)  &
     ,nbr_failed_netest_w (nth)  &
     ,nbr_zero_benefit_u  (nth)  &
     ,nbr_zero_benefit_w  (nth) 

 1200 FORMAT (                                                    &
       10X, 30X, 'DATABASE-SPECIFIC DEBUG COUNTS FOR NTH:', I3    &
      /10X, 30X, '------------------------------------------'     &
      /10X, 37X, 'Unweighted', 11X, 'Weighted'                    &
      /10X, 'FSUs w/head under age 15:              ', I8, F20.0  &
      /10X, 'FSUs w/children only:                  ', I8, F20.0  &
      /10X, 'FSUs w/a person with unknown age:      ', I8, F20.0  &
      /10X, 'FSUs w/only children in HH & DEPDED >0:', I8, F20.0  &  
      /10X, 'FSUs that failed asset income test:    ', I8, F20.0  &
      /10X, 'FSUs that failed gross income test:    ', I8, F20.0  &
      /10X, 'FSUs that failed net income test:      ', I8, F20.0  &
      /10X, 'FSUs with zero benefit:                ', I8, F20.0  &
       )


    call isnewpg(prfile, 5)
    write(prfile,1201)           &
      nbr_pure_pa_diff  (nth)

 1201 FORMAT (                                                    &
     //10X, 'FSUs w/ PURE_PA difference:            ', I8         &
     )

    return

!--------------------------------------------------------------------
! FORMAT STATEMENTS
!--------------------------------------------------------------------
 1000 FORMAT (                                                          & 
      10X, 33X, 'DATABASE-SPECIFIC DEBUG COUNTS FOR IMPUTATION'         &
     /10X, 33X, 'OF EXPENSES USING EXPENSES OF THE ORIGINAL FSU'        &
     /10X, 33X, '----------------------------------------------'        &
     /10X, 44X, 'Unweighted',  4X, 'Weighted'                           &
     /10X, 'No medical expenses:                          ', I8, F13.0  &
     /10X, 'Med expenses split using # of elderly:        ', I8, F13.0  &
     /10X, 'Med expenses split using # of disabled:       ', I8, F13.0  &
     /10X, 'Med expenses, but no medical exp deduction:   ', I8, F13.0  &
       )     


    end
