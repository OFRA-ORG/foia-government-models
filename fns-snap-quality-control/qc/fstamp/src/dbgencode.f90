!**************************************************************************************************
! Source File:  DBGENCODE.F90               
!
! Db-specific calls from generic code
! Dummy routines for other generic calls 
!
 !**************************************************************************************************
    subroutine db_fs_set_fsgrtest(iunit)
    !-----------------------------------------------------------------------
    !  Recompute gross income test 
    !-----------------------------------------------------------------------
    USE global
    USE fswork
    USE fs_dbwork, only : fsnongr
    implicit none

    INTEGER, INTENT(IN) :: iunit 
    
    !! recompute FSGRTEST for units with CSP expenses:
    if (fscspded(iunit) > 0 .and. fsgrinc(iunit) - fscspded(iunit) <= GROSS_SCREEN(IUNIT)) then
       FSGRTEST(IUNIT) = 1
       APPLY_EFFECTIVE_GROSS_INCOME (IUNIT) = .TRUE.
    end if

    !! mark units with an elderly/disabled household member who was removed from unit for specific reasons
    !! as passing the gross income test (the fsnongr variable is constructed in tally when looping through all hh members)    
    if (fsnongr(iunit) == 1) then 
        FSGRTEST(IUNIT) = 1
        APPLY_EFFECTIVE_GROSS_INCOME (IUNIT) = .TRUE. 
    end if   

    return
    end


    subroutine db_fs_save_generic_vars (return_code)
    !-----------------------------------------------------------------------
    !  For QC Minimodel, no processing
    !-----------------------------------------------------------------------
    USE global
    implicit none

    INTEGER, INTENT(IN out) :: return_code

    return_code = 1

    return
    end subroutine


    subroutine db_fs_calc_liheap ()
    !-----------------------------------------------------------------------
    !  For QC Minimodel, no processing
    !-----------------------------------------------------------------------
    USE global
    implicit none


    return
    end subroutine



    subroutine db_fs_display_summ_debug()
    !-----------------------------------------------------------------------
    !  For QC Minimodel, no processing
    !-----------------------------------------------------------------------
    return
    end subroutine


    
    subroutine db_fs_table_b()
    !-----------------------------------------------------------------------
    !  For QC Minimodel, no processing
    !-----------------------------------------------------------------------
    return
    end subroutine


    subroutine db_fs_prob_distr_tab()
    !-----------------------------------------------------------------------
    !  For QC Minimodel, no processing
    !-----------------------------------------------------------------------
    return
    end subroutine


    subroutine db_fs_calc_categ_elig()
    !-----------------------------------------------------------------------
    !  For QC Minimodel, no processing
    !-----------------------------------------------------------------------
    return
    end subroutine

    
    subroutine db_fs_display_partic_debug
    !-----------------------------------------------------------------------
    !  For QC Minimodel, no processing
    !-----------------------------------------------------------------------
    return
    end      



    subroutine db_fs_calc_ben_post ()
    !-----------------------------------------------------------------------
    ! Recompute benefit for LIHEAP households  
    !-----------------------------------------------------------------------

    return
    end
    
