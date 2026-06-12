!**************************************************************************************************
! Source File:  DBPART.F90                  
! Called By:    FSTAMP2                    
!
! Sets the participation flag.  All eligible units participate.
!
!**************************************************************************************************
    subroutine db_fs_participation
 
    use global
    use fswork
    use fs_dblocs 
    implicit none
    integer::  iunit
 
    !--------------------------------------------------------------------
    ! BEGIN PROCESSING
    !--------------------------------------------------------------------
    select case(keof) !----------------
      case(1,3)       !- do nothing for
        return        !- keof 1 & 3
    end select        !----------------

 
    !--------------------------------------------------------------------
    ! BEGIN PROCESSING - KEOF = 2
    !--------------------------------------------------------------------
    !---- Assign the results to each FSU head (non-heads get 0's)
 
    do iunit = 1, ctprhh  
       fspart(iunit) = 0
       if (fsun (iunit) /= iunit) cycle         !  not the fsu head
       if (fsben(iunit) > 0) fspart(iunit) = 1  !  all eligible units participate
    end do

     
    return
    end
