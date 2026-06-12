!**************************************************************************************************
! Source File:  DBFSU.F90                   
! Called By:    FSTAMP2                    
!
! Creates food stamp units by assigning FSUN for each person in the household. 
! There can be more than one food stamp unit in a household.  The value of 
! FSUN is the person number of the head of the food stamp unit.
!
!
!**************************************************************************************************
    subroutine db_fs_unit

    use global
    use fssizes
    use fslocs
    use fswork
    use fs_dbwork, only : fsnongr
    use fs_dblocs
    use fs_dbparm
    implicit none

    integer:: ip, jp

    !--------------------------------------------------------------------
    ! BEGIN PROCESSING
    !--------------------------------------------------------------------
    if (keof ==3) return
    if (keof ==1) return


    !--- set initial unit to the baselaw unit 
    do ip = 1, ctprhh
       fsun(ip)   = l_fsun(1, kist)%iper(ip)
       cashot(ip) = 0  
       ftstud(ip) = 0  
       fsnongr(ip) = l_fsnongr%iper(ip)
    enddo

    !---------------------------------------------------
    !  INSERT REFORM CODE HERE
    !---------------------------------------------------
    
    
    !--- Identifying units that no longer have a head
    !--- due to a reform and assigning them a new head
    do ip = 1,ctprhh
       if (fsun(ip) == 0) cycle
       if (fsun(fsun(ip)) /= fsun(ip)) then
          !! found the first person who is in a unit without a head, so start
          !! looping at the next person (ip+1) to assign a new head
          do jp = ip+1,ctprhh
             if (fsun(jp) == fsun(ip)) fsun(jp) = ip
          enddo  !jp loop
          fsun(ip) = ip   !! now we go back and assign the new head's fsun
       endif
    enddo  !ip loop


    !--- Identify potentially eligible households
    potentially_elig_hh = .false.
    do ip = 1, ctprhh
       if (fsun(ip) > 0)  potentially_elig_hh = .true.
    enddo

    
    return
    end
