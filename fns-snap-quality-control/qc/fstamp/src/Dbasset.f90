!**************************************************************************************************
! Source File:  DBASSET.F90                 
! Called By:    FSTAMP2                    
!
!
!
! Modifications:
!
!
!**************************************************************************************************
    subroutine db_fs_asset(iunit)

    USE global
    USE fswork
    USE fsparm
    implicit none

    INTEGER, INTENT(IN) :: iunit

    integer :: eld_idx

    ELD_IDX = ASSET_IDX(IUNIT)

    ASSET_LIMIT(IUNIT) = ASSETLIM(istate, ELD_IDX, NTH)



    return
    end      
