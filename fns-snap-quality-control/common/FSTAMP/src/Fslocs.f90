!**************************************************************************************************
! Source File:  FSLOCS.F90                  
! Called By:    FSTAMP1                     
!
! Adds/locates FSTAMP input/output variables (keof=1).
!
!**************************************************************************************************
    subroutine fs_locate_vars
    use global
    use states
    use locvar_module
    use addvar_module
    use fssizes
    use fslocs
    use fsparm
    use fswork, ONLY : start_kist, end_kist
    implicit none

    integer     :: idx, ist, iist

    character(LEN=3):: st_code
    character(LEN=1):: suffix

    if (super_prlevel >= max_prlevel)  call isnewpg (prfile, page_break_numlines)

    !-------------------------------------------------------------------
    !  ADD FSTAMP output variables for the NTH simulation.
    !--------------------------------------------------------------------
    idx = nth + 1
    suffix = FS_VARS(NTH)


    DO iIST = start_kist, end_kist
       ist = state_order(iist)

       if (model_code == "MSIP") then
          st_code = char_st(ist)
       else
          st_code =  "   "
        end if

       call addvar (l_fsun     (idx, ist), 'FSUN' // st_code // '     ', 1, 'FSTAMP', SUFFIX)
       call addvar (l_fsga     (idx, ist), 'FSGA' // st_code // '     ', 4, 'FSTAMP', SUFFIX)

       call addvar (l_fsssi    (idx, ist), 'FSSSI' // st_code // '    ', 4, 'FSTAMP', SUFFIX)
       call addvar (l_fsben    (idx, ist), 'FSBEN' // st_code // '    ', 4, 'FSTAMP', SUFFIX)

       call addvar (l_cashot   (idx, ist), 'CASHOT' // st_code // '   ', 1, 'FSTAMP', SUFFIX)
       call addvar (l_ftstud   (idx, ist), 'FTSTUD' // st_code // '   ', 1, 'FSTAMP', SUFFIX)
       call addvar (l_fsTANF   (idx, ist), 'FSTANF' // st_code // '   ', 4, 'FSTAMP', SUFFIX)
       call addvar (l_fsearn   (idx, ist), 'FSEARN' // st_code // '   ', 4, 'FSTAMP', SUFFIX)
       call addvar (l_fsndis   (idx, ist), 'FSNDIS' // st_code // '   ', 1, 'FSTAMP', SUFFIX)
       call addvar (l_fsnkid   (idx, ist), 'FSNKID' // st_code // '   ', 1, 'FSTAMP', SUFFIX)
       call addvar (l_fspart   (idx, ist), 'FSPART' // st_code // '   ', 1, 'FSTAMP', SUFFIX)

       call addvar (l_fsusize  (idx, ist), 'FSUSIZE' // st_code // '  ', 1, 'FSTAMP', SUFFIX)
       call addvar (l_fsallpa  (idx, ist), 'FSALLPA' // st_code // '  ', 1, 'FSTAMP', SUFFIX)
       call addvar (l_fsngmom  (idx, ist), 'FSNGMOM' // st_code // '  ', 1, 'FSTAMP', SUFFIX)
       call addvar (l_fsasset  (idx, ist), 'FSASSET' // st_code // '  ', 4, 'FSTAMP', SUFFIX)
       call addvar (l_fsgrinc  (idx, ist), 'FSGRINC' // st_code // '  ', 4, 'FSTAMP', SUFFIX)

       call addvar (l_fsnelder (idx, ist), 'FSNELDER' // st_code // ' ', 1, 'FSTAMP', SUFFIX)
       call addvar (l_fsnk5t17 (idx, ist), 'FSNK5T17' // st_code // ' ', 1, 'FSTAMP', SUFFIX)
       call addvar (l_fsnoncit (idx, ist), 'FSNONCIT' // st_code // ' ', 1, 'FSTAMP', SUFFIX)
       call addvar (l_fsnabawd (idx, ist), 'FSNABAWD' // st_code // ' ', 1, 'FSTAMP', SUFFIX)
       call addvar (l_fsminben (idx, ist), 'FSMINBEN' // st_code // ' ', 1, 'FSTAMP', SUFFIX)
       call addvar (l_fstotded (idx, ist), 'FSTOTDED' // st_code // ' ', 4, 'FSTAMP', SUFFIX)
       call addvar (l_fsstdded (idx, ist), 'FSSTDDED' // st_code // ' ', 4, 'FSTAMP', SUFFIX)
       call addvar (l_fsernded (idx, ist), 'FSERNDED' // st_code // ' ', 4, 'FSTAMP', SUFFIX)
       call addvar (l_fsdepded (idx, ist), 'FSDEPDED' // st_code // ' ', 4, 'FSTAMP', SUFFIX)
       call addvar (l_fsmedded (idx, ist), 'FSMEDDED' // st_code // ' ', 4, 'FSTAMP', SUFFIX)
       call addvar (l_fssltded (idx, ist), 'FSSLTDED' // st_code // ' ', 4, 'FSTAMP', SUFFIX)
       call addvar (l_fsnetinc (idx, ist), 'FSNETINC' // st_code // ' ', 4, 'FSTAMP', SUFFIX)
       call addvar (l_fsastest (idx, ist), 'FSASTEST' // st_code // ' ', 1, 'FSTAMP', SUFFIX)
       call addvar (l_fsgrtest (idx, ist), 'FSGRTEST' // st_code // ' ', 1, 'FSTAMP', SUFFIX)
       call addvar (l_fsnetest (idx, ist), 'FSNETEST' // st_code // ' ', 1, 'FSTAMP', SUFFIX)


    END DO

    !------------------------------------------------------------------------------------
    !--- Locate baselaw variables
    !------------------------------------------------------------------------------------
    if (nth /= 1) return

    suffix = baselaw(1)
    if (baselaw(1) == ' ' ) suffix = fs_vars(1)

    DO iIST = start_kist, end_kist
       ist = state_order(iist)

       if (model_code == "MSIP") then
          st_code = char_st(ist)
       else
          st_code =  "   "
       end if

       call locvar (l_fsun     (1, IST), 'FSUN' // st_code // '     ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsga     (1, IST), 'FSGA' // st_code // '     ' , 9999, 'FSTAMP', SUFFIX)

       call locvar (l_fsssi    (1, IST), 'FSSSI' // st_code // '    ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsben    (1, IST), 'FSBEN' // st_code // '    ' , 9999, 'FSTAMP', SUFFIX)

       call locvar (l_cashot   (1, IST), 'CASHOT' // st_code // '   ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_ftstud   (1, IST), 'FTSTUD' // st_code // '   ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsTANF   (1, IST), 'FSTANF' // st_code // '   ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsearn   (1, IST), 'FSEARN' // st_code // '   ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsndis   (1, IST), 'FSNDIS' // st_code // '   ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsnkid   (1, IST), 'FSNKID' // st_code // '   ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fspart   (1, IST), 'FSPART' // st_code // '   ' , 9999, 'FSTAMP', SUFFIX)

       call locvar (l_fsusize  (1, IST), 'FSUSIZE' // st_code // '  ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsallpa  (1, IST), 'FSALLPA' // st_code // '  ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsngmom  (1, IST), 'FSNGMOM' // st_code // '  ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsasset  (1, IST), 'FSASSET' // st_code // '  ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsgrinc  (1, IST), 'FSGRINC' // st_code // '  ' , 9999, 'FSTAMP', SUFFIX)

       call locvar (l_fsnelder (1, IST), 'FSNELDER' // st_code // ' ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsnk5t17 (1, IST), 'FSNK5T17' // st_code // ' ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsnoncit (1, IST), 'FSNONCIT' // st_code // ' ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsnabawd (1, IST), 'FSNABAWD' // st_code // ' ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsminben (1, IST), 'FSMINBEN' // st_code // ' ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fstotded (1, IST), 'FSTOTDED' // st_code // ' ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsstdded (1, IST), 'FSSTDDED' // st_code // ' ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsernded (1, IST), 'FSERNDED' // st_code // ' ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsdepded (1, IST), 'FSDEPDED' // st_code // ' ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsmedded (1, IST), 'FSMEDDED' // st_code // ' ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fssltded (1, IST), 'FSSLTDED' // st_code // ' ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsnetinc (1, IST), 'FSNETINC' // st_code // ' ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsastest (1, IST), 'FSASTEST' // st_code // ' ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsgrtest (1, IST), 'FSGRTEST' // st_code // ' ' , 9999, 'FSTAMP', SUFFIX)
       call locvar (l_fsnetest (1, IST), 'FSNETEST' // st_code // ' ' , 9999, 'FSTAMP', SUFFIX)

    END DO

    return
    end



    subroutine fs_allocate_generic_vars()
!**************************************************************************************************
! Allocate array sizes for generic variables
!
! Modifications:
!**************************************************************************************************

    USE global
    USE fswork
    USE fssizes
    USE fslocs
    implicit none

    allocate    (l_fsun     (max_nth, 0 : gen_array_size))
    allocate    (l_fsga     (max_nth, 0 : gen_array_size))
    allocate    (l_fsssi    (max_nth, 0 : gen_array_size))
    allocate    (l_fsben    (max_nth, 0 : gen_array_size))
    allocate    (l_cashot   (max_nth, 0 : gen_array_size))
    allocate    (l_ftstud   (max_nth, 0 : gen_array_size))
    allocate    (l_fsTANF   (max_nth, 0 : gen_array_size))
    allocate    (l_fsearn   (max_nth, 0 : gen_array_size))
    allocate    (l_fsndis   (max_nth, 0 : gen_array_size))
    allocate    (l_fsnkid   (max_nth, 0 : gen_array_size))
    allocate    (l_fspart   (max_nth, 0 : gen_array_size))
    allocate    (l_fsusize  (max_nth, 0 : gen_array_size))
    allocate    (l_fsallpa  (max_nth, 0 : gen_array_size))
    allocate    (l_fsngmom  (max_nth, 0 : gen_array_size))
    allocate    (l_fsasset  (max_nth, 0 : gen_array_size))
    allocate    (l_fsgrinc  (max_nth, 0 : gen_array_size))
    allocate    (l_fsnelder (max_nth, 0 : gen_array_size))
    allocate    (l_fsnk5t17 (max_nth, 0 : gen_array_size))
    allocate    (l_fsnoncit (max_nth, 0 : gen_array_size))
    allocate    (l_fsnabawd (max_nth, 0 : gen_array_size))
    allocate    (l_fsminben (max_nth, 0 : gen_array_size))
    allocate    (l_fstotded (max_nth, 0 : gen_array_size))
    allocate    (l_fsstdded (max_nth, 0 : gen_array_size))
    allocate    (l_fsernded (max_nth, 0 : gen_array_size))
    allocate    (l_fsdepded (max_nth, 0 : gen_array_size))
    allocate    (l_fsmedded (max_nth, 0 : gen_array_size))
    allocate    (l_fssltded (max_nth, 0 : gen_array_size))
    allocate    (l_fsnetinc (max_nth, 0 : gen_array_size))
    allocate    (l_fsastest (max_nth, 0 : gen_array_size))
    allocate    (l_fsgrtest (max_nth, 0 : gen_array_size))
    allocate    (l_fsnetest (max_nth, 0 : gen_array_size))

    end subroutine



    subroutine fs_deallocate_generic_vars()
!-----------------------------------------------------------------------
!  Deallocate generic variables
!-----------------------------------------------------------------------
    USE global
    USE fslocs
    implicit none

 
    end subroutine

