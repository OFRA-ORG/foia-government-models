

 subroutine fs_table1_x()
    USE global
    USE globparm
    USE USERparm, ONLY : dostate
    USE states
    USE fsparm, only : planname, plannbr
    USE fssizes, only : max_nth
    USE fslocs
    implicit none

    integer :: ist, iunit, p, icat, ref_idx
    INTEGER :: nplans = 0

    LOGICAL :: base_none, base_some, base_all
    LOGICAL :: refm_none, refm_some, refm_all

    INTEGER, DIMENSION(TOTSTATES) :: &
     num_base_elig   &
    ,num_base_part   &
    ,num_refm_elig   &
    ,num_refm_part

    INTEGER :: &
     nbr_base_elig   &
    ,nbr_base_part   &
    ,nbr_refm_elig   &
    ,nbr_refm_part

    INTEGER :: elig_idx, part_idx

    INTEGER, parameter :: num_cat = 10

    INTEGER, DIMENSION(num_cat, max_nth) :: nbr_eligs = 0
    INTEGER, DIMENSION(num_cat, max_nth) :: nbr_parts = 0

    CHARACTER(LEN=28), DIMENSION(num_cat) :: cat_label = (/  &
      "None ==> All                "   &  ! 1
     ,"All  ==> None               "   &  ! 2
     ,"None ==> Some               "   &  ! 3
     ,"Some ==> None               "   &  ! 4 
     ,"Some ==> All                "   &  ! 5
     ,"All  ==> Some               "   &  ! 6
     ,"Some ==> More (but not all) "   &  ! 7
     ,"Some ==> Less (but not none)"   &  ! 8
     ,"Some or All ==> No Change   "   &  ! 9
     ,"None in Baselaw and Reform  "   /) ! 10

     select case (keof)
        case (2)
           call Process_hh
        case (3)
           call Print_fstab
        case default
     end select
     return
     
  contains
     subroutine Process_hh
        if (dostate == 1) RETURN

        if (nth > nplans) nplans = nth

        ref_idx = nth + 1

        !!  COUNT UP ELIG & PARTIC HHLDS FOR EACH PSUEUDO-STATE
        num_base_elig = 0
        num_base_part = 0
        num_refm_elig = 0
        num_refm_part = 0

        do ist = 1, nstates
           !!  BASELAW
           do iunit = 1, ctprhh
              if (l_fsun(1, ist)%iper(iunit) /= iunit) cycle

              IF (l_fsben(1, ist)%iper(iunit) > 0) THEN
                num_base_elig(ist) = num_base_elig(ist) + 1
                 IF (l_fspart(1, ist)%iper(iunit) == 1) THEN
                    num_base_part(ist) = num_base_part(ist) + 1
                 end if
              end if
           end do

           !!  reform
           do iunit = 1, ctprhh
              if (l_fsun(ref_idx, ist)%iper(iunit) /= iunit) cycle

              IF (l_fsben(ref_idx, ist)%iper(iunit) > 0) THEN
                num_refm_elig(ist) = num_refm_elig(ist) + 1
                 IF (l_fspart(ref_idx, ist)%iper(iunit) == 1) THEN
                    num_refm_part(ist) = num_refm_part(ist) + 1
                 end if
              end if
           end do

        end do

        !!  collapse units into hhld counts:
        nbr_base_elig = 0
        nbr_base_part = 0
        nbr_refm_elig = 0
        nbr_refm_part = 0
        do ist = 1, nstates
           if (num_base_elig(ist) > 0) then
             nbr_base_elig = nbr_base_elig + 1
           end if

           if (num_base_part(ist) > 0) nbr_base_part = nbr_base_part + 1
           if (num_refm_elig(ist) > 0) nbr_refm_elig = nbr_refm_elig + 1
           if (num_refm_part(ist) > 0) nbr_refm_part = nbr_refm_part + 1
        end do

        !! elig counts
        base_none = .FALSE.
        base_some = .false.
        base_all  = .false.
        select case (nbr_base_elig)
           case (0)
              base_none = .true.
           case (51)
              base_all = .true.
           case default
              base_some = .true.
        end select

        refm_none = .FALSE.
        refm_some = .false.
        refm_all  = .false.
        select case (nbr_refm_elig)
           case (0)
              refm_none = .true.
           case (51)
              refm_all = .true.
           case default
              refm_some = .true.
        end select

        !!  eligs category
        if (base_none .and.refm_all) then
           elig_idx = 1
        elseif (base_all  .and. refm_none) then
           elig_idx = 2
         
        elseif (base_none .and. refm_some) then
           elig_idx = 3
        elseif (base_some .and. refm_none) then
           elig_idx = 4
           
        elseif (base_some .AND. refm_all ) then
           elig_idx = 5
        elseif (base_all  .and. refm_some) then
           elig_idx = 6

        elseif (base_some .and. (nbr_refm_elig > nbr_base_elig .AND. .not. refm_all )) then
           elig_idx = 7
        elseif (base_some .and. (nbr_refm_elig < nbr_base_elig .AND. .not. refm_none)) then
           elig_idx = 8

        elseif (nbr_base_elig  > 0 .and. nbr_base_elig == nbr_refm_elig) then
           elig_idx = 9
        else
           elig_idx = 10 !!  "other"  
        end if

        nbr_eligs(elig_idx, nth) = nbr_eligs(elig_idx, nth) + 1

        !! part counts
        base_none = .FALSE.
        base_some = .false.
        base_all  = .false.
        select case (nbr_base_part)
           case (0)
              base_none = .true.
           case (51)
              base_all = .true.
           case default
              base_some = .true.
        end select

        refm_none = .FALSE.
        refm_some = .false.
        refm_all  = .false.
        select case (nbr_refm_part)
           case (0)
              refm_none = .true.
           case (51)
              refm_all = .true.
           case default
              refm_some = .true.
        end select

        !!  parts category
        if (base_none .and.refm_all) then
           part_idx = 1
        elseif (base_all  .and. refm_none) then
           part_idx = 2

        elseif (base_none .and. refm_some) then
           part_idx = 3
        elseif (base_some .and. refm_none) then
           part_idx = 4

        elseif (base_some .AND. refm_all ) then
           part_idx = 5
        elseif (base_all  .and. refm_some) then
           part_idx = 6

        elseif (base_some .and. (nbr_refm_part > nbr_base_part .AND. .not. refm_all )) then
           part_idx = 7
        elseif (base_some .and. (nbr_refm_part < nbr_base_part .AND. .not. refm_none)) then
           part_idx = 8

        elseif (nbr_base_part  > 0 .and. nbr_base_part == nbr_refm_part) then
           part_idx = 9
        else
           part_idx = 10 !!  "other"
        end if

        nbr_parts(part_idx, nth) = nbr_parts(part_idx, nth) + 1
     end subroutine Process_hh
        
     subroutine Print_fstab
        if (nth > 1) return   !!  write all output 1 time
        if (dostate == 1) RETURN

        do p = 1, nplans

           call isnewpg(tabfile, page_break_numlines)
           WRITE(tabfile, 3000) plannbr(p), planname(p)

           do icat = 1, num_cat
              WRITE(tabfile, 3010) cat_label(icat), nbr_eligs(icat, p), nbr_parts(icat, p)
           end do

           WRITE(tabfile, 3020)

        end do

        call isnewpg(tabfile, page_break_numlines)


3000  FORMAT(///, t60, "Table 1a"  &
             ,//, t50, "Change in SIPP Households under Reform"  &
             ,//, t35, "Plan ", a, ": ", a    &
             ,//,t2,131("-")                  &
              ,/, t32, "Eligible  ", t48, "Participating"  &
               ,/, t2, "Unweighted Counts", t32, "Households", t48, "Households  "   &
             /, t2,131("-")               &
               ,//,t2, "Change in status over"   &
               ,/ ,t2, "all simulated states:"   &
              ,//, t2, "Baselaw  Reform"    &
              ,/,  t2, "-------  ------"    )

3010  FORMAT(t2, a, t32, i10, t45, i13)
3020  FORMAT(///)
      end subroutine Print_fstab
    
 end subroutine fs_table1_x
