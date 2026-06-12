!**************************************************************************************************      
! Called By:    FSTABLES                    
!
!
!
!**************************************************************************************************
    module stat_summ_module
    USE global
    implicit none

    !! summary stats
    real(8), ALLOCATABLE, dimension(:, :, :, :) :: total_summ
    real(8), ALLOCATABLE, dimension(:, :, :, :) :: ptot_summ
    real(8), ALLOCATABLE, dimension(:, :, :, :) :: tot_delta_summ
    real(8), ALLOCATABLE, dimension(:, :, :, :, :) :: sum_delta_summ
    real(8), ALLOCATABLE, dimension(:, :, :, :) :: var_summ
    real(8), ALLOCATABLE, dimension(:, :, :, :) :: sd_summ
    real(8), ALLOCATABLE, dimension(:, :, :, :) :: z1_summ
    real(8), ALLOCATABLE, dimension(:,    :, :) :: z2_summ

    !! benefit stats
    real(8), ALLOCATABLE, dimension(:, :, :, :) :: total_ben
    real(8), ALLOCATABLE, dimension(:, :, :, :) :: ptot_ben
    real(8), ALLOCATABLE, dimension(:, :, :, :) :: tot_delta_ben
    real(8), ALLOCATABLE, dimension(:, :, :, :, :) :: sum_delta_ben
    real(8), ALLOCATABLE, dimension(:, :, :, :) :: var_ben
    real(8), ALLOCATABLE, dimension(:, :, :, :) :: sd_ben
    real(8), ALLOCATABLE,dimension (:, :, :, :) :: z1_ben
    real(8), ALLOCATABLE,dimension (:,    :, :) :: z2_ben


    integer, ALLOCATABLE,dimension(:, :, :, :) :: base_est
    integer, ALLOCATABLE,dimension(:, :, :, :) :: ref_est

    REAL,    ALLOCATABLE,dimension(:, :, :, :) :: base_ben
    REAL,    ALLOCATABLE,dimension(:, :, :, :) :: ref_ben

    end module




    SUBROUTINE FS_STATS_SUMMARY( )

    USE GLOBAL
    USE USERPARM

    USE fsparm, only : dostats,glunit
    USE fslocs


    USE fswork, only :    &
       TAB_summ_Sd        &
     , TAB_summ_Sd_ben    &
     , TAB_summ_STATs     &
     , TAB_summ_STATs_BEN &
     , sig_level_90       &
     , sig_level_95       &
     , sig_level_99       &
     , VAR_protect_summ_STATS   &
     , VAR_protect_ben_STATS    &
     , FSNMALE           &
     , FSNFEMALE         &
     , FSHRACE           &
     , FSHETHNIC         &
     , FSHORIGIN         &
     , base_FSNMALE      &
     , base_FSNFEMALE    &
     , base_FSHRACE      &
     , base_FSHETHNIC    &
     , base_FSHORIGIN    &
     , wgt               &
     , num_wgt           &
     , num_pcts          &
     , var_const         &
     , b_wgt


    use stat_summ_module

    implicit none

    INTEGER :: i, iunit, ref_idx, ib, j, k, kk, m, n, p, gl_ctprhh
    INTEGER :: t
    INTEGER :: part_elig_start_idx, part_elig_end_idx

    integer, parameter :: max_nth2 = 5

    INTEGER :: nplan10 = 0


    INTEGER :: fsize    &
       ,fkid            &
       ,felder          &
       ,fadult          &
       ,fdis            &
       ,sum_male        &
       ,sum_female

     integer ::    &
       h_origin    &
      ,h_race      &
      ,h_ethnic    &
      ,num_units

     integer :: base_fsben
     REAL :: pc_base_fsben
     integer :: fsben
     REAL :: pc_fsben



    LOGICAL :: first_call = .true.
    LOGICAL :: base_elig, base_part, ref_elig, ref_part

    character(len=15), dimension(30) :: stat_label = (/ &
       "Units          "    &  !   1

      ,"w/ Kids        "    &  !   2
      ,"w/ Adults      "    &  !   3
      ,"w/ Elderly     "    &  !   4

      ,"h/ white       "    &  !   5
      ,"h/ black       "    &  !   6
      ,"h/ hisp        "    &  !   7
      ,"h/ asian       "    &  !   8
      ,"h/ other       "    &  !   9
      ,"h/ unknown     "    &  !  10

      ,"h/ hisp        "    &  !  11
      ,"h/ not_his     "    &  !  12

      ,"w/ disab       "    &  !  13
      ,"w/o disab      "    &  !  14

      ,"N America      "    &  !  15
      ,"Europe         "    &  !  16
      ,"Asia           "    &  !  17
      ,"Mideast        "    &  !  18
      ,"Cent/S America "    &  !  19
      ,"Africa         "    &  !  20
      ,"Elsewhere      "    &  !  21
      ,"Unknown        "    &  !  22

      ,"Pers           "    &  !  23

      ,"Kids           "    &  !  24
      ,"Adults         "    &  !  25
      ,"Elderly        "    &  !  26

      ,"Males          "    &  !  27
      ,"Females        "    &  !  28

      ,"Disabled       "    &  !  29
      ,"Not Disabled   "    &  !  30

      /)

    character(len=05), dimension(2) :: stat_label2 = (/"ELIG ", "PART "/)
    character(len=20), dimension(2) :: sub_label = (/  &
      "(In the Subset)     "   &
     ,"(NOT in the Subset) "   &
     /)




    if (first_call) then


       first_call = .false.

       ALLOCATE(total_summ      (           num_pcts, 2, 2, 0:max_nth2))
       ALLOCATE(ptot_summ       (           num_pcts, 2, 2,   max_nth2))
       ALLOCATE(tot_delta_summ  (           num_pcts, 2, 2,   max_nth2))
       ALLOCATE(sum_delta_summ  (0:num_wgt, num_pcts, 2, 2,   max_nth2))
       ALLOCATE(var_summ       (num_pcts, 2, 2,   max_nth2))
       ALLOCATE(sd_summ        (num_pcts, 2, 2,   max_nth2))
       ALLOCATE(z1_summ        (num_pcts, 2, 2,   max_nth2))
       ALLOCATE(z2_summ        (num_pcts, 2, max_nth2))

       ALLOCATE(base_est    (max_persons, num_pcts, 2, 2))
       ALLOCATE(ref_est     (max_persons, num_pcts, 2, 2))

       ALLOCATE(total_ben      (        num_pcts, 2, 2, 0:max_nth2))
       ALLOCATE(ptot_ben       (        num_pcts, 2, 2,   max_nth2))
       ALLOCATE(tot_delta_ben  (        num_pcts, 2, 2,   max_nth2))
       ALLOCATE(sum_delta_ben  (0:num_wgt, num_pcts, 2, 2,   max_nth2))
       ALLOCATE(var_ben       (num_pcts, 2, 2,   max_nth2))
       ALLOCATE(sd_ben        (num_pcts, 2, 2,   max_nth2))
       ALLOCATE(z1_ben        (num_pcts, 2, 2,   max_nth2))
       ALLOCATE(z2_ben        (num_pcts, 2,  max_nth2))

       ALLOCATE(base_ben    (max_persons, num_pcts, 2, 2))
       ALLOCATE(ref_ben     (max_persons, num_pcts, 2, 2))


       !!  initialize allocated arrays:
       total_summ     = 0.0
       ptot_summ      = 0.0
       tot_delta_summ = 0.0
       sum_delta_summ = 0.0
       var_summ       = 0.0
       sd_summ        = 0.0
       z1_summ        = 0.0
       z2_summ        = 0.0

       total_ben      = 0.0
       ptot_ben       = 0.0
       tot_delta_ben  = 0.0
       sum_delta_ben  = 0.0
       var_ben        = 0.0
       sd_ben         = 0.0
       z1_ben         = 0.0
       z2_ben         = 0.0


    end if


    IF (KEOF == 3) GOTO 900


    if (.NOT. dostats(nth)) return


    !------- count up esimates for baselaw plan--------------------------------
    if (NTH > nplan10) nplan10 = NTH

    ref_idx = nth+1

    base_est = 0
    ref_est = 0
    base_ben = 0.0
    ref_ben = 0.0


    if (glunit == 2) then

       do iunit = 1, ctprhh


          base_fsben = 0
          pc_base_fsben = 0.0
          fsben = 0
          pc_fsben = 0.0

          if (l_fsun(1, kist)%iper(iunit) /= iunit) cycle
          !! base elig
          if (l_fsben(1, kist)%iper(iunit) > 0) THEN
             !! if receiving benefits in baseline, then the fill index starts at 1
             part_elig_start_idx = 1
             part_elig_end_idx = 1
             !! if receiving benefits in baseline and you are participating, then the fill index ends at 2
             if (l_fspart(1, kist)%iper(iunit) == 1) part_elig_end_idx = 2

             fsize = l_fsusize(1, kist)%iper(iunit)
             fkid =  l_fsnkid(1, kist)%iper(iunit)
             felder = l_fsnelder(1, kist)%iper(iunit)
             fadult = fsize - fkid - felder
             fdis = l_fsndis(1, kist)%iper(iunit)

             base_fsben = l_fsben(1, kist)%iper(iunit)
             pc_base_fsben = REAL(l_fsben(1, kist)%iper(iunit)) / fsize

             ! units
             base_est(iunit, 1, part_elig_start_idx:part_elig_end_idx, 1) = 1


             if (fkid > 0) then
                base_est(iunit,2,part_elig_start_idx:part_elig_end_idx,1) = 1
             else
                base_est(iunit,2,part_elig_start_idx:part_elig_end_idx,2) = 1
             end if

             if (fadult > 0) then
                base_est(iunit,3,part_elig_start_idx:part_elig_end_idx,1) = 1
             else
                base_est(iunit,3,part_elig_start_idx:part_elig_end_idx,2) = 1
             end if

             if (felder > 0) then
                base_est(iunit,4,part_elig_start_idx:part_elig_end_idx,1) = 1
             else
                base_est(iunit,4,part_elig_start_idx:part_elig_end_idx,2) = 1
             end if

             !! RACE
             do k = 1,6
                j = k+4
                base_est(iunit,j,part_elig_start_idx:part_elig_end_idx,2) = 1
                if (base_fshrace(iunit) == k)  then
                   base_est(iunit,j,part_elig_start_idx:part_elig_end_idx,1) = 1
                   base_est(iunit,j,part_elig_start_idx:part_elig_end_idx,2) = 0
                end if
             end do

             !! ethnic
             if (base_fshethnic(iunit) == 1) THEN
                base_est(iunit,11,part_elig_start_idx:part_elig_end_idx,1) = 1
                base_est(iunit,12,part_elig_start_idx:part_elig_end_idx,2) = 1
             else
                base_est(iunit,11,part_elig_start_idx:part_elig_end_idx,2) = 1
                base_est(iunit,12,part_elig_start_idx:part_elig_end_idx,1) = 1
             end if

             !! DISAB flag
             if (l_fsndis(1, kist)%iper(iunit) > 0) THEN
                base_est(iunit,13,part_elig_start_idx:part_elig_end_idx,1) = 1
                base_est(iunit,14,part_elig_start_idx:part_elig_end_idx,2) = 1
             else
                base_est(iunit,13,part_elig_start_idx:part_elig_end_idx,2) = 1
                base_est(iunit,14,part_elig_start_idx:part_elig_end_idx,1) = 1
             end if

             !!  ORIGIN
             do k = 1,8
                j = k+14
                base_est(iunit,j,1,2) = 1
                if (base_fshorigin(iunit) == k)  then
                   base_est(iunit,j,part_elig_start_idx:part_elig_end_idx,1) = 1
                   base_est(iunit,j,part_elig_start_idx:part_elig_end_idx,2) = 0
                end if
             end do

             !! persons
             base_est(iunit,23,part_elig_start_idx:part_elig_end_idx,1) = fsize

             base_est(iunit,24,part_elig_start_idx:part_elig_end_idx,1) = fkid
             base_est(iunit,25,part_elig_start_idx:part_elig_end_idx,1) = fadult
             base_est(iunit,26,part_elig_start_idx:part_elig_end_idx,1) = felder
             base_est(iunit,24,part_elig_start_idx:part_elig_end_idx,2) = fsize - fkid
             base_est(iunit,25,part_elig_start_idx:part_elig_end_idx,2) = fsize - fadult
             base_est(iunit,26,part_elig_start_idx:part_elig_end_idx,2) = fsize - felder


             base_est(iunit,27,part_elig_start_idx:part_elig_end_idx,1) = base_fsnmale  (iunit)
             base_est(iunit,28,part_elig_start_idx:part_elig_end_idx,1) = base_fsnfemale(iunit)

             base_est(iunit,27,part_elig_start_idx:part_elig_end_idx,2) = base_fsnfemale(iunit)
             base_est(iunit,28,part_elig_start_idx:part_elig_end_idx,2) = base_fsnmale  (iunit)


             base_est(iunit,29,part_elig_start_idx:part_elig_end_idx,1) = l_fsndis(1, kist)%iper(iunit)
             base_est(iunit,30,part_elig_start_idx:part_elig_end_idx,1) = fsize - l_fsndis(1, kist)%iper(iunit)
             base_est(iunit,29,part_elig_start_idx:part_elig_end_idx,2) = fsize - l_fsndis(1, kist)%iper(iunit)
             base_est(iunit,30,part_elig_start_idx:part_elig_end_idx,2) = l_fsndis(1, kist)%iper(iunit)

             if (model_code == "QCMM") THEN
                base_est(iunit,29:30,part_elig_start_idx:part_elig_end_idx,1:2) = 0
             end if

          end if

          !!  reform
          if (l_fsben(ref_idx, kist)%iper(iunit) > 0) THEN
             !! if receiving benefits in reform, then the fill index starts at 1
             part_elig_start_idx = 1
             part_elig_end_idx = 1
             !! if receiving benefits in baseline and you are participating, then the fill index ends at 2
             if (l_fspart(ref_idx, kist)%iper(iunit) == 1) part_elig_end_idx = 2

             fsize = l_fsusize(ref_idx, kist)%iper(iunit)
             fkid =  l_fsnkid(ref_idx, kist)%iper(iunit)
             felder = l_fsnelder(ref_idx, kist)%iper(iunit)
             fadult = fsize - fkid - felder

             fsben = l_fsben(ref_idx, kist)%iper(iunit)
             pc_fsben = REAL(l_fsben(ref_idx, kist)%iper(iunit)) / fsize

             ref_est(iunit, 1,part_elig_start_idx:part_elig_end_idx,1) = 1

             if (fkid > 0) then
                ref_est(iunit,2,part_elig_start_idx:part_elig_end_idx,1) = 1
             else
                ref_est(iunit,2,part_elig_start_idx:part_elig_end_idx,2) = 1
             end if

             if (fadult > 0) then
                ref_est(iunit,3,part_elig_start_idx:part_elig_end_idx,1) = 1
             else
                ref_est(iunit,3,part_elig_start_idx:part_elig_end_idx,2) = 1
             end if

             if (felder > 0) then
                ref_est(iunit,4,part_elig_start_idx:part_elig_end_idx,1) = 1
             else
                ref_est(iunit,4,part_elig_start_idx:part_elig_end_idx,2) = 1
             end if

             !! RACE
             do k = 1,6
                j = k+4
                ref_est(iunit,j,part_elig_start_idx:part_elig_end_idx,2) = 1
                if (fshrace(iunit) == k)  then
                   ref_est(iunit,j,part_elig_start_idx:part_elig_end_idx,1) = 1
                   ref_est(iunit,j,part_elig_start_idx:part_elig_end_idx,2) = 0
                end if
             end do

             !! ethnic
             if (fshethnic(iunit) == 1) THEN
                ref_est(iunit,11,part_elig_start_idx:part_elig_end_idx,1) = 1
                ref_est(iunit,12,part_elig_start_idx:part_elig_end_idx,2) = 1
             else
                ref_est(iunit,11,part_elig_start_idx:part_elig_end_idx,2) = 1
                ref_est(iunit,12,part_elig_start_idx:part_elig_end_idx,1) = 1
             end if

             !! DISAB flag
             if (l_fsndis(ref_idx, kist)%iper(iunit) > 0) THEN
                ref_est(iunit,13,part_elig_start_idx:part_elig_end_idx,1) = 1
                ref_est(iunit,14,part_elig_start_idx:part_elig_end_idx,2) = 1
             else
                ref_est(iunit,13,part_elig_start_idx:part_elig_end_idx,2) = 1
                ref_est(iunit,14,part_elig_start_idx:part_elig_end_idx,1) = 1
             end if

             !!  ORIGIN
             do k = 1,8
                j = k+14
                ref_est(iunit,j,part_elig_start_idx:part_elig_end_idx,2) = 1
                if (fshorigin(iunit) == k)  then
                   ref_est(iunit,j,part_elig_start_idx:part_elig_end_idx,1) = 1
                   ref_est(iunit,j,part_elig_start_idx:part_elig_end_idx,2) = 0
                end if
             end do

             !! Persons
             ref_est(iunit, 23,part_elig_start_idx:part_elig_end_idx,1) = fsize

             ref_est(iunit,24,part_elig_start_idx:part_elig_end_idx,1) = fkid
             ref_est(iunit,25,part_elig_start_idx:part_elig_end_idx,1) = fadult
             ref_est(iunit,26,part_elig_start_idx:part_elig_end_idx,1) = felder
             ref_est(iunit,24,part_elig_start_idx:part_elig_end_idx,2) = fsize - fkid
             ref_est(iunit,25,part_elig_start_idx:part_elig_end_idx,2) = fsize - fadult
             ref_est(iunit,26,part_elig_start_idx:part_elig_end_idx,2) = fsize - felder

             ref_est(iunit,27,part_elig_start_idx:part_elig_end_idx,1) = fsnmale  (iunit)
             ref_est(iunit,28,part_elig_start_idx:part_elig_end_idx,1) = fsnfemale(iunit)
             ref_est(iunit,27,part_elig_start_idx:part_elig_end_idx,2) = fsnfemale(iunit)
             ref_est(iunit,28,part_elig_start_idx:part_elig_end_idx,2) = fsnmale  (iunit)

             ref_est(iunit,29,part_elig_start_idx:part_elig_end_idx,1) = l_fsndis(ref_idx, kist)%iper(iunit)
             ref_est(iunit,30,part_elig_start_idx:part_elig_end_idx,1) = fsize - l_fsndis(ref_idx, kist)%iper(iunit)
             ref_est(iunit,29,part_elig_start_idx:part_elig_end_idx,2) = fsize - l_fsndis(ref_idx, kist)%iper(iunit)
             ref_est(iunit,30,part_elig_start_idx:part_elig_end_idx,2) = l_fsndis(ref_idx, kist)%iper(iunit)

             if (model_code == "QCMM") THEN
                ref_est(iunit,29:30,part_elig_start_idx:part_elig_end_idx,1:2) = 0
             end if

          end if
          !!  benefits for GLUNIT = 2
          do j = 1, num_pcts
             do m = 1,2
                do n = 1,2
                    select case (j)
                        case (1:22) !! unit level
                           base_ben(iunit, j, m, n) = base_est(iunit, j, m, n) * base_fsben
                           ref_ben(iunit, j, m, n) = ref_est(iunit, j, m, n) * fsben
                        case default  !!  person level
                           base_ben(iunit, j, m, n) = base_est(iunit, j, m, n) * pc_base_fsben
                           ref_ben(iunit, j, m, n)  = ref_est(iunit, j, m, n) * pc_fsben
                    end select
                end do
             end do
          end do



       end do  !!  iunit

    else
    !----------------------------------------
    !  GLUNIT = 1
    !----------------------------------------

       !! base elig, glunit = 1
       fsize =  0
       fkid =   0
       felder = 0
       fadult = 0
       fdis = 0
       sum_male = 0
       sum_female = 0
       base_elig = .false.
       base_part = .false.
       h_origin = 0
       h_race = 0
       h_ethnic = 0
       num_units = 0

       base_fsben = 0
       pc_base_fsben = 0.0
       fsben = 0
       pc_fsben = 0.0

       ! Determine baseline eligibility and participation
       do iunit = 1, ctprhh
          if (l_fsun(1, kist)%iper(iunit) /= iunit) cycle

          if (l_fsben(1, kist)%iper(iunit) > 0) THEN
             base_elig = .true.
             fsize = fsize + l_fsusize(1, kist)%iper(iunit)
             fkid =  fkid  + l_fsnkid(1, kist)%iper(iunit)
             felder = felder + l_fsnelder(1, kist)%iper(iunit)
             fdis = fdis + l_fsndis(1, kist)%iper(iunit)
             sum_male = sum_male + base_fsnmale(iunit)
             sum_female = sum_female + base_fsnfemale(iunit)
             num_units = num_units + 1
             if (num_units == 1) then
                h_origin = base_fshorigin(iunit)
                h_race =   base_fshrace(iunit)
                h_ethnic = base_fshethnic(iunit)
             end if

             base_fsben = base_fsben + l_fsben(1, kist)%iper(iunit)

          end if

          if (l_fspart(1, kist)%iper(iunit) == 1) base_part = .true.
       end do

       if (num_units == 0) GOTO 201  !!  no baselaw units: go to reform

       pc_base_fsben = REAL(base_fsben) / fsize

       iunit = 1

       fadult = fsize - (fkid + felder)

       if (.not. base_elig .and. .not. base_part) GOTO 201  !!  nothing to count: go to reform
       ! Logic to determine which indices to update:
       !  elig &  part = 1:2
       ! ~elig &  part = 2:2
       !  elig & ~part = 1:1
       ! ~elig & ~part can't happen (see above if statement)
       part_elig_start_idx = 1
       part_elig_end_idx = 2
       if (.not. base_elig) part_elig_start_idx = 2
       if (.not. base_part) part_elig_end_idx = 1

       ! units
       base_est(iunit, 1,part_elig_start_idx:part_elig_end_idx,1) = 1

       if (fkid > 0) then
          base_est(iunit,2,part_elig_start_idx:part_elig_end_idx,1) = 1
       else
          base_est(iunit,2,part_elig_start_idx:part_elig_end_idx,2) = 1
       end if

       if (fadult > 0) then
          base_est(iunit,3,part_elig_start_idx:part_elig_end_idx,1) = 1
       else
          base_est(iunit,3,part_elig_start_idx:part_elig_end_idx,2) = 1
       end if

       if (felder > 0) then
          base_est(iunit,4,part_elig_start_idx:part_elig_end_idx,1) = 1
      else
          base_est(iunit,4,part_elig_start_idx:part_elig_end_idx,2) = 1
       end if

       !! RACE
       do k = 1,6
          j = k+4
          base_est(iunit,j,part_elig_start_idx:part_elig_end_idx,2) = 1
          if (h_race == k)  then
             base_est(iunit,j,part_elig_start_idx:part_elig_end_idx,1) = 1
             base_est(iunit,j,part_elig_start_idx:part_elig_end_idx,2) = 0
          end if
       end do

       !! ethnic
       if (h_ethnic == 1) THEN
          base_est(iunit,11,part_elig_start_idx:part_elig_end_idx,1) = 1
          base_est(iunit,12,part_elig_start_idx:part_elig_end_idx,2) = 1
       else
          base_est(iunit,11,part_elig_start_idx:part_elig_end_idx,2) = 1
          base_est(iunit,12,part_elig_start_idx:part_elig_end_idx,1) = 1
       end if

       !! DISAB flag
       if (fdis > 0) THEN
          base_est(iunit,13,part_elig_start_idx:part_elig_end_idx,1) = 1
          base_est(iunit,14,part_elig_start_idx:part_elig_end_idx,2) = 1
       else
          base_est(iunit,13,part_elig_start_idx:part_elig_end_idx,2) = 1
          base_est(iunit,14,part_elig_start_idx:part_elig_end_idx,1) = 1
       end if

       !!  ORIGIN
       do k = 1,8
          j = k+14
          base_est(iunit,j,1,2) = 1
          if (h_origin == k)  then
             base_est(iunit,j,part_elig_start_idx:part_elig_end_idx,1) = 1
             base_est(iunit,j,part_elig_start_idx:part_elig_end_idx,2) = 0
          end if
       end do

       ! persons
       base_est(iunit,23,part_elig_start_idx:part_elig_end_idx,1) = fsize

       base_est(iunit,24,part_elig_start_idx:part_elig_end_idx,1) = fkid
       base_est(iunit,25,part_elig_start_idx:part_elig_end_idx,1) = fadult
       base_est(iunit,26,part_elig_start_idx:part_elig_end_idx,1) = felder
       base_est(iunit,24,part_elig_start_idx:part_elig_end_idx,2) = fsize - fkid
       base_est(iunit,25,part_elig_start_idx:part_elig_end_idx,2) = fsize - fadult
       base_est(iunit,26,part_elig_start_idx:part_elig_end_idx,2) = fsize - felder

       base_est(iunit,27,part_elig_start_idx:part_elig_end_idx,1) = sum_male
       base_est(iunit,28,part_elig_start_idx:part_elig_end_idx,1) = sum_female
       base_est(iunit,27,part_elig_start_idx:part_elig_end_idx,2) = sum_female
       base_est(iunit,27,part_elig_start_idx:part_elig_end_idx,2) = sum_male

       base_est(iunit,29,part_elig_start_idx:part_elig_end_idx,1) = fdis
       base_est(iunit,30,part_elig_start_idx:part_elig_end_idx,1) = fsize - FDIS
       base_est(iunit,29,part_elig_start_idx:part_elig_end_idx,2) = fsize - fdis
       base_est(iunit,30,part_elig_start_idx:part_elig_end_idx,2) = fdis

       if (model_code == "QCMM") THEN
          base_est(iunit,29:30,part_elig_start_idx:part_elig_end_idx,1:2) = 0
       end if

 201   continue

       !! ref elig, glunit = 1
       fsize =  0
       fkid =   0
       felder = 0
       fadult = 0
       fdis = 0
       sum_male = 0
       sum_female = 0
       ref_elig = .false.
       ref_part = .false.
       h_origin = 0
       h_race = 0
       h_ethnic = 0
       num_units = 0
       do iunit = 1, ctprhh
          if (l_fsun(ref_idx, kist)%iper(iunit) /= iunit) cycle
          if (l_fsben(ref_idx, kist)%iper(iunit) > 0) THEN
             ref_elig = .true.
             fsize = fsize + l_fsusize(ref_idx, kist)%iper(iunit)
             fkid =  fkid + l_fsnkid(ref_idx, kist)%iper(iunit)
             felder = felder + l_fsnelder(ref_idx, kist)%iper(iunit)
             fdis = fdis + l_fsndis(ref_idx, kist)%iper(iunit)
             sum_male = sum_male + fsnmale(iunit)
             sum_female = sum_female + fsnfemale(iunit)
             num_units = num_units + 1
             if (num_units == 1) then
                h_origin = fshorigin(iunit)
                h_race =   fshrace(iunit)
                h_ethnic = fshethnic(iunit)
             end if

             fsben = fsben + l_fsben(ref_idx, kist)%iper(iunit)

          end if
          if (l_fsPART(ref_idx, kist)%iper(iunit) == 1) ref_part = .true.
       end do

       if (num_units == 0) GOTO 202

       pc_fsben = REAL(fsben) / fsize

       iunit = 1

       fadult = fsize - (fkid + felder)

       if (.not. ref_elig .and. .not. ref_part) GOTO 202  !!  nothing to count: done
       ! Logic to determine which indices to update:
       !  elig &  part = 1:2
       ! ~elig &  part = 2:2
       !  elig & ~part = 1:1
       ! ~elig & ~part can't happen (see above if statement)
       part_elig_start_idx = 1
       part_elig_end_idx = 2
       if (.not. ref_elig) part_elig_start_idx = 2
       if (.not. ref_part) part_elig_end_idx = 1

       ! units
       ref_est(iunit, 1,part_elig_start_idx:part_elig_end_idx,1) = 1

       if (fkid > 0) then
          ref_est(iunit,2,part_elig_start_idx:part_elig_end_idx,1) = 1
       else
          ref_est(iunit,2,part_elig_start_idx:part_elig_end_idx,2) = 1
       end if

       if (fadult > 0) then
          ref_est(iunit,3,part_elig_start_idx:part_elig_end_idx,1) = 1
       else
          ref_est(iunit,3,part_elig_start_idx:part_elig_end_idx,2) = 1
       end if

       if (felder > 0) then
          ref_est(iunit,4,part_elig_start_idx:part_elig_end_idx,1) = 1
       else
          ref_est(iunit,4,part_elig_start_idx:part_elig_end_idx,2) = 1
       end if

       !! RACE
       do k = 1,6
          j = k+4
          ref_est(iunit,j,part_elig_start_idx:part_elig_end_idx,2) = 1
          if (H_RACE == k)  then
             ref_est(iunit,j,part_elig_start_idx:part_elig_end_idx,1) = 1
             ref_est(iunit,j,part_elig_start_idx:part_elig_end_idx,2) = 0
          end if
       end do

       !! ethnic
       if (h_ethnic == 1) THEN
          ref_est(iunit,11,part_elig_start_idx:part_elig_end_idx,1) = 1
          ref_est(iunit,12,part_elig_start_idx:part_elig_end_idx,2) = 1
       else
          ref_est(iunit,11,part_elig_start_idx:part_elig_end_idx,2) = 1
          ref_est(iunit,12,part_elig_start_idx:part_elig_end_idx,1) = 1
       end if

       !! DISAB flag
       if (fdis > 0) THEN
          ref_est(iunit,13,part_elig_start_idx:part_elig_end_idx,1) = 1
          ref_est(iunit,14,part_elig_start_idx:part_elig_end_idx,2) = 1
       else
          ref_est(iunit,13,part_elig_start_idx:part_elig_end_idx,2) = 1
          ref_est(iunit,14,part_elig_start_idx:part_elig_end_idx,1) = 1
       end if

       !!  ORIGIN
       do k = 1,8
          j = k+14
          ref_est(iunit,j,part_elig_start_idx:part_elig_end_idx,2) = 1
          if (h_origin == k)  then
             ref_est(iunit,j,part_elig_start_idx:part_elig_end_idx,1) = 1
             ref_est(iunit,j,part_elig_start_idx:part_elig_end_idx,2) = 0
          end if
       end do

       ! persons
       ref_est(iunit,23,part_elig_start_idx:part_elig_end_idx,1) = fsize

       ref_est(iunit,24,part_elig_start_idx:part_elig_end_idx,1) = fkid
       ref_est(iunit,25,part_elig_start_idx:part_elig_end_idx,1) = fadult
       ref_est(iunit,26,part_elig_start_idx:part_elig_end_idx,1) = felder
       ref_est(iunit,24,part_elig_start_idx:part_elig_end_idx,2) = fsize - fkid
       ref_est(iunit,25,part_elig_start_idx:part_elig_end_idx,2) = fsize - fadult
       ref_est(iunit,26,part_elig_start_idx:part_elig_end_idx,2) = fsize - felder

       ref_est(iunit,27,part_elig_start_idx:part_elig_end_idx,1) = sum_male
       ref_est(iunit,28,part_elig_start_idx:part_elig_end_idx,1) = sum_female
       ref_est(iunit,27,part_elig_start_idx:part_elig_end_idx,2) = sum_female
       ref_est(iunit,27,part_elig_start_idx:part_elig_end_idx,2) = sum_male

       ref_est(iunit,29,part_elig_start_idx:part_elig_end_idx,1) = fdis
       ref_est(iunit,30,part_elig_start_idx:part_elig_end_idx,1) = fsize - FDIS
       ref_est(iunit,29,part_elig_start_idx:part_elig_end_idx,2) = fsize - fdis
       ref_est(iunit,30,part_elig_start_idx:part_elig_end_idx,2) = fdis

       if (model_code == "QCMM") THEN
          ref_est(iunit,29:30,part_elig_start_idx:part_elig_end_idx,1:2) = 0
       end if

 202   continue

       !!  benefits for GLUNIT = 1
       do j = 1, num_pcts
          do m = 1,2
             do n = 1,2
                 select case (j)
                     case (1:22) !! unit level
                        base_ben(1, j, m, n) = base_est(1, j, m, n) * base_fsben
                        ref_ben( 1, j, m, n) = ref_est (1, j, m, n) * fsben
                     case default  !!  person level
                        base_ben(1, j, m, n) = base_est(1, j, m, n) * pc_base_fsben
                        ref_ben( 1, j, m, n) = ref_est (1, j, m, n) * pc_fsben
                 end select
             end do
          end do
       end do


    end if  !!  glunit = 1



    gl_ctprhh = ctprhh
    if (glunit == 1) gl_ctprhh = 1

    do j = 1, num_pcts
       do n = 1, 2
          do m = 1, 2
              do iunit = 1, gl_ctprhh
                 total_summ (j,m,n,0)    = total_summ  (j,m,n,0)   + base_est(iunit, j, m, n) * wgt
                 total_summ (j,m,n,nth)  = total_summ  (j,m,n,nth) + ref_est (iunit, j, m, n) * wgt

                 total_ben (j,m,n,0)    = total_ben  (j,m,n,0)   + base_ben(iunit, j, m, n) * wgt
                 total_ben (j,m,n,nth)  = total_ben  (j,m,n,nth) + ref_ben (iunit, j, m, n) * wgt
              end do
          end do
       end do
    end do



    t = 0

    do j = 1, num_pcts
       do n = 1, 2   !! in/out of subset
          do m = 1, 2      !! elig/part

             do iunit = 1, gl_ctprhh
                do ib = 0, num_wgt
                   sum_delta_summ (ib, j,m,n,nth) = sum_delta_summ (ib, j,m,n,nth) +  &
                      (ref_est(iunit, j, m, n) - base_est(iunit, j, m, n) ) * b_wgt(ib, kist)

                   sum_delta_ben (ib, j,m,n,nth) = sum_delta_ben (ib, j,m,n,nth) +  &
                      (ref_ben(iunit, j, m, n) - base_ben(iunit, j, m, n) ) * b_wgt(ib, kist)

                end do

                tot_delta_summ (j,m,n,nth) = tot_delta_summ (j,m,n,nth) &
                   + wgt * (ref_est(iunit, j, m, n) - base_est(iunit, j, m, n) )

                tot_delta_ben (j,m,n,nth) = tot_delta_ben (j,m,n,nth) &
                   + wgt * (ref_ben(iunit, j, m, n) - base_ben(iunit, j, m, n) )
             end do

          end do
       end do
    end do


    return



!------------------------------------------------------------------------------
!--- phase 3 processing
!------------------------------------------------------------------------------
900 continue

    if (nth > 1) RETURN  !!  calc only once per run

    !-------------- Compute Standard Error of Change Estimates ------------

    write(prfile, *) " "
    write(prfile, *) " In DBSTAT8, keof = 3"
    write(prfile, *) " "

    var_protect_summ_stats = " "
    var_protect_ben_stats = " "


    TAB_SUMM_STATS = " "
    TAB_SUMM_STATS_BEN = " "

    IF (.NOT. DOSTATS(1)) THEN
       TAB_SUMM_STATS = "N/A"
       TAB_SUMM_STATS_BEN = "N/A"
       RETURN
    END IF


    do p = 1, nplan10
       do j = 1, num_pcts
          do m = 1,2
             do n = 1,2

                do i = 1, num_wgt

                     var_summ(j,m,n,p) = var_summ(j,m,n,p) + &
                       (sum_delta_summ(i, j,m,n,p) - sum_delta_summ(0, j,m,n,p) ) &
                     * (sum_delta_summ(i, j,m,n,p) - sum_delta_summ(0, j,m,n,p) )

                     var_ben(j,m,n,p) = var_ben(j,m,n,p) + &
                       (sum_delta_ben(i, j,m,n,p) - sum_delta_ben(0, j,m,n,p) ) &
                     * (sum_delta_ben(i, j,m,n,p) - sum_delta_ben(0, j,m,n,p) )

                end do ! i

                !!  summary tables:
                var_summ(j,m,n,p) = var_const / num_wgt * var_summ(j,m,n,p)

                if (abs(total_summ(j,m,n,0)) > 0.0) & 
                   ptot_summ(j,m,n,p) = tot_delta_summ(j,m,n,p) /  total_summ(j,m,n,0) * 100.0

                sd_summ(j,m,n,p) = sqrt( var_summ(j,m,n,p))
                if (abs(sd_summ(j,m,n,p)) > 0.0) then 
                   z1_summ(j,m,n,p) = tot_delta_summ (j,m,n,p) / sd_summ(j,m,n,p)                   
                   IF (ABS(Z1_summ(J,m,n,p) ) > sig_level_90) var_protect_summ_stats(J,m,n,p)  = "*"
                end if


                !! benefit tables:
                var_ben(j,m,n,p) = var_const / num_wgt * var_ben(j,m,n,p)
                
                if (abs(total_ben(j,m,n,0)) > 0.0) &
                   ptot_ben(j,m,n,p) = tot_delta_ben(j,m,n,p) /  total_ben(j,m,n,0) * 100.0

                sd_ben(j,m,n,p) = sqrt( var_ben(j,m,n,p))
                if (abs(sd_ben(j,m,n,p)) > 0.0) then
                   z1_ben(j,m,n,p) = tot_delta_ben (j,m,n,p) / sd_ben(j,m,n,p)
                   IF (ABS(Z1_ben(J,m,n,p) ) > sig_level_90) var_protect_ben_stats(J,m,n,p)  = "*"
                end if

             end do  ! n

             !!  skip if nobody in "not in group" -- the "total" groups

             if (abs(tot_delta_summ(j,m,2,p)) > 0.0) then
        
                if (abs(var_summ(j,m,1,p)) > 0.0 .and. abs(var_summ(j,m,2,p)) > 0.0) then            
                   z2_summ(j,m,p) = (tot_delta_summ(j,m,1,p) - tot_delta_summ(j,m,2,p) ) & 
                              /  sqrt (var_summ(j,m,1,p) + var_summ(j,m,2,p) )
                end if

                if (abs(z2_summ(j,m,p)) > sig_level_90) then
                   if (var_protect_summ_stats(j,m,1,p) == "*") then
                       var_protect_summ_stats(j,m,1,p) = "#"
                   else
                       var_protect_summ_stats(j,m,1,p) = "+"
                   end if
                end if

             end if


             if (abs(tot_delta_ben(j,m,2,p)) > 0.0) then

                if (abs(var_ben(j,m,1,p)) > 0.0 .and. abs(var_ben(j,m,2,p)) > 0.0) then            
                   z2_ben(j,m,p) = (tot_delta_ben(j,m,1,p) - tot_delta_ben(j,m,2,p) ) &
                              /  sqrt (var_ben(j,m,1,p) + var_ben(j,m,2,p) )
                end if
                if (abs(z2_ben(j,m,p)) > sig_level_90) then
                   if (var_protect_ben_stats(j,m,1,p) == "*") then
                       var_protect_ben_stats(j,m,1,p) = "#"
                   else
                       var_protect_ben_stats(j,m,1,p) = "+"
                   end if
                end if

             end if

          end do ! m
       end do     ! var
    end do        ! plan


    !  summary table stats
    do p = 1, nplan10
        kk = 0
        do m = 1,2
           do j = 1,2
             kk = kk + 1
             if (abs(z1_summ(j,m,1,p)) > sig_level_99) then
               tab_summ_stats(kk,p) = "***"
             elseif (abs(z1_summ(j,m,1,p)) > sig_level_95) then
                tab_summ_stats(kk,p) = "** "
             elseif (abs(z1_summ(j,m,1,p)) > sig_level_90) then
                tab_summ_stats(kk,p) = "*  "
             else
                tab_summ_stats(kk,p) = "   "
             end if

             tab_summ_sd (kk,p) = sd_summ (j,m,1,p)
           end do
       end do

        !! for summary table benefits:
       IF (ABS(Z1_ben(1,2,1,p)) > SIG_LEVEL_99) THEN
         TAB_summ_STATs_BEN(p) = "***"
       ELSEIF (ABS(Z1_ben(1,2,1,p)) > SIG_LEVEL_95) THEN
          TAB_summ_STATs_BEN(p) = "** "
       ELSEIF (ABS(Z1_ben(1,2,1,p)) > SIG_LEVEL_90) THEN
          TAB_summ_STATs_BEN(p) = "*  "
       ELSE
          TAB_summ_STATs_BEN(p) = "   "
       END IF

       TAB_summ_SD_BEN(p) = SD_ben (1,2,1,p)

    end do


!-------------------------------------------------------------------------------------------
!   summary table(s) debug
!-------------------------------------------------------------------------------------------
    do p = 1, nplan10
       do m = 1,2      !! elig/part
          do n = 1,2   !! in/out of subgroup
             write(prfile, 6010) p, sub_label(n)

             do j = 1, num_pcts
                write(prfile, 6020) &
                  stat_label2(m) // stat_label(j) &
                 ,total_summ(j,m,n,0)      &
                 ,total_summ(j,m,n,p)      &
                 ,tot_delta_summ(j,m,n,p)  &
                 ,ptot_summ(j,m,n,p)       &
                 ,sd_summ(j,m,n,p)        &
                 ,var_protect_summ_stats(j,m,n,p)  &
                 ,z1_summ(j,m,n,p)        &
                 ,z2_summ(j,m,p)
             end do

          end do
       end do
    end do  !!  end plan


 6010  format(//, t2, "Summary Tables: Stats Summary for NTH = ", i2, 5x, a  &
 ,//,t2,"Var",t15,"Baselaw",t30,"Reform",t45,"Delta",t60,"Pct Chg",t74,"SD",t88,"Z1_test",t100,"Z2_test" &
 ,/, t2,"---",t15,"-------",t30,"------",t45,"-----",t60,"-------",t74,"--",t88,"-------",t100,"-------" )

 6020 format(t2,a, t15,f12.0, t30, f12.0, t45, f12.0, t60, f8.2, t72, f12.2, a, t86, f10.4, t98,f10.4)

    write(prfile, *) " "
    write(prfile, *) " *** Display summary table stats:"
    write(prfile, *) " *** var_protect_summ_stats", var_protect_summ_stats
    write(prfile, *) " *** tab_summ_stats", tab_summ_stats
    write(prfile, *) " "


!-------------------------------------------------------------------------------------------
!   benefit table(s) debug
!-------------------------------------------------------------------------------------------
    DO p = 1, NPLAN10

       DO m = 1,2      !! elig/part
          DO n = 1,2   !! in/out of subgroup
             WRITE(PRFILE, 6030) p, SUB_LABEL(n)

             do j = 1, num_pcts
                WRITE(PRFILE, 6040)                &
                   STAT_LABEL2(m) // STAT_LABEL(J) &
                  ,TOTAL_ben (J,m,n,0)     &
                  ,tot_delta_ben(j,m,n,p)  &
                  ,PTOT_ben  (J,m,n,p)     &
                  ,SD_ben (J,m,n,p)       &
                  ,var_protect_ben_stats(J,m,n,p)  &
                  ,Z1_ben(J,m,n,p)        &
                  ,Z2_ben(J,m,p)
             end do

          END DO
       END DO
    END DO  !!  END PLAN


 6030  FORMAT(//, T2, "Benefit Tables: STAT SUMMARY FOR NTH = ", I2, 5X, A  &
   ,//,T2,"VAR",T15,"BASELAW",T30,"DELTA ",T45,"PCT CHG",T60,"SD",T77,"Z1_TEST",T91,"Z2_TEST" &
   ,/, T2,"---",T15,"-------",T30,"------",T45,"-------",T60,"--",T77,"-------",T91,"-------" )

 6040 FORMAT(T2,A, T15,F12.0, T30,F12.0, T45,F8.2, T60,F15.4, A,T77, F10.4,T91,F10.4)

    write(prfile, *) " "
    write(prfile, *) " *** Display benefit table stats:"
    write(prfile, *) " var_protect_ben_stats = ",  var_protect_ben_stats
    write(prfile, *) " "




    return
    END SUBROUTINE
