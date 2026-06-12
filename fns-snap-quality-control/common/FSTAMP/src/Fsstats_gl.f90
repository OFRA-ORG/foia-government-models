    module stat_gl_module
    USE global
    implicit none


    real(8), ALLOCATABLE, dimension(   :, :, :,  :, :) :: ptotgl

    real(8), ALLOCATABLE,dimension (:   , :, :, :) :: ptot_ben
    real(8), ALLOCATABLE,dimension (:, :, :, :, :) :: sum_delta_gl
    real(8), ALLOCATABLE,dimension (:, :, :, :, :) :: sum_delta_glb
    real(8), ALLOCATABLE,dimension (:   , :, :, :) :: total_ben
    real(8), ALLOCATABLE,dimension (:   , :, :, :) :: delta_ben

    real(8), ALLOCATABLE,dimension (:, :,    :, :) :: var_gl
    real(8), ALLOCATABLE,dimension (:, :,    :, :) :: sd_gl
    real(8), ALLOCATABLE,dimension (:, :, :, :, :) :: z1_gl
    real(8), ALLOCATABLE,dimension (:, :,    :, :) :: z2_gl

    real(8), ALLOCATABLE,dimension (:,    :, :, :) :: var_glb
    real(8), ALLOCATABLE,dimension (:,    :, :, :) :: sd_glb
    real(8), ALLOCATABLE,dimension (:,    :, :, :) :: z1_glb
    real(8), ALLOCATABLE,dimension (:,       :, :) :: z2_glb


    integer, ALLOCATABLE,dimension(:, :, :) :: baselaw_est
    integer, ALLOCATABLE,dimension(:, :, :) :: reform_est
    real,    ALLOCATABLE,dimension(:, :, :) :: baselaw_ben
    real,    ALLOCATABLE,dimension(:, :, :) :: reform_ben

    real(8), ALLOCATABLE,dimension(   :, :, :,  :, :) :: totalgl

    end module



    subroutine fs_alloc_stats()
    USE global
    USE fswork
    USE fssizes



    implicit none
!-----------------------------------------------------------------------------------------
!  # vars      30,
!  e/p          2
!  W/O W/ CHAR  2
!  PLAN         5
!
!-----------------------------------------------------------------------------------------
    if (nth == 1) then

       !! dimension plans by max plans+1  = 6

       ALLOCATE (tab_protect_summ_stats(30, 2, 2, 6))
       ALLOCATE (tab_protect_ben_stats(30, 2, 2, 6))
       ALLOCATE (var_protect_summ_stats (30, 2, 2, 6))
       ALLOCATE (var_protect_ben_stats (30, 2, 2, 6))

       ALLOCATE (tab_protect_gl_stats(30, 2, 2, 6, 10))
       ALLOCATE (var_protect_gl_stats(30, 2, 2, 6, 10))

       ALLOCATE (tab_protect_gl_stats_ben(30, 2, 2, 6, 10))
       ALLOCATE (var_protect_gl_stats_ben(30, 2, 2, 6, 10))

    end if

    end subroutine



    subroutine fs_dealloc_stats()
    USE global
    USE fswork

    implicit none

    if (nth == 1) then
 
    end if

    end subroutine




    SUBROUTINE FS_STATS_GL( &
                BASE_FSBEN   &  !
               ,FSBEN        &  !
               ,FSUSIZE      &  !
               ,fsndis       &  !
               ,fsnelder     &  !
               ,fsnadult     &  !
               ,fsnkid       &  !
               ,fsnmale      &  !
               ,fsnfemale    &  !
               ,fshrace      &  !
               ,fshethnic    &  !
               ,fshorigin    &  !
               ,KTH          &  ! = 1 for baselaw, > 1 for reforms
               ,BASE_PARTIC  &  !
               ,PARTIC       &  !
               ,WGT          &  !
               ,icat         &
               ,jkist        &
               ,junit        &
               )

    USE GLOBAL
    USE USERPARM

    USE fsparm, only : dostats, prlevel

    USE fswork, only :   &
    num_pcts               &
   ,var_const              &
   ,num_wgt                &
   ,b_wgt                  &
   ,sig_level_90           &
   ,tab_gl_stats           &
   ,tab_gl_stats_ben       &
   ,var_protect_gl_stats      &
   ,var_protect_gl_stats_ben


    USE stat_gl_module
    implicit none

    INTEGER, INTENT(IN)    ::  BASE_FSBEN
    INTEGER, INTENT(IN)    ::  FSBEN
    INTEGER, INTENT(IN)    ::  FSUSIZE
    INTEGER, INTENT(IN)    ::  fsndis
    INTEGER, INTENT(IN)    ::  fsnelder
    INTEGER, INTENT(IN)    ::  fsnadult
    INTEGER, INTENT(IN)    ::  fsnkid
    INTEGER, INTENT(IN)    ::  fsnmale
    INTEGER, INTENT(IN)    ::  fsnfemale

    INTEGER, INTENT(IN)    ::  fshrace
    INTEGER, INTENT(IN)    ::  fshethnic
    INTEGER, INTENT(IN)    ::  fshorigin

    INTEGER, INTENT(IN)    ::  KTH
    INTEGER, INTENT(IN)    ::  icat
    LOGICAL, INTENT(IN)    ::  BASE_PARTIC
    LOGICAL, INTENT(IN)    ::  PARTIC
    REAL   , INTENT(IN)    ::  WGT
    INTEGER, INTENT(IN)    ::  jkist
    INTEGER, INTENT(IN)    ::  junit


    INTEGER :: i, idx, ib, j, jj, k, kk, m, n, p

    integer, parameter :: ncat = 10
    integer, parameter :: max_nth2 = 5

    INTEGER :: nplan10 = 0

    integer, dimension(max_persons,0:max_nth2) :: ielig, ipartic

    INTEGER, dimension(ncat) :: nbr_cat = 0
    Real(8), dimension(ncat) :: wgt_nbr_cat = 0.0

    REAL :: pc_fsben, delta


    LOGICAL :: first_call = .true.

    INTEGER :: kid_flag    &
      ,elder_flag          &
      ,adult_flag          &
      ,head_white_flag     &
      ,head_black_flag     &
      ,head_hisp_flag      &
      ,head_asian_flag     &
      ,head_amer_ind_flag  &
      ,head_unk_flag       &
      ,head_hispanic_flag  &
      ,head_not_hisp_flag  &
      ,disab_flag          &
      ,not_disab_flag

    INTEGER :: not_kid_flag    &
      ,not_elder_flag          &
      ,not_adult_flag          &
      ,not_head_white_flag     &
      ,not_head_black_flag     &
      ,not_head_hisp_flag      &
      ,not_head_asian_flag     &
      ,not_head_amer_ind_flag  &
      ,not_head_unk_flag       &
      ,not_fsnkid              &
      ,not_nadult              &
      ,not_fsnelder

    INTEGER :: head_nami_flag   &
              ,head_euro_flag   &
              ,head_asia_flag   &
              ,head_mide_flag   &
              ,head_cent_flag   &
              ,head_afri_flag   &
              ,head_else_flag   &
              ,head_unkn_flag   &
              ,not_head_nami_flag   &
              ,not_head_euro_flag   &
              ,not_head_asia_flag   &
              ,not_head_mide_flag   &
              ,not_head_cent_flag   &
              ,not_head_afri_flag   &
              ,not_head_else_flag   &
              ,not_head_unkn_flag
  

    !! LOGIC for calculating variance in strata by G/L category:
    !! key:  0 count (no chg)
    !!       1 count (change)
    !!       3 skip
    integer, dimension(ncat, ncat) :: cat_count
    integer, dimension(ncat * ncat) :: cat_count1 = &
                (/1,0,0,0,0,0,0,3,3,3  &    ! L no  longer elig
                 ,0,1,0,0,0,0,0,3,3,3  &    ! L
                 ,0,0,1,0,0,0,0,3,3,3  &    ! L
                 ,0,0,0,1,0,0,0,3,3,3  &    ! no chg
                 ,0,0,0,0,1,0,0,3,3,3  &    ! G
                 ,0,0,0,0,0,1,0,3,3,3  &    ! G
                 ,0,0,0,0,0,0,1,3,3,3  &    ! G
                 ,0,0,0,0,3,3,3,1,3,3  &    ! tot gain
                 ,3,3,3,0,0,0,0,3,1,3  &    ! tot lose
                 ,3,3,3,0,3,3,3,3,3,1  /)   ! tot change



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

    character(len=20), dimension(ncat) :: cat_label = (/  &
       "No Longer Elig      "    &  ! 1
      ,"Still Elig, Not Part"    &  ! 2
      ,"Still Part, Low Ben "    &  ! 3
      ,"No Change           "    &  ! 4
      ,"New Elig & Part     "    &  ! 5
      ,"Still Elig, New Part"    &  ! 6
      ,"Still Part, Hi Ben  "    &  ! 7
      ,"total Gainers       "    &  ! 8
      ,"total Losers        "    &  ! 9
      ,"Change              "    &  ! gl
      /)




    if (first_call) then


       first_call = .false.


       cat_count = reshape (cat_count1, (/ncat,ncat/) )


       ALLOCATE(ptot_ben       (           num_pcts, 2,    max_nth2, ncat))
       ALLOCATE(sum_delta_gl   (0:num_wgt, num_pcts, 2,    max_nth2, ncat))
       ALLOCATE(sum_delta_glb  (0:num_wgt, num_pcts, 2,    max_nth2, ncat))

       ALLOCATE(total_ben      (        num_pcts, 2,  0:max_nth2, ncat))
       ALLOCATE(delta_ben      (        num_pcts, 2,  0:max_nth2, ncat))


       ALLOCATE(var_gl         (        num_pcts, 2,    max_nth2, ncat))
       ALLOCATE(sd_gl          (        num_pcts, 2,    max_nth2, ncat))
       ALLOCATE(z1_gl          (        num_pcts, 2, 2, max_nth2, ncat))
       ALLOCATE(z2_gl          (        num_pcts, 2,    max_nth2, ncat))

       ALLOCATE(var_glb        (        num_pcts,    2, max_nth2, ncat))
       ALLOCATE(sd_glb         (        num_pcts,    2, max_nth2, ncat))
       ALLOCATE(z1_glb         (        num_pcts,    2, max_nth2, ncat))
       ALLOCATE(z2_glb         (        num_pcts,       max_nth2, ncat))

       ALLOCATE(baselaw_est    (max_persons, num_pcts, 2))
       ALLOCATE(reform_est     (max_persons, num_pcts, 2))
       ALLOCATE(baselaw_ben    (max_persons, num_pcts, 2))
       ALLOCATE(reform_ben     (max_persons, num_pcts, 2))

       ALLOCATE(totalgl        (        num_pcts, 2, 2, 0:max_nth2, ncat))
       ALLOCATE(ptotgl         (        num_pcts, 2, 2,   max_nth2, ncat))


       !!  init allocated arrays:

       ptot_ben     = 0.0
       sum_delta_gl = 0.0
       sum_delta_glb = 0.0
       total_ben    = 0.0
       delta_ben    = 0.0


       var_gl         = 0.0
       sd_gl          = 0.0
       z1_gl          = 0.0
       z2_gl          = 0.0

       var_glb       = 0.0
       sd_glb        = 0.0
       z1_glb        = 0.0
       z2_glb        = 0.0

       totalgl       = 0.0
       ptotgl        = 0.0

    end if


    IF (KEOF == 3) GOTO 900


    if (.NOT. dostats(nth)) return








    IF (KFREQ > 0 .and. prlevel(nth) >= 9) THEN
       WRITE(PRFILE, *) " "
       WRITE(PRFILE, *) " IN DB_FSTAB_GL_STATS"

       WRITE(PRFILE, *) " num_wgt =" , num_wgt
       WRITE(PRFILE, *) " nplan10 = ", nplan10
       WRITE(PRFILE, *) " FIRST_CALL = ", FIRST_CALL

       WRITE(PRFILE, *) " FSBEN     = ",  FSBEN
       WRITE(PRFILE, *) " FSUSIZE   = ",  FSUSIZE
       WRITE(PRFILE, *) " FSNDIS    = ",  fsndis
       WRITE(PRFILE, *) " FSNELDER  = ",  fsnelder
       WRITE(PRFILE, *) " FSNADULT  = ",  fsnadult
       WRITE(PRFILE, *) " FSNKID    = ",  fsnkid
       WRITE(PRFILE, *) " FSNMALE   = ",  fsnmale
       WRITE(PRFILE, *) " FSNFEMALE = ",  fsnfemale
       WRITE(PRFILE, *) " FSHRACE   = ",  fshrace
       WRITE(PRFILE, *) " FSHETHNIC = ",  fshethnic
       WRITE(PRFILE, *) " FSHORIGIN = ",  fshorigin
       WRITE(PRFILE, *) " KTH       = ",  KTH
       WRITE(PRFILE, *) " PARTIC    = ",  PARTIC
       WRITE(PRFILE, *) " WGT       = ",  WGT
       WRITE(PRFILE, *) " icat      = ",  icat
       WRITE(PRFILE, *) " "
    END IF

    if  (KTH > 1 .AND. ICAT > 0) then
       nbr_cat(icat) = nbr_cat(icat) + 1
       wgt_nbr_cat(icat) = wgt_nbr_cat(icat) + wgt
    end if


    !------- count up esimates for baselaw plan--------------------------------
    if (NTH > nplan10) nplan10 = NTH


    ielig(junit,0) = 0
    if (base_fsben > 0) ielig(junit,0) = 1

    ipartic(junit,0) = 0
    if (base_partic) ipartic(junit,0) = 1

    ielig(junit,nth) = 0
    if (fsben > 0) ielig(junit,nth) = 1

    ipartic(junit,nth) = 0
    if (partic) ipartic(junit,nth) = 1



    !!  set flags for each variable
    kid_flag = 0
    if (fsnkid > 0) kid_flag = 1
    not_kid_flag = 0
    if (fsnkid == 0) not_kid_flag = 1

    elder_flag = 0
    if (fsnelder > 0) elder_flag = 1
    not_elder_flag = 0
    if (fsnelder == 0) not_elder_flag = 1

    adult_flag = 0
    if (fsnadult > 0) adult_flag = 1
    not_adult_flag = 0
    if (fsnadult == 0) not_adult_flag = 1

    not_fsnkid   = FSUSIZE - FSnkid
    not_fsnelder = FSUSIZE - FSnelder
    not_nadult   = FSUSIZE - fsnadult

    head_white_flag = 0
    head_black_flag = 0
    head_hisp_flag =  0
    head_asian_flag = 0
    head_amer_ind_flag = 0
    head_unk_flag = 0

    not_head_white_flag = 0
    not_head_black_flag = 0
    not_head_hisp_flag =  0
    not_head_asian_flag = 0
    not_head_amer_ind_flag = 0
    not_head_unk_flag = 0

    select case (fshrace)
        case (1)
           head_white_flag = 1
           not_head_black_flag = 1
           not_head_hisp_flag =  1
           not_head_asian_flag = 1
           not_head_amer_ind_flag = 1
           not_head_unk_flag = 1

        case (2)
           head_black_flag = 1
           not_head_white_flag = 1
           not_head_hisp_flag =  1
           not_head_asian_flag = 1
           not_head_amer_ind_flag = 1
           not_head_unk_flag = 1

        case (3)
           head_hisp_flag = 1
           not_head_white_flag = 1
           not_head_black_flag = 1
           not_head_asian_flag = 1
           not_head_amer_ind_flag = 1
           not_head_unk_flag = 1

        case (4)
           head_asian_flag = 1
           not_head_white_flag = 1
           not_head_black_flag = 1
           not_head_hisp_flag =  1
           not_head_amer_ind_flag = 1
           not_head_unk_flag = 1

        case (5)
           head_amer_ind_flag = 1
           not_head_white_flag = 1
           not_head_black_flag = 1
           not_head_hisp_flag =  1
           not_head_asian_flag = 1
           not_head_unk_flag = 1

        case (6)
           head_unk_flag = 1
           not_head_white_flag = 1
           not_head_black_flag = 1
           not_head_hisp_flag =  1
           not_head_asian_flag = 1
           not_head_amer_ind_flag = 1
        case default
           !
    end select



    head_nami_flag      = 0
    head_euro_flag      = 0
    head_asia_flag      = 0
    head_mide_flag      = 0
    head_cent_flag      = 0
    head_afri_flag      = 0
    head_else_flag      = 0
    head_unkn_flag      = 0
    not_head_nami_flag  = 1
    not_head_euro_flag  = 1
    not_head_asia_flag  = 1
    not_head_mide_flag  = 1
    not_head_cent_flag  = 1
    not_head_afri_flag  = 1
    not_head_else_flag  = 1
    not_head_unkn_flag  = 1

    select case (fshorigin)
        case (1)
           head_nami_flag = 1
           not_head_nami_flag = 0
        case (2)
           head_euro_flag = 1
           not_head_euro_flag = 0
        case (3)
           head_asia_flag = 1
           not_head_asia_flag = 0
        case (4)
           head_mide_flag = 1
           not_head_mide_flag = 0
        case (5)
           head_cent_flag = 1
           not_head_cent_flag = 0
        case (6)
           head_afri_flag = 1
           not_head_afri_flag = 0
        case (7)
           head_else_flag = 1
           not_head_else_flag = 0
        case (8)
           head_unkn_flag = 1
           not_head_unkn_flag = 0
        case default
           !
    end select




    head_hispanic_flag = 0
    head_not_hisp_flag = 0
    if (fshethnic == 1) head_hispanic_flag = 1
    if (fshethnic == 0) head_not_hisp_flag = 1

    disab_flag = 0
    not_disab_flag = 0
    if (fsndis  > 0) disab_flag = 1
    if (fsndis == 0) not_disab_flag = 1


    pc_fsben = REAL(base_fsben) / fsusize

    baselaw_est  (junit,1,1) =  1
    baselaw_est  (junit,1,2) =  0

    baselaw_est  (junit,2,1) =  kid_flag
    baselaw_est  (junit,3,1) =  adult_flag
    baselaw_est  (junit,4,1) =  elder_flag

    baselaw_est  (junit,2,2) =  not_kid_flag
    baselaw_est  (junit,3,2) =  not_adult_flag
    baselaw_est  (junit,4,2) =  not_elder_flag

    !--- race
    baselaw_est  (junit,5,1) =  head_white_flag
    baselaw_est  (junit,6,1) =  head_black_flag
    baselaw_est  (junit,7,1) =  head_hisp_flag
    baselaw_est  (junit,8,1) =  head_asian_flag
    baselaw_est  (junit,9,1) =  head_amer_ind_flag
    baselaw_est (junit,10,1) =  head_unk_flag

    baselaw_est  (junit,5,2) =  not_head_white_flag
    baselaw_est  (junit,6,2) =  not_head_black_flag
    baselaw_est  (junit,7,2) =  not_head_hisp_flag
    baselaw_est  (junit,8,2) =  not_head_asian_flag
    baselaw_est  (junit,9,2) =  not_head_amer_ind_flag
    baselaw_est (junit,10,2) =  not_head_unk_flag

    !--- hispanic
    baselaw_est (junit,11,1) =   head_hispanic_flag
    baselaw_est (junit,12,1) =   head_not_hisp_flag
    baselaw_est (junit,11,2) =   head_not_hisp_flag
    baselaw_est (junit,12,2) =   head_hispanic_flag

    !--- disab
    baselaw_est (junit,13,1) =   disab_flag
    baselaw_est (junit,14,1) =   not_disab_flag

    baselaw_est (junit,13,2) =   not_disab_flag
    baselaw_est (junit,14,2) =   disab_flag

    !--- origin
    baselaw_est (junit,15,1) =   head_nami_flag
    baselaw_est (junit,16,1) =   head_euro_flag
    baselaw_est (junit,17,1) =   head_asia_flag
    baselaw_est (junit,18,1) =   head_mide_flag
    baselaw_est (junit,19,1) =   head_cent_flag
    baselaw_est (junit,20,1) =   head_afri_flag
    baselaw_est (junit,21,1) =   head_else_flag
    baselaw_est (junit,22,1) =   head_unkn_flag

    baselaw_est (junit,15,2) =   not_head_nami_flag
    baselaw_est (junit,16,2) =   not_head_euro_flag
    baselaw_est (junit,17,2) =   not_head_asia_flag
    baselaw_est (junit,18,2) =   not_head_mide_flag
    baselaw_est (junit,19,2) =   not_head_cent_flag
    baselaw_est (junit,20,2) =   not_head_afri_flag
    baselaw_est (junit,21,2) =   not_head_else_flag
    baselaw_est (junit,22,2) =   not_head_unkn_flag


    !--- persons
    baselaw_est  (junit,23,1) =  fsusize
    baselaw_est  (junit,23,2) =  0

    baselaw_est (junit,24,1) =   fsnkid
    baselaw_est (junit,25,1) =   fsnadult
    baselaw_est (junit,26,1) =   fsnelder

    baselaw_est (junit,24,2) =   not_fsnkid
    baselaw_est (junit,25,2) =   not_nadult
    baselaw_est (junit,26,2) =   not_fsnelder

    !--- sex
    baselaw_est (junit,27,1) =   fsnmale
    baselaw_est (junit,28,1) =   fsnfemale

    baselaw_est (junit,27,2) =   fsnfemale
    baselaw_est (junit,28,2) =   fsnmale

    !--- disab
    baselaw_est (junit,29,1) =   fsndis
    baselaw_est (junit,30,1) =   fsusize - fsndis
    baselaw_est (junit,29,2) =   fsusize - fsndis
    baselaw_est (junit,30,2) =   fsndis

    if (model_code == "QCMM") THEN
       baselaw_est (junit,29:30,1:2) = 0
    end if

    !!  elig benefits
    baselaw_ben  (junit,1,1) =  1                       * base_fsben
    baselaw_ben  (junit,1,2) =  0

    baselaw_ben  (junit,2,1) =  kid_flag                * base_fsben
    baselaw_ben  (junit,3,1) =  adult_flag              * base_fsben
    baselaw_ben  (junit,4,1) =  elder_flag              * base_fsben

    baselaw_ben  (junit,2,2) =  not_kid_flag            * base_fsben
    baselaw_ben  (junit,3,2) =  not_adult_flag          * base_fsben
    baselaw_ben  (junit,4,2) =  not_elder_flag          * base_fsben

    !--- race
    baselaw_ben  (junit,5,1) =  head_white_flag         * base_fsben
    baselaw_ben  (junit,6,1) =  head_black_flag         * base_fsben
    baselaw_ben  (junit,7,1) =  head_hisp_flag          * base_fsben
    baselaw_ben  (junit,8,1) =  head_asian_flag         * base_fsben
    baselaw_ben  (junit,9,1) =  head_amer_ind_flag      * base_fsben
    baselaw_ben (junit,10,1) =  head_unk_flag           * base_fsben

    baselaw_ben  (junit,5,2) =  not_head_white_flag     * base_fsben
    baselaw_ben  (junit,6,2) =  not_head_black_flag     * base_fsben
    baselaw_ben  (junit,7,2) =  not_head_hisp_flag      * base_fsben
    baselaw_ben  (junit,8,2) =  not_head_asian_flag     * base_fsben
    baselaw_ben  (junit,9,2) =  not_head_amer_ind_flag  * base_fsben
    baselaw_ben (junit,10,2) =  not_head_unk_flag       * base_fsben

    !--- hispanic
    baselaw_ben (junit,11,1) =   head_hispanic_flag     * base_fsben
    baselaw_ben (junit,12,1) =   head_not_hisp_flag     * base_fsben

    baselaw_ben (junit,11,2) =   head_not_hisp_flag     * base_fsben
    baselaw_ben (junit,12,2) =   head_hispanic_flag     * base_fsben

    !--- disab
    baselaw_ben (junit,13,1) =   disab_flag             * base_fsben
    baselaw_ben (junit,14,1) =   not_disab_flag         * base_fsben

    baselaw_ben (junit,13,2) =   not_disab_flag         * base_fsben
    baselaw_ben (junit,14,2) =   disab_flag             * base_fsben


    !--- origin
    baselaw_ben (junit,15,1) =   head_nami_flag         * base_fsben
    baselaw_ben (junit,16,1) =   head_euro_flag         * base_fsben
    baselaw_ben (junit,17,1) =   head_asia_flag         * base_fsben
    baselaw_ben (junit,18,1) =   head_mide_flag         * base_fsben
    baselaw_ben (junit,19,1) =   head_cent_flag         * base_fsben
    baselaw_ben (junit,20,1) =   head_afri_flag         * base_fsben
    baselaw_ben (junit,21,1) =   head_else_flag         * base_fsben
    baselaw_ben (junit,22,1) =   head_unkn_flag         * base_fsben

    baselaw_ben (junit,15,2) =   not_head_nami_flag     * base_fsben
    baselaw_ben (junit,16,2) =   not_head_euro_flag     * base_fsben
    baselaw_ben (junit,17,2) =   not_head_asia_flag     * base_fsben
    baselaw_ben (junit,18,2) =   not_head_mide_flag     * base_fsben
    baselaw_ben (junit,19,2) =   not_head_cent_flag     * base_fsben
    baselaw_ben (junit,20,2) =   not_head_afri_flag     * base_fsben
    baselaw_ben (junit,21,2) =   not_head_else_flag     * base_fsben
    baselaw_ben (junit,22,2) =   not_head_unkn_flag     * base_fsben

    !--- persons (per-capita-ben)

    baselaw_ben  (junit,23,1) = pc_fsben * fsusize
    baselaw_ben  (junit,23,2) = 0


    !--- persons
    baselaw_ben (junit,24,1) =  pc_fsben *  fsnkid
    baselaw_ben (junit,25,1) =  pc_fsben *  fsnadult
    baselaw_ben (junit,26,1) =  pc_fsben *  fsnelder

    baselaw_ben (junit,24,2) =  pc_fsben *  not_fsnkid
    baselaw_ben (junit,25,2) =  pc_fsben *  not_nadult
    baselaw_ben (junit,26,2) =  pc_fsben *  not_fsnelder

    !--- sex
    baselaw_ben (junit,27,1) =  pc_fsben *  fsnmale
    baselaw_ben (junit,28,1) =  pc_fsben *  fsnfemale

    baselaw_ben (junit,27,2) =  pc_fsben *  fsnfemale
    baselaw_ben (junit,28,2) =  pc_fsben *  fsnmale

    !--- disab
    baselaw_ben (junit,29,1) =  pc_fsben *  fsndis
    baselaw_ben (junit,30,1) =  pc_fsben * (fsusize - fsndis)
    baselaw_ben (junit,29,2) =  pc_fsben * (fsusize - fsndis)
    baselaw_ben (junit,30,2) =  pc_fsben *  fsndis

    if (model_code == "QCMM") THEN
       baselaw_ben (junit,29:30,1:2) = 0
    end if



    IF (KTH == 1) THEN

       !!  baselaw
       do j = 1, num_pcts
          do n = 1, 2

             If (Ipartic(JUNIT,0) == 1)  &
                total_ben (j,n,0, 1) = total_ben (j,n,0, 1) + baselaw_ben (junit,j,n) * wgt

             do m = 1, 2
                if ( (m == 1 .and. ielig(junit,0) == 1)  .or. (m == 2 .and. Ipartic(JUNIT,0) == 1) ) then
                   totalgl (j,m,n,0, 1) = totalgl (j,m,n,0, 1) + baselaw_est (junit,j,n) * wgt
                end if
             end do

          end do
       end do

       return

    END IF


    !----------------------------------------------------------------
    !-------- count up estimates for NTH plan -----------------------
    !----------------------------------------------------------------

    idx = nth + 1

    p = Kth-1


    pc_fsben = 0.0


    if (fsusize /= 0) pc_fsben = REAL(fsben) / fsusize


    reform_est  (junit, 1,1) =  1
    reform_est  (junit, 1,2) =  0

    reform_est  (junit, 2,1) =  kid_flag
    reform_est  (junit, 3,1) =  adult_flag
    reform_est  (junit, 4,1) =  elder_flag

    reform_est  (junit, 2,2) =  not_kid_flag
    reform_est  (junit, 3,2) =  not_adult_flag
    reform_est  (junit, 4,2) =  not_elder_flag

    !--- race
    reform_est  (junit, 5,1) =  head_white_flag
    reform_est  (junit, 6,1) =  head_black_flag
    reform_est  (junit, 7,1) =  head_hisp_flag
    reform_est  (junit, 8,1) =  head_asian_flag
    reform_est  (junit, 9,1) =  head_amer_ind_flag
    reform_est  (junit,10,1) =  head_unk_flag

    reform_est  (junit, 5,2) =  not_head_white_flag
    reform_est  (junit, 6,2) =  not_head_black_flag
    reform_est  (junit, 7,2) =  not_head_hisp_flag
    reform_est  (junit, 8,2) =  not_head_asian_flag
    reform_est  (junit, 9,2) =  not_head_amer_ind_flag
    reform_est  (junit,10,2) =  not_head_unk_flag

    !--- hispanic
    reform_est  (junit,11,1) =  head_hispanic_flag
    reform_est  (junit,12,1) =  head_not_hisp_flag

    reform_est  (junit,11,2) =  head_not_hisp_flag
    reform_est  (junit,12,2) =  head_hispanic_flag

    !--- disab
    reform_est  (junit,13,1) =  disab_flag
    reform_est  (junit,14,1) =  not_disab_flag

    reform_est  (junit,13,2) =  not_disab_flag
    reform_est  (junit,14,2) =  disab_flag


    !--- origin
    reform_est (junit,15,1) =   head_nami_flag
    reform_est (junit,16,1) =   head_euro_flag
    reform_est (junit,17,1) =   head_asia_flag
    reform_est (junit,18,1) =   head_mide_flag
    reform_est (junit,19,1) =   head_cent_flag
    reform_est (junit,20,1) =   head_afri_flag
    reform_est (junit,21,1) =   head_else_flag
    reform_est (junit,22,1) =   head_unkn_flag

    reform_est (junit,15,2) =   not_head_nami_flag
    reform_est (junit,16,2) =   not_head_euro_flag
    reform_est (junit,17,2) =   not_head_asia_flag
    reform_est (junit,18,2) =   not_head_mide_flag
    reform_est (junit,19,2) =   not_head_cent_flag
    reform_est (junit,20,2) =   not_head_afri_flag
    reform_est (junit,21,2) =   not_head_else_flag
    reform_est (junit,22,2) =   not_head_unkn_flag


    !--- persons
    reform_est  (junit,23,1) =  fsusize
    reform_est  (junit,23,2) =  0

    reform_est (junit,24,1) =   fsnkid
    reform_est (junit,25,1) =   fsnadult
    reform_est (junit,26,1) =   fsnelder

    reform_est (junit,24,2) =   not_fsnkid
    reform_est (junit,25,2) =   not_nadult
    reform_est (junit,26,2) =   not_fsnelder

    !--- sex
    reform_est (junit,27,1) =   fsnmale
    reform_est (junit,28,1) =   fsnfemale

    reform_est (junit,27,2) =   fsnfemale
    reform_est (junit,28,2) =   fsnmale

    !--- disab
    reform_est (junit,29,1) =   fsndis
    reform_est (junit,30,1) =   fsusize - fsndis
    reform_est (junit,29,2) =   fsusize - fsndis
    reform_est (junit,30,2) =   fsndis

    if (model_code == "QCMM") THEN
       reform_est (junit,29:30,1:2) = 0
    end if


    reform_ben  (junit, 1,1) =  1                       * fsben
    reform_ben  (junit, 1,2) =  0                       * fsben

    reform_ben  (junit, 2,1) =  kid_flag                * fsben
    reform_ben  (junit, 3,1) =  adult_flag              * fsben
    reform_ben  (junit, 4,1) =  elder_flag              * fsben

    reform_ben  (junit, 2,2) =  not_kid_flag            * fsben
    reform_ben  (junit, 3,2) =  not_adult_flag          * fsben
    reform_ben  (junit, 4,2) =  not_elder_flag          * fsben

    !--- race
    reform_ben  (junit, 5,1) =  head_white_flag         * fsben
    reform_ben  (junit, 6,1) =  head_black_flag         * fsben
    reform_ben  (junit, 7,1) =  head_hisp_flag          * fsben
    reform_ben  (junit, 8,1) =  head_asian_flag         * fsben
    reform_ben  (junit, 9,1) =  head_amer_ind_flag      * fsben
    reform_ben  (junit,10,1) =  head_unk_flag           * fsben

    reform_ben  (junit, 5,2) =  not_head_white_flag     * fsben
    reform_ben  (junit, 6,2) =  not_head_black_flag     * fsben
    reform_ben  (junit, 7,2) =  not_head_hisp_flag      * fsben
    reform_ben  (junit, 8,2) =  not_head_asian_flag     * fsben
    reform_ben  (junit, 9,2) =  not_head_amer_ind_flag  * fsben
    reform_ben  (junit,10,2) =  not_head_unk_flag       * fsben

    !--- hispanic
    reform_ben  (junit,11,1) =  head_hispanic_flag      * fsben
    reform_ben  (junit,12,1) =  head_not_hisp_flag      * fsben

    reform_ben  (junit,11,2) =  head_not_hisp_flag      * fsben
    reform_ben  (junit,12,2) =  head_hispanic_flag      * fsben

    !--- disab
    reform_ben  (junit,13,1) =  disab_flag              * fsben
    reform_ben  (junit,14,1) =  not_disab_flag          * fsben

    reform_ben  (junit,13,2) =  not_disab_flag          * fsben
    reform_ben  (junit,14,2) =  disab_flag              * fsben



    !--- origin
    reform_ben (junit,15,1) =   head_nami_flag         * base_fsben
    reform_ben (junit,16,1) =   head_euro_flag         * base_fsben
    reform_ben (junit,17,1) =   head_asia_flag         * base_fsben
    reform_ben (junit,18,1) =   head_mide_flag         * base_fsben
    reform_ben (junit,19,1) =   head_cent_flag         * base_fsben
    reform_ben (junit,20,1) =   head_afri_flag         * base_fsben
    reform_ben (junit,21,1) =   head_else_flag         * base_fsben
    reform_ben (junit,22,1) =   head_unkn_flag         * base_fsben

    reform_ben (junit,15,2) =   not_head_nami_flag     * base_fsben
    reform_ben (junit,16,2) =   not_head_euro_flag     * base_fsben
    reform_ben (junit,17,2) =   not_head_asia_flag     * base_fsben
    reform_ben (junit,18,2) =   not_head_mide_flag     * base_fsben
    reform_ben (junit,19,2) =   not_head_cent_flag     * base_fsben
    reform_ben (junit,20,2) =   not_head_afri_flag     * base_fsben
    reform_ben (junit,21,2) =   not_head_else_flag     * base_fsben
    reform_ben (junit,22,2) =   not_head_unkn_flag     * base_fsben

    !--- persons (per-capita-ben)

    reform_ben  (junit,23,1) = pc_fsben * fsusize
    reform_ben  (junit,23,2) = 0


    !--- persons
    reform_ben (junit,24,1) =  pc_fsben *  fsnkid
    reform_ben (junit,25,1) =  pc_fsben *  fsnadult
    reform_ben (junit,26,1) =  pc_fsben *  fsnelder

    reform_ben (junit,24,2) =  pc_fsben *  not_fsnkid
    reform_ben (junit,25,2) =  pc_fsben *  not_nadult
    reform_ben (junit,26,2) =  pc_fsben *  not_fsnelder

    !--- sex
    reform_ben (junit,27,1) =  pc_fsben *  fsnmale
    reform_ben (junit,28,1) =  pc_fsben *  fsnfemale

    reform_ben (junit,27,2) =  pc_fsben *  fsnfemale
    reform_ben (junit,28,2) =  pc_fsben *  fsnmale

    !--- disab
    reform_ben (junit,29,1) =  pc_fsben *  fsndis
    reform_ben (junit,30,1) =  pc_fsben * (fsusize - fsndis)
    reform_ben (junit,29,2) =  pc_fsben * (fsusize - fsndis)
    reform_ben (junit,30,2) =  pc_fsben *  fsndis

    if (model_code == "QCMM") THEN
       reform_ben (junit,29:30,1:2) = 0
    end if



    do j = 1, num_pcts
       do n = 1, 2   !! in/out of subset
          do m = 1, 2      !! elig/part

             if (kth > 1 .and. icat > 0)  then
                totalgl (j,m,n,nth, icat) = totalgl (j,m,n,nth, icat) + reform_est(junit,j,n) * wgt
             end if

          end do
       end do
    end do




    !! tab protect gl

    if (kth == 1) return
    if (icat == 0) return

    do j = 1,num_pcts     !! var
       do n = 1,2         !! in/out

         do kk = 1, NCAT !! g/l cat
          k = icat

          if (cat_count(k,kk)  == 0)  then
             jj = 0
             delta = 0.0

          elseif  (cat_count(k,kk) == 1) then

             if (ipartic(junit,0) == 0) baselaw_ben (junit, j, n) = 0
             if (ipartic(junit,nth) == 0) reform_ben (junit, j, n) = 0

             jj = reform_est (junit,j,n)
             delta = (reform_ben (junit,j,n) - baselaw_ben (junit,j,n))

          else
             cycle
          endif

          do ib = 0, num_wgt
             sum_delta_glb(ib, j,n,nth,k) = sum_delta_glb(ib, j,n,nth,k) + delta * b_wgt(ib, jkist)
             sum_delta_gl (ib, j,n,nth,k) = sum_delta_gl (ib, j,n,nth,k) + jj    * b_wgt(ib, jkist)
          end do

          delta_ben(j,n,nth,k) = delta_ben(j,n,nth,k) + delta * wgt

         end do  !! kk
       end do    !! n
    end do       !! j


    return



!------------------------------------------------------------------------------
900 continue     !---- phase 3 processing -------------------------------------
!------------------------------------------------------------------------------
    if (nth > 1) RETURN  !!  calc only once per run

    !-------------- Compute Standard Error of Change Estimates ------------

    write(prfile, *) " "
    write(prfile, *) " *---------------------------"
    write(prfile, *) " * In tabgl stats, keof = 3"
    write(prfile, *) " *---------------------------"
    write(prfile, *) " "

    var_protect_gl_stats = " "
    var_protect_gl_stats_ben = " "

    do p = 1, nplan10
       do j = 1, num_pcts
          do m = 1,2
             do n = 1,2

                do k = 1,ncat

                   do i = 1, num_wgt

                      !! table gl
                      if (m == 2) then


                        var_gl(j,n,p,k) = var_gl(j,n,p,k) + &
                          (sum_delta_gl(i, j,n,p,k) - sum_delta_gl(0, j,n,p,k) )   &
                        * (sum_delta_gl(i, j,n,p,k) - sum_delta_gl(0, j,n,p,k) )


                         var_glb(j,n,p,k) = var_glb(j,n,p,k) + &
                          (sum_delta_glb(i, j,n,p,k) - sum_delta_glb(0, j,n,p,k) )   &
                        * (sum_delta_glb(i, j,n,p,k) - sum_delta_glb(0, j,n,p,k) )

                      end if


                   end do ! i


                   !! partic only:
                   if (m == 2) then

                      var_gl(j,n,p,k) =  var_const / num_wgt * var_gl(j,n,p,k)
                      var_glb(j,n,p,k) = var_const / num_wgt * var_glb(j,n,p,k)

                      sd_gl(j,n,p,k) = sqrt( var_gl(j,n,p,k) )
                      if (abs(sd_gl(j,n,p,k)) > 0.0) then 
                        z1_gl(j,2,n,p,k) = totalgl (j,2,n,p,k) / sd_gl(j,n,p,k)                   
                        IF (ABS(z1_gl(J,2,n,p,k) ) > sig_level_90) VAR_protect_gl_STATS(J,2,n,p,k)  = "*"
                      end if
                   end if

                end do ! K


                do k = 1,ncat

                   if (abs(totalgl (j,2,n,0,1)) > 0.0) &
                      ptotgl (j,2,n,p,k) = totalgl (j,2,n,p,k) / totalgl (j,2,n,0,1) * 100.0
 
                   if (abs(total_ben(j,n,0,1)) > 0.0) &
                      ptot_ben(j,n,p,k) =  delta_ben(j,n,p,k) / total_ben(j,n,0,1) * 100.0

                end do

             end do  ! n 
         
          
          end do ! m 
  
          
          do k = 1, ncat 
            
             if (abs(totalgl (j,2,2,p,k)) > 0.0) then
                if (abs(var_gl(j,1,p,k)) > 0.0 .and. abs(var_gl(j,2,p,k)) > 0.0) then
                   z2_gl(j,2,p,k) = (totalgl (j,2,1,p,k) - totalgl (j,2,2,p,k) ) &
                               / sqrt( var_gl(j,1,p,k) + var_gl(j,2,p,k))

                   if (abs(z2_gl(j,2,p,k)) > sig_level_90) then
                      if (var_protect_gl_stats(J,2,1,p,k) == "*") then
                          var_protect_gl_stats(J,2,1,p,k) = "#"
                      else
                          var_protect_gl_stats(J,2,1,p,k) = "+"
                      end if
                   end if

                end if
             end if


             !!  benefit
             do n = 1,2


                sd_glb(j,n,p,k) = sqrt( var_glb(j,n,p,k) )
                if (abs(sd_glb(j,n,p,k)) > 0.0) then
                  z1_glb(j,n,p,k) = sum_delta_glb(0, j,n,p,k) / sd_glb(j,n,p,k)   
               
                  IF (ABS(z1_glb(J,n,p,k) ) > sig_level_90) VAR_protect_gl_STATS_ben(J,2,n,p,k)  = "*"
                end if

             end do

             !!  skip if nobody in "not in group" -- the "total" groups
             if (.NOT. (abs(totalgl (j,2,2,p,k)) > 0.0)) cycle

             if (abs(var_glb(j,1,p,k)) > 0.0 .and. abs(var_glb(j,2,p,k)) > 0.0) then
                z2_glb(j,p,k) = (sum_delta_glb(0,j,2,p,k) - sum_delta_glb(0,j,1,p,k) ) &
                            / sqrt( var_glb(j,1,p,k) + var_glb(j,2,p,k))

                if (abs(z2_glb(j,p,k)) > sig_level_90) then
                   if (var_protect_gl_stats_ben(J,2,1,p,k) == "*") then
                       var_protect_gl_stats_ben(J,2,1,p,k) = "#"
                   else
                       var_protect_gl_stats_ben(J,2,1,p,k) = "+"
                   end if
                end if

             end if  !! end benefit

          end do  ! cat

       end do     ! var
    end do        ! plan




    !!  move stats to g/l table
    tab_gl_stats = " "
    tab_gl_stats_ben = " "

    !! set for 1st wafer only:
    do p = 1, nplan10

       do k = 1,8   !! elig/part
          if (k == 4) cycle
          tab_gl_stats   (1,p,k+2,1) = var_protect_gl_stats    (1,2,1,p,k)
          tab_gl_stats   (2,p,k+2,1) = var_protect_gl_stats    (2,2,1,p,k)
          tab_gl_stats_ben(  p,k+2,1) = var_protect_gl_stats_ben(1,2,1,p,k)
       end do  ! cat

       tab_gl_stats    (1,p,1,1) = var_protect_gl_stats    (1,2,1,p,4)
       tab_gl_stats    (2,p,1,1) = var_protect_gl_stats    (2,2,1,p,4)
       tab_gl_stats_ben(  p,1,1) = var_protect_gl_stats_ben(1,2,1,p,4)

       tab_gl_stats    (1,p,6,1) = var_protect_gl_stats    (1,2,1,p,9)
       tab_gl_stats    (2,p,6,1) = var_protect_gl_stats    (2,2,1,p,9)
       tab_gl_stats_ben(  p,6,1) = var_protect_gl_stats_ben(1,2,1,p,9)

       tab_gl_stats    (1,p,2,1) = var_protect_gl_stats    (1,2,1,p,10)
       tab_gl_stats    (2,p,2,1) = var_protect_gl_stats    (2,2,1,p,10)
       tab_gl_stats_ben(  p,2,1) = var_protect_gl_stats_ben(1,2,1,p,10)

    end do




    do p = 1, nplan10
       do n = 1,2      !! in/out of subgroup
             do k = 1,ncat   !! elig/part
                write(prfile, 6050) p, sub_label(n), cat_label(k)
                write(prfile, 6060) &
                 (stat_label2(2) // stat_label(j) &
                 ,totalgl     (j,2,n,0,1)  &
                 ,totalgl     (j,2,n,p,k)  &
                 ,ptotgl      (j,2,n,p,k)  &
                 ,sd_gl       (j,  n,p,k)  &
                 ,var_protect_gl_stats(j,2,n,p,k)  &
                 ,z1_gl       (j,2,n,p,k)  &
                 ,z2_gl       (j,2,  p,k),  j = 1,num_pcts)

             end do       !! end cat
       end do

    end do  !!  end plan


 6050  format(//, t2, "TAB Protect GL: Stat Summary for NTH = ", i2, 5x, a, "  Cat:", a  &
   ,//,t2,"Var",t15,"Baselaw",t30,"Reform",t45,"Pct Chg",t60,"SD",t72,"Z1_test",t84,"Z2_test"   &
   ,/, t2,"---",t15,"-------",t30,"------",t45,"-------",t60,"--",t72,"-------",t84,"-------" )

 6060  format(t2, a, t15, f12.0, t30, f12.0, t45, f8.2, t60, f10.4, a, t72, f10.4, t84,f10.4)


    !!  BENEFITS
    do p = 1, nplan10
       do n = 1,2      !! in/out of subgroup
             do k = 1,ncat   !!  tot gain, tot lose only
                write(prfile, 7010) p, sub_label(n), cat_label(k)
                write(prfile, 7020) &
                  (stat_label2(2) // stat_label(j) &
                  ,total_ben      (j,  n,0,1)  &
                  ,sum_delta_glb  (0,j,n,p,k)    &
                  ,ptot_ben       (j,  n,p,k)  &
                  ,sd_glb     (j  ,n,p,k) &
                  ,var_protect_gl_stats_ben(j,2,n,p,k) &
                  ,z1_glb    (j,  n,p,k) &
                  ,z2_glb    (j,    p,k) ,   j = 1,num_pcts)
             end do       !! end cat
       end do

    end do  !!  end plan

 7010  format(//, t2, "TABLE gl Benefits: Stat Summary for NTH = ", i2, 5x, a, "  Cat:", a  &
 ,//,t2,"Var",t15,"Baselaw",t30,"Reform",t45,"Pct Chg",t60,"SD",t83,"Z1_test",t100,"Z2_test"&
 ,/, t2,"---",t15,"-------",t30,"------",t45,"-------",t60,"--",t83,"-------",t100,"-------")

 7020  format(t2, a, t15, f12.0, t30, f12.0, t45, f8.3, t60, f15.4, a, t78, f15.4, t90,f15.4)

  
    write(prfile, 8000) 
    do i = 1, ncat
       write(prfile, 8010) i, cat_label(i), nbr_cat(i), wgt_nbr_cat(i)     
    end do
 
 8000 format(//, t2, "Summary Count, G/L Categories"   & 
  ,//, t5, "Cat",2x,"           Cat Label",2x,"Num in Cat",2x,"Wgt Num in Cat"   & 
   ,/, t5, "---",2x,"--------------------",2x,"----------",2x,"--------------"   ) 
 8010  format(t5,i3, 2x,a20, 2x,i10, 2x, f12.0) 




    END SUBROUTINE
