!**************************************************************************************************
! Source File:  DBDEFINE.F90                
! Called By:    FSTAMP2                     
!
! Assigns household definer variables.  None of these vary by NTH.
!
!**************************************************************************************************
    subroutine db_fs_hh_definers

    use global
    USE GLOBPARM
    USE userparm
    use states
    use fssizes
    use fswork
    use fsparm
    use fslocs
    use fs_dblocs
    use fs_dbparm
    use fs_dbwork
    use fs_dbdefine
    use utils
    implicit none

    integer :: ip, i

    INTEGER :: hh8
    INTEGER ::  nbr_repwgt_read = 0
    INTEGER ::  nbr_repwgt_nonmatch = 0
    LOGICAL :: FIRST_CALL  = .true.
    character (130) :: in_wgt


    !---- State codes are used to index into the REGION_LOOKUP table
    !---- to identify the corresponding REGION.

    integer, parameter :: region_lookup(80)  = (/ &
    ! 1   2   3   4   5   6   7   8   9  10       !<----- column #
      3,  4,  0,  4,  3,  4,  0,  4,  1,  3,  &   !  1-10
      3,  3,  3,  0,  4,  4,  2,  2,  2,  2,  &   ! 11-20
      3,  3,  1,  3,  1,  2,  2,  3,  2,  4,  &   ! 21-30
      2,  4,  1,  1,  4,  1,  3,  2,  2,  3,  &   ! 31-40
      4,  1,  0,  1,  3,  2,  3,  3,  4,  1,  &   ! 41-50
      3,  0,  4,  3,  2,  4,  0,  0,  0,  0,  &   ! 51-60
      0,  0,  0,  0,  0,  4,  0,  0,  0,  0,  &   ! 61-70
      0,  0,  0,  0,  0,  0,  0,  4,  0,  0   /)  ! 71-80

    !--------------------------------------------------------------------
    ! BEGIN PROCESSING
    !--------------------------------------------------------------------

    select case(keof)
      case(1)

        !--- set stats parameters
        if (nth == 1) then
           num_pcts = 30
           var_const = 1.0
           num_wgt = 500
           ALLOCATE (b_wgt(0:num_wgt, 1))
        end if

        return
      case(3)
        goto 900
    end select

    !-----------------------------------------------------------------------------------
    !  open bootstrap file if stats are called
    !-----------------------------------------------------------------------------------
    if (first_call .and. dostats(1)) then
       first_call = .false.

       IF (DOSTATS(1)) then
          in_wgt = TRIM (in_directory) // "\MATHPC.BWT"
          open(unit=43, file = in_wgt, ERR=150, FORM='unformatted', ACCESS='stream', STATUS='old')
       end if

       GOTO 160
150    call error_msg('DBDEFINE',' UNABLE TO OPEN THE BOOTSTRAP WGT FILE', ABORT)
       write (prfile, *) " *** Err: Input weight file = ", in_wgt
       RETURN

160    CONTINUE

    end if



    !-----------------------------------------------------------------------------------
    !  Read in replicate wgts.
    !  Notes:
    !
    !  1.  b_wgt(0, 1) = fywgt
    !  2.  there is one record per ORIGINAL QC file.  Skip nonmatches to the final file
    !-----------------------------------------------------------------------------------
    if (dostats(1) .and. NTH == 1) then

100    CONTINUE
       READ(43)   &
           hh8    &
         ,(b_wgt(i, 1), i = 1, num_wgt)

       b_wgt(0, 1) = l_fywgt%hhld

       nbr_repwgt_read = nbr_repwgt_read + 1

       if (hh8 /= hhid) then
          nbr_repwgt_nonmatch = nbr_repwgt_nonmatch + 1
          if (nbr_repwgt_nonmatch <= 5 .and. prlevel(1) >= 5) then
             call debug_msg("DBDEFINE: RepWgt Nonmatch", nbr_repwgt_nonmatch)
             call isnewpg(prfile, 6)
             WRITE(prfile, 2010)  &
                hhid, hh8         &
               ,l_fywgt%hhld,  b_wgt(0, 1)
          end if

          GO TO 100  !! skip to next record
       end if

       !------------------------------
       !---  adjust replicate weights
       !------------------------------
       do i = 1, num_wgt
          b_wgt(i, 1) = b_wgt(i, 1) / 12.0
       end do


    end if


 2010  FORMAT( //, "Rep Weight file debug: "  &
 ,//, t2, "HHID:  ", 2i10    &
 ,/,  t2, "FYWGT: ", 2f10.4  )



    !---  fstate is the actual state
    fstate = l_state%ihhld

    istate = state_idx(fstate)

    !--------------------------------------------------
    !--- Set optional skip MN_FIP & SSI_CAP households:
    !--------------------------------------------------
    select case (fstate)
        !!    mn
        case (27)
           if (l_mn_fip%ihhld == 1) then
              if (xmn_fip(nth)) skip_hh = .true.
           end if

        !!    fl  ma  wa
        case (12, 25, 53)
           if (l_ssi_cap%ihhld == 1) then
              if (FSTATE == 12 .AND. xscap_fl(nth)) skip_hh = .true.
              if (FSTATE == 25 .AND. xscap_ma(nth)) skip_hh = .true.
              if (FSTATE == 53 .AND. xscap_wa(nth)) skip_hh = .true.
           end if

        !!    az  ky  la  md  mi  ms  ny  nc  pa  sc  sd  tx  va
        case ( 4, 21, 22, 24, 26, 28, 36, 37, 42, 45, 46, 48, 51)
           if (l_ssi_cap%ihhld == 2 .or. l_ssi_cap%ihhld == 3) then
              if (FSTATE ==  4 .AND. xscap_AZ(nth)) skip_hh = .true.
              if (FSTATE == 21 .AND. xscap_ky(nth)) skip_hh = .true.
              if (FSTATE == 22 .AND. xscap_la(nth)) skip_hh = .true.
              if (FSTATE == 24 .AND. xscap_md(nth)) skip_hh = .true.
              if (FSTATE == 26 .AND. xscap_mi(nth)) skip_hh = .true.
              if (FSTATE == 28 .AND. xscap_ms(nth)) skip_hh = .true.
              if (FSTATE == 36 .AND. xscap_ny(nth)) skip_hh = .true.
              if (FSTATE == 37 .AND. xscap_nc(nth)) skip_hh = .true.
              if (FSTATE == 42 .AND. xscap_pa(nth)) skip_hh = .true.
              if (FSTATE == 45 .AND. xscap_sc(nth)) skip_hh = .true.
              if (FSTATE == 46 .AND. xscap_sd(nth)) skip_hh = .true.
              if (FSTATE == 48 .AND. xscap_tx(nth)) skip_hh = .true.
              if (FSTATE == 51 .AND. xscap_va(nth)) skip_hh = .true.
           end if

        case default
           !
    end select


    if (nth > 1) return  ! the remaining calculations are only done once.

    !!  FOR PROTECTED_CLASS TABLES:
    PERSON_LEVEL_DISAB = .TRUE.
    AREA_OF_ORIGIN     = .false.
    USE_HEAD_RACE      = .TRUE.



    !--------------------------------------------------------------------------
    !---- weight variable
    !--------------------------------------------------------------------------

    wgt = l_fywgt%hhld

    wgt1(kist) = wgt

    !-------------------------------------------------------------------------
    !---- U.S., Alaska, Hawaii, Guam & VI geographic indicators.  GEOG_DED
    !---- indexes the standard deduction, child care deduction, and shelter
    !---- deduction arrays; GEOG_SCRN indexes the gross & net income screen
    !---- arrays; GEOG_BEN indexes the maximum benefit array; and GEOG_POV
    !---- indexes the POVMONTH array.
    !-------------------------------------------------------------------------

    select case (l_state%ihhld)
      case(15)                        !! hawaii
        geog_ded  = 3
        geog_scrn = 3
        geog_ben  = 5
      case(2)                         !! alaska
        geog_ded  = 2
        geog_scrn = 2

        !-------------------------------------------------------------------------
        !--- use MIN BEN to circumvent lack of localcod info
        !--- Note: benmax renamed to hbenmax to avoide conflict with parm benmax
        !-------------------------------------------------------------------------
        select case(l_ak_area%ihhld)
          case(1)                    !! alaska rural i
            geog_ben = 3
          case(2)                    !! alaska rural ii
            geog_ben = 4
          case default
            geog_ben = 2             !! alaska urban is default
        end select



      case(66)                        !! guam
        geog_ded = 4
        geog_scrn= 1
        geog_ben = 6
      case(78)                        !! virgin islands
        geog_ded = 5
        geog_scrn= 1
        geog_ben = 7
      case default
        geog_ded  = 1
        geog_scrn = 1
        geog_ben  = 1
    end select

    geog_pov = geog_scrn

    region = region_lookup(l_state%ihhld)

    !------------------------------------------------------------------------------
    !--- Get original QC values for imputation of shelter, medical, and
    !--- dependent care expenses (FSSLTEXP, FSMEDEXP, FSDEDEXP), in cases
    !--- where the FSU is not the original FSU.  Note that all of the calculations
    !--- below MUST be based on the original FSU and its data, even if a new
    !--- baselaw has been constructed.
    !------------------------------------------------------------------------------
    orig_fsmedexp = l_original_fsmedexp%ihhld
    orig_fssltexp = l_original_fssltexp%ihhld
    orig_fsdepded = l_original_fsdepded%ihhld
    orig_fscsded  = l_original_fscsded %ihhld

    orig_fsuhead = 0
    hhtanf = 0
    orig_kids_lt15 = 0
    do ip = 1, ctprhh
       if (l_original_fsun%iper(ip) == ip) orig_fsuhead = ip
       if (l_tanf%iper(ip) > 0) hhtanf = hhtanf + l_tanf%iper(ip)
       if (l_original_fsun%iper(ip) == 0) cycle
       if (l_age%iper(ip) >= 0  .and. l_age%iper(ip) < 15) orig_kids_lt15  = orig_kids_lt15  + 1
    enddo

    orig_fsusize  = l_original_fsusize %iper(orig_fsuhead)
    orig_fsnkid   = l_original_fsnkid  %iper(orig_fsuhead)
    orig_fsnelder = l_original_fsnelder%iper(orig_fsuhead)
    orig_fsndis   = l_original_fsndis  %iper(orig_fsuhead)
    orig_fsasset  = l_original_fsasset %iper(orig_fsuhead)  



    return


900 continue

    write(prfile, 9010) &
       nbr_repwgt_read  &
      ,nbr_repwgt_nonmatch

9010  format(//, t2, "Replicate weight file: " &
 ,/,t2, "Nbr RepWgt records read: ", i10       &
 ,/,t2, "Nbr RepWgt non-matches:  ", i10       &
   /)


    return
    end



    function calc_povlinex (isize, geog_pov)
!--------------------------------------------------------------
!   Generic function returns poverty line
!
!   Input parameters:
!      isize is unit size index
!      geog_pov is the geogrphic index
!      DEFL_GEN is the generic income deflator
!      num_povmonth is the number of rows in the povmonth array
!--------------------------------------------------------------
    use GLOBAL
    use GLOBPARM
    USE fswork, ONLY : defl_gen
    implicit none

    real(8) :: CALC_POVLINEx
    integer, intent(in) :: isize, geog_pov
    integer :: j

    j = num_povmonth

    if (isize < j) then
       calc_povlinex = defl_gen *  povmonth(isize, geog_pov)
    else
       calc_povlinex = defl_gen * (povmonth(j-1,   geog_pov) + povmonth(j, geog_pov) * (isize - j + 1))
    end if

    return
    end
