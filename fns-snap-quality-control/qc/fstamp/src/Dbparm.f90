!**************************************************************************************************
! Source File:  DBPARM.F90                 
! Called By:    FSPARM/FSTAMP2             
!
! This file contains three subroutines:
! (1) Reads and processes database-specific user parameters.
! (2) Sets generic FSTAMP array limits to values needed for QC model.
! (3) Performs database-specific validation of user parameters.
!
!**************************************************************************************************
    subroutine db_fs_readparm (param)

    use global
    USE fs_dbparm
    USE fsparm
    USE fswork, ONLY : defl_gen



    implicit none

    character(LEN=8), intent(in) ::  param
    integer :: i,j



    select case (param)

       case ('DOSTATS ')
          read (prmfile, 1590,err=800) DOSTATS(NTH)

       case ('SHELCAP1')
          do i = 1, num_shelcap_region
            read(prmfile,1570,err=800) shelcap1(i,nth), shelcap1_region(i)

            shelcap1(i, nth) = shelcap1(i, nth) * defl_gen

          end do

       case ('MN_BEN')
          read (prmfile, 1550) mn_ben_header
          do i = 1, nbr_mn_ben_fsusize
             read(prmfile,1610,err=800) (MN_BEN(i,j,nth), j=1,max_mn_ben_parms), mn_ben_label(i)

            do j = 1, max_mn_ben_parms
               mn_ben(i, j, nth) = mn_ben(i, j, nth) * defl_gen
            end do

          end do

       case ('MNERNDED')
          read (prmfile, 1280,err=800) MNERNDED(nth)

       case ('XMN_FIP ')
          read (prmfile, 1590,err=800) XMN_FIP(NTH)

       case ('XSCAP_AZ')
          read (prmfile, 1590,err=800) XSCAP_AZ(NTH)

       case ('XSCAP_FL')
          read (prmfile, 1590,err=800) XSCAP_FL(NTH)

       case ('XSCAP_MA')
          read (prmfile, 1590,err=800) XSCAP_MA(NTH)

       case ('XSCAP_MD')
          read (prmfile, 1590,err=800) XSCAP_MD(NTH)

       case ('XSCAP_MI')
          read (prmfile, 1590,err=800) XSCAP_MI(NTH)

       case ('XSCAP_MS')
          read (prmfile, 1590,err=800) XSCAP_MS(NTH)

       case ('XSCAP_NC')
          read (prmfile, 1590,err=800) XSCAP_NC(NTH)

       case ('XSCAP_NJ')
          read (prmfile, 1590,err=800) XSCAP_NJ(NTH)

       case ('XSCAP_NY')
          read (prmfile, 1590,err=800) XSCAP_NY(NTH)

       case ('XSCAP_SC')
          read (prmfile, 1590,err=800) XSCAP_SC(NTH)

       case ('XSCAP_SD')
          read (prmfile, 1590,err=800) XSCAP_SD(NTH)

       case ('XSCAP_TX')
          read (prmfile, 1590,err=800) XSCAP_TX(NTH)

       case ('XSCAP_WA')
          read (prmfile, 1590,err=800) XSCAP_WA(NTH)

       case ('XSCAP_KY')
          read (prmfile, 1590,err=800) XSCAP_KY(NTH)

       case ('XSCAP_LA')
          read (prmfile, 1590,err=800) XSCAP_LA(NTH)

       case ('XSCAP_PA')
          read (prmfile, 1590,err=800) XSCAP_PA(NTH)

       case ('XSCAP_VA')
          read (prmfile, 1590,err=800) XSCAP_VA(NTH)


       case default
          abend_reason = 'ERROR - ' // param // ' IS AN UNKNOWN USER PARAMETER!!'
          call error_msg ('DB_FS_READPARM', abend_reason, abort)


    end select


    ben_chg_date = 201410


    return


800 continue

    abend_reason =  'Error reading the parameter file, on parameter: ' // param
    call error_msg ('DB_FS_READPARM', abend_reason, abort)

    return


1570 FORMAT(t13, F10.0,5X, A)   

1550 format(t13, a)          

1590 FORMAT(t13, L10)
1610 format(t13, 2F10.0, 5X, A)   

1280 FORMAT(t13, F10.4)   

    end



!**************************************************************************************************
    subroutine db_fs_parm_array_sizes
    use global
    use fsparm
    USE fswork, ONLY : defl_gen, defl_veh, start_kist, end_kist, gen_array_size
    USE fs_dbparm
    USE states, ONLY : nstates

    implicit none

    integer :: i

    !--------------------------------------------------------------------
    ! BEGIN PROCESSING
    !--------------------------------------------------------------------
    if (nth >1) return


    defl_gen = 1.0
    defl_VEH = 1.0

    start_kist = 0
    end_kist = 0

    gen_array_size = 1





    num_benmax_region   = 7
    num_benmin_region   = 7
    num_depmax_region   = 5
    num_screen_region   = 3
    num_shelcap_region  = 5
    num_standded_region = 5

    ALLOCATE (ASSETLIM(0:nstates, MAX_ASSETLIM, MAX_NTH))


      do i = 1, num_med_demo
         med_demo_thres(i) = NINT(REAL(med_demo_thres(i)) * defl_gen)
         med_demo_min  (i) = NINT(REAL(med_demo_min  (i)) * defl_gen)
         med_demo_stddedred(i) = NINT(REAL(med_demo_stddedred(i)) * defl_gen)
      end do
    end if



    return
    end




!**************************************************************************************************
    subroutine db_fs_validate_parm

    use global
    use fsparm
    USE fs_dbparm
    implicit none

    !--------------------------------------------------------------------
    ! BEGIN PROCESSING
    !--------------------------------------------------------------------
    !---- The QC model does not support BASELAW = ' '.  This is because a
    !---- new baselaw does not require any processing that is different from
    !---- a normal reform plan.   For new baselaws, use BASELAW = FS_VARS in the
    !---- NTH = 1 parameter set, or write the new baselaw plan to a new MATH file.

    if (baselaw(nth) == ' ')  &
       call error_msg ('DB_FS_VALIDATE_PARM',  &
          'QC MODEL DOES NOT SUPPORT BASELAW = BLANK' , abort)



    !---- FS_VARS = 1 is not allowed, because the ORIG_ variables set in
    !---- DBDEFINE need the ORIGINAL QC baselaw, because the original data is
    !---- needed in DBVARS for imputing medical, shelter, and dependent care
    !---- expenses, and countable assets (when the unit composition is not that
    !---- of the  niginal unit).

    if ( fs_vars(nth) == '1')  &
       call error_msg ('DB_FS_VALIDATE_PARM',  &
         'QC MODEL DOES NOT ALLOW FS_VARS = 1 (ORIGINAL QC VARS NEEDED)', 25)


    !---- dostats parms cannot vary from plan to plan:
          if (DOSTATS(NTH) .neqv. DOSTATS(1)) then
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW DOSTATS TO VARY PLAN TO PLAN', abort)
          end if

    !---- Exclude HH parms cannot vary from plan to plan:
          if (XMN_FIP(NTH) .neqv. XMN_FIP(1)) then
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW XMN_FIP TO VARY PLAN TO PLAN', abort)
          end if
          if (XSCAP_AZ(NTH) .neqv. XSCAP_AZ(1)) &
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW XSCAP_AZ TO VARY PLAN TO PLAN', abort)
          if (XSCAP_FL(NTH) .neqv. XSCAP_FL(1)) &
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW XSCAP_FL TO VARY PLAN TO PLAN', abort)
          if (XSCAP_MA(NTH) .neqv. XSCAP_MA(1)) &
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW XSCAP_MA TO VARY PLAN TO PLAN', abort)
          if (XSCAP_MS(NTH) .neqv. XSCAP_MS(1)) &
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW XSCAP_MS TO VARY PLAN TO PLAN', abort)
          if (XSCAP_NC(NTH) .neqv. XSCAP_NC(1)) &
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW XSCAP_NC TO VARY PLAN TO PLAN', abort)
          if (XSCAP_NY(NTH) .neqv. XSCAP_NY(1)) &
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW XSCAP_NY TO VARY PLAN TO PLAN', abort)
          if (XSCAP_SC(NTH) .neqv. XSCAP_SC(1)) &
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW XSCAP_SC TO VARY PLAN TO PLAN', abort)
          if (XSCAP_TX(NTH) .neqv. XSCAP_TX(1)) &
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW XSCAP_TX TO VARY PLAN TO PLAN', abort)
          if (XSCAP_WA(NTH) .neqv. XSCAP_WA(1)) &
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW XSCAP_WA TO VARY PLAN TO PLAN', abort)

          if (XSCAP_KY(NTH) .neqv. XSCAP_KY(1)) &
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW XSCAP_KY TO VARY PLAN TO PLAN', abort)
          if (XSCAP_LA(NTH) .neqv. XSCAP_LA(1)) &
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW XSCAP_LA TO VARY PLAN TO PLAN', abort)
          if (XSCAP_PA(NTH) .neqv. XSCAP_PA(1)) &
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW XSCAP_PA TO VARY PLAN TO PLAN', abort)
          if (XSCAP_VA(NTH) .neqv. XSCAP_VA(1)) &
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW XSCAP_VA TO VARY PLAN TO PLAN', abort)
          if (XSCAP_MI(NTH) .neqv. XSCAP_MI(1)) &
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW XSCAP_MI TO VARY PLAN TO PLAN', abort)
          if (XSCAP_NJ(NTH) .neqv. XSCAP_NJ(1)) &
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW XSCAP_NJ TO VARY PLAN TO PLAN', abort)
          if (XSCAP_MD(NTH) .neqv. XSCAP_MD(1)) &
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW XSCAP_MD TO VARY PLAN TO PLAN', abort)
          if (XSCAP_NM(NTH) .neqv. XSCAP_NM(1)) &
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW XSCAP_NM TO VARY PLAN TO PLAN', abort)
          if (XSCAP_SD(NTH) .neqv. XSCAP_SD(1)) &
             call error_msg ('DB_FS_VALIDATE_PARM',  &
               'QC MODEL DOES NOT ALLOW XSCAP_SD TO VARY PLAN TO PLAN', abort)


    return
    end
