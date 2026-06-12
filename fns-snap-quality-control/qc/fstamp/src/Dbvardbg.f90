!**************************************************************************************************
! Source File:  DBVARDBG.F90                
! Called By:    FSTAMP2                     
!
! Prints debug for the following database-specific routines:
!    DB_FS_HH_DEFINERS,   DB_FS_UNIT,   DB_FS_VARS
!
!**************************************************************************************************
    subroutine db_fs_display_debug

    use global
    use states
    use fsparm
    use fswork
    use fs_dblocs
    use fs_dbdefine
    use fs_dbwork
    use fs_dbparm, only : mnernded
    implicit none
    integer ::  ip, iunit, num_in_fsu

    integer, dimension(max_persons) :: other_income
    !-----------------------------------------------------------------
    ! BEGIN PROCESSING
    !--------------------------------------------------------------------
    if (kfreq == 0) return   !  no debug requested for this hh
 

    call isnewpg (prfile, 6)
    write(prfile, 900) nth, plannbr(nth), planname(nth) &
           ,hhid   , hh_count                           &
           ,l_state%ihhld                                 &
           ,state_name(state_idx(l_state%ihhld))          &
           ,wgt
 
    if (prlevel(nth) < no_loop_prlevel) return 

    !--------------------------------------------------------------------
    ! DEBUG FOR DB_FS_HH_DEFINERS
    !--------------------------------------------------------------------
    call isnewpg(prfile, 10)
    write(prfile, 1000) &
          geog_ben      &
         ,geog_scrn     &
         ,geog_ded      &
         ,geog_pov 
 
    call isnewpg(prfile, 12)
    write(prfile, 1010) &
              ORIG_FSASSET    ,ORIG_FSUSIZE   & 
             ,ORIG_FSMEDEXP   ,ORIG_KIDS_LT15 & 
             ,ORIG_FSDEPDED   ,ORIG_FSNKID    &   
             ,ORIG_FSSLTEXP   ,ORIG_FSNELDER  & 
             ,ORIG_FSNDIS

    !--------------------------------------------------------------------
    ! DEBUG FOR DB_FS_UNIT
    !--------------------------------------------------------------------
    call isnewpg(prfile, 6 + ctprhh)
    write(prfile, 2000)
 
    do ip = 1, ctprhh
      write(prfile, 2110)    &
            ip               &
           ,fsun(ip)         &
           ,l_fsafil%iper(ip)  &
           ,l_age   %iper(ip)  &
           ,l_sex   %iper(ip)  &
           ,l_rel   %iper(ip)
    end do


    !--------------------------------------------------------------------
    ! LEAVE DEBUG ROUTINE IF NO ONE IN HH IS CATEGORICALLY ELIGIBLE
    !--------------------------------------------------------------------
    if (.not. potentially_elig_hh) return  ! no more debug to show
 

    !--------------------------------------------------------------------
    ! DEBUG FOR DB_FS_VARS
    !--------------------------------------------------------------------
    !--- Determine how many persons have FSUN > 0, to control the NUMLINES closely

    num_in_fsu = 0
    do ip = 1, ctprhh
       if (fsun(ip) > 0) num_in_fsu = num_in_fsu + 1
    end do

    other_income = 0 

    !--------------------------------------------------------------------
    !---- Print details if PRLEVEL set at mid-level
    !--------------------------------------------------------------------
 
    if (prlevel(nth) >= no_loop_prlevel) then
 
       !--- number of children, adults, elderly, disabled
 
       call isnewpg (prfile, num_in_fsu + 10)
       write(prfile, 3000)  max_kid_age , min_school_age , min_elderly_age
       do iunit = 1, ctprhh
          do ip = 1, ctprhh
            if (fsun(ip) /= iunit) cycle ! not in this unit
            write(prfile, 3010) &
             ip               &
            ,fsun    (ip)     &
            ,l_age%iper(ip)     &
            ,fsnkid  (ip)     &
            ,fsnadult(ip)     &
            ,fsusize (ip)     &
            ,fsnk5t17(ip)     &
            ,fsnelder(ip)     &
            ,l_ssi%iper(ip)   &
            ,fsndis  (ip)
          end do
       end do

 3000 FORMAT(                                                          &
       1X,  130('-')                                                   &
     //1X, 'This section shows details regarding the creation of '     &
         , 'variables that describe the composition, income, and '     &
      /1X, 'expenses (if any) of each food stamp unit.  The FSU infor' &
         , 'mation is located on the record of the head of the FSU.'   &
     //1X, 'Unit Composition:'                                         &
      /1X, 'MAX CHILD AGE =', I3, '   MIN SCHOOL AGE =', I3            &
      ,3X, 'MIN ELDERLY AGE =', I3                                     &

     //1X, 'IP  FSUN  AGE  FSNKID  FSNADULT  FSUSIZE  FSNK5T17  FSNELDER  SSI  FSNDIS' &
      /1X, '--  ----  ---  ------  --------  -------  --------  --------  ---  ------' &
      )
 3010 FORMAT(1X, I2, I6, I5, I8, I10, I9, I10, I10, I5, I8)
 
 
       !--- earnings and student definition
 
       call isnewpg (prfile, num_in_fsu + 6)
       write(prfile, 3100)

       do iunit = 1, ctprhh
         do ip = 1, ctprhh
            if (fsun(ip) /= iunit) cycle ! not in this unit
            write(prfile, 3110) &
             ip                 &
            ,fsun       (ip)    &
            ,l_age  %iper (ip)    &
            ,l_emprg%iper (ip)    &
            ,l_wages%iper (ip)    &
            ,l_othern%iper(ip)    &
            ,l_slfemp%iper(ip)    &
            ,fsearn     (ip)
         end do      
       end do      
 
 3100 FORMAT(                                                &
      /1X, 'Countable earnings:'                             &
     //1X, 'IP  FSUN  AGE  EMPRG  WAGES  OTHERN  SLFEMP  FSEARN'   &
      /1X, '--  ----  ---  -----  -----  ------  ------  ------'   &
      )
 3110 FORMAT(1X, I2, I6, I5, I7, I7, I8, I8, I8)


       !--- gross income (over all persons):
 
       call isnewpg(prfile, num_in_fsu + 5)
       write(prfile, 3200) 
       do ip = 1, ctprhh
         write(prfile, 3210) &
          ip                 &
         ,fsun    (ip)       &
         ,l_ssi    %iper(ip)   &
         ,l_tanf   %iper(ip)   &
         ,l_ga     %iper(ip)   &
         ,l_othgov %iper(ip)   &
         ,l_socsec %iper(ip)   &
         ,l_unemp  %iper(ip)   &
         ,l_vet    %iper(ip)   &
         ,l_wcomp  %iper(ip)   &
         ,l_edloan %iper(ip)   &
         ,l_csuprt %iper(ip)   &
         ,l_deem   %iper(ip)   &
         ,l_cont   %iper(ip)   &
         ,l_othun  %iper(ip)   &  
         ,l_diver %iper(ip)    &
         ,l_wgesup%iper(ip)    &
         ,l_energy%iper(ip)    &
         ,l_eitc  %iper(ip)    

          !--- summ 'other income' for debug:
          other_income(ip) = other_income(ip) &  
         + l_othgov %iper(ip)   &
         + l_socsec %iper(ip)   &
         + l_unemp  %iper(ip)   &
         + l_vet    %iper(ip)   &
         + l_wcomp  %iper(ip)   &
         + l_edloan %iper(ip)   &
         + l_csuprt %iper(ip)   &
         + l_deem   %iper(ip)   &
         + l_cont   %iper(ip)   &
         + l_othun  %iper(ip)   &  
         + l_diver %iper(ip)    &
         + l_wgesup%iper(ip)    &
         + l_energy%iper(ip)    &
         + l_eitc  %iper(ip)    
                             
       end do
       
 3200 FORMAT(                           &
 /1X, 'Gross income components:'   &
//1X, 'IP  FSUN  SSI  TANF   GA  OTHGOV  SOSEC  UNEMP  VET  WCOMP  EDLOAN  CSUPRT  DEEM  CONT  OTHUN  DIVER  WGESUP  ENERGY  EITC' &
 /1X, '--  ----  ---  ----  ---  ------  -----  -----  ---  -----  ------  ------  ----  ----  -----  -----  ------  ------  ----' &
      )
 3210 FORMAT(1X, I2, I6, I5, I6, I5, I8, I7, I7, I5, I7, I8, I8, I6, I6, I7, I7, I8, I8, I6)


       call isnewpg(prfile, 5)
       write(prfile, 3220) l_EXFSCSDED%ihhld
       do iunit = 1, ctprhh
          if (fsun(iunit) /= iunit) cycle ! not in this unit
          do ip = 1, ctprhh
            if (fsun(ip) /= iunit) cycle ! not in this unit
            write(prfile, 3230) &
             ip                 &
            ,fsun    (ip)       &
            ,fsearn  (ip)       &
            ,fsssi   (ip)       & 
            ,fstanf  (ip)       & 
            ,fsga    (ip)       & 
            ,other_income(ip)   & 
            ,fsgrinc(ip)
        
          end do
       end do
            
 3220 FORMAT(                   &
 /1X, 'Gross income summary:'   &
//1X, 'EXFSCSDED (subtracted from gross income):', i5   &
//1X, 'IP  FSUN  FSEARN  FSSSI  FSTANF  FSGA   OTHER  FSGRINC ' &
 /1X, '--  ----  ------  -----  ------  ----   -----  ------- ' &
      )
 3230 FORMAT(1X, I2, I6, I8, I7, I8, I6, i8, I9)
            
       do iunit = 1, ctprhh
          if (fsun(iunit) /= iunit) cycle ! not in this unit
          if (fscspded(iunit) > 0) then 
             call isnewpg(prfile, 8)
             write(prfile, 3240)  & 
              fsgrinc(iunit)      &             
             ,fscspded(iunit)     & 
             ,fsgrinc(iunit) - fscspded(iunit) & 
             ,GROSS_SCREEN(IUNIT) & 
             ,FSGRTEST(IUNIT) 
          end if
       end do

3240 format(//,t2, "Gross income test for units with CSP Deduction:"  & 
  ,//,t2, "FSGRINC:      ", i5, 5x, "FSCSPDED: ", i5, 5x, "Difference: ", i5 & 
  , /,t2, "Gross Screen: ", i5, 5x, "FSGRTEST: ", i5 &
  /)  


       !--- All-PA status
 
       call isnewpg(prfile, num_in_fsu + 12)
       do iunit = 1, ctprhh
         if (fsun(iUNIT) /= iunit) cycle ! no unit
         write(prfile, 3300) &
            any_adult, tparent, tkid, paadlt, fsnadult(iunit),fsnpa, l_pure_pa%ihhld
         do ip = 1, ctprhh
           if (fsun(ip) /= iunit) cycle ! not in this unit
           write(prfile, 3310) &
             ip                 &
            ,fsun       (ip)    &
            ,l_age   %iper(ip)  &
            ,PROXY_AGE(ip)      &
            ,l_REL   %iper(ip)  &
            ,l_tanf  %iper(ip)  &
            ,l_ssi   %iper(ip)  &
            ,l_ga    %iper(ip)  &
            ,fstanf     (ip)    &
            ,fsssi      (ip)    &
            ,fsga       (ip)    &
            ,PA_PER     (ip)    &
            ,fsallpa    (ip)
         end do      
       end do      

 3300 FORMAT(                                                   &
   /,1X, 'All-PA determination:'                                &
  ,/,1X, 'ANY_ADULT:     ',L2                                   &
  ,/,1X, 'TPARENT:       ',I2                                   &
  ,/,1X, 'TKID:          ',I2                                   &
  ,/,1X, 'PAADLT:        ',I2                                   &
  ,/,1X, 'FSNADULT:      ',I2                                   &
  ,/,1X, 'FSNPA:         ',I2                                   &
  ,/,1X, 'PURE_PA (FILE):',I2                                   &
 ,//,1X, 'IP  FSUN  AGE  PROXY_AGE  REL  TANF  SSI   GA  FSTANF  FSSSI  FSGA  PA_PER  FSALLPA'  &
  ,/,1X, '--  ----  ---  ---------  ---  ----  ---  ---  ------  -----  ----  ------  -------'  &
  )
 3310 FORMAT(1X, I2, I6, I5, I11, I5, I6, I5, I5, I8, I7, I6, I8, I9)
 

       !--- dependent care expenses, and number < & > age 2 for deduction cap
 
       if ( orig_fsdepded > 0 ) then   
         call isnewpg(prfile, ctprhh + 8)
         write(prfile, 3400) orig_fsdepded  
         do ip = 1, ctprhh
            write(prfile, 3410) &
               ip               &
              ,fsun       (ip)  &
              ,l_age   %iper(ip)  &
              ,l_fsafil%iper(ip)  &
              ,l_ssi   %iper(ip)  &
              ,fndeplt2   (ip)  &
              ,fndepge2   (ip)
         end do
       endif
 
       !--- medical expenses
 
       if ( orig_fsmedexp > 0 ) then
         call isnewpg(prfile, ctprhh + 8)
         write(prfile, 3500) orig_fsmedexp
         do ip = 1, ctprhh
            write(prfile, 3510) &
                ip              &
               ,fsun       (ip) &
               ,l_age   %iper(ip) &
               ,l_fsafil%iper(ip) &
               ,l_ssi   %iper(ip) &
               ,fsnelder(ip)    &
               ,fsndis  (ip)    &
               ,fsmedexp(ip) 
         enddo
       endif
 
       !--- shelter expenses
 
       if (orig_fssltexp > 0) then
         
         call isnewpg(prfile, ctprhh + 8)
         write(prfile, 3600) orig_fssltexp
         do ip = 1, ctprhh
            write(prfile, 3610) &
                ip              &
               ,fsun    (ip)    &
               ,l_fsafil%iper(ip) &
               ,fsusize (ip)    &
               ,fssltexp(ip)
         end do
       endif
 
    endif !-- if prlevel >= loop_prlevel


 
    !--------------------------------------------------------------------
    !---- Print summary of created variables if PRLEVEL set at loop level
    !--------------------------------------------------------------------
 
    if (prlevel(nth) >= no_loop_prlevel) then
 
        call isnewpg(prfile, num_in_fsu + 6)
        write(prfile, 4000)
        do iunit = 1, ctprhh
           do ip = 1, ctprhh
              if (fsun(ip) /= iunit) cycle ! not in this unit
              write(prfile, 4010) &
              ip                  &
             ,fsun    (ip)        &
             ,fsusize (ip)        &
             ,fsnelder(ip)        &
             ,fsndis  (ip)        &
             ,fsnkid  (ip)        &
             ,fsnk5t17(ip)        &
             ,fndeplt2(ip)        &
             ,fndepge2(ip)        &
             ,fsngmom (ip)        &
             ,fstanf  (ip)        &
             ,fsssi   (ip)        &
             ,fsga    (ip)        &
             ,fsallpa (ip)
           end do      
        end do      
        


        call isnewpg (prfile, num_in_fsu + 4)
        write(prfile, 4100)
        do iunit = 1, ctprhh
           do ip = 1, ctprhh
              if (fsun(ip) /= iunit) cycle ! not in this unit
              write(prfile, 4110)&
              ip                 &
             ,fsun    (ip)       &
             ,fsearn  (ip)       &
             ,fsgrinc (ip)       &
             ,fsasset (ip)       &
             ,fsmedexp(ip)       &
             ,fsdepded(ip)       &
             ,fssltexp(ip)
         end do
        end do


    endif !---- if prlevel >= no_loop_prlevel


    if (l_mn_fip%ihhld == 1) then
       do iunit = 1, ctprhh

          call isnewpg(prfile, 18)
          WRITE(prfile, 5000) &
             iunit            &
            ,mn_unit_label(unit_type)   &
            ,fsusize(iunit)   &
            ,fsearn(iunit)    &
            ,fsunearn         &
            ,MAX_FOOD         &
            ,max_cash         &
            ,TRANS_STD        &
            ,FWL              &
            ,mnernded(nth)    &
            ,fp_earnded       &
            ,fsernded(iunit)  &
            ,net_earn         &
            ,earn_diff   , mn_earn_diff_label(unit_type)      &
            ,inter_inc   , mn_inter_inc_label(unit_type)      &
            ,unearn_diff , mn_unearn_diff_label(unit_type)    &
            ,fsben(iunit), mn_ben_label(unit_type)
       end do
    end if

5000  FORMAT(/,T2, "MINNESOTA BENEFIT CALCULATION FOR UNIT: ", I3  &
          ,//, T5, 'UNIT_TYPE:   ', A  &
           ,/, T5, 'UNIT SIZE:   ', I10  &
           ,/, T5, 'FSEARN:      ', I10  &
           ,/, T5, 'FSUNEARN:    ', I10  &
           ,/, T5, 'FOOD PORTION:', I10  &
           ,/, T5, 'CASH PORTION:', I10  &
           ,/, T5, 'TRANS STD:   ', I10  &
           ,/, T5, 'FAM WAGE LVL:', I10  &
           ,/, T5, 'MNERNDED:    ', F10.4  &
           ,/, T5, 'FSERNDED(fp):', F10.4  &
           ,/, T5, 'FSERNDED:    ', I10  &
           ,/, T5, 'NET EARNINGS:', I10  &
           ,/, T5, 'EARN DIFF:   ', I10, 5X, A  &
           ,/, T5, 'INTER INC:   ', I10, 5X, A  &
           ,/, T5, 'UNEARN DIFF: ', I10, 5X, A  &
           ,/, T5, 'FSBEN:       ', I10, 5X, A  &
           /)



    return

!--------------------------------------------------------------------
! FORMAT STATEMENTS
!--------------------------------------------------------------------
  900 FORMAT(                                                       &
        /1X, 130('*')                                               &
       //1X, 'NTH:', I2, '  PLAN NUMBER: ', A , '  PLAN NAME: ', A  &
       //1X, 'Household ID: ', I10 , 5X, 'Record number: ', I6      &
        ,5X, 'State =', I3, 3X, A, 5X, 'Weight =', F11.4            &
     )
 1000 FORMAT(                                                            &
        1X, 130('-')                                                     &
      //1X ,'The following are household definer variables.  '           &
            ,'They do not vary by unit or reform plan number.'           &
      //1X, 'The user parameters BENMAX, GRSSCRN, NETSCRN, STANDDED,  '  &
          , 'etc. may vary by state.  The following indices indicate'    &
      / 1X, 'which set of values will be used for this household:'       &
     // 1X, 'GEOG_BEN  INDEX =', I2                                      &
       ,3X, 'GEOG_SCRN INDEX =', I2                                      &
       ,3X, 'GEOG_DED  INDEX =', I2                                      &
       ,3X, 'GEOG_POV  INDEX =', I2                                      &
     )
 1010 FORMAT(                                                           &
        1X,  130('-')                                                   &
      //1X, 'The following are household definer variables; they are '  &
          , 'used to create assets, expenses, and public assistance '   &
       /1X, 'status for each food stamp unit constructed for this '     &
          , 'simulation.'                                               &
      //1X, 'Variables based on the original food stamp unit on the '   &
          , 'QC database.  The baselaw unit may be different than the ' &
       /1X, 'original unit.  These variables are used to apportion '    &
          , 'the expenses of the original unit to the simulated units.' &
       /5X, 'ORIG_FSASSET  =', I5, '    ORIG_FSUSIZE    =', I3          &
       /5X, 'ORIG_FSMEDEXP =', I5, '    ORIG_KIDS_LT15  =', I3          &
       /5X, 'ORIG_FSDEPDED =', I5, '    ORIG_FSNKID     =', I3          &  
       /5X, 'ORIG_FSSLTEXP =', I5, '    ORIG_FSNELDER   =', I3          &
       /25X                      , '    ORIG_FSNDIS     =', I3          &
       )
 
 2000 FORMAT(                                                          &
        1X,  130('-')                                                  &
     // 1X, 'This section shows who belongs to which food stamp unit.' &
       ,2X, 'The food stamp unit is denoted by FSUN.'                  &
     //' IP  FSUN  FSAFIL  AGE  SEX  REL'                              &
      /' --  ----  ------  ---  ---  ---'                              &
      )
 2110 FORMAT(1X, I2, I6, I8, I5, I5, I5)
 
 

 3400 FORMAT(                                                          &
      /1X, 'Dependent-care expenses and number of dependents:'         &
      /1X, 'Dependent-care expenses of the original FSU are '          &
         , 'apportioned to FSUs in this simulation.  See documentation'&
      /1X, 'for description of the apportionment algorithm used.'      &
      /1X, 'ORIG_FSDEPDED =', I5                                       & 
     //1X, 'IP  FSUN  AGE  FSAFIL  SSI  FSDEPDED  FSNDEPLT2  FSDEPGE2' & 
      /1X, '--  ----  ---  ------  ---  --------  ---------  --------' &
      )
 3410 FORMAT(1X, I2, I6, I5, I8,    I5,    I10,      I10,      I10)
 
 3500 FORMAT(                                                                  &
      /1X, 'Medical expenses:'                                                 &
      /1X, 'Medical expenses of the original FSU are apportioned '             &
         , 'to FSUs in this simulation, based on the number of '               &
      /1X, 'elderly persons, or if no elderly, the number of disabled persons.'&
      /1X, 'ORIG_FSMEDEXP =', I5                                               &
     //1X, 'IP  FSUN  AGE  FSAFIL  SSI  FSNELDER  FSNDIS  FSMEDEXP'            &
      /1X, '--  ----  ---  ------  ---  --------  ------  --------'            &
      )
 3510 FORMAT(1X, I2, I6, I5, I8,    I5,    I10,      I7,      I10)
 
 3600 FORMAT(                                                     &
      /1X, 'Shelter expenses:'                                    &
      /1X, 'Shelter expenses of the original FSU are apportioned '&
         , 'to FSUs in this simulation, based on the number of '  &
      /1X, 'persons in each unit.'                                &
      /1X, 'ORIG_FSSLTEXP =', I5                                  &
     //1X, 'IP  FSUN  FSAFIL  FSUSIZE  FSSLTEXP'                  &
      /1X, '--  ----  ------  -------  --------'                  &
      )
 3610 FORMAT(1X, I2, I6, I8,     I9,      I10   )
 
 4000 FORMAT(                                                      &
    1X,  130('-')                                                  &
  //1X, 'This section shows a summary of the composition, income, '&
      , 'and expenses of each food stamp unit.'                    &
  //1X, 'IP  FSUN  FSUSIZE  FSNELDER  FSNDIS  FSNKID  FSNK5T17  FNDEPLT2  FNDEPGE2  FSNGMOM  FSTANF  FSSSI  FSGA  FSALLPA' &
   /1X, '--  ----  -------  --------  ------  ------  --------  --------  --------  -------  ------  -----  ----  -------' ) 

 4010 FORMAT(1X, I2, I6, I9,    I10,      I8,     I8,      I10,      I10,      I10,      I9,     I8,    I7,   I6,      I9)


 4100 FORMAT(                                                        &
     //1X, 'IP  FSUN  FSEARN  FSGRINC  FSASSET  FSMEDEXP  FSDEPDED  ', 'FSSLTEXP'    &
      /1X, '--  ----  ------  -------  -------  --------  --------  ', '--------'    &
      )
 4110 FORMAT(1X, I2, I6, I8,   I9,      I9,      I10,      I10,          I10)
 
    end
