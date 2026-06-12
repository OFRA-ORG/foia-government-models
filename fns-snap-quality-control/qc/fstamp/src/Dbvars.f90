!**************************************************************************************************
! Source File:  DBVARS.F90                  
! Called By:    FSTAMP2                    
!
! Creates variables that describe various aspects of the
! food stamp unit, such as gross income, earnings, etc.
! Also imputes expenses when the simulate FSP unit is not
! original FSP unit recorded in the QC review.
!
! Modifications:  
!**********************************************************************************************
    subroutine db_fs_vars

    use global
    use fssizes
    use fsparm
    use fslocs
    use fswork
    use fs_dblocs
    use fs_dbdefine
    use fs_dbparm
    use fs_dbwork, ONLY : depexp

    implicit none

    integer :: ip, iunit, i, femadults, j

    integer :: nssi



    !--------------------------------------------------------------------
    ! BEGIN PROCESSING
    !----------------------------------------------------------------------
    select case(keof)   !-- dummy calls made for keof 1 & 3.
      case(1,3)         !-- used sometimes for coding reforms.
        return          !
    end select          !


    !! Accum some baselaw unit variables not saved on file:
    base_fshrace = 0
    base_fshethnic = 0
    base_fshorigin = 0
    base_fsnfemale = 0
    base_fsnmale = 0
    base_fsnadult = 0

    do iunit = 1, ctprhh
       if (l_fsun(1, 1)%iper(iunit) /= iunit) cycle

       select case (l_raceth%iper(iunit))
           !!  old codes
           case (30:34)
                base_fshrace(iunit) = l_raceth%iper(iunit) - 29
           case (:-1, 99)
                base_fshrace(iunit) = 6

           !!  new codes
           case (1,2,12)
                base_fshrace(iunit) = 6
           case (3,8,11)
                base_fshrace(iunit) = 5
           case (4,6,9)
                base_fshrace(iunit) = 4
           case (5,10)
                base_fshrace(iunit) = 2
           case (7)
                base_fshrace(iunit) = 1
           case (13:22)
                base_fshrace(iunit) = 3
           case default
                base_fshrace(iunit) = 1
       end select

       if (base_fshrace(iunit) == 3) base_fshethnic(iunit) = 1


       do ip = 1, ctprhh
          if (l_fsun(1, 1)%iper(ip) /= iunit) cycle
          if (L_sex%iper(ip) == 2) then
             base_fsnfemale(iunit) = base_fsnfemale(iunit) + 1
          else
             base_fsnmale(iunit) = base_fsnmale(iunit) + 1
          end if
          if (L_age%iper(ip) > max_kid_age .or. L_age%iper(ip) < 0) base_fsnadult(iunit) = base_fsnadult(iunit) + 1
       end do

    end do



    do ip = 1, ctprhh

       !--- Initialize values for each unit (IP = IUNIT in this case)
       FSUSIZE (IP) = 0
       FSTANF  (IP) = 0
       FSSSI   (IP) = 0
       FSGA    (IP) = 0
       FSALLPA (IP) = 0
       FSEARN  (IP) = 0
       FSNELDER(IP) = 0
       FSNDIS  (IP) = 0
       FSNADULT(IP) = 0
       FSNKID  (IP) = 0
       FSNK5T17(IP) = 0
       FSNONCIT(IP) = 0
       FSNABAWD(IP) = 0
       FSNGMOM (IP) = 0
       FSASSET (IP) = 0
       FSFINAST(IP) = 0
       FSVEHAST(IP) = 0
       FSGRINC (IP) = 0
       FNDEPLT2(IP) = 0
       FNDEPGE2(IP) = 0
       FSMEDEXP(IP) = 0
       FSSLTEXP(IP) = 0
       FSCSPDED(IP) = 0
       FSHOMEDED(IP) = 0

       FSNMALE(IP)   = 0
       FSNFEMALE(IP) = 0
       FSHRACE(IP)   = 0
       FSHETHNIC(IP) = 0
       FSHORIGIN(IP) = 0

       CATEG_ELIG(IP) = .false.

       asset_idx(ip) = 0

       depexp(ip) = 0

    end do


    !---------------------------------------------------------------------
    !---- For each unit, accumulate information about the food stamp unit.
    !---------------------------------------------------------------------
    do iunit = 1, ctprhh

       if (fsun(iunit) /= iunit) cycle  ! unit has no one in it

       !---------------------------------------------------------------------
       !----   Accumulate income and characteristics of people in the FSU
       !---------------------------------------------------------------------

       femadults = 0
       nssi = 0

       !------------------------------------------------------------------------------------------
       ! Income variables should be summed over persons in household, not unit
       !------------------------------------------------------------------------------------------
       do ip = 1, ctprhh

          if (l_dpcost%iper(ip) > 0) depexp(iunit) = depexp(iunit) + l_dpcost%iper(ip)

          !----------------------------------------------------------------
          !-- (1) Income aggregation --------------------------------------
          !----------------------------------------------------------------

          !--------  WELFARE Support  (Note: missing income values are coded as < 0)
          if (l_tanf%iper(ip) > 0) fstanf(iunit)  = fstanf(iunit) + l_tanf%iper(ip)
          if (l_ssi %iper(ip) > 0) then
               fsssi (iunit)  = fsssi (iunit) + l_ssi %iper(ip)
               nssi = nssi + 1
          endif
          if (l_ga  %iper(ip) > 0) fsga  (iunit)  = fsga  (iunit) + l_ga  %iper(ip)

          !---- Earned income
          if (l_wages %iper(ip) >0) fsearn(iunit) = fsearn(iunit) + l_wages %iper(ip)
          if (l_othern%iper(ip) >0) fsearn(iunit) = fsearn(iunit) + l_othern%iper(ip)
          if (l_slfemp%iper(ip) >0) fsearn(iunit) = fsearn(iunit) + l_slfemp%iper(ip)

          !---- Other unearned income
          if (l_othgov%iper(ip) > 0)  fsgrinc(iunit) = fsgrinc(iunit) + l_othgov%iper(ip)
          if (l_socsec%iper(ip) > 0)  fsgrinc(iunit) = fsgrinc(iunit) + l_socsec%iper(ip)
          if (l_unemp %iper(ip) > 0)  fsgrinc(iunit) = fsgrinc(iunit) + l_unemp %iper(ip)
          if (l_vet   %iper(ip) > 0)  fsgrinc(iunit) = fsgrinc(iunit) + l_vet   %iper(ip)
          if (l_wcomp %iper(ip) > 0)  fsgrinc(iunit) = fsgrinc(iunit) + l_wcomp %iper(ip)
          if (l_edloan%iper(ip) > 0)  fsgrinc(iunit) = fsgrinc(iunit) + l_edloan%iper(ip)
          if (l_csuprt%iper(ip) > 0)  fsgrinc(iunit) = fsgrinc(iunit) + l_csuprt%iper(ip)
          if (l_deem  %iper(ip) > 0)  fsgrinc(iunit) = fsgrinc(iunit) + l_deem  %iper(ip)
          if (l_cont  %iper(ip) > 0)  fsgrinc(iunit) = fsgrinc(iunit) + l_cont  %iper(ip)
          if (l_othun %iper(ip) > 0)  fsgrinc(iunit) = fsgrinc(iunit) + l_othun %iper(ip)
          if (l_diver %iper(ip) > 0)  fsgrinc(iunit) = fsgrinc(iunit) + l_diver %iper(ip)
          if (l_wgesup%iper(ip) > 0)  fsgrinc(iunit) = fsgrinc(iunit) + l_wgesup%iper(ip)
          if (l_energy%iper(ip) > 0)  fsgrinc(iunit) = fsgrinc(iunit) + l_energy%iper(ip)
          if (l_eitc  %iper(ip) > 0)  fsgrinc(iunit) = fsgrinc(iunit) + l_eitc  %iper(ip)
          if (l_foster%iper(ip) > 0)  fsgrinc(iunit) = fsgrinc(iunit) + l_foster%iper(ip)


          !------------------------------------------------------------
          !-- (2) Counts of various types of unit members
          !------------------------------------------------------------

          !--- Cycle if person not in the fsu
          !--- Moved cycle here for FSU counts 
          if (fsun(ip) /= iunit) cycle

          fsusize(iunit) = fsusize(iunit) + 1

          if (l_age%iper(ip) > max_kid_age .or. l_age%iper(ip) < 0) then

             fsnadult(iunit)  = fsnadult(iunit) + 1
             if (l_sex%iper(ip) == 2) femadults = femadults + 1

          else

             fsnkid(iunit)  = fsnkid(iunit) + 1
             if (l_age%iper(ip) >= min_school_age)  fsnk5t17(iunit) = fsnk5t17(iunit) + 1

             if (l_age%iper(ip) < max_toddler_age) then
                fndeplt2(iunit) = fndeplt2(iunit) + 1
             else
                fndepge2(iunit) = fndepge2(iunit) + 1
             end if

          end if

          if (l_age%iper(ip) >= min_elderly_age) fsnelder(iunit) = fsnelder(iunit) + 1


          if (l_ctzn%iper(ip) > 2) fsnoncit(iunit) = fsnoncit(iunit) + 1

          !  ADD ABAWD COUNTER:
          if (l_NDISCA%iper(ip) == 1 .AND. l_fsafil%iper(ip) == 1) fsnabawd(iunit) = fsnabawd(iunit) + 1


          if (l_dis%iper(ip) == 1) fsndis(iunit) = fsndis(iunit) + 1

          if (l_sex%iper(ip) == 2) then
             fsnfemale(iunit) = fsnfemale(iunit) + 1
          else
             fsnmale(iunit) = fsnmale(iunit) + 1
          end if


       end do


       select case (l_raceth%iper(iunit))
           !!  old codes
           case (30:34)
                fshrace(iunit) = l_raceth%iper(iunit) - 29
           case (:-1, 99)
                fshrace(iunit) = 6

           !!  new codes
           case (1,2,12)
                fshrace(iunit) = 6
           case (3,8,11)
                fshrace(iunit) = 5
           case (4,6,9)
                fshrace(iunit) = 4
           case (5,10)
                fshrace(iunit) = 2
           case (7)
                fshrace(iunit) = 1
           case (13:22)
                fshrace(iunit) = 3
           case default
                fshrace(iunit) = 1
       end select

       if (fshrace(iunit) == 3) fshethnic(iunit) = 1


       !------------------------------------------------------------------------------
       !--- Identify FSUs headed by a single female. This is not used for any
       !--- eligibility determination.  It is used for summary counts only (G/L table).
       !--- Note that persons with unknown age are NOT considered adults here.
       !------------------------------------------------------------------------------
       if (fsnadult(iunit) == 1 .and. femadults == 1 .and. fsnkid(iunit) > 0) fsngmom(iunit) = 1

       !---------------------------------------------------------------------
       !----  Add earnings and welfare income to FSGRINC
       !---------------------------------------------------------------------
       fsgrinc(iunit) = fsgrinc(iunit) + fsearn(iunit)  + fsssi(iunit)  &
                                       + fstanf(iunit)  + fsga(iunit)
       !---------------------------------------------------------------------
       !----  Subtract EXFSCSDED from FSGRINC 
       !---------------------------------------------------------------------
       fsgrinc(iunit) = fsgrinc(iunit) - l_exfscsded%ihhld


       !-------------------------------------------------------
       !  Unit is categorially eligible (BBCE) if cat_elig > 0
       !-------------------------------------------------------
       if (l_cat_elig%ihhld > 0) categ_elig(iunit) = .true.



       if (fsndis(iunit) > 0 .or. fsnelder(iunit) > 0) then
          asset_idx(iunit) = 1
       else
          asset_idx(iunit) = 2
       end if


       !------------------------------------------------------------------------------------------
       !--- Assign dependent care deduction
       !------------------------------------------------------------------------------------------
       FSDEPDED(IUNIT) = MAX(l_ORIGINAL_FSDEPDED%IHHLD, 0)


       !------------------------------------------------------------------------------------------
       !--- Impute shelter expenses, based on FSUSIZE, if not using original unit composition.
       !------------------------------------------------------------------------------------------
       fssltexp(iunit) = nint( orig_fssltexp * float(fsusize(iunit)) / orig_fsusize )

       !-----------------------------------------------------------------------
       !--- Impute medical expenses if not using baselaw or unit composition.
       !-----------------------------------------------------------------------
       if (orig_fsmedexp > 0) then
          if (orig_fsnelder > 0 .or. orig_fsndis > 0) then
             fsmedexp(iunit) = nint (real (orig_fsmedexp * (fsnelder(iunit) + fsndis(iunit)) ) / (orig_fsnelder + orig_fsndis))
          else if (orig_fsnelder == 0 .and. orig_fsndis == 0) then
              if (nssi > 0) then
                 ! The unit is allowed a medical deduction based on an elderly or
                 ! disabled person outside the unit (if there are none in the unit).
                 ! The medical deduction goes to whomever in the unit has SSI
                 ! income.
                 do ip = 1, ctprhh
                      !--- Cycle if person not in the fsu
                      if (fsun(ip) /= iunit) cycle
                      fsmedexp(ip) = nint(real(orig_fsmedexp) / nssi)
                 enddo
              else
                  ! The unit is allowed a medical deduction based on an elderly or
                  ! disabled person outside the unit, but nobody has SSI income,
                  ! so assign the medical deduction to the unit head.
                  fsmedexp(iunit) = orig_fsmedexp
              endif
          endif
       else
          fsmedexp(iunit) = 0
       endif

       !-------------------------------------------------------------------------
       !--- medical DEDUCTION DEMO
       !-------------------------------------------------------------------------

       j = 0
       do i = 1, num_med_demo        
          if (fstate == med_demo_state(i) .and. l_yrmonth%ihhld >= med_demo_date(i) ) j = i
       end do
       
       if (j > 0) then
          if (fsmedexp(iunit) > 0 .and. fsmedexp(iunit) <= (med_demo_thres(j)-35)) fsmedexp(iunit) = med_demo_min(j)

          fsstdded(iunit) = fsstdded(iunit) - med_demo_stddedred(j)
       end if
       
       !-----------------------------------------------------------------------------------
       !--- Impute child support payment expenses.  Note the deduction is equal to the
       !--- expenses, so we assign the deduction variable here, rather than in FSELIG.
       !-----------------------------------------------------------------------------------
       if (orig_fscsded > 0 .and. fsun(orig_fsuhead) == iunit) fscspded(iunit) = orig_fscsded

       !-------------------------------------------------------------------------------
       !--- Impute countable assets for this unit.
       !-------------------------------------------------------------------------------
       fsasset (iunit) = orig_fsasset

       !-------------------------------------------------------------------------------
       !--- Assign homeless deduction
       !-------------------------------------------------------------------------------
       if (l_homeded%ihhld == 3) fshomeded(iunit) = l_homelsded%ihhld


       CALL db_fs_CALC_PURE_PA(IUNIT)

    end do ! end of loop over all fs units in the household


    !----------------------------------------------
    !--- assign the correct shelter deduction cap
    !----------------------------------------------
    do i = 1, num_shelcap_region
       shelcap(i,nth) = shelcap1(i,nth)
       shelcap_region(i) = shelcap1_region(i)
    end do


    return
    end



    SUBROUTINE db_fs_CALC_BENEFIT(IUNIT)
!---------------------------------------------------------
!   Calculate SNAP benefits for MN_FIP and SSI_CAP
!   Otherwise return -1
!---------------------------------------------------------
    USE GLOBAL
    USE FSWORK
    USE FSLOCS
    USE FS_DBLOCS
    USE FS_DBWORK
    USE FS_DBPARM
    use fsutils

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: IUNIT

    INTEGER :: USIZE, PRE_NETINC 


    !-------------------------------------------------------------
    !  SET DEFAULT BENEFIT:
    !  if fsben = -1 is returned, the federal benefit is applied
    !-------------------------------------------------------------
    FSBEN(IUNIT) = -1

    !! STATE SPECIFIC CODE GOES HERE:

    !! START MN 
    IF (l_MN_FIP%IHHLD == 1) THEN

       !! RESET ALL DEDUCTIONS TO MISSING:
       FSSTDDED(IUNIT) = -6
       FSMEDDED(IUNIT) = -6
       FSDEPDED(IUNIT) =  0
       FSSLTDED(IUNIT) = -6
       FSCSPDED(IUNIT) = -6
       FSHOMEDED(IUNIT)= -6
       FSNETINC (IUNIT)= -6


       !! RECALC EARNINGS DEDUCTION:
       fp_earnded = MNERNDED(NTH) * FSEARN(IUNIT)
       FSERNDED(IUNIT) = Floor(MNERNDED(NTH) * FSEARN(IUNIT))
       FSERNDED(IUNIT) = MIN(FSERNDED(IUNIT), EARNMAX(NTH))

       !! RECALC THESE VARIABLES FOR MN:
       FSTOTDED(IUNIT) = FSERNDED(IUNIT)


       FSUNEARN = max (0, FSGRINC(IUNIT) - FSEARN(IUNIT) - fstanf(iunit) )

       IF (FSEARN(IUNIT) > 0 .AND. FSUNEARN == 0) THEN
          unit_type = 1
       ELSEIF (FSEARN(IUNIT) == 0) THEN
          Unit_type = 2
       ELSE
          Unit_type = 3
       end if

       USIZE = FSUSIZE(IUNIT)
       IF (USIZE <= 10) THEN
          MAX_FOOD = NINT(MN_BEN(USIZE, 1, NTH))
          MAX_CASH = NINT(MN_BEN(USIZE, 2, NTH))
       ELSE
          MAX_FOOD = NINT(MN_BEN(10, 1, NTH) + (USIZE-10) * MN_BEN(11, 1, NTH) )
          MAX_CASH = NINT(MN_BEN(10, 2, NTH) + (USIZE-10) * MN_BEN(11, 2, NTH) )
       END IF

       TRANS_STD =  MAX_FOOD + MAX_CASH
       FWL = NINT(1.1 * TRANS_STD)

       NET_EARN = FSEARN(IUNIT) - FSERNDED(IUNIT)
       EARN_DIFF =  FWL - NET_EARN

       INTER_INC = 0
       UNEARN_DIFF = 0

       SELECT CASE (UNIT_TYPE)
           CASE (1)
              FSBEN(IUNIT) = MAX(0, MIN(MAX_FOOD, EARN_DIFF) )

           CASE (2)
              UNEARN_DIFF = TRANS_STD - FSUNEARN
              FSBEN(IUNIT) = MAX(0, MIN(MAX_FOOD, UNEARN_DIFF) )

           CASE (3)
              INTER_INC = MIN(TRANS_STD, EARN_DIFF)
              UNEARN_DIFF = INTER_INC - FSUNEARN
              FSBEN(IUNIT) = MAX(0, MIN(MAX_FOOD, UNEARN_DIFF) )

       END SELECT

       !-----------------------------------------------------------------
       ! ensure MN_FIP benefit is >= minimum benefit:
       !-----------------------------------------------------------------
       IF (FSUSIZE(IUNIT) <= MAX_BENMIN_FSUSIZE) THEN
          MIN_BENEFIT(IUNIT) = NINT(BENMIN (FSUSIZE(IUNIT), geog_ben,  NTH))
       ELSE
          MIN_BENEFIT(IUNIT) = NINT(BENMIN (MAX_BENMIN_FSUSIZE, geog_ben,  NTH))
       END IF

       FSBEN(IUNIT) = MAX(FSBEN(IUNIT), MIN_BENEFIT(IUNIT))


    END IF
    !! END MN


    !!  SSI_CAP:
    SELECT CASE (fstate)

        !!    az  ky  la  md  mi  ms  nj  nm  ny  nc  pa  sc  sd  tx  va
        case ( 4, 21, 22, 24, 26, 28, 34, 35, 36, 37, 42, 45, 46, 48, 51)

           if (l_ssi_cap%ihhld == 2 .or. l_ssi_cap%ihhld == 3) then
              fsben(iunit) = l_fsben(1, 1)%iper(iunit)

               IF (FSTATE == 21 .AND. FSUSIZE(IUNIT) == 2) THEN
                  fsben(iunit) = l_fsben(1, 1)%iper(iunit)
               END IF

               FSERNDED(IUNIT) = -6
               FSSTDDED(IUNIT) = -6
               FSMEDDED(IUNIT) = -6
               FSDEPDED(IUNIT) =  0
               FSSLTDED(IUNIT) = -6
               FSCSPDED(IUNIT) = -6
               FSHOMEDED(IUNIT)= -6
               FSTOTDED(IUNIT) = -6
               FSNETINC(IUNIT) = -6
           end if




        !!    FL  MA  WA
        CASE (12, 25, 53)
           if (l_ssi_cap%ihhld == 1) then

               !! RESET some DEDUCTIONS TO ZERO:
               FSERNDED(IUNIT) = -6
               FSMEDDED(IUNIT) = -6
               FSDEPDED(IUNIT) = 0
               FSSLTDED(IUNIT) = 0
               FSHOMEDED(IUNIT) = -6

               PRE_NETINC = FSGRINC(IUNIT) - FSSTDDED(IUNIT) - FSCSPDED(IUNIT)
               PRE_NETINC = MAX(0, PRE_NETINC)

               !--- calc shelter ded
               fssltded(iunit) = snap_shelter_deduction(FSSLTEXP(IUNIT), pre_netinc, fsnelder(iunit), fsndis(iunit), geog_ded)

               !---- TOTAL DEDUCTIONS
               FSTOTDED(IUNIT) = FSSTDDED(IUNIT)  &
                          + FSSLTDED(IUNIT) + FSCSPDED(IUNIT)

               !---- NET INCOME
               FSNETINC(IUNIT) = FSGRINC(IUNIT) - FSTOTDED(IUNIT)
               FSNETINC(IUNIT) = MAX(FSNETINC(IUNIT), 0)

               !--- benefit
               FSBEN(IUNIT) = snap_benefit(MAX_BENEFIT(IUNIT), MIN_BENEFIT(IUNIT), FSNETINC(IUNIT))


           end if
        CASE DEFAULT
           !
    END SELECT
    !!  End SSI_CAP:


    !!  RESET TEST RESULTS TO FILE VALUES FOR CAT_ELIG UNITS
    IF (CATEG_ELIG(IUNIT)) THEN
       FSASTEST(IUNIT) = L_FSASTEST(1, 1)%IPER(IUNIT)
       FSGRTEST(IUNIT) = L_FSGRTEST(1, 1)%IPER(IUNIT)
       FSNETEST(IUNIT) = L_FSNETEST(1, 1)%IPER(IUNIT)
    END IF


    END SUBROUTINE



    SUBROUTINE db_fs_calc_pure_pa(IUNIT)
!---------------------------------------------------------------------------------------------
!  DETERMINE PURE PA STATUS FOR ALL HOUSEHOLDS
!  IF HOUSEHOLD CONTAINS NO ADULTS AND RECEIVES TANF, THEN ALL MEMBERS RECEIVE TANF
!---------------------------------------------------------------------------------------------
    use global
    use fswork
    use fs_dbWORK
    use fs_dblocs
    implicit none

    INTEGER, INTENT(IN) :: iunit
    integer :: ip


    !! IF AGE IS MISSING ASSIGN A PROXY AGE
    DO Ip = 1, CTPRHH
       PROXY_AGE(Ip) = l_age%iper(ip)
       IF (l_AGE%iper(Ip) < 0) THEN
          select case (l_rel%iper(ip))
              case (1,2,3,7)         !! CONSIDER THESE ADULTS, MAKE AGE HIGH
                 PROXY_AGE(Ip) = 97
              case (4,5,6)           !! CONSIDER THESE CHILDREN UNDER 14, MAKE 13
                 PROXY_AGE(Ip) = 13
              case DEFAULT           !! CONSIDER THESE ADULTS, MAKE AGE HIGH
                 PROXY_AGE(Ip) = 98
          end select
       end if
    end do


    adult = 0
    ANY_ADULT = .false.
    PURE_PA_flag = 0

    DO Ip = 1,CTPRHH
       IF (PROXY_AGE(Ip) >= 18) ANY_ADULT = .true.
       if (fsun(ip) ==0) cycle
       IF (l_AGE%iper(Ip) >= 18) ADULT = ADULT + 1  !! adults in unit (not counting missing)
    END do

    IF (.not. ANY_ADULT .AND. FSTANF(iunit) > 0) THEN
       PURE_PA_flag = 1
    END if


    !! ASSIGN INDIVIDUAL PA FLAGS
    PA_PER = 0
    TPARENT = 0
    TKID = 0
    DO IP = 1, CTPRHH
         !! SET PA FLAG TO 1 WHEN RECEIVE TANF, GA, OR SSI
         IF (l_TANF%IPER(IP) > 0 .OR. l_GA%IPER(IP) > 0 .OR. l_SSI%IPER(IP) > 0) PA_PER(Ip) = 1

         IF (l_TANF%IPER(IP) > 0) THEN
           !! CHECK TO SEE IF HOUSEHOLD HEAD RECEIVES TANF
           IF (l_REL%iper(IP) == 1 .OR. l_REL%iper(IP) == 2) TPARENT = 1
           !! CHECK TO SEE IF SON/DAUGHTER RECEIVES TANF
           IF (l_REL%iper(IP) == 4) TKID = 1
         END IF
    END DO


    FSNPA = 0
    PAADLT = 0

    DO IP = 1, CTPRHH
        !! IF PERSON RESIDES IN HOUSEHOLD WHERE HEAD RECEIVES TANF, THEN SET
        !! PA_PER FLAG TO 1 FOR HEAD, SPOUSE OR SON/DAUGHTER.
	!! CHILDREN MUST BE UNDER 18 OR 18 IF NO OTHER KIDS
       IF (TPARENT == 1) THEN
          IF (l_REL%iper(IP) == 1 .OR. l_REL%iper(IP) == 2) PA_PER(Ip) = 1
          IF (l_REL%iper(IP) == 4) THEN
             IF(l_AGE%iper(IP) >= 0 .AND. l_AGE%iper(IP) <= 17) PA_PER(Ip) = 1
             IF (l_AGE%iper(IP) == 18 .AND. FSNKID(IUNIT) == 0) PA_PER(Ip) = 1
          END IF
       END IF

       !! IF OTHER RELATIVE RESIDES IN HOUSEHOLD WITH SON/DAUGHTER RECEIVING
       !! TANF, SET PA_PER FLAG TO 1 FOR OTHER RELATIVE
       IF (TKID == 1 .AND. l_REL%iper(IP) == 5) PA_PER(Ip) = 1
    END DO


    !! SUM NUMBER OF PEOPLE IN UNIT RECEIVING PA
    DO IP = 1, CTPRHH
       IF (PA_PER(Ip) == 1 .AND. FSUN(IP) == IUNIT) THEN
          FSNPA = FSNPA + 1
          IF (PROXY_AGE(Ip) >= 18 .AND. PROXY_AGE(Ip) <= 98) PAADLT = PAADLT + 1
       END IF
    END DO

    !! HOUSEHOLDS WITH TANF MUST HAVE ALL ADULTS RECEIVING PA IN ORDER TO BE PURE PA
    IF (FSTANF(IUNIT) > 0 .AND. PAADLT == ADULT) THEN
       PURE_PA_flag = 1

    !! OTHER HOUSEHOLDS MUST HAVE ALL MEMBERS WITH PA IN ORDER TO BE PURE PA
    ELSEIF (FSNPA == FSUSIZE(IUNIT)) THEN
       PURE_PA_flag = 1

    !!  MN HOUSEHOLDS WITH MN_FIP ARE PURE PA
    ELSEIF (FSTATE == 27 .and. l_mn_fip%ihhld == 1) THEN
       PURE_PA_flag = 1
    END IF

 !  PA_FOUND:
    IF (PURE_PA_flag == 1) fsallpa(iunit) = 1

    return
    end
