!**************************************************************************************************
! Source File:  FSELIG.F90                  
! Called By:    FSTAMP2                     
!
! Determines FSP eligibility and benefit amount for each food stamp unit (keof=2).
!
!**************************************************************************************************
    SUBROUTINE FS_ELIGIBILITY
    USE GLOBAL
    USE GLOBPARM
    USE FSSIZES
    USE FSPARM
    USE FSWORK
    USE FSLOCS
    use fsutils

    IMPLICIT NONE

    INTEGER :: IUNIT, IDED, JDED, FSSLTDED_TEMP, DED(4)  &
              ,PRE_NETINC


    !--------------------------------------------------------------------
    ! BEGIN PROCESSING
    !--------------------------------------------------------------------
    SELECT CASE(KEOF)   !-- Dummy calls made for KEOF 1 & 3.
        CASE(1,3)       !-- Used sometimes for coding reforms.
          RETURN
    END SELECT



    HH_FSBEN = 0

    DO IUNIT = 1, CTPRHH

         !--- skip null units (no one is in the unit)
         IF (FSUN(IUNIT) /= IUNIT) CYCLE

         !---------------------------------------------------------------------------
         !---- POVERTY RATIO notes:
         !---- 1. The poverty ratio is gross income divided by the poverty line.
         !----    The poverty line is a global (supervisor) parameter that
         !----    depends on unit size and geographic area.  This parameter
         !----    represents the HHS-derived monthly poverty guidelines.
         !---- 2. This value is used in the summary tables (not the g/l tables).
         !---------------------------------------------------------------------------
         
         poverty(iunit) = calc_povline(FSUSIZE(IUNIT), GEOG_POV)

         FSPOVRAT(IUNIT) = real(FSGRINC(IUNIT)) / POVERTY(IUNIT)


         !---------------------------------------------------------------------------
         !---- GROSS INCOME SCREEN notes:
         !---- 1. The gross income screen gets adjusted by GRSMULT.
         !---- 2. The gross income screen depends on the size of the unit,
         !----    the geographic area, and on the NTH.
         !---------------------------------------------------------------------------

         IF (GRSM_SEL(NTH) == 1) THEN

         !--- GROSS SCREEN: GROSS_SCREEN = GRSMULT*GRSSCRN

            IF (FSUSIZE(IUNIT) <= MAX_SCREEN_FSUSIZE) THEN
                GROSS_SCREEN(IUNIT) = NINT(GRSMULT(NTH) * GRSSCRN(FSUSIZE(IUNIT),      GEOG_SCRN, NTH)  )
            ELSE
                GROSS_SCREEN(IUNIT) = NINT(GRSMULT(NTH) * (GRSSCRN(MAX_SCREEN_FSUSIZE, GEOG_SCRN, NTH)    &
                  + GRSSCRN(MAX_SCREEN_FSUSIZE + 1, GEOG_SCRN, NTH) * (FSUSIZE(IUNIT) - MAX_SCREEN_FSUSIZE) ))
            END IF

         ELSE

         !--- COMPUTE GROSS SCREEN AS A PERCENTAGE OF POVMONTH (NET SCREEN):
         !--- GROSS_SCREEN = GRSMULT*(ANNUAL POVMONTH  / 12)

            IF (FSUSIZE(IUNIT) <= MAX_SCREEN_FSUSIZE) THEN
                GROSS_SCREEN(IUNIT) =  NINT(DEFL_GEN * CEILING(GRSMULT(NTH) / 12.0 * POVGUIDE(FSUSIZE(IUNIT),       GEOG_SCRN) ))
            ELSE
                GROSS_SCREEN(IUNIT) =  NINT(DEFL_GEN * CEILING(GRSMULT(NTH) / 12.0 * POVGUIDE(MAX_SCREEN_FSUSIZE,   GEOG_SCRN) ))  &
                                    + (NINT(DEFL_GEN * CEILING(GRSMULT(NTH) / 12.0 * POVGUIDE(MAX_SCREEN_FSUSIZE+1, GEOG_SCRN) ))  &
                                    * (FSUSIZE(IUNIT)-MAX_SCREEN_FSUSIZE) )
            END IF


         END IF

         !---------------------------------------------------------------------------
         !---- GROSS INCOME TEST notes:
         !---- 1. Pure PA units are exempt.
         !---- 2. Households with elderly or disabled persons are exempt
         !----    if the AGEDSCRN parameter is 2.
         !---- 3. The gross income screen depends on the size of the unit,
         !----    the geographic area, and on the NTH, (see previous comments).
         !---- 4. FSGRTEST = 1 means the unit passed the gross income test.
         !---------------------------------------------------------------------------

         IF (FSALLPA(IUNIT) == 1  &
        .OR. (AGEDSCRN(NTH) == 2 .AND. (FSNDIS(IUNIT) > 0 .OR. FSNELDER(IUNIT) > 0)) &
        .OR. (FSGRINC(IUNIT) <= GROSS_SCREEN(IUNIT))  ) THEN
             FSGRTEST(IUNIT) = 1
         ELSE
             FSGRTEST(IUNIT) = 0
         END IF


         call db_fs_set_fsgrtest(iunit)

         !---------------------------------------------------------------------------
         !---- DEDUCTIONS notes:
         !---- 1. Five deductions are subtracted from gross income giving net income.
         !----    They are: standard, earnings, medical,dependent care, and excess shelter expense
         !---- 2. These deductions represent the deduction amount to which the
         !----    unit is entitled or the marginal effective deduction amount, depending
         !----    on the DEDTYPE user parameter.
         !---- 3. FSCSPDED (child support payment deduction) is set by database-specific
         !----    code, because there is no generic processing associated with this deduction.

         !---- STANDARD DEDUCTION notes:
         !---- 1. The standard deduction gets adjusted by SDEDMULT, which varies by unit size.
         !---- 2. The standard deduction depends on the geographic area and on NTH.
         !---- 3. The plus here is to deal with reductions to achieve cost neutrality
         !----    as calculated in DB_FS_VARS.
         !---------------------------------------------------------------------------

         IF (FSUSIZE(IUNIT) <= MAX_STANDDED_FSUSIZE) THEN
             FSSTDDED(IUNIT) = FSSTDDED(IUNIT) + NINT(STANDDED(FSUSIZE(IUNIT), GEOG_DED, NTH) )
         ELSE
             FSSTDDED(IUNIT) = FSSTDDED(IUNIT) + NINT(STANDDED(MAX_STANDDED_FSUSIZE, GEOG_DED, NTH) )
         END IF


         !---------------------------------------------------------------------------
         !---- EARNINGS DEDUCTION notes:
         !---- 1. The earnings deduction is some portion of the unit's
         !----    total earnings (excluding student earnings).
         !---- 2. The deduction amount is rounded to the nearest $1
         !---- 3. The deduction amount cannot be larger than EARNMAX(NTH).
         !---------------------------------------------------------------------------
         FSERNDED(IUNIT) = Floor(EARNDED(NTH) * FSEARN(IUNIT))
         FSERNDED(IUNIT) = MIN(FSERNDED(IUNIT), EARNMAX(NTH))

         !---------------------------------------------------------------------------
         !---- MEDICAL DEDUCTION notes:
         !---- 1. The medical deduction is the amount of medical expenses
         !----    in excess of some threhold.  Only the medical expenses
         !----    incurred by elderly or disabled persons are considered.
         !---- 2. The deduction amount must be non-negative.
         !---------------------------------------------------------------------------
         FSMEDDED(IUNIT) = FSMEDEXP(IUNIT) - NINT(MDTHRESH(NTH))
         FSMEDDED(IUNIT) = MAX(FSMEDDED(IUNIT), 0)

         !---------------------------------------------------------------------------
         !---- DEPENDENT CARE DEDUCTION notes:
         !---- 1. The dependent care deduction is the amount of dependent
         !----    FSDEPDED(IUNIT) now calculated in dbvars
         !---------------------------------------------------------------------------


         !---------------------------------------------------------------------------
         !---- EXCESS SHELTER EXPENSE DEDUCTION notes:
         !---- 1. The excess shelter expense deduction is the amount of shelter
         !----    expenses in excess of some percentage of gross income less
         !----    the previous four deductions.  The percentage amount depends
         !----    on NTH.  The deduction amount of non-elderly non-disabled units
         !----    are subject to a maximum, which depends on geographic area.
         !---- 2. The reduced net income amount (after applying the percent
         !----    reduction) is rounded to the nearest $1.
         !---- 3. The deduction amount must be non-negative.
         !---- 4. The deduction amount of non-elderly non-disabled units
         !----    are subject to a maximum, which depends on
         !----    geographic area and on NTH.  This amount is adjusted by SHLCMULT.
         !---------------------------------------------------------------------------

         PRE_NETINC = FSGRINC(IUNIT)                    &
                    - FSSTDDED(IUNIT) - FSERNDED(IUNIT) &
                    - FSMEDDED(IUNIT) - FSDEPDED(IUNIT) - FSCSPDED(IUNIT)

         PRE_NETINC = MAX(0, PRE_NETINC)

         fssltded(iunit) = snap_shelter_deduction(FSSLTEXP(IUNIT), pre_netinc, fsnelder(iunit), fsndis(iunit), geog_ded)


         IF (FSHOMEDED(IUNIT) > 0) THEN
             FSSLTDED(IUNIT) = 0
         END IF


         !---- TOTAL DEDUCTIONS
         FSTOTDED(IUNIT) = FSSTDDED(IUNIT) + FSERNDED(IUNIT)  &
                         + FSMEDDED(IUNIT) + FSDEPDED(IUNIT)  &
                         + FSSLTDED(IUNIT) + FSCSPDED(IUNIT)  &
                         + FSHOMEDED(IUNIT)


         !---- NET INCOME
         FSNETINC(IUNIT) = FSGRINC(IUNIT) - FSTOTDED(IUNIT)
         FSNETINC(IUNIT) = MAX(FSNETINC(IUNIT), 0)

         !-------------------------------------------------------------------
         !---- NET INCOME SCREEN notes:
         !---- 1. The net income screen gets adjusted by NETMULT.
         !---- 2. The net income screen depends on the size of the unit,
         !----    the geographic area, and on the NTH.
         !-------------------------------------------------------------------
         IF (FSUSIZE(IUNIT) <= MAX_SCREEN_FSUSIZE) THEN
             NET_SCREEN(IUNIT) = &
             NINT(NETMULT(NTH) * NETSCRN(FSUSIZE(IUNIT), GEOG_SCRN, NTH))
         ELSE
             NET_SCREEN(IUNIT) =  &
             NINT(NETMULT(NTH) *  &
                  (NETSCRN(MAX_SCREEN_FSUSIZE, GEOG_SCRN, NTH)     &
                 + NETSCRN(MAX_SCREEN_FSUSIZE + 1, GEOG_SCRN, NTH) &
                   * (FSUSIZE(IUNIT) - MAX_SCREEN_FSUSIZE)         &
                 ))
         END IF

         !-------------------------------------------------------------------
         !---- NET INCOME TEST notes:
         !---- 1. Pure PA units are exempt.
         !---- 2. Households with elderly or disabled persons are exempt
         !----    if the AGEDSCRN parameter is 1.
         !---- 3. The net income screen depends on the size of the unit,
         !----    the geographic area, and on the NTH (see previous comments).
         !---- 4. FSNETEST = 1 means the unit passed the net income test.
         !-------------------------------------------------------------------

         IF (FSALLPA(IUNIT) == 1  &
          .OR. (AGEDSCRN(NTH) == 1 .AND. (FSNDIS(IUNIT) > 0 .OR. FSNELDER(IUNIT) > 0)) &
          .OR. (FSNETINC(IUNIT) <= NET_SCREEN(IUNIT))  ) THEN
             FSNETEST(IUNIT) = 1
         ELSE
             FSNETEST(IUNIT) = 0
         END IF


         !----  Compute countable assets for this unit
         CALL DB_FS_ASSET(IUNIT) ! FSASSET is returned via MODULE

         !---------------------------------------------------------------------------
         !---- ASSET TEST notes:
         !---- 1. Pure PA units are exempt from the asset test.
         !---- 2. The asset limit depends on the presence of elderly
         !----    persons in the unit and on the NTH.
         !---- 3. FSASTEST = 1 means the unit passed the asset test.
         !---------------------------------------------------------------------------

         IF (FSALLPA(IUNIT) == 1 .OR. FSASSET(IUNIT) <= ASSET_LIMIT(IUNIT)) THEN
             FSASTEST(IUNIT) = 1
         ELSE
             FSASTEST(IUNIT) = 0
         END IF


         !---------------------------------------------------------------------------
         !--- Calculate BBCE;  code in dbvars.f90
         !--- Units that are categ_elig have all standard test results set to 'pass'
         !---------------------------------------------------------------------------
         CALL db_fs_CALC_CATEG_ELIG(IUNIT)

         IF (CATEG_ELIG(IUNIT)) THEN
            FSASTEST(IUNIT) = 1
            FSGRTEST(IUNIT) = 1
            FSNETEST(IUNIT) = 1
         END IF


         !-------------------------------------------------------------------
         !---- MAXIMUM BENEFIT notes:
         !---- 1. The maximum benefit depends on the size of the unit,
         !----    the geographic area, the NTH, and on the parameter BENMULT.
         !---- 2. If the unit size does not exceed MAX_BENMAX_FSUSIZE, a
         !----    lookup table is used to get the maximum benefit;
         !----    otherwise a lookup table is used and
         !----    an incremental amount is included in the maximum benefit
         !----    for each addt'l person beyond MAX_BENMAX_FSUSIZE.
         !-------------------------------------------------------------------

         IF (FSUSIZE(IUNIT) <= MAX_BENMAX_FSUSIZE) THEN
            MAX_BENEFIT(IUNIT) = &
               NINT(BENMULT(NTH) * BENMAX(FSUSIZE(IUNIT), GEOG_BEN, NTH) )
         ELSE
            MAX_BENEFIT(IUNIT) =                                              &
               NINT(BENMULT(NTH) * (BENMAX(MAX_BENMAX_FSUSIZE, GEOG_BEN, NTH)   &
               + BENMAX(MAX_BENMAX_FSUSIZE + 1, GEOG_BEN, NTH) * (FSUSIZE(IUNIT) - MAX_BENMAX_FSUSIZE) ))
         END IF


         !-------------------------------------------------------------------
         !---- MINIMUM BENEFIT notes:
         !---- 1. The minimum benefit depends on the size of the unit,
         !----    the geographic area, the NTH, and on the parameter BENMULT.
         !---- 2. The minimum benefit size is capped at MAX_BENMIN_FSUSUZE
         !-------------------------------------------------------------------
         IF (FSUSIZE(IUNIT) <= MAX_BENMIN_FSUSIZE) THEN
            MIN_BENEFIT(IUNIT) = NINT(benmult(nth) * BENMIN(FSUSIZE(IUNIT), GEOG_BEN, NTH))
         ELSE
            MIN_BENEFIT(IUNIT) = NINT(benmult(nth) * BENMIN(MAX_BENMIN_FSUSIZE, GEOG_BEN, NTH))
         END IF



         !-------------------------------------------------------------------------
         !---- BENEFIT AMOUNT notes:
         !---- 1. A benefit is computed for all households that pass the asset
         !----    and both income tests or are BBCE.
         !---- 2. The benefit amount is equal to the maximum benefit allotment
         !----    less some portion of net income.
         !---- 3. The reduced net income amount (after applying the benefit
         !----    reduction rate) is rounded to the nearest $1.
         !---- 4. The maximum benefit amount depends on geographic area, unit size,
         !---     and on NTH.
         !---- 5. The benefit reduction rate depends on NTH.
         !---- 6. The benefit amount must be non-negative.
         !---- 7. The minimum benefit varies by geographic area, unit size and NTH
         !----    and is only applied if the unit is eligible for a positive benefit.
         !--------------------------------------------------------------------------

         IF (    FSASTEST(IUNIT) == 1  &
           .AND. FSGRTEST(IUNIT) == 1  &
           .AND. FSNETEST(IUNIT) == 1)  THEN


            !------------------------------------------------------------------
            !---  Calculate any state provision benefits included in the model.
            !---  If -1 is returned, calculate standard benefit:
            !------------------------------------------------------------------
            CALL db_fs_CALC_BENEFIT(IUNIT)

            IF (FSBEN(IUNIT) == -1) THEN
               FSBEN(IUNIT) = snap_benefit(MAX_BENEFIT(IUNIT), MIN_BENEFIT(IUNIT), FSNETINC(IUNIT))
            end if

         ELSE
            FSBEN(IUNIT) = 0
         END IF


         CALL db_fs_calc_ben_post(iunit)


         HH_FSBEN = HH_FSBEN + FSBEN(IUNIT)

         !-------------------------------------------------------------------
         !---- AT MINIMUM BENEFIT notes:
         !---- 1. Units are at the minimum benefit if they have a positive
         !----    benefit and it equals the mimimum benefit (see above comments).
         !---- 2. FSMINBEN  = 0/1 indicating unit has minimum benefit
         !-------------------------------------------------------------------

         IF (FSBEN(IUNIT) > 0) THEN
             IF (FSBEN(IUNIT) == MIN_BENEFIT(IUNIT)) THEN
                 FSMINBEN(IUNIT) = 1
             ELSE
                 FSMINBEN(IUNIT) = 0
             END IF
         ELSE
            FSMINBEN(IUNIT) = 0
         END IF



         !---------------------------------------------------------------------------
         !---- Finally, calculate marginal effective deduction amounts (e.g. the amount
         !---- by which net income would increase if the deduction were not available.)
         !---- Only used for debug output, and possibly in Table 5 (depending on DEDTYPE user parameter).
         !---------------------------------------------------------------------------

         DED(1)= FSSTDDED(IUNIT)
         DED(2)= FSERNDED(IUNIT)
         DED(3)= FSMEDDED(IUNIT)
         DED(4)= FSDEPDED(IUNIT)

         DO IDED = 1, 5

           PRE_NETINC = FSGRINC(IUNIT)

           DO JDED = 1, 4
             IF (JDED == IDED) CYCLE  ! don't count this deduction, & see marginal effect on net income
             PRE_NETINC = PRE_NETINC - DED(JDED)
           ENDDO

           PRE_NETINC = MAX(0, PRE_NETINC)

           IF (IDED == 5) THEN
             FSSLTDED_TEMP = 0
           ELSE
             fssltded_temp   = snap_shelter_deduction(FSSLTEXP(IUNIT), pre_netinc, fsnelder(iunit), fsndis(iunit), geog_ded)
           ENDIF

           PRE_NETINC = MAX(0, PRE_NETINC - FSSLTDED_TEMP)

           SELECT CASE(IDED)
             CASE(1)
               FSSTDDED_ME(IUNIT) = PRE_NETINC - FSNETINC(IUNIT)
             CASE(2)
               FSERNDED_ME(IUNIT) = PRE_NETINC - FSNETINC(IUNIT)
             CASE(3)
               FSMEDDED_ME(IUNIT) = PRE_NETINC - FSNETINC(IUNIT)
             CASE(4)
               FSDEPDED_ME(IUNIT) = PRE_NETINC - FSNETINC(IUNIT)
             CASE(5)
               FSSLTDED_ME(IUNIT) = PRE_NETINC - FSNETINC(IUNIT)
           END SELECT
         ENDDO

         FSTOTDED_ME(IUNIT) = FSGRINC(IUNIT) - FSNETINC(IUNIT)  !-- marginal total deduction

    END DO  !-- loop over food stamp units

    RETURN
    END
