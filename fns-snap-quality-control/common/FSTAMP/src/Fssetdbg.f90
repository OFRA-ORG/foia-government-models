!**************************************************************************************************
! Source File:  FSSETDBG.F90                
! Called By:    FSTAMP2                     
!
! This subroutine identifies and tabulates interesting debug cases.
! We display the interesting cases only during nth = 1, but 
! tabulate them for all nths.
!
!**************************************************************************************************
    SUBROUTINE FS_SET_DEBUG
    USE GLOBAL
    USE USERPARM, ONLY : SHOWSTATE
    USE STATES
    USE FSSIZES
    USE FSPARM
    USE FSLOCS
    USE FSCNTS
    USE FSWORK
    use utils
    IMPLICIT NONE
    
    INTEGER ::  IUNIT , IP , NBR_UNITS, j
    LOGICAL ::  HH_W_CASHOT ,HH_W_FTSTUD ,base_potentially_ELIG

    character(len=2) :: in_state
    
    !--------------------------------------------------------------------
    ! BEGIN PROCESSING
    !--------------------------------------------------------------------
    NBR_UNITS = 0

    !----------------------------------------------------------------
    !---- Begin unit-level debug loop
    !----------------------------------------------------------------

    !! j is the showstate index:
    if (model_code == "MSIP") then
       j = KIST
       in_state = CHAR_ST(KIST)(2:3)
    else
       j = US_POS
       in_state = char_st(state_idx(fstate))(2:3)
    end if


    DO IUNIT = 1, CTPRHH
 
       IF (FSUN(IUNIT) /= IUNIT) CYCLE ! not an FSU head
 
       NBR_UNITS = NBR_UNITS + 1  ! number of units for this NTH
 
       !---- 'ZERO DEBUG'

       !---- TANF/FSP unit
       IF (FSTANF(IUNIT) > 0) THEN
          NBR_TANF(kist, nth) = NBR_TANF(kist, nth) + 1
          WGT_NBR_TANF(kist, nth) = WGT_NBR_TANF(kist, nth) + WGT
          IF (NBR_TANF(kist, nth) <= ZERO_DEBUG_DISPLAY .AND. DEBUGSTD .AND. SHOWSTATE(j)) THEN
             CALL DEBUG_MSG ('FSU WITH TANF IN ' // in_state, NBR_TANF(kist, nth))
          END IF
       END IF

 
       !---- SSI/FSP unit
       IF (FSSSI(IUNIT) > 0) THEN
          NBR_SSI(kist, nth) = NBR_SSI(kist, nth) + 1
          WGT_NBR_SSI(kist, nth) = WGT_NBR_SSI(kist, nth) + WGT
          IF (NBR_SSI(kist, nth) <= ZERO_DEBUG_DISPLAY .AND. DEBUGSTD .AND. SHOWSTATE(J)) THEN
             CALL DEBUG_MSG ('FSU WITH SSI IN ' // in_state, NBR_SSI(kist, nth))
          END IF
       END IF
 

       !---- 'MIN DEBUG'

       !---- Eligible not participating
       IF (FSBEN(IUNIT) > 0 .AND. FSPART(IUNIT) == 0) THEN
          NBR_ELIG_NO_PART(kist, nth) = NBR_ELIG_NO_PART(kist, nth) + 1
          WGT_NBR_ELIG_NO_PART(kist, nth) = WGT_NBR_ELIG_NO_PART(kist, nth) + WGT
          IF (NBR_ELIG_NO_PART(kist, nth) <= MIN_DEBUG_DISPLAY &
             .AND.DEBUGSTD          &
             .AND. SHOWSTATE(J)  &
             .AND. NTH == 1) THEN
             CALL DEBUG_MSG( & 
             'ELIGIBLE, NOT PARTICIPATING UNIT IN '// in_state, NBR_ELIG_NO_PART(kist, nth))
          END IF
       END IF
 

       !---- Eligible and participating
       IF  (FSBEN(IUNIT) > 0 .AND. FSPART(IUNIT) == 1) THEN
          NBR_ELIG_PART(kist, nth) = NBR_ELIG_PART(kist, nth) + 1
          WGT_NBR_ELIG_PART(kist, nth) = WGT_NBR_ELIG_PART(kist, nth) + WGT
          IF (NBR_ELIG_PART(kist, nth) <= ZERO_DEBUG_DISPLAY   &
              .AND. DEBUGSTD                                   &
              .AND. SHOWSTATE(J)                            &
              .AND. NTH == 1) THEN
             CALL DEBUG_MSG ( & 
             'ELIGIBLE, PARTICIPATING UNIT IN ' // in_state, NBR_ELIG_PART(kist, nth))
          END IF
        END IF

 
       !---- Eligible for $0 benefit
       IF (FSASTEST(IUNIT) == 1       &
        .AND. FSGRTEST(IUNIT) == 1    &
        .AND. FSNETEST(IUNIT) == 1    &
        .AND. FSBEN(IUNIT) == 0) THEN
          NBR_ELIG_NO_BEN(kist, nth) = NBR_ELIG_NO_BEN(kist, nth) + 1
          WGT_NBR_ELIG_NO_BEN(kist, nth) = WGT_NBR_ELIG_NO_BEN(kist, nth) + WGT
          IF (NBR_ELIG_NO_BEN(kist, nth) <= ZERO_DEBUG_DISPLAY &
             .AND. DEBUGSTD                &
             .AND. SHOWSTATE(J)            &
             .AND. NTH == 1) THEN
            CALL DEBUG_MSG ('ELIGIBLE FOR NO BENEFIT IN ' // in_state, NBR_ELIG_NO_BEN(kist, nth))
          END IF
       END IF
 
       !---- Minimum benefit
       IF (FSMINBEN(IUNIT) == 1) THEN
          NBR_MINBEN(kist, nth)    =  NBR_MINBEN(kist, nth) + 1
          WGT_NBR_MINBEN(kist, nth) = WGT_NBR_MINBEN(kist, nth) + WGT
          IF (NBR_MINBEN(kist, nth) <= ZERO_DEBUG_DISPLAY &
             .AND. DEBUGSTD        &
             .AND. SHOWSTATE(J) &
             .AND. NTH == 1) THEN
             CALL DEBUG_MSG ('RECEIVES MINIMUM BENEFIT IN '// in_state, NBR_MINBEN(kist, nth))
          END IF
       END IF

 
       !---- Zero net income (maximum benefit)
       IF (FSNETINC(IUNIT) == 0) THEN
          NBR_0NET(kist, nth) = NBR_0NET(kist, nth) + 1
          WGT_NBR_0NET(kist, nth) =WGT_NBR_0NET(kist, nth) + WGT
          IF (NBR_0NET(kist, nth) <= ZERO_DEBUG_DISPLAY &
             .AND. DEBUGSTD        &
             .AND. SHOWSTATE(J) &
             .AND. NTH == 1) THEN
             CALL DEBUG_MSG ('MAX BENEFIT (ZERO NET INCOME) IN ' // in_state, NBR_0NET(kist, nth))
          END IF
       END IF
 

       !---- Dependents and reported dependent care expense
       IF (FSDEPEXP(IUNIT) > 0 .AND. (FNDEPLT2(IUNIT) > 0 .OR. FNDEPGE2(IUNIT) > 0)) THEN
          NBR_W_FSNDEP_FSDEPEXP(kist, nth) = NBR_W_FSNDEP_FSDEPEXP(kist, nth) + 1
          WGT_NBR_W_FSNDEP_FSDEPEXP(kist, nth) = WGT_NBR_W_FSNDEP_FSDEPEXP(kist, nth) + WGT
          IF (NBR_W_FSNDEP_FSDEPEXP(kist, nth) <= ZERO_DEBUG_DISPLAY &
             .AND. DEBUGSTD        &
             .AND. SHOWSTATE(J) &
             .AND. NTH == 1) THEN
             CALL DEBUG_MSG (& 
             'DEPENDENTS AND DEPCARE EXPENSE IN ' // in_state,NBR_W_FSNDEP_FSDEPEXP(kist, nth))
          END IF
       END IF
 

       !---- Medical expenses over the threshold amount
       IF (FSMEDEXP(IUNIT) > MDTHRESH(nth)) THEN
          NBR_W_FSMEDEXP_GT_THRESH(kist, nth)     = NBR_W_FSMEDEXP_GT_THRESH(kist, nth) + 1
          WGT_NBR_W_FSMEDEXP_GT_THRESH(kist, nth) = WGT_NBR_W_FSMEDEXP_GT_THRESH(kist, nth) + WGT
          IF (NBR_W_FSMEDEXP_GT_THRESH(kist, nth) <= ZERO_DEBUG_DISPLAY &
             .AND. DEBUGSTD        &
             .AND. SHOWSTATE(J) &
             .AND. NTH == 1) THEN
             CALL DEBUG_MSG ('MEDICAL EXPENSES > THRESHOLD IN '// in_state, NBR_W_FSMEDEXP_GT_THRESH(kist, nth))
          END IF
       END IF


       !---- Apportioning shelter expenses
       IF  (CTPRHH /= FSUSIZE(IUNIT) .AND. FSSLTEXP(IUNIT) > 0) THEN
          NBR_APPORTION_FSSLTEXP(kist, nth) = NBR_APPORTION_FSSLTEXP(kist, nth) + 1
          WGT_NBR_APPORTION_FSSLTEXP(kist, nth) = WGT_NBR_APPORTION_FSSLTEXP(kist, nth) + WGT
          IF (NBR_APPORTION_FSSLTEXP(KIST, NTH) <= MIN_DEBUG_DISPLAY &
             .AND. DEBUGSTD      &
             .AND. SHOWSTATE(J)  &
             .AND. NTH == 1) THEN
             CALL DEBUG_MSG (& 
             'APPORTIONING SHELTER EXPENSES IN ' // in_state, NBR_APPORTION_FSSLTEXP(kist, nth))
          END IF
       END IF
 

       !---- Apportioning child care expenses
       IF (CTPRHH /= FSUSIZE(IUNIT) .AND. FSDEPEXP(IUNIT) > 0) THEN
          NBR_APPORTION_FSDEPEXP(kist, nth) =  NBR_APPORTION_FSDEPEXP(kist, nth) + 1
          WGT_NBR_APPORTION_FSDEPEXP(kist, nth) = WGT_NBR_APPORTION_FSDEPEXP(kist, nth) + WGT
          IF (NBR_APPORTION_FSDEPEXP(kist, nth) <= MIN_DEBUG_DISPLAY &
             .AND. DEBUGSTD        &
             .AND. SHOWSTATE(J) &
             .AND. NTH == 1) THEN
             CALL DEBUG_MSG (& 
             'APPORTIONING DEPENDENT CARE EXPENSE IN '// in_state, NBR_APPORTION_FSDEPEXP(kist, nth))
          END IF
       END IF
 



       IF  (BASELAW(nth) > ' ') THEN

          !---- Reform debug - baselaw elig/nonparticipant reform elig for more benefit
          IF (L_FSBEN(1, KIST)%IPER(IUNIT) > 0         &
             .AND.  L_FSPART(1, KIST)%IPER(IUNIT) == 0 &
             .AND.  L_FSBEN(1, KIST)%IPER(IUNIT) < L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT) ) THEN
             NBR_ELIG_MORE_BEN(kist, nth) = NBR_ELIG_MORE_BEN(kist, nth) + 1
             WGT_NBR_ELIG_MORE_BEN(kist, nth) = WGT_NBR_ELIG_MORE_BEN(kist, nth) + WGT
             IF (NBR_ELIG_MORE_BEN(kist, nth) <= MIN_DEBUG_DISPLAY &
                .AND. DEBUGSTD &
                .AND. SHOWSTATE(J)) THEN
                CALL DEBUG_MSG ( &
                'ELIGIBLE FOR MORE BENEFITS IN ' // in_state, NBR_ELIG_MORE_BEN(kist, nth))
             END IF
          END IF
 
          !---- Reform debug - baselaw participant reform elig for less benefit
          IF (L_FSBEN(1, KIST)%IPER(IUNIT) > 0         &
             .AND. L_FSPART(1, KIST)%IPER(IUNIT) == 1  &
             .AND. L_FSBEN(1, KIST)%IPER(IUNIT) > L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT)) THEN
             NBR_ELIG_LESS_BEN(kist, nth) =  NBR_ELIG_LESS_BEN(kist, nth) + 1
             WGT_NBR_ELIG_LESS_BEN(kist, nth) = WGT_NBR_ELIG_LESS_BEN(kist, nth) + WGT
             IF (NBR_ELIG_LESS_BEN(kist, nth) <= MIN_DEBUG_DISPLAY &
                .AND. DEBUGSTD  &
                .AND. SHOWSTATE(J)) THEN
                CALL DEBUG_MSG( &
                'ELIGIBLE FOR LESS BENEFITS IN ' // in_state, NBR_ELIG_LESS_BEN(kist, nth))
             END IF
          END IF

          !---- Reform debug - change in unit definition
          IF (L_FSUSIZE(1, KIST)%IPER(IUNIT) /= L_FSUSIZE(REFORM_IDX, KIST)%IPER(IUNIT)  &
         .OR. L_FSUN   (1, KIST)%IPER(IUNIT) /= L_FSUN   (REFORM_IDX, KIST)%IPER(IUNIT)) THEN
              NBR_REFORM_UNIT_CHG(kist, nth) = NBR_REFORM_UNIT_CHG(kist, nth) + 1
              WGT_NBR_REFORM_UNIT_CHG(kist, nth) = WGT_NBR_REFORM_UNIT_CHG(kist, nth) + WGT
              IF (NBR_REFORM_UNIT_CHG(kist, nth) <= DEBUGNBR  &
                 .AND. SHOWSTATE(J)) THEN
                 CALL DEBUG_MSG ( &
                 'CHANGE IN UNIT DEFINITION IN ' // in_state, NBR_REFORM_UNIT_CHG(kist, nth))
             END IF
          END IF
  
          !---- Reform debug - change in the asset/income test results
          IF (L_FSASTEST(1, KIST)%IPER(IUNIT) /= L_FSASTEST(REFORM_IDX, KIST)%IPER(IUNIT)  &
         .OR. L_FSGRTEST(1, KIST)%IPER(IUNIT) /= L_FSGRTEST(REFORM_IDX, KIST)%IPER(IUNIT)  &
         .OR. L_FSNETEST(1, KIST)%IPER(IUNIT) /= L_FSNETEST(REFORM_IDX, KIST)%IPER(IUNIT)) THEN
             NBR_REFORM_TEST_CHG(kist, nth) = NBR_REFORM_TEST_CHG(kist, nth) + 1
             WGT_NBR_REFORM_TEST_CHG(kist, nth) =  WGT_NBR_REFORM_TEST_CHG(kist, nth) + WGT
             IF (NBR_REFORM_TEST_CHG(kist, nth) <= DEBUGNBR  &
                .AND. SHOWSTATE(J)) THEN
                CALL DEBUG_MSG ( &
                'CHANGE IN ASSET/INCOME TEST RESULTS IN ' // in_state, NBR_REFORM_TEST_CHG(kist, nth))
             END IF
          END IF

          !---- Reform debug - change in the deduction amounts
          IF (L_FSTOTDED(1, KIST)%IPER(IUNIT) /=       &
              L_FSTOTDED(REFORM_IDX, KIST)%IPER(IUNIT) &
         .OR. L_FSSTDDED(1, KIST)%IPER(IUNIT) /=       &
              L_FSSTDDED(REFORM_IDX, KIST)%IPER(IUNIT) &
         .OR. L_FSERNDED(1, KIST)%IPER(IUNIT) /=       &
              L_FSERNDED(REFORM_IDX, KIST)%IPER(IUNIT) &
         .OR. L_FSMEDDED(1, KIST)%IPER(IUNIT) /=       &
              L_FSMEDDED(REFORM_IDX, KIST)%IPER(IUNIT) &
         .OR. L_FSDEPDED(1, KIST)%IPER(IUNIT) /=       &
              L_FSDEPDED(REFORM_IDX, KIST)%IPER(IUNIT) &
         .OR. L_FSSLTDED(1, KIST)%IPER(IUNIT) /=       &
              L_FSSLTDED(REFORM_IDX, KIST)%IPER(IUNIT)) THEN
             NBR_REFORM_DED_CHG(kist, nth) = NBR_REFORM_DED_CHG(kist, nth) + 1
             WGT_NBR_REFORM_DED_CHG(kist, nth) = WGT_NBR_REFORM_DED_CHG(kist, nth) + WGT
             IF (NBR_REFORM_DED_CHG(kist, nth) <= DEBUGNBR  &
                .AND. SHOWSTATE(J)) THEN
                CALL DEBUG_MSG( &
                'CHANGE IN DEDUCTION AMOUNTS IN ' // in_state, NBR_REFORM_DED_CHG(kist, nth))
             END IF
          END IF

          !---- Reform debug - change in the benefit amount
          IF (L_FSBEN(1, KIST)%IPER(IUNIT) /= L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT)) THEN
             NBR_REFORM_BEN_CHG(kist, nth) = NBR_REFORM_BEN_CHG(kist, nth) + 1
             WGT_NBR_REFORM_BEN_CHG(kist, nth) = WGT_NBR_REFORM_BEN_CHG(kist, nth) + WGT
             IF (NBR_REFORM_BEN_CHG(kist, nth) <= DEBUGNBR  &
                .AND. SHOWSTATE(J)) THEN
                CALL DEBUG_MSG( &
                'CHANGE IN BENEFIT AMOUNT IN ' // in_state, NBR_REFORM_BEN_CHG(kist, nth))
             END IF
          END IF

          !---- Reform debug - change in the participation decision
          IF (L_FSPART(1, KIST)%IPER(IUNIT) /=  L_FSPART(REFORM_IDX, KIST)%IPER(IUNIT)) THEN
             NBR_REFORM_PARTIC_CHG(kist, nth) =  NBR_REFORM_PARTIC_CHG(kist, nth) + 1
             WGT_NBR_REFORM_PARTIC_CHG(kist, nth) =  WGT_NBR_REFORM_PARTIC_CHG(kist, nth) + WGT
             IF (NBR_REFORM_PARTIC_CHG(kist, nth) <= DEBUGNBR  &
                .AND. SHOWSTATE(J)) THEN
                CALL DEBUG_MSG( &
                'CHANGE IN PARTICIPATION DECISION IN ' // in_state, NBR_REFORM_PARTIC_CHG(kist, nth))
             END IF
          END IF
   
       END IF !---- IF BASELAW > ' '
 
    END DO !--------------- end of unit loop

 
    !----------------------------------------------------------------
    !---- Begin gainer/loser unit loop (only during reforms)
    !----------------------------------------------------------------
 
    IF (BASELAW(nth) > ' ') THEN

       DO IUNIT = 1, CTPRHH
 
          IF (GL_BASE_FSUSIZE(IUNIT, kist) == 0) CYCLE ! not gainer/loser unit
 
          !---- Reform debug - baselaw participant, now ineligible
          IF  (GL_BASE_PARTIC(IUNIT, kist) .AND. GL_FSBEN(IUNIT) == 0) THEN
             NBR_PART_NEW_INELIG(kist, nth) =  NBR_PART_NEW_INELIG(kist, nth) + 1
             WGT_NBR_PART_NEW_INELIG(kist, nth) = WGT_NBR_PART_NEW_INELIG(kist, nth) + WGT
             IF (NBR_PART_NEW_INELIG(kist, nth) <= DEBUGNBR  &
                .AND. SHOWSTATE(J)) THEN
                CALL DEBUG_MSG ( &
                'BASELAW PARTICIPANT, NOW INELIGIBLE IN ' // in_state, NBR_PART_NEW_INELIG(kist, nth))
             END IF
          END IF
 
          !---- Reform debug - baselaw participant, now eligible, non-participant
          IF  (GL_BASE_PARTIC(IUNIT, kist)  &
              .AND. GL_FSBEN(IUNIT) > 0  &
              .AND. .NOT. GL_PARTIC(IUNIT)) THEN
             NBR_PART_NEW_NOPART(kist, nth)   =  NBR_PART_NEW_NOPART(kist, nth) + 1
             WGT_NBR_PART_NEW_NOPART(kist, nth)= WGT_NBR_PART_NEW_NOPART(kist, nth) + WGT
             IF (NBR_PART_NEW_NOPART(kist, nth) <= DEBUGNBR  &
                .AND. SHOWSTATE(J)) THEN
                CALL DEBUG_MSG ( &
                'BASELAW PARTIC, NOW ELIG NON-PARTIC IN ' // in_state, NBR_PART_NEW_NOPART(kist, nth))
             END IF
          END IF
 
          !---- Reform debug - baselaw ineligible, now participant
          IF  (GL_PARTIC(IUNIT).AND. GL_BASE_FSBEN(IUNIT, kist) == 0) THEN
             NBR_INELIG_NEW_PART(kist, nth) = NBR_INELIG_NEW_PART(kist, nth) + 1
             WGT_NBR_INELIG_NEW_PART(kist, nth) = WGT_NBR_INELIG_NEW_PART(kist, nth) + WGT
             IF (NBR_INELIG_NEW_PART(kist, nth) <= DEBUGNBR  &
                .AND. SHOWSTATE(J)) THEN
                CALL DEBUG_MSG (&
                'BASELAW INELIGIBLE, NOW PARTICIPANT IN ' // in_state, NBR_INELIG_NEW_PART(kist, nth))
             END IF
          END IF
 
          !---- Reform debug - baselaw eligible non-participant, now participant
          IF  (GL_PARTIC(IUNIT)           &
             .AND. GL_BASE_FSBEN(IUNIT, kist) > 0  &
             .AND. .NOT. GL_BASE_PARTIC(IUNIT, kist)) THEN
             NBR_ELIG_NEW_PART(kist, nth) = NBR_ELIG_NEW_PART(kist, nth) + 1
             WGT_NBR_ELIG_NEW_PART(kist, nth) = WGT_NBR_ELIG_NEW_PART(kist, nth) + WGT
             IF (NBR_ELIG_NEW_PART(kist, nth) <= DEBUGNBR  &
                .AND. SHOWSTATE(J)) THEN
                CALL DEBUG_MSG( &
                'BASELAW ELIG NON-PARTIC, NOW PARTIC IN ' // in_state, NBR_ELIG_NEW_PART(kist, nth))
             END IF
          END IF
 
       END DO ! end of gainer/loser units
 
    END IF  ! end of reform only clause

 
    !----------------------------------------------------------------
    !---- Begin debug (after unit-level information is tabulated)
    !----------------------------------------------------------------
    
                                               

    !---- Multiple food stamp units in a household
    IF (NBR_UNITS > 1) THEN
       NBR_HHS_MULT_FSU(kist, nth) = NBR_HHS_MULT_FSU(kist, nth) + 1
       WGT_NBR_HHS_MULT_FSU(kist, nth) = WGT_NBR_HHS_MULT_FSU(kist, nth) + WGT
       IF (NBR_HHS_MULT_FSU(kist, nth) <= MIN_DEBUG_DISPLAY  &
          .AND. debugstd .AND. NTH == 1 .AND. SHOWSTATE(J)) THEN
          CALL DEBUG_MSG ( &
          'MULTIPLE FOOD STAMP UNITS IN HHLD IN ' // in_state, NBR_HHS_MULT_FSU(kist, nth))
       END IF
    END IF
 

    !---- SSI cashout households and postsecondary students in household
    HH_W_CASHOT = .FALSE.
    HH_W_FTSTUD = .FALSE.
    DO IP = 1, CTPRHH
       IF (CASHOT(IP) == 1) HH_W_CASHOT = .TRUE.
       IF (FTSTUD(IP) == 1) HH_W_FTSTUD = .TRUE.
    END DO
 
    IF (HH_W_CASHOT) THEN
       NBR_CASHOT(kist, nth) = NBR_CASHOT(kist, nth) + 1
       WGT_NBR_CASHOT(kist, nth) = WGT_NBR_CASHOT(kist, nth) + WGT
       IF (NBR_CASHOT(kist, nth) <= MIN_DEBUG_DISPLAY &
          .AND. DEBUGSTD .AND. SHOWSTATE(J)) THEN
          CALL DEBUG_MSG ('SSI CASHOUT HOUSEHOLD IN ' // in_state, NBR_CASHOT(kist, nth))
       END IF
    END IF
 
    IF (HH_W_FTSTUD) THEN
       NBR_FTSTUD(kist, nth) = NBR_FTSTUD(kist, nth) + 1
       WGT_NBR_FTSTUD(kist, nth) = WGT_NBR_FTSTUD(kist, nth) + WGT
       IF (NBR_FTSTUD(kist, nth) <= ZERO_DEBUG_DISPLAY &
          .AND. debugstd .AND. NTH == 1 .AND. SHOWSTATE(J)) THEN
          CALL DEBUG_MSG ('POST-SECONDARY STUDENT HOUSEHOLD IN ' // in_state, NBR_FTSTUD(kist, nth))
       END IF
    END IF
 
    IF (NBR_UNITS == 0) THEN
       NBR_NO_FSU(kist, nth) = NBR_NO_FSU(kist, nth) + 1
       WGT_NBR_NO_FSU(kist, nth) = WGT_NBR_NO_FSU(kist, nth) + WGT
       IF (NBR_NO_FSU(kist, nth) <= ZERO_DEBUG_DISPLAY &
          .AND. debugstd  .AND. NTH == 1 .AND. SHOWSTATE(J)) THEN
          CALL DEBUG_MSG ('NO FOOD STAMP UNITS IN HH IN ' // in_state, NBR_NO_FSU(kist, nth))
       END IF
    END IF


    !--- Reform debug - no longer potentially eligible
    IF (BASELAW(nth) > ' ') THEN

       !---- Were there any potentially eligible units in this household
       !---- during baselaw?
       base_potentially_ELIG = .FALSE.
       DO IP = 1, CTPRHH
          IF (L_FSUN(1, KIST)%IPER(IP) > 0)   base_potentially_ELIG = .TRUE.
       END DO
 
       IF (NBR_UNITS == 0 .AND. base_potentially_ELIG) THEN
          NBR_NO_LONGER_CAT_ELIG(kist, nth) = NBR_NO_LONGER_CAT_ELIG(kist, nth) + 1
          WGT_NBR_NO_LONGER_CAT_ELIG(kist, nth) = WGT_NBR_NO_LONGER_CAT_ELIG(kist, nth) + WGT
          IF (NBR_NO_LONGER_CAT_ELIG(kist, nth) <= DEBUGNBR .AND. SHOWSTATE(J)) THEN
             CALL DEBUG_MSG ( &
             'NO LONGER potentially ELIGIBLE IN ' // in_state, NBR_NO_LONGER_CAT_ELIG(kist, nth))
          END IF
       END IF
 
       !---- Reform debug - now potentially eligible
       IF (NBR_UNITS > 0 .AND. .NOT. base_potentially_ELIG) THEN
          NBR_NOW_CAT_ELIG(kist, nth) = NBR_NOW_CAT_ELIG(kist, nth) + 1
          WGT_NBR_NOW_CAT_ELIG(kist, nth) = WGT_NBR_NOW_CAT_ELIG(kist, nth) + WGT
          IF (NBR_NOW_CAT_ELIG(kist, nth) <= DEBUGNBR .AND. SHOWSTATE(J)) THEN
             CALL DEBUG_MSG (&
             'NOW potentially ELIGIBLE IN ' // in_state, NBR_NOW_CAT_ELIG(kist, nth))
          END IF
       END IF
 
    END IF ! end of reform plan
 
    END
