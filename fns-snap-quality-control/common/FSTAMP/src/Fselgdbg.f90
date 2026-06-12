!**************************************************************************************************
! Source File:  FSELGDBG.F90                
! Called By:    FSTAMP2                     
!
! Displays debug print for the FS_ELIGIBILITY routine (keof=2).
!
! Modifications:
!**************************************************************************************************
    SUBROUTINE FS_DISPLAY_ELIG_DEBUG
    USE GLOBAL
    USE USERPARM, ONLY : DOSTATE
    USE STATES
    USE FSSIZES
    USE FSPARM
    USE FSWORK
    USE FSLOCS, ONLY : l_fsastest, l_fsgrtest, l_fsnetest, l_fsben
    IMPLICIT NONE

    INTEGER       ::  IUNIT  ,NBR_UNITS ,PREV_DEDS, ELD_IDX
    CHARACTER(60) ::  TEST_RESULT
    CHARACTER(60) ::  RUN_TYPE
    CHARACTER(60) ::  BASE_TEST_RESULT
    CHARACTER (9) ::  DEDUCTION_LABEL(2) = (/'EFFECTIVE' , 'ENTITLED '/)

    integer :: gross_csp_deduction, effective_gross_income
    !--------------------------------------------------------------------
    ! BEGIN PROCESSING
    !--------------------------------------------------------------------
 
    IF (KFREQ == 0) RETURN  ! no debug requested for this household

    NBR_UNITS = 0
    DO IUNIT = 1, CTPRHH
       IF (FSUN(IUNIT) == IUNIT)  NBR_UNITS = NBR_UNITS + 1
    END DO
 
    IF (NBR_UNITS == 0) RETURN ! no food stamp units to display in debug
 

    IF (DOSTATE == 1) THEN
       RUN_TYPE = 'NATIONAL SIMULATION'
    ELSE
       RUN_TYPE = 'STATE SIMULATION'
    END IF


    NBR_UNITS = 0
 
    DO IUNIT = 1, CTPRHH
 
       IF (FSUN(IUNIT) /= IUNIT) CYCLE  ! not an FSU head
 
       NBR_UNITS = NBR_UNITS + 1
 
       CALL ISNEWPG(PRFILE, 6 )
       WRITE(PRFILE, 1020) NBR_UNITS, FSUN(IUNIT), FSUSIZE(IUNIT)
 
       !----   ASTEST debug
       CALL ISNEWPG(PRFILE, 15)
       WRITE(PRFILE, 2000)
 
       IF (FSASTEST(IUNIT) == 1) THEN
           TEST_RESULT = 'PASSED'
       ELSE
           TEST_RESULT = 'FAILED'
       END IF

       IF (L_FSASTEST(1, KIST)%IPER(IUNIT) == 1) THEN
           BASE_TEST_RESULT = 'PASSED'
       ELSE
           BASE_TEST_RESULT = 'FAILED'
       END IF

 
       ELD_IDX = ASSET_IDX(IUNIT)

       WRITE(PRFILE, 2020)         &
            NTH                    &
           ,CHAR_ST(STATE_IDX(FSTATE))(2:3) &
           ,RUN_TYPE               & 
           ,IUNIT                  &
           ,FSUSIZE(IUNIT)         &
           ,CATEG_ELIG(IUNIT)      &
           ,FSALLPA(IUNIT)         &
           ,FSNELDER(IUNIT)        &
           ,FSNDIS  (IUNIT)        &
           ,FSASSET(IUNIT)         &
           ,NINT(ASSETLIM(istate, ELD_IDX, NTH)) & 
           ,TEST_RESULT            &
           ,BASE_TEST_RESULT
 
        !----   GRTEST debug

         IF (APPLY_EFFECTIVE_GROSS_INCOME(IUNIT)) THEN
            gross_csp_deduction = FSCSPDED(IUNIT)   
            EFFECTIVE_GROSS_INCOME = MAX(0, FSGRINC(IUNIT) - FSCSPDED(IUNIT))   
         ELSE
            gross_csp_deduction = 0 
            EFFECTIVE_GROSS_INCOME = FSGRINC(IUNIT)                   
         END IF      
        
        CALL ISNEWPG(PRFILE, 16)
        WRITE(PRFILE, 3000)
 
        IF (FSGRTEST(IUNIT) == 1) THEN
            TEST_RESULT = 'PASSED'
        ELSE
            TEST_RESULT = 'FAILED'
        END IF

        IF (L_FSGRTEST(1, KIST)%IPER(IUNIT) == 1) THEN
           BASE_TEST_RESULT = 'PASSED'
        ELSE
           BASE_TEST_RESULT = 'FAILED'
        END IF

        WRITE(PRFILE,3020)     &
            NTH, IUNIT, FSUSIZE(IUNIT) &
           ,CATEG_ELIG(IUNIT)  &
           ,FSALLPA(IUNIT)     &
           ,AGEDSCRN(NTH)      &
           ,FSNELDER(IUNIT)    &
           ,FSNDIS(IUNIT)      &
           ,FSGRINC(IUNIT)     &
           ,FSCSPDED(IUNIT)    & 
           ,EFFECTIVE_GROSS_INCOME  & 
           ,GROSS_SCREEN(IUNIT)&
           ,TEST_RESULT        &
           ,BASE_TEST_RESULT
 
        !----   DEDUCTIONS debug
 
        PREV_DEDS  = FSSTDDED(IUNIT) + FSERNDED(IUNIT) &
                   + FSMEDDED(IUNIT) + FSDEPDED(IUNIT) + FSCSPDED(IUNIT)
 
        CALL ISNEWPG(PRFILE, 19)
        WRITE(PRFILE, 3040)                                             &
            FSGRINC(IUNIT)                                              &
           ,FSSTDDED(IUNIT)                                             &
           ,FSERNDED(IUNIT), EARNDED(NTH), FSEARN(IUNIT), EARNMAX(NTH)  &
           ,FSMEDDED(IUNIT), FSMEDEXP(IUNIT), MDTHRESH(NTH)             &
           ,FSDEPDED(IUNIT), FSDEPEXP(IUNIT)                            &
           ,FSCSPDED(IUNIT)                                             &
           ,FSHOMEDED(IUNIT)                                            &
           ,FSSLTDED(IUNIT), FSSLTEXP(IUNIT)                            &
           ,SHLTRPCT(NTH)                                               &
           ,FSGRINC(IUNIT), PREV_DEDS                                   &
           ,SHELCAP(GEOG_DED, NTH), SHLCMULT(NTH)                       &
           ,FSNETINC(IUNIT)                                             &
           ,FSTOTDED(IUNIT)
 
         CALL ISNEWPG(PRFILE, 10)
         WRITE(PRFILE, 3045)    & 
            FSSTDDED_ME(IUNIT)  &
           ,FSERNDED_ME(IUNIT)  &
           ,FSMEDDED_ME(IUNIT)  &
           ,FSDEPDED_ME(IUNIT)  &
           ,FSSLTDED_ME(IUNIT)  &
           ,FSTOTDED_ME(IUNIT)  &
           ,DEDUCTION_LABEL(DEDTYPE(NTH))
 
        !----   NETEST debug
        CALL ISNEWPG(PRFILE, 16)
        WRITE(PRFILE, 4000)
 
        IF (FSNETEST(IUNIT) == 1) THEN
            TEST_RESULT = 'PASSED'
        ELSE
            TEST_RESULT = 'FAILED'
        END IF

        IF (L_FSNETEST(1, KIST)%IPER(IUNIT) == 1) THEN
           BASE_TEST_RESULT = 'PASSED'
        ELSE
           BASE_TEST_RESULT = 'FAILED'
        END IF
 
        WRITE(PRFILE, 4020)  &
            NTH, IUNIT, FSUSIZE(IUNIT) &
           ,CATEG_ELIG(IUNIT)   &
           ,FSALLPA(IUNIT)   &
           ,AGEDSCRN(NTH)    &
           ,FSNELDER(IUNIT)  &
           ,FSNDIS(IUNIT)    &
           ,FSNETINC(IUNIT)  &
           ,NET_SCREEN(IUNIT)&
           ,TEST_RESULT      & 
           ,BASE_TEST_RESULT
 
        !---- Only show the benefit amount debug print if the unit
        !---- passes the asset and income tests or the unit is BBCE
        CALL ISNEWPG(PRFILE, 4)
        WRITE(PRFILE, 5000)
 
        IF    ((FSASTEST(IUNIT) == 1  &
          .AND. FSGRTEST(IUNIT) == 1  &
          .AND. FSNETEST(IUNIT) == 1) & 
           .OR. CATEG_ELIG(IUNIT) ) THEN
 
            CALL ISNEWPG(PRFILE, 7)
            WRITE(PRFILE, 5020)   &
               FSBEN(IUNIT)       &
              ,MAX_BENEFIT(IUNIT) &
              ,BRR(NTH)           &
              ,FSNETINC(IUNIT)    &
              ,MIN_BENEFIT(IUNIT) &
              ,FSMINBEN(IUNIT)    &
              ,L_FSBEN(1, KIST)%IPER(IUNIT)
 
         ELSE
 
            CALL ISNEWPG(PRFILE, 3)
            WRITE(PRFILE, 5010)
 
         END IF  ! end of benefit amt debug
 
         CALL ISNEWPG(PRFILE, 2)
         WRITE(PRFILE, 5040) &
            FSPOVRAT(IUNIT)  &
           ,FSGRINC(IUNIT)   &
           ,POVERTY(IUNIT)
           
    END DO
 
    !---- Final results
    CALL ISNEWPG(PRFILE, 5)
    WRITE(PRFILE, 6000)
 
    CALL ISNEWPG(PRFILE, NBR_UNITS + 3)
    WRITE(PRFILE, 6010)
 
    DO IUNIT = 1, CTPRHH
       IF (FSUN(IUNIT) /= IUNIT) CYCLE  ! not an FSU head
 
       WRITE(PRFILE, 6020)  &
            IUNIT           &
           ,FSASTEST(IUNIT) &
           ,FSGRTEST(IUNIT) &
           ,FSNETEST(IUNIT) &
           ,FSBEN(IUNIT)    &
           ,FSMINBEN(IUNIT) &
           ,FSPOVRAT(IUNIT) &
           ,FSGRINC(IUNIT)  
    END DO
 
    CALL ISNEWPG(PRFILE, NBR_UNITS + 4)
    WRITE(PRFILE, 6030)
    DO IUNIT = 1, CTPRHH
       IF (FSUN(IUNIT) /= IUNIT) CYCLE  ! not an FSU head
 
        WRITE(PRFILE, 6040) &
            IUNIT           &
           ,FSSTDDED(IUNIT) &
           ,FSERNDED(IUNIT) &
           ,FSMEDDED(IUNIT) &
           ,FSDEPDED(IUNIT) &
           ,FSSLTDED(IUNIT) &
           ,FSCSPDED(IUNIT) &
           ,FSHOMEDED(IUNIT) &
           ,FSTOTDED(IUNIT) &
           ,FSNETINC(IUNIT)
    END DO
    RETURN

!--------------------------------------------------------------------
! FORMAT STATEMENTS
!--------------------------------------------------------------------
1020   FORMAT(1X, 130('-')                                    &
      // 1X, 'THIS SECTION SHOWS THE APPLICATION OF THE FOOD' &
           , ' STAMP PROGRAM''S ELIGIBILITY RULES TO  '       &
       / 1X, 'EACH UNIT.  THE FSP BENEFIT TO WHICH EAC'       &
           , 'H ELIGIBLE UNIT IS ENTITLED IS THEN CALCULATED.'&
      // 1X, 'UNIT#: ', I5, 3X, 'FSUN: ', I5,                 &
         3X, 'FSUSIZE: ', I5)                                  
 
2000    FORMAT(/, 1X, 'ASSET TEST: '  &
               /, 1X, '------------')
2020    format(    1x, 'NTH:                       ', i2, 5x, a2, 2x, a  &
                /, 1x, 'UNIT:                      ', i2, &
                /, 1x, 'UNIT SIZE:                 ', i2  &
                /, 1x, 'CATEG ELIG UNIT:           ', L2, 5x, '(VALUE OF T MEANS AUTOMATICALLY PASS THIS TEST)'  &
                /, 1x, 'PURE PA UNIT:              ', i2, 5x, '(VALUE OF 1 MEANS AUTOMATICALLY PASS THIS TEST)'  &
               ,/, 1x, 'NUMBER OF ELDERLY:         ', i2           &
               ,/, 1x, 'NUMBER OF DISABLED:        ', i2           &
               ,/, 1x, 'COUNTABLE ASSETS:  ', i10                  &
               ,/, 1x, 'ASSET LIMIT:       ', i10                  &
              ,//, 1x, 'TEST RESULT:           ', a  &
                /, 1x, 'BASELAW TEST RESULT:   ', a  &
               )
 
3000    FORMAT(/, 1X, 'GROSS INCOME TEST: ' ,/, 1X, '------------------')
 
3020    FORMAT( &
       ' NTH:                     ', I2  &
     / ' UNIT:                    ', I2  &
     / ' UNIT SIZE:               ', I2  &
     //' CATEG ELIG UNIT:         ', L2, 5X, '(VALUE OF T MEANS AUTOMATICALLY PASS THIS TEST)'  &
      /' PURE PA UNIT:            ', I2, 5X, '(VALUE OF 1 MEANS AUTOMATICALLY PASS THIS TEST)'  &
      /' AGED SCREEN USER PARAM:  ', I2, 5X, '(VALUE OF 2 AND PRESENCE OF ELDERLY OR DISABLED ' &
                                            ,'MEANS THIS TEST IS AUTOMATICALLY PASSED)'         &
      /' NUMBER OF ELDERLY:       ', I2                   &
      /' NUMBER OF DISABLED:      ', I2                   &
      /' GROSS INCOME:        ', I6                       &
      /' CHILD SUPPORT DED:   ', I6                       &
      /' EFFECTIVE GROSS INC: ', I6                       &
      /' GROSS INCOME SCREEN: ', I6                       &
     //' TEST RESULT:         ', A  &
     /,' BASELAW TEST RESULT: ', A  &
               )
 
3040    FORMAT(/  1X, 'DEDUCTIONS FROM GROSS INCOME: '          &
         /  1X, '-----------------------------'                 &
      / 1X, 'GROSS INCOME:         ', I10                       &
      / 1X, ' LESS STANDARD DEDUCTION: ', I6                    &
      / 1X, ' LESS EARNINGS DEDUCTION: ', I6, ' (', F6.4, ' TIMES EARNINGS ', I8, ', BUT CANNOT BE MORE THAN ', I8, ')'  &
      / 1X, ' LESS MEDICAL  DEDUCTION: ', I6, ' (MEDICAL EXPENSES OF ELDERLY/DISABLED PERSONS', I6, ' LESS', f5.0, ')'   &
      / 1X, ' LESS DEPCARE  DEDUCTION: ', I6, ' (EQUALS THE REPORTED DEPCARE EXPENSE ',I6, ')'  &
      / 1X, ' LESS CHILD SUPPORT DED:  ', I6, ' (EQUAL TO CHILD SUPPORT PAYMENT EXPENES)'       &
      / 1X, ' LESS HOMELESS DED:       ', I6,                                                   &
      / 1X, ' LESS EXCESS SHELTER DED: ', I6, ' (REPORTED SHELTER EXPENSE ', I6                 &
      /35X, 'LESS ', F6.4, ' TIMES (GROSS INCOME ', I6          &
          , ' LESS THE PREVIOUS DEDUCTIONS ', I6, ')'           &
      /35X, 'SUBJECT TO A CAP OF',  f7.0                        &
          , ' AND A MULTIPLIER OF ', F10.4, ')'                 &
      / 1X, 'EQUALS NET INCOME:        ', I6                    &
     // 1X, 'TOTAL DEDUCTIONS:         ', I6                    &
     // 1X, 'NOTE: DEDUCTION AMOUNTS SHOWN ABOVE ARE ENTITLED DEDUCTION AMOUNTS.'               &
      )
 
3045  FORMAT(                                                       &
        / 1X, 'MARGINAL EFFECTIVE VALUE OF EACH DEDUCTION:'         &
        / 1X, '  STANDARD:      ' , I6                              &
        / 1X, '  EARNINGS:      ' , I6                              &
        / 1X, '  MEDICAL:       ' , I6                              &
        / 1X, '  DEPENDENT CARE:' , I6                              &
        / 1X, '  EXCESS SHELTER:' , I6                              &
        / 1X, '  ALL DEDUCTIONS:' , I6                              &
       // 1X, 'NOTE: TABLE 5 WILL USE THE ', A, ' DEDUCTION VALUES')
 
4000    FORMAT(//, 1X, 'NET INCOME TEST: '  /  1X, '----------------')
 
4020    FORMAT( &
          1X, 'NTH:                     ', I2  &
        / 1X, 'UNIT:                    ', I2  &
        / 1X, 'UNIT SIZE:               ', I2  &
       // 1X, 'CATEG ELIG UNIT:         ', L2, 5X,'(VALUE OF T MEANS AUTOMATICALLY PASS THIS TEST)'  &
       /  1X, 'PURE PA UNIT:            ', I2, 5X,'(VALUE OF 1 MEANS AUTOMATICALLY PASS TEST)'       &
       /  1X, 'AGED SCREEN USER PARAM:  ', I2, 5X,'(VALUE OF 1 AND PRESENCE OF ELDERLY OR DISABLED ' &
                                                 ,'MEANS THIS TEST IS AUTOMATICALLY PASSED)'         &
       /  1X, 'NUMBER OF ELDERLY:       ', I2               &
       /  1X, 'NUMBER OF DISABLED:      ', I2               &
       /  1X, 'NET INCOME:          ', I6                   &
       /  1X, 'NET INCOME SCREEN:   ', I6                   &
      //  1X, 'TEST RESULT:         ', A   &
      /,  1X, 'BASELAW TEST RESULT: ', A   &
               )
 
5000  FORMAT(// 1X, 'BENEFIT AMOUNT: ' / 1X, '--------------- ')
 
5010  FORMAT(                                                     &
       / 1X, 'BENEFIT AMOUNT: NOT COMPUTED SINCE THE UNIT DID NOT'&
       / 1X, 'PASS THE ASSET, GROSS INCOME, AND/OR NET INCOME '   &
           , 'SCREENS'                                            &
        )
 
5020  FORMAT(                                            &
      / 1X, 'BENEFIT AMOUNT: ', I6, ' (MAXIMUM BENEFIT ALLOTMENT   ', I6, ' LESS ', F5.4, ' TIMES NET INCOME ', I6  &
      /25X, 'BENEFIT AMT MUST NOT BE LESS THAN ', I6, ')'  &
     // 1X, 'AT THE MINIMUM BENEFIT: ', I2, ' (VALUE OF 1 MEANS YES)'   &
      / 1X, 'BASELAW AMOUNT: ', I6 &
       )

 
5040  FORMAT(                                           &
      / 1X, 'POVERTY RATIO: ', F6.2                     &
          , ' (GROSS INCOME ', I6, ' DIVIDED BY THE'    &
          , ' BASELINE FSP NET INCOME SCREEN', f6.0, ')'  &
        )
 
6000  FORMAT(/ 1X, 130('-')                         &
           // 1X, 'SUMMARY OF ELIGIBILITY RESULTS:' &
            / 1X, '-------------------------------' &
        )
 
6010  FORMAT(                              &
          /,1X, ' FSUN'   , 2X, 'FSASTEST' &
           ,2X, 'FSGRTEST', 2X, 'FSNETEST' &
           ,2X, '   FSBEN', 2X, 'FSMINBEN' &
           ,2X, 'FSPOVRAT', 2X, ' FSGRINC' &
          /,1X, '-----'   , 2X, '--------' &
           ,2X, '--------', 2X, '--------' &
           ,2X, '--------', 2X, '--------' &
           ,2X, '--------', 2X, '--------' &
        )
 
6020  FORMAT(1X, I5, 5(2X, I8), 2X, F8.2, 2X, I8)
 

6030  FORMAT(/                             &
             / 1X,' FSUN',2X,'FSSTDDED',2X,'FSERNDED',2X,'FSMEDDED' ,2X,'FSDEPDED', &
               2X, 'FSSLTDED',2x, 'FSCSPDED',2x,'FSHOMEDED',2X,'FSTOTDED',2X,'FSNETINC' &
             / 1X,' ----',2X,'--------',2X,'--------',2X,'--------' ,2X,'--------',2X, '--------', &
               2x, '--------',2x,'---------',2X,'--------',2X,'--------' &
        )
 
6040  FORMAT(1X, I5, 6(2X, I8), 2x,I9, 2(2x,i8))

    END
