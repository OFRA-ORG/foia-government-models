!**************************************************************************************************
! Source File:  FSREFALG.F90                
! Called By:    DB_FS_PARTICIPATION         
!
! This is the reform participation algorithm that uses a probit
! model to determine the probability of participation for two cases:
!    1. Household is eligible but not participating in baselaw.
!       What is the likelihood that this household will participate
!       in reform given an increase in its benefit?
!    2. Household participates in baselaw.  What is the likelihood
!       that this household will continue to participate in reform
!       given a decrease in its benefit?
!
! Modifications:
!**************************************************************************************************
    SUBROUTINE FS_REFALGO
    USE GLOBAL
    USE FSWORK
    USE FSREFALG
    use mathrand_module
    
    IMPLICIT NONE
 
    !--------------------------------------------------------------------
    ! BEGIN PROCESSING
    !--------------------------------------------------------------------

    !---- Check whether HH_BASE_FSBEN is positive
    !---- If not, this algorithm does not apply
 
    MU = 0.0
 
    IF (HH_BASE_FSBEN(kist) <= 0) &
       CALL ERROR_MSG('FS_REFALGO', 'SHOULD NOT CALL REFALGO IF BASE_FSBEN = 0', ABORT)
 
    FSBEN_DIFF = HH_FSBEN - HH_BASE_FSBEN(kist)
 
    !---- Put household characteristics into vector X_VAR(), making sure
    !---- order matches that of probit coefficients

    X_VAR = 0
 
    X_VAR(1) = 1    ! constant
 
    !---- Household size recodes
    IF (CTPRHH == 2) THEN
       X_VAR(2)  = 1
    ELSE IF (CTPRHH == 3) THEN
       X_VAR(3)  = 1
    ELSE IF  (CTPRHH == 4) THEN
       X_VAR(4)  = 1
    ELSE IF (CTPRHH == 5) THEN
       X_VAR(5)  = 1
    ELSE IF (CTPRHH .GE. 6) THEN
        X_VAR(6)  = 1
    END IF
 
    !---- Poverty ratio recodes
    POVRAT = real(HH_TOTINC) / HH_POVLEV   

    IF (POVRAT <= 0.0 ) THEN
         X_VAR(7)  = 1
    ELSE IF (POVRAT <= 0.50) THEN
         X_VAR(8)  = 1
    ELSE IF  (POVRAT <= 0.75) THEN
         X_VAR(9)  = 1
    ELSE IF  (POVRAT .GE. 1.00 )THEN
         X_VAR(10) = 1
    END IF
 
    !---- Reference person's age
    IF (R_AGE .GE. 30 .AND. R_AGE <= 39) THEN
         X_VAR(11) = 1
    ELSE IF (R_AGE .GE. 40 .AND. R_AGE <= 59) THEN
         X_VAR(12) = 1
    ELSE IF (R_AGE .GE. 60 .AND. R_AGE <= 69) THEN
         X_VAR(13) = 1
    ELSE IF (R_AGE .GE. 70) THEN
         X_VAR(14) = 1
    END IF
 
    !---- Reference person's hispanic and race recodes
    IF (R_RACETH== 3) THEN
       X_VAR(15) = 1    ! hispanic
    ELSE IF (R_RACETH == 2) THEN
       X_VAR(16) = 1    ! black non-hispanic
    END IF
 
    !---- Reference person's education recodes
    IF (R_GENERIC_EDUC == 1) THEN
        X_VAR(17) = 1       ! less than h.s. diploma
    ELSE IF (R_GENERIC_EDUC == 3) THEN
        X_VAR(18) = 1       ! more than h.s. diploma
    END IF
 
    IF (HH_NKIDS > 0)   X_VAR(19) = 1
    IF (HH_MEAN > 0)    X_VAR(20) = 1
    IF (HH_ASSET > 0)   X_VAR(21) = 1
    IF (HH_WAGE > 0)    X_VAR(22) = 1
 
    !---- X_VAR(22) is the last dummy variable in probit equation
 
    !---- Compute XB
    XB = 0.0
    IF (X_VAR(01) > 0) XB = XB + BETA01
    IF (X_VAR(02) > 0) XB = XB + BETA02
    IF (X_VAR(03) > 0) XB = XB + BETA03
    IF (X_VAR(04) > 0) XB = XB + BETA04
    IF (X_VAR(05) > 0) XB = XB + BETA05
    IF (X_VAR(06) > 0) XB = XB + BETA06
    IF (X_VAR(07) > 0) XB = XB + BETA07
    IF (X_VAR(08) > 0) XB = XB + BETA08
    IF (X_VAR(09) > 0) XB = XB + BETA09
    IF (X_VAR(10) > 0) XB = XB + BETA10
    IF (X_VAR(11) > 0) XB = XB + BETA11
    IF (X_VAR(12) > 0) XB = XB + BETA12
    IF (X_VAR(13) > 0) XB = XB + BETA13
    IF (X_VAR(14) > 0) XB = XB + BETA14
    IF (X_VAR(15) > 0) XB = XB + BETA15
    IF (X_VAR(16) > 0) XB = XB + BETA16
    IF (X_VAR(17) > 0) XB = XB + BETA17
    IF (X_VAR(18) > 0) XB = XB + BETA18
    IF (X_VAR(19) > 0) XB = XB + BETA19
    IF (X_VAR(20) > 0) XB = XB + BETA20
    IF (X_VAR(21) > 0) XB = XB + BETA21
    IF (X_VAR(22) > 0) XB = XB + BETA22
 
    XB = XB + ALPHA*ALOG(FLOAT(HH_BASE_FSBEN(kist)))
 
    !---- Compute density and cdf of standard normal, and Mill's ratio
 
    DENSITY = DEN(XB)
    CDF = CUM(XB)
 
    IF (FSBEN_DIFF > 0) THEN
        MILLS = DENSITY / (1.0 - CDF)
    ELSE
        MILLS = DENSITY / CDF
    END IF
 
    !---- Compute MU, which is returned to the calling routine
 
    MU = MILLS * ALPHA * ABS(FSBEN_DIFF) / HH_BASE_FSBEN(kist)
 
    !---- Since MU is a probability, reset it if larger than 1 or smaller than 0
    IF (MU > 1.0) MU = 1.0
    IF (MU < 0.0) MU = 0.0
    RETURN
    END
