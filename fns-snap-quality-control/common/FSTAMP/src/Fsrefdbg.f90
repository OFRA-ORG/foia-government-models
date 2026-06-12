!**************************************************************************************************
! Source File:  FSREFDBG.F90                
! Called By:    DB_FS_PARTICIPATION         
!
! Prints debug for the FS_REFALGO routine (keof=2).
!
! Modifications:
!**************************************************************************************************
    SUBROUTINE FS_DISPLAY_REFALGO_DEBUG

    USE GLOBAL
    USE FSSIZES
    USE FSPARM
    USE FSWORK
    USE FSREFALG
    IMPLICIT NONE

    !----------------------------------------------------------------
    ! BEGIN PROCESSING
    !--------------------------------------------------------------------
 
    IF (BASELAW(NTH) == ' ') RETURN  ! HH_BASE_ vars not yet computed, so return
 
    !---- Baselaw participating household, increase in benefits
    !---- Debugged in DB_FS_DISPLAY_PARTIC_DEBUG
    IF (HH_BASE_FSPART(kist) == 1 .AND. HH_FSBEN >= HH_BASE_FSBEN(kist)) RETURN
 
    !---- Baselaw non-participating household, decrease in benefits
    !---- Debugged in DB_FS_DISPLAY_PARTIC_DEBUG
    IF (HH_BASE_FSPART(kist) == 0 .AND. HH_FSBEN <= HH_BASE_FSBEN(kist)) RETURN
 
    !---- Newly eligible household
    !---- Debugged in DB_FS_DISPLAY_PARTIC_DEBUG
    IF  (HH_BASE_FSBEN(kist) == 0 .AND. HH_FSBEN > 0) RETURN
     

    IF (PRLEVEL(NTH) < MAX_PRLEVEL) RETURN
    
    !--------------------------------------------------------------------
    ! DETAILED REFALGO DEBUG PRINT
    !--------------------------------------------------------------------
 
 
     CALL ISNEWPG(PRFILE, 16)
     WRITE(PRFILE, 1000)

     WRITE(PRFILE, 1010)  ALPHA  ,'PROBIT COEFFICIENT ON LOG(BENEFITS)'

     WRITE(PRFILE, 1015)

     WRITE(PRFILE, 1020)                      &
     1, BETA01, X_VAR(1), 'CONSTANT'          & 
    ,2 ,BETA02, X_VAR(2), 'HHSIZE = 2'        &
    ,3 ,BETA03, X_VAR(3), 'HHSIZE = 3'        &
    ,4 ,BETA04, X_VAR(4), 'HHSIZE = 4'        &
    ,5 ,BETA05, X_VAR(5), 'HHSIZE = 5'        &
    ,6 ,BETA06, X_VAR(6), 'HHSIZE = 6+'       & 
    ,7 ,BETA07, X_VAR(7), 'POVRAT =  0'       &
    ,8 ,BETA08, X_VAR(8), 'POVRAT =  0 - 50'  &
    ,9 ,BETA09, X_VAR(9), 'POVRAT = 50 - 75'  &
    ,10,BETA10, X_VAR(10),'POVRAT = 100+'

     CALL ISNEWPG(PRFILE, 12)
     WRITE(PRFILE, 1020)                                 &
     11 ,BETA11, X_VAR(11), 'HEAD AGE = 30 - 39'         &
    ,12 ,BETA12, X_VAR(12), 'HEAD AGE = 40 - 59'         &
    ,13 ,BETA13, X_VAR(13), 'HEAD AGE = 60 - 69'         &
    ,14 ,BETA14, X_VAR(14), 'HEAD AGE = 70+'             &
    ,15 ,BETA15, X_VAR(15), 'HEAD RACE = HISP'           &
    ,16 ,BETA16, X_VAR(16), 'HEAD RACE = BLACK'          &
    ,17 ,BETA17, X_VAR(17), 'HEAD EDUC = < H.S. DIPLOMA' &
    ,18 ,BETA18, X_VAR(18), 'HEAD EDUC = > H.S. DIPLOMA' &
    ,19 ,BETA19, X_VAR(19), 'KIDS PRESENT'               &
    ,20 ,BETA20, X_VAR(20), 'RECEIVES PA'                &
    ,21 ,BETA21, X_VAR(21), 'HAS COUNTABLE ASSETS'       &
    ,22 ,BETA22, X_VAR(22), 'HAS EARNINGS'

    RETURN
!--------------------------------------------------------------------
! FORMAT STATEMENTS
!--------------------------------------------------------------------
1000  FORMAT(//1X, 'DEBUG FOR REFORM PARTICIPATION PROBIT MODEL:')
 
1010  FORMAT(1X, 'ALPHA = ', F9.6, 5X, A)
 
1015  FORMAT(1X, ' VAR#', 3X, '   BETA  ', 3X, ' XVAR' &
           / 1X, '-----', 3X, '---------', 3X, '-----')

1020  FORMAT(1X, I5, 3X, F9.6, 3X, I5, 3X, A)

    END
