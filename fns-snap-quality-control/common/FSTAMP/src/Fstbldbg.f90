!**************************************************************************************************
! Source File:  FSTBLDBG.F90                
! Called By:    FSTAMP2                     
!
! Prints debug for the FS_TABLES routine (keof=2).
!
!**************************************************************************************************
    SUBROUTINE FS_DISPLAY_TABLES_DEBUG
    USE GLOBAL
    USE FSSIZES
    USE FSPARM
    USE FSLOCS
    USE FSWORK
    IMPLICIT NONE
    INTEGER       ::  IUNIT
    LOGICAL       ::  FOUND_UNIT, same_unit_defn 
    !--------------------------------------------------------------------
    ! BEGIN PROCESSING
    !--------------------------------------------------------------------
    IF (KFREQ == 0) RETURN  ! no debug requested for this household
 
    IF (PRLEVEL(NTH) < MAX_PRLEVEL) RETURN
 
    !---- Different debug print is shown depending on whether the reform
    !---- includes a unit definition change
    same_unit_defn = .false.  !Initializing outside of loop to satify compiler. Reset in loop.
    IF (BASELAW(NTH) > ' ') THEN
       SAME_UNIT_DEFN = .TRUE.
       DO IUNIT = 1, CTPRHH
          IF ( L_FSUN(1, KIST)         %IPER(IUNIT) /= &
               L_FSUN(REFORM_IDX, KIST)%IPER(IUNIT) )  SAME_UNIT_DEFN = .FALSE.
       END DO
    END IF
 
    !---- Different debug is shown depending on whether there is an eligible
    !---- unit in the household
    FOUND_UNIT = .FALSE.
    DO IUNIT = 1, CTPRHH
       IF ( L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT) > 0)  FOUND_UNIT = .TRUE.
    END DO
 
    !---------------------------------------------------------------------------
    !---- TABLE 1 debug --------------------------------------------
    !---------------------------------------------------------------------------
    NUMLINES = CTPRHH + 12 
    CALL ISNEWPG(PRFILE, NUMLINES)
 
    IF (BASELAW(NTH) > ' ') THEN
       IF (SAME_UNIT_DEFN) THEN   ! title line
          WRITE(PRFILE, 1000)
       ELSE
          WRITE(PRFILE, 1050)
       END IF
 
       IF (FOUND_UNIT) THEN
          DO IUNIT = 1, CTPRHH
             IF (L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT) > 0) THEN
                IF (SAME_UNIT_DEFN) THEN  ! show baselaw and reform
                   WRITE(PRFILE, 1010)                 &
                   IUNIT                               &
                  ,L_FSBEN(1, KIST)%IPER(IUNIT)              &
                  ,L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT)     &
                  ,BASE_PARTIC(IUNIT, kist)                  &
                  ,PARTIC(IUNIT)                       &
                  ,L_FSUSIZE(1, KIST)%IPER(IUNIT)            &
                  ,L_FSUSIZE(REFORM_IDX, KIST)%IPER(IUNIT)
                ELSE   ! only show reform information
                   WRITE(PRFILE, 1040)                 &
                   IUNIT                               &
                  ,L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT)     &
                  ,PARTIC(IUNIT)                       &
                  ,L_FSUSIZE(REFORM_IDX, KIST)%IPER(IUNIT)
                END IF
             END IF
          END DO
       ELSE
          WRITE(PRFILE, 1020)  'NO UNITS ARE ELIGIBLE IN REFORM, SO TABLE 1 NOT CALLED'
       END IF
    ELSE
       WRITE(PRFILE, 1030)
       IF (FOUND_UNIT) THEN
          DO IUNIT = 1, CTPRHH
             IF (L_FSBEN(1, KIST)%IPER(IUNIT) > 0)   &
                WRITE(PRFILE, 1040)            &
                IUNIT                          &
               ,L_FSBEN(1, KIST)%IPER(IUNIT)         &
               ,BASE_PARTIC(IUNIT, kist)             &
               ,L_FSUSIZE(1, KIST)%IPER(IUNIT) 
          END DO
       ELSE
          WRITE(PRFILE, 1020) 'NO UNITS ARE ELIGIBLE IN BASELAW, SO TABLE 1 NOT CALLED'
       END IF
    END IF


    !---------------------------------------------------------------------------
    !---- table 2 debug ----------------------------------------------------------
    !---------------------------------------------------------------------------
    NUMLINES = CTPRHH + 7  
    CALL ISNEWPG(PRFILE, NUMLINES)
 
    IF (BASELAW(NTH) > ' ') THEN
       IF (SAME_UNIT_DEFN) THEN   ! title line
          WRITE(PRFILE, 2000)
       ELSE
          WRITE(PRFILE, 2050)
       END IF
 
       IF (FOUND_UNIT) THEN
          DO IUNIT = 1, CTPRHH
             IF (L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT) > 0) THEN
                IF (SAME_UNIT_DEFN) THEN
                   WRITE(PRFILE, 2010)               &
                   IUNIT                             &
                  ,L_FSBEN(1, KIST)%IPER(IUNIT)            &
                  ,L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT)   &
                  ,BASE_PARTIC(IUNIT, kist)                &
                  ,PARTIC(IUNIT)                     &
                  ,L_FSUSIZE(1, KIST)%IPER(IUNIT)          &
                  ,L_FSUSIZE(REFORM_IDX, KIST)%IPER(IUNIT) &
                  ,BASE_POVRAT(IUNIT, kist)                &
                  ,FSPOVRAT(IUNIT)
                ELSE  ! only show reform information
                   WRITE(PRFILE, 2040)                &
                   IUNIT                              &
                  ,L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT)    &
                  ,PARTIC(IUNIT)                      &
                  ,L_FSUSIZE(REFORM_IDX, KIST)%IPER(IUNIT)  &
                  ,FSPOVRAT(IUNIT)
                END IF
             END IF
          END DO
       ELSE
          WRITE(PRFILE, 1020) 'NO UNITS ARE ELIGIBLE IN REFORM, SO TABLE 2 NOT CALLED'
       END IF
    ELSE
       WRITE(PRFILE, 2030)
       IF (FOUND_UNIT) THEN
          DO IUNIT = 1, CTPRHH
             IF (L_FSBEN(1, KIST)%IPER(IUNIT) > 0) THEN
                WRITE(PRFILE, 2040)       &
                IUNIT                     &
               ,L_FSBEN(1, KIST)%IPER(IUNIT)    &
               ,BASE_PARTIC(IUNIT, kist)        &
               ,L_FSUSIZE(1, KIST)%IPER(IUNIT)  &
               ,BASE_POVRAT(IUNIT, kist)
             END IF
          END DO
       ELSE
          WRITE(PRFILE, 1020) 'NO UNITS ARE ELIGIBLE IN BASELAW, SO TABLE 2 NOT CALLED'
       END IF
    END IF

    !---------------------------------------------------------------------------------
    !---- table 3 debug --------------------------------------------------------------
    !---------------------------------------------------------------------------
    NUMLINES = CTPRHH + 7  
    CALL ISNEWPG(PRFILE, NUMLINES)
 
    IF (BASELAW(NTH) > ' ') THEN
       IF (SAME_UNIT_DEFN) THEN   ! title line
          WRITE(PRFILE, 3000)
       ELSE
          WRITE(PRFILE, 3035)
       END IF
 
       IF (FOUND_UNIT) THEN
          DO IUNIT = 1, CTPRHH
             IF (L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT) > 0) THEN
                IF (SAME_UNIT_DEFN) THEN  ! show baselaw and reform
                   WRITE(PRFILE, 3010)                  &
                   IUNIT                                &
                  ,L_FSBEN(1, KIST)%IPER(IUNIT)               &
                  ,L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT)      &
                  ,BASE_PARTIC(IUNIT, kist)                   &
                  ,PARTIC(IUNIT)                        &
                  ,L_FSGRINC(1, KIST)          %IPER(IUNIT)   &
                  ,L_FSGRINC(REFORM_IDX, KIST)%IPER(IUNIT)   &
                  ,L_FSNETINC(1, KIST)         %IPER(IUNIT)   &
                  ,L_FSNETINC(REFORM_IDX, KIST)%IPER(IUNIT)
                ELSE   ! only show reform information
                   WRITE(PRFILE, 3040)                 &
                   IUNIT                               &
                  ,L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT)     &
                  ,PARTIC(IUNIT)                       &
                  ,L_FSGRINC(REFORM_IDX, KIST)%IPER(IUNIT)   &
                  ,L_FSNETINC(REFORM_IDX, KIST)%IPER(IUNIT)
                END IF
             END IF
          END DO
       ELSE
          WRITE(PRFILE, 1020) 'NO UNITS ARE ELIGIBLE IN REFORM, SO TABLE 3 NOT CALLED'
       END IF
    ELSE
       WRITE(PRFILE, 3030)
       IF (FOUND_UNIT) THEN
          DO IUNIT = 1, CTPRHH
             IF (L_FSBEN(1, KIST)%IPER(IUNIT) > 0) THEN
                WRITE(PRFILE, 3040)           &
                IUNIT                         &
               ,L_FSBEN(1, KIST)%IPER(IUNIT)        &
               ,BASE_PARTIC(IUNIT, kist)            &
               ,L_FSGRINC(1, KIST)%IPER(IUNIT)      &
               ,L_FSNETINC(1, KIST)%IPER(IUNIT)
             END IF
          END DO
       ELSE
          WRITE(PRFILE, 1020) 'NO UNITS ARE ELIGIBLE IN BASELAW, SO TABLE 3 NOT CALLED'
       END IF
    END IF
 
    !---- table 3 debug (continued)
    NUMLINES = CTPRHH + 8  
    CALL ISNEWPG(PRFILE, NUMLINES)
 
    IF (BASELAW(NTH) > ' ') THEN
       IF (SAME_UNIT_DEFN) THEN   ! title line
          WRITE(PRFILE, 3050)
       ELSE
          WRITE(PRFILE, 3075)
       END IF
 
       IF (FOUND_UNIT) THEN
          DO IUNIT = 1, CTPRHH
             IF (L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT) > 0) THEN
                IF (SAME_UNIT_DEFN) THEN  ! show baselaw and reform
                   WRITE(PRFILE, 3060)      &
                   IUNIT                    &
                  ,BASE_HAS_ELDER(IUNIT, kist)    &
                  ,HAS_ELDER(IUNIT)         &
                  ,BASE_HAS_ELDDIS(IUNIT, kist)   &
                  ,HAS_ELDDIS(IUNIT)        &
                  ,BASE_HAS_KIDS(IUNIT, kist)     &
                  ,HAS_KIDS(IUNIT)          &
                  ,BASE_HAS_K5TO17(IUNIT, kist)   &
                  ,HAS_K5TO17(IUNIT)
                ELSE   ! only show reform information
                   WRITE(PRFILE, 3080)  &
                   IUNIT                &
                  ,HAS_ELDER(IUNIT)     &
                  ,HAS_ELDDIS(IUNIT)    &
                  ,HAS_KIDS(IUNIT)      &
                  ,HAS_K5TO17(IUNIT)
                END IF
             END IF
          END DO
       ELSE
          WRITE(PRFILE, 1020) 'NO UNITS ARE ELIGIBLE IN REFORM, SO TABLE 3 NOT CALLED'
       END IF
    ELSE
       WRITE(PRFILE, 3070)
       IF (FOUND_UNIT) THEN
          DO IUNIT = 1, CTPRHH
             IF (L_FSBEN(1, KIST)%IPER(IUNIT) > 0) THEN
                WRITE(PRFILE, 3080)   &
                IUNIT                 &
               ,BASE_HAS_ELDER(IUNIT, kist) &
               ,BASE_HAS_ELDDIS(IUNIT, kist)&
               ,BASE_HAS_KIDS(IUNIT, kist)  &
               ,BASE_HAS_K5TO17(IUNIT, kist)
             END IF
          END DO
       ELSE
          WRITE(PRFILE, 1020) 'NO UNITS ARE ELIGIBLE IN BASELAW, SO TABLE 3 NOT CALLED'
       END IF
    END IF
 
    !---- table 3 debug (continued)
    NUMLINES = CTPRHH + 8  
    CALL ISNEWPG(PRFILE, NUMLINES)
 
    IF (BASELAW(NTH) > ' ') THEN
       IF (SAME_UNIT_DEFN) THEN   ! title line
          WRITE(PRFILE, 3090)
       ELSE
          WRITE(PRFILE, 3115)
       END IF
 
       IF (FOUND_UNIT) THEN
          DO IUNIT = 1, CTPRHH
             IF (L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT) > 0) THEN
                IF (SAME_UNIT_DEFN) THEN  ! show baselaw and reform
                   WRITE(PRFILE, 3100)  &
                   IUNIT                &
                  ,BASE_HAS_EARN(IUNIT, kist) &
                  ,HAS_EARN(IUNIT)      &
                  ,BASE_HAS_MIN(IUNIT, kist)  &
                  ,HAS_MIN(IUNIT)       &
                  ,BASE_HAS_0NET(IUNIT, kist) &
                  ,HAS_0NET(IUNIT)
                ELSE   ! only show reform information
                   WRITE(PRFILE, 3120) &
                   IUNIT               &
                  ,HAS_EARN(IUNIT)     &
                  ,HAS_MIN(IUNIT)      &
                  ,HAS_0NET(IUNIT)
                END IF
             END IF
          END DO
       ELSE
          WRITE(PRFILE, 1020) 'NO UNITS ARE ELIGIBLE IN REFORM, SO TABLE 3 NOT CALLED'
       END IF
    ELSE
       WRITE(PRFILE, 3110)
       IF (FOUND_UNIT) THEN
          DO IUNIT = 1, CTPRHH
             IF (L_FSBEN(1, KIST)%IPER(IUNIT) > 0) THEN
                WRITE(PRFILE, 3120)  &
                IUNIT                &
               ,BASE_HAS_EARN(IUNIT, kist) &
               ,BASE_HAS_MIN(IUNIT, kist)  &
               ,BASE_HAS_0NET(IUNIT, kist)
             END IF
          END DO
       ELSE
          WRITE(PRFILE, 1020) 'NO UNITS ARE ELIGIBLE IN BASELAW, SO TABLE 3 NOT CALLED'
       END IF
    END IF

    !--------------------------------------------------------------------------------------
    !---- table 4 debug (welfare status)
    !--------------------------------------------------------------------------------------
    NUMLINES = CTPRHH + 8  
    CALL ISNEWPG(PRFILE, NUMLINES)
 
    IF (BASELAW(NTH) > ' ') THEN
       IF (SAME_UNIT_DEFN) THEN   ! title line
          WRITE(PRFILE, 4000)
       ELSE
          WRITE(PRFILE, 4035)
       END IF
 
       IF (FOUND_UNIT) THEN
          DO IUNIT = 1, CTPRHH
             IF (L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT) > 0) THEN
                IF (SAME_UNIT_DEFN) THEN  ! show baselaw and reform
                   WRITE(PRFILE, 4010)  &
                   IUNIT                &
                  ,BASE_HAS_TANF(IUNIT, kist) &
                  ,HAS_TANF(IUNIT)      &
                  ,BASE_HAS_GA(IUNIT, kist)   &
                  ,HAS_GA(IUNIT)        &
                  ,BASE_HAS_SSI(IUNIT, kist)  &
                  ,HAS_SSI(IUNIT)       &
                  ,BASE_ALLPA(IUNIT, kist)    &
                  ,ALLPA(IUNIT)
                ELSE
                   WRITE(PRFILE, 4040)  &
                   IUNIT                &
                  ,HAS_TANF(IUNIT)      &
                  ,HAS_GA(IUNIT)        &
                  ,HAS_SSI(IUNIT)       &
                  ,ALLPA(IUNIT)
                END IF
             END IF
          END DO
       ELSE
          WRITE(PRFILE, 1020)  'NO UNITS ARE ELIGIBLE IN REFORM, SO TABLE 4 NOT CALLED'
       END IF
    ELSE
       WRITE(PRFILE, 4030)
       IF (FOUND_UNIT) THEN
          DO IUNIT = 1, CTPRHH
             IF (L_FSBEN(1, KIST)%IPER(IUNIT) > 0) THEN
                WRITE(PRFILE, 4040)  &
                IUNIT                &
               ,BASE_HAS_TANF(IUNIT, kist) &
               ,BASE_HAS_GA(IUNIT, kist)   &
               ,BASE_HAS_SSI(IUNIT, kist)  &
               ,BASE_ALLPA(IUNIT, kist)
             END IF
          END DO
       ELSE
          WRITE(PRFILE, 1020)  'NO UNITS ARE ELIGIBLE IN BASELAW, SO TABLE 4 NOT CALLED'
       END IF
    END IF

    !---------------------------------------------------------------------------
    !------- table 5 debug (deductions)
    !---------------------------------------------------------------------------
    NUMLINES = CTPRHH + 7  
    CALL ISNEWPG(PRFILE, NUMLINES)
 
    IF (BASELAW(NTH) > ' ') THEN
       IF (SAME_UNIT_DEFN) THEN   ! title line
          WRITE(PRFILE, 5000)
       ELSE
          WRITE(PRFILE, 5035)
       END IF
 
       IF (FOUND_UNIT) THEN
          DO IUNIT = 1, CTPRHH
             IF (L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT) > 0) THEN
                IF (SAME_UNIT_DEFN) THEN  ! show baselaw and reform
                   WRITE(PRFILE, 5010)                  &
                   IUNIT                                &
                  ,L_FSERNDED(1, KIST)%IPER(IUNIT)            &
                  ,L_FSERNDED(REFORM_IDX, KIST)%IPER(IUNIT)   &
                  ,L_FSDEPDED(1, KIST)%IPER(IUNIT)            &
                  ,L_FSDEPDED(REFORM_IDX, KIST)%IPER(IUNIT)   &
                  ,L_FSMEDDED(1, KIST)%IPER(IUNIT)            &
                  ,L_FSMEDDED(REFORM_IDX, KIST)%IPER(IUNIT)   &
                  ,L_FSSLTDED(1, KIST)%IPER(IUNIT)            &
                  ,L_FSSLTDED(REFORM_IDX, KIST)%IPER(IUNIT)
                ELSE   ! only show reform information
                   WRITE(PRFILE, 5040)                 &
                   IUNIT                               &
                  ,L_FSERNDED(REFORM_IDX, KIST)%IPER(IUNIT) &
                  ,L_FSDEPDED(REFORM_IDX, KIST)%IPER(IUNIT) &
                  ,L_FSMEDDED(REFORM_IDX, KIST)%IPER(IUNIT) &
                  ,L_FSSLTDED(REFORM_IDX, KIST)%IPER(IUNIT)
                END IF
             END IF
          END DO
       ELSE
          WRITE(PRFILE, 1020)  'NO UNITS ARE ELIGIBLE IN REFORM, SO TABLE 5 NOT CALLED'
       END IF
    ELSE
       WRITE(PRFILE, 5030)
       IF (FOUND_UNIT) THEN
          DO IUNIT = 1, CTPRHH
             IF (L_FSBEN(1, KIST)%IPER(IUNIT) > 0) THEN
                WRITE(PRFILE, 5040)       &
                IUNIT                     &
               ,L_FSERNDED(1, KIST)%IPER(IUNIT) &
               ,L_FSDEPDED(1, KIST)%IPER(IUNIT) &
               ,L_FSMEDDED(1, KIST)%IPER(IUNIT) &
               ,L_FSSLTDED(1, KIST)%IPER(IUNIT)
             END IF
          END DO
       ELSE
          WRITE(PRFILE, 1020)  'NO UNITS ARE ELIGIBLE IN BASELAW, SO TABLE 5 NOT CALLED'
       END IF
    END IF
 
    !---- table 5 debug (continued)
    NUMLINES = CTPRHH + 7  
    CALL ISNEWPG(PRFILE, NUMLINES)
 
    IF (BASELAW(NTH) > ' ') THEN
       IF (SAME_UNIT_DEFN) THEN   ! title line
          WRITE(PRFILE, 5050)
       ELSE
          WRITE(PRFILE, 5075)
       END IF
 
       IF (FOUND_UNIT) THEN
          DO IUNIT = 1, CTPRHH
             IF (L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT) > 0) THEN
                IF (SAME_UNIT_DEFN) THEN  ! show baselaw and reform
                   WRITE(PRFILE, 5060)                   &
                   IUNIT                                 &
                  ,L_FSGRINC(1, KIST)%IPER(IUNIT)              &
                  ,L_FSGRINC(REFORM_IDX, KIST)%IPER(IUNIT)     &
                  ,L_FSSTDDED(1, KIST)%IPER(IUNIT)             &
                  ,L_FSSTDDED(REFORM_IDX, KIST)%IPER(IUNIT)    &
                  ,L_FSTOTDED(1, KIST)%IPER(IUNIT)             &
                  ,L_FSTOTDED(REFORM_IDX, KIST)%IPER(IUNIT)
                ELSE   ! only show reform information
                   WRITE(PRFILE, 5080)                   &
                   IUNIT                                 &
                  ,L_FSGRINC(REFORM_IDX, KIST)%IPER(IUNIT)    &
                  ,L_FSSTDDED(REFORM_IDX, KIST)%IPER(IUNIT)   &
                  ,L_FSTOTDED(REFORM_IDX, KIST)%IPER(IUNIT)
                END IF
             END IF
          END DO
       ELSE
          WRITE(PRFILE, 1020) 'NO UNITS ARE ELIGIBLE IN REFORM, SO TABLE 5 NOT CALLED'
       END IF
    ELSE
       WRITE(PRFILE, 5070)
       IF (FOUND_UNIT) THEN
          DO IUNIT = 1, CTPRHH
             IF (L_FSBEN(1, KIST)%IPER(IUNIT) > 0) THEN
                WRITE(PRFILE, 5080)      &
                IUNIT                    &
               ,L_FSGRINC(1, KIST)%IPER(IUNIT) &
               ,L_FSSTDDED(1, KIST)%IPER(IUNIT)&
               ,L_FSTOTDED(1, KIST)%IPER(IUNIT)
             END IF
          END DO
       ELSE
          WRITE(PRFILE, 1020) 'NO UNITS ARE ELIGIBLE IN BASELAW, SO TABLE 5 NOT CALLED'
       END IF
    END IF
 
 
    !--------------------------------------------------------------------------------------
    !---- table 6 debug (gainer/loser tables) for one unit per household
    !--------------------------------------------------------------------------------------
    IF (GLUNIT == 1 .AND. BASELAW(NTH) > ' ') THEN
 
       NUMLINES = 8
       CALL ISNEWPG(PRFILE, NUMLINES)
       WRITE (PRFILE, 6000)
 
       IUNIT = 1  ! for glunit = 1, there is only one unit
 
       IF (GL_BASE_PARTIC(1, kist) .OR. GL_PARTIC(1)) THEN
          WRITE(PRFILE, 6010)   &
          IUNIT                 &
         ,REGION                &
         ,GL_BASE_FSUSIZE (1, kist)   &
         ,GL_BASE_POVRAT  (1, kist)   &
         ,HH_BASE_FSBEN(kist)         &
         ,HH_FSBEN                    &
         ,GL_BASE_PARTIC  (1, kist)   &
         ,GL_PARTIC       (1)
       ELSE
          WRITE(PRFILE, 1020) 'NO UNITS PARTICIPATE IN BASELAW OR REFORM, SO TABLE 6 NOT CALLED'
       END IF
 
       NUMLINES = 8
       CALL ISNEWPG(PRFILE, NUMLINES)
       WRITE(PRFILE, 6030)
 
       IF (GL_BASE_PARTIC(1, kist) .OR. GL_PARTIC(1)) THEN
          WRITE(PRFILE, 6040)    &
          IUNIT                  &
         ,GL_BASE_HAS_EARN   (1, kist) &
         ,GL_BASE_HAS_ELDER  (1, kist) &
         ,GL_BASE_HAS_ELDDIS (1, kist) &
         ,GL_BASE_HAS_KIDS   (1, kist) &
         ,GL_BASE_HAS_TANF_GA(1, kist) &
         ,GL_BASE_HAS_SNGMOM (1, kist)
       ELSE
          WRITE(PRFILE, 1020) 'NO UNITS PARTICIPATE IN BASELAW OR REFORM, SO TABLE 6 NOT CALLED'
       END IF
 
    ELSE IF (GLUNIT == 2 .AND. BASELAW(NTH) > ' ') THEN
    
       !---- table 6 debug (gainer/loser tables) for one unit per household
 
       NUMLINES = CTPRHH + 8 
       CALL ISNEWPG(PRFILE, NUMLINES)
       WRITE(PRFILE, 6005)
 
       FOUND_UNIT = .FALSE.
       DO IUNIT = 1, CTPRHH
          IF (GL_BASE_PARTIC(IUNIT, kist) .OR. GL_PARTIC(IUNIT)) THEN
             FOUND_UNIT = .TRUE.
             WRITE(PRFILE, 6010)      &
             IUNIT                    &
            ,REGION                   &
            ,GL_BASE_FSUSIZE (IUNIT, kist)  &
            ,GL_BASE_POVRAT  (IUNIT, kist)  &
            ,GL_BASE_FSBEN   (IUNIT, kist)  &
            ,GL_FSBEN        (IUNIT)  &
            ,GL_BASE_PARTIC  (IUNIT, kist)  &
            ,GL_PARTIC       (IUNIT)
          END IF
       END DO

       IF (.NOT. FOUND_UNIT) THEN
          WRITE(PRFILE, 1020) 'NO UNITS PARTICIPATE IN BASELAW OR REFORM, SO TABLE 6 NOT CALLED'
       END IF
 
       NUMLINES = CTPRHH + 8 
       CALL ISNEWPG(PRFILE, NUMLINES)
       WRITE(PRFILE, 6030)
 
       DO IUNIT = 1, CTPRHH
          IF (GL_BASE_PARTIC(IUNIT, kist) .OR. GL_PARTIC(IUNIT)) THEN
             WRITE(PRFILE, 6040)          &
             IUNIT                        &
            ,GL_BASE_HAS_EARN   (IUNIT, kist)   &
            ,GL_BASE_HAS_ELDER  (IUNIT, kist)   &
            ,GL_BASE_HAS_ELDDIS (IUNIT, kist)   &
            ,GL_BASE_HAS_KIDS   (IUNIT, kist)   &
            ,GL_BASE_HAS_TANF_GA(IUNIT, kist)   &
            ,GL_BASE_HAS_SNGMOM (IUNIT, kist)
          END IF
       END DO
       IF (.NOT. FOUND_UNIT) THEN
          WRITE(PRFILE, 1020) 'NO UNITS PARTICIPATE IN BASELAW OR REFORM, SO TABLE 6 NOT CALLED'
       END IF
 
    END IF ! end of GLUNIT clause
 
    RETURN
!--------------------------------------------------------------------
! FORMAT STATEMENTS
!--------------------------------------------------------------------
1000  FORMAT(                                              &
        //,131('-')                                        &
        // 1X, 'IN THIS SECTION WE ARE DISPLAYING HOW EA'  &
              , 'CH FOOD STAMP UNIT IS TABULATED IN THE  ' &
        / 1X, 'SUMMARY AND GAINER/LOSER TABLES.'           &
        //1X, 'DEBUG FOR TABLE 1 (COMPARISON TABLE): '     &
        //1X, '     '                                      &
           ,2X, '  BASE', 2X, 'REFORM'                     &
           ,2X, '  BASE', 2X, 'REFORM'                     &
           ,2X, '  BASE', 2X, 'REFORM'                     &
         /1X, ' FSUN'                                      &
           ,2X, '   BEN', 2X, '   BEN'                     &
           ,2X, '  PART', 2X, '  PART'                     &
           ,2X, '  SIZE', 2X, '  SIZE'                     &
         /1X, '-----'                                      &
           ,2X, '------', 2X, '------'                     &
           ,2X, '------', 2X, '------'                     &
           ,2X, '------', 2X, '------'                     &
        )
 
1010  FORMAT(1X, I5, 2(2X, I6), 2(2X, L6), 2(2X, I6))
 
1020  FORMAT(1X, A)
                                                         
1030 FORMAT(/                                         &
         /1X, 'DEBUG FOR TABLE 1 (COMPARISON TABLE): '&
        //1X, '     '                                 &
           ,2X, '  BASE'                              &
           ,2X, '  BASE'                              &
           ,2X, '  BASE'                              &
         /1X, ' FSUN'                                 &
           ,2X, '   BEN'                              &
           ,2X, '  PART'                              &
           ,2X, '  SIZE'                              &
         /1X, '-----'                                 &
           ,2X, '------'                              &
           ,2X, '------'                              &
           ,2X, '------'                              &
        )
1040  FORMAT(1X, I5, 1(2X, I6), 1(2X, L6), 1(2X, I6))
                                                         
1050 FORMAT(/                                         &
         /1X, 'DEBUG FOR TABLE 1 (COMPARISON TABLE): '&
        //1X, '     '                                 &
           ,2X, 'REFORM'                              &
           ,2X, 'REFORM'                              &
           ,2X, 'REFORM'                              &
         /1X, ' FSUN'                                 &
           ,2X, '   BEN'                              &
           ,2X, '  PART'                              &
           ,2X, '  SIZE'                              &
         /1X, '-----'                                 &
           ,2X, '------'                              &
           ,2X, '------'                              &
           ,2X, '------'                              &
        )
                                                                     
2000 FORMAT(/                                                     &
         /1X, 'DEBUG FOR TABLE 2 (ELIGIBLES/PARTICIPANTS TABLE): '&
        //1X, '     '                                             &
           ,2X, '  BASE', 2X, 'REFORM'                            &
           ,2X, '  BASE', 2X, 'REFORM'                            &
           ,2X, '  BASE', 2X, 'REFORM'                            &
           ,2X, '  BASE', 2X, 'REFORM'                            &
         /1X, ' FSUN'                                             &
           ,2X, '   BEN', 2X, '   BEN'                            &
           ,2X, '  PART', 2X, '  PART'                            &
           ,2X, '  SIZE', 2X, '  SIZE'                            &
           ,2X, 'POVRAT', 2X, 'POVRAT'                            &
         /1X, '-----'                                             &
           ,2X, '------', 2X, '------'                            &
           ,2X, '------', 2X, '------'                            &
           ,2X, '------', 2X, '------'                            &
           ,2X, '------', 2X, '------'                            &
        )
 
2010  FORMAT(1X, I5, 2(2X, I6), 2(2X, L6), 2(2X, I6), 2(2X, F6.2))
 
2030 FORMAT(/  &
         /1X, 'DEBUG FOR TABLE 2 (ELIGIBLES/PARTICIPANTS TABLE): '  &
        //1X, '     '      &
           ,2X, '  BASE'   &
           ,2X, '  BASE'   &
           ,2X, '  BASE'   &
           ,2X, '  BASE'   &
         /1X, ' FSUN'      &
           ,2X, '   BEN'   &
           ,2X, '  PART'   &
           ,2X, '  SIZE'   &
           ,2X, 'POVRAT'   &
         /1X, '-----'      &
           ,2X, '------'   &
           ,2X, '------'   &
           ,2X, '------'   &
           ,2X, '------'   &
        ) 
 
2040  FORMAT(1X, I5, 1(2X, I6), 1(2X, L6), 1(2X, I6), 1(2X, F6.2))
 
2050  FORMAT(/ &
         /1X, 'DEBUG FOR TABLE 2 (ELIGIBLES/PARTICIPANTS TABLE): ' &
        //1X, '     '     &
           ,2X, 'REFORM'  &
           ,2X, 'REFORM'  &
           ,2X, 'REFORM'  &
           ,2X, 'REFORM'  &
         /1X, ' FSUN'     &
           ,2X, '   BEN'  &
           ,2X, '  PART'  &
           ,2X, '  SIZE'  &
           ,2X, 'POVRAT'  &
         /1X, '-----'     &
           ,2X, '------'  &
           ,2X, '------'  &
           ,2X, '------'  &
           ,2X, '------'  &
        )            

3000 FORMAT(/                                              &
         /1X, 'DEBUG FOR TABLE 3 (CHARACTERISTICS TABLE): '&
        //1X, '     '                                      &
           ,2X, '  BASE', 2X, 'REFORM'                     &
           ,2X, '  BASE', 2X, 'REFORM'                     &
           ,2X, '  BASE', 2X, 'REFORM'                     &
           ,2X, '  BASE', 2X, 'REFORM'                     &
         /1X, ' FSUN'                                      &
           ,2X, '   BEN', 2X, '   BEN'                     &
           ,2X, '  PART', 2X, '  PART'                     &
           ,2X, ' GRINC', 2X, ' GRINC'                     &
           ,2X, 'NETINC', 2X, 'NETINC'                     &
         /1X, '-----'                                      &
           ,2X, '------', 2X, '------'                     &
           ,2X, '------', 2X, '------'                     &
           ,2X, '------', 2X, '------'                     &
           ,2X, '------', 2X, '------'                     &
        )
 
3010  FORMAT(1X, I5, 2(2X, I6), 2(2X, L6), 4(2X, I6))
                                                            
3030 FORMAT(/                                              &
         /1X, 'DEBUG FOR TABLE 3 (CHARACTERISTICS TABLE): '&
        //1X, '     '                                      &
           ,2X, '  BASE'                                   &
           ,2X, '  BASE'                                   &
           ,2X, '  BASE'                                   &
           ,2X, '  BASE'                                   &
         /1X, ' FSUN'                                      &
           ,2X, '   BEN'                                   &
           ,2X, '  PART'                                   &
           ,2X, ' GRINC'                                   &
           ,2X, 'NETINC'                                   &
         /1X, '-----'                                      &
           ,2X, '------'                                   &
           ,2X, '------'                                   &
           ,2X, '------'                                   &
           ,2X, '------'                                   &
        )
 
3035 FORMAT(/                                              & 
         /1X, 'DEBUG FOR TABLE 3 (CHARACTERISTICS TABLE): '&
        //1X, '     '                                      &
           ,2X, 'REFORM'                                   &
           ,2X, 'REFORM'                                   &
           ,2X, 'REFORM'                                   &
           ,2X, 'REFORM'                                   &
         /1X, ' FSUN'                                      &
           ,2X, '   BEN'                                   &
           ,2X, '  PART'                                   &
           ,2X, ' GRINC'                                   &
           ,2X, 'NETINC'                                   &
         /1X, '-----'                                      &
           ,2X, '------'                                   &
           ,2X, '------'                                   &
           ,2X, '------'                                   &
           ,2X, '------'                                   &
        )
 
3040  FORMAT(1X, I5, 1(2X, I6), 1(2X, L6), 4(2X, I6))
                                                 
3050 FORMAT(/                                   &
         /1X, 'DEBUG FOR TABLE 3 (CONTINUED): ' &
        //1X, '     '                           &
           ,2X, '  BASE', 2X, 'REFORM'          &
           ,2X, '  BASE', 2X, 'REFORM'          &
           ,2X, '  BASE', 2X, 'REFORM'          &
           ,2X, '  BASE', 2X, 'REFORM'          &
         /1X, '     '                           &
           ,2X, '   HAS', 2X, '   HAS'          &
           ,2X, '   HAS', 2X, '   HAS'          &
           ,2X, '   HAS', 2X, '   HAS'          &
           ,2X, '   HAS', 2X, '   HAS'          &
         /1X, ' FSUN'                           &
           ,2X, ' ELDER', 2X, ' ELDER'          &
           ,2X, 'ELDDIS', 2X, 'ELDDIS'          &
           ,2X, '  KIDS', 2X, '  KIDS'          &
           ,2X, 'K5TO17', 2X, 'K5TO17'          &
         /1X, '-----'                           &
           ,2X, '------', 2X, '------'          &
           ,2X, '------', 2X, '------'          &
           ,2X, '------', 2X, '------'          &
           ,2X, '------', 2X, '------'          &
        )
 
3060  FORMAT(1X, I5, 8(2X, L6))
                                                 
3070 FORMAT(/                                  &
         /1X, 'DEBUG FOR TABLE 3 (CONTINUED): '&
        //1X, '     '                          &
           ,2X, '  BASE'                       &
           ,2X, '  BASE'                       &
           ,2X, '  BASE'                       &
           ,2X, '  BASE'                       &
         /1X, '     '                          &
           ,2X, '   HAS'                       &
           ,2X, '   HAS'                       &
           ,2X, '   HAS'                       &
           ,2X, '   HAS'                       &
         /1X, ' FSUN'                          &
           ,2X, ' ELDER'                       &
           ,2X, 'ELDDIS'                       &
           ,2X, '  KIDS'                       &
           ,2X, 'K5TO17'                       &
         /1X, '-----'                          &
           ,2X, '------'                       &
           ,2X, '------'                       &
           ,2X, '------'                       &
           ,2X, '------'                       &
        )
                                                  
3075 FORMAT(/                                   &
         /1X, 'DEBUG FOR TABLE 3 (CONTINUED): ' &
        //1X, '     '                           &
           ,2X, 'REFORM'                        &
           ,2X, 'REFORM'                        &
           ,2X, 'REFORM'                        &
           ,2X, 'REFORM'                        &
         /1X, '     '                           &
           ,2X, '   HAS'                        &
           ,2X, '   HAS'                        &
           ,2X, '   HAS'                        &
           ,2X, '   HAS'                        &
         /1X, ' FSUN'                           &
           ,2X, ' ELDER'                        &
           ,2X, 'ELDDIS'                        &
           ,2X, '  KIDS'                        &
           ,2X, 'K5TO17'                        &
         /1X, '-----'                           &
           ,2X, '------'                        &
           ,2X, '------'                        &
           ,2X, '------'                        &
           ,2X, '------'                        &
        )
 
3080  FORMAT(1X, I5, 4(2X, L6))
 
3090 FORMAT(/                                  & 
         /1X, 'DEBUG FOR TABLE 3 (CONTINUED): '&
        //1X, '     '                          &
           ,2X, '  BASE', 2X, 'REFORM'         &
           ,2X, '  BASE', 2X, 'REFORM'         &
           ,2X, '  BASE', 2X, 'REFORM'         &
         /1X, '     '                          &
           ,2X, '   HAS', 2X, '   HAS'         &
           ,2X, '   HAS', 2X, '   HAS'         &
           ,2X, '   HAS', 2X, '   HAS'         &
         /1X, ' FSUN'                          &
           ,2X, '  EARN', 2X, '  EARN'         &
           ,2X, 'MINBEN', 2X, 'MINBEN'         &
           ,2X, ' 0 NET', 2X, ' 0 NET'         &
         /1X, '-----'                          &
           ,2X, '------', 2X, '------'         &
           ,2X, '------', 2X, '------'         &
           ,2X, '------', 2X, '------'         &
        ) 
 
3100  FORMAT(1X, I5, 6(2X, L6))
                                                
3110 FORMAT(/                                  &
         /1X, 'DEBUG FOR TABLE 3 (CONTINUED): '&
        //1X, '     '                          &
           ,2X, '  BASE'                       &
           ,2X, '  BASE'                       &
           ,2X, '  BASE'                       &
         /1X, '     '                          &
           ,2X, '   HAS'                       &
           ,2X, '   HAS'                       &
           ,2X, '   HAS'                       &
         /1X, ' FSUN'                          &
           ,2X, '  EARN'                       &
           ,2X, 'MINBEN'                       &
           ,2X, ' 0 NET'                       &
         /1X, '-----'                          &
           ,2X, '------'                       &
           ,2X, '------'                       &
           ,2X, '------'                       &
        )
 
3115 FORMAT(/                                  & 
         /1X, 'DEBUG FOR TABLE 3 (CONTINUED): '&
        //1X, '     '                          &
           ,2X, 'REFORM'                       &
           ,2X, 'REFORM'                       &
           ,2X, 'REFORM'                       &
         /1X, '     '                          &
           ,2X, '   HAS'                       &
           ,2X, '   HAS'                       &
           ,2X, '   HAS'                       &
         /1X, ' FSUN'                          &
           ,2X, '  EARN'                       &
           ,2X, 'MINBEN'                       &
           ,2X, ' 0 NET'                       &
         /1X, '-----'                          &
           ,2X, '------'                       &
           ,2X, '------'                       &
           ,2X, '------'                       &
        )
 
3120  FORMAT(1X, I5, 3(2X, L6))
                                                    
4000 FORMAT(/                                      & 
         /1X, 'DEBUG FOR TABLE 4 (WELFARE STATUS):'& 
        //1X, '     '                              & 
           ,2X, '  BASE', 2X, 'REFORM'             & 
           ,2X, '  BASE', 2X, 'REFORM'             & 
           ,2X, '  BASE', 2X, 'REFORM'             & 
           ,2X, '  BASE', 2X, 'REFORM'             & 
         /1X, '     '                              & 
           ,2X, '   HAS', 2X, '   HAS'             & 
           ,2X, '   HAS', 2X, '   HAS'             & 
           ,2X, '   HAS', 2X, '   HAS'             & 
           ,2X, '   HAS', 2X, '   HAS'             & 
         /1X, ' FSUN'                              & 
           ,2X, '  TANF', 2X, '  TANF'             & 
           ,2X, '    GA', 2X, '    GA'             &
           ,2X, '   SSI', 2X, '   SSI'             & 
           ,2X, ' ALLPA', 2X, ' ALLPA'             & 
         /1X, '-----'                              & 
           ,2X, '------', 2X, '------'             & 
           ,2X, '------', 2X, '------'             &
           ,2X, '------', 2X, '------'             &
           ,2X, '------', 2X, '------'             &
        )
 
4010  FORMAT(1X, I5, 8(2X, L6))
                                                    
4030 FORMAT(/                                      &
         /1X, 'DEBUG FOR TABLE 4 (WELFARE STATUS):'&
        //1X, '     '                              &
           ,2X, '  BASE'                           &
           ,2X, '  BASE'                           &
           ,2X, '  BASE'                           &
           ,2X, '  BASE'                           &
         /1X, '     '                              &
           ,2X, '   HAS'                           &
           ,2X, '   HAS'                           &
           ,2X, '   HAS'                           &
           ,2X, '   HAS'                           &
         /1X, ' FSUN'                              &
           ,2X, '  TANF'                           &
           ,2X, '    GA'                           &
           ,2X, '   SSI'                           &
           ,2X, ' ALLPA'                           &
         /1X, '-----'                              &
           ,2X, '------'                           &
           ,2X, '------'                           &
           ,2X, '------'                           &
           ,2X, '------'                           &
        )
                                                    
4035 FORMAT(/                                      &
         /1X, 'DEBUG FOR TABLE 4 (WELFARE STATUS):'&
        //1X, '     '                              &
           ,2X, 'REFORM'                           &
           ,2X, 'REFORM'                           &
           ,2X, 'REFORM'                           &
           ,2X, 'REFORM'                           &
         /1X, '     '                              &
           ,2X, '   HAS'                           &
           ,2X, '   HAS'                           &
           ,2X, '   HAS'                           &
           ,2X, '   HAS'                           &
         /1X, ' FSUN'                              &
           ,2X, '  TANF'                           &
           ,2X, '    GA'                           &
           ,2X, '   SSI'                           &
           ,2X, ' ALLPA'                           &
         /1X, '-----'                              &
           ,2X, '------'                           &
           ,2X, '------'                           &
           ,2X, '------'                           &
           ,2X, '------'                           &
        )
 
4040  FORMAT(1X, I5, 4(2X, L6))
                                                 
5000 FORMAT(/                                   &
         /1X, 'DEBUG FOR TABLE 5 (DEDUCTIONS): '&
        //1X, '     '                           &
           ,2X, '  BASE', 2X, 'REFORM'          &
           ,2X, '  BASE', 2X, 'REFORM'          &
           ,2X, '  BASE', 2X, 'REFORM'          &
           ,2X, '  BASE', 2X, 'REFORM'          &
         /1X, ' FSUN'                           &
           ,2X, 'ERNDED', 2X, 'ERNDED'          &
           ,2X, 'DEPDED', 2X, 'DEPDED'          &
           ,2X, 'MEDDED', 2X, 'MEDDED'          &
           ,2X, 'SLTDED', 2X, 'SLTDED'          &
         /1X, '-----'                           &
           ,2X, '------', 2X, '------'          &
           ,2X, '------', 2X, '------'          &
           ,2X, '------', 2X, '------'          &
           ,2X, '------', 2X, '------'          &
        )
 
5010  FORMAT(1X, I5, 8(2X, I6))
                                                 
5030 FORMAT(/                                   &
         /1X, 'DEBUG FOR TABLE 5 (DEDUCTIONS): '&
        //1X, '     '                           &
           ,2X, '  BASE'                        &
           ,2X, '  BASE'                        &
           ,2X, '  BASE'                        &
           ,2X, '  BASE'                        &
         /1X, ' FSUN'                           &
           ,2X, 'ERNDED'                        &
           ,2X, 'DEPDED'                        &
           ,2X, 'MEDDED'                        &
           ,2X, 'SLTDED'                        &
         /1X, '-----'                           &
           ,2X, '------'                        &
           ,2X, '------'                        &
           ,2X, '------'                        &
           ,2X, '------'                        &
        )
                                                  
5035 FORMAT(/                                    &
         /1X, 'DEBUG FOR TABLE 5 (DEDUCTIONS): ' &
        //1X, '     '                            &
           ,2X, 'REFORM'                         &
           ,2X, 'REFORM'                         &
           ,2X, 'REFORM'                         &
           ,2X, 'REFORM'                         &
         /1X, ' FSUN'                            &
           ,2X, 'ERNDED'                         &
           ,2X, 'DEPDED'                         &
           ,2X, 'MEDDED'                         &
           ,2X, 'SLTDED'                         &
         /1X, '-----'                            &
           ,2X, '------'                         &
           ,2X, '------'                         &
           ,2X, '------'                         &
           ,2X, '------'                         &
        )
 
5040  FORMAT(1X, I5, 4(2X, I6))
                                                 
5050 FORMAT(/                                   &
         /1X, 'DEBUG FOR TABLE 5 (CONTINUED): ' &
        //1X, '     '                           &
           ,2X, '  BASE', 2X, 'REFORM'          &
           ,2X, '  BASE', 2X, 'REFORM'          &
           ,2X, '  BASE', 2X, 'REFORM'          &
         /1X, ' FSUN'                           &
           ,2X, ' GRINC', 2X, ' GRINC'          &
           ,2X, 'STDDED', 2X, 'STDDED'          &
           ,2X, 'TOTDED', 2X, 'TOTDED'          &
         /1X, '-----'                           &
           ,2X, '------', 2X, '------'          &
           ,2X, '------', 2X, '------'          &
           ,2X, '------', 2X, '------'          &
        )
 
5060  FORMAT(1X, I5, 6(2X, I6))
                                                
5070 FORMAT(/                                  &
         /1X, 'DEBUG FOR TABLE 5 (CONTINUED): '&
        //1X, '     '                          &
           ,2X, '  BASE'                       &
           ,2X, '  BASE'                       &
           ,2X, '  BASE'                       &
         /1X, ' FSUN'                          &
           ,2X, ' GRINC'                       &
           ,2X, 'STDDED'                       &
           ,2X, 'TOTDED'                       &
         /1X, '-----'                          &
           ,2X, '------'                       &
           ,2X, '------'                       &
           ,2X, '------'                       &
        )
                                                
5075 FORMAT(/                                  &
         /1X, 'DEBUG FOR TABLE 5 (CONTINUED): '&
        //1X, '     '                          &
           ,2X, 'REFORM'                       &
           ,2X, 'REFORM'                       &
           ,2X, 'REFORM'                       &
         /1X, ' FSUN'                          &
           ,2X, ' GRINC'                       &
           ,2X, 'STDDED'                       &
           ,2X, 'TOTDED'                       &
         /1X, '-----'                          &
           ,2X, '------'                       &
           ,2X, '------'                       &
           ,2X, '------'                       &
        )
 
5080  FORMAT(1X, I5, 3(2X, I6))
                                                   
6000 FORMAT(/                                     &
         /1X, 'DEBUG FOR TABLE 6 (GAINER/LOSER): '&
        //1X, '     ' , 2X, '      '              &
           ,2X, '  BASE', 2X, '  BASE'            &
           ,2X, '  BASE', 2X, 'REFORM'            &
           ,2X, '  BASE', 2X, 'REFORM'            &
         /1X, ' FSUN', 2X,  'REGION'              &
           ,2X, '  SIZE', 2X, 'POVRAT'            &
           ,2X, ' HHBEN', 2X, ' HHBEN'            &
           ,2X, '  PART', 2X, '  PART'            &
         /1X, '-----',  2X, '------'              &
           ,2X, '------', 2X, '------'            &
           ,2X, '------', 2X, '------'            &
           ,2X, '------', 2X, '------'            &
        )
                                                   
6005 FORMAT(/                                     &  
         /1X, 'DEBUG FOR TABLE 6 (GAINER/LOSER): '&
        //1X, '     ' , 2X, '      '              &
           ,2X, '      ', 2X, '      '            &
           ,2X, '  BASE', 2X, 'REFORM'            &
           ,2X, '  BASE', 2X, 'REFORM'            &
         /1X, ' FSUN', 2X,  'REGION'              &
           ,2X, '  SIZE', 2X, 'POVRAT'            &
           ,2X, '   BEN', 2X, '   BEN'            &
           ,2X, '  PART', 2X, '  PART'            &
         /1X, '-----',  2X, '------'              &
           ,2X, '------', 2X, '------'            &
           ,2X, '------', 2X, '------'            &
           ,2X, '------', 2X, '------'            &
        )
 
6010  FORMAT(1X, I5, 2(2X, I6), 2X, F6.2, 2(2X, I6), 2(2X, L6))
 
6030 FORMAT(/                                  & 
         /1X, 'DEBUG FOR TABLE 6 (CONTINUED): '&
        //1X, '     '                          &
           ,2X, '  BASE', 2X, '  BASE'         &
           ,2X, '  BASE', 2X, '  BASE'         &
           ,2X, '  BASE', 2X, '  BASE'         &
         /1X, '     '                          &
           ,2X, '   HAS', 2X, '   HAS'         &
           ,2X, '   HAS', 2X, '   HAS'         &
           ,2X, '   HAS', 2X, '   HAS'         &
         /1X, ' FSUN'                          &
           ,2X, '  EARN', 2X, ' ELDER'         &
           ,2X, 'ELDDIS', 2X, '  KIDS'         &
           ,2X, 'TANFGA', 2X, 'SNGMOM'         &
         /1X, '-----'                          &
           ,2X, '------', 2X, '------'         &
           ,2X, '------', 2X, '------'         &
           ,2X, '------', 2X, '------'         &
        )
 
6040  FORMAT(1X, I5, 6(2X, L6))
 
    END
