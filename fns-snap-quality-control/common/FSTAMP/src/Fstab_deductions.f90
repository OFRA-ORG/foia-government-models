!**************************************************************************************************
! Source File:  FSTAB5.F90                  
! Called By:    FS_TABLES                   
!
! TABLE 5 -  Deductions of Eligible and Participating Food Stamp Units
!
!**************************************************************************************************
      SUBROUTINE FS_TAB_deductions (&
                FSDEPDED,   &  ! CHILD CARE DEDUCTION
                FSERNDED,   &  ! EARNINGS DEDUCTION
                KTH,        &  ! PLAN NUMBER
                PARTIC,     &  ! LOGICAL FOR PARTICIPATION
                FSMEDDED,   &  ! MEDICAL DEDUCTION
                FSSTDDED,   &  ! STANDARD DEDUCTION
                FSSLTDED,   &  ! SHELTER DEDUCTION
                FSTOTDED,   &  ! TOTAL DEDUCTION
                WGT         )  ! WEIGHT
      USE GLOBAL
      USE FSWORK, ONLY: SHOW_ELIG, PLANNAME_TABLE, PLANNBR_TABLE, T5NOTE_NUM
      use utils
      
      IMPLICIT NONE
      INTEGER, PARAMETER :: MAX_KTH = 5
      !---- Declare parameters from calling program
      INTEGER, intent(in)  ::        FSDEPDED
      INTEGER, intent(in)  ::        FSERNDED
      INTEGER, intent(in)  ::        KTH
      LOGICAL, intent(in)  ::        PARTIC
      INTEGER, intent(in)  ::        FSMEDDED
      INTEGER, intent(in)  ::        FSSTDDED
      INTEGER, intent(in)  ::        FSSLTDED
      INTEGER, intent(in)  ::        FSTOTDED
      REAL, intent(in)  ::           WGT
      
!---- Variables for tables
      INTEGER  ::  II,  I, J, K, ITH, PASS, NBR_OF_KTHS =0
      INTEGER, PARAMETER :: NROWS=6
      REAL  ::           X

!---- Report lines for table results
      CHARACTER (6) :: CROW1(12) =' '  ! Character detail line 1 variables
      CHARACTER (6) :: CROW2(12) =' '  ! Character detail line 2 variables
      CHARACTER (6) :: CROW3(12) =' '  ! Character detail line 3 variables

!---- Row labels for both tables
      CHARACTER(18) :: ROWLAB(6) =(/&
      'Avg Standard      ',         &
      'Avg Earnings      ',         &
      'Avg Medical       ',         &
      'Avg Dependent Care',         &
      'Avg Shelter       ',         &
      'Avg Total         '         /)

!---- Column labels must be initialized to blanks for each potential plan
      CHARACTER (16) :: PLANNUMS(MAX_KTH + 1) = ' '
      CHARACTER (16) :: C2(MAX_KTH + 1) = ' '
      CHARACTER (17) :: C3(MAX_KTH + 1) = ' '
      CHARACTER (16) :: C4(MAX_KTH + 1) = ' '


      ! dimension by deduction type for avgs
      REAL ::  ELIG_TOT_UNITS(NROWS, MAX_KTH + 1) =  0.0            !! Eligibles
      REAL ::  PART_TOT_UNITS(NROWS, MAX_KTH + 1) =  0.0            !! Participants

!---- Total units by deduction and KTH
      REAL ::  ELIG_DED_UNITS(NROWS,MAX_KTH + 1) =   0.0      !! Eligibles
      REAL ::  PART_DED_UNITS(NROWS,MAX_KTH + 1) =   0.0      !! Participants

!---- Total deduction amt by deduction and KTH
      REAL ::  ELIG_TOT_DED_AMT(NROWS,MAX_KTH + 1) =   0.0    !! Eligibles
      REAL ::  PART_TOT_DED_AMT(NROWS,MAX_KTH + 1) =   0.0    !! Participants

!---- Average deduction amt by deduction, KTH, and denominator
      REAL ::  ELIG_AVG_DED_AMT(NROWS,MAX_KTH + 1,2) =   0.0  !! Eligibles
      REAL ::  PART_AVG_DED_AMT(NROWS,MAX_KTH + 1,2) =   0.0  !! Participants

!---- Percent of units with deduction
      REAL ::  ELIG_DED_PCT(NROWS,MAX_KTH + 1) =   0.0        !! Eligibles
      REAL ::  PART_DED_PCT(NROWS,MAX_KTH + 1) =   0.0        !! Participants

!---- Percent change from baseline
      REAL ::  ELIG_CHG_PCT(NROWS,MAX_KTH + 1,2) =   0.0      !! Eligibles
      REAL ::  PART_CHG_PCT(NROWS,MAX_KTH + 1,2) =   0.0      !! Participants

!---- By-pass calculations if print requested

      IF (KEOF == 3) GOTO 900
!****************************************************************
!     Perform table calculations
!****************************************************************
      IF (NTH==1) THEN
         plannums(1) = '     BASELAW    '
         C2(1)       = '----------------'
         C3(1)       = 'All    Units with'
         C4(1)       = 'Units  Deduction'
      ENDIF

!---- Keep track of highest KTH computed
      IF (KTH > MAX_KTH+1  )  RETURN  !--- Table5 supports a maximum of 5 reforms
      IF (KTH > NBR_OF_KTHS)  NBR_OF_KTHS = KTH

!>>   ELIG_TOT_UNITS(KTH) = ELIG_TOT_UNITS(KTH) + WGT
!>>   IF (PARTIC) PART_TOT_UNITS(KTH) = PART_TOT_UNITS(KTH) + WGT

!---- Standard deduction
      X = FSSTDDED
      IF (X .GE. 1.0) THEN
         ELIG_TOT_DED_AMT(1,KTH) = ELIG_TOT_DED_AMT(1,KTH) + X * WGT
         ELIG_DED_UNITS  (1,KTH) = ELIG_DED_UNITS  (1,KTH) + WGT
         IF (PARTIC) THEN
           PART_TOT_DED_AMT(1,KTH) = PART_TOT_DED_AMT(1,KTH) + X * WGT
           PART_DED_UNITS  (1,KTH) = PART_DED_UNITS  (1,KTH) + WGT
         ENDIF
      ENDIF
      IF (X >= 0.0) THEN
         ELIG_TOT_UNITS(1, KTH) = ELIG_TOT_UNITS(1, KTH) + WGT
         IF (PARTIC) PART_TOT_UNITS(1, KTH) = PART_TOT_UNITS(1, KTH) + WGT
      END IF


!---- Earnings deduction
      X = FSERNDED
      IF (X .GE. 1.0) THEN
         ELIG_TOT_DED_AMT(2,KTH) = ELIG_TOT_DED_AMT(2,KTH) + X * WGT
         ELIG_DED_UNITS  (2,KTH) = ELIG_DED_UNITS(2,KTH) + WGT
         IF (PARTIC) THEN
           PART_TOT_DED_AMT(2,KTH) = PART_TOT_DED_AMT(2,KTH) + X * WGT
           PART_DED_UNITS  (2,KTH) = PART_DED_UNITS  (2,KTH) + WGT
         ENDIF
      ENDIF
      IF (X >= 0.0) THEN
         ELIG_TOT_UNITS(2, KTH) = ELIG_TOT_UNITS(2, KTH) + WGT
         IF (PARTIC) PART_TOT_UNITS(2, KTH) = PART_TOT_UNITS(2, KTH) + WGT
      END IF

!---- Medical deduction
      X = FSMEDDED
      IF (X .GE. 1.0) THEN
         ELIG_TOT_DED_AMT(3,KTH) = ELIG_TOT_DED_AMT(3,KTH) + X * WGT
         ELIG_DED_UNITS  (3,KTH) = ELIG_DED_UNITS  (3,KTH) + WGT
         IF (PARTIC) THEN
           PART_TOT_DED_AMT(3,KTH) = PART_TOT_DED_AMT(3,KTH) + X * WGT
           PART_DED_UNITS  (3,KTH) = PART_DED_UNITS  (3,KTH) + WGT
         ENDIF
      ENDIF
      IF (X >= 0.0) THEN
         ELIG_TOT_UNITS(3, KTH) = ELIG_TOT_UNITS(3, KTH) + WGT
         IF (PARTIC) PART_TOT_UNITS(3, KTH) = PART_TOT_UNITS(3, KTH) + WGT
      END IF


!---- Child care deduction
      X = FSDEPDED
      IF (X .GE. 1.0) THEN
         ELIG_TOT_DED_AMT(4,KTH) = ELIG_TOT_DED_AMT(4,KTH) + X * WGT
         ELIG_DED_UNITS  (4,KTH) = ELIG_DED_UNITS  (4,KTH) + WGT
         IF (PARTIC) THEN
           PART_TOT_DED_AMT(4,KTH) = PART_TOT_DED_AMT(4,KTH) + X * WGT
           PART_DED_UNITS  (4,KTH) = PART_DED_UNITS  (4,KTH) + WGT
         ENDIF
      ENDIF
      IF (X >= 0.0) THEN
         ELIG_TOT_UNITS(4, KTH) = ELIG_TOT_UNITS(4, KTH) + WGT
         IF (PARTIC) PART_TOT_UNITS(4, KTH) = PART_TOT_UNITS(4, KTH) + WGT
      END IF

!---- Shelter deduction
      X = FSSLTDED
      IF (X .GE. 1.0) THEN
         ELIG_TOT_DED_AMT(5,KTH) = ELIG_TOT_DED_AMT(5,KTH) + X * WGT
         ELIG_DED_UNITS  (5,KTH) = ELIG_DED_UNITS  (5,KTH) + WGT
         IF (PARTIC) THEN
           PART_TOT_DED_AMT(5,KTH) = PART_TOT_DED_AMT(5,KTH) + X * WGT
           PART_DED_UNITS  (5,KTH) = PART_DED_UNITS  (5,KTH) + WGT
         ENDIF
      ENDIF
      IF (X >= 0.0) THEN
         ELIG_TOT_UNITS(5, KTH) = ELIG_TOT_UNITS(5, KTH) + WGT
         IF (PARTIC) PART_TOT_UNITS(5, KTH) = PART_TOT_UNITS(5, KTH) + WGT
      END IF


!---- Total deductions
      X = FSTOTDED
      IF (X .GE. 1.0) THEN
         ELIG_TOT_DED_AMT(6,KTH) = ELIG_TOT_DED_AMT(6,KTH) + X * WGT
         ELIG_DED_UNITS  (6,KTH) = ELIG_DED_UNITS(6,KTH) + WGT
         IF (PARTIC) THEN
           PART_TOT_DED_AMT(6,KTH) = PART_TOT_DED_AMT(6,KTH) + X * WGT
           PART_DED_UNITS  (6,KTH) = PART_DED_UNITS  (6,KTH) + WGT
         ENDIF
      ENDIF
      IF (X >= 0.0) THEN
         ELIG_TOT_UNITS(6, KTH) = ELIG_TOT_UNITS(6, KTH) + WGT
         IF (PARTIC) PART_TOT_UNITS(6, KTH) = PART_TOT_UNITS(6, KTH) + WGT
      END IF



      RETURN
!******************************************************************
!     Print the table
!******************************************************************
  900 CONTINUE
!---- Final calculations
      DO ITH = 1,NBR_OF_KTHS  ! OVER EACH PLAN
        DO I = 1,NROWS    ! OVER EACH DEDUCTION

!---- Average deduction for all units
!>>       ELIG_AVG_DED_AMT(I,ITH,1) =  ELIG_TOT_DED_AMT(I,ITH) / ELIG_TOT_UNITS(ITH)
!>>       IF (PART_TOT_UNITS(ITH) .NE. 0) &
!>>           PART_AVG_DED_AMT(I,ITH,1) =PART_TOT_DED_AMT(I,ITH) / PART_TOT_UNITS(ITH)
          IF (abs(ELIG_TOT_UNITS(6, ITH)) > 0.0) &
             ELIG_AVG_DED_AMT(I,ITH,1) =  ELIG_TOT_DED_AMT(I,ITH) / ELIG_TOT_UNITS(6, ITH)
          IF (abs(PART_TOT_UNITS(6, ITH)) > 0.0) &
              PART_AVG_DED_AMT(I,ITH,1) = PART_TOT_DED_AMT(I,ITH) / PART_TOT_UNITS(6, ITH)

!---- Average deduction for units with deduction
          IF (abs(ELIG_TOT_DED_AMT(I,ITH)) > 0.0) &
              ELIG_AVG_DED_AMT(I,ITH,2) =   &
              ELIG_TOT_DED_AMT(I,ITH) / ELIG_DED_UNITS(I,ITH)
          IF (abs(PART_TOT_DED_AMT(I,ITH)) > 0.0) &
              PART_AVG_DED_AMT(I,ITH,2) =       &
              PART_TOT_DED_AMT(I,ITH) / PART_DED_UNITS(I,ITH)

!---- Percentage of units with deduction
!>>       ELIG_DED_PCT(I,ITH) = 100 *  ELIG_DED_UNITS(I,ITH) / ELIG_TOT_UNITS(ITH)
!>>       IF (PART_TOT_UNITS(ITH) .NE. 0)  &
!>>           PART_DED_PCT(I,ITH) = 100 * PART_DED_UNITS(I,ITH) / PART_TOT_UNITS(ITH)
          IF (abs(ELIG_TOT_UNITS(I, ITH)) > 0)  &
             ELIG_DED_PCT(I,ITH) = 100 * ELIG_DED_UNITS(I,ITH) / ELIG_TOT_UNITS(I, ITH)
          IF (abs(PART_TOT_UNITS(I, ITH)) > 0)  &
             PART_DED_PCT(I,ITH) = 100 * PART_DED_UNITS(I,ITH) / PART_TOT_UNITS(I, ITH)

          IF (ITH .NE. 1) THEN
!---- Change in average deduction
            DO J = 1,2
              IF (ELIG_AVG_DED_AMT(I,1,J) > 0 ) &
                  ELIG_CHG_PCT(I,ITH,J) = 100 * &
                 (ELIG_AVG_DED_AMT(I,ITH,J) / ELIG_AVG_DED_AMT(I,1,J) - 1.0)
              IF (PART_AVG_DED_AMT(I,1,J) > 0 ) &
                  PART_CHG_PCT(I,ITH,J) = 100 *   &
                 (PART_AVG_DED_AMT(I,ITH,J) / PART_AVG_DED_AMT(I,1,J) - 1.0)
            ENDDO

            PLANNUMS(ITH) = 'PLAN ' // PLANNBR_TABLE(ITH-1)
            CALL REMOVE_BLANKS(PLANNUMS(ITH))
            CALL center_text(PLANNUMS(ITH), 16)

            C2(ITH) = '----------------'
            C3(ITH) = 'All    Units with'
            C4(ITH) = 'Units  Deduction'
          ELSE
            PLANNUMS(1) = 'BASELAW'
            CALL CENTER_TEXT(PLANNUMS(1), 16)
          ENDIF
        ENDDO  ! DO I = 1,NROWS
      ENDDO    ! DO ITH = 1,NBR_OF_KTHS


!---- Print the table for eligibles (PASS=1)
!---- Print the table for participants (PASS=2)

      DO PASS = 1, 2

        IF (PASS == 1 .AND. .NOT. SHOW_ELIG  ) CYCLE

        CALL ISNEWPG(TABFILE, PAGE_BREAK_NUMLINES + 1)

        IF (PASS == 1) THEN
          WRITE(TABFILE,1101)   ! E title
        ELSE IF (PASS == 2) THEN
          WRITE(TABFILE,1102)   ! P title
        END IF

        WRITE (TABFILE,1110) (PLANNUMS(I),I= 1, MAX_KTH + 1) &
         ,(C2(I),I=1,MAX_KTH + 1)                            &
         ,(C3(I),I=1,MAX_KTH + 1)                            &
         ,(C4(I),I=1,MAX_KTH + 1)

!---- Move appropriate values to report lines
        DO II = 1, NROWS ! Do for each deduction type
          I = 0
          DO J=1, NBR_OF_KTHS ! Do for each plan
            IF (PASS == 1) THEN ! Eligibles
              I = I + 1
              WRITE(CROW1(I),FMT='(F6.1)') ELIG_AVG_DED_AMT(II,J,1)
              IF (J == 1) THEN ! Baselaw -- NA
                CROW2(I) = '   NA'
              ELSE
                WRITE(CROW2(I),FMT='(F6.1)') ELIG_CHG_PCT(II,J,1)
              ENDIF
              I = I + 1
              WRITE(CROW1(I),FMT='(F6.1)') ELIG_AVG_DED_AMT(II,J,2)
              IF (J == 1) THEN ! Baselaw -- NA
                CROW2(I) = '   NA'
              ELSE
                WRITE(CROW2(I),FMT='(F6.1)') ELIG_CHG_PCT(II,J,2)
              ENDIF
              WRITE(CROW3(I),FMT='(F6.1)') ELIG_DED_PCT(II,J)
            ELSE ! Participants
              I = I + 1
              WRITE(CROW1(I),FMT='(F6.1)') PART_AVG_DED_AMT(II,J,1)
              IF (J == 1) THEN ! Baselaw -- NA
                CROW2(I) = '   NA'
              ELSE
                WRITE(CROW2(I),FMT='(F6.1)') PART_CHG_PCT(II,J,1)
              ENDIF
              I = I + 1
              WRITE(CROW1(I),FMT='(F6.1)') PART_AVG_DED_AMT(II,J,2)
              IF (J == 1) THEN
                CROW2(I) = '   NA'
              ELSE
                WRITE(CROW2(I),FMT='(F6.1)') PART_CHG_PCT(II,J,2)
              ENDIF
              WRITE(CROW3(I),FMT='(F6.1)') PART_DED_PCT(II,J)
            ENDIF
          ENDDO
          WRITE(TABFILE,1120)ROWLAB(II), &
          (CROW1(K),K=1,12),             &
          (CROW2(K),K=1,12),             &
          (CROW3(K),K=1,12)
        ENDDO

        WRITE (TABFILE,1260) ! underline

        SELECT CASE(T5NOTE_NUM(1))
          CASE(1)
            WRITE (TABFILE,1271) 'BASELAW:'
          CASE(2)
            WRITE (TABFILE,1272) 'BASELAW:'
          CASE(3)
            WRITE (TABFILE,1273)
        END SELECT

!---- Print reform plan descriptions and deduction notes
        DO I = 1, NBR_OF_KTHS - 1
          WRITE(TABFILE,1275) PLANNBR_TABLE(I), PLANNAME_TABLE(I)
          SELECT CASE(T5NOTE_NUM(I+1))
           CASE(1)
             WRITE (TABFILE,1271) '        '
           CASE(2)
             WRITE (TABFILE,1272) '        '
          END SELECT
        END DO

      ENDDO ! PASS LOOP


      RETURN

!----------------------------------------------------------------------
! FORMAT STATEMENTS
!----------------------------------------------------------------------
1101 FORMAT( T63,'        ' //  &
             T50,'DEDUCTIONS OF ELIGIBLE SNAP UNITS' //)

1102 FORMAT( T63,'        ' //  &
             T50,'DEDUCTIONS OF PARTICIPATING SNAP UNITS'//)
1110 FORMAT(/           &
        1X, 131('-')  / &
        T21,6(A16,3X) / &
        T21,6(A16,3X) / &
        T21,6(A17,2X) / &
        T21,6(A16,3X) / &
        1X,131('-')   /)
1120  FORMAT( &
          1X, A18,T21,6(2(A6,3X),1X),/,              &
          1X, '% Chg from Baselaw ',6(2(A6,3X),1X),/,&
          1X, '% Units with Deduct',6(2(A6,3X),1X),/ )
1260  FORMAT(/ 1X, 131('-')/                                         &
     /1X, 'NOTE: For valid baselaw/reform comparisons, use the same '&
        , 'value of the parameter DEDTYPE for baselaw and reform '   &
     /1X, 'simulations.  See notes below to determine the DEDTYPE '  &
        , 'value used in each simulation.'/)
1271 FORMAT(1X, A, 3X  &
       ,'Deductions tabulations use the marginal effective deduction amounts (DEDTYPE=1).'  )

1272 FORMAT(1X, A, 3X &
     , 'Deductions tabulations use the entitled deduction amounts (DEDTYPE = 2).')

1273  FORMAT(1X, 'BASELAW:', 3X &
     &      ,'Check the baselaw simulation output file to determine the DEDTYPE value that was used.')

1275  FORMAT(/,1X,'PLAN ',A,':  ',A70)

      END
      