!**************************************************************************************************
! Source File:  FSPARM.F90                  
! Called By:    FSTAMP1                     
!
! Reads and validates FSTAMP user parameters (keof=1).
!
!**************************************************************************************************
    SUBROUTINE FS_READPARM
    USE GLOBAL
    USE FSSIZES
    USE FSPARM
    USE GLOBPARM, ONLY : POVGUIDE
    USE FSWORK, ONLY : DEFL_GEN, DEFL_VEH
    use states, only: nstates


    IMPLICIT NONE

    INTEGER, PARAMETER :: &
         MIN_AGEDSCRN = 1 &
        ,MAX_AGEDSCRN = 3 &
        ,MIN_DEDTYPE  = 1 &
        ,MAX_DEDTYPE  = 2 &
        ,MIN_GLUNIT   = 1 &
        ,MAX_GLUNIT   = 2 &
        ,MIN_PUREPA   = 1 &
        ,MAX_PUREPA   = 4

    CHARACTER (80) ::  &
        BENMAX_RDFMT   &
       ,BENMAX_WRFMT   &
       ,BENMIN_RDFMT   &
       ,BENMIN_WRFMT   &
       ,DEPMAX_RDFMT   &
       ,SCREEN_RDFMT   &
       ,SCREEN_WRFMT   &
       ,STANDDED_RDFMT &
       ,STANDDED_WRFMT &
       ,STANDDED_WRFMT_TITLE


    CHARACTER (8) ::    VAR, CCODE8
    CHARACTER (2) ::    CHAR_NUM_COLS
    CHARACTER (1) ::    PREV_BASELAW  = ' '

    CHARACTER(70) ::    BENMAX_TITLE, SCREEN_TITLE
    CHARACTER(30) ::    BENMAX_SIZES(MAX_BENMAX_FSUSIZE + 1)    &
                       ,BENMIN_SIZES(MAX_BENMIN_FSUSIZE + 1)    &
                       ,SCREEN_SIZES  (MAX_SCREEN_FSUSIZE + 1)  &
                       ,STANDDED_SIZES(MAX_STANDDED_FSUSIZE)

    CHARACTER(70) :: STANDDED_TITLE
    CHARACTER(70) :: ASSETLIM_TITLE

    INTEGER            :: ICODE, I, J,  NUM_GENERIC_PARAMS_READ

    LOGICAL ::    MORE_PARAMETERS

!--------------------------------------------------------------------
! BEGIN PROCESSING
!--------------------------------------------------------------------
    NUMLINES = PAGE_BREAK_NUMLINES + 2  ! force a new page + 2 header lines
    CALL ISNEWPG(PRFILE, NUMLINES)

    WRITE(PRFILE,1000) NTH

    !---- some format statements depend on the size of the array


    WRITE(CHAR_NUM_COLS, '(I2)') NUM_BENMAX_REGION
    BENMAX_RDFMT = '(2X,A,2X,' // CHAR_NUM_COLS // 'F10.0, 5X, A)'
    BENMAX_WRFMT = '(1X,T5,A,' // CHAR_NUM_COLS // 'I10)'

    WRITE(CHAR_NUM_COLS, '(I2)') NUM_BENMIN_REGION
    BENMIN_RDFMT = '(2X,A,2X,' // CHAR_NUM_COLS // 'F10.0, 5X, A)'
    BENMIN_WRFMT = '(1X,T5,A,' // CHAR_NUM_COLS // 'I10)'


    WRITE(CHAR_NUM_COLS, '(I2)') NUM_DEPMAX_REGION
    DEPMAX_RDFMT = '(2X,A,2X,' // CHAR_NUM_COLS //   'F10.0)'

    WRITE(CHAR_NUM_COLS, '(I2)') NUM_SCREEN_REGION
    SCREEN_RDFMT = '(2X,A,2X,' // CHAR_NUM_COLS //  'F10.0, 5X, A)'
    SCREEN_WRFMT = '(1X,T5,A,' // CHAR_NUM_COLS //'I10)'

    WRITE(CHAR_NUM_COLS, '(I2)') NUM_STANDDED_REGION
    STANDDED_RDFMT = '(2X,A,2X,' // CHAR_NUM_COLS //  'F10.0, 5X, A)'
    STANDDED_WRFMT = '(1X,T5,A,T30,' // CHAR_NUM_COLS //  'I10, 5x, a)'
    STANDDED_WRFMT_TITLE = '(1X,T30,' // CHAR_NUM_COLS //   'A)'

    !---- read/validate MRVALUES ----
    READ(PRMFILE,1020,ERR=800) VAR, CCODE8, ICODE

    IF (VAR /= 'MRVALUES' .OR. CCODE8 /= 'FSTAMP  ') THEN
       CALL ERROR_MSG('FS_READPARM', 'PARAMETERS OUT OF ORDER, EXPECTING MRVALUES', ABORT)
    ELSE IF (ICODE /= NTH) THEN
       CALL ERROR_MSG('FS_READPARM', 'FSTAMP NTH <> ALTSEQ FSTAMP NTH', ABORT)
    ELSE IF (ICODE > MAX_NTH) THEN
       CALL ERROR_MSG('FS_READPARM', 'NTH > MAX_NTH -- TOO MANY FSTAMP REFORMS', ABORT)
       NTH = MAX_NTH
    END IF

    !---- read in deflation factors ----
    READ(PRMFILE,1290,ERR=800) var, DEFL_GEN
    READ(PRMFILE,1290,ERR=800) var, DEFL_VEH

    !------------------------------------------------------------------------
    !---- Top of loop for reading FSTAMP parameters
    !------------------------------------------------------------------------
    MORE_PARAMETERS = .TRUE.
    NUM_GENERIC_PARAMS_READ = 0

    DO WHILE (MORE_PARAMETERS)

      READ(PRMFILE, 1010) VAR
      BACKSPACE(PRMFILE)
      NUM_GENERIC_PARAMS_READ =  NUM_GENERIC_PARAMS_READ + 1

      SELECT CASE (VAR)

       CASE ('ENDPARMS','MRVALUES')

         MORE_PARAMETERS = .FALSE.
         NUM_GENERIC_PARAMS_READ =  NUM_GENERIC_PARAMS_READ - 1


       CASE ('AGEDSCRN')

         READ(PRMFILE,1100,ERR=800) VAR, AGEDSCRN(NTH)

         IF (    AGEDSCRN(NTH) < MIN_AGEDSCRN &
            .OR. AGEDSCRN(NTH) > MAX_AGEDSCRN)&
            CALL ERROR_MSG('FS_READPARM', 'AGEDSCRN DOES NOT HAVE A VALID VALUE, SEE USERS GUIDE', ABORT)

       CASE ('APROCSTA')

         READ(prmfile, 1100, ERR=800) var, icode
         IF (icode < 0 .OR. icode > nstates) &
            CALL ERROR_MSG('FS_READPARM', 'APROCSTA DOES NOT HAVE A VALID VALUE, SEE USERS GUIDE', ABORT)
         aprocsta(icode, nth) = .TRUE.

       CASE ('ASSETLIM')

          READ(PRMFILE,1550,ERR=800) VAR, ASSETLIM_TITLE

          DO I = 1, nstates
             READ(PRMFILE,1122,ERR=800) VAR, (ASSETLIM(I, J, NTH), J = 1, MAX_ASSETLIM)
             IF (VAR /= 'ASSETLIM') THEN
               CALL ERROR_MSG('FS_READPARM', 'PARAMETERS OUT OF ORDER, EXPECTING ASSETLIM', ABORT)
             END IF

             do j = 1, max_assetlim
                ASSETLIM(I, J, NTH) = REAL(NINT(ASSETLIM(I, J, NTH) * defl_gen))
             end do

          END DO



       CASE ('BASELAW ')

         READ(PRMFILE,1240,ERR=800) VAR, BASELAW(NTH)

       CASE ('BENMAX  ')


         READ(PRMFILE,1550,ERR=800) VAR, BENMAX_TITLE

         !---- now read the BENMAX values
         DO I = 1, MAX_BENMAX_FSUSIZE + 1

           READ(PRMFILE,BENMAX_RDFMT,ERR=800) VAR    &
            ,(BENMAX(I,J,NTH), J=1, NUM_BENMAX_REGION), BENMAX_SIZES(I)

            do j = 1,  NUM_BENMAX_REGION
               BENMAX(I,J,NTH) = REAL(NINT(BENMAX(I,J,NTH) * defl_gen))
            end do

           IF (VAR /= 'BENMAX  ') &
             CALL ERROR_MSG('FS_READPARM','PARAMETERS OUT OF ORDER, EXPECTING BENMAX' ,ABORT)
         END DO


       CASE ('BENMIN  ')

         READ(PRMFILE,1550,ERR=800) VAR, BENMAX_TITLE

         DO I = 1, MAX_BENMIN_FSUSIZE

           READ(PRMFILE,BENMIN_RDFMT,ERR=800) VAR    &
            ,(BENMIN(I,J,NTH), J=1, NUM_BENMIN_REGION), BENMIN_SIZES(I)

           do j = 1, NUM_BENMIN_REGION
              BENMIN (I, J, NTH) = REAL(NINT(BENMIN(I, J, NTH) * defl_gen))
           END DO

           IF (VAR /= 'BENMIN  ') THEN
             ABEND_REASON ='PARAMETERS OUT OF ORDER, EXPECTING BENMIN'
             CALL ERROR_MSG('FS_READPARM', ABEND_REASON, ABORT)
           END IF

         END DO


       CASE ('BENMULT ')

         READ(PRMFILE,1280,ERR=800) VAR, BENMULT(NTH)

       CASE ('BRR     ')
         READ(PRMFILE,1280,ERR=800) VAR, BRR(NTH)

       CASE ('DEDTYPE ')

         READ(PRMFILE,1100,ERR=800) VAR, DEDTYPE(NTH)

         IF (   DEDTYPE(NTH) < MIN_DEDTYPE  &
           .OR. DEDTYPE(NTH) > MAX_DEDTYPE) &
           CALL ERROR_MSG('FS_READPARM', 'DEDTYPE DOES NOT HAVE A VALID VALUE, SEE USERS GUIDE', ABORT)

       CASE ('DEPMAX')

         READ(PRMFILE,*) ! label line
         DO I = 1, MAX_DEPMAX_AGE
           READ(PRMFILE,DEPMAX_RDFMT,ERR=800) VAR &
             ,(DEPMAX(I,J,NTH), J=1, NUM_DEPMAX_REGION)

           DO J = 1, NUM_DEPMAX_REGION
              DEPMAX(I,J,NTH) = REAL(NINT(DEPMAX(I,J,NTH) * defl_gen))
           end do

           IF (VAR /= 'DEPMAX  ') &
              CALL ERROR_MSG('FS_READPARM', 'PARAMETERS OUT OF ORDER, EXPECTING DEPMAX',ABORT)

         END DO

       CASE ('EARNDED ')

         READ(PRMFILE,1280,ERR=800) VAR, EARNDED(NTH)


       CASE ('EARNMAX ')

         READ(PRMFILE,1100,ERR=800) VAR, EARNMAX(NTH)


       CASE ('FS_VARS ')

         READ(PRMFILE,1240,ERR=800) VAR, FS_VARS(NTH)


       CASE ('GLUNIT  ')

         READ(PRMFILE,1100,ERR=800) VAR, I

         IF (NTH == 1) THEN   !---- ONLY USE GLUNIT FROM FIRST NTH
            GLUNIT = I
            IF (GLUNIT < MIN_GLUNIT  .OR. GLUNIT > MAX_GLUNIT) &
                CALL ERROR_MSG('FS_READPARM', 'GLUNIT DOES NOT HAVE A VALID VALUE, SEE USERS GUIDE', ABORT)
         ENDIF

       CASE ('GRSMULT ')

         READ(PRMFILE,1280,ERR=800) VAR, GRSMULT(NTH)


       CASE ('GRSM_SEL')

         READ(PRMFILE,1100,ERR=800) VAR, GRSM_SEL(NTH)

       CASE ('GRSSCRN ')

         READ(PRMFILE,1550,ERR=800) VAR, SCREEN_TITLE

         DO I = 1, MAX_SCREEN_FSUSIZE + 1
            READ(PRMFILE,SCREEN_RDFMT,ERR=800) VAR     &
           ,(GRSSCRN(I,J,NTH), J=1, NUM_SCREEN_REGION) ,SCREEN_SIZES(I)

            DO  J = 1, NUM_SCREEN_REGION
               GRSSCRN(I,J,NTH) = REAL(NINT(GRSSCRN(I,J,NTH) * DEFL_GEN))
            END DO

            IF (VAR /= 'GRSSCRN ') &
               CALL ERROR_MSG('FS_READPARM',  'PARAMETERS OUT OF ORDER, EXPECTING GRSSCRN' , ABORT)
         END DO

       CASE ('MDTHRESH')

         READ(PRMFILE,1120,ERR=800) VAR, MDTHRESH(NTH)
         MDTHRESH(NTH) = REAL(NINT(MDTHRESH(NTH) * defl_gen))


       CASE ('NETMULT ')

         READ(PRMFILE,1280,ERR=800) VAR, NETMULT(NTH)

       CASE ('NETSCRN ')

         READ(PRMFILE,1550,ERR=800) VAR, SCREEN_TITLE

         DO I = 1, MAX_SCREEN_FSUSIZE + 1
            READ(PRMFILE,SCREEN_RDFMT,ERR=800) VAR     &
            ,(NETSCRN(I,J,NTH), J=1, NUM_SCREEN_REGION)  ,SCREEN_SIZES(I)

            DO J = 1, NUM_SCREEN_REGION
               NETSCRN(I,J,NTH) = REAL(NINT(NETSCRN(I,J,NTH) * defl_gen))
            END DO

            IF (VAR /= 'NETSCRN ') &
               CALL ERROR_MSG('FS_READPARM', 'PARAMETERS OUT OF ORDER, EXPECTING NETSCRN', ABORT)
         END DO

       CASE ('PLANNAME')

         READ(PRMFILE,1550,ERR=800) VAR, PLANNAME(NTH)

       CASE ('PLANNBR ')

         READ(PRMFILE,1250,ERR=800) VAR, PLANNBR(NTH)

       CASE ('PRLEVEL ')

         READ(PRMFILE,1100,ERR=800) VAR, PRLEVEL(NTH)

         IF (     PRLEVEL(NTH) < MIN_PRLEVEL  &
             .OR. PRLEVEL(NTH) > MAX_PRLEVEL) &
           CALL ERROR_MSG('FS_READPARM', 'PRLEVEL DOES NOT HAVE A VALID VALUE, SEE USERS GUIDE', ABORT)

       CASE ('PUREPA  ')

         READ(PRMFILE,1100,ERR=800) VAR, PUREPA(NTH)

         IF (     PUREPA(NTH) < MIN_PUREPA  &
             .OR. PUREPA(NTH) > MAX_PUREPA) &
           CALL ERROR_MSG('FS_READPARM', 'PUREPA DOES NOT HAVE A VALID VALUE, SEE USERS GUIDE', ABORT)


       CASE ('SHLCMULT')

         READ(PRMFILE,1280,ERR=800) VAR, SHLCMULT(NTH)

       CASE ('SHLTRPCT')

         READ(PRMFILE,1280,ERR=800) VAR, SHLTRPCT(NTH)


       CASE ('STANDDED')

         READ(PRMFILE,1550,ERR=800) VAR, STANDDED_TITLE

         DO I = 1, MAX_STANDDED_FSUSIZE
            READ(PRMFILE,STANDDED_RDFMT,ERR=800) VAR     &
            ,(STANDDED(I,J,NTH), J=1, NUM_STANDDED_REGION)  ,STANDDED_SIZES(I)

            DO J = 1, NUM_STANDDED_REGION
               STANDDED(I,J,NTH) = REAL(NINT(STANDDED(I,J,NTH) * DEFL_GEN))
            END DO

            IF (VAR /= 'STANDDED') &
               CALL ERROR_MSG('FS_READPARM', 'PARAMETERS OUT OF ORDER, EXPECTING STANDDED', ABORT)
         END DO


       CASE ('STUDAGE ')

         READ(PRMFILE,1100,ERR=800) VAR, STUDAGE(NTH)

       CASE DEFAULT  !------- parameter is not a generic parameter

         NUM_GENERIC_PARAMS_READ =  NUM_GENERIC_PARAMS_READ - 1

         CALL DB_FS_READPARM (VAR)

         IF (KERR .GE. ABORT) RETURN  ! stop processing if unknown parameter encountered

      END SELECT

    END DO  !--- loop over all FSTAMP parameters



 

    !--------------------------------------------------------------------
    ! INITIALIZE THE GLTABS PARAMETER
    !--------------------------------------------------------------------

    GLTABS(NTH) = .TRUE.

    !--------------------------------------------------------------------
    ! COMPLEX VALIDATION PROCESSING
    !--------------------------------------------------------------------

    !------ BASELAW parameter must not be blank for NTH > 1 - baselaw simulation
    !------ allowed only in NTH = 1
    IF (NTH > 1 .AND. BASELAW(NTH) == ' ') THEN
       CALL ERROR_MSG('FS_READPARM', 'BASELAW CAN ONLY BE SIMULATED AS NTH = 1, SEE USERS GUIDE', ABORT)
    END IF


    IF (BASELAW(NTH) /= ' ') THEN !--- Reform simulation checks----


        !------ BASELAW parameter must be the same for each reform simulation,
        !------ otherwise the g/l tables would be comparing apples and oranges.
        DO I = 0, nstates
           IF (APROCSTA(I, NTH) .and. .not. APROCSTA(I, 1)) THEN
              CALL ERROR_MSG('FS_READPARM', 'APROCSTA MUST BE THE SAME FOR EACH REFORM PLAN, SEE USERS GUIDE', ABORT)
           END IF
        END DO


        !------ BASELAW parameter must be the same for each reform simulation,
        !------ otherwise the g/l tables would be comparing apples and oranges.
        IF (PREV_BASELAW == ' ') THEN
            PREV_BASELAW = BASELAW(NTH)     ! first reform plan
        ELSE IF (BASELAW(NTH) /= PREV_BASELAW) THEN
           CALL ERROR_MSG('FS_READPARM', 'BASELAW PARAMETER MUST BE THE SAME FOR EACH REFORM PLAN, SEE USERS GUIDE', ABORT)
        END IF

    END IF

    !---- PLANNBR should be blank if this is a baselaw simulation
    IF (BASELAW(NTH) == ' ' .and. PLANNBR(NTH) /= '        ') &
          CALL ERROR_MSG('FS_READPARM', &
          'PLANNBR PARAMETER MUST BE BLANK, GIVEN BASELAW, SEE USERS GUIDE', ABORT)

    !---- PLANNBR should be non-blank if this is a reform simulation
    IF (BASELAW(NTH) /= ' ' .AND. PLANNBR(NTH) == ' ') &
          CALL ERROR_MSG('FS_READPARM', &
         'PLANNBR PARAMETER MUST BE NON-BLANK, GIVEN REFORM, SEE USERS GUIDE', ABORT)

    !---- PLANNAME should be non-blank if this is a baseline simulation
    IF (BASELAW(NTH) == ' '.AND. PLANNAME(NTH) == ' ') &
       CALL ERROR_MSG('FS_READPARM', &
       'PLANNAME PARAMETER MUST BE NON-BLANK, GIVEN REFORM, SEE USERS GUIDE', ABORT)


    !---- BRR can not be negative
    IF (BRR(NTH) < 0.0) &
       CALL ERROR_MSG('FS_READPARM', 'BRR PARAMETER MUST BE NON-NEGATIVE, SEE USERS GUIDE', ABORT)

    !---- BENMULT can not be negative
    IF (BENMULT(NTH) < 0.0) &
       CALL ERROR_MSG('FS_READPARM', 'BENMULT PARAMETER MUST BE NON-NEGATIVE, SEE USERS GUIDE', ABORT)

    !---- EARNDED can not be negative
    IF (EARNDED(NTH) < 0.0) &
       CALL ERROR_MSG('FS_READPARM', 'EARNDED PARAMETER MUST BE NON-NEGATIVE, SEE USERS GUIDE', ABORT)

    !---- GRSMULT can not be negative
    IF (GRSMULT(NTH) < 0.0) &
        CALL ERROR_MSG('FS_READPARM', 'GRSMULT PARAMETER MUST BE NON-NEGATIVE, SEE USERS GUIDE', ABORT)

    !---- NETMULT can not be negative
    IF (NETMULT(NTH) < 0.0) &
       CALL ERROR_MSG('FS_READPARM', 'NETMULT PARAMETER MUST BE NON-NEGATIVE, SEE USERS GUIDE', ABORT)

    !---- SHLCMULT can not be negative
    IF (SHLCMULT(NTH) < 0.0) &
       CALL ERROR_MSG('FS_READPARM', 'SHLCMULT PARAMETER MUST BE NON-NEGATIVE, SEE USERS GUIDE', ABORT)

    !---- SHLTRPCT can not be negative
    IF (SHLTRPCT(NTH) < 0.0)  &
       CALL ERROR_MSG('FS_READPARM', 'SHLTRPCT PARAMETER MUST BE NON-NEGATIVE, SEE USERS GUIDE', ABORT)

    !--------------------------------------------------------------------
    ! SHOW FSTAMP VALUES AFTER MULTIPLIERS ARE USED
    !--------------------------------------------------------------------
    IF (KERR == 0) THEN   ! no errors

       !---- Show BENMAX * BENMULT

       NUMLINES = MAX_BENMAX_FSUSIZE + 1 + 5
       CALL ISNEWPG(PRFILE, NUMLINES)
       WRITE(PRFILE, 2000) BENMAX_TITLE

       DO I = 1, MAX_BENMAX_FSUSIZE + 1
          WRITE(PRFILE,BENMAX_WRFMT)               &
           BENMAX_SIZES(I)                         &
          ,(NINT(BENMAX(I,J,NTH) * BENMULT(NTH))   &
                         ,J=1, NUM_BENMAX_REGION)
       END DO

       !---- Show GRSSCRN * GRSMULT

       NUMLINES = MAX_SCREEN_FSUSIZE + 1 + 6
       CALL ISNEWPG(PRFILE, NUMLINES)

       if (grsm_sel(nth) == 1) then
          WRITE(PRFILE, 3000) SCREEN_TITLE
       else
          WRITE(PRFILE, 3001) SCREEN_TITLE
       end if


       IF (GRSM_SEL(NTH) == 1) THEN
          DO I = 1, MAX_SCREEN_FSUSIZE + 1
             WRITE(PRFILE,SCREEN_WRFMT)              &
               SCREEN_SIZES(I)                       &
              ,(NINT(GRSSCRN(I,J,NTH) * GRSMULT(NTH))&
                             ,J=1, NUM_SCREEN_REGION)
          END DO
       ELSE
          DO I = 1, MAX_SCREEN_FSUSIZE + 1
             WRITE(PRFILE,SCREEN_WRFMT)              &
               SCREEN_SIZES(I)                       &
              ,(NINT(defl_gen * ceiling(povguide(I,J) / 12.0 * GRSMULT(NTH))), J=1, NUM_SCREEN_REGION)
          END DO
       END IF

       !---- Show NETSCRN * NETMULT

       NUMLINES = MAX_SCREEN_FSUSIZE + 1 + 6
       CALL ISNEWPG(PRFILE, NUMLINES)
       WRITE(PRFILE, 4000) SCREEN_TITLE

       DO I = 1, MAX_SCREEN_FSUSIZE + 1
          WRITE(PRFILE,SCREEN_WRFMT)                &
            SCREEN_SIZES(I)                         &
           ,(NINT(NETSCRN(I,J,NTH) * NETMULT(NTH))  &
                          ,J=1, NUM_SCREEN_REGION)
       END DO


     END IF

    !---- Show STANDDED

    NUMLINES = MAX_STANDDED_FSUSIZE + 6
    CALL ISNEWPG(PRFILE, NUMLINES)
    WRITE(PRFILE, 5000)  STANDDED_TITLE

    DO I = 1, MAX_STANDDED_FSUSIZE
       WRITE(PRFILE,STANDDED_WRFMT)                &
         STANDDED_SIZES(I), (NINT(STANDDED(I, J,NTH)), J = 1, NUM_STANDDED_REGION)
    END DO

    RETURN

!--------------------------------------------------------------------
! ERROR PROCESSING
!--------------------------------------------------------------------
800 CALL ERROR_MSG('FS_READPARM', 'ERROR READING THE PARAMETER FILE: ' // VAR, ABORT)

!--------------------------------------------------------------------
! FORMAT STATEMENTS
!--------------------------------------------------------------------
!---- read statements
1010  FORMAT(2X,A)            ! read parameter name
1020  FORMAT(2X,A,2X,A,I2)    ! read MRVALUES line
1100  FORMAT(2X,A,2X,I10)     ! read integer
1120  FORMAT(2X,A,2X,F10.0)   ! read real w/0 decimals
1122  FORMAT(2X,A,2X,2F10.0)  ! read 2 real w/0 decimals
1240  FORMAT(2X,A,11X,A)      ! read char 1
1250  FORMAT(2X,A,8X,A)       ! read char 4
1280  FORMAT(2X,A,2X,F10.4)   ! read real w/4 decimals
1290  FORMAT(2X,A,2X,F10.6)   ! read real w/6 decimals
1550  FORMAT(2X,A,2X,A)       ! read char 70  then label

!---- write statements

1000  FORMAT(1X, 'FSTAMP PARAMETER PROCESSING FOR NTH = ',I1 &
        ,/, 1X, '---------------------------------------')

2000 FORMAT(   &
         /, 1X, T30, 'MAXIMUM BENEFIT AMOUNTS' &
         /, 1X, T30, 'BY UNIT SIZE AND REGION' &
        ,/, 1X, T30, '  (BENMAX * BENMULT)   ' &
       ,//, 1X, T35, A                         &
        )

3000 FORMAT(                                  &
        //, 1X, T30, '  GROSS INCOME SCREEN  '&
         /, 1X, T30, 'BY UNIT SIZE AND REGION'&
        ,/, 1X, T30, '  (GRSSCRN * GRSMULT)  '&
       ,//, 1X, T35, A                        &
        )

3001 FORMAT(                                  &
        //, 1X, T30, '  GROSS INCOME SCREEN  '&
         /, 1X, T30, 'BY UNIT SIZE AND REGION'&
        ,/, 1X, T30, '  (HHS POVERTY INCOME GUIDELINES * GRSMULT)  '&
       ,//, 1X, T35, A                        &
        )

4000 FORMAT(                                   &
        //, 1X, T30, '   NET INCOME SCREEN   ' &
         /, 1X, T30, 'BY UNIT SIZE AND REGION' &
        ,/, 1X, T30, '  (NETSCRN * NETMULT)  ' &
       ,//, 1X, T35, A                         &
        )
5000 FORMAT(                              &
        // T30, '  STANDARD DEDUCTION   ' &
         / T30, 'BY UNIT SIZE AND REGION' &
       ,//, 1X, T30, A                    &
          )

         
    END
