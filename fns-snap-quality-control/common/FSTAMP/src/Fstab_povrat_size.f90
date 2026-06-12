!**************************************************************************************************
! Source File:  FSTAB2.F90                  
! Called By:    FS_TABLES                   
!
! TABLE 2 - Food Stamp Units by Unit Size and Gross Income
!
!**************************************************************************************************
      SUBROUTINE FS_TAB_povrat_size( & 
                FSBEN,         &  !--- simulated FSP benefit amount
                FSUSIZE,       &  !--- FSP unit size
                KTH,           &  !--- plan number (plus 1, because 1 used for baseline)
                PARTIC,        &  !--- participation flag (t/f)
                POVRAT,        &  !--- povert ratio of the FSP unit
                WGT            )  !--- sample weight
      USE GLOBAL
      USE FSSIZES, ONLY: MAX_NTH
      USE FSWORK , ONLY: SHOW_ELIG, PLANNAME_TABLE, PLANNBR_TABLE
      use Utils
      IMPLICIT NONE

!---- Declare parameters from calling program
      INTEGER, INTENT(IN) :: FSBEN
      INTEGER, INTENT(IN) :: KTH
      LOGICAL, INTENT(IN) :: PARTIC
      REAL(8), INTENT(IN) :: POVRAT
      INTEGER, INTENT(IN) :: FSUSIZE
      REAL   , INTENT(IN) :: WGT

!---- Variables for tables
      REAL  (8)  ::            WGT_FSBEN

      INTEGER :: I, ITH,  PASS,  RPOVRAT,  RFSUSIZE,   NBR_OF_KTHS = 0
      INTEGER, PARAMETER :: NROWS=5, NCOLS=6

      CHARACTER(132) ::  PLANTEMP
      CHARACTER(132) ::  ELIG_PART

!---- Row labels for both tables
      CHARACTER(9) :: ROWLAB(5) = (/ &
      '  <= 0.0%' ,   &
      '   >0-50%' ,   &
      ' >50-100%' ,   &
      '>100-130%' ,   &
      '    >130%'    /)


!---- Total units by poverty ratio and KTH(row total)
      REAL  (8)  ::   ELIG_UNITS_ROW_TOT(NROWS,max_nth+1) = 0.0   !! Eligibles
      REAL  (8)  ::   PART_UNITS_ROW_TOT(NROWS,max_nth+1) = 0.0   !! Participants

!---- Total units by unit size and KTH (column total)
      REAL  (8)  ::   ELIG_UNITS_COL_TOT(NCOLS,max_nth+1) = 0.0    !! Eligibles
      REAL  (8)  ::   PART_UNITS_COL_TOT(NCOLS,max_nth+1) = 0.0    !! Participants

!---- Percent of total units by poverty ratio and KTH(row percent)
      REAL  (8)  ::   ELIG_UNITS_ROW_PCT(NROWS,max_nth+1) = 0.0     !! Eligibles
      REAL  (8)  ::   PART_UNITS_ROW_PCT(NROWS,max_nth+1) = 0.0     !! Participants

!---- Percent of total units by unit size and KTH
      REAL  (8)  ::   ELIG_UNITS_COL_PCT(NCOLS,max_nth+1) = 0.0    !! Eligibles
      REAL  (8)  ::   PART_UNITS_COL_PCT(NCOLS,max_nth+1) = 0.0    !! Participants

!---- Number of units by unit size, poverty ratio, and KTH (cells)
      REAL  (8)  ::   ELIG_UNITS_CELL(NCOLS,NROWS,max_nth+1) = 0.0  !! Eligibles
      REAL  (8)  ::   PART_UNITS_CELL(NCOLS,NROWS,max_nth+1) = 0.0  !! Participants

!---- Benefits (dollars) by poverty ratio and KTH (row total)
      REAL  (8)  ::   ELIG_BEN_ROW_TOT(NROWS,max_nth+1) = 0.0    !! Eligibles
      REAL  (8)  ::   PART_BEN_ROW_TOT(NROWS,max_nth+1) = 0.0    !! Participants

!---- Total persons by unit size and KTH (column total)
      REAL  (8)  ::   ELIG_PER_COL_TOT(NCOLS,max_nth+1) = 0.0    !! Eligibles
      REAL  (8)  ::   PART_PER_COL_TOT(NCOLS,max_nth+1) = 0.0    !! Participants

!---- Total units by KTH
      REAL  (8)  ::   ELIG_UNITS_TOT(max_nth+1) = 0.0     !! Eligibles
      REAL  (8)  ::   PART_UNITS_TOT(max_nth+1) = 0.0     !! Participants

!---- Total benefits by KTH
      REAL  (8)  ::   ELIG_BEN_TOT(max_nth+1) = 0.0     !! Eligibles
      REAL  (8)  ::   PART_BEN_TOT(max_nth+1) = 0.0     !! Participants

!---- Total persons by KTH
      REAL  (8)  ::   ELIG_PER_TOT(max_nth+1) = 0.0       !! Eligibles
      REAL  (8)  ::   PART_PER_TOT(max_nth+1) = 0.0       !! Participants

!---- Comma display fields
      INTEGER, PARAMETER  ::   UNIT_LEN = 10
      INTEGER, PARAMETER  ::   BEN_LEN  = 14
      CHARACTER(10) :: UNIT_FIELD(7)
      CHARACTER(14) :: BEN_FIELD

!---- By-pass calculations if print requested

      IF (KEOF == 3) GOTO 900
!****************************************************************
!     Perform table calculations
!****************************************************************
      WGT_FSBEN = FSBEN * WGT

!---- Poverty rate recode
      IF (POVRAT.LE.0.) THEN
        RPOVRAT = 1
      ELSE IF (POVRAT .LE. 0.50) THEN
        RPOVRAT = 2
      ELSE IF (POVRAT .LE. 1.00) THEN
        RPOVRAT = 3
      ELSE IF (POVRAT .LE. 1.30) THEN
        RPOVRAT = 4
      ELSE
        RPOVRAT = 5
      END IF

!---- Recode unit size
      IF (FSUSIZE>5) THEN
        RFSUSIZE = 6
      ELSE
        RFSUSIZE = FSUSIZE
      END IF

!---- Keep track of highest KTH
      IF (KTH > NBR_OF_KTHS)  NBR_OF_KTHS = KTH

!---- Count persons by unit size
      ELIG_PER_COL_TOT(RFSUSIZE,KTH)= &                     !! Eligibles
      ELIG_PER_COL_TOT(RFSUSIZE,KTH)  + (WGT*FSUSIZE)

!---- Units by unit size and poverty rate (table cells)
      ELIG_UNITS_CELL(RFSUSIZE,RPOVRAT,KTH) = &             !! Eligibles
      ELIG_UNITS_CELL(RFSUSIZE,RPOVRAT,KTH) + WGT

!---- Food stamp benefit by poverty rate (row)
      ELIG_BEN_ROW_TOT(RPOVRAT,KTH) = &                  !! Eligibles
      ELIG_BEN_ROW_TOT(RPOVRAT,KTH) + WGT_FSBEN

      IF (PARTIC) THEN
!---- Count persons by unit size
        PART_PER_COL_TOT(RFSUSIZE,KTH)=  &                  !! Participants
        PART_PER_COL_TOT(RFSUSIZE,KTH) + (WGT*FSUSIZE)

!---- Units by unit size and poverty rate (table cells)
        PART_UNITS_CELL(RFSUSIZE,RPOVRAT,KTH) =  &          !! Participants
        PART_UNITS_CELL(RFSUSIZE,RPOVRAT,KTH) + WGT

!---- Food stamp benefit by poverty rate (row)
        PART_BEN_ROW_TOT(RPOVRAT,KTH) = &                !! Participants
        PART_BEN_ROW_TOT(RPOVRAT,KTH) + WGT_FSBEN
      ENDIF

      return

  900 CONTINUE
!******************************************************************
!     Print the table
!******************************************************************
!---- Final calculations - Print all plans at once
!---- Row and column total units (marginals)
      DO ITH = 1, NBR_OF_KTHS        !! Loop over all plans
        DO RFSUSIZE = 1, NCOLS
          DO RPOVRAT = 1, NROWS
            ELIG_UNITS_COL_TOT(RFSUSIZE,ITH) =  &
            ELIG_UNITS_COL_TOT(RFSUSIZE,ITH) +  &
            ELIG_UNITS_CELL(RFSUSIZE,RPOVRAT,ITH)

            PART_UNITS_COL_TOT(RFSUSIZE,ITH) =  &
            PART_UNITS_COL_TOT(RFSUSIZE,ITH) +  &
            PART_UNITS_CELL(RFSUSIZE,RPOVRAT,ITH)

            ELIG_UNITS_ROW_TOT(RPOVRAT,ITH) =   &
            ELIG_UNITS_ROW_TOT(RPOVRAT,ITH) +   &
            ELIG_UNITS_CELL(RFSUSIZE,RPOVRAT,ITH)

            PART_UNITS_ROW_TOT(RPOVRAT,ITH) =   &
            PART_UNITS_ROW_TOT(RPOVRAT,ITH) +   &
            PART_UNITS_CELL(RFSUSIZE,RPOVRAT,ITH)
          ENDDO
        ENDDO

!---- Total units and benefits (across the rows)
        DO RPOVRAT = 1,NROWS
          ELIG_UNITS_TOT(ITH) = ELIG_UNITS_TOT(ITH) + &
          ELIG_UNITS_ROW_TOT(RPOVRAT,ITH)

          PART_UNITS_TOT(ITH) = PART_UNITS_TOT(ITH) + &
          PART_UNITS_ROW_TOT(RPOVRAT,ITH)

          ELIG_BEN_TOT(ITH) = ELIG_BEN_TOT(ITH) +  &
          ELIG_BEN_ROW_TOT(RPOVRAT,ITH)

          PART_BEN_TOT(ITH) = PART_BEN_TOT(ITH) +  &
          PART_BEN_ROW_TOT(RPOVRAT,ITH)
        ENDDO

!---- Row percents
        DO RPOVRAT = 1,NROWS
          IF (ELIG_UNITS_TOT(ITH) > 0)                  &
            ELIG_UNITS_ROW_PCT(RPOVRAT,ITH) = 100.0 *   &
            ELIG_UNITS_ROW_TOT(RPOVRAT,ITH) / ELIG_UNITS_TOT(ITH)

          IF (PART_UNITS_TOT(ITH) > 0)                  &
            PART_UNITS_ROW_PCT(RPOVRAT,ITH) = 100.0 *   &
            PART_UNITS_ROW_TOT(RPOVRAT,ITH) / PART_UNITS_TOT(ITH)
        ENDDO

!---- Column percents
        DO RFSUSIZE = 1,NCOLS
          IF (ELIG_UNITS_TOT(ITH) > 0)                 &
            ELIG_UNITS_COL_PCT(RFSUSIZE,ITH) = 100.0 * &
              ELIG_UNITS_COL_TOT(RFSUSIZE,ITH) / ELIG_UNITS_TOT(ITH)

          IF (PART_UNITS_TOT(ITH) > 0)                 &
            PART_UNITS_COL_PCT(RFSUSIZE,ITH) = 100.0 * &
            PART_UNITS_COL_TOT(RFSUSIZE,ITH) / PART_UNITS_TOT(ITH)
        ENDDO

!---- Total persons
        DO RFSUSIZE = 1,NCOLS
          ELIG_PER_TOT(ITH) = &
          ELIG_PER_TOT(ITH) + ELIG_PER_COL_TOT(RFSUSIZE,ITH)

          PART_PER_TOT(ITH) = &
          PART_PER_TOT(ITH) + PART_PER_COL_TOT(RFSUSIZE,ITH)
        ENDDO

        CALL ISNEWPG(TABFILE, PAGE_BREAK_NUMLINES)      ! +1 lets ISNEWPG know
                                                        ! lines are coming - but
                                                        ! not exactly how many.
        IF (ITH == 1) THEN
          PLANTEMP = 'BASELAW'
        ELSE
          PLANTEMP = 'PLAN ' // PLANNBR_TABLE(ITH-1) // ': '// PLANNAME_TABLE(ITH-1)
        ENDIF

        CALL CENTER_TEXT(PLANTEMP,131)

        IF (SHOW_ELIG) THEN
          PASS = 1
          WRITE (TABFILE,1101) PLANTEMP  ! E & P title
        ELSE
          PASS = 2
          WRITE (TABFILE,1100)   PLANTEMP  ! Partic only title
        ENDIF

!---- Print the table for eligibles (PASS=1),
!---- then cycle back and print participants (PASS=2)

  100   CONTINUE
        ELIG_PART = 'ELIGIBLES'
        IF (PASS == 2) ELIG_PART = 'PARTICIPANTS'

        CALL CENTER_TEXT (ELIG_PART,131)

        WRITE (TABFILE,1110) ELIG_PART  ! col heading

!---- Row labels, table cells, total units, % total units, benefits
        DO RPOVRAT = 1,5
          IF (PASS == 1) THEN
            DO I = 1,6
              unit_field(i) = COMMA8(ELIG_UNITS_CELL(I,RPOVRAT,ITH),UNIT_LEN)
            ENDDO
            unit_field(7) =   COMMA8(ELIG_UNITS_ROW_TOT(RPOVRAT,ITH),UNIT_LEN)
            ben_field     =   COMMA8(ELIG_BEN_ROW_TOT(RPOVRAT,ITH),BEN_LEN)

            WRITE (TABFILE,1120)   ROWLAB(RPOVRAT),  (UNIT_FIELD(I),I=1,7),  &
                ELIG_UNITS_ROW_PCT(RPOVRAT,ITH),   BEN_FIELD
          ELSE
            DO I = 1,6
              unit_field(i) =  COMMA8(PART_UNITS_CELL(I,RPOVRAT,ITH),UNIT_LEN)
            ENDDO
            unit_field(7)   =  COMMA8(PART_UNITS_ROW_TOT(RPOVRAT,ITH),UNIT_LEN)
            ben_field       =  COMMA8(PART_BEN_ROW_TOT(RPOVRAT,ITH),BEN_LEN)

            WRITE (TABFILE,1120)  ROWLAB(RPOVRAT),  (UNIT_FIELD(I),I=1,7), &
               PART_UNITS_ROW_PCT(RPOVRAT,ITH),    BEN_FIELD
          ENDIF
        ENDDO

!---- Total units
        IF (PASS == 1) THEN
          DO I = 1,6
            unit_field(i) = COMMA8(ELIG_UNITS_COL_TOT(I,ITH),UNIT_LEN)
          ENDDO
          unit_field(7)   = COMMA8(ELIG_UNITS_TOT(ITH),UNIT_LEN)
          ben_field       = COMMA8(ELIG_BEN_TOT(ITH),BEN_LEN)
        ELSE
          DO I = 1,6
            unit_field(i) = COMMA8(PART_UNITS_COL_TOT(I,ITH),UNIT_LEN)
          ENDDO
          unit_field(7)   = COMMA8(PART_UNITS_TOT(ITH),UNIT_LEN)
          BEN_FIELD       = COMMA8(PART_BEN_TOT(ITH),BEN_LEN)
        ENDIF

        WRITE (TABFILE,1240) (UNIT_FIELD(I),I=1,7),  BEN_FIELD

!---- Total persons
        IF (PASS == 1) THEN
          DO I = 1,6
            unit_field(i) =  COMMA8(ELIG_PER_COL_TOT(I,ITH),UNIT_LEN)
          ENDDO
          unit_field(7) =    COMMA8(ELIG_PER_TOT(ITH),UNIT_LEN)
        ELSE
          DO I = 1,6
            unit_field(i) =  COMMA8(PART_PER_COL_TOT(I,ITH),UNIT_LEN)
          ENDDO
          unit_field(7) =    COMMA8(PART_PER_TOT(ITH),UNIT_LEN)
        ENDIF

        WRITE (TABFILE,1250) (UNIT_FIELD(I),I=1,7)

!---- % of total units
        IF (PASS == 1) THEN
          WRITE (TABFILE,1255) (ELIG_UNITS_COL_PCT(I,ITH),I=1,6)
        ELSE
          WRITE (TABFILE,1255) (PART_UNITS_COL_PCT(I,ITH),I=1,6)
        ENDIF

        !---- Go back and do participants on 2nd pass, then quit
        PASS = PASS + 1
        IF (PASS == 2) GO TO 100
        WRITE (TABFILE,1260)  ! underline

      ENDDO               !! End of loop over all plans




      RETURN

!----------------------------------------------------------------------
! FORMAT STATEMENTS
!----------------------------------------------------------------------
1100 FORMAT(                                                      &
      T41,'                             ' //                      &
      T41,'   DISTRIBUTION OF PARTICIPATING SNAP UNITS' /           &
      T41,'BY GROSS INCOME RELATIVE TO POVERTY AND UNIT SIZE' //  &
      A120)

1101 FORMAT(                                                               &
       T41,'                            '//                               &
       T41,'DISTRIBUTION OF ELIGIBLE AND PARTICIPATING SNAP UNITS' / &
       T41,'  BY GROSS INCOME RELATIVE TO POVERTY AND UNIT SIZE'//           &
       A120)


1110 FORMAT(///                                                    &
       A131/                                                       &
       1X,131('-')/                                                &
       T35,'   Number of Units by Unit Size'/                      &
       2X,'Gross Income',T18,73('-'),T98,'     ',6X,' % of ', 7X,'  Total  '/   &
       2X,'as a Percent',T98,'Total',6X,'Total ',7X,' Benefits'/   &
       2X,'of Poverty',T23,                                        &
       '1',11X,'2',11X,'3',11X,'4',11X,'5',11X,'6+',13X,           &
                             'Units',6X,'Units ',7X,'(dollars)'/   &
       1X,131('-'))

1120  FORMAT(/A10,6X,6(A10,2X),4X,A10,4X,F6.1,'%   ',A14)

1240  FORMAT(//1X,'Total Units',4X,6(A10,2X),4X,A10,  '     100.0%',3X,A14)

1250  FORMAT(//1X,'Total Persons',2X,6(A10,2X),4X,A10)

1255  FORMAT(//1X,'% of Total',5X,6(3X,F7.1,2X),'         100.0%',   /1X,'Units')

1260  FORMAT(/1X,131('-'))

      END
