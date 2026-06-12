!**************************************************************************************************
! Source File:  FSTAB3.F90                  
! Called By:    FS_TABLES                   
!
! TABLE 3 - Characteristics of Eligible and Participating SNAP Units
!
!**************************************************************************************************
      SUBROUTINE FS_Tab_characteristics ( &
                FSBEN,       &  ! BENEFITS
                FSNETINC,    &  ! NET MONTHLY INCOME
                FSGRINC,     &  ! GROSS MONTHLY INCOME
                HAS_EARN,    &  ! EARNERS IN HOUSEHOLD
                HAS_ELDER,   &  ! ELDERLY IN HOUSEHOLD
                HAS_DIS,     &  ! DISABLED IN HOUSEHOLD
                HAS_KIDS,    &  ! NUMBER OF KIDS IN HOUSEHOLD
                HAS_0NET,    &  ! LOGICAL FOR ZERO NET INCOME
                HAS_NONCIT,  &  ! LOGICAL FOR NONCITIZENS IN HOUSEHOLD
                HAS_ABAWD,   &  ! LOGICAL FOR ABAWDS IN HOUSEHOLD
                KTH,         &  ! PLAN NUMBER
                PARTIC,      &  ! LOGICAL FOR PARTICIPATION
                WGT          )  ! SAMPLE WEIGHT
      USE GLOBAL
      USE FSWORK, ONLY: SHOW_ELIG, PLANNAME_TABLE, PLANNBR_TABLE, create_table_extracts
      USE FSPARM, ONLY: JSON_FILE
      use Utils
      IMPLICIT NONE
      
      INTEGER, parameter ::  MAX_KTH = 5  !--- not the same as the Max_KTH used in FSSIZES module
      
!---- Declare parameters from calling program
      INTEGER, intent(in)         ::  FSBEN
      INTEGER, intent(in)         ::  FSNETINC
      INTEGER, intent(in)         ::  FSGRINC
      LOGICAL, intent(in)         ::  HAS_EARN
      LOGICAL, intent(in)         ::  HAS_ELDER
      LOGICAL, intent(in)         ::  HAS_DIS
      LOGICAL, intent(in)         ::  HAS_KIDS
      LOGICAL, intent(in)         ::  HAS_0NET
      LOGICAL, intent(in)         ::  HAS_NONCIT
      LOGICAL, intent(in)         ::  HAS_ABAWD
      INTEGER, intent(in)         ::  KTH
      LOGICAL, intent(in)         ::  PARTIC
      REAL, intent(in)            ::  WGT
      
      CHARACTER (12)  ::  PLANTEMP(MAX_KTH)
      REAL (8)        ::  WGT_FSBEN

!---- Variables for tables
      INTEGER         ::     I,  J,  NBR_OF_KTHS=0,   PASS, row
      integer , parameter :: nrows = 11   ! total rows


!---- Row labels for both tables
      CHARACTER (41)  :: ROWLAB(nrows)= (/&
     '  Earners                                ' , &  ! 1
     '  Elderly                                ' , &  ! 2
     '  Nonelderly Disabled                    ' , &  ! 3
     '  Children                               ' , &  ! 4
     '  Nonelderly Nondisabled Childless Adults' , &  ! 5
     '  Noncitizens                            ' , &  ! 6
     '  Zero Net Income                        ' , &  ! 7
     '  Average Monthly Gross Income           ' , &  ! 8
     '  Average Monthly Net Income             ' , &  ! 9
     ' Total Units                             ' , &  ! 10
     ' Total Benefits ($)                      '   &  ! 11
     /)

!---- Row labels for JSON table
      CHARACTER(64) :: json_rowlab(nrows) = (/&
      "Earners                                                         ", &
      "Elderly individuals                                             ", &
      "Nonelderly individuals with disabilities                        ", &
      "Children                                                        ", &
      "Adults age 18 to 49 without disabilities in childless households", &
      "Noncitizens                                                     ", &
      "Zero net income                                                 ", &
      "Average monthly gross income ($)                                ", &
      "Average monthly net income ($)                                  ", &
      "Total units                                                     ", &
      "Total benefits ($)                                              " /)

!---- Number of units by characteristic and KTH (cells)
      REAL (8)  :: ELIG_CELL_VALUE(NROWS,MAX_KTH + 1) = 0.0
      REAL (8)  :: PART_CELL_VALUE(NROWS,MAX_KTH + 1) = 0.0



!---- Percent change by characteristic and KTH (cells)
      REAL (8)  :: ELIG_CELL_PCT(NROWS,MAX_KTH + 1)   = 0.0
      REAL (8)  :: PART_CELL_PCT(NROWS,MAX_KTH + 1)   = 0.0

!---- Total monthly income variables by KTH
      REAL (8)  :: ELIG_MONTHLY_INC(2,MAX_KTH + 1) = 0.0
      REAL (8)  :: PART_MONTHLY_INC(2,MAX_KTH + 1) = 0.0

!---- Percent of total unit variables by KTH
      REAL (8)  :: ELIG_TOTUNIT_PCT(8,MAX_KTH + 1) = 0.0
      REAL (8)  :: PART_TOTUNIT_PCT(8,MAX_KTH + 1) = 0.0

!---- Comma display fields
      CHARACTER (15) :: COMMA_FIELD (NROWS,MAX_KTH + 1)
      CHARACTER (15) :: PCT_FIELD (NROWS,MAX_KTH + 1)
      CHARACTER (15) :: PCT_TOTAL (8,MAX_KTH + 1)
      CHARACTER (15) :: BLANKS = ' '
      INTEGER, parameter :: FIELD_LEN = 15

      
      character(len=38) :: tab3_plan


!---- By-pass calculations if print requested

      IF (KEOF == 3) GOTO 900
!****************************************************************
!     Perform table calculations
!****************************************************************

      WGT_FSBEN = FSBEN * WGT

!---- Keep track of highest KTH computed
      IF (KTH > MAX_KTH + 1) RETURN !--- TABLE3 CAN ONLY HANDLE 5 REFORMS
      IF (KTH > NBR_OF_KTHS) NBR_OF_KTHS = KTH

!---- Units with earners
      IF (HAS_EARN) THEN
        ELIG_CELL_VALUE (1,KTH) = ELIG_CELL_VALUE (1,KTH) + WGT
        IF (PARTIC)   PART_CELL_VALUE (1,KTH) = PART_CELL_VALUE (1,KTH) + WGT
      ENDIF

!---- Units with elderly
      IF (HAS_ELDER) THEN
        ELIG_CELL_VALUE (2,KTH) = ELIG_CELL_VALUE (2,KTH) + WGT
        IF (PARTIC)   PART_CELL_VALUE (2,KTH) = PART_CELL_VALUE (2,KTH) + WGT
      ENDIF

!---- Units with disabled
      IF (HAS_DIS) THEN
        ELIG_CELL_VALUE (3,KTH) = ELIG_CELL_VALUE (3,KTH) + WGT
        IF (PARTIC) PART_CELL_VALUE (3,KTH) = PART_CELL_VALUE (3,KTH) + WGT
      ENDIF

!---- Units with children
      IF (HAS_KIDS) THEN
        ELIG_CELL_VALUE (4,KTH) = ELIG_CELL_VALUE (4,KTH) + WGT
        IF (PARTIC)  PART_CELL_VALUE (4,KTH) = PART_CELL_VALUE (4,KTH) + WGT
      ENDIF

!---- Units with adults without disabilities, no children in unit
      IF (HAS_ABAWD ) THEN
        ELIG_CELL_VALUE (5,KTH) = ELIG_CELL_VALUE (5,KTH) + WGT
        IF (PARTIC)  PART_CELL_VALUE (5,KTH) = PART_CELL_VALUE (5,KTH) + WGT
      ENDIF

!---- Units with NONCITIZENS
      IF (HAS_NONCIT) THEN
        ELIG_CELL_VALUE (6, KTH) = ELIG_CELL_VALUE (6, KTH) + WGT
        IF (PARTIC)  PART_CELL_VALUE (6, KTH) = PART_CELL_VALUE (6, KTH) + WGT
      ENDIF

!---- Units with zero net income
      IF (HAS_0NET) THEN
        ELIG_CELL_VALUE (7,KTH) = ELIG_CELL_VALUE (7,KTH) + WGT
        IF (PARTIC)  PART_CELL_VALUE (7,KTH) = PART_CELL_VALUE (7,KTH) + WGT
      ENDIF


!---- Total monthly gross income (averaged later)
      ELIG_CELL_VALUE (8,KTH) = ELIG_CELL_VALUE (8,KTH) + WGT
      IF (PARTIC)   PART_CELL_VALUE (8,KTH) = PART_CELL_VALUE (8,KTH) + WGT

      ELIG_MONTHLY_INC (1,KTH) =  ELIG_MONTHLY_INC (1,KTH) + WGT*FSGRINC
      IF (PARTIC) PART_MONTHLY_INC (1,KTH) =  PART_MONTHLY_INC (1,KTH) + WGT*FSGRINC


      ! only count if non-negative:
      if (fsnetinc >= 0) then
         ELIG_CELL_VALUE (9,KTH) = ELIG_CELL_VALUE (9,KTH) + WGT
         IF (PARTIC) PART_CELL_VALUE (9,KTH) = PART_CELL_VALUE (9,KTH) + WGT

!---- Total monthly net income (averaged later)
         ELIG_MONTHLY_INC (2,KTH) = ELIG_MONTHLY_INC (2,KTH) + WGT*FSNETINC
         IF (PARTIC) PART_MONTHLY_INC (2,KTH) = PART_MONTHLY_INC (2,KTH) + WGT*FSNETINC

      end if

!---- Total units
      ELIG_CELL_VALUE (10,KTH) = ELIG_CELL_VALUE (10,KTH) + WGT
      IF (PARTIC) PART_CELL_VALUE (10,KTH) = PART_CELL_VALUE (10,KTH) + WGT

!---- Total benefits
      ELIG_CELL_VALUE (11,KTH) = ELIG_CELL_VALUE (11,KTH) + WGT_FSBEN
      IF (PARTIC) PART_CELL_VALUE (11,KTH) = PART_CELL_VALUE (11,KTH) + WGT_FSBEN

      return



!******************************************************************
!     Print the tables
!******************************************************************
900   CONTINUE
!---- Calculate average income amounts

      DO I = 1,NBR_OF_KTHS
        IF (ELIG_CELL_VALUE(8,I) .GT. 0) THEN
          ELIG_CELL_VALUE(8,I) =  ELIG_MONTHLY_INC(1,I)/ELIG_CELL_VALUE(8,I)
        ENDIF
        IF (PART_CELL_VALUE(8,I) .GT. 0) THEN
          PART_CELL_VALUE(8,I) =  PART_MONTHLY_INC(1,I)/PART_CELL_VALUE(8,I)
        ENDIF

        !!  NET (OVER NON-NEG VALUES)
        IF (ELIG_CELL_VALUE(9, I) .GT. 0) THEN
          ELIG_CELL_VALUE(9, I) =  ELIG_MONTHLY_INC(2,I)/ELIG_CELL_VALUE(9, I)
        ENDIF
        IF (PART_CELL_VALUE(9, I) .GT. 0) THEN
          PART_CELL_VALUE(9, I) =  PART_MONTHLY_INC(2,I)/PART_CELL_VALUE(9, I)
        ENDIF

      ENDDO



!---- Calculate percent change of plans from baseline
      IF (NBR_OF_KTHS > 1) THEN
        DO J = 2, NBR_OF_KTHS
          DO I = 1,NROWS
            IF (ELIG_CELL_VALUE(I,1) .GT. 0 )   ELIG_CELL_PCT(I,J) =  &
                100.0 * (ELIG_CELL_VALUE(I,J) - ELIG_CELL_VALUE(I,1))  / ELIG_CELL_VALUE(I,1)
            IF (PART_CELL_VALUE(I,1) .GT. 0 )   PART_CELL_PCT(I,J) =   &
                100.0 * (PART_CELL_VALUE(I,J) - PART_CELL_VALUE(I,1))  / PART_CELL_VALUE(I,1)
          ENDDO
        ENDDO
      ENDIF


!---- Calculate percent of total units for top half of table
      DO J = 1, NBR_OF_KTHS
        DO I = 1, 7
          IF (ELIG_CELL_VALUE(10,J) > 0.0)  ELIG_TOTUNIT_PCT(I,J) = &
              100.0*(ELIG_CELL_VALUE(I,J)/ELIG_CELL_VALUE(10,J))

          IF (PART_CELL_VALUE(10,J) > 0.0)  PART_TOTUNIT_PCT(I,J) =  &
              100.0*(PART_CELL_VALUE(I,J)/PART_CELL_VALUE(10,J))
        ENDDO
      ENDDO


!---- Pass = 1 for Eligibles table
!---- Pass = 2 for Participants table

      DO PASS = 1,2

        IF (PASS == 1 .AND. .NOT. SHOW_ELIG  ) CYCLE

!---- Print table
        CALL ISNEWPG(TABFILE, PAGE_BREAK_NUMLINES + 1)  ! +1 lets ISNEWPG know
                                                        ! lines are coming - but
                                                        ! not exactly how many.
        IF (PASS == 1) THEN
          WRITE (TABFILE,1100)  ! E title
        ELSE
          WRITE (TABFILE,1101)  ! P title
        ENDIF

        DO I = 1, NBR_OF_KTHS - 1
          PLANTEMP(I) = 'PLAN ' // PLANNBR_TABLE(I)
          CALL REMOVE_BLANKS(PLANTEMP(I))
          PLANTEMP(I) = ADJUSTR(PLANTEMP(I))
        END DO

        WRITE (TABFILE,1200)  (PLANTEMP(I),I= 1, NBR_OF_KTHS-1)   ! col heading

        WRITE (TABFILE,1210)  ! underline


!------ Move appropriate values to display fields
        DO I=1,NROWS
          DO J=1,6
            IF (J <= NBR_OF_KTHS) THEN
              IF (PASS == 1) THEN
                COMMA_FIELD(I,J) = COMMA8(ELIG_CELL_VALUE(I,J),FIELD_LEN)
                WRITE(PCT_FIELD(I,J),'(F15.1)') ELIG_CELL_PCT(I,J)
              ELSE
                comma_field(i,j) = COMMA8(PART_CELL_VALUE(I,J),FIELD_LEN)
                WRITE(PCT_FIELD(I,J),'(F15.1)') PART_CELL_PCT(I,J)
              ENDIF
            ELSE
              COMMA_FIELD(I,J) = BLANKS
              PCT_FIELD(I,J) = BLANKS
            ENDIF
            IF (I <= 8) THEN
              IF (J <= NBR_OF_KTHS) THEN
                IF (PASS == 1) THEN
                  WRITE(PCT_TOTAL(I,J),'(F15.1)')ELIG_TOTUNIT_PCT(I,J)
                ELSE
                  WRITE(PCT_TOTAL(I,J),'(F15.1)')PART_TOTUNIT_PCT(I,J)
                ENDIF
              ELSE
                PCT_TOTAL(I,J) = BLANKS
              ENDIF
            ENDIF
          ENDDO !For each plan
        ENDDO   !For each row

        WRITE (TABFILE,1215)                  &
         (ROWLAB(I),                          &
         (COMMA_FIELD(I,J),J=1,MAX_KTH + 1),  &
         (PCT_FIELD(I,J),J=2,MAX_KTH + 1),    &
         (PCT_TOTAL(I,J),J=1,MAX_KTH + 1),  I=1,NROWS-4)

        WRITE (TABFILE,1216)                  &
          (ROWLAB(I),                         &
          (COMMA_FIELD(I,J),J=1,MAX_KTH + 1), &
          (PCT_FIELD(I,J),J=2,MAX_KTH + 1),   &
          I=NROWS-3,NROWS)

        WRITE (TABFILE,1210)

        ! Open JSON table
        SELECT CASE (PASS)
        CASE (1)
            WRITE (JSON_FILE, '(A)', ADVANCE='no') '"Table 4": ['

             ! Everything up to the last four rows are counts (as ints) and percentages (as floats), plus a total.
            DO row = 1, NROWS-4
                WRITE (JSON_FILE, 1111, ADVANCE='no') TRIM(json_rowlab(row)), &
                    NINT(ELIG_CELL_VALUE(row, 1), SELECTED_INT_KIND(12)), &
                    ELIG_TOTUNIT_PCT(row, 1), &
                    NINT(ELIG_CELL_VALUE(row, 2), SELECTED_INT_KIND(12)), &
                    ELIG_CELL_PCT(row, 2), &
                    ELIG_TOTUNIT_PCT(row, 2)
                WRITE (JSON_FILE, *) "," ! not the last...
            END DO

            ! The last four rows (average incomes, total units, and total benefits) do not include percentages,
            ! so use a different format label.
            DO row = NROWS-3, NROWS
                WRITE (JSON_FILE, 1112, ADVANCE='no') TRIM(json_rowlab(row)), &
                    NINT(ELIG_CELL_VALUE(row, 1), SELECTED_INT_KIND(12)), &
                    NINT(ELIG_CELL_VALUE(row, 2), SELECTED_INT_KIND(12)), &
                    ELIG_CELL_PCT(row, 2)
                IF (row /= NROWS) WRITE (JSON_FILE, *) "," ! not the last...
            END DO

            ! Close JSON table, get ready for the next one
            WRITE (JSON_FILE, *) ! newline
            WRITE (JSON_FILE, *) '],'

        CASE (2)
            WRITE (JSON_FILE, '(A)', ADVANCE='no') '"Table 4A": ['

             ! Everything up to the last four rows are counts (as ints) and percentages (as floats), plus a total.
            DO row = 1, NROWS-4
                WRITE (JSON_FILE, 1111, ADVANCE='no') TRIM(json_rowlab(row)), &
                    NINT(PART_CELL_VALUE(row, 1), SELECTED_INT_KIND(12)), &
                    PART_TOTUNIT_PCT(row, 1), &
                    NINT(PART_CELL_VALUE(row, 2), SELECTED_INT_KIND(12)), &
                    PART_CELL_PCT(row, 2), &
                    PART_TOTUNIT_PCT(row, 2)
                WRITE (JSON_FILE, *) "," ! not the last...
            END DO

            ! The last four rows (average incomes, total units, and total benefits) do not include percentages,
            ! so use a different format label.
            DO row = NROWS-3, NROWS
                WRITE (JSON_FILE, 1112, ADVANCE='no') TRIM(json_rowlab(row)), &
                    NINT(PART_CELL_VALUE(row, 1), SELECTED_INT_KIND(12)), &
                    NINT(PART_CELL_VALUE(row, 2), SELECTED_INT_KIND(12)), &
                    PART_CELL_PCT(row, 2)
                IF (row /= NROWS) WRITE (JSON_FILE, *) "," ! not the last...
            END DO

            ! Close JSON table, get ready for the next one
            WRITE (JSON_FILE, *) ! newline
            WRITE (JSON_FILE, *) '],'

        END SELECT

!---- Print plan descriptions below tables
        DO I = 1, NBR_OF_KTHS - 1
          WRITE (TABFILE,1270) PLANNBR_TABLE(I), PLANNAME_TABLE(I)
        END DO




      END DO ! end of pass loop

      if (create_table_extracts) then

       if (show_elig) then
        do j = 1, nbr_of_kths
          if (j == 1) then
            tab3_plan = 'Baselaw'
          else
            tab3_plan = planname_table(j-1) (:38)
          end if
          do i = 1, 8
            write(33, 3301) 't_char      ', j, adjustl(tab3_plan), 'W', 'ELIG', rowlab(i), elig_cell_value(i,j), &
                            ELIG_CELL_PCT(I,J), ELIG_TOTUNIT_PCT(I,J)
          end do
          do i = 9, nrows
            write(33, 3301) 't_char      ', j, adjustl(tab3_plan), 'W', 'ELIG', rowlab(i), elig_cell_value(i,j), &
                            ELIG_CELL_PCT(I,J), 0.0
          end do
        end do
       end if

       do j = 1, nbr_of_kths
          if (j == 1) then
            tab3_plan = 'Baselaw'
          else
            tab3_plan = planname_table(j-1) (:38)
          end if
          do i = 1, 8
            write(33, 3301) 't_char      ', j, adjustl(tab3_plan), 'W', 'PART', rowlab(i), part_cell_value(i,j), &
                            part_CELL_PCT(I,J), part_TOTUNIT_PCT(I,J)
          end do
          do i = 9, nrows
            write(33, 3301) 't_char      ', j, adjustl(tab3_plan), 'W', 'PART', rowlab(i), part_cell_value(i,j), &
                            part_CELL_PCT(I,J), 0.0
          end do
       end do
      end if

 3301 format (1x,a12, i3, a40, a2, a5, a42,  f20.4, 2f10.4)



      RETURN
!----------------------------------------------------------------------
! FORMAT STATEMENTS
!----------------------------------------------------------------------
1100 format( T37,'                              ' //     &
             T37,'         CHARACTERISTICS OF ELIGIBLE SNAP UNITS' //)
1101 format( T37,'                              ' //    &
             T37,'       CHARACTERISTICS OF PARTICIPATING SNAP UNITS' //)
1200 format(/ 1X, 131('-')         &
      / 2X, 'Characteristic',      &
        T50, ' BASELAW', 5(3X, A))
1210 format(1X,131('-'))

! JSON row format
1111 format ('["', A, '",', 2(I12, ',', F15.2, ','), F15.2, ']') ! weights and percentages, alternating
1112 format ('["', A, '",', I12, ',"n.a.",', I12, ',', F15.2, ',"n.a."]') ! weights, no percentages

1215 format (/ ' Units with:'                             &
            // 7(1x,A41,t43, 6(A15)                       &
            / '       % Chg from Baselaw', T58,5(A15)     &
            / '       % Of Total Units',   T43,6(A15) //) &
         )

1216 format ( ' Average Income Amounts ($):' //           &
         2(1x,A41, t43, 6(A15)                            &
         / '       % Chg from Baselaw',T58,5(A15)//)/     &
         2(1x,A41, t43, 6(A15)                            &
         / '   % Chg from Baselaw',    T58,5(A15)//)      &
         )

1270 format(/ 1X,'PLAN ',A,':  ',A70)

    end
