!***************************************************************************
! SAVED AS:      FSTABLES.F90
! CALLED BY:     FSTAMP2
!
! THIS SUBROUTINE COMPUTES VARIABLES USED IN THE SUMMARY AND
! G/L TABLES.  THIS SUBROUTINE ALSO CALLS THOSE ROUTINES.
!
! THIS SUBROUTINE IS CALLED FROM FSTAMP2 (KEOF = 2).
!
! NOTES:
!    1. TABULATE BASELAW INFO ONCE (NTH = 1), SINCE THERE IS AT MOST
!       ONE BASELAW SIMULATION PER RUN. IF BASELAW IS BEING CREATED
!       DURING THIS RUN, IT CAN ONLY OCCUR WHEN NTH = 1.
!    2. THE FIRST CALL TO THE TABLES' SUBROUTINES IS FOR THE BASELAW
!       TABULATION.  BASELAW DATA ARE STORED IN SLOT #1.  THE BASELAW
!       CALL IS ONLY NEEDED ONCE, SO IT IS DONE WHEN NTH = 1.
!    3. THE SUBSEQUENT CALLS TO THE TABLES' SUBROUTINES ARE FOR
!       EACH REFORM PLAN.  REFORM DATA ARE STORED IN SLOT # NTH+1, WHICH
!       WE HAVE RENAMED TO BE REFORM-IDX.
!    4. SUMMARY TABLES PROCESS EACH FSU.
!    5. GAINER/LOSER TABLES COLLAPSE FSUS.  IF GLUNIT = 1, ALL FSUS ARE
!       COMBINED INTO ONE FSU. IF GLUNIT = 2, ALL REFORM FSUS ARE
!       COMBINED INTO THEIR CORRESPONDING BASELINE FSU.
!
!
!
!
! SUBROUTINES:
! ISNEWPG     TALLIES LINE & PAGE NBRS; PRINTS HEADER WHEN REQUESTED/NEEDED
! ERROR_MSG   DISPLAYS AN ERROR MESSAGE
!
! FS_TABLE1   SUMMARY TABLE THAT COMPARES OVERALL RESULTS
! FS_TABLE2   TABLES OF ELIGIBLES AND PARTICIPANTS BY  UNIT SIZE AND POVERTY
! FS_TABLE3   CHARACTERISTICS TABLES
! FS_TABLE4   WELFARE STATUS TABLES
! FS_TABLE5   DEDUCTIONS TABLES
! FS_TABLE6   GAINER/LOSER TABLES
! FS_TABLE7   PERSON-LEVEL GAINER/LOSER TABLE
!
!***************************************************************************

!**
!* make_modeltype
!* Makes the modeltype string for JSON output based on model code, whether or
!* not to calculate standard deviations, and whether or not this is a state-
!* based or national-only simulation.
!**
FUNCTION make_modeltype(model_code, do_stats, do_states) RESULT(code)
    IMPLICIT NONE

    CHARACTER(4), INTENT(in) :: model_code
    LOGICAL, INTENT(in) :: do_stats
    INTEGER, INTENT(in) :: do_states
    CHARACTER(15) :: code

    code = ""

    IF (model_code == "MSIP") code = "msip-"

    IF (.NOT. do_stats) code = trim(code) // "non"
    code = trim(code) // "std"

    IF (model_code /= "QCMM") THEN
        IF (do_states == 1) THEN
            code = trim(code) // "-NAT"
        ELSE
            code = trim(code) // "-STA"
        END IF
    END IF
END FUNCTION make_modeltype

!**
!* concat_titles
!* Concatenates the three title lines into one, trimming where necessary.
!**
FUNCTION concat_titles(titles, n) RESULT(title)
    IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    CHARACTER(72), INTENT(in) :: titles(n)
    CHARACTER(218) :: title
    INTEGER :: i

    IF (n <= 0) RETURN

    title = trim(adjustl(titles(1)))
    DO i = 2,n
        title = trim(title) // " " // trim(adjustl(titles(i)))
    END DO
END FUNCTION concat_titles


SUBROUTINE FS_TABLES(call_summ)
  USE GLOBAL
  USE GLOBPARM
  USE USERPARM, ONLY : DOSTATE, title
  USE FSSIZES
  USE FSWORK
  USE FSPARM
  USE FSLOCS
  USE FSCNTS
  use fsutils
  use states, only: TOTSTATES, US_POS


  IMPLICIT NONE

  INTEGER, INTENT(in) :: call_summ


  REAL(8) :: &
       BASE_POVERTY_LINE


  INTEGER ::                     &
       IUNIT                       &
       ,IP                          &
       ,JP                          &
       ,REFORM_UNIT                 &
       ,BASE_UNIT                   &
       ,BASE_FSUN                   &
       ,NEW_BASE_FSUN               &
       ,REFORM_FSUN                 &
       ,BASE_UNITNUM (MAX_PERSONS)  &
       ,NBR_BASE_UNITS

  LOGICAL ::                      &
       ALL_CATEG_INELIG            &
       ,FOUND_UNIT                  &
       ,FOUND_DIFF_REFORM_UNIT      &
       ,FOUND_MIX_MATCH


  INTEGER :: I, kth
  INTEGER :: ABEND_KERR

  INTEGER :: UNIT_1

  INTEGER, DIMENSION(MAX_NTH) :: NBR_BAD_REFORM_UNITS = 0
  INTEGER, DIMENSION(MAX_NTH) :: NBR_BAD_GLUNIT = 0
  REAL(8), DIMENSION(MAX_NTH) :: WGT_NBR_BAD_REFORM_UNITS = 0.0
  REAL(8), DIMENSION(MAX_NTH) :: WGT_NBR_BAD_GLUNIT = 0.0


  LOGICAL, DIMENSION(MAX_PERSONS, 0:TOTSTATES) :: GL_BASE_HAS_SSI

  INTEGER, DIMENSION(MAX_PERSONS, 0:TOTSTATES) :: GL_BASE_NKIDS
  INTEGER, DIMENSION(MAX_PERSONS, 0:TOTSTATES) :: GL_BASE_NELDER
  INTEGER, DIMENSION(MAX_PERSONS, 0:TOTSTATES) :: GL_BASE_NDIS
  INTEGER, DIMENSION(MAX_PERSONS, 0:TOTSTATES) :: GL_BASE_NFEMALE
  INTEGER, DIMENSION(MAX_PERSONS, 0:TOTSTATES) :: GL_BASE_NMALE
  INTEGER, DIMENSION(MAX_PERSONS, 0:TOTSTATES) :: GL_BASE_HRACE
  INTEGER, DIMENSION(MAX_PERSONS, 0:TOTSTATES) :: GL_BASE_HETHNIC
  INTEGER, DIMENSION(MAX_PERSONS, 0:TOTSTATES) :: GL_BASE_HORIGIN

  INTEGER, PARAMETER :: dp = selected_real_kind(15, 307) ! 15 digits, 10^-307 to 10^307-1
  INTEGER, PARAMETER :: sp = selected_real_kind(6, 37)   ! 6 digits, 10^-37 to 10^37-1
  
  INTEGER :: temp_zero = 0
  INTEGER :: d13 = 0

  INTEGER :: nbr_plans_tabulated = 0

  LOGICAL :: first_call = .TRUE.

  character(15) :: make_modeltype
  character(218) :: concat_titles


  !--------------------------------------------------------------------
  ! BEGIN PROCESSING
  !--------------------------------------------------------------------

  !--------------------------------------------------------------------------
  ! Summary tables:  call these last and only once per household (call_summ=1)
  !--------------------------------------------------------------------------
  IF (call_summ == 1) THEN
     CALL FS_TAB_summary()
     CALL FS_TAB_summary_state()
     RETURN
  END IF


  IF (KEOF == 1) THEN
     !! initialize these for each nth
     CALL fs_tab_summary()
     CALL fs_tab_summary_state()
     CALL FS_TAB_GAINER_LOSER_PERS()
     RETURN
  END IF

  IF (KEOF == 3) GOTO 300


  create_table_extracts = .TRUE.
  nbr_plans_tabulated = nth


  !  open extract files
  IF (nth == 1 .AND. first_call .AND. create_table_EXTRACTS) THEN
     OPEN(unit=30, file = "tab_xt_parm.out", status = "replace")
     OPEN(unit=31, file = "tab_xt_summ.out", status = "replace")
     OPEN(unit=32, file = "tab_xt_summ_st.out", status = "replace")
     OPEN(unit=33, file = "tab_xt_char.out", status = "replace")
     OPEN(unit=36, file = "tab_xt_gl.out", status = "replace")
     OPEN(unit=37, file = "tab_xt_gl_pers.out", status = "replace")
     OPEN(unit=38, file = "tab_xt_prot_summ.out", status = "replace")
     OPEN(unit=39, file = "tab_xt_prot_ben.out", status = "replace")
     OPEN(unit=40, file = "tab_xt_prot_gl.out", status = "replace")
     OPEN(unit=41, file = "tab_xt_gl_st.out", status = "replace")
     OPEN(unit=JSON_FILE, file = "tables.json", status = "replace")

     WRITE(JSON_FILE, *) "{" ! Everything in the JSON file is contained in an object

     WRITE(JSON_FILE, *) ' "modeltype": "', TRIM(make_modeltype(model_code, dostats(nth), dostate)), '",'

     ! Footnotes for tables
     WRITE(JSON_FILE, *) ' "source": "Source: ', TRIM(concat_titles(title, 3)), '",'
     WRITE(JSON_FILE, *) ' "runtime": "Simulation run at ', TRIM(timestamp_time), ' on ', TRIM(timestamp_date), '",'
     WRITE(JSON_FILE, *) ' "reform": "', TRIM(ADJUSTL(planname(1))),'",'
     
     first_call = .FALSE.
  END IF

  !!  select baselaw data to tabulate
  IF (model_code == "MSIP") THEN
     SELECT CASE (dostate)
        !!  only tab national results
     CASE (1)
        IF (kist /= US_POS) RETURN
        !!  only tab 51 simstates
     CASE (2)
        IF (kist == US_POS) RETURN
        !!  only tab SELECTED simstates
     CASE (3)
        IF (kist == US_POS .OR. .NOT. aprocsta(kist, nth)) RETURN
     CASE default
        CALL error_msg("FSTABLES","Invalid DOSTATE", abort)
        RETURN
     END SELECT
  END IF


  ROUTINE_W_ABEND = 'FS_TABULATE_RESULTS'


  !--------------------------------------------------------------------
  !---- Tabulation of the BASELAW SUMMARY TABLES
  !--------------------------------------------------------------------
  IF (NTH == 1) THEN


     !------- Note that HH_BASE_FSBEN may have been computed in DBDEFINE (SIPP model),
     !------- but it must be done here so that the code works proberly with the QC data.
     HH_BASE_FSBEN(kist)  = 0
     HH_BASE_FSpart(kist)  = 0

     DO IUNIT = 1, CTPRHH

        HH_BASE_FSBEN(kist) = HH_BASE_FSBEN(kist) + L_FSBEN(1, KIST)%iper(iunit)

        IF (L_FSUN(1, KIST)%iper(iunit) /= IUNIT) CYCLE  ! not an FSU head

        IF (L_FSPART(1, KIST)%IPER(IUNIT) == 1) THEN
           BASE_PARTIC (IUNIT, kist) = .TRUE.
           HH_BASE_FSpart(kist) = 1
        ELSE
           BASE_PARTIC (IUNIT, kist) = .FALSE.
        ENDIF

        IF (L_FSTANF(1, KIST)%IPER(IUNIT) >  0) THEN
           BASE_HAS_TANF (IUNIT, kist) = .TRUE.
        ELSE
           BASE_HAS_TANF (IUNIT, kist) = .FALSE.
        END IF

        IF (L_FSSSI(1, KIST)%IPER(IUNIT) >  0) THEN
           BASE_HAS_SSI (IUNIT, kist) = .TRUE.
        ELSE
           BASE_HAS_SSI (IUNIT, kist) = .FALSE.
        END IF

        IF (L_FSGA(1, KIST)%IPER(IUNIT) >  0) THEN
           BASE_HAS_GA (IUNIT, kist) = .TRUE.
        ELSE
           BASE_HAS_GA (IUNIT, kist) = .FALSE.
        END IF

        IF (L_FSALLPA(1, KIST)%IPER(IUNIT) == 1) THEN
           BASE_ALLPA (IUNIT, kist) = .TRUE.
        ELSE
           BASE_ALLPA (IUNIT, kist) = .FALSE.
        END IF

        IF (L_FSEARN(1, KIST)%IPER(IUNIT) >  0 ) THEN
           BASE_HAS_EARN (IUNIT, kist) = .TRUE.
        ELSE
           BASE_HAS_EARN (IUNIT, kist) = .FALSE.
        END IF

        IF (L_FSNELDER(1, KIST)%IPER(IUNIT) >  0  &
             .OR. L_FSNDIS  (1, KIST)%IPER(IUNIT) >  0) THEN
           BASE_HAS_ELDDIS (IUNIT, kist) = .TRUE.
        ELSE
           BASE_HAS_ELDDIS (IUNIT, kist) = .FALSE.
        END IF

        IF (L_FSNDIS  (1, KIST)%IPER(IUNIT) >  0) THEN
           BASE_HAS_DIS (IUNIT, kist) = .TRUE.
        ELSE
           BASE_HAS_DIS (IUNIT, kist) = .FALSE.
        END IF

        IF (L_FSNELDER(1, KIST)%IPER(IUNIT) >  0) THEN
           BASE_HAS_ELDER (IUNIT, kist) = .TRUE.
        ELSE
           BASE_HAS_ELDER (IUNIT, kist) = .FALSE.
        END IF

        IF (L_FSNKID(1, KIST)%IPER(IUNIT) >  0) THEN
           BASE_HAS_KIDS (IUNIT, kist) = .TRUE.
        ELSE
           BASE_HAS_KIDS (IUNIT, kist) = .FALSE.
        END IF

        IF (L_FSNK5T17(1, KIST)%IPER(IUNIT) >  0) THEN
           BASE_HAS_K5TO17 (IUNIT, kist) = .TRUE.
        ELSE
           BASE_HAS_K5TO17 (IUNIT, kist) = .FALSE.
        END IF

        IF (L_FSNONCIT(1, KIST)%IPER(IUNIT) >  0) THEN
           BASE_HAS_NONCIT (IUNIT, kist) = .TRUE.
        ELSE
           BASE_HAS_NONCIT (IUNIT, kist) = .FALSE.
        END IF

        IF (L_FSNABAWD(1, KIST)%IPER(IUNIT) >  0) THEN
           BASE_HAS_ABAWD (IUNIT, kist) = .TRUE.
        ELSE
           BASE_HAS_ABAWD (IUNIT, kist) = .FALSE.
        END IF

        IF (L_FSNETINC(1, KIST)%IPER(IUNIT) == 0) THEN
           BASE_HAS_0NET (IUNIT, kist) = .TRUE.
        ELSE
           BASE_HAS_0NET (IUNIT, kist) = .FALSE.
        END IF

        IF (L_FSMINBEN(1, KIST)%IPER(IUNIT) == 1) THEN
           BASE_HAS_MIN (IUNIT, kist) = .TRUE.
        ELSE
           BASE_HAS_MIN (IUNIT, kist) = .FALSE.
        END IF


        BASE_POVERTY_LINE = calc_povline(L_FSUSIZE(1, KIST)%IPER(IUNIT), GEOG_POV)

        BASE_POVRAT (IUNIT, kist) = REAL (L_FSGRINC(1, KIST)%IPER(IUNIT)) / BASE_POVERTY_LINE


        !---- Call these table routines, only if the unit is eligible:
        IF (L_FSBEN(1, KIST)%IPER(IUNIT) >  0) THEN

           !--- Tab eligible/participants baselaw call
           CALL FS_TAB_povrat_size (          &
                L_FSBEN(1, KIST)%IPER(IUNIT)    &
                ,L_FSUSIZE(1, KIST)%IPER(IUNIT)  &
                ,1                               & !! baselaw
                ,BASE_PARTIC (IUNIT, kist)       &
                ,BASE_POVRAT (IUNIT, kist)       &
                ,WGT                             &
                )

           !--- Characteristics table baselaw call
           CALL FS_TAB_characteristics (        &
                L_FSBEN(1, KIST)%IPER(IUNIT)     &
                ,L_FSNETINC(1, KIST)%IPER(IUNIT)  &
                ,L_FSGRINC(1, KIST)%IPER(IUNIT)   &
                ,BASE_HAS_EARN   (IUNIT, kist)    &
                ,BASE_HAS_ELDER  (IUNIT, kist)    &
                ,BASE_HAS_DIS    (IUNIT, kist)    &
                ,BASE_HAS_KIDS   (IUNIT, kist)    &
                ,BASE_HAS_0NET   (IUNIT, kist)    &
                ,BASE_HAS_NONCIT (IUNIT, kist)    &
                ,BASE_HAS_ABAWD  (IUNIT, kist)    &
                ,1                                & !! baselaw
                ,BASE_PARTIC     (IUNIT, kist)    &
                ,WGT                              &
                )

           !--- Welfare status baselaw call
           CALL FS_TAB_welfare_status (   &
                BASE_ALLPA       (IUNIT, kist) &
                ,BASE_HAS_TANF    (IUNIT, kist) &
                ,BASE_HAS_GA      (IUNIT, kist) &
                ,BASE_HAS_SSI     (IUNIT, kist) &
                ,1                              & !! baselaw
                ,BASE_PARTIC      (IUNIT, kist) &
                ,WGT                            &
                )

           !--- Deductions table baselaw call
           CALL FS_TAB_deductions (         &
                L_FSDEPDED(1, KIST)%IPER(IUNIT)  &
                ,L_FSERNDED(1, KIST)%IPER(IUNIT)  &
                ,1                                &    !! baselaw
                ,BASE_PARTIC    (IUNIT, kist)     &
                ,L_FSMEDDED(1, KIST)%IPER(IUNIT)  &
                ,L_FSSTDDED(1, KIST)%IPER(IUNIT)  &
                ,L_FSSLTDED(1, KIST)%IPER(IUNIT)  &
                ,L_FSTOTDED(1, KIST)%IPER(IUNIT)  &
                ,WGT                              &
                )

           !--- Protected Classes table baselaw call
           CALL FS_Tab_protected_summary (    &
                1                                &  ! = 1 for baselaw, > 1 for reforms
                ,L_FSBEN    (1, KIST)%IPER(IUNIT) &
                ,L_FSUSIZE  (1, KIST)%IPER(IUNIT) &
                ,L_fsndis   (1, KIST)%IPER(IUNIT) &
                ,L_fsnelder (1, KIST)%IPER(IUNIT) &
                ,BASE_fsnadult      (IUNIT) &
                ,L_fsnkid   (1, KIST)%IPER(IUNIT) &
                ,BASE_Fsnmale       (IUNIT) &
                ,BASE_fsnfemale     (IUNIT) &
                ,BASE_fshrace       (IUNIT) &
                ,BASE_fshethnic     (IUNIT) &
                ,BASE_fshorigin     (IUNIT) &
                ,BASE_PARTIC        (IUNIT, KIST) &
                ,WGT                        &
                )

           !--- Protected Classes table baselaw call
           CALL FS_Tab_protected_benefits (   &
                1                                &  ! = 1 for baselaw, > 1 for reforms
                ,L_FSBEN    (1, KIST)%IPER(IUNIT) &
                ,L_FSUSIZE  (1, KIST)%IPER(IUNIT) &
                ,L_fsndis   (1, KIST)%IPER(IUNIT) &
                ,L_fsnelder (1, KIST)%IPER(IUNIT) &
                ,BASE_fsnadult      (IUNIT) &
                ,L_fsnkid   (1, KIST)%IPER(IUNIT) &
                ,BASE_Fsnmale       (IUNIT) &
                ,BASE_fsnfemale     (IUNIT) &
                ,BASE_fshrace       (IUNIT) &
                ,BASE_fshethnic     (IUNIT) &
                ,BASE_fshorigin     (IUNIT) &
                ,BASE_PARTIC        (IUNIT, KIST)     &
                ,WGT                        &
                )

        END IF  ! end of baselaw summary tabulation

     END DO  ! end of unit loop

  END IF   ! end of baselaw tabulation (NTH = 1)

  !--------------------------------------------------------------------
  !---- Tabulation of the REFORM SUMMARY TABLES
  !--------------------------------------------------------------------

  IF (BASELAW(NTH) /= ' ') THEN

     DO IUNIT = 1, CTPRHH

        IF (L_FSUN(REFORM_IDX, KIST)%IPER(IUNIT) /= IUNIT) CYCLE  ! not an FSU head

        IF (L_FSPART(REFORM_IDX, KIST)%IPER(IUNIT) == 1) THEN
           PARTIC (IUNIT) = .TRUE.
        ELSE
           PARTIC (IUNIT) = .FALSE.
        END IF

        IF (L_FSTANF(REFORM_IDX, KIST)%IPER(IUNIT) >  0) THEN
           HAS_TANF (IUNIT) = .TRUE.
        ELSE
           HAS_TANF (IUNIT) = .FALSE.
        END IF

        IF (L_FSSSI(REFORM_IDX, KIST)%IPER(IUNIT) >  0) THEN
           HAS_SSI (IUNIT) = .TRUE.
        ELSE
           HAS_SSI (IUNIT) = .FALSE.
        END IF

        IF (L_FSGA(REFORM_IDX, KIST)%IPER(IUNIT) >  0) THEN
           HAS_GA (IUNIT) = .TRUE.
        ELSE
           HAS_GA (IUNIT) = .FALSE.
        END IF

        IF (L_FSALLPA(REFORM_IDX, KIST)%IPER(IUNIT) == 1) THEN
           ALLPA (IUNIT) = .TRUE.
        ELSE
           ALLPA (IUNIT) = .FALSE.
        END IF

        IF (L_FSEARN(REFORM_IDX, KIST)%IPER(IUNIT) >  0) THEN
           HAS_EARN (IUNIT) = .TRUE.
        ELSE
           HAS_EARN (IUNIT) = .FALSE.
        END IF

        IF (L_FSNELDER(REFORM_IDX, KIST)%IPER(IUNIT) >  0  &
             .OR. L_FSNDIS(REFORM_IDX, KIST)%IPER(IUNIT) >  0) THEN
           HAS_ELDDIS (IUNIT) = .TRUE.
        ELSE
           HAS_ELDDIS (IUNIT) = .FALSE.
        END IF

        IF (L_FSNDIS (REFORM_IDX, KIST)%IPER(IUNIT) >  0) THEN
           HAS_DIS (IUNIT) = .TRUE.
        ELSE
           HAS_DIS (IUNIT) = .FALSE.
        END IF


        IF (L_FSNELDER(REFORM_IDX, KIST)%IPER(IUNIT) >  0) THEN
           HAS_ELDER (IUNIT) = .TRUE.
        ELSE
           HAS_ELDER (IUNIT) = .FALSE.
        END IF

        IF (L_FSNKID(REFORM_IDX, KIST)%IPER(IUNIT) >  0) THEN
           HAS_KIDS (IUNIT) = .TRUE.
        ELSE
           HAS_KIDS (IUNIT) = .FALSE.
        END IF

        IF (L_FSNK5T17(REFORM_IDX, KIST)%IPER(IUNIT) >  0) THEN
           HAS_K5TO17 (IUNIT) = .TRUE.
        ELSE
           HAS_K5TO17 (IUNIT) = .FALSE.
        END IF

        IF (L_FSNONCIT(REFORM_IDX, KIST)%IPER(IUNIT) >  0) THEN
           HAS_NONCIT (IUNIT) = .TRUE.
        ELSE
           HAS_NONCIT (IUNIT) = .FALSE.
        END IF

        IF (L_FSNABAWD(REFORM_IDX, KIST)%IPER(IUNIT) >  0) THEN
           HAS_ABAWD  (IUNIT) = .TRUE.
        ELSE
           HAS_ABAWD  (IUNIT) = .FALSE.
        END IF

        IF (L_FSNETINC(REFORM_IDX, KIST)%IPER(IUNIT) == 0) THEN
           HAS_0NET (IUNIT) = .TRUE.
        ELSE
           HAS_0NET (IUNIT) = .FALSE.
        END IF

        IF (L_FSMINBEN(REFORM_IDX, KIST)%IPER(IUNIT) == 1) THEN
           HAS_MIN (IUNIT) = .TRUE.
        ELSE
           HAS_MIN (IUNIT) = .FALSE.
        END IF


        !--- Call the table routines, only if the unit is eligible
        IF (L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT) >  0) THEN

           !--- Tab eligible/participants reform call
           CALL FS_TAB_povrat_size              (  &
                L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT)   &
                ,L_FSUSIZE(REFORM_IDX, KIST)%IPER(IUNIT) &
                ,REFORM_IDX                        &
                ,PARTIC  (IUNIT)                   &
                ,FSPOVRAT(IUNIT)                   &
                ,WGT                               &
                )

           !--- Characteristics table reform call
           CALL FS_TAB_characteristics (             &
                L_FSBEN(REFORM_IDX, KIST)%IPER(IUNIT)     &
                ,L_FSNETINC(REFORM_IDX, KIST)%IPER(IUNIT) &
                ,L_FSGRINC(REFORM_IDX, KIST)%IPER(IUNIT)  &
                ,HAS_EARN   (IUNIT)                    &
                ,HAS_ELDER  (IUNIT)                    &
                ,HAS_DIS    (IUNIT)                    &
                ,HAS_KIDS   (IUNIT)                    &
                ,HAS_0NET   (IUNIT)                    &
                ,HAS_NONCIT (IUNIT)                    &
                ,HAS_ABAWD  (IUNIT)                    &
                ,REFORM_IDX                            &
                ,PARTIC     (IUNIT)                    &
                ,WGT                                   &
                )

           !--- Welfare status reform call
           CALL FS_TAB_welfare_status ( &
                ALLPA   (IUNIT)    &
                ,HAS_TANF(IUNIT)    &
                ,HAS_GA  (IUNIT)    &
                ,HAS_SSI (IUNIT)    &
                ,REFORM_IDX         &
                ,PARTIC     (IUNIT) &
                ,WGT                &
                )

           !--- Deductions table reform call
           CALL FS_TAB_deductions (    &
                L_FSDEPDED(REFORM_IDX, KIST)%IPER(IUNIT) &
                ,L_FSERNDED(REFORM_IDX, KIST)%IPER(IUNIT) &
                ,REFORM_IDX                         &
                ,PARTIC  (IUNIT)                    &
                ,L_FSMEDDED(REFORM_IDX, KIST)%IPER(IUNIT) &
                ,L_FSSTDDED(REFORM_IDX, KIST)%IPER(IUNIT) &
                ,L_FSSLTDED(REFORM_IDX, KIST)%IPER(IUNIT) &
                ,L_FSTOTDED(REFORM_IDX, KIST)%IPER(IUNIT) &
                ,WGT                                &
                )

           !--- Protected Classes table reform call
           CALL FS_Tab_protected_summary ( &
                REFORM_IDX          &  ! = 1 for baselaw, > 1 for reforms
                ,FSBEN     (IUNIT)   &  !
                ,FSUSIZE   (IUNIT)   &  !
                ,fsndis    (IUNIT)   &  !
                ,fsnelder  (IUNIT)   &  !
                ,fsnadult  (IUNIT)   &  !
                ,fsnkid    (IUNIT)   &  !
                ,fsnmale   (IUNIT)   &  !
                ,fsnfemale (IUNIT)   &  !
                ,fshrace   (IUNIT)   &  !
                ,fshethnic (IUNIT)   &  !
                ,fshorigin (IUNIT)   &  !
                ,PARTIC    (IUNIT)   &  !
                ,WGT                 &
                )

           !--- Protected Classes table reform call
           CALL FS_Tab_protected_benefits( &
                REFORM_IDX          &  ! = 1 for baselaw, > 1 for reforms
                ,FSBEN     (IUNIT)   &  !
                ,FSUSIZE   (IUNIT)   &  !
                ,fsndis    (IUNIT)   &  !
                ,fsnelder  (IUNIT)   &  !
                ,fsnadult  (IUNIT)   &  !
                ,fsnkid    (IUNIT)   &  !
                ,fsnmale   (IUNIT)   &  !
                ,fsnfemale (IUNIT)   &  !
                ,fshrace   (IUNIT)   &  !
                ,fshethnic (IUNIT)   &  !
                ,fshorigin (IUNIT)   &  !
                ,PARTIC    (IUNIT)   &  !
                ,WGT                 &
                )


        END IF  ! end of reform summary tabulation

     END DO  ! end of unit loop

  END IF   ! end of reform tabulation (BASELAW > ' ')

  !-----------------------------------------
  ! Summary stats call, keof=2
  !-----------------------------------------
  CALL fs_stats_summary()


  !--------------------------------------------------------------------
  !---- Tabulation of the BASELAW GAINER/LOSER TABLES (tables 6)
  !---- when GLUNIT = 1 (make one FSU per household).
  !---- Tabulate this info. once during the first reform simulation.
  !---- All GL values are put in slot #1
  !--------------------------------------------------------------------

  !universal initialization of all_categ_inelig to avoid compiler warning
  !code to set all_categ_inelig inside loops remains the same
  ALL_CATEG_INELIG = .TRUE.
  
  IF (GLUNIT      == 1                           &
       .AND. ( (NTH == 1 .AND. BASELAW(1) /= ' ')  &
       .OR. (NTH == 2 .AND. BASELAW(1) == ' '))    &
       ) THEN

     GL_BASE_FSBEN       (1, kist) =  HH_BASE_FSBEN(kist) ! created in DB_FS_HH_DEFINERS
     GL_BASE_PARTIC      (1, kist) = .FALSE.
     GL_BASE_HAS_TANF_GA (1, kist) = .FALSE.
     GL_BASE_HAS_EARN    (1, kist) = .FALSE.
     GL_BASE_HAS_DIS     (1, kist) = .FALSE.
     GL_BASE_HAS_ELDER   (1, kist) = .FALSE.
     GL_BASE_HAS_KIDS    (1, kist) = .FALSE.
     GL_BASE_HAS_SSI     (1, kist) = .FALSE.
     GL_BASE_HAS_NONCIT  (1, kist) = .FALSE.
     GL_BASE_HAS_ABAWD   (1, kist) = .FALSE.


     GL_BASE_NKIDS        =  0
     GL_BASE_NELDER       =  0
     GL_BASE_NDIS         =  0
     GL_BASE_NFEMALE      =  0
     GL_BASE_NMALE        =  0
     GL_BASE_HRACE        =  0
     GL_BASE_HETHNIC      =  0
     GL_BASE_HORIGIN      =  0


     !----    all units must be led by a single female in order for the household
     !----    to be classified as a single female.
     GL_BASE_HAS_SNGMOM  (1, kist) = .TRUE.

     GL_BASE_FSUSIZE (1, kist) = 0
     GL_BASE_FSGRINC (1, kist) = 0

     !---- Determine if baselaw household is categorically ineligible
     !---- If categ inelig, assign the reform unit's characteristics;
     !---- but set the baselaw participation flag to 0.

     ALL_CATEG_INELIG = .TRUE.
     DO IP = 1, CTPRHH
        IF (L_FSUN(1, KIST)%IPER(IP) >  0) THEN
           ALL_CATEG_INELIG = .FALSE.
        END IF
     END DO

     unit_1 = 0

     DO IUNIT = 1, CTPRHH

        IF (.NOT. ALL_CATEG_INELIG) THEN

           IF (L_FSUN(1, KIST)%IPER(IUNIT) /= IUNIT) CYCLE  ! not a head

           unit_1 = unit_1 + 1

           IF (L_FSPART(1, KIST)%IPER(IUNIT) == 1) THEN
              GL_BASE_PARTIC (1, kist) = .TRUE.
           END IF

           IF (L_FSTANF(1, KIST)%IPER(IUNIT) > 0 .OR. L_FSGA(1, KIST)%IPER(IUNIT) > 0) THEN
              GL_BASE_HAS_TANF_GA (1, kist) = .TRUE.
           END IF

           IF (L_FSSSI(1, KIST)%IPER(IUNIT) > 0 ) THEN
              GL_BASE_HAS_SSI (1, kist) = .TRUE.
           END IF


           IF (L_FSEARN(1, KIST)%IPER(IUNIT) > 0) THEN
              GL_BASE_HAS_EARN (1, kist) = .TRUE.
           END IF

           IF (L_FSNDIS(1, KIST)%IPER(IUNIT) > 0) THEN
              GL_BASE_HAS_DIS (1, kist) = .TRUE.
           END IF

           IF (L_FSNELDER(1, KIST)%IPER(IUNIT) >  0) THEN
              GL_BASE_HAS_ELDER (1, kist) = .TRUE.
           END IF

           IF (L_FSNKID(1, KIST)%IPER(IUNIT) >  0) THEN
              GL_BASE_HAS_KIDS (1, kist) = .TRUE.
           END IF

           IF (L_FSNGMOM(1, KIST)%IPER(IUNIT) == 0) THEN
              GL_BASE_HAS_SNGMOM (1, kist) = .FALSE.
           END IF

           IF (L_FSNONCIT(1, KIST)%IPER(IUNIT) > 0) THEN
              GL_BASE_HAS_NONCIT(1, kist) = .TRUE.
           END IF

           IF (L_FSNABAWD(1, KIST)%IPER(IUNIT) > 0) THEN
              GL_BASE_HAS_ABAWD (1, kist) = .TRUE.
           END IF


           GL_BASE_FSUSIZE (1, kist) = GL_BASE_FSUSIZE (1, kist) + L_FSUSIZE(1, KIST)%IPER(IUNIT)

           GL_BASE_FSGRINC (1, kist) = GL_BASE_FSGRINC(1, kist)  + L_FSGRINC(1, KIST)%IPER(IUNIT)

           GL_BASE_NELDER  (1, kist) = GL_BASE_NELDER  (1, kist) + L_FSNELDER(1, KIST)%IPER(IUNIT)
           GL_BASE_NKIDS   (1, kist) = GL_BASE_NKIDS   (1, kist) + L_FSNKID  (1, KIST)%IPER(IUNIT)
           GL_BASE_NDIS    (1, kist) = GL_BASE_NDIS    (1, kist) + L_FSNDIS  (1, KIST)%IPER(IUNIT)
           GL_BASE_NFEMALE (1, kist) = GL_BASE_NFEMALE (1, kist) + base_FSNFEMALE (IUNIT)
           GL_BASE_NMALE   (1, kist) = GL_BASE_NMALE   (1, kist) + base_FSNMALE   (IUNIT)

           !! For head's characteristics, use the head of the 1st unit:
           IF (UNIT_1 == 1) THEN
              GL_BASE_HRACE   (1, kist) = base_FSHRACE   (IUNIT)
              GL_BASE_HETHNIC (1, kist) = base_FSHETHNIC (IUNIT)
              GL_BASE_HORIGIN (1, kist) = base_FSHORIGIN (IUNIT)
           END IF

        ELSE  ! assign the characteristics of the reform unit(s)

           IF (L_FSUN(REFORM_IDX, KIST)%IPER(IUNIT) /= IUNIT) CYCLE  ! not a head

           IF (L_FSTANF(REFORM_IDX, KIST)%IPER(IUNIT) > 0   &
                .OR. L_FSGA(REFORM_IDX, KIST)%IPER(IUNIT) > 0) THEN
              GL_BASE_HAS_TANF_GA (1, kist) = .TRUE.
           END IF

           IF (L_FSSSI (REFORM_IDX, KIST)%IPER(IUNIT) > 0) THEN
              GL_BASE_HAS_SSI (1, kist) = .TRUE.
           END IF

           IF (L_FSEARN(REFORM_IDX, KIST)%IPER(IUNIT) > 0) THEN
              GL_BASE_HAS_EARN (1, kist) = .TRUE.
           END IF

           IF (L_FSNDIS(REFORM_IDX, KIST)%IPER(IUNIT) > 0) THEN
              GL_BASE_HAS_DIS (1, kist) = .TRUE.
           END IF

           IF (L_FSNELDER(REFORM_IDX, KIST)%IPER(IUNIT) >  0) THEN
              GL_BASE_HAS_ELDER (1, kist) = .TRUE.
           END IF

           IF (L_FSNKID(REFORM_IDX, KIST)%IPER(IUNIT) >  0) THEN
              GL_BASE_HAS_KIDS (1, kist) = .TRUE.
           END IF

           IF (L_FSNGMOM(REFORM_IDX, KIST)%IPER(IUNIT) == 0) THEN
              GL_BASE_HAS_SNGMOM (1, kist) = .FALSE.
           END IF

           IF (L_FSNONCIT(REFORM_IDX, KIST)%IPER(IUNIT) > 0) THEN
              GL_BASE_HAS_NONCIT(1, kist) = .TRUE.
           END IF

           IF (L_FSNABAWD(REFORM_IDX, KIST)%IPER(IUNIT) > 0) THEN
              GL_BASE_HAS_ABAWD (1, kist) = .TRUE.
           END IF


           GL_BASE_FSUSIZE (1, kist) = GL_BASE_FSUSIZE (1, kist) + L_FSUSIZE(REFORM_IDX, KIST)%IPER(IUNIT)

           GL_BASE_FSGRINC (1, kist) = GL_BASE_FSGRINC(1, kist)  + L_FSGRINC(REFORM_IDX, KIST)%IPER(IUNIT)


           GL_BASE_NELDER  (1, kist) = GL_BASE_NELDER  (1, kist) + L_FSNELDER(REFORM_IDX, KIST)%IPER(IUNIT)
           GL_BASE_NKIDS   (1, kist) = GL_BASE_NKIDS   (1, kist) + L_FSNKID  (REFORM_IDX, KIST)%IPER(IUNIT)
           GL_BASE_NDIS    (1, kist) = GL_BASE_NDIS    (1, kist) + L_FSNDIS  (REFORM_IDX, KIST)%IPER(IUNIT)
           GL_BASE_NFEMALE (1, kist) = GL_BASE_NFEMALE (1, kist) + FSNFEMALE (IUNIT)
           GL_BASE_NMALE   (1, kist) = GL_BASE_NMALE   (1, kist) + FSNMALE   (IUNIT)

           !! For head's characteristics, use the head of the 1st unit:
           IF (UNIT_1 == 1) THEN
              GL_BASE_HRACE   (1, kist) = base_FSHRACE   (IUNIT)
              GL_BASE_HETHNIC (1, kist) = base_FSHETHNIC (IUNIT)
              GL_BASE_HORIGIN (1, kist) = base_FSHORIGIN (IUNIT)
           END IF

        END IF
     END DO

     IF (GL_BASE_FSUSIZE(1, kist) == 0) THEN
        GL_BASE_POVERTY_LINE(1, kist) = 0
     ELSE
        GL_BASE_POVERTY_LINE (1, kist) = calc_povline(GL_BASE_FSUSIZE(1, kist) , GEOG_POV)
     END IF


     IF (GL_BASE_POVERTY_LINE(1, kist) > 0) THEN
        GL_BASE_POVRAT (1, kist) = REAL(GL_BASE_FSGRINC(1, kist)) / GL_BASE_POVERTY_LINE(1, kist)
     ELSE
        GL_BASE_POVRAT (1, kist) = 0.0
     END IF

     !--- Gainer/loser tables) baselaw call when GLUNIT = 1

     IF (GL_BASE_PARTIC(1, kist)) THEN

        CALL FS_TAB_gainer_loser    (   &
              GL_BASE_HAS_DIS    (1, kist)  &
             ,GL_BASE_HAS_EARN   (1, kist)  &
             ,GL_BASE_HAS_ELDER  (1, kist)  &
             ,GL_BASE_HAS_KIDS   (1, kist)  &
             ,GL_BASE_HAS_NONCIT (1, kist)  &
             ,GL_BASE_HAS_ABAWD  (1, kist)  &
             ,GL_BASE_FSUSIZE    (1, kist)  &
             ,GL_BASE_POVRAT     (1, kist)  &
             ,HH_BASE_FSBEN(kist)           &
             ,GL_BASE_PARTIC     (1, kist)  &
             ,0                       & !! reform hhld benefit amt (dummy)
             ,.TRUE.                  & !! reform participation (dummy)
             ,1                       & !! baselaw
             ,REGION                  &
             ,WGT                     &
             )

     END IF

     IF (GL_BASE_PARTIC(1, kist) .OR. hh_base_fsben(kist) > 0) THEN
        CALL FS_TAB_protected_gainer_loser (   &
             1                       & !! baselaw
             ,GL_BASE_FSUSIZE    (1, KIST)  &
             ,GL_BASE_NELDER     (1, kist)  &
             ,GL_BASE_NKIDS      (1, kist)  &
             ,GL_BASE_NDIS       (1, kist)  &
             ,GL_BASE_NFEMALE    (1, kist)  &
             ,GL_BASE_NMALE      (1, kist)  &
             ,GL_BASE_HRACE      (1, kist)  &
             ,GL_BASE_HETHNIC    (1, kist)  &
             ,GL_BASE_HORIGIN    (1, kist)  &
             ,HH_BASE_FSBEN         (kist)  &
             ,GL_BASE_PARTIC     (1, kist)  &
             ,temp_zero               & !! reform hhld benefit amt (dummy)
             ,.TRUE.                  & !! reform participation (dummy)
             ,WGT                     &
             ,kist                    &
             ,1                       &
             )



     END IF ! end call if baselaw participant

  END IF  ! Baselaw GL tabulation when GLUNIT = 1

  !--------------------------------------------------------------------
  !---- Tabulation of the REFORM GAINER/LOSER TABLES
  !---- when GLUNIT = 1 (make one FSU per household)
  !---- GL values are put in slot #1.
  !--------------------------------------------------------------------

  IF (GLUNIT == 1 .AND. BASELAW(NTH) /= ' ') THEN

     GL_PARTIC(1) = .FALSE.
     GL_FSBEN(1) = HH_FSBEN  ! created in FS_ELIGIBILITY routine

     DO IUNIT = 1, CTPRHH

        IF (L_FSUN(REFORM_IDX, KIST)%IPER(IUNIT) /= IUNIT) CYCLE  ! not an FSU head

        IF (L_FSPART(REFORM_IDX, KIST)%IPER(IUNIT) == 1) THEN
           GL_PARTIC(1) = .TRUE.
        END IF

     END DO

     ! gainer/loser tables: reform call when GLUNIT = 1

     IF (GL_BASE_PARTIC (1, kist) .OR. GL_PARTIC(1)) THEN
        CALL FS_TAB_gainer_loser    (     &
              GL_BASE_HAS_DIS    (1, kist)   &
             ,GL_BASE_HAS_EARN   (1, kist)   &
             ,GL_BASE_HAS_ELDER  (1, kist)   &
             ,GL_BASE_HAS_KIDS   (1, kist)   &
             ,GL_BASE_HAS_NONCIT (1, kist)   &
             ,GL_BASE_HAS_ABAWD  (1, kist)   &
             ,GL_BASE_FSUSIZE    (1, kist)   &
             ,GL_BASE_POVRAT     (1, kist)   &
             ,HH_BASE_FSBEN      (   kist)   &
             ,GL_BASE_PARTIC     (1, kist)   &
             ,HH_FSBEN                       &
             ,GL_PARTIC          (1)         &
             ,REFORM_IDX                     &
             ,REGION                         &
             ,WGT                            &
             )


     END IF

     IF (GL_BASE_PARTIC (1, kist)  .OR. GL_PARTIC(1)  &
          .OR. hh_BASE_FSBEN (kist) > 0 .OR. HH_FSBEN > 0 ) THEN
        CALL FS_TAB_protected_gainer_loser (  &
             REFORM_IDX                     &
             ,GL_BASE_FSUSIZE   (1, KIST)    &
             ,GL_BASE_NELDER    (1, kist)    &
             ,GL_BASE_NKIDS     (1, kist)    &
             ,GL_BASE_NDIS      (1, kist)    &
             ,GL_BASE_NFEMALE   (1, kist)    &
             ,GL_BASE_NMALE     (1, kist)    &
             ,GL_BASE_HRACE     (1, kist)    &
             ,GL_BASE_HETHNIC   (1, kist)    &
             ,GL_BASE_HORIGIN   (1, kist)    &
             ,HH_BASE_FSBEN        (kist)    &
             ,GL_BASE_PARTIC    (1, KIST)    &
             ,HH_FSBEN                 &
             ,GL_PARTIC         (1)    &
             ,WGT                      &
             ,kist                     &
             ,1                        &
             )


     END IF  ! end of gl call if base or reform participant

  END IF  ! Reform GL tabulation when GLUNIT = 1

  !--------------------------------------------------------------------
  !---- Tabulation of the BASELAW GAINER/LOSER TABLES (tables 6)
  !---- when GLUNIT = 2 (combine reform FSUs to conform with baselaw FSU)
  !---- By definition, this needs to be a reform run.
  !---- First we will establish the reform units' information, then
  !---- we will establish the baselaw units' information, and then
  !---- we will make calls to the tabulation routines.
  !--------------------------------------------------------------------

  IF (GLUNIT == 2 .AND. BASELAW(NTH) /= ' ') THEN

     !---- initialize reform data
     DO IUNIT = 1, CTPRHH
        GL_FSBEN(IUNIT) = 0
        GL_PARTIC(IUNIT) = .FALSE.
     END DO

     !---- Establish connection between reform and baselaw units, and
     !---- accumulate reform data, but put the data in the reform unit's
     !---- corresponding baselaw unit's slot

     DO IP = 1, CTPRHH
        IF  (L_FSUN(1, KIST)%IPER(IP) == 0  &                ! not in baselaw
             .AND. L_FSUN(REFORM_IDX, KIST)%IPER(IP) == 0) CYCLE    ! nor reform FSU

        REFORM_FSUN = L_FSUN(REFORM_IDX, KIST)%IPER(IP)
        IF (REFORM_FSUN == 0) REFORM_FSUN = IP

        BASE_FSUN = L_FSUN(1, KIST)%IPER(REFORM_FSUN)
        IF (BASE_FSUN == 0) BASE_FSUN = REFORM_FSUN

        GL_FSBEN(BASE_FSUN) = GL_FSBEN(BASE_FSUN) + L_FSBEN(REFORM_IDX, KIST)%IPER(IP)

        IF (L_FSPART(REFORM_IDX, KIST)%IPER(IP) == 1) GL_PARTIC(BASE_FSUN) = .TRUE.

     END DO

     !---- Establish connection between reform and baselaw units, and
     !---- accumulate baselaw data.  Since reform units must be the same
     !---- across multiple reforms (see below), we can do this process
     !---- once during the first reform.

     IF ((NTH == 1 .AND. BASELAW(1) /= ' ')  &
          .OR. (NTH == 2 .AND. BASELAW(1) == ' ')) THEN

        !---- initialize baselaw data
        DO IUNIT = 1, CTPRHH
           GL_BASE_FSBEN(IUNIT, kist)       = 0
           GL_BASE_FSUSIZE(IUNIT, kist)     = 0
           GL_BASE_FSGRINC(IUNIT, kist)     = 0
           GL_BASE_PARTIC(IUNIT, kist)      = .FALSE.
           GL_BASE_HAS_TANF_GA(IUNIT, kist) = .FALSE.
           GL_BASE_HAS_SSI    (IUNIT, kist) = .FALSE.
           GL_BASE_HAS_EARN(IUNIT, kist)    = .FALSE.
           GL_BASE_HAS_DIS(IUNIT, kist)     = .FALSE.
           GL_BASE_HAS_ELDER(IUNIT, kist)   = .FALSE.
           GL_BASE_HAS_KIDS(IUNIT, kist)    = .FALSE.
           GL_BASE_HAS_SNGMOM(IUNIT, kist)  = .TRUE.
           GL_BASE_HAS_NONCIT(IUNIT, kist)  = .FALSE.
           GL_BASE_HAS_ABAWD(IUNIT, kist)   = .FALSE.


           GL_BASE_NELDER    (IUNIT, kist)   = 0
           GL_BASE_NKIDS     (IUNIT, kist)   = 0
           GL_BASE_NDIS      (IUNIT, kist)   = 0
           GL_BASE_NFEMALE   (IUNIT, kist)   = 0
           GL_BASE_NMALE     (IUNIT, kist)   = 0

           GL_BASE_HRACE     (IUNIT, kist)   = 0
           GL_BASE_HETHNIC   (IUNIT, kist)   = 0
           GL_BASE_HORIGIN   (IUNIT, kist)   = 0


        END DO




        !---- Determine if baselaw unit is categorically ineligible

        DO IP = 1, CTPRHH

           IF  (L_FSUN(1, KIST)%IPER(IP) == 0  &              ! not in baselaw
                .AND. L_FSUN(REFORM_IDX, KIST)%IPER(IP) == 0) CYCLE  ! or reform FSU

           IF  (L_FSUN(1, KIST)%IPER(IP) == 0  &
                .AND. L_FSUN(REFORM_IDX, KIST)%IPER(IP) == IP) THEN ! head of reform
              ALL_CATEG_INELIG = .TRUE.                    ! unit

              DO JP = 1, CTPRHH
                 IF (L_FSUN(REFORM_IDX, KIST)%IPER(JP) /= IP) CYCLE ! not in unit

                 IF (L_FSUN(1, KIST)%IPER(JP)  >  0) THEN
                    ALL_CATEG_INELIG = .FALSE.
                 END IF
              END DO

           END IF  ! end of determining if the baselaw unit is categ inelig

           !---- If the baselaw unit is categorically eligible and this person
           !---- is not categorically eligible, skip the person

           IF  (L_FSUN(1, KIST)%IPER(IP) == 0 .AND. .NOT. ALL_CATEG_INELIG) CYCLE

           !---- Assign the baselaw FSUN.  In most cases the baselaw FSUN will
           !---- be unchanged, but if a unit is categorically ineligible during
           !---- baselaw, the baselaw unit will equal the corresponding reform
           !---- unit and will obtain its characteristics from the reform unit,
           !---- except the benefit amt and participation decision, which will
           !---- be zero and false respectively.

           IF (L_FSUN(1, KIST)%IPER(IP) >  0) THEN

              NEW_BASE_FSUN = L_FSUN(1, KIST)%IPER(IP)

              IF (NEW_BASE_FSUN /= IP) CYCLE ! not baselaw unit head

              GL_BASE_FSBEN(NEW_BASE_FSUN, kist) =  &
                   GL_BASE_FSBEN(NEW_BASE_FSUN, kist) +  L_FSBEN(1, KIST)%IPER(IP)

              IF (L_FSPART(1, KIST)%IPER(IP) == 1) GL_BASE_PARTIC(NEW_BASE_FSUN, kist) = .TRUE.

              GL_BASE_FSUSIZE(NEW_BASE_FSUN, kist) = &
                   GL_BASE_FSUSIZE(NEW_BASE_FSUN, kist) + L_FSUSIZE(1, KIST)%IPER(IP)

              GL_BASE_FSGRINC(NEW_BASE_FSUN, kist) = &
                   GL_BASE_FSGRINC(NEW_BASE_FSUN, kist) + L_FSGRINC(1, KIST)%IPER(IP)

              IF (L_FSTANF(1, KIST)%IPER(IP) > 0 .OR. L_FSGA(1, KIST)%IPER(IP) > 0) THEN
                 GL_BASE_HAS_TANF_GA(NEW_BASE_FSUN, kist) = .TRUE.
              END IF

              IF (L_FSSSI (1, KIST)%IPER(IP) > 0) THEN
                 GL_BASE_HAS_SSI(NEW_BASE_FSUN, kist) = .TRUE.
              END IF

              IF (L_FSEARN(1, KIST)%IPER(IP) > 0) THEN
                 GL_BASE_HAS_EARN(NEW_BASE_FSUN, kist) = .TRUE.
              END IF

              IF (L_FSNDIS(1, KIST)%IPER(IP) > 0) THEN
                 GL_BASE_HAS_DIS(NEW_BASE_FSUN, kist) = .TRUE.
              END IF

              IF (L_FSNELDER(1, KIST)%IPER(IP) > 0) THEN
                 GL_BASE_HAS_ELDER(NEW_BASE_FSUN, kist) = .TRUE.
              END IF

              IF (L_FSNKID(1, KIST)%IPER(IP) > 0) THEN
                 GL_BASE_HAS_KIDS(NEW_BASE_FSUN, kist) = .TRUE.
              END IF

              IF (L_FSNGMOM(1, KIST)%IPER(IP) == 0) THEN
                 GL_BASE_HAS_SNGMOM(NEW_BASE_FSUN, kist) = .FALSE.
              END IF

              IF (L_FSNONCIT(1, KIST)%IPER(IP) > 0) THEN
                 GL_BASE_HAS_NONCIT(NEW_BASE_FSUN, kist) = .TRUE.
              END IF

              IF (L_FSNABAWD(1, KIST)%IPER(IP) > 0) THEN
                 GL_BASE_HAS_ABAWD (NEW_BASE_FSUN, kist) = .TRUE.
              END IF


              GL_BASE_NELDER    (NEW_BASE_FSUN, kist)  = L_FSNELDER(1, KIST)%IPER(IP)
              GL_BASE_NKIDS     (NEW_BASE_FSUN, kist)  = L_FSNKID(1, KIST)%IPER(IP)
              GL_BASE_NDIS      (NEW_BASE_FSUN, kist)  = L_FSNDIS(1, KIST)%IPER(IP)
              GL_BASE_NFEMALE   (NEW_BASE_FSUN, kist)  = base_FSNFEMALE(IP)
              GL_BASE_NMALE     (NEW_BASE_FSUN, kist)  = base_FSNMALE (IP)
              GL_BASE_HRACE     (NEW_BASE_FSUN, kist)  = base_FSHRACE    (ip)
              GL_BASE_HETHNIC   (NEW_BASE_FSUN, kist)  = base_FSHETHNIC  (ip)
              GL_BASE_HORIGIN   (NEW_BASE_FSUN, kist)  = base_FSHORIGIN  (ip)



           ELSE  ! categorically ineligible during baselaw

              NEW_BASE_FSUN = L_FSUN(REFORM_IDX, KIST)%IPER(IP)

              IF (NEW_BASE_FSUN /= IP) CYCLE ! not reform (and therefore)
              ! baselaw unit head

              GL_BASE_FSBEN(NEW_BASE_FSUN, kist) =  0
              GL_BASE_PARTIC(NEW_BASE_FSUN, kist) = .FALSE.

              GL_BASE_FSUSIZE(NEW_BASE_FSUN, kist) =  &
                   GL_BASE_FSUSIZE(NEW_BASE_FSUN, kist)  + L_FSUSIZE(REFORM_IDX, KIST)%IPER(IP)

              GL_BASE_FSGRINC(NEW_BASE_FSUN, kist) =  &
                   GL_BASE_FSGRINC(NEW_BASE_FSUN, kist) + L_FSGRINC(REFORM_IDX, KIST)%IPER(IP)

              IF (L_FSTANF(REFORM_IDX, KIST)%IPER(IP) > 0  .OR. &
                   L_FSGA(REFORM_IDX, KIST)%IPER(IP) > 0) THEN
                 GL_BASE_HAS_TANF_GA(NEW_BASE_FSUN, kist) = .TRUE.
              END IF

              IF (L_FSSSI(REFORM_IDX, KIST)%IPER(IP) > 0) THEN
                 GL_BASE_HAS_SSI(NEW_BASE_FSUN, kist) = .TRUE.
              END IF

              IF (L_FSEARN(REFORM_IDX, KIST)%IPER(IP) > 0) THEN
                 GL_BASE_HAS_EARN(NEW_BASE_FSUN, kist) = .TRUE.
              END IF

              IF (L_FSNDIS(REFORM_IDX, KIST)%IPER(IP) > 0) THEN
                 GL_BASE_HAS_DIS(NEW_BASE_FSUN, kist) = .TRUE.
              END IF

              IF (L_FSNELDER(REFORM_IDX, KIST)%IPER(IP) >  0) THEN
                 GL_BASE_HAS_ELDER(NEW_BASE_FSUN, kist) = .TRUE.
              END IF

              IF (L_FSNKID(REFORM_IDX, KIST)%IPER(IP) >  0) THEN
                 GL_BASE_HAS_KIDS(NEW_BASE_FSUN, kist) = .TRUE.
              END IF

              IF (L_FSNGMOM(REFORM_IDX, KIST)%IPER(IP) == 0) THEN
                 GL_BASE_HAS_SNGMOM(NEW_BASE_FSUN, kist) = .FALSE.
              END IF

              IF (L_FSNONCIT(REFORM_IDX, KIST)%IPER(IP) >  0) THEN
                 GL_BASE_HAS_NONCIT(NEW_BASE_FSUN, kist) = .TRUE.
              END IF

              IF (L_FSNABAWD(REFORM_IDX, KIST)%IPER(IP) > 0) THEN
                 GL_BASE_HAS_ABAWD (NEW_BASE_FSUN, kist) = .TRUE.
              END IF


              GL_BASE_NELDER    (NEW_BASE_FSUN, kist)  = L_FSNELDER(REFORM_IDX, KIST)%IPER(IP)
              GL_BASE_NKIDS     (NEW_BASE_FSUN, kist)  = L_FSNKID(REFORM_IDX, KIST)%IPER(IP)
              GL_BASE_NDIS      (NEW_BASE_FSUN, kist)  = L_FSNDIS(REFORM_IDX, KIST)%IPER(IP)
              GL_BASE_NFEMALE   (NEW_BASE_FSUN, kist)  = FSNFEMALE(IP)
              GL_BASE_NMALE     (NEW_BASE_FSUN, kist)  = FSNMALE (IP)

              GL_BASE_HRACE     (NEW_BASE_FSUN, kist)  = FSHRACE    (ip)
              GL_BASE_HETHNIC   (NEW_BASE_FSUN, kist)  = FSHETHNIC  (ip)
              GL_BASE_HORIGIN   (NEW_BASE_FSUN, kist)  = FSHORIGIN  (ip)

           END IF

        END DO  ! end of person loop


        DO IUNIT = 1, CTPRHH

           IF (GL_BASE_FSUSIZE(IUNIT, kist) == 0) CYCLE ! no one in unit

           GL_BASE_POVERTY_LINE (IUNIT, kist) = calc_povline (GL_BASE_FSUSIZE(IUNIT, kist), GEOG_POV)

           GL_BASE_POVRAT(IUNIT, kist) = &
                REAL(GL_BASE_FSGRINC(IUNIT, kist)) /  GL_BASE_POVERTY_LINE(IUNIT, kist)

        END DO

        !---- Validate that GLUNIT = 2 is reasonable.
        !---- If a reform unit was created by one or more baselaw units,
        !---- GLUNIT must be 1.

        DO REFORM_UNIT = 1, CTPRHH

           DO IUNIT = 1, CTPRHH
              BASE_UNITNUM(IUNIT) = 0
           END DO

           NBR_BASE_UNITS = 0

           DO IP = 1, CTPRHH

              !---- If not in the reform unit that is under evaluation, skip out
              !---- If not in a baselaw unit, skip out
              IF (L_FSUN(REFORM_IDX, KIST)%IPER(IP) /= REFORM_UNIT) CYCLE
              IF (L_FSUN(1, KIST)%IPER(IP) == 0) CYCLE

              !---- Determine if the baselaw unit is already loaded in the unit array

              FOUND_UNIT = .FALSE.
              DO BASE_UNIT = 1, NBR_BASE_UNITS
                 IF (L_FSUN(1, KIST)%IPER(IP) == BASE_UNITNUM(BASE_UNIT)) FOUND_UNIT = .TRUE.
              END DO

              ! ---- Load the baselaw unit into the array, if not already loaded

              IF (.NOT. FOUND_UNIT) THEN
                 NBR_BASE_UNITS = NBR_BASE_UNITS + 1
                 BASE_UNITNUM(NBR_BASE_UNITS) = L_FSUN(1, KIST)%IPER(IP)
              END IF

           END DO ! end of person loop

           IF (NBR_BASE_UNITS >  1) THEN
              NBR_BAD_GLUNIT(NTH) = NBR_BAD_GLUNIT(NTH) + 1
              WGT_NBR_BAD_GLUNIT(NTH) = WGT_NBR_BAD_GLUNIT(NTH) + WGT
              IF (NBR_BAD_GLUNIT(NTH) <= DEBUGNBR) THEN
                 KFREQ = 1
                 ! write diagnostics
                 NUMLINES = 5
                 CALL ISNEWPG(PRFILE, NUMLINES)
                 WRITE(PRFILE, 8010)     &
                      NBR_BAD_GLUNIT(NTH)  &
                      ,HHID                 &
                      ,REFORM_UNIT          &
                      ,NBR_BASE_UNITS
                 ! and abend
                 ABEND_REASON = &
                      'GLUNIT(NTH) = 2 BUT TWO OR MORE BASELAW UNITS BECAME A REFORM UNIT'
                 ABEND_KERR = ABORT
                 CALL ERROR_MSG(ROUTINE_W_ABEND, ABEND_REASON, ABEND_KERR)
              END IF
           END IF

        END DO  ! end of reform unit loop

        !---- Double check that GLUNIT = 2 is valid by determining if an
        !---- an excluded person in baselaw now joins an FSU with other
        !---- persons (who were not excluded in baselaw) and
        !---- becomes the FSU head.  If so, this is a mix-match case and GLUNIT=2
        !---- will not be allowed.

        IF (NBR_BASE_UNITS <= 1) THEN

           DO REFORM_UNIT = 1, CTPRHH

              FOUND_MIX_MATCH = .FALSE.

              IF (L_FSUN(REFORM_IDX, KIST)%IPER(REFORM_UNIT) /= REFORM_UNIT) CYCLE  ! not a reform head

              IF (L_FSUN(1, KIST)%IPER(REFORM_UNIT) == 0) THEN

                 DO IP = 1, CTPRHH
                    IF (L_FSUN(REFORM_IDX, KIST)%IPER(IP) /= REFORM_UNIT) CYCLE  ! not in the evaluated unit

                    IF (L_FSUN(1, KIST)%IPER(IP) > 0) THEN
                       FOUND_MIX_MATCH = .TRUE.
                    END IF

                 END DO

                 IF (FOUND_MIX_MATCH) THEN
                    NBR_BAD_GLUNIT(NTH) = NBR_BAD_GLUNIT(NTH) + 1
                    WGT_NBR_BAD_GLUNIT(NTH) = WGT_NBR_BAD_GLUNIT(NTH) + WGT
                    IF (NBR_BAD_GLUNIT(NTH) <= DEBUGNBR) THEN
                       KFREQ = 1
                       ! write diagnostics
                       NUMLINES = 25
                       CALL ISNEWPG(PRFILE, NUMLINES)
                       WRITE(PRFILE, 8010)        &
                            NBR_BAD_GLUNIT(NTH)    &
                            ,HHID                   &
                            ,REFORM_UNIT            &
                            ,2 ! (nbr_base_units)
                       write(prfile, 8050)                       
                       do ip = 1, ctprhh
                          write(prfile, 8051) ip                    &
                                 ,l_fsun(1, kist)%iper(ip)          &
                                 ,l_fsusize(1, kist)%iper(ip)       &
                                 ,l_fsun(reform_idx, kist)%iper(ip) &
                                 ,l_fsusize(reform_idx, kist)%iper(ip) 
                       end do
                       ! and abend
                       ABEND_REASON = &
                            'GLUNIT(NTH) = 2 BUT A BASELAW-EXCLUDED PERSON BECAME HEAD OF UNIT'
                       ABEND_KERR = ABORT
                       CALL ERROR_MSG(ROUTINE_W_ABEND, ABEND_REASON, ABEND_KERR)
                    END IF
                 END IF
              END IF ! end of excluded head clause

           END DO ! end of double-check loop

        END IF ! end of double-checking validity of GLUNIT = 2

     END IF ! end of baselaw (during first reform) computations

     !---- Validate that the reform unit is the same across all reform plans
     !---- We must ensure that the reform unit is the same, because
     !---- the baselaw unit, in some cases, depends on the reform unit definition.
     !---- The baselaw unit must be the same, otherwise the GL tables would
     !---- compare apples and oranges.

     FOUND_DIFF_REFORM_UNIT = .FALSE.

     IF (NTH >  1) then
        if (BASELAW(NTH - 1) /= ' ') THEN

           DO IP = 1, CTPRHH

              IF  (L_FSUN(REFORM_IDX, KIST)%IPER(IP) /= L_FSUN(REFORM_IDX - 1, KIST)%IPER(IP) ) THEN
                 FOUND_DIFF_REFORM_UNIT = .TRUE.
              END IF

           END DO

           IF (FOUND_DIFF_REFORM_UNIT) THEN

              GLTABS(NTH) = .FALSE. ! do not print gl tables

              NBR_BAD_REFORM_UNITS(NTH) = NBR_BAD_REFORM_UNITS(NTH) + 1
              WGT_NBR_BAD_REFORM_UNITS(NTH) = WGT_NBR_BAD_REFORM_UNITS(NTH) + WGT

              IF (NBR_BAD_REFORM_UNITS(NTH) <= 10) THEN           ! stop writing message after 10 times,
                 ABEND_REASON = 'REFORM UNIT DEFINITION IS NOT ' & ! in case user increases ABORT and
                      // 'THE SAME ACROSS REFORM PLANS'                 ! continues with the run
                 ABEND_KERR = 2 ! give a mild warning
                 CALL ERROR_MSG(ROUTINE_W_ABEND, ABEND_REASON, ABEND_KERR)
              ENDIF

              IF (NBR_BAD_REFORM_UNITS(NTH) <= DEBUGNBR) THEN
                 KFREQ = 1
                 NUMLINES = 10
                 CALL ISNEWPG(PRFILE, NUMLINES)
                 WRITE(PRFILE, 8020) ABEND_REASON   &
                      ,NBR_BAD_REFORM_UNITS(NTH)      &
                      ,HHID
              END IF
           END IF
        end if  !baselaw(nth-1) has value
     END IF ! end of checking that reform units are the same

     IF (.NOT. FOUND_DIFF_REFORM_UNIT) THEN

        !---- Call the g/l tables once for baselaw tabulation, but call it during
        !---- the first reform plan (since the unit definition of baselaw depends
        !---- on the reform's unit definition).
        !---- If this simulation contains all reform plans, NTH = 1 would be
        !---- the first reform plan.
        !---- If this simulation contains a new baselaw followed by a reform,
        !---- the new baselaw would be created during NTH = 1, so the first
        !---- reform would be during NTH = 2.
        IF ((NTH == 1 .AND. BASELAW(1) /= ' ') .OR. (NTH == 2 .AND. BASELAW(1) == ' ')) THEN

           !---- Table 6 (gainer/loser tables) baselaw call when GLUNIT = 2

           !---- Call for each baselaw FSU (remember that some baselaw units
           !---- are created from categorically ineligible persons, so we cannot
           !---- use L_FSUN(1))%IPER(IP).
           !---- Tabulate the units only if they are participating in baselaw

           DO IUNIT = 1, CTPRHH

              IF (GL_BASE_FSUSIZE(IUNIT, kist) == 0) CYCLE  ! not gl base unit

              IF (GL_BASE_PARTIC(IUNIT, kist)) THEN
                 CALL FS_TAB_gainer_loser    (        &
                       GL_BASE_HAS_DIS     (IUNIT, kist) &
                      ,GL_BASE_HAS_EARN    (IUNIT, kist) &
                      ,GL_BASE_HAS_ELDER   (IUNIT, kist) &
                      ,GL_BASE_HAS_KIDS    (IUNIT, kist) &
                      ,GL_BASE_HAS_NONCIT  (IUNIT, kist) &
                      ,GL_BASE_HAS_ABAWD   (IUNIT, kist) &
                      ,GL_BASE_FSUSIZE     (IUNIT, kist) &
                      ,GL_BASE_POVRAT      (IUNIT, kist) &
                      ,GL_BASE_FSBEN       (IUNIT, kist) &
                      ,GL_BASE_PARTIC      (IUNIT, kist) &
                      ,0                           &!! reform hhld benefit amt (dummy)
                      ,.TRUE.                      &!! reform participation (dummy)
                      ,1                           &!! baselaw
                      ,REGION                      &
                      ,WGT                         &
                      )


              END IF

              IF (GL_BASE_PARTIC(IUNIT, kist) .OR. GL_BASE_FSBEN(IUNIT, kist) > 0) THEN
                 CALL FS_TAB_protected_gainer_loser (      &
                      1                                 &!! baselaw
                      ,GL_BASE_FSUSIZE   (IUNIT, kist)   &
                      ,GL_BASE_NELDER    (IUNIT, kist)   &
                      ,GL_BASE_NKIDS     (IUNIT, kist)   &
                      ,GL_BASE_NDIS      (IUNIT, kist)   &
                      ,GL_BASE_NFEMALE   (IUNIT, kist)   &
                      ,GL_BASE_NMALE     (IUNIT, kist)   &
                      ,GL_BASE_HRACE     (IUNIT, kist)   &
                      ,GL_BASE_HETHNIC   (IUNIT, kist)   &
                      ,GL_BASE_HORIGIN   (IUNIT, kist)   &
                      ,GL_BASE_FSBEN     (IUNIT, kist)   &
                      ,GL_BASE_PARTIC    (IUNIT, kist)   &
                      ,TEMP_zero                   &!! reform hhld benefit amt (dummy)
                      ,.TRUE.                      &!! reform participation (dummy)
                      ,WGT                         &
                      ,kist                        &
                      ,iunit                       &
                      )


              END IF ! end of participation test

           END DO ! end of baselaw units

        END IF ! end of baselaw call

        !---- Gainer/loser tables reform call when GLUNIT = 2
        !---- Protected Class Gainer/loser tables reform call when GLUNIT = 2
        !---- Call for each reform FSU

        DO IUNIT = 1, CTPRHH

           IF (GL_BASE_FSUSIZE(IUNIT, kist) == 0) CYCLE  ! not a reform unit

           IF (GL_BASE_PARTIC(IUNIT, kist) .OR. GL_PARTIC(IUNIT) ) THEN
              CALL FS_TAB_gainer_loser    (         &
                    GL_BASE_HAS_DIS     (IUNIT, kist)  &
                   ,GL_BASE_HAS_EARN    (IUNIT, kist)  &
                   ,GL_BASE_HAS_ELDER   (IUNIT, kist)  &
                   ,GL_BASE_HAS_KIDS    (IUNIT, kist)  &
                   ,GL_BASE_HAS_NONCIT  (IUNIT, kist)  &
                   ,GL_BASE_HAS_ABAWD   (IUNIT, kist)  &
                   ,GL_BASE_FSUSIZE     (IUNIT, kist)  &
                   ,GL_BASE_POVRAT      (IUNIT, kist)  &
                   ,GL_BASE_FSBEN       (IUNIT, kist)  &
                   ,GL_BASE_PARTIC      (IUNIT, kist)  &
                   ,GL_FSBEN            (IUNIT)        &
                   ,GL_PARTIC           (IUNIT)        &
                   ,REFORM_IDX                   &
                   ,REGION                       &
                   ,WGT                          &
                   )

           END IF

           IF (GL_BASE_PARTIC(IUNIT, kist) .OR. GL_PARTIC(IUNIT) &
                .OR. GL_BASE_fsben(IUNIT, kist) > 0 .OR. GL_fsben (IUNIT) > 0) THEN
              CALL FS_TAB_protected_gainer_loser (          &
                   REFORM_IDX                        &
                   ,GL_BASE_FSUSIZE   (IUNIT, KIST)   &
                   ,GL_BASE_NELDER    (IUNIT, kist)   &
                   ,GL_BASE_NKIDS     (IUNIT, kist)   &
                   ,GL_BASE_NDIS      (IUNIT, kist)   &
                   ,GL_BASE_NFEMALE   (IUNIT, kist)   &
                   ,GL_BASE_NMALE     (IUNIT, kist)   &
                   ,GL_BASE_HRACE     (IUNIT, kist)   &
                   ,GL_BASE_HETHNIC   (IUNIT, kist)   &
                   ,GL_BASE_HORIGIN   (IUNIT, kist)   &
                   ,GL_BASE_FSBEN     (IUNIT, KIST)   &
                   ,GL_BASE_PARTIC    (IUNIT, KIST)   &
                   ,GL_FSBEN          (IUNIT)         &
                   ,GL_PARTIC         (IUNIT)         &
                   ,wgt                               &
                   ,kist                              &
                   ,iunit                             &
                   )

           END IF ! end of participation test

        END DO ! end of reform units

     END IF ! end of ensuring reform units are the same

  END IF ! end of GLUNIT = 2 clause


  !--- Person-level gainer loser table
  CALL FS_TAB_GAINER_LOSER_PERS()



  !--------------------------------------------------------------------
  ! FORMAT STATEMENTS
  !--------------------------------------------------------------------

  !---- error statements
8010 FORMAT(/                                                   &
       ,/, 1X, 'NBR OF CASES WITH THIS ERROR = ', I5             &
       ,/, 1X, 'HHID = ', I20                                    &
       ,/, 1X, 'REFORM FSUN = ', I5, ' NBR OF BASE UNITS = ', I5 &
       )

8020 FORMAT(/, ' ***************************************'        &
       ,  '***************************************'              &
       ,/,' FROM SUBROUTINE FS_TABULATE_RESULTS'                 &
       ,/, 1X, A                                                 &
       ,/, 1X, 'NBR CASES: ', I5                                 &
       ,/, 1X, 'HOUSEHOLD: ', I20                                &
       ,//, 1X, 'GAINER/LOSER TABLES WILL NOT BE GENERATED'      &
       ,//, 1X, 'SET GLUNIT=1 FOR ALL NTHS AND RERUN'            &
       ,/,' ***************************************'             &
       ,  '***************************************'              &
       )

8050 format(/                                                   &
       ,/, ' PERSON   BASELINE FSU   BASELINE FSUSIZE   REFORM FSU   REFORM FSU SIZE')
8051 FORMAT(/, 2X, I5, 6X, I5, 15X, I2, 12X, I5, 9X, I2)

  RETURN



300 CONTINUE

  !-----------------------------------------
  ! Generate table parameter file
  !-----------------------------------------

  IF (create_table_extracts) THEN
     WRITE(30, 3000)  &
          TRIM(ADJUSTL(model_label))     &       ! from header  12
          ,TRIM(ADJUSTL(model_code))      &       !              12
          ,TRIM(ADJUSTL(model_version))   &       ! from header  12
          ,dostate        &       ! integer 5
          ,nbr_plans_tabulated &  ! integer 5
          ,dostats(1)     &       ! logical 5
          ,TRIM(ADJUSTL(title(1)))       &       ! top title from parm file 72
          ,TRIM(ADJUSTL(title(2)))       &       ! 2nd title from parm file 72
          ,TRIM(ADJUSTL(title(3)))       &       ! 3rd title from parm file 72
          ,timestamp_time &       ! from isnewpg
          ,timestamp_date         ! from isnewpg

  END IF


3000 FORMAT( &
       /,t2, "%LET model_label         = '",   a, "' ;"  &
       ,/,t2, "%LET model_code          = '",   a, "' ;"  &
       ,/,t2, "%LET model_version       = '",   a, "' ;"  &
       ,/,t2, "%LET dostate             = ",   i2, "  ;"  &
       ,/,t2, "%LET nbr_plans_tabulated = ",   i2, "  ;"  &
       ,/,t2, "%LET dostats             = '",  L1, "' ;"  &
       ,/,t2, "%LET title1              = '",   a, "' ;"  &
       ,/,t2, "%LET title2              = '",   a, "' ;"  &
       ,/,t2, "%LET title3              = '",   a, "' ;"  &
       ,/,t2, "%LET timestamp_time      = '",  a8, "' ;"  &
       ,/,t2, "%LET timestamp_date      = '", a10, "' ;"  &
       )


  !-----------------------------------------
  ! Summary stats call, keof=3
  !-----------------------------------------
  CALL fs_stats_summary()

  !-----------------------------------------------------------------------
  !--------------- Set Up Parameters For Tables Calls --------------------
  !-----------------------------------------------------------------------
  !---Get up the proper labels for each non-baselaw plan in the run.
  DO I = 1, MAX_NTH
     PLANNBR_Table (I) = PLANNBR (I)
     PLANNAME_Table(I) = PLANNAME(I) (:69)
  ENDDO

  IF (BASELAW(1) == ' ') THEN
     DO I = 1, MAX_NTH - 1
        PLANNBR_Table (I) = PLANNBR (I+1)
        PLANNAME_Table(I) = PLANNAME(I+1) (:69)
        GLTABS(I) = GLTABS  (I+1)
     ENDDO
  ENDIF

  !---Set up some control parameters for tables

  SHOW_ELIG = .TRUE.
  IF (MODEL_LABEL == '   QC MINI') SHOW_ELIG = .FALSE.

  !---For Welfare status & deductions tables, choose the proper set of footnotes
  IF (BASELAW(1) == ' ') THEN

     DO I= 1, MAX_NTH
        T4NOTE_NUM(I) = PUREPA (I)
        T5NOTE_NUM(I) = DEDTYPE(I)
     ENDDO

  ELSE

     T4NOTE_NUM(1) = 3  !-- 3 is the ususal baselaw file development value
     T5NOTE_NUM(1) = 3  !-- 3 refers user to the baselaw run

     DO I= 2, MAX_NTH + 1
        T4NOTE_NUM(I) = PUREPA (I-1)
        T5NOTE_NUM(I) = DEDTYPE(I-1)
     ENDDO

  ENDIF

  !-----------------------------------------------------------------------
  !----------------- Call Tables Routines --------------------------------
  !-----------------------------------------------------------------------
  ! 1st call in KEOF=3 to generate all G/L stats
  kth = 99
  CALL FS_Tab_protected_gainer_loser(&
       kth        &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,.FALSE.    &
       ,d13        &
       ,.FALSE.    &
       ,0.0        &
       ,0          &
       ,0          &
       )



  !---Print comparison summary table
  CALL FS_TAB_summary

  !--- Print gainer/loser tables
  CALL FS_TAB_gainer_loser (&     !-- Gainer/loser tables
        .FALSE.   &               !! has dis  (dummy)
       ,.FALSE.   &               !! has earn (dummy)
       ,.FALSE.   &               !! has elder (dummy)
       ,.FALSE.   &               !! has kids  (dummy)
       ,.FALSE.   &               !! has NONCIT (dummy)
       ,.FALSE.   &               !! has ABAWD  (dummy)
       ,0         &               !! unit size (dummy)
       ,0.d0      &               !! poverty ratio
       ,0         &               !! baselaw benefit amt (dummy)
       ,.FALSE.   &               !! baselaw participation (dummy)
       ,0         &               !! reform benefit amt (dummy)
       ,.FALSE.   &               !! reform participation (dummy)
       ,0         &               !! NTH (dummy)
       ,0         &               !! partic (dummy)
       ,0.e0      &               !! weight (dummy)
       )



  !--- Print summary state table if applicable:
  CALL fs_tab_summary_state


  !--- Print characteristics table:
  CALL FS_TAB_characteristics (   &   !---- Print characteristics table
       0            &             !! benefit amt (dummy)
       ,0            &            !! FSNETINC    (dummy)
       ,0            &            !! FSGRINC     (dummy)
       ,.FALSE.      &            !! has earn    (dummy)
       ,.FALSE.      &            !! has elder   (dummy)
       ,.FALSE.      &            !! has DIS     (dummy)
       ,.FALSE.      &            !! has kids    (dummy)
       ,.FALSE.      &            !! has 0 net   (dummy)
       ,.FALSE.      &            !! has noncit  (dummy)   
       ,.FALSE.      &            !! has ABAWD   (dummy)
       ,0            &            !! NTH (dummy)
       ,.FALSE.      &            !! participation (dummy)
       ,0.0          )            !! weight (dummy)



  !--- Print Protected Classes tables
  CALL FS_Tab_protected_summary( &
       0       &
       ,0       &
       ,0       &
       ,0       &
       ,0       &
       ,0       &
       ,0       &
       ,0       &
       ,0       &
       ,0       &
       ,0       &
       ,0       &
       ,.FALSE. &
       ,0.0     )


  CALL FS_Tab_protected_benefits( &
       0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,.FALSE.    &
       ,0.0        )

  !--- Protected Classes G/L table
  !    2nd call, to print
  CALL FS_Tab_protected_gainer_loser(&
       0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,0          &
       ,.FALSE.    &
       ,d13        &
       ,.FALSE.    &
       ,0.0        &
       ,0          &
       ,0          &
       )


  !--- Print person-level G/L table:
  CALL FS_TAB_GAINER_LOSER_PERS()


  !--- Print additional tables:

  !---Print eligible/participants by poverty status & unit size:
  CALL FS_TAB_povrat_size ( &  !---- Print eligible/participants by povrat & size
       0       &        !! benefit amt (dummy)
       ,0       &        !! unit size (dummy)
       ,0       &        !! NTH (dummy)
       ,.FALSE. &        !! participation (dummy)
       ,0._dp   &        !! poverty ratio
       ,0._sp   )        !! weight (dummy)


  !---Print eligible/participants by poverty status & unit size:
  CALL FS_TAB_welfare_status ( & !---- Print welfare status
       .FALSE.     &             !! has allpa   (dummy)
       ,.FALSE.     &             !! has TANF    (dummy)
       ,.FALSE.     &             !! has GA      (dummy)
       ,.FALSE.     &             !! has SSI     (dummy)
       ,0           &             !! NTH (dummy)
       ,.FALSE.     &             !! participation (dummy)
       ,0.0         )             !! weight (dummy)


  !---Print deductions table:
  CALL FS_TAB_deductions(&  !--- Print eductions table
       0         &              !! FSDEPDED    (dummy)
       ,0         &              !! FSERNDED    (dummy)
       ,0         &              !! NTH (dummy)
       ,.FALSE.   &              !! participation (dummy)
       ,0         &              !! FSMEDDED    (dummy)
       ,0         &              !! FSSTDDED    (dummy)
       ,0         &              !! FSSLTDED    (dummy)
       ,0         &              !! FSTOTDED    (dummy)
       ,0.0       )              !! weight (dummy)


  WRITE(JSON_FILE, *) "}" ! Everything in the JSON file is contained in an object, so close that now
  CLOSE(JSON_FILE)
    
    
  RETURN
END SUBROUTINE FS_TABLES
