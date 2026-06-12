!**************************************************************************************************
! Source File:  FSTAMP.F90                  
! Called By:    Supervisor                  
!
! Driver routines for the FSTAMP master routine, which simulates the
! food stamp program.
!
!**************************************************************************************************
    SUBROUTINE FSTAMP
    USE GLOBAL
    USE userparm, only : num_debugid, hhlds_to_print, showstate
    use fswork, ONLY : start_kist, end_kist
    use states, only : state_order
    IMPLICIT NONE

    INTEGER :: save_kfreq, i, ikist


    !---------------------------------------------------
    ! In KEOF=2, call FSTAMP2 over all simulated states
    ! Note: start_kist & end_kist are model-dependent
    !---------------------------------------------------
    IF (KEOF == 2) THEN

       save_kfreq = kfreq

       !--- loop over states and/or national model
       DO iKIST = start_kist, end_kist
          kist = state_order(ikist)
          kfreq = 0
          if (showstate(kist)) then
             do i = 1,  num_debugid
                if (hhid == hhlds_to_print(i)) kfreq = 1
             end do
          end if

          CALL FSTAMP2

          save_kfreq = MAX(save_kfreq, kfreq)

       END DO

       !---------------------------------------------------------------------------------
       ! Tabulate the standard simulation results, this 2nd call picks up summary tables
       !---------------------------------------------------------------------------------
       CALL FS_TABLES(1)


       kfreq = MAX(save_kfreq, kfreq)

       CALL DB_FS_DISPLAY_SUMM_DEBUG

    END IF


    IF (KEOF == 1) CALL FSTAMP1   !-- initialze run

    IF (KEOF == 3) CALL FSTAMP3   !-- print tables


    RETURN
    END





!**************************************************************************************************
    SUBROUTINE FSTAMP1
    USE GLOBAL

    IMPLICIT NONE

    CALL DB_FS_PARM_ARRAY_SIZES  !---- Establish database-specific array dimensions

    CALL FS_READPARM             !---- Read and validate the FSTAMP parameters.

    CALL DB_FS_VALIDATE_PARM     !---- Validate DB-specific parameters

    CALL DB_FS_HH_DEFINERS       !--- Assign values to household definer variables

    if (NTH == 1)  &             !---- Allocate generic varible arrays
       call fs_allocate_generic_vars()

    CALL DB_FS_LOCATE_VARS       !---- Locate database-specific FSTAMP input variables

    CALL FS_LOCATE_VARS          !---- Locate generic FSTAMP input/output variables

    call fs_alloc_stats          !---- Initialize Table stats


    CALL DB_FS_PARTICIPATION     !---- Initialize random numbers needed for participation simulation

    CALL DB_FS_UNIT              !---- Initialize random numbers needed for FSU simulation

    !---- Make dummy calls to phase 2 routines, for easy coding of reforms.
    CALL DB_FS_VARS      !-- dummy call (return only)
    CALL FS_ELIGIBILITY  !-- dummy call (return only)

    CALL FS_TABLES(0)

    RETURN
    END





!**************************************************************************************************
    SUBROUTINE FSTAMP2
    USE GLOBAL
    USE USERPARM, ONLY : SHOWSTATE, dostate
    USE FSSIZES
    USE FSPARM
    USE FSLOCS
    USE FSWORK
    IMPLICIT NONE
    INTEGER ::  IDX, IP, return_code

    !--------------------------------------------------------------------
    ! BEGIN PROCESSING
    !--------------------------------------------------------------------

    !---- Initialize the output variables, which are located on the person record
    !---- Baselaw will be stored in the first slot.
    !---- Reforms will be stored in slot nth + 1.

    skip_hh = .false.

    REFORM_IDX = NTH + 1
    IDX = REFORM_IDX     !--- local version of slot used

    DO IP = 1, CTPRHH

       L_FSUN     (IDX, KIST)%IPER(IP) = 0
       L_CASHOT   (IDX, KIST)%IPER(IP) = 0
       L_FTSTUD   (IDX, KIST)%IPER(IP) = 0

       L_FSUSIZE  (IDX, KIST)%IPER(IP) = 0
       L_FSTANF   (IDX, KIST)%IPER(IP) = 0
       L_FSSSI    (IDX, KIST)%IPER(IP) = 0
       L_FSGA     (IDX, KIST)%IPER(IP) = 0
       L_FSALLPA  (IDX, KIST)%IPER(IP) = 0
       L_FSEARN   (IDX, KIST)%IPER(IP) = 0
       L_FSNELDER (IDX, KIST)%IPER(IP) = 0
       L_FSNDIS   (IDX, KIST)%IPER(IP) = 0
       L_FSNKID   (IDX, KIST)%IPER(IP) = 0
       L_FSNK5T17 (IDX, KIST)%IPER(IP) = 0
       L_FSNONCIT (IDX, KIST)%IPER(IP) = 0
       L_FSNABAWD (IDX, KIST)%IPER(IP) = 0
       L_FSNGMOM  (IDX, KIST)%IPER(IP) = 0
       L_FSMINBEN (IDX, KIST)%IPER(IP) = 0

       L_FSTOTDED (IDX, KIST)%IPER(IP) = 0
       L_FSSTDDED (IDX, KIST)%IPER(IP) = 0
       L_FSERNDED (IDX, KIST)%IPER(IP) = 0
       L_FSDEPDED (IDX, KIST)%IPER(IP) = 0
       L_FSMEDDED (IDX, KIST)%IPER(IP) = 0
       L_FSSLTDED (IDX, KIST)%IPER(IP) = 0

       L_FSASSET  (IDX, KIST)%IPER(IP) = 0
       L_FSGRINC  (IDX, KIST)%IPER(IP) = 0
       L_FSNETINC (IDX, KIST)%IPER(IP) = 0

       L_FSASTEST (IDX, KIST)%IPER(IP) = 0
       L_FSGRTEST (IDX, KIST)%IPER(IP) = 0
       L_FSNETEST (IDX, KIST)%IPER(IP) = 0
       L_FSBEN    (IDX, KIST)%IPER(IP) = 0

       L_FSPART   (IDX, KIST)%IPER(IP) = 0

    ENDDO


    !--- Save baselaw values in non-processed state in state runs
    return_code = 1
    if (dostate == 2 .or. dostate == 3) then
       call db_fs_save_generic_vars(return_code)
    end if


    !--- For non-selected states in a state run, return
    if (return_code == 0) then
       return
    end if


    !---- If print flag has already been set (by DEBUG_MSG), either by a previous
    !---- master routine or by a previous NTH of FSTAMP, then start a new page.
    IF (KFREQ > 0) CALL ISNEWPG(PRFILE, PAGE_BREAK_NUMLINES)


    CALL DB_FS_HH_DEFINERS     !------ Assign values to household definer variables


    if (skip_hh) then
       return
    end if


    call fs_init_working_vars()

    CALL DB_FS_UNIT            !------ Create the food stamp units

    DO IP = 1, CTPRHH          !------ Move the person-level information to the output variables
       L_FSUN  (IDX, KIST)%IPER(IP) = FSUN  (IP)
       L_CASHOT(IDX, KIST)%IPER(IP) = CASHOT(IP)
       L_FTSTUD(IDX, KIST)%IPER(IP) = FTSTUD(IP)
    ENDDO

    HH_FSBEN = 0               !------ Reset total household benefit


    IF (POTENTIALLY_ELIG_HH) THEN

         CALL DB_FS_VARS          !----  Create the FSU-related variables

         CALL FS_ELIGIBILITY      !----  Determine eligibility and benefit of each food stamp unit

         CALL DB_FS_PARTICIPATION !----  Determine participation of each food stamp unit

         CALL DB_FS_PROB_DISTR_TAB

         DO IP = 1, CTPRHH        !----  Move the unit-level information to the output variables

            IF (FSUN(IP) /= IP) CYCLE  ! not a FSU head

            L_FSUSIZE  (IDX, KIST)%IPER(IP) = FSUSIZE  (IP)
            L_FSTANF   (IDX, KIST)%IPER(IP) = FSTANF   (IP)
            L_FSSSI    (IDX, KIST)%IPER(IP) = FSSSI    (IP)
            L_FSGA     (IDX, KIST)%IPER(IP) = FSGA     (IP)
            L_FSALLPA  (IDX, KIST)%IPER(IP) = FSALLPA  (IP)
            L_FSEARN   (IDX, KIST)%IPER(IP) = FSEARN   (IP)
            L_FSNELDER (IDX, KIST)%IPER(IP) = FSNELDER (IP)
            L_FSNDIS   (IDX, KIST)%IPER(IP) = FSNDIS   (IP)
            L_FSNKID   (IDX, KIST)%IPER(IP) = FSNKID   (IP)
            L_FSNK5T17 (IDX, KIST)%IPER(IP) = FSNK5T17 (IP)
            L_FSNONCIT (IDX, KIST)%IPER(IP) = FSNONCIT (IP)
            L_FSNABAWD (IDX, KIST)%IPER(IP) = FSNABAWD (IP)
            L_FSNGMOM  (IDX, KIST)%IPER(IP) = FSNGMOM  (IP)
            L_FSMINBEN (IDX, KIST)%IPER(IP) = FSMINBEN (IP)

            IF (DEDTYPE(NTH) == 1) THEN
              L_FSTOTDED (IDX, KIST)%IPER(IP) = FSTOTDED_ME (IP)
              L_FSSTDDED (IDX, KIST)%IPER(IP) = FSSTDDED_ME (IP)
              L_FSERNDED (IDX, KIST)%IPER(IP) = FSERNDED_ME (IP)
              L_FSDEPDED (IDX, KIST)%IPER(IP) = FSDEPDED_ME (IP)
              L_FSMEDDED (IDX, KIST)%IPER(IP) = FSMEDDED_ME (IP)
              L_FSSLTDED (IDX, KIST)%IPER(IP) = FSSLTDED_ME (IP)
            ELSE
              L_FSTOTDED (IDX, KIST)%IPER(IP) = FSTOTDED (IP)
              L_FSSTDDED (IDX, KIST)%IPER(IP) = FSSTDDED (IP)
              L_FSERNDED (IDX, KIST)%IPER(IP) = FSERNDED (IP)
              L_FSDEPDED (IDX, KIST)%IPER(IP) = FSDEPDED (IP)
              L_FSMEDDED (IDX, KIST)%IPER(IP) = FSMEDDED (IP)
              L_FSSLTDED (IDX, KIST)%IPER(IP) = FSSLTDED (IP)
            ENDIF

            L_FSASSET  (IDX, KIST)%IPER(IP) = FSASSET  (IP)
            L_FSGRINC  (IDX, KIST)%IPER(IP) = FSGRINC  (IP)
            L_FSNETINC (IDX, KIST)%IPER(IP) = FSNETINC (IP)

            L_FSASTEST (IDX, KIST)%IPER(IP) = FSASTEST (IP)
            L_FSGRTEST (IDX, KIST)%IPER(IP) = FSGRTEST (IP)
            L_FSNETEST (IDX, KIST)%IPER(IP) = FSNETEST (IP)
            L_FSBEN    (IDX, KIST)%IPER(IP) = FSBEN    (IP)
            L_FSPART   (IDX, KIST)%IPER(IP) = FSPART   (IP)

          END DO

    END IF  !----- end categorically eligible households

    !------------------------------------------------------
    ! Tabulate the standard simulation results, first call
    !------------------------------------------------------
    CALL FS_TABLES(0)

    CALL db_fs_TABLE_B  !----  Tabulate additional simulation results.

    CALL FS_SET_DEBUG   !----  Set standard debug cases and increment debug counters

    CALL DB_FS_COUNTS   !----  Increment model-specific debug counters


    !---------------------------------------
    ! Display debug information
    !---------------------------------------
    IF (KFREQ > 0) then

       select case (model_code)
           case ("MSIP")
              if (.NOT. showstate(kist)) return
           case default
       end select

       CALL DB_FS_DISPLAY_DEBUG

       IF (POTENTIALLY_ELIG_HH) THEN
           CALL FS_DISPLAY_ELIG_DEBUG
           CALL DB_FS_DISPLAY_PARTIC_DEBUG
       END IF

       CALL FS_DISPLAY_TABLES_DEBUG

    END IF ! end of debug print



    RETURN
    END





!**************************************************************************************************
    SUBROUTINE FSTAMP3
    USE GLOBAL
    IMPLICIT NONE
    !--------------------------------------------------------------------
    ! BEGIN PROCESSING
    !--------------------------------------------------------------------

    !---- Print debug counts
    CALL DB_FS_HH_DEFINERS

    CALL DB_FS_COUNTS
    CALL FS_COUNTS

    !---- call these routines once per run
    IF (NTH == 1) then
       call fs_deallocate_generic_vars()
       CALL FS_TABLES(0)
       CALL DB_FS_DISPLAY_SUMM_DEBUG
       call db_fs_TABLE_B
    END if

    CALL DB_fs_PROB_DISTR_TAB

    !---- Make dummy calls to phase 2 routines, for easy coding of reforms:
    CALL DB_FS_UNIT             !--- dummy call (return only)
    CALL DB_FS_VARS             !--- dummy call (return only)
    CALL FS_ELIGIBILITY         !--- dummy call (return only)
    CALL DB_FS_PARTICIPATION    !--- dummy call (return only)
    CALL DB_FS_HH_DEFINERS      !--- Assign values to household definer variables

    call fs_dealloc_stats    !--- Free mem for Table stats

    RETURN
    END



   subroutine fs_init_working_vars()
!--------------------------------------------------------------
!  Ensure all generic working variables are initialized to zero
!--------------------------------------------------------------
   use global
   use fswork
   implicit none

   FSUN = 0

   FSUSIZE  = 0
   FSTANF   = 0
   FSSSI    = 0
   FSGA     = 0
   FSALLPA  = 0
   FSEARN   = 0
   FSNELDER = 0
   FSNDIS   = 0
   FSNKID   = 0
   FSNK5T17 = 0
   FSNONCIT = 0
   FSNABAWD = 0
   FSNGMOM  = 0
   FSMINBEN = 0

   FSTOTDED_ME = 0
   FSSTDDED_ME = 0
   FSERNDED_ME = 0
   FSDEPDED_ME = 0
   FSMEDDED_ME = 0
   FSSLTDED_ME = 0

   FSTOTDED = 0
   FSSTDDED = 0
   FSERNDED = 0
   FSDEPDED = 0
   FSMEDDED = 0
   FSSLTDED = 0

   FSASSET  = 0
   FSGRINC  = 0
   FSNETINC = 0

   FSASTEST = 0
   FSGRTEST = 0
   FSNETEST = 0
   FSBEN    = 0
   FSPART   = 0


   return
   end   
