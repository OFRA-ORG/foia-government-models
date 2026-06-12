module tallywork

    USE global
    implicit none

    TYPE(ihhld8_ptr) :: l_hhid
    TYPE(ihhld_ptr)  :: l_hdepded, l_hhid4, L_homelsded
    TYPE(ihhld_ptr)  :: l_hfsndis, l_hfsnoncit
    TYPE(iper_ptr)  :: &
       L_X_MN_FIP      &
      ,L_X_SSI_CAP     &
      ,L_X_CAT_ELIG    &
      ,L_X_TANF_IND    &
      ,L_X_WRK_POOR    &
      ,L_X_EXFSCSDED   &
      ,L_X_FSUNEARN

    TYPE(iHHLD_ptr)  :: &
       L_MN_FIP      &
      ,L_SSI_CAP     &
      ,L_CAT_ELIG    &
      ,L_TANF_IND    &
      ,L_WRK_POOR    &
      ,L_EXFSCSDED   &
      ,L_FSUNEARN    &
      ,l_pure_Pa

    TYPE(iper_ptr)   :: l_fsdepded, l_fsun, l_fsusize,  l_fsndis, l_fsallpa
    TYPE(iper_ptr)   :: l_fsafil, l_ndisca
    TYPE(iper_ptr)   :: l_eitc, l_age, l_dis
    TYPE(iper_ptr)   :: l_fsastest

    TYPE(iper_ptr)   :: l_ctzn
    TYPE(iper_ptr)   :: l_fsnoncit
    TYPE(iper_ptr)   :: l_fsnabawd
    TYPE(iper_ptr)   :: l_fsnongr

    TYPE(iper_ptr)   :: l_fsnetinc1, l_fsben1, l_fsminben1
    TYPE(iper_ptr)   :: l_fsnetinc2, l_fsben2, l_fsminben2

    TYPE(iper_ptr)   :: l_fsGRINC1, l_FSGRINC2
    TYPE(iper_ptr)   :: l_fsMEDDED1, l_FSMEDDED2


    TYPE(per_ptr)    :: l_seedp

    TYPE(HHLD_PTR)   :: FYWGT
    TYPE(IHHLD_PTR)  :: L_STRATUM, L_YRMONTH, L_STATE


    INTEGER ::  idx = 0

    INTEGER ::  NBR_WA = 0
    INTEGER ::  NBR_KY = 0
    INTEGER ::  NBR_LA = 0
    INTEGER ::  NBR_PA = 0
    INTEGER ::  NBR_VA = 0

    INTEGER ::  NBR_SC = 0
    INTEGER ::  NBR_MS = 0

    INTEGER :: CAP_IDX, ST_IDX
    INTEGER, DIMENSION(80, 5) :: NBR_CAP = 0


    INTEGER ::  NBR_HL = 0
    INTEGER ::  NBR_eitc = 0
    INTEGER ::  NBR_noncit = 0
    INTEGER ::  NBR_abawd  = 0
    INTEGER ::  NBR_dis = 0
    INTEGER ::  NBR_noncit_diff = 0
    INTEGER ::  NBR_disab_diff = 0



    INTEGER, DIMENSION(20) :: nbr_fip = 0
    
end module



subroutine tally()
    USE global
    implicit none

    if (nth == 1) then
       call tally1()
    end if

end subroutine

    
subroutine tally1()
    USE global
    USE tallywork
    USE locvar_module
    USE addvar_module

    implicit none

    INTEGER :: iunit,  ip, i, J
    INTEGER :: in_tally = 0

    integer, dimension(max_persons) :: fsnoncit, fsnabawd, fsndis

    select case (keof)
       case(1)
          call locate_vars
       case(2)
          call compute_assets
       case(3)
          call output_tally
       case default
    end select
    return


 contains
  subroutine locate_vars 

    call locvar(l_hdepded,  "HDEPDED     ", 9999, "TALLY" )
    call locvar(l_hfsnoncit,"HFSNONCIT   ", 9999, "TALLY" )
    call locvar(l_hfsndis,  "HFSNDIS     ", 9999, "TALLY" )

    call locvar(l_fsun,     "FSUN       1", 9999, "TALLY" )
    call locvar(l_fsastest, "FSASTEST   1", 9999, "TALLY" )

    call addvar(l_SEEDP,    "SEEDP       ", 4,    "MINIQC01" )
    call addvar(l_fsallpa,  "FSALLPA    1", 1,    "FSTAMP" )
    call addvar(l_fsnoncit, "FSNONCIT   1", 1,    "FSTAMP" )
    call addvar(l_fsnabawd, "FSNABAWD   1", 1,    "FSTAMP" )
    call addvar(l_fsndis,   "FSNDIS     1", 1,    "FSTAMP" )
    
    call addvar(l_fsnongr,  "FSNONGR     ", 1,    "FSTAMP" )   
    call locvar(l_age,      "AGE         ", 9999, "TALLY" )
    
    CALL LOCVAR(L_stratum,  "STRATUM     ", 9999, "TALLY")
    CALL LOCVAR(L_YRMONTH,  "YRMONTH     ", 9999, "TALLY")
    CALL LOCVAR(L_STATE,    "STATE       ", 9999, "TALLY")
    CALL LOCVAR(L_homelsded,"HOMELSDED   ", 9999, "TALLY")

    CALL LOCVAR(L_pure_pa,  "PURE_PA     ", 9999, "TALLY")
    CALL LOCVAR(L_SSI_CAP,  "SSI_CAP     ", 9999, "TALLY")

    call addvar(l_fsdepded, "FSDEPDED   1", 4,    "FSTAMP" )

    CALL LOCVAR(L_eitc,     "EITC        ", 9999, "TALLY")
    CALL LOCVAR(L_ctzn,     "CTZN        ", 9999, "TALLY")
    CALL LOCVAR(L_dis,      "DIS         ", 9999, "TALLY")

    CALL LOCVAR(L_fsafil,   "FSAFIL      ", 9999, "TALLY")
    CALL LOCVAR(L_ndisca,   "NDISCA      ", 9999, "TALLY")

    call locvar(l_mn_fip,   "MN_FIP      ", 9999, "TALLY" )
    call locvar(l_fsusize,  "FSUSIZE    1", 9999, "TALLY" )


  end subroutine locate_vars


  subroutine compute_assets
  
    use utils

    in_tally = in_tally + 1

    call generate_seeds()

    l_fsallpa%iper = 0

    fsnoncit = 0
    fsnabawd = 0
    fsndis = 0

    do iunit = 1, ctprhh
       l_fsdepded%iper(iUNIT) = 0
       l_fsnoncit%iper(iunit) = 0
       l_fsnabawd%iper(iunit) = 0
       l_fsndis%iper(iunit) = 0
       l_fsnongr%iper(iunit) = 0

       if (l_fsun%iper(iUNIT) /= iunit) cycle

       if (l_hdepded%ihhld > 0) then
          l_fsdepded%iper(iUNIT) = l_hdepded%ihhld
       end if


       l_fsastest%iper(iunit) = 1   

       if (l_pure_pa%ihhld == 1) l_fsallpa%iper(iunit) = 1

       do ip = 1, ctprhh
             
          if ((l_fsafil%iper(ip) == 8 .OR. l_fsafil%iper(ip) == 9 .OR.  l_fsafil%iper(ip) == 11 .OR. l_fsafil%iper(ip) == 13) &
             .AND. (l_dis%iper(ip) == 1 .OR. l_age%iper(ip) > 59)) then
                l_fsnongr%iper(iunit) = 1
          end if       
                
          if (l_fsun%iper(ip) /= iunit) cycle
          
          if (l_ctzn%iper(ip) > 2) fsnoncit(iunit) = fsnoncit(iunit) + 1
          if (l_dis %iper(ip) == 1) fsndis(iunit) = fsndis(iunit) + 1

          if (l_NDISCA%iper(ip) == 1 .AND. l_fsafil%iper(ip) == 1) fsnabawd(iunit) = fsnabawd(iunit) + 1
          
       end do

       l_fsnoncit%iper(iunit) = fsnoncit(iunit)
       l_fsnabawd%iper(iunit) = fsnabawd(iunit)
       l_fsndis%iper(iunit) = fsndis(iunit)
    
    end do


    do iunit = 1, ctprhh
      if (l_fsun%iper(iUNIT) /= iunit) cycle

      if (l_fsnoncit%iper(iunit) /= l_hfsnoncit%ihhld) then 
         nbr_noncit_diff = nbr_noncit_diff + 1 
         if (nbr_noncit_diff <= debugnbr) then 
            call debug_msg("Found Nbr Noncit diff", nbr_noncit_diff) 
         end if
      end if

      if (l_fsndis%iper(iunit) /= l_hfsndis%ihhld) then 
         nbr_disab_diff = nbr_disab_diff + 1 
         if (nbr_disab_diff <= debugnbr) then 
            call debug_msg("Found Nbr Disab diff", nbr_disab_diff) 
         end if
      end if
          

    end do


    do ip = 1, ctprhh

       if (l_eitc%iper(ip) > 0) then
          nbr_eitc = nbr_eitc + 1
          if (nbr_eitc <= debugnbr) then
             call debug_msg("With EITC in household", nbr_eitc)
          end if
       end if

       if (l_ctzn%iper(ip) > 2) then
          nbr_noncit = nbr_noncit + 1
          if (nbr_noncit <= debugnbr) then
             call debug_msg("Found Noncitizen in household", nbr_noncit)
          end if
       end if

       if (l_ndisca%iper(ip) == 1 .and. l_fsafil%iper(ip) == 1) then
          nbr_abawd  = nbr_abawd  + 1
          if (nbr_abawd  <= debugnbr) then
             call debug_msg("Found ABAWD in unit", nbr_abawd)
          end if
       end if

       if (l_dis %iper(ip) == 1) then
          nbr_dis = nbr_dis + 1
          if (nbr_dis <= debugnbr+10) then
             call debug_msg("Found disabled in household", nbr_dis)
          end if
       end if

    end do


    if (in_tally <= debugnbr) then
       call debug_msg("1st few HHLDS", in_tally)
    end if


     if (l_mn_fip%ihhld == 1) then
        do iunit = 1, ctprhh
           if (l_fsun%iper(iunit) /= iunit) cycle
           i = l_fsusize%iper(iunit)
           nbr_fip(i) = nbr_fip(i) + 1
           nbr_fip(20) = nbr_fip(20) + 1
        end do

     end if


     IF (L_STATE%IHHLD == 21 .and. L_SSI_CAP%Ihhld > 0) THEN
        NBR_ky = NBR_ky + 1
        IF (NBR_ky <= debugnbr) THEN
           CALL DEBUG_MSG("KY SSI_CAP HH", NBR_KY)
        END IF
     END IF

     IF (L_STATE%IHHLD == 22 .and. L_SSI_CAP%Ihhld > 0) THEN
        NBR_la = NBR_la + 1
        IF (NBR_LA <= debugnbr) THEN
           CALL DEBUG_MSG("LA SSI_CAP HH", NBR_LA)
        END IF
     END IF

     IF (L_STATE%IHHLD == 42 .and. L_SSI_CAP%Ihhld > 0) THEN
        NBR_PA = NBR_PA + 1
        IF (NBR_PA <= debugnbr) THEN
           CALL DEBUG_MSG("PA SSI_CAP HH", NBR_PA)
        END IF
     END IF

     IF (L_STATE%IHHLD == 51 .and. L_SSI_CAP%Ihhld > 0) THEN
        NBR_VA = NBR_VA + 1
        IF (NBR_VA <= debugnbr) THEN
           CALL DEBUG_MSG("VA SSI_CAP HH", NBR_VA)
        END IF
     END IF

     IF (L_STATE%IHHLD == 45 .and. L_SSI_CAP%Ihhld > 0) THEN
        NBR_SC = NBR_SC + 1
        IF (NBR_SC <= debugnbr) THEN
           CALL DEBUG_MSG("SC SSI_CAP HH", NBR_SC)
        END IF
     END IF

     IF (L_STATE%IHHLD == 28 .and. L_SSI_CAP%Ihhld > 0) THEN
        NBR_MS = NBR_MS + 1
        IF (NBR_MS <= debugnbr) THEN
           CALL DEBUG_MSG("MS SSI_CAP HH", NBR_MS)
        END IF
     END IF



     IF (L_SSI_CAP%Ihhld > 0) THEN
        CAP_IDX = L_SSI_CAP%Ihhld
        ST_IDX = L_STATE%Ihhld
        NBR_CAP(ST_IDX, CAP_IDX) = NBR_CAP(ST_IDX, CAP_IDX) + 1
     END IF




    IF (L_HOMELSDED%IHHLD > 0) then
       NBR_HL = NBR_HL + 1
       IF (NBR_HL <= 5) THEN
          CALL DEBUG_MSG("HOMELESS DED", NBR_HL)
       END IF
    END IF

  end subroutine compute_assets


  subroutine output_tally

     WRITE(PRFILE, 3010) &
        NBR_WA        &
       ,NBR_HL        &
       ,NBR_noncit_diff &
       ,NBR_disab_diff

 3010  FORMAT(/,"  NBR WA SSI_CAP =      ", I10  &
             ,/,"  NBR W/ HOMELESS DED = ", I10  &
             ,/,"  NBR W/ NBR NONCIT DIFF = ", I10  &
             ,/,"  NBR W/ NBR DISAB DIFF  = ", I10  &
           )

     WRITE(PRFILE, 3020)
     DO I = 1, 80
        WRITE(PRFILE, 3030) I, (NBR_CAP(I, J), J = 1, 5)
     END DO

 3020  FORMAT(//, T2, "SSI_CAP SUMMARY"  &
   ,//, T2, "STATE", T10, "NBR CAP"     &
   , /, T2, "-----", T10, "-------"     )

 3030  FORMAT(T2, I5, T10, 5I5)

     WRITE(prfile, 4000)
     do i = 1, 20
        WRITE(prfile, 4010) i, nbr_fip(i)
     end do

 4000  FORMAT(//, t2, "MN_FIP Summary: "  &
  ,//, t2, "Size", 5x, "  Nbr"   &
  ,/,  t2, "----", 5x, "  ---"   )
 4010  FORMAT(t2, i4, 5x, i5)

  end subroutine output_tally

 end subroutine tally1


 SUBROUTINE generate_seeds

      use global
      USE tallywork

      IMPLICIT NONE

      INTEGER :: i, J, M, IEXP, ISEED, DIGIT
      integer :: PSEED = 57697
      integer :: NUM_DIGITS = 7
      REAL ::  X



      DO J = 1,CTPRHH
         IEXP = 1
         ISEED = 0

         DO I = 1,NUM_DIGITS
210         PSEED  = PSEED * 65539

            select case (pseed)
               case (0:)
                  GOTO 230
               case default
                  GOTO 220
            end select

220         PSEED = PSEED + 2147483647 + 1
230         X = PSEED
            X = X * 0.4656613E-9
            IF (X .GE. 1.) GO TO 210
            DIGIT= int(X * 10.)
            ISEED =  ISEED + (DIGIT * IEXP)
            IEXP = IEXP*10
         ENDDO

         M = (ISEED/2)*2
         IF(M .EQ. ISEED) ISEED = ISEED + 1
         L_SEEDP%per(j) = Real(iseed)

      ENDDO


 END subroutine generate_seeds

