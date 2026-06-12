!**************************************************************************************************
! Source File:  DBLOCS.F90                 
! Called By:    FSTAMP1                    
!
! Locates database-specific input variables.
!
!
!**************************************************************************************************
    subroutine db_fs_locate_vars
 
    use global
    use locvar_module
    use addvar_module
    use fsparm
    use fs_dblocs

    implicit none

 
    !--------------------------------------------------------------------
    ! BEGIN PROCESSING
    !-------------------------------------------------------------------- 
    if (nth > 1 ) return
 
    if (super_prlevel >= max_prlevel) call isnewpg (prfile, page_break_numlines)
  
    call locvar (l_fywgt              ,  'FYWGT       '  ,  9999,  'FSTAMP'   )
    call locvar (l_state              ,  'STATE       '  ,  9999,  'FSTAMP'   )
    call locvar (l_minimum_ben        ,  'MINIMUM_BEN '  ,  9999,  'FSTAMP'   )
    call locvar (l_ak_area            ,  'AK_AREA     '  ,  9999,  'FSTAMP'   )
    call locvar (l_rcntactn           ,  'RCNTACTN    '  ,  9999,  'FSTAMP'   )
    call locvar (l_yrmonth            ,  'YRMONTH     '  ,  9999,  'FSTAMP'   )

    call locvar (l_original_fsmedexp  ,  'FSMEDEXP    '  ,  9999,  'FSTAMP'   )  
    call locvar (l_original_fsdepded  ,  'HDEPDED     '  ,  9999,  'FSTAMP'   )  
    call locvar (l_original_fssltexp  ,  'FSSLTEXP    '  ,  9999,  'FSTAMP'   )
    call locvar (l_original_fscsded   ,  'FSCSDED     '  ,  9999,  'FSTAMP'   )

    call locvar (l_age                ,  'AGE         '  ,  9999,  'FSTAMP'   )
    call locvar (l_ctzn               ,  'CTZN        '  ,  9999,  'FSTAMP'   )
    call locvar (l_raceth             ,  'RACETH      '  ,  9999,  'FSTAMP'   )
    call locvar (l_emprg              ,  'EMPRG       '  ,  9999,  'FSTAMP'   )
    call locvar (l_wrkreg             ,  'WRKREG      '  ,  9999,  'FSTAMP'   )  
    call locvar (l_wages              ,  'WAGES       '  ,  9999,  'FSTAMP'   )
    call locvar (l_slfemp             ,  'SLFEMP      '  ,  9999,  'FSTAMP'   )
    call locvar (l_othern             ,  'OTHERN      '  ,  9999,  'FSTAMP'   )
    call locvar (l_ssi                ,  'SSI         '  ,  9999,  'FSTAMP'   )
    call locvar (l_tanf               ,  'TANF        '  ,  9999,  'FSTAMP'   )
    call locvar (l_ga                 ,  'GA          '  ,  9999,  'FSTAMP'   )
    call locvar (l_othgov             ,  'OTHGOV      '  ,  9999,  'FSTAMP'   )
    call locvar (l_socsec             ,  'SOCSEC      '  ,  9999,  'FSTAMP'   )
    call locvar (l_unemp              ,  'UNEMP       '  ,  9999,  'FSTAMP'   )
    call locvar (l_vet                ,  'VET         '  ,  9999,  'FSTAMP'   )
    call locvar (l_wcomp              ,  'WCOMP       '  ,  9999,  'FSTAMP'   )
    call locvar (l_edloan             ,  'EDLOAN      '  ,  9999,  'FSTAMP'   )
    call locvar (l_csuprt             ,  'CSUPRT      '  ,  9999,  'FSTAMP'   )
    call locvar (l_deem               ,  'DEEM        '  ,  9999,  'FSTAMP'   )
    call locvar (l_cont               ,  'CONT        '  ,  9999,  'FSTAMP'   )
    call locvar (l_othun              ,  'OTHUN       '  ,  9999,  'FSTAMP'   )
    call locvar (l_diver              ,  'DIVER       '  ,  9999,  'FSTAMP'   )  
    call locvar (l_eitc               ,  'EITC        '  ,  9999,  'FSTAMP'   )  
    call locvar (l_foster             ,  'FOSTER      '  ,  9999,  'FSTAMP'   )  
    call locvar (l_fsafil             ,  'FSAFIL      '  ,  9999,  'FSTAMP'   )
    call locvar (l_sex                ,  'SEX         '  ,  9999,  'FSTAMP'   )
    call locvar (l_rel                ,  'REL         '  ,  9999,  'FSTAMP'   )
    call locvar (l_dis                ,  'DIS         '  ,  9999,  'FSTAMP'   )  
    call locvar (l_ndisca             ,  'NDISCA      '  ,  9999,  'FSTAMP'   )  

    call locvar (l_original_fsun      ,  'FSUN       1'  ,  9999,  'FSTAMP'   )  
    call locvar (l_original_fsusize   ,  'FSUSIZE    1'  ,  9999,  'FSTAMP'   )  
    call locvar (l_original_fsnkid    ,  'FSNKID     1'  ,  9999,  'FSTAMP'   )  
    call locvar (l_original_fsnelder  ,  'FSNELDER   1'  ,  9999,  'FSTAMP'   )  
    call locvar (l_original_fsndis    ,  'FSNDIS     1'  ,  9999,  'FSTAMP'   )  
    call locvar (l_original_fsasset   ,  'FSASSET    1'  ,  9999,  'FSTAMP'   )  
    call locvar (l_original_fsben     ,  'FSBEN      1'  ,  9999,  'FSTAMP'   )  

    call locvar (l_exfscsded          ,  'EXFSCSDED   '  ,  9999,  'FSTAMP'   )
    call locvar (l_wgesup             ,  'WGESUP      '  ,  9999,  'FSTAMP'   )
    call locvar (l_energy             ,  'ENERGY      '  ,  9999,  'FSTAMP'   )
    call locvar (l_cat_elig           ,  'CAT_ELIG    '  ,  9999,  'FSTAMP'   )
    call locvar (l_pure_pa            ,  'PURE_PA     '  ,  9999,  'FSTAMP'   )
    call locvar (l_mn_fip             ,  'MN_FIP      '  ,  9999,  'FSTAMP'   )
    call locvar (l_ssi_cap            ,  'SSI_CAP     '  ,  9999,  'FSTAMP'   )

    call locvar (l_rent               ,  'RENT        '  ,  9999,  'FSTAMP'   )
    call locvar (l_util               ,  'UTIL        '  ,  9999,  'FSTAMP'   )

    call locvar (l_homeded            ,  'HOMEDED     '  ,  9999,  'FSTAMP'   )
    call locvar (l_homelsded          ,  'HOMELSDED   '  ,  9999,  'FSTAMP'   )

    call locvar (l_med_ded_demo       ,  'MED_DED_DEMO'  ,  9999,  'FSTAMP'   )  

    call locvar (l_dpcost             ,  'DPCOST      '  ,  9999,  'FSTAMP'   )
    call locvar (l_fsvehast1          ,  'FSVEHAST    '  ,  9999,  'FSTAMP'   )
    call locvar (l_fsnongr            ,  'FSNONGR     '  ,  9999,  'FSTAMP'   )  

    return
    end

