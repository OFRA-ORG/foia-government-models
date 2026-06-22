********************************************************************************************************************************;
********************************************************************************************************************************;
/*

This file generates the variables to be used with the statistical models of certification error.

*/
********************************************************************************************************************************;
********************************************************************************************************************************;

%let inlib  = D:\TRANSLATE\sas_data;
%let outlib = D:\TRANSLATE\sas_data;

options nocenter ls=170 noxwait mprint nolabel compress=yes;

libname in  "&inlib.";
libname out "&outlib.";

/*----------------------------------------------------------------*/

data vsr_constructs;
 set in.vsr_imputation;

 i_enrollment   = imp_enrollment;
 i_p_free_red   = imp_p_free_red;
 i_p_non_income = imp_p_non_income;

 %let imputevars = member numsch pmigrant pell staff1 staff2 staff3 tottch expfoodtot expbustot expfoodlabor expbuslabor
                   psch_level1 psch_level2 psch_level3 psch_level4 psch_level5 plevel3 plevel2 plevel1 plevel5 plevel4
                   psch_charter pchart pst1 pt1 psch_st1 psch_t1 pfrp pfree prp psch_local1 psch_local4 psch_local5
                   psch_local2 psch_local3 psch_local6 plocal1 plocal2 plocal3 plocal4 plocal5 plocal6 race1 race2 race3 race4 race5
                   unemp typeofverif numnslpschools numprovisionschools enrollment enrollmentprovision puborpriv freeeligtot freeeligcat
                   freeeligincome freeelignotverified freereportedprovision freecatappstu freeincappstu rpelig rpappstu rpprovstu randverif
                   a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10;

 array imputevars {*} &imputevars;

 %let imputevars_h =    member_h numsch_h pmigrant_h pell_h staff1_h staff2_h staff3_h tottch_h expfoodtot_h expbustot_h expfoodlabor_h expbuslabor_h
                        psch_level1_h psch_level2_h psch_level3_h psch_level4_h psch_level5_h plevel3_h plevel2_h plevel1_h plevel5_h plevel4_h
                        psch_charter_h pchart_h pst1_h pt1_h psch_st1_h psch_t1_h pfrp_h pfree_h prp_h psch_local1_h psch_local4_h psch_local5_h
                        psch_local2_h psch_local3_h psch_local6_h plocal1_h plocal2_h plocal3_h plocal4_h plocal5_h plocal6_h race1_h race2_h race3_h race4_h race5_h
                        unemp_h typeofverif_h numnslpschools_h numprovisionschools_h enrollment_h enrollmentprovision_h puborpriv_h freeeligtot_h freeeligcat_h
                        freeeligincome_h freeelignotverified_h freereportedprovision_h freecatappstu_h freeincappstu_h rpelig_h rpappstu_h rpprovstu_h randverif_h
                        a1_h a2_h a3_h a4_h a5_h a6_h a7_h a8_h a9_h a10_h b1_h b2_h b3_h b4_h b5_h b6_h b7_h b8_h b9_h b10_h c1_h c2_h c3_h c4_h c5_h c6_h c7_h c8_h c9_h c10_h;

 array imputevars_h {*} &imputevars_h;

 do i=1 to dim(imputevars);
   imputevars{i} = imputevars_h{i};
 end;

 drop &imputevars_h imp_:;

 keep year state matchid sfaname leaname leaid sfaid enrollment enrollment_cat p_free_red p_free_red_cat randverif &imputevars. pop5to17 pov5to17 i_:;

run;

data out.vsr_constructs;
 set vsr_constructs;

 yr = input(year,4.);

 **************************************************;
 * Generating Count Variables from Imputed Percentages;

 migrnt = pmigrant*member;
 ell    = pell*member;

 level1 = psch_level1*numsch;
 level2 = psch_level2*numsch;
 level3 = psch_level3*numsch;
 level4 = psch_level4*numsch;
 level5 = psch_level5*numsch;

 charter = psch_charter*numsch;

 st1 = psch_st1*numsch;
 t1  = psch_t1*numsch;

 totfrl = pfrp*member;
 frelch = pfree*member;
 redlch = prp*member;

 local1 = psch_local1*numsch;
 local2 = psch_local2*numsch;
 local3 = psch_local3*numsch;
 local4 = psch_local4*numsch;
 local5 = psch_local5*numsch;

 freecateligapps = freecatappstu*freeeligcat;
 freeincomeapps = freeincappstu*freeeligincome;

 rpapps = rpappstu*rpelig;
 rpreportedprovision = rpprovstu*rpelig;

**************************************************;
* VSR variable recoding and new variable construction;

puborpriv=1;

if enrollment=0 then enrollment = member;
if member=0     then member = enrollment;

* Method of Selecting Verification Sample;

if yr<2007  & not missing(typeofverif) then randverif=(typeofverif=1);
if yr>=2007 & not missing(typeofverif) then randverif=(typeofverif=2);

* Total Number of Applications Verified;

vsr_numapps_verif_freecatelig = a1+a3+a5+a7;
vsr_numapps_verif_freeincelig = b1+b3+b5+b7;
vsr_numapps_verif_free = vsr_numapps_verif_freecatelig+vsr_numapps_verif_freeincelig;
vsr_numapps_verif_redpincelig = c1+c3+c5+c7;
vsr_numapps_verif = vsr_numapps_verif_free + vsr_numapps_verif_redpincelig;

vsr_numapps = freecateligapps + freeincomeapps + rpapps;
if vsr_numapps~=0 then vsr_perc_apps_verifd = vsr_numapps_verif/vsr_numapps;

* Percentage Distribution of Applications Selected for Verification by Type;
  * Creates missing values because denominators are zero in some cases;

if vsr_numapps_verif~=0 then vsr_perc_appfree_cat_elig = vsr_numapps_verif_freecatelig/vsr_numapps_verif;
if vsr_numapps_verif~=0 then vsr_perc_appfree_inc_elig = vsr_numapps_verif_freeincelig/vsr_numapps_verif;
if vsr_numapps_verif~=0 then vsr_perc_appredp_inc_elig = vsr_numapps_verif_redpincelig/vsr_numapps_verif;

* Apps not responding ;
  * Creates missing values because denominators are zero in some cases;

if vsr_numapps_verif~=0 then vsr_perc_all_aps_no_resp = (a7+b7+c7)/vsr_numapps_verif;
if vsr_numapps_verif_free~=0 then vsr_perc_fr_aps_no_resp = (a7+b7)/vsr_numapps_verif_free;
if vsr_numapps_verif_redpincelig ~=0 then vsr_perc_rp_aps_no_resp = c7/vsr_numapps_verif_redpincelig;
if vsr_numapps_verif_redpincelig=0 & vsr_numapps_verif~=0 then vsr_perc_rp_aps_no_resp = 0;

* Verification Results;

if vsr_numapps_verif_free~=0 then vsr_perc_fr_aps_chng =(a3+a5+a7+b3+b5+b7)/vsr_numapps_verif_free;
if vsr_numapps_verif_free~=0 then vsr_perc_fr_aps_chng_rp =(a3+b3)/vsr_numapps_verif_free;
if vsr_numapps_verif_free~=0 then vsr_perc_fr_aps_chng_pd =(a5+b5)/vsr_numapps_verif_free;

if vsr_numapps_verif_redpincelig~=0 then vsr_perc_rp_aps_chng =(c3+c5+c7)/vsr_numapps_verif_redpincelig;
if vsr_numapps_verif_redpincelig~=0 then vsr_perc_rp_aps_chng_fr =c3/vsr_numapps_verif_redpincelig;
if vsr_numapps_verif_redpincelig~=0 then vsr_perc_rp_aps_chng_pd =c5/vsr_numapps_verif_redpincelig;

vsr_noapps_verif    = (vsr_numapps_verif=0);
vsr_noapps_verif_fr = (vsr_numapps_verif_free=0);
vsr_noapps_verif_rp = (vsr_numapps_verif_redpincelig=0);

if vsr_noapps_verif=1    then vsr_perc_all_aps_no_resp=0;
if vsr_noapps_verif_fr=1 then vsr_perc_fr_aps_no_resp=0;
if vsr_noapps_verif_rp=1 then vsr_perc_rp_aps_no_resp=0;

if vsr_numapps_verif~=0 then vsr_perc_all_aps_chng = (a3+a5+b3+b5+c3+c5)/vsr_numapps_verif;
if vsr_noapps_verif=1   then vsr_perc_all_aps_chng = 0;

if vsr_numapps_verif_free~=0 then vsr_perc_fr_aps_chng = (a3+a5+b3+b5)/vsr_numapps_verif_free;
if vsr_noapps_verif_fr=1 then vsr_perc_fr_aps_chng = 0;

if vsr_numapps_verif_free~=0 then vsr_perc_fr_aps_chng_rp = (a3+b3)/vsr_numapps_verif_free;
if vsr_noapps_verif_fr=1 then vsr_perc_fr_aps_chng_rp = 0;

if vsr_numapps_verif_free~=0 then vsr_perc_fr_aps_chng_pd = (a5+b5)/vsr_numapps_verif_free;
if vsr_noapps_verif_fr=1 then vsr_perc_fr_aps_chng_pd = 0;

if vsr_numapps_verif_redpincelig~=0 then vsr_perc_rp_aps_chng =(c3+c5)/vsr_numapps_verif_redpincelig;
if vsr_noapps_verif_rp=1 then vsr_perc_rp_aps_chng = 0;

if vsr_numapps_verif_redpincelig~=0 then vsr_perc_rp_aps_chng_fr = c3/vsr_numapps_verif_redpincelig;
if vsr_noapps_verif_rp=1 then vsr_perc_rp_aps_chng_fr = 0;

if vsr_numapps_verif_redpincelig~=0 then vsr_perc_rp_aps_chng_pd =c5/vsr_numapps_verif_redpincelig;
if vsr_noapps_verif_rp=1 then vsr_perc_rp_aps_chng_pd = 0;

vsr_intc_rand_fr_aps_chng     = vsr_perc_fr_aps_chng*randverif;
vsr_intc_rand_fr_aps_no_resp  = vsr_perc_fr_aps_no_resp*randverif;
vsr_intc_rand_rp_aps_chng_fr  = vsr_perc_rp_aps_chng_fr*randverif;
vsr_intc_rand_rp_aps_chng_pd  = vsr_perc_rp_aps_chng_pd*randverif;
vsr_intc_rand_rp_aps_no_resp  = vsr_perc_rp_aps_no_resp*randverif;
vsr_intc_rand_all_aps_chng    = vsr_perc_all_aps_chng*randverif;
vsr_intc_rand_all_aps_no_resp = vsr_perc_all_aps_no_resp*randverif;

* Certification characteristics;

if rpelig+freeeligtot~=0 then vsr_perc_cert_nonapp = freeelignotverified/(rpelig+freeeligtot);
if missing(vsr_perc_cert_nonapp) then vsr_perc_cert_nonapp = 0;

if rpelig+freeeligtot~=0 then vsr_perc_cert_cat=freeeligcat/(rpelig+freeeligtot);
if missing(vsr_perc_cert_cat)    then vsr_perc_cert_cat = 0;

if rpelig+freeeligtot~=0 then vsr_perc_cert_inc=(freeeligincome+rpelig)/(rpelig+freeeligtot);
if missing(vsr_perc_cert_inc)    then vsr_perc_cert_inc = 0;

************************************************************************************************;
* LEA characteristics in VSR;

if numnslpschools~=0 then vsr_pctprovisionschools = numprovisionschools/numnslpschools;
if enrollment~=0     then vsr_pctenrollprov = enrollmentprovision/enrollment;

/* Percentage of enrolled students certified free eligible */
if enrollment~=0 then vsr_perc_students_free = freeeligtot/enrollment;

/* Percenteage of Enrolled Students Certified Reduced Price */
if enrollment~=0 then vsr_perc_students_redp = rpelig/enrollment;

/* Percentage of enrolled students certified free eligible by method */

if enrollment~=0  then vsr_perc_students_free_nonapp = freeelignotverified/enrollment;
if freeeligtot~=0 then vsr_perc_students_f_f_nonapp = freeelignotverified/freeeligtot;

if enrollment~=0  then vsr_perc_students_free_cat = freeeligcat/enrollment;
if freeeligtot~=0 then vsr_perc_students_f_f_cat = freeeligcat/freeeligtot;

/* Percentage of enrolled students certified free in P23 non-base year schools */
if enrollment~=0  then vsr_perc_students_free_p23nby = freereportedprovision/enrollment;
if freeeligtot~=0 then vsr_perc_students_f_f_p23nby = freereportedprovision/freeeligtot;

/* Percenteage of Enrolled Students Certified Reduced Price in P23 non-base year schools*/
if enrollment~=0 then vsr_perc_students_redp_p23nby = rpreportedprovision/enrollment;
if rpelig~=0     then vsr_perc_students_r_r_p23nby = rpreportedprovision/rpelig;

/* Total Number of Applications Certified */
vsr_tot_num_apps_cert = sum(freecateligapps,freeincomeapps,rpapps);
vsr_tot_free_apps_cert = sum(freecateligapps,freeincomeapps);

/* Percentage of Applications Certified by Type */
if vsr_tot_num_apps_cert~=0 then vsr_perc_apps_free_cat = freecateligapps/vsr_tot_num_apps_cert;
if vsr_tot_free_apps_cert~=0 then vsr_perc_freeapps_free_cat = freecateligapps/vsr_tot_free_apps_cert;

if vsr_tot_num_apps_cert~=0 then vsr_perc_apps_free_inc = freeincomeapps/vsr_tot_num_apps_cert;
if vsr_tot_free_apps_cert~=0 then vsr_perc_freeapps_free_inc = freeincomeapps/vsr_tot_free_apps_cert;

if vsr_tot_num_apps_cert~=0 then vsr_perc_apps_redp_inc = rpapps/vsr_tot_num_apps_cert;

*************************************************;
** CCD variables;

if numsch~=0 then nss_stud_persch = member/numsch;

nss_pct_asian = race2/100;
nss_pct_black = race3/100;
nss_pct_hisp = race4/100;
nss_pct_white = race5/100;

nss_asian_dst = nss_pct_asian*member;
nss_black_dst = nss_pct_black*member;
nss_hisp_dst = nss_pct_hisp*member;
nss_white_dst = nss_pct_white*member;
nss_migrnt_perst = pmigrant/100;

if member~=0 then nss_frelch_pct = frelch/member;
if member~=0 then nss_redlch_pct = redlch/member;

************** construct based on # of schools (local1-6) or % of enrollment;

nss_urban_pct    = (plocal1 + plocal2)/100;
nss_suburban_pct = plocal3/100;
nss_town_pct     = plocal4/100;
nss_rural_pct    = plocal5/100;

nss_urban    = local1 + local2;
nss_suburban = local3;
nss_town     = local4;
nss_rural    = local5;

nss_pk_g05_pct = plevel1/100;
nss_g06_08_pct = plevel2/100;

nfs_e11 = expfoodtot*member;
nfs_v29v30 = expfoodlabor*member;
nfs_exptotbus = expbustot*member;
nfs_explaborbus = expbuslabor*member;

*************************************************;
* OTHER (SAIPE and LAUS);

if pop5to17~=0 then pctpov5to17 = pov5to17/pop5to17;
cd_povratio517 = pctpov5to17/100;

rename numnslpschools        = vsr_numnslpschools
       numprovisionschools   = vsr_numprovisionschools
       enrollment            = vsr_enrollment
       enrollmentprovision   = vsr_enrollmentprovision
       freeeligtot           = vsr_freeeligtot
       rpelig                = vsr_rpelig
       freeelignotverified   = vsr_freeelignotverified
       freeeligcat           = vsr_freeeligcat
       freereportedprovision = vsr_freereportedprov
       rpreportedprovision   = vsr_rpreportedprov
       freecateligapps       = vsr_freecateligapps
       freeeligincome        = vsr_freeeligincome
       freeincomeapps        = vsr_freeincomeapps
       rpapps                = vsr_rpapps
       migrnt                = nss_migrnt
       frelch                = nss_frelch_dst
       redlch                = nss_redlch_dst
       numsch                = nss_sch
       staff1                = nus_leaadm_perst
       staff2                = nus_leasup_perst
       staff3                = nus_schadm_perst
       expfoodtot            = nfs_e11_perst
       expfoodlabor          = nfs_v29_v30_perst
       expbustot             = nfs_exptotbus_perst
       expbuslabor           = nfs_explaborbus_perst
       unemp                 = bls_unemployment_rate
       pop5to17              = cd_estimated_population_5_17
       pov5to17              = cd_pov5to17
       randverif             = vsr_vmeth_rand;

run;

proc means data=out.vsr_constructs N MEAN;
run;

proc contents data=out.vsr_constructs;
run;

ENDSAS;



