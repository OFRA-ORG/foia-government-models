********************************************************************************************************************************;
********************************************************************************************************************************;
/*

This file generates the final VSR analysis file by labeling, ordering, and finalizing the naming conventions for all variables.

*/
********************************************************************************************************************************;
********************************************************************************************************************************;

%let inlib  = D:\TRANSLATE\sas_data;
%let outlib = D:\TRANSLATE\sas_data;

options nocenter ls=170 noxwait mprint compress=yes;

libname in  "&inlib.";
libname out "&outlib.";

/*----------------------------------------------------------------*/

data vsr_analysisfile;
 set in.vsr_reimbursements;

 %let addvar  = vsr_enr10k vsr_enroll_2 vsr_enroll_3 vsr_enrcat_1 vsr_enrcat_2 vsr_enrcat_3 vsr_enrcat_4 vsr_intc_rand_enroll;

 %let candvar = &addvar puborpriv vsr_numnslpschools vsr_numprovisionschools vsr_enrollmentprovision vsr_pctenrollprov vsr_freeeligtot
                vsr_rpelig vsr_perc_students_free vsr_perc_students_redp vsr_freeelignotverified vsr_perc_students_free_nonapp
                vsr_perc_students_f_f_nonapp vsr_tot_num_apps_cert vsr_freeeligcat vsr_perc_students_free_cat vsr_perc_students_f_f_cat
                vsr_freereportedprov vsr_perc_students_free_p23nby vsr_perc_students_f_f_p23nby vsr_rpreportedprov vsr_perc_students_redp_p23nby
                vsr_perc_students_r_r_p23nby vsr_freecateligapps vsr_perc_apps_free_cat vsr_perc_freeapps_free_cat vsr_freeeligincome
                vsr_freeincomeapps vsr_perc_apps_free_inc vsr_perc_freeapps_free_inc vsr_rpapps vsr_perc_apps_redp_inc
                vsr_numapps_verif vsr_numapps_verif_free vsr_numapps_verif_redpincelig vsr_perc_apps_verifd nss_stud_persch nss_asian_dst
                nss_pct_asian nss_black_dst nss_pct_black nss_hisp_dst nss_pct_hisp nss_white_dst nss_pct_white nss_migrnt nss_migrnt_perst nss_frelch_dst
                nss_frelch_pct nss_redlch_dst nss_redlch_pct nss_urban nss_suburban nss_town nss_rural nss_urban_pct nss_suburban_pct nss_town_pct
                nss_rural_pct nss_sch member tottch nus_tottch_perst nss_pk_g05_pct nss_g06_08_pct nus_leaadm nus_leaadm_perst nus_schadm
                nus_schadm_perst nus_leasup nus_leasup_perst nfs_e11 nfs_e11_perst nfs_v29v30 nfs_v29_v30_perst nfs_explaborbus_perst cd_pov5to17
                cd_povratio517 bls_unemployment_rate;

 %let basevar_fc = vsr_vmeth_rand vsr_perc_fr_aps_chng vsr_intc_rand_fr_aps_chng vsr_perc_fr_aps_no_resp vsr_intc_rand_fr_aps_no_resp
                   vsr_noapps_verif_fr vsr_perc_students_free_nonapp vsr_perc_students_free_cat vsr_enrollment vsr_perc_students_free
                   vsr_perc_students_redp;

 %let basevar_rc = vsr_vmeth_rand vsr_perc_rp_aps_chng_fr vsr_intc_rand_rp_aps_chng_fr vsr_perc_rp_aps_chng_pd vsr_intc_rand_rp_aps_chng_pd
                   vsr_perc_rp_aps_no_resp  vsr_intc_rand_rp_aps_no_resp vsr_noapps_verif_rp vsr_perc_students_free_cat
                   vsr_enrollment vsr_perc_students_free vsr_perc_students_redp;

 %let basevar_nc = vsr_vmeth_rand vsr_perc_all_aps_chng vsr_intc_rand_all_aps_chng vsr_perc_all_aps_no_resp vsr_intc_rand_all_aps_no_resp
                   vsr_noapps_verif vsr_enrollment vsr_freeeligcat vsr_enrollmentprovision;

 array setzero {*} &candvar;

 do i=1 to dim(setzero);
   if missing(setzero{i}) then setzero{i}=0;
 end;

 label vsr_enr10k                 = "vsr: enrollment expressed in 10,000s"
      vsr_enroll_2                = "vsr: enrollment squared"
      vsr_enroll_3                = "vsr: enrollment cubed"
      vsr_enrcat_1                = "vsr: enrollment<1000 "
      vsr_enrcat_2                = "vsr: 1000<=enrollment<5000 "
      vsr_enrcat_3                = "vsr: 5000<=enrollment<10,000 "
      vsr_enrcat_4                = "vsr: enrollment>10,000 "
      vsr_intc_rand_enroll        = "vsr: random verification interacted with enrollment"
      vsr_pay_free                = "vsr: total reimbursements for free meals, based on imputed meal cts"
      vsr_pay_rp                  = "vsr: total reimbursements for rp meals, based on imputed meal cts"
      vsr_pay_pd                  = "vsr: total reimbursements for paid meals, based on imputed meal cts"
      vsr_pay_total               = "vsr: total reimbursements for all meals, based on imputed meal cts"
      vsr_nlun_freeShdBePd        = "vsr: total fcne meals"
      vsr_nlun_freeShdBeRP        = "vsr: total fcre meals"
      vsr_nlun_rpShdBePd          = "vsr: total rcne meals"
      vsr_nlun_rpShdBeFree        = "vsr: total rcfe meals"
      vsr_pay_rcfe                = "vsr: total reimbursements for fcne meals"
      vsr_errpay_total            = "vsr: total reimbursements in error, excluding ncfe and ncre meals"
      vsr_numlun_free             = "vsr: total imputed free meals"
      vsr_numlun_rp               = "vsr: total imputed rp meals"
      vsr_numlun_pd               = "vsr: total imputed paid meals"
      vsr_pay_rcfe                = "vsr: dollar payments made to students RP certified, free eligible"
      vsr_pay_fcne                = "vsr: dollar payments made to students free certified, not eligible"
      vsr_pay_fcre                = "vsr: dollar payments made to students free certified, RP eligible"
      vsr_pay_rcne                = "vsr: dollar payments made to students RP certified, not eligible"
      vsr_pay_over                = "vsr: dollar amount of under payments due to administrative certification error"
      vsr_pct_ep                  = "vsr: percentage of reimbursements in error"
      vsr_pay_ep                  = "vsr: dollar amount of reimbursements in error"
      vsr_pct_fcne                = "vsr: percentage of reimbursements to fcne students"
      vsr_pct_fcre                = "vsr: percentage of reimbursements to fcre students"
      vsr_pct_rcfe                = "vsr: percentage of reimbursements to rcfe students"
      vsr_pct_rcne                = "vsr: percentage of reimbursements to rcne students"
      vsr_pct_f_fcne              = "vsr: percentage of free reimbursements to fcne students"
      vsr_pct_f_fcre              = "vsr: percentage of free reimbursements to fcre students"
      vsr_pct_r_rcfe              = "vsr: percentage of RP reimbursements to rcfe students"
      vsr_pct_r_rcne              = "vsr: percentage of RP reimbursements to rcne students"
      vsr_numapps_verif           = "vsr: number of applications verified"
      vsr_perc_all_aps_no_resp    = "vsr: percentage of verified applications with no response"
      vsr_perc_fr_aps_no_resp     = "vsr: percentage of verified free applications with no response"
      vsr_perc_rp_aps_no_resp     = "vsr: percentage of verified RP applications with no response"
      vsr_pctenrollprov           = "vsr: percentage of students enrolled in P23 school"
      vsr_perc_students_free      = "vsr: percentage of students certified for free meals"
      vsr_perc_students_redp      = "vsr: percentage of students certified for RP meals"
      vsr_perc_students_free_nonapp = "vsr: percentage of students certified free without an application"
      vsr_perc_students_f_f_nonapp  = "vsr: percentage of free certified students certified free without an application"
      vsr_perc_students_free_cat    = "vsr: percentage of students certified free categorically"
      vsr_perc_students_f_f_cat     = "vsr: percentage of free certified students certified free categorically"
      vsr_perc_students_free_p23nby = "vsr: percentage of students reported free eligible from non-base year P23 schools"
      vsr_perc_students_f_f_p23nby  = "vsr: percentage of free certified students reported free eligible from non-base year P23 schools"
      vsr_perc_students_redp_p23nby = "vsr: percentage of students reported RP eligible from non-base year P23 schools"
      vsr_perc_students_r_r_p23nby  = "vsr: percentage of RP students reported RP eligible from non-base year P23 schools"
      vsr_tot_num_apps_cert         = "vsr: total number of applications certified for free or RP meals"
      vsr_perc_apps_free_cat        = "vsr: percentage of applications certified categorically for free meals"
      vsr_perc_freeapps_free_cat    = "vsr: percentage of free applications certified categorically for free meals"
      vsr_perc_apps_free_inc        = "vsr: percentage of applications certified for free meals based on income"
      vsr_perc_freeapps_free_inc    = "vsr: percentage of free applications certified for free meals based on income"
      vsr_perc_apps_redp_inc        = "vsr: percentage of applications certified for RP meals"
      nss_stud_persch               = "ccd-ss: average number of students per school"
      nss_pct_black                 = "ccd-ss: percentage of students black"
      nss_pct_hisp                  = "ccd-ss: percentage of students hispanic"
      nss_pct_white                 = "ccd-ss: percentage of students white"
      nss_asian_dst                 = "ccd-ss: number of students asian"
      nss_black_dst                 = "ccd-ss: number of students black"
      nss_hisp_dst                  = "ccd-ss: number of students hispanid"
      nss_white_dst                 = "ccd-ss: number of students white"
      nss_migrnt_perst              = "ccd-ss: percentage of students who are migrants"
      nss_frelch_pct                = "ccd-ss: percentage of students who are certified for free meals"
      nss_redlch_pct                = "ccd-ss: percentage of students who are certified for RP meals"
      nss_urban_pct                 = "ccd-ss: percentage of schools in urban location"
      nss_suburban_pct              = "ccd-ss: percentage of schools in suburban location"
      nss_town_pct                  = "ccd-ss: percentage of schools in town location"
      nss_rural_pct                 = "ccd-ss: percentage of schools in rural location"
      nss_urban                     = "ccd-ss: schools in urban location"
      nss_suburban                  = "ccd-ss: schools in suburban location"
      nss_town                      = "ccd-ss: schools in town location"
      nss_rural                     = "ccd-ss: schools in rural location"
      nss_pk_g05_pct                = "ccd-ss: percentage of students in pre-Kindergarten through grade 5"
      nss_g06_08_pct                = "ccd-ss: percentage of students in grades 6 through 8"
      nus_leaadm                    = "ccd-us: number of LEA administrators"
      nus_schadm                    = "ccd-us: number of school administrators"
      nus_leasup                    = "ccd-us: number of LEA support staff"
      nfs_e11                       = "ccd-fs: food services spending"
      nfs_v29v30                    = "ccd-fs: food services salary spending"
      cd_povratio517                = "saipe: poverty ratio among 5-17 year olds"
      year                          = "vsr: year"
      nus_tottch_perst              = "ccd: student-teacher ratio"
      vsr_vmeth_rand                = "vsr: district selected applications for verification randomly"
      vsr_noapps_verif              = "vsr: no applications were verified"
      vsr_noapps_verif_fr           = "vsr: no free applications were verified"
      vsr_noapps_verif_rp           = "vsr: no RP applications were verified"
      vsr_perc_all_aps_chng         = "vsr: percentage of all applications changed through verification"
      vsr_perc_fr_aps_chng          = "vsr: percentage of free applications changed through verification"
      vsr_perc_rp_aps_chng_fr       = "vsr: percentage of RP applications changed to free through verification"
      vsr_perc_rp_aps_chng_pd       = "vsr: percentage of RP applications changed not certified through verification"
      vsr_perc_apps_verifd          = "vsr: percentage of applications verified"
      vsr_intc_rand_fr_aps_chng     = "vsr: interaction of random verification with percentage of free applications changed"
      vsr_intc_rand_fr_aps_no_resp  = "vsr: interaction of random verification with percentage of free applications with no response"
      vsr_intc_rand_rp_aps_chng_fr  = "vsr: interaction of random verification with percentage of RP applications changed to free"
      vsr_intc_rand_rp_aps_chng_pd  = "vsr: interaction of random verification with percentage of RP applications changed to not certified"
      vsr_intc_rand_rp_aps_no_resp  = "vsr: interaction of random verification with percentage of RP applications with no response"
      vsr_intc_rand_all_aps_chng    = "vsr: interaction of random verification with percentage of all applications changed"
      vsr_intc_rand_all_aps_no_resp = "vsr: interaction of random verification with percentage of all applications with no response"
      vsr_freecateligapps           = "vsr: number of applications certified free categorically"
      vsr_freeincomeapps            = "vsr: number of applications certified free based on income"
      vsr_numapps_verif_free        = "vsr: number of free applications verified"
      vsr_numapps_verif_redpincelig = "vsr: number of RP applications verified"
      vsr_rpreportedprov            = "vsr: number of RP students reported for non-baseyear P23 schools"
      nss_migrnt                    = "ccd-ss: number of students who are migrants"
      nss_redlch_dst                = "ccd-ss: number of students who are certified for RP meals"
      nss_frelch_dst                = "ccd-ss: number of students who are certified for free meals"
      nss_pct_asian                 = "ccd-ss: percentage of students asian"
      vsr_rpapps                    = "vsr: number of RP certified apps"
      bls_unemployment_rate         = "laus: local unemployment rate"
      cd_pov5to17                   = "saipe: number age 5-17 in poverty"
      vsr_perc_cert_inc             = "vsr: percentage of certified students that were certified based on income"
      vsr_perc_cert_cat             = "vsr: percentage of certified students that were certified categorically"
      vsr_perc_cert_nonapp          = "vsr: percentage of certified students that were certified nonapplicants"
      state                         = "vsr: state identifier"
      vsr_rpelig                    = "vsr: number of rp eligible students"
      matchid                       = "vsr-ccd match id"
      sfaid                         = "vsr: sfaid (unique within state-year)"
      leaname                       = "ccd: lea name"
      leaid                         = "ccd: lea id"
      sfaname                       = "vsr: sfa name"
      nfs_explaborbus_perst         = "ccd-fs: labor cost- salaries (v37) & bnfts (v38) -bus/central/oth, per student"
      nfs_v29_v30_perst             = "ccd-fs: labor cost- salaries (v29) & bnfts (v30) -food services, per student"
      nfs_e11_perst                 = "ccd-fs: current exp - food services (e11), per student"
      nss_sch                       = "ccd: number of schools (max of ccd-us and ccd-sus)"
      nus_leaadm_perst              = "ccd-us: lea administrators per student"
      nus_leasup_perst              = "ccd-us: lea administrators support staff per student"
      member                        = "ccd: total students (max of ccd-us and ccd-sus)"
      nus_schadm_perst              = "ccd-us: school administrators support staff per student"
      tottch                        = "ccd-us: total fte teachers (no implied decimal)"
      vsr_enrollment                = "vsr: enrollment w/access to nslp"
      vsr_enrollmentprovision       = "vsr: enrollment w/access to nslp-prov 2/3 schools"
      vsr_freeeligcat               = "vsr: # free elig kids based on categ apps"
      vsr_freeeligincome            = "vsr: # free elig kids based on income"
      vsr_freeelignotverified       = "vsr: # free elig kids not verified"
      vsr_freeeligtot               = "vsr: # total free eligible students"
      vsr_freereportedprov          = "vsr: # free elig kids reported for provision 2/3"
      vsr_numnslpschools            = "vsr: # nslp schools"
      vsr_numprovisionschools       = "vsr: # provision 2/3 schools"
      puborpriv                     = "vsr: public (1), private (2)";

 keep &candvar &basevar_nc &basevar_fc &basevar_rc vsr_pay: vsr_pct_:
      matchid sfaid sfaname leaid year state
      vsr_numlun_free vsr_numlun_rp vsr_numlun_pd leaname vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_perc_cert_inc;

 rename puborpriv         = vsr_sfa_public
        member            = nus_member
        tottch            = nus_tottch
        state             = vsr_state
        nfs_e11_perst     = nfs_fsspend_perst
        nfs_v29_v30_perst = nfs_fssalspend_perst
        nfs_e11           = nfs_fsspend
        nfs_v29v30        = nfs_fssalspend;

run;

proc sql noprint;
   select distinct name
   into : varlist separated by ' '
   from dictionary.columns
   where libname='WORK' and memname='VSR_ANALYSISFILE';
quit;

data out.vsr_analysisfile;
 length year $4. matchid 8. sfaid $35. sfaname $80. leaid $7. leaname $60.;
 retain &varlist;
 set vsr_analysisfile;

 array topcode {*} vsr_perc_all_aps_chng vsr_perc_all_aps_no_resp vsr_perc_apps_free_cat vsr_perc_apps_free_inc vsr_perc_apps_redp_inc
                   vsr_perc_apps_verifd vsr_perc_fr_aps_chng vsr_perc_fr_aps_no_resp vsr_perc_freeapps_free_cat vsr_perc_freeapps_free_inc
                   vsr_perc_rp_aps_chng_fr vsr_perc_rp_aps_chng_pd vsr_perc_rp_aps_no_resp vsr_perc_students_f_f_cat vsr_perc_students_f_f_nonapp
                   vsr_perc_students_f_f_p23nby vsr_perc_students_free vsr_perc_students_free_cat vsr_perc_students_free_nonapp vsr_perc_students_free_p23nby
                   vsr_perc_students_r_r_p23nby vsr_perc_students_redp vsr_perc_students_redp_p23nby vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_perc_cert_inc
                   nss_frelch_pct nss_g06_08_pct nss_pct_asian nss_pct_black nss_pct_hisp nss_pct_white nss_pk_g05_pct nss_redlch_pct nss_rural_pct nss_suburban_pct
                   nss_town_pct nss_urban_pct vsr_pct_ep vsr_pct_f_fcne vsr_pct_f_fcre vsr_pct_fcne vsr_pct_fcre vsr_pct_r_rcfe vsr_pct_r_rcne vsr_pct_rcfe
                   vsr_pct_rcne vsr_pctenrollprov vsr_intc_rand_all_aps_chng vsr_intc_rand_fr_aps_chng vsr_intc_rand_rp_aps_chng_fr vsr_intc_rand_rp_aps_chng_pd
                   vsr_intc_rand_all_aps_no_resp vsr_intc_rand_fr_aps_no_resp vsr_intc_rand_rp_aps_no_resp;

 do i=1 to dim(topcode);
   if topcode{i}>1 & not missing(topcode{i}) then topcode{i}=1;
   topcode{i}=topcode{i}*100;
 end;

 drop i;

run;

proc means data=out.vsr_analysisfile N NMISS MEAN;
run;

proc contents data=out.vsr_analysisfile order=varnum;
run;


endsas;










