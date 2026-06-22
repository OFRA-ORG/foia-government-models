********************************************************************************************************************************;
********************************************************************************************************************************;
/*

This file applies the statistical models of overall certification error derived from APEC to the VSR analysis file. This analysis
is used to classify districts' risk of certification error based on their certification error risk scores.

*/
********************************************************************************************************************************;
********************************************************************************************************************************;

****************************************;
* Set up data;
****************************************;

%let inlib  = D:\TRANSLATE\sas_data;
%let outlib = D:\TRANSLATE\sas_data;

options nocenter ls=170 noxwait mprint nolabel compress=yes;

libname in  "&inlib.";
libname out "&outlib.";

/*----------------------------------------------------------------*/

* Get APEC and VSR data;

data overall_error;
 set in.apec_analysis (in=a) in.vsr_analysisfile (in=b);

 apec = (a=1);
 if not missing(year) then yr = input(year,4.);

*make payment values real dollars;

 cpi_2004 = 188.9;
 cpi_2005 = 195.3;
 cpi_2006 = 201.6;
 cpi_2007 = 207.342;
 cpi_2008 = 215.303;

 if      yr=2004 then deflator = cpi_2005/cpi_2004;
 else if yr=2005 then deflator = cpi_2005/cpi_2005;
 else if yr=2006 then deflator = cpi_2005/cpi_2006;
 else if yr=2007 then deflator = cpi_2005/cpi_2007;
 else if yr=2008 then deflator = cpi_2005/cpi_2008;

 if apec=0 then vsr_pay_ep_nom     = vsr_pay_ep;
 if apec=0 then vsr_pay_fcne_nom   = vsr_pay_fcne;
 if apec=0 then vsr_pay_fcre_nom   = vsr_pay_fcre;
 if apec=0 then vsr_pay_free_nom   = vsr_pay_free;
 if apec=0 then vsr_pay_over_nom   = vsr_pay_over;
 if apec=0 then vsr_pay_pd_nom     = vsr_pay_pd;
 if apec=0 then vsr_pay_rcfe_nom   = vsr_pay_rcfe;
 if apec=0 then vsr_pay_rcne_nom   = vsr_pay_rcne;
 if apec=0 then vsr_pay_rp_nom     = vsr_pay_rp;
 if apec=0 then vsr_pay_total_nom  = vsr_pay_total;

 label vsr_pay_ep_nom     = "vsr_pay_ep payments expressed in nominal dollars"
       vsr_pay_fcne_nom   = "vsr_pay_fcne payments expressed in nominal dollars"
       vsr_pay_fcre_nom   = "vsr_pay_fcre payments expressed in nominal dollars"
       vsr_pay_free_nom   = "vsr_pay_free payments expressed in nominal dollars"
       vsr_pay_over_nom   = "vsr_pay_over payments expressed in nominal dollars"
       vsr_pay_pd_nom     = "vsr_pay_pd payments expressed in nominal dollars"
       vsr_pay_rcfe_nom   = "vsr_pay_rcfe payments expressed in nominal dollars"
       vsr_pay_rcne_nom   = "vsr_pay_rcne payments expressed in nominal dollars"
       vsr_pay_rp_nom     = "vsr_pay_rp payments expressed in nominal dollars"
       vsr_pay_total_nom  = "vsr_pay_total payments expressed in nominal dollars";

 if apec=0 then vsr_pay_ep     = vsr_pay_ep*deflator;
 if apec=0 then vsr_pay_fcne   = vsr_pay_fcne*deflator;
 if apec=0 then vsr_pay_fcre   = vsr_pay_fcre*deflator;
 if apec=0 then vsr_pay_free   = vsr_pay_free*deflator;
 if apec=0 then vsr_pay_over   = vsr_pay_over*deflator;
 if apec=0 then vsr_pay_pd     = vsr_pay_pd*deflator;
 if apec=0 then vsr_pay_rcfe   = vsr_pay_rcfe*deflator;
 if apec=0 then vsr_pay_rcne   = vsr_pay_rcne*deflator;
 if apec=0 then vsr_pay_rp     = vsr_pay_rp*deflator;
 if apec=0 then vsr_pay_total  = vsr_pay_total*deflator;

 rename vsr_pay_pd       = vsr_pay_nc
        vsr_pay_free     = vsr_pay_f
        vsr_pay_pd_nom   = vsr_pay_nc_nom
        vsr_pay_free_nom = vsr_pay_f_nom;

 id = _N_;

run;

****************************************;
* Apply APEC overall certification error model;
****************************************;

/*--- SET UP COVARIATE LISTS ---*/

%let spec_vsr_t_fcne_pref = vsr_vmeth_rand vsr_perc_fr_aps_chng vsr_intc_rand_fr_aps_chng vsr_perc_fr_aps_no_resp vsr_intc_rand_fr_aps_no_resp
                            vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free vsr_perc_students_redp vsr_sfa_public
                            vsr_enrcat_3 vsr_enrcat_2 vsr_numprovisionschools;

%let spec_vsr_t_fcre_pref = vsr_vmeth_rand vsr_perc_fr_aps_chng vsr_intc_rand_fr_aps_chng vsr_perc_fr_aps_no_resp vsr_intc_rand_fr_aps_no_resp
                            vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free vsr_perc_students_redp vsr_perc_students_f_f_p23nby
                            vsr_noapps_verif_rp vsr_enrcat_1;

%let spec_vsr_t_rcne_pref = vsr_vmeth_rand vsr_perc_rp_aps_chng_fr vsr_intc_rand_rp_aps_chng_fr vsr_perc_rp_aps_chng_pd vsr_intc_rand_rp_aps_chng_pd
                            vsr_perc_rp_aps_no_resp vsr_intc_rand_rp_aps_no_resp vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free
                            vsr_perc_students_redp vsr_perc_students_r_r_p23nby;

%let spec_vsr_t_rcfe_pref = vsr_vmeth_rand vsr_perc_rp_aps_chng_fr vsr_intc_rand_rp_aps_chng_fr vsr_perc_rp_aps_chng_pd vsr_intc_rand_rp_aps_chng_pd
                            vsr_perc_rp_aps_no_resp vsr_intc_rand_rp_aps_no_resp vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free
                            vsr_perc_students_redp vsr_sfa_public vsr_perc_students_free_p23nby vsr_perc_apps_verifd vsr_perc_apps_redp_inc vsr_enrollmentprovision;

%let spec_vsr_t_ncfe_pref = vsr_vmeth_rand vsr_perc_all_aps_chng vsr_intc_rand_all_aps_chng vsr_perc_all_aps_no_resp vsr_intc_rand_all_aps_no_resp vsr_perc_cert_nonapp
                            vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free vsr_perc_students_redp vsr_perc_students_free_p23nby vsr_noapps_verif_fr;

%let spec_vsr_t_ncre_pref = vsr_vmeth_rand vsr_perc_all_aps_chng vsr_intc_rand_all_aps_chng vsr_perc_all_aps_no_resp vsr_intc_rand_all_aps_no_resp vsr_perc_cert_nonapp
                            vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free vsr_perc_students_redp vsr_perc_students_r_r_p23nby;

*run tobits on apec sample and calculate imputed erroneous payments for vsr sample;

/*--- TOBIT MACRO ---*/

%macro tobit(indata,outdata,x);

  %if %sysfunc(substr(&x,1,1))=f %then %let t = f;
  %if %sysfunc(substr(&x,1,1))=r %then %let t = rp;
  %if %sysfunc(substr(&x,1,1))=n %then %let t = nc;

  * RUN TOBIT AND OUTPUT XBETA PREDICTED VALUES;

  proc qlim data=&indata covest=qml method=newrap;
    model ep_pct_ov_&t._&x. = &&spec_vsr_t_&x._pref;
    endogenous ep_pct_ov_&t._&x. ~ censored (lb=0);
    output out = beta xbeta;
  run;

  * RUN UNIVARIATE AND OUTPUT 98TH PERCENTILE DEPVAR VALUE;

  proc univariate data=&indata noprint;
    var ep_pct_ov_&t._&x.;
    weight apec_sfawtps_fnl;
    output out = pctile pctlpts=98 pctlpre=P;
  run;

  * SET PREDICTED PROBABILITES TOGETHER WITH 98TH PERCENTILE AND CONSTRUCT ADDITIONAL VARIABLES;

  data &outdata;
    if _N_=1 then set pctile;
                  set beta;

    * SET PREDICTED VALUES TO ZERO IF LESS THAN ZERO;

    if xbeta_ep_pct_ov_&t._&x.<0 then xbeta_ep_pct_ov_&t._&x. = 0;

    * SET PREDICTED VALUES TO 98TH PERCENTILE OF DEPENDENT VARIALBE IF HIGHER;

    if (xbeta_ep_pct_ov_&t._&x. > p98) then xbeta_ep_pct_ov_&t._&x. = p98;

    * CREATE A DOLLAR VALUE OF ERRONEOUS PAYMENT VARIABLE;

    xbeta_ep_tot_ov_&t._&x. = xbeta_ep_pct_ov_&t._&x.*vsr_pay_&t/100;

    label xbeta_ep_pct_ov_&t._&x. = "imputed error rate due to &x"
          xbeta_ep_tot_ov_&t._&x. = "imputed dollar value of erroneous payments due to &x";

    keep  id xbeta_ep_pct_ov_&t._&x. xbeta_ep_tot_ov_&t._&x. p98;

    rename xbeta_ep_pct_ov_&t._&x. = ep_pct_ov_&x._hat
           xbeta_ep_tot_ov_&t._&x. = ep_tot_ov_&x._hat;
  run;

%mend;

%tobit(overall_error,two,fcne)
%tobit(overall_error,one,fcre)
%tobit(overall_error,three,rcfe)
%tobit(overall_error,four,rcne)
%tobit(overall_error,five,ncfe)
%tobit(overall_error,six,ncre)

***set thresholds using overall cert error values;
**simple threshold;

data overall_error;
 merge overall_error one two three four five six;
 by id;

 ep_tot_ov_hat = SUM(ep_tot_ov_fcre_hat,ep_tot_ov_fcne_hat,ep_tot_ov_rcfe_hat,ep_tot_ov_rcne_hat,ep_tot_ov_ncfe_hat,ep_tot_ov_ncre_hat);

 if vsr_pay_total~=0 then ep_pct_ov_hat=ep_tot_ov_hat/vsr_pay_total*100;

 label ep_pct_ov_hat = "imputed percentage of reimbursements due to overall certification error"
       ep_tot_ov_hat = "imputed dollar value of erroneous payments due to overall certification error";

 risk_hi     =  (ep_tot_ov_hat>50000 | ep_tot_ov_hat=.);
 risk_med    =  (ep_tot_ov_hat>25000 & ep_tot_ov_hat<=50000);
 risk_low    =  (ep_tot_ov_hat<=25000 & ep_tot_ov_hat~=.);

 risk_himed  =  (risk_hi=1 | risk_med=1);

 if risk_low=1 then riskcat=1;
 if risk_med=1 then riskcat=2;
 if risk_hi =1 then riskcat=3;

 drop id P98;

run;

**adjusted threshold;
*adjustment factors;

* CREATING MEDIAN OF VSR_ENROLLMENT VARIABLES BY YEAR, STATE/YEAR;

proc sort data=overall_error; by yr vsr_state; run;

PROC MEANS data=overall_error NOPRINT;
 VAR vsr_enrollment;
 BY yr vsr_state;
 OUTPUT OUT = temp MEDIAN(vsr_enrollment) = med_enr_st;
RUN;

proc sort data=temp; by yr vsr_state; run;

data overall_error;
 merge overall_error temp (drop=_TYPE_ _FREQ_);
 by yr vsr_state;
run;

/*----------------------------------------------*/

proc sort data=overall_error; by yr; run;

PROC MEANS data=overall_error NOPRINT;
 VAR vsr_enrollment;
 BY yr;
 OUTPUT OUT = temp MEDIAN(vsr_enrollment) = med_enr_nat;
RUN;

proc sort data=temp; by yr; run;

data overall_error (drop=_TYPE_ _FREQ_);
 merge overall_error temp;
 by yr;
run;

*adjusted risk category;

data overall_error;
 set overall_error;

 th_hi_adj  = 50000*(med_enr_st/med_enr_nat);
 th_med_adj = 25000*(med_enr_st/med_enr_nat);

 risk_adj_hi  = (ep_tot_ov_hat>th_hi_adj | ep_tot_ov_hat=.);
 risk_adj_med = (ep_tot_ov_hat>th_med_adj & ep_tot_ov_hat<=th_hi_adj);
 risk_adj_low = (ep_tot_ov_hat<=th_med_adj & ep_tot_ov_hat~=.);

 risk_adj_himed = (risk_adj_med=1 | risk_adj_hi);

 keep ep_pct_ov_fcre_hat ep_tot_ov_fcre_hat ep_pct_ov_fcne_hat ep_tot_ov_fcne_hat ep_pct_ov_rcfe_hat
      ep_tot_ov_rcfe_hat ep_pct_ov_rcne_hat ep_tot_ov_rcne_hat ep_pct_ov_ncfe_hat ep_tot_ov_ncfe_hat
      ep_pct_ov_ncre_hat ep_tot_ov_ncre_hat ep_tot_ov_hat ep_pct_ov_hat th_hi_adj th_med_adj risk: apec year;
run;

proc means data=overall_error;
 var risk: ep_pct_ov_fcne_hat ep_pct_ov_fcre_hat ep_pct_ov_rcfe_hat
           ep_pct_ov_rcne_hat ep_pct_ov_ncfe_hat ep_pct_ov_ncre_hat
           ep_pct_ov_hat;
 class year;
 title1; title2 "OVERALL ERROR PERCENTAGE OF REIMBURSEMENTS";
run;

ENDSAS;



