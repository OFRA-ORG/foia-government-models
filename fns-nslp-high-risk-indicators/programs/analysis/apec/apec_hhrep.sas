********************************************************************************************************************************;
********************************************************************************************************************************;
/*


This file estimates statistical models of household reporting certification error from APEC and applies them to the VSR analysis file.

*/
********************************************************************************************************************************;
********************************************************************************************************************************;

****************************************;
* Section 1: Set up data;
****************************************;

%let inlib  = D:\TRANSLATE\sas_data;
%let outlib = D:\TRANSLATE\sas_data;

options nocenter ls=170 noxwait mprint nolabel;

libname in  "&inlib.";
libname out "&outlib.";

/*----------------------------------------------------------------*/

* Get APEC and VSR data;

data apec_analysis;
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

/*--- TOBIT MACRO ---*/

%macro tobit(indata,outdata,x,model);

  %if %sysfunc(substr(&x,1,1))=f %then %let t = f;
  %if %sysfunc(substr(&x,1,1))=r %then %let t = rp;
  %if %sysfunc(substr(&x,1,1))=n %then %let t = nc;

  * SPECIFYING OPTIMIZATION METHOD;

  %if (&model=rep) AND (&x=rcfe) %then %let method=QUANEW;
  %else                                %let method=NEWRAP;

  * RUN TOBIT AND OUTPUT XBETA PREDICTED VALUES;

  proc qlim data=&indata covest=qml method=&method;
    model ep_pct_&model._&t._&x. = &&spec_&model._t_&x._pref;
    endogenous ep_pct_&model._&t._&x. ~ censored (lb=0);
    output out = beta xbeta;
  run;

  * RUN UNIVARIATE AND OUTPUT 98TH PERCENTILE DEPVAR VALUE;

  proc univariate data=&indata noprint;
    var ep_pct_&model._&t._&x.;
    weight apec_sfawtps_fnl;
    output out = pctile pctlpts=98 pctlpre=P;
  run;

  * SET PREDICTED PROBABILITES TOGETHER WITH 98TH PERCENTILE AND CONSTRUCT ADDITIONAL VARIABLES;

  data &outdata;
    if _N_=1 then set pctile;
                  set beta;

    * SET PREDICTED VALUES TO ZERO IF LESS THAN ZERO;

    if xbeta_ep_pct_&model._&t._&x.<0 then xbeta_ep_pct_&model._&t._&x. = 0;

    * SET PREDICTED VALUES TO 98TH PERCENTILE OF DEPENDENT VARIALBE IF HIGHER;

    if (xbeta_ep_pct_&model._&t._&x. > p98) then xbeta_ep_pct_&model._&t._&x. = p98;

    * CREATE A DOLLAR VALUE OF ERRONEOUS PAYMENT VARIABLE;

    xbeta_ep_tot_&model._&t._&x. = xbeta_ep_pct_&model._&t._&x.*vsr_pay_&t/100;

    keep  id xbeta_ep_pct_&model._&t._&x. xbeta_ep_tot_&model._&t._&x. p98;

    rename xbeta_ep_pct_&model._&t._&x. = ep_pct_&model._&x._hat
           xbeta_ep_tot_&model._&t._&x. = ep_tot_&model._&x._hat;
  run;

%mend;

**************************************;
* HH Reporting error;
**************************************;

%let spec_rep_t_fcne_pref = vsr_vmeth_rand vsr_perc_fr_aps_chng vsr_intc_rand_fr_aps_chng vsr_perc_fr_aps_no_resp vsr_intc_rand_fr_aps_no_resp
                            vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free vsr_perc_students_redp vsr_sfa_public vsr_enrcat_2;

%let spec_rep_t_fcre_pref = vsr_vmeth_rand vsr_perc_fr_aps_chng vsr_intc_rand_fr_aps_chng vsr_perc_fr_aps_no_resp vsr_intc_rand_fr_aps_no_resp
                            vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free vsr_perc_students_redp vsr_enrcat_3 vsr_perc_freeapps_free_inc;

%let spec_rep_t_rcne_pref = vsr_vmeth_rand vsr_perc_rp_aps_chng_fr vsr_intc_rand_rp_aps_chng_fr vsr_perc_rp_aps_chng_pd vsr_intc_rand_rp_aps_chng_pd
                            vsr_perc_rp_aps_no_resp vsr_intc_rand_rp_aps_no_resp vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free
                            vsr_perc_students_redp vsr_perc_students_r_r_p23nby;

%let spec_rep_t_rcfe_pref = vsr_vmeth_rand vsr_perc_rp_aps_chng_fr vsr_intc_rand_rp_aps_chng_fr vsr_perc_rp_aps_chng_pd vsr_intc_rand_rp_aps_chng_pd
                            vsr_perc_rp_aps_no_resp vsr_intc_rand_rp_aps_no_resp vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free
                            vsr_perc_students_redp vsr_perc_apps_verifd vsr_perc_students_free_p23nby vsr_enrcat_4 vsr_enroll_2 vsr_freeincomeapps vsr_sfa_public;

%let spec_rep_t_ncfe_pref = vsr_vmeth_rand vsr_perc_all_aps_chng vsr_intc_rand_all_aps_chng vsr_perc_all_aps_no_resp vsr_intc_rand_all_aps_no_resp
                            vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free vsr_perc_students_redp vsr_perc_apps_free_inc;

%let spec_rep_t_ncre_pref = vsr_vmeth_rand vsr_perc_all_aps_chng vsr_intc_rand_all_aps_chng vsr_perc_all_aps_no_resp vsr_intc_rand_all_aps_no_resp
                            vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free vsr_perc_students_redp vsr_enrcat_2;

%tobit(apec_analysis,two,fcne,rep)
%tobit(apec_analysis,one,fcre,rep)
%tobit(apec_analysis,three,rcfe,rep)
%tobit(apec_analysis,four,rcne,rep)
%tobit(apec_analysis,five,ncfe,rep)
%tobit(apec_analysis,six,ncre,rep)

data reporting_error;
 merge apec_analysis one two three four five six;
 by id;

 ep_tot_rep_hat = SUM(ep_tot_rep_fcre_hat,ep_tot_rep_fcne_hat,ep_tot_rep_rcfe_hat,ep_tot_rep_rcne_hat,ep_tot_rep_ncfe_hat,ep_tot_rep_ncre_hat);

 if vsr_pay_total~=0 then ep_pct_rep_hat=ep_tot_rep_hat/vsr_pay_total*100;

 label ep_pct_rep_hat = "imputed percentage of reimbursements due to reporting error"
       ep_tot_rep_hat = "imputed dollar value of erroneous payments due to reporting error";

 keep ep_pct_rep_fcre_hat ep_tot_rep_fcre_hat ep_pct_rep_fcne_hat ep_tot_rep_fcne_hat ep_pct_rep_rcfe_hat ep_tot_rep_rcfe_hat
      ep_pct_rep_rcne_hat ep_tot_rep_rcne_hat ep_pct_rep_ncfe_hat ep_tot_rep_ncfe_hat ep_pct_rep_ncre_hat ep_tot_rep_ncre_hat
      ep_tot_rep_hat ep_pct_rep_hat year;

run;

proc means data=reporting_error;
 var ep_pct_rep_fcne_hat ep_pct_rep_fcre_hat  ep_pct_rep_rcfe_hat ep_pct_rep_rcne_hat ep_pct_rep_ncfe_hat ep_pct_rep_ncre_hat ep_pct_rep_hat;
 class year;
 title1; title2 "REPORTING ERROR PERCENTAGE OF REIMBURSEMENTS";
run;

ENDSAS;



