********************************************************************************************************************************;
********************************************************************************************************************************;
/*


This program estimates Tobit models on the APEC administrative certification error measure. It then applies these models to data from
the VSR in order to validate the models.

*/
********************************************************************************************************************************;
********************************************************************************************************************************;

****************************************;
* Set up data;
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

**************************************;
* Admin error;
**************************************;

%let spec_adm_vsr_t_b = vsr_vmeth_rand vsr_perc_all_aps_chng vsr_intc_rand_all_aps_chng vsr_perc_all_aps_no_resp vsr_intc_rand_all_aps_no_resp
                        vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free vsr_perc_students_redp;

* RUN TOBIT AND OUTPUT XBETA PREDICTED VALUES;

proc qlim data=apec_analysis covest=qml method=newrap;
  model ep_pct_adm_yr_tot = &spec_adm_vsr_t_b;
  endogenous ep_pct_adm_yr_tot ~ censored (lb=0);
  output out = beta xbeta;
run;

* RUN UNIVARIATE AND OUTPUT 98TH PERCENTILE DEPVAR VALUE;

proc univariate data=apec_analysis noprint;
  var ep_pct_adm_yr_tot;
  weight apec_sfawtps_fnl;
  output out = pctile pctlpts=98 pctlpre=P;
run;

* SET PREDICTED PROBABILITES TOGETHER WITH 98TH PERCENTILE AND CONSTRUCT ADDITIONAL VARIABLES;

data admin_error;
  if _N_=1 then set pctile;
                set beta;

  * SET PREDICTED VALUES TO ZERO IF LESS THAN ZERO;

  if xbeta_ep_pct_adm_yr_tot <0 then xbeta_ep_pct_adm_yr_tot = 0;

  * SET PREDICTED VALUES TO 98TH PERCENTILE OF DEPENDENT VARIALBE IF HIGHER;

  if (xbeta_ep_pct_adm_yr_tot > p98) then xbeta_ep_pct_adm_yr_tot = p98;

  * CREATE A DOLLAR VALUE OF ERRONEOUS PAYMENT VARIABLE;

  ep_tot_adm_hat = xbeta_ep_pct_adm_yr_tot*vsr_pay_total/100;

  label xbeta_ep_pct_adm_yr_tot = "imputed percentage of reimbursements due to administrative error"
        ep_tot_adm_hat          = "imputed value of erroneous payments due to administrative error";

  keep xbeta_ep_pct_adm_yr_tot ep_tot_adm_hat year;
  rename xbeta_ep_pct_adm_yr_tot = ep_pct_adm_hat;
run;

proc means data=admin_error;
 var ep_pct_adm_hat;
 class year;
title1; title2 "ADMINISTRATIVE ERROR PERCENTAGE OF REIMBURSEMENTS";
run;

ENDSAS;



