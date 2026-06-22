********************************************************************************************************************************;
********************************************************************************************************************************;
/*


This file estimates statistical models of administrative certification error derived from RORA and applies them to the VSR analysis file.

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

* Get RORA and VSR data;

data rora_analysis;
 set in.rora_analysis (in=a drop=matchid) in.vsr_analysisfile (in=b drop=matchid);

 rora = (a=1);
 if not missing(year) then yr = input(year,4.);

 * make payment values real dollars;

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

 if rora=0 then vsr_pay_ep_nom     = vsr_pay_ep;
 if rora=0 then vsr_pay_fcne_nom   = vsr_pay_fcne;
 if rora=0 then vsr_pay_fcre_nom   = vsr_pay_fcre;
 if rora=0 then vsr_pay_free_nom   = vsr_pay_free;
 if rora=0 then vsr_pay_over_nom   = vsr_pay_over;
 if rora=0 then vsr_pay_pd_nom     = vsr_pay_pd;
 if rora=0 then vsr_pay_rcfe_nom   = vsr_pay_rcfe;
 if rora=0 then vsr_pay_rcne_nom   = vsr_pay_rcne;
 if rora=0 then vsr_pay_rp_nom     = vsr_pay_rp;
 if rora=0 then vsr_pay_total_nom  = vsr_pay_total;

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

 if rora=0 then vsr_pay_ep     = vsr_pay_ep*deflator;
 if rora=0 then vsr_pay_fcne   = vsr_pay_fcne*deflator;
 if rora=0 then vsr_pay_fcre   = vsr_pay_fcre*deflator;
 if rora=0 then vsr_pay_free   = vsr_pay_free*deflator;
 if rora=0 then vsr_pay_over   = vsr_pay_over*deflator;
 if rora=0 then vsr_pay_pd     = vsr_pay_pd*deflator;
 if rora=0 then vsr_pay_rcfe   = vsr_pay_rcfe*deflator;
 if rora=0 then vsr_pay_rcne   = vsr_pay_rcne*deflator;
 if rora=0 then vsr_pay_rp     = vsr_pay_rp*deflator;
 if rora=0 then vsr_pay_total  = vsr_pay_total*deflator;

 rename vsr_pay_pd       = vsr_pay_nc
        vsr_pay_free     = vsr_pay_f
        vsr_pay_pd_nom   = vsr_pay_nc_nom
        vsr_pay_free_nom = vsr_pay_f_nom;

 if yr~=2007 then rora_pct_allep = .;

run;

*******************;
* Admin error;
*******************;

%let spec_vsr_07_t_adm_pref = vsr_vmeth_rand vsr_perc_all_aps_chng vsr_intc_rand_all_aps_chng vsr_perc_all_aps_no_resp vsr_intc_rand_all_aps_no_resp
                              vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free vsr_perc_students_redp vsr_enrcat_2;

* RUN TOBIT AND OUTPUT XBETA PREDICTED VALUES;

proc qlim data=rora_analysis covest=qml method=newrap;
  model rora_pct_allep = &spec_vsr_07_t_adm_pref;
  endogenous rora_pct_allep ~ censored (lb=0);
  output out = beta xbeta;
run;

* RUN UNIVARIATE AND OUTPUT 98TH PERCENTILE DEPVAR VALUE;

proc univariate data=rora_analysis noprint;
  var rora_pct_allep;
  output out = pctile pctlpts=98 pctlpre=P;
run;

* SET PREDICTED PROBABILITES TOGETHER WITH 98TH PERCENTILE AND CONSTRUCT ADDITIONAL VARIABLES;

data admin_error;
  if _N_=1 then set pctile;
                set beta;

  * SET PREDICTED VALUES TO ZERO IF LESS THAN ZERO;

  if xbeta_rora_pct_allep <0 then xbeta_rora_pct_allep = 0;

  * SET PREDICTED VALUES TO 98TH PERCENTILE OF DEPENDENT VARIALBE IF HIGHER;

  if (xbeta_rora_pct_allep > p98) then xbeta_rora_pct_allep = p98;

  * CREATE A DOLLAR VALUE OF ERRONEOUS PAYMENT VARIABLE;

  ep_tot_07_hat = xbeta_rora_pct_allep*vsr_pay_total/100;

  keep xbeta_rora_pct_allep ep_tot_07_hat year;
  rename xbeta_rora_pct_allep = ep_pct_07_hat;
run;

proc means data=admin_error;
 var ep_pct_07_hat;
 class year;
 title1; title2 "ADMINISTRATIVE ERROR PERCENTAGE OF REIMBURSEMENTS";
run;

ENDSAS;



