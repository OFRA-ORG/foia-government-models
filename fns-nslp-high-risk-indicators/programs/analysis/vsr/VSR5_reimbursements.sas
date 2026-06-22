********************************************************************************************************************************;
********************************************************************************************************************************;
/*

This file calculates the amounts of reimbursements as well as the overall certification error measure available in the VSR data.
As discussed in the report, this certification error measure has a number of problematic characteristics and, based on various analyses,
was not included in the final analysis.

*/
********************************************************************************************************************************;
********************************************************************************************************************************;

%let inlib  = D:\TRANSLATE\sas_data;
%let outlib = D:\TRANSLATE\sas_data;

options nocenter ls=170 noxwait mprint nolabel compress=yes;

libname in  "&inlib.";
libname out "&outlib.";

/*----------------------------------------------------------------*/

proc import datafile = "&inlib.\payment_amts.csv" out=payment_amts;
run;

data payment_amts (rename=(year2=year));
 set payment_amts;
 yr = year - 1;
 year2 = put(yr,4.);
 drop year;
run;

data vsr_impute_meals;
 set in.vsr_impute_meals;
 state_loc = "contig";
 if upcase(state)="AK"         then state_loc="AK";
 if upcase(state)="HI"         then state_loc="HI";
 if vsr_enrollment~=0          then perc_students_frp_inVSR = (vsr_freeeligtot+vsr_rpelig)/vsr_enrollment;
 if perc_students_frp_inVSR~=. then less60perc = (perc_students_frp_invsr<.6);
 if missing(less60perc)        then less60perc = 1;
run;

proc freq data=vsr_impute_meals;
 tables year less60perc state_loc;
run;

proc sort data=vsr_impute_meals; by year less60perc state_loc; run;
proc sort data=payment_amts;     by year less60perc state_loc; run;

data out.vsr_reimbursements (rename=(_cost_fr=cost_fr _cost_rp=cost_rp _cost_pd=cost_pd));
 merge vsr_impute_meals (in=a) payment_amts (in=b);
 by year less60perc state_loc;
 if ~a then delete;

 if tottch~=0 then nus_tottch_perst = member/tottch;

 if missing(nus_tottch_perst) then nus_tottch_perst=0;

 nus_leaadm = nus_leaadm_perst*member;
 nus_leasup = nus_leasup_perst*member;
 nus_schadm = nus_schadm_perst*member;

 nkids_verif_freecatelig = sum(a2,a4,a6,a8);
 nkids_verif_freeincelig = sum(b2,b4,b6,b8);
 nkids_verif_free        = sum(nkids_verif_freecatelig,nkids_verif_freeincelig);
 nkids_verif_redpincelig = sum(c2,c4,c6,c8);

 if nkids_verif_free~=0 then vsr_perc_fr_kids_chng_rp =(a4+b4)/nkids_verif_free;
 if nkids_verif_free~=0 then vsr_perc_fr_kids_chng_pd =(a6+b6)/nkids_verif_free;

 if nkids_verif_redpincelig~=0 then vsr_perc_rp_kids_chng_fr =c4/nkids_verif_redpincelig;
 if nkids_verif_redpincelig~=0 then vsr_perc_rp_kids_chng_pd =c6/nkids_verif_redpincelig;

 if nkids_verif_free=0        then vsr_perc_fr_kids_chng_rp = 0;
 if nkids_verif_free=0        then vsr_perc_fr_kids_chng_pd = 0;
 if nkids_verif_redpincelig=0 then vsr_perc_rp_kids_chng_fr = 0;
 if nkids_verif_redpincelig=0 then vsr_perc_rp_kids_chng_pd = 0;

 *********************************;
 * Payments;
 *********************************;

* Define differentces reimbursement rates;
 FreeMinusFullPrice = cost_fr-cost_pd;
 FreeMinusRedPrice  = cost_fr-cost_rp;
 FreeMinusRedPrice  = .40;
 RedMinusFullPrice  = cost_rp-cost_pd;

 _cost_fr = cost_fr + commodity;
 _cost_rp = cost_rp + commodity;
 _cost_pd = cost_pd + commodity;

* Get payment amounts;

 vsr_pay_free  = _cost_fr*vsr_numlun_free;
 vsr_pay_rp    = _cost_rp*vsr_numlun_rp;
 vsr_pay_pd    = _cost_pd*vsr_numlun_pd;
 vsr_pay_total = sum(vsr_pay_free,vsr_pay_rp,vsr_pay_pd);

 if missing(vsr_pay_total) then vsr_pay_total=0;

* Dollar amounts in error;
 * OVERPAYMENTS;

 vsr_nlun_freeShdBePd = vsr_perc_fr_kids_chng_pd*vsr_numlun_free;
 vsr_nlun_freeShdBeRP = vsr_perc_fr_kids_chng_rp*vsr_numlun_free;
 vsr_nlun_rpShdBePd   = vsr_perc_rp_kids_chng_pd*vsr_numlun_rp;

 vsr_pay_freeShdBePd = FreeMinusFullPrice*vsr_nlun_freeShdBePd;
 vsr_pay_freeShdBeRP = FreeMinusRedPrice*vsr_nlun_freeShdBeRP;
 vsr_pay_rpShdBePd   = RedMinusFullPrice*vsr_nlun_rpShdBePd;

 vsr_pay_over        = sum(vsr_pay_freeShdBePd,vsr_pay_freeShdBeRP,vsr_pay_rpShdBePd);

 * UNDERPAYMENT;

 vsr_nlun_rpShdBeFree = vsr_perc_rp_kids_chng_fr*vsr_numlun_rp;
 vsr_pay_rpShdBeFree = FreeMinusRedPrice*vsr_nlun_rpShdBeFree;

 vsr_errpay_total = sum(vsr_pay_over,vsr_pay_rpShdBeFree);

* Rates of error;

 vsr_pay_ep=(vsr_pay_over+vsr_pay_rpShdBeFree);

 if vsr_pay_total~=0 then vsr_pct_ep=(vsr_pay_over+vsr_pay_rpShdBeFree)/vsr_pay_total;
 if vsr_pay_total~=0 then vsr_pct_fcne=vsr_pay_freeShdBePd/vsr_pay_total;
 if vsr_pay_total~=0 then vsr_pct_fcre=vsr_pay_freeShdBeRP/vsr_pay_total;
 if vsr_pay_total~=0 then vsr_pct_rcfe=vsr_pay_rpShdBeFree/vsr_pay_total;
 if vsr_pay_total~=0 then vsr_pct_rcne=vsr_pay_rpShdBePd /vsr_pay_total;
 if vsr_pay_free~=0  then vsr_pct_f_fcne=vsr_pay_freeShdBePd/vsr_pay_free;
 if vsr_pay_free~=0  then vsr_pct_f_fcre=vsr_pay_freeShdBeRP/vsr_pay_free;
 if vsr_pay_rp~=0    then vsr_pct_r_rcfe=vsr_pay_rpShdBeFree/vsr_pay_rp;
 if vsr_pay_rp~=0    then vsr_pct_r_rcne=vsr_pay_rpShdBePd /vsr_pay_rp;

 drop cost_fr cost_rp cost_pd;

 rename vsr_pay_rpShdBeFree = vsr_pay_rcfe
        vsr_pay_rpShdBePd   = vsr_pay_rcne
        vsr_pay_freeShdBeRP = vsr_pay_fcre
        vsr_pay_freeShdBePd = vsr_pay_fcne;

run;

proc means data=out.vsr_reimbursements N MEAN;
run;

proc contents data=out.vsr_reimbursements;
run;
