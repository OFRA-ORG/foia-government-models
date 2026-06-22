********************************************************************************************************************************;
********************************************************************************************************************************;
/*

This file imputes meal counts for all VSR districts.

*/
********************************************************************************************************************************;
********************************************************************************************************************************;

%let inlib  = D:\TRANSLATE\sas_data;
%let outlib = D:\TRANSLATE\sas_data;

options nocenter ls=170 noxwait mprint nolabel compress=yes;

libname in  "&inlib.";
libname out "&outlib.";

/*----------------------------------------------------------------*/

* add data from the FNS national datafile;

proc sort data=in.vsr_constructs out=vsr_constructs; by state; run;
proc sort data=in.fnsnd_0408     out=fnsnd_0408;     by state; run;

data vsr_impute_meals;
 merge fnsnd_0408 (in=a) vsr_constructs (in=b);
 by state;
 if (state="GU" OR state="PR") then delete;
 if vsr_enrollment<(vsr_freeeligtot+vsr_rpelig) & nmiss(vsr_enrollment,vsr_freeeligtot,vsr_rpelig)=0 then vsr_enrollment=vsr_freeeligtot+vsr_rpelig;
run;

*total enrollment by state;

proc sort data=vsr_impute_meals;
 by state year;
run;

proc means data=vsr_impute_meals noprint;
 var vsr_enrollment vsr_freeeligtot vsr_rpelig;
 by state year;
 output out=temp sum(vsr_enrollment vsr_freeeligtot vsr_rpelig) = vsr_stateenroll vsr_stenr_free vsr_stenr_rp;
run;

data out.vsr_impute_meals;
 merge vsr_impute_meals temp (drop=_TYPE_ _FREQ_);
 by state year;

* Nonlinear vsr_enrollment variables;

 vsr_enroll_2=vsr_enrollment**2;
 vsr_enroll_3=vsr_enrollment**3;

 if not missing(vsr_enrollment) then vsr_enrcat_1=(vsr_enrollment<1000);
 if not missing(vsr_enrollment) then vsr_enrcat_2=(vsr_enrollment>=1000 & vsr_enrollment<5000);
 if not missing(vsr_enrollment) then vsr_enrcat_3=(vsr_enrollment>=5000 & vsr_enrollment<10000);
 if not missing(vsr_enrollment) then vsr_enrcat_4=(vsr_enrollment>=10000);

 vsr_intc_rand_enroll=vsr_enrollment*vsr_vmeth_rand;

 vsr_enr10k=vsr_enrollment/10000;

 vsr_stenr_pd = vsr_stateenroll-vsr_stenr_free-vsr_stenr_rp;

 *district percentages of state enrollment by certification type;

 vsr_pctstenrll=vsr_enrollment/vsr_stateenroll;
 vsr_propst_free=vsr_freeeligtot/vsr_stenr_free;
 vsr_propst_rp=vsr_rpelig/vsr_stenr_rp;
 vsr_propst_pd=(vsr_enrollment - vsr_freeeligtot - vsr_rpelig) / vsr_stenr_pd;

 *impute meal counts;

 vsr_numlun_free=.;
 vsr_stlunper_free=.;

 vsr_numlun_rp=.;
 vsr_stlunper_rp=.;

 vsr_numlun_pd=.;
 vsr_stlunper_pd=.;

 * free meals;

 if year="2004" then vsr_numlun_free=vsr_propst_free * nslpfreelunches2004;

 if year="2005" then vsr_numlun_free=vsr_propst_free * nslpfreelunches2005;

 if year="2006" then vsr_numlun_free=vsr_propst_free * nslpfreelunches2006;

 if year="2007" then vsr_numlun_free=vsr_propst_free * nslpfreelunches2007;

 if year="2008" then vsr_numlun_free=vsr_propst_free * nslpfreelunches2008;

 * reduced price meals;

 if year="2004" then vsr_numlun_rp=vsr_propst_rp * nslpreducedpricelunches2004;

 if year="2005" then vsr_numlun_rp=vsr_propst_rp * nslpreducedpricelunches2005;

 if year="2006" then vsr_numlun_rp=vsr_propst_rp * nslpreducedpricelunches2006;

 if year="2007" then vsr_numlun_rp=vsr_propst_rp * nslpreducedpricelunches2007;

 if year="2008" then vsr_numlun_rp=vsr_propst_rp * nslpreducedpricelunches2008;

 * paid meals;

 if year="2004" then vsr_numlun_pd=vsr_propst_pd * nslppaidlunches2004;

 if year="2005" then vsr_numlun_pd=vsr_propst_pd * nslppaidlunches2005;

 if year="2006" then vsr_numlun_pd=vsr_propst_pd * nslppaidlunches2006;

 if year="2007" then vsr_numlun_pd=vsr_propst_pd * nslppaidlunches2007;

 if year="2008" then vsr_numlun_pd = vsr_propst_pd * nslppaidlunches2008;

*remove districts with no matchid from data;
 if missing(matchid) then delete;

run;

proc means data=out.vsr_impute_meals N MEAN;
run;

proc contents data=out.vsr_impute_meals;
run;

ENDSAS;

