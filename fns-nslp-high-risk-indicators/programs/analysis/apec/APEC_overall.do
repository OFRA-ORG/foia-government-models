****************************************************************************
****************************************************************************
*	Overall certification error analysis (tobits, imputation, and risk categories)
****************************************************************************
****************************************************************************


**************************************
* Set up data
**************************************

clear all
capture log close
set more off
set mem 500m

*pull data
use "C:\Files\a_mpr\_nlsp_m\analysis\analysis files\apec_analysis", clear
gen apec=1
append using "C:\Files\a_mpr\_nlsp_m\analysis\analysis files\vsr_analysis"
replace apec=0 if mi(apec)
gen yr=real(year)
rename vsr_pay_pd vsr_pay_nc
rename vsr_pay_free vsr_pay_f


*make payment values real dollars
gen cpi_2004=188.9
gen cpi_2005=195.3
gen cpi_2006=201.6
gen cpi_2007=207.342
gen cpi_2008=215.303
gen deflator=.
foreach X of numlist 2004/2008 {
	replace deflator=cpi_2005/cpi_`X' if yr==`X'
}
foreach X of varlist vsr_pay* {
	gen `X'_nom=`X'
	label var `X'_nom "`X' payments expressed in nominal dollars"
	replace `X'=`X'*deflator
}



**************************************
* Overall certification error models
**************************************



*set up covariate lists
global spec_vsr_t_fcne_pref "   vsr_vmeth_rand vsr_perc_fr_aps_chng vsr_intc_rand_fr_aps_chng vsr_perc_fr_aps_no_resp vsr_intc_rand_fr_aps_no_resp vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free vsr_perc_students_redp vsr_sfa_public vsr_enrcat_3 vsr_enrcat_2 vsr_numprovisionschools "
global spec_vsr_t_fcre_pref "   vsr_vmeth_rand vsr_perc_fr_aps_chng vsr_intc_rand_fr_aps_chng vsr_perc_fr_aps_no_resp vsr_intc_rand_fr_aps_no_resp vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free vsr_perc_students_redp vsr_perc_students_f_f_p23nby vsr_noapps_verif_rp vsr_enrcat_1 "
global spec_vsr_t_rcne_pref "   vsr_vmeth_rand vsr_perc_rp_aps_chng_fr vsr_intc_rand_rp_aps_chng_fr vsr_perc_rp_aps_chng_pd vsr_intc_rand_rp_aps_chng_pd vsr_perc_rp_aps_no_resp vsr_intc_rand_rp_aps_no_resp vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free vsr_perc_students_redp vsr_perc_students_r_r_p23nby "
global spec_vsr_t_rcfe_pref "   vsr_vmeth_rand vsr_perc_rp_aps_chng_fr vsr_intc_rand_rp_aps_chng_fr vsr_perc_rp_aps_chng_pd vsr_intc_rand_rp_aps_chng_pd vsr_perc_rp_aps_no_resp vsr_intc_rand_rp_aps_no_resp vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free vsr_perc_students_redp vsr_sfa_public vsr_perc_students_free_p23nby vsr_perc_apps_verif vsr_perc_apps_redp_inc vsr_enrollmentprovision "
global spec_vsr_t_ncfe_pref "   vsr_vmeth_rand vsr_perc_all_aps_chng vsr_intc_rand_all_aps_chng vsr_perc_all_aps_no_resp vsr_intc_rand_all_aps_no_resp vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free vsr_perc_students_redp vsr_perc_students_free_p23nby vsr_noapps_verif_fr "
global spec_vsr_t_ncre_pref "   vsr_vmeth_rand vsr_perc_all_aps_chng vsr_intc_rand_all_aps_chng vsr_perc_all_aps_no_resp vsr_intc_rand_all_aps_no_resp vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free vsr_perc_students_redp vsr_perc_students_r_r_p23nby "

*run tobits on apec sample and calculate imputed erroneous payments for vsr sample
gen ep_tot_ov_hat=0
label var ep_tot_ov_hat "imputed dollar value of erroneous payments due to overall certification error"

foreach X in fcre fcne rcfe rcne ncfe ncre {
	if strpos("`X'", "f")==1 local t="f"
	if strpos("`X'", "r")==1 local t="rp"
	if strpos("`X'", "n")==1 local t="nc"

	tobit ep_pct_ov_`t'_`X' ${spec_vsr_t_`X'_pref} if apec==1, vce(robust) ll(0)
	predict ep_pct_ov_`X'_hat
	replace ep_pct_ov_`X'_hat=0 if ep_pct_ov_`X'_hat<0
	_pctile ep_pct_ov_`t'_`X' [pw=apec_sfawtps_fnl], p(98)
	gen temp=r(r1)
	replace ep_pct_ov_`X'_hat=temp if ep_pct_ov_`X'_hat>temp & !mi(ep_pct_ov_`X'_hat)
	drop temp
	label var ep_pct_ov_`X'_hat "imputed error rate due to `X'"

	gen ep_tot_ov_`X'_hat=ep_pct_ov_`X'_hat*vsr_pay_`t'/100
	label var ep_tot_ov_`X'_hat "imputed dollar value of erroneous payments due to `X'"

	replace ep_tot_ov_hat=ep_tot_ov_hat+ep_tot_ov_`X'_hat
}

gen ep_pct_ov_hat=ep_tot_ov_hat/vsr_pay_total*100
label var ep_pct_ov_hat "imputed percentage of reimbursements due to overall certification error"

*info for table III.2
codebook ep_pct_ov_hat if apec==0 & yr==2005


***set thresholds using overall cert error values

**simple threshold
gen risk_hi=(ep_tot_ov_hat>50000) 
gen risk_med=(ep_tot_ov_hat>25000 & ep_tot_ov_hat<=50000) 
gen risk_low=(ep_tot_ov_hat<=25000) 
egen risk_himed=rowmax(risk_hi risk_med)
gen riskcat=1 if risk_low==1
replace riskcat=2 if risk_med==1
replace riskcat=3 if risk_hi==1


**adjusted threshold
*adjustment factors
egen med_enr_st=median(vsr_enrollment), by(yr vsr_state)
egen med_enr_nat=median(vsr_enrollment), by(yr)
gen th_hi_adj=50000*(med_enr_st/med_enr_nat)
gen th_med_adj=25000*(med_enr_st/med_enr_nat)

*adjusted risk category
gen risk_adj_hi=(ep_tot_ov_hat>th_hi_adj) 
gen risk_adj_med=(ep_tot_ov_hat>th_med_adj & ep_tot_ov_hat<=th_hi_adj) 
gen risk_adj_low=(ep_tot_ov_hat<=th_med_adj) 
egen risk_adj_himed=rowmax(risk_adj_hi risk_adj_med)

bysort yr: sum risk* if apec==0


