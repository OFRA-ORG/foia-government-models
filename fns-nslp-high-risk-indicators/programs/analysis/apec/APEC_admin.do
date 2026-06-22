****************************************************************************
****************************************************************************
*	Admin error analysis (tobits, imputation)
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
* Admin error models
**************************************


global spec_adm_vsr_t_b "   vsr_vmeth_rand vsr_perc_all_aps_chng vsr_intc_rand_all_aps_chng vsr_perc_all_aps_no_resp vsr_intc_rand_all_aps_no_resp vsr_perc_cert_nonapp vsr_perc_cert_cat vsr_enr10k vsr_perc_students_free vsr_perc_students_redp "

tobit ep_pct_adm_yr_tot $spec_adm_vsr_t_b if apec==1, vce(robust) ll(0)
predict ep_pct_adm_hat
replace ep_pct_adm_hat=0 if ep_pct_adm_hat<0
_pctile ep_pct_adm_yr_tot [pw=apec_sfawtps_fnl], p(98)
gen temp=r(r1)
replace ep_pct_adm_hat=temp if ep_pct_adm_hat>temp & !mi(ep_pct_adm_hat)
drop temp
label var ep_pct_adm_hat "imputed percentage of reimbursements due to administrative error"


gen ep_tot_adm_hat=ep_pct_adm_hat*vsr_pay_total/100
label var ep_tot_adm_hat "imputed value of erroneous payments due to administrative error"

*info for table III.8
codebook ep_pct_adm_hat if apec==0 & yr==2005

cap restore, not
preserve
keep if apec==0
keep *id* *state* yr year *risk* ep*
order *id* *state*, first
drop  ep_pct_adm_f_fcne- year
save vsr_riskcat, replace
restore



