cd "loc"
set scheme plotplainblind

*make 3 distinct datasets and cross them:
{

    clear all
    local N_shifts 2400
    set obs `N_shifts'
    egen time = seq(),  block(1)
    tempfile time 
    save `time', replace
    save baseframe.dta , replace
}
*only pulmonary department:
{
    cap program drop pvalcalc
    program pvalcalc, rclass
        drop _all
        local N_patients = 20
        local p_death_shift = 130/2400
        local p_patientdeath_shift = `p_death_shift'/20
        local p_death_shift_V = 130/2400 + 20/(2400/5) 
        local p_patientdeath_shift_V = `p_death_shift_V'/20
        di `p_patientdeath_shift'
        di `p_patientdeath_shift_V'
        use baseframe.dta, clear
        gen V_works = _n<2401/5
        gen binomial_noV = rbinomial(`N_patients',`p_patientdeath_shift')
        gen binomial_V =  rbinomial(`N_patients',`p_patientdeath_shift_V')
        gen outcome = binomial_noV*(1-V_works) + binomial_V*V_works
        ppmlhdfe outcome V_works, noabsorb cluster(i.time)
        return scalar b = _b[V_works]
        return scalar pfine = (2*(normal(-abs(_b[V_works]/_se[V_works]))))
    end

    simulate b = r(b) pfine = r(pfine) , reps(1000) seed(420    ): pvalcalc
    hist pfine, xtitle("p-value")
    graph export pval_main.png, replace
    gen pfine_sigpos=(pfine<0.05)&(b>0)
    su pfine_sigpos


    *What if patients would have died anyways?
    cap program drop pvalcalc
    program pvalcalc, rclass
        drop _all
        local N_patients = 20

        local p_death_shift = 150/2400 - 20/((4*2400)/5) 
        local p_patientdeath_shift = `p_death_shift'/20
        local p_death_shift_V = 150/2400 + 20/(2400/5) 
        local p_patientdeath_shift_V = `p_death_shift_V'/20
        use baseframe.dta, clear
        gen V_works = _n<2401/5
        gen binomial_noV = rbinomial(`N_patients',`p_patientdeath_shift')
        gen binomial_V =  rbinomial(`N_patients',`p_patientdeath_shift_V')
        gen outcome = binomial_noV*(1-V_works) + binomial_V*V_works
        ppmlhdfe outcome V_works, noabsorb cluster(i.time)
        return scalar b = _b[V_works]
        return scalar pfine = (2*(normal(-abs(_b[V_works]/_se[V_works]))))
    end

    simulate b = r(b) pfine = r(pfine) , reps(1000) seed(420    ): pvalcalc
    hist pfine, xtitle("p-value")
    graph export pval_main_dieanyways.png, replace

    gen pfine_sigpos=(pfine<0.05)&(b>0)
    su pfine_sigpos
}
*full hospital numbers:
{
    cap program drop pvalcalc
    program pvalcalc, rclass
        drop _all
        local N_patients 250
        local p_death_shift = 222/2400
        local p_patientdeath_shift = `p_death_shift'/250
        local p_death_shift_V = 222/2400 + 20/(2400/5)
        local p_patientdeath_shift_V = `p_death_shift_V'/250
        use baseframe.dta, clear
        gen V_works = _n<2401/5
        gen binomial_noV = rbinomial(`N_patients',`p_patientdeath_shift')
        gen binomial_V =  rbinomial(`N_patients',`p_patientdeath_shift_V')
        gen outcome = binomial_noV*(1-V_works) + binomial_V*V_works
        ppmlhdfe outcome V_works, noabsorb cluster(i.time)
        return scalar b = _b[V_works]
        return scalar pfine = (2*(normal(-abs(_b[V_works]/_se[V_works]))))
    end

    simulate b = r(b) pfine = r(pfine) , reps(1000) seed(420): pvalcalc
    hist pfine, xtitle("p-value")
    graph export pval_whole_hospital.png, replace
    gen pfine_sig=(pfine<0.05)
    su pfine_sig
}
