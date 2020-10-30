

find_t_f = function(ta,ts){
  ts = ts
  ta = ta
  tm = 1-ta-ts
  
  age1 = 0.24
  age2 = 0.43
  age3 = 0.21
  age4 = 1-age1-age2-age3
  
  # Conditional probability
  prob.age1.given.s = 1.11E-02
  prob.age2.given.s = 1.65E-01
  prob.age3.given.s = 3.33E-01
  prob.age4.given.s = 1-prob.age1.given.s-prob.age2.given.s-prob.age3.given.s
  
  prob.age1.given.m = 4.14E-02
  prob.age2.given.m = 4.55E-01
  prob.age3.given.m = 3.60E-01
  prob.age4.given.m = 1-prob.age1.given.m-prob.age2.given.m-prob.age3.given.m
  
  ts_age1 = ts*prob.age1.given.s/age1
  ts_age2 = ts*prob.age2.given.s/age2
  ts_age3 = ts*prob.age3.given.s/age3
  ts_age4 = ts*prob.age4.given.s/age4
  
  tm_age1 = tm*prob.age1.given.m/age1
  tm_age2 = tm*prob.age2.given.m/age2
  tm_age3 = tm*prob.age3.given.m/age3
  tm_age4 = tm*prob.age4.given.m/age4
  
  ta_age1 = 1-ts_age1-tm_age1
  ta_age2 = 1-ts_age2-tm_age2
  ta_age3 = 1-ts_age3-tm_age3
  ta_age4 = 1-ts_age4-tm_age4
  
  ### In the uninfected population
  
  fs_age1 = 9.61866E-06
  fs_age2 = 9.64228E-06
  fs_age3 = 3.02396E-05
  fs_age4 = 7.90697E-05
  
  fm_age1 = 2.82E-04
  fm_age2 = 1.29E-04
  fm_age3 = 1.18E-04
  fm_age4 = 1.02E-04
  
  fa_age1 = 1-fs_age1-fm_age1
  fa_age2 = 1-fs_age2-fm_age2
  fa_age3 = 1-fs_age3-fm_age3
  fa_age4 = 1-fs_age4-fm_age4
  
  fs = fs_age1*age1 + fs_age2*age2 + fs_age3*age3 + fs_age4*age4
  fm = fm_age1*age1 + fm_age2*age2 + fm_age3*age3 + fm_age4*age4
  fa = fa_age1*age1 + fa_age2*age2 + fa_age3*age3 + fa_age4*age4
  
  return(list(ts = ts,
              tm = tm,
              ta = ta,
              ts_age1 = ts_age1,
              ts_age2 = ts_age2,
              ts_age3 = ts_age3,
              ts_age4 = round(ts_age4,1),
              tm_age1 = tm_age1,
              tm_age2 = tm_age2,
              tm_age3 = tm_age3,
              tm_age4 = tm_age4,
              ta_age1 = ta_age1,
              ta_age2 = ta_age2,
              ta_age3 = ta_age3,
              ta_age4 = round(ta_age4,1),
              fs = fs,
              fm = fm,
              fa = fa,
              fs_age1 = fs_age1,
              fs_age2 = fs_age2,
              fs_age3 = fs_age3,
              fs_age4 = fs_age4,
              fm_age1 = fm_age1,
              fm_age2 = fm_age2,
              fm_age3 = fm_age3,
              fm_age4 = fm_age4,
              fa_age1 = fa_age1,
              fa_age2 = fa_age2,
              fa_age3 = fa_age3,
              fa_age4 = fa_age4,
              age1 = age1,
              age2 = age2,
              age3 = age3,
              age4 = age4
  ))
}
