
each_generation_withZ = function(N,D,ta,ts,Total,beta,alpha,c_pos,w){
  tf = find_t_f(ta=ta,ts=ts)
  
  ts = tf$ts
  ts_age1 = tf$ts_age1
  ts_age2 = tf$ts_age2
  ts_age3 = tf$ts_age3
  ts_age4 = tf$ts_age4
  
  tm = tf$tm
  tm_age1 = tf$tm_age1
  tm_age2 = tf$tm_age2
  tm_age3 = tf$tm_age3
  tm_age4 = tf$tm_age4
  
  ta = tf$ta
  ta_age1 = tf$ta_age1
  ta_age2 = tf$ta_age2
  ta_age3 = tf$ta_age3
  ta_age4 = tf$ta_age4
  
  fs = tf$fs
  fs_age1 = tf$fs_age1
  fs_age2 = tf$fs_age2
  fs_age3 = tf$fs_age3
  fs_age4 = tf$fs_age4
  
  fm = tf$fm
  fm_age1 = tf$fm_age1
  fm_age2 = tf$fm_age2
  fm_age3 = tf$fm_age3
  fm_age4 = tf$fm_age4
  
  fa = tf$fa
  fa_age1 = tf$fa_age1
  fa_age2 = tf$fa_age2
  fa_age3 = tf$fa_age3
  fa_age4 = tf$fa_age4
  
  # age information
  age1 = tf$age1
  age2 = tf$age2
  age3 = tf$age3
  age4 = tf$age4
  
  ################################################
  Ns.age1 = (ts_age1*D+fs_age1*(N-D))*age1
  Ns.age2 = (ts_age2*D+fs_age2*(N-D))*age2
  Ns.age3 = (ts_age3*D+fs_age3*(N-D))*age3
  Ns.age4 = (ts_age4*D+fs_age4*(N-D))*age4
  Nm.age1 = (tm_age1*D+fm_age1*(N-D))*age1
  Nm.age2 = (tm_age2*D+fm_age2*(N-D))*age2
  Nm.age3 = (tm_age3*D+fm_age3*(N-D))*age3
  Nm.age4 = (tm_age4*D+fm_age4*(N-D))*age4
  Na.age1 = (ta_age1*D+fa_age1*(N-D))*age1
  Na.age2 = (ta_age2*D+fa_age2*(N-D))*age2
  Na.age3 = (ta_age3*D+fa_age3*(N-D))*age3
  Na.age4 = (ta_age4*D+fa_age4*(N-D))*age4
  # population_distribution = c(Ns.age1, Ns.age2, Ns.age3, Ns.age4,
  #                             Nm.age1, Nm.age2, Nm.age3, Nm.age4,
  #                             Na.age1, Na.age2, Na.age3, Na.age4)
  
  
  ################################################
  N_sym = c((ts_age1*D+fs_age1*(N-D))*age1,(ts_age2*D+fs_age2*(N-D))*age2,(ts_age3*D+fs_age3*(N-D))*age3,(ts_age4*D+fs_age4*(N-D))*age4,
            (tm_age1*D+fm_age1*(N-D))*age1,(tm_age2*D+fm_age2*(N-D))*age2,(tm_age3*D+fm_age3*(N-D))*age3,(tm_age4*D+fm_age4*(N-D))*age4,
            (ta_age1*D+fa_age1*(N-D))*age1,(ta_age2*D+fa_age2*(N-D))*age2,(ta_age3*D+fa_age3*(N-D))*age3,(ta_age4*D+fa_age4*(N-D))*age4)
  B = c(((1-beta)*ts_age1*D+alpha*fs_age1*(N-D))*age1,((1-beta)*ts_age2*D+alpha*fs_age2*(N-D))*age2,((1-beta)*ts_age3*D+alpha*fs_age3*(N-D))*age3,((1-beta)*ts_age4*D+alpha*fs_age4*(N-D))*age4,
        ((1-beta)*tm_age1*D+alpha*fm_age1*(N-D))*age1,((1-beta)*tm_age2*D+alpha*fm_age2*(N-D))*age2,((1-beta)*tm_age3*D+alpha*fm_age3*(N-D))*age3,((1-beta)*tm_age4*D+alpha*fm_age4*(N-D))*age4,
        ((1-beta)*ta_age1*D+alpha*fa_age1*(N-D))*age1,((1-beta)*ta_age2*D+alpha*fa_age2*(N-D))*age2,((1-beta)*ta_age3*D+alpha*fa_age3*(N-D))*age3,((1-beta)*ta_age4*D+alpha*fa_age4*(N-D))*age4)
  
  N_sym_r = N_sym[-(1:4)]
  B_r = B[-(1:4)]
  Ps = sum(B[1:4])
 
  ######## use 'optiSolve' for optimization ######
  
  Q = w * B_r %*% t(B_r) + (1-w)*(B_r-c_pos*N_sym_r)%*%t(B_r-c_pos*N_sym_r)
  a = 2*w*(Ps-D)*B_r+2*(1-w)*(Ps-c_pos*sum(N_sym[1:4]))*(B_r-c_pos*N_sym_r)
  d = w*(Ps-D)^2+(1-w)*(Ps-c_pos*sum(N_sym[1:4]))^2
  
  f = optiSolve::quadfun(Q=Q,a=a,d=d)
  
  # set up the constraints, only input the coefficient
  const1 = N_sym_r/N
  A = matrix(c(const1),nrow=1,byrow = TRUE)
  
  dir = c("<=")
  val = c((Total-sum(N_sym[1:4]))/N)#+1e-8)
  
  colnames(A) = 1:ncol(A)
  rownames(A) = 1:nrow(A)
  lc = optiSolve::lincon(A=A,dir=dir,val=val,use=rep(TRUE,nrow(A)),id=colnames(A),name = 1:nrow(A))
  
  # minimum number of tests
  lbval = c(rep(0,8))
  ubval = c(rep(1,8))
  
  lbfunc = optiSolve::lbcon(val = lbval,id=colnames(A))
  ubfunc = optiSolve::ubcon(val = ubval,id=colnames(A))
  
  mycop = optiSolve::cop(f = f,
              max = FALSE,
              lb = lbfunc,
              ub = ubfunc,
              lc=lc)
  X.init = rep(1-1e-8,8)
  names(X.init) = colnames(A)
  
  optimal = optiSolve::solvecop(mycop, 
                     solver="default", 
                     quiet=TRUE,
                     X=X.init)
  optiSolve::validate(mycop,optimal)
  
  
  # selection probability
  prob = as.data.frame(c(1,1,1,1,optimal$x))
  rownames(prob) = c("selectProb.s.age1",
                     "selectProb.s.age2",
                     "selectProb.s.age3",
                     "selectProb.s.age4",
                     "selectProb.m.age1",
                     "selectProb.m.age2",
                     "selectProb.m.age3",
                     "selectProb.m.age4",
                     "selectProb.a.age1",
                     "selectProb.a.age2",
                     "selectProb.a.age3",
                     "selectProb.a.age4")
  
  # positive tests
  P.hat = sum(B*prob)
  
  # Number of tests to each group
  tests = N_sym*t(prob)
  
  # marginal selection probability
  ## severe group
  selectProb.s = sum(tests[1:4])/(Ns.age1+Ns.age2+Ns.age3+Ns.age4)
  ## mild group 
  selectProb.m = sum(tests[5:8])/(Nm.age1+Nm.age2+Nm.age3+Nm.age4)
  ## asymptomatics
  selectProb.a = sum(tests[9:12])/(Na.age1+Na.age2+Na.age3+Na.age4)
  ## age1
  selectProb.age1 = sum(tests[1]+tests[5]+tests[9])/(Ns.age1+Nm.age1+Na.age1)
  ## age2
  selectProb.age2 = sum(tests[2]+tests[6]+tests[10])/(Ns.age2+Nm.age2+Na.age2)
  ## age3
  selectProb.age3 = sum(tests[3]+tests[7]+tests[11])/(Ns.age3+Nm.age3+Na.age3)
  ## age4
  selectProb.age4 = sum(tests[4]+tests[8]+tests[12])/(Ns.age4+Nm.age4+Na.age4)
  
  
  
  return(list(ta=ta,
              Total = sum(tests),
              P.hat = P.hat,
              prob = prob,
              assignedTests=tests,
              selectProb.s = selectProb.s,
              selectProb.m = selectProb.m,
              selectProb.a = selectProb.a,
              selectProb.age1 = selectProb.age1,
              selectProb.age2 = selectProb.age2,
              selectProb.age3 = selectProb.age3,
              selectProb.age4 = selectProb.age4
  ))
}