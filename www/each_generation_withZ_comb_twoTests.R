
each_generation_withZ_twoTests = function(N,D,ta,ts,money,y1,y2,beta1,alpha1,beta2, alpha2, agedist){
  N = N
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
  age1 = agedist[1]
  age2 = agedist[2]
  age3 = agedist[3]
  age4 = agedist[4]
  
  # warning message
  t_f_prob = c(ts_age1,ts_age2,ts_age3,ts_age4,
               tm_age1,tm_age2,tm_age3,tm_age4,
               ta_age1,ta_age2,ta_age3,ta_age4,
               fs_age1,fs_age2,fs_age3,fs_age4,
               fm_age1,fm_age2,fm_age3,fm_age4,
               fa_age1,fa_age2,fa_age3,fa_age4)
  if (any(t_f_prob <0) || any(t_f_prob>1)) warning(paste('When ta is',ta, ',','some tj(z) or fj(z) not between 0 and 1',sep = " "))
  
  
  
  ################################################
  N_sym1 = c((ts_age1*D+fs_age1*(N-D))*age1,(ts_age2*D+fs_age2*(N-D))*age2,(ts_age3*D+fs_age3*(N-D))*age3,(ts_age4*D+fs_age4*(N-D))*age4,
            (tm_age1*D+fm_age1*(N-D))*age1,(tm_age2*D+fm_age2*(N-D))*age2,(tm_age3*D+fm_age3*(N-D))*age3,(tm_age4*D+fm_age4*(N-D))*age4,
            (ta_age1*D+fa_age1*(N-D))*age1,(ta_age2*D+fa_age2*(N-D))*age2,(ta_age3*D+fa_age3*(N-D))*age3,(ta_age4*D+fa_age4*(N-D))*age4)
  N_sym2 = N_sym1
  B1 = c(((1-beta1)*ts_age1*D+alpha1*fs_age1*(N-D))*age1,((1-beta1)*ts_age2*D+alpha1*fs_age2*(N-D))*age2,((1-beta1)*ts_age3*D+alpha1*fs_age3*(N-D))*age3,((1-beta1)*ts_age4*D+alpha1*fs_age4*(N-D))*age4,
        ((1-beta1)*tm_age1*D+alpha1*fm_age1*(N-D))*age1,((1-beta1)*tm_age2*D+alpha1*fm_age2*(N-D))*age2,((1-beta1)*tm_age3*D+alpha1*fm_age3*(N-D))*age3,((1-beta1)*tm_age4*D+alpha1*fm_age4*(N-D))*age4,
        ((1-beta1)*ta_age1*D+alpha1*fa_age1*(N-D))*age1,((1-beta1)*ta_age2*D+alpha1*fa_age2*(N-D))*age2,((1-beta1)*ta_age3*D+alpha1*fa_age3*(N-D))*age3,((1-beta1)*ta_age4*D+alpha1*fa_age4*(N-D))*age4)
  B2 = c(((1-beta2)*ts_age1*D+alpha2*fs_age1*(N-D))*age1,((1-beta2)*ts_age2*D+alpha2*fs_age2*(N-D))*age2,((1-beta2)*ts_age3*D+alpha2*fs_age3*(N-D))*age3,((1-beta2)*ts_age4*D+alpha2*fs_age4*(N-D))*age4,
         ((1-beta2)*tm_age1*D+alpha2*fm_age1*(N-D))*age1,((1-beta2)*tm_age2*D+alpha2*fm_age2*(N-D))*age2,((1-beta2)*tm_age3*D+alpha2*fm_age3*(N-D))*age3,((1-beta2)*tm_age4*D+alpha2*fm_age4*(N-D))*age4,
         ((1-beta2)*ta_age1*D+alpha2*fa_age1*(N-D))*age1,((1-beta2)*ta_age2*D+alpha2*fa_age2*(N-D))*age2,((1-beta2)*ta_age3*D+alpha2*fa_age3*(N-D))*age3,((1-beta2)*ta_age4*D+alpha2*fa_age4*(N-D))*age4)
  

  
  
  ######## use 'optiSolve' for optimization ######
  
  B = c(B1,B2)
  N_sym = c(N_sym1,N_sym2)
  Q = B%*%t(B)
  a = -2*B*D
  d = D^2
  f = optiSolve::quadfun(Q=Q,a=a,d=d)
  
  
  # constraints
  const1 = c(y1*N_sym1,y2*N_sym2) # cost constraint
  const2.1 = c(N_sym1[1],rep(0,11),N_sym2[1],rep(0,11)) # P(s=1|Sym=s)=1
  const2.1_ = c(N_sym1[1],rep(0,11),N_sym2[1],rep(0,11)) # P(s=1|Sym=s)=1
  const2.2 = c(rep(0,1),N_sym1[2],rep(0,10),rep(0,1),N_sym2[2],rep(0,10)) # P(s=1|Sym=s)=1
  const2.2_ = c(rep(0,1),N_sym1[2],rep(0,10),rep(0,1),N_sym2[2],rep(0,10)) # P(s=1|Sym=s)=1
  const2.3 = c(rep(0,2),N_sym1[3],rep(0,9),rep(0,2),N_sym2[3],rep(0,9)) # P(s=1|Sym=s)=1
  const2.3_ = c(rep(0,2),N_sym1[3],rep(0,9),rep(0,2),N_sym2[3],rep(0,9)) # P(s=1|Sym=s)=1
  const2.4 = c(rep(0,3),N_sym1[4],rep(0,8),rep(0,3),N_sym2[4],rep(0,8)) # P(s=1|Sym=s)=1
  const2.4_ = c(rep(0,3),N_sym1[4],rep(0,8),rep(0,3),N_sym2[4],rep(0,8)) # P(s=1|Sym=s)=1
  
  ### CHANGED N_sym1 to N_sym2 in these formulas!!!
  const2.5 = c(rep(0,4),N_sym1[5],rep(0,7),rep(0,4),N_sym2[5],rep(0,7)) # P(s=1|Sym=s)=1
  const2.5_ = c(rep(0,4),N_sym1[5],rep(0,7),rep(0,4),N_sym2[5],rep(0,7)) # P(s=1|Sym=s)=1
  const2.6 = c(rep(0,5),N_sym1[6],rep(0,6),rep(0,5),N_sym2[6],rep(0,6)) # P(s=1|Sym=s)=1
  const2.6_ = c(rep(0,5),N_sym1[6],rep(0,6),rep(0,5),N_sym2[6],rep(0,6)) # P(s=1|Sym=s)=1
  const2.7 = c(rep(0,6),N_sym1[7],rep(0,5),rep(0,6),N_sym2[7],rep(0,5)) # P(s=1|Sym=s)=1
  const2.7_ = c(rep(0,6),N_sym1[7],rep(0,5),rep(0,6),N_sym2[7],rep(0,5)) # P(s=1|Sym=s)=1
  const2.8 = c(rep(0,7),N_sym1[8],rep(0,4),rep(0,7),N_sym2[8],rep(0,4)) # P(s=1|Sym=s)=1
  const2.8_ = c(rep(0,7),N_sym1[8],rep(0,4),rep(0,7),N_sym2[8],rep(0,4)) # P(s=1|Sym=s)=1
  const2.9 = c(rep(0,8),N_sym1[9],rep(0,3),rep(0,8),N_sym2[9],rep(0,3)) # P(s=1|Sym=s)=1
  const2.9_ = c(rep(0,8),N_sym1[9],rep(0,3),rep(0,8),N_sym2[9],rep(0,3)) # P(s=1|Sym=s)=1
  const2.10 = c(rep(0,9),N_sym1[10],rep(0,2),rep(0,9),N_sym2[10],rep(0,2)) # P(s=1|Sym=s)=1
  const2.10_ = c(rep(0,9),N_sym1[10],rep(0,2),rep(0,9),N_sym2[10],rep(0,2)) # P(s=1|Sym=s)=1
  const2.11 = c(rep(0,10),N_sym1[11],rep(0,1),rep(0,10),N_sym2[11],rep(0,1)) # P(s=1|Sym=s)=1
  const2.11_ = c(rep(0,10),N_sym1[11],rep(0,1),rep(0,10),N_sym2[11],rep(0,1)) # P(s=1|Sym=s)=1
  const2.12 = c(rep(0,11),N_sym1[12],rep(0,11),N_sym2[12]) # P(s=1|Sym=s)=1
  const2.12_ = c(rep(0,11),N_sym1[12],rep(0,11),N_sym2[12]) # P(s=1|Sym=s)=1
  
  
  A = matrix(c(const1,
               const2.1,const2.1_,
               const2.2,const2.2_,
               const2.3,const2.3_,
               const2.4,const2.4_,
               const2.5,const2.5_,
               const2.6,const2.6_,
               const2.7,const2.7_,
               const2.8,const2.8_,
               const2.9,const2.9_,
               const2.10,const2.10_,
               const2.11,const2.11_,
               const2.12,const2.12_),nrow=25,byrow = TRUE)

  
  dir = c("<=",
          "<=",">=",
          "<=",">=",
          "<=",">=",
          "<=",">=",
          "<=",">=",
          "<=",">=",
          "<=",">=",
          "<=",">=",
          "<=",">=",
          "<=",">=",
          "<=",">=",
          "<=",">=")
  val = c(money,
          N_sym[1],N_sym[1],
          N_sym[2],N_sym[2],
          N_sym[3],N_sym[3],
          N_sym[4],N_sym[4],
          N_sym[5],0,
          N_sym[6],0,
          N_sym[7],0,
          N_sym[8],0,
          N_sym[9],0,
          N_sym[10],0,
          N_sym[11],0,
          N_sym[12],0)
  
  colnames(A) = 1:ncol(A)
  rownames(A) = 1:nrow(A)
  lc = optiSolve::lincon(A=A,dir=dir,val=val,use=rep(TRUE,nrow(A)),id=colnames(A),name = 1:nrow(A))
  
  # minimum number of tests
  lbval = c(rep(0,length(N_sym)))
  ubval = c(rep(1,length(N_sym)))
  
  lbfunc = optiSolve::lbcon(val = lbval,id=colnames(A))
  ubfunc = optiSolve::ubcon(val = ubval,id=colnames(A))
  
  mycop = optiSolve::cop(f = f,
              max = FALSE,
              lb = lbfunc,
              ub = ubfunc,
              lc=lc)
  
  optimal = optiSolve::solvecop(mycop,
                     solver="alabama",
                     quiet=FALSE,
                     madeDefinite=TRUE)
  
  optiSolve::validate(mycop,optimal)
  
  #######################
  prob = optimal$x
  
  # positive tests
  P.hat = sum(B*prob)
  
  # Number of tests to each group
  tests = N_sym*t(prob)
  colnames(tests)=c("test1.s.age1","test1.s.age2","test1.s.age3","test1.s.age4",
                    "test1.m.age1","test1.m.age2","test1.m.age3","test1.m.age4",
                    "test1,a.age1","test1.a.age1","test1.a.age1","test1.a.age4",
                    "test2.s.age1","test2.s.age2","test2.s.age3","test2.s.age4",
                    "test2.m.age1","test2.m.age2","test2.m.age3","test2.m.age4",
                    "test2,a.age1","test2.a.age1","test2.a.age1","test2.a.age4")
  
  
  return(list(ta=ta,
              Total = sum(tests),
              P.hat = P.hat,
              prob = prob,
              asignedTests=tests
  ))
}
