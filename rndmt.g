/*
**  (C) Copyright 2000 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
** random multivariate t draws
**
** y = rndmt(mu,vc,df,n);
**
** inputs: mu = kx1 means
**         vc = kxk variance matrix
**         df = scalar degrees of freedom
**          n = scalar number of simulations
**
** output:  y = nxk matrix of dependent Multivariate T Random Variables
**              each row of y is one 1xk simulation
**
** example:
**
**      c=rndn(30,5);
**      vc=c'c;                 @ some theoretical var-cov matrix,
**                                c'c for the example to assure vc ispos def @
**      mu=0|5|-10|130|3;
**      y=rndmt(mu,vc,3,500);
**      "the theoretical correlation matrix";;
**          d=1/sqrt(diag(vc));   d.*vc.*d';?;
**      "simulated correlation matrix";;  corrx(y);
**      "theoretical mean matrix: ";;     mu';
**      "simulated mean matrix:   ";;     meanc(y)';
**
**  History:  
**  12/6/94  Added capability to do cholsky decomposition on matrices 
**           that have symmetric columns and rows of all zeros -- e.g.,
**           the vc matrix returned from a restricted regression.  Also,
**           vectorized computation of result.  Curt Signorino 
*/
proc rndmt(mu,vc,df,n);
  local k,c,r,i,t,vcd,ad,a,res;
  k=rows(mu);
  c=cols(mu);
  r=rows(vc);
  if ((r/=k).or(cols(vc)/=k)).and(r/=1);
    errorlog "rndmt: mu must be kx1, and vc kxk or scalar"; 
    stop;
  endif;
  if n<1; 
    errorlog "rndmt: number of simulations must be >=1   "; 
    stop;
  endif;
  if c/=1 and c/=n;
    errorlog "rndmt: mu must be kxn or kx1";
    end;
  endif;
  
  if vc==0; 
    retp(mu'.*ones(n,1)); 
  endif;
  i=sumc(dotfeq(vc,0)).==r; @ which columns are all zeros?  @
  if sumc(i)==0;            @ no all-zero columns/rows      @
    a=chol(vc)';            @ matrix square root function   @
  else;                     @ all-zero columns/rows exist   @
    t=delif(eye(r),i);      @ create transform matrix       @
    vcd=t*vc*t';            @ create nonsingular submatrix  @
    ad=chol(vcd);           @ cholsky decomp of submatrix   @
    a=t'ad*t;               @ rebuild full square-root matrix @
  endif;
  res=(mu+a*(rndn(k,n).*sqrt(df./rndchi(1,n,df))))';
  retp(res);
endp;
