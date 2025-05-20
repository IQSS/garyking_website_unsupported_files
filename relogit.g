/*
** (C) Copyright 1999-2004 Gary King and Langche Zeng, All Rights Reserved.
** King@Harvard.Edu, langche@everest.fas.harvard.edu
** Current version available at http://GKing.Harvard.Edu
**
** RElogit: Rare Events Logistic regression.
**
** This procedure estimates the logit model with corrections for rare events,
** finite samples, and selection on Y.  It implements suggestions in
** Gary King and Langche Zeng's. "Logistic Regression in Rare Events
** Data," and "Inference in Case-Control Studies With Limited Auxilliary
** Information", both available at http://GKing.Harvard.Edu.  See
** ReLogitQ() to compute quantities of interest from relogit() results.
**
** USAGE
**
** library maxlik;
** {b,vc,llik,ret} = relogit(y,x);  @ estimates logit parameters @
** {Q.U} = ReLogitQ(x0,b,vc);  @ quantities of interest & uncertainty @
**
** INPUTS to relogit
**
** y = nx1 dependent variable vector
** x = nxk explanatory variable matrix (with constant in column 1)
**
** GLOBALS:  (let p=fraction of ones in the population, which may 
**                  systematically differ from mean(y) due to response-
**                  based sampling.  p may be known, completely unknown,
**                  or known to fall within an interval, [lo,hi])
**
** _relogit_sel = 0  no correction for response-based sampling. For use with 
**                   sampling designs that are at random or stratified
**                   within categories of X (default).
**              1|p  run logit and then prior-correct based on known p.  For 
**                   use with response-based sampling designs.
**          1|lo|hi  Use if p is known only to be in the interval [lo,hi]
**                   (or completely unknown, [0,1]).  This option changes 
**                   corrects and also changes the output b, which can then 
**                   be used in ReLogitQ to get bounds on probabilities,
**                   relative risks, risk differences, and number needed to
**                   treat.
**              2|p  run logit by weighting based on p.  For use with 
**                   response-based sampling designs, and when 
**                   efficency is unimportant and/or the odds of 
**                   misspecification are high. (2|lo|hi is not presently
**                   allowed)
**
** _relogit_McN = 0  do not correct for small sample & rare events bias
**                1  use McCullagh/Nelder bias correction as modified by
**                   King/Zeng to work with prior correction & weighting
**                   (default)
**                2  use Firth's penalized likelihood for bias correction
**                   (Not presently available if _relogit_sel = 2|p)
**
** _relogit_st  = kx1 vector of user specified starting values or
**                kx1 vector of normal random numbers (default)
**
** _relogit_out = 0 if turn off unnecessary printing (default)
**                1 print intermediate results from iteration
**
**
** OUTPUTS
**
** b    = kx1 vector of coefficients; or if _relogit_sel=1|lo|hi, 
**        (k+3)x1 vector = lo|hi|meanc(y)|coefficients
** vc   = kxk variance matrix
** llik = scalar log-likelihood at the maximum
** ret  = maxlik return code
**
**
** OUTPUT GLOBAL:
**
** _relogit_mx = global, kx1 (if p is known) or kx2 (if p is known to
**	fall in [lo,hi]) matrix of means of the x variables (including the
**	constant term) in the population, useful for setting x values in
**	calling relogitq.
** _relogit_n  = global, 2x1 matrix of numbers of 0's and 1's in the sample.
**      Maybe needed in relogitq.g in recoding lo/hi.
**
*/
declare matrix _relogit_sel=0;
declare matrix _relogit_McN=1;
declare matrix _relogit_st=0;
declare matrix _relogit_out=0;
declare matrix _relogit_mx=0;
declare matrix _relogit_n=0;

proc (4)=relogit(y,x);
  local k,s,n,ret,b,vc,g,p,w,inverse,q,ksi,biasMcN,llik,my,stval,w1,w2,wti;

  /* verify inputs */
  if _relogit_sel[1]==0;
    if rows(_relogit_sel)/=1;
      "_relogit_sel error";end;
    endif;
  elseif _relogit_sel[1]==1;
    if rows(_relogit_sel)==2;
      if _relogit_sel[2]>1 or _relogit_sel[2]<0;
	"_relogit_sel error: p must be in [0,1]";end;
      endif;
    elseif rows(_relogit_sel)==3;
      if _relogit_sel[2]>=_relogit_sel[3];
	"_relogit_sel error: lo must be < hi";end;
      endif; 
      if _relogit_sel[2]>1 or _relogit_sel[2]<0 or
	 _relogit_sel[3]>1 or _relogit_sel[3]<0;
        "_relogit_sel error: lo and hi must be within [0,1]";end;
      endif;
    else;
      "_relogit_sel may have only 2 or 3 rows";end;
    endif;
  elseif _relogit_sel[2]==2;  
    if rows(_relogit_sel)/=2;
      "if _relogit_sel[1]==2, then p must be the 2nd row";end;
    endif;
    if _relogit_sel[2]>1 or _relogit_sel[2]<0;
      "_relogit_sel[2] must be between 0 and 1";end;
    endif;
    if  _relogit_McN==2;
      "Firth correction and weighting cannot be used together presently";end;
    endif;
  endif;
  if  not(in(_relogit_McN,0|1|2,1));
    "_relogit_McN must be either 0, 1 or 2";
    end;
  endif;
  k=cols(x);
  if not(rows(_relogit_st)==k or scalzero(_relogit_st));
    "_relogit_st error: must be 0 or a rows(X)x1 vector";end;
  endif;
  
  /* setup maxlik globals */
  if scalzero(_relogit_st);
    stval=rndn(k,1);
  else;
    stval=_relogit_st;
  endif;
  if _relogit_out==0;
    _max_diagnostic=miss(0,0);     @ turn off unnecessary printing @   
    __output=0;                 
  endif;
  _max_covpar=3;        @ White se's  @
  _max_gradCheckTol=0; 

  /* run maxlik */
  if _relogit_McN==2; @ proceed numerically only for Firth's correction @
    _max_HessProc = 0; 
    _max_GradProc = 0;
    {b,llik,g,vc,ret}=maxlik(y~x,0,&llf,stval); 
  else;
    _max_HessProc = &hs; 
    _max_GradProc = &gd;
    {b,llik,g,vc,ret}=maxlik(y~x,0,&ll,stval); 
  endif;
  if scalmiss(vc);
    vc=miss(ones(k,k),1);
  endif;
  if scalmiss(b);
    b=miss(ones(k,1),1);
  endif;
  
  /* McN Bias Correction */
  {w1,w2,wti}=relogit_wt(y);
  if _relogit_McN==1;  
    p=logiti(x*b);
    w=(p-p^2).*wti;
    trap 1;
    inverse=invpd(x'.*w'*x);        @ kxk @
    trap 0;
    if scalmiss(inverse);
      biasMcN=miss(ones(k,1),1);
    else;
      q=sumc((x*inverse.*x)');      @ nx1 @
      ksi=0.5*q.*((1+w1).*p-w1);    @ nx1 @
      biasMcN=inverse*x'*(w.*ksi);  @ kx1 @
    endif;
    b=b-biasMcN;		    @ correct coeffs @
    n=rows(y);
    vc=((n/(n+k))^2)*vc;	    @ correct vc @
  endif;
  
  /* intercept prior correction */
  my=meanc(y);
  if _relogit_sel[1]==1;
    if rows(_relogit_sel)==2;
      b[1]=b[1]-(ln(1-_relogit_sel[2])-ln(_relogit_sel[2])+ln(my)-ln(1-my)); 
    else;
      b=_relogit_sel[2 3]|my|b;
    endif;
  endif;

 /* output global _relogit_n: */
 _relogit_n=(rows(y)-sumc(y))|sumc(y);

 /* output global _relogit_mx: */
  if _relogit_sel[1]==0;
     _relogit_mx=meanc(x);
  elseif  rows(_relogit_sel)==2;
     _relogit_mx=_relogit_sel[2]*meanc(selif(x,y.==1))+
                 (1-_relogit_sel[2])*meanc(selif(x,y.==0));
  else;
     _relogit_mx=(_relogit_sel[2]*meanc(selif(x,y.==1))+
                 (1-_relogit_sel[2])*meanc(selif(x,y.==0)))~
                 (_relogit_sel[3]*meanc(selif(x,y.==1))+
                 (1-_relogit_sel[3])*meanc(selif(x,y.==0)));
  endif;
  retp(b,vc,llik,ret);
endp;

/** loglik proc used in relogit **/
proc ll(b,dta);
  local wti,y,llik,w1,w2; 
  y=dta[.,1];
  {w1,w2,wti}=relogit_wt(y);
  llik=-wti.*ln(1+exp((1-2*y).*(dta[.,2:cols(dta)]*b)));
  retp(llik);
endp;  

/** loglik proc used in relogit with Firth's correction**/
proc llf(b,dta);
  local y,llik,p,w,x; 
  x=dta[.,2:cols(dta)];
  y=dta[.,1];
  p=logiti(x*b);
  w=p-p^2;
  trap 1; 
  llik=-ln(1+exp((1-2*y).*(x*b)))+ 0.5*ln(det(x'.*w'*x))/rows(y);
  trap 0;
  retp(llik);
endp;  

/** gradient proc used in relogit **/
proc gd(b,dta);
  local x,y,wti,w1,w2;
  y=dta[.,1];
  {w1,w2,wti}=relogit_wt(y);
  x=dta[.,2:cols(dta)];
  retp((w2.*y-wti.*(1./(1+exp(-x*b)))).*x);
endp;

/** hessian proc used in relogit **/
proc hs(b,dta);
  local x,p,hss,y,w1,w2,wti;
  y=dta[.,1];
  {w1,w2,wti}=relogit_wt(y);
  x=dta[.,2:cols(dta)];
  p=logiti(x*b);
  p=wti.*p.*(p-1);
  retp(x'*(x.*p));
endp;

/* support proc for relogit; creates weights */
proc (3)=relogit_wt(y);
  local w1,w2,wti,my;
  if _relogit_sel[1]==2;
    my=meanc(y);
    w1=((1-_relogit_sel[2])/(1-my));
    w2=(_relogit_sel[2]/my);
    wti=w1.*(1-y)+w2.*y; 
  else;
    w1=1;
    w2=1;
    wti=1;
  endif;  
  retp(w1,w2,wti);
endp;
