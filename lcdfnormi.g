/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  lncdfn2 for an unstandarized normal
**
**  y = Lcdfnormi(bounds,mu,sig2);
**
**  y = ln( cdfnorm(bounds[.,2],mu,sig2) - cdfnorm(bounds[.,1],mu,sig2) );
**  but much more numerically precise than doing it this way.
**
*/
proc lcdfnormi(bounds,mu,sig2);
  local res,a,b,res2,T,c1,c2,c3,c4,ba,c0,r,la,lb,lma,lmb,rs,s,bs,as,m,rb;
  rs=rows(mu);
  rb=rows(bounds);
  if rb/=1 and rb/=rs;
    "lcdfnormi: input error";
  elseif rb==1;
    bounds=ones(rs,2).*bounds;
  endif;
  a=bounds[.,1];
  b=bounds[.,2];
  if sumc((b-a).<0)>0;
    "lcdfnormi: bounds error";
    stop;
  endif;
  s=sqrt(sig2);
  bs=(b-mu)./s;
  as=(a-mu)./s;

  m={.};
  res=infrv(lncdfn2(as,bs-as),m,m);
  
  retp(res);
endp;
