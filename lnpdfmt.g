/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
** log of the multivariate t pdf
**
** y = lnpdfmt(y,mu,vc,df);
**
**  y = kx1
** mu = kx1 vector of means
** vc = kxk var covar matrix
** df = degrees of freedom
**
** y = log of Multivariate T pdf without normalizing constant (scalar)
**
** GLOBAL:  _Eivc = invpd(vc) to save computation time or missing to compute
**          must be kxk.
**
** NOTE:  detl (system global) will be used, so do not call invpd() except
**        to define _Eivc before calling this proc.
*/
proc lnpdfmt(y,mu,vc,df);
  local a,b,c,k,ivc,w,ymu;
  k=rows(mu);

  ymu=y-mu;
  if ismiss(_Eivc);
    ivc=invpd(vc);
  else;
    ivc=_Eivc;
  endif;
  
  w=ymu'ivc*ymu;
  b=-0.5*ln(detl);
  c=-(df+k).*ln(1+w./df)./2;
  retp(b+c);
endp;
