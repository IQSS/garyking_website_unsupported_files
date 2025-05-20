/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  log of the multivariate normal pdf
**
**  y = lnpdfmn(y,mu,vc);
**
**  y = kx1
** mu = kx1 vector of means
** vc = kxk var covar matrix
**
** y = log of the Multivariate Normal Density (scalar)
**
** GLOBAL:  _Eivc = invpd(vc) to save computation time (kxk)
**                  or missing to compute 
**
** NOTE:  detl (system global) will be used, so do not call invpd() except
**        to define _Eivc before calling this proc.
*/
proc lnpdfmn(y,mu,vc);
  local a,b,c,k,ivc,w,ymu,res;
  if ismiss(_Eivc);
    ivc=invpd(vc);
  else;
    ivc=_Eivc;
  endif;
  ymu=y-mu;

  res=-(rows(mu)*1.83787706640934548+ln(detl)+ymu'ivc*ymu)/2;
  
  retp(res);
endp;
