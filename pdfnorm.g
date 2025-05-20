/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
** pdf of an unstandardized normal
**
** p = pdfnorm(y,mu,sigma2);
**
** INPUTS:
** mu = scalar mean 
** sigma2 = scalar variance
** y = normal variate
**
** OUTPUT:
** p value of the probability density
**
*/
proc pdfNorm(y,mu,s2);
  local res,s;
  s=sqrt(s2);
  res=pdfn((y-mu)./s)./s;
  retp(res);
endp;
