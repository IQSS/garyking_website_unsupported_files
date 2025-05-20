/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
** unstandardized cumulative normal distribution
**
** p = cdfnorm(y,mu,sigma2);
**
** INPUTS:
** mu = mean
** sigma2 = variance
** y = normal variate
**
** OUTPUT:
** p = Prob(Y<y|mu,sigma2), where y is a realization of the r.v. Y
**
*/
proc cdfnorm(y,mu,sigma2);
  local sigma;
  if sigma2<=0;
    "cdfnorm: variance must be positive";
    end;
  endif;
  sigma=sqrt(sigma2);
  retp( cdfn((y-mu)./sigma) );
endp;
