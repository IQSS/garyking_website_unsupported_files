/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
** a computationally precise method of computing ln(pdfnorm())
**
**  a = lpdfnorm(y,mu,sig2);
**
*/
proc lpdfnorm(y,mu,sig2);
  local x;
  x=y-mu;
  retp( -.918938533204672741 - ( ln(sig2) +  ( (x.*x) / sig2 ) ) /2);
endp;
