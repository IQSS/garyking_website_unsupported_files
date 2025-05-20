/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
** random sample without replacement 
**
** a = rndsmpl2(dta,m);
**
** INPUTS:
** dta = a data matrix
** m = number of random draws WITHOUT replacement (m<=rows(dta))
**
** OUTPUT:
** a = a matrix comoposed of m randomly selected rows of dta
*/
proc rndsmpl2(dta,m);
  local k,inx,r;
  r=rows(dta);
  if m>r;
    "rndsmpl2: m must be <= dta";
    stop;
  endif;
  inx=sortind(rndu(r,1));
  dta=dta[inx,.];
  retp(dta[1:m,.]);
endp;
