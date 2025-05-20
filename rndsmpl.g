/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
** random sample with replacement 
**
** a = rndsmpl(dta,m);
**
** INPUTS:
** dta = a data matrix
** m = number of random draws WITH replacement
**
** OUTPUT:
** a = dta with m rows randomly selected
*/
proc rndsmpl(dta,m);
  local k,inx;
  redo:
  k=rndu(m,1);
  if minc(k)==0; 
    goto redo; 
  endif;
  inx = ceil(k*rows(dta));
  retp(dta[inx,.]);
endp;
