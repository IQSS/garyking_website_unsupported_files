/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  weighted mean (ok with missing values)
**
**  y = meanWc(x,wt);
**
**  x = var to be avg'd
**  wt = weight used in averaging
*/
proc meanWc(x,wt);
  local a,wwt;
  if scalmiss(packr(wt)) or wt==1;
    wt=ones(rows(x),1);
  endif;
  wwt=missrv(wt,0);
  a=sumc(missrv(x,0).*wwt)./sumc((1-ismissm(x+wt)).*wwt);
  retp(a);
endp;
