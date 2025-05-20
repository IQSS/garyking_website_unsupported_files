/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  creates vector of evenly spaced points 
**
**  y = seqas(a,b,n);
**
**  y = vector of n points evenly spaced between a and b.  Y does not
**      include a or b
*/
proc seqas(strt,endd,pts);
  local res;
  res=(endd-strt)/pts;
  res=seqa(strt+0.5*res,res,pts);
  retp(res);
endp;
