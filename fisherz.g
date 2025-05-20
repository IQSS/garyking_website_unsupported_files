/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**   z = fisherz(x);
**   fisher's z transformation
*/
proc fisherz(x);
  local t;
  t=0.5*ln((1+x)./(1-x));
  retp(t);
endp;
