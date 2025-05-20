/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**   z = fisherzi(x);
**   inverse of Fisher's z transformation
**
*/
proc fisherzi(x);
  local t;
  t=exp(2*x);
  t=(t-1)./(t+1);
  retp(t);
endp;
