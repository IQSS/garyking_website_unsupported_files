/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  create vector of evenly spaced points including endpoints
**
**  y = seqase(a,b,n);
**
**  y = vector of n evenly spaced points between a and b.  Y includes the
**      endpoints, a and b.
*/

proc seqase(strt,endd,pts);local t;t=(endd-strt)/(pts-1);
    retp( seqa(strt,t,pts) );endp;
