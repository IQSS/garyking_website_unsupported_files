/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
** element-by-element matrix version of ismiss
**
**  y = ismissm(x)
**
**      x = an n x m matrix
**
**      y = an n x m matrix of 0's (indicating not missing) and 1's (missing)
**
*/
proc ismissm(x);
    retp(missrv(x*0,1));
    endp;

