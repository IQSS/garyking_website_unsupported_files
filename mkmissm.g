/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  make elements of matrix missing
**
**  y = mkmissm(x,m);
**
**  x = n x k data matrix
**  m = n x k matrix of 0's and 1's
**
**  y[i,j] = missing if m[i,j]=1; else y[i,j]=x[i,j]
**
**  example:
**
**  m=ismissm(d);       /* remember which are missing */
**  d2=missrv(d,-9)     /* change missing to -9's */
**           /* do recodes to d2 as if no missing values were present */
**  d3=mkmissm(data2,m) /* after recodes, return to missing*/
*/
proc mkmissm(x,m);
    local y;
    y=miss(m,1)+x;
    retp(y);endp;

