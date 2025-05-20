/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  check for scalar zero
**
**  t = scalzero(y);
**
**  y = matrix
**  t = 1 if y is a scalar zero and 0 otw
**
*/
proc scalzero(y);
    if ismiss(y);
        retp(0);
    endif;
    if rows(y)==1 and cols(y)==1 and y==0;
        retp(1);
    else;
        retp(0);
    endif;
    endp;

