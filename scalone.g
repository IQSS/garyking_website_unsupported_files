/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
** check for scalar one
**
**  t = scalone(y);
**
**  y = matrix
**  t = 1 if y is a scalar one and 0 otw
**
*/
proc scalone(y);
    if ismiss(y);
        retp(0);
    endif;
    if rows(y)==1 and cols(y)==1 and y==1;
        retp(1);
    else;
        retp(0);
    endif;
    endp;

