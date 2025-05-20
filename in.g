/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  Tests whether one set is contained in another
**  (useful for simplifying complex if statements, among other things)
**
** usage:  tst = in(y,vars,flag);
**
**         y    = a scalar or matrix
**         vars = a scalar or matrix
**         flag = 0 for character, 1 for numeric.  if flag=0, y and vars will
**                all be converted to upper case for comparison.
**
**         tst  = 1 if y is contained in vars and 0 otherwise
*/
external proc setdif;
proc in(y,vars,flag);
    local t;
    if flag==0;y=upper(y);vars=upper(vars);endif;
    t=ismiss(setdif(y,vars,flag));
    retp(t);endp;
