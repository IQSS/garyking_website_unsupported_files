/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
** randomly permute rows of a set of variables
**
** usage:   call rndinx(vars);
**
**          vars = Nx1 vector of symbols representing existing vector names
**
**          output:  each existing vector, whose symbol is named in vars,
**                   will be randomly permuted in the same way
*/
proc (0)=rndinx(vars);
  local t,i,tst,inx,n;
  n=rows(varget(vars[1]));
  inx=sortind(rndu(n,1));
  i=1;
  do while i<=rows(vars);
    t=varget(vars[i]);
    tst=varput(t[inx],vars[i]);
    if tst/=1;
      errorlog "rndinx: problem VARPUTting";
      end;
    endif;
    i=i+1;
  endo;
endp;
  
  