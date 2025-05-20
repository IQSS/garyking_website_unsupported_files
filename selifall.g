/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
** apply selif to a set of variables
**
** usage:   call selifall(vars,ck);
**
**          vars = Nx1 vector of symbols representing existing vector names
**          ck = vector of 1's to delete and 0's to keep rows.
**
**          output:  each existing vector, whose symbol is named in vars,
**                   will be selif'd with condition ck.
*/
proc (0)=selifall(vars,ck);
    local t,i,tst;
    if ((cols(vars)/=1).or(cols(ck)/=1));
      errorlog "args must be vecs";
      end;
    endif;
    i=1;do while i<=rows(vars);
        t=selif(varget(vars[i]),ck);
        tst=varput(t,vars[i]);
        if tst/=1;errorlog "problem VARPUTting";end;endif;
    i=i+1;endo;
    endp;

