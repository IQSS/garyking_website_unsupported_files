/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
** applies delif to a set of variables all at once
**
** usage:   delifall(vars,ck);
**
**  vars = Nx1 vector of symbols representing existing vector names
**    ck = vector of 1's to delete and 0's to keep rows.
**
**  output:  each existing global matrix, whose symbol is named in vars,
**           will be delif'd with condition ck.
**
*/
proc (0)=delifall(vars,ck);
    local t,i,tst;
    if ((cols(vars)/=1).or(cols(ck)/=1));
      errorlog "args must be vecs";
      end;
    endif;
    i=1;do while i<=rows(vars);
        t=delif(varget(vars[i]),ck);
        tst=varput(t,vars[i]);
        if tst/=1;errorlog "problem VARPUTting";end;endif;
    i=i+1;endo;
    endp;
