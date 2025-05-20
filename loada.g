/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  load in ascii file
**
**  dta = loada(file,vars);
**
**  file = string with ascii file name
**  vars = number of variables in file
**
**  dta = matrix with the contents of file in it
*/
proc loada(file,vars);
  local d,n;
  loadm d[]=^file;
  n=rows(d)/vars;
  if n/=int(n);
    "Wrong number of vars, or not the same number in each row";end;
  endif;
  d=reshape(d,n,vars);
  retp(d);
endp;
