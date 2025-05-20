/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  vput a vector of variables into a data buffer
**
**  dbuf = vputall(vrs);
**
**  vrs = character matrix of global variables to store in data buffer
**
**  dbuf is output data buffer
**
** e.g.: let vrs = this that then;
**       buf = vputall(vrs);
**
*/
proc vputall(vrs);
  local i,nums,dbuf;
  dbuf="";
  i=1;
  do while i<=rows(vrs);
    nums=varget(vrs[i]);
    dbuf=vput(dbuf,nums,vrs[i]);
    i=i+1;
  endo;
  retp(dbuf);
endp;
