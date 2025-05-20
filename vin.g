/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  checks if variable is in a data buffer
**
**  tst = vin(dbuf,"var");
**
**  dbuf = gauss data buffer
**  var  = name of variable
**
**  tst = 1 if var is in dbuf and 
**        0 if not
*/
proc vin(dbuf,str);
  local cv,strc;
  cv=vnamecv(dbuf);
  strc=strput(str,"        ",1);
  retp(in(strc,cv,0));
endp;
