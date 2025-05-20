/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
** dumps data buffer to memory in stored variable names
**
** call vreadall(dbuf);
**
** INPUT
** dbuf = input data buffer
**
** OUTPUT
** procedure dumps data buffer to memory in stored variable names
**
** GLOBAL
** _vreadallvrs = character vector of selected variables to read
*/
declare matrix _vreadallvrs ?= 0;
proc 0=vreadall(dbuf);
  local i,nums,str,cv,r;
  if scalzero(_vreadallvrs);
    cv=vnamecv(dbuf);
  else;
    cv=_vreadallvrs;
  endif;
  r=rows(cv);
  i=1;
  do while i<=r;
    str=strstrip(""$+cv[i]);    /* force type to string */
    nums=vread(dbuf,str);
    if varput(nums,str)==0;
      "vreadall: varputting problems";
      stop;
    endif;
    i=i+1;
  endo;
endp;

