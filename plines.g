/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**
**  add a set of lines to a graph
**
**  call plines(x,y);
**
**  for globals, see pline()
*/
proc 0=plines(x,y);
  if rows(x)<2 or rows(y)<2 or rows(x)/=rows(y);
    errorlog "plines: input error";
    stop;  
  endif;
  pline(trimr(lag(x),1,0),trimr(lag(y),1,0),trimr(x,1,0),trimr(y,1,0));
endp;
