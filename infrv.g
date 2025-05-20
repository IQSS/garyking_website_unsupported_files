/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
** reverse (recode) infinities
**
** y = infrv(x,minus,plus);
**
** x = input vector
** minus, plus = scalars
**
** y = an ExE conformable matrix with -INF changed to minus and +INF changed
**       to plus
*/
proc infrv(x,m,p);
  local plus,minus,res,s,pinf,minf;
  plus = 0v7ff0000000000000;
  minus = 0vfff0000000000000;
  s=seqa(1,1,rows(x));

  pinf=selif(s,(.not(x ./= plus)));
  minf=selif(s,(.not(x ./= minus)));
  res=x;
  
  if not(scalmiss(pinf));
    res[pinf]=p.*ones(rows(pinf),1);
  endif;
  
  if not(scalmiss(minf));
    res[minf]=m.*ones(rows(minf),1);
  endif;
  retp(res);
endp;
