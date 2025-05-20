/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  add a line to a graph
**
**  pline(xstart,ystart,xend,yend);
**
**
*/
declare matrix _pline ?=0;
declare matrix _plinet ?=6;     /* line type */
declare matrix _plineth ?=0;    /* line thickness */
declare matrix _plineC ?= 14;		@ color  @
declare matrix _plineRev ?= 0;    /* draw these lines 0=last 1=first */
proc 0=pline(xst,yst,xend,yend);
  local res,o;
  o=ones(rows(xst),1);
  res=o*1~o*_plinet~xst~yst~xend~yend~o*1~o*_plineC~o*_plineth;
  if _pline==0;
    _pline=res;
  elseif _plineRev;
    _pline=res|_pline;
  else;
    _pline=_pline|res;
  endif;
endp;
