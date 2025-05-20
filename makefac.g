/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  create factorial designs
**
**  y = makefac(vars,levels);
**
**  vars = number of columns to make in y
**  levels= number of levels of y
**
**  y = (levels^vars x vars).  first column is 1,2,3,...,vars,
**      second column is 1,1,1,1(vars times),2,2,2,, etc
**      third column...
**   
** example:
** y = makefac(2,4);
** y;
**  1   1
**  2   1
**  3   1
**  4   1
**  1   2
**  2   2
**  3   2
**  4   2
**  1   3
**  2   3
**  3   3
**  4   3
**  1   4
**  2   4
**  3   4
**  4   4
**
** NOTE: round() corrects for numerical inaccuracies.
*/
proc (1) = makefac(vars,levels);
  local i,tmp;
  if rows(vars)/=1 or rows(levels)/=1 or cols(vars)/=1 or cols(levels)/=1;
    "makefac: arguments must be scalars";
    stop;
  endif;
  tmp=ones(round(levels^vars),1);
  #ifdos;
  i=1; do while i<=vars;
  #else;
  for i (1, vars, 1);
  #endif;
    tmp=tmp~facvec(i+0,vars,levels);
  #ifdos;
  i=i+1; endo;
  #else;
  endfor;
  #endif;
  retp(tmp[.,2:cols(tmp)]);
endp;


/*  facvec: support proc for makefac
**  
   y = facvec(i,v,l);
**
** i = var number
** v = value
** l = level
**
** y = output column vector
**
** round() corrects for numerical inaccuracies.
*/
fn facvec(i,v,l)=vec(vec((seqa(1,1,l)'
                 .*ones(round(l^(i-1)),l)))
		 .*ones(1,round(l^(v-i))));
