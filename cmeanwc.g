/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  conditional weighted means
**
**  {ux,a,nx} = cmeanc(y,x,w);
**
** INPUTS:
**  y = n x 1 vector
**  x = n x 1 vector of values to compute meanc(y|x)
**  w = n x 1 vector of weights
**
** OUTPUTS:
**  ux = list of unique values of x
**  a  = meanc(y|x for each value of ux)
**  nx = number of y's within each mean calculation
**
*/
proc 3=cmeanwc(y,x,w);
  local a,ux,r,t,i,nx,tw,s;
  if (rows(y) /= rows(x)) or (rows(y)/=rows(w));
    "cmeanc error: rows need to be same size"; 
    end; 
  endif;
  if cols(y) /= 1 or cols(x) /= 1 or cols(w)/=1;
    "cmeanc error: use only 1 column";
    end;
  endif;
  ux=unique(x,1);
  r=rows(ux);
  a=zeros(r,1);
  nx=a;
  i=1;do while i<=r;
    s=(x.==ux[i]);
    t=selif(y,s);
    tw=selif(w,s);
    a[i]=meanwc(t,tw);
    nx[i]=rows(t);
    i=i+1;
  endo;
  retp(ux,a,nx);
endp;

