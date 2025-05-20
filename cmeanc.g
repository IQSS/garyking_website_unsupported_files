/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  conditional means
**
**  {ux,a,nx} = cmeanc(y,x);
**
** INPUTS:
**  y = n x 1
**  x = n x 1 vector of values to compute meanc(y|x)
**
** OUTPUTS:
**  ux = list of unique values of x
**  a  = meanc(y|x for each value of ux)
**  nx = number of y's within each mean calculation
**
*/
proc 3=cmeanc(y,x);
  local a,ux,r,t,i,nx;
  if rows(y) /= rows(x);
    "cmeanc error: rows need to be same size"; 
    end; 
  endif;
  if cols(y) /= 1 or cols(x) /= 1;
    "cmeanc error: use only 1 column";
    end;
  endif;
  ux=unique(x,1);
  r=rows(ux);
  a=zeros(r,1);
  nx=a;
  i=1;do while i<=r;
    t=selif(y,x.==ux[i]);
    a[i]=meanc(t);
    nx[i]=rows(t);
    i=i+1;
  endo;
  retp(ux,a,nx);
endp;

