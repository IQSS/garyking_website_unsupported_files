/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  create labeled factorial designs
**
**  y = makefacn(vars,nums);
**
**  vars   = number of columns to make in y
**  nums   = (levels x 1) vector of numbers to use in place of 1,2,3... in
**          makefac().
**       OR (levels x vars) matrix
**
** example:
**
**  nums=(1~ 2 ~3)|
**       (9~10~11);
**  call makefacn(3,nums);
**   
**   1    2    3 
**   9    2    3 
**   1   10    3 
**   9   10    3 
**   1    2   11 
**   9    2   11 
**   1   10   11 
**   9   10   11 
**
*/
proc makefacn(vars,nums);
  local x,k,i,c;
  c=cols(nums);
  k=rows(nums);
  
  if c==1;
    x=makefac(vars,k);
    i=1;
    do while i<=vars;
      x[.,i]=nums[x[.,i]];
      i=i+1;
    endo;
  
  elseif c==vars;
    x=makefac(vars,k);
    i=1;
    do while i<=vars;
      x[.,i]=nums[x[.,i],i];
      i=i+1;
    endo;
  
  else;
    "makefacn: input error, cols(nums) must = 1 or vars";
    stop;
  endif;

  retp(x);
endp;
