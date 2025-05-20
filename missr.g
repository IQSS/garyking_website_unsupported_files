/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  finds missing values in set of variables
**
**  y = missr(x);
**
**  x = string of variable names (e.g.: x = "dog cat sex race income");
**
**  y = column vector of 1 if a missing value appears in any variable in
**      that row
**
**  vars in x are only used if they are /= 0
*/
proc missr(x);
  local y,i,maxrows,t;
  x=str2mat(x);
  
  /* identify maximum number of rows among all input vectors */
  maxrows=0;
  i=1;
  do while i<=rows(x);
    t=varget(x[i]);
    y=rows(t);
    if y>maxrows; 
      maxrows=y; 
    endif;
    i=i+1;
  endo;

  /* create output vector */
  y=0;
  i=1;
  do while i<=rows(x);
    t=varget(x[i]);
    if rows(t)==maxrows;
      if y==0;    
	y=t;
      else;       
	y=y~t;  
      endif;
    endif;
    i=i+1;
  endo;
  y=ismissm(y);
  y=sumc(y').>=1;
  retp(y);
endp;
  
