/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  convert string to a character vector matrix
**
**  y = str2mat(x);
**
**  x = string with elements delimited by blanks or commas or tabs etc
**
**  y = character vector
*/
proc str2mat(x);
  local y,tok;
  y=0; 
  tok="aaa";
  do while tok$/="";
    {tok,x}=token2(x);
    if tok$/="";    
      y=y|tok;    
    endif;
  endo;
  y=trimr(y,1,0);
  retp(y);
endp;
