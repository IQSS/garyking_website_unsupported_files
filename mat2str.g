/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  character vector to string
**
**  y = mat2str(x);
**
**  x = character vector
**
**  y = string with elements delimited by blanks or commas
*/
proc mat2str(x);
    local y,i;
    y="";
    i=1;do while i<=rows(x);
        y=y$+" "$+x[i];
    i=i+1;endo;
    y=strsect(y,2,strlen(y));
    retp(y);endp;
