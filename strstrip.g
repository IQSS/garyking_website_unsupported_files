/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  strip and format a string
**
**  s = strstrip(y);
**
**  y = a string
**  s = the same string with blanks removed from the ends
**      and <CR>'s inserted when lines are >70 columns
*/
proc strstrip(y);
    local t,cr,i;
cr="
   ";
    y=lower(y);
    do while strsect(y,1,1)$==" ";
        y=strsect(y,2,strlen(y));
    endo;
    do while strsect(y,strlen(y),1)$==" ";
        y=strsect(y,1,strlen(y)-1);
    endo;
    i=60;
    do while i<=strlen(y)-5;
        t=strindx(y," ",i);
        if t/=0;
            y=strsect(y,1,t-1)$+cr$+strsect(y,t+1,strlen(y));
        else;
            t=i;
        endif;
        i=t+60;
    endo;
    retp(y);endp;

