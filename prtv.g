/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  nicely prints a set of variables (vectors) with names at top 
**
**  call prt(vrs);
**
**  vrs = a vector of symbols, each of which is a var name of a  nx1 vector
**        in memory.
**
**  prtv will print out the vectors concatinated horizontally with labels
**
**  _prtlong = 0; print vars in columns
**             1; print vars in rows, one and a time
**             2; print only row and col number
*/
declare matrix _prtlong ?= 0;
proc 0=prtv(vrs);
    local res,i,t;
    if _prtlong==0;
        i=1;
	do while i<=rows(vrs);
            if i==1; 
	      res=varget(vrs[1]);
	    else;    
	      res=res~varget(vrs[i]);
            endif;
        i=i+1;endo;
        $lower(vrs)';;
	if rows(res)==1;
	  ?;
	endif;
        res;
    elseif _prtlong==1;
        i=1;do while i<=rows(vrs);
            t=varget(vrs[i]);if typecv(vrs[i])==6;t=t';endif;
            $vrs[i]$+"= ";;t;
        i=i+1;endo;
    elseif _prtlong==2;
        i=1;do while i<=rows(vrs);
            t=varget(vrs[i]);
            if typecv(vrs[i])==6;$vrs[i];;" rows=";;rows(t);;" cols=";;cols(t);
            else;   $vrs[i]$+"= a string variable ";    endif;
        i=i+1;endo;
    else; errorlog "problem with _prtlong";endif;
    endp;

