/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  Does file exist on disk?
**
**  yes = exist("input.asc");  
**
**  Input:  filename = a string
**  output: yes = 1 if file exists, 0 otherwise
*/
proc exist(filename);
    retp(not files(filename,0) $== 0);
endp;
