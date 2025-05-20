/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
** trim a set of vectors the same way
**
** trimall(vars,ttop,tbot);
**
** INPUTS
** vars = vector of symbols representing existing vector names
** ttop = scalar number of rows to trim from top
** tbot = scalar number of rows to trim from bottom
** 
** OUTPUT  each existing vector, whose symbol is named in vars,
**         will be trimr'd ttop rows from top and tbot rows from bot.
*/
proc (0)=trimall(vars,ttop,tbot);
    local t,tt,ttt,i,tst;
    if cols(vars)/=1;errorlog "VARS must be Nx1";end;endif;
    i=1;do while i<=rows(vars);
        t=trimr(varget(vars[i]),ttop,tbot);
        tst=varput(t,vars[i]);
        if tst/=1;errorlog "problem VARPUTting";end;endif;
    i=i+1;endo;
    endp;



