/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**      loadvars file var1 var2 var3 ...;   
** OR   loadvars file;
**
** Load ascii data file into vectors (variables) with names designated on
** command line or on the first line in the ascii file.
**
**
** INPUTS:
**
**    file = string with file name of ascii data, tab, column, or blank-
**           delimited (delimitors can be changed via globals in token2).
**
**    var1 var2 var2 ... = names of variables to label columns of data in file
**
**             This variable list may be omitted from the command line if it
**             is included as the first line of the ASCII file.  Do NOT include
**             the names in both places or the first observation of each loaded
**             variable will be incorrect.
** GLOBALS:
** see token2()
**
** NOTES:
**
** The number of vars named must equal the number of variables in the file.
** If used in a batch file, be sure to #include loadvars.g; at the the 
** start of the program and to clear the variable names before calling
** loadvars().
*/
keyword loadvars(s);
  local file,vars,tok,dta,i,str;
  {file,s}=token2(s);
  if not(exist(file));
    "loadvrs error: file does not exist";
  endif;
  
  /* Get var names from file */
  if s $== "";				
    str=getf(file,0);
    i=strindx(str,chrs(10),1);
    if i==1;
      "loadvars: need var names on command line or on 1st line of datafile";
      stop;
    endif;
    vars=str2mat(strsect(str,1,i));
    dta=trimr(loada(file,rows(vars)),1,0);
    
  /* Get var names from command line */
  else;
    {vars,s}=token2(s);
    do until s $== "";
      {tok,s}=token2(s);
      vars=vars|tok;
    endo;
    dta=loada(file,rows(vars));
  
  endif;
  call makevars(dta,0,vars);
endp;