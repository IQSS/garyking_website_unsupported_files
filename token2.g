/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  more flexible string parser, faster for larger springs
**
** Format:     { token,str_left } = TOKEN2(str);
**
** Input:      str         string, the string to parse.
**
** Output:     token       string, the first token in <str>.
**
**             str_left    string, the remainder of the input string.
**
** GLOBALS:    _tokdel = ascii values of acceptable delimiters
**             _tokwds = ascii values of individual characters taken to be
**                       tokens, regardless of delimiter placement
*/
declare matrix _tokdel={32,10,13,44,9}; @ space, lf, cr, comma, tab @
declare matrix _tokwds={59,60,61,62};   @ ; = < > @
proc (2) = token2(str);
  local st,en,t,skwds,tok,str_left,slen;
  if _tokwds/=-1;
    skwds=_tokdel|_tokwds;
  else;
    skwds=_tokdel;
  endif;
  slen=strlen(str);
  if str$=="";
    retp("","");
  endif;
  st=1;
  do while in(vals(strsect(str,st,1)),_tokdel,1);
    st=st+1;
    if st>slen;
      retp("","");
    endif;
  endo;    
  t=str$+" ";
  en=st;
  do until in(vals(strsect(t,en,1)),skwds,1);
    en=en+1;
  endo;   
  if in(vals(strsect(t,en,1)),_tokwds,1) and st==en; 
    en=en+1; 
  endif;
  tok=strsect(str,st,en-st);
  str_left=strsect(str,en,slen);
  retp(tok,str_left);
endp;

