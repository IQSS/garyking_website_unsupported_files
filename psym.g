/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  easily add extra plot symbols
**
**  call psym(x,y);
**
**  INPUTS:  x,y are x-y coordinates, each is an nx1 vector
**
**  GLOBALS: each is a scalar or the same dimensions of x and y
**
**  _psym_t = 8; sets the types of symbols
**            1 circle            8  solid circle
**            2 square            9  solid square
**            3 Triange           10 Solid triange
**            4 plus              11 solid plus
**            5 diamond           12 solid diamond
**            6 inverted triangle 13 solid inverted triangle
**            7 star              14 solid star
**
** _psym_c = 9; sets colors of symbols
**            0 Black       8 Dark grey
**            1 blue        9 Light blue
**            2 green      10 light green
**            3 cyan       11 light cyan
**            4 red        12 light red
**            5 magenta    13 light magenta
**            6 brown      14 yellow
**            7 grey       15 white
**
** _psym_h = 1;  sets hight of symbols (0-5 or so)
*/
declare matrix _psym_t != 8;
declare matrix _psym_c != 10;
declare matrix _psym_h != 1;
proc 0=psym(x,y);
  local psym,o,r,a;
  r=rows(x);

  if r/=rows(y);
    "psym: input dimensions are wrong";
    stop;
  endif;
  
  a=rows(_psym_t);
  if a/=1 and a/=r;
    #ifunix;
    wincloseall;
    #endif;
    "psym: problem with _psym_t";
    stop;
  endif;
  a=rows(_psym_c);
  if a/=1 and a/=r;
    #ifunix;
    wincloseall;
    #endif;
    "psym: problem with _psym_c";
    stop;
  endif;
  a=rows(_psym_h);
  if a/=1 and a/=r;
    #ifunix;
    wincloseall;
    #endif;
    "psym: problem with _psym_h";
    stop;
  endif;
  
  o=ones(rows(x),1);
  
  psym=x~y~(_psym_t.*o)~(_psym_h.*o)~(_psym_c.*o)~o*1~o*0;
  
  if scalzero(_psym);
    _psym=psym;
  else;
    _psym=_psym|psym;
  endif;
  
endp;
