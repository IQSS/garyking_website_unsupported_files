/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  Graphics procedures for use with Unix
*/
/* graphon;
**
** graphics setup: open window & switch to it
**
** OUTPUT GLOBAL: _graphon_pqg = graphics window number
*                 (used automatically for other procedures)
*/
proc 0=graphon;
  local wattr;
  #ifunix;
  wincloseall;
  clearg _graphon_pqg;
  let wattr=0 0 640 480 0 0 1 6 15 0 1 0 2;	  
  _graphon_pqg = winopenpqg(wattr,"Graphics","Graphics");
  call winsetactive(_graphon_pqg);
  #endif;
  graphset;
endp;


/* graphclr;
**
** switch to pqg window and clear
*/
proc 0=graphclr;
  graphset;
  #ifunix;
  winclear(_graphon_pqg);
  call winsetactive(_graphon_pqg);
  #endif;
endp;


/* graphno;
**
** set text window active
**/
proc 0=graphno;
  #ifunix;
  call winsetactive(1);
  #endif;
endp;


/* graphyes;
**
** set text window active
**/
proc 0=graphyes;
  #ifunix;
  call winsetactive(_graphon_pqg);
  #endif;
endp;


/* graphwait;
**
** switch to text; wait for key; switch back to pqg and clear;
** (use for batch files of multiple graphs)
*/
proc 0=graphwait;
  #ifunix;
  call winsetactive(1);
  #endif;
  wait;
  graphset;
  #ifunix;
  winclear(_graphon_pqg);
  call winsetactive(_graphon_pqg);
  #endif;
endp;


/* graphoff;
**
** graphics off: switch to text, wait for key, close window
** (use for last graphics command)
*/
proc 0=graphoff;
  #ifunix;
  call winsetactive(1);
  #endif;
  wait;
  #ifunix;
  wincloseall;
  #endif;
endp;
