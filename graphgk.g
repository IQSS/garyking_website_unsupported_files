/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  reset gk's graphics globals
*/
proc 0=graphgk;
  
  /* for pline() */
  clearg _plinet,_plineth,_plinec,_plines,_plinesc,_plinesTh,_plineRev,
    _psym_t,_psym_c,_psym_h,
    _ternary_l,_ternary_w,_ternary_t,_ternary_cr,_ternary_cov,_ternary_df;
  _plinet=6;
  _plineth=0;
  _plinec=14;
  _plineRev=0;
  
  /* for plines() */
  _plines=0;
  _plinesc=12;
  
  /* for psym() */
  _psym_t=8;
  _psym_c=10;
  _psym_h=1;
  
  /* for ternary() */
  _ternary_L = "u\000v\000w";
  _ternary_w = 1;
  _ternary_t = 1;
  _ternary_cr = 0;
  _ternary_cov = 0;
  _ternary_df = 0;
  
endp;
