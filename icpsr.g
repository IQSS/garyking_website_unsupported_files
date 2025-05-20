/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  change state code from postal to ICPSR abreviation
**
**  st2 = icpsr(st);
**
**  GLOBAL:
**  if _icpsrA = 1
**    st  = 2-letter state postal id, a character vector
**    st2 = numerical icpsr state code
**
**  elseif _icpsrA = 0
**    st  = numerical code representing alphabetical order, a vector
**    st2 = numerical icpsr state code
*/
declare matrix _icpsrA ?= 1;
proc icpsr(s);
  local t,al,poid,icp;
  gosub getdata;
  if _icpsrA;  
    s=upper(s);
    t=sumc(icp.*(poid.$==s'));
    t=mkmissm(t,0);
  elseif _icpsrA==0;
    t=sumc(icp.*(al.==s'));
    t=mkmissm(t,0);
  endif;
  if ismiss(t);
    "input contains invalid entries; missing values output";
  endif;
  retp(t);

  getdata:
  @ poid = State Postal ID,
    al   = Usual alphabetical numbering,
    icp  = ICPSR code @
  let t[51,3]=
  AL   1 41
  AK   2 81
  AZ   3 61
  AR   4 42
  CA   5 71
  CO   6 62
  CT   7  1
  DE   8 11
  DC  51 55
  FL   9 43
  GA  10 44
  HI  11 82
  ID  12 63
  IL  13 21
  IN  14 22
  IA  15 31
  KS  16 32
  KY  17 51
  LA  18 45
  ME  19 02
  MD  20 52
  MA  21 03
  MI  22 23
  MN  23 33
  MS  24 46
  MO  25 34
  MT  26 64
  NE  27 35
  NV  28 65
  NH  29 04
  NJ  30 12
  NM  31 66
  NY  32 13
  NC  33 47
  ND  34 36
  OH  35 24
  OK  36 53
  OR  37 72
  PA  38 14
  RI  39 05
  SC  40 48
  SD  41 37
  TN  42 54
  TX  43 49
  UT  44 67
  VT  45 06
  VA  46 40
  WA  47 73
  WV  48 56
  WI  49 25
  WY  50 68;
  poid=t[.,1];
  al=t[.,2];
  icp=t[.,3];
  return;
endp;
