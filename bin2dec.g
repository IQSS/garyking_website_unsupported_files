/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**
**  d = bin2dec(m)
**  compresses a large 0/1 matrix into a small decimal-valued matrix
**
** INPUT: m = nxp matrix of 0s and 1s
**
** OUTPUT: d = [n x ceil(p/53)] matrix of compressed decimal values
**
** EXAMPLE (simulated)
** m=rndu(20,5).<0.4;    @ create large simulated 0/1 matrix @
** dec=bin2dec(m);       @ store in decimal value matrix of smaller dimensions @
** m2=dec2bin(dec);      @ expand back to larger dimension 0/1 matrix @
**                       @ at this point, m==m2 @
**
** EXPLANATION OF INTEGER PRECISION IN GAUSS (from Sam Jones):
** The IEEE double precision format supports 53 bits of precision for
** integers. It actually uses 52 bits for the significand.
** The significand is normalized to look like this (binary digits):
** 
**  1.000000000000000000000000000000000000000000
**
** In other words, one implied binary integer bit (always 1 except in the
** value zero) and 52 binary fraction bits. This gives you 53 bits of
** precision using only 52 actual bits.
**
** The exponent in an IEEE double precision number is 11 bits.
** The sign bit is 1 bit.  Thus, 52+11+1 = 64 bits total.
*/
proc bin2dec(m);
  local s,d,c,pr;
  d={};
  do while not(ismiss(m));
    c=minc(cols(m)|53);   @ 53 is integer precision in Gauss @
    s=seqa(0,1,c);
    d=d~sumc(((2^s').*m[.,1:c])');
    if c<cols(m);
      m=m[.,c+1:cols(m)];
    else;
      m={.};
    endif;
  endo;
  retp(d);
endp;
