/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**
**  m = dec2bin(d,v)
**  uncompresses a small decimal-valued matrix (compressed with bin2dec)
**  into part or all of the corresponding 0/1 matrix
**
** INPUT:  d = nxq matrix of compressed decimal values created by bin2dec()
**         v = list of columns to unpack (or 0 for all columns)
**
** OUTPUT: m = nxp matrix of 0s and 1s [ceil(p/16)=q]
**
** Simulated Example: 
** m=rndu(2000,45).<0.4;   @ create large simulated 0/1 matrix @
** dec=bin2dec(m);         @ store in smaller decimal value matrix @
** let v=1 5 43;           @ variables to pull out @
** m2=dec2bin(dec,v);      @ equivalent to m[.,v] @
*/
proc dec2bin(d,v);
  local m,d2,id2,i,ii,col;
  m={};
  v=sortc(v,1);
  col=1;
  for ii(1,cols(d),1);i=ii;
    do while maxc(d[.,i])>0;
      d2=d[.,i]/2;
      id2=int(d2);
      if in(col,v,1) or scalzero(v);
	m=m~((id2-d2).==-0.5);
      endif;
      col=col+1;
      d[.,i]=id2;
    endo;
  endfor;
  retp(m);
endp;
