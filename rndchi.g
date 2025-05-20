/*
** sims = rndchi(r,c,v);
**
** inputs: r = row
**         c = column
**         v = df
**
** output: sim = rxc matrix of independent chi-square sims with v df
**
** 4/13/99 KS
*/
proc rndchi(r,c,v);
    retp(2*rndgam(r,c,v/2));
endp;
