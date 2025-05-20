/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
** hypergeometric pmf
**
** {pr,m,sd} = hyperg(x,n,m,num);
**
** INPUTS:
** N = number of balls in the bin
** M = number of balls of which M are of type 1
**                          (N-M) are of type 2
** num = number chosen without replacement from bin
** X = number of items of type 1 drawn (x is one of 0,1,...,num)
**
** OUTPUTS:
** pr = probability of x occurring
** m = expected number of items of type 1 drawn, E(X)
** sd = standard deviation of X, sqrt(Var(X))
**
*/
proc 3=hyperg(x,n,m,num);
    local a,b,c,pr,sd,m;
    a = combin(m,x);
    b = combin(n-m,num-x);
    c = combin(n,num);
    pr = (a.*b)./c;
    m = (num.*m)./n;  @ mean @
    sd = sqrt(num.*(m./n).*(1-(m./n)).*(n-num)./(n-1));
    retp(pr,m,sd);endp;
