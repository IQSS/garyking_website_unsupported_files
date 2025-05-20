/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  regression with approximate randomization.
**
**  {bs,b,se}=regrand(x,y,sims)
**
**      x = n x k matrix of explanatory variables
**      y = n x 1 dependent variable vector
**      sims = number of simulations
**
**      bs = sims x rows(x)+1 matrix of simulated b's. rows are simulations;
**           columns correspond to different regression coefficients.
**      b = regression coefficients with unsorted data
**      se = standard errors from unsorted data
**
*/
external proc reg,sortind;
external matrix _rweight _routput;
proc 3=regrand(x,y,sims);
    local t,inx,b,se,n,vc,yhat,e,wt,bs,i,yy,bt;
    n=rows(y);
    _routput=0;
    {b,se,vc,yhat,e}=reg(x,y);
    bs=zeros(1,rows(b));
    "Approximate Randomization Simulation:";format /rd 4,0;
    i=1;do while i<=sims;
        inx=sortind(rndu(n,1));
        yy=y[inx];
        if rows(_rweight)>1; wt=_rweight[inx]; endif;
        {bt,t,t,t,t}=reg(x,yy);
        bs=bs|(bt');
        if 10*int(i/10)==i;i;;endif;
    i=i+1;endo;
    bs=trimr(bs,1,0);   format /rd 6,4;
    _routput=1;
    retp(bs,b,se);
    endp;
