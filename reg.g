/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  LS regression
**
**  usage:  {b,se,vc,yhat,e,sig} = reg(x,y);
**
**  input:  x = NxK explanatory variable matrix (no constant term by default)
**          y = Nx1 dependent variable
**
**  output: b = regression coefficients
**          se = sqrt(diag(vc)), standard errors
**          vc = variance-covariance matrix (see _Rrobust, below)
**          yhat = X'b, predicted values
**          e = y-yhat = residuals
**          sig = sqrt(e'e/(n-k))
**
** globals:
**          _Rweight = SD(y), for the weight vector for Weighted Least Squares
**          _RXnames = vector of x variable names
**          _RYname  = vector of y variable names
**          _Rfmt    = digits to the right of decimal point for output
**          _Rselect = vector of 1's to select and 0's to delete
**          _Rrobust = defines VC.  if -1 (default) use usual method;
**                     if 0 use White's heteroskedasticity-consistent VC matrx;
**                     if an integer >0, use MA(N)-time-series and
**                        heteroskedasticity-consistent VC matrix
**          _Rtheta = if _Rrobust>0 doesn't produce a non-neg.definate VC matrix
**                    then set this to some value >0 and usually <1.  Possibly
**                    also increase _Rrobust.  (Default _Rtheta=0)
**          _Routput = 0 if none; 1 (default) if print output
**          _Rconst =  1 if include constant term (default), 0 don't include
**
** example:     let _RYname = income;
**              let _RXnames = educat race;
**              call reg(educat~race,income);
**
*/
declare matrix _Rweight ?= 1;
declare matrix _Rxnames ?= 0;
declare matrix _Ryname  ?= 0;
declare matrix _Rfmt    ?= 4;
declare matrix _Rselect ?= 1;
declare matrix _Rrobust ?= -1;
declare matrix _Rtheta  ?= 0;
declare matrix _Routput ?= 1;
declare matrix _Rconst  ?= 1;
external proc selif;
proc 6=reg(x,y);
    local t,e,sig2,yhat,vc,se,b,ixx,mask,fmt,vbls,r2,yvar,xy,bin,wt,xx,yy,i,twt,
          sel,m,my,j,damp,rxnames,ryname;  m={.};
    if type(_rXnames)==13;rXnames=stof(_rXnames); else; rXnames=_rXnames; endif;
    rXnames=reshape(rXnames,rows(rXnames)*cols(rXnames),1);
    if type(_rYname)==13; rYname =stof(_rYname);  else; rYname= _rYname;  endif;

    if _Rconst==1;   x=ones(rows(x),1)~x;    endif;
    wt=_Rweight;  sel=_rselect;
    if rows(sel)/=1;
        if ((rows(y)/=rows(x)).and(rows(sel)/=rows(Y)));
            "Rows of x, y, and _Rselect must be identical";end;endif;
        if rows(wt)>1;
            if (rows(y)/=rows(wt));"rows(_Rweight)/=rows(y or x)";end;endif;
        endif;
        t=((sumc(ismissm(x)').>=1).or ismissm(y).or (1-sel));
        if rows(wt)>1;t=t.or ismissm(wt);endif;
        x=delif(x,t);y=delif(y,t);
        if rows(wt)>1;wt=delif(wt,t);endif;
        if rows(sel)>1;sel=delif(sel,t);endif;
    else;
        t=((sumc(ismissm(x)').>=1).or ismissm(y));
        if rows(wt)>1;t=t.or ismissm(wt);endif;
        x=delif(x,t);y=delif(y,t);if rows(wt)>1;wt=delif(wt,t);endif;
        if rows(sel)>1;sel=delif(sel,t);endif;
    endif;
    if rows(wt)>1;
        if sumc(wt.<=0)>0;
            errorlog "can't run WLS with zero or negative weights.";
            errorlog "returning missing values.";
            retp(m*ones(cols(x),1),m,m,m,m,m); ndpclex;
        endif;
    endif;

    xx=x;       yy=y;     @ y & x unweighted @
    x=x./wt;    y=y./wt;  @   and weighted @
    trap 1;
        ixx=invpd(moment(x,0));
        if scalerr(ixx);
            errorlog "X'X matrix not invertable, returning missing values";
            retp(m*ones(cols(x),1),m,m,m,m,m); ndpclex;
        endif; trap 0;
    b=ixx*x'y;
    yhat=x*b;
    e=y-yhat;
    sig2=e'e/(rows(x)-cols(x));
    vc=sig2*ixx;
    my=meanc(y);
    r2=(yhat-my)'(yhat-my)/(y-my)'(y-my);
    if _Rweight/=1;
        yhat=xx*b;
        e=yy-yhat;
    endif;
    if _Rrobust>=0;
        if int(_Rrobust)/=_Rrobust;
            errorlog "_Rrobust must be an integer ";endif;
        if _Routput/=0;
            "Robust se's (_Rrobust=";;_Rrobust;;"; _Rtheta=";;_Rtheta;;"), ";
        endif;
        bin=0;
        j=-_Rrobust;do while j<=_Rrobust;
            i=1+_Rrobust;do while i<rows(y)-_Rrobust;
                damp=((_Rrobust+1-abs(j))/(_Rrobust+1))^_Rtheta;
                bin=bin+e[i]*x[i,.]'x[i-j,.]*e[i-j]*damp;
            i=i+1;endo;
        j=j+1;endo;
        vc=ixx*bin*ixx;
        trap 1;
        if scalerr(chol(vc));
            errorlog "VC isn't Non-Neg.Definate: increase _Rtheta";
            retp(b,m,m,m,m,m); ndpclex;
        endif;   trap 0;
    endif;
    se=sqrt(diag(vc));
    if _Routput==1;
    if rxnames==0;
        if wt==1;"LS: b=";;b';
        else;"WLS: b=";;b';endif;
        "    se=";;se';
        "     t=";;(b./se)';format /ld 6,0;
        "n=";;rows(y);;format /rd 6,_rfmt;"sig=";;sqrt(sig2);;" R^2=";;r2;
    else;
        if rows(rxnames)+_Rconst/=cols(x);
            errorlog "ERROR: _rxnames incorrect.";stop;endif;
        if wt/=1;"WLS: ";;else;"LS: ";;endif;
        if ryname==0;"";
            else;"Dependent Variable: ";;$lower(ryname);endif;
        "     vars         b          se         t";
        if _Rconst==1;
            vbls=lower("const"|rxnames);
        else;
            vbls=lower(rxnames);
        endif;
        mask=0~1~1~1;
                 fmt=("*.*s"~  10~ 8)|
                     ("*.*lf"~ 10~ _rfmt)|
                     ("*.*lf"~ 10~ _rfmt)|
                     ("*.*lf"~ 10~ _rfmt);
        call printfm(vbls~b~se~(b./se),mask,fmt);format /ld 6,0;
        "n=";;rows(y);;format /rd 6,_rfmt;"sig=";;sqrt(sig2);;" R^2=";;r2;
    endif;endif;
    ndpclex;
    retp(b,se,vc,yhat,e,sqrt(sig2));endp;

