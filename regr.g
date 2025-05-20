/*
**  regr.g  adapted from reg.g by Curt Signorino, 12/3/94
**
**  usage:  {b,se,vc,yhat,e,sig} = regr(x,y);
**
**  input:  x = NxK explanatory variable matrix (no constant term by default)
**          y = Nx1 dependent variable
**
**  output: b = regression coefficients
**          se = sqrt(diag(vc)), standard errors
**          vc = variance-covariance matrix (see _Rrobust, below)
**          yhat = X'b = predicted values
**          e = y-yhat = residuals
**          sig = sqrt(e'e/(n-k))
**
**  globals:
**          _Rweight = sd(y), the weight vector for Weighted Least Squares
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
**          _RrestR =  restriction design matrix, including restriction
**                     (if any) on constant term
**          _Rrestq =  restriction parameters column vector
**
**          For restricted least squares, _RrestR is a design matrix of
**          linear relationships and _Rrestq is the parameter vector.
**          Let R=_RrestR and q=_Rrestq.  Then the linear restrictions are
**          described by R*beta=q.  The restricted least squares estimates
**          and their variance are calculated as
**
**        bhat = b - (X'X)^-1 R'[R(X'X)^-1 R']^-1 (R b - q)
**   Var(bhat) = sig^2(X'X)^-1 - sig^2(X'X)^-1 R'[R(X'X)^-1 R']^-1 R(X'X)^-1
**
**          where b are the unrestricted least squares estimates.  For
**          more information, see Greene (1993, 204) or Judge (1982, 552).
**
**    Ex 1: Calculate least squares estimates with the following
**          restrictions:  beta1=0.34, beta1 to beta4 sum to 1, and
**          beta2=beta3.
**
**          _RrestR = { 0  1  0  0  0,
**                      0  1  1  1  1,
**                      0  0  1 -1  0 };
**          _Rrestq = { 0.34, 1, 0 };
**          call regr(x1~x2~x3~x4,y);
**
**    Ex 2: Calculate unrestricted least squares estimates, assigning
**          variable names.
**
**          let _RYname = income;
**          let _RXnames = educat race;
**          call regr(educat~race,income);
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
declare matrix _RrestR  ?= {.};
declare matrix _Rrestq  ?= {.};
external proc selif;

proc (6) = regr(x,y);
    local t,e,sig2,yhat,vc,se,b,ixx,mask,fmt,vbls,r2,yvar,xy,bin,wt,xx,yy,
          i,twt,sel,m,my,j,damp,rxnames,ryname,tstat,vcr,vcc,R,q,iRixxR;
    m={.};
    if sumc(sumc(_RrestR.==0))==
        sumc(sumc(ones(rows(_RrestR),cols(_RrestR))));
        _RrestR=m;                     @ design matrix all zeros @
    endif;
    if scalmiss(_RrestR);              @ if unrestricted,              @
        R=zeros(1,cols(x)+1);          @ zero-out R and q              @
        q=0;
    else;                              @ else if restricted,           @
        R=_RrestR;                     @ R = restriction design matrix @
        q=_Rrestq;                     @ q = restriction param vector  @
    endif;
    if rows(R)/=rows(q);
        errorlog "regr: Must have same number of rows in _RrestR and _Rrestq.";
        retp(m*ones(cols(x),1),m,m,m,m,m);
    endif;
    if cols(R)/=cols(x)+1;
        errorlog "regr: Must have same number of columns in _RrestR and X.";
        retp(m*ones(cols(x),1),m,m,m,m,m);
    endif;
    if type(_rXnames)==13;
        rXnames=stof(_rXnames);
    else;
        rXnames=_rXnames;
    endif;
    rXnames=reshape(rXnames,rows(rXnames)*cols(rXnames),1);
    if type(_rYname)==13;
        rYname =stof(_rYname);
    else;
        rYname= _rYname;
    endif;

    if _Rconst==1;
        x=ones(rows(x),1)~x;
    endif;
    wt=_Rweight;
    sel=_rselect;
    if rows(sel)/=1;
        if ((rows(y)/=rows(x)).and(rows(sel)/=rows(Y)));
            Errorlog "regr: Rows of X, Y, and _Rselect must be identical.";
            end;
        endif;
        if rows(wt)>1;
            if (rows(y)/=rows(wt));
                "rows(_Rweight)/=rows(y or x)";
                end;
            endif;
        endif;
        t=((sumc(ismissm(x)').>=1).or ismissm(y).or (1-sel));
        if rows(wt)>1;
            t=t.or ismissm(wt);
        endif;
        x=delif(x,t);
        y=delif(y,t);
        if rows(wt)>1;
            wt=delif(wt,t);
        endif;
        if rows(sel)>1;
            sel=delif(sel,t);
        endif;
    else;
        t=((sumc(ismissm(x)').>=1).or ismissm(y));
        if rows(wt)>1;
            t=t.or ismissm(wt);
        endif;
        x=delif(x,t);
        y=delif(y,t);
        if rows(wt)>1;
            wt=delif(wt,t);
        endif;
        if rows(sel)>1;
            sel=delif(sel,t);
        endif;
    endif;
    if rows(wt)>1;
        if sumc(wt.<=0)>0;
            errorlog "regr: Can't run WLS with zero or negative weights.";
            errorlog "      Returning missing values.";
            retp(m*ones(cols(x),1),m,m,m,m,m); ndpclex;
        endif;
    endif;
    xx=x;                           @ unwtd x @
    x=x./wt;                        @ wtd x   @
    yy=y;                           @ unwtd y @
    y=y./wt;                        @ wtd y   @
    trap 1;
    ixx=invpd(moment(x,0));         @ (X'X)^-1 matrix @
    if scalerr(ixx);
        errorlog "regr: X'X not invertable, returning missing values.";
        retp(m*ones(cols(x),1),m,m,m,m,m);
        ndpclex;
    endif;
    trap 0;
    b=ixx*x'y;                      @ unrestricted estimates @
    if scalmiss(_RrestR);           @ if no restrictions,    @
        iRixxR=0;                   @ set equal to zero      @
    else;                           @ otw, calc inverse.     @
        trap 1;
        iRixxR=invpd(R*ixx*R');
        if scalerr(iRixxR);
            errorlog "regr: R (X'X)^-1 R not invertable, returning "\
                     "missing values.";
            retp(m*ones(cols(x),1),m,m,m,m,m);
            ndpclex;
        endif;
        trap 0;
    endif;
    b=b-ixx*R'*iRixxR*(R*b-q);           @ restricted estimates   @
    yhat=x*b;                            @ predicted values       @
    e=y-yhat;                            @ residuals              @
    sig2=e'e/(rows(x)-cols(x));          @ estimated sigma^2      @
    vc=sig2*ixx-sig2*ixx*R'*iRixxR*R*ixx;@ Var(b)                 @
    my=meanc(y);
    r2=(yhat-my)'(yhat-my)/(y-my)'(y-my);
    if _Rweight/=1;                      @ if wtd, correct for wting @
        yhat=xx*b;                       @ correct predicted values  @
        e=yy-yhat;                       @ correct residuals         @
    endif;

    if _Rrobust>=0;                      @ if White's or MA @
        if int(_Rrobust)/=_Rrobust;
            errorlog "_Rrobust must be an integer ";
        endif;
        if _Routput/=0;
            "Robust se's (_Rrobust=";;_Rrobust;;"; _Rtheta=";;_Rtheta;;"), ";
        endif;
        bin=0;
        j=-_Rrobust;
        do while j<=_Rrobust;
            i=1+_Rrobust;
            do while i<rows(y)-_Rrobust;
                damp=((_Rrobust+1-abs(j))/(_Rrobust+1))^_Rtheta;
                bin=bin+e[i]*x[i,.]'x[i-j,.]*e[i-j]*damp;
                i=i+1;
            endo;
            j=j+1;
        endo;
        vc=ixx*bin*ixx;                     @ robust variance matrix @
        vc=vc-vc*R'*iRixxR*R*ixx;           @ restricted and robust  @
        trap 1;
        if scalerr(chol(vc));
            errorlog "regr: VC isn't Non-Neg.Definite: increase _Rtheta.";
            retp(b,m,m,m,m,m);
            ndpclex;
        endif;
        trap 0;
    endif;

    vc= (vc.>10^(-15)).*vc + (vc.<-10^(-15)).*vc;
    se=sqrt(diag(vc));                      @ std errors of estimates @
    tstat=b./miss(se,0);                    @ t-statistics @
    if _Routput==1;                         @ print output @
        if rxnames==0;                      @ no x variable names @
            if not(scalmiss(_RrestR));
                "Restricted ";;
            endif;
            if wt==1;
                "LS: b=";;b';
            else;
                "WLS: b=";;b';
            endif;
            "    se=";;se';
            "     t=";;tstat';
            format /ld 6,0;
            "n=";;rows(y);;
            format /rd 6,_rfmt;
            "sig=";;sqrt(sig2);;"
            R^2=";;r2;
        else;                               @ x variable names given @
            if rows(rxnames)+_Rconst/=cols(x);
                errorlog "regr: _rxnames incorrect.";
                stop;
            endif;
            if not(scalmiss(_RrestR));
                "Restricted ";;
            endif;
            if wt/=1;
                "WLS: ";;
            else;
                "LS: ";;
            endif;
            if ryname==0;
                "";
            else;
                "Dependent Variable: ";;$lower(ryname);
            endif;
            "        vars           b          se           t";
            if _Rconst==1;
                vbls=lower("const"|rxnames);
            else;
                vbls=lower(rxnames);
            endif;
            mask=0~1~1~1;
            fmt=("*.*s"~  12~ 8)|
                ("*.*lf"~ 12~ _rfmt)|
                ("*.*lf"~ 12~ _rfmt)|
                ("*.*lf"~ 12~ _rfmt);
            call printfm(vbls~b~se~tstat,mask,fmt);
            format /ld 6,0;
            "n=";;rows(y);;
            format /rd 6,_rfmt;
            "sig=";;sqrt(sig2);;
            " R^2=";;r2;
        endif;
    endif;
    ndpclex;
    retp(b,se,vc,yhat,e,sqrt(sig2));

endp;
