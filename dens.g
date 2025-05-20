/*  
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  Kernel density estimation
**
**   Format:    {px,py}=dens(y);
**
**   input:     y = Nx1 vector
**
**   output:    plot of y (horizontally) by its estd density f(y) (vertically).
**              Whiskers are also plotted along horizontal axis for each datum
**
**              px = x coordinates for the plot (_pts x 1 vector)
**              py = y coordinates for the plot (_pts x 1 vector)
**
**    Global values are as follows, and may be changed:
**   _smth=0;         smoothing parameter>0. the larger, the more smoothing.
**                    =0 for automatic calculation of density.
**                    =-1 for automatic calculation for description.
**
**   _strt=0;         x axis minimum. (if _strt=_endd, set at min and max).
**   _endd=0;         x axis maximum.
**   _pts=100;        number of points to plot
**   _whiskr=-1;      -1 = draw whiskers at 1/10th size;
**                    >0 = draw whiskers at size _whiskr;
**                     0 = do not draw whiskers
**                    Whiskers are always drawn at y==0, so you may have to
**                    use ytics to scale the y-axis.
**   _jitter=0;       0=nothing extra. #=add # jitter to each
**                      whisker if _whiskr/=0;.
**
**   let _kern=E;     kernel type. N=normal, E=Epanechnikov,
**                                 B=biweight, T=triangular, R=rectangular
**                                 TN=Doubly Truncated Normal
**  for option _kern=="TN",
**      _Tleft=_strt;   truncate distribution on left at _Tleft
**      _Tright=_endd;  truncate distribution on right at _Tright
**
**   _output = 1;     1 = print density plot (default); 0 = do not print.
**
**  Technical Reference:  
**    B.W. Silverman. 1986. _Density Estimation for Statistics
**    and Data Analysis_.  London: Chapman and Hall.
**
**  Application (code developed for):
**    King, Gary. "Constituency Service and Incumbency Advantage," British
**    Journal of Political Science, 21, 1 (January, 1991): 119-128, 
**    (replication dataset: ICPSR s1108)
*/
declare matrix _smth ?= 0;
declare matrix _strt ?= 0;
declare matrix _endd ?= 0;
declare matrix _pts  ?= 100;
declare matrix _Tleft ?= 0;
declare matrix _Tright ?= 0;
declare string _kern ?= "E";
declare matrix _whiskr ?= -1;
declare matrix _jitter ?= 0;
declare matrix _output ?= 1;
external proc code,xy,seqas;
external matrix _pline;
@ Kernels @
@ arguments of kernels: z=input value, m=mean, h=smoothing parameter _smth @
proc kerneln(z,m,h);local res;z=(z-m)./h;      @ NORMAL kernel @
    res=(1./sqrt(2*pi))*exp(-(1/2).*(z^2));retp(res);endp;
proc kernelTN(z,m,h,Tleft,Tright);
    local t,tl,res,tr,zz;zz=(z-m)./h;          @ TRUNCATED NORMAL kernel @
    tl=(Tleft-m)./h; tr=(Tright-m)./h;
    t=((z.>Tleft).and (z.<Tright));
    res=t.*pdfn(zz)./(1-cdfn(tl)-(1-cdfn(tr)));
    retp(res);endp;
proc kernele(z,m,h);local a,res,t;z=(z-m)./h;  @ EPANECHNIKOV kernel @
    t=(abs(z).<sqrt(5)); a=code(t,sqrt(5)|1);
    res=t.*((3/4)*(1-(1/5).*(z^2))./a);retp(res);endp;
proc kernelb(z,m,h);local a,res,t;z=(z-m)./h;  @ BIWEIGHT kernel @
    t=(abs(z).<1);
    res=t.*((15/16)*((1-(z^2))^2));retp(res);endp;
proc kernelt(z,m,h);local a,res,t;z=(z-m)./h;  @ TRIANGULAR kernel @
    t=(abs(z).<1);
    res=t.*(1-abs(z));retp(res);endp;
proc kernelr(z,m,h);local a,res,t;z=(z-m)./h;  @ RECTANGULAR kernel @
    t=(abs(z).<1);
    res=t.*0.5;retp(res);endp;

@ creates two nx1 vectors to plot @
proc (2)=density(y,strt,endd,pts,h,kerna,Tleft,Tright);
    local i,t,px,py;    local kern:proc;
    kerna=upper(kerna);
    px=seqas(strt,endd,pts);    
    py=px;
    format /rdn 4,0;
    "Kernel Density Estimation";
    "Calculating ";;pts;;" Points: ";;
    i=1;do while i<=pts;
        if     (kerna$=="N"); t=kernelN(py[i,1],y,h)./h;
        elseif (kerna$=="E"); t=kernelE(py[i,1],y,h)./h;
        elseif (kerna$=="B"); t=kernelB(py[i,1],y,h)./h;
        elseif (kerna$=="T"); t=kernelT(py[i,1],y,h)./h;
        elseif (kerna$=="R"); t=kernelR(py[i,1],y,h)./h;
        elseif (kerna$=="TN");t=kernelTN(py[i,1],y,h,Tleft,Tright)./h;
        else; errorlog "_kernel specified incorrectly";stop;endif;
        py[i,1]=sumc( t )/rows(y);
        if i==10*floor(i/10);i;;endif;
    i=i+1;endo;?;retp(px,py);endp;

@ Kernel Density Estimate; sets defaults, calls density, then xy @
proc 2=dens(y);local strt,endd,h,kern,pts,px,py,smth,std,Tleft,Tright,os;
    if cols(y)/=1; errorlog "Argument must be a column vector";end;endif;
    strt=_strt;endd=_endd;
    if     strt>endd;errorlog "error: _strt>_endd";end;
    elseif strt==endd;strt=minc(y);endd=maxc(y);endif;
    pts=int(_pts);
    if pts<=2;errorlog "_pts must be greater than 2.  Try _pts=100;";endif;
    smth=_smth;
    if (smth<0).and(smth/=-1);errorlog "_smth must be -1 or > than 0";stop;
    elseif smth==0; py=sortc(y,1);
           std=minc((py[int(3*rows(py)/4)]-py[int(rows(py)/4)])/1.34|stdc(py));
           smth=0.9*std*(rows(py)^(-0.2));
    elseif smth==-1; py=sortc(y,1);
           std=minc((py[int(3*rows(py)/4)]-py[int(rows(py)/4)])/1.34|stdc(py));
           smth=0.9*std*(rows(py)^(-0.2))/2; endif;
    kern=_kern;
    Tleft=0;Tright=0;
    if kern$=="TN";
        if _Tleft==_Tright;  Tleft=strt;   Tright=endd;
        else;                Tleft=_Tleft; Tright=_Tright; endif;
    endif;
    {px,py}=density(y,strt,endd,pts,smth,kern,Tleft,Tright);
    std=rndu(rows(y),1)*_jitter-(_jitter/2);
    y=y+std;
    if _output==1;
    if _whiskr==-1;
        os=ones(rows(y),1);
        _pline=os~ (os*6)~ y~ (os*0)~ y~ (os*(maxc(py)/15))~ os~ (os*15)~(os*0);
        clear os;
    elseif _whiskr>0;
        os=ones(rows(y),1);
        _pline=os~ (os*6)~ y~ (os*0)~ y~ (os*_whiskr)~ os~ (os*15)~(os*0);
        clear os;
    endif;
        xy(px,py);
        format/rd 5,4;
        if rows(_smth)==1;?;"Smoothing Parameter: _smth=";;smth;endif;
    endif;
    retp(px,py);endp;

