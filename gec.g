/*
**  (C) Copyright 2002 Gary King and Curt Signorino
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**
**  Generalized Event Count Regression Model
**
**  FORMAT:     { b,logl,g,vc,ret }=gec(dataset,dep,ind);
**
**  INPUT:
**      dataset = name of Gauss dataset or name of matrix in memory
**      dep     = dependent variable name or column number
**      ind     = vector of independent variable names or column numbers
**
**  OUTPUT:
**      b    = vector of effect parameters that maximize the likelihood
**      logl = value of the log-likelihood at the maximum
**      g    = vector of gradients at b
**      vc   = variance-covariance matrix of b
**      ret  = return code from maxlik routine
**
**  GLOBALS:
**      _cnstrn=0;  -- 0=do nothing extra.
**                     symbol=include this variable from a dataset,
**                            constraining its coefficient to 1.0.
**                     integer=include log of this column of input matrix,
**                            constraining its coefficient to 1.0.
**      _cnstart=0; -- choose method of calculating starting values.
**                     0=LS, 1=loadm b, 2=rndu-0.5, 3=zeros, or set to vector
**      _cndspar=3; -- starting value for scalar dispersion parameter.
**     _output="2"; -- 0=do not print, 1=print on screen only,
**                     2=print on screen and in output file dataset.OUT,
**                     or set it to the output file name.
**        _cnfmt=4; -- number of decimal points to print.
**
**  OTHER GLOBALS:
**      see maxlik.SRC and count.SRC, which gec calls.
**
**  EXAMPLE 1:
**      let dep=wars;
**      let ind=age party unem;
**      dataset="\\gauss\\prg\\sample";
**      call gec(dataset,dep,ind);
**
**  REFERENCES:  
** 
** Gary King. 1989. "Variance Specification in Event Count Models: From
** Restrictive Assumptions to a Generalized Estimator," AMERICAN JOURNAL OF
** POLITICAL SCIENCE, 33, 2 (June): 762-784.
** 
** Winkelmann, Rainer; Curtis Signorino; and Gary King. ``A Correction for an
** Underdispersed Event Count Probability Distribution,'' POLITICAL ANALYSIS,
** (1995): Pp. 215-228.
** 
** King, Gary and Curtis S. Signorino. ``The Generalization in the
** Generalized Event Count Model, With Comments on Achen, Amato, and
** Londregan,'' a response to three authors in a symposium on ``Gary King's
** Generalized Event Count Model'' Pp. 225-252 in POLITICAL ANALYSIS, Vol. 6
** (1996).  available at http://gking.harvard.edu/preprints.shtml
** 
*/
#include count.ext;
#include gauss.ext;
#include maxlik.ext;
declare matrix _cnstart ?=0;
declare matrix _cndspar ?=0;
declare matrix _cnstrn ?=0;

proc SVgec(dataset,dep,ind); local res,b0,b1,b,pars;
  _max_covpar=3; 
  if ind==0;
    pars=2;
  else;
    pars=rows(ind)+2;
  endif;
  if _cnstart==0;
    if ind==0;
      b0=0;
    else;
      b0=lols(dataset,dep,ind);
    endif;
    res=b0|_cndspar;
  elseif _cnstart==1;
    loadm b;
    res=b;
    if rows(b)/=pars;
      "b is the wrong size for _cnstart\g";
	end;
      endif;
    elseif _cnstart==2;
      res=rndu(pars,1)-0.5;
    elseif _cnstart==3;
      res=zeros(pars,1);
    else;
      res=_cnstart;
      if rows(res)/=pars;
	errorlog "rows(_cnstart) is wrong.\g";
	end;
      endif;
    endif;
    retp(res);
  endp;
  
proc rmod(m,n);
  retp(abs(m-n.*floor(m./n)));
endp;

proc limfloor(x);
  local tmp,z;
  tmp=rmod(x+1,round(x)+1);
  z=(tmp.==0).*(round(x-1))+(tmp./=0).*(floor(x));
  retp(z);
endp;

proc lngamma(x);
    if x>1;
        retp(lnfact(x-1));
    else;
        retp(ln(gamma(x)));
    endif;
endp;

proc dbin(u,s);
    local res,us,m,lgdif,mlimit;
    us=(-u/(s-1));
    mlimit={ 0, 0 };
    mlimit[1,1]=limfloor(us)+1;
    mlimit[2,1]=floor(ln(10e-308)/ln(1-s));
    m=seqa(0,1,minc(mlimit)+1);
    lgdif=lngamma(us+1)-lngamma(us+1-m);
    lgdif=(lgdif .>= 709.1962).*709.1962 + (lgdif .< 709.1962).*lgdif;
    res=(exp(lgdif)./m!).*((1-s).^m).*(s.^(us-m));
    retp(sumc(res));
endp;

proc LIgec(b,dta);
    local tst,lns,s1,i,d,j1,s,u,res,c,cc,nn,x,y,fixone;
    nn=rows(dta);
    s=exp(b[rows(b)]);
    if s<.001;
        retp((-9999-1000*s)*ones(nn,1));
    endif;
    y=dta[.,1];
    x=ones(nn,1);
    if cols(dta)>1;
        x=x~dta[.,2:cols(dta)];
    endif;
    if _cnstrn==0;
        fixone=0;
    else;
        fixone=ln(dta[.,_cnstrn]);
    endif;
    b=trimr(b,0,1);
    u=exp(x*b+fixone);
    lns=ln(s);
    s1=s-1;
    if s<1.0;
        tst=sumc(y.>(limfloor(-u./s1)+1));
        if tst > 0;
            retp((-9999-tst)*ones(nn,1));
        endif;
        d=zeros(nn,1);
        i=1;
        do while i<=nn;
            d[i,1]=ln(dbin(u[i,1],s));
            i=i+1;
        endo;
        c=-u.*(lns/s1)-d;
    elseif s>1.0;
        c=-u.*(lns/s1);
    else;
        c=-u;
    endif;
    cc=zeros(nn,1);
    i=1;
    u=(u .== 0).*10e-308 + (u .> 0).*u;
    do while i<=nn;
        if y[i,1]>0;
            j1=seqa(1,1,y[i,1])-1;
            cc[i,1]=sumc(ln(u[i,1]+s1*j1));
        endif;
        i=i+1;
    endo;
    res=c-(y.*lns)+cc;
    retp(res);
endp;

proc (5) = gec(dataset,dep,ind);
    local vars,b,logl,g,vc,se,parnames,st,ret;
    clearg __title;
    _max_covpar=3;
    if dep$==0;
        errorlog "DEP must = variable name or number";
        end;
    endif;
    if ((type(dataset)/=13) and (_cnstrn>cols(dataset)));
        errorlog "If dataset=matrix, _cnstrn must= 0 or a col of dataset\g";
        end;
    endif;
    if ((type(dataset)/=13) and ((maxc(ind)>cols(dataset))
      or (dep>cols(dataset))) );
      errorlog "If DATASET=matrix, DEP and IND must be column numbers"\
      "of the input matrix.\g";
      end;
    endif;
    vars=dep;
    if ind/=0;
      vars=vars|ind;
    endif;
    
    st=SVgec(dataset,dep,ind);
    __title="Generalized Event Count Regression Model";
    { b,logl,g,vc,ret }=maxlik(dataset,vars,&LIgec,st);
    if type(dataset)==13;
      vars="beta0";
      if ind/=0;
	vars=vars|ind;
      endif;
    else;
      vars="beta0";
      if ind/=0;
	for i(1,rows(ind),1);
	  vars=vars|("Col."$+ftos(ind[i],"%*.*lf",3,0));
	endfor;
      endif;
    endif;
    vars=vars|"gamma0";
    _cn_vr = vars;
    _cn_dp = dep;
    ndpclex;
    retp(b,logl,g,vc,ret);
endp;
