/*
**  (C) Copyright 1999 Gary King and Jonathan Katz
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  plots a ternary diagram 
**  (also called barycentric or triangular coordinate
**  systems diagrams or probability simplexes)
**
**  call ternary(u,v);
**
**  INPUTS:
**  u,v = (nx1) proportion of the vote for the first 2 parties in a 3-party
**        system; the third party's vote fractions are w=1-u-v.
**  OR  = (nx2) where the 2 columns represent two successive years and an
**        arrow is drawn between the two sets of coordinates.
**
**  GLOBAL:
**  _psym_c = (scalar or nx1) COLOR for arrows & dots; defaults in psym.g
**  _psym_h = (scalar or nx1) HEIGHT of dots; defaults in psym.g
**  _psym_t = (scalar or nx1) TYPE of dots; defaults in psym.g
**  _ternary_L = (string of three elements separated by \000) LABELS, 
**               top (party for v), bottom right (party for u),
**		 and bottom left (omitted party, w); 
**		 Formatted for single letters. default = "u\000v\000w";
**  _ternary_w = 1 draw in win lines; 0 don't draw
**  _ternary_t = 1 draw ends of the triangle; 0 draw entire triangle
**  _ternary_cr = {mu1,mu2,s1,s2,cov,pct} to draw pct% Confidence Region
**              given the mean1, mean2, variance1,  variance2, 
**              and covariance parameters of a normal on the
**              additive logistic scale.  If pct is a px1 vector, p concentric
**              confidence regions will be drawn; default = 0 (don't draw).
**  _ternary_df = 0 use normal distribution;  scalar>0 = degrees of freedom
**              for multivariate Student t distribution
**  _ternary_g = 1 do graph (default), 0 do not draw graph
**
**  OUTPUT GLOBAL:
**  _ternary_cov = (rows(pct)x2), confidence region COVerage
**              first column =pct from _ternary_cr, 
**              second column=fraction of points falling within region.
**
**  BUG:
**  getting Gauss graphics to do equilateral triangles is too hard!
**
**  REFERENCE:
**  Jonathan Katz and Gary King. 1999. "A Statistical Model for Multiparty 
**  Electoral Data," AMERICAN POLITICAL SCIENCE REVIEW, forthcoming, March.
**  copy available at http://gking.harvard.edu/preprints.shtml
*/
declare matrix _ternary_df != 0;
declare string _ternary_L != "u\000v\000w";
declare matrix _ternary_w != 1;
declare matrix _ternary_t != 1;
declare matrix _ternary_cr != 0;
declare matrix _ternary_cov != 0;
declare matrix _ternary_g != 1;
#include pline.g;
#include psym.g;
#include probs.src;  @ David Baird's probability procs @
proc 0=ternary(u,v);
  local x,y,w,isq3,cu,cv,o,r,clr,md,f,vc,tt,e1,e2,tt3,tomogc,i,e,z1,z2,rho;
  cu=cols(u);
  cv=cols(v);
  md=ismissm(sumc((u+v)'));
  u=delif(u,md);
  v=delif(v,md);
  if rows(_psym_t)/=1;
    _psym_t=delif(_psym_t,md);
  endif;
  if rows(_psym_c)/=1;
    _psym_c=delif(_psym_c,md);
  endif;
  if rows(_psym_h)/=1;
    _psym_h=delif(_psym_h,md);
  endif;
  if not(scalzero(_ternary_g)) and not(scalone(_ternary_g));
    "ternary: _ternary_g must be 0 or 1";
    stop;
  endif;
  w=1-u-v;
  if sumc(vec(u~v~w).>1)>0 or sumc(vec(u~v~w).<0)>0
    or cu>2 or cv>2 or cu/=cv;
    #ifunix;
    wincloseall;
    #endif;
    "ternary: problem with inputs";
    end;
  endif;
  isq3=1/sqrt(3);
  
  /* translate to ternary plot coordinates */
  x=(v-w)*isq3;
  y=u;
  
  /* draw arrows when inputs are nx2, symbols otherwise */
  if cols(u)==2;
    r=rows(u);
    o=ones(r,1);
    if rows(_psym_c)==1;
      clr=o*_psym_c;
    elseif rows(_psym_c)==r;
      clr=_psym_c;
    else;
      #ifunix;
      wincloseall;
      #endif;
      "ternary: problem with global _psym_c";
      end;
    endif;
    _parrow=x[.,1]~y[.,1]~x[.,2]~y[.,2]~o*2~o*0.05~o*21~clr~o*1~o*6~o*0.1;
  else;            /* plot symbols */
    psym(x,y);
  endif;

  /* draw win lines */
  if scalone(_ternary_w);
    pline(0,0,0,1/3);
    pline(-isq3/2,0.5,0,1/3);
    pline(isq3/2,0.5,0,1/3);
  endif;

  /* draw triangle */
  if scalone(_ternary_t); @ ends of triangle only @
    f=0.25;
    pline(-isq3,0,-isq3+f*isq3/2,f/2);
    pline(-isq3,0,-isq3+f*isq3,0);
    pline(0,1,f*isq3/2,1-f/2);
    pline(0,1,-f*isq3/2,1-f/2);
    pline(isq3,0,isq3-f*isq3/2,f/2);
    pline(isq3,0,isq3-f*isq3,0);
  else;                   @ draw full triangle @
    pline(-isq3,0,isq3,0);
    pline(-isq3,0,0,1);
    pline(0,1,isq3,0);
  endif;
  
  /* draw confidence regions */
  if not(scalzero(_ternary_cr));
    r=rows(_ternary_cr);
    if r<6;
      "ternary: _ternary_cr must have at least 6 elements";
      #ifunix;
      wincloseall;
      #endif;
      stop;
    endif;
    clearg _ternary_cov;
    _ternary_cov=_ternary_cr[6:r].*(1~1);
    i=6;
    do while i<=rows(_ternary_cr);
      if _ternary_cr[i]>1 or _ternary_cr[i]<0;
	"ternary: _ternary_cr[r] r>5 must be in [0,1]";
	#ifunix;
	wincloseall;
	#endif;
	stop;
      endif;

      /* prepare ellipses on logistic scale */
      if scalzero(_ternary_df);
	tomogC=invcfchi(_ternary_cr[i],2);
      elseif _ternary_df>0;
	tomogC=invcdff(_ternary_cr[i],2,_ternary_df)*2;
      else;
	"ternary: _ternary_df must be >=0";
	stop;
      endif;
      vc=xpnd(_ternary_cr[3 5 4]);
      tt=seqas(0,2*pi,1000);
      e=_ternary_cr[1 2]'+sqrt(tomogC)*(sin(tt)~cos(tt))*chol(vc);

      /* compute fraction of points covered by confidence region */
      tt=mlogit(u~v);
      z1=(tt[.,1]-_ternary_cr[1])./sqrt(vc[1,1]);
      z2=(tt[.,2]-_ternary_cr[2])./sqrt(vc[2,2]);
      rho=vc[1,2]/(sqrt(vc[1,1]*vc[2,2]));
      tt=(z1^2+z2^2-2*rho*z1.*z2)/(1-rho^2);
      _ternary_cov[i-5,2]=meanc(tt.<=tomogC);
      
      /* translate: */
      e=mlogiti(e);                       @ ---> to votes scale @
      e1=(e[.,2]-(1-e[.,1]-e[.,2]))*isq3; @ ---> to ternary coordinates @
      e2=e[.,1];
      
      /* draw region */
      _plineC=12;
      plines(e1,e2);
  
      i=i+1;
    endo;
  endif;
  
  /* graph parameters */
  _paxes=0;
  _pframe={0,0};
  _plotsiz={3,3};
  _plctrl=-1;
  _pdate="";
  _pmsgstr=_ternary_L;
  _pmsgctl=(0-.02~1.04~.25~0~1~13~0)|
          ((isq3+0.04)~0~.25~0~1~13~0)|
	  ((-isq3-0.08)~0~.25~0~1~13~0);
  
  /* invisibly plot corners of triangle for scaling */
  _psymsiz=0.0001;
  x=-isq3|0|isq3;
  y=0|1|0;
  if _ternary_g;
    xy(x,y);
  endif;
endp;
