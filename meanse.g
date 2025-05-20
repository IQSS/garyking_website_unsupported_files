/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  conditional means, standard errors, standard deviations
**  (useful for summarizing results of Monte Carlo experiments)
**
**  {cats,m}=meanse(x,flag);
**
**  INPUTS:
**  x = ixj, 1st column is integer categories; others are to be averaged
**  flag = (j-1)x1, corresponding to column of x except the first compute
**             within categories of unique(x[.,1]), 
**         0, compute mean, meanc(x)
**         1, compute standard error of the average, sqrt(meanc(x^2)/rows(x))
**         2, compute average standard deviation, sqrt(meanc(x^2))
**        -1, don't calculate
**
**  OUTPUTS:
**  cats = kx1 vector of unique elements of x[.,1]
**  m    = kxj vector of statistics indicated by flag computed within
**         categories of cats
**
**  GLOBALS:  _mtruncU = (j-1)x1, .=do nothing, #=trunc at # if above
**            _mtruncL = (j-1)x1, .=do nothing, #=trunc at # if below
**                  both are ignored if flag[i]=-1;
*/
declare matrix _mtruncU ?={.};
declare matrix _mtruncL ?={.};
proc 2=meanse(x,flag);
    local u,i,cde,mns,t,Tmns,tt,j,m,mtruncU,mtruncL;   
    m={.};
  if cols(x)<=2; 
    errorlog "x must be at least 2 cols"; 
  endif;
  if rows(flag)/=(cols(x)-1);
    errorlog "FLAG wrong length";
    end;
  endif;
  if (sumc(flag.==1)+sumc(flag.==0)+sumc(flag.==-1)+
    sumc(flag.==2))/=rows(flag);
    "FLAG wrong values";
    end;
  endif;
  cde=x[.,1];
  u=unique(cde,1);
  x=x[.,2:cols(x)];
  t=sumc(flag./=-1);
  if (not scalmiss(_mtruncU));
    if rows(_mtruncU)/=rows(flag) or cols(_mtruncU)/=1;
      "Error in _mtruncU";
      end;
    endif;
    mtruncU=_mtruncU;
  else;
    mtruncU=m*ones(rows(flag),1);
  endif;
  if (not scalmiss(_mtruncL));
    if rows(_mtruncL)/=rows(flag) or cols(_mtruncL)/=1;
      "Error in _mtruncL";
      end;
    endif;
    mtruncL=_mtruncL;
  else;
    mtruncL=m*ones(rows(flag),1);
  endif;
  mns=zeros(1,t);

  i=1;
  do while i<=rows(u);
    t=selif(x,cde.==u[i]);
    Tmns=0;
    j=1;
    do while j<=cols(x);
      tt=packr(t[.,j]);
      if not scalmiss(mtruncL[j]);
	tt=recode(tt,tt.<mtruncL[j],mtruncL[j]);    
      endif;
      if not scalmiss(mtruncU[j]);
	tt=recode(tt,tt.>mtruncU[j],mtruncU[j]);    
      endif;
      if     flag[j]==1;
	Tmns=Tmns|sqrt(meanc((tt^2)/rows(tt)));
      elseif flag[j]==2;
	Tmns=Tmns|sqrt(meanc((tt^2)));
      elseif flag[j]==0;
	Tmns=Tmns|meanc(tt);
      endif;
      j=j+1;
    endo;
    mns=mns|trimr(Tmns,1,0)';
    i=i+1;
  endo;
  mns=trimr(mns,1,0);
  retp(u,mns);
endp;
