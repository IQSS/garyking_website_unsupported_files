/*
** (C) Copyright 1999 Gary King and Langche Zeng, All Rights Reserved.
** King@Harvard.Edu, langche@everest.fas.harvard.edu
** Current version available at http://GKing.Harvard.Edu
**
** ReLogitQ: Quantities of Interest from Rare Events Logistic regression.
**
** This procedure computes absolute risks (probabilities), relative risks, 
** first differences (risk differences), and number needed to treat
** along with measures of uncertainty from ReLogit() model coefficient
** estimates, with corrections for rare events, finite samples, and 
** selection on Y.  It implements suggestions in Gary King and 
** Langche Zeng's "Logistic Regression in Rare Events Data," and
** "Inference in Case-Control Studies With Limited Auxilliary
** Information", both available at http://GKing.Harvard.Edu.
**
** USAGE
**
** library maxlik;
** {b,vc,llik,ret} = relogit(y,x);  @ estimates logit parameters @
** {Q,U} = relogitQ(x0,b,vc);  @ quantities of interest & uncertainties @
**
** INPUTS to relogitQ
**
** x0 = 2xk matrix of values of explanatory variables (with constant)
** 	for relative risks and risk differences and number needed to treat (to
** 	estimate what happens when x changes from  x0[1,.] to x0[2,.]); 
**	or mxk for probabilities (to set to means, use the relogit() output
**      global _relogit_mx).
** (b & vc are exactly as output from relogit)
** b  = kx1 vector of coefficients; or if _relogit_sel=1|lo|hi from relogit,
**      (k+3)x1 vector, lo|hi|meanc(y)|coefficients
** vc = kxk estimated variance matrix of b
**
** OUTPUTS
**
** Q = point estimate of probabilities (mx1), relative risks,
**     risk differences, or number needed to treat (1x1); 
** U = confidence intervals (1x2 for AR,RR,NNT or mx2 for P) or matrix 
**     of all simulations. 
**
** GLOBALS
**
** _relogitQ_Q: define output Q Quantity of interest.
**              "P"   probability,       Pr(y=1|x0)  (default)
**              "RR"  relative risk,     Pr(y=1|x0[2,.]) / Pr(y=1|x0[1,.])
**              "RD"  risk difference, Pr(y=1|x0[2,.]) - Pr(y=1|x0[1,.])
**              "NNT" number needed to treat, 1/[Pr(y=1|x0[1,.]) - C]
**                    where C=min(Pr(y=1|x0[1,.]),Pr(y=1|x0[2,.])) 
**              
** _relogitQ_U: define output U Uncertainty measure.
**              1|ci  ci*100% confidence interval    (default=1|0.90)
** 		2     (_relogitQ_sims x C) matrix of all simulations, 
**		      [where C=m (for P) or 1 (for other Q's) if rows(b)=k
**		      or rows(b)=k+3 & _relogitQ_Pt==3, otherwise C=2*m 
**                    (for P) or 2 (for other Q's) with the first C/2 columns
**                    corresponding to "lo" and the rest to "hi".]
**
** _relogitQ_pt: 1       use analytical method for point estimate
**                       (default, cannot be used if rows(b)=k+3)
**               2|alpha Treat information about tau generalized from Manski: 
**                       with probability alpha, tau is believed to be in 
**                       [lo,hi] with unknown density; and with probability 
**                       (1-alpha)/2 each in [0,lo] and [hi,1] with Jeffrey's
**                       invariance prior density.
**               3|prior Fully Bayesian (assuming _relogit_mcn=2 was set in 
**                       relogit()). If "prior" is 0, use uniform prior for 
**                       tau in [lo,hi]), if 1, use Jeffrey's invariance
**                       prior. When tau is known exactly, "prior" is ignored.
**                       Compute point estimates with posterior median.
**
** _relogitQ_bsims: 0 draw simulations via normal approximation (default);
**                  1 draw simulations via importance resampling (_relogitq_ds
**                    must be set, and _relogit_mcn=2 must be used in 
**                    relogit())  (_relogitQ_sims x rows(b)) matrix of 
**                    simulated b's. Useful for saving time in Monte Carlo 
**                    experiments.
**
** _relogitQ_sims: number of simulations in _relogitQ_pt (default=10000);
**                 reduce for speed, increase for precision
**
** _relogitq_pop: size of the population under study. lo=0
**                is recoded to _relogit_n[2]/_relogitq_pop and
**                hi=1 to 1-_relogit_n[1]/_relogitq_pop.
**                Default is 270000000 (US pop).
*/
declare matrix _relogitq_q="P";
declare matrix _relogitq_u={1,0.9};
declare matrix _relogitq_pt=1;
declare matrix _relogitq_sims=10000;
declare matrix _relogitq_ds={.};
declare matrix _relogitq_bsims=0;
declare matrix _relogitq_pop=270000000;

proc (2)=relogitq(x,b,vc);
  local q,bsims,rx,u,ci,lo,hi,Slo,Shi,s,cs,rs,blo,bhi,my,c,k,bs,i,e,ii,
        interval, manski,tau, rlow,rhi, njeffrey, nbounds, btau, stau;

  /* verify inputs */
  _relogitq_q=upper(_relogitq_q);
  rx=rows(x);
  if in(_relogitq_q,"RD"|"RR"|"NNT",0);
    if rx/=2;
      "rows(x) must be 2 with _relogitq_q=RD, RR or NNT";end;
    endif;
  elseif _relogitq_q$/="P";
    "_relogitq_q must be P, RD, RR, or NNT (don't forget quote marks)";end;
  endif;
  if not(in(_relogitq_pt[1],1|2|3,1));
    "_relogitq_pt[1] must be 1, 2 or 3";end;
  endif;
  if rows(_relogitq_sims)/=1 or _relogitq_sims<1;
    "_relogit_sims error";end;
  endif;    
  if not(scalzero(_relogitq_bsims)) and not(scalone(_relogitq_bsims));
    if rows(_relogitq_bsims)/=_relogitq_sims or cols(_relogitq_bsims)/=rows(vc);
      "_relogitq_bsims error";end;
    endif;
  endif;
  if _relogitq_bsims==1;			@ IS @
    if scalmiss(_relogitq_ds);
      "_relogitq_ds must be defined if _relogitq_bsims=1";	end;
    endif;
  endif;

  if rows(b)>rows(vc); 		     @ bounds on tau provided in relogit @
    if _relogitq_pt[1]==1;
      "Can't use analytical formula when using bounds for tau";end;
    endif;
    lo=b[1];
    hi=b[2];
    lo=recode(lo,lo==0,_relogit_n[2]/_relogitq_pop);
    hi=recode(hi,hi==1, 1-_relogit_n[1]/_relogitq_pop);  
    my=b[3];
    b=trimr(b,3,0);
    if _relogitq_pt[1]==2;	      @ manski@
      njeffrey=round(_relogitq_sims*(1-_relogitq_pt[2])/2); 
          @number of Jeffrey's rnd numbers to draw for tau @
      nbounds=round(_relogitq_sims*_relogitq_pt[2]); @ sims of lo/hi bounds @
      if njeffrey>0;
        tau=lo*rndu(njeffrey,1);	
        tau=tau|(hi+(1-hi)*rndu(njeffrey,1));
        tau=(sin((pi/2)*tau))^2;		@ Jeffrey's @
        tau=recode(tau,(tau.==0)~(tau.==1),(_relogit_n[2]/_relogitq_pop)|
                         (1-_relogit_n[1]/_relogitq_pop));
      endif;
    endif;
  else;					@ tau is known in relogit @
    lo={.};
    hi={.};
  endif;
  

  /* simulations */
  if scalzero(_relogitq_bsims);          @ normal approx @
    bsims=rndmn(b,vc,_relogitq_sims);	
  elseif scalone(_relogitq_bsims);      @ importance resampling @
    bsims=rndisamp(&llf,b,vc,_relogitq_ds,_relogitq_sims);
  else;
    bsims=_relogitq_bsims;              @ input sims @
  endif;
  if _relogitq_pt[1]==3 and not(ismiss(lo));   @ Bayes & tau in [lo,hi] @
    tau=lo+(hi-lo)*rndu(_relogitq_sims,1);	@uniform prior on [lo,hi]@
    if _relogitq_pt[2]==1;			@ Use Jeffrey's prior @
       tau=(sin((pi/2)*tau))^2;
    endif;
       tau=recode(tau,(tau.==0)~(tau.==1),(_relogit_n[2]/_relogitq_pop)|
                         (1-_relogit_n[1]/_relogitq_pop));
    bsims[.,1]=bsims[.,1]-(ln(1-tau)-ln(tau)+ln(my)-ln(1-my));
  endif;
  s=logiti(x*bsims')';			@ _relogitq_sims by cols(x) @

  /* point estimate */
  if _relogitq_q$=="P";			@ probability @
    if ismiss(lo) or _relogitq_pt[1]==3;	@ tau known or bayesian @
      if _relogitq_pt[1]==1;
        gosub ana;
      else;
        q=median(s);
      endif;
      gosub uncer;
    else;				@ tau in [lo,hi] and manski @
      blo=bsims[1:nbounds,.];
      blo[.,1]=blo[.,1]-(ln(1-lo)-ln(lo)+ln(my)-ln(1-my));
      Slo=(logiti(x*blo'))';
      bhi=bsims[1:nbounds,.];
      bhi[.,1]=bhi[.,1]-(ln(1-hi)-ln(hi)+ln(my)-ln(1-my));
      Shi=(logiti(x*bhi'))';
      if njeffrey>0;
        btau=bsims[(nbounds+1):rows(bsims),.];
        btau[.,1]=btau[.,1]-(ln(1-tau)-ln(tau)+ln(my)-ln(1-my));
        stau=(logiti(x*btau'))';
        Slo=Slo|stau;
        Shi=Shi|stau;
      endif;
      s=Slo~Shi;
      gosub uncer;
      q=meanc(interval');
    endif;
      
  elseif _relogitq_q$=="RR";		@ relative risk @
    if ismiss(lo) or _relogitq_pt[1]==3;	@ tau known or bayesian @
      s=s[.,2]./s[.,1];
      if _relogitq_pt[1]==1;
	gosub ana;
	q=q[2]/q[1];
      else;
	q=median(s);
      endif;
      gosub uncer;
    else;				@ tau in [lo,hi] and manski @
      blo=bsims[1:nbounds,.];
      blo[.,1]=blo[.,1]-(ln(1-lo)-ln(lo)+ln(my)-ln(1-my));
      Slo=((1+exp(-x[1,.]*blo'))./(1+exp(-x[2,.]*blo')))';
      bhi=bsims[1:nbounds,.];
      bhi[.,1]=bhi[.,1]-(ln(1-hi)-ln(hi)+ln(my)-ln(1-my));
      Shi=((1+exp(-x[1,.]*bhi'))./(1+exp(-x[2,.]*bhi')))';
      s=minc((Slo~Shi)')~maxc((Slo~Shi)');
      if njeffrey>0;
        btau=bsims[(nbounds+1):rows(bsims),.];
        btau[.,1]=btau[.,1]-(ln(1-tau)-ln(tau)+ln(my)-ln(1-my));
        Stau=((1+exp(-x[1,.]*btau'))./(1+exp(-x[2,.]*btau')))';
        s=s|(stau~stau);
      endif;
      gosub uncer;
      q=meanc(interval');
    endif;
  
  else;					 @ Risk Difference or NNT @
    if ismiss(lo) or _relogitq_pt[1]==3; @ tau known or bayesian @
      if _relogitq_q$=="RD";		 @  RD @
        s=s[.,2]-s[.,1];
        if _relogitq_pt[1]==1;
          gosub ana;
          q=q[2]-q[1];
        else;
          q=median(s);
        endif;
      else;				 @ NNT @
        s=1./(s[.,1]-minc(s[.,1]'|s[.,2]'));
        if _relogitq_pt[1]==1;
	  gosub ana;
          q=1/(q[1]-minc(q[1]|q[2]));
        else;       
          q=median(s);
        endif;
      endif;
      gosub uncer;
    else;				@ tau in [lo,hi] and manski @
      s=sqrt(exp((x[2,.]-x[1,.])*bsims[1:nbounds,.]')');
      s=(s-1)./(s+1);
      blo=bsims[1:nbounds,.];
      blo[.,1]=blo[.,1]-(ln(1-lo)-ln(lo)+ln(my)-ln(1-my));
      Slo=logiti(x[2,.]*blo')'-logiti(x[1,.]*blo')';
      bhi=bsims[1:nbounds,.];
      bhi[.,1]=bhi[.,1]-(ln(1-hi)-ln(hi)+ln(my)-ln(1-my));
      Shi=logiti(x[2,.]*bhi')'-logiti(x[1,.]*bhi')';
      k=cols(x);
      bs=(-(x[1,2:k]+x[2,2:k])*bsims[1:nbounds,2:k]'/2)';
      c=((blo.<=bs).and(bs.<=bhi));  @ [lo,hi] contains tau*? @
      s=minc((Slo~Shi~s)')~maxc((Slo~Shi~s)');  @ if yes @
      s=c.*s+(1-c).*(minc((Slo~Shi)')~maxc((Slo~Shi)'));
      s=substute(s,(s[.,1].<-1)~(s[.,2].>1),-1~1);	@ s must be in [-1,1]@
      if njeffrey>0;
        btau=bsims[(nbounds+1):rows(bsims),.];
        btau[.,1]=btau[.,1]-(ln(1-tau)-ln(tau)+ln(my)-ln(1-my));
        Stau=logiti(x[2,.]*btau')'-logiti(x[1,.]*btau')';
        s=s|(stau~stau);
      endif;
      if _relogitq_q$=="NNT"; @deriving bounds for NNT based on that for RD @
        s=1./(-substute(s,s.>=0,0));
        s=minc(s')~maxc(s');
      endif;
      gosub uncer;
      q=meanc(interval');
    endif;

  endif;
    
  retp(q,u);
  
  ana:  /* analytical computation of point estimates */
    q=logiti(x*b);
    q=q+(q-q^2).*(0.5-q).*sumc((x.*(x*vc))');
    q=recode(q,(q.<0)~(q.>1),0|1);
  return;  

  uncer:  /* uncertainty measure */
  manski=(not(ismiss(lo)) and _relogitq_pt[1]==2);  @ Manski & [lo,hi] @
  if _relogitq_u[1]==1 or manski;   @ need CI for U and/or computing Q @
    cs=cols(s);
    rs=rows(s);
    for ii (1,cs,1);i=ii;
      s[.,i]=sortc(s[.,i],1);
    endfor;
    
    if manski and _relogitq_u[1]/=1;
      ci=(1-0.9)/2;  @ ci default @
    else;
      ci=(1-_relogitq_u[2])/2; 
    endif;
    
    rhi=int((1-ci)*rs);		@ row of s to take for the upper bound of CI@
    rlow=int(ci*rs);		@ row of s to take for the lower bound of CI@
    if rlow<1;
       rlow=1;
    endif;

    if manski; 			@ s corresponding to lo~hi @
      interval=(s[rlow,1:cs/2]')~(s[rhi,(cs/2+1):cs]');
    else;	        	@ s is posterior draws @
      interval=(s[rlow,.]')~(s[rhi,.]');
    endif;
  endif;

  /* U output */
  if _relogitq_u[1]==1;         @ CI @
    u=interval;
  else;				   @ report all sims @
    u=s;
  endif;
    
  return;

endp;






