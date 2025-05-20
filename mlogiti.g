/*
** vote = mlogiti(lvote);
**
** vote  = n x p-1 matrix of logit of votes from n districts and p parties
** lvote = n x p-1 matrix of exp(party[.,i]/(1+sumc(exp(lvote)))
**
** inverse of mlogit()
*/
proc mlogiti(lvote);
  local vote;
  lvote=exp(lvote)./(1+sumc(exp(lvote)'));
  retp(lvote);
endp;
