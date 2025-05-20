/*
** y = mlogit(vote);
**
** vote = n x p-1 matrix of votes or compositions, each element of which is
**        [0,1], and the sum of each row is <=1 (the last composition, to make
**        the sum equal 1, is omitted).
**
** y    = n x p-1 matrix of ln(vote[.,i]./lastparty), where lastparty
**        is the omitted party from vote
*/
proc mlogit(vote);
  local lvote,lastvote,a;
  lastvote=1-sumc(vote');
  lvote=ln(vote./lastvote);
  retp(lvote);
endp;
