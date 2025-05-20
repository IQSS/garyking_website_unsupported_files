/*  regv.g
**
**  same as reg.g except arguments are lists of variable Names
**
**  example 1:  call subdatv("thedata","age sex race educ income");
**              call regv("age sex race educ","income");
**
**  example 2:  let x=age sex race educ;
**              let y=income;
**              call subdatv("data",x|y);
**              {b,se,vc,yhat,e,sig}=regv(x,y);
**
**  output is the same as reg.g
**  globals are the same as reg.g, except _rYname and _rXnames are ignored.
**
*/
proc 6=regv(x,y);
    local b,se,vc,yhat,e,sig;
    clearg _rXnames,_rYname;
    _rXnames=x;
    _rYname=y;
    {b,se,vc,yhat,e,sig}=reg(mergevar(x),mergevar(y));
    clearg _rXnames,_rYname;
    retp(b,se,vc,yhat,e,sig);endp;

