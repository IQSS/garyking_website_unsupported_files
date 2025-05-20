/*  regvr.g, adapted from regv.g by Curt Signorino 12/2/92
**
**  same as regr.g except arguments are lists of variable Names
**
**  example 1:  call subdatv("thedata","age sex race educ income");
**              call regvr("age sex race educ","income",c);
**
**  example 2:  let x=age sex race educ;
**              let y=income;
**              call subdatv("data",x|y);
**              {b,se,vc,yhat,e,sig}=regv(x,y,c);
**
**  output is the same as regc.g
**  globals are the same as regc.g, except _rYname and _rXnames are ignored.
**
*/
proc 6=regvr(x,y);
    local b,se,vc,yhat,e,sig;
    clearg _rXnames,_rYname;
    _rXnames=x;
    _rYname=y;
    {b,se,vc,yhat,e,sig}=regc(mergevar(x),mergevar(y),c);
    clearg _rXnames,_rYname;
    retp(b,se,vc,yhat,e,sig);
endp;

