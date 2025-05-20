/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
** triple scatter plot
**
** Usage:  call triple(x,y,z);
**
**         x is plotted horizontally,
**         y is plotted vertically,
**         z is the size of the circle to be plotted at the x,y coordinate.
**           it should be linearly scaled to be from about 0.5 to about 10.
**
**         xtics and ytics should be set before running this proc.
*/
external proc xy,xtics,ytics;
external matrix _pxscale;
external matrix _pyscale;
declare matrix _psym != 0;
proc 0=triple(x,y,z);
    local os,min,max;
    if rows(z)==1;
      os=ones(rows(x),1);
      z=os*z;
    endif;
    {x,y,z}=listwis3(x,y,z);
    os=ones(rows(x),1);

    if cols(_psym)==6;    
      _psym=_psym|(x~y~os*1~z~os*15~os~os);
    else;                       
      _psym=(x~y~os*1~z~os*15~os~os);  
    endif;
    if _pxscale==0;
        min=floor(minc(x)*10)/10;max=ceil(maxc(x)*10)/10;
        call xtics(min,max,(max-min)/10,2);
    endif;
    if _pyscale==0;
        min=floor(minc(y)*10)/10;max=ceil(maxc(y)*10)/10;
        call ytics(min,max,(max-min)/10,2);
    endif;
    draw();
    endp;

/*
** {a1,b1,c1}=listwis3(a,b,c);
**
** INPUTS:  a,b,c have same number of rows
**
** a1,b1,c1 equal a,b,c except rows with any missing values are deleted.
*/
proc 3=listwis3(a,b,c);
  clearg ma,mb,mc,m;
  ma=ismissm(a);
  mb=ismissm(b);
  mc=ismissm(c);
  m=sumc((ma~mb~mc)').>0;
  a=delif(a,m);
  b=delif(b,m);
  c=delif(c,m);
  retp(a,b,c);
endp;