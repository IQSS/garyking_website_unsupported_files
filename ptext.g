/*
**  (C) Copyright 1999 Ken Benoit, Curt Signorino, Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
** Scatter plot w/text plotted at (x,y) coordinates
**
** Usage:  ptext(x,y,text,textht,textwt);
**
**         x      = Nx1 vector. X coord to plot string.
**         y      = Nx1 vector. Y coord to plot string.
**         text   = Nx1 vector whose elements are the strings to be plotted
**                  at the (x,y) coords.
**         textht = Nx1 vector of text heights corresponding to each string,
**                  or scalar text height for all strings.  If textht<=0,
**                  the default textht (.15) will be used.
**         textwt = Nx1 vector of line widths corresponding to each string,
**                  or scalar line width for all strings.  If textwt<=0,
**                  the default textwt (0) will be used.
**
** Also:   All other DRAW globals may be used (e.g., to turn axis or line
**         numbering on or off, etc).  Additionally, the user should run
**         commands like TITLE, XLABEL, YLABEL, etc before running PTEXT.
**         However, if XTICS and YTICS have not been run, PTEXT will perform
**         the scaling automatically.
**
** This proc is a modification of an earlier proc, xylabc.g, by Ken Benoit.
** edited by Curt Signorino, 8/1/94
** more changes by Gary King 1/14/97
**
*/
proc (0) = ptext(x,y,text,textht,textwt);
    local onevec, min, max, i, tcolor, buff;
    tcolor=10;
    onevec=ones(rows(x),1);                 @ Nx1 vector of 1's @
    _pmsgstr="";
    i=1;
    do until i==rows(x);
        _pmsgstr=_pmsgstr$+text[i]$+"\000"; @ create plot msg string @
        i=i+1;
    endo;
    _pmsgstr = _pmsgstr$+text[rows(x)];
    if rows(textht)==1;                     @ if textht is scalar @
        if textht<=0;                       @ if default value desired @
            textht=.15;                     @ default height = .15 inches @
        endif;
        textht=ones(rows(x),1).*textht;     @ create Nx1 vector of hts @
    endif;
    if rows(textwt)==1;                     @ if textwt is scalar @
        if textwt<=0;                       @ if default value desired @
            textwt=0;                       @ default thickness = 0 @
        endif;
        textwt=ones(rows(x),1).*textwt;     @ create Nx1 vector of wts @
    endif;
    _pmsgctl = x~y~textht~(onevec.*0)~onevec~(onevec.*tcolor)~textwt;
    if _pxscale==0;                         @ if x-axis not scaled @
/*        min=floor(minc(x));                 @ min x value @
        max=ceil(maxc(x));                  @ max x value @
*/	
        min=(minc(x));                 @ min x value @
        max=(maxc(x));                  @ max x value @
        buff=0.1*abs(max-min);              @ 10% buffer around min & max @
        xtics(min-buff,max+buff,(max-min+2*buff)/10,2); @ scale x-axis @
    endif;
    if _pyscale==0;                         @ if y-axis not scaled @
/*
        min=floor(minc(x));                 @ min x value @
        max=ceil(maxc(x));                  @ max x value @
*/
      min=(minc(y));                 @ min y value @
      max=(maxc(y));                  @ max y value @

      buff=0.1*abs(max-min);    @ 10% buffer around min & max @ 
      ytics(min-buff,max+buff,(max-min+2*buff)/10,2); @ scale y-axis @
    endif;      
    draw;
endp;
  



