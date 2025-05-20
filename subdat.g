/*
** SUBDAT.G  
** (typed in from an old Gauss newsleter).  I use this primarily
** as called by subdatv.g
**
** Purpose:     Returns a submatrix of a data set.
** 
** Format :     y = SUBDAT(file,rv,cv);
**
** Input  :     file -- string, name of data set
**                           or
**                      scalar, file handle of an open file
**
**                      If a file handle is passed, each call will begin
**                      reading from the current position in the file.  If
**                      a vector of row indices is passed in they will also
**                      be relative to the current position in the file.
**
**               rv -- Nx1 numeric vector, the row indices of the rows to read
**                              or
**                     negative scalar, |rv| rows will be read
**                              or
**                     scalar 0, all of the rows will be read
**
**               cv -- Nx1 numeric vector, the column indices
**                              or
**                     scalar 0, all of the columns will be read
**
** Output :      y -- the data read from the file
**
** Remarks :
**               If a file handle (fp) and a 3x1 vector of row indices (rv)
**               are passed, then 3 rows will be read and the row indices
**               will be interpreted relative to the current position in
**               the file.  Row 1 in the vector of indices is interpreted
**               as the current row.
**
**               For example, if the next sequential row to be read from the
**               file is the 100th row:
**
**                   rv = { 1,3,5 };
**                   cv = { 3,4,7,11 };
**                   y  = subdat(fp,rv,cv);
**
**               y will be a 3x4 matrix from rows 100,102,104.
**
**                   rv = { -1,3,-5 };
**                   cv = { 3,4,7,11 };
**                   y  = subdat(fp,rv,cv);
**
**               y will be a 3x4 matrix from rows 98,102,94.
**
**               In general the actual row indices read are:
**
**                    (current_row -1) + rv
**
**               Error returns are controlled by the trap flag:
**
**                   Trap 0     exit with error message
**
**                   Trap 1     return error code
**
**                              98  seeks out of range
**
**                              99  file can't be opened
*/

proc subdat(file,rv,cv);
    local oneshot,i,k,nr,fp,x,temp;
    if type(file) == 13;      /* string, file name */
        open fp = ^file;
        if fp == -1;
            if trapchk(1);
                retp(error(99));
            else;
                errorlog "ERROR: SUBDAT can't open file";
                end;
             endif;
         endif;
         oneshot = 1;
     else;              /* should be scalar, file handle */
         fp = file;
         oneshot =0;
     endif;
     k = colsf(fp);
     nr = floor(minc(coreleft/(k*8*5)|8000/(k+1)));
     if rv == 0;      /* read all rows */
           temp = readr(fp,nr);
           x = temp[.,cv];
           do until eof(fp);
               temp = readr(fp,nr);
               x = x|temp[.,cv];
           endo;
      elseif rows(rv)*cols(rv) == 1 and rv < 0;   /* read |rv| rows */
          rv = abs(rv);
          if nr > rv;
              nr = rv;
          endif;
          temp = readr(fp,nr);
          x = temp[.,cv];
          rv = rv-rows(temp);
          do until not(rv) or eof(fp);
              print "loop";
              if nr > rv;
                   nr = rv;
              endif;
              temp = readr(fp,nr);
              rv = rv-rows(temp);
              x = x|temp[.,cv];
           endo;
        else;          /* read selected rows  */
            rv = (seekr(fp,-1)-1)+rv;
            if not(rv >0) or not(rv <= rowsf(fp));
                if oneshot;
                    fp =  close(fp);
                endif;
                if trapchk(1);
                    retp(error(98));
                else;
                    errorlog "ERROR: seeks out of range in SUBDAT";
                    end;
                endif;
             endif;
             call seekr(fp,rv[1]);
             temp = readr(fp,1);
             x = temp[1,cv];
             i = 2;
             do while i <= rows(rv);
                 call seekr(fp,rv[i]);
                 temp = readr(fp,1);
                 x = x|temp[1,cv];
                 i= i + 1;
              endo;
         endif;
         if  oneshot;
             fp = close(fp);
         endif;
         retp(x);
    endp;

