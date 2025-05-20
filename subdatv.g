/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  creates named vectors in memory from variables in dataset
**
**  dta = subdatv(dataset,vars);
**
**  INPUTS
**  dataset = string with a dataset name on disk
**  vars = 0 for all variables or vector of names
**
**  OUTPUT
**  dta = matrix with only vars varables
**  also created are global variables by the names in vrs
**
**  example:    dataset="c:\\data\\myfile";
**              clear age,sex,race;     @ must clear if not used interactively @
**              let vars=age sex race;      @ vars to read in  @
**              call subdatv(dataset,vars); @ creates age, sex, and race @
**
**  example 2:  call subdatv("mydata",0);
**              @ creates a global vector for each vars in mydata @
**
**  example 3:  dta = subdatv("mydata","age"|"sex");
**              'age' and 'sex' are created and dta=age~sex;
**
**  GLOBALS:
**      _subrow = 0 (for all rows)
**                or vector of row indices to be read (e.g. rows 2 5 97).
**      _makevar = 1 (default) create variables in memory
**                 0 no vars created.
*/
external proc indices,makevars,subdat;
declare matrix _subrow ?= 0;
declare matrix _makevar ?= 1;
proc subdatv(dataset,vars);
    local f1,d,nm,inx;
    if vars==0;  vars=getname(dataset);  endif;
    open f1=^dataset for read;
    {nm,inx}=indices(dataset,vars);
    d=subdat(f1,_subrow,inx);
    if _makevar;
        call makevars(d,0,vars);
    endif;
    closeall f1;   ndpclex;
    retp(d);endp;

