/*
**  (C) Copyright 1999 Gary King
**  All Rights Reserved.
**  http://GKing.Harvard.Edu, King@Harvard.Edu
**  Department of Government, Harvard University
**
**  rename element in data buffer
**
**  useage:  dbuf = vrename(dbuf,"old","new");
*/
proc vrename(dbuf,stro,strn);
  local x;
  {x,dbuf}=vget(dbuf,stro);
  dbuf=vput(dbuf,x,strn);
  retp(dbuf);
endp;