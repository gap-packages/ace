
/*****************************************************************************

  enum02.inc
  27 Oct 03, 5 Apr 14
  Colin Ramsay, uqcramsa@uq.edu.au

  ACE 4.100: Advanced Coset Enumerator, Version 4.1, Release 00

Copyright (c)  2014  Centre for Discrete Mathematics and Computing, 
                     The University of Queensland, Australia

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.


To fully process all deductions properly we need to invoke the stack clearing 
routine several times in _rpefn().  The following code allow us to do this.  
It's equivalent to the `standard' code, but the loops have been unrolled & 
jumps removed.  This allows multiple copies to be included without problems, 
although we still have to be careful about the variable names used.  The 
(local) variables used are: irow, icol, ires, rcol, first, last, i, beg, end,
ifront, iback, fwd, l, bwd, m, mi.

*****************************************************************************/

while (topded >= 0)
  {
  irow = dedrow[topded];  
  icol = dedcol[topded--];

  if (COL1(irow) < 0)  { continue; }
  else                 { ires = CT(irow,icol);  rcol = invcol[icol]; }

  if ((first = edpbeg[icol]) >= 0)
    {
    last = edpend[icol];
    for (i = first; i <= last; i += 2)
      {
      beg = &(relators[edp[i]]);  end = beg + edp[i+1]-1;

      ifront = l = ires;  iback = irow;

      for (fwd = beg+1; fwd <= end; fwd++) 
        { 
        if ((l = CT(ifront, *fwd)) > 0)  { ifront = l; }
        else                             { break; }
        }

      if (l == 0)
        {
        for (bwd = end; bwd >= fwd; bwd--)
          {
          m = *bwd;  mi = invcol[m];

          if ((l = CT(iback, mi)) > 0)  { iback = l; }
          else if (bwd == fwd)
            {
            CT(iback, mi) = ifront;  CT(ifront, m) = iback;

            SAVED(iback, mi);
            iback = ifront;
            }
          else  { iback = ifront;  break; }
          }
        }

      if (iback != ifront)
        {
        if ((l = al0_coinc(ifront,iback,TRUE)) > 0)  { return(l); }
        if (COL1(irow) < 0 || COL1(ires) < 0)        { break; }
        }
      }
    }

  if (COL1(irow) >= 0 && COL1(ires) >= 0 && (irow != ires || icol != rcol))
    {
    if ((first = edpbeg[rcol]) >= 0)
      {
      last = edpend[rcol];
      for (i = first; i <= last; i += 2)
        {
        beg = &(relators[edp[i]]);  end = beg + edp[i+1]-1;

        ifront = l = irow;  iback = ires;

        for (fwd = beg+1; fwd <= end; fwd++) 
          { 
          if ((l = CT(ifront, *fwd)) > 0)  { ifront = l; }
          else                             { break; }
          }

        if (l == 0)
          {
          for (bwd = end; bwd >= fwd; bwd--)
            {
            m = *bwd;  mi = invcol[m];

            if ((l = CT(iback, mi)) > 0)  { iback = l; }
            else if (bwd == fwd)
              {
              CT(iback, mi) = ifront;  CT(ifront, m) = iback;

              SAVED(iback, mi);
              iback = ifront;
              }
            else  { iback = ifront;  break; }
            }
          }

        if (iback != ifront)
          {
          if ((l = al0_coinc(ifront,iback,TRUE)) > 0)  { return(l); }
          if (COL1(irow) < 0 || COL1(ires) < 0)        { break; }
          }
        }
      }
    }

  if (msgctrl && --msgnext == 0)
    {
    msgnext = msgincr;
    ETINT;
    printf("DD: a=%"PC" r=%"PC" h=%"PC" n=%"PC";", nalive, knr, knh, nextdf);
    printf(" l=%"PS" c=+%4.2f;", lcount, deltatime);
    printf(" d=%"PS"\n", topded+1);
    begintime = endtime;
    }
  }

