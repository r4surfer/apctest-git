/* copyright (c) BYTE DESIGNS ltd - 1988-1993
----------------------------------------------------------------------
By: Zortec Inc. for Byte Designs Ltd.  
- for internal use by the decimal numbering system. 
----------------------------------------------------------------------
*/

static char decxtra_SccsId[] = "@(#) decxtra.h  Version 3.21  05/15/95";

#define ACCSIZE (DECSIZE+1)

struct decacc
  {
  short dec_exp, dec_pos, dec_ndgts;
  char dec_dgts [ACCSIZE];
  };

typedef struct decacc dec_a;
