/* copyright BYTE DESIGNS LTD. (c) 1988-1993
------------------------------------------------------------------------------
This header contains information necessary for the use of the decimal type
numbering system.
------------------------------------------------------------------------------
*/

static char disamdec_SccsId[] = "@(#) disamdec.h  Version 3.21  05/15/95";

#ifndef DECSIZE

/** DEFINES */
#define DECSIZE 16
#define DECUNKNOWN -2
#define DECPOSNULL (-1)        /* if dec_pos == DECPOSNULL then value is 
                                  TRUE NULL (less than anything) */


/** STRUCTURES */

struct decimal                 /* the structure of an UNPACKED decimal */
      { short dec_exp;         /* the exponent */
        short dec_pos;         /* is the value "positive", flag */
        short dec_ndgts;       /* the number of valid digits in dec_dgts */
        char  dec_dgts[DECSIZE];       /* the digits, base 100 */
      };

typedef struct decimal dec_t;

/** PSEUDO FUNCTIONS */
/* declen, sig = # of significant digits, rd # digits to right of decimal,
           returns # bytes required to hold such */
#define DECLEN( sig,rd )  (( (sig) + ( (rd)&1 ) + 3 ) / 2 )
#define DECLENGTH( len )  DECLEN( PRECTOT( len ), PRECDEC( len ))
#define DECPREC( size )   (( size - 1 ) << 9 ) + 2 )
#define PRECTOT( len )  ( (( len ) >> 8 ) & 0xff )
#define PRECDEC( len )  ( (( len ) & 0xff )
#define PRECMAKE( len,dlen )  (( (len) << 8 ) + (dlen) )

/*
** value of an integer that generates a decimal flagged DECPOSNULL
**     an int of 2 bytes produces 0x8000
**     an int of 4 bytes produces 0x80000000
*/

                              /* v --------cp1-v */
#define VAL_DECPOSNULL(type)	((unsigned long)1L << ((sizeof(type)*8) - 1))

/** FUNCTION DECLARATIONS */

#endif
