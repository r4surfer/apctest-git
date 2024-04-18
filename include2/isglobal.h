/* copyright BYTE DESIGNS LTD. (c) 1988-1993
------------------------------------------------------------------------------
this header holds all internal global variables, structures and defines.
------------------------------------------------------------------------------
*/

static char isglobal_SccsId[] = "@(#) isglobal.h  Version 3.21  05/15/95";

#include <stdio.h>
#include <errno.h>

/* global storage control */
#ifdef ISAMMAIN
#define extern
#endif


/* flag library compile mode */

#define INTERNAL

#include <disam.h>


/* general access info & #defines */

#define salloc( x )		( struct x * )iSmalloc( sizeof( struct x ) )
#define smove( x, y, z )	( iSmove( y, z, sizeof( struct x ) ) )


/* INFO FOR KEYS */

struct path                    
  {
  struct path *p_up;	/* the parent map or 0 if top */ 
  int p_level;		/* tree level, 0 = data access */
  U32 p_recd;		/* the record # @ this level */
  U8 *p_key;		/* ptr to key relative to iSpad */
  U8 *p_kdsc;		/* ptr to the key description */
  long p_kn;		/* which duplicate is it ( # dups so far ) */
  U8 p_fkd[1];		/* full key description */
  };			/* the map of the path to the present iSpad */ 

/* always allocate iSpath items with ISPATHSIZE */
#define ISPATHSIZE	( sizeof( struct path ) + iSkey->k_len )
#define PATHALLOC	( ( struct path *)iSmalloc( ISPATHSIZE ) )

#define iSlevel ( iSpath->p_level )
#define iSrecd  ( iSpath->p_recd )


struct Mkeydesc			/* modified key description */
  {				/**** starts IDENTICAL to keydesc *****/
  short k_flags;		/* key characteristics */
  short k_nparts;		/* number of parts in key */
  struct keypart k_part[NPARTS];/* description of each part */
  short k_len;			/* length of key */
  U32  k_rootnode;		/* which record is root */
  				/**** extension ****/
  short k_alen;			/* length of key being considered */
  short k_dlen;			/* max length of key + data */
  short k_dupnum;		/* which duplicate ( 0 = first ) */
  U32  k_trans;			/* confirmed transaction */
  int  k_endflag;		/* 1 if @ start, 2 if @ end, 
				   3 if @ deleted record else 0 */
  struct path *k_path;		/* the tree, dynamically ALLOCATED */
  struct path *k_opth;		/* old path saved - jk1 */
  };

/* the pointer in struct dictinfo (disam.h) actually points to an Mkeydesc even 
   though it states that it points to a keydesc. */

struct rootstr                 /* the organization of the index header */
       { U8    roo_valid[2];   /* validation - FE53 */
         char  roo_rfu[6];     /* info reserved for future use/compatability */
         char  roo_nkeys[2];   /* # of keys - independant fmt. */
         char  roo_rfu2[3];
         char  roo_dlen[2];    /* data record length - independant fmt. */
         char  roo_kdsc[4];    /* address of key description */
         char  roo_rfu3[6];
         char  roo_fdata[4];   /* address of data free list */
         char  roo_findex[4];  /* address of index free list */
         char  roo_dsize[4];   /* # of records used on data file */
         char  roo_isize[4];   /* # of index records used */
         char  roo_trans[4];   /* transaction number */
         char  roo_uniq[4];    /* unique id */
         char  roo_audit[4];   /* audit trail info */
       };


extern struct lockstr 			/* maintenance of locking info */
       { int  ls_excl; 			/* set if exclusive lock is on */
         int  ls_index; 		/* set if index is locked */
         U32 ls_list[ MAXLOCKS ]; 	/* list of locked data records */
         int  ls_llen; 			/* # of entries valid in ls_list */
       } *isfdlock[ MAXFDS ];


/* LOCKING OFFSETS
   the default values provided permit locking compatability with c-isam */

#define DATAOFFS	0x40000000L
#define FILEOFFS	0x40000000L
#define KEYOFFS		0x3FF00000L

#ifdef LONGLOCK
#  define DATAALL	0xFFFFFFFL
#  define KEYALL	0x100000L
#else
#  define DATAALL	0xFFFFL
#  define KEYALL	0xFFFFL
#endif


#define TRANSNO	41L
#define UNIQUE	45L
#define AUDIT	49L
#define DOSPID	100L

#ifdef IsNBUFS

/* The following structures and variables are used by the intelligent 
   disk buffering system. */

extern struct is_bufstruct                   /* individual buffers */
      { struct is_bufstruct *next;             /* list, only maintained when
                                                  attatched to id_struct */
        char wrflag;                           /* set if info different than
                                                  file */
        U32 recno;                            /* the record number */
        char buf[IsMAXBLK];
      } iSbuflist[IsNBUFS];

extern struct is_bufstruct *iSbufstak[IsNBUFS];/* stack of unused buffers */
extern int is_buftos;                          /* top of stack */

#endif /* IsNBUFS */

extern struct id_struct			/* one of these per file descriptor */
      { struct id_struct *lru, *mru;	/* less & more recently used */
        char wrflag;			/* writes not cleaned, flag */
        struct is_bufstruct *list;	/* block number ordered list
					   of active buffers */
        int me;				/* which fd am I */
      } id_buf[MAXFDS],*id_mru,*id_lru;                        

         
/* global values - remain constant through any given key operation */

extern int iSfd;               /* file being processed */
extern struct dictinfo *iSkd;  /* dictionary being used  isfdmap[ iSfd ] */
extern int iSknum;             /* iSkd->di_actvkey */
extern struct Mkeydesc *iSkey; /* key being processed iSkd[ iSknum ] */
extern struct path *iSpath;    /* iSkey->k_path */
extern char *iSpad;            /* which pad is available (iSkd->k_pad) */
extern char *iStmp,*iStmp2,*iStmp3;    /* a key description buffer, allocated */
extern short iStsize;           /* present size of iStmp */
extern char *iS1data;           /* an allocated block big enough to hold any 
                                  opened data record */
extern char *iS2data;           /* another */
extern int iSdsize;            /* present size of iS1data */
extern char iSscratch[IsMAXBLK]; /* a buffer which holds a block */


#ifdef ISDOS

#  define getuid()	1
#  define GETPID( X )	isdospid( X )

#  ifdef ISMODE
#  undef ISMODE
#  endif

#  ifdef LOCKFCTN	

#    include <share.h>
#    include <sys/types.h>
#    include <sys/stat.h>

#    ifdef ISEXIST
#      define ISCREATE	O_RDWR | O_BINARY | SH_DENYNO | O_CREAT | O_EXCL
#    else
#      define ISCREATE	O_RDWR | O_BINARY | SH_DENYNO | O_CREAT | O_TRUNC
#    endif

#    define ISOPEN	O_RDWR | O_BINARY | SH_DENYNO
#    define ISREAD	O_RDONLY | O_BINARY | SH_DENYNO

#    define ISMODE	S_IREAD | S_IWRITE

#  else
  
#    ifdef ISEXIST
#      define ISCREATE	O_RDWR | O_BINARY | O_CREAT | O_EXCL
#    else
#      define ISCREATE	O_RDWR | O_BINARY | O_CREAT | O_TRUNC
#    endif

#    define ISOPEN	O_RDWR | O_BINARY
#    define ISREAD	O_RDONLY | O_BINARY

#    define ISMODE	0666

#  endif

#else

#  define GETPID( X )	getpid()

#  ifdef ISEXIST
#    define ISCREATE	O_RDWR | O_CREAT | O_EXCL
#  else
#    define ISCREATE	O_RDWR | O_CREAT | O_TRUNC
#  endif

#  define ISOPEN	O_RDWR
#  define ISREAD	O_RDONLY

#endif
       

/* VIRTUAL FILE HANDLE CONTROL */

#ifdef ISVIRTUAL

# define V_DATFD(vm) ( vm->di_datfd != -2 ) ? vm->di_datfd : isVdatfd(vm)
# define V_IDXFD(vm) ( vm->di_idxfd != -2 ) ? vm->di_idxfd : isVidxfd(vm)

#else /* disarm virtual files */

# define V_DATFD(vm) vm->di_datfd
# define V_IDXFD(vm) vm->di_idxfd

# define isVopen isXopen
# define isVclose close

#endif


/* DEBUG CONTROL */

#ifdef ISDEBUG			/* arm the debugging routines */

  extern char dbbuf[100];	/* temporary buffer */

# define ISDB { fprintf(stderr," %s(%d) ",isdbfn,__LINE__); fflush( stderr ); }
# define iSdb( p )	isdbug( p )
# define ret( r )	{ isdbun( (int)r ); return( r ); }
# define eret( x, r )	{ iserrno = x; isdbbad(x,isdbfn,__LINE__); ret(r); }
# define iSmalloc	iSDBmalloc
# define iSfree( PP )	iSDBfree( PP )

#else			/* define away all of the debug stuff */

# define iSdb( p )	/* */
# define ret( x )	return( x )
# define eret( x, r )	{ iserrno = x; return( r ); }
# define isdbug( x )	/* */
# define isdbun( x )	/* */
# define isdbbad()	/* */
# define isrptkey( k )	/* */
# define isrptrec( s )	/* */
# define isrpt( s )	/* */
# define iSnote(s)	/* */
# define iSmalloc	malloc
# define iSfree( PP )	free( *PP )

#endif


/* TRANSACTION PROCESSING */

#ifdef ISTRANS
#include <istrans.h>
#endif


/* DECLARATIONS */

char *iSmalloc();
U32 lseek();

int ldint();
long ldlong();

#ifdef ISDECL

# include <isdclint.h>		/* declare internal functions */

#else				/* declare internal non integer returns */

  long ldDup();
  U32 iSKinfo();
  U32 iSdget();
  U32 iSiget();
  U32 transno();
  U32 count();
  struct keydesc *iSOgetkd();
  static struct keydesc *getblank();
 
# ifdef ISLINK
    char *islink();		/* declare islink function */
# endif

#endif

#ifndef ISLINK
# define islink( x ) x	/* replace link call with it's argument */
#endif

#ifdef ISAMMAIN
# undef extern
#endif
