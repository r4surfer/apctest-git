/* copyright BYTE DESIGNS LTD. (c) 1988-1993
-------------------------------------------------------------------------------
D-ISAM 3.2.1 version

this header file is normally included in any module which uses D-ISAM
porting definitions have been moved to isport.h
-------------------------------------------------------------------------------
To our past project leader Bruce Fast
and to our current project leader James Knight: 
"Companies don't make people, it's the people that make a company."
Byte Designs thanks you, Heinz Wittenbecher, President. 
-------------------------------------------------------------------------------
*/

static char disam_SccsId[] = "@(#) disam.h  Version 3.21  05/15/95";

#ifndef ISAM_INCL		/* allow multiple calls to disam.h */
#define ISAM_INCL

#include "isport.h"		/* porting details */

#ifdef ISAMMAIN			/* global variable control */
#define STATUS
#else
#define STATUS extern
#endif

STATUS U32 isrecnum;		/* record number of current record */
STATUS int iserrno;		/* on err, value of error is returned here */
STATUS int iserrio;		/* on return, contains value of most recent
				   isam function called; see FC_ defines */

#ifdef ISSTAT			/* extended status reporting, see isport */
STATUS int isstat1;
STATUS int isstat2;
#endif


#ifndef ISAMMAIN

extern int issingleuser;       /* if set true, then all locking is abandoned */
extern char *iscopyright;      /* our copyright notice */
extern char *isserial;         /* the program's serial number */
extern char *isversnumber;     /* the program's version number */
extern int is_nerr;            /* the highest isam error code */
extern char *is_errlist[];     /* names of the errors */
extern int IsBLOCK;            /* dynamically set to present block size */

#else

#ifdef LOCKFCTN
int issingleuser = 0;		/* default to multiuser */
#else
int issingleuser = 1;		/* locking abandoned */
#endif

char *iscopyright = "copyright BYTE DESIGNS ltd. 1988-1993";
char *isserial = "";
char *isversnumber = "V.3.2.1";
int is_nerr = 150;
int IsBLOCK = IsMAXBLK;
char *is_errlist[] =     /* less terse descriptions can be inserted here */
       { "EDUPL",      /* error 100 */
         "ENOTOPEN",
         "EBADARG",
         "EBADKEY",
         "ETOOMANY",
         "EBADFILE",
         "ENOTEXCL",
         "ELOCKED",
         "EKEXISTS",
         "EPRIMKEY",
         "EENDFILE",
         "ENOREC",
         "ENOCURR",
         "EFLOCKED",
         "EFNAME",
         "ENOLOK",
         "EBADMEM",
	 "EBADCOLL",
	 "EUSER",
	 "EVIRTUAL" };

#endif 

struct keypart                 
       { short kp_start;       /* offset to key part */
         short kp_leng;        /* # of bytes in keypart */
         short kp_type;        /* processing directions for keypart */
       };

#define k_start k_part[0].kp_start
#define k_leng  k_part[0].kp_leng
#define k_type  k_part[0].kp_type
       
struct keydesc
       { short k_flags;			/* key characteristics */
         short k_nparts;		/* number of parts in key */
         struct keypart k_part[NPARTS];	/* description of each part */
					/* the rest are INTERNAL */
         short k_len;			/* length of key */
         U32 k_rootnode;		/* record number of root node */
       };

STATUS struct dictinfo
       { short di_nkeys;	/* # of keys */
         short di_recsize;	/* # of bytes in a data record */
         short di_idxsize;	/* # of bytes in an index block */
         U32   di_nrecords;	/* # of records - valid after isindexinfo */

         int   di_datfd;	/* open() file descriptor of the .dat file */
         int   di_idxfd;	/* open() file descriptor of the .idx file */
         U32   di_datrecd;	/* which data record is "CURRENT" 0 if none */
         short di_actvkey;	/* which key is the active key */
         short di_openmode;	/* the type of open which was used */
         char *di_map;		/* map of autoconversions (allocated) */
         char *di_name;		/* the name given to the file at open time */
         char *di_pad;		/* allocated key description */
         char  di_disjoint;	/* if set, then CURR & NEXT give same result */
         U32   di_locrec;	/* relocation record */
         char *di_lockey;	/* the associated key */ 
         long  di_locdup;	/* relocation duplicate # */
         int   di_locidx;	/* the associated index */       
         int   di_noprim;	/* if( di_noprim ) then has null primary key */
	 char  di_dupw;		/* duplicate number width */
         struct keydesc *di_desc[MAXSUBS]; /* pointer to key descr. info */
#ifdef ISVIRTUAL
	U32	di_vfreq;	/* count of times reopened */ 
#endif
#ifdef ISDOS
	U16	di_dospid;	/* dos process id analog - see isdospid.c */
#endif
#ifdef ISTRANS
	int	di_txnpid;	/* recovery process id */
	int	di_txnfile;	/* recovery file number */
#endif
#ifdef ISFILTER
	int	(*di_filter)();	/* pointer to filter routine */
#endif
#ifdef ISMASK
	U16	di_mask;	/* sixteen bit mask flag */
#endif
       } *isfdmap[ MAXFDS ];/* info can be accessed as per:
				   isfdmap[ isfd ]->xxx,
				   if file not open, isfdmap[ isfd ] = (NULL) */ 

/******************************************
             USER CONSTANTS
*******************************************/

#define SUCCESS 0
#define WHOLE 0

/** OPENING **/
#define ISINPUT        0               /* open read only */
#define ISOUTPUT       1               /* open write only */
#define ISINOUT        2               /* open with read & write permission */

/** LOCKING **/
#define ISAUTOLOCK     0x200           /* record locking automatic */
#define ISMANULOCK     0x400           /* manual locking */
#define ISEXCLLOCK     0x800           /* full file lock */
#define ISLOCK         0x100           /* lock on read request */      
#define ISWAIT         0x400           /* wait until record free */
#define ISLCKW         0x500           /* wait for and lock */

/** KEY DEFINING OPTIONS **/
#define ISNODUPS       0               /* duplicates not allowed */
#define ISDUPS         1               /* duplicate keys permitted */
#define DCOMPRESS      2               /* compress duplicates */
#define LCOMPRESS      4               /* leading redundancy compress */
#define TCOMPRESS      8               /* trailing constant compress */
#define TNULL          0x10            /* use null as trailing constant */
#define COMPRESS ( LCOMPRESS + DCOMPRESS + TCOMPRESS )

/** KEY PART TYPES **/

#define ISDESC         0x80            /* use descending order, flag */

#define CHARTYPE       0               
#define CHARSIZE       1

#define INTTYPE        1
#define INTSIZE        2

#define LONGTYPE       2
#define LONGSIZE       4

#define DOUBLETYPE     3
#define DOUBLESIZE     sizeof( double )

#define FLOATTYPE      4
#define FLOATSIZE      sizeof( float )

#define MINTTYPE       5
#define MINTSIZE       sizeof( short )

#define MLONGTYPE      6
#define MLONGSIZE      sizeof( long )

#define STRINGTYPE     7
#define STRINGSIZE     1

/** READING OPTIONS **/
#define ISFIRST        0               /* find logical first record */
#define ISLAST         1               /* find logical last record */
#define ISNEXT         2               /* find logical next record */
#define ISPREV         3               /* find logical previous record */
#define ISCURR         4               /* find "current" record */
#define ISEQUAL        5               /* find exact key match */
#define ISGREAT        6               /* find nearest greater than key */
#define ISGTEQ         7               /* find exact or nearest key */

/** AUDIT TRAIL INFO **/

#define AUDSETNAME     0               /* DEFINES for isaudit() */
#define AUDGETNAME     1
#define AUDSTART       2
#define AUDSTOP        3
#define AUDINFO        4


/****************************************************************************
                    F U N C T I O N   P R O T O T Y P E S
*****************************************************************************/

#ifndef INTERNAL

#ifdef ISDECL

#include "isdecl.h"		/* library function declarations */

#else	/* ISDECL */

long ldlong();
long ldmlong();
double ldfloat();	/* cp2 */
double ldfltnull();
double lddbl();
double lddblnull();
char *isseekey();
unsigned long isseecurr();

#endif	/* ISDECL */

#endif	/* INTERNAL */


/****************************************************************************
                         E R R O R    C O D E S
*****************************************************************************/

/** ERROR MNEMONICS **/
#define EDUPL          100             /* duplicate record */
#define ENOTOPEN       101             /* file not open */
#define EBADARG        102             /* invalid argument */
#define EBADKEY        103             /* invalid key description */
#define ETOOMANY       104             /* out of file descriptors */
#define EBADFILE       105             /* invalid isam file format */
#define ENOTEXCL       106             /* exclusive lock required */
#define ELOCKED        107             /* record claimed by another */
#define EKEXISTS       108             /* key already exists */
#define EPRIMKEY       109             /* primary key may not be used */
#define EENDFILE       110             /* beginning or end of file reached */
#define ENOREC         111             /* no match was found */
#define ENOCURR        112             /* there is no "current" established */
#define EFLOCKED       113             /* entire file locked by another */
#define EFNAME         114             /* file name too long */
#define ENOLOK         115             /* cannot create lock file */
#define EBADMEM        116             /* memory allocation request failed */
#define EBADCOLL       117             /* bad custom collating */
#define EUSER          129             /* too many users */
#define EVIRTUAL       140             /* unable to reopen virtual file */

/***************************************************************
POTENTIAL BUG ERRORS, SET TO EBADFILE UNLESS #EXTENDED SET 
****************************************************************/

#ifdef EXTENDED
#define BAD_REQ 201
#define TOO_FAR 202
#define NO_INFO 203
#define NOT_SET 204
#define BAD_INFO 205
#define BUG_CHECK 250

#else
#define BAD_REQ 105
#define TOO_FAR 105
#define NO_INFO 105
#define NOT_SET 105
#define BAD_INFO 105
#define BUG_CHECK 105
#endif
 
/******************************************************************************
                  F U N C T I O N   M N E M O N I C S 
******************************************************************************/

/* these values are found in iserrio after the return of the associated 
   function call */

#define FC_DELCURR     1
#define FC_DELETE      2
#define FC_READ        3
#define FC_REWCURR     4
#define FC_REWRITE     5
#define FC_WRCURR      6
#define FC_WRITE       7
#define FC_START       8
#define FC_OPEN        9
#define FC_DELREC      11
#define FC_REWREC      12
#define FC_ADDIND      13
#define FC_LOCK        14
#define FC_DELIND      15
#define FC_REL         16
#define FC_CONTROL	17
#define FC_CLUSTER	18

#endif
