/* copyright BYTE DESIGNS ltd. (c) 1988-1993
----------------------------------------------------------------------
this file is used to control the features and options of the library.
it also defines the important details of the target operating system.

this is the generic version and is appropriate for most unix targets
by default.  if your intended target is different you will have to
adjust the definitions manually or refer to the documentation in the
port directory.

IF ANYTHING IS CHANGED THE ENTIRE LIBRARY MUST BE RECOMPILED.
----------------------------------------------------------------------
*/

static char isport_SccsId[] = "@(#) isport.h  Version 3.21  05/15/95";

/* FUNCTION PROTOTYPING - FORWARD DECLARATIONS
   two header files of prototypes are included in the source.  the
   library functions are declared in isdecl.h and the internal routines
   in isdclint.h.  some compilers will object to the way the prototypes
   are laid out owing to various inconsistencies in the implementation
   of the ANSI standard.  if this occurs you should comment out ISDECL.
   */

#define ISDECL			/* uncomment to include */


/* DATA TYPE DEFINITION
   in order to assure compatibility with not only the ansi standard
   but as many compilers as possible, the following type definitions
   must be provided.
   NB: some compilers do not recognise the signed keyword - all types
   are signed by default.  if this is the case you can safely remove
   the signed keyword.  this is true particularly of sun compilers */

typedef signed char	S8;		/* 8 bit signed */
typedef unsigned char	U8;		/* 8 bit unsigned */
typedef signed short	S16;		/* 16 bit signed */
typedef unsigned short	U16;		/* 16 bit unsigned */

#if defined(ULTRIX) || defined(ALPHA)
typedef signed int	S32;		/* 32 bit signed */
typedef unsigned int	U32;		/* 32 bit unsigned */
#else
typedef signed long	S32;		/* 32 bit signed */
typedef unsigned long	U32;		/* 32 bit unsigned */
#endif

/* WORD ORDERING
   uncomment the following if your processor stores integers with the
   most significant byte first.  this is usually the case with the intel
   family of microprocessors ( 80x86 ) */

#if !defined _HPUX_SOURCE && !defined AIX 
#define ISORDER         /* sco xenix, dos, etc */
#endif


/* OPERATING SYSTEM
   select ONE of the following */

#define ISUNIX   		/* UNIX, XENIX, AIX, etc. */
/* #define ISBERKLEY		/* BERKLEY unix */
/* #define ISDOS    		/* MS or PC DOS operating system */


/* COMPILER SPECIFICS
   in some cases it is necessary to provide special code to take ad-
   vantage of the features of a particular compiler.  if you compiler
   appears in the following list you should uncomment the associated
   define */

/* #define ISBORLAND		/* borland turbo C compilers */


/* LOCKING MECHANISM
   select ONE of the following definitions IF locking is required.
   you should also uncomment LONGLOCK if the lock function's length
   argument is of type long (32 bits). */

#define LOCKFCTN lockf  	/* if you have lockf() ( most systems ) */
/* #define LOCKFCTN locking	/* if you have locking() ( microsoft ) */
/* #define LOCKFCTN borlock	/* for borland turbo C */

#define LONGLOCK		/* prevalent on most systems */


/* INDEX BLOCKING SIZE
   D-ISAM will build files with the block size set here, and will read
   files with this block size or smaller, but not larger.  a large block
   size produces faster results, but costs more memory per open file and
   produces a larger initial index file.  Normally 512 or 1024 */

#define IsMAXBLK 1024

/* DUPLICATE KEY WIDTH
   D-ISAM normally stores the duplicate number ( which identifies the
   cronological order in which the duplicates were written ) in a short
   integer field.  You now have the option to make this a long integer
   for situations where a very large number of an individual duplicate
   are expected.  D-ISAM will dynamically adjust to suit the configured
   width of the file on open, this define controls the width with which
   new files are built.

   NOTE: a value of 4 is required for compatibility with C-ISAM version
         4 or greater */

#define IsDUPWIDTH 2		/* either 2 or 4 */


/* INTERNAL BUFFERING
   buffering is now only enabled if this macro is defined.  most newer
   operating systems are capable of providing sufficient disk caching
   to make this ALMOST redundant.  buffers are essentially a trade off
   between speed and memory.  you can adjust the number to suit your
   requirements.  each buffer costs just over one IsMAXBLK bytes. */

#define IsNBUFS 20			/* maximum buffers available */


/* ACCESS LIMITATION FEATURE
   this provides control over the number of times any file within the
   locking precinct can be opened.  this, in turn, provides a means of
   limiting the number of concurrent processes ( provided they all use
   a common file ) allowed on the system.  useful for enforcing license
   agreements or for controlling system loading.  leave undefined to
   disable, or set to the limit required. */

/* #define ISACCESS 3			/* uncomment to activate */


/* VIRTUAL FILE SYSTEM
   this system permits the isam to open more files than is permitted by
   your operating system.  it accomplishes this by closing inactive files
   to make room for the new ones and then reopening them when they are
   needed.  this file handle paging system requires some overhead.  the
   sizeof the dictionary structure for each file is increased by two longs
   and one int.  each reference to a file handle must first be checked for
   validity and reopened if necessary. */

#define ISVIRTUAL		/* uncomment to activate */


/* FILE HANDLE LIMITATION
   used in conjunction with the virtual file system, this feature allows
   control over the number of system file handles disam is allowed to use
   before virtual swapping takes over.  this forces disam to leave room
   for the handles required by your application. */

/*  #define ISHANDLES 60		/* uncomment to activate */


/* FILE LINKS
   if a file named <isamfile>.lnk is present isam will read the first
   line of the file and use that in place of the file specified in the
   isopen call */

/* #define ISLINK			/* uncomment to activate */


/* STATUS VARIABLES
   we have implemented only the first level of the isstat1 and isstat2
   variables.  this means that, after a succesful read, isstat1 will
   equal 0 and isstat2 will be set to 2 if the key following is a dup-
   licate of the current */

/* #define ISSTAT			/* uncomment to activate */


/* TRANSACTION PROCESSING
   the transaction processing feature, which provides the functions
   islogopen, islogclose, isbegin, iscommit, isrollback and isrecover,
   can only be activated if you have the optional source files included
   in your source directory.
   */

/* #define ISTRANS 4			/* uncomment to activate */


/* WRITE AND LOCK
   additional calls to permit locking a record automatically on write
   ( iswrlock ) and rewrite ( isrewlock ).  only active if file is open
   for manual locking */

/* #define ISWRLOCK			/* uncomment to activate */


/* PURE INDEXES
   with this option active, building a datafile with the record
   length set to zero will result in an isam file consisting of
   only the index.  only the key fields of records written to a
   pure index will stored and only the information that makes up
   the current key will be loaded on reading. */

/* #define ISPURE			/* uncomment to activate */


/* MAXIMUMS
   set the following values as required. */

#define MAXFDS 100		/* maximum number of open files. */
#define MAXSUBS 16		/* maximum number of keys per isam file .*/
#define NPARTS 8		/* number of key parts per key. */
#define MAXKEYLEN 150		/* maximum length of complete key */
#define MAXLOCKS 20		/* maximum number of manual locked records 
				   per opened file. */


/* MISCELLANEOUS */

/* select whether ISBUILD will create a new (EMPTY) file on top of an
   existing file, or return EEXISTS if filename already in use */

#define ISEXIST			/* return error if build exisiting */


/* define the octal file creation mask for isbuild() files. */

#define	ISMODE 0666		/* full access */


/* DOS ONLY: select which order your rename is -- it seems to be
   inconsistantly implemented */

#define rname( x,y ) rename( x,y )	/* the normal order */
/* #define rname( x,y ) rename( y,x )	/* xenix x-compile order */


/* uncomment the following line if you wish access to DEBUGGING facilities */
/* #define ISDEBUG			/* see PORTING(I) for usage details */

/* uncomment the following line if you want EXTENDED ERROR CODES */
#define EXTENDED		/* if so, you will get further information on
				   EBADFILE errors, useful when debugging */

