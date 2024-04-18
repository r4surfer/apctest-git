/*****************************************************************************
                   C A E L U S   I N C L U D E   M O D U L E
                    	(c)Copyright 1991 CAELUS, Inc.
                     A L L   R I G H T S   R E S E R V E D
==============================================================================
                      M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------
	File and I/O definitions

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
	1991		unknown Started
	03/19/92	MDH	Added header and definitions of EXTERN to 
 				eliminate need for exact copy in file0.h
	07/8/92		MDH	Added declaration of SPCLINPT as XSPCLIN
	01/21/94	MDH	Added declaration of XNATIVE and XFLATAS for
				FAINPUT - the open mode for native files

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/
static char file_SccsId[] = "@(#)include file.h  Version 5.1  09/16/94";

#ifndef EXTERN                   /* if not defined make it external var's */
#  define EXTERN extern
#endif

#define XCONSEC		0x1		/* consecutive file */
#define XNATIVE		0x9		/* Flat ASCII file */
#define XINDEXED	0x2		/* INDEXED file */
#define XPRINTER	0x4		/* printer file */
#define XTAPE   	0x8		/* tape file */
#define XVAR		0x10		/* VARiable length record w/o comp */
#define XVARC		0x20		/* VARiable length record w comp */
#define XIL		0x40
#define XNL		0x80
#define XAL		0x100

#define XINPUT		0x1
#define XIO		0x2
#define XSHARED		0x4
#define XEXTEND		0x8
#define XOUTPUT		0x10
#define XSPCLIN		0x11
#define XFLATAS		0x20

#define XIOERR		0x1
#define XEOD		0x2
#define XTIMEO		0x3

#define INT4		long
#define MAX_ALT		16		/* max alternate keys */
#define MAXFILE		64		/* max file opened */

/* User File Block for SELECT */
EXTERN struct ufb {
	long	typ;			/* file type structure */
	long    iogo;			/* IOERR GOTO xxxx */
	long    eodgo;			/* DATA GOTO xxxx  */
	long imask;			/* mask statement here */
					/* == -1 -> no mask is used */
					/* otherwise coresponding key used */
	long dup;			/* duppilcated alt key */
					/* == -1 -> no dup alt key  is used */
					/* == 0 -> no dup alt key  is used */
					/* suggest == 0 -> no dup alt key */
					/* otherwise coresponding key used */
	long  keypath;			/* current keypath number */
	long  curr_recno;		/* current record number */
	long  curr_recsize;		/* record size of last record */
	long  curr_ioerr;		/* current I/O error code (iserrno) */
	char prname[12];		/* prname in SELECT */
	char 	*record;		/* init == 0L, set it when allocated */
					/* by malloc, when being opened */	
					/* free the space when being closed */
	int	fd;			/* file descriptor */
					/* == -1 --> ufb not in use */
	int	file_no;		/* channel number in WANG BASIC */
					/* == -1 --> channel not selected */
	int	filetype;		/* file type */
					/* == CONSEC,INDEXED,PRINTER,TAPE */
					/* VAR,VARC AL,IL,NL */
	int	recsize;		/* max of var, or fixed size */
	int	keypos;			/* primary key pos. start at 1 */
					/* used on INDEXED file only */
	int	keylen;			/* key length, used in INDEXED only */
	int     keyused;		/* primary key or alternate key */
					/* KEY function KEY(#?,xxx) */
	int     sizeused;		/* SIZE (#?) */
	int     opmode;			/* open mode */
	int     iotyp;			/* IOERR GOTO xxxx */
	int     eodtyp;			/* DATA GOTO xxxx  */
	struct alternate {
		int	keypos;		/* key position, used in INDEXED file */
					/* for alternate key only */
		int 	keylen;		/*  key length */
	} alt[MAX_ALT];
	char  vol[9];			/* volume name */
	char  filenam[9];		/* file name */
	char  lib[9];			/* library name */
	char  path[81];			/* maximum of 80 char */
					/* same rule as WISP */
	char  cmask[3];			/* MASK(#?) */
}	*Wufb;

EXTERN int	Filetyp;			/* file type */
EXTERN int	FileNO;				/* file ch NO*/
EXTERN int	Filerec;			/* file recsize */
EXTERN int	Errtyp;				/* IOERR, EOD */
EXTERN int	Keypos;
EXTERN int	Keylen;
EXTERN int	Recsize;
EXTERN int	Blksize;
EXTERN int	Density;
EXTERN int	Keysel;
EXTERN long	Dupval;
