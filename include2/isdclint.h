/* copyright (c) BYTE DESIGNS ltd - 1988-1993
----------------------------------------------------------------------
disam internal prototypes
----------------------------------------------------------------------
*/

static char isdclint_SccsId[] = "@(#) isdclint.h  Version 3.21  05/15/95";

/* isassist.c */
	int	iSAentry( int, int );
	int	iSAmod( int, int );
	int	iSAexit();
	int	iSAfailed();
	int	iSAcurr( U32 );
	int	iSAhere( char * );
	int	iSApushprim();
	int	iSApopprim();
	int	iSAlocate( int );
	int	iSAwcheck( char * );
	int	iSAmcheck( char * );
	int	iSAdcheck();
	int	iSAfix();
static	int	save();

/* isaudit.c */
	int	iSAUrew( char *, U32 );
	int	iSAUwr( char *, U32 );
	int	iSAUdel( U32 );
static	int	aud_chk();
static	int	rd_aud();
static	int	wr_aud();
static	int	do_aud( char *, char *, U32 );

/* isbuild.c */
static	int	addkey( int, struct keydesc *, int );
static	int	addnul( int );
static	int	deleted( U32 );
static	int	namelen( char * );
static	int	delfree( U32 );
static	int	deldefn( int );
static	int	unopen();

/* isdebug.c */
#ifdef ISDEBUG
	int	iSnote( char * );
	int	isdbug( char * );
	int	isdebun( int );
	int	isdbbad( int, char *, int );
static	int	check( int );
	int	isrptkey( struct keydesc * );
	int	isrptrec( char * );
	int	isrpt( char * );
	int	isrptfile( int );
	int	isrptmap( int );
	int	iserrout( int, char *, int );
#endif

/* isdelete.c */
	int	iSidelete();

/* isgetblk.c */
	U32	iSdget();
	int	iSdfree( U32 );
	U32	iSiget();
	int	iSifree( U32 );
	int	iSiinit( char *, int, int );
static	int	dexpand();
static	int	iexpand( U32 );

/* isinfo.c */
static	U32	count();

/* isinsert.c */
	int	iSinsert( char *, U32, long );

/* iskeys.c */
	int	iSKload( U32 );
	int	iSKpop();
	U32	iSKinfo();
	int	iSKmake( char * );
	int	iSKsee( char * );
	int	iSKcopy( char *, char * );
	int	iSKcomp( U8 *, U8 * );
	int	iSK2comp( U8 *, U8 * );
	int	iSKlcmp( char *, char *, int );
	int	iSKdcmp( char *, long, char *, long );
	int	iSKgoodkey( struct keydesc *, int, int );
	int	iSKmeasure( char * );
	int	iSKmcmp( U8 *, U8 *, int );
	int	iSKfirst();
	int	iSKlast();
	int	iSKnext();
	int	iSKprev();
	int	iSKnto( U8 *, int );
	int	iSKncmp( U8 *, int );
	int	iSKinit();
static	int	typlen( int );
static	int	tokey( char **, char **, int, int );
static	int	frkey( char **, char **, int, int );
static	int	keycomp( char *, char *, int, int );
	int	iSKfixkey();
static	int	fixkey();
static	int	approx( char * );
	int	dsppath();
	int	iSKdisplay( char *, int, FILE * );

/* iskmod.c */
	int	iSKins( char *, U32, long );
	int	iSKcat( char *, U32, long );
	int	iSKfixmom( int );
	int	iSKgreater();
	int	iSKdel();
	int	iSKsplit();
static	int	makekey( char *, char * );
static	int	extract( char *, char *, char *, int );
static	int	safecopy( char *, char *, int );

/* islink.c */
#ifdef ISLINK
	char *	islink( char * );
#endif

/* islock.c */
	int	iSLdget( U32, int );
	int	iSLiget();
	int	iSLiwget();
	int	iSLdfree( U32 );
	int	iSLifree();
	int	iSLdgall();
	int	iSLdall();
	int	iSLdcheck( U32 );
	int	iSLeget( int );
	int	iSLefree( int );
	int	iSLfget( int );
	int	iSLffree();

/* ismachin.c */
	int	iSMlock( int, U32, U32, int );
	int	iSMfree( int, U32, U32 );

/* isopen.c */
	struct keydesc *iSOgetkd( struct dictinfo *, int, U32 );
static	struct keydesc *getblank();

static	U32	isseecurr( int );

/* isrw.c */
	int	iSiread( int, char *, U32 );
	int	iSiwrite( int, char *, U32 );
	int	NDXread( char *, int, U32 );
	int	NDXwrite( char *, int, U32 );
	int	iSiflush( struct id_struct * );
	int	iSiaflush();
	int	iSiclear( struct id_struct * );
	int	iSdread( int, char *, U32 );
	int	iSdwrite( int, char *, U32 );
#ifdef IsNBUFS
static	struct is_bufstruct *get_active( U32 );
static	struct is_bufstruct *get_mpty( U32 );
static	int	to_mru();
static	int	id_unlink( struct id_struct * );
static	int	bufinit();
static	int	push_blk( struct is_bufstruct * );
static	struct is_bufstruct *pop_blk();
#endif

/* issearch.c */
	int	iSsearch( char *, int );
	int	iSfirst();
	int	iSlast();
	int	iSprev();
	int	iSnext();
	int	iSrekey( int, int );
	int	iSreset();
	int	iSassure();
	int	iSestablish( int );
	int	iSreestab( int );
	int	iSlocate( int );
	int	iSrepath( char *, int, int );
static	int	subsearch( char * );
static	int	searchkey( char * );
static	int	man_up( char * );
static	int	inhere( char * );
static	U32	transno();

/* isstart.c */
static	int	confirm( struct dictinfo *, struct Mkeydesc *, int );
static	int	empty();

/* isvirt.c */
	int	isVopen( char *, int, int );
	int	isVdatfd( struct dictinfo * );
	int	isVidxfd( struct dictinfo * );
static	int	Vreopen( struct dictinfo * );
static	int	Vclose();

/* isxtra.c */
	int	isXopen( char *, int, int );
	long	ldDup( U8 * );
	int	stDup( long, U8 * );
#ifdef ISBERKELY
	int	memcpy( register char *, register char *, register int );
	int	memcmp( register char *, register char *, register int );
	int	memset( register char *, int, register int );
#endif

/* ispath.c */
	int	iSPpop();
	int	iSPunpop();
	int	iSPfree();
