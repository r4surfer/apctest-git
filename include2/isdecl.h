/* copyright (c) BYTE DESIGNS ltd - 1988-1993
----------------------------------------------------------------------
disam library funtions - full forward declarations
----------------------------------------------------------------------
*/

static char isdecl_SccsId[] = "@(#) isdecl.h  Version 3.21  05/15/95";

int	isaddindex( int, struct keydesc * );
int	isaudit( int, char *, int );
int	isbuild( char *, int, struct keydesc *, int );
int	isclose( int );
int	isdelcurr( int );
int	isdelete( int, char * );
int	isdelindex( int, struct keydesc * );
int	isdelrec( int, long );
int	iserase( char * );
int	isflush( int );
int	isindexinfo( int, struct keydesc *, int );
int	islock( int );
int	isopen( char *, int );
int	isread( int, char *, int );
int	isrelcurr( int );
int	isrelease( int );
int	isrelrec( int, long );
int	isrename( char *, char * );
int	isrewcurr( int, char * );
int	isrewrec( int, long, char * );
int	isrewrite( int, char * );
char *	isseekey( int );
int	issetunique( int, long );
int	isstart( int, struct keydesc *, int, char *, int );
int	isuniqueid( int, long * );
int	isunlock( int );
int	iswrcurr( int, char * );
int	iswrite( int, char * );
int	isdata( int, char *, long );
int	isgoto( int, long );
int	isindex( int, int, int );
int 	ispop( int, int, long );
int	iscopy( int, int, char *, int );
int	isgetmask( int );
int	issetmask( int, int );
int	iscluster( int, struct keydesc * );
char *  istempname( char * );

int	ldchar( char *, int, char * );
int	ldint( char * );
int	ldmint( char * );
long	ldlong( char * );
long	ldmlong( char * );
double	ldfloat( char * );	/* cp2 */
double	ldfltnull( char *, short * );
double	lddbl( char * );
double	lddblnull( char *, short * );

int	stchar( char *, char *, int );
int	stint( int, char * );
int	stmint( int, char * );
int	stlong( long, char * );
int	stmlong( long, char * );
int	stfloat( double, char * );
int	stfltnull( double, char *, short * );
int	stdbl( double, char * );
int	stdblnull( double, char *, short * );
