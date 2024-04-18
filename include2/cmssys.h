/*****************************************************************************
                 C A E L U S   P R O G R A M   M O D U L E
                      (c)Copyright 1997 CAELUS, Inc.
                   A L L   R I G H T S   R E S E R V E D
==============================================================================
                   M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------

	cmssys - Include this Header File for CMS Wisp Stuff.

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
        12-11-97        DXL     Original.
*****************************************************************************/

#ifndef CMS_SYSTEM_SUBS

#include <pwd.h>
#include <math.h>

#if defined(AIX) || defined(ALPHA)
#include <sys/time.h>
#endif
#include <time.h>

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <termios.h>
#include <varargs.h>
#include <sys/stat.h>
#include <sys/wait.h>

#ifndef FIND_C
#include <dirent.h>
#endif

#include <errno.h>

#include "wvsb.h"
#include "wfname.h"

#ifndef EXTERN
#define EXTERN extern
#endif

EXTERN int Call_args;
EXTERN char **environ;
EXTERN char slash;
EXTERN char cmspath[100];			/* Path to LGMAP ... 	*/
EXTERN char userpath[100];			/* Path to LGMAP ... 	*/
EXTERN char SysFile[100];			/* Path to cmssys.ini 	*/
EXTERN char LiveFile[100];			/* Path to the active copy of cmsusr.ini */
EXTERN char ProgramName[10];			/* Application's Name 	*/
EXTERN char cmsinstance[10];			/* Unique ID for each Logon */
EXTERN char vwang_type_ahead_buffer[512];       /* Used by ReadKeyBoard */
EXTERN struct timeval starttime;		/* Time Application Started */

void call_resetterm();
void call_fixterm();
void CopyUserProfile();                         /* Create LiveFile          */
void GetLGMAP( char *, char *, S32 );
int  GetPrivateProfileString( char*, char*, char*, char*, S32, char *);
void GetSystemCons( char *, char *, char *, S32 );
void GetSystemCons32 ( char *, char *, S32 *);
void GetUsageCons ( char *, char *, char *, S32 );
void GetUsageConS32 ( char *, char *, S32 *);
void makepath( char * );
void makeToken( char * );
int parseKey( char *, char *, char *, char *, S32 , char * );
int parseSection( char *, char *, char *, char * );
void PutUsageCons (char *, char *, char *);
void PutUsageConS32 ( char *, char *, S32 *);
void setretcode( char * );
void str2lo ( char *, char *, int);		/* make int chars lower case */
void str2up ( char *, char *, int);          	/* make int chars upper case */
void strb2c ( char *, int );			/* convert basic string to c */
void strc2b ( char *, int );			/* convert c string to basic */
int  va_count();
void vwang (char *, char *, char *, char *, char *, char *);
int  wb_err( char * );
void wbw_errlog( char * );
void wdelwrk( char * );
void wfname( S32 *, char *, char *, char *, char * );
int  WritePrivateProfileString( char*, char*, char*, char*);
void wvaset( int * );

#define CMS_SYSTEM_SUBS

#ifdef AIX
#define X_OK X_ACC
#define F_OK E_ACC
#endif

#endif
