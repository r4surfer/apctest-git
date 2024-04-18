/*****************************************************************************
                 C A E L U S   P R O G R A M   M O D U L E
                      (c)Copyright 1991 CAELUS, Inc.
                   A L L   R I G H T S   R E S E R V E D
==============================================================================
                   M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------

	COPY - an imulation of the WANG copy utility.  This routine will
	       accept WANG style file information file, library, volume for
               the source file and for the destination file from the 
               command line or if thay are not supplied getparms will be
               issued.  wb_open is used to validate existance and access to the
               the files, and to allow the user an opportunity to remove an 
               output file that already exists.
               Once the files have been converted to UNIX paths,
               a copy is made of both the DISAM .dat and .idx files.
 
               Command line parameters are assumed to be in the followinf order:
			source file name
			source library name
			source volume name	
			destination file name
			destination library name
			destination volume name	

	NOTE - A versions numbers is displayed as text in GETPARM screen.
	       Search for Version to find & update.

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
	06/10-17/92	MDH	Original
	05/17/95        KAB     Fixed minor problems with timeout, bypass, lock
				Added cmsdefs.h for ALPHA.

*****************************************************************************/

#include <stdio.h>
#include <sys/wait.h>
#include <errno.h> 
#include <fcntl.h>
#include "cmsdefs.h"
#include "file.h"
#include "wisp.h"
#include "wfname.h"

#define MAXLINES 15   /* max num of message & text lines for getparm call */

 /* the following definitions are used by all of the getparm routines */
  char key_rec;			 /* response key identifier */
  char prname[12];               /* program name passed to getparm */
  char msgid[5];		 /* Message number displayed on screen */
  char req_type[3];              /* request identifier to getparm */
  int  n_mlines;                 /* number of message lines passed */
  int  mlsize[MAXLINES];         /* # of characters in each message line */
  char mline[MAXLINES][80];      /* message lines passed */

  char ttype[MAXLINES];          /* text field types for call to getparm */
  int  tlsize[MAXLINES];         /* # of characters in each text line */
  char tline[MAXLINES][80];      /* text lines passed */
  int  trow[MAXLINES];	         /* row value for text fields */
  char trowflg[MAXLINES];        /* row flags for text fields */
  int  tcol[MAXLINES];	         /* column value for text fields */
  char tcolflg[MAXLINES];        /* column flags for text fields */

  char ktype[MAXLINES];          /* keyword field types */
  int  klsize[MAXLINES];         /* # of characters in each keyword line */
  char kline[MAXLINES][80];      /* keyword lines passed */
  int  krow[MAXLINES];	         /* row value for keyword fields */
  char krowflg[MAXLINES];        /* row flags for keyword fields */
  int  kcol[MAXLINES];	         /* column value for keyword fields */
  char kcolflg[MAXLINES];        /* column flags for keyword fields */
  char datatype[MAXLINES];       /* data types for keyword fields */

  char pftype;	     	         /* keyword for PFkey mask */
  char  pfmask[5];	         /* PF key mask */
 /******** end of getparm defs *******/
  int Call_args;		 /* number of args used by wb_open */

  /* local routines */
  static int val_params();
  static void term_name();
  static int getp1();
  static void getp2();
  static void getp3();
  static int getp4();
  static int getp5();
  static int chk_valid();
  static incr_id();

#define PF16 'P' 		 /* value of returned key from getparm */
#define PF1 'A'

main(argc, argv)
 int  argc;
 char **argv;
{
   int  i;			/* local index */
   char *p;			/* local temp char ptr */
   char infilen[9], inlib[9], invol[7];    /* original path info */
   char outfilen[9], outlib[9], outvol[7]; /* new path info */
   int  got_param[6]; 			   /* parameter filled flags */
   char inpath[80];		/* UNIX path name of source file */
   char fullinpath[80];		/* source file path with ext */
   char	outpath[80];		/* UNIX path name of destination file */
   char	fulloutpath[80];	/* destination file path with ext */
   long fil_mode;		/* mode for D-ISAM call for pathname */
   int  pid;      		/* process id */
   union wait *status;  	/* exec process return status */
   char *strchr();		/* string search for character routine */
   int  complete;		/* copy complete flag */
   int  lock;		 	/* flag to lock file before copy */
   int  timeout;	 	/* number of seconds to wait on lock */
   int  bypass;			/* flag for bypass file if timeout expires */
   int  getp5();		/* getparm on error to see if should exit */
   char gp5str[80];		/* message to be put out by getparm 5 */
   int  resp;

   static struct symbol *allvar[]={NULL};
   int Iflag = 0;

   wb_init (&Iflag, allvar, argv[0]);	/* init for main program */
   /* blank fill name info  & set flag to no got */
   memset(infilen, ' ', 8);
   memset(outfilen, ' ', 8);
   memset(inlib, ' ', 8);
   memset(outlib, ' ', 8);
   memset(invol, ' ', 6);
   memset(outvol, ' ', 6);
   for (i = 0; i < 6; i++) got_param[i] = 0;
   
   /* check argument list to file info */
   switch (argc)
     {
      case 7:			/* destination library passed in - grab it */
          strncpy(outvol, argv[6], 6);
          outvol[6] = 0;
          got_param[5] = 1;
          /* fall through and get the rest */
      case 6:			/* destination volume passed in - grab it */
          strncpy(outlib, argv[5], 8);
          outlib[8] = 0;
          got_param[4] = 1;
          /* fall through and get the rest */
      case 5:			/* destination file passed in - grab it */
          strncpy(outfilen, argv[4], 8);
          outfilen[8] = 0;
          got_param[3] = 1;
          /* fall through and get the rest */
      case 4:			/* source library passed in - grab it */
          strncpy(invol, argv[3], 6);
          inlib[6] = 0;
          got_param[2] = 1;
          /* fall through and get the rest */
      case 3:			/* source volume passed in - grab it */
          strncpy(inlib, argv[2], 8);
          inlib[8] = 0;
          got_param[1] = 1;
          /* fall through and get the rest */
      case 2:			/* source file passed in - grab it */
          strncpy(infilen, argv[1], 8);
          infilen[8] = 0;
          got_param[0] = 1;
          break;

      default:
          ;
     }
   complete = 0;
   do		/* loop until complete */
     {    
      /* validate params &/or issue getparms */
      resp = val_params(infilen, inlib, invol, outfilen, outlib, outvol, 
	       got_param, &lock, &timeout, &bypass);
      if (resp == -1)  /* user asked to break out */
          sprintf(gp5str, " No File Copied.");
      else
        {
          /* get path to source file */
          fil_mode = IS_IO;
          wfname(&fil_mode, invol, inlib, infilen, inpath, "");
          p = strchr(inpath,' '); 
          if (p) *p = 0;     	/* ensure NULL termination */

          /* get path to destination file */
          fil_mode = IS_IO;
          wfname(&fil_mode, outvol, outlib, outfilen, outpath, "");
          p = strchr(outpath,' '); 
          if (p) *p = 0;     	/* ensure NULL termination */

          /* COPY the data file */
          strcpy(fullinpath, inpath);   /* create paths for .dat file */
          strcat(fullinpath, ".dat");
          strcpy(fulloutpath, outpath);
          strcat(fulloutpath, ".dat");

          resp = do_cp(fullinpath, fulloutpath);   /* do copy */
          if (resp != 0)
            {
             sprintf(gp5str, " Error %d : Attempting to copy %s to %s",
                         resp, fullinpath, fulloutpath);
            }
          else
            {
             /* COPY the index file */
             strcpy(fullinpath, inpath);   /* create paths for .dat file */
             strcat(fullinpath, ".idx");
             strcpy(fulloutpath, outpath);
             strcat(fulloutpath, ".idx");
      
             resp = do_cp(fullinpath, fulloutpath);
             if (resp != 0)
               {
                 sprintf(gp5str, " Error %d : Attempting to copy %s to %s",
                          resp, fullinpath, fulloutpath);
               }
             else
                sprintf(gp5str, "file %s created in library %s on volume %s",
                           outfilen, outlib, outvol);
            }
        }
      if (getp5(gp5str))
        complete = 1;
     } while (!complete);
   wb_end(0);
}

/**********************************************************************
*                                                                     *
*  VAL_PARAMS: validate parameters.  If problems do getparms          *
*                                                                     *
**********************************************************************/
static int val_params(infilen,  inlib,  invol, outfilen, outlib, outvol, 
                       got_param, lock, timeout, bypass)
  char infilen[],  inlib[],  invol[],      /* input file info */
       outfilen[], outlib[], outvol[];     /* output file info */
  int  got_param[6];     	/* command line info */
  int  *lock;		 	/* flag to lock file before copy */
  int  *timeout;	 	/* number of seconds to wait on lock */
  int  *bypass;		 	/* flag for bypass file if timeout expires */
{
  int  i;		 /* local index */
  int  got_sum;		 /* sum of got_param flags */
  int  argcnt;		 /* number of arguments passed to EXTRACT */
  int  resp;		 /* response status from getparm screens */
  char in_volume[7],	 /* user default input info */ 
       in_library[9];
  char out_volume[7],    /* user default output info */
       out_library[9];
  int getp1(),           /* initial getparm screen for input file specs */
      getp4();   	 /* output file specs getparm */
  void getp2(), 	 /* option getparm for shared mode */
       getp3();	 	 /* file organization getparm */
  void term_name();	 /* routine to trim a & terminate parameter names */
  struct ufb *pufbi, *pufbo;

   pufbi = (struct ufb *) malloc(sizeof(struct ufb));
   pufbo = (struct ufb *) malloc(sizeof(struct ufb));
   memset(pufbi, 0, sizeof(struct ufb));
   memset(pufbo, 0, sizeof(struct ufb));
   got_sum = 0;
   for (i = 0; i < 6; i++)
     got_sum += got_param[i];

   if (got_sum < 6)
     {  /* need to do getparms -- here we go ..... */
       /* get users defaults */
       argcnt = 8;
       wvaset(&argcnt);
       EXTRACT("IV", in_volume, "IL", in_library, 
               "OV", out_volume, "OL", out_library);
       if (got_param[1] == 0)
         strncpy(inlib, in_library, 8);
       if (got_param[2] == 0)
         strncpy(invol, in_volume, 6);
       if (got_param[4] == 0)
         strncpy(outlib, out_library, 8);
       if (got_param[5] == 0)
         strncpy(outvol, out_volume, 6);

       resp = getp1(infilen, inlib, invol);
       if (resp == -1)      /* user hit PF16 to end copy */
         return(-1);
     }

   /* call wb_open to force any applicable getparms */
   pufbi->filetype = XINDEXED;     /* set up dummy ufb */
   pufbi->recsize = 80;
   pufbi->keypos = 1;
   pufbi->keylen = 6;
   pufbi->fd = -1;
   strcpy(pufbi->prname, "COPY");
   resp=wb_open(pufbi, 1, 0, XINPUT, 0, 0, 0, 0, 0, infilen, inlib, invol);
   wb_close(0, pufbi);
   strncpy(infilen, pufbi->filenam, 8);    /* get name back out in case */
   strncpy(inlib, pufbi->lib, 8);          /* user changed it in open */
   strncpy(invol, pufbi->vol, 6);
   if (got_sum < 6)
     {  /* need to do getparms -- here we go ..... */
       getp2(lock, timeout, bypass);
       getp3();
       resp = getp4(outfilen, outlib, outvol);
       if (resp == -1)      /* user hit PF16 to end copy */
         return(-1);
     }
   /* call wb_open to force any applicable getparms forr output file */
   pufbo->filetype = XINDEXED;    /* set up this guys ufb */
   pufbo->recsize = 80;
   pufbo->keypos = 1;
   pufbo->keylen = 6;
   pufbo->fd = -1;
   strcpy(pufbo->prname, "COPY");
   resp=wb_open(pufbo, 1, 0, XOUTPUT, 0, 0, 0, 0, 0, outfilen, outlib, outvol);
   wb_close(0, pufbo);
   strncpy(outfilen, pufbo->filenam, 8);    /* pull name info back out in  */
   strncpy(outlib, pufbo->lib, 8);          /* case user changed it in open */
   strncpy(outvol, pufbo->vol, 6);          /* getparm */

   term_name(infilen, 8);		    /* clean up all info */
   term_name(invol, 6);
   term_name(inlib, 8);
   term_name(outfilen, 8);
   term_name(outvol, 6);
   term_name(outlib, 8);
   if (pufbi != NULL) free(pufbi);          /* free pufb space */
   if (pufbo != NULL) free(pufbo);
   return(1);
}

/**********************************************************************
*                                                                     *
*  TERM_NAME: strip trailing blanks and ensure null terminated        *
*                                                                     *
**********************************************************************/
static void term_name(name, size)
 char *name;          /* name to ensure terminated */
 int  size;
{
  char *p;    /* local temp ptr */

  for (p = (name+size); p >= name; p--)
    {
      if ((*p == '\0') || (*p == ' '))
	*p = '\0';
      else
        break;
    }
}

/**********************************************************************
 *
 *	getp1 - 1st getparm in copy: get input file info
 *
 **********************************************************************/
static int getp1(fil, lib, vol)
 char *fil, *lib, *vol;  /* file information */
{
  int  valid_resp;     	         /* indicator of response validity */

  int  tmp;		         /* to check file variable lengths */
  int  argcnt; 	                 /* number of calling arguments */
 
 for (tmp = 0; tmp < MAXLINES; tmp++)
   {
     mlsize[tmp] = tlsize[tmp] = klsize[tmp] = 0;
     trow[tmp] = tcol[tmp] = krow[tmp] = kcol[tmp] = 0;
   }
 memset(mline, ' ', MAXLINES*80);
 memset(tline, ' ', MAXLINES*80);
 memset(kline, ' ', MAXLINES*80);
 strcpy(prname, "INPUT   ");
 prname[8] = '\0';

 req_type[0] = 'I';
 req_type[1] = ' ';
 req_type[2] = 0;

 strcpy(msgid, "0001");      /* numbers */
 msgid[4] = 0;

 n_mlines = 1; 
 strcpy(mline[0], "UNIX - Wang VS Copy Emulation - Version 1.1");
 mlsize[0] = 43;
       
 if ((tmp = strlen(vol)) < 6) 
   for (;tmp < 6;tmp++) vol[tmp] = ' ';
 if ((tmp = strlen(lib)) < 8) 
   for (;tmp < 8;tmp++) lib[tmp] = ' ';
 if ((tmp = strlen(fil)) < 8) 
   for (;tmp < 8;tmp++) fil[tmp] = ' ';
 vol[6] = lib[8] = fil[8] = 0;

 ttype[0] =  'U';         /* set text fields types to Underline */
 strcpy(tline[0], "Specify the file, library, and volume information:");
 tlsize[0] = 50;
 trowflg[0] = 'A'; trow[0] = 10; 
 tcolflg[0] = 'A'; tcol[0] = 3; 
 ttype[1] =  'T';
 strcpy(tline[1], "in ");
 tlsize[1] = 3;
 trowflg[1] = 'A'; trow[1] = 12; 
 tcolflg[1] = 'A'; tcol[1] = 26; 
 ttype[2] =  'T'; 
 strcpy(tline[2], "on ");
 tlsize[2] = 3;
 trowflg[2] = 'A'; trow[2] = 12; 
 tcolflg[2] = 'A'; tcol[2] = 49; 
 ttype[3] =  'U';         /* set text fields types to Underline */
 strcpy(tline[3], "Specify the desired copy option:");
 tlsize[3] = 32;
 trowflg[3] = 'A'; trow[3] = 14; 
 tcolflg[3] = 'A'; tcol[3] = 3; 
 ttype[4] =  'T';
 strcpy(tline[4], "COPY     = FILE***        (Options not available)");
 tlsize[4] = 49;
 trowflg[4] = 'A'; trow[4] = 16; 
 tcolflg[4] = 'A'; tcol[4] = 6; 
 ttype[5] =  'U';         /* set text fields types to Underline */
 strcpy(tline[5], "Specify the mode in which to open the files to be copied:");
 tlsize[5] = 57;
 trowflg[5] = 'A'; trow[5] = 18; 
 tcolflg[5] = 'A'; tcol[5] = 3; 
 ttype[6] =  'T';
 strcpy(tline[6], "MODE     = INPUT*         (Options not available)");
 tlsize[6] = 48;
 trowflg[6] = 'A'; trow[6] = 20; 
 tcolflg[6] = 'A'; tcol[6] = 6; 
 ttype[7] =  'T';
 strcpy(tline[7], "Press PF16 to end COPY processing");
 tlsize[7] = 33;
 trowflg[7] = 'A'; trow[7] = 22; 
 tcolflg[7] = 'A'; tcol[7] = 25; 

 memset(ktype, 'K', 3); /* for initial set all keyword fields to K */
 strcpy(kline[0], "FILE   ");
 klsize[0] = 8;
 krowflg[0] = 'A'; krow[0] = 12; 
 kcolflg[0] = 'A'; kcol[0] = 6; 
 strcpy(kline[1], "LIBRARY");
 klsize[1] = 8;
 krowflg[1] = 'R'; krow[1] = 0; 
 kcolflg[1] = 'A'; kcol[1] = 29; 
 strcpy(kline[2], "VOLUME");
 klsize[2] = 8;
 krowflg[2] = 'R'; krow[2] = 0; 
 kcolflg[2] = 'A'; kcol[2] = 52; 
 memset(datatype, 'L', 3);  /* set data types for keyword fields */

 pftype = 'P';	             /* pf key mask parameter */
 wb_hex("00010000", pfmask); /* enable PF16 */
 valid_resp = 0;
 do
  {  /* do getparm until we get valid responses */
   argcnt = 94;
   wvaset(&argcnt);
   GETPARM(req_type, "R", prname, &key_rec, msgid, "COPY",
         &n_mlines, mline[0], &mlsize[0],
         &pftype, pfmask,
         &ttype[0], tline[0], &tlsize[0],
            &trowflg[0], &trow[0], &tcolflg[0], &tcol[0],
         &ttype[1], tline[1], &tlsize[1],
            &trowflg[1], &trow[1], &tcolflg[1], &tcol[1],
         &ttype[2], tline[2], &tlsize[2],
            &trowflg[2], &trow[2], &tcolflg[2], &tcol[2],
         &ttype[3], tline[3], &tlsize[3],
            &trowflg[3], &trow[3], &tcolflg[3], &tcol[3],
         &ttype[4], tline[4], &tlsize[4],
            &trowflg[4], &trow[4], &tcolflg[4], &tcol[4],
         &ttype[5], tline[5], &tlsize[5],
            &trowflg[5], &trow[5], &tcolflg[5], &tcol[5],
         &ttype[6], tline[6], &tlsize[6],
            &trowflg[6], &trow[6], &tcolflg[6], &tcol[6],
         &ttype[7], tline[7], &tlsize[7],
            &trowflg[7], &trow[7], &tcolflg[7], &tcol[7],
         &ktype[0], kline[0], fil, &klsize[0],
            &krowflg[0], &krow[0], &kcolflg[0], &kcol[0], &datatype[0],
         &ktype[1], kline[1], lib, &klsize[1],
            &krowflg[1], &krow[1], &kcolflg[1], &kcol[1], &datatype[1],
         &ktype[2], kline[2], vol, &klsize[2],
            &krowflg[2], &krow[2], &kcolflg[2], &kcol[2], &datatype[2]);

   if (key_rec == PF16)    /* pf16 says stop now */
     return(-1);  
   vol[6] = lib[8] = fil[8] = 0;
   valid_resp = chk_valid(vol, lib, fil, ktype);
   incr_id(msgid);           /* incriment count */
  }
 while (!valid_resp);
 return(1);
 
}

/**********************************************************************
 *
 *	getp2 - 2nd getparm in copy: Lock, timeout etc
 *
 **********************************************************************/
static void getp2(lock, timeout, bypass)
  int *lock;		/* flag for locking file before copy */
  int *timeout;		/* number of seconds to wait for lock */
  int *bypass;		/* flag to bypass file if time out expires */
{
  int  valid_resp;     	         /* indicator of response validity */

  int  tmp;		         /* to check file variable lengths */
  int  argcnt; 	                 /* number of calling arguments */
  char lvar[3], tvar[3], bvar[3]; /* variables from getparm */
 
 for (tmp = 0; tmp < MAXLINES; tmp++)
   {
     mlsize[tmp] = tlsize[tmp] = klsize[tmp] = 0;
     trow[tmp] = tcol[tmp] = krow[tmp] = kcol[tmp] = 0;
   }
 memset(mline, ' ', MAXLINES*80);
 memset(tline, ' ', MAXLINES*80);
 memset(kline, ' ', MAXLINES*80);
 strcpy(prname, "LOCK    ");
 prname[8] = '\0';
 strcpy(lvar, "YES");
 strcpy(tvar, "10 ");
 strcpy(bvar, "NO ");

 req_type[0] = 'I';
 req_type[1] = 'D';
 req_type[2] = 0;

 strcpy(msgid, "0057");      /* numbers */
 msgid[4] = 0;

 n_mlines = 0; 
       
 ttype[0] =  'U';         /* set text fields types to Underline */
 strcpy(tline[0], 
     "Specify the following options to process a file in Shared Mode.");
 tlsize[0] = 63;
 trowflg[0] = 'A'; trow[0] = 9; 
 tcolflg[0] = 'A'; tcol[0] = 10; 
 ttype[1] =  'T';
 strcpy(tline[1], "Should a lock be placed on the file before copying?   ");
 tlsize[1] = 54;
 trowflg[1] = 'A'; trow[1] = 11; 
 tcolflg[1] = 'A'; tcol[1] = 2; 
 ttype[2] =  'T';
 strcpy(tline[2], "(YES, NO)");
 tlsize[2] = 9;
 trowflg[2] = 'A'; trow[2] = 11; 
 tcolflg[2] = 'A'; tcol[2] = 72; 
 ttype[3] =  'T'; 
 strcpy(tline[3], "If YES, changes cannot be made to the file during copying.");
 tlsize[3] = 58;
 trowflg[3] = 'A'; trow[3] = 13; 
 tcolflg[3] = 'A'; tcol[3] = 4; 
 ttype[4] =  'T';  
 strcpy(tline[4], "If NO, changes to the file can be made.");
 tlsize[4] = 39;
 trowflg[4] = 'A'; trow[4] = 14; 
 tcolflg[4] = 'A'; tcol[4] = 4; 
 ttype[5] =  'U';         /* set text fields types to Underline */
 strcpy(tline[5], "If LOCK=YES, Specify TIMEOUT and BYPASS:");
 tlsize[5] = 40;
 trowflg[5] = 'A'; trow[5] = 17; 
 tcolflg[5] = 'A'; tcol[5] = 21; 
 ttype[6] =  'T';
 strcpy(tline[6], "How many seconds should the TIMEOUT be?");
 tlsize[6] = 39;
 trowflg[6] = 'A'; trow[6] = 19; 
 tcolflg[6] = 'A'; tcol[6] = 2; 
 ttype[7] =  'T';
 strcpy(tline[7], "Seconds");
 tlsize[7] = 7;
 trowflg[7] = 'A'; trow[7] = 19; 
 tcolflg[7] = 'A'; tcol[7] = 72; 
 ttype[8] =  'T';
 strcpy(tline[8], "If the TIMEOUT expires, should the file be bypassed?");
 tlsize[8] = 52;
 trowflg[8] = 'A'; trow[8] = 21; 
 tcolflg[8] = 'A'; tcol[8] = 2; 
 ttype[9] =  'T';         /* set text fields types to just text */
 strcpy(tline[9], "(YES, NO)");
 tlsize[9] = 9;
 trowflg[9] = 'A'; trow[9] = 21; 
 tcolflg[9] = 'A'; tcol[9] = 72; 

 memset(ktype, 'K', 3); /* for initial set all keyword fields to K */
 strcpy(kline[0], "LOCK  ");
 klsize[0] = 3;
 krowflg[0] = 'A'; krow[0] = 11; 
 kcolflg[0] = 'A'; kcol[0] = 57; 
 datatype[0] = 'L';
 strcpy(kline[1], "TIMEOUT ");
 klsize[1] = 3;
 krowflg[1] = 'A'; krow[1] = 19; 
 kcolflg[1] = 'A'; kcol[1] = 57; 
 datatype[0] = 'N';
 strcpy(kline[2], "BYPASS  ");
 klsize[2] = 3;
 krowflg[2] = 'A'; krow[2] = 21; 
 kcolflg[2] = 'A'; kcol[2] = 57; 
 datatype[0] = 'L';

 pftype = 'P';	             /* pf key mask parameter */
 wb_hex("00000000", pfmask); /* disable PF keys */
 valid_resp = 0;
 do
  {  /* do getparm until we get valid responses */
   argcnt = 106;
   wvaset(&argcnt);
   GETPARM(req_type, "R", prname, &key_rec, msgid, "COPY",
         &n_mlines,
         &pftype, pfmask,
         &ttype[0], tline[0], &tlsize[0],
            &trowflg[0], &trow[0], &tcolflg[0], &tcol[0],
         &ttype[1], tline[1], &tlsize[1],
            &trowflg[1], &trow[1], &tcolflg[1], &tcol[1],
         &ttype[2], tline[2], &tlsize[2],
            &trowflg[2], &trow[2], &tcolflg[2], &tcol[2],
         &ttype[3], tline[3], &tlsize[3],
            &trowflg[3], &trow[3], &tcolflg[3], &tcol[3],
         &ttype[4], tline[4], &tlsize[4],
            &trowflg[4], &trow[4], &tcolflg[4], &tcol[4],
         &ttype[5], tline[5], &tlsize[5],
            &trowflg[5], &trow[5], &tcolflg[5], &tcol[5],
         &ttype[6], tline[6], &tlsize[6],
            &trowflg[6], &trow[6], &tcolflg[6], &tcol[6],
         &ttype[7], tline[7], &tlsize[7],
            &trowflg[7], &trow[7], &tcolflg[7], &tcol[7],
         &ttype[8], tline[8], &tlsize[8],
            &trowflg[8], &trow[8], &tcolflg[8], &tcol[8],
         &ttype[9], tline[9], &tlsize[9],
            &trowflg[9], &trow[9], &tcolflg[9], &tcol[9],
         &ktype[0], kline[0], lvar, &klsize[0],
            &krowflg[0], &krow[0], &kcolflg[0], &kcol[0], &datatype[0],
         &ktype[1], kline[1], tvar, &klsize[1],
            &krowflg[1], &krow[1], &kcolflg[1], &kcol[1], &datatype[1],
         &ktype[2], kline[2], bvar, &klsize[2],
            &krowflg[2], &krow[2], &kcolflg[2], &kcol[2], &datatype[2]);

   lvar[3] = tvar[3] = bvar[3] = 0;
   valid_resp = chk_valid(lvar, tvar, bvar, ktype);
   incr_id(msgid);           /* incriment count */
  }
 while (!valid_resp);

 for (tmp = 0; tmp < 3; tmp++)
   {
    if (lvar[tmp] > 0x5A)  lvar[tmp] -= 0x20;   /* convert to uppercase */
    if (bvar[tmp] > 0x5A)  bvar[tmp] -= 0x20;
   }
 if (strncmp(lvar, "YES", 3) == 0)
   *lock = 1;
 else
   *lock = 0;
 if (strncmp(bvar, "YES", 3) == 0)
   *bypass = 1;
 else
   *bypass = 0;
 sscanf(tvar, "%d", timeout);
}

/**********************************************************************
 *
 *	getp3 - 3rd getparm in copy: Nothing important to UNIX
 *
 **********************************************************************/
static void getp3()
{
  int  tmp;		         /* to check file variable lengths */
  int  argcnt; 	                 /* number of calling arguments */
 
 for (tmp = 0; tmp < MAXLINES; tmp++)
   {
     mlsize[tmp] = tlsize[tmp] = klsize[tmp] = 0;
     trow[tmp] = tcol[tmp] = krow[tmp] = kcol[tmp] = 0;
   }
 memset(mline, ' ', MAXLINES*80);
 memset(tline, ' ', MAXLINES*80);
 memset(kline, ' ', MAXLINES*80);
 strcpy(prname, "OPTIONS ");
 prname[8] = '\0';

 req_type[0] = 'I';
 req_type[1] = 'D';
 req_type[2] = 0;

 strcpy(msgid, "0030");      /* numbers */
 msgid[4] = 0;

 n_mlines = 1; 
 strcpy(mline[0], "This information not used in UNIX Environment!");
 mlsize[0] = 46;
       
 ttype[0] =  'U';         /* set text fields types to Underline */
 strcpy(tline[0], "Specify the output file organization:");
 tlsize[0] = 37;
 trowflg[0] = 'A'; trow[0] = 9; 
 tcolflg[0] = 'A'; tcol[0] = 3; 
 ttype[1] =  'T';
 strcpy(tline[1], 
 "FILEORG  = I         (C-Consecutive; I-Indexed; N-Indexed-plus; R-Relative)");
 tlsize[1] = 75;
 trowflg[1] = 'A'; trow[1] = 10; 
 tcolflg[1] = 'A'; tcol[1] = 4; 
 ttype[2] =  'T';
 strcpy(tline[2], "LENGTH   = F         (F-Fixed length; V-Variable length)");
 tlsize[2] = 56;
 trowflg[2] = 'A'; trow[2] = 11; 
 tcolflg[2] = 'A'; tcol[2] = 4; 
 ttype[3] =  'T'; 
 strcpy(tline[3], "COMPRESS = N         (Y or N)");
 tlsize[3] = 30;
 trowflg[3] = 'A'; trow[3] = 12; 
 tcolflg[3] = 'A'; tcol[3] = 4; 
 ttype[4] =  'T';  
 strcpy(tline[4], 
  "REORG    = NO*       (YES = Reorganize file; NO = Do not reorganize file)");
 tlsize[4] = 73;
 trowflg[4] = 'A'; trow[4] = 13; 
 tcolflg[4] = 'A'; tcol[4] = 4; 
 ttype[5] =  'U';         /* set text fields types to Underline */
 strcpy(tline[5], 
  "Specify the followinf information for indexed or indexed-plus files:");
 tlsize[5] = 69;
 trowflg[5] = 'A'; trow[5] = 15; 
 tcolflg[5] = 'A'; tcol[5] = 3; 
 ttype[6] =  'T';
 strcpy(tline[6], "KEYLEN   = 025");
 tlsize[6] = 14;
 trowflg[6] = 'A'; trow[6] = 16; 
 tcolflg[6] = 'A'; tcol[6] = 4; 
 ttype[7] =  'T';
 strcpy(tline[7], "KEYPOS   = 00001     (From 1)");
 tlsize[7] = 29;
 trowflg[7] = 'A'; trow[7] = 17; 
 tcolflg[7] = 'A'; tcol[7] = 4; 
 ttype[8] =  'T';
 strcpy(tline[8], "IPACK    = 100 %     (Packing density for index blocks)");
 tlsize[8] = 72;
 trowflg[8] = 'A'; trow[8] = 18; 
 tcolflg[8] = 'A'; tcol[8] = 4; 
 ttype[9] =  'T';         /* set text fields types to just text */
 strcpy(tline[9], "DPACK    = 100 %     (Packing density for data blocks)");
 tlsize[9] = 71;
 trowflg[9] = 'A'; trow[9] = 19; 
 tcolflg[9] = 'A'; tcol[9] = 4; 
 ttype[10] =  'T';         /* set text fields types to just text */
 strcpy(tline[10],
 "RECBLK   = N         (DMX/TX Recovery blocks; N=None, A=Allocated, U=Used)"); 
 tlsize[10] = 75;
 trowflg[10] = 'A'; trow[10] = 20; 
 tcolflg[10] = 'A'; tcol[10] = 4; 
 ttype[11] =  'U';         /* set text fields types to Underline */
 strcpy(tline[11], "Specify the following information for indexed-plus files:"); 
 tlsize[11] = 58;
 trowflg[11] = 'A'; trow[11] = 22; 
 tcolflg[11] = 'A'; tcol[11] = 3; 
 ttype[12] =  'T';
 strcpy(tline[12],
  "ORDER    = N         (Y = YES, Entry ordered; N = NO, key ordered)"); 
 tlsize[12] = 67;
 trowflg[12] = 'A'; trow[12] = 23; 
 tcolflg[12] = 'A'; tcol[12] = 4; 
 ttype[13] =  'T';
 strcpy(tline[13],
  "HASHSIZE = ********* (Hashed, size in blocks; 0 = Primary key not hashed)");
 tlsize[13] = 74;
 trowflg[13] = 'A'; trow[13] = 24; 
 tcolflg[13] = 'A'; tcol[13] = 4; 


 pftype = 'P';	             /* pf key mask parameter */
 wb_hex("00000000", pfmask); /* disable PF keys */
   argcnt = 109;
   wvaset(&argcnt);
   GETPARM(req_type, "R", prname, &key_rec, msgid, "COPY",
         &n_mlines, mline[0], &mlsize[0],
         &pftype, pfmask,
         &ttype[0], tline[0], &tlsize[0],
            &trowflg[0], &trow[0], &tcolflg[0], &tcol[0],
         &ttype[1], tline[1], &tlsize[1],
            &trowflg[1], &trow[1], &tcolflg[1], &tcol[1],
         &ttype[2], tline[2], &tlsize[2],
            &trowflg[2], &trow[2], &tcolflg[2], &tcol[2],
         &ttype[3], tline[3], &tlsize[3],
            &trowflg[3], &trow[3], &tcolflg[3], &tcol[3],
         &ttype[4], tline[4], &tlsize[4],
            &trowflg[4], &trow[4], &tcolflg[4], &tcol[4],
         &ttype[5], tline[5], &tlsize[5],
            &trowflg[5], &trow[5], &tcolflg[5], &tcol[5],
         &ttype[6], tline[6], &tlsize[6],
            &trowflg[6], &trow[6], &tcolflg[6], &tcol[6],
         &ttype[7], tline[7], &tlsize[7],
            &trowflg[7], &trow[7], &tcolflg[7], &tcol[7],
         &ttype[8], tline[8], &tlsize[8],
            &trowflg[8], &trow[8], &tcolflg[8], &tcol[8],
         &ttype[9], tline[9], &tlsize[9],
            &trowflg[9], &trow[9], &tcolflg[9], &tcol[9],
         &ttype[10], tline[10], &tlsize[10],
            &trowflg[10], &trow[10], &tcolflg[10], &tcol[10],
         &ttype[11], tline[11], &tlsize[11],
            &trowflg[11], &trow[11], &tcolflg[11], &tcol[11],
         &ttype[12], tline[12], &tlsize[12],
            &trowflg[12], &trow[12], &tcolflg[12], &tcol[12],
         &ttype[13], tline[13], &tlsize[13],
            &trowflg[13], &trow[13], &tcolflg[13], &tcol[13]);

}

/**********************************************************************
 *
 *	getp4 - 4th getparm in copy: get output file info
 *
 **********************************************************************/
static int getp4(fil, lib, vol)
 char *fil, *lib, *vol;  /* file information */
{
  int  valid_resp;     	         /* indicator of response validity */
  int  tmp;		         /* to check file variable lengths */
  int  argcnt; 	                 /* number of calling arguments */
 
 for (tmp = 0; tmp < MAXLINES; tmp++)
   {
     mlsize[tmp] = tlsize[tmp] = klsize[tmp] = 0;
     trow[tmp] = tcol[tmp] = krow[tmp] = kcol[tmp] = 0;
   }
 memset(mline, ' ', MAXLINES*80);
 memset(tline, ' ', MAXLINES*80);
 memset(kline, ' ', MAXLINES*80);
 strcpy(prname, "OUTPUT  ");
 prname[8] = '\0';

 req_type[0] = 'I';
 req_type[1] = ' ';
 req_type[2] = 0;

 strcpy(msgid, "OUT1");      /* numbers */
 msgid[4] = 0;

 n_mlines = 0; 
       
 if ((tmp = strlen(vol)) < 6) 
   for (;tmp < 6;tmp++) vol[tmp] = ' ';
 if ((tmp = strlen(lib)) < 8) 
   for (;tmp < 8;tmp++) lib[tmp] = ' ';
 if ((tmp = strlen(fil)) < 8) 
   for (;tmp < 8;tmp++) fil[tmp] = ' ';
 vol[6] = lib[8] = fil[8] = 0;

 ttype[0] =  'U';         /* set text fields types to Underline */
 strcpy(tline[0], 
  "Please enter the parameters necessary to define the output file:");
 tlsize[0] = 64;
 trowflg[0] = 'A'; trow[0] = 9; 
 tcolflg[0] = 'A'; tcol[0] = 9; 
 ttype[1] =  'T';
 strcpy(tline[1], "in ");
 tlsize[1] = 3;
 trowflg[1] = 'A'; trow[1] = 11; 
 tcolflg[1] = 'A'; tcol[1] = 29; 
 ttype[2] =  'T'; 
 strcpy(tline[2], "on ");
 tlsize[2] = 3;
 trowflg[2] = 'A'; trow[2] = 11; 
 tcolflg[2] = 'A'; tcol[2] = 54; 
 ttype[3] =  'U';         /* set text fields types to Underline */
 strcpy(tline[3], "Press:");
 tlsize[3] = 6;
 trowflg[3] = 'A'; trow[3] = 19; 
 tcolflg[3] = 'A'; tcol[3] = 6; 
 ttype[4] =  'T';
 strcpy(tline[4], "(ENTER) To create the output file");
 tlsize[4] = 33;
 trowflg[4] = 'A'; trow[4] = 20; 
 tcolflg[4] = 'A'; tcol[4] = 9; 
 ttype[5] =  'T'; 
 strcpy(tline[5], " (16)   To exit COPY");
 tlsize[5] = 20;
 trowflg[5] = 'A'; trow[5] = 24; 
 tcolflg[5] = 'A'; tcol[5] = 9; 

 memset(ktype, 'K', 3); /* for initial set all keyword fields to K */
 strcpy(kline[0], "FILE   ");
 klsize[0] = 8;
 krowflg[0] = 'A'; krow[0] = 11; 
 kcolflg[0] = 'A'; kcol[0] = 8; 
 strcpy(kline[1], "LIBRARY");
 klsize[1] = 8;
 krowflg[1] = 'R'; krow[1] = 0; 
 kcolflg[1] = 'A'; kcol[1] = 33; 
 strcpy(kline[2], "VOLUME");
 klsize[2] = 8;
 krowflg[2] = 'R'; krow[2] = 0; 
 kcolflg[2] = 'A'; kcol[2] = 58; 
 memset(datatype, 'L', 3);  /* set data types for keyword fields */

 pftype = 'P';	             /* pf key mask parameter */
 wb_hex("00010000", pfmask); /* enable PF16 */
 valid_resp = 0;
 do
  {  /* do getparm until we get valid responses */
   argcnt = 78;
   wvaset(&argcnt);
   GETPARM(req_type, "R", prname, &key_rec, msgid, "COPY",
         &n_mlines,
         &pftype, pfmask,
         &ttype[0], tline[0], &tlsize[0],
            &trowflg[0], &trow[0], &tcolflg[0], &tcol[0],
         &ttype[1], tline[1], &tlsize[1],
            &trowflg[1], &trow[1], &tcolflg[1], &tcol[1],
         &ttype[2], tline[2], &tlsize[2],
            &trowflg[2], &trow[2], &tcolflg[2], &tcol[2],
         &ttype[3], tline[3], &tlsize[3],
            &trowflg[3], &trow[3], &tcolflg[3], &tcol[3],
         &ttype[4], tline[4], &tlsize[4],
            &trowflg[4], &trow[4], &tcolflg[4], &tcol[4],
         &ttype[5], tline[5], &tlsize[5],
            &trowflg[5], &trow[5], &tcolflg[5], &tcol[5],
         &ktype[0], kline[0], fil, &klsize[0],
            &krowflg[0], &krow[0], &kcolflg[0], &kcol[0], &datatype[0],
         &ktype[1], kline[1], lib, &klsize[1],
            &krowflg[1], &krow[1], &kcolflg[1], &kcol[1], &datatype[1],
         &ktype[2], kline[2], vol, &klsize[2],
            &krowflg[2], &krow[2], &kcolflg[2], &kcol[2], &datatype[2]);

   vol[6] = lib[8] = fil[8] = 0;
   if (key_rec == PF16)   /* pf16 says stop now */
     return(-1);  
   valid_resp = chk_valid(vol, lib, fil, ktype);
   incr_id(msgid);           /* incriment count */
  }
 while (!valid_resp);
 return(1);
 
}

/**********************************************************************
 *
 *	getp5 - 5th getparm in copy: On error check if user wants to quit
 *
 **********************************************************************/
static int getp5(msg)
  char *msg;
{
  int  tmp;		         /* to check file variable lengths */
  int  argcnt; 	                 /* number of calling arguments */
 
 for (tmp = 0; tmp < MAXLINES; tmp++)
   {
     mlsize[tmp] = tlsize[tmp] = 0;
     trow[tmp] = tcol[tmp] = 0;
   }
 memset(mline, ' ', MAXLINES*80);
 memset(tline, ' ', MAXLINES*80);
 strcpy(prname, "EOJ     ");
 prname[8] = '\0';

 req_type[0] = 'I';
 req_type[1] = ' ';
 req_type[2] = 0;

 strcpy(msgid, "0045");      /* numbers */
 msgid[4] = 0;

 n_mlines = 1; 
 strcpy(mline[0], msg);
 mlsize[0] = strlen(msg);
       
 ttype[0] =  'T';
 strcpy(tline[0], "Select ENTER or PF16 to End the program,");
 tlsize[0] = 40;
 trowflg[0] = 'A'; trow[0] = 9; 
 tcolflg[0] = 'A'; tcol[0] = 2; 
 ttype[1] =  'T';
 strcpy(tline[1], "             or PF1  to rerun the program.");
 tlsize[1] = 42;
 trowflg[1] = 'A'; trow[1] = 10; 
 tcolflg[1] = 'A'; tcol[1] = 2; 

 pftype = 'P';	               /* pf key mask parameter */
 wb_hex("80010000", pfmask);   /* enable PF16 & PF1 */
   argcnt = 25;
   wvaset(&argcnt);
   GETPARM(req_type, "R", prname, &key_rec, msgid, "COPY",
         &n_mlines, mline[0], &mlsize[0],
         &pftype, pfmask,
         &ttype[0], tline[0], &tlsize[0],
            &trowflg[0], &trow[0], &tcolflg[0], &tcol[0],
         &ttype[1], tline[1], &tlsize[1],
            &trowflg[1], &trow[1], &tcolflg[1], &tcol[1]);

   if (key_rec == PF1)    /* PF1 says rerun COPY */
     return(0);  
 return(1);
 
}

/**********************************************************************
 *
 *	chk_valid - check getparm response for validity
 *		    reset any field indicators for any invalid fields
 *
 **********************************************************************/
static int chk_valid(vol, lib, fil, types)
 char *vol, *lib, *fil;  /* file information */
 char types[];           /* type indicators for getparm calls */
{
  int i;	   /* local index */
  int tst_len;     /* length of string being tested */
  int ret_flag;    /* status to return */
  int firstnonblk; /* index of first non blank character in string (from
		      right end ) */

  ret_flag = 1;            /* assume no problem to start */

  tst_len = strlen(vol);   /* test volume name */
  if (tst_len == 0)        /* ensure that a string exists */
    {
      types[2] = 'R'; ret_flag = 0;    /* no string here */
    }
  else                  
    {   /* and ensure that it is all alphanumeric characters with no
           embedded blanks */
     firstnonblk = -1;
     for (i = (tst_len - 1); i >= 0; i--)
       {
        if ((!isalnum(vol[i])) && (vol[i] != ' ')) 
          { 
           types[2] = 'R'; ret_flag = 0; break;  /* non alphanumeric */
          }
        if ((vol[i] == ' ') && (firstnonblk >= 0))
          { 
           types[2] = 'R'; ret_flag = 0; break;  /* embedded blank */
          }
        if ((vol[i] != ' ') && (firstnonblk < 0))
          firstnonblk = i;
       }
    }

  tst_len = strlen(lib);   /* test library name */
  if (tst_len == 0)        /* ensure that a string exists and is all */
    {
      types[1] = 'R'; ret_flag = 0;    /* no string here */
    }
  else                  
    {   /* and ensure that it is all alphanumeric characters with no
           embedded blanks */
     firstnonblk = -1;
     for (i = (tst_len - 1); i >= 0; i--)
       {
        if ((!isalnum(lib[i])) && (lib[i] != ' ')) 
          { 
           types[1] = 'R'; ret_flag = 0; break;  /* non alphanumeric */
          }
        if ((lib[i] == ' ') && (firstnonblk >= 0))
          { 
           types[1] = 'R'; ret_flag = 0; break;  /* embedded blank */
          }
        if ((lib[i] != ' ') && (firstnonblk < 0))
          firstnonblk = i;
       }
    }

  tst_len = strlen(fil);   /* test file name */
  if (tst_len == 0)        /* ensure that a string exists and is all */
    {
      types[0] = 'R'; ret_flag = 0;     /* no string here */
    }
  else                  
    {   /* and ensure that it is all alphanumeric characters with no
           embedded blanks */
     firstnonblk = -1;
     for (i = (tst_len - 1); i >= 0; i--)
       {  /* Allow special characters in file names */
        if ((!isalnum(fil[i])) && (fil[i] != ' ') && 
            (fil[i] != '#') && (fil[i] != '@') && (fil[i] != '$')) 
          { 
           types[0] = 'R'; ret_flag = 0; break;  /* non alphanumeric */
          }
        if ((fil[i] == ' ') && (firstnonblk >= 0))
          { 
           types[0] = 'R'; ret_flag = 0; break;  /* embedded blank */
          }
        if ((fil[i] != ' ') && (firstnonblk < 0))
          firstnonblk = i;
       }
    }

  return(ret_flag);
}

/**********************************************************************
 *
 *	incr_id - little routine to incriment a 4 diget ascii value
 *
 **********************************************************************/
static incr_id(desc)               
 char desc[5];
{
 if (desc[3] == '9')
   {
     desc[3] = '0';
     if (desc[2] == '9')
       {
        desc[2] = '0';
        if (desc[1] == '9')
          {
           desc[1] = '0';
           if (desc[0] == '9')
             desc[0] == '0';
           else
             desc[0] += 1;
          }
        else
          desc[1] += 1;
       }
     else
       desc[2] += 1;
    }
  else
    desc[3] += 1;
}

/**********************************************************************
 *
 *	do_cp - copy input file to output file
 *
 **********************************************************************/
int do_cp(in, out)
 char *in, *out;
{
  int fdi, fdo;        /* file descriptor numbers */
  char buf[4096];
  int reqbytes, got_bytes;

  reqbytes = 4096;
  fdi = open(in, O_RDONLY, 0);     /* open input file read only*/
  if (fdi < 0)
    return(errno);

  fdo = open(out, O_WRONLY|O_CREAT|O_TRUNC, 0666);   /* open output file */
  if (fdo < 0)
    return(errno);

  /* do copy */
  while ((got_bytes = read(fdi, buf, reqbytes)) == reqbytes)
    {
      write(fdo, buf, got_bytes);
    }
  if (got_bytes > 0)
     write(fdo, buf, got_bytes);

  fsync(fdo);     /* ensure data is out to disk and */
  close(fdi);     /* close files */
  close(fdo);

  return(0);
}
