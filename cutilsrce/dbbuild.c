/*****************************************************************************
                   C A E L U S   P R O G R A M   M O D U L E
                    (c)Copyright 1991 CAELUS, Incorporated
                     A L L   R I G H T S   R E S E R V E D
==============================================================================
                      M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------
 
    dbbuild - A program to convert Wang VS data file to DISAM
              The file must have been run through the Caelus utility dbconvrt
	      on the Wang and then brought to the UNIX system.
 
==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
	1991		PP	Wrote original
	02/03/92	MDH	reduced REC_END from 5 to 4 bytes: no newline 
	02/04/92	MDH	Added timing code
	05/27/92	MDH	Use alt key number instead of position when
				creating keys so if one is unused it is still
				defined.  Make inactive alt key start at 0,
				have length 1 and allow dups
	09/11/92	MDH	Fix build of sequencial file. k_nparts = 0
	09/14/92	MDH	Don't add 4 bytes of REC_END id seq file  
	09/17/92	MDH	Pick up additions to file structure to handle
				fragmented files
	09/30/92	MDH	Fixed problem w/ missing single alt key
	10/30/93	MDH	Trap & report errors & continue
	12/13/93	MDH	Change to use Exclusive lock rather than using
                                manual locks
	01/23/97	kab	Update to match wb_open for alt keys

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/time.h>
#include "disam.h"

#define REC_END 4	/* a four byte filler */
#define ON 1
#define OFF 0
/*  Comment out the following line to turn off timeing code */
#define TIMING
struct db_head
  {
	char	filename[8];	/* FILENAME */
	char	library[8];	/* SOURCE LIBRARY */
	char	volume[6];	/* SOURCE VOLUME */
	char	filtyp[1];	/* File Type */
	char	crtid[3];	/* Creator ID */
	char	crtdat[6];	/* Creation Date */
	char	expdat[6];	/* Expiration Date (not used) */
	char	moddat[6];	/* Modified Date (use UNIX) */
	char	recsize[4];	/* Number of bytes in record */
	char	keypos[4];	/* offset of primary key */
	char	keylen[4];	/* number of bytes in primary key */
	char	numalt[4];	/* number of alternate keys */
	char	numrecs[8];	/* total number of records in file complete*/
	char	seqno[2];	/* 2 byte sequence number if fragment
				   spaces if no fragments */
  };

struct alt_keyinfo
  {
	char	num[2];		/* alternate key number (01-16) */
	char	pos[4];		/* alt key position */
	char	len[4];		/* alt key length */
	char	dup[1];		/* duplicat key flag */
	char	rec[8];		/* associated records (N/A UNIX) */
	char	fill1[1];	/* one byte filler */
  };

struct hdr_block
  {
	struct db_head hdr;		/* file header info */
	struct alt_keyinfo alt[16];	/* alternate key info */
	char   fill2[2];		/* 2 byte filler */
	char   frag_numrec[8];		/* num records in this file frag */ 
  };

struct alt_keys
  {
	int	pos;	/* key position offset from zero */
	int	len;	/* key length (zero if key not used) */
	int	dup;	/* duplicate flag */
  };

main(argc,argv)
  int argc;
  char *argv[];
{
   char Recbuf[2049];			/* record buffer */
   int Keypos;				/* primary key position */
   int Keylen;				/* primary key length */
   int Reclen;				/* data file record length */
   char Inpath[80];			/* Input file path name */
   char Vspath[80];			/* DISAM file path name */
   int N_alts;				/* number of alternate keys */
   int i,j,k,istat;
   FILE *fd;				/* input file pointer */
   char *strnull(); 
   int rlen, rbeg, rcnt;		
   struct dictinfo dict_info;		/* DISAM file info block */
   struct keydesc keys;			/* DISAM key description block */
   struct hdr_block Dbdef;		/* header block from input file */
   struct alt_keys Alt_tmp[16]; 	/* array of alt keys */
   int isfd;				/* DISAM file number */
   int Seqno;				/* file fragment number */
   int hdr_flag;			/* list header flag */
   int idx_flag;			/* list key info flag */
   int silent_flg;			/* Don't ask on error */
   char answ[2];			/* user response when error */
   int br_no;				/* bad record count */
   
#  ifdef TIMING
   struct timeval stp, ftp;		/* structures for timing */
   struct timezone stzp, ftzp;
   FILE *time_fd;
   FILE *record_fd;
   float tot_time, elap_time;
#  endif

   memset(Vspath,0,80);
   hdr_flag = idx_flag = silent_flg = OFF;

   /* test for the proper number of input arguments */
   if ((argc < 2) || (argc > 4))
     {
	printf("Invalid Number of arguments! \n\n");
   	printf("\nUSAGE - dbbuild  file [dest] [-hiq]");
   	printf("\n        file	Input Filename");
   	printf("\n        dest	Destination Directory Path");
   	printf("\n      Options: ");
   	printf("\n        q   Don't Stop and ask on error, Just continue");
   	printf("\n        h   (Debug run) List Input File Header Information");
   	printf("\n        i   (Debug run) List Input File Index Information");
	printf("\n      Debug options cause termination WITHOUT conversion!\n");
   	exit(-1);
     }

   /* get the input filename (path) */
   strcpy(Inpath,argv[1]);

   switch (argc)
     {
       case 2:   /* program name + input path only */
   	  strcpy(Vspath,"");
   	  break;
       case 3:   /* program name + input path and? */
   	  if (argv[2][0] == '-')   /* options */
   	    {
   		setopts(argv[2], &hdr_flag, &idx_flag, &silent_flg);
   		strcpy(Vspath,"");
   	    }
   	  else   		  /* output path name */
   	    {
   	      strcpy(Vspath,argv[2]);
   	      strcat(Vspath,"/");
   	    }
   	  break;
       case 4:
   	  setopts(argv[3], &hdr_flag, &idx_flag, &silent_flg);
   	  strcpy(Vspath,argv[2]);
   	  strcat(Vspath,"/");
   	  break;
     }

#ifdef TIMING
   /* open the time file */
   time_fd = fopen("./tmp_time","r+");
   if (time_fd == NULL)
     {
	if (errno == ENOENT)
	  {   /* file does not exist, create it */
	    istat = creat("./tmp_time", 0666);
	    if (istat < 0)
	      {
   		printf("UNABLE TO CREATE Timing file\n");
   		perror("");
   		exit(-1);
	      }
   	    time_fd = fopen("./tmp_time","r+");
     	    tot_time = 0.0;	/* file is empty so initialize total time */
	    istat = unlink("./time_record");
	    istat = creat("./time_record", 0666);
	    if (istat < 0)
	      {
   		printf("UNABLE TO CREATE Timing record file\n");
   		perror("");
   		exit(-1);
	      }
   	    record_fd = fopen("./time_record","a");
	    fprintf(record_fd, "\n DATABASE Conversion timing record for ");
   	    gettimeofday(&stp, &stzp);   /* get current time: not start time */
	    fprintf(record_fd, "%s \n\n", ctime((time_t *)&stp.tv_sec));
            
	  }
     }
   else
     {
       istat = fscanf(time_fd, "%f", &tot_time);    /* read current total */
       istat = fseek(time_fd, 0, SEEK_SET);  	    /* rewind file */
       record_fd = fopen("./time_record","a");      /* open record file */
     }
#endif
   
   /* open the input file */
   fd = fopen(Inpath,"r");
   if (fd == NULL)
     {
   	printf("UNABLE TO OPEN <%s>\n",Inpath);
#       ifdef TIMING
   	  fprintf(record_fd, "UNABLE TO OPEN <%s>\n",Inpath);
#       endif
   	perror("");
   	exit(-1);
     }

   /* read in the input header info */
   istat = fread(&Dbdef,sizeof(Dbdef),1,fd);
   if (istat != 1)
     {
   	printf("\nUNABLE TO READ HEADER RECORD FOR <%s>\n",Inpath);
#       ifdef TIMING
   	  fprintf(record_fd,"\nUNABLE TO READ HEADER RECORD FOR <%s>\n",Inpath);
#       endif
   	perror("");
   	exit(-1);
     }

   /* set some important variables */
   Reclen = atoi(strnull(4,Dbdef.hdr.recsize));
   Keypos = atoi(strnull(4,Dbdef.hdr.keypos)) - 1;
   Keylen = atoi(strnull(4,Dbdef.hdr.keylen));

   if (strncmp(Dbdef.hdr.seqno, "  ",2))
     Seqno = atoi(strnull(2,Dbdef.hdr.seqno));
   else
     Seqno = 0;
   
   if (Keypos < 0) 
      Keypos = 0;

   /* build the pathname */
   j = 8;
   while (Dbdef.hdr.filename[j-1] == ' ')
     j--;
   for (i = 0; i < j; i++)
    {
      if (isupper(Dbdef.hdr.filename[i]))
         Dbdef.hdr.filename[i] = tolower(Dbdef.hdr.filename[i]);
    }
   Dbdef.hdr.filename[i] = '\0';
   strcat(Vspath, Dbdef.hdr.filename);

   if (Seqno == 0)
     {
       printf("\n>>>>> Converting %s to %s\n", Inpath, Vspath);
#      ifdef TIMING
         fprintf(record_fd, "\n>>>>> Converting %s to %s\n", Inpath, Vspath);
#      endif
     }
   else
     {
       printf("\n>>>>> Converting Fragment %d of %s from %s\n",
    	                      Seqno, Vspath, Inpath);
#      ifdef TIMING
         fprintf(record_fd, "\n>>>>> Converting Fragment %d of %s from %s\n",
	                      Seqno, Vspath, Inpath);
#      endif
     }

   if(hdr_flag != OFF)
	headdef(&Dbdef);	/* list header info for DBUG */
   
   keydef(&Dbdef, Alt_tmp, &N_alts, idx_flag);	/* key info to Alt_tmp & list */
   
   if (hdr_flag == ON || idx_flag == ON)
     {   /* this was a dbug run - close the files gracefully */
        fclose(fd);
   	exit(0);
     }

   if (Seqno > 1)
     {  /* working on a fragment other than the first just open the file */
       isfd = isopen(Vspath, ISINOUT+ISEXCLLOCK);
       if (isfd < 0)
         {
           printf("Unable to open file %s when expecting to extend.\n",Vspath);
#       ifdef TIMING
           fprintf(record_fd, 
                 "Unable to open file %s when expecting to extend.\n",Vspath);
#       endif
           exit(-1);
         }
       
       /* Check if sequential file and adjust k_nparts so recsize calc'ed ok */
       if (Keypos == 0 && Keylen == 0)
         keys.k_nparts = 0;
       else
         keys.k_nparts = 1;

#      ifdef TIMING
         gettimeofday(&stp, &stzp);     /* get start time */
#      endif 
     }
   else
     {  /* full file or first of sequence */
	
       istat = iserase(Vspath);	/* delete the .dat and .idx files */
       if (istat < 0 && iserrno != ENOENT)
         {
           perror(Vspath);
           exit(-1);
         }
	
        /* create the DISAM Files */
       keys.k_flags = ISNODUPS;
       if (Keypos == 0 && Keylen == 0)
         keys.k_nparts = 0;
       else
         keys.k_nparts = 1;
       keys.k_part[0].kp_start = Keypos;
       keys.k_part[0].kp_leng  = Keylen;
       keys.k_part[0].kp_type  = CHARTYPE;

       isfd = -1;	/* set to neg to flag not tried yet */
       isfd = isbuild(Vspath, Reclen, &keys, ISOUTPUT+ISEXCLLOCK);
       if (isfd < 0)
         { /* create failed */
	   printf("\nUNABLE TO CREATE %s - DISAM ERROR %d\n", Vspath, iserrno);
#          ifdef TIMING
	      fprintf(record_fd, "\nUNABLE TO CREATE %s - DISAM ERROR %d\n", 
                  Vspath, iserrno);
#          endif
    	   exit(-1);
	 }
 
#      ifdef TIMING
         gettimeofday(&stp, &stzp);     /* get start time */
#      endif

       /* add the alternate indexes */
       for(i = 0; (i < 16 && i < N_alts); i++)
         {
	   for (k = 0; k < NPARTS; k++)
	     { /* initialize all parts */
	     keys.k_part[k].kp_start = 0;
	     keys.k_part[k].kp_leng  = 0;
	     keys.k_part[k].kp_type  = 0;
	     }

     	   if (Alt_tmp[i].len == 0)	/* no alt key defined */
    	     {
   		keys.k_flags  = ISDUPS;
   		keys.k_nparts = 1;
   		keys.k_len    = 1;
   		keys.k_part[0].kp_start = 0;
   		keys.k_part[0].kp_leng  = 1;
   	     }
   	   else
   	     {
   		keys.k_flags = Alt_tmp[i].dup;
   		keys.k_nparts = 1;
   		keys.k_part[0].kp_start = Alt_tmp[i].pos;
   		keys.k_part[0].kp_leng  = Alt_tmp[i].len;
   		keys.k_len              = Alt_tmp[i].len;
		if (keys.k_flags == ISDUPS)
		   {
   		    keys.k_nparts = 2;
   	            keys.k_part[1].kp_type  = CHARTYPE;
   		    keys.k_part[1].kp_start = Keypos;
   		    keys.k_part[1].kp_leng  = Keylen;
		   }
   	     }

   	   keys.k_part[0].kp_type  = CHARTYPE;
   	   keys.k_rootnode         = 0;

   	   istat = isaddindex(isfd,&keys);
           if (istat < 0)
   	     {
   		printf("**** isaddindex error - iserrno = %d\n", iserrno);
#               ifdef TIMING
   		  fprintf(record_fd, "**** isaddindex error - iserrno = %d\n", 
                      iserrno);
#               endif
                isclose(isfd);
                iserase(Vspath);
                exit(-1);
   	     }
         }
     }
   /* start processing the records here */
   if (keys.k_nparts == 0)
     rlen = Reclen;
   else
     rlen = Reclen + REC_END;
   rbeg = rlen * (400 / rlen);
   if (rbeg < 400)
   	rbeg += rlen;
   
   /* set the file pointer to rbeg offset */
   istat = fseek(fd, (long)rbeg, 0);
   if (istat != 0)
     {
   	perror("Seek on input file");
   	exit(-1);
     }

   br_no = 0;	/* bad record count */
   /* read the input records and DISAM the output */
   for(rcnt = 0;;)
     {
   	istat = fread(Recbuf, rlen, 1, fd);  /* read 1 buffer of rlen bytes */
   	if (istat != 1)
   	   break;
   	istat = iswrite(isfd, Recbuf);   /* write same buffer to DISAM */
   	if (istat < 0)
   	  {
	    br_no++;
   	    printf("DISAM ISAM WRITE ERROR - %d on input record %d\n", 
					    iserrno, rcnt+br_no);
	    if (silent_flg == ON)
	      continue;
            else
	      {
	        printf("\n No You wish to continue(Y/N)? [N]:");
	        scanf("%s", answ);
	        if ((answ[0] == 'y') || (answ[0] == 'Y'))
	          {
   		    printf("  Note: timing record has been effected!\n");
	            continue;
                  }
                else
   		    break;
              }
   	  }
   	rcnt++;
     }
#ifdef TIMING
   gettimeofday(&ftp, &ftzp);      /* get finish time */
  fprintf(record_fd, "\nRECORDS EXPECTED: %-8ld",
                     (long)atol(strnull(8,Dbdef.frag_numrec)));
  fprintf(record_fd, "\nRECORDS WRITTEN:  %-8ld", rcnt);
  if (br_no > 0)
    fprintf(record_fd, "\n  Encountered %d errors writing file!", br_no);
#endif
  printf("\nRECORDS EXPECTED: %-8ld", (long)atol(strnull(8,Dbdef.frag_numrec)));
  printf("\nRECORDS WRITTEN:  %-8ld", rcnt);
  if (br_no > 0)
    printf("\n  Encountered %d errors writing file!", br_no);
   if (Seqno != 0)
     {
       istat = isindexinfo(isfd, (struct keydesc *)&dict_info, 0);
       if (istat == 0) 
         {
	   printf("\nFile Total Records Expected: %-8ld",
	 	                     (long)atol(strnull(8,Dbdef.hdr.numrecs)));
     	   printf("\nCurrent File Total Records: %-8ld",dict_info.di_nrecords);
#ifdef TIMING
	   fprintf(record_fd, "\nFile Total Records Expected: %-8ld",
	 	                     (long)atol(strnull(8,Dbdef.hdr.numrecs)));
     	   fprintf(record_fd, "\nCurrent File Total Records: %-8ld",
				      dict_info.di_nrecords);
#endif
	 }
     }	

#ifdef TIMING
   elap_time = (ftp.tv_sec + (ftp.tv_usec * 0.000001)) - 
     	       (stp.tv_sec + (stp.tv_usec * 0.000001));
   printf("\nTime elapsed %f seconds ", elap_time);
   fprintf(record_fd, "\nTime elapsed %f seconds ", elap_time);
   if (br_no > 0)
     {
       printf("\nTiming effected by errors!");
       fprintf(record_fd, "\nTiming effected by errors!");
     }
					   
   if ((isclose(isfd) == 0) && (iserrno == 0))
     printf("\n File Convertion Complete! \n");
     fprintf(record_fd, "\n File Convertion Complete! \n");

   /* close the files gracefully */
   fclose(fd);
   tot_time += elap_time;
   fprintf(time_fd, "%f", tot_time);
   fclose(time_fd);
   fprintf(record_fd, "%s converted in %f seconds\n",
		 strnull(j, Dbdef.hdr.filename), elap_time);
   fclose(record_fd);
#endif
  printf("\n");
}

char *strnull(n, str)      /* place a null at end of n characters */
  int n;
  char *str;
{
   static char tmp[80];
   char *ret;

  memcpy(tmp,str,n);
  tmp[n] = 0;
  ret = tmp;
  return(ret);
}


/*************************************************************************
 *                                                                       *
 *                                                                       *
 *  print the File header information                                    *
 *                                                                       *
 *                                                                       *
 ************************************************************************/
headdef(struct hdr_block *Dbdef)
{
   printf("\n");
   printf("\nHEADER INFORMATION");
   printf("\n==================");
   printf("\n");
   printf("Field Name   Contents\n");
   printf("-----------  --------\n");
   printf("FILENAME     %8s\n",strnull(8,Dbdef->hdr.filename));
   printf("LIBRARY      %8s\n",strnull(8,Dbdef->hdr.library));
   printf("VOLUME       %6s\n",strnull(6,Dbdef->hdr.volume));
   printf("FILE TYPE    %1s\n",strnull(1,Dbdef->hdr.filtyp));
   printf("CREATOR ID   %3s\n",strnull(3,Dbdef->hdr.crtid));
   printf("CREATED      %6s\n",strnull(6,Dbdef->hdr.crtdat));
   printf("EXPIRES      %6s\n",strnull(6,Dbdef->hdr.expdat));
   printf("MODIFIED     %6s\n",strnull(6,Dbdef->hdr.moddat));
   printf("RECSIZE      %4s\n",strnull(4,Dbdef->hdr.recsize));
   printf("KEY POS.     %4s\n",strnull(4,Dbdef->hdr.keypos));
   printf("KEY LEN      %4s\n",strnull(4,Dbdef->hdr.keylen));
   printf("NO ALTKEY    %4s\n",strnull(4,Dbdef->hdr.numalt));
   printf("NUM RECS     %8s\n",strnull(8,Dbdef->hdr.numrecs));
   printf("SEQ NUM      %2s\n",strnull(2,Dbdef->hdr.seqno));
   printf("FRAG NUM REC %8s\n",strnull(8,Dbdef->frag_numrec));
   printf("\n");
   printf("\nHit ENTER to continue: ");
   getchar();
}


/*************************************************************************
 *                                                                       *
 *                                                                       *
 * dump the alternate key info                                           *
 *                                                                       *
 *                                                                       *
 ************************************************************************/
keydef(struct hdr_block *Dbdef, struct alt_keys Alt_tmp[], int *N_alts, int idx_flag)
{
   int i,j,k;
   char num[3], pos[5], len[5], dup[2];


   /* initialize the Alt_tmp array */
   memset(Alt_tmp, 0, sizeof(Alt_tmp));
   
   k = atoi(strnull(4, Dbdef->hdr.numalt));
   for(j = -1, i = 0; i < k; i++)
     {
   	strcpy(num, strnull(2, Dbdef->alt[i].num));
   	strcpy(pos, strnull(4, Dbdef->alt[i].pos));
   	strcpy(len, strnull(4, Dbdef->alt[i].len));
   	strcpy(dup, strnull(1, Dbdef->alt[i].dup));

   	/* now stuff it into the Alt_tmp array */
   	j = atoi(num) - 1;
   	Alt_tmp[j].pos = atoi(pos) - 1;
   	Alt_tmp[j].len = atoi(len);
   	Alt_tmp[j].dup = (dup[0] == 'D') ? ISDUPS : ISNODUPS;
     }
   *N_alts = j + 1;

   if(idx_flag == ON)
     {
	if (*N_alts == 0)
	   printf("\nNo Alternate Keys\n");
        else
          {
   	    printf("\nALTERNATE KEY INFO");
   	    printf("\n==================\n");
   	    printf("\nNO  POS   LEN  D");
   	    printf("\n--  ----  ---- -\n");
   	    for(i = 0; i < *N_alts; i++)
   	      {
   	        strcpy(num,strnull(2, Dbdef->alt[i].num));
   	        strcpy(pos,strnull(4, Dbdef->alt[i].pos));
   	        strcpy(len,strnull(4, Dbdef->alt[i].len));
   	        strcpy(dup,strnull(1, Dbdef->alt[i].dup));
   	        printf("%2s  %4s  %4s %1s\n", num, pos, len, dup);
   	      }
          }
   	printf("\nHit ENTER to continue: ");
   	getchar();
     }
}


/*************************************************************************
 *                                                                       *
 *                                                                       *
 *   Read options from input arg and set global flags                    *
 *                                                                       *
 *                                                                       *
 ************************************************************************/
setopts(arg, hdr_flag, idx_flag, silent_flg)
  char *arg;
  int *hdr_flag, *idx_flag, *silent_flg;
{
   int i;
   char ch;

  for(i = 1; i < strlen(arg); i++)
    {
      switch(ch=arg[i])
	{
	  case 'h':
	  case 'H':
		*hdr_flag = ON;
		break;
	  case 'i':
	  case 'I':
		*idx_flag = ON;
		break;
	  case 'q':
	  case 'Q':
		*silent_flg = ON;
		break;
	  default:
		break;
	}
    }
  return;
}

