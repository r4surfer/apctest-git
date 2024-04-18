/*****************************************************************************
                 C A E L U S   P R O G R A M   M O D U L E
                      (c)Copyright 1991 CAELUS, Inc.
                   A L L   R I G H T S   R E S E R V E D
==============================================================================
                   M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------

	RELDELS - remove deleted records utility.  This routine will
	       accept WANG style file information file, library, volume for
               the source file or if they are not supplied, a getparm will be
               issued.  Wb_open is used to validate existance and access to the
               the files.
               Once the file has been opened the D-ISAM iscluster routine is 
               used to rewrite & pack the file.
 
              Command line parameters are assumed to be in the following order:
			source file name
			source library name
			source volume name	

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
	03/15/93	MDH	Original

*****************************************************************************/

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "disam.h"
#include "file.h"
#include "wisp.h"

int Call_args;			 /* number of args used by wb_open */

main(argc, argv)
 int  argc;
 char **argv;
{
   char infilen[9], inlib[9], invol[7]; /* original file info */
   long fil_mode;           		/* mode for D-ISAM call for pathname */
   struct keydesc key;			/* D-ISAM key descriptor */
   int  resp;
   int  del_cnt;			/* number of deleted records in file */

   struct ufb in_ufb;
   static struct symbol *allvar[]={NULL};
   int Iflag = 0;

   wb_init (&Iflag, allvar, argv[0]);	/* init for main program */
   /* blank fill name info  & set flag to no got */
   memset(infilen, ' ', 8);
   memset(inlib, ' ', 8);
   memset(invol, ' ', 6);
   memset(in_ufb, 0, sizeof(in_ufb));
   in_ufb.fd = -1;
   
   /* check argument list to file info */
   switch (argc)
     {
      case 4:			/* source library passed in - grab it */
          strncpy(invol, argv[3], 6);
          inlib[6] = 0;
          /* fall through and get the rest */
      case 3:			/* source volume passed in - grab it */
          strncpy(inlib, argv[2], 8);
          inlib[8] = 0;
          /* fall through and get the rest */
      case 2:			/* source file passed in - grab it */
          strncpy(infilen, argv[1], 8);
          infilen[8] = 0;
          break;

      default:
          ;
     }
  
   /* open original file */
   fil_mode = XIO;   /* use exclusive lock */
   in_ufb.filetype = XINDEXED;
   resp = wb_open(&in_ufb, 0, 0, fil_mode, 0,0,0,0,0, infilen, inlib, invol);
  
   if ((del_cnt = ck_del(&in_ufb)) > 0)   /* check if any deleted records */
     {
       printf("Removing %d records from %s\n",del_cnt, in_ufb.path);
       resp = isindexinfo(in_ufb.fd, &key, 1);
       if (resp != 0)
           printf("Error getting key info: %d\n", iserrno);
       else
         {  /* do repack */
           resp = iscluster(in_ufb.fd, &key);
           if (resp < 0)
             printf("Error clustering file: %d\n", iserrno);
           else
             in_ufb.fd = resp;
         }
     }
   else
     printf("No deleted records in %s\n", in_ufb.path);

   resp = wb_close(0, &in_ufb);		/* close file */
   wb_end(0);
}


int ck_del(pufb)
 struct ufb *pufb;
{

  struct dictinfo dict;		/* D-ISAM dictionary structure */
  int resp, diff;
  struct stat buf;
 
  if (fstat(isfdmap[pufb->fd]->di_datfd, &buf) != 0)
    {
      printf("Error getting file stat info: %d\n", errno);
      return(0);
    }

  resp = isindexinfo(pufb->fd, &dict, (int) 0);
  if (resp != 0)
    {
      printf("Error getting dict info: %d\n", iserrno);
      return(0);
    }

  diff = buf.st_size - ((dict.di_recsize+1) * dict.di_nrecords);
  return(diff / (dict.di_recsize+1));
}
