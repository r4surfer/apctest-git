/*****************************************************************************
                 C A E L U S   P R O G R A M   M O D U L E
                      (c)Copyright 1991 CAELUS, Inc.
                   A L L   R I G H T S   R E S E R V E D
==============================================================================
                   M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------

 
              Command line parameters are assumed to be in the following order:
			source file name
			source library name
			source volume name	

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
	06/10/93	MDH	Adopted from REMDELS

*****************************************************************************/

#include <stdio.h>
#include "disam.h"
#include "file.h"
#include "wisp.h"

int Call_args;			 /* number of args used by wb_open */

main(argc, argv)
 int  argc;
 char **argv;
{
   char *trans_buf;			/* buffer for data transfer */
   char infilen[9], inlib[9], invol[7]; /* original file info */
   char tmpfilen[9];			/* temperary file info */
   long fil_mode;           		/* mode for D-ISAM call for pathname */
   int  resp;

   struct ufb in_ufb, tmp_ufb;
   static struct symbol *allvar[]={NULL};
   int Iflag = 0;

   wb_init (&Iflag, allvar, argv[0]);	/* init for main program */
   /* blank fill name info  & set flag to no got */
   memset(infilen, ' ', 8);
   memset(inlib, ' ', 8);
   memset(invol, ' ', 6);
   memset(in_ufb, 0, sizeof(in_ufb));
   memset(tmp_ufb, 0, sizeof(tmp_ufb));
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
          printf("USAGE: reset_alt file library volume\n");
          exit(-1);
     }
  
   /* open original file */
   fil_mode = XINPUT;
   in_ufb.filetype = XINDEXED;
   resp = wb_open(&in_ufb, 0, 0, fil_mode, 0,0,0,0,0, infilen, inlib, invol);
  
   /* set up and open temperary file */
   strcpy(tmpfilen, "rdtmp", 8);
   fil_mode = XOUTPUT;
   memcpy(&tmp_ufb, &in_ufb, sizeof(struct ufb));
   strcpy(in_ufb.filenam, tmpfilen);
   tmp_ufb.fd = -1;

   resp = wb_open(&tmp_ufb, 0, 0, fil_mode, 0,0,0,0,0, tmpfilen, inlib, invol);

   trans_buf = (char *) malloc(in_ufb.recsize);

   while ((resp = isread(in_ufb.fd, trans_buf, ISNEXT)) == 0)
     {
       resp = iswrite(tmp_ufb.fd, trans_buf);
       if (resp)
         printf("Error: iswrite returned %d\n", resp);
     }

   resp = isclose(in_ufb.fd);		/* close both files */
   resp = isclose(tmp_ufb.fd);
   if (iserase(in_ufb.path) != 0)        /* remove original file */
     wb_iserr("Unable to remove original file",in_ufb.path);
   if (isrename(tmp_ufb.path, in_ufb.path) != 0)    /* rename temp file */
     wb_iserr("Unable to rename temp file", tmp_ufb.path);
   wb_end(0);
}
