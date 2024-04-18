/*****************************************************************************
                   C A E L U S   P R O G R A M   M O D U L E
                    (c)Copyright 1991 CAELUS, Incorporated
                     A L L   R I G H T S   R E S E R V E D
==============================================================================
                           D E S C R I P T I O N
------------------------------------------------------------------------------
	U2isam - convert a flat UNIX file to a D-isam sequential file
		 Input parameters:
			input filename  - UNIX file to convert
			record size     - size of records in original and 
                                                         converted file
                        output filename - the d-isam file created
               To compile use:
                    cc u2isam.c -o u2isam -I/cms/currel/include2
                                  -L/cms/currel/load2 -ldisam

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
	05/24/93	MDH	Original 
	07/08/93	MDH	Make it work if input file is not fixed length
				pad to record size

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/
#include <stdio.h>
#include <errno.h>
#include <sys/file.h>
#include "disam.h"
#define EOL 0x0a

main(argc, argv)
 int argc;		/* number of parameters */
 char **argv;		/* pointer to array of parameters */
{
  /* UNIX file variables */
  int   ufd;		/* UNIX file descriptor */
  char  in_file[80];    /* input file path */
  char  *buf;           /* pointer to buffer area - dynamic allocated */

  /* D-isam file variables */
  int    dfd;		/* D-isam file descriptor */
  char   out_file[80];  /* output file path */
  struct keydesc key;   /* disam key structure for file */
  int    flags;         /* open flags  for disam file */

  /* common variables */
  int   rec_size;       /* record size */
  int   i;		/* local: index */
  char  c;		/* local: character read from file */

  /* check inputs */
  if (argc != 4)
    {
      printf("USAGE: u2isam input_file recsize output_file\n");
      printf("        Where input_file is the name of the input file,\n");
      printf("              recsize is the size of the input records.\n");
      printf("              output_file is the name of the file created.\n");
      exit(-1);
    }

  strcpy(in_file, argv[1]);	/* get input file name */
  rec_size = atoi(argv[2]);	/* get record size */
  strcpy(out_file, argv[3]);	/* get output file name */

  printf("Convert file %s to %s: recsize = %d\n", in_file, out_file, rec_size);

  /* open UNIX file */
  ufd = open(in_file, O_RDONLY);
  if (ufd < 0)
    {
      printf("Unable to open input file %s: error = %d\n",in_file, errno);
      exit(-1);
    }

  /* create and open D-isam file */
  key.k_flags = ISDUPS;
  key.k_nparts =  0;
  key.k_part[0].kp_start = 0;
  key.k_part[0].kp_leng  = 0;
  key.k_part[0].kp_type  = CHARTYPE;
  flags = ISOUTPUT + ISEXCLLOCK;

  dfd = isbuild (out_file, rec_size, &key, flags);
  if (dfd < 0)
    {
      printf("Unable to create disam file %s: disam error = %d\n", 
                              out_file, iserrno);
      close(ufd);
      exit(-1);
    }

  /* allocate transfer buffer */
  buf = (char *) malloc(rec_size);
  if (buf == NULL)
    {
      printf("Unable to buffer space for transfer\n");
      close(ufd);
      isclose(dfd);
      exit(-1);
    }
 
  /* transfer data */
  i = 0;
  while (read(ufd, &c, 1) == 1)
    {
      if (c == EOL)
        { /* end of record */
          while (i < rec_size)
            buf[i++] = 0x20;     /* space fill record */
          iswrite(dfd, buf);
          i = 0;
        }
      else
          buf[i++] = c;
      if (i > rec_size)
        {
         printf("RECORD SIZE ERROR: Current record: %d > %d\n",i,rec_size);
         exit(-1);
        }
    }


  /* clean up */
  close(ufd);
  isclose(dfd);
  free(buf);

}
