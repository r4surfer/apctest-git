/********************************************************/
/* Utility program to manipulate output of ls > flist 	*/
/* into a format suitable for embedding in a makefile 	*/
/* optional arguments allow control of:               	*/
/*       suffixes passed                              	*/
/*       prefix addition                              	*/
/*       dropping leading characters                  	*/
/*       common suffix addition                       	*/
/*       designating input file name                  	*/
/*       designating output file name                 	*/
/* Maximum output length is 14, with 5 per line       	*/
/*						      	*/
/* Default is list for source -> exe or .o		*/
/* Parameters allow lists for .o -> .sl, .sh, .a, exe	*/
/*  load library lists, and explicit .o lists for       */
/*  static linkage & more				*/
/*							*/
/********************************************************/

#include <stdio.h>
#include <string.h>

int rev=1,patch=0;	/* Revision, Patch level	*/

char in[100];		/* Input file string 		*/
char out[100];		/* Output file string 		*/

char bas[20];		/* Suffix to pass 		*/
char ccc[20];		/* Suffix to pass 		*/
char csv[20];		/* Suffix to pass 		*/
char sav[20];		/* Suffix to pass 		*/
char ooo[20];		/* Suffix to append 		*/
char ppp[20];		/* Prefix to append 		*/

int bln;		/* Suffix length 		*/
int cln;		/* Suffix length 		*/
int vln;		/* Suffix length 		*/
int sln;		/* Suffix length 		*/
int oln;		/* Suffix length 		*/
int pln;		/* Suffix length 		*/
int cut;		/* # char to drop 		*/


main(argc, argv)
	int	argc;
	char	*argv[];

{
FILE *fp1;		/* Input file			*/
FILE *fp2;		/* Output file			*/

char *loc;		/* Location of '.'		*/

char buf1[40];		/* Input record			*/
char buf2[100];		/* Output record		*/
char buf3[20];		/* Work Area			*/

int ch;			/* Search character '.'		*/
int pos;		/* Current output position	*/
int tmp;		/* Work Variable		*/

unsigned char eor;	/* Pick up end of record from input */

decode_args(argc, argv);	/* Decode optional arguments */

memset(buf3, 32, 20);	/* Initialize output		*/
memset(buf2, 0, 100);
strncat(buf2, buf3, 2);
pos = 2;

ch = 0x2e;		/* char '.'	*/

/* Open files, error if not possible 	*/

if ((fp1 = fopen(in, "r")) == NULL)
	{
	printf ("\nUnable to open input: %s", in);
	printf ("\nEnter %s [-h or help] for help\n",argv[0]);
	exit(1);
	}

if ((fp2 = fopen(out, "w")) == NULL)
	{
	printf ("Unable to open output: %s", out);
	printf ("\nEnter %s [-h or help] for help\n",argv[0]);
	exit(1);
	}

/* Process input file			*/

while(fgets(buf1, 40, fp1) != NULL)
	{
	loc = strchr(buf1, ch);

	if (loc != NULL)
		{
		tmp = strlen(buf1);	/* Pick up eor from input */
		eor = buf1[tmp-1];
		bas[bln] = eor;
		ccc[cln] = eor;
		csv[vln] = eor;
		sav[sln] = eor;

/* check suffix, including eor so as not to pass partials */

		if ((strncmp(loc,bas,bln+1) == 0) |
		    (strncmp(loc,ccc,cln+1) == 0) |
		    (strncmp(loc,csv,vln+1) == 0) |
		    (strncmp(loc,sav,sln+1) == 0))
			{
			if (pos > 70)	/* write buffer if full */
				{
				buf2[pos] = 0x20;	/* got one, */
				buf2[pos+1] = 0x5c;	/* so need  */
				buf2[pos+2] = eor;	/* cont char*/
				fputs (buf2, fp2);	/* & eor    */
				memset(buf2, 0, 100);   /* re - init*/
				strncat(buf2, buf3, 2);
				pos = 2;
				}
			if (pln != 0)	/* process prefix 	*/
			  {
			  if (pln > 14)
			    {
			    printf ("\nPrefix too long %s\n",ppp);
			    exit(1);
			    }
			  strcat(buf2, ppp);
			  }
			tmp = loc - buf1;	/* Process input file name */
			if (tmp - cut < 1)	/* cut chars, not too many */
			  {
			  printf ("\nCut too long %d, %s\n",cut,buf1);
			  exit(1);
			  }
			tmp = tmp - cut;
			if (pln + tmp > 14)	/* dont overflow */
			  {
			  printf ("\nPrefix/file too long %s,%s\n",ppp,buf1);
			  exit(1);
			  }
			strncat(buf2, buf1+cut, tmp);
			if (oln != 0)		/* Process suffix */
			  {
			  if (pln + oln + tmp > 14)	/* dont overflow */
			    {
			    printf ("\nSuffix too long %s, %s\n",ooo,buf2);
			    exit(1);
			    }
			  strncat(buf2, ooo, oln);
			  tmp = tmp + oln;
			  }
			tmp = tmp + pln;
			strncat(buf2, buf3, 15 - tmp); /* space fill */
			pos = pos +15;
			}

		}
	}

/* Done processing, clean up & close 		   */

if (pos > 2)		/* write last if not empty */
	{
	buf2[pos] = eor;
	fputs(buf2, fp2);
	}
fclose (fp1);		/* close & go	*/
fclose (fp2);
exit(0);
}

decode_args (argc, argv)
	int	argc;
	char	*argv[];

{
int tmp;
char *tmpc;
char *name;

/* set initial values */

strcpy(in, "./flist1.txt");
strcpy(out,"./flist2.txt");

memset(bas,0,20);
memset(ccc,0,20);
memset(csv,0,20);
memset(sav,0,20);
strncpy(bas, ".bas",4);
strncpy(ccc, ".c",2);
strncpy(csv, ".csv",4);
strncpy(sav, ".c.sav",6);
memset(ooo,0,20);
memset(ppp,0,20);

bln = strlen(bas);
cln = strlen(ccc);
vln = strlen(csv);
sln = strlen(sav);
oln = 0;
pln = 0;
cut = 0;

if (argc > 1)				/* any arguments? */
	{
	if ((strncmp(argv[1],"help",4) == 0) ||
	    (strncmp(argv[1],"-h",2) == 0))
		{
		name = argv[0];
		explain_args(name);	/* explain myself */
		exit (0);
		}


	for (tmp = 1; tmp < argc; tmp++)
		{
		if (strncmp(argv[tmp],"-i",2) == 0)	/* input file */
			{
			tmpc = argv[tmp];
			tmpc = tmpc + 2;	/* here or next word */
			if (strncmp(tmpc, "\0",1) == 0)
				{
				tmpc = NULL;
				if (tmp < argc - 1)
					{
					if (strncmp(argv[tmp+1],"-",1) != 0)
						{
						tmpc = argv[++tmp];
						}
					}
				}
			if (tmpc != NULL)
				{
				memset (in, 0, 80);
				strcpy(in,tmpc);
				}
			}

		if (strncmp(argv[tmp],"-o",2) == 0)	/* output file */
			{
			tmpc = argv[tmp];
			tmpc = tmpc + 2;	/* here or next word */
			if (strncmp(tmpc, "\0",1) == 0)
				{
				tmpc = NULL;
				if (tmp < argc - 1)
					{
					if (strncmp(argv[tmp+1],"-",1) != 0)
						{
						tmpc = argv[++tmp];
						}
					}
				}

			if (tmpc != NULL) 
				{
				memset (out, 0, 80);
				strcpy(out,tmpc);
				}
			}

		if (strncmp(argv[tmp],"-s",2) == 0)	/* suffixes */
			{
			tmpc = argv[tmp];
			tmpc = tmpc + 2;	/* here or next word */
			if (strncmp(tmpc, "\0",1) == 0)
				{
				tmpc = NULL;
				if (tmp < argc - 1)
					{
					if (strncmp(argv[tmp+1],"-",1) != 0)
						{
						tmpc = argv[++tmp];
						}
					}
				}
			if (tmpc != NULL) 
				{
				memset (bas, 0, 20);	/* init all */
				memset (ccc, 0, 20);
				memset (csv, 0, 20);
				memset (sav, 0, 20);
				bln = 0;
				cln = 0;
				vln = 0;
				sln = 0;

				bas[0] = 0x2e;
				strcat(bas, tmpc);
				bln = strlen(bas);
				tmpc = NULL;

				if (tmp < argc - 1)	/* another? */
					{
					if (strncmp(argv[tmp+1],"-",1) != 0)
						{
						tmpc = argv[++tmp];
						}
					}
				}
			if (tmpc != NULL) 
				{
				ccc[0] = 0x2e;
				strcat(ccc, tmpc);
				cln = strlen(ccc);
				tmpc = NULL;

				if (tmp < argc - 1)	/* another? */
					{
					if (strncmp(argv[tmp+1],"-",1) != 0)
						{
						tmpc = argv[++tmp];
						}
					}
				}
			if (tmpc != NULL) 
				{
				csv[0] = 0x2e;
				strcat(csv, tmpc);
				vln = strlen(csv);
				tmpc = NULL;

				if (tmp < argc - 1)	/* another? */
					{
					if (strncmp(argv[tmp+1],"-",1) != 0)
						{
						tmpc = argv[++tmp];
						}
					}
				}
			if (tmpc != NULL) 
				{
				sav[0] = 0x2e;
				strcat(sav, tmpc);
				sln = strlen(sav);
				}
			}

		if (strncmp(argv[tmp],"-x",2) == 0)	/* output suffix */
			{
			tmpc = argv[tmp];
			tmpc = tmpc + 2;	/* here or next word */
			if (strncmp(tmpc, "\0",1) == 0)
				{
				tmpc = NULL;
				if (tmp < argc - 1)
					{
					if (strncmp(argv[tmp+1],"-",1) != 0)
						{
						tmpc = argv[++tmp];
						}
					}
				}
			if (tmpc != NULL)
				{
				memset (ooo, 0, 20);
				/* ooo[0] = 0x2e; */
				strcat(ooo,tmpc);
				oln = strlen(ooo);
				}
			}

		if (strncmp(argv[tmp],"-p",2) == 0)	/* output prefix */
			{
			tmpc = argv[tmp];
			tmpc = tmpc + 2;	/* here or next word */
			if (strncmp(tmpc, "\0",1) == 0)
				{
				tmpc = NULL;
				if (tmp < argc - 1)
					{
					if (strncmp(argv[tmp+1],"-",1) != 0)
						{
						tmpc = argv[++tmp];
						}
					}
				}
			if (tmpc != NULL)
				{
				memset (ppp, 0, 10);
				strcat(ppp, tmpc);
				pln = strlen(ppp);
				}
			}
		if (strncmp(argv[tmp],"-c",2) == 0)	/* cut chars */
			{
			tmpc = argv[tmp];
			tmpc = tmpc + 2;	/* here or next word */
			if (strncmp(tmpc, "\0",1) == 0)
				{
				tmpc = NULL;
				if (tmp < argc - 1)
					{
					if (strncmp(argv[tmp+1],"-",1) != 0)
						{
						tmpc = argv[++tmp];
						}
					}
				}
			if (tmpc != NULL)
				{
				cut = 0;
				cut = tmpc[0] & 0x0f;	/* yup, 0 - 15 */
				}
			}
		}

	if (strcmp (in, out) == 0)
		{
		printf ("\nArgument list error, input = output");
		exit (1);
		}
	}
return(0);
}

explain_args(name)
	char	*name;

{
printf ("\n%s: [-i -o -s -x -p -c -h help] (revision: %d.%d)", name,rev,patch);
printf ("\n");
printf ("\nParses output of ls > flist1 to flist2");
printf ("\ninto a format suitable for embedding in makefiles.");
printf ("\n [five names per line with continuation if needed.]");
printf ("\n");
printf ("\nOptional Arguments:");
printf ("\n  -i input file  [default = flist1.txt]");
printf ("\n  -o output file [default = flist2.txt]");
printf ("\n  -s suffixes to pass [default .bas .c .csv .c.sav]");
printf ("\n     Warning: Files without suffixes will not be passed.");
printf ("\n              Up to four suffixes may be passed.");
printf ("\n              Only defaults or explicit lists are passed.");
printf ("\n              [-s bas c csv c.sav == defaults]");
printf ("\n              [-s bas <or> -sbas == .bas only]");
printf ("\n              [-s bas c <or> -sbas c == .bas or .c only]");
printf ("\n");
printf ("\nFiles are passed without suffix or prefix unless:");
printf ("\n");
printf ("\n  -x suffix to append to file name [default = none]");
printf ("\n  -p prefix to append to file name [default = none]");
printf ("\n  -c# No. of char to cut from front of file name [default = none]");
printf ("\n");

return(0);
}
