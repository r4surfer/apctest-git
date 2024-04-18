
/*
 test: Fri Feb 23 15:28:08 2007
 */

/* CALLED SUBPROGRAM */
#include "wvsb.h" 
#include <stdio.h>

SEQREAD (FUNC_TS, SEQFILE_TS, ERR_IS)
struct symbol *FUNC_TS;
struct symbol *SEQFILE_TS;
struct symbol *ERR_IS;
{


    int  tmp, recs;
    char *rc;
    char buffer[257], cmd[11];
    char filename[257];
    int x, y, a, b, c, l;
    FILE *input;
    FILE *logfile;
/* Declare and initialize the local variables */
	int g_ret;
	int g_argcnt = 0;

static struct symbol *allvar[]={
NULL};
static struct ufb *ufbptr[64]={
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL};

static int Iflag = 1;
       int G_hold;

extern int Call_args;	/* count of callers args */
int CMS_nargs = Call_args;	/* Local var to keep nargs */

/* local BCD & int pointers to callers parms */
	long *ERR_is;

/* set the Str COMs */

	g_cidx = 0L;

/* Set pointers to callers parms */
	if ((Call_args >= 3) && (ERR_IS != NULL))
		ERR_is = (long *) *( (long *) ERR_IS + 1);


	wb_init (&Iflag, allvar, NULL);	/* init for sub program */
	G_hold = G_scnt;		/* hold the stack count */
/* 000001        sub "SEQFILE" (func$,seqfile$, err%)                      */ 
/* 000002 err% = 0%*/ 
	*(ERR_is) =  0;
/* 000003*/ 
/* 000004::-----------------------------------::*/ 
    rc=0;
    x=0;
    y=0;
    l=0;
    for(x=0;x<256;x++) buffer[x]=' ';
    buffer[256]=0x00;
    logfile=fopen("destmp.log","a");
    for(l=0;x==0||l<10;l++)  {
	 cmd[l]=FUNC_TS->p[l];
	 if (FUNC_TS->p[l] <= ' ') {
	     cmd[l] = 0x00;
	     x = 1;
	 }
    }  
    cmd[04]=0x00;
    for(l=0;l<257;l++) filename[l]=' ';
    l=0;
    fprintf(logfile,"-> %s\n",cmd);   
    if (strcmp(cmd,"open") == 0) {
          for(l=0;x==0||l<10;l++)  {
              filename[l]=SEQFILE_TS->p[l];
	      if (SEQFILE_TS->p[l] <= ' ') {
	          filename[l] = 0x00;
	          x = 1;
	      }
          }  
          filename[256]=0x00;
  	/*  strcpy(filename,"test.csv.dat");  */ 
    	  if ((input=fopen(filename,"r")) == 0) {
	      fprintf(logfile,"open error\n");
	      *(ERR_is) =  1;
	   } 
    } else if (strcmp(cmd,"close") == 0) {
	  fclose(input);   
    } else if (strcmp(cmd,"read") == 0) {
  	  rc= fgets(buffer,256,input);
          for (x=0;x<256;x++)     
              if (buffer[x] < 0x20 || buffer[x] > 0x7f) buffer[x]=' ';    
	  buffer[256]=0x00;
          if (feof(input)) {
              for (x=0;x<256;x++) buffer[x]=' ';    
	      *(ERR_is) =  1;
	      fclose(input);   
	  }     
    }
      if (strcmp(cmd,"read") == 0)  {
          y = strlen(buffer);
          for(x=y;x<256;x++) buffer[x]=' ';
          for(x=0;x<256;x++) SEQFILE_TS->p[x]=buffer[x];  
      }
      fclose(logfile);
/*      str_asgn ((char *)buffer, (long) 256 , 
		(char *)(SEQFILE_TS->p), (long)(SEQFILE_TS->len));  */
/* 000006::-----------------------------------::*/ 
/* 000007 */ 
/* 000008FINISH:    end      */ 
lFINISH:	;
/* 000008 */
	G_scnt = G_hold;	/* restore stack count */
	return (0);
#ifdef END
#undef END
#endif
END:
	G_scnt = G_hold;	/* restore stack count */
	return  (0);


	/* NO GOSUBS SO NO RETURN TABLE*/
}
/* END OF test */