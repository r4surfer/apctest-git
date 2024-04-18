#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "symbol.h"


LOGFILE2(INFX,LOGFL)      
struct symbol *INFX;
struct symbol *LOGFL;
{ 
    FILE *logfile;
    int l;
    char flnm[256];
    memcpy(flnm,LOGFL->p,255);
    flnm[255]='\0'; 
    for(l=254;l>0;l--) {
        if (flnm[l]<=' ')
            flnm[l]='\0'; 
	else
	    l=0;
    }
    logfile=fopen(flnm,"a");   
    INFX->p[255]=0x00; 
    fprintf(logfile,"%s|\n",INFX->p);
    fclose(logfile);

}
