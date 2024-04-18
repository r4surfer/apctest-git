#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "symbol.h"


ADPLOG(INFX)      
struct symbol *INFX;
{ 
    FILE *logfile;

    logfile=fopen("adplog.dat","a");
    INFX->p[255]=0x00; 
    fprintf(logfile,"%s|\n",INFX->p);
    fclose(logfile);

}
