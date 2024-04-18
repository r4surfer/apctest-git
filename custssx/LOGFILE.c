#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "symbol.h"


LOGFILE(INFX)      
struct symbol *INFX;
{ 
    FILE *logfile;

    logfile=fopen("destmp.dat","a");
    INFX->p[255]=0x00; 
    fprintf(logfile,"%s|\n",INFX->p);
    fclose(logfile);

}
