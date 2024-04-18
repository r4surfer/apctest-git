#include <stdlib.h>
#include <string.h>
#include "symbol.h"

SKUMAIL(INFX)      
struct symbol *INFX;
{         
    char command[90], *infx;
    int  rc, a;
    infx      = INFX->p;
    a         = INFX->len;
    infx[a]   = '\0';
    strcpy(command,"/apcsrc/apcobj/skumail ");
    strcat(command,infx);
    rc = system(command); 
    return rc;
}
