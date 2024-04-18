#include <stdlib.h>
#include <string.h>
#include "symbol.h"

ARIMAIL(INFX)      
struct symbol *INFX;
{ 
    char command[90], *infx;
    int  rc, a;
    infx      = INFX->p;
    a         = INFX->len;
    infx[a]   = '\0';
    strcpy(command,"/apcsrc/apcobj/arimail ");
    strcat(command,infx);
    rc = system(command); 
    return rc;
}
