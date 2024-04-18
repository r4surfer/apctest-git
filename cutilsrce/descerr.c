#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "disam.h"
#include "symbol.h"

main(argc, argv)
 int argc;
 char **argv;
{
   int eno;
   char msg[80];
   void snderr();
  
   if (argc != 2) 
     {
       printf("Enter error number: ");
       scanf("%d%*c", &eno);
     }
   else
     {
       eno = atoi(argv[1]);
     }
   if (eno < 100)	  /* UNIX system errors */
     sprintf(msg, "UNIX ERROR:(%d) - %s\n", eno, strerror(eno));
   else   /* Disam errors */
     sprintf (msg, "DISAM ERROR(%d) - %s\n", eno, is_errlist[eno-100]);

   printf("\n %s\n", msg);
   printf("\n  Press any key to continue:");
   scanf("%*c");
}
