#include "wfname.h"
#include "wisp.h"
int Call_args;
main(c, v)
  int c; char **v;
{
  long mode; 
  char vol[7], lib[9], file[9], native[80];
  char *p, *strchr();
  int l;

  vol[0]  = '.';	/* default them */
  lib[0]  = '.';
  file[0] = '.';
  switch (c)
    {
     case 4:
       strncpy(file, v[3], 8);
     case 3:
       strncpy(lib, v[2], 8); 
     case 2:
       strncpy(vol, v[1], 6); 
       break;
     default:
       printf("Usage: getpath [volume] [library] [file]\n");
       exit(-1);
    }
  vol[6] = 0; lib[8] = 0; file[8] = 0;
  if (vol[0] == '.') vol[0] = '\0';
  if (lib[0] == '.') lib[0] = '\0';
  if (file[0] == '.') file[0] = '\0';
  mode = 0;
  wfname(&mode, vol, lib, file, native,"");
  p = strchr(native, ' '); 
  if (p) *p = 0;
  l = strlen(native);
  
  while (native[l-1] == '/')
     native[--l] = 0;
  printf("%s \n",native);
}
