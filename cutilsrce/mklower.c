/* routine to rename files to lowercase - MDH 11-02-92 */
#include <stdio.h>
#include <sys/file.h>
#include <limits.h>
#include <errno.h>
#include <ctype.h>

main()
{

 int fl, doflag;
 int i, lcnt, ccnt;
 char ch, nfname[80], ofname[80];

 fl = open("flist", O_RDONLY, 0666);
 if (fl < 0)
   {
     printf("Can't open flist! %d\n",errno);
     exit(-1);
   }

 i = lcnt = ccnt = doflag = 0;
 memset(ofname, '\0', 80);
 memset(nfname, '\0', 80);

 while(read(fl, &ch, 1) == 1)
  {
    if (ch != '\n')
      {
        ofname[i] = ch;
        if (isupper(ch))
          {
           nfname[i++] = tolower(ch);
           if (!doflag) doflag = 1;
          }
        else
           nfname[i++] = ch;

      }
    else
      {
        if (doflag)
          {
            if (rename(ofname, nfname) != 0)
                printf("rename %s to %s failure %d\n", ofname, nfname, errno);
            ccnt++;
            doflag = 0;
          }
        lcnt++;
        i = 0;
        memset(ofname, '\0', 80);
        memset(nfname, '\0', 80);
      }
  }
 printf(" %d lines processed %d converted: error = %d\n", lcnt, ccnt, errno);

}       
