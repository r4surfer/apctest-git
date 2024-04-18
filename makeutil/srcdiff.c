/* cc -Aa ./prog.c -lcurses  -o ./prog (HP)     */
/* cc     ./prog.c -lcurses  -o ./prog (AIX)    */
/* cc     ./prog.c -lcurses  -o ./prog (ALPHA)  */
/* cc     ./prog.c -lcursesX -o ./prog (ULTRIX) */
/*         & change include to <cursesX>        */

#include <curses.h>
#include <string.h>
#include <stdarg.h>

int xfread(FILE *, char *);
void xsetlno(int, int, char *);
void xhelp(char *);

int tablen;

main(int argc, char *argv[])

{
FILE *fpa;
FILE *fpb;

char a[640], b[640];
int i, j, k, ia, ib, redo, diff, eofa, eofb, keyreq, cnt, down;
int argoff, cmp;
char cin;
char filea[80], fileb[80];
char buf[200];
char *loc;
char *loc1;

long arec[10000];
long brec[10000];

ia=0;
ib=0;
eofa = 0;
eofb = 0;
keyreq = 0;
down = 0;
argoff = 0;
cmp = 0;
tablen = 8;

memset (a, 0x00, 640);
memset (b, 0x00, 640);

memset (filea, 0x00, 80);
memset (fileb, 0x00, 80);

if (argc > 1)
   {loc1 = argv[1];
    if (loc1[0] == '-')
      {argoff = 1;
       loc = strchr(loc1,'h');
       if (loc != NULL) xhelp(argv[0]);
       loc = strchr(loc1,'c');
       if (loc != NULL) cmp = loc[1] & 0x0f;
       loc = strchr(loc1,'t');
       if (loc != NULL) tablen = loc[1] & 0x0f;
       if (tablen < 1) tablen = 1;
      }
    }

if (argc > 1 + argoff)
     {strcpy(filea, argv[1 + argoff]);}
    else
     {printf ("\nEnter file path (a): ");
      gets (filea);
     }

if (argc > 2 + argoff)
     {strcpy(fileb, argv[2 + argoff]);}
    else
     {printf ("\nEnter file path (b): ");
      gets (fileb);
     }

if ((fpa = fopen (filea, "r")) == NULL)
   {printf ("\nCould not open: %s\n",filea);
    exit(1);
   }

if ((fpb = fopen (fileb, "r")) == NULL)
   {printf ("\nCould not open: %s\n",fileb);
    exit(1);
   }

arec[0] = ftell(fpa);
brec[0] = ftell(fpb);

for (i=0; i < 8; i++)
  {++ia;
   ++ib;
   xsetlno(ia, i, a);
   strncpy (b+(80*i), a+(80*i), 80);

   if (!eofa)
       eofa = xfread(fpa, buf);
   if (!eofa)
     {strcat (a+(80*i + 7), buf);
      arec[ia] = ftell(fpa);
     }
   else arec[ia] = -1;

   if (!eofb)
       eofb = xfread(fpb, buf);
   if (!eofb)
     {strcat (b+(80*i + 7), buf);
      brec[ib] = ftell(fpb);
     }
   else brec[ib] = -1;

   }

initscr();
cbreak();
noecho();
nonl();

clear();

cin = 'x';
redo = 1;

while (cin != 'q')
  {if (cin == 'q')
     {addch (cin);
      continue;
      }
   if ((cin == 'U') || (cin == 'b') || (cin == 'B'))
    {addch (cin);
     cnt = 1;
     if (cin == 'B') cnt = 8;
     for (k = 0; k < cnt ; k++)
     {
     if (!eofa)
      {eofa = xfread(fpa, buf);
       if (!eofa)
          {for (j=0; j < 7; j++)
	     {memset (a + (80*j), 0x00, 80);
	     strcpy (a+(80*j), a+(80*(j+1)));
	     }
           ++ia;
           xsetlno(ia, 7, a);
           strcat (a+((7*80)+7), buf);
   	   redo = 1;
	   keyreq = 0;
           arec[ia] = ftell(fpa);
	   }
      }
     else break;
     }
    }

   if ((cin == 'u') || (cin == 'b') || (cin == 'B'))
    {if (cin == 'u') addch (cin);
     cnt = 1;
     if (cin == 'B') cnt = 8;
     for (k = 0; k < cnt ; k++)
     {
     if (!eofb)
      {eofb = xfread(fpb, buf);
       if (!eofb)
          {for (j=0; j < 7; j++)
	     {memset (b + (80*j), 0x00, 80);
	     strcpy (b+(80*j), b+(80*(j+1)));
	     }
           ++ib;
           xsetlno(ib, 7, b);
           strcat (b+((7*80)+7), buf);
	   redo = 1;
	   keyreq = 0;
           brec[ib] = ftell(fpb);
	   }
      }
     else break;
     }
    }

   if (((cin == 'D') || (cin == 'T')) && (ia > 8))
    {addch (cin);
     ia -= 9;
     if (cin == 'T') ia = 0;
     j = fseek (fpa, arec[ia], SEEK_SET);
     for (i=0; i < 8; i++)
       {++ia;
        xsetlno(ia, i, a);
        eofa = xfread(fpa, buf);
        if (!eofa)
          {strcat (a+(80*i + 7), buf);
           arec[ia] = ftell(fpa);
          }
        }
     redo = 1;
     keyreq = 0;
     down = 1;
    }

   if (((cin == 'd') || (cin == 'T')) && (ib > 8))
    {if (cin == 'd') addch (cin);
     ib -= 9;
     if (cin == 'T') ib = 0;
     j = fseek (fpb, brec[ib], SEEK_SET);
     for (i=0; i < 8; i++)
       {++ib;
        xsetlno(ib, i, b);
        eofb = xfread(fpb, buf);
        if (!eofb)
          {strcat (b+(80*i + 7), buf);
           brec[ib] = ftell(fpb);
          }
       }
     redo = 1;
     keyreq = 0;
     down = 1;
    }

   if ((eofa) && (eofb)) redo = 1;

   if (redo)
     {move (1,1);
      addstr ("a:");
      addstr (filea);
      move (2,1);
      if (eofa)
	 {attron (A_BOLD);
	 addstr("End of File");
	 attroff (A_BOLD);
	 }
      else
	 addstr("           ");

      move (12,1);
      addstr ("b:");
      addstr (fileb);
      move (13,1);
      if (eofb)
	 {attron (A_BOLD);
	 addstr("End of File");
	 attroff (A_BOLD);
	 }
      else
	 addstr("           ");

      diff = 0;
      if ((eofa) && (eofb)) diff = 1;
      if (down == 1) diff = 1;

      for (i=0; i < 8; i ++)
        {move (i+3,0);
	 attroff (A_BOLD);
         addstr (a+(80*i));
         move (i+14,0);
	 attroff (A_BOLD);
         addstr (b+(80*i));
	 if (strncmp (a+(80*i + (7+cmp)), b+(80*i + (7+cmp)),72 - cmp) != 0)
	    {move (i+3,5);
	    addch('*');
	    move (i+14,5);
	    addch('*');
	    diff = 1;
	    for (j=0; j < 67; j++)
	      {if (a[(80*i)+(7+cmp)+j] != b[(80*i)+(7+cmp)+j])
		 {move (i+3,(7+cmp)+j);
		  attron (A_BOLD);
		  addstr (a+(80*i)+(7+cmp)+j);
		  move (i+14,(7+cmp)+j);
		  attron (A_BOLD);
		  addstr (b+(80*i)+(7+cmp)+j);
		  break;
		 }
	       }
	    move (i+4,0);
	    attroff (A_BOLD);
	    move (i+15,0);
	    attroff (A_BOLD);
	    }
         }

      move (23,0);
      addstr ("U,u,b,B,D,d,T,q ");
      move (23,17);
      addstr (" Up(a),up(b),both(1),Both(ALL),Down(a),down(b),Top(both),quit");
      move (23,16);
      if (diff)
         {refresh();
         redo=0;
	 down=0;
	 keyreq = 1;
	 }
      else
	 {cin = 'b';
	 keyreq = 0;
	 }
      }

   if (keyreq)
     {move (23, 16);
      cin = getch();
     }
   }

move (23,15);
addstr ("done");
refresh();
clear();
refresh();

endwin();
}


int xfread(FILE *fx, char *buf)
   {
   int xj,xk,tab;
   char bufin[200];

   memset (buf, 0x00, 200);
   memset (bufin, 0x00, 200);
   if (fgets (bufin, 200, fx) != NULL)
      {xj = strlen(bufin) - 1;
       if (xj < 80) memset (bufin + xj, 0x20, 80 - xj);
       bufin[72] = 0x00;
       for (xj = 0, xk=0; xj < 72, xk < 72 ; xj++, xk++)
	 {if (bufin[xj] != 0x09)
	   {buf[xk] = bufin[xj];}
	  else
	   {tab = (((xk + tablen)/tablen) * tablen);
	   tab = tab - xk;
	   memset (buf + xk, 0x20, tab);
	   xk = xk + tab - 1;
	   }
	 }
       buf[72] = 0x00;
       return(0);
       }
   else
      return(1);
   }

void xsetlno(int ix, int xj, char *x)
   {
   memset (x+(80*xj),0x00,80);
   x[80*xj]     = ((ix/1000)%10) | 0x30;
   x[80*xj + 1] = ((ix/100)%10)  | 0x30;
   x[80*xj + 2] = ((ix/10)%10)   | 0x30;
   x[80*xj + 3] =  (ix%10)       | 0x30;
   strcat (x+(80*xj), ":  ");
   }

void xhelp(char *prog)
    {
    printf ("\n%s:", prog);
    printf ("\nsrcdiff [-hcntn] [file a path] [file b path]");
    printf ("\nCompares source files interacively:  Compares 72 bytes");
    printf (" of each records.");
    printf ("\n   flags - c Skip n columns on compare");
    printf ("\n             Default = 0 (appropriate for source)");
    printf ("\n         - t Tab spacing, n is number of columns");
    printf ("\n             per tab stop. (Default = 8, Min. = 1)");
    printf ("\n             1-9 are clear. To set 10 use j, 11 use k");
    printf ("\n             etc. [Think Hex]");
    printf ("\n         - h You are here.");
    printf ("\n   file path - if not supplied, you will be asked.");
    printf ("\n Displays records from each file. [Rec no:[*] Record]");
    printf ("\n Movement - q Exit immediately.");
    printf ("\n            U Advance top file one record.");
    printf ("\n            u Advance bottom file one record.");
    printf ("\n            b Advance both files one record.");
    printf ("\n            B Advance both files all records displayed.");
    printf ("\n            D Reverse top file one record.");
    printf ("\n            d Reverse bottom file one record.");
    printf ("\n            T Beginning of both files.");
    printf ("\n After movement, records are compared.  If not equal,");
    printf (" or at end of files,");
    printf ("\n they are displayed. [*,highlight denote differences]");
    printf (" else search continues.\n");

    exit(0);}


