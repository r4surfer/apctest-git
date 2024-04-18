/*****************************************************************************
                 C A E L U S   P R O G R A M   M O D U L E
                      (c)Copyright 1998 CAELUS, Inc.
                   A L L   R I G H T S   R E S E R V E D
==============================================================================
                   M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------

	display - display a print file.

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
        02-19-98        DXL     Original.
*****************************************************************************/
#include "wvsb0.h"

int main ( int argc, char *argv[] ) {
        char *getparm_type = "ID",
             *pfkey = "@",
              inputmessage[250],
              file[9],
              library[9],
              volume[7],
              filespec[61],
              errormsg[101],
              blankfilespec[61];
        S32   n0  = 0,
              n2  = 2,
              n3  = 3,
              n5  = 5,
              n6  = 6,
              n8  = 8,
              n12 =12,
              n24 =24,
              n60 =60,
              n78 =78,
              n237=237,
              ret = -1,
              pfmask = 1<<16 | 1<<31;

struct symbol *allvar[]={
NULL};
int Iflag = 0;
wb_init (&Iflag, allvar, argv[0]);      /* init for main program */

        if( argc > 1 ) {
                Call_args = 2;
                PDISPLAY( argv[1], &ret );
        }

        sprintf( inputmessage, "%-78.78s%c%-78.78s%c%-79.79s",
                "To Display a File, enter the Name and Location of the File, then press", 0x0d,
                "ENTER to continue or F1 to Exit.", 0x0d,
                "Optionally you may enter the native FileSpec Location in the space provided." );

        sprintf( filespec, "%60.60s", " " );
        sprintf( blankfilespec, "%60.60s", " " );

        while( ret ) {
                Call_args = 54;
                GETPARM (
 /* 8*/         getparm_type, "R", "INPUT   ", pfkey, "0001", "DISPLY", inputmessage, &n237,
 /* 9*/         "K", "FILE    ", file,    &n8, "A", &n12, "A", &n5, "C",
 /* 9*/         "K", "LIBRARY ", library, &n8, "R", &n0,  "R", &n5, "U",
 /* 9*/         "K", "VOLUME  ", volume,  &n6, "R", &n0,  "R", &n5, "U",
 /* 9*/         "K", "FILESPEC", filespec,&n60,"R", &n2,  "A", &n5, "C",
 /* 2*/         "P", &pfmask,
 /* 1*/         "E",
 /* 7*/         "T", errormsg, &n78, "A", &n24, "A", &n3 );

                getparm_type[0]='R';
                getparm_type[1]=' ';

                if(*pfkey!='@') ret = 0;
                else {
                        if( !strcmp(filespec, blankfilespec) ) {
                                Call_args = 4;
                                PDISPLAY ( file, library, volume, &ret );
                                sprintf( errormsg, "%-100.100s", "Cannot Find File, Lib, & Vol" );
                        } else {
                                strb2c( filespec, 60 );
                                Call_args = 2;
                                PDISPLAY ( filespec, &ret );
                                strc2b( filespec, 60 );
                                sprintf( errormsg, "%-100.100s", "Cannot Find Filespec" );
                        }
                }
        }
        return(0);
}
