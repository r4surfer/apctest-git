/*****************************************************************************
                 C A E L U S   P R O G R A M   M O D U L E
                      (c)Copyright 1998 CAELUS, Inc.
                   A L L   R I G H T S   R E S E R V E D
==============================================================================
                   M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------

	wdelwrk - Remove User's work files & directory

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
        02-26-98        DXL     Original.
*****************************************************************************/

#include "cmssys.h"
#include "wfname.h"

int main ( int argc, char *argv[] ) {
        char workdir[200], worklib[9], workvol[7];
        S32 FileType;

        struct symbol *allvar[]={NULL};
        int Iflag = 0;

        if( argc < 1 ) exit(-1);

        wb_init (&Iflag, allvar, argv[0]);      /* init for main program */

        Call_args = 4;
        cms_xtract ( "WV", workvol, "WL", worklib );

        memset( workdir, ' ', 200 );
        FileType = IS_IO;
        wfname( &FileType, workvol, worklib, " ", workdir );
        strb2c( workdir, 200 );

        wdelwrk( workdir );                     /* delete work files    */

        return(0);
}
