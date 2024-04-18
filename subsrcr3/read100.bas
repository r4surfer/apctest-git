        REM *************************************************************~
            *                                                           *~
            *   RRRR   EEEEE   AAA   DDDD     1    000    000           *~
            *   R   R  E      A   A  D   D   11   0   0  0   0          *~
            *   RRRR   EEEE   AAAAA  D   D    1   0   0  0   0          *~
            *   R   R  E      A   A  D   D    1   0   0  0   0          *~
            *   R   R  EEEEE  A   A  DDDD   11111  000    000           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * READ100  - Reads a record from a given file. Args include *~
            *            the File Number, the Key to use on the Read,   *~
            *            and the Read Status.  Returns F1% = 1 if       *~
            *            record found, 0 if not found.                  *~
            *-----------------------------------------------------------*~
            *                   M O D I F I C A T I O N S               *~
            *---WHEN---+-----------------WHAT---------------------+-WHO-*~
            * 04/13/80 ! ORIGINAL.                                ! BCW *~
            * 01/22/86 ! Now just driver for REDALT0.             ! LDJ *~
            *************************************************************

        sub "READ100" (#1, key$, f1%)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10013
            cms2v$ = "04.15.09 01/27/86 Object code size and dfloat    "
L10013: REM *************************************************************
            call "REDALT0" (#1, key$, 0%, f1%)
            end
