        REM *************************************************************~
            *                                                           *~
            *   RRRR   EEEEE   AAA   DDDD     1    000   55555          *~
            *   R   R  E      A   A  D   D   11   0   0  5              *~
            *   RRRR   EEEE   AAAAA  D   D    1   0   0   555           *~
            *   R   R  E      A   A  D   D    1   0   0      5          *~
            *   R   R  EEEEE  A   A  DDDD   11111  000   5555           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * READ105  - Reads the next key (record) > or = the         *~
            *            passed in Key value with HOLD.  Return F1% = 1 *~
            *            if record found, F1% = 0 if end-of-file or file*~
            *            not open.                                      *~
            *-----------------------------------------------------------*~
            *                   M O D I F I C A T I O N S               *~
            *---WHEN---+-----------------WHAT---------------------+-WHO-*~
            * 04/13/80 ! ORIGINAL.                                ! BCW *~
            * 01/22/86 ! Now just a driver for REDALT5.           ! LDJ *~
            *************************************************************

        sub "READ105" (#1, key$, f1%)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10022
            cms2v$ = "04.15.09 01/27/86 Object code size and dfloat    "
L10022: REM *************************************************************
            call "REDALT5" (#1, key$, 0%, f1%)
            end
