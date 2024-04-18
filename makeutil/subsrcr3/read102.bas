        REM *************************************************************~
            *                                                           *~
            *   RRRR   EEEEE   AAA   DDDD     1    000   2222           *~
            *   R   R  E      A   A  D   D   11   0   0      2          *~
            *   RRRR   EEEE   AAAAA  D   D    1   0   0   222           *~
            *   R   R  E      A   A  D   D    1   0   0  2              *~
            *   R   R  EEEEE  A   A  DDDD   11111  000   22222          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * READ102  - Reads the next key (record) greater than the   *~
            *            passed in Key value.  Return F1% = 1 if record *~
            *            found, F1% = 0 if end-of-file or file not open.*~
            *-----------------------------------------------------------*~
            *                   M O D I F I C A T I O N S               *~
            *---WHEN---+-----------------WHAT---------------------+-WHO-*~
            * 04/13/80 ! ORIGINAL.                                ! BCW *~
            * 01/22/86 ! Now just a driver for REDALT2.           ! LDJ *~
            *************************************************************

        sub "READ102" (#1, key$, f1%)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10022
            cms2v$ = "04.15.09 01/27/86 Object code size and dfloat    "
L10022: REM *************************************************************
            call "REDALT2" (#1, key$, 0%, f1%)
            end
