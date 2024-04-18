        REM *************************************************************~
            *                                                           *~
            *   RRRR   EEEEE   AAA   DDDD     1    000     1            *~
            *   R   R  E      A   A  D   D   11   0   0   11            *~
            *   RRRR   EEEE   AAAAA  D   D    1   0   0    1            *~
            *   R   R  E      A   A  D   D    1   0   0    1            *~
            *   R   R  EEEEE  A   A  DDDD   11111  000   11111          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * READ101  - Reads a record with HOLD from the given file.  *~
            *            Args include the File Number, the Key to use on*~
            *            the Read, and the Read Status.  Returns F1% = 1*~
            *            if record found, 0 if not found.               *~
            *-----------------------------------------------------------*~
            *                   M O D I F I C A T I O N S               *~
            *---WHEN---+-----------------WHAT---------------------+-WHO-*~
            * 04/13/80 ! ORIGINAL.                                ! BCW *~
            * 01/22/86 ! Now just driver for REDALT1.             ! LDJ *~
            *************************************************************

        sub "READ101" (#1, key$, f1%)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10022
            cms2v$ = "04.15.09 01/27/86 Object code size and dfloat    "
L10022: REM *************************************************************
            call "REDALT1" (#1, key$, 0%, f1%)
            end
