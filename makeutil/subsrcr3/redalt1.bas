        REM *************************************************************~
            *                                                           *~
            *  RRRR   EEEEE  DDDD    AAA   L      TTTTT    1            *~
            *  R   R  E      D   D  A   A  L        T     11            *~
            *  RRRR   EEEE   D   D  AAAAA  L        T      1            *~
            *  R   R  E      D   D  A   A  L        T      1            *~
            *  R   R  EEEEE  DDDD   A   A  LLLLL    T    11111          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * REDALT1  - READS THE SPECIFIED ALTERNATE KEY (RIPPED OFF  *~
            *            FROM THE "READ100" SERIES...FAIRLY OBVIOUS.    *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/11/80 ! ORIGINAL                                 ! BCW *~
            *************************************************************

        sub "REDALT1" (#1, key$, key%, f1%)
            dim key$255

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10032
            cms2v$ = "04.15.09 01/27/86 Object code size and dfloat    "
L10032: REM *************************************************************
            call "GETUFBS1" addr (#1, f1%)
            if f1% = 0%  then end
               /* THIS IF FILE NOT OPEN.  BULLETPROOFER THAN F2%()     */
            f1% = 0
            read #1, hold,                                               ~
                     key key% = key$,                                    ~
                     eod goto L10110
            f1% = 1
L10110:     end
