        REM *************************************************************~
            *                                                           *~
            *  RRRR   EEEEE   AAA   DDDD   N   N  X   X  TTTTT    1     *~
            *  R   R  E      A   A  D   D  NN  N   X X     T     11     *~
            *  RRRR   EEEE   AAAAA  D   D  N N N    X      T      1     *~
            *  R   R  E      A   A  D   D  N  NN   X X     T      1     *~
            *  R   R  EEEEE  A   A  DDDD   N   N  X   X    T    11111   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * READNXT1 - READS FROM THE 'LAST POINT OF REFERENCE' IN    *~
            *            HOLD MODE.  USEFUL FOR UPDATING FILES WHILE    *~
            *            ROMPING DOWN A CHAIN OF ALTERNATE KEYS.        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/15/80 ! ORIGINAL                                 ! TEM *~
            *************************************************************

        sub "READNXT1" (#1, f1%)

            REM GETS FILE STATUS FLAG FROM THE UFB AND FINDS OUT IF OPEN
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10023
            cms2v$ = "04.15.09 01/27/86 Object code size and dfloat    "
L10023: REM *************************************************************
                call "GETUFBS1" addr (#1, f1%)
                if f1% = 0% then end
                     /* RETURN IF FILE NOT OPEN CURRENTLY.             */
            f1% = 0
            read #1, hold, eod goto L10080
            f1% = 1
L10080:     end
