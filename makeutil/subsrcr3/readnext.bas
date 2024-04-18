        REM *************************************************************~
            *                                                           *~
            *   RRRR   EEEEE   AAA   DDDD   N   N  EEEEE  X   X  TTTTT  *~
            *   R   R  E      A   A  D   D  NN  N  E       X X     T    *~
            *   RRRR   EEEE   AAAAA  D   D  N N N  EEEE     X      T    *~
            *   R   R  E      A   A  D   D  N  NN  E       X X     T    *~
            *   R   R  EEEEE  A   A  DDDD   N   N  EEEEE  X   X    T    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * READNEXT - READS A RECORD FROM A GIVEN FILE, USING THE    *~
            *            "PREVIOUS KEY OF REFERENCE", RETURNING F1% = 1 *~
            *            IF THE RECORD IS ON FILE, 0 IF NOT. ARGS ARE   *~
            *            FILE NUMBER AND F1%.                   .       *~
            *-----------------------------------------------------------*~
            *                   M O D I F I C A T I O N S               *~
            *---WHEN---+-----------------WHAT---------------------+-WHO-*~
            * 04/28/80 ! ORIGINAL (RIPPED OFF FROM READ100)       ! BCW *~
            *************************************************************

        sub "READNEXT" (#1, f1%)

            REM GETS FILE STATUS FLAG FROM THE UFB AND FINDS OUT IF OPEN
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10023
            cms2v$ = "04.15.09 01/27/86 Object code size and dfloat    "
L10023: REM *************************************************************
                call "GETUFBS1" addr (#1, f1%)
                if f1% = 0% then end
            f1% = 0%
                     /* RETURN IF FILE NOT OPEN CURRENTLY.             */
            read #1, eod goto L10080
            f1% = 1
L10080:     end
