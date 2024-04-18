        REM *************************************************************~
            *                                                           *~
            *  PPPP   L       OOO   W   W   AAA   L      TTTTT   SSS    *~
            *  P   P  L      O   O  W   W  A   A  L        T    S       *~
            *  PPPP   L      O   O  W   W  AAAAA  L        T     SSS    *~
            *  P      L      O   O  W W W  A   A  L        T        S   *~
            *  P      LLLLL   OOO    W W   A   A  LLLLL    T     SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLOWALTS - Read Generic Key routine. Allows caller to     *~
            *            retrieve next record on an alternate key path  *~
            *            for the specified file whose key value is      *~
            *            greater than the passed in key value and       *~
            *            is equal to the passed in key value in the     *~
            *            first BREAK% bytes (If 0 no test).             *~
            *            If record found which passes the above test    *~
            *            then F1% is set to 1 to indicate successful    *~
            *            read and OLDREADKEY$ is set equal to the       *~
            *            alternate key value of the retrieved record.   *~
            *            If end-of-file encountered or the control break*~
            *            test fails the F1% is set to 0 and OLDREADKEY$ *~
            *            remains unchanged from the passed in value.    *~
            *            Please note that key path 0 or alternate key   *~
            *            0 is the same thing as the primary key path.   *~
            *            Also note that if key path allows duplicates   *~
            *            this routine will only retrieve the FIRST      *~
            *            occurrence of alternate key with duplicates.   *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/18/81 ! ORIGINAL (FROM PLOWNEXT)                 ! BCW *~
            * 01/21/86 ! Header Documentation Update              ! LDJ *~
            * 02/24/87 ! Speed Enhancements For Large Files       ! LDJ *~
            *************************************************************

        sub "PLOWALTS" (#1, oldreadkey$, key%, break%, f1%)

            dim oldreadkey$255,          /* KEY FOR READING STUFF IN   */~
                newreadkey$255           /* OTHER READ IN STUFF KEY    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            goto  L11100 : dim cms2v$50
            cms2v$ = "04.19.01 03/30/87 Patch release                   "
L11100: REM *************************************************************

            call "GETUFBS1" addr (#1, f1%)
                 if f1% = 0% then end
            call "GETUFBPK" addr(#1, f1%)  /* Current Key Path   */
            if f1% <> key% then L11200
            if str(key(#1, key%)) <> oldreadkey$ then L11200
            call "READNEXT" (#1, f1%)
            if f1% = 0% then end
            if str(key(#1, key%)) <= oldreadkey$ then L11200 /*Dupe Key?*/
            goto L11400

L11200:     call "REDALT2" (#1, oldreadkey$, key%, f1%)
                 if f1% = 0% then end
L11400:     newreadkey$ = key(#1, key%)

            if break% = 0% then L11900
            if str(newreadkey$,,break%) <> str(oldreadkey$,,break%) then ~
               L12200
L11900:     oldreadkey$ = newreadkey$
            end

L12200:     REM THIS HERE CASE IS IF CONTROL BREAK HAPPENED.
                f1% = 0%
                end

