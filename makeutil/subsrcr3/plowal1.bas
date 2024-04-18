        REM *************************************************************~
            *                                                           *~
            *  PPPP   L       OOO   W   W   AAA   L        11           *~
            *  P   P  L      O   O  W   W  A   A  L       1 1           *~
            *  PPPP   L      O   O  W   W  AAAAA  L         1           *~
            *  P      L      O   O  W W W  A   A  L         1           *~
            *  P      LLLLL   OOO    W W   A   A  LLLLL   11111         *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLOWAL1  - Read Generic Key WITH HOLD.  Allows caller to  *~
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
            *            occurrence of alternate key with duplicates    *~
            *            (Same as PLOWALTS except does read with HOLD). *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * ??/??/84 ! ORIGINAL (FROM PLOWALTS)                 ! KAB *~
            * 01/21/86 ! Header Documentation Update              ! LDJ *~
            * 02/26/87 ! Speed Enhancements For Large Files       ! LDJ *~
            *************************************************************

        sub "PLOWAL1" (#1, oldreadkey$, key%, break%, f1%)

            dim oldreadkey$255,          /* Key for reading stuff in   */~
                newreadkey$255           /* Other read in stuff key    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            goto  L10100 : dim cms2v$50
            cms2v$ = "04.19.01 03/30/87 Patch release                   "
L10100: REM *************************************************************

            call "GETUFBS1" addr (#1, f1%)
                 if f1% = 0% then end
            call "GETUFBPK" addr(#1, f1%)  /* Get Current Key Path */
            if f1% <> key% then L10200      /* Same Key Path ? */
            if str(key(#1, key%)) <> oldreadkey$ then L10200 /*Same Key?*/
            call "READNXT1" (#1, f1%)
            if f1% = 0% then end
            if str(key(#1, key%)) <= oldreadkey$ then L10200 /*Dupe Key?*/
            goto L10220

L10200:     call "REDALT3"  (#1, oldreadkey$, key%, f1%)
                 if f1% = 0% then end
L10220:     newreadkey$ = key(#1, key%)

            if break% = 0% then L10270
            if str(newreadkey$,,break%) <> str(oldreadkey$,,break%)      ~
               then L10300
L10270:     oldreadkey$ = newreadkey$
            end

L10300:     REM This here case is if control break happened.
                call "STRTRLSE" addr(#1)
                f1% = 0%
                end

