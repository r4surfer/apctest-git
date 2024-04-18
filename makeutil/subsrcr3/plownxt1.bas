        REM *************************************************************~
            *                                                           *~
            *  PPPP   L       OOO   W   W  N   N  X   X  TTTTT    1     *~
            *  P   P  L      O   O  W   W  NN  N   X X     T     11     *~
            *  PPPP   L      O   O  W   W  N N N    X      T      1     *~
            *  P      L      O   O  W W W  N  NN   X X     T      1     *~
            *  P      LLLLL   OOO    W W   N   N  X   X    T    11111   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLOWNXT1 - Read Generic Key WITH HOLD.  Allows caller to  *~
            *            retrieve next record on the primary key path   *~
            *            for the specified file whose key value is      *~
            *            greater than the passed in key value and       *~
            *            is equal to the passed in key value in the     *~
            *            first BREAK% bytes (If 0 no test).             *~
            *            If record found which passes the above test    *~
            *            then F1% is set to 1 to indicate successful    *~
            *            read and OLDREADKEY$ is set equal to the       *~
            *            primary key value of the retrieved record.     *~
            *            If end-of-file encountered or the control break*~
            *            test fails the F1% is set to 0 and OLDREADKEY$ *~
            *            remains unchanged from the passed in value.    *~
            *            NOTE; Same as PLOWNEXT except does read with   *~
            *            HOLD.                                          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/18/81 ! ORIGINAL (FROM PLOWNEXT)                 ! TEM *~
            * 06/23/82 ! REMOVED EXTRANEOUS "GETUFBKL"            ! ECR *~
            * 01/22/86 ! Changed to just be a driver for PLOWAL1. ! LDJ *~
            *************************************************************

        sub "PLOWNXT1" (#1, oldreadkey$, break%, f1%)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10022
            cms2v$ = "04.15.09 01/27/86 Object code size and dfloat    "
L10022: REM *************************************************************
            call "PLOWAL1" (#1, oldreadkey$, 0%, break%, f1%)
            end
