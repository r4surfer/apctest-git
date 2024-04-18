        REM *************************************************************~
            *                                                           *~
            *  PPPP   L       OOO   W   W  N   N  EEEEE  X   X  TTTTT   *~
            *  P   P  L      O   O  W   W  NN  N  E       X X     T     *~
            *  PPPP   L      O   O  W   W  N N N  EEEE     X      T     *~
            *  P      L      O   O  W W W  N  NN  E       X X     T     *~
            *  P      LLLLL   OOO    W W   N   N  EEEEE  X   X    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLOWNEXT - Read Generic Key routine. Allows caller to     *~
            *            retrieve next record on the primary key path   *~
            *            for the specified file whose key value is      *~
            *            greater than the passed in key value and       *~
            *            is equal to the passed in key value in the     *~
            *            first BREAK% bytes (If 0 no test).             *~
            *            If record found which passes the above test    *~
            *            then F1% is set to 1 to indicate successful    *~
            *            read and PLOWKEY$ is set equal to the primary  *~
            *            key value of the retrieved record.             *~
            *            If end-of-file encountered or the control break*~
            *            test fails the F1% is set to 0 and PLOWKEY$    *~
            *            remains unchanged from the passed in value.    *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/19/80 ! ORIGINAL                                 ! BCW *~
            * 09/20/80 ! "GETUFBWA" INSTEAD OF BASIC "GET"        ! BCW *~
            * 03/11/81 ! REVISION FOR OFFSET KEYS                 ! BCW *~
            * 06/23/82 ! REMOVED EXTRANEOUS "GETUFBKL"            ! ECR *~
            * 01/22/86 ! Changed to just be a driver for PLOWALTS.! LDJ *~
            *************************************************************

        sub "PLOWNEXT" (#1, plowkey$, break%, f1%)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10022
            cms2v$ = "04.15.09 01/27/86 Object code size and dfloat    "
L10022: REM *************************************************************
            call "PLOWALTS" (#1, plowkey$, 0%, break%, f1%)
            end
