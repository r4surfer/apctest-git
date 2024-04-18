        REM *************************************************************~
            *                                                           *~
            *  W   W   OOO   RRRR   K   K   OOO   PPPP   EEEEE  N   N   *~
            *  W   W  O   O  R   R  K  K   O   O  P   P  E      NN  N   *~
            *  W   W  O   O  RRRR   KKK    O   O  PPPP   EEEE   N N N   *~
            *  W W W  O   O  R   R  K  K   O   O  P      E      N  NN   *~
            *   W W    OOO   R   R  K   K   OOO   P      EEEEE  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * WORKOPEN - OPENS A WORKFILE USING THE SNEAKY SYSTEM       *~
            *            WORKFILE OPEN TRICK WITH THE DOUBLE POUND SIGN *~
            *            IN THE NAME BIT.  INPUTS ARE THE UFB ADDRESS OF*~
            *            THE FILE, THE DESIRED MODE (SAME AS FILEOPEN), *~
            *            RETURNING AN F2%() VALUE TO TELL IF OPEN NOW.  *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/28/81 ! ORIGINAL                                 ! BCW *~
            * 01/24/86 ! TEST SPACE% TO AVOID GETPARM             ! KAB *~
            * 08/13/90 ! Basic 4.3 & SSL Compatibility            ! KAB *~
            *************************************************************

            sub "WORKOPEN" (#1, mode$, space%, f2%)

        dim                                                              ~
            file$8,                      /* FILE NAME                  */~
            library$8,                   /* LIBRARY                    */~
            mode$5,                      /* MODE TO OPEN FILE IN       */~
            volume$6                     /* VOLUME                     */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10047
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
L10047: REM *************************************************************
            nrecs% = max(10%, space%)
            f2% = 1
            file$ = "##      ": library$, volume$ = " "
            open nodisplay #1, output, file = file$,                     ~
                                       library = library$,               ~
                                       volume = volume$, space = nrecs%

            if mode$ <> "OUTPT" then L10120
               f2% = 0
               end
L10120:     if mode$ <> "INPUT" then L10170
               close #1
               open nodisplay #1, input
               f2% = 0
               end
L10170:     if mode$ <> "EXTND" then L10220
               close #1
               open nodisplay #1, extend
               f2% = 0
               end
L10220:     if mode$ <> "IO   " then L10270
               close #1
               open nodisplay #1, io
               f2% = 0
               end
L10270:     if mode$ <> "SHARE" then end
               close #1
               open nodisplay #1, shared
               f2% = 0
               end
