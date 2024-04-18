        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS   TTTTT    A    RRRR   TTTTT   OOO   V   V  RRRR    *~
            *  S        T     A A   R   R    T    O   O  V   V  R   R   *~
            *   SSS     T    AAAAA  RRRR     T    O   O  V   V  RRRR    *~
            *      S    T    A   A  R   R    T    O   O   V V   R   R   *~
            *   SSS     T    A   A  R   R    T     OOO     V    R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STARTOVR - General routine for display of the "START OVER"*~
            *            screen.  Overlays startover window onto current*~
            *            screen & waits for user response (PF 0 = return*~
            *            signaling startover, PF 1 = no startover).     *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/21/85 ! Original  (per request by R. Nelson)     ! LDJ *~
            * 10/22/85 ! Cleaned up & simplified, seg. 2 decreased! KAB *~
            *          ! Subroutine cured of AIDs$                ! KAB *~
            *          ! (in other words changed to call ASKUSER).!     *~
            * 10/10/86 ! Minor change for consistency. If KEYHIT% ! LDJ *~
            *          !   passed in as negative automatically    !     *~
            *          !   changes it to 0.                       !     *~
            * 11/10/95 ! Changes for GUI Environment              ! LDJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC


            sub "ZSTARTOV" (keyhit%)

        dim                                                              ~
            hdr$40,                      /* Message header             */~
            mid$80,                      /* Middle Line (usually -OR-) */~
            pf2$80,                      /* PF 2   Key Prompt          */~
            pf1$80                       /* PF 1   Key Prompt          */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 08/12/96 Last Wang Release               "
        REM *************************************************************

            call "CHECKGUI" addr(gui%)
            on gui% goto gui_message
            save% = max(0%,keyhit%)
L00320:     keyhit% = save%
            hdr$ = "** START OVER **"
            pf1$ = "Press PF (1) to Return to Display"
            mid$ = "- OR -"
            pf2$ =                                                       ~
             "Press RETURN to Start Over Without Saving the Current Entry"

            close ws
            call "ZASKUSER" (keyhit%, hdr$, pf1$, mid$, pf2$)

            if keyhit% = 0% then end_routine
            if keyhit% = 1% then end_routine
               goto L00320

        gui_message
            call "ASKGUI" (36%,"Start Over / Cancel ?","Are you sure you ~
        ~want to Cancel the current operation?", keyhit%)
            if keyhit% = 6% then keyhit% = 0% else keyhit% = 1%

        end_routine
            end
