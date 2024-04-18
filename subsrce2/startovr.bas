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
            * 02/20/91 ! Converted to stub for ZSTARTOV           ! MJB *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "STARTOVR" (arg1)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
        REM *************************************************************

            close ws

            call "ZSTARTOV" (arg1)

            end
