        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y   AAA   PPPP   PPPP   N   N  DDDD    *~
            *  H   H  NN  N  Y   Y  A   A  P   P  P   P  NN  N  D   D   *~
            *  HHHHH  N N N   YYY   AAAAA  PPPP   PPPP   N N N  D   D   *~
            *  H   H  N  NN    Y    A   A  P      P      N  NN  D   D   *~
            *  H   H  N   N    Y    A   A  P      P      N   N  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYAPPND - Handles all file maintenance for 'HNYMAST2',   *~
            *            the appendix file for 'HNYMASTR'.              *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/13/83 ! ORIGINAL                                 ! HES *~
            * 04/07/87 ! Slight modifications made                ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**


        sub "HNYAPPND" (f%,              /* FUNCTION CODE              */~
                                         /*   1 = Read Record          */~
                                         /*   2 = Write Record         */~
                                         /*   3 = Input Mode           */~
                                         /*   4 = Edit Mode            */~
                        part$,           /* Part Number to maintain    */~
                        return%)         /* RETURN CODE                */
                                         /*   1 - Start Over           */
                                         /*   4 - Previous Screen      */
                                         /*   5 - Next Screen          */
                                         /*  16 - Data Save            */
                                         /*  99 - Routine Disabled     */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto    L09000
            cms2v$ = "05.00.00 09/08/87 Standard cost to 12             "

L09000: REM *************************************************************
        return% = 99%  :  goto L65000

*        NOTES
*          Routine is written into HNYINPUT assuming the following:
*            1) Will be used as an extra screen of data
*            2) That RETURN% will be set appropriately
*            3) That FUNCTION% will be handled properly
*
*          HNYMASTR2 file must be defined and opened by the subroutine.

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            end
