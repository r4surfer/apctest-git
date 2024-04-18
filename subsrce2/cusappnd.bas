        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   U   U   SSS    AAA   PPPP   PPPP   N   N  DDDD    *~
            *  C   C  U   U  S      A   A  P   P  P   P  NN  N  D   D   *~
            *  C      U   U   SSS   AAAAA  PPPP   PPPP   N N N  D   D   *~
            *  C   C  U   U      S  A   A  P      P      N  NN  D   D   *~
            *   CCC    UUU    SSS   A   A  P      P      N   N  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CUSAPPND - Subroutine to handle all and any ancilliary    *~
            *            customer master maintenance (normally to       *~
            *            maintain CUSTOMR2 file).                       *~
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
            * 10/12/83 ! ORIGINAL                                 ! HES *~
            * 06/13/86 ! Slightly modified                        ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "CUSAPPND" (f%,              /* FUNTION CODE               */~
                                         /*   1- Read Record           */~
                                         /*   2- Write Record          */~
                                         /*   3- Input Mode            */~
                                         /*   4- Edit Mode             */~
                        cuscode$,        /* Customer Number            */~
                        return% )        /* Return Status Code         */~
                                         /*   1- Start Over            */~
                                         /*   4- Previous Screen       */~
                                         /*   5- Next Screen           */~
                                         /*  16- Save Data             */~
                                         /*  99- Routine Disabled      */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto    L00412
            cms2v$ = "04.17.01 11/20/86 Order process & planning #2     "
L00412: REM *************************************************************
        return% = 99%  :  goto L65000

*        NOTES
*          Routine is written into CUSINPUT assuming the following:
*            1) Will be used as an extra screen of data
*            2) That RETURN% will be set appropriately
*            3) That FUNCTION% will be handled properly
*
*          CUSMASTR2 file must be defined and opened by the subroutine.

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
