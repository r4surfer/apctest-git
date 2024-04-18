        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    CCC   K   K  PPPP   RRRR   EEEEE  FFFFF  X   X   *~
            *  B   B  C   C  K  K   P   P  R   R  E      F       X X    *~
            *  BBBB   C      KKK    PPPP   RRRR   EEEE   FFFF     X     *~
            *  B   B  C   C  K  K   P      R   R  E      F       X X    *~
            *  BBBB    CCC   K   K  P      R   R  EEEEE  F      X   X   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKPREFX - Checks that Sales Order Number Prefix is not   *~
            *            comprised of any of a long list of ineligables.*~
            *            This is done to remove ambiguity from the      *~
            *            PIP labels.                                    *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/11/87 ! Original                                 ! ERN *~
            * 10/11/93 ! Purchased Jobs - added 'BW' and 'RW'.    ! MLJ *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**


        sub "BCKPREFX" (so$, errormsg$)


        dim                                                              ~
            p%(1),                       /* Search Receiver            */~
            nonos$50                     /* List of invalid prefixes   */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "

        REM *************************************************************~
            *             M A I N   L O G I C                           *~
            * --------------------------------------------------------- *~
            * The crux of the matter, as it were.                       *~
            *************************************************************

            nonos$ = "JOWOBOROEXPOQCBWRW"
            errormsg$ = " "
            if len(so$) < 2 then end

            search nonos$ = str(so$,,2) to p%() step 2%
            if p%(1) = 0% then end
                errormsg$ = "Sales Order number may not begin with '" &  ~
                            str(so$,,2) & "'."
                end

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

