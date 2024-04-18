        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   TTTTT   CCC   RRRR   EEEEE  M   M   AAA   PPPP    *~
            *  S        T    C   C  R   R  E      MM MM  A   A  P   P   *~
            *   SSS     T    C      RRRR   EEEE   M M M  AAAAA  PPPP    *~
            *      S    T    C   C  R   R  E      M   M  A   A  P       *~
            *   SSS     T     CCC   R   R  EEEEE  M   M  A   A  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCREMAP - Remaps the costs passed in per the mapping     *~
            *            passed in.                                     *~
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
            * 07/22/87 ! Original                                 ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "STCREMAP" (map%(), costs$)

*        MAP%() consists of 12 elements.  Each element specifies the
*               new bucket.  For example if MAP%(1) = 2 then old
*               bucket 2 is placed into new bucket 1.  A value of 0
*               indicates that no destination exists.
*
*        COSTS$ is a 96 character string.  It is passed in as the old
*               cost element breakdown.  It is remapped, passed thru
*               PACKZERO and returned.

        dim                                                              ~
            costs$96,                    /* In and Out costs           */~
            map%(12),                    /* Mapping Definition         */~
            newcosts(12),                /* Outgoing Costs (remapped)  */~
            oldcosts(12)                 /* Incomming Costs            */

        REM *************************************************************~
            *          S U B R O U T I N E    L O G I C                 *~
            *************************************************************

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10042
            cms2v$ = "05.00.00 09/08/87 Standard cost to 12            "
L10042: REM *************************************************************
            get costs$ using L10050, oldcosts()
L10050:         FMT 12*PD(14,4)

            mat newcosts = zer

            for b% = 1% to 12%
                if map%(b%) > 0% then newcosts(map%(b%)) =               ~
                                      newcosts(map%(b%)) + oldcosts(b%)
            next b%

            call "PACKZERO" (newcosts(), costs$)

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
