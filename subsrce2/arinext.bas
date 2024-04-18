        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   IIIII  N   N  EEEEE  X   X  TTTTT          *~
            *  A   A  R   R    I    NN  N  E       X X     T            *~
            *  AAAAA  RRRR     I    N N N  EEEE     X      T            *~
            *  A   A  R  R     I    N  NN  E       X X     T            *~
            *  A   A  R   R  IIIII  N   N  EEEEE  X   X    T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARINEXT  - Derives Next Invoice Number.                   *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/04/86 ! Original                                 ! ERN *~
            * 11/26/86 ! Added Bill-to check for settlement #     ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "ARINEXT"    (#1,            /* STORNAME Channel           */~
                          #2,            /* SYSFILE2 Channel           */~
                          #3,            /* ARINUMBR Channel           */~
                          #4,            /* ARMTRIAL Channel           */~
                          invtypein$,    /* Invoice Type               */~
                          store$,        /* Store Number               */~
                          cuscode$,      /* Ship-to Customer Code      */~
                          billto$,       /* Bill-to Customer Code      */~
                          inv$ )         /* Invoice Number             */

*        If INV$ is passed in as 'NO STL' then checking of the
*        settlement number is suspended.

        dim                                                              ~
            billto$9,                    /* Bill-to Customer Number    */~
            cuscode$9,                   /* Ship-to Customer Number    */~
            inv$8,                       /* Invoice Number- Passed Back*/~
            invstore$3,                  /* Store to find Invoice #    */~
            invtype$1, invtypein$,       /* Invoice Type               */~
            next$8,                      /* Next Document Number       */~
            nextadj$,                    /* Next Adjustment Number     */~
            nextcm$8,                    /* Next Credit Memo Number    */~
            nextfc$8,                    /* Next Finance Charge Number */~
            nextinv$8,                   /* Next Invoice Number        */~
            readkey$50,                  /* A Read Key                 */~
            stlkey$50,                   /* Plow key for ARMTRIAL      */~
            store$3,                     /* Store Number on Invoice    */~
            temp$8                       /* Temporary Variable         */

        dim f1%(12)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.18.00 02/06/87 A/R PhaseII & SA Reports        "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! STORNAME ! Store Master File                        *~
            * #2  ! SYSFILE2 ! System file- For next FC Number          *~
            * #3  ! ARINUMBR ! Duplicate Invoice Number Check File      *~
            * #4  ! ARMTRIAL ! A/R Trial Balance                        *~
            *************************************************************~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            stlmnt% = 1% : if inv$     = "NO STL" then stlmnt% = 0%

            inv$ = " "
            call "BCKSWTCH" ("AR ", "INVSTORE", invstore$, r, temp%)
            if invstore$ = " " then invstore$ = store$
            r = r

            invtype$ = invtypein$
            if invtype$ = "C" or invtype$ = "A" or invtype$ = "F"        ~
                                         then L10000  else invtype$ = "I"

L10000: REM *************************************************************~
            *                 M A I N   L O G I C                       *~
            *-----------------------------------------------------------*~
            * Do what we are supposed to do.                            *~
            *************************************************************

*       ****************************************************************
*        GET THE NEXT NUMBER as indicated by Invoice Type              *
*       ****************************************************************
        if invtype$ = "F" then L10230
L10100
*        Get from Store Record
            call "READ101" (#1, invstore$, f1%(1))
            if f1%(1) = 0% then end

            get #1 using L10150, nextinv$, nextcm$, nextadj$
L10150:         FMT POS(154), CH(8), POS(201), 2*CH(8)
            if invtype$ = "C" and nextcm$  = " " then invtype$ = "I"
            if invtype$ = "A" and nextadj$ = " " then invtype$ = "I"
            if invtype$ = "I" then next$ = nextinv$
            if invtype$ = "C" then next$ = nextcm$
            if invtype$ = "A" then next$ = nextadj$
            goto L10320

L10230
*        Get Next Finance Charge Number from SYSFILE2
            readkey$ = "SWITCHS.ARM"
            call "READ101" (#2, readkey$, f1%(2))
            if f1%(2) = 1% then L10280
                invtype$ = "I" : goto L10100
L10280:     get #2 using L10290, nextfc$
L10290:         FMT POS(25), CH(8)
            next$ = nextfc$

L10320
*       ****************************************************************
*        CHECK THAT NEXT NUMBER IS NOT ALREADY USED.                   *
*       ****************************************************************

*        First check for a duplicate invoice number
L10370:     call "REDALT0" (#3, next$, 1%, f1%(3))
            if f1%(3) = 0% then L10420
L10390:         gosub up_it_by_one
                goto L10370

L10420
*        Next Check that settlement number does not exist for bill-to
            if stlmnt% = 0% then L10480
                stlkey$ = str(billto$,,9) & str(next$,,8)
                call "PLOWNEXT" (#4, stlkey$, 17%, f1%(4))
                if f1%(4) = 1% then L10390

L10480
*       ****************************************************************
*        SAVE next number and get out                                  *
*       ****************************************************************

            inv$ = next$

*        Increment number by one and save the new next number
            gosub up_it_by_one
            if invtype$ = "F" then L10660

*         Save other than Finance Charge
            if invtype$ = "A" then nextadj$ = next$
            if invtype$ = "C" then nextcm$  = next$
            if invtype$ = "I" then nextinv$ = next$
            put #1 using L10150, nextinv$, nextcm$, nextadj$
            rewrite #1
            goto L10700

L10660
*         Save Next Finance Charge Number
            put #2 using L10290, next$
            rewrite #2

L10700
*        Write invoice number to dup check file ARINUMBR
            write #3 using L10720, cuscode$, inv$, eod goto L10000
L10720:         FMT CH(9), CH(8)
            end


        up_it_by_one  /* Derive next number */
            for p% = 3% to 1% step -1%
                if str(next$,p%,1) < "0" or str(next$,p%,1) > "9"        ~
                                                              then L10820
            next p%
            p% = 0%  /* No Prefix        */
L10820:     convert str(next$,p%+1%) to temp%
            temp% = temp% + 1%
            if temp% >= 10**(8-p%) then temp% = 1%
            convert temp% to temp$, pic(00000000)
            if p% <> 0% then str(temp$,,p%) = str(next$,,p%)
            next$ = temp$
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution.                                     *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
