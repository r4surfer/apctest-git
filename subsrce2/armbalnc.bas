        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M  BBBB    AAA   L      N   N   CCC    *~
            *  A   A  R   R  MM MM  B   B  A   A  L      NN  N  C       *~
            *  AAAAA  RRRR   M M M  BBBB   AAAAA  L      N N N  C       *~
            *  A   A  R  R     I        S  A   A  L      N  NN  C       *~
            *  A   A  R   R  IIIII   SSS   A   A  LLLLL  N   N   CCC    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMBALNC - Determines A/R Balance for the following       *~
            *            levels - (1) Payment Group (10 characters)     *~
            *                     (2) Settlement Group (8 characters)   *~
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
            * 12/20/86 ! Original                                 ! ERN *~
            * 06/17/87 ! Added 's' type for special settling logic! HES *~
            * 12/10/87 ! Allow passing 'current' check number     ! HES *~
            * 07/11/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "ARMBALNC"   (billto$,       /* Bill-to Customer Code      */~
                          stlmnt$,       /* Settlement Number          */~
                          asof$,         /* As Of Date, unformatted    */~
                          level%,        /*  8% = Settlement Group     */~
                                         /* 10% = Payment Group        */~
                          buffer$,       /* N/Y/s Include CRCBUFFRs    */~
                                         /* Check # to bypass          */~
                          #1,            /* ARMTRIAL Channel           */~
                          #2,            /* CRCBUF2  Channel           */~
                          balances())    /* (1)ARMTRIAL Balance        */
                                         /* (2)Buffer File Balances    */
                                         /* (3)Sum of 1 & 2            */

*        STLMNT$ must be passed in as at least 8 or 10 characters
*                depending on the level specified.
*

        dim                                                              ~
            asof$6,                      /* As Of Date                 */~
            balances(3),                 /* Returned Balances          */~
            billto$9,                    /* Bill-to Customer Number    */~
            blankdate$8,                 /* Blank date for comparison  */~
            buffer$8,                    /* Include Buffers? (Y/N)     */~
            plowkey$50,                  /* A Plow Key                 */~
            postdate$6,                  /* Date transaction posted    */~
            stlmnt$12                    /* Settlement Number          */

        dim f1%(12)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! ARMTRIAL ! A/R Trial Balance                        *~
            * #2  ! CRCBUF2  ! Cash Receipts Buffer- Distribution       *~
            *************************************************************~


        REM *************************************************************~
            *             I N I T I L I Z A T I O N                     *~
            *-----------------------------------------------------------*~
            * Check some arguments, clear some variables.               *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            mat balances = zer
            if asof$ = " " or asof$ = blankdate$ then asof$ = date
            if billto$ = " " then end
            if level% <> 10% then level% = 8%
            break% = level% + 9%

        REM *************************************************************~
            *                 M A I N   L O G I C                       *~
            *-----------------------------------------------------------*~
            * Do what we are supposed to do.                            *~
            *************************************************************

*        FIRST get the Trial Balance's Balance
            plowkey$ = str(billto$,,9) & str(stlmnt$,,level%) & hex(00)

        tb_loop
            call "PLOWNEXT" (#1, plowkey$, break%, f1%(1))
            if f1%(1) = 0% then L10190
                get #1 using L10130, amt, postdate$
L10130:              FMT POS(68), PD(14,4), POS(97), CH(6)
                if postdate$ > asof$ and buffer$ = "s" then L10170
                if postdate$ > asof$ then tb_loop
                     balances(1) = balances(1) + amt
                     goto tb_loop

L10170:     REM For item not to settle...
            balances(1) = 9999999999.9999
            goto L10340

L10190
*        NEXT get the entries in the Buffer File.
            if len(str(buffer$)) <> 8% then L10340

            plowkey$ = str(billto$,,9) & str(stlmnt$,,level%) & hex(00)

        buffer_loop
            call "PLOWALTS" (#2, plowkey$, 1%, break%, f1%(2))
            if f1%(2) = 0% then L10340
                get #2 using L10280, temp$, type$, amt, allowd, unallowd
L10280:              FMT POS(10),CH(8),POS(22),CH(1),POS(93),3*PD(14,4)
                if temp$ = buffer$ then buffer_loop
                if type$ = "S" or type$ = "G" then buffer_loop
                     balances(2) = balances(2) - (amt - allowd - unallowd)
                     goto buffer_loop


L10340
*        Add the two balances together and we are done
            balances(3) = balances(1) + balances(2)


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

