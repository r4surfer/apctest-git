        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  EEEEE  W   W  DDDD    AAA    GGG   IIIII  N   N   GGG    *~
            *  E      W   W  D   D  A   A  G        I    NN  N  G       *~
            *  EEEE   W W W  D   D  AAAAA  G  GG    I    N N N  G  GG   *~
            *  E      WW WW  D   D  A   A  G   G    I    N  NN  G   G   *~
            *  EEEEE  W   W  DDDD   A   A   GGG   IIIII  N   N   GGG    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDAGING - Determines A/R Aging at a Bill-to, Payment     *~
            *            Group (10 characters), or Settlement Group     *~
            *            (8 characters) Level, and/or Currency Level.   *~
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
            * 06/09/88 ! Added aging by source document date      ! ERN *~
            * 04/11/89 ! Added Args to handle Multi-Currency.     ! RJM *~
            *          !   CYCLE_FLAG% controls it all especially !     *~
            *          !   if Customer is Invoiced in more than   !     *~
            *          !   one currency.                          !     *~
            * 11/10/89 ! Rewrote MC handling                      ! MJB *~
            * 04/26/92 ! G/L Range Include/exclude for aging      ! KAB *~
            * 05/26/94 ! PRR 13189- Under special conditions, the ! JBK *~
            *          !   index to the aging array was set to    !     *~
            *          !   zero which caused crash.               !     *~
            * 07/11/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "EWDAGING"   (billto$,       /* Bill-to Customer Code      */~
                          stlmnt$,       /* Settlement Number          */~
                          asof$,         /* As Of Date, unformatted    */~
                          ageper$,       /* 'D'isc, 'N'et, 'I'nvoice   */~
                          parms%(),      /* Aging Parameters           */~
                          #1,            /* ARMTRIAL Channel           */~
                          aging(),       /* Aging figures elements 1-7 */~
                                         /* (8) is total due,          */~
                                         /* (9) is past due per as of  */~
                          #2,            /* ARMTBCEX Channel           */~
                          #3,            /* SYSFILE2 CHANNEL           */~
                          cycle_flag%,   /* Current Currency Flag      */~
                              /*  0% = Return in Statutory Currency.   */~
                              /*       Accumulates all Transactions    */~
                              /*       Reguardless of Currency type.   */~
                              /*       (ie, Works Like before)         */~
                              /*  1% = Return in Transaction Currency  */~
                              /*  2% = Return Aging Amt's in currency  */~
                              /*       specific array the follows.     */~
                          curcode$,      /* Currency of Aged Amounts   */~
                          currage$(),                                    ~
                              /*        50 * 76 array for returning    */~
                              /*        currency specific agings.      */~
                              /*  pos 1 - 4 is currency code           */~
                              /*  pos 5 - 76 are the 9 aged amounts    */~
                              /*      same as returned in AGING()      */~
                          acctie$,                                       ~
                          acctl$, accth$,                                ~
                              /* G/L Account Range to Include/exclude  */~
                              /* Only looked at if STLMNT$ = " "       */~
                          types$)        /* Selected Doc Srces/Types   */

*        STLMNT$ must be passed in blank for bill-to aging, 1-8 for
*                group and > 8 for payment.
*
*        PARMS%() passes the aging parameters defined in ARMDATES.
*
*        CURRAGE$() is currently in statements only, it passes back
*         the currency code and the aging for that currency in each of
*         up to 50 different currencies.  The cycle flag must = 2 to
*         activate this option.
*        TYPES is a string of two character entries: Source then Type.
*          If passed blank the all sources and types are assumed.

        dim                                                              ~
            acctie$1,                    /* GL Account Incl/excl flag  */~
            acctl$9, accth$9, acct$9,    /* GL Account Range           */~
            agedate$6,                   /* Date to use for aging      */~
            ageper$1,                    /* D=Discount, N=Net Due Date */~
            aging(9),                    /* Aging Buckets              */~
            asof$6,                      /* As Of Date                 */~
            blankdate$8,                 /* Blankdate for comparison   */~
            billto$9,                    /* Bill-to Customer Number    */~
            curcode$4,                   /* Currency of Aged Amounts   */~
            currency$4,                  /* Transaction Currency       */~
            curr$1,                      /* Multi-Currency Flag        */~
            currage$(50)76,              /* Currency & aged amts array */~
            discdue$6,                   /* Cash Discount Due Date     */~
            netdue$6,                    /* Net Due Date               */~
            p%(1),                       /* Receiver for Searches      */~
            parms%(10),                  /* Aging Parameters           */~
            plowkey$50,                  /* A Plow Key                 */~
            postdate$6,                  /* Date transaction posted    */~
            sddate$6,                    /* Source Doc Date            */~
            stat$4,                      /* Statutory Currency Code    */~
            stlmnt$12,                   /* Settlement Number          */~
            type$2,                      /* Source and Type Codes      */~
            types$20                     /* Selected Document Types    */

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
            * #2  ! ARMTBCEX ! A/R Trial Balance Currency Exposure File *~
            * #3  ! SYSFILE2 ! SYSTEM FILE                              *~
            *************************************************************~


        REM *************************************************************~
            *             I N I T I L I Z A T I O N                     *~
            *-----------------------------------------------------------*~
            * Check some arguments, clear some variables.               *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            if beenherebefore% = 1% then L09130
                curr$ = "N"
                call "READ100" (#3, "SWITCHS.CUR", f1%(3))
                     if f1%(3) = 1% then get #3 using L09110, curr$, stat$
L09110:                  FMT POS(21), CH(1), CH(4)

L09130:     init(hex(00)) currage$()
            mat aging = zer
            beenherebefore% = 1%
            if asof$   = " " or asof$ = blankdate$ then asof$ = date
            if billto$ = " " then end
            if pos("DNI" = ageper$) = 0% then ageper$ = "D"
            break% = 9%
            if stlmnt$ <> " "    then break% = 17%
            if len(stlmnt$) > 8% then break% = 19%
            if curr$ <> "Y" and cycle_flag% <> 0% then cycle_flag% = 0%


        REM *************************************************************~
            *                 M A I N   L O G I C                       *~
            *-----------------------------------------------------------*~
            * Do what we are supposed to do.                            *~
            *************************************************************


            plowkey$ = str(billto$,,9) & stlmnt$
            str(plowkey$,20) = hex(00)

        tb_loop
            call "PLOWNEXT" (#1, plowkey$, break%, f1%(1))
            if f1%(1) = 0% then L65000

            get #1 using L10150, discdue$, netdue$, amt, acct$, type$,    ~
                                postdate$, sddate$, currency$
L10150:         FMT POS(30), CH(6), XX(1), CH(6), POS(68), PD(14,4),     ~
                    POS(76), CH(9), XX(2), CH(2), POS(97), 2*CH(6),      ~
                    POS(177), CH(4)

            if str(plowkey$,20,2) <> "00" then L10180
               tstage = 0%
            if break% > 9% then L10180
               if acctl$ = "ALL" then L10180
                  if acctie$ = "I" then L10177
                     if acct$ < acctl$ then L10180
                     if acct$ > accth$ then L10180
L10175:                 str(plowkey$,20,2) = hex(ff)
                        goto tb_loop
L10177:              if acct$ < acctl$ then L10175
                     if acct$ > accth$ then L10175

L10180:     if postdate$ <= asof$ then L10190
                if str(plowkey$,20%,2%) = "00" then b% = 1%
                goto tb_loop

L10190:     if cycle_flag% = 0% then L10340
                call "READ100" (#2, key(#1), f1%(2))
                     if f1%(2) = 0% then L10340   /* OH WELL            */
                get #2 using L10206, currency$, amt
L10206:              FMT CH(4), POS(26), PD(14,4)

L10340:     if currency$ = " " then currency$ = stat$
            if str(plowkey$,20,2) <> "00" then L10540
                if ageper$ = "D" then agedate$ = discdue$
                if ageper$ = "N" then agedate$ = netdue$
                if ageper$ = "I" then agedate$ = sddate$
                call "DATE" addr("G-", agedate$, asof$, days%, u3%)
                if days% >= parms%(8) and days% <= parms%(9) then L10430
                     str(plowkey$,20,2) = hex(ff)
                     goto tb_loop
L10430:         b% = 1%  :  u3% = u3%
                for x% = 1% to parms%(10) - 1%
                     if days% > parms%(x%) then b% = x% + 1%
                next x%
                curcode$ = currency$

L10540:     if str(plowkey$,20,2) <> "00" then gosub tst_bal
            aging(b%) = aging(b%) + amt  /* That's a big 'b' */
            aging(8%) = aging(8%) + amt  /* and That's an eight */
            if days%  > 0% then aging(9%) = aging(9%) + amt
            if cycle_flag% = 2% then mc_load
            goto tb_loop

        mc_load
            for i% = 1% to 50%
                if str(currage$(i%),,4) = hex(00000000) then             ~
                    str(currage$(i%),,4) = curcode$
                if str(currage$(i%),,4) <> curcode$ then L11150
                get str(currage$(i%), 8*b%-3, 8) using L11170, currage
                    currage = currage + amt
                put str(currage$(i%), 8*b%-3, 8) using L11170, currage
                get str(currage$(i%), 61, 8) using L11170, currage
                    currage = currage + amt
                put str(currage$(i%), 61, 8) using L11170, currage
                if days% <= 0% then L11140
                get str(currage$(i%), 69, 8) using L11170, currage
                    currage = currage + amt
                put str(currage$(i%), 69, 8) using L11170, currage
L11140:         goto tb_loop
L11150:     next i%

L11170:         FMT PD(14,4)

        tst_bal
            if types$ = " " then return
                search types$ = type$ to p%() step 2
                if p%(1) = 0% then tstage = tstage + amt
        return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
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
            if aging(b%) <> 0.00 then aging(b%) = (aging(b%) - tstage)

            end

