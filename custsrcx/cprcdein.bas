        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   PPPP   RRRR    CCC   DDD    EEEEE  IIIII  N   N   *~
            *  C      P   P  R   R  C      D  D   E        I    NN  N   *~
            *  C      PPPP   RRRR   C      D   D  EEEE     I    N N N   *~
            *  C      P      R  R   C      D  D   E        I    N  NN   *~
            *   CCC   P      R   R   CCC   DDD    EEEEE  IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CPRCDEIN - Manage Customer Pricing Codes File.            *~
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
            * 06/05/86 ! ORIGINAL                                 ! ERN *~
            * 02/02/87 ! Specials Pricing Expansion               ! ERN *~
            * 05/06/87 ! Standard Cost Enahancements              ! ERN *~
            * 12/11/87 ! Correct Margin validation/computation.   ! JIM *~
            * 02/01/89 ! Proj 7880714 new price set implementation! JIM *~
            * 04/10/89 ! CONV in HNYMASTR should be 7 decimal posn! GGO *~
            * 03/07/90 ! Corrected goto line #'s in TEST_MARGINS  ! LAB *~
            *          ! subroutine                               !     *~
            * 04/10/90 ! No longer calls CPRUPDSB at WRITE time.  ! JDH *~
            * 04/23/91 ! Fixed PRR's                              ! JBK *~
            *          !  11840- PF(7) to Copy Current to Future  !     *~
            *          ! Added call to ALLFREE                    !     *~
            *          ! Fixed unformated date on future screen   !     *~
            * 01/26/93 ! PRR 11509 Added copy with % increase.    ! JDH *~
            * 06/26/96 ! Add blank date for tests                 ! DER *~
            *          ! Add effective date display               !     *~
            * 07/05/90 ! APC MODS - FOR 35 PRICE CODES            ! RHH *~
            *          !   LINES - 1060,1072,1150,2050,11420,11640!     *~
            *          !           11820,(30190-30210),(30250-    !     *~
            *          !           30262),(30270-30280),(31090-   !     *~
            *          !           31110)(40330-41720),50221,50260!     *~
            * 11/20/97 ! Mod for Upgrade to new Release R6.04.03  ! RHH *~
            * 04/07/98 ! Mod to Upgrade to Current Version (EWD)  !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blank_date$8,                /* Blank date for tests       */~
            cby$3, fby$3, by$3,          /* User who made last change  */~
            clcdat$8, flcdat$8, lcdat$8, /* Last Change Date           */~
            lcdat_dsply$8,               /* Last Change display        */~
            date$8,                      /* Date for screen display    */~
            descr$32,                    /* Part Description           */~
            eff_date$8, eff_save$8,      /* Fut pr set date & save area*/~
            eff_dt_dsply$10,             /* Fut pr display date        */~
            eff$(2)1,                    /* Fut pr set date FACs       */~
            eff_msg$10,                                                  ~
            errormsg$79,                 /* Error message              */~
            fac1$1,    fac2$(36%)1,      /* Part and Price Code FACsAPC*/~
            fac3$(8)1, fac4$( 8)1,       /* Markup and Margin FACS     */~
            fut_data$207,                /* Future price set data      */~
            filler1$200,                 /* APC MOD - FILLER           */~
            filler2$156,                 /* APC MOD - FILLER           */~
            hdr1$6, hdr2$6, hdr3$10,     /* Column Headings            */~
            inpmessage$79,               /* Informational Message      */~
            key$50,                      /* Record Key                 */~
            line2$79,                    /* Second screen line         */~
            cmarkup(8), markup$(8)7,     /* Markup Percentages         */~
            cmargin(8), margin$(8)7,     /* Margin Percentages         */~
            fmarkup(8), fmargin(8),      /* Markup, margin %ages       */~
            part$25,                     /* Part Number                */~
            cpc(36%),fpc(36%),pc$(36%)10,/* Price Code Unit Prices  APC*/~
            cprice(8), fprice(8), price$(8)10, /* Percent Price Display*/~
            pf$(3)79,                    /* PF Key Prompts             */~
            pf11msg$18,                                                  ~
            pfk$12,                      /* PF Keys Available          */~
            std$10,                      /* Current Std Cost           */~
            stkuom$4,                    /* Stocking UOM               */~
            uom$4, uomdescr$16,          /* Pricing Unit of Measure    */~
            userid$                      /* Current User               */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Pricing Maintenance Utility       "
            pname$ = "CPRCDEIN - Rev: R7.00"

        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! CPRPRICE ! Customer Pricing File ( APC File Change )*~
            * #2  ! HNYMASTR ! Parts Master File                        *~
            * #3  ! GENCODES ! General Codes File                       *~
            * #4  ! SYSFILE2 ! CMS System Information File              *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "CPRPRICE",                                      ~
                        varc, indexed,                                   ~
                        recsize =  700,                                  ~
                        keypos =     1, keylen =  47

            select  #2, "HNYMASTR",                                      ~
                         varc, indexed,                                  ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos =  90, keylen = 4, dup

            select  #3, "GENCODES",                                      ~
                         varc, indexed,                                  ~
                         recsize = 128,                                  ~
                         keypos = 1, keylen = 24

            select  #4, "SYSFILE2",                                      ~
                         varc, indexed, recsize = 500,                   ~
                         keypos = 1, keylen = 20

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, 0%, f2%(1), 100%, " ")
            call "OPENCHCK" (#2, 0%, f2%(2),   0%, " ")
            call "OPENCHCK" (#3, 0%, f2%(3),   0%, " ")
            call "OPENCHCK" (#4, 0%, f2%(4),   0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date  :  call "DATEFMT" (date$)
            blank_date$ = " " : call "DATUFMTC" ( blank_date$ )
            call "EXTRACT" addr("ID", userid$)
            str(line2$,62) = "CPRCDEIN: " & cms2v$
            hdr1$ = "Markup"
            hdr2$ = "Margin"
            hdr3$ = "Unit Price"
            eff_msg$ = "Effective:"
            gosub default_mode

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * No real input mode - just load defaults from disk.        *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, part$, descr$, uom$,       ~
                uomdescr$, clcdat$, cby$, pc$(), markup$(), margin$(),   ~
                price$(), stkuom$, std$, by$, flcdat$, fby$,             ~
                str(line2$,37,10), lcdat$, eff_date$, eff_dt_dsply$
            call "ALLFREE"

            for fieldnr% = 1% to 1%      /* Just want to get the Part  */
L10120:         gosub'111(0%, 1%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then       L65000
                      if keyhit% <>  0 then       L10120
                gosub'151(0%)
                      if errormsg$ <> " " then L10120
                next fieldnr%


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editmode
L11060:     gosub'111(1%, 0%)            /* Get which section to edit  */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  7 then       L11080
                        gosub copy_current
                        goto L11060
L11080:           if keyhit%  =  8 then       edit_codes
                  if keyhit%  =  9 then       edit_markups
                  if keyhit%  = 10 then       edit_margins
                  if keyhit% <> 11 then       L11110
                        if future% = 0%                                  ~
                            then gosub future_prices                     ~
                            else gosub current_prices
                        goto L11060
L11110:           if keyhit%  = 12 then gosub delete_record
                  if keyhit%  = 16 then       datasave
                  goto L11060

            edit_codes   : section% = 2% : goto L11190
            edit_markups : section% = 3% : goto L11190
            edit_margins : section% = 4% : goto L11190

L11190:     gosub'111(section%, fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11190
            gosub'151(section%)
                  if errormsg$ <> " " then L11190
                  goto L11060


        delete_record
            keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "D E L E T E",                     ~
                            "Enter PF-16 To DELETE the above record",    ~
                            "-OR-", "Press RETURN to Cancel Delete."  )
            if keyhit1% <> 16% then return
                key$ = "C" & str(part$)
                call "DELETE" (#1, key$, 47%)
                return clear all
                goto inputmode


        current_prices
            gosub default_mode
            by$ = cby$ : lcdat$ = clcdat$
            lcdat_dsply$ = lcdat$
            call "DATEFMT" (lcdat_dsply$)
            init (" ") pc$(), margin$(), markup$(), price$()
            for i% = 1% to 36%                     /* APC MOD 07/05/90 */
                if cpc(i%) >= 0 then call "CONVERT" (cpc(i%),4.4, pc$(i%))
            next i%
            for i% = 1% to 8%
                if cmargin(i%) = -999.99 then L11500
                     call "CONVERT" (cmargin(i%), 1.2, margin$(i%))
                     call "CONVERT" (cmarkup(i%), 1.2, markup$(i%))
                     call "CONVERT" (cprice (i%), 4.4, price$ (i%))
L11500:     next i%
            return

        future_prices
L11540:     u3% = 2% /* Bottom of screen */
            call "ASKUSER" (u3%, "*** USER CONFIRMATION ***",            ~
                "Please confirm that you wish to change to Future Price M~
        ~ode.", "Press PF(1) for Future Price Mode.", "Press PF(16) to con~
        ~tinue current prices.")
            if u3% = 16% then return
            if u3% <> 1% then goto L11540
            future% = 1%
            str(line2$,,63) = "Mode: Future Price Set effective on " &   ~
                eff_dt_dsply$
            pf11msg$ = "                  "        /* APC MOD 07/05/90 */
            eff$(1) = hex(8c) : eff$(2) = hex(84)
            init (" ") by$, lcdat$
            init (" ") pc$(), margin$(), markup$(), price$()
            if eff_date$ = blank_date$ or eff_date$ = " " then goto L11700
                by$ = fby$ : lcdat$ = flcdat$
L11700:     for i% = 1% to 16%
                if fpc(i%) >= 0 then call "CONVERT" (fpc(i%),4.4, pc$(i%))
            next i%
            for i% = 1% to 8%
                if fmargin(i%) = -999.99 then L11780
                     call "CONVERT" (fmargin(i%), 1.2, margin$(i%))
                     call "CONVERT" (fmarkup(i%), 1.2, markup$(i%))
                     call "CONVERT" (fprice (i%), 4.4, price$ (i%))
L11780:     next i%
            lcdat_dsply$ = lcdat$
            call "DATEFMT" (lcdat_dsply$)
            return

        default_mode /* FUTURE% = 0 is 'current'; = 1 is 'future' */
            future% = 0% : pf11msg$ = "(11)FUTURE prices"
            init (hex(9c)) eff$()
            str(line2$,,61) = "Mode: Current Price Set Structure"
            return

        copy_current /* Copy current price data to future data */
            if future% <> 1% then return
            u3% = 0%  /* Bottom of screen */
            call "ASKNUMBR" (u3%, "*** COPY CURRENT TO FUTURE ***",      ~
                 "Indicate a percent increase (or decrease) or accept " &~
                 "zero for no change.", -100, 1000, factor, 1.1)
            if u3% = 1% then return
            factor = (1 + (factor / 100))
            mat fpc     = (factor) * cpc
            mat fmargin = cmargin
            mat fmarkup = cmarkup
            mat fprice  = cprice

            init (" ") pc$(), margin$(), markup$(), price$()
            for i% = 1% to 16%
                if fpc(i%) <  0 then fpc(i%) = -1  /* keep -1 if it was */
                if fpc(i%) >= 0 then call "CONVERT" (fpc(i%),4.4, pc$(i%))
            next i%
            for i% = 1% to 8%
                if fmargin(i%) = -999.99 then L12130
                     call "CONVERT" (fmargin(i%), 1.2, margin$(i%))
                     call "CONVERT" (fmarkup(i%), 1.2, markup$(i%))
                     call "CONVERT" (fprice (i%), 4.4, price$ (i%))
L12130:     next i%
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub L31000
            goto  inputmode


        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            * --------------------------------------------------------- *~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%  /* PUT MSG AREA AT BOTTOM OF SCREEN  */
            call "STARTOVR" (keyhit1%)
                if keyhit1% = 1% then return

                return clear all
                goto inputmode

        REM *************************************************************~
            *                  L O A D   D A T A                        *~
            * --------------------------------------------------------- *~
            * Pull data from the file and format it.                    *~
            *************************************************************
        load_data
*        First load in part data.  Record already in buffer.
            get #2 using L30080, stkuom$, uom$, conv
L30080:         FMT XX(73), 2*CH(4), PD(14,7)
            call "STCCOSTS" (part$, " ", #4, 1%, std)
            std = std * conv
            call "CONVERT" (std, -4.4, std$)

            uomdescr$ = "** Not of File **"
            call "READ100" (#3, "UOM      " & uom$, f1%(3))
            if f1%(3) = 0% then L30180
                get #3 using L30160, uomdescr$
L30160:              FMT XX(24), CH(30)

L30180                                             /* (EWD) - Begin   */ 
*        Now load in the Pricing information
            mat cpc = con : mat cpc = (-1) * cpc
            mat fpc = con : mat fpc = (-1) * fpc
        REM FOR I% = 1% TO 8%                      /* APC MOD 07/05/90 */
        REM     CMARKUP(I%), CMARGIN(I%), FMARKUP(I%), FMARGIN(I%)       ~
        REM         = -999.99
        REM NEXT I%
            key$ = "C" & part$
            call "CPRUPDSB" (#1, 0%, "00", key$, 0%, f1%(1))
            if f1%(1) = 0% then return             /* APC MOD 07/05/90 */
            get #1 using L30260, cby$, clcdat$, cpc(), filler1$, filler2$

L30260:       FMT XX(47), CH(3), CH(6), 36*PD(14,4), CH(200), CH(156)

        REM GOSUB CALC_PER_MARKUP                  /* APC MOD 07/05/90 */
            for i% = 1% to 36%                     /* APC MOD 07/05/90 */
                if future% = 0% then goto L30290
                    if fpc(i%) >= 0 then call "CONVERT" (fpc(i%),4.4,    ~
                        pc$(i%))
                    goto L30300
L30290:         if cpc(i%) >= 0 then call "CONVERT" (cpc(i%),4.4, pc$(i%))
L30300:     next i%                                /* (EWD) - End      */
            eff_dt_dsply$ = eff_date$
            call "DATFMTC" (eff_dt_dsply$)
            if future% = 0% then goto L30317
                by$ = fby$ : lcdat$ = flcdat$
                lcdat_dsply$ = lcdat$
                call "DATEFMT" (lcdat_dsply$)
                str(line2$,37,10) = eff_dt_dsply$
                return
L30317:     by$ = cby$ : lcdat$ = clcdat$
            lcdat_dsply$ = lcdat$
            call "DATEFMT" (lcdat_dsply$)
            return


        calc_per_markup
            for i% = 1% to 8%
                if cmarkup(i%) <>  -999.99 then L30368
                     cmargin(i%) = -999.99
                     goto L30378
L30368:         cprice (i%) = std + (std * cmarkup(i%) * .01)
                cmargin(i%) = 0 : if cprice(i%) = 0 then L30374
                     cmargin(i%) = round(100 * (1 - (std/cprice(i%))), 2)
L30374:         cprice (i%) = round(cprice(i%), 4)

L30378:         if fmarkup(i%) <>  -999.99 then L30384
                     fmargin(i%) = -999.99
                     goto L30430
L30384:         fprice (i%) = std + (std * fmarkup(i%) * .01)
                fmargin(i%) = 0 : if fprice(i%) = 0 then L30390
                     fmargin(i%) = round(100 * (1 - (std/fprice(i%))), 2)
L30390:         fprice (i%) = round(fprice(i%), 4)
L30430:     next i%
            gosub format_numbers
            return

        calc_per_margin
            for i% = 1% to 8%
                if cmargin(i%) <>  -999.99 then L30488
                     cmarkup(i%) = -999.99
                     goto L30500
L30488:         cprice (i%) = (100 * std) / (100 - cmargin(i%))
                cmarkup(i%) = 0 : if std = 0 then L30494
                    cmarkup(i%) = round(((100*cprice(i%))/ std) - 100, 2)
L30494:         cprice (i%) = std + (std * cmarkup(i%) * .01)
                cprice (i%) = round(cprice(i%), 4)

L30500:         if fmargin(i%) <>  -999.99 then L30506
                     fmarkup(i%) = -999.99
                     goto L30560
L30506:         fprice (i%) = (100 * std) / (100 - fmargin(i%))
                fmarkup(i%) = 0 : if std = 0 then L30512
                    fmarkup(i%) = round(((100*fprice(i%))/ std) - 100, 2)
L30512:         fprice (i%) = std + (std * fmarkup(i%) * .01)
                fprice (i%) = round(fprice(i%), 4)
L30560:     next i%
            gosub format_numbers
            return


        format_numbers
            init (" ") margin$(), markup$(), price$()
            for i% = 1% to 8%
                if future% = 0% then goto L30646
                    if fmargin(i%) = -999.99 then L30680
                        call "CONVERT" (fmargin(i%), 1.2, margin$(i%))
                        call "CONVERT" (fmarkup(i%), 1.2, markup$(i%))
                        call "CONVERT" (fprice (i%), 4.4, price$ (i%))
                        goto L30680

L30646:         if cmargin(i%) = -999.99 then L30680
                     call "CONVERT" (cmargin(i%), 1.2, margin$(i%))
                     call "CONVERT" (cmarkup(i%), 1.2, markup$(i%))
                     call "CONVERT" (cprice (i%), 4.4, price$ (i%))
L30680:     next i%
            return


L31000: REM *************************************************************~
            *                  S A V E   D A T A                        *~
            * --------------------------------------------------------- *~
            * Write this stuff back to the file.                        *~
            *************************************************************

            key$ = "C" & part$
            if eff_date$ <> blank_date$ and eff_date$ <> " " then goto L31062
                mat fpc = con : mat fpc = (-1) * fpc
                for i% = 1% to 8%
                    fmarkup(i%) = -999.99
                next i%
L31062: REM PUT FUT_DATA$ USING 31066, EFF_DATE$, USERID$, DATE, FPC(),  ~
        REM     FMARKUP()
        REM     FMT CH(6), CH(3), CH(6), 16*PD(14,4), 8*PD(14,4)
            call "CPRUPDSB" (#1, 0%, "01", key$, 0%, f1%(1))
                                                   /* APC MOD 07/05/90 */
            put #1 using L31066, "C", part$, " ", userid$, date, cpc(),   ~
                                filler1$, filler2$
L31066:              FMT CH(1), CH(25), CH(21), CH(3), CH(6),            ~
                         36*PD(14,4), CH(200), CH(156)
            if f1%(1) = 0% then write #1 else rewrite #1
            return

        REM *************************************************************~
            *             S C R E E N   H A N D L I N G                 *~
            * --------------------------------------------------------- *~
            * Handles the only, the only, screen.                       *~
            *************************************************************

        deffn'111(section%, fieldnr%)
            init (hex(84)) fac1$
            init (hex(8c)) fac2$(), fac3$(), fac4$()
            on section% + 1%  goto part, which, codes, markups, margins
            part   : fac1$ = hex(81)
                     gosub pfkeys_part
                     goto  L40330

            which  : gosub pfkeys_which
                     if future% = 1% then eff$(2) = hex(84)
                     init (hex(84)) fac1$, fac2$(), fac3$(), fac4$()
                     goto  L40330

            codes  : if fieldnr% = 0% then init(hex(82)) fac2$()         ~
                                      else init(hex(82)) fac2$(fieldnr%)
                     if future% = 0% then goto L40200
                        eff$(2) = hex(82)
                     if eff_date$ = blank_date$ or eff_date$ = blankdate$ ~
                        then eff_date$ = eff_save$
L40200:              gosub pfkeys_edit
                     goto  L40330

            markups: if fieldnr% = 0% then init(hex(82)) fac3$()         ~
                                      else init(hex(82)) fac3$(fieldnr%)
                     if future% = 0% then goto L40250
                        eff$(2) = hex(82)
                     if eff_date$ = blank_date$ or eff_date$ = blankdate$ ~
                        then eff_date$ = eff_save$
L40250:              gosub pfkeys_edit
                     goto  L40330

            margins: if fieldnr% = 0% then init(hex(82)) fac4$()         ~
                                      else init(hex(82)) fac4$(fieldnr%)
                     if future% = 0% then goto L40300
                        eff$(2) = hex(82)
                     if eff_date$ = blank_date$ or eff_date$ = blankdate$ ~
                        then eff_date$ = eff_save$
L40300:              gosub pfkeys_edit
                     goto  L40330

L40330:   accept                    /* APC MOD 11/20/97 (EWD) - Begin */          ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
            at (05,02), "Part Code: ",                                   ~
            at (05,13), fac(fac1$), part$                       , ch(25),~
            at (05,47), "UOMs: Pricing:",                                ~
            at (05,62), fac(hex(84)), uom$                      , ch(04),~
            at (05,67), "Stocking: ",                                    ~
            at (05,77), fac(hex(84)), stkuom$                   , ch(04),~
            at (06,13), fac(hex(8c)), descr$                    , ch(32),~
            at (06,47), "Last Change: ",                                 ~
            at (06,60), fac(hex(8c)), lcdat_dsply$              , ch(08),~
            at (06,70), "by ",                                           ~
            at (06,73), fac(hex(8c)), by$                       , ch(03),~
            at (07,02), fac(eff$(1)), eff_msg$                  , ch(10),~
            at (07,13), fac(eff$(2)), eff_dt_dsply$             , ch(10),~
                                                                         ~
            at (08,02), "** Unit Price Assignments **",                  ~
            at (09,06),"A", at(09,24),"B", at(09,42),"C", at(09,60), "D",~
            at (10,06),"E", at(10,24),"F", at(10,42),"G", at(10,60), "H",~
            at (11,06),"I", at(11,24),"J", at(11,42),"K", at(11,60), "L",~
            at (12,06),"M", at(12,24),"N", at(12,42),"O", at(12,60), "P",~
                                                                         ~
            at (14,06),"R", at(14,24),"S", at(14,42),"T", at(14,60), "U",~
            at (15,06),"V", at(15,24),"W", at(15,42),"X", at(15,60), "Y",~
            at (16,06),"Z", at(16,24),"0", at(16,42),"1", at(16,60), "2",~
            at (17,06),"3", at(17,24),"4", at(17,42),"5", at(17,60), "6",~
            at (18,06),"7", at(18,24),"8", at(18,42),"9"                ,~
                    at (09,08), fac(fac2$( 1%)),   pc$( 1%)             , ch(10),~
            at (09,26), fac(fac2$( 2%)),   pc$( 2%)             , ch(10),~
            at (09,44), fac(fac2$( 3%)),   pc$( 3%)             , ch(10),~
            at (09,63), fac(fac2$( 4%)),   pc$( 4%)             , ch(10),~
            at (10,08), fac(fac2$( 5%)),   pc$( 5%)             , ch(10),~
            at (10,26), fac(fac2$( 6%)),   pc$( 6%)             , ch(10),~
            at (10,44), fac(fac2$( 7%)),   pc$( 7%)             , ch(10),~
            at (10,63), fac(fac2$( 8%)),   pc$( 8%)             , ch(10),~
            at (11,08), fac(fac2$( 9%)),   pc$( 9%)             , ch(10),~
            at (11,26), fac(fac2$(10%)),   pc$(10%)             , ch(10),~
            at (11,44), fac(fac2$(11%)),   pc$(11%)             , ch(10),~
            at (11,63), fac(fac2$(12%)),   pc$(12%)             , ch(10),~
            at (12,08), fac(fac2$(13%)),   pc$(13%)             , ch(10),~
            at (12,26), fac(fac2$(14%)),   pc$(14%)             , ch(10),~
            at (12,44), fac(fac2$(15%)),   pc$(15%)             , ch(10),~
            at (12,63), fac(fac2$(16%)),   pc$(16%)             , ch(10),~
                                                                         ~
            at (14,08), fac(fac2$(18%)),   pc$(18%)             , ch(10),~
            at (14,26), fac(fac2$(19%)),   pc$(19%)             , ch(10),~
            at (14,44), fac(fac2$(20%)),   pc$(20%)             , ch(10),~
            at (14,63), fac(fac2$(21%)),   pc$(21%)             , ch(10),~
            at (15,08), fac(fac2$(22%)),   pc$(22%)             , ch(10),~
            at (15,26), fac(fac2$(23%)),   pc$(23%)             , ch(10),~
            at (15,44), fac(fac2$(24%)),   pc$(24%)             , ch(10),~
            at (15,63), fac(fac2$(25%)),   pc$(25%)             , ch(10),~
            at (16,08), fac(fac2$(26%)),   pc$(26%)             , ch(10),~
            at (16,26), fac(fac2$(27%)),   pc$(27%)             , ch(10),~
            at (16,44), fac(fac2$(28%)),   pc$(28%)             , ch(10),~
            at (16,63), fac(fac2$(29%)),   pc$(29%)             , ch(10),~
            at (17,08), fac(fac2$(30%)),   pc$(30%)             , ch(10),~
            at (17,26), fac(fac2$(31%)),   pc$(31%)             , ch(10),~
            at (17,44), fac(fac2$(32%)),   pc$(32%)             , ch(10),~
            at (17,63), fac(fac2$(33%)),   pc$(33%)             , ch(10),~
            at (18,08), fac(fac2$(34%)),   pc$(34%)             , ch(10),~
            at (18,26), fac(fac2$(35%)),   pc$(35%)             , ch(10),~
            at (18,44), fac(fac2$(36%)),   pc$(36%)             , ch(10),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)), pf$(1)                     ,ch(79),~
            at (23,02), fac(hex(8c)), pf$(2)                     ,ch(79),~
            at (24,02), fac(hex(8c)), pf$(3)                     ,ch(79),~
                                                                         ~
                keys(pfk$),                                              ~
                key (keyhit%)

            if keyhit% <> 13 then L41250
                call "MANUAL" ("CPRCDEIN")
                goto L40330

L41250:     if keyhit% <> 15 then L41290
                call "PRNTSCRN"
                goto L40330

L41290:     if keyhit% <> 14 then return
                call "PLOWCODE" (#3, "PRICECODE ", " ", 9%, 0.3, f1%(3))
                goto L40330


        pfkeys_part      /* Keys for entering Part Numebr             */
            inpmessage$ = "Enter Part Code."
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            str(pf$(3),63,1) = hex(84)
            pfk$ = hex(0001ffff0d0f10ffffffff)
            return

        pfkeys_which     /* Keys for entering Which Section to edit   */
            inpmessage$ = "Select Section to manage by pressing PF(8), "&~
                "PF(9), or PF(10)"
            pf$(1) = "(1)Start Over   (8)Manage Prices        " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                    (12)" &        ~
                     "Delete Record          (15)Print Screen"
            pf$(3) = "                                    (14)" &        ~
                     "See Code Descriptions  (16)Save Data   "
            str(pf$(1),37,18) = pf11msg$
            pfk$ = hex(000108090a0b0c0d0e0f10)
            if future% <> 1% then L41550
                str(pf$(3),1,15) = "(7)Copy Prices"
                pfk$ = hex(00010708090a0b0c0d0e0f10)
L41550:     return

        pfkeys_edit      /* Keys for when editing a section           */
            if section% = 2% then                                        ~
                inpmessage$ = "Enter Prices and press (RETURN)."
            if section% = 3% then                                        ~
                inpmessage$ = "Enter Markup %s and press (RETURN)."
            if section% = 4% then                                        ~
                inpmessage$ = "Enter Margin %s and press (RETURN)."
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                    (14)" &        ~
                     "See Code Descriptions                  "
            pfk$ = hex(0001ffffffffff0d0e0fff)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 1.                        *~
            *************************************************************

        deffn'151(section%)
            errormsg$ = " "
            fieldnr%  = 0%

            on section% + 1% goto test_part, test_part, test_prices,     ~
                                  test_markups, test_margins

        test_part    /* Test that part is on-file and then load data   */
            call "GETCODE" (#2, part$, descr$, 0%, 0.32, f1%(2))
            if f1%(2) = 1% then L50170
                errormsg$ = "Part not found on file."  :  return
L50170:     gosub load_data
            return clear all
            goto editmode


        test_prices
            future% = 0%                           /* APC MOD 07/05/90 */
            if future% = 1% then gosub val_date    /* (EWD) - Mods     */
            if errormsg$ <> " " then return
            init (hex(8c)) fac2$()
            for fieldnr% = 1% to 36%               /* APC MOD 07/05/90 */
                f% = fieldnr%                      /* (EWD) - Mod      */
*           If entry is blank then flag in array and get out.
                if pc$(f%) = " " and future% = 0% then cpc(f%) = -1
                if pc$(f%) = " " and future% = 1% then fpc(f%) = -1
                if pc$(f%) = " " then L50400
*           Test if entry is A-Ok
                if future% = 0% then goto L50340
                    convert pc$(f%) to fpc(f%), data goto L50350
                    goto L50371
L50340:         convert pc$(f%) to cpc(f%), data goto L50350 : goto L50371
L50350:              errormsg$ = "Invalid entry for price."
                     fac2$(f%) = hex(82)
                     return
L50371:         if future% = 0% then goto L50380
                    if fpc(f%) < 0 or fpc(f%) > 9e10 then L50350
                    call "CONVERT" (fpc(f%), 4.4, pc$(f%))
                    goto L50400
L50380:         if cpc(f%) < 0 or cpc(f%) > 9e10 then L50350
                call "CONVERT" (cpc(f%), 4.4, pc$(f%))
L50400:     next fieldnr%
            return



        test_markups
            if future% = 1% then gosub val_date
            if errormsg$ <> " " then return
            init (hex(8c)) fac3$()
*        First Check that entries are valid
            for fieldnr% = 1% to 8%
                f% = fieldnr%
                if markup$(f%) <> " " then L50510
                    if future% = 0%                                      ~
                        then cmarkup(f%) = -999.99                       ~
                        else fmarkup(f%) = -999.99
                    goto L50580
L50510:         if future% = 0% then goto L50525
                    convert markup$(f%) to fmarkup(f%), data goto L50535
                    goto L50550
L50525:         convert markup$(f%) to cmarkup(f%), data goto L50535
                goto L50550
L50535:              errormsg$ = "Markup must be 0 to 10000"
                     fac3$(f%) = hex(82)
                     return
L50550:         if future% = 0% then goto L50570
                    if fmarkup(f%) < 0 or fmarkup(f%) > 10000 then L50535
                    call "CONVERT" (fmarkup(f%), 1.2, markup$(f%))
                    goto L50580
L50570:         if cmarkup(f%) < 0 or cmarkup(f%) > 10000 then L50535
                call "CONVERT" (cmarkup(f%), 1.2, markup$(f%))
L50580:     next fieldnr%
            gosub calc_per_markup
            return

        test_margins
            if future% = 1% then gosub val_date
            if errormsg$ <> " " then return
            init (hex(8c)) fac4$()
*        First Check that entries are valid
            for fieldnr% = 1% to 8%
                f% = fieldnr%
                if future% = 0% then goto L50730
                    if margin$(f%) <> " " then L50710
                        fmargin(f%) = -999.99
                        goto L50800
L50710:             convert margin$(f%) to fmargin(f%), data goto L50755
                    if fmargin(f%) < 0 or fmargin(f%) > 99 then L50755
                    call "CONVERT" (fmargin(f%), 1.2, margin$(f%))
                    goto L50800
L50730:         if margin$(f%) <> " " then L50745
                     cmargin(f%) = -999.99
                     goto L50800
L50745:         convert margin$(f%) to cmargin(f%), data goto L50755
                goto L50770
L50755:              errormsg$ = "Margin valid range is 0.00 to 99.00"
                     fac4$(f%) = hex(82)
                     return
L50770:         if cmargin(f%) < 0 or cmargin(f%) > 99 then L50755
                call "CONVERT" (cmargin(f%), 1.2, margin$(f%))
L50800:     next fieldnr%
            gosub calc_per_margin
            return


        val_date
            if eff_dt_dsply$ <> " " then goto L50900
                str(line2$,37,10) = " "
                init (" ") pc$(), price$(), markup$(), margin$()
                return
L50900:     call "DATEOKC" (eff_dt_dsply$, eff_date%, errormsg$)
            if errormsg$ <> " " then return
            call "DATUFMTC" (eff_dt_dsply$)
            if str(eff_dt_dsply$, 1%, 6%) > date then goto L50960
            call "DATFMTC" (eff_dt_dsply$)
                errormsg$ = "Effective Date must be later than today"
                return
L50960:     eff_date$ = eff_dt_dsply$
            eff_save$ = eff_date$
            call "DATFMTC" (eff_dt_dsply$)
            str(line2$,37,10) = eff_dt_dsply$
            return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")

            end 
