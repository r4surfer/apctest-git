        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS   H   H  PPPP    AAA   RRRR   RRRR   PPPP   TTTTT   *~
            *  S      H   H  P   P  A   A  R   R  R   R  P   P    T     *~
            *   SSS   HHHHH  PPPP   AAAAA  RRRR   RRRR   PPPP     T     *~
            *      S  H   H  P      A   A  R   R  R   R  P        T     *~
            *   SSS   H   H  P      A   A  R   R  R   R  P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SHPARRPT - THIS PROGRAM GENERATES A REPORT OF ORDERS THAT *~
            *            HAVE BEEN EITHER SCHEDULED FOR SHIPMENT, OR    *~
            *            HAVE ACTUALLY BEEN SHIPPED BUT HAVE NOT YET    *~
            *            BEEN INVOICED.  YOU MAY ALSO PRINT A REPORT    *~
            *            SHOWING ALL SALES ORDERS MEETING THIS CRITERIA *~
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
            * 09/17/87 ! ORIGINAL                                 ! DAW *~
            * 02/21/92 ! PRR 11723  BOLs w/no supporting SO will  ! JDH *~
            *          !   state 'Orphaned' and have no value.    !     *~
            *          ! Now shows Rpt ID, GL Acct is formatted,  !     *~
            *          !   call to MANUAL fixed, time fixed.      !     *~
            * 03/26/92 ! Corrected image statement at ln 50320    ! MJB *~
            * 11/30/92 ! PRR 12668  Corrected double GL format.   ! JDH *~
            * 03/17/93 ! PRR 12823  1100 PRRs and 13 months ago   ! JDH *~
            *          !   I made a mistake about orphaned BOLs.  !     *~
            *          !   Now an orphaned line is just skipped.  !     *~
            * 06/19/95 ! PRR 13332-Add Rpt on Core Value UnInvoicd! RJH *~
            * 04/03/96 ! PRR 13567  Use the Stocking Price.       ! JDH *~
            * 09/05/96 ! Millie date conversion                   ! DER *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC
        dim                                                              ~
            blankdate$8,                 /* blank unfmt date           */~
            bol$3,                       /* Bill of Lading Number      */~
            bolextcost$10,               /* Line cost - report         */~
            bolprice$10,                 /* Line price - report        */~
            boldisc$10,                  /* Line discount - report     */~
            bol_price$10,                /* Line total after disc.     */~
            carrier$6,                   /* Carrier Number             */~
            core_descr$32,               /* Core Part Number Descr     */~
            core_part$25,                /* Core Part Number           */~
            core_fg_acct$12,             /* Core Finished Goods Acct   */~
            core_rpt$1,                  /* Core Report Flag           */~
            core_value$12,               /* Core Value                 */~
            core_fg_old$12,              /* Core Finished Goods Acct   */~
            cursor%(2),                  /* Cursor location for edit   */~
            compname$60,                 /* Company name for header    */~
            cost(30,12),                 /* Unit cost array            */~
            costw(12),                   /* Work array                 */~
            costt(30),                   /* Work array                 */~
            date$8,                      /* Date for screen display    */~
            extcost(30),                 /* Extended cost array        */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            grndextcost$12,              /* Grand total ext. cost      */~
            grndunitprc$12,              /* Grand total unit price     */~
            grnddisc$12,                 /* Grand total sales discount */~
            grndextunit$12,              /* Grand total ext. unit price*/~
            hnyship$12,                  /* Interim Cost of Goods      */~
            i$(24)80,                    /* Screen Image               */~
            invoice$8,                   /* Invoice Number             */~
            inpmessage$79,               /* Informational Message      */~
            line2$79,                    /* Screen Line #2             */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            output$1,                    /* Report type 1, 2, or 3     */~
            part$25,                     /* Part Number                */~
            prtmsg$60,                   /* Report Title String        */~
            plowkey2$25,                 /* Secondary plow string      */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            readkey$25,                  /* Miscellaneous Read Key     */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            seqnr$3,                     /* Sequnce number             */~
            shpcosts$(30)96,             /* Work array for costs       */~
            shpqty(30),                  /* Quantity shipped/scheduled */~
            shipto$9,                    /* Ship to Customer Number    */~
            so_no$16,                    /* Sales Order Number         */~
            sched_date$8,                /* Ship to Date - Scheduled   */~
            ship_date$9,                 /* Ship to Date - Actual      */~
            userid$3                     /* Current User Id            */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con
                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SHPHDRS  ! Shipment Scheduling / Pre-Invoicing- Hea *~
            * # 2 ! SHPLINES ! Shipment Scheduling Lines                *~
            * # 3 ! SHPCOSTS ! Appendix File for Costs                  *~
            * # 4 ! BCKLINES ! Sales Orders Line Items                  *~
            * # 5 ! SYSFILE2 ! CMS System info file                     *~
            * # 6 ! HNYMASTR ! Inventory Master File                    *~
            * # 7 ! HNYQUAN  ! Inventory Store Quantity File            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "SHPHDRS",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  28

            select # 2, "SHPLINES",                                      ~
                        varc,     indexed, recsize  =  600,              ~
                        keypos =   10, keylen =  22

            select # 3, "SHPCOSTS",                                      ~
                        varc,     indexed, recsize  = 1500,              ~
                        keypos =    1, keylen =  23

            select # 4, "BCKLINES",                                      ~
                        varc,     indexed, recsize  =  300,              ~
                        keypos =   10, keylen =  19

            select # 5, "SYSFILE2",                                      ~
                        varc,     indexed, recsize  =  500,              ~
                        keypos =    1, keylen =  20

            select # 6, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select # 7, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  650,                                  ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =   44

            select #50, "WORKFILE",                                      ~
                         varc,     indexed,  recsize =   51,             ~
                         keypos =   1, keylen = 34,                      ~
                         alt key  1, keypos = 10, keylen = 34

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (# 6, fs%( 6%), f2%( 6%), 0%, rslt$( 6%))
            call "OPENCHCK" (# 7, fs%( 7%), f2%( 7%), 0%, rslt$( 7%))
            workopen% = 1%     /* #50 Not Opened */

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            call "COMPNAME" (12%, compname$, 0%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            str(line2$,62) = "SHPARRPT: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub L29000

            for fieldnr% = 1% to  2%
L09230:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0 then L09350
L09250:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L09330
L09280:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L09250
                         if fieldnr% = 1% then L09230
                         goto L09280
L09330:               if keyhit% = 16 and fieldnr% = 1 then exit_program
                      if keyhit% <>  0 then       L09250
L09350:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L09250
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************~

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editpg1
L09510:     fieldnr% = cursor%(1) - 14  /* Input line is on line 15 */
            if fieldnr% < 1 or fieldnr% >  2% then editpg1
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
L09560:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L09560
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L09560
                  lastfieldnr% = fieldnr%
            goto L09510

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave     /* Actually Prints the Report Here */
            time$ = " "
            call "TIME" (time$)
            select printer(134)
            call "SETPRNT" ("SHP006", " ", 0%, 0%)
            goto L30000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20050,         /* output type        */    ~
                              L20100          /* Cpore Value        */
            return
L20050: REM Def/Enable enter report format type    OUTPUT$
            output$ = "3"
            return
L20100: REM Def/Enable enter Core Value            CORE_RPT$
            if core_rpt$ = " " then core_rpt$ = "N"
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter enter report type: 1-Scheduled, 2-Shipped, 3-Both Types",~
         "Enter 'Y' to report on Core Values Shipped but not invoiced"

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
            init(" ") errormsg$, inpmessage$, output$, core_rpt$
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************
        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
            call "SHOSTAT" ("Compiling Shipped-But-Not-Invoiced Report")
            grndextcost, grndunitprc, grnddisc, grndextunit = 0%
            if output$ = "1" then prtmsg$ = "SCHEDULED BUT NOT SHIPPED"
            if output$ = "2" then prtmsg$ = "SHIPPED ONLY"
            if output$ = "3" then                                        ~
               prtmsg$ = "SHIPPED/SCHEDLUED BUT NOT INVOICED"
            call "FMTTITLE" (prtmsg$, " ", 2%)  /* Center, No Expand */
            pagenumber% = 0%
            linecount% = 1000%

            plowkey$ = all (hex(00))
        main_loop
            bolextcost, bolprice, boldisc, bol_price = 0%
        REM Go after SHPHDRS
            call "PLOWNEXT" (#1, plowkey$, 0%, f1%(1))
              if f1%(1) = 0% then print_grnd_totals
            get #1, using L30230,shipto$,so_no$,bol$,store$,sched_date$,  ~
                carrier$, ship_date$, invoice$
L30230:         FMT CH(9), CH(16), CH(3), CH(3), POS(32), CH(6), CH(6),  ~
                    POS(184), CH(6), POS(234), CH(8)
            if invoice$ <> " " then main_loop
            if output$ = "1" and  sched_date$ <> blankdate$ and ~
                                  sched_date$ <> " "       and ~
                                 (ship_date$  =  blankdate$ or  ~
                                  ship_date$  =  " ") then L30350
            if output$ = "2" and ship_date$ <> " "          and ~
                                 ship_date$ <> blankdate$ then L30350
            if output$ = "3" then L30350
            goto main_loop

        REM Go after SHPLINES
L30350:     call "DATEFMT" (ship_date$)
            call "DATEFMT" (sched_date$)
            plowkey2$ = str(so_no$,,16) & str(bol$,,3) & hex(00)
        line_loop
            mat cost = zer: mat costt = zer: mat costw = zer
            init (hex(00)) shpcosts$()
            totqty, ext_price, totprice, totextcost, saledisc = 0
            mat shpqty = zer
            call "PLOWNEXT" (#2, plowkey2$, 19%, f1%(2))
              if f1%(2) = 0% then line_prints
            get #2 using L30440, seqnr$, qty_sched, shpqty()
L30440:       FMT POS(29), CH(3), PD(14,4), POS(220), 30 * PD(14,4)

        REM Go after SHPCOSTS
            readkey$=str(so_no$,,16) & str(bol$,,3) & str(seqnr$,,3) &"1"
            call "READ100" (#3, readkey$, f1%(3))
            if f1%(3) = 0% then L30570      /* Get GL Account # */
                get #3 using L30510, str(shpcosts$(),,1440)
L30510:              FMT XX(23), CH(1440)
                str(readkey$,23,1) = "2"
                call "READ100" (#3, readkey$, f1%(3))
                if f1%(3) = 0% then L30570  /* Get GL Account # */
                     get #3 using L30510, str(shpcosts$(),1441,1440)

L30570: REM Now go after GL account for shipped/not billed - HNYSHIP$
            call "ARMGLGET" (3%, " ", " ",  " ", " ", store$, " ",       ~
                             #5, #5, #5, #5, #5, hnyship$)
            if hnyship$ = " " then hnyship$ = "?interim"

        REM Go after BCKLINES
            readkey$ = str(so_no$,,16) & str(seqnr$,,3)
            call "READ100" (#4, readkey$, f1%(4))
              if f1%(4) = 0% then line_loop /* SO Line has been Deleted */
            get #4 using L30670, part$, unit_price, sale_disc
L30670:       FMT POS(32), CH(25), POS(141), PD(14,4), POS(173), PD(14,4)

        REM Accum unitprice, qtys, costs, discounts
            for row% = 1% to 30%
              get str(shpcosts$(row%)) using L30720, costw()
L30720:       FMT 12*PD(14,4)
              for col% = 1% to 12%
                cost(row%,col%) = costw(col%)
              next col%
            next row%
            for row% = 1% to 30%
              for col% = 1% to 12%
                costt(row%) = costt(row%) + cost(row%,col%)
              next col%
            next row%
        REM Calculate costs for pre-invoiced lines
            for x% = 1% to 30%
              extcost(x%) = shpqty(x%) * costt(x%)
              totqty      = totqty + shpqty(x%)
              totextcost  = totextcost + extcost(x%)
            next x%
            if qty_sched <> 0% then totprice = qty_sched * unit_price    ~
            else totprice = totqty * unit_price

        REM Sales discount calculation here
            if totprice = 0% then L30960  /* Don't Calc Discount */
            ext_price = (totprice - (totprice * (sale_disc / 100)))
            saledisc  = (totprice * (sale_disc / 100))

L30960:     bolextcost = bolextcost + totextcost
            bolprice   = bolprice   + totprice
            boldisc    = boldisc    + saledisc
            bol_price  = bol_price  + ext_price

        REM Check for cores and sum the core values as needed
            gosub accumulate_core_values   /* If requested */

            goto line_loop

        new_page
            print page
            pagenumber% = pagenumber% + 1%
            print using L50180, date$, compname$, pagenumber%
            print using L50210, time$, prtmsg$
            print skip(1)
            print using L50240
            print using L50270
            print using L50290
            linecount% = 6%
            return

        line_prints
            if linecount% > 55% then gosub new_page
            call "CONVERT" (bolextcost, 2.2, bolextcost$)
            call "CONVERT" (bolprice,   2.2, bolprice$)
            call "CONVERT" (boldisc,    2.2, boldisc$)
            call "CONVERT" (bol_price,  2.2, bol_price$)
            grndextcost = grndextcost + bolextcost
            grndunitprc = grndunitprc + bolprice
            grnddisc    = grnddisc    + boldisc
            grndextunit = grndextunit + bol_price
            if ship_date$ = " " or ship_date$ = blankdate$ then ~
               ship_date$ = "Scheduled"
            print using L50320,shipto$, so_no$, bol$, sched_date$,        ~
              carrier$, ship_date$, hnyship$, bolextcost$, bolprice$,    ~
              boldisc$, bol_price$
            linecount% = linecount% + 1%
            goto main_loop

        print_grnd_totals
            if linecount% + 3 > 55% then gosub new_page
            call "CONVERT" (grndextcost, 2.2, grndextcost$)
            call "CONVERT" (grndunitprc, 2.2, grndunitprc$)
            call "CONVERT" (grnddisc,    2.2, grnddisc$)
            call "CONVERT" (grndextunit, 2.2, grndextunit$)
            print skip(1)
            print using L50350
            print using L50380, grndextcost$, grndunitprc$, grnddisc$,    ~
                               grndextunit$

            gosub print_core_values

            time$ = " " : call "TIME" (time$)
            print skip(1)
            print using L50420, time$
            close printer
            call "SETPRNT" ("SHP006", " ", 0%, 1%)
            goto inputmode

        REM *************************************************************~
            *           C O R E   V A L U E   S U B S                   *~
            *-----------------------------------------------------------*~
            * Accumulate Core Value Information and Report Subroutines  *~
            *************************************************************
        accumulate_core_values   /* Use work file to gather Part/Acct's */

            if core_rpt$  <> "Y" then return
            if ship_date$  = " " or ship_date$ = blankdate$ then return
            core_value_total = 0

            if workopen% <> 0% then                                      ~
                call "WORKOPEN" (#50, "IO", 500%, workopen%)

*        Check to see if this is a Reman part or not.  Call CORVALSB
*        with 'CK' Check option, get back core finished goods account
*        and the core standard costs as extras

            core_part$ = part$
            call "CORVALSB" ("CK", core_part$, " ", " ", corestccost,    ~
                             " ", core_fg_acct$, " ", " ", " ", 0%, " ", ~
                             " ", #7, #5, #6, #50, corevalue%)
                                        /* Note- #50 is dumby, Not Used */
            if corevalue%  <> 0% then return
            if corestccost  = 0  then return

            core_value = round(totqty * corestccost, 2)

            readkey$ = str(core_fg_acct$,,9%) & core_part$
            call "READ101" (#50, readkey$, f1%(50%))
            if f1%(50%) <> 0% then L32330
                put #50 using L32300, str(core_fg_acct$,,9%), core_part$, ~
                                     str(core_fg_acct$,,9%),  core_value
L32300:         FMT CH(9), CH(25), CH(9), PD(14,4)
                write #50
                return
L32330:     get #50 using L32340, core_value_total
L32340:         FMT POS(44), PD(14,4)
            core_value_total = core_value_total + core_value
            put #50 using L32340, core_value_total
            rewrite #50

            return

        print_core_values
            if core_rpt$ <> "Y" then return
            core_fg_old$  = " "
            core_tot_value, core_sub_value = 0
            gosub new_core_page

            readkey$ = hex(00)
            call "READ104" (#50, readkey$, f1%(50%))
            goto L32520
         print_core_loop
            call "READNEXT" (#50, f1%(50%))
L32520:     if f1%(50%) = 0% then goto print_core_value_end
            get #50 using L32540, core_fg_acct$, core_part$, core_value
L32540:         FMT CH(9), CH(25), POS(44), PD(14,4)
            call "GLFMT" (core_fg_acct$)
            if core_fg_acct$ <> core_fg_old$ then gosub print_new_coreacct
            call "CONVERT"(core_value, 2.2, core_value$)
            core_tot_value = core_tot_value + core_value
            core_sub_value = core_sub_value + core_value
            call "DESCRIBE" (#6, core_part$, core_descr$, 0%, f1%(6%))

            if linecount% < 56% then L32650
                 core_continue% = 1%
                 gosub new_core_page
L32650:     print using L50520, core_part$, core_descr$, core_value$

            linecount% = linecount% + 1%
            core_fg_old$ = core_fg_acct$

            goto print_core_loop
          /* ** END Print Core Loop Section ** */

        new_core_page
            prtmsg$ = "Core Value Report"
            call "FMTTITLE" (prtmsg$, " ", 2%)  /* Center, No Expand */
            print page
            pagenumber% = pagenumber% + 1%
            print using L50180, date$, compname$, pagenumber%
            print using L50210, time$, prtmsg$
            print skip(1)
            linecount% = 3%
            if core_fg_old$ = " " then return
            gosub print_new_coreacct

            return

        print_new_coreacct
            if core_fg_old$ = " " or core_continue% = 1% then L32950
                print 50490
                call "CONVERT"(core_sub_value, 2.2, core_value$)
                print using L50550, core_value$
                print
                core_sub_value = 0
                linecount% = linecount% + 3%

L32950:     print
            if core_continue% = 0% then print using L50440,  core_fg_acct$~
                 else print using L50440,  str(core_fg_acct$) & "  Cont..."
            print using L50460
            print using L50490
            linecount% = linecount% + 4%
            core_continue% = 0%

            return

        print_core_value_end
                if core_fg_old$ <> " " then L33065
                     gosub print_new_coreacct
                     print using L50520, " ", "No Core Parts", " "
L33065:         print using L50490
                call "CONVERT"(core_sub_value, 2.2, core_value$)
                print using L50550, core_value$

                call "CONVERT"(core_tot_value, 2.2, core_value$)
                print using L50580, core_value$
                print
                call "FILEBGON" (#50)
                workopen% = 1%     /* Not Opened */
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************
        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40130,         /* output type       */   ~
                                L40130          /* Core Value Report */
              goto L40150

L40130:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */

L40150:     accept                                                       ~
               at (01,02),                                               ~
                  "Shipped and/or Scheduled but not Invoiced Report",    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,05), "This program will generate a listing of items~
        ~ that have been",                                                ~
               at (07,05), "Shipped, or have been Scheduled for shipment ~
        ~ BUT have not yet",                                              ~
               at (08,05), "been Invoiced.",                             ~
               at (09,05), "These reports will help you analyze which ord~
        ~ers need to be either",                                          ~
               at (10,05), "Shipped and/or Invoiced.",                   ~
               at (11,05), "Indicate below 1) Scheduled for Shipment.",  ~
               at (12,05), "               2) Shipped but not Invoiced.",~
               at (13,05), "               3) Both Scheduled and Shipped.~
        ~",                                                               ~
                                                                         ~
               at (15,05), "Enter the Report Output Type",               ~
               at (15,35), fac(lfac$( 1)), output$              , ch(01),~
               at (16,05),"Report On Core Parts Shipped but not Invoiced(~
        ~Y/N)?",                                                          ~
               at (16,60), fac(lfac$( 2%)), core_rpt$           , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40470
                  call "MANUAL" ("SHPARRPT") : goto L40150

L40470:        if keyhit% <> 15 then L40500
                  call "PRNTSCRN" : goto L40150

L40500:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40690     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40650
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40650:     if fieldnr% > 2% then L40670
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40670:     return

L40690: if fieldnr% > 0% then L40780  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40780:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* output type       */     ~
                              L50140          /* Core Value Report */
            return
L50100: REM Test for enter report format type     OUTPUT$
            if pos("321" = output$) = 0 then                             ~
               errormsg$ = "Report type MUST be a 1, 2, or 3"
            return

L50140: REM Test for Core Value Report           CORE_RPT$
            if pos("YN" = core_rpt$) = 0 then                            ~
               errormsg$ = "Please Enter 'Y'es or 'N'o"
            return

        REM *************************************************************~
            *  PRINT LINE FORMATS                                       *~
            *************************************************************
L50180: %########                        ################################~
        ~###########################                            Page: ###

L50210: %########                        ################################~
        ~###########################                        SHPARRPT: SHP0~
        ~06
L50240: %                                  Scheduled           Shipped   ~
        ~ Interim                       Extended         Sales         Tot~
        ~al
L50270: %Ship To    Sales Order No.   BOL  Date      Carrier   Date      ~
        ~ C.O.G.S.       Ext. Cost    Unit Price      Discount    Ext. Pri~
        ~ce
L50290: %---------  ----------------  ---  --------  --------  --------- ~
        ~ ------------  ----------    ----------    ----------    --------~
        ~--
L50320:    %#########  ################  ###  ########  ########  #######~
        ~##  ############  ##########    ##########    ##########    #####~
        ~#####
L50350: %================================================================~
        ~=================================================================~
        ~==
L50380: %                                                      REPORT GRA~
        ~ND TOTALS    ############  ############  ############  ##########~
        ~##

L50420: %                        *** END OF REPORT @ ######## ***

L50440: % Core Finished Goods Account: ############

L50460: %   Core Part Number          Core Part Description           Val~
        ~ue

L50490: %   ------------------------  ------------------------------  ---~
        ~---------

L50520: %   ########################  ##############################  ###~
        ~#########

L50550: %                                           Account Subtotal  ###~
        ~#########

L50580: %                                           Core Value Total  ###~
        ~#########

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

        exit_program
            call "SHOSTAT" ("Closing Data Files, One Moment Please")

            end
