        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   PPPP   RRRR    CCC   DDD    EEEEE  RRRR   PPPP    *~
            *  C      P   P  R   R  C      D  D   E      R   R  P   P   *~
            *  C      PPPP   RRRR   C      D   D  EEEE   RRRR   PPPP    *~
            *  C      P      R  R   C      D  D   E      R  R   P       *~
            *   CCC   P      R   R   CCC   DDD    EEEEE  R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CPRCDERP - Customer Pricing- Codes Report.                *~
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
            * 06/11/86 ! ORIGINAL                                 ! ERN *~
            * 02/02/87 ! Specials Pricing Expansion               ! ERN *~
            * 05/06/87 ! Standard Cost Enhancements               ! ERN *~
            * 02/02/89 ! Proj 7880714 new price set implementation! JIM *~
            * 04/10/89 ! CONV from HNYMASTR should be 7 decimals  ! GGO *~
            * 04/21/92 ! Remove extra lines for 'non printed' part! KAB *~
            * 06/26/90 ! APC MODIFICATIONS FOR 35 PRICE CODES     ! RHH *~
            *          ! (NO MARKUP OR MARGINS)                   !     *~
            * 11/20/97 ! Mod for Upgrade to new Release R6.04.03  ! RHH *~
            * 04/07/98 ! Make (EWD) Mods to the Latest Caelus     ! RHH *~
            *          !    Version. Conditioned for (Y2K)        !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            code$1,                      /* Price Code                 */~
            comment$50,                  /* Report Comment             */~
            company$60,                  /* Company Name               */~
            cursor%(2),                  /* Cursor Location            */~
            date$8,                      /* Date for screen display    */~
            descr$32,                    /* Part Description           */~
            errormsg$79,                 /* Error message              */~
            file$1,                      /* Logical file ID            */~
            from$25, rfrom$26,           /* From range parameters      */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Screen FACs                */~
            line2$79,                    /* Second screen line         */~
            margin$7,                    /* Margin Percent             */~
            markups$(8)1,                /* Markups to Print           */~
            markup(8),                   /* Markups                    */~
            markup$7,                    /* Markup Percent             */~
            part$25,                     /* Part Number                */~
            pcs$(36)1,                   /* Price Codes to Print       */~
            plowkey$99,                  /* A Plow Key            (EWD)*/~
            price(36),                   /* Percent Price Displays(EWD)*/~
            price$10,                    /* Unit Price                 */~
            pf16$16,                     /* PF 16 Prompt               */~
            std$10,                      /* Standard Cost              */~
            time$8,                      /* Run Time                   */~
            to$25, rto$26,               /* To range parameters        */~
            uom$4                        /* Pricing Unit of Measure    */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Caelus Price Code Report Utility  "
            pname$ = "CPRCDERP - Rev: R7.00"

        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! CPRPRICE ! Customer Pricing File                    *~
            * #2  ! HNYMASTR ! Parts Master File                        *~
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

            select  #4, "SYSFILE2",                                      ~
                         varc, indexed, recsize = 500,                   ~
                         keypos = 1, keylen = 20

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, 0%, f2%(1), 100%, " ")
            call "OPENCHCK" (#2, 0%, f2%(2),   0%, " ")
            call "OPENCHCK" (#4, 0%, f2%(4),   0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date  :  call "DATEFMT" (date$)
            str(line2$,62) = "CPRCDERP: " & cms2v$

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * Get initial values for selection criteria.                *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, from$, to$, pcs$(),        ~
                      markups$(), comment$

            for fieldnr% = 1% to 3%            /* APC MOD - 06/26/90 */
                gosub'051(fieldnr%)            /* (EWD) - Mod        */
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then       L65000
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
                next fieldnr%


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editmode
L11060:     gosub'111(0%)
                if keyhit%  =  1 then gosub startover
                if keyhit%  = 16 then       print_report
                                                   /* (EWD) - Begin */
                l% = cursor%(1)
                if l% >=  6% and l% <=  7% then fieldnr% = 1%
                if l% >=  9% and l% <= 15% then fieldnr% = 2%
        REM     IF L% >= 13% AND L% <= 14% THEN FIELDNR% = 3%
                if l% >= 16% and l% <= 16% then fieldnr% = 3%
                if fieldnr% = 0% then L11060
                                                   /* (EWD) - End   */
            gosub'051(fieldnr%)
L11190:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11190
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11190
                  goto L11060


        REM *************************************************************~
            *             P R I N T    R E P O R T                      *~
            * --------------------------------------------------------- *~
            * Print the report with the codes requested.                *~
            *************************************************************
        print_report
            if str(pcs$()) <> " " or str(markups$()) <> " " then L12100
                errormsg$ = "You must select something to print"
                goto editmode

L12100:     call "SHOSTAT" ("Printing Report")
            call "TIME" (time$)
            call "COMPNAME" (12%, company$, u%)
            line% = 857%
            plowkey$ = rfrom$
            call "CPRUPDSB" (#1, 0%, "PN", plowkey$, 1%, f1%(1))
            if f1%(1) = 1% then L12210
                call "ASKUSER" (2%, "NO RECORDS",                        ~
                   "No records were found within the report parameters", ~
                   "Press any key to continue...", " ")
                goto inputmode
L12210:     select printer(134)
            call "SETPRNT" ("CPR001", " ", 0%, 0%)


*        Print out Select Criteria

L12252:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L12260
               str(i$(), i%, 1%) = hex(20)
               goto L12252

L12260:     page% = -1%  :  gosub print_heading
            print skip(3), tab(20), "Report Selection Criteria:"
            print skip(2)
            for i% = 6% to 19%
                print tab(25), i$(i%)
            next i%
            line% = 857%
            goto L12400

        report_loop                              /* APC MOD - 06/26/90 */
            call "CPRUPDSB" (#1, 0%, "RN", key(#1), 0%, f1%(1)) /*(EWD)*/
                if f1%(1) = 0% then goto end_of_report
            get  #1 using L12380, file$
L12380:         FMT CH(1)
            if file$ <> "C" then end_of_report
L12400:     get  #1 using L12410, part$, price()  /* APC MOD - 06/26/90 */
L12410:         FMT XX(1), CH(25), POS(57), 36*PD(14,4) /* (EWD) Mod    */
            if part$ > str(rto$,2) then end_of_report
            full_line% = 1%

            call "READ100" (#2, part$, f1%(2))
            if f1%(2) = 1% then L12520
                descr$ = "** NOT ON FILE **"
                uom$   = "****"
                std    = 0
                conv   = 1
                goto L12560
L12520:     get #2 using L12530, descr$, uom$, conv
L12530:         FMT XX(25), CH(32), POS(78), CH(4), PD(14,7)
            call "STCCOSTS" (part$, " ", #4, 1%, std)
            std = std * conv
L12560:     call "CONVERT" (std, 4.4, std$)

*        Print Std Price Code First
            if str(pcs$()) = " " then L12750
                for p% = 1% to 36%                    /* (EWD) - Begin */
                     if pcs$(p%) = " " or price(p%) = -1 then L12730
                          price$, markup$, margin$ = " "
                          if p% > 26% then goto L12632 /* APC 06/26/90 */
                             code$ = bin(p%+64%, 1)
                             goto L12640
L12632:                   code$ = bin(p%+21%, 1)

L12640:                   price = price(p%)
                          call "CONVERT" (price, 4.4, price$)
                          if std = 0 then L12690
                               markup = 100 * ((price/std) - 1)
                               call "CONVERT" (markup, 2.2, markup$)
L12690:                   if price = 0 then L12720
                               margin = 100 * (1 - (std/price))
                               call "CONVERT" (margin, 2.2, margin$)
L12720:                   gosub print_line
L12730:         next p%
                goto L12920               /* APC MOD - 06/26/90 */
                                                       /* (EWD) - End */
L12750
*        Print Markups
            goto L12920                   /* APC MOD - 06/26/90 */
            if str(markups$()) = " " then L12920       /* (EWD) - Mod */ 
                for p% = 1% to 8%
                     if markups$(p%) = " " or markup(p%) = -999.99       ~
                                                              then L12900
                          price$, markup$, margin$ = " "
                          convert p% to code$, pic(0)
                          markup = markup(p%)
                          price = std + (std * markup * .01)
                          call "CONVERT" (price,  4.4, price$)
                          call "CONVERT" (markup, 2.2, markup$)
                          if price = 0 then L12890
                               margin = 100 * (1 - (std/price))
                               call "CONVERT" (margin, 2.2, margin$)
L12890:                   gosub print_line
L12900:         next p%

L12920: if full_line% = 1% then report_loop
        print " " : line% = line% + 1%
        goto report_loop


        print_line
            if line% > 52% then gosub print_heading
            if full_line% = 1% then                                      ~
                print using L13490, part$, descr$, uom$, std$,            ~
                                   code$, price$, markup$, margin$  else ~
                print using L13520, code$, price$, markup$, margin$
            full_line% = 0%
            line% = line% + 1%
            return


        print_heading
            page% = page% + 1%
            line% = 7%
            print page
            print using L13310, date$, time$, company$
            print using L13340, page%
            if page% = 0% then return
            if comment$ <> " " then print using L13370, comment$
            if comment$ <> " " then line% = line% + 1%
            print
            print using L13400
            print using L13430
            print using L13460
            full_line% = 1%
        return


        end_of_report
            print
            print "END OF REPORT"
            call "SETPRNT" ("CPR001", " ", 0%, 1%)
            goto L65000


L13310: %RUN DATE: ######## ########            #########################~
        ~###################################               CPRCDERP-CPR001

L13340: %                                                          STANDA~
        ~RD PRICE BOOK                                         PAGE: ###

L13370: %##################################################


L13400: %                                                                ~
        ~     STANDARD    PRICE

L13430: %PART NUMBER                PART DESCRIPTION                  UOM~
        ~       COST       CODE   UNIT PRICE   % MARKUP   % MARGIN

L13460: %-------------------------  --------------------------------  ---~
        ~-  ----------    -----   ----------   --------   --------

L13490: %#########################  ################################  ###~
        ~#  ##########      #     ##########    #######    #######

L13520: %                                                                ~
        ~                   #     ##########    #######    #######

        REM *************************************************************~
            *             D E F A U L T   /  E N A B L E S              *~
            * --------------------------------------------------------- *~
            * Actually justs sets INPMESSAGE$.                          *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "                       /* (EWD) - Begin */
            on fieldnr% gosub            L20200,    /* Part Range       */~
                                         L20250,    /* Codes            */~
                                         L20350     /* Rpt Comment      */

        REM                              20300,    /* Markups          */~
        REM                              20350     /* Rpt Comment      */
            return                                  /* (EWD) - End   */

L20200
*        Set Message for PART NUMBER RANGE
            inpmessage$ = "Enter Part Number Range, 'ALL', 'FIRST'," &   ~
                          " 'LAST'."
            return

L20250
*        Set Message for STANDARD PRICE CODES
            inpmessage$ = "Place a non-blank character next to codes" &  ~
                          " to print."
            return

L20300
*        Set Message for MARKUP PRICE CODES
            inpmessage$ = "Place a non-blank character next to codes" &  ~
                          " to print."
            return

L20350
*        Set Message for REPORT COMMENT
            inpmessage$ = "Enter comment to appear on report"
            return


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
            *             S C R E E N   H A N D L I N G                 *~
            * --------------------------------------------------------- *~
            * Handles the only, the only, screen.                       *~
            *************************************************************

        deffn'101(fieldnr%)              /* Input Mode                 */
            init (hex(8c)) lfac$()
            pf16$ = "(16)Exit Program"
            goto L40210

        deffn'111(fieldnr%)              /* Edit Mode                  */
            if fieldnr% = 0% then init (hex(84)) lfac$()  else           ~
                                  init (hex(8c)) lfac$()
            if fieldnr% = 0% then pf16$ = "(16)Print Report" else        ~
                                  pf16$ = " "
            if fieldnr% = 0% then                                        ~
                inpmessage$ = "Locate Cursor and press (RETURN) to" &    ~
                              " modify displayed fields."

                                                    /* (EWD) - Begin   */ 
L40210:     on fieldnr% gosub            L40280,    /* Part Range       */~
                                         L40270,    /* Price Codes      */~
                                         L40270     /* Report Comment   */

        REM                              40270,    /* Markups          */~
        REM                              40270     /* Report Comment   */
            goto L40320                             /* (EWD) - End    */

L40270:         lfac$(fieldnr%) = hex(80) : return
L40280:         lfac$(fieldnr%) = hex(81) : return
                lfac$(fieldnr%) = hex(82) : return

                                           /* (EWD) - Next 23 Lines */
L40320:   accept                                                         ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
            at (06,02), "Part Number Range- From",                       ~
            at (06,30), fac(lfac$( 1)), from$                   , ch(25),~
            at (07,02), "                   To  ",                       ~
            at (07,30), fac(lfac$( 1)),   to$                   , ch(25),~
                                                                         ~
            at (09,02), "Standard Prices Codes",                         ~
            at (10,14),"A", at(10,21),"B", at(10,28),"C", at(10,35), "D",~
            at (10,42),"E", at(10,49),"F", at(10,56),"G", at(10,63), "H",~
            at (11,14),"I", at(11,21),"J", at(11,28),"K", at(11,35), "L",~
            at (11,42),"M", at(11,49),"N", at(11,56),"O", at(11,63), "P",~
                                                                         ~
            at (13,14),"R", at(13,21),"S", at(13,28),"T", at(13,35), "U",~
            at (13,42),"V", at(13,49),"W", at(13,56),"X", at(13,63), "Y",~
            at (14,14),"Z", at(14,21),"0", at(14,28),"1", at(14,35), "2",~
            at (14,42),"3", at(14,49),"4", at(14,56),"5", at(14,63), "6",~
            at (15,14),"7", at(15,21),"8", at(15,28),"9"                ,~
            at (10,12), fac(lfac$(2%)),   pcs$( 1%)             , ch(01),~
            at (10,19), fac(lfac$(2%)),   pcs$( 2%)             , ch(01),~
            at (10,26), fac(lfac$(2%)),   pcs$( 3%)             , ch(01),~
            at (10,33), fac(lfac$(2%)),   pcs$( 4%)             , ch(01),~
            at (10,40), fac(lfac$(2%)),   pcs$( 5%)             , ch(01),~
            at (10,47), fac(lfac$(2%)),   pcs$( 6%)             , ch(01),~
            at (10,54), fac(lfac$(2%)),   pcs$( 7%)             , ch(01),~
            at (10,61), fac(lfac$(2%)),   pcs$( 8%)             , ch(01),~
            at (11,12), fac(lfac$(2%)),   pcs$( 9%)             , ch(01),~
            at (11,19), fac(lfac$(2%)),   pcs$(10%)             , ch(01),~
            at (11,26), fac(lfac$(2%)),   pcs$(11%)             , ch(01),~
            at (11,33), fac(lfac$(2%)),   pcs$(12%)             , ch(01),~
            at (11,40), fac(lfac$(2%)),   pcs$(13%)             , ch(01),~
            at (11,47), fac(lfac$(2%)),   pcs$(14%)             , ch(01),~
            at (11,54), fac(lfac$(2%)),   pcs$(15%)             , ch(01),~
            at (11,61), fac(lfac$(2%)),   pcs$(16%)             , ch(01),~
                                                                         ~
            at (13,12), fac(lfac$(2%)),   pcs$(18%)             , ch(01),~
            at (13,19), fac(lfac$(2%)),   pcs$(19%)             , ch(01),~
            at (13,26), fac(lfac$(2%)),   pcs$(20%)             , ch(01),~
            at (13,33), fac(lfac$(2%)),   pcs$(21%)             , ch(01),~
            at (13,40), fac(lfac$(2%)),   pcs$(22%)             , ch(01),~
            at (13,47), fac(lfac$(2%)),   pcs$(23%)             , ch(01),~
            at (13,54), fac(lfac$(2%)),   pcs$(24%)             , ch(01),~
            at (13,61), fac(lfac$(2%)),   pcs$(25%)             , ch(01),~
            at (14,12), fac(lfac$(2%)),   pcs$(26%)             , ch(01),~
            at (14,19), fac(lfac$(2%)),   pcs$(27%)             , ch(01),~
            at (14,26), fac(lfac$(2%)),   pcs$(28%)             , ch(01),~
            at (14,33), fac(lfac$(2%)),   pcs$(29%)             , ch(01),~
            at (14,40), fac(lfac$(2%)),   pcs$(30%)             , ch(01),~
            at (14,47), fac(lfac$(2%)),   pcs$(31%)             , ch(01),~
            at (14,54), fac(lfac$(2%)),   pcs$(32%)             , ch(01),~
            at (14,61), fac(lfac$(2%)),   pcs$(33%)             , ch(01),~
            at (15,12), fac(lfac$(2%)),   pcs$(34%)             , ch(01),~
            at (15,19), fac(lfac$(2%)),   pcs$(35%)             , ch(01),~
            at (15,26), fac(lfac$(2%)),   pcs$(36%)             , ch(01),~
                                                                         ~
            at (16,02), "Report Comment",                                ~
            at (16,18), fac(lfac$( 4)),   comment$              , ch(50),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), "(1)Start Over",                                 ~
            at (22,65), "(13)Instructions",                              ~
            at (23,65), "(15)Print Screen",                              ~
            at (24,65), fac(hex(84)), pf16$                      ,ch(16),~
                keys(hex(00010d0f10)),                                   ~
                key (keyhit%)

            if keyhit% <> 13 then L40950
                call "MANUAL" ("CPRCDERP")
                goto L40320

L40950:     if keyhit% <> 15 then L40990
                call "PRNTSCRN"
                goto L40320

L40990:     close ws
            call "SCREEN" addr("C", u%, "I", i$(), cursor%())
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 1.                        *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "                         /* (EWD) - Begin   */
            on fieldnr% gosub            L50130,    /* Part Range       */~
                                         L50230,    /* Price Codes      */~
                                         L50290     /* Comment          */

        REM                              50260,    /* Markups          */~
        REM                              50290     /* Comment          */
            return                                  /* (EWD) - End    */


L50130
*        Test PART NUMBER RANGE
            call "TESTRNGE" (from$, to$, str(rfrom$,2), str(rto$,2),     ~
                                                              errormsg$)
            str(rfrom$,,1), str(rto$,,1) = "C"
            return

L50230
*        Test STANDARD PRICE CODES
            return

L50260
*        Test MARKUPS
            return

L50290
*        Test COMMENT
            return


L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * Closes all the files currently open, and also displays    *~
            * a message (ONLY if in foreground) while linking to the    *~
            * next program.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please")

            end
