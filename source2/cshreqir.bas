        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC    SSS   H   H  RRRR   EEEEE   QQQ   IIIII  RRRR    *~
            *  C   C  S      H   H  R   R  E      Q   Q    I    R   R   *~
            *  C       SSS   HHHHH  RRRR   EEEE   Q   Q    I    RRRR    *~
            *  C   C      S  H   H  R   R  E      Q Q Q    I    R   R   *~
            *   CCC    SSS   H   H  R   R  EEEEE   QQQ   IIIII  R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CSHREQIR - GENERATES A CASH REQUIREMENTS REPORT           *~
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
            * 06/14/83 ! ORIGINAL                                 ! JRW *~
            * 05/19/86 ! Invoice Files Format Changes, Clean Up   ! HES *~
            * 08/23/89 ! Added printing of 2nd line if invoice is ! JDH *~
            *          !   in foreign currency.                   !     *~
            * 10/13/89 ! Aligned negative signs on report         ! MJB *~
            * 01/20/94 ! PRR 12990 - Modified method used to      ! JBK *~
            *          !   calculate balance in foreign currency  !     *~
            *          !   in an attempt to reduce rounding errors!     *~
            * 08/02/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            checkcurrkey$20,             /* Currency Check Key         */~
            currency$4,                  /* Currency code              */~
            curr_desc$20,                /* Currency description       */~
            curr_flag$1,                 /* Multi-Currency on?         */~
            curr_key$50,                 /* Currency PLOWKEY           */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            date$(3)8,                   /* ENTER THREE DATES          */~
            daten%(3),                   /* THREE DATES ENTERED IN INT */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            from_ven$9,                  /* ENTER VENDOR RANGE    FROM:*/~
            grand(8),                    /* GRAND TOTALS               */~
            header$79,                   /* Screen Title               */~
            hold$1,                      /* INVOICE HOLD FLAG          */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            message$79,                  /* MESSAGE TO SCREEN          */~
            name$30,                     /* FOR PRINT                  */~
            oldname$30,                  /* FOR PRINT                  */~
            pfdescr$(3)79,               /* FUNCTION KEYS ENABLED LISTS*/~
            pfkeys$32,                   /* FUNCTION KEYS ENABLED LISTS*/~
            plowkey$25,                  /* PAYABLES MASTER PLOWKEY$   */~
            printamt(8),                 /* LINE ITEM PRINTED AMOUNTS  */~
            printamt$(8)10,              /* LINE ITEM PRINTED AMOUNTS  */~
            stat$4,                      /* Statutory Currency code    */~
            subtot(8),                   /* VENDOR SUBTOTALS           */~
            subtot$(8)10,                /* VENDOR SUBTOTALS           */~
            tempcheck$8,                 /* Check Number               */~
            tempseq$3,                   /* Check Line Sequence Number */~
            tempvencode$9,               /* Check Vendor Code          */~
            tempinvoice$16,              /* Invoice Check Written To   */~
            time$8,                      /* RUN TIME                   */~
            to_ven$9                     /*                         TO:*/~

        dim f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! USERINFO ! Users Default Information File           *~
            * #02 ! PAYMASTR ! Payables main file                       *~
            * #03 ! VENDOR   ! VENDOR MASTER FILE                       *~
            * #04 ! SYSFILE2 ! System info file                         *~
            * #05 ! CSHLINES ! Disbursements Check Detail File          *~
            * #40 ! CURMASTR ! Currency Master file                     *~
            * #41 ! PAYLNCUR ! PAYLINES Currency Shadow file            *~
            * #42 ! CSHLNCUR ! Currency Information for CSHLINES        *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1, "USERINFO",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =   3

            select #2, "PAYMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos = 1,    keylen = 25

            select #3, "VENDOR",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos = 1,    keylen =  9,                      ~
                        alt key  1, keypos = 10, keylen = 30, dup

            select #04, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos =    1, keylen =  20

            select #05, "CSHLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 20,                         ~
                        alternate key 1, keypos = 21, keylen = 16, dup

            select #40, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            select #41,  "PAYLNCUR",                                     ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =   5,  keylen = 28,                      ~
                        alt key  1, keypos =   1, keylen =  32

            select #42, "CSHLNCUR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos =    5, keylen = 20,                      ~
                        alt key  1, keypos =  1, keylen =  24

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#2, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#3, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#4, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#5, 0%, 0%, 0%, " ")

*        Check for Multi-Currency
            curr_flag$ = "N" : stat$ = " "
            call "READ100" (#04, "SWITCHS.CUR", f1%(4))
            if f1%(4) <> 0% then get #04 using L02430, curr_flag$, stat$
L02430:         FMT POS(21), CH(1), CH(4)
            if curr_flag$ <> "Y" then goto L09000
                call "OPENCHCK" (#40, 0%, 0%, 0%, " ")
                call "OPENCHCK" (#41, 0%, 0%, 0%, " ")
                call "OPENCHCK" (#42, 0%, 0%, 0%, " ")

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)

            call "READ100" (#1, userid$, f1%(1))
            if f1%(1) <> 0% then L10000
                 call "ASKUSER" (2%, "Sorry", " ",                       ~
                      "You Are Not A Valid User In This Database",       ~
                                               "Press (RETURN) To Exit.")
                goto L65000

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
        editmode% = 0%
            init(" ") errormsg$, message$, from_ven$, to_ven$,date$()
            mat daten% = zer

            for fieldnr% = 1 to  5
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10230
L10140:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then L10210
L10170:                  fieldnr% = max(1%, fieldnr%-1%)
                         gosub'051(fieldnr%)
                         if enabled% = 0 and fieldnr% > 1 then L10170
                         goto L10140
L10210:               if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then L10140
L10230:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10140
                next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************

            editmode% = 1%
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press RETURN."

L11100:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       startreport
                  if keyhit% <>  0 then       L11100

            if cursor%(1) <= 7% then fieldnr% = cursor%(1) - 5           ~
                                else fieldnr% = cursor%(1) - 7
            if cursor%(1) = 8% or cursor%(1) = 9% then L11100

            if fieldnr% < 1 or fieldnr% >  5 then L11100

L11210:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11210
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11210
            goto L11100

        REM *************************************************************~
            *             S T A R T   R E P O R T                       *~
            *                                                           *~
            *************************************************************

        startreport
            call "TIME" (time$)
            header$ = "(Vendor Range:  From " & from_ven$
            header$ = header$ & " To " & to_ven$
            header$ = header$ & ")"
            if from_ven$ = "ALL" then header$ = "(Vendor Range: ALL)"
            call "STRING" addr ("CT", header$, 47%)
            select printer (134)
            firsthit% = 0%

            if from_ven$ = "ALL" then L15190
                plowkey$ = from_ven$ addc all(hex(ff))
                goto L15220

L15190:         plowkey$ = all(hex(00))
                to_ven$  = all(hex(ff))

L15220:     linecounter% = 999%

        REM **************** THIS IS THE MAIN LOOP **********************

L15260:     call "PLOWNEXT" (#2, plowkey$, 0%, f1%(2))
                if f1%(2) = 0% then eoj

                gosub get_data                     /* AT 30000         */
                if hold$ = "Y" then L15260
                if abs(invbal) < .001 then L15260
                if vencode$ > to_ven$ then eoj

                if old_ven$ <> vencode$ then call "SHOSTAT" ("Printing Re~
        ~port For Vendor: " & vencode$)

                if firsthit%<>0% and old_ven$<>vencode$                  ~
                     then gosub subtotal
                old_ven$= vencode$
                oldname$ = name$

                firsthit% = 1%
                if linecounter% > 56% then gosub pageheader

                gosub totaling

                gosub printline

                goto L15260

            eoj /********** END OF JOB ********************************/
                if firsthit% = 0% then gosub pageheader
                gosub subtotal
                gosub grandtotal
                goto L65000

        REM *************** TOTALING ************************************
        totaling:

            mat printamt = zer

            for i% = 1% to 3%
                if pay_date% <= daten%(i%) then L16100
            next i%
            i% = 4%

L16100:     printamt(i%) = invbal
            if invamt <> invbal then disc_amt = 0  /* Partialy Payed */

            for i% = 1% to 3%
                if disc_date% < daten%(i%) then L16180
            next i%
            i% = 4

L16180:     printamt(i%+4) = disc_amt
            for i% = 1% to 8%
                grand (i%) = round(grand(i%) + printamt(i%),2)
                subtot(i%) = round(subtot(i%)+ printamt(i%),2)
            next i%

            return

        REM *************** PAGEHEADER **********************************
        pageheader:
            print page
            page% = page% + 1%

            print using L19120, date$, time$, cms2v$
            print using L19150, userid$, header$, page%
            print skip(1)
            print using L19180
            print using L19210, date$(1),date$(2),date$(3),"  Future",    ~
                    "    Lost",date$(1),date$(2),date$(3)
            print using L19250
            linecounter% = 7%
            return

        REM *************** PRINTLINE ***********************************
        printline:
            call "CONVERT" (printamt(1), 2.2, printamt$(1))
            call "CONVERT" (printamt(2), 2.2, printamt$(2))
            call "CONVERT" (printamt(3), 2.2, printamt$(3))
            call "CONVERT" (printamt(4), 2.2, printamt$(4))
            call "CONVERT" (printamt(5), 2.2, printamt$(5))
            call "CONVERT" (printamt(6), 2.2, printamt$(6))
            call "CONVERT" (printamt(7), 2.2, printamt$(7))
            call "CONVERT" (printamt(8), 2.2, printamt$(8))
            print using L19300, vencode$,invoice$,pay_date$,disc_date$,   ~
                               printamt$(1),printamt$(2),printamt$(3),   ~
                               printamt$(4),printamt$(5),printamt$(6),   ~
                               printamt$(7),printamt$(8)
            linecounter% = linecounter% + 1%

            if print_curr% = 0% then return
            print using L19460, currency$, curr_desc$, curramt
            linecounter% = linecounter% + 1%
            return

        REM *************** SUBTOTAL ************************************
        subtotal:
            call "CONVERT" (subtot(1), 2.2, subtot$(1))
            call "CONVERT" (subtot(2), 2.2, subtot$(2))
            call "CONVERT" (subtot(3), 2.2, subtot$(3))
            call "CONVERT" (subtot(4), 2.2, subtot$(4))
            call "CONVERT" (subtot(5), 2.2, subtot$(5))
            call "CONVERT" (subtot(6), 2.2, subtot$(6))
            call "CONVERT" (subtot(7), 2.2, subtot$(7))
            call "CONVERT" (subtot(8), 2.2, subtot$(8))

            print using L19344
            print using L19350, oldname$, subtot$(1), subtot$(2),         ~
                             subtot$(3), subtot$(4), subtot$(5),         ~
                             subtot$(6), subtot$(7), subtot$(8)

            print skip(2)

            linecounter% = linecounter% + 4%
            mat subtot = zer
            return

        REM *************** GRANDTOTAL **********************************
        grandtotal:

            print skip(2)
            print using L19400, grand(1), grand(3),grand(5), grand(7)
            print using L19420, grand(2), grand(4),grand(6), grand(8)
            return

        REM ************* IMAGE STATEMENTS ******************************

        REM  IMAGES FOR HEADER
L19120: %RUN: ########  ########                  C A S H   R E Q U I R E~
        ~ M E N T S   R E P O R T                           CSHREQIR:#####~
        ~###
L19150: %USER: ###                                #######################~
        ~########################                               PAGE:####
L19180: %                                               ------------ PAYA~
        ~BLES DUE BY ------------   ------- DISCOUNTS AVAILABLE UNTIL ----~
        ~---
L19210: %VENDOR    INVOICE NUMBER   DUE DATE DISC DATE  ########   ######~
        ~##   ########   ########   ########   ########   ########   #####~
        ~###
L19250: %--------- ---------------- -------- ---------  -----------------~
        ~------------------------   --------------------------------------~
        ~---

        REM IMAGES FOR LINES
L19300: %######### ################ ######## ######## ########## ########~
        ~## ########## ########## ########## ########## ########## #######~
        ~###

        REM IMAGES FOR SUBTOTALS
L19344: %                                              ---------  -------~
        ~--  ---------  ---------  ---------  ---------  ---------  ------~
        ~---
L19350: %Total for ##############################     ########## ########~
        ~## ########## ########## ########## ########## ########## #######~
        ~###

        REM IMAGES FOR GRAND TOTALS
L19400: %GRAND TOTALS:                             -#########.##         ~
        ~-#########.##         -#########.##         -#########.##
L19420: %                                                     -#########.~
        ~##         -#########.##         -#########.##         -#########~
        ~.##

        REM Image for foreign currency
L19460: %  in #### #################### #########.##

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 0
                  message$ = " "
                  on fieldnr% gosub L20150,         /* FROM_VEN         */~
                                    L20200,         /* TO_VEN           */~
                                    L20280,         /* DATE ONE         */~
                                    L20310,         /* DATE TWO         */~
                                    L20340          /* DATE THREE       */
                     return
L20150:     rem DEFAULT/ENABLE FOR ENTER VENDOR RANGE    FROM:
                enabled% = 1%
                if from_ven$ = " " then from_ven$ = "ALL"
                message$ = "Enter 'ALL' To Select All Vendors For Report"
                return
L20200:     rem DEFAULT/ENABLE FOR                         TO:
                if from_ven$ = "ALL" then L20250
                enabled% = 1%
                if to_ven$ = " " then to_ven$ = from_ven$
                return
L20250:         enabled% = 0%
                to_ven$ = " "
                return
L20280:     rem DEFAULT/ENABLE FOR ENTER THREE DATES   DATE 1:
                enabled% = 1%
                return
L20310:     rem DEFAULT/ENABLE FOR                     DATE 2:
                enabled% = 1%
                return
L20340:     rem DEFAULT/ENABLE FOR                     DATE 3:
                enabled% = 1%
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
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode
                                                                         ~
        REM *************************************************************~
            *                     G E T   D A T A                       *~
            *                                                           *~
            * Gets data out of PAYMASTR file.                           *~
            *************************************************************

        get_data:
            print_curr% = 0%
            get #2 using L30700, vencode$, invoice$, pay_date$,           ~
                                disc_date$, invamt, invbal, disc_amt,    ~
                                hold$, currency$
            call "DESCRIBE" (#3, vencode$, name$, 0%, f1%(3))
                if f1%(3) = 0 then name$ = "** VENDOR NOT ON FILE **"

            pay_date%, disc_date% = 0%
            if pay_date$  = " " then pay_date$  = blankdate$
            if disc_date$ = " " then disc_date$ = blankdate$

            if pay_date$ <> blankdate$ then ~
              call "DATEFMT" (pay_date$, pay_date%) else pay_date$ = "00/00/00"

            if disc_date$ <> blankdate$ then ~
              call "DATEFMT" (disc_date$, disc_date%) else disc_date$="00/00/00"

        REM Now get the transaction amount. Add of the currency shadow   ~
             lines for the invoice and then subtract the transaction     ~
             amount from any checks that have been processed against the ~
             invoice.
            if curr_flag$ <> "Y" then return
            if currency$ = " " or currency$ = stat$ then return
            call "DESCRIBE" (#40, currency$, curr_desc$, 0%, f1%(40))
                if f1%(40) = 0% then curr_desc$ = "* Foreign Currency *"
            curr_key$ = vencode$  :  curramt = 0
            str(curr_key$, 10%) = invoice$
L30190:     call "PLOWNEXT" (#41, curr_key$, 25%, f1%(41%))
                if f1%(41%) = 0% then L30230
            get #41 using L30205, lncuramt
L30205:         FMT POS(33), PD(14,4)
            lncuramt = round(lncuramt, 2%)
            curramt = curramt + lncuramt
            print_curr% = 1%
            goto L30190

L30230
*        Now find checks and subtract
            call "REDALT1" (#5, invoice$, 1%, f1%(5%))
L30240:         if f1%(5%) = 0% then return
            get #5 using L30255, tempvencode$, tempcheck$, tempseq$,      ~
                                tempinvoice$
L30255:         FMT CH(9), CH(8), CH(3), CH(16)
            if tempinvoice$ <> invoice$ then return
            if tempvencode$ <> vencode$ then L30370
            checkcurrkey$ = str(tempvencode$) & str(tempcheck$) &        ~
                            str(tempseq$)

            call "READ100" (#42, checkcurrkey$, f1%(42%))
                if f1%(42%) = 0% then L30370
            get #42 using L30320, lncuramt
L30320:         FMT POS(69), PD(14,4)
            lncuramt = round(lncuramt, 2%)
            curramt = curramt - lncuramt

*        Read next check record
L30370:     call "READNXT1" (#5, f1%(5%))
            goto L30240
L30700: FMT                      /* FILE: PAYMASTR                     */~
            CH(9),               /* vendor code                        */~
            CH(16),              /* invoice number                     */~
            XX(16),              /* Receiver Number                    */~
            XX(6),               /* invoice date                       */~
            XX(9),               /* purchases account                  */~
            XX(9),               /* payables account                   */~
            XX(1),               /* payables account type (from glmain */~
            CH(6),               /* pay without discount date          */~
            CH(6),               /* pay, taking discount date          */~
            XX(08),              /* non-discountable amount            */~
            XX(6),               /* vendor module date for this user   */~
            XX(6),               /* originally input on (date)         */~
            XX(3),               /* originally input by (user)         */~
            XX(6),               /* last modified on (date)            */~
            XX(3),               /* last modified by (user)            */~
            PD(14,4),            /* invoice total                      */~
            PD(14,4),            /* outstanding balance this invoice   */~
            XX(20),              /* free test field                    */~
            XX(08),              /* Default Discount Percent           */~
            PD(14,4),            /* discount amount                    */~
            XX(04),              /* 1099 Category                      */~
            CH(01),              /* Invoice In Hold Flag               */~
            XX(01),              /* Invoice Type                       */~
            XX(04),              /* Text ID                            */~
            CH(04)               /* Transaction currency               */

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                init(hex(8c)) lfac$()
                pfdescr$(1) = "(1)Start Over     (4)Previous Field       ~
        ~                     (13)Instructions"
                pfdescr$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfdescr$(3) = "                                          ~
        ~                     (16)EXIT PROGRAM"
                pfkeys$ = hex(0001040d0f10)

                header$ = " "
                if fieldnr% > 1% then L40220
                str(pfdescr$(1),19,19) = " "
                str(pfkeys$,3,2) = hex(ffff)
                goto L40410
L40220:         pfdescr$(3) = " "
                str(pfkeys$,6,1) = hex(ff)
                goto L40410

            deffn'111(fieldnr%)
                REM Editmode logic...
                pfdescr$(1) = "(1)Start Over                             ~
        ~                     (13)Instructions"
                pfdescr$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfdescr$(3) = "                                          ~
        ~                     (16)PRINT REPORT"
                pfkeys$ = hex(00010d0f10)
                init(hex(86)) lfac$()
                if fieldnr% = 0% then L40410
                     pfdescr$(3) = " "
                     str(pfkeys$,5,1) = all(hex(ff))
                     init(hex(8c)) lfac$()

L40410:           str(pfdescr$(3),63,1) = hex(84)
                  str(header$,63) = "CSHREQIR:" & cms2v$
                  on fieldnr% gosub L40530,         /* FROM_VEN         */~
                                    L40530,         /* TO_VEN           */~
                                    L40530,         /* DATE ONE         */~
                                    L40530,         /* DATE TWO         */~
                                    L40530          /* DATE THREE       */
                     goto L40600

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40530:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40600:     accept                                                       ~
               at (01,02),"Cash Requirements Report",                    ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)),   date$                , ch(08),~
               at (02,02), fac(hex(ac)),   header$              , ch(79),~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (06,02), "ENTER VENDOR RANGE    FROM:",                ~
               at (06,30), fac(lfac$( 1)), from_ven$            , ch(09),~
               at (07,02), "                        TO:",                ~
               at (07,30), fac(lfac$( 2)), to_ven$              , ch(09),~
               at (09,02), "   PROPOSED PAYABLES DATES ",                ~
               at (10,02), "                    DATE 1:",                ~
               at (10,30), fac(lfac$( 3)), date$(1)             , ch(08),~
               at (11,02), "                    DATE 2:",                ~
               at (11,30), fac(lfac$( 4)), date$(2)             , ch(08),~
               at (12,02), "                    DATE 3:",                ~
               at (12,30), fac(lfac$( 5)), date$(3)             , ch(08),~
                                                                         ~
            at(17,02),"Dates entered are used to distribute payables over~
        ~ ranges, which will allow",                                      ~
            at(18,02),           "comparison of discountable and non-disc~
        ~ountable cash requirements aged into",                           ~
            at(19,02),          "the future.",                           ~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L40970
                  call "MANUAL" ("CSHREQIR")
                  goto L40600

L40970:        if keyhit% <> 15 then L41010
                  call "PRNTSCRN"
                  goto L40600

L41010:        if fieldnr% <> 0 then return
               close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50150,         /* FROM_VEN         */~
                                    L50190,         /* TO_VEN           */~
                                    L50270,         /* DATE ONE         */~
                                    L50330,         /* DATE TWO         */~
                                    L50420          /* DATE THREE       */
                     return

L50150:     rem TEST DATA FOR ENTER VENDOR RANGE    FROM:
            if from_ven$ = "ALL" then to_ven$ = " "
                return

L50190:     rem TEST DATA FOR                         TO:
            if from_ven$ <> "ALL" then L50230
                to_ven$ = " "
                return
L50230:     if from_ven$ > to_ven$ then errormsg$ = "'TO' Vendor Must Be ~
        ~Greater Than 'FROM' Vendor."
                return

L50270:     rem TEST DATA FOR ENTER THREE DATES   DATE 1:
            dt% = 1%
            if editmode% <> 0% then L50510
            call "DATEOK" (date$(1),daten%(1),errormsg$)
            return

L50330:     rem TEST DATA FOR                     DATE 2:
            dt% = 2%
            if editmode% <> 0% then L50510
            call "DATEOK" (date$(2),daten%(2),errormsg$)
            if errormsg$ <> " " then return
            if daten%(1) >= daten%(2) then errormsg$ = "'DATE 2' Must Be ~
        ~Greater Than 'DATE 1'."
                return

L50420:     rem TEST DATA FOR                     DATE 3:
            dt% = 3%
            if editmode% <> 0% then L50510
            call "DATEOK" (date$(3),daten%(3),errormsg$)
            if errormsg$ <> " " then return
            if daten%(2) >= daten%(3) then errormsg$ = "'DATE 3' Must Be ~
        ~Greater Than 'DATE 2'."
                return

L50510: REM ***********COMMON EDITMODE TESTING OF DATES *****************

            call "DATEOK" (date$(dt%),daten%(dt%),errormsg$)
                if errormsg$ <> " " then return
                if daten%(1) < daten%(2) and daten%(2) < daten%(3)       ~
                     then return
                errormsg$ = "Dates MUST Be In Increasing Order."
                return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
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
