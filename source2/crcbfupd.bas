        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   RRRR    CCC   BBBB   FFFFF  U   U  PPPP   DDDD    *~
            *  C   C  R   R  C   C  B   B  F      U   U  P   P  D   D   *~
            *  C      RRRR   C      BBBB   FFF    U   U  PPPP   D   D   *~
            *  C   C  R   R  C   C  B   B  F      U   U  P      D   D   *~
            *   CCC   R   R   CCC   BBBB   F       UUU   P      DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CRCBFUPD - Distributes Balance Forward Payment Amounts.   *~
            *            Routine IS a form of Cash Receipts input and   *~
            *            IS under session control.                      *~
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
            * 12/04/86 ! Original                                 ! ERN *~
            * 12/03/87 ! Added multi-currency, CRCCURCY.          ! JIM *~
            * 08/02/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            bals(3), sbals(3), cbals(3), /* Balances                   */~
            billto$9,                    /* Bill-to Number             */~
            chknr$8,                     /* Check Number               */~
            convdate$6,                  /* Currency conversion date   */~
            curr$1,                      /* SYSFILE2 Currency code     */~
            currency$4,                  /* Currency code              */~
            dummy$8,                     /* Dummy for ARMBALNC         */~
            msg$60,                      /* Informational Message      */~
            plowkey$50,                  /* Miscellaneous Read/Plow Key*/~
            postdate$8,                  /* Session Post Date          */~
            readkey$50,                  /* Miscellaneous Read/Plow Key*/~
            seq$4,                       /* Line Item Sequence         */~
            session$6,                   /* Session ID                 */~
            srcetype$2,                  /* Test Source and Type       */~
            tbplowkey$50,                /* ARMTRIAL Plow key          */~
            userid$3                     /* Current User Id            */~

        dim                              /* LINE ITEM VARIABLES        */~
            adjdue$(1500)6,              /* Adjusted Due Dates         */~
            cracct$(1500)9,              /* Credit Accounts            */~
            convdate$(1500)6,            /*                            */~
            conveqv(1500),               /*                            */~
            convunt(1500),               /*                            */~
            discpct(1500),               /* Cash Discount Percent      */~
            duedate$(1500)6,             /* Discount Due Dates         */~
            idx$(1500)11,                /* Auto Index / Sort          */~
            net(1500),                   /* Net Payment Amounts        */~
            po$(1500)16,                 /* Purchase Order Numbers     */~
            srcedoc$(1500)8,             /* Source Doc Applied Against */~
            srcetype$(1500)2,            /* Srce Doc Srce and Type     */~
            stlmnt$(1500)12              /* Stlmnt #s applied to       */

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
            * #01 ! UPDSESSN ! Session Control File                     *~
            * #02 ! SYSFILE2 ! Caelus Management System General Informa *~
            * #05 ! CRCMASTR ! Cash Receipts Check Header File          *~
            * #06 ! ARMTRIAL ! Accounts Receivable Trial Balance        *~
            * #09 ! CRCBUFFR ! Cash Receipts Buffer- Checks             *~
            * #10 ! CRCBUF2  ! Cash Receipts Buffer- Distribution       *~
            * #41 ! CURCONVR ! Multi-Currency Conversion Tables         *~
            * #42 ! CRCMSCUR ! Multi-Currency Master Information        *~
            * #43 ! CRCLNCUR ! Multi-Currency Line Information          *~
            * #44 ! ARMTBCEX ! Multi-Currency Trial Balance Exposure    *~
            * #45 ! CRCL2CUR ! Multi-Currency Line Information          *~
            * #60 ! WORKFILE ! Work File                                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "UPDSESSN",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =  4,   keylen = 17,                      ~
                        alt key  1, keypos =     1, keylen =  20

            select #02, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #05, "CRCMASTR",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  17

            select #06, "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  21

            select #09, "CRCBUFFR",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen = 17,                      ~
                        alt key  1, keypos =  201, keylen =  23

            select #10, "CRCBUF2",                                       ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen = 21,                      ~
                        alt key  1, keypos =  201, keylen =  33

            select #41, "CURCONVR",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  11


            select #42, "CRCMSCUR",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 17,                      ~
                        alt key  1, keypos =  1, keylen =  21

            select #43, "CRCLNCUR",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 21,                      ~
                        alt key  1, keypos =  1, keylen =  25

            select #44, "ARMTBCEX",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 21,                      ~
                        alt key  1, keypos =  1, keylen =  25

            select #45, "CRCL2CUR",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 21,                      ~
                        alt key  1, keypos =  1, keylen =  25

            select #60, "WORKFILE",                                      ~
                        varc,     indexed,  recsize = 13,                ~
                        keypos =    1, keylen = 13

        call "SHOSTAT" ("Balance Forward Update: Opening Files")
            call "OPENCHCK" (#01, fs%(1 ), f2%(1 ),   0%, rslt$(1 ))
            call "OPENCHCK" (#02, fs%(2 ), f2%(2 ),   0%, rslt$(2 ))
            call "OPENCHCK" (#05, fs%(5 ), f2%(5 ),   0%, rslt$(5 ))
            call "OPENCHCK" (#06, fs%(6 ), f2%(6 ),   0%, rslt$(6 ))
            call "OPENCHCK" (#09, fs%(9 ), f2%(9 ), 100%, rslt$(9 ))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 200%, rslt$(10))

            call "OPENCHCK" (#41, fs%(41), f2%(41),   0%, rslt$(41))
            call "OPENCHCK" (#44, fs%(44), f2%(44),   0%, rslt$(44))
            if fs%(44) <= 0% then L05120
               f1%(42) = 100%:f1%(43) = 200%:f1%(45) = 200%
L05120:     call "OPENCHCK" (#42, fs%(42), f2%(42), f1%(42), rslt$(42))
            call "OPENCHCK" (#43, fs%(43), f2%(43), f1%(43), rslt$(43))
            call "OPENCHCK" (#45, fs%(45), f2%(45), f1%(45), rslt$(45))

*        Check for Multi-Currency
            curr$ = "N"
            call "READ100" (#02, "SWITCHS.CUR", f1%(2))
            if f1%(2) = 0% then L09000
               get #02 using L05210, curr$, statutory$, currtype$
L05210:            FMT POS(21), CH(1), CH(4), POS(33), CH(1)

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            ml% = 1500%   /* Maximum Lines allowed on a Check.         */

            call "EXTRACT" addr("ID", userid$)

*        Kill any tasking record outstanding for this user.
            plowkey$ = hex(00) & str(userid$)
            call "PLOWAL1" (#09, plowkey$, 1%, 4%, f1%(9))
            if f1%(9) = 0% then L09210
                plowkey$ = str(key(#09,1%),7,9)
                delete #09
                call "ASKUSER" (2%, "OUTSTANDING TASKING RECORD",        ~
                     "A Tasking record for Bill-to Customer " & plowkey$,~
                     "with your User ID was found and removed.",         ~
                     "Press RETURN to continue....")

L09210
*        Get which Session to place checks into.
            u3% = 1%
            call "UPDUSRLG" ("CRCUPDTE", "CRCINPUT",                     ~
                             "Balance Forward Update", "1", session$,    ~
                             u3%, postdate$, " ")
            if u3% <> 0% then exit_program

*        And finish off any other misc tasks
            print page
            print at(02,10), "B A L A N C E   F O R W A R D   U P D A T E"

            call "DATE" addr("GJ", date, chknr$, u3%)
               if u3% <> 0% then exit_program  /* NOT EXPECTED         */
            call "DATEFMT" (chknr$,0%,chknr$)
            chknr$ = str(chknr$,2%)
            str(chknr$,,2) = "BF"              /* 'BF' & YDDD (Julian) */
            call "DATREVRS" ( date, rev_date$, " " )

            init (hex(00)) dummy$

            call "WORKOPEN" (#60, "IO", 500%, f2%(60))

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Emulate physical entry of check header.                   *~
            *************************************************************

        next_billto  /* Get next Bill-to with an open Bal Fwd Payment  */
            init(" ") billto$, srcedoc$(), po$(), srcetype$(), adjdue$(),~
                      duedate$(), cracct$(), idx$()
            chknr% = 0%
            mat discpct = zer  :  mat net     = zer
            tbplowkey$  = str(tbplowkey$,,9) & hex(ff)

        tb_loop
            call "PLOWNEXT" (#06, tbplowkey$, 0%, f1%(6))
               if f1%(6) = 0% then exit_program
            billto$  = str(tbplowkey$,,9)
            gosub L11000
            if srcetype$ <> "CB" then tb_loop

*        Task out this Bill-to.  If already tasked notify and wait.
L10320:     readkey$ = str(billto$) & hex(0000000000000000)
            write #09 using L10360, readkey$, userid$, " ",               ~
                                  hex(00), userid$, readkey$, " ",       ~
                                  eod goto L10380
L10360:         FMT CH(17), CH(3), CH(180), CH(1), CH(5), CH(17), CH(77)
            goto L10460
L10380:         call "READ100" (#09, readkey$, f1%(9))
                if f1%(9) = 0% then L10320
                     msg$ = "Bill-to Held: " & billto$ & " by " &        ~
                            str(key(#09,1%),2,3)
                     call "SHOSTAT" (msg$)
                     call "PAUSE" addr(6000%)
                     goto L10380

L10460
*        Now process Bill-to
            msg$ = "Processing Bill-to " & billto$: call "SHOSTAT" (msg$)

L10500:     gosub data_load
            if paymnts = 0                                               ~
               or bfs% = 0%                                              ~
               or drs% = 0%                                              ~
               or used% = 0% then L10560
                  gosub apply_payments
                  if paid% > 0% then gosub save_check

L10560:     call "PLOWNEXT" (#06, tbplowkey$, 9%, f1%(6))
               if f1%(6) = 0% then L10660
            gosub L11000
            if srcetype$ <> "CB" then L10560

            init(" ") srcedoc$(), po$(), srcetype$(), adjdue$(),         ~
                      duedate$(), cracct$(), idx$()
            mat discpct = zer  :  mat net     = zer
            goto L10500

L10660:     gosub clear_tasking
            goto  next_billto

L11000
*        OK to Process this guy?

            get #06 using L11030, srcetype$
L11030:         FMT POS(87), CH(2)
            if srcetype$ <> "CB" then return
            currency$ = statutory$
            call "READ100" (#44, key(#6), f1%(44))
            if f1%(44) = 0% then L11500    /* STATUTORY */

            currency$ = str(key(#44,1%),,4)
            readkey$ = str(currency$,,4) & str(billto$)
            call "READ100" (#60, readkey$, f1%(60))
               if f1%(60) <> 0% then L11400    /* PROCESSED, THIS RUN */
            write #60 using L11095, str(readkey$,,13)
L11095:           FMT CH(13)
            readkey$ = str(currency$,,4) & str(billto$) & str(chknr$,,6)
            call "PLOWALTS" (#42, readkey$, 1%, 19%, f1%(42))
               if f1%(42) <> 0% then L11400    /* PROCESSED, ANOTHER RUN */
L11140:     chknr% = chknr% + 1%
            if chknr% > 99% then L11400
            readkey$ = str(billto$) & str(chknr$,,6)
            convert chknr% to str(readkey$,16,2), pic(00)
            call "READ100" (#05, readkey$, f1%(5))
                if f1%(5) = 1% then L11140
            call "READ100" (#09, readkey$, f1%(9))
                if f1%(9) = 1% then L11140
            convert chknr% to str(chknr$,7,2), pic(00)
            return

L11400:     srcetype$ = " "
            return

L11500:     readkey$ = hex(20202020) & str(billto$)
            call "READ100" (#60, readkey$, f1%(60))
               if f1%(60) <> 0% then L11400
            write #60 using L11504, str(readkey$,,13)
L11504:           FMT CH(13)
            readkey$ = str(billto$) & str(chknr$,,6) & "00"
            call "READ100" (#05, readkey$, f1%(5))
                if f1%(5) = 1% then L11400
            call "READ100" (#09, readkey$, f1%(9))
                if f1%(9) = 1% then L11400
            str(chknr$,7,2) = "00"
            return

*       ***  SUB-ROUTINES  ********************************************
        data_load         /* Load candidates for payment               */
            used%, bfs%, drs%, crs% = 0%
            plowkey$ = str(billto$) & hex(00)

          data_load_loop
L12050:     call "PLOWNEXT" (#06, plowkey$, 9%, f1%(6))
            if f1%(6) = 0% then return
            if str(plowkey$,20,2) <> "00" then next_stlmnt
            temp$ = statutory$
            call "READ100" (#44, key(#6), f1%(44))
               if f1%(44) <> 0% then temp$ = str(key(#44,1%),,4)
            if temp$ <> currency$ then L12050

            c% = used% + 1% : if c% < ml% - 1% then L12180
                u3% = 2%
                call "ASKUSER" (u3%, "OUT OF SPACE",                     ~
                       "Unable to load all open items for processing.",  ~
                       "Press RETURN to abort Bill-to's processing -OR-",~
                       "PF-16 to apply to items already loaded.")
                if u3% <> 16% then used% = 0%
                return

L12180:     get #06 using L12220, discpct(c%), duedate$(c%), grace%,      ~
                                po$(c%), cracct$(c%), srcetype$(c%),     ~
                                srcedoc$(c%)

L12220:         FMT POS(22), PD(14,4), CH(6), BI(1), XX(6), CH(16),      ~
                    POS(76), CH(9), XX(2), CH(2), CH(8), POS(177)

            call "DATE" addr("G+",duedate$(c%),-grace%,adjdue$(c%), u3%)
            stlmnt$(c%) = str(plowkey$,10,10)

            call "ARMCBLNC" (billto$, stlmnt$(c%), date, 10%, dummy$,    ~
                             #06, #10, sbals(), #44, #43, #45, temp$,    ~
                             convdate$(c%), conveqv(c%), convunt(c%),    ~
                             cbals())

            if temp$ <> " " then L12370
               mat bals = sbals
               goto L12500
L12370:     mat bals = cbals

L12500:     if bals(3) = 0 then next_stlmnt
                net(c%) = bals(3)
                used%   = used% + 1%
                if net(c%) < 0 then L12550
                   drs% = drs% + 1%:goto L12600
L12550:         idx$(used%) = hex(01):crs% = crs% + 1%
                if srcetype$(c%) <> "CB" then L12600
                     idx$(used%) = hex(00)
                     bfs%        = bfs% + 1%
                     paymnts     = paymnts - net(c%)
L12600:         str(idx$(used%),2,6) = adjdue$(c%)
                convert c% to str(idx$(used%),8,4), pic(0000)
            next_stlmnt
                str(plowkey$,20,2) = hex(ffff)
                goto data_load_loop

        apply_payments
            left  = 0
            paid% = 0%
            if used% <= 0% then return
            call "SORT" addr(str(idx$()), used%, 11%)
            for a% = 1% to used%
                convert str(idx$(a%),8,4) to c%
                if net(c%) < 0 then L14100
                     net(c%) = min(net(c%), left)
                     if net(c%) > 0 then paid% = paid% + 1%
L14100:         left = left - net(c%)
            next a%

            if left = 0 then return
            for a% = bfs% to 1% step -1%
                if left = 0 then return
                convert str(idx$(a%),8,4) to c%
                temp = net(c%)
                net(c%) = min(left + temp, 0)
                left    = max(left + temp, 0)
            next a%

            if left = 0 then return
            if crs% <= bfs% then L14350
            for a% = crs% to bfs% + 1% step -1%
                if left = 0 then return
                convert str(idx$(a%),8,4) to c%
                temp = net(c%)
                net(c%) = min(left + temp, 0)
                left    = max(left + temp, 0)
            next a%

L14350:     if left = 0 then return
                convert str(idx$(bfs%),8,4) to c%
                net(c%) = left + net(c%) /* THIS SHOULDN'T HAPPEN */
                return

        clear_tasking
            readkey$ = str(billto$) & hex(0000000000000000)
            call "DELETE" (#09, readkey$, 17%)
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        save_check

            convdate$ = " " : conveqv, convunt = 1
            if currency$ = statutory$ then L31190
            readkey$ = str(currtype$,,1) & str(currency$,,4) & rev_date$
            call "PLOWNEXT" (#41, readkey$, 5%, f1%(41))
               if f1%(41) = 0% then L31190
            get #41 using L31130, convdate$, conveqv, convunt
L31130:         FMT POS(12), CH(6), 2*PD(14,7)

L31190:     s% = 0%
            for c% = 1% to used%
              if net(c%) = 0 then L31580
                s% = s% + 1% : convert s% to seq$, pic(0000)

                if currency$ = statutory$ then L31500

                   write #45 using L31300, currency$, billto$, chknr$,    ~
                             seq$, convdate$(c%), conveqv(c%),           ~
                             convunt(c%), convdate$, conveqv, convunt,   ~
                             net(c%), 0, 0, " "
L31300:                  FMT CH(4), CH(9), CH(8), CH(4), CH(6),          ~
                             2*PD(14,7), CH(6), 2*PD(14,7), 3*PD(14,4),  ~
                             CH(7)

                   write #43 using L31370, currency$, billto$, chknr$,    ~
                             seq$, convdate$, conveqv, convunt,          ~
                             net(c%), 0, 0, " "
L31370:                  FMT CH(4), CH(9), CH(8), CH(4), CH(6),          ~
                             2*PD(14,7), 3*PD(14,4), CH(29)

                   net  (c%) = net  (c%) * conveqv

L31500:         write #10 using L31550, billto$, chknr$, seq$, "D", " ",  ~
                    stlmnt$(c%), srcedoc$(c%), srcetype$(c%), po$(c%),   ~
                    discpct(c%), adjdue$(c%), duedate$(c%), net(c%), 0,  ~
                    0, cracct$(c%), " ", billto$, stlmnt$(c%), chknr$,   ~
                    seq$
L31550:              FMT CH(9), CH(8), CH(4), CH(1), 2*CH(12), CH(8),    ~
                         CH(2), CH(16), PD(14,4), 2*CH(6), 3*PD(14,4),   ~
                         CH(9), CH(75), CH(9), CH(12), CH(8),CH(4),CH(67)
L31580:     next c%


*        And now write Header to buffer
            if currency$ = statutory$ then L31800
               write #42 using L31650, currency$, billto$, chknr$,        ~
                         convdate$, conveqv, convunt, 0, 0, 0, 0, " "
L31650:              FMT CH(4), CH(9), CH(8), CH(6), 2*PD(14,7),         ~
                         4*PD(14,4), CH(25)


L31800:     write #09 using L31830, billto$, chknr$, date, " ", date,     ~
                0, 0, 0, 0, " ", userid$, date, " ", session$, billto$,  ~
                chknr$, currency$, " "
L31830:         FMT CH(9), CH(8), CH(6), CH(10), CH(6), 4*PD(14,4),      ~
                    CH(27), CH(3), CH(6), CH(93), CH(6), CH(9), CH(8),   ~
                    CH(4), CH(73)
            return


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
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")
            u3% = 2%
            call "UPDUSRLG" ("CRCUPDTE", " ", " ", " ", session$, u3%,   ~
                                                                " ", " ")
            call "FILEBGON" (#60)
            end
