        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB    QQQ   CCCCC  RRRRR  EEEEE  DDDD    TTTTTT *~
            *    J    B   B  Q   Q  C      R   R  E      D    D     T   *~
            *    J    BBBB   Q   Q  C      RRRRR  EEEEE  D    D     T   *~
            *  J J    B   B  Q Q Q  C      R   R  E      D    D     T   *~
            *   J     BBBB    QQQ   CCCCC  R   R  EEEEE  DDDDD      T   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBQCREDT - Displays the Job CREDIT    Ledger for the job  *~
            *            passed by the caller. NEW FUNCTION CLONED FROM *~
            *            JBQMATL                                        *~
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
            * 03/18/88 ! Original - Cloned From JBQMATL           ! MDE *~
            * 03/31/88 ! Fixed Display Problems                   ! MDE *~
            * 05/15/93 ! Core Project                             ! KAB *~
            *          !    Support for core ledgers              ! KAB *~
            *          !    Major Clean-up & Structure            ! KAB *~
            *          !    PRR 12432                             ! KAB *~
            * 09/30/93 ! Problem with Total Units Credited        ! KAB *~
            * 06/02/94 ! PRR 13170- Drill down to show details if ! RJH *~
            *          !  Qty moved from Inventory back to Job.   !     *~
            * 05/31/95 ! Added support for Completion Method 'B'  ! JDH *~
            *          !  which is referenced as 'M' on disk.     !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "JBQCREDT"                   /* Job CREDIT    Ledger S/R   */~
                (jobnbr$,                /* Job Number passed fr caller*/~
                #01,                     /* SYSFILE2 UFB               */~
                #02,                     /* HNYMASTR UFB               */~
                #03,                     /* JBMATER2 UFB               */~
                #07,                     /* JBCREDT  UFB               */~
                #05,                     /* JBMASTR2 UFB               */~
                #06,                     /* JBVALUE2 UFB               */~
                #34,                     /* PIPOUT UFB                 */~
                #60,                     /* HNYQUAN UFB                */~
                #61,                     /* HNYPOOL UFB                */~
                #08)                     /* JBMASTRC UFB               */

        dim                                                              ~
            column1$79, c$1,             /* Screen column headers, FAC */~
            cursor%(2),                  /* Cursor location for edit   */~
            coretxt$12,                  /* Core Prompt                */~
            coretcst$10,                 /* Core Total Debits          */~
            coreccrdts$10,               /* Core Total Credits         */~
            cost(12),                                                    ~
            tcost(12),                                                   ~
            d$1, u$1,                    /* Data FAC                   */~
            d$(10)1,                     /* Data FAC Array for Qty     */~
            g$1,                         /* Edit Fac                   */~
            date$8,                      /* Date for screen display    */~
            dispcost(12),                /* Display Costs              */~
            edtmessage$79,               /* Edit screen message        */~
            costeach$(10)96,             /* COST EACH                  */~
            each_cost(12),               /* Each Cost                  */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            i(1),                        /* For Plow                   */~
            jobto$8,                     /* Job Transferred To         */~
            jobnbr$8, jobdesc$32,        /* Job Number passed fr caller*/~
            jobnme$8,                    /* Job Number                 */~
            quantity(10),                /* Quantities                 */~
            qty_out(10), qty_out$10,     /* Quantities completed to INV*/~
            qty_rev(10), qty_rev$10,     /* Quantities reversed frm INV*/~
            qty_orig$10,                 /* Original Qty Ordered       */~
            qty_adj$10,                  /* Adjusted Quantity          */~
            qtymake$10,                  /* Original Quantity To Make  */~
            qtycomp$10,                  /* Quantity Completed To Date */~
            qtyleft$10,                  /* Quantity Remaining         */~
            keytab$18,                   /* PF keys enabled            */~
            line2$79,                    /* Second Line of Screen Headr*/~
            lot$(10)8,                   /* Lot numbers                */~
            nomore$25,                   /* ** No more Parts on file   */~
            page$3,                      /* Page # for screens         */~
            partobld$25,                 /* Part To Build              */~
            descript$30,                 /* description                */~
            partcost(12),partcost$(10)96,/* Part cost breakdown        */~
            part$(10)25, partdesc$(10)32,/* Part Numbers & descriptions*/~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf1$18,                      /* PF 1 Screen Literal        */~
            pf2$13,                      /* PF 2 Screen Literal        */~
            pf5$13,                      /* PF 5 Screen Literal        */~
            pf6$20,                      /* PF 6 Screen Literal        */~
            pf7$20,                      /* PF 7 Screen Literal        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$70,                  /* READKEY                    */~
            readkey1$70,                 /* Key For Plow               */~
            header$(3)80,                /* Header Information         */~
            descr_map(10),               /* For Plow                   */~
            recdate$(10)8,               /* Date of JBMATER2 record    */~
            quantity$(10)10,             /* Quantity moved to job      */~
            rmain$10,                    /* Number Remaining To Build  */~
            findte$8,                                                    ~
            summ$79,                     /* Summary Data               */~
            store$(10)5,                 /* Store numbers              */~
            temp$16,                     /* Temp                       */~
            totlcost(10),                /* CREDIT ARRAY               */~
            total_crdt$10,                                               ~
            total_qty$10,                                                ~
            tottcst$10,                  /* Total Cost                 */~
            totccrdts$10,                /* Total Credits JBMASTR2     */~
            user$(10)3,                                                  ~
            cstflag$1, cstflag1$1,       /* Cost Flag(s)               */~
            costtype$(10)10,             /* B= Backflush, P=Prorated   */~
            totlcost$(10)10,             /* Total cost per part        */~
            unitcost(12),                /* Unit Costs                 */~
            unitcost$(10)10,             /* Unit Costs                 */~
            valtype$1                    /* JBQVALUE type              */~

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
            cms2v$ = "R6.04.01 06/23/95 Patch Finalization of R6.04.01  "
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
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! HNYMASTR ! Inventory Master File                    *~
            * #03 ! JBMATER2 ! Production job material used detail file *~
            * #05 ! JBMASTR2 ! Production job master file               *~
            * #06 ! JBVALUE2 ! Production job value added detail        *~
            * #07 ! JBCREDIT ! Production job credited to inventory     *~
            * #08 ! JBMASTRC ! Production job master core appendix      *~
            * #34 ! PIPOUT   ! PIPOUT FILE                              *~
            * #60 ! HNYQUAN  ! INventory Quantity File                  *~
            * #61 ! HNYPOOL  ! Inventory Pool File                      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            call "OPENCHCK" (#01, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (#02, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (#03, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (#07, fs%( 7), f2%( 7), 0%, rslt$( 7))
            call "OPENCHCK" (#05, fs%( 5), f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (#06, fs%( 6), f2%( 6), 0%, rslt$( 6))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************


            date$ = date : call "DATEFMT" (date$)

            c$ = hex(ac) : d$ = hex(84) : g$ = hex(86) : u$ = hex(a4)
            s% = dim(part$(), 1) /* Establish number of lines per screen*/

            column1$ = "Date       Quantity   Tot Credit  Unit Credit"  &~
                       "  Str   Lot        By  Cost Method"

            summ$    = " Orig Qty  Qty Build  Completed   Adjusted"     &~
                       "     Remain   Tot Cost  Tot Credit"

             u3% = 0%


            qtyleft, qtymake, qtycomp, tottcst, totcst = 0

            jobnme$ = jobnbr$
            readkey$ = all(hex(00))
            readkey$ = str(jobnme$) & hex(00)
            nomore$ = "* End Of Credit Ledger"
            jobdesc$ = "Job not found in master file"
            call "READ100" (#5, jobnbr$,f1%(5))
               if f1%(5) = 0% then end
            get #5 using L09340, jobdesc$, partobld$, qtymake, qtycomp,    ~
                               findte$, qty_orig, qty_scr, qty_rw,       ~
                               tottcst, totcst

L09340:         FMT POS(9), CH(30), POS(58), CH(25), POS(83), PD(14,4),  ~
                    PD(14,4), POS(153), CH(6), POS(180), PD(14,4),       ~
                    POS(212), PD(14,4), PD(14,4), POS(232), PD(14,4),    ~
                    POS(528), PD(14,4)

            qty_adj = -(qty_orig - qty_scr - qty_rw - qtymake)
            qtyleft = qtymake - qtycomp
            call "CONVERT" (qty_orig, 2.2, qty_orig$)
            call "CONVERT" (qty_adj , 2.2, qty_adj$)
            call "CONVERT" (qtyleft , 2.2, qtyleft$)
            call "CONVERT" (qtycomp , 2.2, qtycomp$)
            call "CONVERT" (qtymake , 2.2, qtymake$)
            call "CONVERT" (tottcst , 2.2, tottcst$)
            call "CONVERT" (totcst  , 2.2, totccrdts$)
            call "CONVERT" (qtyleft , 2.2, rmain$)

            call "DESCRIBE" (#02, partobld$, descript$, 0%, f1%(2))

             /* Establish The Status Of this Job   */
             if findte$ <> " " then edtmessage$ = "Notice: This Job Is"& ~
                  " Closed And All Production Has Been Reported Complete"

             if qtyleft = 0 and findte$ = " " then edtmessage$ = "Noti" &~
                   "ce: This Job Is Open And All Production Has Been Re"&~
            "ported Complete"

             if qtyleft > 0  then edtmessage$ = "Notice: This Job Is Op"&~
            "en with " & rmain$ & " Remaining To Complete"

            jobdesc$ = "(" & jobdesc$ & ")"

            line2$ = "Job: " & jobnbr$ & " " & jobdesc$
            str(line2$,62%) = "JBQCREDT: " & str(cms2v$,,8%)
            pf1$ = "(1)Closing Adj.   "

            coretxt$, coretcst$, coreccrdts$, pf6$, pf7$ = " "
            call "READ100" (#8, jobnbr$, core%)
               if core% = 0% then L09790
                  pf1$ = "(1/17)Closing Adj."
                  pf6$ = "(6)Core Credits"
                  pf7$ = "(7)Core Comp. Memos"
                  coretxt$ = "Core Ledger:"
                  get #8 using L09750, coredbt, corecdt
L09750:               FMT POS(9), PD(14,4), POS(209), PD(14,4)
                  call "CONVERT" (coredbt, 2.2, coretcst$)
                  call "CONVERT" (corecdt, 2.2, coreccrdts$)

L09790:     gosub totals_routine
            gosub first_page

        REM *************************************************************~
            * SCREEN CONTROL                                            *~
            *************************************************************

        screen_control
            errormsg$ = " "
            gosub L40000
            if keyhit% =  1% then gosub jbqvalue_call_3
            if keyhit% = 17% then gosub jbqvalue_call_4
            if keyhit% =  2% then gosub first_page
            if keyhit% =  5% then gosub next_page
            if keyhit% =  6% then gosub jbqvalue_call_1
            if keyhit% =  7% then gosub jbqvalue_call_2
            if keyhit% =  8% then gosub inv_stat
            if keyhit% =  9% then gosub show_jobs
            if keyhit% = 10% then gosub show_distribution
            if keyhit% = 14% then gosub print_cost_details
            if keyhit% = 16% then end
            if keyhit% =  0% then gosub check_for_reversed_qty
            goto screen_control

        check_for_reversed_qty
            ln% = cursor%(1%) - 8%
            if qty_rev(ln%) = 0 then return /* Not on a line w/ Rev Qty */
            if qty_rev(ln%) < 0 then qty_rev(ln%)= - qty_rev(ln%)
            call "CONVERT" (qty_out(ln%), 2.2, qty_out$)
            call "CONVERT" (qty_rev(ln%), 2.2, qty_rev$)

            ask% = 2%
            call "ASKUSER"( ask% , "JOB TO INVENTORY MOVEMENT",          ~
                           qty_out$ & " Part(s) Moved to Inventory",     ~
                           qty_rev$ & " Part(s) Returned to Job   ",     ~
                           "Press Any Key to Return" )

            return

        REM *************************************************************~
            *           C O M M O N   S U B R O U T I N E S             *~
            *************************************************************

        show_distribution
            x% = cursor%(1) - 8%
            if x% < 1% or x% > s% then goto L15080
            if part$(x%) <> " " and part$(x%) <> nomore$ then L15110
L15080:         errormsg$ = "Place cursor on desired Line Then , Press "&~
                      "PF(10)"
                return
L15110:     gosub  disp_cost_breakdown
            errormsg$ = " "
            return

        inv_stat
            call "HNYQDISP" (partobld$, #02,#60, #61, #1)
            errormsg$ = " "
            return

        print_cost_details
            call "JBQCDTLS" (jobnbr$, #01, #05, #03, #06, #02, #08)
            return

        disp_cost_breakdown
            get partcost$(x%) using L15260 , dispcost()
L15260:     FMT 12*PD(14,4)

            if quantity(x%) <> 0 then L15320
               mat each_cost = zer
               goto L15360

L15320:     for i% = 1% to 12%
            each_cost(i%) = dispcost(i%)  / quantity(x%)
            next i%

L15360:     put costeach$(x%) using L15370, each_cost()
L15370:     FMT 12*PD(14,4)

            call "HNYCDIST" ("D", partobld$, descript$, " ", #1,         ~
                              costeach$(x%), temp$, 0)
            return

        show_jobs
            readkey1$ = partobld$
            i(1) = 1.11 : i$() = "JOB ORDER:"
            descr_map(01) =  12.08  : descr_map(02) = 01      /* Job# */
            descr_map(03) = -09.30  : descr_map(04) = 10      /* Desc */
            descr_map(05) =-168.061 : descr_map(06) = 41      /* Start*/
            descr_map(07) =-174.061 : descr_map(08) = 50      /* End  */
            descr_map(09) =  57.08  : descr_map(10) = 67.104  /*Qty   */
            header$(1%) = "  Job Number/Description          " &         ~
                          "        Planned Start/End    Quantity Needed"
            header$(3%) = hex(a4) & "Below Are Current Jobs That" &      ~
                          " Are Planned To Use Part " & partobld$

            call "PLOWCODE" (#34, readkey1$, " ", 9025%, 1.30,           ~
                             f1%(34), header$(), 0, -12, i(), i$(), "D", ~
                             "Y", #5, descr_map())

            return

        jbqvalue_call_1
            valtype$ = "Y"
            goto jbqvalue_call
        jbqvalue_call_2
            valtype$ = "F"
            goto jbqvalue_call
        jbqvalue_call_3
            valtype$ = "C"
            goto jbqvalue_call
        jbqvalue_call_4
            valtype$ = "Z"
            goto jbqvalue_call

        jbqvalue_call
            call "JBQVALUE" (jobnbr$, valtype$, #01, #05, #06, #03, #02)
            return

        REM *************************************************************~
            * DATA LOAD LOOP                                            *~
            *************************************************************

        first_page

        REM Start reading JBCREDIT at the beginning of the job passed in
            p%, endofjob% = 0%
            plowkey$ = all(hex(00))
            str(plowkey$,,8) = str(jobnme$) & hex(00)

        next_page

        REM Initialize all arrays/fields used for screen-loads
            p% = p% + 1%              /* Bump screen page number up one */
            init (" ") recdate$(), part$(),  quantity$(),                ~
                totlcost$(),jobto$,unitcost$(),store$(), lot$(),         ~
                user$(), errormsg$ , costtype$()
            init (hex(00)) partcost$()
            mat cost = zer
            mat tcost = zer
            init (hex(84)) d$()

        REM Read the next screen-load of records for the job passed in
            for l% = 1% to s%
                call "PLOWNEXT" (#07, plowkey$, 8%, f1%(7))
                if f1%(7) <> 0% then goto L30270
                     endofjob% = 1%
                     goto L30780
L30270:         get #07 using L30320, recdate$(l%), part$(l%), store$(l%),~
                   lot$(l%), quantity(l%), totlcost(l%), cost(), tcost(),~
                   qty_rev(l%), jobto$, partdesc$(l%), user$(l%),        ~
                   cstflag$ , cstflag1$

L30320:              FMT  /* File #07- JBCREDIT                        */~
                          POS(9), CH(6), POS(23), CH(25), CH(3), CH(6),  ~
                          POS(71), PD(14,4), PD(14,4), 12*PD(14,4),      ~
                          12*PD(14,4), POS(375), PD(14,4), POS(383),     ~
                          CH(8), CH(40), CH(3), CH(1), XX(6), CH(1)

        REM Now we've got a record, so edit it for display

                qty_out(l%)  = quantity(l%)
                quantity(l%) = quantity(l%) - qty_rev(l%)
                unitcost(l%) = 0
                if quantity(l%) = 0 then L30450
                   unitcost(l%) = totlcost(l%) / quantity(l%)

L30450:         if str(store$(l%),1,1) <> "*" then L30520
                str(tmp$,,2) = str(store$(l%),2)
                str(tmp$,3,6) = str(lot$(l%),,6)
                store$(l%) = "Job:"

                lot$(l%) = tmp$

L30520:         mat partcost = cost + tcost
                put partcost$(l%) using L30540, partcost()
L30540:              FMT 12*PD(14,4)
                call "DATEFMT" (recdate$(l%))
                call "CONVERT" (quantity(l%), 2.2, quantity$(l%))
                call "CONVERT" (totlcost(l%), 2.2, totlcost$(l%))
                call "CONVERT" (unitcost(l%), 2.4, unitcost$(l%))

            if cstflag1$ = " " then cstflag1$ = cstflag$

            if cstflag$ = "A" then costtype$(l%) = "All"
            if cstflag$ = "B" then costtype$(l%) = "All"
            if cstflag$ = "M" then costtype$(l%) = "BOM"
            if cstflag$ = "P" then costtype$(l%) = "Pro"
            if cstflag$ = "S" then costtype$(l%) = "Std"
            if cstflag$ = "Z" then costtype$(l%) = "Zero"

            str(costtype$(l%), 5%, 1%) = "/"

            if cstflag1$ = "A" then str(costtype$(l%),7%) = "All"
            if cstflag1$ = "B" then str(costtype$(l%),7%) = "All"
            if cstflag1$ = "P" then str(costtype$(l%),7%) = "Pro"
            if cstflag1$ = "S" then str(costtype$(l%),7%) = "Std"
            if cstflag1$ = "Z" then str(costtype$(l%),7%) = "Zero"

            if qty_rev(l%) = 0 then d$(l%) = d$ else  d$(l%) = hex(8c)

            next l%

L30780: REM Set up the PF keys according to the page we are displaying
            convert p% to page$, pic (###)
            pf2$, pf5$ = " " : pf16$ = "(16)Return"
            keytab$ = hex(01ffffffffffff08090affff0d0e0f10ff00)

            if p% = 1% then goto L30850
                pf2$ = "(2)First Page" : str(keytab$, 2, 1) = hex(02)
L30850:     if endofjob% = 1% then L30880
                pf5$ = "(5)Next Page" : str(keytab$, 5, 1) = hex(05)
            if core% = 0% then return
L30880:        str(keytab$, 6, 2) = hex(0607)
               str(keytab$,17, 1) = hex(11)
               return

        REM *************************************************************~
            * TOTALS ROUTINE                                            *~
            *************************************************************

        totals_routine

            totqty, totcrdt = 0
            total_qty$, total_crdt$ = all(hex(00))
            plowkey$ = all(hex(00))
            str(plowkey$,,8) = str(jobnme$) & hex(00)
L31100:     call "PLOWNEXT" (#7, plowkey$, 8%, f1%(7)) /* Totals */
            if f1%(7) = 0% then L31180
                get #7 using L31130, tqty, crdt, rqty
L31130:              FMT POS(71), PD(14,4), PD(14,4), POS(375), PD(14,4)
                totqty = totqty + (tqty - rqty)
                totcrdt = totcrdt + crdt
                goto L31100

L31180:     call "CONVERT" (totqty, 2.2, total_qty$)
            call "CONVERT" (totcrdt, 2.2, total_crdt$)
            return

L40000: REM *************************************************************~
            *     'N O R M A L'   S C R E E N   P A G E   (PARTS)       *~
            *************************************************************

L40040:     accept                                                       ~
               at (01,02), "Job Credit Ledger: page",                    ~
               at (01,29), fac(hex(8c)), page$                  , ch(03),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
                                                                         ~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Part:",                                      ~
               at (04,08), fac(hex(84)), partobld$              , ch(25),~
               at (04,34), "Desc:",                                      ~
               at (04,40), fac(hex(84)), descript$              , ch(30),~
                                                                         ~
               at (05,02), fac(hex(8c)), summ$                  , ch(79),~
                                                                         ~
               at (06,02), fac(hex(84)), qty_orig$              , ch(10),~
               at (06,12), fac(hex(84)), qtymake$               , ch(10),~
               at (06,23), fac(hex(84)), qtycomp$               , ch(10),~
               at (06,34), fac(hex(84)), qty_adj$               , ch(10),~
               at (06,45), fac(hex(84)), qtyleft$               , ch(10),~
               at (06,56), fac(hex(84)), tottcst$               , ch(10),~
               at (06,68), fac(hex(84)), totccrdts$             , ch(10),~
                                                                         ~
               at (07,43), fac(hex(8c)), coretxt$               , ch(12),~
               at (07,56), fac(hex(84)), coretcst$              , ch(10),~
               at (07,68), fac(hex(84)), coreccrdts$            , ch(10),~
                                                                         ~
               at (08,02), fac(hex(ac)), column1$               , ch(79),~
                                                                         ~
               at (09,02), fac(g$),      recdate$ ( 1%)         , ch(08),~
               at (09,11), fac(d$( 1%)), quantity$( 1%)         , ch(10),~
               at (09,24), fac(d$),      totlcost$( 1%)         , ch(10),~
               at (09,37), fac(d$),      unitcost$( 1%)         , ch(10),~
               at (09,49), fac(d$),      store$   ( 1%)         , ch(05),~
               at (09,54), fac(d$),      lot$     ( 1%)         , ch(08),~
               at (09,66), fac(d$),      user$    ( 1%)         , ch(03),~
               at (09,70), fac(d$),      costtype$( 1%)         , ch(10),~
                                                                         ~
               at (10,02), fac(g$),      recdate$ ( 2%)         , ch(08),~
               at (10,11), fac(d$( 2%)), quantity$( 2%)         , ch(10),~
               at (10,24), fac(d$),      totlcost$( 2%)         , ch(10),~
               at (10,37), fac(d$),      unitcost$( 2%)         , ch(10),~
               at (10,49), fac(d$),      store$   ( 2%)         , ch(05),~
               at (10,54), fac(d$),      lot$     ( 2%)         , ch(08),~
               at (10,66), fac(d$),      user$    ( 2%)         , ch(03),~
               at (10,70),fac(d$),       costtype$( 2%)         , ch(10),~
                                                                         ~
               at (11,02), fac(g$),      recdate$ ( 3%)         , ch(08),~
               at (11,11), fac(d$( 3%)), quantity$( 3%)         , ch(10),~
               at (11,24), fac(d$),      totlcost$( 3%)         , ch(10),~
               at (11,37), fac(d$),      unitcost$( 3%)         , ch(10),~
               at (11,49), fac(d$),      store$   ( 3%)         , ch(05),~
               at (11,54), fac(d$),      lot$     ( 3%)         , ch(08),~
               at (11,66), fac(d$),      user$    ( 3%)         , ch(03),~
               at (11,70), fac(d$),      costtype$( 3%)         , ch(10),~
                                                                         ~
               at (12,02), fac(g$),      recdate$ ( 4%)         , ch(08),~
               at (12,11), fac(d$( 4%)), quantity$( 4%)         , ch(10),~
               at (12,24), fac(d$),      totlcost$( 4%)         , ch(10),~
               at (12,37), fac(d$),      unitcost$( 4%)         , ch(10),~
               at (12,49), fac(d$),      store$   ( 4%)         , ch(05),~
               at (12,54), fac(d$),      lot$     ( 4%)         , ch(08),~
               at (12,66), fac(d$),      user$    ( 4%)         , ch(03),~
               at (12,70), fac(d$),      costtype$( 4%)         , ch(10),~
                                                                         ~
               at (13,02), fac(g$),      recdate$ ( 5%)         , ch(08),~
               at (13,11), fac(d$( 5%)), quantity$( 5%)         , ch(10),~
               at (13,24), fac(d$),      totlcost$( 5%)         , ch(10),~
               at (13,37), fac(d$),      unitcost$( 5%)         , ch(10),~
               at (13,49), fac(d$),      store$   ( 5%)         , ch(05),~
               at (13,54), fac(d$),      lot$     ( 5%)         , ch(08),~
               at (13,66), fac(d$),      user$    ( 5%)         , ch(03),~
               at (13,70), fac(d$),      costtype$( 5%)         , ch(10),~
                                                                         ~
               at (14,02), fac(g$),      recdate$ ( 6%)         , ch(08),~
               at (14,11), fac(d$( 6%)), quantity$( 6%)         , ch(10),~
               at (14,24), fac(d$),      totlcost$( 6%)         , ch(10),~
               at (14,37), fac(d$),      unitcost$( 6%)         , ch(10),~
               at (14,49), fac(d$),      store$   ( 6%)         , ch(05),~
               at (14,54), fac(d$),      lot$     ( 6%)         , ch(08),~
               at (14,66), fac(d$),      user$    ( 6%)         , ch(03),~
               at (14,70), fac(d$),      costtype$( 6%)         , ch(10),~
                                                                         ~
               at (15,02), fac(g$),      recdate$ ( 7%)         , ch(08),~
               at (15,11), fac(d$( 7%)), quantity$( 7%)         , ch(10),~
               at (15,24), fac(d$),      totlcost$( 7%)         , ch(10),~
               at (15,37), fac(d$),      unitcost$( 7%)         , ch(10),~
               at (15,49), fac(d$),      store$   ( 7%)         , ch(05),~
               at (15,54), fac(d$),      lot$     ( 7%)         , ch(08),~
               at (15,66), fac(d$),      user$    ( 7%)         , ch(03),~
               at (15,70), fac(d$),      costtype$( 7%)         , ch(10),~
                                                                         ~
               at (16,02), fac(g$),      recdate$ ( 8%)         , ch(08),~
               at (16,11), fac(d$( 8%)), quantity$( 8%)         , ch(10),~
               at (16,24), fac(d$),      totlcost$( 8%)         , ch(10),~
               at (16,37), fac(d$),      unitcost$( 8%)         , ch(10),~
               at (16,49), fac(d$),      store$   ( 8%)         , ch(05),~
               at (16,54), fac(d$),      lot$     ( 8%)         , ch(08),~
               at (16,66), fac(d$),      user$    ( 8%)         , ch(03),~
               at (16,70), fac(d$),      costtype$( 8%)         , ch(10),~
                                                                         ~
               at (17,02), fac(d$),      recdate$ ( 9%)         , ch(08),~
               at (17,11), fac(d$( 9%)), quantity$( 9%)         , ch(10),~
               at (17,24), fac(d$),      totlcost$( 9%)         , ch(10),~
               at (17,37), fac(d$),      unitcost$( 9%)         , ch(10),~
               at (17,49), fac(d$),      store$   ( 9%)         , ch(05),~
               at (17,54), fac(d$),      lot$     ( 9%)         , ch(08),~
               at (17,66), fac(d$),      user$    ( 9%)         , ch(03),~
               at (17,70), fac(d$),      costtype$( 9%)         , ch(10),~
                                                                         ~
               at (18,02), fac(g$),      recdate$ (10%)         , ch(08),~
               at (18,11), fac(d$(10%)), quantity$(10%)         , ch(10),~
               at (18,24), fac(d$),      totlcost$(10%)         , ch(10),~
               at (18,37), fac(d$),      unitcost$(10%)         , ch(10),~
               at (18,49), fac(d$),      store$   (10%)         , ch(05),~
               at (18,54), fac(d$),      lot$     (10%)         , ch(08),~
               at (18,66), fac(d$),      user$    (10%)         , ch(03),~
               at (18,70), fac(d$),      costtype$(10%)         , ch(10),~
                                                                         ~
               at (19,11), fac(u$), dummy$                      , ch(10),~
               at (19,24), fac(u$), dummy$                      , ch(10),~
                                                                         ~
               at (20,02), "Totals:",                                    ~
               at (20,11), fac(hex(84)), total_qty$             , ch(10),~
               at (20,24), fac(hex(84)), total_crdt$            , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)), edtmessage$            , ch(79),~
                                                                         ~
               at (22,02), fac(hex(8c)), pf1$,                           ~
               at (22,21), fac(hex(8c)), pf6$,                           ~
               at (22,41), "(9)Jobs Requiring Part",                     ~
               at (22,65), "(13)Instructions",                           ~
                                                                         ~
               at (23,02), fac(hex(8c)), pf2$,                           ~
               at (23,21), fac(hex(8c)), pf7$,                           ~
               at (23,41), "(10)See Cost Breakdown",                     ~
               at (23,65), "(15)Print Screen",                           ~
                                                                         ~
               at (24,02), fac(hex(8c)), pf5$,                           ~
               at (24,21), "(8)Inventory Status",                        ~
               at (24,41), "(14)Cost Detail Report",                     ~
               at (24,65), fac(hex(84)), pf16$,                          ~
                                                                         ~
               keys (keytab$), key (keyhit%)

               if keyhit% <> 13 then L41540
                  call "MANUAL" ("JBQCREDT")
                  goto L40040

L41540:        if keyhit% <> 15 then L41580
                  call "PRNTSCRN"
                  goto L40040

L41580:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
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
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
