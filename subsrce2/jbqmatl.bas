        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB    QQQ   M   M   AAA   TTTTT  L              *~
            *    J    B   B  Q   Q  MM MM  A   A    T    L              *~
            *    J    BBBB   Q   Q  M M M  AAAAA    T    L              *~
            *  J J    B   B  Q Q Q  M   M  A   A    T    L              *~
            *   J     BBBB    QQQ   M   M  A   A    T    LLLLL          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBQMATL  - Displays the Job Materials Ledger for the job  *~
            *            passed by the caller. Replaces PF(1) in the old*~
            *            JBMANAG2.                                      *~
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
            * 07/06/87 ! Original                                 ! JIM *~
            * 03/23/88 ! Added Plow To Display Unkitted Parts List! MDE *~
            *          ! And Allow Access To S/R PIPATCSB         ! MDE *~
            * 09/06/90 ! Took out 2nd PIPOUT channel & fixed      ! JDH *~
            *          !   PLOWCODE Parts Remaining display.      !     *~
            * 05/25/93 ! Core Ledgers - Code Reorg                ! KAB *~
            * 09/03/97 ! Add all(hex(00)) to plow for year 2000.  ! RJH *~
	    * 09/05/97 ! Correct File now passed to PIPATCSB.     ! RJH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "JBQMATL"                    /* Job Materials Ledger S/R   */~
                (jobnbr_in$,             /* Job Number passed fr caller*/~
                #01,                     /* SYSFILE2 UFB               */~
                #02,                     /* HNYMASTR UFB               */~
                #03,                     /* JBMATER2 UFB               */~
                #04,                     /* PIPOUT UFB                 */~
                #05,                     /* JBMASTR2 UFB               */~
                #06,                     /* JBVALUE2 UFB               */~
                #55,                     /* PIPMASTR UFB               */~
                #56,                     /* SFCUM2 UFB                 */~
                #59,                     /* CALMASTR  UFB              */~
                #58,                     /* PIPIN UFB                  */~
                #57,                     /* HNYDETAL UFB               */~
                #07,                     /* DEMMASTR UFB               */~
                #35,                     /* PIPCROSS UFB               */~
                #08)                     /* HNYMASTC UFB               */~

        dim                                                              ~
            column1$79, column2$79, c$1, /* Screen column headers, FAC */~
            cursor%(2),                  /* Cursor location for edit   */~
            d$1,                         /* Data FAC                   */~
            g$1,                         /* Editable FAC               */~
            daystart$8,                  /* Actual Start Date          */~
            dayend$8,                    /* Planned End Date           */~
            date$8,                      /* Date for screen display    */~
            descr_map(08),               /* For Plow                   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i(1),                        /* For Plow                   */~
            i$(24)80,                    /* Screen Image               */~
            header$(3)80,                /* Header Info For Plow       */~
            jobnbr$8, jobdesc$32,        /* Job Number passed fr caller*/~
            keytab$17,                   /* PF keys enabled            */~
            line2$79,                    /* Second Line of Screen Headr*/~
            ltype$30,                    /* Ledger type                */~
            lot$(15)6,                   /* Lot numbers                */~
            nomore$25,                   /* ** No more Parts on file   */~
            msg$79,                      /* Plow Message               */~
            page$3,                      /* Page # for screens         */~
            partcost(12),partcost$(15)96,/* Part cost breakdown        */~
            part$(15)25, partdesc$(15)32,/* Part Numbers & descriptions*/~
            partobld$25,                 /* Part To Build              */~
            pipoutplow$56,               /* PIPOUT Plow key            */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf2$16,                      /* PF 2 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            pf6$18,                      /* PF 6 Screen Literal        */~
            pf7$18,                      /* PF 7 Screen Literal        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            recdate$(15)8,               /* Date of JBMATER2 record    */~
            quantity$(15)10,             /* Quantity moved to job      */~
            standard$(15)10,             /* Standard cost per part     */~
            store$(15)3,                 /* Store numbers              */~
            total_act$10, total_std$10,  /* Screen Totals              */~
            totlcost$(15)10,             /* Total cost per part        */~
            unitcost$(15)10              /* Total cost per unit        */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */
*          FS%(64),                     /* = 1 if file open, -1 if it */~
*                                       /*   doesn't exist, or 0 if   */~
*                                       /*   not yet checked (OPENCHCK*/~
*          RSLT$(64)20                  /* Text from file opening    */

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
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! HNYMASTR ! Inventory Master File                    *~
            * #03 ! JBMATER2 ! Production job material used detail file *~
            * #04 ! PIPOUT   ! Planned inventory use detail rec         *~
            * #05 ! JBMASTR2 ! Production job master file               *~
            * #06 ! JBVALUE2 ! Production job value added detail        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

*          CALL "OPENCHCK" (#01, FS%( 1), F2%( 1), 0%, RSLT$( 1))
*          CALL "OPENCHCK" (#02, FS%( 2), F2%( 2), 0%, RSLT$( 2))
*          CALL "OPENCHCK" (#03, FS%( 3), F2%( 3), 0%, RSLT$( 3))
*          CALL "OPENCHCK" (#04, FS%( 4), F2%( 4), 0%, RSLT$( 4))
*          CALL "OPENCHCK" (#05, FS%( 5), F2%( 5), 0%, RSLT$( 5))
*          CALL "OPENCHCK" (#06, FS%( 6), F2%( 6), 0%, RSLT$( 6))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

	    jobnbr$ =  jobnbr_in$
            date$ = date : call "DATEFMT" (date$)
            c$ = hex(ac) : d$ = hex(84) : g$ = hex(86)
            s% = dim(part$(), 1) /* Establish number of lines per screen*/
            column1$ = "  Date  " & c$ & "Part Number              " &   ~
                c$ & "  Quantity" & c$ & " Unit Cost" & c$ & "Total Cost"~
                & c$ & " Total Std" & hex(8c)
            column2$ = "  Date  " & c$ & "Part Description         " &   ~
                c$ & "  Quantity" & c$ & " Unit Cost" & c$ & "Total Cost"~
                & c$ & "Str" & c$ & "Lot   " & hex(8c)

            edtmessage$  = "Notice: This job is NOT kitted complete.  T"&~
                "here are still components due."
            init (hex(00)) pipoutplow$
            str(pipoutplow$,,19) = "JOB ORDER: " & str(jobnbr$)
            call "PLOWNEXT" (#4, pipoutplow$, 19%, f1%(4))
            if f1%(4) <> 1% then edtmessage$= "This job has been kitted"&~
                     " COMPLETE, so all components may be seen."

            nomore$ = "** No more Parts on file"

            jobdesc$ = "Job not found in master file"
            partobld$, daystart$, dayend$ = " "

            call "READ100" (#5, jobnbr$, f1%(5))
            if f1%(5) = 1% then get #5 using L09380, jobdesc$, partobld$,  ~
                                                   daystart$, dayend$
L09380:         FMT  POS(9), CH(30), POS(58), CH(25), POS(147), CH(6),   ~
                     POS(174), CH(6)

            call "DATEFMT" (daystart$)
            call "DATEFMT" (dayend$)

            jobdesc$ = "(" & jobdesc$ & ")"

            line2$ = "Job: " & jobnbr$ & " " & jobdesc$
            str(line2$,63%) = "JBQMATL: " & str(cms2v$,,8%)

            toggle% = 1%
            do_cores% = 0%

            call "READ100" (#8, jobnbr$, cores%)
            ltype$, pf6$, pf7$ = " "
            if cores%  = 0% then L09510
               pf6$ = "(6)Core Materials"
               pf7$ = "(7)Core Ind. Value"
               ltype$ = "New Materials"

L09510:     gosub totals_routine
            gosub first_screen

        REM *************************************************************~
            *        M A I N   S U B R O U T I N E   L O O P            *~
            *************************************************************

        screen_control
            errormsg$ = " "
            on toggle% gosub L40000, L45000
            if keyhit% =  2% then gosub first_screen
            if keyhit% =  5% then gosub next_screen
            if keyhit% =  6% then goto toggle_core
            if keyhit% =  7% then gosub jbqvalue_call
            if keyhit% =  8% then toggle% = 2% -  mod(toggle% + 1%, 2%)
            if keyhit% =  9% then gosub comp_list
            if keyhit% = 10% then gosub show_distribution
            if keyhit% = 14% then gosub print_cost_details
            if keyhit% = 16% then end
            goto screen_control

        toggle_core
            if do_cores% = 0% then L10220
               do_cores% = 0%
               call "CONVERT" (total_act, 4.4, total_act$)
               call "CONVERT" (total_std, 4.4, total_std$)
               ltype$ = "New Materials"
               goto L10260

L10220:        do_cores% = 1%
               call "CONVERT" (core_act, 4.4, total_act$)
               call "CONVERT" (core_std, 4.4, total_std$)
               ltype$ = "Core Materials"

L10260:        gosub first_screen
               goto screen_control

        REM *************************************************************~
            *           C O M M O N   S U B R O U T I N E S             *~
            *************************************************************

        show_distribution
            x% = cursor%(1) - 4%
            if x% < 1% or x% > s% then goto L15080
            if part$(x%) <> " " and part$(x%) <> nomore$ then L15120
L15080:         errormsg$ = "Place cursor on desired part's line, then "&~
                      "press PF(10)"
                return

L15120:     call "HNYCDIST" ("D", part$(x%), partdesc$(x%),              ~
                 "JBQMATL: Job Materials Ledger cost breakdown"         ,~
                 #01, partcost$(x%), temp$, temp)

            errormsg$ = " "
            return

        print_cost_details
            call "JBQCDTLS" (jobnbr$, #01, #05, #03, #06, #02, #08)
            return

        jbqvalue_call
            call "JBQVALUE" (jobnbr$, "X", #01, #05, #06, #03, #02)
            return

        comp_list
L15510:
                msg$ = hex(06) & "Position Cursor And Press Return To" & ~
            " See ATC, PF(16) To Exit"
                  str(pipoutplow$,,19) = "JOB ORDER: " & str(jobnbr$)
                  init (hex(00)) str(pipoutplow$,20)
                  i(1) = 0 : i$() = " "
                  descr_map(01) =  20.25 :descr_map(02) = 01.0  /*part*/
                  descr_map(03) = -26.32 :descr_map(04) = 26.0 /* Desc */
                  descr_map(07) =  57.08 :descr_map(08) = 58.1040

                  header$(1%) = "  Part Number              Descript" &  ~
                           "ion                       Qty Needed        "
                  header$(3%) = hex(a4) & "Parts Remaining To Be Kitte" &~
                           "d To Job Number "& jobnbr$ & "  Begin: " &   ~
                            daystart$ & " End: " & dayend$

                  call "PLOWCODE" (#04, pipoutplow$,msg$, 9019%, 0.25,   ~
                             f1%(04), header$(), 0, 20.00260, i(), i$(), ~
                             "D", " ", #2, descr_map())

                  if f1%(4)  = 0% then return  /* Exit - Done */

                  readkey$ = str(pipoutplow$,20,25)
                  call "PIPATCSB" (readkey$, #55, #2, #56, #59, #58, #04,~
                                   #57, #7, #35)
                  errormsg$ = " "

                  goto L15510 /* Return To Kit List Display Till Done */

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
            * DATA LOAD LOOP                                            *~
            *************************************************************

        first_screen

        REM Start reading JBMATER2 at the beginning of the job passed in
            p%, endofjob% = 0%
            plowkey$ = str(jobnbr$) & all(hex(00))

        next_screen

        REM Initialize all arrays/fields used for screen-loads
            p% = p% + 1%              /* Bump screen page number up one */
            init (" ") recdate$(), part$(), partdesc$(), quantity$(),    ~
                unitcost$(), totlcost$(), standard$(), store$(), lot$(), ~
                temp$, errormsg$
            init (hex(00)) partcost$()
            temp = 0

        REM Read the next screen-load of records for the job passed in
            for l% = 1% to s%
L30220:         call "PLOWNEXT" (#03, plowkey$, 8%, f1%(3))
                if f1%(3) <> 0% then goto L30270
                     endofjob% = 1%
                     part$(l%), partdesc$(l%) = nomore$
                     goto L30500
L30270:         get #03 using L30300, recdate$(l%), part$(l%), store$(l%),~
                                     lot$(l%), quantity, totlcost,       ~
                                     partcost(), standard, coreflag$

L30300:              FMT  /* File #03- JBMATER2                        */~
                          POS(9), CH(6), POS(23), CH(25), POS(48), CH(3),~
                          CH(6), POS(71), 15*PD(14,4), POS(338), CH(4)

                if coreflag$ = " " then L30334
                   if do_cores% <> 0% then L30340
                      goto L30335
L30334:            if do_cores%  = 0% then L30340
L30335:               recdate$(l%), part$(l%), store$(l%), lot$(l%) = " "
                      goto L30220

L30340: REM Now we've got a record, so edit it for display
                for i% = 1% to 12% /* Total costs to unit costs */
                     partcost(i%) = partcost(i%) / quantity
                next i%
                put partcost$(l%) using L30390, partcost()
L30390:              FMT 12*PD(14,4)
                unitcost = totlcost / quantity
                standard = standard * quantity
                call "DATEFMT" (recdate$(l%))
                call "DESCRIBE" (#02, part$(l%), partdesc$(l%),0%, f1%(2))
                call "CONVERT" (quantity, 2.2, quantity$(l%))
                call "CONVERT" (unitcost, 4.4, unitcost$(l%))
                call "CONVERT" (totlcost, 4.4, totlcost$(l%))
                call "CONVERT" (standard, 4.4, standard$(l%))
            next l%

L30500: REM Set up the PF keys according to the page we are displaying
            convert p% to page$, pic (###)
            pf2$, pf5$ = " " : pf16$ = "(16)Return"
            keytab$ = hex(ffffffffffffff08090affff0d0e0f1000)
            if p% = 1% then goto L30560
                pf2$ = "(2)First Page" : str(keytab$, 2, 1) = hex(02)
L30560:     if endofjob% = 1% then L30581
                pf5$ = "(5)Next Page" : str(keytab$, 5, 1) = hex(05)

L30581:     if cores% = 0% then return
               str(keytab$, 6, 2) = hex(0607)
               pf6$ = "(6)New Materials"
               if do_cores%  = 0% then pf6$ = "(6)Core Materials"
            return

        REM *************************************************************~
            * TOTALS ROUTINE                                            *~
            *************************************************************

        totals_routine

            total_act$, total_std$ = " "
            total_act , total_std  = 0
            core_act  , core_std   = 0

            plowkey$ = str(jobnbr$) & all(hex(00))
L31090:     call "PLOWNEXT" (#3, plowkey$, 8%, f1%(3))
            if f1%(3) = 0% then L31170
                get #3 using L31120, qty, act, std, coreflag$
L31120:             FMT POS(71), 2*PD(14,4), POS(183), PD(14,4),         ~
                        POS(338), CH(4)
                if coreflag$ = " " then L31130
                   core_act = core_act + act
                   core_std = core_std + std * qty
                   goto L31090
L31130:         total_act = total_act + act
                total_std = total_std + std * qty
                goto L31090

L31170:     call "CONVERT" (total_act, 4.4, total_act$)
            call "CONVERT" (total_std, 4.4, total_std$)
            return

L40000: REM *************************************************************~
            *     'N O R M A L'   S C R E E N   P A G E   (PARTS)       *~
            *************************************************************

L40040:     accept                                                       ~
               at (01,02), "Job Materials Ledger: page",                 ~
               at (01,29), fac(hex(8c)), page$                  , ch(03),~
               at (01,35), fac(hex(84)), ltype$                 , ch(30),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), fac(c$),      column1$               , ch(79),~
                                                                         ~
               at (05,02), fac(g$),      recdate$ ( 1)          , ch(08),~
               at (05,11), fac(d$),      part$    ( 1)          , ch(25),~
               at (05,37), fac(d$),      quantity$( 1)          , ch(10),~
               at (05,48), fac(d$),      unitcost$( 1)          , ch(10),~
               at (05,59), fac(d$),      totlcost$( 1)          , ch(10),~
               at (05,70), fac(d$),      standard$( 1)          , ch(10),~
                                                                         ~
               at (06,02), fac(g$),      recdate$ ( 2)          , ch(08),~
               at (06,11), fac(d$),      part$    ( 2)          , ch(25),~
               at (06,37), fac(d$),      quantity$( 2)          , ch(10),~
               at (06,48), fac(d$),      unitcost$( 2)          , ch(10),~
               at (06,59), fac(d$),      totlcost$( 2)          , ch(10),~
               at (06,70), fac(d$),      standard$( 2)          , ch(10),~
                                                                         ~
               at (07,02), fac(g$),      recdate$ ( 3)          , ch(08),~
               at (07,11), fac(d$),      part$    ( 3)          , ch(25),~
               at (07,37), fac(d$),      quantity$( 3)          , ch(10),~
               at (07,48), fac(d$),      unitcost$( 3)          , ch(10),~
               at (07,59), fac(d$),      totlcost$( 3)          , ch(10),~
               at (07,70), fac(d$),      standard$( 3)          , ch(10),~
                                                                         ~
               at (08,02), fac(g$),      recdate$ ( 4)          , ch(08),~
               at (08,11), fac(d$),      part$    ( 4)          , ch(25),~
               at (08,37), fac(d$),      quantity$( 4)          , ch(10),~
               at (08,48), fac(d$),      unitcost$( 4)          , ch(10),~
               at (08,59), fac(d$),      totlcost$( 4)          , ch(10),~
               at (08,70), fac(d$),      standard$( 4)          , ch(10),~
                                                                         ~
               at (09,02), fac(g$),      recdate$ ( 5)          , ch(08),~
               at (09,11), fac(d$),      part$    ( 5)          , ch(25),~
               at (09,37), fac(d$),      quantity$( 5)          , ch(10),~
               at (09,48), fac(d$),      unitcost$( 5)          , ch(10),~
               at (09,59), fac(d$),      totlcost$( 5)          , ch(10),~
               at (09,70), fac(d$),      standard$( 5)          , ch(10),~
                                                                         ~
               at (10,02), fac(g$),      recdate$ ( 6)          , ch(08),~
               at (10,11), fac(d$),      part$    ( 6)          , ch(25),~
               at (10,37), fac(d$),      quantity$( 6)          , ch(10),~
               at (10,48), fac(d$),      unitcost$( 6)          , ch(10),~
               at (10,59), fac(d$),      totlcost$( 6)          , ch(10),~
               at (10,70), fac(d$),      standard$( 6)          , ch(10),~
                                                                         ~
               at (11,02), fac(g$),      recdate$ ( 7)          , ch(08),~
               at (11,11), fac(d$),      part$    ( 7)          , ch(25),~
               at (11,37), fac(d$),      quantity$( 7)          , ch(10),~
               at (11,48), fac(d$),      unitcost$( 7)          , ch(10),~
               at (11,59), fac(d$),      totlcost$( 7)          , ch(10),~
               at (11,70), fac(d$),      standard$( 7)          , ch(10),~
                                                                         ~
               at (12,02), fac(g$),      recdate$ ( 8)          , ch(08),~
               at (12,11), fac(d$),      part$    ( 8)          , ch(25),~
               at (12,37), fac(d$),      quantity$( 8)          , ch(10),~
               at (12,48), fac(d$),      unitcost$( 8)          , ch(10),~
               at (12,59), fac(d$),      totlcost$( 8)          , ch(10),~
               at (12,70), fac(d$),      standard$( 8)          , ch(10),~
                                                                         ~
               at (13,02), fac(g$),      recdate$ ( 9)          , ch(08),~
               at (13,11), fac(d$),      part$    ( 9)          , ch(25),~
               at (13,37), fac(d$),      quantity$( 9)          , ch(10),~
               at (13,48), fac(d$),      unitcost$( 9)          , ch(10),~
               at (13,59), fac(d$),      totlcost$( 9)          , ch(10),~
               at (13,70), fac(d$),      standard$( 9)          , ch(10),~
                                                                         ~
               at (14,02), fac(g$),      recdate$ (10)          , ch(08),~
               at (14,11), fac(d$),      part$    (10)          , ch(25),~
               at (14,37), fac(d$),      quantity$(10)          , ch(10),~
               at (14,48), fac(d$),      unitcost$(10)          , ch(10),~
               at (14,59), fac(d$),      totlcost$(10)          , ch(10),~
               at (14,70), fac(d$),      standard$(10)          , ch(10),~
                                                                         ~
               at (15,02), fac(g$),      recdate$ (11)          , ch(08),~
               at (15,11), fac(d$),      part$    (11)          , ch(25),~
               at (15,37), fac(d$),      quantity$(11)          , ch(10),~
               at (15,48), fac(d$),      unitcost$(11)          , ch(10),~
               at (15,59), fac(d$),      totlcost$(11)          , ch(10),~
               at (15,70), fac(d$),      standard$(11)          , ch(10),~
                                                                         ~
               at (16,02), fac(g$),      recdate$ (12)          , ch(08),~
               at (16,11), fac(d$),      part$    (12)          , ch(25),~
               at (16,37), fac(d$),      quantity$(12)          , ch(10),~
               at (16,48), fac(d$),      unitcost$(12)          , ch(10),~
               at (16,59), fac(d$),      totlcost$(12)          , ch(10),~
               at (16,70), fac(d$),      standard$(12)          , ch(10),~
                                                                         ~
               at (17,02), fac(g$),      recdate$ (13)          , ch(08),~
               at (17,11), fac(d$),      part$    (13)          , ch(25),~
               at (17,37), fac(d$),      quantity$(13)          , ch(10),~
               at (17,48), fac(d$),      unitcost$(13)          , ch(10),~
               at (17,59), fac(d$),      totlcost$(13)          , ch(10),~
               at (17,70), fac(d$),      standard$(13)          , ch(10),~
                                                                         ~
               at (18,02), fac(g$),      recdate$ (14)          , ch(08),~
               at (18,11), fac(d$),      part$    (14)          , ch(25),~
               at (18,37), fac(d$),      quantity$(14)          , ch(10),~
               at (18,48), fac(d$),      unitcost$(14)          , ch(10),~
               at (18,59), fac(d$),      totlcost$(14)          , ch(10),~
               at (18,70), fac(d$),      standard$(14)          , ch(10),~
                                                                         ~
               at (19,02), fac(g$),      recdate$ (15)          , ch(08),~
               at (19,11), fac(d$),      part$    (15)          , ch(25),~
               at (19,37), fac(d$),      quantity$(15)          , ch(10),~
               at (19,48), fac(d$),      unitcost$(15)          , ch(10),~
               at (19,59), fac(hex(a4)), totlcost$(15)          , ch(10),~
               at (19,70), fac(hex(a4)), standard$(15)          , ch(10),~
                                                                         ~
               at (20,42), "Material Ledger:",                           ~
               at (20,59), fac(hex(84)), total_act$             , ch(10),~
               at (20,70), fac(hex(84)), total_std$             , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)), edtmessage$            , ch(79),~
               at (22,18), fac(hex(8c)), pf6$,                           ~
               at (22,38), "(9)See Remaining Kit List",                  ~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), fac(hex(8c)), pf2$,                           ~
               at (23,18), fac(hex(8c)), pf7$,                           ~
               at (23,38), "(10)See Cost Breakdown",                     ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,02), fac(hex(8c)), pf5$,                           ~
               at (24,18), "(8)Toggle Screen ",                          ~
               at (24,38), "(14)Cost Detail Report",                     ~
               at (24,65), fac(hex(84)), pf16$,                          ~
               keys (keytab$), key (keyhit%)

               if keyhit% <> 13 then L41380
                  call "MANUAL" ("JBQMATL ")
                  goto L40040

L41380:        if keyhit% <> 15 then L41420
                  call "PRNTSCRN"
                  goto L40040

L41420:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

L45000: REM *************************************************************~
            *'A L T E R N A T E'   S C R E E N   P A G E  (DESCRIPTIONS)*~
            *************************************************************

L45040:     accept                                                       ~
               at (01,02), "Job Materials Ledger: page",                 ~
               at (01,29), fac(hex(8c)), page$                  , ch(03),~
               at (01,35), fac(hex(84)), ltype$                 , ch(30),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), fac(c$),      column2$               , ch(79),~
                                                                         ~
               at (05,02), fac(g$),      recdate$ ( 1)          , ch(08),~
               at (05,11), fac(d$),      partdesc$( 1)          , ch(25),~
               at (05,37), fac(d$),      quantity$( 1)          , ch(10),~
               at (05,48), fac(d$),      unitcost$( 1)          , ch(10),~
               at (05,59), fac(d$),      totlcost$( 1)          , ch(10),~
               at (05,70), fac(d$),      store$   ( 1)          , ch(03),~
               at (05,74), fac(d$),      lot$     ( 1)          , ch(06),~
                                                                         ~
               at (06,02), fac(g$),      recdate$ ( 2)          , ch(08),~
               at (06,11), fac(d$),      partdesc$( 2)          , ch(25),~
               at (06,37), fac(d$),      quantity$( 2)          , ch(10),~
               at (06,48), fac(d$),      unitcost$( 2)          , ch(10),~
               at (06,59), fac(d$),      totlcost$( 2)          , ch(10),~
               at (06,70), fac(d$),      store$   ( 2)          , ch(03),~
               at (06,74), fac(d$),      lot$     ( 2)          , ch(06),~
                                                                         ~
               at (07,02), fac(g$),      recdate$ ( 3)          , ch(08),~
               at (07,11), fac(d$),      partdesc$( 3)          , ch(25),~
               at (07,37), fac(d$),      quantity$( 3)          , ch(10),~
               at (07,48), fac(d$),      unitcost$( 3)          , ch(10),~
               at (07,59), fac(d$),      totlcost$( 3)          , ch(10),~
               at (07,70), fac(d$),      store$   ( 3)          , ch(03),~
               at (07,74), fac(d$),      lot$     ( 3)          , ch(06),~
                                                                         ~
               at (08,02), fac(g$),      recdate$ ( 4)          , ch(08),~
               at (08,11), fac(d$),      partdesc$( 4)          , ch(25),~
               at (08,37), fac(d$),      quantity$( 4)          , ch(10),~
               at (08,48), fac(d$),      unitcost$( 4)          , ch(10),~
               at (08,59), fac(d$),      totlcost$( 4)          , ch(10),~
               at (08,70), fac(d$),      store$   ( 4)          , ch(03),~
               at (08,74), fac(d$),      lot$     ( 4)          , ch(06),~
                                                                         ~
               at (09,02), fac(g$),      recdate$ ( 5)          , ch(08),~
               at (09,11), fac(d$),      partdesc$( 5)          , ch(25),~
               at (09,37), fac(d$),      quantity$( 5)          , ch(10),~
               at (09,48), fac(d$),      unitcost$( 5)          , ch(10),~
               at (09,59), fac(d$),      totlcost$( 5)          , ch(10),~
               at (09,70), fac(d$),      store$   ( 5)          , ch(03),~
               at (09,74), fac(d$),      lot$     ( 5)          , ch(06),~
                                                                         ~
               at (10,02), fac(g$),      recdate$ ( 6)          , ch(08),~
               at (10,11), fac(d$),      partdesc$( 6)          , ch(25),~
               at (10,37), fac(d$),      quantity$( 6)          , ch(10),~
               at (10,48), fac(d$),      unitcost$( 6)          , ch(10),~
               at (10,59), fac(d$),      totlcost$( 6)          , ch(10),~
               at (10,70), fac(d$),      store$   ( 6)          , ch(03),~
               at (10,74), fac(d$),      lot$     ( 6)          , ch(06),~
                                                                         ~
               at (11,02), fac(g$),      recdate$ ( 7)          , ch(08),~
               at (11,11), fac(d$),      partdesc$( 7)          , ch(25),~
               at (11,37), fac(d$),      quantity$( 7)          , ch(10),~
               at (11,48), fac(d$),      unitcost$( 7)          , ch(10),~
               at (11,59), fac(d$),      totlcost$( 7)          , ch(10),~
               at (11,70), fac(d$),      store$   ( 7)          , ch(03),~
               at (11,74), fac(d$),      lot$     ( 7)          , ch(06),~
                                                                         ~
               at (12,02), fac(g$),      recdate$ ( 8)          , ch(08),~
               at (12,11), fac(d$),      partdesc$( 8)          , ch(25),~
               at (12,37), fac(d$),      quantity$( 8)          , ch(10),~
               at (12,48), fac(d$),      unitcost$( 8)          , ch(10),~
               at (12,59), fac(d$),      totlcost$( 8)          , ch(10),~
               at (12,70), fac(d$),      store$   ( 8)          , ch(03),~
               at (12,74), fac(d$),      lot$     ( 8)          , ch(06),~
                                                                         ~
               at (13,02), fac(g$),      recdate$ ( 9)          , ch(08),~
               at (13,11), fac(d$),      partdesc$( 9)          , ch(25),~
               at (13,37), fac(d$),      quantity$( 9)          , ch(10),~
               at (13,48), fac(d$),      unitcost$( 9)          , ch(10),~
               at (13,59), fac(d$),      totlcost$( 9)          , ch(10),~
               at (13,70), fac(d$),      store$   ( 9)          , ch(03),~
               at (13,74), fac(d$),      lot$     ( 9)          , ch(06),~
                                                                         ~
               at (14,02), fac(g$),      recdate$ (10)          , ch(08),~
               at (14,11), fac(d$),      partdesc$(10)          , ch(25),~
               at (14,37), fac(d$),      quantity$(10)          , ch(10),~
               at (14,48), fac(d$),      unitcost$(10)          , ch(10),~
               at (14,59), fac(d$),      totlcost$(10)          , ch(10),~
               at (14,70), fac(d$),      store$   (10)          , ch(03),~
               at (14,74), fac(d$),      lot$     (10)          , ch(06),~
                                                                         ~
               at (15,02), fac(g$),      recdate$ (11)          , ch(08),~
               at (15,11), fac(d$),      partdesc$(11)          , ch(25),~
               at (15,37), fac(d$),      quantity$(11)          , ch(10),~
               at (15,48), fac(d$),      unitcost$(11)          , ch(10),~
               at (15,59), fac(d$),      totlcost$(11)          , ch(10),~
               at (15,70), fac(d$),      store$   (11)          , ch(03),~
               at (15,74), fac(d$),      lot$     (11)          , ch(06),~
                                                                         ~
               at (16,02), fac(g$),      recdate$ (12)          , ch(08),~
               at (16,11), fac(d$),      partdesc$(12)          , ch(25),~
               at (16,37), fac(d$),      quantity$(12)          , ch(10),~
               at (16,48), fac(d$),      unitcost$(12)          , ch(10),~
               at (16,59), fac(d$),      totlcost$(12)          , ch(10),~
               at (16,70), fac(d$),      store$   (12)          , ch(03),~
               at (16,74), fac(d$),      lot$     (12)          , ch(06),~
                                                                         ~
               at (17,02), fac(g$),      recdate$ (13)          , ch(08),~
               at (17,11), fac(d$),      partdesc$(13)          , ch(25),~
               at (17,37), fac(d$),      quantity$(13)          , ch(10),~
               at (17,48), fac(d$),      unitcost$(13)          , ch(10),~
               at (17,59), fac(d$),      totlcost$(13)          , ch(10),~
               at (17,70), fac(d$),      store$   (13)          , ch(03),~
               at (17,74), fac(d$),      lot$     (13)          , ch(06),~
                                                                         ~
               at (18,02), fac(g$),      recdate$ (14)          , ch(08),~
               at (18,11), fac(d$),      partdesc$(14)          , ch(25),~
               at (18,37), fac(d$),      quantity$(14)          , ch(10),~
               at (18,48), fac(d$),      unitcost$(14)          , ch(10),~
               at (18,59), fac(d$),      totlcost$(14)          , ch(10),~
               at (18,70), fac(d$),      store$   (14)          , ch(03),~
               at (18,74), fac(d$),      lot$     (14)          , ch(06),~
                                                                         ~
               at (19,02), fac(g$),      recdate$ (15)          , ch(08),~
               at (19,11), fac(d$),      partdesc$(15)          , ch(25),~
               at (19,37), fac(d$),      quantity$(15)          , ch(10),~
               at (19,48), fac(d$),      unitcost$(15)          , ch(10),~
               at (19,59), fac(hex(a4)), totlcost$(15)          , ch(10),~
               at (19,70), fac(d$),      store$   (15)          , ch(03),~
               at (19,74), fac(d$),      lot$     (15)          , ch(06),~
                                                                         ~
               at (20,42), "Material Ledger:",                           ~
               at (20,59), fac(hex(84)), total_act$             , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)), edtmessage$            , ch(79),~
               at (22,18), fac(hex(8c)), pf6$,                           ~
               at (22,38), "(9)See Remaining Kit List",                  ~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), fac(hex(8c)), pf2$,                           ~
               at (23,18), fac(hex(8c)), pf7$,                           ~
               at (23,38), "(10)See Cost Breakdown",                     ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,02), fac(hex(8c)), pf5$,                           ~
               at (24,18), "(8)Toggle Screen ",                          ~
               at (24,38), "(14)Cost Detail Report",                     ~
               at (24,65), fac(hex(84)), pf16$,                          ~
               keys (keytab$), key (keyhit%)

               if keyhit% <> 13 then L46530
                  call "MANUAL" ("JBQMATL ")
                  goto L45040

L46530:        if keyhit% <> 15 then L46720
                  call "PRNTSCRN"
                  goto L45040

L46720:           close ws
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
