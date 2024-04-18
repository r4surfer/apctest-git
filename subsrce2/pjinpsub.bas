        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   JJJJJ  IIIII  N   N  PPPP    SSS   U   U  BBBB    *~
            *  P   P    J      I    NN  N  P   P  S      U   U  B   B   *~
            *  PPPP     J      I    N N N  PPPP    SSS   U   U  BBBB    *~
            *  P      J J      I    N  NN  P          S  U   U  B   B   *~
            *  P       J     IIIII  N   N  P       SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PJINPSUB - Manual input / maint of Kit lists (BOMs)       *~
            *            tied to Purchase Jobs.                         *~
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
            * 07/20/93 ! ORIGINAL                                 ! RAN *~
            *          !   Cribbed from JBINPKIT - Thanks KEN     !     *~
            * 09/10/93 ! More Changes for Purchase Jobs Project   ! JBK *~
            * 07/21/97 ! Tag No back to YYMMDD format             ! DER *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "PJINPSUB" (part$,           /* Part to 'Buy'              */~
                        partdescr$,      /* Description of Purch. Part */~
                        vencode$,        /* Vendor to Purchase From    */~
                        vendescr$,       /* Vendor Description         */~
                        venpart$,        /* Vendor Part #              */~
                        pipkey$,         /* PIPOUT Tag                 */~
                        orddate$,        /* Default Date for Pipouts   */~
                        duedate$,        /* Due Date for Part to 'Buy' */~
                        bomid$,          /* The Effective BOM          */~
                        qtymake,         /* Quantity of part to 'Buy'  */~
                        pipload%,        /* PIP Load to Workfile Status*/~
                                         /*  Uses the MOD(PIPLOAD%,2%) */~
                                         /*  to determine course of    */~
                                         /*  events:                   */~
                                         /*        0 = Kit List Moved  */~
                                         /*        1 = Move Kit List   */~
                        kitmode%,        /* What to do with Kit List   */~
                                         /*    0% = Dsply Kit List Only*/~
                                         /*    1% = Create, Edit,      */~
                                         /*         Auto Append, etc.  */~
                                         /*    2% = Create, Edit, No   */~
                                         /*         Auto Append, etc.  */~
                                         /*    3% = Update PIP, No Dply*/~
                        #1,              /* PIPOUT   Channel           */~
                        #2,              /* HNYMASTR Channel           */~
                        #3,              /* BOMMASTR Channel           */~
                        #4,              /* ENGMASTR Channel           */~
                        #5,              /* PIPMASTR Channel           */~
                        #6,              /* SFCUM2   Channel           */~
                        #7,              /* SYSFILE2 Channel           */~
                        #8)              /* Workfile - PIPOUTs         */

        dim                                                              ~
            ask$(3)80,                   /* Ask User Text              */~
            bdate$6,                     /* Planning Calendar Base Date*/~
            bom$3,                       /* BOM Id.                    */~
            bom$(490)3,                  /* BOM List                   */~
            bomid$3,                     /* Which Alt BOM?             */~
            copypart$25,                 /* part number for copy BOM   */~
            copybomid$3,                 /* BOM ID to copy             */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            duedate$8,                   /* Due Date for 'Buy' Part    */~
            errormsg$79,                 /* Error message              */~
            hdr$(3)79,                   /* PLOWCODE header strings    */~
            header$79,                   /* Header                     */~
            header$(1)1,                 /* PLOWCODE Argument          */~
            hfac$(20)1,                  /* Field attribute characters */~
            i$(24)80,                    /* Screen image               */~
            incl(1),                     /* PLOWCODE Argument          */~
            inpmessage$79,               /* Input message              */~
            jbdescr$30,                  /* Description                */~
            picksl_order$1,              /* Pick List Order            */~
            pipkey$19,                   /* PIPIN key, PIPOUT plowkey  */~
            pipworkkey$19,               /* PIPIN key, PIPOUT plowkey  */~
            lfac$(20,7)1,                /* Field attribute characters */~
            line2$79,                    /* Screen Line 2              */~
            mkr$2,                       /* BOM Marker                 */~
            orddate$8,                   /* Purchased Job Order Date   */~
            outtagnr$100,                /* Work Variable              */~
            part$25, partdescr$34,       /* Part Number to Build       */~
            parttype$3,                  /* Part Type Code             */~
            pfkeys$(2)18,                /* P.F. Keys Active           */~
            pipdata$64,                  /* PIPOUT data                */~
            plstdate$8,                  /* Planned Start              */~
            qtymake$10,                  /* Quantity to Make           */~
            readkey$100,                 /* Working String             */~
            readkey$(100)100,            /* Working String             */~
            phfact(101),                 /* Phantom Multiplier         */~
            screen$(2,3)79,              /* Screen Messages            */~
            seq$(1600)5,                 /* Sequence numbers for screen*/~
            stp$4,                       /* Pick before step from bom  */~
            systime$8,                   /* System Time                */~
            tagdate$6,                   /* Tag No date, YYMMDD used   */~
            tagdtfull$8,                 /* Tag No date, CCYYMMDD      */~
            tagdttmp$8,                  /* temp/scratch date          */~
            type$3,                      /* Part Type                  */~
            userid$3,                    /* Guess who                  */~
            vencode$9,                   /* Vendor Code                */~
            vendescr$32,                 /* Vendor Description         */~
            venpart$25,                  /* Vendor Part #              */~
            wipdescr$32,                 /* WIP Acct                   */~
            wipsave$16                   /* WIP ACCT                   */

        dim                                                              ~
            codes$(100)3,                /* Release Codes              */~
            part$(1601)25,               /* Kit Part Number            */~
            partdescr$(1601)45,          /* Part Description           */~
            outdate%(1601),              /* PIPOUT Date Index          */~
            outdate$(1601)8,             /* PIPOUT Date Formatted      */~
            newdate%(1601),              /* PIPOUT Date Index          */~
            newquantity(1601),           /* PIPOUT Quantity            */~
            quantity(1601),              /* PIPOUT Quantity            */~
            quantity$(1601)10,           /* PIPOUT Quantity Formatted  */~
            f2%(32),                     /* FILE STATUS FLAGS FOR      */~
            f1%(32)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! PIPOUT   ! Planned Inventory Withdrawals file       *~
            * #2  ! HNYMASTR ! Inventory Master File                    *~
            * #3  ! BOMMASTR ! BOM Relationship File                    *~
            * #4  ! ENGMASTR ! Engineering Master File                  *~
            * #5  ! PIPMASTR ! Planned Inventory Postion Master file    *~
            * #6  ! SFCUM2   ! Cumulative Sales Forecast file           *~
            * #7  ! SYSFILE2 ! Caelus Management System Information     *~
            * #8  ! WORKFILE ! Workfile for PIPOUTS                     *~
            *************************************************************


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date  :  call "DATEFMT" (date$)
            call "EXTRACT" addr ("ID", userid$)

            tagdttmp$ = date
            call "DATEFMT" ( tagdttmp$, 0%, tagdtfull$ )
            tagdate$ = str( tagdtfull$, 3%, 6% )

            pok% = dim(part$(),1)

            call "READ100" (#7, "MONTHS OPEN", f1%(7%))
                if f1%(7%) = 0% then L65000
            get #7, using L09260, bdate$
L09260:     FMT XX(32), CH(6)

*        See if operator is an administrator or not
            call "CMSMACHK" ("SFC", hfac$(1%), hfac$(2%))
            if hfac$(1%) = "Y" or hfac$(2%) = "Y" then L09430

*        Load Up His Release Codes For Cross Checking...
*          CALL "READNEXT" (#1, F1%(1))
*          IF F1%(1) = 0% THEN 9430  /* Not using feature */
*              READKEY$ = STR(USERID$) & HEX(00000000)
*              CALL "PLOWNEXT" (#1, READKEY$, 3%, F1%(1))
*              IF F1%(1) = 0 THEN 9420
*                   C% = C% + 1%
*                   GET #1, USING 9400, CODES$(C%)
*                        FMT XX(3), CH(3)
*                   GOTO 9360
*              IF CODES$() <> "ALL" THEN 9450   /* What did we get?   */
L09430
*          ADMIN% = 1%  /* This guy can do as he pleases...   */

*        On to bigger and better things...
            pfkeys$(1)=hex(000102060d0f10ff08ffffffffffffffff0e)
            pfkeys$(2)=hex(0001020304050607080b0c0dff0f101cff0e)

            screen$(1,2)="                                               ~
        ~                (15)Print Screen"
            screen$(2,1)="(1)Start Over    (4)Prev Lines  (6)Down One    ~
        ~(11)Insert Line (13)Instructions"
            screen$(2,2)="(2)First Lines   (5)Next Lines  (7)Up One      ~
        ~(12)Delete Line (15)Print Screen"

            for i% = 1% to dim(seq$(),1)
                convert i% to seq$(i%), pic(####)
                str(seq$(i%),5) = ")"
            next i%

            readkey$ = "SWITCHS.SFC"
            call "READ100" (#7, readkey$, f1%(7%))
                if f1%(7%) = 0% then L10000
            get #7, using L09700, picksl_order$
L09700:     FMT POS(31), CH(1)

*          GOSUB CHECK_FOR_RESTRICTIONS


L10000: REM *************************************************************~
            *       I N I T I A L I Z A T I O N  -  E N T R Y           *~
            *-----------------------------------------------------------*~
            * Initialize variables; proceed to edit Kit list.           *~
            *************************************************************

            if kitmode% < 1% or kitmode% > 3% then end

            call "PIPINDEX" (#7, orddate$, orddate%, ret%)
                if ret% = 0% then L10105
                     ask$(1%) = "Order Date is Outside the Planning "   &~
                                "Calendar."
                     goto L10220
L10105:     plstdate$ = orddate$
            call "DATEFMT" (plstdate$)

            call "PIPINDEX" (#7, duedate$, duedate%, ret%)
                if ret% = 0% then L10180
                     ask$(1%) = "Order Due Date is Outside the Planning"&~
                                " Calendar."
                     goto L10220

L10180:     tooldate% = duedate%
                if tooldate% < 490% then tooldate% = tooldate% + 1%
            goto L10300

L10220:     ask$(2%) = "Parts cannot be kitted for " & part$ & "."
            ask$(3%) = "Hit any key to continue..."
            keyhit% = 0%
            call "ASKUSER" (keyhit%, "DATE ERROR", ask$(1%), ask$(2%),   ~
                                                             ask$(3%))
            goto L65000

L10300:     if pipkey$ <> " " then L10350
                init (" ")  pipkey$
                convert orddate% to str(pipkey$,3%,3%), pic(###)
                str(pipkey$,1%, 2%) = "RW"
                str(pipkey$,6%,14%) = tagdate$ & time

L10350:     pipworkkey$ = pipkey$
            str(pipworkkey$,2%,1%) = "W"

            inpmessage$, jbdescr$, qtymake$, readkey$, wipdescr$,        ~
            wipsave$, errormsg$ = " "

            init(" ")  outdate$(), part$(), partdescr$(), quantity$()

            str(pfkeys$(1), 18) = hex(0e) : str(pfkeys$(2), 18) = hex(0e)

            maxlines%, pipouts_changed% = 0%

            mat quantity    = zer
            mat outdate%    = zer
            mat newquantity = zer
            mat newdate%    = zer

            if kitmode% > 2% then L10800
                call "CONVERT" (qtymake, 0.2, qtymake$)
                call "SHOSTAT" ("Preparing for Kit List")
                if mod(pipload%,2%) = 1% then gosub pip_transfer_work
                gosub pip_load_work
                if maxlines% = 0% and kitmode% = 1% then gosub load_bom
                if maxlines% = 0% then gosub inputmode_kit_list
                goto  edit_kit_list

L10800:     if kitmode% > 3% then L65000
                if mod(pipload%,2%) = 1% then end
                gosub update_pips
                end

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Input mode main program.                                  *~
            *************************************************************

        inputmode_kit_list
            lastdate$ = plstdate$
            pbas%, screenline%, c% = 0%

L11100:     screenline% = screenline% + 1%
            if screenline% < 13% then L11140
                pbas% = pbas% + 1%
                screenline% = 12%
L11140:     c% = pbas%+screenline%
            if maxlines% > pok%-2% then edit_kit_list
            gosub input_kit_list
            if keyhit% <> 16% then L11100 else return

        input_kit_list
            inpmessage$  = " "
            for fieldnr% = 1% to 4%
                if fieldnr% = 4% then outdate$(c%) = lastdate$
                if fieldnr% = 2% then L11360
L11240:         gosub'201(screenline%,fieldnr%)
                     if keyhit% =   1% then gosub startover
                     if keyhit% =   2% then       columnone1
                     if keyhit% =   6% then gosub lineabove1
                     if keyhit% <> 14% then L11320
                          gosub load_bom
                          return clear all
                          goto edit_kit_list
L11320:              if keyhit% =  16% and fieldnr% = 1% then return
                     if keyhit% <>  0% and keyhit% <> 6% then L11240
                gosub'153(c%,fieldnr%)
                     if errormsg$<>" " then L11240
L11360:     next fieldnr%
            maxlines% = maxlines% + 1%
            return

        columnone1
            init (" ") part$(c%), partdescr$(c%), quantity$(c%),         ~
                       outdate$(c%), errormsg$
            goto input_kit_list

        lineabove1
            if c% = 1% then return
                on fieldnr% goto L11490, L11500, L11510, L11520
                return
L11490:              part$(c%)      = part$(c%-1%)     : return
L11500:              partdescr$(c%) = partdescr$(c%-1%): return
L11510:              quantity$(c%)  = quantity$(c%-1%) : return
L11520:              outdate$(c%)   = outdate$(c%-1%)  : return

        REM *************************************************************~
            *               E D I T   K I T   L I S T                   *~
            *-----------------------------------------------------------*~
            * Handles operation of edit mode for PIPOUT line screen     *~
            *************************************************************

        edit_kit_list
            pbas% = 0%
        cont_edit_kit
            c%, screenline% = 0%
            pbas% = max(0%,min(pbas%,maxlines%-12%))
L12110:     inpmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Field And Press RETURN."
            errormsg$=" "

L12150:     gosub'202(0%,0%)
                  if keyhit% =  1% then gosub startover
                  if keyhit% =  2% then pbas% = 0%
                  if keyhit% =  3% then pbas% = pok%
                  if keyhit% =  4% then pbas% = pbas% - 12%
                  if keyhit% =  5% then pbas% = pbas% + 12%
                  if keyhit% =  6% then pbas% = pbas% - 1%
                  if keyhit% =  7% then pbas% = pbas% + 1%
                  pbas% = max(0%,min(pbas%,maxlines%-12%))
                  if keyhit% = 11% then insertline1
                  if keyhit% = 14% then gosub load_bom
                  if keyhit% = 28% then gosub delete_all1
                  if keyhit% = 16% then datasave
                  if keyhit% <> 0% and keyhit% <> 12% then L12150

            screenline% = cursor%(1%) - 7%
            if screenline% < 1% or screenline% > 12% then cont_edit_kit
            c% = screenline% + pbas%
            if c% > maxlines% then cont_edit_kit
            if keyhit% = 12% then deleteline1
            fieldnr% = 0%
            if cursor%(2%) > 60% then fieldnr% = 3%
            if cursor%(2%) > 71% then fieldnr% = 4%
            if fieldnr% = 0% then L12470
            inpmessage$ = " "
L12400:     gosub'202(screenline%,fieldnr%)
                if keyhit% =  1% then gosub startover
                if keyhit% <> 0% then L12400
            gosub'153(c%,fieldnr%)
            if errormsg$ <> " " then L12400
            goto L12110

L12470:     for fieldnr%= 3% to 4%
L12480:     gosub'202(screenline%,fieldnr%)
                if fieldnr% = 0% and keyhit% = 16% then cont_edit_kit
                if keyhit% =  1% then gosub startover
                if keyhit% <> 0% then L12480
            gosub'153(c%,fieldnr%)
            if errormsg$ <> " " then L12480
            next fieldnr%
            goto L12110

        insertline1
            if maxlines% > pok% - 2% then cont_edit_kit
            screenline% = min(13%, max(cursor%(1%)-6%,1%))
            if screenline% < 1% or screenline% > 13% then cont_edit_kit
L12600:     c% = min(screenline% + pbas%, maxlines% + 1%)
            if screenline% > 12% then pbas% = max(0%, c% - 12%)
            if screenline% > 12% then screenline% = min(12%,c%)
            if c% <= maxlines% then L12680
            screenline% = c% - pbas%
            if c% > pok% - 2% then cont_edit_kit
            goto L12790

L12680:     for i%=maxlines% to c% step -1
                part$      (i%+1%) = part$       (i%)
                partdescr$ (i%+1%) = partdescr$  (i%)
                quantity$  (i%+1%) = quantity$   (i%)
                outdate$   (i%+1%) = outdate$    (i%)
                quantity   (i%+1%) = quantity    (i%)
                outdate%   (i%+1%) = outdate%    (i%)
                newquantity(i%+1%) = newquantity (i%)
                newdate%   (i%+1%) = newdate%    (i%)
            next i%

L12790:         init (" ") part$(c%), partdescr$(c%), quantity$(c%),     ~
                           outdate$(c%)
                newdate%(c%), outdate%(c%)    = 0%
                newquantity(c%), quantity(c%) = 0

            gosub input_kit_list
            if maxlines% > pok% - 2% then cont_edit_kit
            if keyhit% = 16% then L12930
            screenline% = screenline% + 1%
            if screenline% < 13% then L12600
                pbas% = pbas% + 12%
                screenline% = 1%
            goto L12600

L12930:     if c% > maxlines% then cont_edit_kit
            for i% = c% to maxlines%
                part$       (i%)  = part$       (i%+1%)
                partdescr$  (i%)  = partdescr$  (i%+1%)
                quantity$   (i%)  = quantity$   (i%+1%)
                outdate$    (i%)  = outdate$    (i%+1%)
                quantity    (i%)  = quantity    (i%+1%)
                outdate%    (i%)  = outdate%    (i%+1%)
                newquantity (i%)  = newquantity (i%+1%)
                newdate%    (i%)  = newdate%    (i%+1%)
            next i%
                init (" ") part$(i%+1%), partdescr$(i%+1%),              ~
                           outdate$(i%+1%), quantity$(i%+1%)
                newdate%(i%+1%), outdate%(i%+1%)    = 0%
                newquantity(i%+1%), quantity(i%+1%) = 0
                goto cont_edit_kit

        deleteline1
            gosub'201(screenline%,5%)
                if keyhit% <> 0% then cont_edit_kit
            quantity$(c%) = " "
            gosub'153(c%,3%)
            goto cont_edit_kit

        delete_all1
                keyhit% = 2%
                ask$()  = " "
                ask$(1%) = hex(8c) &                                     ~
            "Press RETURN to clear all lines for re-entry or deletion" & ~
                          hex(84)
                ask$(2%) = hex(8c) &                                     ~
            "Note that NOTHING is permanently changed for the order " &  ~
            "until SAVED" & hex(84)
                ask$(3%) = "Press Any PF Key To Return To Edit"

                call "ASKUSER" (keyhit%, "Delete it ?",                  ~
                                ask$(1%), ask$(2%), ask$(3%))

                if keyhit% <> 0% then return
                pipouts_changed% = 1%
                init (" ") part$(), partdescr$(), quantity$(), outdate$()
                mat newdate%    = zer
                mat outdate%    = zer
                mat newquantity = zer
                mat quantity    = zer
                maxlines%, pbas% = 0%
                return


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after input/editing.                   *~
            *************************************************************

          datasave
            gosub pip_delete_work    /* Clean out Work File */
            gosub pip_save_work      /* Save Kit List */
            end

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
            *-----------------------------------------------------------*~
            * Gives the user the ability to start over when he wants to *~
            * else return to the menu.  Notice that he has to push 2    *~
            * different buttons to start over--a little harder.         *~
            *************************************************************

        startover
            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return
            return clear all
            kitmode% = 99%
            end

        REM *************************************************************~
            *                     D A T A   L O A D                     *~
            *-----------------------------------------------------------*~
            * Load old job from file, if found                          *~
            *************************************************************

        REM *************************************************************~
            *        Read data from WORKFILE  (present kit list)        *~
            *-----------------------------------------------------------*~
            * Format data for display and edit.                         *~
            *************************************************************

        pip_load_work
            maxlines% = 0%
            outtagnr$ = pipkey$

            call "SHOSTAT" ("Loading Kit List")

L32090:     call "PLOWNEXT" (#8, outtagnr$, 19%, f1%(8%))
                if f1%(8%) = 0% then L32350

            i%, maxlines% = maxlines% + 1%
            if picksl_order$ = "B" then L32300
            get #8% using L32260, part$(i%), outdate%(i%), quantity(i%)
L32260:         FMT XX(19), CH(25), BI(4), XX(8), PD(14,4)
            gosub format_pip_data
            goto L32090

L32300:     get #8 using L32320, str(partdescr$(i%),9%,29%),              ~
                     str(partdescr$(i%),,8%), str(partdescr$(i%),38%,8%)
L32320:         FMT XX(19), CH(29), CH(8), CH(8)

            goto L32090

L32350: REM Sort Pick List Into BOM Order via Time Stamp...
            if picksl_order$ <> "B" or maxlines% = 0% then return
            call "SORT" addr(str(partdescr$(),1%), i%, 45%,              ~
                                       str(partdescr$(),1%), 1%, 8%, "A")
            for i% = 1% to maxlines%
                get partdescr$(i%), using L32420, part$(i%), outdate%(i%),~
                                            quantity(i%)
L32420:         FMT XX(8), CH(25), BI(4), PD(14,4)
                gosub format_pip_data
            next i%

            return

        format_pip_data
            call "DATE" addr("G+", bdate$, outdate%(i%)-1%,              ~
                                                str(outdate$(i%),,6), 0%)
            call "DATEFMT" (outdate$(i%))
            call "DESCRIBE" (#2, part$(i%), partdescr$(i%), 0%, f1%(2%))
            call "CONVERT" (quantity(i%), 0.2, quantity$(i%))
            newdate%(i%)    = outdate%(i%)
            newquantity(i%) = quantity(i%)
            return

        pip_transfer_work
            if mod(pipload%,10%) = 0% then return
            if pipkey$ = " "  then return

            outtagnr$ = str(pipkey$)
            init (hex(00))  str(outtagnr$,20%)

L32670:     call "PLOWNEXT" (#1, outtagnr$, 19%, f1%(1%))
                if f1%(1%) = 0% then L32750
            get #1 using L32695, pipdata$
L32695:         FMT CH(64)

            write #8 using L32720, pipworkkey$, str(pipdata$,20%),        ~
                                                           eod goto L32760
L32720:         FMT CH(19), CH(45)
            goto L32670

L32750:     pipload% = pipload% + 1%
L32760:     return

        pip_delete_work
            outtagnr$ = str(pipkey$)
            init (hex(00))  str(outtagnr$,20%)

            call "DELETE" (#8, outtagnr$, 19%)

            return

        pip_save_work
            if maxlines% = 0% then return

            for i% = 1% to maxlines%
L32940:         systime$ = time
                write #8 using L32980, pipkey$, part$(i%), newdate%(i%),  ~
                                      systime$, newquantity(i%), eod     ~
                                      goto L32940
L32980:              FMT CH(19), CH(25), BI(4), CH(8), PD(14,4)
            next i%
            return

        REM *************************************************************~
            *                 P L O W   R O U T I N E                   *~
            *        I F   N O   K I T   L I S T   O N   F I L E        *~
            *-----------------------------------------------------------*~
            * Plows through the bill of materials for the assembly part *~
            * number and 'PIPOUTS'                                      *~
            *************************************************************

        load_bom
            readkey$, copypart$ = part$ : copybomid$ = bomid$
            if maxlines% = 0% then str(readkey$,26%,3%) = bomid$
            incl(1%) = 0
            hdr$(2%) = "  Part Assemblies             Part Descriptions"
            hdr$(1%) = "  Existing BOMs For Part.  Use PF-1 To Select Ano~
        ~ther Part."
            hdr$(3%) = hex(ac) & "Select the Assembly Part And/Or BOM To ~
        ~Copy From.  Use PF-16 to Cancel Copy."
            outtagnr$ = hex(06) & "Select the Part Assembly"
            REM *** Get Part & BOMID To Copy ***
            errormsg$ = hex(84) & "Copy Request Cancelled"
            call "PLOWCODE" (#3,readkey$, outtagnr$, 8025%, -.30,        ~
              f1%(3%), hdr$(), 3.32, 57, incl(), header$(), " ", " ", #2)
                if f1%(3%) = 0% then return
            copypart$  = readkey$
            copybomid$ = str(readkey$,26%,3%)
            init (hex(00)) readkey$

            phfact(1%), ph% = 1%
            call "SHOSTAT" ("Loading BOM Structure")
            errormsg$ = "  "
            readkey$ = str(copypart$,,25%) & str(copybomid$,,3%) & "  0"
L34130:     call "PLOWNEXT" (#3, readkey$, 28%, f1%(3%))
                if f1%(3%) = 0% then return
            if maxlines% > pok%-2% then return
            maxlines% = maxlines% + 1%
            get #3, using L34180, part$(maxlines%), qu, tu, fx, ov, mkr$, ~
                                 bom$, stp$
L34180:     FMT CH(25), XX(31), 4*PD(14,4), CH(2), XX(1), CH(3), XX(4),  ~
                            CH(4)
            if mkr$ = "SP" or mkr$ = "RE" then L34740 /* skip these */
            if part$(maxlines%) = part$ then L34740   /* skip redundancy */
            call "READ100" (#2,part$(maxlines%),f1%(2%))
              get #2, using L34220, partdescr$(maxlines%), parttype$
L34220:             FMT XX(25), CH(32), XX(122), CH(3)
                convert parttype$ to type%, data goto L34740
            if type% > 0% and type% < 200% then L34740
            ext = qu*tu+ov
            if type% > 199% and type% < 500% then L34290
            if str(mkr$,,1%) = "P" then L34380
L34290:     quantity(maxlines%), newquantity(maxlines%) =                ~
                                      round(ext*qtymake*phfact(ph%)+fx,2)
            call "CONVERT" (quantity(maxlines%),0.2,quantity$(maxlines%))
            newdate%(maxlines%), outdate%(maxlines%) = orddate%
            outdate$(maxlines%) = plstdate$
            pipouts_changed% = 1%
            if type% > 489% and type% < 500% then L34590
            if type% > 789% and type% < 800% then L34590
          goto L34130

L34380
*        PHANTOM PROCESSOR
            if ph% > 100% then L34740
            readkey$(ph%)=readkey$
            ph% = ph% + 1%
            phfact(ph%) = qu * tu * phfact(ph%-1%)
            if bom$ <> " " then L34520
                if orddate% = 0% then L34740
                outtagnr$ = str(part$(maxlines%),,25%) & "1" & hex(00)
                call "PLOWNEXT" (#4, outtagnr$, 26%, f1%(4%))
                    if f1%(4%) = 0% then L34740
                       gosub get_bom_string

                    if bom$(orddate%) = " " then L34740
                bom$ = bom$(orddate%)
L34520:     readkey$ = str(part$(maxlines%),,25%) & str(bom$,,3%) & "  0"

            gosub L34740
            ph% = ph% - 1%
            readkey$ = readkey$(ph%)
            goto L34130

L34590
*        TOOL PROCESSOR
            if maxlines% > pok%-2% then L34740
            maxlines% = maxlines% + 1%
            part$(maxlines%) = part$(maxlines%-1%)
            partdescr$(maxlines%) = str(partdescr$(maxlines%-1%),,20%) & ~
                                                           ":TOOL RETURN"
            quantity(maxlines%), newquantity(maxlines%) =                ~
                                                  -quantity(maxlines%-1%)
            call "CONVERT" (quantity(maxlines%),0.2,quantity$(maxlines%))
            newdate%(maxlines%), outdate%(maxlines%) = tooldate%
            call "DATE" addr("G+", bdate$, tooldate% - 1%,               ~
                                          str(outdate$(maxlines%),,6%),0%)
            call "DATEFMT" (outdate$(maxlines%))
            goto L34130

L34740:   init (" ") part$(maxlines%), partdescr$(maxlines%),            ~
                                quantity$(maxlines%), outdate$(maxlines%)
          newdate%(maxlines%), outdate%(maxlines%)    = 0%
          quantity(maxlines%), newquantity(maxlines%) = 0
          maxlines% = maxlines% - 1%
          goto L34130

            FMT CH(25),                  /* COMPONENT PART NUMBER      */~
                CH(25),                  /* ASSEMBLY PART NUMBER       */~
                CH(3),                   /* WHICH BOM STRUCTURE?       */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                PD(14,4),                /* QUANTITY REQUIRED          */~
                PD(14,4),                /* TIMES USED (SIZE)          */~
                CH(2),                   /* BOM MARKER                 */~
                CH(21),                  /* DESCRIPTION                */~
                CH(3),                   /* WHICH RTE STRUCTURE?       */~
                BI(2),                   /* RTE STEP NUMBER            */~
                CH(3),                   /* COMPONENTS SPEC BOM        */~
                CH(47)                   /* FILLER                     */

        get_bom_string
            get #4, using L34960, bom$()
L34960:       FMT XX(29), 490 * CH(3)
            return


        REM *************************************************************~
            *  Data save section - update pip with new info             *~
            *-----------------------------------------------------------*~
            * Replaces old with new if needed.                          *~
            *************************************************************

        update_pips
            outtagnr$ = str(pipkey$)
                call "SHOSTAT" ("Updating KIT List")

            init (hex(00)) str(outtagnr$,20)
L37110:     call "PLOWNXT1" (#1, outtagnr$, 19%, f1%(1%))
                if f1%(1%) = 0% then L37210
            get #1 using L37150, part$(pok%), outdate%(pok%),             ~
                                                           quantity(pok%)
L37150:         FMT XX(19), CH(25), BI(4), XX(8), PD(14,4)
            delete #1
            call "PIPFLAGS" (part$(pok%), 1%, outdate%(pok%),            ~
                                                  quantity(pok%), #5, #6)
            goto L37110

L37210:     outtagnr$ = str(pipkey$)
            init (hex(00)) str(outtagnr$,20)

L37240:     call "PLOWNXT1" (#8, outtagnr$, 19%, f1%(8%))
                if f1%(8%) = 0% then L37390
            get #8 using L37280, part$(pok%), outdate%(pok%),             ~
                                                           quantity(pok%)
L37280:         FMT XX(19), CH(25), BI(4), XX(8), PD(14,4)

            delete #8
            if abs(quantity(pok%)) < .0001 then L37240
            temp = round(quantity(pok%),2%)
L37320:     systime$ = time
            write #1 using L37420, outtagnr$, part$(pok%), outdate%(pok%),~
                                           systime$, temp, eod goto L37320

            call "PIPFLAGS" (part$(pok%), 1%, outdate%(pok%), -temp, #5, ~
                                                                      #6)
            goto L37240

L37390:     return


L37420: FMT                      /* FILE: PIPOUT                       */~
            CH(19),              /* Tag number                         */~
            CH(25),              /* Part code                          */~
            BI(4),               /* Date out of PIP in date subscript  */~
            CH(8),               /* Time from the system clock         */~
            PD(14,4)             /* Quantity                           */~

        REM *************************************************************~
            *           T E S T   A U T H O R I Z A T I O N             *~
            *-----------------------------------------------------------*~
            * See if user can access the indicated part.                *~
            *************************************************************

        check_for_restrictions
            errormsg$ = " "
*          IF ADMIN% = 1% THEN RETURN
            call "READ100" (#2, part$, f1%(2%))
            if f1%(2%) = 0% then return
                get #2, using L39120, partclass$
L39120:              FMT POS(309), CH(3)
                search codes$() = str(partclass$,,3) to cursor%() step 3
                if cursor%(1) <> 0% then return
                     errormsg$ = "Sorry, you're not a valid Production" &~
                                 " Scheduler for this Part.  Part is"   &~
                                 " class " & partclass$
                     return

        REM *************************************************************~
            *            K I T L I S T  S C R E E N                     *~
            *-----------------------------------------------------------*~
            * Handles input and edit of kitlist (PIPOUTS)               *~
            *************************************************************

            deffn'202(screenline%,fieldnr%)
                  screen%=2%
                  str(pfkeys$(2),18) = hex(0e)
                  screen$(2,3)="(3)Last Lines   (14)Append BOM To List   ~
        ~      (28)Delete All  (16)Save BOM   "
                  if part$(c%) = " " then fieldnr% = 0%
                  if fieldnr%<>3 then L45130
                     convert quantity$(c%) to temp, data goto L45130
                     call "CONVERT" (temp,-0.2,quantity$(c%))
L45130:           init (hex(86)) lfac$()
                  if fieldnr% = 0% then  goto L45290  else goto L45280

            deffn'201(screenline%,fieldnr%)
                  screen$(1,1)="(1)Start Over      (14)Append BOM To List~
        ~                      (13)Instructions"
                  screen$(1,3)="(2)Start Line Over  (6)Same As Line Above~
        ~                      (16)Edit Mode"
                  str(pfkeys$(1),18) = hex(0e)

*                IF POS(PART$() <> " ") <> 0 THEN 45230 ELSE 45270
*
*                SCREEN$(1,1)="(1)Start Over                            ~
*                            (13)Instructions"
*                STR(PFKEYS$(1),18) = HEX(FF)

                  screen%=1%
L45280:           init(hex(8c)) lfac$()
L45290:           line2$  = "Purchase Part : " & part$ &" - "& partdescr$
                  header$="Seq. Part #                    Description    ~
        ~               Quantity   Date"
                  on fieldnr% gosub L45420,         /* PART             */~
                                    L45420,         /* DESCRIPTION      */~
                                    L45450,         /* QUANTITY         */~
                                    L45420,         /* DATE OUT         */~
                                    L45480          /* SPEC DELETE      */
                     goto L45530

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(screenline%,fieldnr%) = hex(80)
                      return
L45420:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(screenline%,fieldnr%) = hex(81)
                      return
L45450:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(screenline%,fieldnr%) = hex(82)
                      return
L45480:           REM SET BLINKING FAC FOR QUANTITY
                      init (hex(94)) str(lfac$(),7*screenline%-6%,7)
                      str(screen$(1,1),,25) = "(1)Cancel Delete Request"
                      screen$(1,3) = "(RETURN)Delete Item"
                      return

L45530:     if fieldnr% <> 0% then L45630
                for wph% = 1% to 12%
                     if quantity$(wph%+pbas%) = " " then L45610
                     convert quantity$(wph%+pbas%) to wph,data goto L45610
                     if wph <> 0 then L45610
                          for wphf% = 1% to 4%
                               lfac$(wph%, wphf%) = hex(8c)
                          next wphf%
L45610:         next wph%
L45630:     accept                                                       ~
               at (01,02), "Manage Purchase Job's Kit (Materials) List", ~
               at (01,59), "Today's Date:", fac(hex(8c)), date$ , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), "Vendor",                                     ~
               at (04,19), fac(hex(84)),   vencode$             , ch(09),~
               at (04,46), fac(hex(8c)),   vendescr$            , ch(32),~
               at (05,02), "Vendor Part",                                ~
               at (05,19), fac(hex(84)),   venpart$             , ch(25),~
               at (05,46), "Quantity to Buy ",                           ~
               at (05,65), fac(hex(84)), qtymake$               , ch(10),~
               at (06,02), fac(hex(94)), errormsg$              , ch(79),~
               at (07,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
                                                                         ~
               at (08,01), fac(hex(8c)),      seq$  (01%+pbas%) , ch(05),~
               at (09,01), fac(hex(8c)),      seq$  (02%+pbas%) , ch(05),~
               at (10,01), fac(hex(8c)),      seq$  (03%+pbas%) , ch(05),~
               at (11,01), fac(hex(8c)),      seq$  (04%+pbas%) , ch(05),~
               at (12,01), fac(hex(8c)),      seq$  (05%+pbas%) , ch(05),~
               at (13,01), fac(hex(8c)),      seq$  (06%+pbas%) , ch(05),~
               at (14,01), fac(hex(8c)),      seq$  (07%+pbas%) , ch(05),~
               at (15,01), fac(hex(8c)),      seq$  (08%+pbas%) , ch(05),~
               at (16,01), fac(hex(8c)),      seq$  (09%+pbas%) , ch(05),~
               at (17,01), fac(hex(8c)),      seq$  (10%+pbas%) , ch(05),~
               at (18,01), fac(hex(8c)),      seq$  (11%+pbas%) , ch(05),~
               at (19,01), fac(hex(8c)),      seq$  (12%+pbas%) , ch(05),~
                                                                         ~
               at (08,07), fac(lfac$( 1, 1)), part$( 1%+pbas%)  , ch(25),~
               at (09,07), fac(lfac$( 2, 1)), part$( 2%+pbas%)  , ch(25),~
               at (10,07), fac(lfac$( 3, 1)), part$( 3%+pbas%)  , ch(25),~
               at (11,07), fac(lfac$( 4, 1)), part$( 4%+pbas%)  , ch(25),~
               at (12,07), fac(lfac$( 5, 1)), part$( 5%+pbas%)  , ch(25),~
               at (13,07), fac(lfac$( 6, 1)), part$( 6%+pbas%)  , ch(25),~
               at (14,07), fac(lfac$( 7, 1)), part$( 7%+pbas%)  , ch(25),~
               at (15,07), fac(lfac$( 8, 1)), part$( 8%+pbas%)  , ch(25),~
               at (16,07), fac(lfac$( 9, 1)), part$( 9%+pbas%)  , ch(25),~
               at (17,07), fac(lfac$(10, 1)), part$(10%+pbas%)  , ch(25),~
               at (18,07), fac(lfac$(11, 1)), part$(11%+pbas%)  , ch(25),~
               at (19,07), fac(lfac$(12, 1)), part$(12%+pbas%)  , ch(25),~
                                                                         ~
               at (08,33), fac(lfac$( 1, 2)),partdescr$( 1+pbas%),ch(27),~
               at (09,33), fac(lfac$( 2, 2)),partdescr$( 2+pbas%),ch(27),~
               at (10,33), fac(lfac$( 3, 2)),partdescr$( 3+pbas%),ch(27),~
               at (11,33), fac(lfac$( 4, 2)),partdescr$( 4+pbas%),ch(27),~
               at (12,33), fac(lfac$( 5, 2)),partdescr$( 5+pbas%),ch(27),~
               at (13,33), fac(lfac$( 6, 2)),partdescr$( 6+pbas%),ch(27),~
               at (14,33), fac(lfac$( 7, 2)),partdescr$( 7+pbas%),ch(27),~
               at (15,33), fac(lfac$( 8, 2)),partdescr$( 8+pbas%),ch(27),~
               at (16,33), fac(lfac$( 9, 2)),partdescr$( 9+pbas%),ch(27),~
               at (17,33), fac(lfac$(10, 2)),partdescr$(10+pbas%),ch(27),~
               at (18,33), fac(lfac$(11, 2)),partdescr$(11+pbas%),ch(27),~
               at (19,33), fac(lfac$(12, 2)),partdescr$(12+pbas%),ch(27),~
                                                                         ~
               at (08,61), fac(lfac$( 1, 3)), quantity$( 1+pbas%),ch(10),~
               at (09,61), fac(lfac$( 2, 3)), quantity$( 2+pbas%),ch(10),~
               at (10,61), fac(lfac$( 3, 3)), quantity$( 3+pbas%),ch(10),~
               at (11,61), fac(lfac$( 4, 3)), quantity$( 4+pbas%),ch(10),~
               at (12,61), fac(lfac$( 5, 3)), quantity$( 5+pbas%),ch(10),~
               at (13,61), fac(lfac$( 6, 3)), quantity$( 6+pbas%),ch(10),~
               at (14,61), fac(lfac$( 7, 3)), quantity$( 7+pbas%),ch(10),~
               at (15,61), fac(lfac$( 8, 3)), quantity$( 8+pbas%),ch(10),~
               at (16,61), fac(lfac$( 9, 3)), quantity$( 9+pbas%),ch(10),~
               at (17,61), fac(lfac$(10, 3)), quantity$(10+pbas%),ch(10),~
               at (18,61), fac(lfac$(11, 3)), quantity$(11+pbas%),ch(10),~
               at (19,61), fac(lfac$(12, 3)), quantity$(12+pbas%),ch(10),~
                                                                         ~
               at (08,72), fac(lfac$( 1, 4)), outdate$ ( 1+pbas%),ch(08),~
               at (09,72), fac(lfac$( 2, 4)), outdate$ ( 2+pbas%),ch(08),~
               at (10,72), fac(lfac$( 3, 4)), outdate$ ( 3+pbas%),ch(08),~
               at (11,72), fac(lfac$( 4, 4)), outdate$ ( 4+pbas%),ch(08),~
               at (12,72), fac(lfac$( 5, 4)), outdate$ ( 5+pbas%),ch(08),~
               at (13,72), fac(lfac$( 6, 4)), outdate$ ( 6+pbas%),ch(08),~
               at (14,72), fac(lfac$( 7, 4)), outdate$ ( 7+pbas%),ch(08),~
               at (15,72), fac(lfac$( 8, 4)), outdate$ ( 8+pbas%),ch(08),~
               at (16,72), fac(lfac$( 9, 4)), outdate$ ( 9+pbas%),ch(08),~
               at (17,72), fac(lfac$(10, 4)), outdate$ (10+pbas%),ch(08),~
               at (18,72), fac(lfac$(11, 4)), outdate$ (11+pbas%),ch(08),~
               at (19,72), fac(lfac$(12, 4)), outdate$ (12+pbas%),ch(08),~
                                                                         ~
               at (21,02), fac(hex(ac)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), screen$(screen%,1)     , ch(79),~
               at (23,02), fac(hex(8c)), screen$(screen%,2)     , ch(79),~
               at (24,02), fac(hex(8c)), screen$(screen%,3)     , ch(79),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)

               if keyhit% <> 13% then L46540
                  call "MANUAL" ("PJINPSUB")
                  goto L45530

L46540:        if keyhit% <> 15% then L46580
                  call "PRNTSCRN"
                  goto L45530

L46580:        if screen%=1% then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return




*        Test data for PART NUMBER
            call "GETCODE" (#2, part$, partdescr$, 1%, .32, f1%(2%))
            if f1%(2%) <> 0% then L50500
                errormsg$ = "Part Not On File: " & part$
                return
L50500:     gosub check_for_restrictions
            if errormsg$ <> " " then return
                if jbdescr$ = " " then get #2, using L50530, jbdescr$
L50530:              FMT XX(25), CH(32)
                if wipsave$ <> " " then return
                     readkey$ = part$
                     call "READ100" (#2, readkey$, f1%(2%))
                     if f1%(2%) = 1% then L50600
L50580:                   wipsave$ = " "
                          return
L50600:              get #2, using L50610, parttype$, wipsave$
L50610:                   FMT POS(180), CH(3), POS(344), CH(9)
                     call "GLFMT" (wipsave$)
                     convert parttype$ to typework%, data go to L50580
                     if typework% > 499% then L50670
                          wipsave$ = " "
                          return
L50670:              call "GETCODE" (#6, wipsave$, wipdescr$,            ~
                                                         1%, 99, f1%(6))
                     return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Tests data for the kitting line items                     *~
            *************************************************************

            deffn'153(c%,fieldnr%)
            pipouts_changed% = 1%
            errormsg$=" "
            on fieldnr% gosub L53150,               /* PART             */~
                              L53330,               /* DESCRIPTION      */~
                              L53360,               /* QUANTITY         */~
                              L53410                /* DATE OUT         */
            return

L53150: REM TEST DATA FOR PART
            if part$(c%) <> part$ then L53190
                errormsg$="Cannot Commit Part To Build For Jobs"
                return
L53190:     partdescr$(c%) = hex(06) & "Select the Part to Kit"
            call "GETCODE" (#2, part$(c%),partdescr$(c%),0%,.32,f1%(2%))
            if f1%(2%) <> 0% then L53260
                errormsg$ = "Part Not On File"
                return
L53260:     get #2, using L53270, type$
L53270:         FMT POS(180), CH(3)
            convert type$ to type%, data goto L53300
            if type% = 0% or type% > 199% then return
L53300:         errormsg$="Not A Planned Part... Type=" & type$
                return

L53330: REM TEST DATA FOR DESCRIPTION
            return

L53360: REM TEST DATA FOR QUANTITY
            call "NUMTEST" (quantity$(c%),-9e7,9e7, errormsg$,-0.2,temp)
            if errormsg$ = " " then newquantity(c%) = temp
            return

L53410: REM TEST DATA FOR DATE
            call "DATEOK" (outdate$(c%), u3%, errormsg$)
                if errormsg$<>" " then return
            lastdate$ = outdate$(c%) /* DEFAULT FOR NEXT LINE OM INPUT */
            temp$ = outdate$(c%)
            call "DATUNFMT" (temp$)
            call "PIPINDEX" (#7, temp$, newdate%(c%), u3%)
                if u3% = 0% then return
                errormsg$ = "Date is outside planning Calendar"
                return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
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

