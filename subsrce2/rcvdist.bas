        REM *************************************************************~
            *                                                           *~
            *  RRRR    CCC   V   V  DDDD   IIIII   SSS   TTTTT          *~
            *  R   R  C   C  V   V  D   D    I    S        T            *~
            *  RRRR   C      V   V  D   D    I     SSS     T            *~
            *  R   R  C   C   V V   D   D    I        S    T            *~
            *  R   R   CCC     V    DDDD   IIIII   SSS     T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RCVDIST  - DISTRIBUTE QUANTITIES RECEIVED TO THEIR        *~
            *            FINAL RESTING PLACE, AT LEAST AS FAR AS QC/RCV *~
            *            IS CONCERNED.  RUNS IN EITHER QC DIST. OR RCV  *~
            *            HOLD DISTRIBUTION.  DUAL MODES MAKE THE CODE   *~
            *            A LITTLE TRICKY.                               *~
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
            * 05/06/86 ! ORIGINAL                                 ! KAB *~
            * 10/31/86 ! Added PF3 Option                         ! HDC *~
            * 02/09/87 ! Ehanced Lot Tracking                     ! ERN *~
            * 05/19/87 ! Standard Costing Enhancements            ! ERN *~
            * 02/24/88 ! Added Enhanced Access To Inv & ATC       ! MDE *~
            * 03/11/88 ! Added Receiver Ticket Printing           ! MDE *~
            * 01/04/89 ! Added Rejection Codes Required Flag      ! MJB *~
            * 06/26/91 ! Added PF24 access to HNYLCSUB permitting ! MLJ *~
            *          !  Location Management.  Also documented   !     *~
            *          !  file channels used in program.          !     *~
            * 07/19/96 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "RCVDIST" (record$(), lines$(), maxlines%, subh$(), mode%,   ~
                       #19, #20, #21, #22, #2, #3, #40, #41, #42,        ~
                       #51, #52, #23, #4, #5, #1,                        ~
                       return%, rcvhnyds%(), rcvhnyds$(), maxdist%,      ~
                       parts$(), rejlot$(), rej%,                        ~
                       bufflot$(), buffstr$(), buffqty(),                ~
                       postlot$(), poststr$(), postqty(),                ~
                       buffidx%(), postidx%(), buffmax%, postmax%,       ~
                       delvr$, reqstd$, #13, #14)

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            buffidx%(1),                 /* Held index                 */~
            bufflot$(1)6,                /* Held Lot                   */~
            buffstr$(1)6,                /* Held Stores                */~
            buffqty(1),                  /* Held Stores                */~
            date$8,                      /* System Date For Display    */~
            deflot$6,                    /* Default Target Lot Number  */~
            defstr$3,                    /* Default Target Store Number*/~
            cursor%(2),                  /* Cursor Location            */~
            errormsg$79,                 /* Error Message Text         */~
            header$79,                   /* Screen Header              */~
            i$(24)80,                    /* Screen Image (Not Used)    */~
            index%(100),                 /* Ser number indexes         */~
            inprocess$(2)30,             /* In Process Message         */~
            item$(99)3,                  /* Reference Number For Disply*/~
            job$8,                       /* Job Number                 */~
            lfac$(20)1,                  /* FACs For Input             */~
            lfac1$(20)1, lfac2$(20)1,    /* FACs For Input             */~
            lines$(100)50,               /* Distribution Lines         */~
            lot_sys$1, lot_unique$1,     /* Lot System Flags           */~
            message$79,                  /* Instructions For Input     */~
            mode$(2)16,                  /* Mode of operation (header) */~
            note$14,                     /* Screen Message             */~
            ohmsg$50,                    /* On Hand Message            */~
            ohass$(100)9,                /* On Hand Asset Accounts     */~
            ohdate$(100)6,               /* On Hand Dates Moved        */~
            ohlot$(100)6,                /* On Hand Lots               */~
            ohstr$(100)3,                /* On Hand Stores             */~
            ohvenlot$(100)9,             /* On Hand Vendor's Lot       */~
            part$25,                     /* Part Number                */~
            partdescr$34,                /* Part Description           */~
            pfdescr$(3)79,               /* PF Text For Display        */~
            pfkeys$16,                   /* PF Actual values           */~
            pgmname$8,                   /* Calling Program Name       */~
            poline$3,                    /* Purchase Order Line Number */~
            pono$16,                     /* Purchase Order Number      */~
            psno$16,                     /* Packing Slip Number        */~
            postidx%(1),                 /* Post Index                 */~
            postlot$(1)6,                /* Post Lot                   */~
            poststr$(1)6,                /* Post Stores                */~
            postqty(1),                  /* Post Quantity              */~
            qtyback(2),                  /* Quantity Returned To Vendor*/~
            qtyback$(2)10,               /* Quantity Returned To Vendor*/~
            qtyhold(3,2),                /* Quantities on Hold, RCV,   */~
            qtyhold$(2)10,               /*    QC Pending, QC Hold rsp.*/~
            qtyunds$10,                  /* Quantity To Distribute     */~
            qtyunav$10,                  /* Quantity Unavailable       */~
            qtyoh(100),                  /* Quantities Moved To On Hand*/~
            qtyoh$(100,2)10,             /* Quantities Moved To On Hand*/~
            qtyrcvd$10,                  /* Total Quantity Received    */~
            qtyscrp(2),                  /* Quantity Scrapped          */~
            qtyscrp$(2)10,               /* Quantity Scrapped          */~
            rcvdate$8,                   /* Date Of Receipt            */~
            rcvhnyds%(1),                /* Distr Array for PO         */~
            rcvhnyds$(1)50,              /*                            */~
            rcvno$16,                    /* Receiver Number            */~
            readkey$50,                  /* A Read Key                 */~
            record$(16)50,               /* Work Variable              */~
            parts$(100,2)25, rejlot$(100)6, /* For entire PO           */~
            rejcode$6,                   /* Current Rejection Code     */~
            rjflag$1,                    /* Rej Codes Req'd Flag       */~
            rejdescr$32,                 /* Current Rejection Descript.*/~
            scraplot$6,                  /* Scrap Lot                  */~
            scrapstr$3,                  /* Scrap Store                */~
            ser%(100),                   /* How Many Serials Avail     */~
            subh$(3)80,                  /* Subroutine Header Block    */~
            text$(84,1)70,               /* For TExt Subs              */~
            textid$(2)4,                 /* Text I. D.'S               */~
            texttype$(2)3,               /* Text Type                  */~
            textmsg$(2)16,               /* Text Message               */~
            tttle1$27,                   /* Screen Sub Title           */~
            tttle2$51,                   /* Screen Sub Title           */~
            unavail$(2)30,               /* Unavailable, this mode.    */~
            unknown$(2)30,               /* Unknowns Disp. Message     */~
            unitsmsg$80,                 /* Unit of measure message    */~
            vendor$9,                    /* Vendor Number              */~
            work$(2)150,                 /* Work Area                  */~
            workkey$100                  /* Work Area                  */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            f1% = 0%

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! USERINFO ! User Default Information File            *~
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 3 ! HNYQUAN  ! Inventory Costs & Quantity Master        *~
            * # 4 ! VBKMASTR ! Backlog Header File                      *~
            * # 5 ! VBKLINES ! Backlog Lines File                       *~
            * #13 ! HNYLOCNS ! Location Quantity Detail File (HNYLOCNS) *~
            * #14 ! LOCATION ! Location Master File (HNYLOCNS)          *~
            * #19 ! STORNAME ! Store Information File                   *~
            * #20 ! TXTFILE  ! Text File                                *~
            * #21 ! QCRTYPES ! Rejection Codes                          *~
            * #22 ! HNYMASTR ! Inventory Master File                    *~
            * #23 ! HNYPOOL  ! Inventory LIFO/FIFO Pool Records File    *~
            * #40 ! SERTIF   ! Addiotions Buffer for Inventory SN's     *~
            * #41 ! SERWORK  ! Temporary SN's Work File                 *~
            * #42 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #51 ! SERWKHLD ! Temporary SN's Work File (RCVDIST)       *~
            * #52 ! SERMHOLD ! Serial Number Master File Copy (RCVDIST) *~
            *************************************************************


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)
            tttle1$ = "Disposition"
            tttle2$ = "Current     Quantity To Add"
            for i% = 1 to 99
                convert i% to item$(i%), pic(##)
                str(item$(i%),3) = ")"
            next i%

            call "EXTRACT" addr("CF", pgmname$)
               if mode% = 1% then pgmname$ = "RCVDIST"
            return% = 99%

            mode$(1) = "in Receiver Hold"
            mode$(2) = "in Q. C."

            inprocess$(1) = "In Receiver Hold"
            inprocess$(2) = "Q. C. In Process"

            unknown$(1) = "Quantity to Q.C. in Process"
            unknown$(2) = "Q.C. Hold  (Awaiting Disp.)"

            unavail$(1) = "In Q. C. Hold"
            unavail$(2) = "In Rcvr. Hold"

            texttype$(1) = "005" /* RECEIVER TEXT */
            texttype$(2) = "006" /* Q. C.    TEXT */

            textmsg$ (1) = "(8)Receiver Text"
            textmsg$ (2) = "(8)QC Free Text"

            ohmsg$ = "   Accepted to On Hand      (Net:"

        if lot_unique$ <> " " then L10000
            lot_sys$, lot_unique$ = "N"
            readkey$ = "SWITCHS.HNY"
            call "READ100" (#2, readkey$, f1%)
            if f1% = 0% then L10000
                get #2 using L09440, lot_sys$, lot_unique$
L09440:              FMT POS(92), 2*CH(1)
                if lot_sys$ = "N" then lot_unique$ = "N"

            call "VBKSWTCH" ("REJCODES", rjflag$, rjflag, temp%)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        REM INPUTMODE
            errormsg$, message$, part$, partdescr$, rcvno$, qtyhold$(),  ~
            poline$, rcvdate$, qtyrcvd$, qtyunds$, qtyback$(), scrapstr$,~
            qtyscrp$(), qtyoh$(), ohstr$(), ohlot$(), scraplot$, pono$,  ~
            textid$(), text$(), defstr$, deflot$, psno$, vendor$,        ~
            ohass$(), ohdate$(), ohvenlot$() = " "

            mat qtyback = zer : mat qtyhold = zer : mat qtyscrp = zer
            mat qtyoh   = zer : mat index% = zer  : rjflag = 0
            editmode%, highfield%, fieldnr%, base% ,load%   = 0%

            gosub L30000
            if qtyhold(1% + mode%, 1) = 0 then editmode

L10530:     fieldnr% = min(fieldnr% + 1%, 9%)
                gosub'052(fieldnr%)
                      if enabled% = 0 then L10810
L10560:         gosub'202(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  3 then L10580
                         load% = 1%
                         goto L10810
L10580:               if keyhit%  =  8 then gosub edit_text
                      if keyhit% <>  4 then L10650
                         highfield% = max(fieldnr%, highfield%)
L10600:                  fieldnr% = max(1%, fieldnr%-1%)
                         gosub'052(fieldnr%)
                         if enabled% = 0 then L10600
                         goto L10560
L10650:               if keyhit%  = 16 then L10810
                      if keyhit% <>  0 then L10560
L10810:         gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10560
                      highfield% = max(fieldnr%, highfield%)
                      if fieldnr% < highfield% then L10840
                      if keyhit% = 16% then editmode
                      if load%   =  1% then load_qc_to_store
                      if load%   >  1% then editmode
                      if fieldnr% < 5% then L10530
                         if qtyoh$(test%, 1%) = " " then editmode
L10840:         if fieldnr% < 9% then L10530
                      base% = base% + 1%
                      if base%  > 94% then editmode
                      goto L10530

        load_qc_to_store
            if maxlines% >  99% then editmode
            base%=max(0%, maxlines%-4)
            fieldnr% = (maxlines% - base%) + 4%
            editmode% = 0%
            goto L10530

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************

        editmode

            editmode% = 1 : load% = 0%
            base%=max(0%,min(base%,maxlines%-4,94))
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Field And Press RETURN."
L11120:     gosub'202(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then base% = 0
                  if keyhit% <>  3 then L11150
                     load% = 1%
                     goto load_qc_to_store
L11150:           if keyhit%  =  4 then base% = base% - 4
                  if keyhit%  =  5 then base% = base% + 4
                  if keyhit%  =  8 then gosub edit_text
                  if keyhit%  = 11 then load_qc_to_store
                  base%=max(0%,min(base%,maxlines%-4%,94%))
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11120
            fieldnr% = cursor%(1) - 10
            if fieldnr% < 1 or fieldnr% > 9 then L11120
            if fieldnr% = 4 then L11120
            fieldnr% = min(fieldnr%, 5% + maxlines% - base%)

L11310:     gosub'052(fieldnr%)
                  if enabled% = 0 then editmode
L11330:     gosub'202(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11330
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L11330
            if fieldnr% <> 3 then editmode
            fieldnr% = fieldnr% + 1
            goto L11310

        edit_text
            errormsg$ = "Vendor: " & vendor$ & ", PART: " & part$
            errormsg$ = errormsg$ & " " & partdescr$
            call "TXTINSUB" (#20, 0%, texttype$(mode% + 1%), errormsg$,  ~
                                            textid$(mode% + 1%), text$())
            errormsg$ = " "
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave

            REM FIRST A LITTLE TEST ...
                if qtyhold(1%+mode%,1) >= 0 then L19100
                errormsg$ = "UNDISTRIBUTED QUANTITY CANNOT BE LESS THAN Z~
        ~ERO."
                   goto editmode  /* OOPS */

L19100:     REM NOW GO SAVE...
                gosub L31000
                return% = 0%
                goto L65000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 2 OF INPUT. *~
            *************************************************************

            deffn'052(fieldnr%)
                  enabled% = 1%
                  message$ = " "
                  on fieldnr% gosub L20200,         /* Q.C. Hold        */~
                                    L20260,         /* Scrap Quantity   */~
                                    L20290,         /* Return To Vendor */~
                                    L20195,         /* Never Happens    */~
                                    L20390,         /* Move To On Hand  */~
                                    L20480,         /* Move To On Hand  */~
                                    L20480,         /* Move To On Hand  */~
                                    L20480,         /* Move To On Hand  */~
                                    L20480          /* Move To On Hand  */
                     return

L20195:           enabled% = 0%:return

L20200:     REM DEFAULT/ENABLE FOR Q.C. HOLD
                message$ = "Enter Quantity To Place in QC"
                if mode% = 0% then message$ = message$ & " In Process."  ~
                              else message$ = message$ & " Hold."
                return

L20260:     REM DEFAULT/ENABLE FOR SCRAP QUANTITY
                message$ = "Enter Scrap Quantity & Store/Lot. Store can't~
        ~ be Numeric."
                return

L20290:     REM DEFAULT/ENABLE FOR REJECTED
                message$ = "Enter quantity to REJECT plus Rejection Cod"&~
                     "e, blanks, or '?' to see list"
                if rejcode$ <> " " then return
                call "READ100" (#22, part$, f1%)
                     if f1% = 0% then return
                get #22, using L20310, str(rejcode$,1,4)
L20310:             FMT POS(90), CH(4)
                return
L20390:     REM DEFAULT/ENABLE FOR MOVE TO ON HAND
                test% = (fieldnr%-4%) + base%
                if qtyoh$(test%,1) <> " " then L20450
                   ohstr$(test%) = defstr$
                   ohlot$(test%) = deflot$
                   qtyoh$(test%,1) = "0"
L20450:         goto L20510
L20480:     REM DEFAULT/ENABLE FOR MOVE TO ON HAND
                test% = (fieldnr%-4%) + base%
                if ohstr$(test%) = " " then ohstr$(test%) = defstr$
                if qtyoh$(test%,1) = " " then qtyoh$(test%,1) = "0"
L20510:         if qtyoh$(test%,2) <> " " then L20520
                   if load% = 0% then L20520
                      if qtyhold(1% + mode%, 1) <= 0 then L20516
                      call "CONVERT" (qtyhold(1% + mode%, 1), -0.4,      ~
                                                         qtyoh$(test%,2))
                      qtyohnet = qtyohnet + qtyhold(1% + mode%, 1)
L20516:               load% = 2%
L20520:         message$ = "Enter Store, Lot, Quantity, and Vendor's Lot ~
        ~to Move to Inventory."
                temp = 0:convert qtyoh$(test%,2) to temp, data goto L20532
L20532:         qtyohnet = qtyohnet - temp
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
L29590:     k% = 2%
            call "STARTOVR" (k%)
            on k%+1% goto L29660, L29640
            goto L29590

L29640:        REM RETURN TO DISPLAY.    (P.F. KEY 1)
                   return
L29660:        REM START OVER            (ENTER)
                   return% = 1%
                   return clear all

                   if maxlines% = 0% then L29880

                   for c% = 1% to maxlines%
                     if index%(c%) = 0% then L29860
                        init (hex(00)) workkey$
                        temp% = 0%
                        convert poline$ to temp%, data goto L29750
L29750:                 temp% = 10000%*temp% + index%(c%)
                        str(workkey$,1,3) = bin(temp%,3)

L29770:                 call "PLOWNXT1" (#41, workkey$, 3%, f1%)
                           if f1% = 0% then L29860
                        get #41, using L29810, str(work$(),26,20),        ~
                                              str(work$(), 1,25)
L29810:                     FMT POS(4), CH(20), CH(25)
                        delete #41
                        call "DELETE" (#42, str(work$(),1,45), 45%)
                        goto L29770

L29860:              next c%

L29880:              init (hex(00)) workkey$
                     call "PLOWNEXT" (#51, workkey$, 0%, f1%)
L29900:                 if f1% = 0% then L29972
                     get #51, using L29940, str(work$(),1,48)
                     write #41, using L29940, str(work$(),1,48),          ~
                                                         eod goto L29950
L29940:                  FMT CH(48)
L29950:              call "READNEXT" (#51, f1%)
                     goto L29900

L29972:              init (hex(00)) workkey$
                     call "PLOWNEXT" (#52, workkey$, 0%, f1%)
L29976:                 if f1% = 0% then L65000
                     get #52, using L29984, str(work$(),1,300)
                     write #42, using L29984, str(work$(),1,300),         ~
                                                         eod goto L29986
L29984:                  FMT CH(300)
L29986:              call "READNEXT" (#52, f1%)
                     goto L29976

L30000: REM *************************************************************~
            *             R E C A L L   O L D   D A T A                 *~
            *                                                           *~
            * LOADS EXISTING DATA FROM FILE                             *~
            *************************************************************

            get record$(), using L35070, part$, rcvno$, vendor$, pono$,   ~
                           poline$, partdescr$,                          ~
                           rcvdate$, psno$, qtyrcvd,                     ~
                           qtyhold(1,1), qtyhold(2,1), qtyhold(3,1),     ~
                           qtyscrp(1), qtyback(1), factor, meas$,        ~
                           defstr$, deflot$, scrapstr$, scraplot$,       ~
                           rejcode$, textid$(), index%, job$
            if index% > 2000000% then index% = 0%
            call "LOTENABL" (part$, lot%, ll%, #2, #22)
                if lot% = 0% then deflot$ = " "
            call "CONVERT" (qtyhold(2% + mode%,1), -0.4, qtyhold$(1))
            call "CONVERT" (qtyhold(1% + mode%,1), -0.4, qtyunds$)
            qtyunav = qtyhold(3% - (2%*mode%),1)
            call "CONVERT" (qtyunav, -0.4, qtyunav$)
            call "CONVERT" (qtyback(1), -0.4, qtyback$(1))
            call "CONVERT" (qtyscrp(1), -0.4, qtyscrp$(1))
            call "CONVERT" (qtyrcvd, -0.4, qtyrcvd$)
            call "GETCODE" (#21, rejcode$, rejdescr$, 1%, 99, f1%)
            call "STRING" addr("RJ", rejdescr$, 32%)
            unitsmsg$ = " "
            if factor = 1 then L30350
            unitsmsg$ = "(Ordered in units of " & meas$ & " at"
            call "CONVERT" (factor, -2.4,                                ~
                                  str(unitsmsg$, len(unitsmsg$) + 2, 10))
            unitsmsg$ = unitsmsg$ & " Stocking Units per Vendor's Unit)"
            header$ = "Distribution is in INTERNAL Units"

L30350:     qtyohnet = 0
            if maxlines% = 0% then L30700

            for c% = 1% to maxlines%
            get lines$(c%) using L30410, ohstr$(c%), ohlot$(c%), qtyoh,   ~
                       ohass$(c%), ohdate$(c%), ohvenlot$(c%),           ~
                       index%(c%), ser%(c%)
L30410:     FMT CH(3),CH(6),PD(14,4),CH(9),CH(6),CH(9), BI(3), BI(4)
            call "CONVERT" (qtyoh, -0.4, qtyoh$(c%,1))
            qtyohnet = qtyohnet + qtyoh
            if index%(c%) > 2000000% then index%(c%) = 0%
            index% = max(index%, index%(c%))
            if index%(c%) = 0% then L30680

            init (hex(00)) workkey$
                 temp% = 0%:convert poline$ to temp%, data goto L30462
L30462:          temp% = 10000%*temp% + index%(c%)
                 str(workkey$,1,3) = bin(temp%,3)

L30480:     call "PLOWNEXT" (#41, workkey$, 3%, f1%)
               if f1% = 0% then L30680

            if workfiles_created% <> 0% then L30560
               call "WORKOPEN" (#51, "IO   ", 100%, temp%)
               call "WORKOPEN" (#52, "IO   ", 100%, temp%)
               workfiles_created% = 1%

L30560:     get #41 using L30570, str(work$(),1,48)
L30570:         FMT CH(48)
            call "REDALT0" (#42, str(work$(),4,45), 1%, f1%)
               if f1% = 0% then L30480
            write #51, using L30570, str(work$(),1,48)
            get #42 using L30620, str(work$(),1,300)
L30620:         FMT CH(300)
            write #52 using L30620, str(work$(),1,300)
            goto L30480

L30680:     next c%

L30700:     str(ohmsg$,35) = " "
            call "CONVERT" (qtyohnet , -0.4, str(ohmsg$,35,10))
            str(ohmsg$,len(ohmsg$) + 1) = ")"

            return

L31000: REM *************************************************************~
            *              S A V E   N E W   D A T A                    *~
            *                                                           *~
            * SAVES DATA ENTERED ON FILE                                *~
            *************************************************************

            init (" ") lines$()
            qtyoht = 0

            if maxlines% = 0% then L31310
            l% = 1%
            for c% = 1 to maxlines%
                qtyoh, temp = 0
                convert qtyoh$(c%,1) to qtyoh, data goto L31170
L31170:         convert qtyoh$(c%,2) to temp, data goto L31180
L31180:         qtyoh = qtyoh + temp
                if qtyoh <> 0 then L31190
                   gosub serial_delete
                   goto L31270
L31190:         qtyoht = qtyoht + qtyoh
                if qtyoh$(c%,2) <> "0" then ohdate$(c%) = date
                if ohdate$(c%) = " " or ohdate$(c%) = blankdate$ ~
                   then ohdate$(c%) = date

                put lines$(l%) using L31255,                              ~
                                 ohstr$(c%), ohlot$(c%), qtyoh,          ~
                                 ohass$(c%), ohdate$(c%), ohvenlot$(c%), ~
                                 index%(c%), ser%(c%)

L31255:         FMT CH(3),CH(6),PD(14,4),CH(9),CH(6),CH(9), BI(3), BI(4)
                l% = l% + 1%
L31270:     next c%

                maxlines% = l% - 1%

L31310:     qtyhold(1%+mode%,1), qtyrcvd = 0
            qtyhold(2%+mode%,1) = qtyhold(2%+mode%,1)+qtyhold(2%+mode%,2)
            qtyback(1) = qtyback(1) + qtyback(2)
            qtyscrp(1) = qtyscrp(1) + qtyscrp(2)
            convert qtyunds$ to qtyhold(1%+mode%,1), data goto L31360
L31360:     convert qtyrcvd$ to qtyrcvd, data goto L31380

L31380:     put record$(), using L35670, part$, rcvno$, vendor$, pono$,   ~
                           poline$, rcvdate$, psno$, qtyrcvd,            ~
                           qtyhold(1,1), qtyhold(2,1), qtyhold(3,1),     ~
                           qtyscrp(1), qtyback(1), qtyoht, scrapstr$,    ~
                           scraplot$, rejcode$, textid$(), index%

            return

        serial_delete
            if index%(c%) = 0% then return
            temp% = 0%:convert poline$ to temp%, data goto L31490
L31490:     temp% = 10000%*temp% + index%(c%)
            call "SERSTOVR" (temp%, "7", "2", #42, #41)

            init (hex(00)) workkey$
            str(workkey$,1,3) = bin(temp%,3)

L31550:     call "PLOWNEXT" (#51, workkey$, 3%, f1%)
               if f1% = 0% then return
            get #51 using L31580, str(work$(),26,20), str(work$(), 1,25)
L31580:         FMT POS(4), CH(20), CH(25)
            call "DELETE" (#42, str(work$(),1,45), 45%)
            goto L31550

        REM *************************************************************~
            *          F O R M A T   S T A T E M E N T S                *~
            *-----------------------------------------------------------*~
            * Major Format Satatements for Receiver Files               *~
            *************************************************************

        REM FOR LINE ITEMS, TIF AND 'MASTER' FILES IDENTICAL
L35070:     FMT                          /* RECEIVER LINE ITEM         */~
                CH(25),                  /* Part Number                */~
                CH(16),                  /* Receiver Number            */~
                CH(9),                   /* Vendor Code                */~
                CH(16),                  /* P.O. Number (PIPIN Tag)    */~
                CH(3),                   /* P.O. Line Sequence Number  */~
                XX(8),                   /* Date/Time Stamp            */~
                CH(32),                  /* Part Description           */~
                XX(6),                   /* Date Ordered               */~
                XX(6),                   /* Date Due                   */~
                CH(6),                   /* Date Received              */~
                CH(16),                  /* Vendor's Packslip          */~
                XX(8),                   /* Date/Time Stamp            */~
                XX(4),                   /* P.O. level Receiver Text   */~
                PD(14,4),                /* Total Quantity Received    */~
                PD(14,4),                /*       Receiver Hold        */~
                PD(14,4),                /*       Q.C. Pending         */~
                PD(14,4),                /*       Q.C. Hold            */~
                PD(14,4),                /*       Scrapped/Reject      */~
                PD(14,4),                /*       Return to Vendor     */~
                XX(8),                   /*       Total to On-Hand     */~
                XX(8),                   /* Internal Unit Price (Mat)  */~
                XX(8),                   /* Internal Unit Price (Lab)  */~
                XX(8),                   /* Internal Unit Price (Ovhd) */~
                PD(14,4),                /* Conversion Factor          */~
                CH(4),                   /* Unit of Measure            */~
                XX(8),                   /* Extension (Receiver Value) */~
                XX(8),                   /* Open Amount                */~
                XX(8),                   /* A/P Invoiced Amount        */~
                XX(8),                   /* A/P Adjustment             */~
                XX(8),                   /* Scrap Adjustment           */~
                XX(8),                   /* Rework Adjustment          */~
                XX(9),                   /* (Pre) Payables Liab. Acct. */~
                XX(9),                   /* Receiver Holding Account   */~
                XX(9),                   /* Q.C. Holding Account       */~
                XX(9),                   /* Pur. Dist. (Exp) Account   */~
                CH(3),                   /* Default Store Target       */~
                CH(6),                   /* Default Lot Target         */~
                CH(3),                   /* Scrap Store                */~
                CH(6),                   /* Scrap Lot                  */~
                CH(6),                   /* Rejection Code             */~
                XX(32),                  /* Skip A/P Block             */~
                CH(4),                   /* Line level Receiver Text   */~
                CH(4),                   /* Q.C. Line Item Text        */~
                BI(4),                   /* Ser. Number High Index     */~
                CH(8)                    /* Job Number                 */~

            FMT                          /* RECEIVER INVENTORY DIST.   */~
                CH(16),                  /* Receiver Number            */~
                CH(9),                   /* Vendor code                */~
                CH(16),                  /* P.O. Number                */~
                CH(3),                   /* P.O. Line Sequence         */~
                CH(25),                  /* Part                       */~
                CH(3),                   /* Store                      */~
                CH(6),                   /* Lot                        */~
                CH(8),                   /* 2*BI(4) DATE/TIME STAMP    */~
                PD(14,4),                /* Quantity                   */~
                PD(14,4),                /* Unit Price (Material)      */~
                PD(14,4),                /* Unit Price (Labor)         */~
                PD(14,4),                /* Unit Price (Overhead)      */~
                CH(9),                   /* HNY Asset Account          */~
                CH(6),                   /* Date Moved                 */~
                CH(9)                    /* On Hand Vendor's Lot       */~
            /*  CH(8)                    /* Filler                     */~

L35670:     FMT                          /* LINE FOR PUT ONLY!!        */~
                CH(25),                  /* Part Number                */~
                CH(16),                  /* Receiver Number            */~
                CH(9),                   /* Vendor Code                */~
                CH(16),                  /* P.O. Number (PIPIN Tag)    */~
                CH(3),                   /* P.O. Line Sequence Number  */~
                POS(122),                /*                            */~
                CH(6),                   /* Date Received              */~
                CH(16),                  /* Vendor's Packslip          */~
                POS(156),                /*                            */~
                PD(14,4),                /* Total Quantity Received    */~
                PD(14,4),                /*       Rcv Hold             */~
                PD(14,4),                /*       Q.C. Pending         */~
                PD(14,4),                /*       Q.C. Hold            */~
                PD(14,4),                /*       Scrapped/Reject      */~
                PD(14,4),                /*       Return to Vendor     */~
                PD(14,4),                /*       Total to On Hand     */~
                POS(341),                /* Q.C. Holding Account et al */~
                CH(3),                   /* Scrap Store                */~
                CH(6),                   /* Scrap Lot                  */~
                CH(6),                   /* Rejection code             */~
                POS(388),                /* Skip A/P Block             */~
                2*CH(4),                 /* Q.C. Line Item Text        */~
                BI(4)                    /* Ser. High Index            */~

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'202(fieldnr%)

                init(hex(8c)) lfac$(), lfac1$(), lfac2$()
                str(header$,62) = " RCVDIST: " & str(cms2v$,,8)

                if editmode% <> 0 then L40155
                note$ = " "
                pfdescr$(1) = "(1)Start Over     (4)Previous Field       ~
        ~ (6)See Inventory    (13)Instructions"
                pfdescr$(2) = "                  (7)See Use              ~
        ~ (8)QC Free Text     (15)Print Screen"
                pfdescr$(3) = "(3)All To Inventory                       ~
        ~(24)Locations        (16)Edit Mode   "
                pfkeys$ = hex(000103040607080d0f1018)
                str(pfdescr$(2),43,17) = textmsg$(mode% + 1%)

                REM Determine appropriate fields...
                if qtyhold(1% + mode%, 1%) > 0 then L40130
                   str(pfdescr$(3),1,40) = " "
                   str(pfkeys$,3,1) = hex(ff)
L40130:         if fieldnr% > 1% then L40275
                   str(pfdescr$(1),19,19) = " "
                   str(pfkeys$,4,1) = hex(ff)
                goto L40275

L40155:         REM Editmode logic...
                note$ = "(Continued...)"
                pfdescr$(1) = "(1)Start Over      (6)Inv Status    (7)See~
        ~ Use                 (13)Instructions"
                pfdescr$(2) = "(2)1st Details     (4)Prev Details  (8)QC ~
        ~Free Text            (15)Print Screen"
                pfdescr$(3) = "(3)All To On Hand  (5)Next Details  (11)In~
        ~sert  (24)Locations  (16)End Dist.   "
                str(pfdescr$(2),37,16) = textmsg$(mode% + 1%)
                pfkeys$ = hex(0001020304050607080b0d0f1018)
                if fieldnr% <> 0% then L40260
                     init(hex(84)) lfac1$(), lfac2$()
                     init(hex(86)) lfac$ ()
                     if base% <> 0 then L40235
                          str(pfdescr$(2),,36) = " "
                          str(pfkeys$,3,1), str(pfkeys$,5,1) = hex(ff)
L40235:              if maxlines% > base% + 4% then L40275
                          str(pfdescr$(3),20,16) = " "
                          str(pfkeys$,6,1) = hex(ff)
                          note$ = " "
                     goto L40275
L40260:         str(pfdescr$(2),,63), str(pfdescr$(3),,63), note$ = " "
                str(pfkeys$,3,5) = all(hex(ff))

L40275:           str(pfdescr$(3),64,1) = hex(84)

                  on fieldnr% gosub L40355,         /* Q.C. Hold        */~
                                    L40385,         /* Scrap Quantity   */~
                                    L40400,         /* Return To Vendor */~
                                    L40350,         /* Null             */~
                                    L40415,         /* Move To On Hand  */~
                                    L40415,         /* Move To On Hand  */~
                                    L40415,         /* Move To On Hand  */~
                                    L40415,         /* Move To On Hand  */~
                                    L40415          /* Move To On Hand  */
                     goto L40495

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
L40350:               return
L40355:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return
L40385:           REM SET FAC'S FOR SCRAP
                      if qtyscrp$(1) = "0" then lfac1$(fieldnr%) = hex(81)
                      goto L40355
L40400:           REM SET FAC'S FOR REJECTION
                      lfac1$(fieldnr%) = hex(81)
                      goto L40355
L40415:           REM SET FAC'S FOR ON HAND
                      if qtyoh$((fieldnr%-4%)+base%,1) = "0" then        ~
                                               lfac1$(fieldnr%) = hex(81)
                      lfac1$(fieldnr%) = hex(81)
                      if index%((fieldnr%-4%)+base%) <> 0% then          ~
                                               lfac1$(fieldnr%) = hex(8c)
                      if qtyoh$((fieldnr%-4%)+base%,2) = " " and         ~
                         qtyoh$((fieldnr%-4%)+base%,1) = "0" then L40355
                      call "HNYAVAIL" (#22, #3, part$, ohstr$(test%),    ~
                                    ohlot$(test%), workkey$, 9e9, avail, ~
                                    return%)
                      if workkey$ = " " then L40355
                         workkey$ = " "
                         lfac1$(fieldnr%) = hex(8c)
                         goto L40355

L40495:     str(lfac2$()) = str(lfac1$())
            if lot% = 0% then init(hex(8c)) lfac2$()

            accept                                                       ~
               at (01,02), "Manage Disposition Of Goods",                ~
               at (01,30), fac(hex(8c)), mode$(1% + mode%)      , ch(16),~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)),   date$                , ch(08),~
               at (02,02), fac(hex(ac)),   header$              , ch(79),~
               at (03,02), fac(hex(84)),   unitsmsg$            , ch(79),~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (05,02), fac(hex(8c)), subh$(1)               , ch(79),~
               at (06,02), fac(hex(8c)), subh$(2)               , ch(79),~
               at (07,02), fac(hex(8c)), subh$(3)               , ch(79),~
               at (08,02), "Qty  Received:",                             ~
               at (08,17), fac(hex(8c)),   qtyrcvd$             , ch(10),~
               at (08,28), fac(hex(8c)),   unavail$  (1%+mode%) , ch(13),~
               at (08,42), fac(hex(8c)),   qtyunav$             , ch(10),~
               at (08,53), fac(hex(8c)),   inprocess$(1%+mode%) , ch(16),~
               at (08,70), fac(hex(84)),   qtyunds$             , ch(10),~
               at (09,02), "Ord. By:",                                   ~
               at (09,11), fac(hex(8c)),   reqstd$              , ch(20),~
               at (09,32), "Dlvr To:",                                   ~
               at (09,41), fac(hex(8c)),   delvr$               , ch(20),~
               at (09,62), "Job Nmbr:",                                  ~
               at (09,72), fac(hex(8c)),   job$                 , ch(08),~
                                                                         ~
               at (10,02), fac(hex(ac)),   tttle1$              , ch(27),~
               at (10,30), fac(hex(ac)),   tttle2$              , ch(51),~
               at (11,02), fac(hex(8c)),   unknown$  (1%+mode%) , ch(27),~
               at (11,30), fac(hex(84)),   qtyhold$(1)          , ch(10),~
               at (11,42), fac(lfac$(01)), qtyhold$(2)          , ch(10),~
                                                                         ~
               at (12,03), "  Accepted to Scrap/Rework",                 ~
               at (12,30), fac(hex(84)),   qtyscrp$(1)          , ch(10),~
               at (12,42), fac(lfac$(02)), qtyscrp$(2)          , ch(10),~
                                                                         ~
               at (12,54), "Store      Lot",                             ~
               at (12,60), fac(lfac1$(02)), scrapstr$           , ch(03),~
               at (12,69), fac(lfac2$(02)), str(scraplot$,,ll%),         ~
                                                                         ~
               at (13,03), "                  Rejected",                 ~
               at (13,30), fac(hex(84)),   qtyback$(1)          , ch(10),~
               at (13,42), fac(lfac$(03)), qtyback$(2)          , ch(10),~
               at (13,54), "Reason",                                     ~
               at (13,61), fac(lfac1$(03)), str(rejcode$,,4)    , ch(04),~
               at (13,66), fac(lfac1$(03)), str(rejcode$,5)     , ch(02),~
                                                                         ~
               at (14,02), fac(hex(ac)),    ohmsg$              , ch(45),~
               at (14,49), fac(hex(8c)),    rejdescr$           , ch(32),~
                                                                         ~
               at (15,03), fac(hex(8c)),   item$(base%+1)       , ch(03),~
               at (15,07), "Store      Lot",                             ~
               at (15,13), fac(lfac1$(05)), ohstr$(base%+1)     , ch(03),~
               at (15,22), fac(lfac2$(05)), str(ohlot$(base%+1),,ll%),   ~
               at (15,30), fac(hex(84)),   qtyoh$(base%+1,1)    , ch(10),~
               at (15,42), fac(lfac$(05)), qtyoh$(base%+1,2)    , ch(10),~
               at (15,54), "Vendor's Lot",                               ~
               at (15,69), fac(lfac$(05)), ohvenlot$(base%+1)   , ch(09),~
                                                                         ~
               at (16,03), fac(hex(8c)),   item$(base%+2)       , ch(03),~
               at (16,07), "Store      Lot",                             ~
               at (16,13), fac(lfac1$(06)), ohstr$(base%+2)     , ch(03),~
               at (16,22), fac(lfac2$(06)), str(ohlot$(base%+2),,ll%),   ~
               at (16,30), fac(hex(84)),   qtyoh$(base%+2,1)    , ch(10),~
               at (16,42), fac(lfac$(06)), qtyoh$(base%+2,2)    , ch(10),~
               at (16,54), "Vendor's Lot",                               ~
               at (16,69), fac(lfac$(06)), ohvenlot$(base%+2)   , ch(09),~
                                                                         ~
               at (17,03), fac(hex(8c)),   item$(base%+3)       , ch(03),~
               at (17,07), "Store      Lot",                             ~
               at (17,13), fac(lfac1$(07)), ohstr$(base%+3)     , ch(03),~
               at (17,22), fac(lfac2$(07)), str(ohlot$(base%+3),,ll%),   ~
               at (17,30), fac(hex(84)),   qtyoh$(base%+3,1)    , ch(10),~
               at (17,42), fac(lfac$(07)), qtyoh$(base%+3,2)    , ch(10),~
               at (17,54), "Vendor's Lot",                               ~
               at (17,69), fac(lfac$(07)), ohvenlot$(base%+3)   , ch(09),~
                                                                         ~
               at (18,03), fac(hex(8c)),   item$(base%+4)       , ch(03),~
               at (18,07), "Store      Lot",                             ~
               at (18,13), fac(lfac1$(08)), ohstr$(base%+4)     , ch(03),~
               at (18,22), fac(lfac2$(08)), str(ohlot$(base%+4),,ll%),   ~
               at (18,30), fac(hex(84)),   qtyoh$(base%+4,1)    , ch(10),~
               at (18,42), fac(lfac$(08)), qtyoh$(base%+4,2)    , ch(10),~
               at (18,54), "Vendor's Lot",                               ~
               at (18,69), fac(lfac$(08)), ohvenlot$(base%+4)   , ch(09),~
                                                                         ~
               at (19,03), fac(hex(8c)),   item$(base%+5)       , ch(03),~
               at (19,07), "Store      Lot",                             ~
               at (19,13), fac(lfac1$(09)), ohstr$(base%+5)     , ch(03),~
               at (19,22), fac(lfac2$(09)), str(ohlot$(base%+5),,ll%),   ~
               at (19,30), fac(hex(84)),   qtyoh$(base%+5,1)    , ch(10),~
               at (19,42), fac(lfac$(09)), qtyoh$(base%+5,2)    , ch(10),~
               at (19,54), "Vendor's Lot",                               ~
               at (19,69), fac(lfac$(09)), ohvenlot$(base%+5)   , ch(09),~
                                                                         ~
               at (20,30), fac(hex(84)),   note$                , ch(14),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 6% then L42060
                  call "HNYQDISP" (part$, #22, #3, #23, #2)
                  goto L40495

L42060:        if keyhit% <> 7% then L42100
                  call "PIPOASUB" (part$,#1,#22,#3,#22,#2)
                  goto L40495

L42100:        if keyhit% <> 13% then L42140
                  call "MANUAL" (pgmname$)
                  goto L40495

L42140:        if keyhit% <> 15% then L42162
                  call "PRNTSCRN"
                  goto L40495

L42162:        if keyhit% <> 24% then L42180
                  gosub locations
                  goto L40495

L42180:        if fieldnr% <> 0% then return
               close ws
               call "SCREEN" addr ("C", k%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50200,         /* Q.C. Hold        */~
                                    L50400,         /* Scrap Quantity   */~
                                    L50900,         /* Return To Vendor */~
                                    L50170,         /* Null             */~
                                    L51200,         /* Move To On Hand  */~
                                    L51200,         /* Move To On Hand  */~
                                    L51200,         /* Move To On Hand  */~
                                    L51200,         /* Move To On Hand  */~
                                    L51200          /* Move To On Hand  */
L50170:              return

L50200:     REM TEST DATA FOR Q.C. HOLD
                low = -qtyhold(2% + mode%,1)
                high = qtyhold(1% + mode%,1) + qtyhold(2% + mode%,2)
                high = qtyrcvd + low - qtyunav
                call "NUMTEST" (qtyhold$(2), low, high, errormsg$, 0.4,  ~
                                                                 qtyhold)
                if errormsg$ <> " " then return
                qtyhold(1% + mode%,1) = (qtyhold(1% + mode%,1) +         ~
                                         qtyhold(2% + mode%,2)) - qtyhold
                qtyhold(2% + mode%,2) = qtyhold
                goto L51830

L50400:     REM TEST DATA FOR SCRAP QUANTITY
                low = -qtyscrp(1)
                high = qtyhold(1% + mode%,1) + qtyscrp(2)
                high = qtyrcvd + low - qtyunav
                call "NUMTEST" (qtyscrp$(2), low, high, errormsg$, 0.4,  ~
                                                                 qtyscrp)
                if errormsg$ <> " " then return
                qtyhold(1% + mode%,1) = (qtyhold(1% + mode%,1) +         ~
                                                    qtyscrp(2)) - qtyscrp
                qtyscrp(2) = qtyscrp

*        TEST DATA FOR SCRAP STORE & LOT
            if qtyscrp(1) <> 0 then L50560
            if qtyscrp(2) <> 0 then L50560
                scrapstr$, scraplot$ = " "
                goto L51830
L50560:     call "GETCODE" (#19, scrapstr$, " ", 0%, 0, f1%)
            if f1% <> 0% then L50600
                errormsg$ = "Store Not On File: " & scrapstr$
                return
L50600:     if str(scrapstr$,,1) > "A" then L50630
                errormsg$="Store Code Must Start With Letter: "&scrapstr$
                return
L50630:     call "LOTVALID" (part$, scrapstr$, scraplot$, #2, #22, #3,   ~
                                                               errormsg$)
            if errormsg$ <> " " then return
            if rej% = 0% then L51830
                rejlot$(rej%) = scraplot$
                call "LOTUNQ2E" (parts$(), rejlot$(), rej%, #2, errormsg$)
                if errormsg$ <> " " then return
                if maxdist% = 0% or lot_unique$ <> "U"                   ~
                                 or scraplot$    = " " then L51830
                   for i% = 1% to maxdist%
                     if rcvhnyds%(i%) = 0% then L50790
                     if scraplot$ <> str(rcvhnyds$(i%),4, 6) then L50790
                     if part$      = parts$(rcvhnyds%(i%),1%) then L50790
                        errormsg$ = "Lot already used for Part: " &      ~
                                                 parts$(rcvhnyds%(i%),1%)
                        return
L50790:            next i%
                   goto L51830

L50900:     REM TEST DATA FOR RETURN TO VENDOR
                low = -qtyback(1)
                high = qtyhold(1% + mode%,1) + qtyback(2)
                high = qtyrcvd + low - qtyunav
                call "NUMTEST" (qtyback$(2), low, high, errormsg$, 0.4,  ~
                                                                 qtyback)
                if errormsg$ <> " " then return
                qtyhold(1% + mode%,1) = (qtyhold(1% + mode%,1) +         ~
                                                    qtyback(2)) - qtyback
                qtyback(2) = qtyback
                if qtyback(2) <> 0 then L51070
                if qtyback(1)  = 0 then L51050
                   get record$() using L51030, rejcode$
L51030:                FMT POS(350), CH(6)
                   if rejcode$ <> " " then L51080
L51050:               rejcode$, rejdescr$ = " "
                      goto L51830
L51070:         if rejcode$ = " " and rjflag$ = "N" then L51050
L51080:         call "GETCODE" (#21, rejcode$, rejdescr$, 0%, 0, f1%)
                   if f1% = 0% then L51120
                call "STRING" addr("RJ", rejdescr$, 32%)
                goto L51830
L51120:         errormsg$ = "Rejection code not on file."
                return

L51200:     REM TEST DATA FOR MOVE TO ON HAND
                low = 0
                test% = (fieldnr%-4) + base%
                convert qtyoh$(test%,1) to low, data goto L51240
L51240:         high = qtyhold(1% + mode%,1) + qtyoh(test%)
                high = qtyrcvd - (low + qtyunav)
                if index%(test%) = 0% then L51280
                   low = ser%(test%)
L51280:         call "NUMTEST" (qtyoh$(test%,2), -low, high, errormsg$,  ~
                                                             0.4, qtyoh)
                if errormsg$ <> " " then return
                if qtyoh <> 0 then L51380
                   if qtyoh$(test%, 1) <> "0" then L51380
                   if test% < maxlines% then L51720
                      qtyoh$(test%,1), qtyoh$(test%,2), ohstr$(test%),   ~
                      ohlot$(test%), ohvenlot$(test%) = " "
                   if test% = maxlines% then maxlines% = maxlines% - 1%
                   goto L51730
L51380:         call "GETCODE" (#19, ohstr$(test%), " ", 0%, 0, f1%)
                if f1% <> 0 then L51420
                     errormsg$ = "Store Not On File: " & ohstr$(test%)
                     return
L51420:         call "LOTVALID" (part$, ohstr$(test%), ohlot$(test%),    ~
                                                  #2, #22, #3, errormsg$)
                if errormsg$ <> " " then return
                if maxdist% = 0% or lot_unique$ <> "U"                   ~
                                 or ohlot$(test%) = " " then L51550
                 for i% = 1% to maxdist%
                  if rcvhnyds%(i%) = 0% then L51540
                  if ohlot$(test%) <> str(rcvhnyds$(i%),4, 6) then L51540
                  if part$          = parts$(rcvhnyds%(i%),1%) then L51540
                     errormsg$ = "Lot already used for Part: " &         ~
                                                 parts$(rcvhnyds%(i%),1%)
                     return
L51540:          next i%
L51550:         if rej% = 0% or lot_unique$ <> "U"                       ~
                             or ohlot$(test%) = " " then L51640
                     for i% = 1% to dim(rejlot$(),1)
                        if rejlot$(i%)  = " " or parts$(i%,1) = part$ or ~
                           rejlot$(i%) <> ohlot$(test%) then L51630
                             errormsg$ = "Lot already used for Part: " & ~
                                         parts$(i%,1)
                             return
L51630:              next i%
L51640:         temp = 0:convert qtyoh$(test%,1) to temp, data goto L51650
L51650:         if qtyoh < qtyoh(temp%) then gosub test_qty_avail
                   if errormsg$ <> " " then return
                if temp + qtyoh = 0 then L51710
                call "SERENABL" (part$, temp%, f1%, #2, #22)
                if temp% <> 0% then gosub serial_input
                   if errormsg$ <> " " then return
L51710:         if test% >= maxlines% then defstr$ = ohstr$(test%)
L51720:         maxlines% = max(maxlines%, test%)
L51730:         qtyhold(1% + mode%,1) = (qtyhold(1% + mode%,1) +         ~
                                                  qtyoh(test%)) - qtyoh
                qtyoh(test%) = qtyoh
                qtyohnet = qtyohnet + qtyoh
                str(ohmsg$,35) = " "
                call "CONVERT" (qtyohnet , -0.4, str(ohmsg$,35,10))
                str(ohmsg$,len(ohmsg$) + 1) = ")"

        REM SET HEADER DISPLAY TO REFLECT NEW STUFF

L51830:         call "CONVERT" (qtyhold(1% + mode%, 1), -0.4, qtyunds$)
                return

        serial_input

                if index%(test%) = 0% then                               ~
                                     index%, index%(test%) = index% + 1%
                temp% = 0%:convert poline$ to temp%, data goto L52050
L52050:         temp% = 10000%*temp% + index%(test%)
                readkey$ = str(rcvno$,,16) & str(pono$,,16) &            ~
                           str(poline$,,3)
                str(readkey$,36,3) = bin(index%(c%),3)
                serqty = low + qtyoh + temp
                call "SERINSUB" (part$, ohstr$(test%), ohlot$(test%),    ~
                                 serqty, temp%, 100%, "PO",              ~
                                 readkey$, errormsg$, #2, #22, #42, #41)
                return

        REM *************************************************************~
            * TEST QUANTITY FOR HNYHOLD LOGIC                           *~
            *************************************************************
        test_qty_avail

            call "HNYAVAIL" (#22, #3, part$, ohstr$(test%),              ~
                             ohlot$(test%), errormsg$, 9e9, avail,       ~
                             return%)
            if errormsg$ = " " then return

            inmem, buffqty, postqty = 0

            for i% = 1% to maxlines%
                if i% = test% then L52370
                if ohstr$(i%) <> ohstr$(test%) then L52370
                if ohlot$(i%) <> ohlot$(test%) then L52370
                   inmem = inmem + qtyoh(i%)
L52370:     next i%

            if buffmax% = 0% then L52490
            for i% = 1% to buffmax%
                if buffidx%(i%) = 0% then L52460
*              IF BUFFIDX%(I%) = REJ% THEN 52280
                if buffstr$(i%) <> ohstr$(test%) then L52460
                if bufflot$(i%) <> ohlot$(test%) then L52460
                   buffqty = buffqty + buffqty(i%)
L52460:     next i%


L52490:     if postmax% = 0% then L52580
            for i% = 1% to postmax%
                if postidx%(i%) = 0% then L52560
*              IF POSTIDX%(I%) = REJ% THEN 52380
                if poststr$(i%) <> ohstr$(test%) then L52560
                if postlot$(i%) <> ohlot$(test%) then L52560
                   postqty = postqty + postqty(i%)
L52560:     next i%

L52580:     held = 0
            if buffmax% > 0% then held = max(0, postqty - buffqty)
            inmem = inmem + temp + qtyoh
            need = max(0, postqty - inmem)
            need = need - held
            if need <= 0 or need <= avail then errormsg$ = " "

            return

        REM *************************************************************~
            *                                                           *~
            *                   LOCATION MANAGEMENT                     *~
            *                                                           *~
            *************************************************************

        locations
            totqty, qty1, qty2, qty3, qty4, qty5 = 0

            if qtyoh$(base%+1,2) <> " " then                             ~
                convert qtyoh$(base%+1,2) to qty1
            if qtyoh$(base%+2,2) <> " " then                             ~
                convert qtyoh$(base%+2,2) to qty2
            if qtyoh$(base%+3,2) <> " " then                             ~
                convert qtyoh$(base%+3,2) to qty3
            if qtyoh$(base%+4,2) <> " " then                             ~
                convert qtyoh$(base%+4,2) to qty4
            if qtyoh$(base%+5,2) <> " " then                             ~
                convert qtyoh$(base%+5,2) to qty5

            totqty = qty1 + qty2 + qty3 + qty4 + qty5

            call "HNYLCSUB" (part$,        /*  Part Number             */~
                             defstr$,      /*  Default Store Number    */~
                             deflot$,      /*  Default Lot Number      */~
                             totqty,       /*  Quantity To Be Added    */~
                             3%,           /*  Action = ADD            */~
                             #2,           /*  SYSFILE2                */~
                             #19,          /*  STORNAME                */~
                             #1,           /*  USERINFO                */~
                             #22,          /*  HNYMASTR                */~
                             #13,          /*  HNYLOCNS                */~
                             #3,           /*  HNYQUAN                 */~
                             #14)          /*  LOCATION                */

            return


L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            init (hex(00)) workkey$
            call "DELETE" (#51, workkey$, 0%)
            call "DELETE" (#52, workkey$, 0%)

            end
