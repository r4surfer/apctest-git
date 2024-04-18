        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  RRRR    CCC   V   V  H   H  N   N  Y   Y  RRRR   TTTTT   *~
            *  R   R  C   C  V   V  H   H  NN  N  Y   Y  R   R    T     *~
            *  RRRR   C      V   V  HHHHH  N N N   YYY   RRRR     T     *~
            *  R   R  C   C   V V   H   H  N  NN    Y    R   R    T     *~
            *  R   R   CCC     V    H   H  N   N    Y    R   R    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RCVHNYRT - PRINTS LOG OF INVENTORY RECEIVED/DISTRIBUTED   *~
            *            CAN RUN EITHER BY RECEIVER/VENDOR/PO           *~
            *            OR BY PART OR BOTH.                            *~
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
            * 07/17/86 ! ORIGINAL                                 ! KAB *~
            * 04/16/91 ! PRR 11330/11884.  Fixed subtotal amount, ! SID *~
            *          !       and page break error condition.    !     *~
            * 04/17/91 ! PRR 11605  Added Vendor Unit of Measure  ! SID *~
            *          !       to the report.                     !     *~
            * 01/06/93 ! PRR 12486,12742 Fixed 2nd PO column qtys.! JDH *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~

        dim                                                              ~
            company$60,                                                  ~
            date$8,                                                      ~
            last_part$25,                                                ~
            last_po$16,                                                  ~
            last_receiver$16,                                            ~
            last_vendor$9,                                               ~
            lastrecdate$8,                                               ~
            line$3,                                                      ~
            part$25,                                                     ~
            pipkey$100,                                                  ~
            pipmsg$40,                                                   ~
            plowkey$100,                                                 ~
            print$(7)10,                                                 ~
            print_title$60,                                              ~
            po$16,                                                       ~
            receiver$16,                                                 ~
            runmode$3,                                                   ~
            uom$,                                                        ~
            vendor$9                                                     ~

        dim                                                              ~
            newqty(7),                                                   ~
            netqty(7),                                                   ~
            oldqty(7),                                                   ~
            totqty(7)                                                    ~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! RCVHNYRP ! Accounts Payable Inventory Report File   *~
            * #2  ! SYSFILE2 ! System Control File                      *~
            * #3  ! PIPOUT   ! Planned Inventory Withdrawal             *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "RCVHNYRP",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 400,                                   ~
                        keypos = 29,   keylen = 55,                      ~
                        alt key 1, keypos =  1, keylen = 83              ~

            select  #2, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select  #3,  "PIPOUT",                                       ~
                          varc,                                          ~
                          indexed,                                       ~
                          recsize = 64,                                  ~
                          keypos = 1, keylen = 56,                       ~
                          alt key 1, keypos = 20, keylen = 37

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#1,  "SHARE", f2%(1 ), rslt$(1 ), axd$(1 ))
            call "OPENFILE" (#2,  "SHARE", f2%(2 ), rslt$(2 ), axd$(2 ))
            call "OPENFILE" (#3,  "SHARE", f2%(3 ), rslt$(3 ), axd$(3 ))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date:call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, l%)

            rpt_time$ = " "             /* Get System Time            */
            call "TIME" (rpt_time$)

            call "EXTRACT" addr ("ID", str(plowkey$,,3))

            call "GETPARM" addr ("ID", "R", "RUNMODE ",  " ", "0001",    ~
                                     "RCVHNY",                           ~
                                    "INPUT THE RUN MODE FOR THIS REPORT",~
                                      34%, "K", "RUNMODE ", runmode$, 3%,~
                                      5%, 32%, "A")

            call "PIPINDEX" (#2, " ", today%, pip%)

            close ws
                select printer (134)
                call "SETPRNT" ("RCV003", " ", 0%, 0%)
                call "SHOSTAT" ("Printing Inventory Receipts Log")

        REM *************************************************************~
            *   PICK UP EACH INVENTORY TRANSACTION RECORD AND PRINT IT  *~
            *      NOTE - "RCVUPSUB" ONLY WRITES TO THIS FILE IF        *~
            *             MOVEMENT HAS OCCURRED.                        *~
            *************************************************************

            if runmode$ = "INV" then L20000

            last_part$, last_receiver$, last_po$, last_vendor$ = " "
            l% = -1%
               page% = 0%
                  init (hex(00)) str(plowkey$, 4)

L10090:     call "PLOWNEXT" (#1, plowkey$, 3%, f1%(1))
                if f1%(1) <> 1 then L10390

            get #1, using L10140, part$, receiver$, vendor$, po$, line$,  ~
                    lastrecdate$, newqty(), oldqty()

L10140:         FMT XX(3), CH(25), XX(3), CH(16), CH(9), CH(16), CH(3),  ~
                    POS(128), CH(6), POS(162), 7*PD(14,4),               ~
                    POS(254), 7*PD(14,4)

            mat netqty = newqty - oldqty
            gosub print_report1
            goto L10090

L10390:     if page% < 1% then gosub print_heading1
            print skip(2)
            print "                                         ********** E ~
        ~N D   O F   R E P O R T ********** "

            goto L20000

        REM *************************************************************~
            *                P R I N T  R E P O R T  1                  *~
            *-----------------------------------------------------------*~
            *  Report by Receiver Code.                                 *~
            *************************************************************

        print_report1
            if l% < 1% then gosub print_heading1
            if receiver$ <> last_receiver$ then gosub print_receiver_hdng1
            call "CONVERT" (netqty(1)   , 0.2, print$(1))
            call "CONVERT" (netqty(2)   , 0.2, print$(2))
            call "CONVERT" (netqty(7)   , 0.2, print$(3))
            call "CONVERT" (netqty(6)   , 0.2, str(print$(4),,9))
            call "CONVERT" (netqty(5)   , 0.2, str(print$(5),,9))
            call "CONVERT" (netqty(3)   , 0.2, str(print$(6),,9))
            call "CONVERT" (netqty(4)   , 0.2, str(print$(7),,9))
            call "STRING" addr ("LJ", line$, 3%)
            if last_vendor$ = vendor$ then  L11210
               print using L12290, vendor$, po$, line$, part$, print$(1), ~
                                  print$(2), print$(3), print$(4),       ~
                                  print$(5), print$(6), print$(7)
               goto L11290
L11210:     if last_po$ = po$ then L11260
               print using L12290, " ", po$, line$, part$, print$(1),     ~
                                  print$(2), print$(3), print$(4),       ~
                                  print$(5), print$(6), print$(7)
               goto L11290
L11260:        print using L12290, " ", " ", line$, part$, print$(1),     ~
                                  print$(2), print$(3), print$(4),       ~
                                  print$(5), print$(6), print$(7)
L11290:     l% = l% - 1%
            last_vendor$ = vendor$
            last_po$ = po$
            return

        print_receiver_hdng1
            if l% < 6% then gosub print_heading1
            last_receiver$ = receiver$
            last_vendor$ = " "
            last_po$ = " "
            if len(lastrecdate$) < 8% then call "DATEFMT" (lastrecdate$)
            print
            print using L12160, receiver$, lastrecdate$
            l% = l% - 2%
            return

        print_heading1
            print page
            page% = page% + 1%
            print using L12060, date$, rpt_time$, company$
            print_title$ = "RECEIVER/Q.C. INVENTORY MOVEMENT REPORT"
            call "FMTTITLE" (print_title$, " ", 12%)
            print using L12100, print_title$, page%
            print_title$ = "BY RECEIVER NUMBER"
            call "FMTTITLE" (print_title$, " ", 12%)
            print using L12130, print_title$

            l% = 57%
            print skip(1)
            print using L12180
            print using L12210
            print using L12250
            last_receiver$ = " "
            l% = l% - 4%
            return

        REM *************************************************************~
            *                R E P O R T   F O R M A T S                *~
            *-----------------------------------------------------------*~
            *  Report format lines used by print statements.            *~
            *************************************************************

L12060: % ######## ########                  ############################~
        ~################################                    RCVHNYRT:RCV0~
        ~03

L12100: %                                    ############################~
        ~################################                        PAGE:####

L12130: %                                    ############################~
        ~################################

L12160: %***** RECEIVER: ################ DATE RECEIVED: ######## *****

L12180: %                                                            TOTA~
        ~L     .........................DISTRIBUTION......................~
        ~..
L12210: %VENDOR    PURCHASE ORDER & LINE  PART NUMBER                RECE~
        ~IVED  RCVER-HOLD      STOCK    REJECT  SCRP/RWK     IN QC   QC-HO~
        ~LD

L12250: %--------- ---------------- ----  ------------------------- -----~
        ~----- ---------- ---------- --------- --------- --------- -------~
        ~--

L12290: %######### ################ ###   ######################### #####~
        ~##### ########## ########## ######### ######### ######### #######~
        ~##

L20000: REM *************************************************************~
            *   PICK UP EACH INVENTORY TRANSACTION RECORD AND PRINT IT  *~
            *      NOTE - "RCVUPSUB" ONLY WRITES TO THIS FILE IF        *~
            *             MOVEMENT HAS OCCURRED.                        *~
            *************************************************************

            if runmode$ = "RCV" then L65000

            last_part$, last_receiver$, last_po$, last_vendor$ = " "
            mat totqty = zer
            l% = -1%
               page% = 0%
                  init (hex(00)) str(plowkey$, 4)

L20110:     call "PLOWALTS" (#1, plowkey$, 1%, 3%, f1%(1))
                if f1%(1) <> 0 then L20140
                   gosub print_part_subtotal
                   goto L20250
L20140:     get #1, using L20170, part$, receiver$, vendor$, po$, line$,  ~
                    lastrecdate$, newqty(), uom$, oldqty()

L20170:         FMT XX(3), CH(25), XX(3), CH(16), CH(9), CH(16), CH(3),  ~
                    POS(128), CH(6), POS(162), 7*PD(14,4),               ~
                    POS(250), CH(4), 7*PD(14,4)

            mat netqty = newqty - oldqty
            gosub print_report2
            goto L20110

L20250:     if page% < 1% then gosub print_heading2
            print skip(2)
            print "                                         ********** E ~
        ~N D   O F   R E P O R T ********** "

            goto L65000

        REM *************************************************************~
            *                P R I N T  R E P O R T  2                  *~
            *-----------------------------------------------------------*~
            *  Report by Receiver Code.                                 *~
            *************************************************************

        print_report2
            if l% < 3% then gosub print_heading2
            if part$ <> last_part$ then gosub print_part_hdng2
            mat totqty = totqty + netqty
            call "CONVERT" (netqty(1)   , 0.2, print$(1))
            call "CONVERT" (netqty(2)   , 0.2, print$(2))
            call "CONVERT" (netqty(7)   , 0.2, print$(3))
            call "CONVERT" (netqty(6)   , 0.2, str(print$(4),,9))
            call "CONVERT" (netqty(5)   , 0.2, str(print$(5),,9))
            call "CONVERT" (netqty(3)   , 0.2, str(print$(6),,9))
            call "CONVERT" (netqty(4)   , 0.2, str(print$(7),,9))
            call "STRING" addr ("LJ", line$, 3%)
            if last_receiver$ = receiver$ then L21115
               print using L22290, receiver$,vendor$,po$,line$, uom$,     ~
                                  print$(1), print$(2), print$(3),       ~
                                  print$(4), print$(5), print$(6),       ~
                                  print$(7)
               goto L21180
L21115:     if last_vendor$ = vendor$ then L21140
               print using L22290, " ", vendor$, po$, line$, uom$,        ~
                                  print$(1), print$(2), print$(3),       ~
                                  print$(4), print$(5), print$(6),       ~
                                  print$(7)
               goto L21180
L21140:     if last_po$ = po$ then L21165
               print using L22290, " ", " ", po$, line$, uom$,            ~
                                  print$(1), print$(2), print$(3),       ~
                                  print$(4), print$(5), print$(6),       ~
                                  print$(7)
               goto L21180
L21165:        print using L22290, " ", " ", " ", line$, uom$,            ~
                                  print$(1), print$(2), print$(3),       ~
                                  print$(4), print$(5), print$(6),       ~
                                  print$(7)
L21180:     l% = l% - 1%
            last_receiver$ = receiver$
            last_vendor$ = vendor$
            last_po$ = po$
            return

        print_part_hdng2
            if last_part$ <> " " then gosub print_part_subtotal
            if l% < 6% then gosub print_heading2
            last_part$ = part$
            last_receiver$ = " "
            last_vendor$ = " "
            last_po$ = " "
            pipmsg$ = " "
            init (hex(00)) pipkey$
            str(pipkey$,1,25) = str(part$,1,25)
            call "PLOWALTS" (#3, pipkey$, 1%, 25%, f1%(3))
               if f1%(3) = 0 then L21305
            get #3, using L21271, pip%
L21271:         FMT POS(45), BI(4)
            if pip% < (today% + 2%) then                                 ~
                      pipmsg$ = "(SCHEDULED WITHDRAWAL FOR TOM0RROW)"
            if pip% < (today% + 1%) then                                 ~
                      pipmsg$ = "(SCHEDULED WITHDRAWAL FOR TODAY)"
            if pip% < today% then                                        ~
                      pipmsg$ = "(SCHEDULED WITHDRAWAL IS PAST DUE)"
L21305:     print
            print using L22160, part$, pipmsg$
            l% = l% - 2%
            return

        print_part_subtotal
            if last_part$ = " " then return
            call "CONVERT" (totqty(1)   , 0.2, print$(1))
            call "CONVERT" (totqty(2)   , 0.2, print$(2))
            call "CONVERT" (totqty(7)   , 0.2, print$(3))
            call "CONVERT" (totqty(6)   , 0.2, str(print$(4),,9))
            call "CONVERT" (totqty(5)   , 0.2, str(print$(5),,9))
            call "CONVERT" (totqty(3)   , 0.2, str(print$(6),,9))
            call "CONVERT" (totqty(4)   , 0.2, str(print$(7),,9))
            print using L22370
            print using L22330, last_part$, print$(1),print$(2),print$(3),~
                               print$(4), print$(5), print$(6), print$(7)
            l% = l% - 2%
            mat totqty = zer
            return

        print_heading2
            print page
            page% = page% + 1%
            print using L22060, date$, rpt_time$, company$
            print_title$ = "RECEIVER/Q.C. INVENTORY MOVEMENT REPORT"
            call "FMTTITLE" (print_title$, " ", 12%)
            print using L22100, print_title$, page%
            print_title$ = "BY INVENTORY PART CODE"
            call "FMTTITLE" (print_title$, " ", 12%)
            print using L22130, print_title$

            l% = 57%
            print skip(1)
            print using L22180
            print using L22210
            print using L22250
            l% = l% - 4%
*          LAST_PART$ = " "
            return

        REM *************************************************************~
            *                R E P O R T   F O R M A T S                *~
            *-----------------------------------------------------------*~
            *  Report format lines used by print statements.            *~
            *************************************************************

L22060: % ######## ########                  ############################~
        ~################################                    RCVHNYRT:RCV0~
        ~03

L22100: %                                    ############################~
        ~################################                        PAGE:####

L22130: %                                    ############################~
        ~################################

L22160: %***** PART: ######################### ***** ####################~
        ~####################
L22180: %                                                            TOTA~
        ~L     .........................DISTRIBUTION......................~
        ~..
L22210: %RECEIVER         VENDOR    PURCHASE ORDER & LINE  UOM       RECE~
        ~IVED  RCVER-HOLD      STOCK    REJECT  SCRP/RWK     IN QC   QC-HO~
        ~LD

L22250: %---------------- --------- ---------------- ----  ----     -----~
        ~----- ---------- ---------- --------- --------- --------- -------~
        ~--

L22290: %################ ######### ################ ###   ####     #####~
        ~##### ########## ########## ######### ######### ######### #######~
        ~##

L22330: %     *** SUBTOTAL FOR PART: #########################      #####~
        ~##### ########## ########## ######### ######### ######### #######~
        ~##

L22370: %                                                           -----~
        ~----- ---------- ---------- --------- --------- --------- -------~
        ~--
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

            call "SETPRNT" (" ", " ", 0%, 1%)

            init (hex(00)) str(plowkey$,4)
            call "DELETE" (#1, plowkey$, 3%)

            end
