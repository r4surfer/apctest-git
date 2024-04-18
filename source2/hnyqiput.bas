        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  H   H  N   N  Y   Y   QQQ   IIIII  PPPP   U   U  TTTTT   *~
            *  H   H  NN  N  Y   Y  Q   Q    I    P   P  U   U    T     *~
            *  HHHHH  N N N   YYY   Q   Q    I    PPPP   U   U    T     *~
            *  H   H  N  NN    Y    Q Q Q    I    P      U   U    T     *~
            *  H   H  N   N    Y     QQQ   IIIII  P       UUU     T     *~
            *                                                           *~
            *----------------------------------------------------------Q*~
            * HNYQIPUT - Used normally at setup to enter the original   *~
            *            Store/Lot Quantity records for Inventory and   *~
            *            to build the LIFO/FIFO Pools.  May also be used*~
            *            to adjust existing quantities / pool records   *~
            *            but be careful - no edits to keep user from    *~
            *            shooting self in foot!.                        *~
            *            A balance report may be generated from here    *~
            *            also.                                          *~
            *----------------------------------------------------------Q*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/10/83 ! ORIGINAL                                 ! GLW *~
            * 10/04/85 ! Changed VENDOR File Format               ! MJB *~
            * 11/07/85 ! Screen format & Other cleanup changes.   ! LDJ *~
            * 01/06/86 ! Removed ABC Class, expanded Bin Location.! LDJ *~
            * 02/26/86 ! ERROR IN PIPMASTR HANDLING, FIX MORE THAN! KAB *~
            *          ! ARRAY ALLOWABLE POOL RECORDS, ONE KEY    !     *~
            *          ! CONSOLIDATION OF POOLS, DISPLAY NUMBER OF!     *~
            *          ! POOL RECORDS.                            !     *~
            * 02/10/87 ! New HNYQUAN format; potency & exp. date  ! JRH *~
            *          !   added; create PIPOUT if exp. date is   !     *~
            *          !   changed; add Variable fields & free    !     *~
            *          !   text;                                  !     *~
            * 03/25/87 ! Added support for Serial Numbers.        ! LDJ *~
            *          !   (Plus considerable cleanup work).      !     *~
            * 04/09/87 ! Standard Cost Project changes.  Also,    ! ERN *~
            *          !   combined functions of HNYEDT.          !     *~
            *          !   (Essentially rewrote program - any     !     *~
            *          !    resemblance to the older version is   !     *~
            *          !    purely coincendental).                !     *~
            * 10/27/87 ! Stopped double formatting of GL accts    ! HES *~
            * 03/17/88 ! Modified Administrator/Edit logic.       ! JIM *~
            * 03/17/88 ! Let sales dist & variance acct be blank  ! JIM *~
            * 09/26/88 ! Fixed problem w/ changing Qty's on       ! RJM *~
            *          !    Serailized Parts. Added section to    !     *~
            *          !    Build SERTIF with serial #'s for only !     *~
            *          !    the current Part, Store & Lot.        !     *~
            * 03/23/89 ! Fixed PRR's 10502,10543 FIXED FOLLOWING- ! RJM *~
            *          !    1)Shared Mode error on SERMASTR       !     *~
            *          !    2)Edit of Ser #'d Part in Shared Mode !     *~
            *          !    3)Defaults in STD Costs if applicable !     *~
            *          !    4)No more edit of unused cost buckets !     *~
            * 03/28/89 ! Corrected ln 32500 to be PUT #11         ! MJB *~
            * 07/18/89 ! Added GLFMT before GETCODE @ ln 30541    ! MJB *~
            * 10/10/89 ! Added Cost Type 'B'                      ! KAB *~
            * 10/29/90 ! Moved call to SETPRNT to 65000 section   ! MJB *~
            *          !  to reset print params AFTER report complete   *~
				* 06/25/96 ! Add blank date for checking              ! DER *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            acct$(18)12,                 /* G/L Account Numbers        */~
            acctdescr$(18)30,            /*                            */~
            acctmsg$(18)27,              /* Account Descriptors        */~
            bin$8,                       /* Bin Location               */~
				blank_date$8,                /* Blank date test            */~
            bucket_id$(12)10,            /* Cost Set Buckets           */~
            bucket_descr$(12)20,         /*                            */~
            company$60,                  /* A visitor at your house    */~
            continue$79,                 /* Message                    */~
            cost(12), cost$(12)12,       /* Inventory Costs            */~
            costtype$1, oldcosttype$1,   /* Costing Method             */~
            costdescr$(11)32,            /* Cost Method Descriptions   */~
            costtypedescr$32,            /*                            */~
            cursor%(2),                  /* Cursor Position            */~
            date$8,                      /* Today's date               */~
            datexpr$8,                   /* Exp. date                  */~
            dfac$(14),                   /* Balance Display FACs       */~
            edttran$80,                  /* Field locations for pools  */~
            edtmessage$79,               /* "To modify values..." text */~
            errormsg$79,                 /* Error message text         */~
            exprdate$8,                  /* Expiration Date            */~
            exprsav1$8, exprsav2$8,      /*                            */~
            fac$(20,6)1, lfac$(20)1,     /* Field Attribute Characters */~
            i$(24)80,                    /* Screen image               */~
            inpmessage$79,               /* Input Message              */~
            line1$26, line2$79,          /* Screen Lines               */~
            location$30,                 /* Serial Numbers Location    */~
            location2$30,                /* Serial Numbers Location    */~
            lot$6, lot_msg$30,           /* Lot Number & Message       */~
            minimum$10,                  /* Minimum on hand quantity   */~
            maximum$10,                  /* Maximum on hand quantity   */~
            mode$5,                      /* HNYQUAN Open mode          */~
            msg$79,                      /* A message                  */~
            ncost$10,                    /* Misc Display Variable      */~
            packed$96,                   /* Packed costs for writes    */~
            part$25, partdescr$34,       /* Part Number                */~
            pfkey$32, pfmsg$(3)79,       /* PF Keys and Prompts        */~
            pip%(490),                   /* PIPMASTR buckets           */~
            pipamt(4),                   /* PIPMASTR amounts           */~
            plowkey$99,                  /* Misc Plow Key              */~
                                                                         ~
            pacct1$(500)12,              /* Pool Asset Account         */~
            pacct2$(500)12,              /* Pool Adjustment Account    */~
            pccosts(12), pctcosts(12),   /* Pool Combining Work Vars   */~
            pcost$(500)10,               /* Pool total Cost            */~
            pcosts$(500)96,              /* Pool Costs                 */~
            pdate$(500)8,                /* Pool Transaction Date      */~
            perr$79, pinfo$79,           /* Error and Info Msgs        */~
            phdr$(4)40,                  /* Pool Column Headers        */~
            poolkey$38,                  /* Pool Record Key            */~
            porig(500),                  /* Pool Original Quantity     */~
            pqty$(500)10,                /* Pool Current Quantity      */~
            ptext$(500)40,               /* Pool Posting Text          */~
                                                                         ~
            potdate$8,                   /* Date potency last modified */~
            potency$8,                   /* Potency Factor             */~
            qty(6), qty$(6)12,           /* HNYQUAN Qtys               */~
            quan$10,                     /* Misc Display Variable      */~
            readkey$100,                 /* Misc Read/Plow Key         */~
            runtime$8,                   /* Starting run time          */~
            serkey$50,                   /* Serial Number Files Key    */~
            set%(255), scr%(5,20),       /* Soft Enable Tables         */~
            setid$8,                     /* Cost Set ID                */~
            stat$3,                      /* F2% edited for ASKUSER     */~
            stat1$1,                     /* Serial # Status            */~
            stat2$1,                     /* Prior Serial # Status      */~
            std(12),                     /* Standard Cost              */~
            store$3, strdescr$32,        /* Store number               */~
            temp$10,                     /* Work variables             */~
            text$(196,1)70,              /* Text Array                 */~
            textid$4,                    /* Text ID                    */~
            totcost$12,                  /* Total Cost- for display    */~
            userexpr$3,                  /* User last mod. expr date   */~
            userid$3,                    /* Current user               */~
            userpot$3,                   /* User last mod. potency     */~
            variable$200,                /* Variable fields            */~
            warn$21

        dim f2%(64),                     /* File Open Status           */~
            f1%(64),                     /* Record-on-file flags       */~
            rslt$(64)20                  /* Return code from "FILEOPEN"*/~

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
            * #01 ! HNYMASTR ! Inventory Parts Master                   *~
            * #02 ! HNYQUAN  ! Inventory Quantities File                *~
            * #03 ! STORNAME ! Store Master                             *~
            * #04 ! SYSFILE2 ! CMS System Information File              *~
            * #05 ! GLMAIN   ! General Ledger Chart of Accounts         *~
            * #10 ! HNYPOOL  ! LIFO/FIFO Pools Records                  *~
            * #11 ! PIPMASTR ! Planned Inventory Position               *~
            * #13 ! SFCUM2   ! Cum Net Sales Fcst Position File         *~
            * #14 ! TXTFILE  ! System Text File                         *~
            * #20 ! SERTIF   ! Additions buffer for inventory S/N's     *~
            * #21 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #22 ! SERWORK  ! Temporary Serial #'s Work File           *~
            *************************************************************

            select #01, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 900,                                   ~
                        keypos = 1, keylen = 25,                         ~
                        alternate key 1, keypos = 102, keylen = 9, dup,  ~
                                  key 2, keypos = 90,  keylen = 4, dup

            select #02, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos=17, keylen = 44,                          ~
                        alternate key 1, keypos =  1, keylen = 44

            select #03, "STORNAME",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 3

            select #04, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #05, "GLMAIN",                                        ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 300,                                    ~
                       keypos = 1, keylen = 9

            select #10, "HNYPOOL",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 38

            select #11, "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos = 2, keylen = 25,                         ~
                        alternate key 1, keypos=1, keylen=26

            select #13, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos = 1, keylen = 25

            select #14, "TXTFILE", varc, indexed, recsize = 2024,        ~
                        keypos  = 1, keylen = 11

            select #20, "SERTIF",                                        ~
                       varc,                                             ~
                       indexed,                                          ~
                        recsize =  100,                                  ~
                        keypos = 1, keylen = 62


            select #21, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #22, "SERWORK",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  48,                                   ~
                        keypos = 1, keylen = 23

*        Find out if full editing is to be allowed or not.
            call "CMSMACHK" ("HNY", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then admin% = 1%
            mode$ = "SHARE"
            if admin% = 0% then L03340   /* Skip over warning messages */

*        Full edit is allowed -- give appropriate warnings.
            warn$ = "*** W A R N I N G ***"
L03020:     accept                                                       ~
                at (02,30), fac(hex(84)), warn$,                         ~
                at (04,21), "HNYQIPUT is an Administrative Function.",   ~
                at (05,17), "It does NOT post to G/L & leaves NO audit tr~
        ~ail!",                                                           ~
                at (10,02), "Press: (RETURN)- Continue with the HNYQUAN f~
        ~ile in SHARED mode.",                                            ~
                at (11,02), "                 CAUTION: Updates may confli~
        ~ct with other HNYQUAN file users.",                              ~
                at (12,02), "                 NO Editing of Serialized Pa~
        ~rts is allowed in SHARED mode.",                                 ~
                at (14,02), "            (1)- Continue with the HNYQUAN f~
        ~ile used EXCLUSIVELY. No other",                                 ~
                at (15,02), "                 user may use HNYQUAN while ~
        ~you run this program, eliminating",                              ~
                at (16,02), "                 the possibility of update c~
        ~onflicts.",                                                      ~
                at (18,02), "           (13)- Instructions.",            ~
                at (19,02), "           (15)- Print this screen.",       ~
                at (20,02), "           (16)- EXIT this program.",       ~
                keys(hex(00010d0f10)), key(keyhit%)

            if keyhit% <> 13% then L03250
                call "MANUAL" ("HNYQIPUT")
                goto L03020
L03250:     if keyhit% <> 15% then L03280
                call "PRNTSCRN"
                goto L03020
L03280:     print page                    /* Clear screen; Home cursor */
            if keyhit% = 16% then exit_program
            if keyhit% <> 1% then L03320
                mode$ = "IO   "
                goto L03340
L03320:     if keyhit% <> 0% then L03020

L03340: call "SHOSTAT"  ("Opening files; one moment please")
            rslt$(1) = "REQUIRED"
            call "OPENCHCK" (#01, f1%(1), f2%(1), 0%, rslt$(1))
            if f1%(1) < 0% then exit_program

L03390:     call "OPENFILE" (#02, mode$, f2%(2), rslt$(2), " ")
            if f2%(2) =  0% then L03540 /* Successful open */
            if f2%(2) > 99% then L03460
                call "OPENFILE" (#02, "OUTPT", f2%(2), rslt$(2), " ")
                close #02
                goto L03390

L03460:         convert f2%(2) to stat$, pic (###)
                u3% = 0%
                call "ASKUSER" (u3%, "*** HNYQUAN FILE ERROR ***",       ~
                     "An error has occurred attempting to open HNYQUAN", ~
                     "Note status: " & stat$ & "  " & rslt$(2),          ~
                     "Press (RETURN) to exit program")
                goto exit_program

L03540:     call "OPENCHCK" (#03, f1%(3), f2%(3), 0%, rslt$(3))
            call "OPENCHCK" (#04, f1%(4), f2%(4), 0%, rslt$(4))
            call "OPENCHCK" (#05, f1%(5), f2%(5), 0%, rslt$(5))

            if admin% = 0% then L09000
                call "OPENCHCK" (#10, f1%(10), f2%(10), 100%, rslt$(10))
                call "OPENCHCK" (#11, f1%(11), f2%(11), 100%, rslt$(11))
                call "OPENCHCK" (#13, f1%(13), f2%(13),   0%, rslt$(13))
                call "OPENCHCK" (#20, f1%(20), f2%(20),   0%, rslt$(20))
L03630:         call "OPENFILE" (#21, mode$, f2%(21), rslt$(21), " ")
                  if f2%(21) =  0% then L09000 /* Successful open */
                  if f2%(21) > 99% then L03700
                  call "OPENFILE" (#21, "OUTPT", f2%(21), rslt$(21), " ")
                    close #21
                    goto L03630

L03700:         convert f2%(21) to stat$, pic (###)
                u3% = 0%
                call "ASKUSER" (u3%, "*** SERMASTR FILE ERROR ***",      ~
                     "An error has occurred attempting to open SERMASTR",~
                     "Note status: " & stat$ & "  " & rslt$(2),          ~
                     "Press (RETURN) to exit program")
                goto exit_program

L09000: REM *************************************************************~
            *    I N I T I A L I Z A T I O N   F O R   P R O G R A M    *~
            * --------------------------------------------------------- *~
            * Sets up variables for program.                            *~
            *************************************************************

            date$ = date : call "DATEFMT" (date$)
            call "DATEOK" (date$, today%, errormsg$)
				blank_date$ = " "
				call "DATUFMTC" ( blank_date$ )
            call "EXTRACT" addr ("ID", userid$)
            ll% = 6%      /* Default Lot Length */

            edtmessage$ = "To Modify Displayed Values, Position Cursor" &~
                          " to Desired Line And Press (RETURN)."
            continue$ = "Editing these fields is not allowed."

*        Set up variables for pool items screen.
            init(hex(00)) edttran$
            init(hex(01)) str(edttran$,  2)
            init(hex(02)) str(edttran$, 15)
            init(hex(03)) str(edttran$, 26)
            init(hex(04)) str(edttran$, 37)

            phdr$(1) = "  Quantity"
            phdr$(2) = " Cost Each"
            phdr$(3) = "Trans Date"
            phdr$(4) = "Transaction Text"

            maxpools% = dim(porig(),1)

*        And set up some variables.
            acctmsg$( 1) = "Purchases Source Account"
            acctmsg$( 2) = "Work in Process Account"
            acctmsg$( 3) = "Inventory Asset Account"
            acctmsg$( 4) = "Cost of Goods Sold Account"
            acctmsg$( 5) = "Sales account"
            acctmsg$( 6) = "Inventory Adjustment Acct"
            acctmsg$( 7) = "Variance Account #1"
            acctmsg$( 8) = "Variance Account #2"
            acctmsg$( 9) = "Variance Account #3"
            acctmsg$(10) = "Variance Account #4"
            acctmsg$(11) = "Variance Account #5"
            acctmsg$(12) = "Variance Account #6"
            acctmsg$(13) = "Variance Account #7"
            acctmsg$(14) = "Variance Account #8"
            acctmsg$(15) = "Variance Account #9"
            acctmsg$(16) = "Variance Account #10"
            acctmsg$(17) = "Variance Account #11"
            acctmsg$(18) = "Variance Account #12"

            costdescr$( 1) = "Average Cost"
            costdescr$( 2) = "Mod. Average Cost"
            costdescr$( 3) = "Fixed Cost"
            costdescr$( 4) = "Last Cost"
            costdescr$( 5) = "Manual Cost"
            costdescr$( 6) = "Actual Value, FIFO"
            costdescr$( 7) = "Actual Value, LIFO"
            costdescr$( 8) = "Standard LIFO"
            costdescr$( 9) = "Standard FIFO"
            costdescr$(10) = "Actual LIFO/Adj Acct"
            costdescr$(11) = "Actual FIFO/Adj Acct"

            gosub init_enables

            readkey$ = "VF1:HNYQUAN"
            call "READ100" (#04, readkey$, vf%)
            if vf% = 1% then last_screen% = 5% else last_screen% = 4%

            call "COMPNAME" (12%, company$, u3%)
            call "TIME" (runtime$)
            rline% = 857%

*        Get Cost Bucket Descriptors
            call "READ100" (#04, "STC.CONTROL", f1%(4))
            if f1%(4) = 1% then L09390
L09380:         setid$ = "--NONE--"
                goto L10000
L09390:     get #04 using L09395, setid$
L09395:         FMT POS(28), CH(8)
            if setid$ = " " then L09380
            readkey$ = "STC.HDR." & setid$
            call "READ100" (#04, readkey$, f1%(4))
            if f1%(4) = 1% then get #04 using L09430, buckets%,            ~
                                        bucket_id$(), bucket_descr$()
L09430:         FMT POS(59), BI(1), 12*CH(10), 12*CH(20)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.  Note that   *~
            * if we are not in full edit mode only the key fields are   *~
            * input here (i.e., the record must exist).                 *~
            *************************************************************

        inputmode
            gosub L29000
*             Find out if full editing is to be allowed or not.
                 call "CMSMACHK" ("HNY", lfac$(1), lfac$(2))
                 if lfac$(1) = "Y" or lfac$(2) = "Y" then admin% = 1%    ~
                                                     else admin% = 0%

            for fieldnr% = 1% to 3%     /* Key fields - Screen 0      */
L10060:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                     if enabled% = 0% then L10125
L10075:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  4% then       L10111
L10090:                   fieldnr% = max(1%, fieldnr% - 1%)
                          gosub'051(fieldnr%)
                          if enabled% = 1% then L10075
                          if fieldnr% = 1% then L10060
                          goto L10090
L10111:              if keyhit%  =  8% then       L10125
                     if keyhit%  = 16% then       exit_program
                     if keyhit% <>  0% then       L10075
L10125:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then L10075
            next fieldnr%

            for fieldnr% = 1% to 6%      /* Quantity fields- Screen 1 */
L10165:         gosub'054(fieldnr%, 1%)
                     if enabled% = 0% then L10235
L10180:         gosub'104(fieldnr%, 1%)
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  4% then       L10220
L10195:                   fieldnr% = max(1%, fieldnr% - 1%)
                          gosub'054(fieldnr%, 1%)
                          if enabled% = 1% then L10180
                          if fieldnr% = 1% then L10165
                          goto L10195
L10220:              if keyhit%  =  6% then       editpg1
                     if keyhit% <>  0% then       L10180
L10235:         gosub'154(fieldnr%)
                      if errormsg$ <> " " then L10180
            next fieldnr%

            for fieldnr% = 1% to 13%       /* Cost fields- Screen 2    */
L10275:         gosub'055(fieldnr%, 1%)
                     if enabled% = 0% then L10345
L10290:         gosub'105(fieldnr%, 1%)
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  4% then       L10330
L10305:                   fieldnr% = max(1%, fieldnr% - 1%)
                          gosub'055(fieldnr%, 1%)
                          if enabled% = 1% then L10290
                          if fieldnr% = 1% then L10275
                          goto L10305
L10330:              if keyhit%  =  6% then       editpg1
                     if keyhit% <>  0% then       L10290
L10345:         gosub'155(fieldnr%)
                     if errormsg$ <> " " then L10290
            next fieldnr%

            for fieldnr% = 1% to 18%      /* G/L Accounts- Screen 3   */
L10385:         gosub'052(fieldnr%, 1%)
                      if enabled% = 0 then L10455
L10400:         gosub'102(fieldnr%, 1%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4% then       L10440
L10415:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%, 1%)
                         if enabled% = 1% then L10400
                         if fieldnr% = 1% then L10385
                         goto L10415
L10440:               if keyhit%  =  6 then       editpg1
                      if keyhit% <>  0 then       L10400
L10455:         gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10400
            next fieldnr%

            for fieldnr% = 1% to 5%       /* Misc. fields */
L10495:         gosub'053(fieldnr%, 1%)
                      if enabled% = 0 then L10565
L10510:         gosub'103(fieldnr%, 1%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4% then       L10550
L10525:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'053(fieldnr%, 1%)
                         if enabled% = 1% then L10510
                         if fieldnr% = 1% then L10495
                         goto L10525
L10550:               if keyhit%  =  6 then       editpg1
                      if keyhit% <>  0 then       L10510
L10565:         gosub'153(fieldnr%)
                      if errormsg$ <> " " then L10510
            next fieldnr%

*        Variable fields
            call "VFINPSUB" ("HNYQUAN ", "I",                            ~
                "Inventory Display- Input Variable Fields",              ~
                "Part: " & part$ & " Store: " & store$ & " Lot: " &      ~
                lot$, "NN", variable$, keyhit%)
            if keyhit% =  1% then inputmode

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1:     /* Quantities Screen                              */
            lastfieldnr% = 0%
            gosub'104(0%, 2%)           /* Display Screen - No Entry   */
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  3% then       editpg5
                if keyhit%  =  5% then       editpg2
                if keyhit%  =  8% then gosub test_balance
                if keyhit%  = 16% then       datasave
                if keyhit%  = 20% then       poolinput
                if keyhit% <> 28% then       L11170
                     if maxlines% >= 2% then gosub verify_consolidation
L11170:         if keyhit%  = 25% then gosub edit_text
                if keyhit%  = 29% then       L11200
                if keyhit% <>  0% then       editpg1
L11200:     if admin% = 0% then editpg1
L11210:     fieldnr% = cursor%(1) - 5%
            if fieldnr% < 1% or fieldnr% > 6% then editpg1
            if fieldnr% = lastfieldnr% then editpg1
            if keyhit% <> 29% then L11270
                gosub'049(1%, fieldnr%)
                goto editpg1
L11270:     gosub'054(fieldnr%, 2%)     /* Check Enables, Set Defaults */
                if enabled% = 0% then       editpg1
L11290:     gosub'104(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then L11290
            gosub'154(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " " then L11290
                lastfieldnr% = fieldnr%
                goto L11210

        editpg2:          /* Costs Screen                              */
            lastfieldnr% = 0%
            gosub'105(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       editpg1
                  if keyhit%  =  3% then       editpg5
                  if keyhit%  =  4% then       editpg1
                  if keyhit%  =  5% then       editpg3
                  if keyhit%  =  8% then gosub test_balance
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 20% then       poolinput
                  if keyhit% <> 28% then       L11500
                      if maxlines% >= 2% then gosub verify_consolidation
L11500:           if keyhit%  = 25% then gosub edit_text
                  if keyhit%  = 29% then       L11530
                  if keyhit% <>  0% then       editpg2
L11530:     if admin% = 0% then editpg2
L11540:     fieldnr% = cursor%(1) - 4%
            if fieldnr% > 2% then fieldnr% = fieldnr% - 1%
            if fieldnr% < 1% or fieldnr% > 13% then editpg2
            if fieldnr% = lastfieldnr% then editpg2
            if keyhit% <> 29% then L11610
                gosub'049(2%, fieldnr%)
                goto editpg2
L11610:     gosub'055(fieldnr%, 2%)     /* Check Enables, Set Defaults */
                if enabled% = 0% then       editpg2
L11630:     gosub'105(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then L11630
            gosub'155(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " " then L11630
                     lastfieldnr% = fieldnr%
                     goto L11540

        editpg3:     /* G/L Accounts                                   */
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       editpg1
                  if keyhit%  =  3% then       editpg5
                  if keyhit%  =  4% then       editpg2
                  if keyhit%  =  5% then       editpg4
                  if keyhit%  =  8% then gosub test_balance
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 20% then       poolinput
                  if keyhit% <> 28% then       L11840
                     if maxlines% >= 2% then gosub verify_consolidation
L11840:           if keyhit%  = 25% then gosub edit_text
                  if keyhit%  = 29% then       L11870
                  if keyhit% <>  0% then       editpg3
L11870:     fieldnr% = cursor%(1) - 5%
            if fieldnr% <  1% then editpg3
            if fieldnr% <= 6% then L11930
                fieldnr% = cursor%(1) - 7%
                if fieldnr% < 7% or fieldnr% > 12% then editpg3
                if cursor%(2) > 40% then fieldnr% = fieldnr% + 6%
L11930:     if fieldnr% = lastfieldnr% then editpg3
            if keyhit% <> 29% then L11970
                gosub'049(3%, fieldnr%)
                goto editpg3
L11970:     gosub'052(fieldnr%, 2%)     /* Check Enables, Set Defaults */
                if enabled% = 0% then editpg3
L11990:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%  =  1 then gosub startover
                if keyhit% <>  0 then L11990
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " " then L11990
                lastfieldnr% = fieldnr%
                goto L11870

        editpg4:     /* Miscellaneous Fields                           */
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       editpg1
                  if keyhit%  =  3% then       editpg5
                  if keyhit%  =  4% then       editpg3
                  if keyhit%  =  5% then       editpg5
                  if keyhit%  =  8% then gosub test_balance
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 20% then       poolinput
                  if keyhit% <> 28% then       L12200
                     if maxlines% >= 2% then gosub verify_consolidation
L12200:           if keyhit%  = 25% then gosub edit_text
                  if keyhit%  = 29% then       L12230
                  if keyhit% <>  0% then       editpg4
L12230:     fieldnr% = cursor%(1) - 5%
            if fieldnr% < 1% or fieldnr% > 5% then editpg4
            if fieldnr% = lastfieldnr% then editpg4
            if keyhit% <> 29% then L12290
                gosub'049(4%, fieldnr%)
                goto editpg4
L12290:     gosub'053(fieldnr%, 2%)     /* Check Enables, Set Defaults */
                if enabled% = 0% then editpg4
L12310:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then L12310
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12310
                  lastfieldnr% = fieldnr%
                  goto L12230

        editpg5
          if last_screen% = 4% then editpg4
            call "VFINPSUB" ("HNYQUAN ", "E",                            ~
                "Inventory Display- Edit Variable Fields",               ~
                "Part: " & part$ & " Store: " & store$ & " Lot: " &      ~
                lot$, "YN", variable$, keyhit%)
            if keyhit% =  1% then inputmode
            if keyhit% =  4% then editpg4
            if keyhit% = 16% then datasave
            goto editpg5

        verify_consolidation:
            if mode$ = "SHARE" and ser_flag% = 1% then return
L12510:     keyhit% = 2%
            call "ASKUSER" (keyhit%, "*** VERIFY ***",                   ~
                "Consolidation of LIFO/FIFO pools has been requested",   ~
                "Press RETURN to continue consolidation,",               ~
                "Press PF(1) to return to edit mode.")
            if keyhit%  = 1% then return
            if keyhit% <> 0% then L12510
                gosub combine_pools
                return

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

        edit_text
            call "TXTINSUB" (#14, f2%(14), "019",                        ~
                "Part: " & part$ & " Store: " & store$ & " Lot: " &      ~
                lot$, textid$, text$())
            return

        cost_method_lookup:
            costtypedescr$=" "
            if pos("ABFLMPRSTXY" = costtype$) = 0 then return
            costtypedescr$ = costdescr$(pos("ABFLMPRSTXY" = costtype$))
            return

        average_pools:
*        Determine the total pool qty (PCTQTY) and the average inventory
*        costs of the pools (PCTCOSTS(), total in PCTCOST).
            pctqty, pctorig, pctcost = 0    /* Total Pool Qty, Orig   */
            mat pctcosts = zer              /* Average Costs          */
            mat pccosts = zer
            if maxlines% = 0% then return
                for l% = 1% to maxlines%
                     pcqty = 0
                     convert pqty$(l%) to pcqty, data goto L13250
L13250:              pctqty  = pctqty  + pcqty
                     pctorig = pctorig + porig(l%)
                     get str(pcosts$(l%)) using L13280, pccosts()
L13280:                   FMT 12*PD(14,4)
                     for  x% = 1% to 12%
                          pctcosts(x%) = pctcosts(x%) +                  ~
                                         (pcqty * pccosts(x%))
                     next x%
                next l%

                pctqty = round(pctqty,2)
                if pctqty <> 0 then goto L13390
                     mat pctcosts = zer
                     goto L13430
L13390:         for  x% = 1% to 12%
                     pctcosts(x%) = round(pctcosts(x%) / pctqty, 4)
                     pctcost = pctcost + pctcosts(x%)
                next x%
L13430:         return


        REM *************************************************************~
            *              I N P U T   L I N E   I T E M S              *~
            * --------------------------------------------------------- *~
            * Input Pool entries.                                       *~
            *************************************************************
        poolinput
            if poolloaded% = 1% then goto pooledit
            if mode$ = "SHARE" and ser_flag% = 1% then editpg1
            maxlines%, screenline%, currentline% , line% = 0%
            ins% = 1%
L14100:     screenline% = screenline%  + 1%
            currentline%, c% = currentline% + 1%
            if pos("ABFLM" = costtype$) > 0% and currentline% > 1%       ~
                                                            then pooledit
            if currentline% > 500% then pooledit
            if screenline% <= 15% then L14230
                screenline% = 1%
                line% = line% + 15%

L14190:     perr$ = " "
L14230:     for poolnum% = 1% to 4%
                gosub'163(poolnum%, 1%)
                     if enabled% = 0% then L14320
L14260:         gosub'203(poolnum%)
                     if keyhit%  =  1% then gosub startover
                     if keyhit%  =  2% then gosub columnone
                     if keyhit%  =  4% then gosub lineabove
                     if keyhit%  = 16% and poolnum% = 1% then pooledit
                     if keyhit% <>  0% then       L14260
L14320:         gosub'253(poolnum%)
                     if perr$ <> " " then L14260
            next poolnum%

            maxlines% = maxlines% + 1%
            goto L14100

        REM *************************************************************~
            *               E D I T   P O O L   I T E M S               *~
            * --------------------------------------------------------- *~
            * EDITS POOL ITEMS.                                         *~
            *************************************************************

        pooledit
            if maxlines% > 0% then poolloaded% = 1%
            if poolloaded% = 0% then editpg1
            ins%, line%, currentline%, screenline% = 0%
            perr$ = " "
L14500:     gosub'213(0%)      /* Display */
                if keyhit%  =  0% then       L14670
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  2% then line% = 0%
                if keyhit%  =  3% then line% = max(0, maxlines% - 15)
                if keyhit%  =  4% then line% = max(0, line% - 15)
                if keyhit%  =  5% then line% = min(line% + 15,           ~
                                                 max(0, maxlines% - 15))
                if keyhit%  =  6% then line% = max(0, line% - 1)
                if keyhit%  =  7% then line% = min(line% + 1,            ~
                                                 max(0, maxlines% - 15))
                if keyhit%  =  9% then       editpg1
                if keyhit%  = 11% then gosub insertmode
                if keyhit%  = 12% then gosub deletemode
                if keyhit% <> 28% then       L14640
                     if maxlines% >= 2% then gosub verify_consolidation
L14640:         if keyhit%  = 16% then       datasave
                goto L14500

L14670
*        Figure out which line and field.
            screenline% = max(0%, cursor%(1) - 4%)
            if screenline% = 0% then L14500
            currentline%, c% = screenline% + line%
            if currentline% > maxlines% then L14500
            poolnum% = val(str(edttran$, cursor%(2)))
            if poolnum% = 0% then L14820    /* Lazy operator loop       */
                pinfo$ = " "
                gosub'163(poolnum%, 2%)
                     if enabled% = 0% then L14500
L14750:         gosub'213(poolnum%)        /* Now get modification     */
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  0% then L14750
                gosub'253(poolnum%)
                     if perr$ <> " " then L14750
                     goto L14500

L14820:     if mode$ = "SHARE" and ser_flag% = 1% then L14500
            for poolnum% = 1% to 4%      /* LOOP THROUGH ALL FIELDS    */
                pinfo$ = " "
                gosub'163(poolnum%, 2%)
L14840:         gosub'213(poolnum%)      /* Get modification           */
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  0% then       L14840
                gosub'253(poolnum%)
                     if perr$ <> " " then L14840
            next poolnum%
            goto L14500

        REM *************************************************************~
            *        C O L U M N   O N E ,   L I N E   A B O V E        *~
            *-----------------------------------------------------------*~
            * Column one key and line above key functions handled here. *~
            *************************************************************

        columnone
            c% = currentline%
            init(" ") pqty$(c%), pcost$(c%), pinfo$, perr$, ptext$(c%),  ~
                      ptext$(c%), pdate$(c%), pacct1$(c%), pacct2$(c%)
            init(hex(00)) pcosts$(c%)
            porig(c%) = 0
            return clear
            goto L14190

        lineabove:
          if currentline% = 1% then return
            c% = currentline%
            on poolnum% gosub L15140,     /* Quantity                   */~
                                   ,     /* Costs                      */~
                              L15150,     /* Posting Date               */~
                              L15160      /* Posting Text               */
            return
L15140:                       pqty$ (c%) = pqty$ (c%-1) : return
L15150:                       pdate$(c%) = pdate$(c%-1) : return
L15160:                       ptext$(c%) = ptext$(c%-1) : return

        REM *************************************************************~
            *              I N S E R T   M O D E   C O D E              *~
            *-----------------------------------------------------------*~
            * Handles insertion of a line item.                         *~
            *************************************************************

        insertmode
            if mode$ = "SHARE" and ser_flag% = 1% then return
            if pos("ABFLM" = costtype$) > 0% then return
            if maxlines%  = 500% then return     /* Array full, can't  */

*        Set CURRENTLINE%, SCREENLINE%
            ins% = 1%
            screenline% = max(1, cursor%(1) - 4)
            if line% + screenline% < maxlines% then L15330
                screenline% = maxlines% - line%    /* To ins at end    */
L15330:     if screenline% < 15% then L15360        /* Bottom of page   */
                line% = line% + 1%
                screenline% = 14%
L15360:     currentline%, c% = screenline% + line%

*        Copy all the elements past the insert point down one
            if c% >= maxlines% then L15510
                for temp% = maxlines% to c% step -1%
                     pqty$  (temp%+1) = pqty$  (temp%)
                     porig  (temp%+1) = porig  (temp%)
                     pcost$ (temp%+1) = pcost$ (temp%)
                     pcosts$(temp%+1) = pcosts$(temp%)
                     pdate$ (temp%+1) = pdate$ (temp%)
                     ptext$ (temp%+1) = ptext$ (temp%)
                     pacct1$(temp%+1) = pacct1$(temp%)
                     pacct2$(temp%+1) = pacct2$(temp%)
                next temp%

L15510:     screenline% = screenline% + 1%
            c%, currentline% = currentline% + 1%

            pqty$(c%), pcost$(c%), pdate$(c%), ptext$(c%), pacct1$(c%),  ~
                       pacct2$(c%) = " "
            init(hex(00)) pcosts$(c%)
            porig(c%) = 0

*        Now input the line, enable cancel out option
            pinfo$ = " "
            for poolnum% = 1% to 4%
                gosub'163(poolnum%, 1%)
                     if enabled% = 0% then L15710
L15660:         gosub'223(poolnum%)
                     if keyhit%  =  1% then L15780     /* END INSERT*/
                     if keyhit% <>  0% then L15660
                gosub'253(poolnum%)
                     if perr$ <> " " then L15660
L15710:     next poolnum%

            maxlines%  = maxlines% + 1%
            ins% = 0%
            goto insertmode

L15780
*        This routine aborts insert mode and destroys SCREENLINE%
            c% = currentline%
            ins% = 0%
            if currentline% <= maxlines% then gosub L15930

            temp% = maxlines% + 1%
            init (" ") pqty$(temp%), pcost$(temp%), perr$, pinfo$,       ~
                       ptext$(temp%), pdate$(temp%), pacct1$(temp%),     ~
                       pacct2$(temp%)
            init(hex(00)) pcosts$(temp%)
            porig(temp%) = 0

            if currentline% >= maxlines% and screenline% = 15            ~
                                         then line% = max(0%, line%- 1%)
            return

L15930:     for temp% = currentline% to maxlines%
                pqty$  (temp%) = pqty$  (temp%+1)
                porig  (temp%) = porig  (temp%+1)
                pcost$ (temp%) = pcost$ (temp%+1)
                pcosts$(temp%) = pcosts$(temp%+1)
                ptext$ (temp%) = ptext$ (temp%+1)
                pacct1$(temp%) = pacct1$(temp%+1)
                pacct2$(temp%) = pacct2$(temp%+1)
                pdate$ (temp%) = pdate$ (temp%+1)
            next temp%
            return

        REM *************************************************************~
            *              D E L E T E   M O D E   C O D E              *~
            *-----------------------------------------------------------*~
            * Deletes a line item.                                      *~
            *************************************************************

        deletemode
            if mode$ = "SHARE" and ser_flag% = 1% then return
            if maxlines% = 0% then return
            screenline%  = cursor%(1) - 4%
            if screenline% < 1% then return
            currentline%, c% = screenline% + line%
            if currentline% > maxlines% then return

L16180:     gosub'233(screenline%)
                if keyhit%  =  1% then return
                if keyhit% <>  0% then L16180

            c% = currentline%
            if currentline% < maxlines% then gosub L15780
                                         /* ACTUALLY DELETE LINE @ C%  */
            temp% = maxlines%
            init(" ") pqty$(temp%), pcost$(temp%), ptext$(temp%), perr$, ~
                      pinfo$, pdate$(temp%), pacct1$(temp%),             ~
                      pacct2$(temp%)
            init(hex(00)) pcosts$(temp%)
            porig(temp%)= 0

            maxlines% = maxlines% - 1%
            if maxlines% > 0% then return
                poolloaded% = 0%
                return clear
                goto poolinput

        REM *************************************************************~
            *        W R I T E   D A T A   T O   T H E   F I L E        *~
            *-----------------------------------------------------------*~
            * Actually, this only calls the routine that writes data.   *~
            *************************************************************
        datasave
*        If we are in 'HNYEDT' mode then just save part of HNYQUAN
            if admin% = 1% then L19110
                gosub hnyedt_save
                goto  inputmode

L19110
*        First, make sure the Pool Quantity = Quantity On-hand.
            gosub out_of_balance

*        Now Save Data to disk
            call "SHOSTAT" ("Saving Quantity and Pool Records")
            gosub print_report
            gosub save_hnyquan
            gosub save_pools
            gosub save_serial_numbers
            goto  inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20140,         /* Part Number        */    ~
                              L20190,         /* Store Code         */    ~
                              L20240          /* Lot Number         */
            return

L20140
*        Def/Enable PART NUMBER                 PART$
            inpmessage$ = "Enter Part Number (enter partial or blank to"&~
                          " see parts on file)."
            return

L20190
*        Def/Enable Store Code                  STORE$
            inpmessage$ = "Enter Store Code (enter partial or blank to" &~
                          " see stores on file)."
            return

L20240
*        Def/Enable Lot Number                  LOT$
            inpmessage$ = "Enter Lot Number.  Press PF-8 to see lots"  & ~
                          " on file for this Part / Store."
            call "LOTENABL" (part$, le%, ll%, #04, #01)
            if le% =  0% then lot_msg$ = "Part does not use lot numbers."
            if le% =  1% then lot_msg$ = "Lot number not required."
            if le% =  2% then lot_msg$ = "Part requires lot numbers."
            if le% <> 0% then return
                plowkey$ = str(part$) & str(store$)
                call "PLOWNEXT" (#02, plowkey$, 28%, enabled%)
                return

        REM *************************************************************~
            * D E F A U L T / E N A B L E   F O R   A C C O U N T S     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  3  of Input. *~
            *************************************************************

        deffn'052(fieldnr%, edit%)
            inpmessage$ = " "
            enabled% = 1%
            call "ENABLSUB" ("SET", "HNYQIPUT", scr%(), set%(), 3%,      ~
                             fieldnr%, edit%, enabled%)

*        Def/Enable Inventory G/L accounts
            inpmessage$ = "Enter a G/L account # for the " &             ~
                          acctmsg$(fieldnr%) & " or partial to see list"
            return

        REM *************************************************************~
            * D E F A U L T / E N A B L E   F O R   M I S C   I N F O   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  4  of Input. *~
            *************************************************************

        deffn'053(fieldnr%, edit%)
            inpmessage$ = " "
            call "ENABLSUB" ("SET", "HNYQIPUT", scr%(), set%(), 4%,      ~
                                               fieldnr%, edit%, enabled%)
            on fieldnr% gosub L24160,         /* Bin Location       */    ~
                              L24200,         /* Minimum Stock      */    ~
                              L24240,         /* Maximum Stock      */    ~
                              L24280,         /* Expiration Date    */    ~
                              L24320          /* Potency            */
            return

L24160
*        Bin Location                           BINLOC$
            inpmessage$ = "Enter the part's Primary Bin/Location"
            return

L24200
*        Minimum Stock                          MINSTOK$
            inpmessage$ = "Enter the Minimum Stock Level"
            return

L24240
*        Maximum Stock                          MAXSTOK$
            inpmessage$ = "Enter the Maximum Stock Level"
            return

L24280
*        Expiration Date                        DATEXPR$
            inpmessage$ = "Enter the date this lot expires"
            return

L24320
*        Potency                                POTENCY$
            inpmessage$ = "Enter Lot Potency Percentage in decimal format~
        ~ (eg. 98% = .98 or 104% = 1.04)"
            return

        REM *************************************************************~
            * D E F A U L T / E N A B L E   F O R   Q U A N T I T I E S *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'054(fieldnr%, edit%)
            inpmessage$ = " "
            enabled% = 1%
            if mode$ = "SHARE" and ser_flag% = 1% then enabled% = 0%

            if enabled% = 0% then return
            call "ENABLSUB" ("SET", "HNYQIPUT", scr%(), set%(), 1%,      ~
                                               fieldnr%, edit%, enabled%)
            on fieldnr% gosub L25170,         /* Qty On-Hand        */    ~
                              L25210,         /* Qty Back Ordered   */    ~
                              L25250,         /* Qty On Order       */    ~
                              L25290,         /* Qty Committed      */    ~
                              L25330,         /* Qty In QC          */    ~
                              L25370          /* Qty Pending        */
            call "STRING" addr("LJ", qty$(fieldnr%), 12%)
            return

L25170
*        Qty On-Hand
            inpmessage$ = "Enter the Quantity On-hand"
            return

L25210
*        Qty Back-Ordered  (Open Sales Orders)
            inpmessage$ = "Enter the Quantity on Open Sales Orders."
            return

L25250
*        Qty On Order (Open POs)
            inpmessage$ = "Enter the Quantity on Open Purchase Orders."
            return

L25290
*        Qty committed (Number in Insane Asylums)
            inpmessage$ = "Enter the Quantity Committed or Issued."
            return

L25330
*        Qty in QC
            inpmessage$ = "Enter the Quantity in Quality Control"
            return

L25370
*        Qty Pending
            inpmessage$ = "Enter the Quantity Pending Withdrawal"
            return

        REM *************************************************************~
            * D E F A U L T / E N A B L E   F O R   C O S T S           *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'055(fieldnr%, edit%)
            inpmessage$ = " "
            call "ENABLSUB" ("SET", "HNYQIPUT", scr%(), set%(), 2%,      ~
                                               fieldnr%, edit%, enabled%)
            if fieldnr% = 1% then L26140          /* Cost method        */
            if fieldnr% > 1% then L26190          /* Labor cost         */
            return

L26140
*        Cost Method
            inpmessage$ = "Enter the Cost Method code: A, B, F, L, M," & ~
                          " P, R, S, T, X, or Y."
            return

L26190
*        Inventory Cost
            if edit% = 2% and cost(fieldnr% - 1%) >= 0.0001 then L26200
            if fieldnr% - 1% <= buckets% then L26200
                   enabled% = 0%
                   return
L26200:     inpmessage$ = "Enter the Inventory Cost for " &              ~
                                               bucket_id$(fieldnr% - 1%)
            if cost(fieldnr% - 1%) = 0 then                              ~
                   cost(fieldnr% - 1%) = std(fieldnr% - 1%)
            call "CONVERT" (cost(fieldnr% - 1%), -4.4,                   ~
                   cost$(fieldnr% - 1%))
            return

        REM *************************************************************~
            * D E F A U L T / E N A B L E   F O R   P O O L   L I N E S *~
            * --------------------------------------------------------- *~
            * Set messages and enables for pool fields.                 *~
            *************************************************************

            deffn'163(poolnum%, edit%)
                enabled% = 1%
                on poolnum%  gosub  L28140,         /* Quantity         */~
                                    L28180,         /* Inventory Cost   */~
                                    L28250,         /* Posting date     */~
                                    L28330          /* Posting text     */
                     return

L28140
*        CUURRENT QTY
            if mode$ = "SHARE" and ser_flag% = 1% then enabled% = 0%
            pinfo$ = "Enter current quantity in pool."
            return

L28180
*        INVENTORY COST
            pinfo$ = " "
            if edit% <> 1% then return
                put str(pcosts$(currentline%)) using L28220, cost()
L28220:              FMT 12*PD(14,4)
                return

L28250
*        TRANS DATE
            pinfo$ = "Enter transaction Date."
            if pdate$(currentline%) <> " " then return
                pdate$ (currentline%) = date$
                pacct1$(currentline%) = acct$(3)
                pacct2$(currentline%) = acct$(6)
                return

L28330
*        TRANS TEXT
            pinfo$ = "Enter transaction text."
            return

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *************************************************************

            init (" ") part$, lot$, cost$(), partdescr$, lot_msg$,       ~
                       qty$(), errormsg$, inpmessage$, variable$,        ~
                       minimum$, maximum$, acct$(), acctdescr$(),        ~
                       pacct1$(), pacct2$(), store$, strdescr$, bin$,    ~
                       ptext$(), pdate$(),costtype$, costtypedescr$,     ~
                       oldcosttype$, pqty$(), pcost$(), potdate$,        ~
                       datexpr$, userexpr$, userpot$, potency$,          ~
                       exprdate$, exprsav1$, exprsav2$, textid$, text$()
            init (hex(00)) pcosts$()
            potency, potsave = 1
            maxlines%, poolloaded% = 0%
            mat qty   = zer
            mat porig = zer
            mat cost  = zer
            mat std   = zer
            startpipquantity = 0
            call "TXTFUTIL" (#14, f2%(14), "INTL", textid$)
            textid$ = all(hex(ff))
            return

        REM *************************************************************~
            *         I N I T I A L I Z E   E N A B L E S               *~
            * --------------------------------------------------------- *~
            * Initialize Soft Enable Settings.                          *~
            *************************************************************
        init_enables
*        Define Screen, Field Cross Ref and Field Enable Settings.
            mat set% = con   : mat set% = (99%) * set%
            mat scr% = zer
            scr%(1, 1) =  1% : set%( 1) =  2%      /* Qty On Hand      */
            scr%(1, 2) =  2% : set%( 2) =  2%      /* Open Sales Order */
            scr%(1, 3) =  3% : set%( 3) =  2%      /* Qty on Order     */
            scr%(1, 4) =  4% : set%( 4) =  2%      /* Qty Committed    */
            scr%(1, 5) =  5% : set%( 5) =  2%      /* Qty in QC        */
            scr%(1, 6) =  6% : set%( 6) =  2%      /* Qty pending wdwl */

            scr%(2, 1) = 21% : set%(21) =  2%      /* Costing Method   */
            scr%(2, 2) = 22% : set%(22) =  2%      /* HNYQUAN Costs    */
            scr%(2, 3) = 23% : set%(23) =  2%      /*                  */
            scr%(2, 4) = 24% : set%(24) =  2%      /*                  */
            scr%(2, 5) = 25% : set%(25) =  2%      /*                  */
            scr%(2, 6) = 26% : set%(26) =  2%      /*                  */
            scr%(2, 7) = 27% : set%(27) =  2%      /*                  */
            scr%(2, 8) = 28% : set%(28) =  2%      /*                  */
            scr%(2, 9) = 29% : set%(29) =  2%      /*                  */
            scr%(2,10) = 30% : set%(30) =  2%      /*                  */
            scr%(2,11) = 31% : set%(31) =  2%      /*                  */
            scr%(2,12) = 32% : set%(32) =  2%      /*                  */
            scr%(2,13) = 33% : set%(33) =  2%      /*                  */

            scr%(3, 1) = 41% : set%(41) =  2%      /* Accounts         */
            scr%(3, 2) = 42% : set%(42) =  2%      /*                  */
            scr%(3, 3) = 43% : set%(43) =  2%      /*                  */
            scr%(3, 4) = 44% : set%(44) =  2%      /*                  */
            scr%(3, 5) = 45% : set%(45) =  2%      /*                  */
            scr%(3, 6) = 46% : set%(46) =  2%      /*                  */
            scr%(3, 7) = 47% : set%(47) =  2%      /*                  */
            scr%(3, 8) = 48% : set%(48) =  2%      /*                  */
            scr%(3, 9) = 49% : set%(49) =  2%      /*                  */
            scr%(3,10) = 50% : set%(50) =  2%      /*                  */
            scr%(3,11) = 51% : set%(51) =  2%      /*                  */
            scr%(3,12) = 52% : set%(52) =  2%      /*                  */
            scr%(3,13) = 53% : set%(53) =  2%      /*                  */
            scr%(3,14) = 54% : set%(54) =  2%      /*                  */
            scr%(3,15) = 55% : set%(55) =  2%      /*                  */
            scr%(3,16) = 56% : set%(56) =  2%      /*                  */
            scr%(3,17) = 57% : set%(57) =  2%      /*                  */
            scr%(3,18) = 58% : set%(58) =  2%      /*                  */

            scr%(4, 1) = 60% : set%(60) =  2%      /* Bin Location     */
            scr%(4, 2) = 61% : set%(61) =  2%      /* Min Stock Level  */
            scr%(4, 3) = 62% : set%(62) =  2%      /* Max Stock Level  */
            scr%(4, 4) = 63% : set%(63) =  2%      /* Expiration Date  */
            scr%(4, 5) = 64% : set%(64) =  2%      /* Lot Potency      */

            call "ENABLSUB" ("INIT", "HNYQIPUT", scr%(), set%(),         ~
                                                         0%, 0%, 0%, 0%)
            return


        deffn'049(s%, f%)      /* Screen and Field Numbers             */
            if admin% <> 1% then return            /* Not Authorized   */
            call "ENABLSUB" ("MODIFY", "HNYQIPUT", scr%(), set%(),       ~
                                                          s%, f%, 0%, 0%)
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the user the ability to start over when he wants to *~
            * else return to the menu.  Notice that he has to push 2    *~
            * different buttons to start over--a little harder.         *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
                call "SERSTOVR" (1%, "6", " ",  #21, #22)
                return clear all
                if ser_flag% = 0% or admin% = 0% then inputmode
                   readkey$ = xor readkey$
                   str(readkey$,,42) = "IP" & str(serkey$,,40)
                   call "DELETE" (#20, readkey$, 42%)  /* CLEAR SERTIF */
                goto inputmode

        REM *************************************************************~
            *   L O A D   O L D   Q U A N T I T Y   F R O M   F I L E   *~
            *-----------------------------------------------------------*~
            * Loads detail from quantity record and from lifo/fifo pool *~
            *************************************************************

        dataload
            plowkey$ = str(part$,,25) & str(store$,,3) & lot$
            call "READ100" (#02, plowkey$, f1%(2))
            if f1%(2) = 1% then L30200
                for n% = 1% to 18%   /* No HNYQUAN- get default values */
                     p% = n%
                     call "HNYGLGET" (part$, store$, lot$, acct$(n%),    ~
                        p%, #01, #02)
                next n%
                get #01 using L30160, bin$, costtype$
L30160:              FMT POS(155), CH(8), POS(307), CH(1)
                minimum$ = "0" : maximum$ = "0"
                goto L30460

L30200:     get #02 using L30250,                                         ~
                     part$, store$, lot$, bin$, qty(), origcost, cost(), ~
                     minimum$, maximum$, acct$(), costtype$, datexpr$,   ~
                     potency, textid$, variable$, userexpr$, exprdate$,  ~
                     userpot$, potdate$
L30250:         FMT POS(17), CH(25), CH(3), CH(6), XX(10), CH(8),        ~
                    19*PD(14,4), 2*CH(10), 18*CH(9), CH(1), CH(6),       ~
                    PD(14,4), CH(4), CH(200), CH(3), CH(6), CH(3), CH(6)
            origqty = qty(1)
            if admin% = 0% then L30440
                if ser_flag% = 0% then L30440
                serkey$  = str(store$,,3%) & lot$
                call "SHOSTAT" ("Loading Serial Number Info")
                gosub build_sertif
                call "SERLOAD"                                           ~
                       (1%,              /* Line Item Pointer.         */~
                        "IP",            /* Source Transaction Type    */~
                        serkey$,         /* Source Transaction Key     */~
                        1%,              /* # Trans to Create File for */~
                        " ",             /* Load from WIP only those   */~
                        " ",             /* S/N's that are in this Loc.*/~
                        #04,             /* SYSFILE2 UFB               */~
                        #20,             /* SERTIF   UFB               */~
                        #21,             /* SERMASTR UFB               */~
                        #22, u3%)        /* SERWORK  UFB               */

L30440:     call "TXTFUTIL" (#14, f2%(14), "LOAD", textid$)

L30460:     for n% = 1% to 6%
                call "CONVERT" (qty(n%), 2.2, qty$(n%))
            next n%
            for n% = 1% to 12%
                call "CONVERT" (cost(n%), 4.4, cost$(n%))
                if cost(n%) = 0 and bucket_id$(n%) = " " then            ~
                                                          cost$(n%) = " "
            next n%
            for n% = 1% to 18%
                if n% = 5% and acct$(5%) = " " then goto L30580
                call "GLFMT" (acct$(n%))
                call "GETCODE" (#05, acct$(n%), acctdescr$(n%), 0%, 99,  ~
                                                                  f1%(5))
L30580:     next n%
            exprsav1$, exprsav2$ = datexpr$ : call "DATEFMT" (datexpr$)
            call "DATEFMT" (exprdate$)
            call "DATEFMT" (potdate$)
            potsave = potency
            call "CONVERT" (potency, -2.4, potency$)
            gosub cost_method_lookup

*        Load Pool Records
            if admin% = 0% then return
            poolkey$ = str(part$) & str(store$) &                        ~
                                            str(lot$,1,6) & hex(00000000)
            poolloaded%, i%,  maxlines% = 0%
            startpipquantity = 0

L30730:     call "PLOWNEXT" (#10, poolkey$, 34%, f1%(10))
            if f1%(10) = 0% then return
                i%, maxlines% = i% + 1%
                if maxlines% > maxpools% then too_many_pools
                poolloaded% = 1%
                get #10 using L30810, pqty, porig(i%), pcost, pcosts$(i%),~
                                     pdate$(i%), pacct1$(i%),pacct2$(i%),~
                                     ptext$(i%)
L30810:             FMT POS(39), 3*PD(14,4), CH(96), CH(6), 2*CH(9),     ~
                        CH(40)
                call "CONVERT" (pqty,  2.2, pqty$(i%))
                call "CONVERT" (pcost, 4.4, pcost$(i%))
                call "DATEFMT" (pdate$(i%))
                call "GLFMT" (pacct1$(i%))
                call "GLFMT" (pacct2$(i%))
                startpipquantity = startpipquantity + pqty
                goto L30730

        too_many_pools
            convert maxpools% to temp$, pic(####)
L30930:     keyhit% = 2%
            call "ASKUSER" (keyhit%, "*** W A R N I N G ***",            ~
                     "This part has more than" & temp$ & " pool records",~
                     "Press PF(1) to return to input mode, or",          ~
                     "Press PF(28) to consolidate pools to one record.")
            if keyhit% <> 1% then L31010
                return clear all
                goto inputmode
L31010:     if keyhit% <> 28% then L30930
            toomanypools% = 1%

        combine_pools:
            if toomanypools% = 0% then gosub average_pools

            init (" ") pqty$(), pcost$(), pdate$(), ptext$(), pacct1$(), ~
                       pacct2$()
            init (hex(00)) pcosts$()
            mat porig = zer
            poolloaded%, i%, line%, maxlines% = 0%
            startpipquantity = 0

            if toomanypools% = 1% then L31180
                if pctqty = 0 then return
                goto L31410

L31180:     poolkey$ = str(part$) & str(store$) & str(lot$) &            ~
                                                            hex(00000000)
            pctqty, pctorig, pctcost = 0
            toomanypools%, maxlines% = 0%
            mat pctcosts = zer

L31240:     call "PLOWNEXT"(#10, poolkey$, 34%, f1%(10))
            if f1%(10) = 0% then L31350
                get #10 using L31270, pcqty, pcorig, pccosts()
L31270:              FMT XX(38), 2*PD(14,4), XX(8), 12*PD(14,4)
                pctqty  = pctqty  + pcqty    /* Current Total Qty      */
                pctorig = pctorig + pcorig   /* Total Original Qty     */
                for  x% = 1% to 12%          /* Total Costs            */
                     pctcosts(x%) = pctcosts(x%) + (pccosts(x%)*pcqty)
                next x%
                goto L31240

L31350:     if pctqty = 0 then return    /* Nets to zero      */
                for  x%  = 1% to 12%
                     pctcosts(x%) = round(pctcosts(x%)/pctqty, 4)
                     pctcost = pctcost + pctcosts(x%)
                next x%

L31410:     poolloaded%, i%, maxlines% = 1%
            startpipquantity = pctqty
            porig(1) = pctorig
            put str(pcosts$(1)) using L31450, pctcosts()
L31450:         FMT 12*PD(14,4)
            call "CONVERT" (pctqty , 2.2, pqty$ (1))
            call "CONVERT" (pctcost, 4.4, pcost$(1))
            pcost      = pctcost
            pdate$ (1) = date$
            pacct1$(1) = acct$(3)
            pacct2$(1) = acct$(6)
            ptext$ (1) = "COMBINED POOLS"
            return


        REM *************************************************************~
            * B U I L D   S E R T I F   F I L E   F O R   S E R I A L # *~
            *************************************************************
        build_sertif

            plowkey$ = xor plowkey$
            str(plowkey$,,25) = str(part$)
L31835:     call "PLOWNEXT" (#21, plowkey$, 25%, f1%(21))
                if f1%(21) = 0% then return
            get #21 using L31850, stat1$, location$, stat2$, location2$
L31850:         FMT CH(1), CH(30), POS(258), CH(1), CH(30)

            if stat1$ = "2" and                                          ~
                 str(location$,,3) = store$ and                          ~
                 str(location$,4,6) = lot$ then write_sertif

            if stat1$ = "7" and stat2$ = "2" and                         ~
                 str(location2$,,3) = store$ and                         ~
                 str(location2$,4,6) = lot$ then write_sertif

            if stat1$ > hex(40) and stat2$ = "2" and                     ~
                 str(location2$,,3) = store$ and                         ~
                 str(location2$,4,6) = lot$ then write_sertif

            goto L31835

        write_sertif:
            get #21 using L31940, location$      /* SERIAL NUMBER */
L31940:         FMT POS(32), CH(20)
            readkey$ = "IP" & str(serkey$,,40) & str(location$,20)
            call "READ101" (#20, readkey$, f1%(20))
                if f1%(20) = 1% then delete #20

            put #20 using L31970, "IP", str(serkey$,,40), location$,      ~
                                 part$, 0, " "
L31970:         FMT CH(2), CH(40), CH(20), CH(25), PD(14,4), CH(5)
            write #20
            goto L31835


        REM *************************************************************~
            *              S A V E   D A T A   S E C T I O N            *~
            * --------------------------------------------------------- *~
            * Write HNYQUAN & HNYPOOL Records Back to Disk.             *~
            *************************************************************
        save_hnyquan:
            newcost = 0
            call "PACKZERO" (cost(), packed$)
            for x% = 1% to 12% : newcost = newcost + cost(x%) : next x%
            for n% = 1% to 18%
                if acct$(n%) <> " " then call "GLUNFMT" (acct$(n%))
            next n%
            call "DATUNFMT" (exprdate$)
            call "DATUNFMT" (potdate$)
            call "DATUNFMT" (datexpr$)
            if datexpr$ <> exprsav1$ then gosub write_pipout
            convert potency$ to potency, data goto L32170
L32170:     readkey$ = str(part$) & str(store$) & lot$
            call "READ101" (#02, readkey$, f1%(2))
            put #02, using L32230, lot$, part$, store$, lot$, bin$,       ~
                    qty(), newcost, packed$, minimum$, maximum$, acct$(),~
                    costtype$, datexpr$, potency, textid$, variable$,    ~
                    userexpr$, exprdate$, userpot$, potdate$, " "
L32230:         FMT CH(16), CH(25), CH(3), CH(16), CH(8), 7*PD(14,4),    ~
                    CH(96), 2*CH(10), 18*CH(9), CH(1), CH(6), PD(14,4),  ~
                    CH(4), CH(200), CH(3), CH(6), CH(3), CH(6), CH(11)
            if f1%(2) = 1% then rewrite #02 else write #02
            call "TXTFUTIL" (#14, f2%(14), "TOS2", textid$)
            return

        write_pipout:
            call "HNYEXSUB" (lot$, store$, part$, datexpr$, qty(1))
            return

        save_pools
*        Adjust PIPMASTR with any changes (note- must have just averaged
*        pools)
            if str(store$,,1) < "0" or str(store$,,1) > "9" then L32540

            spq  = round(pctqty - startpipquantity, 2%)
            spq% = sgn(spq) * int(abs(spq))
            if spq% = 0% then L32540
                call "READ101" (#11, part$, f1%(11))  /* PIPMASTR */
                if f1%(11) = 0% then L32540
                     get #11 using L32450, pip%(), pipamt()
L32450:                   FMT POS(27), 490*BI(4), 4*PD(14,4)
                     for temp% = 1% to 490%
                          pip%(temp%) = pip%(temp%) + spq%
                     next temp%
                     pipamt(1) = round(pipamt(1) + spq, 2)
                     put #11 using L32450, pip%(), pipamt()
                     rewrite #11
                     call "PIPFLAGS" (part$, 1%, 1%, 0, #11, #13)

L32540:     poolkey$ = str(part$) & str(store$) & str(lot$) &            ~
                                                            hex(00000000)
            call "DELETE" (#10, poolkey$, 34%)
            if maxlines% = 0% then return
            k% = 1%
            if pos("ABFML" = oldcosttype$) <> 0% then L32640
            if pos("RXS"   = oldcosttype$) <> 0% and                     ~
               pos("PYT"   = costtype$)    <> 0% then k% = -1%
            if pos("PYT"   = oldcosttype$) <> 0% and                     ~
               pos("RXS"   = costtype$)    <> 0% then k%= -1%
L32640:     for i% = 1 to maxlines%
                j% = 9999% + (i% * k%)
                pqty = 0
                convert pqty$(i%) to pqty, data goto L32680
L32680:         porig(i%) = round(porig(i%), 2)
                pqty      = round(pqty, 2)
                pcost     = 0
                get str(pcosts$(i%)) using L32720, pccosts()
L32720:              FMT 12*PD(14,4)
                for x% = 1% to 12% : pcost = pcost + pccosts(x%) : next x%
                call "PACKZERO" (pccosts(), pcosts$(i%))
                call "GLUNFMT" (pacct1$(i%))
                call "GLUNFMT" (pacct2$(i%))
                call "DATUNFMT" (pdate$(i%))

                write #10 using L32920,                                   ~
                     part$,              /* Part Number                */~
                     store$,             /* Store                      */~
                     lot$,               /* Lot                        */~
                     j%,                 /* Reverse Seq Number         */~
                     pqty,               /* Quantity Left in Pool      */~
                     porig(i%),          /* Original Quantity          */~
                     pcost,              /* Total Cost                 */~
                     pcosts$(i%),        /* Costs                      */~
                     pdate$(i%),         /* Transaction Date           */~
                     pacct1$(i%),        /* Assets Account             */~
                     pacct2$(i%),        /* Adjustment Account         */~
                     ptext$(i%),         /* Posting text               */~
                     " "                 /* Filler                     */
L32920:                   FMT CH(25), CH(3), CH(6), BI(4), 3*PD(14,4),   ~
                              CH(96), CH(6), 2*CH(9), CH(40), CH(78)
            next i%
            return

        save_serial_numbers:
*          READKEY$ = STR(STORE$) & LOT$
            call "SERSAVE"                                               ~
                       (1%,              /* Line Item Pointer.         */~
                        "IP",            /* Source Transaction Type    */~
                        serkey$,         /* Source Transaction Key     */~
                        1%,              /* # Trans to Create File for */~
                        part$,           /* Part Code                  */~
                        userid$,         /* Current User ID            */~
                        "2",             /* Change Status to ...       */~
                        "6",             /* Change Status from ...     */~
                        1%,              /* Clear TIF after Save (YES) */~
                        #04,             /* SYSFILE2 UFB               */~
                        #20,             /* SERTIF UFB                 */~
                        #21,             /* SERMASTR UFB               */~
                        #22)             /* SERWORK  UFB               */
            return

        hnyedt_save
            plowkey$ = str(part$) & str(store$) & lot$
            call "READ101" (#02, plowkey$, f1%(2))
            for n% = 1% to 18%
                if acct$(n%) <> " " then call "GLUNFMT" (acct$(n%))
            next n%
            call "DATUNFMT" (exprdate$)
            call "DATUNFMT" (datexpr$)
            call "DATUNFMT" (potdate$)
            convert potency$ to potency, data goto L33240
L33240:     put #02 using L33270, bin$, minimum$, maximum$, acct$(),      ~
                                datexpr$, potency, textid$, variable$,   ~
                                userexpr$, exprdate$, userpot$, potdate$
L33270:         FMT POS(61), CH(8), POS(221), 2*CH(10), 18*CH(9),        ~
                    POS(404), CH(6), PD(14,4), CH(4), CH(200), CH(3),    ~
                    CH(6), CH(3), CH(6)
            if f1%(2) <> 0% then rewrite #02
            return

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            * --------------------------------------------------------- *~
            * Print report show adjusted HNYQUAN records.               *~
            *************************************************************
        print_report
*        First Determine if a report line is required
            newcost = 0
            for x% = 1% to 12% : newcost = newcost + cost(x%) : next x%
            if origqty  - qty(1)  <> 0 then L34150
            if origcost - newcost <> 0 then L34150
            return

*        Do it to it!
            if rline% <> 857% then L34170
L34150:         select printer(134)
                call "SETPRNT" ("HNY003", " ", 0%, 0%)
L34170:     if rline% < 55% then L34290
                page% = page% + 1%    /* Page Heading */
                print page
                print using L34370, date$, runtime$, company$
                print using L34400, userid$, page%
                print
                print using L34420
                print using L34450
                print using L34480
                rline% = 6%

L34290:     print using L34510, part$, partdescr$, store$, lot$,          ~
                               origqty         , origcost,               ~
                               qty(1)          , newcost,                ~
                               qty(1) - origqty, newcost - origcost
            rline% = rline% + 1%
            select ws
            return

L34370: %RUN DATE: ######## ########            #########################~
        ~###################################                 HNYQIPUT-HNY0~
        ~03
L34400: %USER: ###                                DIRECT INVENTORY QUANTI~
        ~TY/COST ADJUSTMENT AUDIT LISTING                        PAGE: ##
L34420: %                                                                ~
        ~** O R I G I N A L **  *** C U R R E N T ***  **** C H A N G E **~
        ~**
L34450: %PART CODE                 PART DESCRIPTION         STORE  LOT   ~
        ~  QUANTITY  COST EACH    QUANTITY  COST EACH    QUANTITY  COST EA~
        ~CH
L34480: %------------------------- ------------------------ ----- ------ ~
        ~---------- ----------  ---------- ----------  ---------- --------~
        ~--
L34510: %######################### ########################  ###  ###### ~
        ~ -######.## -####.####  -######.## -####.####  -######.## -####.#~
        ~###

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Screen to input Key fields.                               *~
            *************************************************************

        deffn'101(fieldnr%)
            gosub set_pf1
            line2$ = " "
            str(line2$,62%) = "HNYQIPUT: " & str(cms2v$,,8%)
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()

            on fieldnr%  gosub  L40180,         /* Part Number       */   ~
                                L40180,         /* Store Code        */   ~
                                L40180          /* Lot Number        */
            goto L40210
                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02),                                               ~
                  "Directly Manage Part Quantities and Costs",           ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Part Number:",                               ~
               at (06,15), fac(lfac$( 1)), part$                , ch(25),~
               at (06,42), fac(hex(8c)),   partdescr$           , ch(34),~
               at (07,03), "Store Code:",                                ~
               at (07,15), fac(lfac$( 2)), store$               , ch(03),~
               at (07,42), fac(hex(8c)),   strdescr$            , ch(32),~
               at (08,03), "Lot Number:",                                ~
               at (08,15), fac(lfac$( 3)), str(lot$,,ll%),               ~
               at (08,42), fac(hex(8c)),   lot_msg$             , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfmsg$(1)            , ch(79),~
               at (23,02), fac(hex(8c)),   pfmsg$(2)            , ch(79),~
               at (24,02), fac(hex(8c)),   pfmsg$(3)            , ch(79),~
                     keys(pfkey$), key(keyhit%)

               if keyhit% <> 13 then L40490
                  call "MANUAL" ("HNYQIPUT")
                  goto L40210

L40490:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40210

        set_pf1
            pfmsg$(1) = "(1)Start Over    (4)Previous Field      " &     ~
                        "                       (13)Instructions"
            pfmsg$(2) = "                                        " &     ~
                        "                       (15)Print Screen"
            pfmsg$(3) = "(8)Find Existing Lots                   " &     ~
                        "                       (16)Exit Program"
            pfkey$ = hex(01ffff04ffffff08ffffffff0dff0f10ff00)
            if fieldnr%  > 1% then L40630
                str(pfmsg$(1),18,18) = " " : str(pfkey$, 4,1) = hex(ff)
L40630:     if fieldnr%  = 3% then L40650
                str(pfmsg$(3), 1,22) = " " : str(pfkey$, 7,1) = hex(ff)
L40650:     return

*        Just because its here, we'll define the standard PF keys
        set_pf_input
            pfmsg$(1) = "(1)Start Over    (4)Previous Field      " &     ~
                        "                       (13)Instructions"
            pfmsg$(2) = "                                        " &     ~
                        "                       (15)Print Screen"
            pfmsg$(3) = "                 (6)Proceed to Edit     " &     ~
                        "                                       "
            pfkey$ = hex(01ffff04ff06ffffffffffff0dff0fffff00)
            if fieldnr%  > 1% then L40780
                str(pfmsg$(1),18,18) = " " : str(pfkey$, 4,1) = hex(ff)
L40780:     return

        set_pf_display
            inpmessage$ = edtmessage$
            pfmsg$(1) = "(1)Start Over    (4)Previous Screen     " &     ~
                        "(20)Edit Pool Items    (13)Instructions"
            pfmsg$(2) = "(2)First Screen  (5)Next Screen         " &     ~
                        "(25)Manage Text        (15)Print Screen"
            pfmsg$(3) = "(3)Last Screen   (8)Check Status        " &     ~
                        "(28)Combine Pools      (16)Save Data   "
            pfkey$ = hex(0102030405ffff08ffffffff0dff0f1014191c1d00)
            if admin% = 1% then L40960
                str(pfmsg$(1),41,20) = " " : str(pfkey$,17,1) = hex(ff)
                str(pfmsg$(3),41,20) = " " : str(pfkey$,19,1) = hex(ff)
                str(pfmsg$(3),18,16) = " " : str(pfkey$, 8,1) = hex(ff)
                                             str(pfkey$,20,1) = hex(ff)
L40960:     if screen% <> 1% then L40990
                str(pfmsg$(2), 1,16) = " " : str(pfkey$, 2,1) = hex(ff)
                str(pfmsg$(1),18,20) = " " : str(pfkey$, 4,1) = hex(ff)
L40990:     if screen% <> last_screen% then L41020
                str(pfmsg$(3), 1,16) = " " : str(pfkey$, 3,1) = hex(ff)
                str(pfmsg$(2),18,20) = " " : str(pfkey$, 5,1) = hex(ff)
L41020:     return

        set_pf_edit
            pfmsg$(1) = "(1)Start Over                           " &     ~
                        "                       (13)Instructions"
            pfmsg$(2) = "                                        " &     ~
                        "                       (15)Print Screen"
            pfmsg$(3) = "                                        " &     ~
                        "                                       "
            pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
            gosub set_pf1
            line1$ = "EDIT G/L ACCOUNTS"
            str(line2$,,61%) = "Part: " & part$ & "  Store: " & store$ & ~
                                                  "  Lot: " & lot$
            screen%  = 3%
            if edit% = 1% then gosub set_pf_input
            if edit% = 2% and fieldnr% = 0% then gosub set_pf_display
            if edit% = 2% and fieldnr% > 0% then gosub set_pf_edit
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            if fieldnr% > 0% then lfac$(fieldnr%) = hex(81)


L42250:     accept                                                       ~
               at (01,02), "Manage Part Quantities/Costs",               ~
               at (01,35), fac(hex(84)), line1$,                         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Source Account- Purchasing",                 ~
               at (06,30), fac(lfac$( 1)), acct$( 1)            , ch(12),~
               at (06,49), fac(hex(8c)),  acctdescr$( 1)        , ch(32),~
                                                                         ~
               at (07,02), "Source Account- WIP",                        ~
               at (07,30), fac(lfac$( 2)), acct$( 2)            , ch(12),~
               at (07,49), fac(hex(8c)),  acctdescr$( 2)        , ch(32),~
                                                                         ~
               at (08,02), "Inventory Assets Account",                   ~
               at (08,30), fac(lfac$( 3)), acct$( 3)            , ch(12),~
               at (08,49), fac(hex(8c)),  acctdescr$( 3)        , ch(32),~
                                                                         ~
               at (09,02), "Cost of Sales Account",                      ~
               at (09,30), fac(lfac$( 4)), acct$( 4)            , ch(12),~
               at (09,49), fac(hex(8c)),  acctdescr$( 4)        , ch(32),~
                                                                         ~
               at (10,02), "Sales Distribution",                         ~
               at (10,30), fac(lfac$( 5)), acct$( 5)            , ch(12),~
               at (10,49), fac(hex(8c)),  acctdescr$( 5)        , ch(32),~
                                                                         ~
               at (11,02), "Inventory Adjustments",                      ~
               at (11,30), fac(lfac$( 6)), acct$( 6)            , ch(12),~
               at (11,49), fac(hex(8c)),  acctdescr$( 6)        , ch(32),~
                                                                         ~
               at (13,02), "Inventory Variance Accounts:",               ~
               at (14,02), " 1", at(15,02), " 2",  at (16,02), " 3",     ~
               at (17,02), " 4", at(18,02), " 5",  at (19,02), " 6",     ~
               at (14,42), " 7", at(15,42), " 8",  at (16,42), " 9",     ~
               at (17,42), "10", at(18,42), "11",  at (19,42), "12",     ~
               at (14,05), fac(lfac$( 7)), acct$( 7)            , ch(12),~
               at (15,05), fac(lfac$( 8)), acct$( 8)            , ch(12),~
               at (16,05), fac(lfac$( 9)), acct$( 9)            , ch(12),~
               at (17,05), fac(lfac$(10)), acct$(10)            , ch(12),~
               at (18,05), fac(lfac$(11)), acct$(11)            , ch(12),~
               at (19,05), fac(lfac$(12)), acct$(12)            , ch(12),~
               at (14,45), fac(lfac$(13)), acct$(13)            , ch(12),~
               at (15,45), fac(lfac$(14)), acct$(14)            , ch(12),~
               at (16,45), fac(lfac$(15)), acct$(15)            , ch(12),~
               at (17,45), fac(lfac$(16)), acct$(16)            , ch(12),~
               at (18,45), fac(lfac$(17)), acct$(17)            , ch(12),~
               at (19,45), fac(lfac$(18)), acct$(18)            , ch(12),~
               at (14,18), fac(hex(8c))  , acctdescr$( 7)       , ch(23),~
               at (15,18), fac(hex(8c))  , acctdescr$( 8)       , ch(23),~
               at (16,18), fac(hex(8c))  , acctdescr$( 9)       , ch(23),~
               at (17,18), fac(hex(8c))  , acctdescr$(10)       , ch(23),~
               at (18,18), fac(hex(8c))  , acctdescr$(11)       , ch(23),~
               at (19,18), fac(hex(8c))  , acctdescr$(12)       , ch(23),~
               at (14,58), fac(hex(8c))  , acctdescr$(13)       , ch(23),~
               at (15,58), fac(hex(8c))  , acctdescr$(14)       , ch(23),~
               at (16,58), fac(hex(8c))  , acctdescr$(15)       , ch(23),~
               at (17,58), fac(hex(8c))  , acctdescr$(16)       , ch(23),~
               at (18,58), fac(hex(8c))  , acctdescr$(17)       , ch(23),~
               at (19,58), fac(hex(8c))  , acctdescr$(18)       , ch(23),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg$(1)              , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg$(2)              , ch(79),~
               at (24,02), fac(hex(8c)), pfmsg$(3)              , ch(79),~
                     keys(pfkey$), key(keyhit%)
               errormsg$ = " "

               if keyhit% <> 13 then L42940
                  call "MANUAL" ("HNYQIPUT")
                  goto L42250

L42940:        if keyhit% <> 15 then L42980
                  call "PRNTSCRN"
                  goto L42250

L42980:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
            line1$ = "EDIT MISCELLANEOUS FIELDS"
            str(line2$,,61%) = "Part: " & part$ & "  Store: " & store$ & ~
                                                  "  Lot: " & lot$
            screen%  = 4%
            if edit% = 1% then gosub set_pf_input
            if edit% = 2% and fieldnr% = 0% then gosub set_pf_display
            if edit% = 2% and fieldnr% > 0% then gosub set_pf_edit
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr%  gosub  L44240,         /* Bin Location      */   ~
                                L44240,         /* Minimum stock     */   ~
                                L44240,         /* Maximum stock     */   ~
                                L44240,         /* Expiration Date   */   ~
                                L44240          /* Potency           */
            goto L44270
                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L44240:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L44270:     accept                                                       ~
               at (01,02), "Manage Part Quantities/Costs",               ~
               at (01,35), fac(hex(84)), line1$,                         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (06,02), "Primary Bin/Location",                       ~
               at (06,30), fac(lfac$( 1)), bin$                 , ch(08),~
                                                                         ~
               at (07,02), "Minimum Stock Level",                        ~
               at (07,30), fac(lfac$( 2)), minimum$             , ch(10),~
                                                                         ~
               at (08,02), "Maximum Stock Level",                        ~
               at (08,30), fac(lfac$( 3)), maximum$             , ch(10),~
                                                                         ~
               at (09,02), "Expiration Date",                            ~
               at (09,30), fac(lfac$( 4)), datexpr$             , ch(08),~
               at (09,39), "Last modified",                              ~
               at (09,53), fac(hex(8c)),   exprdate$            , ch(08),~
               at (09,62), "by"                                 ,        ~
               at (09,65), fac(hex(8c)),   userexpr$            , ch(03),~
                                                                         ~
               at (10,02), "Lot Potency",                                ~
               at (10,30), fac(lfac$( 5)), potency$             , ch(06),~
               at (10,39), "Last modified",                              ~
               at (10,53), fac(hex(8c)),   potdate$             , ch(08),~
               at (10,62), "by"                                 ,        ~
               at (10,65), fac(hex(8c)),   userpot$             , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfmsg$(1)            , ch(79),~
               at (23,02), fac(hex(8c)),   pfmsg$(2)            , ch(79),~
               at (24,02), fac(hex(8c)),   pfmsg$(3)            , ch(79),~
                   keys(pfkey$), key(keyhit%)

               if keyhit% <> 13 then L44690
                  call "MANUAL" ("HNYQIPUT")
                  goto L44270

L44690:        if keyhit% <> 15 then L44730
                  call "PRNTSCRN"
                  goto L44270

L44730:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   4                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'104(fieldnr%, edit%)
            line1$ = "EDIT QUANTITY FIELDS"
            str(line2$,,61%) = "Part: " & part$ & "  Store: " & store$ & ~
                                                  "  Lot: " & lot$
            screen%  = 1%
            if edit% = 1% then gosub set_pf_input
            if edit% = 2% and fieldnr% = 0% then gosub set_pf_display
            if edit% = 2% and fieldnr% > 0% then gosub set_pf_edit
            if admin% = 1% then goto L45170
                if edit% = 1% then goto L45170
                    line1$ = " "
                    inpmessage$ = continue$
                    init(hex(8c)) lfac$()
                    goto L45300
L45170:     if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr%  gosub  L45270,         /* Qty on-hand       */   ~
                                L45270,         /* Qty back ordered  */   ~
                                L45270,         /* Qty on order      */   ~
                                L45270,         /* Qty committed     */   ~
                                L45270,         /* Qty in QC         */   ~
                                L45270          /* Qty pending       */
              goto L45300
                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L45270:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L45300:     accept                                                       ~
               at (01,02), "Manage Part Quantities/Costs",               ~
               at (01,35), fac(hex(84)), line1$,                         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Quantity On Hand",                           ~
               at (06,30), fac(lfac$( 1)), qty$(1)              , ch(12),~
                                                                         ~
               at (07,02), "Open Sales Order",                           ~
               at (07,30), fac(lfac$( 2)), qty$(2)              , ch(12),~
                                                                         ~
               at (08,02), "Quantity On Order",                          ~
               at (08,30), fac(lfac$( 3)), qty$(3)              , ch(12),~
                                                                         ~
               at (09,02), "Quantity Committed/Issued",                  ~
               at (09,30), fac(lfac$( 4)), qty$(4)              , ch(12),~
                                                                         ~
               at (10,02), "Quantity In QC",                             ~
               at (10,30), fac(lfac$( 5)), qty$(5)              , ch(12),~
                                                                         ~
               at (11,02), "Qty Pending Withdrawal",                     ~
               at (11,30), fac(lfac$( 6)), qty$(6)              , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfmsg$(1)            , ch(79),~
               at (23,02), fac(hex(8c)),   pfmsg$(2)            , ch(79),~
               at (24,02), fac(hex(8c)),   pfmsg$(3)            , ch(79),~
                   keys(pfkey$), key(keyhit%)

               if keyhit% <> 13 then L45660
                  call "MANUAL" ("HNYQIPUT")
                  goto L45300

L45660:        if keyhit% <> 15 then L45700
                  call "PRNTSCRN"
                  goto L45300

L45700:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Enter / Edit Costs.                                       *~
            *************************************************************

        deffn'105(fieldnr%, edit%)
            line1$ = "EDIT INVENTORY COSTS"
            str(line2$,,61%) = "Part: " & part$ & "  Store: " & store$ & ~
                                                  "  Lot: " & lot$
            screen%  = 2%
            if edit% = 1% then gosub set_pf_input
            if edit% = 2% and fieldnr% = 0% then gosub set_pf_display
            if edit% = 2% and fieldnr% > 0% then gosub set_pf_edit
            if admin% = 1% then goto L46075
                if edit% = 1% then goto L46075
                    line1$ = " "
                    inpmessage$ = continue$
                    init(hex(8c)) lfac$()
                    goto L46095
L46075:     if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            if fieldnr% = 1% then gosub L46110     /* Cost method       */
            if fieldnr% > 1% then gosub L46115     /* Inventory Cost    */
L46095:     lfac$(13) = lfac$(13) or hex(20)   /* Underscore 12th cost */
            goto L46125
                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L46110:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L46115:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L46125:     totcost = 0
            for  x% = 1% to 12%
                totcost = totcost + cost(x%)
            next x%
            call "CONVERT" (totcost, 4.4, totcost$)

L46155:     accept                                                       ~
               at (01,02), "Manage Part Quantities/Costs",               ~
               at (01,35), fac(hex(84)), line1$,                         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Cost Method",                                ~
               at (05,30), fac(lfac$( 1)), costtype$            , ch(01),~
               at (05,49), fac(hex(8c)),   costtypedescr$       , ch(32),~
                                                                         ~
               at (06,02), "Inventory Costs:",                           ~
               at (06,49), "Current Cost Set = ",                        ~
               at (06,68), fac(hex(8c)),   setid$,                       ~
               at (07,04), " 1- ", at(08,04), " 2- ", at(09,04), " 3- ", ~
               at (10,04), " 4- ", at(11,04), " 5- ", at(12,04), " 6- ", ~
               at (13,04), " 7- ", at(14,04), " 8- ", at(15,04), " 9- ", ~
               at (16,04), "10- ", at(17,04), "11- ", at(18,04), "12- ", ~
               at (07,08), fac(hex(8c)),   bucket_id$   ( 1),            ~
               at (08,08), fac(hex(8c)),   bucket_id$   ( 2),            ~
               at (09,08), fac(hex(8c)),   bucket_id$   ( 3),            ~
               at (10,08), fac(hex(8c)),   bucket_id$   ( 4),            ~
               at (11,08), fac(hex(8c)),   bucket_id$   ( 5),            ~
               at (12,08), fac(hex(8c)),   bucket_id$   ( 6),            ~
               at (13,08), fac(hex(8c)),   bucket_id$   ( 7),            ~
               at (14,08), fac(hex(8c)),   bucket_id$   ( 8),            ~
               at (15,08), fac(hex(8c)),   bucket_id$   ( 9),            ~
               at (16,08), fac(hex(8c)),   bucket_id$   (10),            ~
               at (17,08), fac(hex(8c)),   bucket_id$   (11),            ~
               at (18,08), fac(hex(8c)),   bucket_id$   (12),            ~
               at (19,04), "** Total Cost **",                           ~
               at (19,30), fac(hex(8c)),   totcost$             , ch(12),~
               at (07,49), fac(hex(8c)),   bucket_descr$( 1),            ~
               at (08,49), fac(hex(8c)),   bucket_descr$( 2),            ~
               at (09,49), fac(hex(8c)),   bucket_descr$( 3),            ~
               at (10,49), fac(hex(8c)),   bucket_descr$( 4),            ~
               at (11,49), fac(hex(8c)),   bucket_descr$( 5),            ~
               at (12,49), fac(hex(8c)),   bucket_descr$( 6),            ~
               at (13,49), fac(hex(8c)),   bucket_descr$( 7),            ~
               at (14,49), fac(hex(8c)),   bucket_descr$( 8),            ~
               at (15,49), fac(hex(8c)),   bucket_descr$( 9),            ~
               at (16,49), fac(hex(8c)),   bucket_descr$(10),            ~
               at (17,49), fac(hex(8c)),   bucket_descr$(11),            ~
               at (18,49), fac(hex(8c)),   bucket_descr$(12),            ~
                                                                         ~
               at (07,30), fac(lfac$( 2)), cost$( 1)            , ch(12),~
               at (08,30), fac(lfac$( 3)), cost$( 2)            , ch(12),~
               at (09,30), fac(lfac$( 4)), cost$( 3)            , ch(12),~
               at (10,30), fac(lfac$( 5)), cost$( 4)            , ch(12),~
               at (11,30), fac(lfac$( 6)), cost$( 5)            , ch(12),~
               at (12,30), fac(lfac$( 7)), cost$( 6)            , ch(12),~
               at (13,30), fac(lfac$( 8)), cost$( 7)            , ch(12),~
               at (14,30), fac(lfac$( 9)), cost$( 8)            , ch(12),~
               at (15,30), fac(lfac$(10)), cost$( 9)            , ch(12),~
               at (16,30), fac(lfac$(11)), cost$(10)            , ch(12),~
               at (17,30), fac(lfac$(12)), cost$(11)            , ch(12),~
               at (18,30), fac(lfac$(13)), cost$(12)            , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfmsg$(1)            , ch(79),~
               at (23,02), fac(hex(8c)),   pfmsg$(2)            , ch(79),~
               at (24,02), fac(hex(8c)),   pfmsg$(3)            , ch(79),~
                     keys(pfkey$), key(keyhit%)

               if keyhit% <> 13 then L46500
                  call "MANUAL" ("HNYQIPUT")
                  goto L46155

L46500:        if keyhit% <> 15 then L46520
                  call "PRNTSCRN"
                  goto L46155

L46520:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *     L I N E   I T E M   H A N D L I N G   S C R E E N     *~
            *-----------------------------------------------------------*~
            * This screen handles input, edit, insert and delete modes  *~
            * for the pool entries.                                     *~
            *************************************************************

        deffn'203(poolnum%)                        /* Insert Mode      */
            pfmsg$(1) = "(1)Start Over    (4)Line Above          " &     ~
                        "                       (13)Instructions"
            pfmsg$(2) = "(2)Restart Line                         " &     ~
                        "                       (15)Print Screen"
            pfmsg$(3) = "                                        " &     ~
                        "                       (16)End Entry   "
            pfkey$ = hex(0102ff04ffffffffffffffff0dff0f10ff00)
            goto L47640

        deffn'213(poolnum%)                        /* Display & Edit   */
          if poolnum% <> 0% then L47300
            pfmsg$(1) = "(1)Start Over                ( 9)Header " &     ~
                        "                       (13)Instructions"
            pfmsg$(2) = "(2)First (4)Prev (6)Down     (11)Insert " &     ~
                        "                       (15)Print Screen"
            pfmsg$(3) = "(3)Last  (5)Next (7)Up       (12)Delete " &     ~
                        "  (28)Combine Pools    (16)Save Data   "
            pfkey$ = hex(01020304050607ff09ff0b0c0dff0f101c00)
            pinfo$ = "To EDIT, INSERT or DELETE, move cursor to line, t"&~
                "hen press (RETURN) or PF key."
            init(hex(8e)) fac$()
            goto L47740

L47300:     pfmsg$(1) = "(1)Start Over                           " &     ~
                        "                       (13)Instructions"
            pfmsg$(2) = "                                        " &     ~
                        "                       (15)Print Screen"
            pfmsg$(3) = "                                        " &     ~
                        "                                       "
            pfkey$ = hex(01ffffffffffffffffffffff0dff0fffff00)
            goto L47640

        deffn'223(poolnum%)                        /* Insert Mode      */
            pfmsg$(1) = "(1)Exit Insert Mode                     " &     ~
                        "                       (13)Instructions"
            pfmsg$(2) = "                                        " &     ~
                        "                       (15)Print Screen"
            pfmsg$(3) = "                                        " &     ~
                        "                                       "
            pfkey$ = hex(01ffffffffffffffffffffff0dff0fffff00)
            goto L47640

        deffn'233(screenline%)                     /* Delete Mode      */
            poolnum%  = 0%
            pfmsg$(1) = "(1)Exit Delete                          " &     ~
                        "                       (13)Instructions"
            pfmsg$(2) = "                                        " &     ~
                        "                       (15)Print Screen"
            pfmsg$(3) = "                                        " &     ~
                        "                                       "
            pfkey$ = hex(01ffffffffffffffffffffff0dff0fffff00)
            pinfo$ = "Press RETURN to delete line, PF-1 to exit delete."
            init(hex(8c)) fac$()
            for  temp% = 1% to 4%
                fac$(screenline%, temp%) = hex(94)
            next temp%
            goto L47740

L47640:     init(hex(8c)) fac$()
            on poolnum%  gosub      L47720,         /* Quantity         */~
                                    L47720,         /* Inventory Costs  */~
                                    L47710,         /* Posting date     */~
                                    L47700          /* Posting text     */
            goto L47740
L47700:              fac$(screenline%, poolnum%) = hex(80)  :  return
L47710:              fac$(screenline%, poolnum%) = hex(81)  :  return
L47720:              fac$(screenline%, poolnum%) = hex(82)  :  return

L47740:     line1$ = "LIFO/FIFO Pools"
            ncost = 0
            for x% = 1% to 12% : ncost = ncost + cost(x%) : next x%
            call "CONVERT" (qty(1), -0.2, quan$)
            call "CONVERT" (ncost , -2.4, ncost$)
            str(line2$,,61%) = part$ & "/" & store$ & "/" & lot$ & " (" &~
                               quan$ & " @ " & ncost$ & ")"

            if poolnum% <> 2% then L47820
                call "HNYCDIST" ("M", part$, partdescr$, str(line2$,,60),~
                                 #04, pcosts$(screenline% + line%),      ~
                                 pcost$(screenline% + line%), temp)
                keyhit% = 0%
                return

L47820:     accept                                                       ~
               at (01,02), "Manage Part Quantities/Costs",               ~
               at (01,35), fac(hex(84)), line1$,                         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), perr$                  , ch(79),~
                                                                         ~
               at (04,04), fac(hex(ac)), phdr$(1)                ,ch(10),~
               at (05,04), fac(fac$( 1,1)), pqty$    (line%+ 1)  ,ch(10),~
               at (06,04), fac(fac$( 2,1)), pqty$    (line%+ 2)  ,ch(10),~
               at (07,04), fac(fac$( 3,1)), pqty$    (line%+ 3)  ,ch(10),~
               at (08,04), fac(fac$( 4,1)), pqty$    (line%+ 4)  ,ch(10),~
               at (09,04), fac(fac$( 5,1)), pqty$    (line%+ 5)  ,ch(10),~
               at (10,04), fac(fac$( 6,1)), pqty$    (line%+ 6)  ,ch(10),~
               at (11,04), fac(fac$( 7,1)), pqty$    (line%+ 7)  ,ch(10),~
               at (12,04), fac(fac$( 8,1)), pqty$    (line%+ 8)  ,ch(10),~
               at (13,04), fac(fac$( 9,1)), pqty$    (line%+ 9)  ,ch(10),~
               at (14,04), fac(fac$(10,1)), pqty$    (line%+10)  ,ch(10),~
               at (15,04), fac(fac$(11,1)), pqty$    (line%+11)  ,ch(10),~
               at (16,04), fac(fac$(12,1)), pqty$    (line%+12)  ,ch(10),~
               at (17,04), fac(fac$(13,1)), pqty$    (line%+13)  ,ch(10),~
               at (18,04), fac(fac$(14,1)), pqty$    (line%+14)  ,ch(10),~
               at (19,04), fac(fac$(15,1)), pqty$    (line%+15)  ,ch(10),~
                                                                         ~
               at (04,15), fac(hex(ac)), phdr$(2)                ,ch(10),~
               at (05,15), fac(fac$( 1,2)), pcost$   (line%+ 1)  ,ch(10),~
               at (06,15), fac(fac$( 2,2)), pcost$   (line%+ 2)  ,ch(10),~
               at (07,15), fac(fac$( 3,2)), pcost$   (line%+ 3)  ,ch(10),~
               at (08,15), fac(fac$( 4,2)), pcost$   (line%+ 4)  ,ch(10),~
               at (09,15), fac(fac$( 5,2)), pcost$   (line%+ 5)  ,ch(10),~
               at (10,15), fac(fac$( 6,2)), pcost$   (line%+ 6)  ,ch(10),~
               at (11,15), fac(fac$( 7,2)), pcost$   (line%+ 7)  ,ch(10),~
               at (12,15), fac(fac$( 8,2)), pcost$   (line%+ 8)  ,ch(10),~
               at (13,15), fac(fac$( 9,2)), pcost$   (line%+ 9)  ,ch(10),~
               at (14,15), fac(fac$(10,2)), pcost$   (line%+10)  ,ch(10),~
               at (15,15), fac(fac$(11,2)), pcost$   (line%+11)  ,ch(10),~
               at (16,15), fac(fac$(12,2)), pcost$   (line%+12)  ,ch(10),~
               at (17,15), fac(fac$(13,2)), pcost$   (line%+13)  ,ch(10),~
               at (18,15), fac(fac$(14,2)), pcost$   (line%+14)  ,ch(10),~
               at (19,15), fac(fac$(15,2)), pcost$   (line%+15)  ,ch(10),~
                                                                         ~
               at (04,26), fac(hex(ac)), phdr$(3)                ,ch(10),~
               at (05,27), fac(fac$( 1,3)), pdate$   (line%+ 1)  ,ch(08),~
               at (06,27), fac(fac$( 2,3)), pdate$   (line%+ 2)  ,ch(08),~
               at (07,27), fac(fac$( 3,3)), pdate$   (line%+ 3)  ,ch(08),~
               at (08,27), fac(fac$( 4,3)), pdate$   (line%+ 4)  ,ch(08),~
               at (09,27), fac(fac$( 5,3)), pdate$   (line%+ 5)  ,ch(08),~
               at (10,27), fac(fac$( 6,3)), pdate$   (line%+ 6)  ,ch(08),~
               at (11,27), fac(fac$( 7,3)), pdate$   (line%+ 7)  ,ch(08),~
               at (12,27), fac(fac$( 8,3)), pdate$   (line%+ 8)  ,ch(08),~
               at (13,27), fac(fac$( 9,3)), pdate$   (line%+ 9)  ,ch(08),~
               at (14,27), fac(fac$(10,3)), pdate$   (line%+10)  ,ch(08),~
               at (15,27), fac(fac$(11,3)), pdate$   (line%+11)  ,ch(08),~
               at (16,27), fac(fac$(12,3)), pdate$   (line%+12)  ,ch(08),~
               at (17,27), fac(fac$(13,3)), pdate$   (line%+13)  ,ch(08),~
               at (18,27), fac(fac$(14,3)), pdate$   (line%+14)  ,ch(08),~
               at (19,27), fac(fac$(15,3)), pdate$   (line%+15)  ,ch(08),~
                                                                         ~
               at (04,37), fac(hex(ac)), phdr$(4)                ,ch(40),~
               at (05,37), fac(fac$( 1,4)), ptext$   (line%+ 1) , ch(40),~
               at (06,37), fac(fac$( 2,4)), ptext$   (line%+ 2) , ch(40),~
               at (07,37), fac(fac$( 3,4)), ptext$   (line%+ 3) , ch(40),~
               at (08,37), fac(fac$( 4,4)), ptext$   (line%+ 4) , ch(40),~
               at (09,37), fac(fac$( 5,4)), ptext$   (line%+ 5) , ch(40),~
               at (10,37), fac(fac$( 6,4)), ptext$   (line%+ 6) , ch(40),~
               at (11,37), fac(fac$( 7,4)), ptext$   (line%+ 7) , ch(40),~
               at (12,37), fac(fac$( 8,4)), ptext$   (line%+ 8) , ch(40),~
               at (13,37), fac(fac$( 9,4)), ptext$   (line%+ 9) , ch(40),~
               at (14,37), fac(fac$(10,4)), ptext$   (line%+10) , ch(40),~
               at (15,37), fac(fac$(11,4)), ptext$   (line%+11) , ch(40),~
               at (16,37), fac(fac$(12,4)), ptext$   (line%+12) , ch(40),~
               at (17,37), fac(fac$(13,4)), ptext$   (line%+13) , ch(40),~
               at (18,37), fac(fac$(14,4)), ptext$   (line%+14) , ch(40),~
               at (19,37), fac(fac$(15,4)), ptext$   (line%+15) , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   pinfo$               , ch(79),~
               at (22,02), fac(hex(8c)),   pfmsg$(1)            , ch(79),~
               at (23,02), fac(hex(8c)),   pfmsg$(2)            , ch(79),~
               at (24,02), fac(hex(8c)),   pfmsg$(3)            , ch(79),~
                     keys(pfkey$), key(keyhit%)

               if keyhit% <> 13 then L48680
                  call "MANUAL" ("HNYQIPUT")
                  goto L47820

L48680:        if keyhit% <> 15 then L48720
                  call "PRNTSCRN"
                  goto L47820

L48720:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *        O U T - O F - B A L A N C E   S C R E E N          *~
            * --------------------------------------------------------- *~
            * Get resolution of out-of-balance condition.               *~
            *************************************************************

        test_balance:     /* Display HNYPOOL/HNYQUAN Balances          */
            gosub check_balance
            line1$ = "BALANCE DISPLAY"
            pfmsg$(1) = "The Quantity Record does not balance wit" &     ~
                        "h the LIFO/FIFO Pool ! (13)Instructions"
            pfmsg$(2) = "Record(s).  This condition should be cor" &     ~
                        "rected as soon as    ! (15)Print Screen"
            pfmsg$(3) = "possible.                               " &     ~
                        "                     !                 "
            pfkey$ = hex(000d0f)
            if flag% = 0% then let str(pfmsg$(1),,63),str(pfmsg$(2),,63),~
                                   str(pfmsg$(3),,63) = " "
            inpmessage$ = "Press RETURN to continue..."
            goto L49275

        out_of_balance:
            gosub check_balance
            if flag% = 0% then return
            line1$ = "OUT-OF-BALANCE NOTICE"
            pfmsg$(1) = "(4)Modify Quantity Record   (14)Exit wit" &     ~
                        "hout Balancing         (13)Instructions"
            pfmsg$(2) = "(8)Modify LIFO/FIFO Pools   (16)Set Quan" &     ~
                        "tity Record = Pools    (15)Print Screen"
            pfmsg$(3) = "                                        " &     ~
                        "                                       "
            pfkey$ = hex(04080d0e0f10)
            inpmessage$ = "Enter PF-Key for Option Desired."
            goto L49275

        check_balance     /* Check Balance and determine differences   */
            gosub average_pools
            qdiff = 0
            flag% = 0%  :  init(hex(8c)) dfac$()
            if qty(1) = 0 and pctqty = 0 then return
            if abs(qty(1) - pctqty) < .01 then L49200
                flag% = 1% : dfac$(13) = hex(84)
L49200:     for  x% = 1% to 12%
                if abs(cost(x%)-pctcosts(x%)) < .001 then L49215
                     flag% = 1% : dfac$(x%) = hex(84)
L49215:     next x%
            dfac$(14) = dfac$(13) or hex(a0)
            if flag% > 0% then errormsg$ =                               ~
                "LIFO/FIFO Pools do not balance to the Quantity Record."
            str(line2$,,61%) = "Part: " & part$ & "  Store: " & store$ & ~
                                                  "  Lot: " & lot$
            for  x% = 1% to 12%
                pccosts(x%) = cost(x%) - pctcosts(x%)
            next x%
            qdiff = qty(1) - pctqty
            return

L49275: accept                                                           ~
         at (01,02), "Manage Part Quantities/Costs",                     ~
         at (01,35), fac(hex(84)), line1$,                               ~
         at (01,66), "Today:",                                           ~
         at (01,73), fac(hex(8c)), date$                        , ch(08),~
         at (02,02), fac(hex(ac)), line2$                       , ch(79),~
         at (04,02), fac(hex(9c)), errormsg$                    , ch(79),~
                                                                         ~
         at(10,02), "Quantity Record: ",                                 ~
         at(10,19), fac(dfac$(13)), qty(1), pic(-#######.##),            ~
         at(11,02), "LIFO/FIFO Pools: ",                                 ~
         at(11,19), fac(dfac$(14)), pctqty, pic(-#######.##),            ~
         at(12,02), "     Difference: ",                                 ~
         at(12,19), fac(dfac$(13)), qdiff , pic(-#######.##),            ~
                                                                         ~
         at(06,32), "--Cost Bucket-- -Quantity- ---Pool--- Difference",  ~
         at(07,32), " 1. ", fac(hex(8c)), bucket_id$( 1),                ~
         at(08,32), " 2. ", fac(hex(8c)), bucket_id$( 2),                ~
         at(09,32), " 3. ", fac(hex(8c)), bucket_id$( 3),                ~
         at(10,32), " 4. ", fac(hex(8c)), bucket_id$( 4),                ~
         at(11,32), " 5. ", fac(hex(8c)), bucket_id$( 5),                ~
         at(12,32), " 6. ", fac(hex(8c)), bucket_id$( 6),                ~
         at(13,32), " 7. ", fac(hex(8c)), bucket_id$( 7),                ~
         at(14,32), " 8. ", fac(hex(8c)), bucket_id$( 8),                ~
         at(15,32), " 9. ", fac(hex(8c)), bucket_id$( 9),                ~
         at(16,32), "10. ", fac(hex(8c)), bucket_id$(10),                ~
         at(17,32), "11. ", fac(hex(8c)), bucket_id$(11),                ~
         at(18,32), "12. ", fac(hex(8c)), bucket_id$(12),                ~
                                                                         ~
         at(07,48), fac(dfac$( 1)), cost( 1),            pic(-####.####),~
         at(08,48), fac(dfac$( 2)), cost( 2),            pic(-####.####),~
         at(09,48), fac(dfac$( 3)), cost( 3),            pic(-####.####),~
         at(10,48), fac(dfac$( 4)), cost( 4),            pic(-####.####),~
         at(11,48), fac(dfac$( 5)), cost( 5),            pic(-####.####),~
         at(12,48), fac(dfac$( 6)), cost( 6),            pic(-####.####),~
         at(13,48), fac(dfac$( 7)), cost( 7),            pic(-####.####),~
         at(14,48), fac(dfac$( 8)), cost( 8),            pic(-####.####),~
         at(15,48), fac(dfac$( 9)), cost( 9),            pic(-####.####),~
         at(16,48), fac(dfac$(10)), cost(10),            pic(-####.####),~
         at(17,48), fac(dfac$(11)), cost(11),            pic(-####.####),~
         at(18,48), fac(dfac$(12)), cost(12),            pic(-####.####),~
                                                                         ~
         at(07,59), fac(dfac$( 1)), pctcosts( 1),        pic(-####.####),~
         at(08,59), fac(dfac$( 2)), pctcosts( 2),        pic(-####.####),~
         at(09,59), fac(dfac$( 3)), pctcosts( 3),        pic(-####.####),~
         at(10,59), fac(dfac$( 4)), pctcosts( 4),        pic(-####.####),~
         at(11,59), fac(dfac$( 5)), pctcosts( 5),        pic(-####.####),~
         at(12,59), fac(dfac$( 6)), pctcosts( 6),        pic(-####.####),~
         at(13,59), fac(dfac$( 7)), pctcosts( 7),        pic(-####.####),~
         at(14,59), fac(dfac$( 8)), pctcosts( 8),        pic(-####.####),~
         at(15,59), fac(dfac$( 9)), pctcosts( 9),        pic(-####.####),~
         at(16,59), fac(dfac$(10)), pctcosts(10),        pic(-####.####),~
         at(17,59), fac(dfac$(11)), pctcosts(11),        pic(-####.####),~
         at(18,59), fac(dfac$(12)), pctcosts(12),        pic(-####.####),~
                                                                         ~
         at(07,70), fac(dfac$( 1)), pccosts( 1),         pic(-####.####),~
         at(08,70), fac(dfac$( 2)), pccosts( 2),         pic(-####.####),~
         at(09,70), fac(dfac$( 3)), pccosts( 3),         pic(-####.####),~
         at(10,70), fac(dfac$( 4)), pccosts( 4),         pic(-####.####),~
         at(11,70), fac(dfac$( 5)), pccosts( 5),         pic(-####.####),~
         at(12,70), fac(dfac$( 6)), pccosts( 6),         pic(-####.####),~
         at(13,70), fac(dfac$( 7)), pccosts( 7),         pic(-####.####),~
         at(14,70), fac(dfac$( 8)), pccosts( 8),         pic(-####.####),~
         at(15,70), fac(dfac$( 9)), pccosts( 9),         pic(-####.####),~
         at(16,70), fac(dfac$(10)), pccosts(10),         pic(-####.####),~
         at(17,70), fac(dfac$(11)), pccosts(11),         pic(-####.####),~
         at(18,70), fac(dfac$(12)), pccosts(12),         pic(-####.####),~
                                                                         ~
         at (21,02), fac(hex(ac)), inpmessage$                  , ch(79),~
         at (22,02), fac(hex(8c)), pfmsg$(1)                    , ch(79),~
         at (23,02), fac(hex(8c)), pfmsg$(2)                    , ch(79),~
         at (24,02), fac(hex(8c)), pfmsg$(3)                    , ch(79),~
                keys(pfkey$), key(keyhit1%)

            if keyhit1% <>  0% then L49665
                errormsg$ = " "
                return

L49665:     if keyhit1% <> 13% then L49685
                call "MANUAL" ("HNYQIPUT")
                goto L49275

L49685:     if keyhit1% <> 15% then L49705
                call "PRNTSCRN"
                goto L49275

L49705:     if keyhit1% <> 16% then L49770
                errormsg$ = " "
                qty(1) = pctqty            /* Set HNYQUAN = HNYPOOL    */
                call "CONVERT" (qty(1), 2.2, qty$(1))
                for  x% = 1% to 12%
                     cost(x%) = pctcosts(x%)
                     call "CONVERT" (cost(x%), 4.4, cost$(x%))
                     if cost(x%) = 0 and bucket_id$(x%) = " " then       ~
                                                          cost$(x%) = " "
                next x%
                gosub L55150    /* Serial Numbers */
                if errormsg$ = " " then return else editpg1

L49770:     if keyhit1% <> 14% then L49790
                errormsg$ = " "            /* Leave out of balance     */
                return

L49790:     if keyhit1% <>  4% then L49815
                return clear               /* Edit HNYQUAN             */
                errormsg$ = " "
                goto editpg1

L49815:     if keyhit1% <>  8% then L49275
                return clear               /* Edit HNYPOOL             */
                errormsg$ = " "
                goto poolinput

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50130,         /* Part Number       */     ~
                              L50300,         /* Store Code        */     ~
                              L50440          /* Lot Number        */
            return

L50130
*        Test for PART NUMBER                               PART$
            partdescr$ = hex(06) & "Select a Part Number"
            call "PLOWCODE" (#01, part$, partdescr$, 0%, 0.32, f1%(1))
            if f1%(1) = 1% then L50190
                errormsg$ = "Part Number not on file."
                return
L50190:     call "SERENABL" (part$, ser_flag%, u3%, #04, #01)
            if admin% = 0% then L50240
                if not(ser_flag% = 1% and mode$ = "SHARE") then L50230
                     call "ASKUSER" (0%, "SERIAL NUMBERED PART",         ~
                          "Serial Numbered Parts can only be Edited",    ~
                          "in EXCLUSIVE Mode.  Press a PF Key to ",      ~
                          "continue in a Non-Edit Mode.")
        REM          ADMIN% = 0%       /* TURN OFF ADMINISTRATOR EDIT */

L50230:     if admin% = 1% then return
L50240:         plowkey$ = part$
                call "PLOWNEXT" (#02, plowkey$, 25%, f1%(2))
                if f1%(2) = 1% then return
                     errormsg$ = "No quantity records exist for this part"
                     return

L50300
*        Test for STORE CODE                                STORE$
            strdescr$ = hex(06) & "Select a Store Number"
            call "PLOWCODE" (#03, store$, strdescr$, 0%, 0.30, f1%(3))
            if f1%(3) = 1% then L50360
                errormsg$ = "Store Number not on file."
                return
L50360:     if admin% = 1% then return
                plowkey$ = str(part$) & str(store$) & hex(00)
                call "PLOWNEXT" (#02, plowkey$, 28%, f1%(2))
                if f1%(2) = 1% then return
                     errormsg$ = "No quantity records exist for this" &  ~
                                 " Part & Store."
                     return

L50440
*        Test for LOT NUMBER                                LOT$
            if lot$ = "?" then keyhit% = 8%  /* Thanks LDJ !!!!! */
            if keyhit% <> 8% then L50530
                msg$ = hex(06) & "Select a Lot Number"
                plowkey$ = str(part$) & str(store$) & hex(00)
                call "PLOWCODE" (#02, plowkey$, msg$, 28%, 0, f1%(2))
                if f1%(2) = 1% then L50520
                     errormsg$ = "No Lots on File."
                     return
L50520:         lot$ = str(plowkey$,29,6)
L50530:     gosub dataload
            if f1%(2) = 0% then L50570
                return clear all
                goto editpg1
L50570:     if admin% = 1% then L50600
                errormsg$ = "No quantity record found for this lot."
                return
L50600:     call "LOTVALID"(part$, store$, lot$, #04, #01, #02, errormsg$)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "

*        Does lookup & validates G/L numbers
            if fieldnr% < 7% or acct$(fieldnr%) <> " " then L52121
            if fieldnr% > 18% then L52121
                acctdescr$(fieldnr%) = " "
                return
L52121:     if fieldnr% <> 5% or acct$(5%) <> " " then L52130
                acctdescr$(5%) = " " /* Sales acct may be blank */
                return
L52130:     msg$ = hex(06) & "Select an " & acctmsg$(fieldnr%)
            call "PLOWCODE" (#05, acct$(fieldnr%), msg$, 0%, 0.30, f1%(5))
            if f1%(5) = 1% then goto L52180
                errormsg$ = "Account Number Not On File"
                return
L52180:     acctdescr$(fieldnr%) = msg$
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************

        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L54150,         /* Bin Location      */     ~
                              L54180,         /* Minimum Stock     */     ~
                              L54220,         /* Maximum Stock     */     ~
                              L54260,         /* Expiration Date   */     ~
                              L54410          /* Potency           */
            return

L54150: REM Bin Location                          BINLOC$
            return

L54180: REM Minimum safety stock                  MINSTOK$
            call "NUMTEST" (minimum$, 0, 9999999.99, errormsg$, 2.2, 0)
            return

L54220: REM Maximum stock on-hand                 MAXSTOK$
            call "NUMTEST" (maximum$, 0, 9999999.99, errormsg$, 2.2, 0)
            return

L54260: REM Test for Date Expired                 DATEXPR$
            if datexpr$ = " " or ~
				  ~datexpr$ = blank_date$ then goto L54340
            call "DATEOK" (datexpr$, u3%, errormsg$)
            if errormsg$ <> " " then return
            if u3% > today% then goto L54330
                errormsg$ = "Expiration date must be a future date"
                return
L54330:     call "DATUNFMT" (datexpr$)
L54340:     if datexpr$ = exprsav2$ then goto L54380
                exprsav2$ = datexpr$
                userexpr$ = userid$
                exprdate$ = date : call "DATEFMT" (exprdate$)
L54380:     call "DATEFMT" (datexpr$)
            return

L54410: REM Test for Lot Potency                  POTENCY$
            call "NUMTEST" (potency$, 0, 9e7, errormsg$, 2.4, potency)
            if errormsg$ <> " " then return
            if potency > 0 then L54470
            errormsg$ = "Standard potency must be greater than zero."
            return
L54470:     if potency = potsave then return
                potsave = potency
                userpot$ = userid$
                potdate$ = date : call "DATEFMT" (potdate$)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 4.                      *~
            *************************************************************

        deffn'154(fieldnr%)
            errormsg$ = " "

*        Quantity fields
            call "NUMTEST" (qty$(fieldnr%), 0, 9999999.99, errormsg$,    ~
                                                     -2.2, qty(fieldnr%))
            if errormsg$ <> " " then return

            if fieldnr% <> 1% then return
L55150
*              CALL "SERENABL" (PART$, ENABLED%, U3%, #04, #01)
                     if ser_flag% = 0% then return
*              READKEY$ = STR(STORE$) & LOT$
                call "SERINSUB" (part$, store$, lot$, qty(1), 1%, 1%,    ~
                    "IP", serkey$, errormsg$, #04, #01, #21, #22)
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 5.                      *~
            *************************************************************

        deffn'155(fieldnr%)
            errormsg$ = " "
            if fieldnr% = 1% then L56110           /* Cost method       */~
                             else L56240           /* Cost Field        */

L56110
*        Cost Method
            if pos("ABFLMPRSTXY" = costtype$) = 0% then goto L56170
            if pos("ABFLM"       = costtype$) = 0% then goto L56150
            if maxlines% > 1% then goto L56200
L56150:         gosub cost_method_lookup
                if pos("STF" = costtype$) = 0% then return
                rqst% = 2% : temp = 0
                call "STCCOSTS" (part$, " ", #4, rqst%, temp, std())
                return
L56170:     errormsg$ = "Valid cost methods are A, B, F, L, M, P, R, S,"&~
                        " T, X, and Y"
            return
L56200:     errormsg$ = "You may not use Average, Fixed, Last or Manual"&~
                " with more than one pool"
            return

L56240
*        Cost Fields
            call "NUMTEST" (cost$(fieldnr%-1%), 0, 9999999.99, errormsg$,~
                                                 -4.4, cost(fieldnr%-1%))
            if cost(fieldnr%-1%) = 0% and bucket_id$(fieldnr%-1%) = " "  ~
                                            then cost$(fieldnr%-1%) = " "
            return

        REM *************************************************************~
            *      T E S T   D A T A   F O R   P O O L   I T E M S      *~
            * --------------------------------------------------------- *~
            * Test data elements for pools.                             *~
            *************************************************************

            deffn'253(poolnum%)
                perr$ = " "
                cl% = line% + screenline%
                on poolnum%  gosub       L58150,    /* Quantity         */~
                                         L58290,    /* Inv Costs        */~
                                         L58350,    /* Posting date     */~
                                         L58390     /* Posting text     */
                return

L58150
*        Test QUANTITY.   All pools should have the same sign.
            temp$ = "0"
            if cl% > 1% then temp$ = pqty$(1)
            if cl% = 1% and maxlines% > 1% then temp$ = pqty$(2)
            convert temp$ to temp
            on sgn(temp) + 2% goto L58210, L58220, L58230
L58210:         ok1 = -9e9 : ok2 = -.01 : goto L58240
L58220:         ok1 = -9e9 : ok2 = 9e9  : goto L58240
L58230:         ok1 =  .01 : ok2 = 9e9
L58240:     call "NUMTEST" (pqty$(cl%), ok1, ok2, perr$, -2.2, pqty)
            if perr$ <> " " then return
            if pqty   = 0   then perr$ = "Quantity cannot be zero"
            return

L58290
*        Test Inventory Costs
               return

L58350
*        Test DATE
            call "DATEOK" (pdate$(cl%), u3%, perr$)
            return

L58390
*        Test TEXT
            /* The lengths to which we go in the name of consistency   */
            return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            call "SETPRNT" ("HNY003", " ", 0%, 1%)
            call "SHOSTAT" ("One moment, please")
            end
