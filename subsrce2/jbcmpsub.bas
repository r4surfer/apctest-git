        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB    CCC   M   M  PPPP    SSS   U   U  BBBB    *~
            *    J    B   B  C   C  MM MM  P   P  S      U   U  B   B   *~
            *    J    BBBB   C      M M M  PPPP    SSS   U   U  BBBB    *~
            *  J J    B   B  C   C  M   M  P          S  U   U  B   B   *~
            *   J     BBBB    CCC   M   M  P       SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBCMPSUB - Reports completion of all or part of a job and *~
            *            handles attendant G/L and Inventory posting    *~
            *            (including Lot Tracking, Serial # Tracking, etc*~
            *            if applicable).                                *~
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
            * 11/02/83 ! ORIGINAL                                 ! KEN *~
            * 02/02/84 ! USES USERS HNYDATE FOR POSTING           ! KAB *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION - ! RAC *~
            * 10/10/85 ! Posting moved to background, changed call! HES *~
            * 09/30/86 ! Allow neg. QTY in PIPOUT during input    ! HAL *~
            * 02/19/87 ! Changes for Serial Number Tracking, ...  ! LDJ *~
            * 10/10/87 ! JOB TO JOB COMP. OVER/UNDER ADJ          ! KAB *~
            * 02/04/88 ! Correct Rounding in Backflush            ! HES *~
            * 02/18/88 ! Added PIPOUT Search Function             ! HES *~
            * 08/05/88 ! Does not force lot entry for scrap if    ! TLJ *~
            *          ! lot tracking for the part is not set.    !     *~
            * 01/19/90 ! Change to allow update of the new        ! SID *~
            *          ! ESTIMATED PERCENT COMPLETE field in      !     *~
            *          ! JBMASTR2 to 100% when qty remain to build!     *~
            *          ! being decremented to 0.                  !     *~
            * 09/05/90 ! G/L Export file modifications            ! RAC *~
            * 06/27/91 ! Added PF24 access to HNYLCSUB permitting ! MLJ *~
            *          !   Location Management, added HNYLOCNS &  !     *~
            *          !   LOCATION as channels not passed by any !     *~
            *          !   caller.                                !     *~
            * 08/23/91 ! Passed a store/lot on to Locations.  It  ! JDH *~
            *          !   is the 1st one with qty.               !     *~
            * 08/29/91 ! Prorated Scrap & Rework Material & Added ! JDH *~
            *          !   Value values now use the edited qty    !     *~
            *          !   remaining rather than the original qty.!     *~
            * 04/14/92 ! PRR 12108. Flag Job as In-use.           ! JDH *~
            * 10/05/92 ! Ensure that in-use is relieved when done.! JDH *~
            * 05/15/93 ! Core Project -                           ! KAB *~
            *          !    Report Completion at STD - control of !     *~
            *          !      valuation of completion (BOM and TL)!     *~
            *          !    Defaults for Valuation (JBFLAGS)      !     *~
            *          !    Default To-Job set via linkage, prior !     *~
            *          !      reporting.  Qty checked.            !     *~
            *          !    Additional Warnings etc. to help      !     *~
            *          !      failure prediction                  !     *~
            *          ! PRRs 12377, 11229, 12584, 11434, 10396   !     *~
            * 08/13/93 ! PRR 12183 - Added ASKUSER warning when   ! MLJ *~
            *          !   goods are valued at standard and std   !     *~
            *          !   is currently 0.                        !     *~
            *          ! Purchased Jobs -                         !     *~
            *          !   1) Cannot report completion of purchase!     *~
            *          !      jobs via this program.              !     *~
            *          !   2) Removed (4)Edit Rework/Scrap Deflts,!     *~
            *          !      must use JBFLAGS instead.           !     *~
            * 10/19/93 ! Ser #'d Parts Can't move to Jobs, don't  ! KAB *~
            *          !   load uneditable default.               !     *~
            *          !   Also, why block to ser# job if this    !     *~
            *          !   part isn't ser# or lot tracked?        !     *~
            * 03/24/94 ! SES Correction - Deleted Line # 11120.   ! MLJ *~
            * 04/27/95 ! Added support for Partial Completion BOM ! JDH *~
            *          !   method 'B' - Prorate expected BOM cost.!     *~
            *          ! Scrap & Rework values blink if zero and  !     *~
            *          !   completing to one of them. PRR 13417.  !     *~
            *          ! If completing using 'S', costs defaulted !     *~
            *          !   to JBCDIST are the appropriate stds.   !     *~
            *          ! PRRs 12802,13292. Loosened the restriction     *~
            *          !   of completing to unique scrap lots.    !     *~
            *          ! Value can be added and components kitted !     *~
            *          !   even if no goods 'finished' regular.   !     *~
            *          !   (eg, only completed to scrap or rework)!     *~
            * 07/16/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

          sub "JBCMPSUB" (modno$, jnlid$,        /* G/L CONTROL STUFF  */~
                          #3,                    /* HNYMASTR  File UFB */~
                          #4,                    /* JBMASTR2  File UFB */~
                          #7,                    /* JBCREDIT  File UFB */~
                          #8,                    /* JBSTATUS  File UFB */~
                          #9,                    /* JBMASTRC  File UFB */~
                          #10,                   /* RTEMASTR  File UFB */~
                          #11,                   /* WCMASTR   File UFB */~
                          #16,                   /* WCOUT     File UFB */~
                          #45,                   /* JBCROSS2  File UFB */~
                          #52,                   /* HNYQUAN   File UFB */~
                          #54,                   /* SYSFILE2  File UFB */~
                          #59,                   /* STORNAME  File UFB */~
                          #20,                   /* USERINFO  File UFB */~
                          #34,                   /* PIPOUT    File UFB */~
                          #12,                   /* GLMAIN    File UFB */~
                          #5,                    /* JBMATER2  File UFB */~
                          #6,                    /* JBVALUE2  File UFB */~
                          #61,                   /* SERTIF   File UFB  */~
                          #62,                   /* SERMASTR File UFB  */~
                          #63,                   /* SERWORK  File UFB  */~
                          jobnr$, from$)

        dim                                                              ~
            acct$12,                     /* ADD OFFSET ACCOUNT         */~
            acctdescr$32,                /* ADD OFFSET ACCOUNT DESC    */~
            addvalue$1,                  /* ADD VALUE TO JOB           */~
            adddescr$30,                 /* ADD VALUE TO JOB           */~
            adjqty$10,                   /* ADJUSTED QTY REMAINING     */~
            ask$(3)80,                   /* Just for Askuser           */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            compstr$3, complot$6,        /* Most Likely Target         */~
            compjob$8,                   /* Most Likely Target         */~
            cost(12),                    /* Cost Returned from JBBOMCST*/~
            cost0(12),                   /* COST WORK ARRAY            */~
            cost1(12),                   /* COST WORK ARRAY            */~
            cost2(12),                   /* COST WORK ARRAY            */~
            cost3(12),                   /* COST WORK ARRAY            */~
            costt(1),                    /* COST WORK ARRAY            */~
            costw(1,12),                 /* COST WORK ARRAY            */~
            cost$96, jbcost$96,          /* COST WORK ARRAY            */~
            costmethod$2,                /* COSTING METHOD             */~
            costdescr$30,                /* COSTING METHOD DESCRIPTION */~
            cursor%(2),                  /* Cursor location for edit   */~
            defaddacct$12,               /* Add Value Exp. Account     */~
            defaddrtef$1,                /* Add Std Val Mthd - First   */~
            defaddrtep$1,                /* Add Std Val Mthd - Partial */~
            defaddrtel$1,                /* Add Std Val Mthd - Last    */~
            defkitmthd$1,                /* Kit Components Method      */~
            defkitstr$3,                 /* Kit Components Store       */~
            defvalmthd$2,                /* Comp Valuation Mthd - Part.*/~
            defvalcmthd$2,               /* Comp Valuation Mthd - Last */~
            descr_map(10),               /* PLOWCODE Argument          */~
            errormsg$79,                 /* Error message              */~
            errmsg2$79,                  /* Error message              */~
            from$1,                      /*                            */~
            header$(3)80,                /* PLOWCODE Argument          */~
            hnymcmthd$1, hnyqcmthd$1,    /* Cost Methods               */~
            i(1),                        /* PLOWCODE Argument          */~
            i$(24)80,                    /* Screen image               */~
            inpmessage$79,               /* Input message              */~
            jnlid$3,                     /* Journal id                 */~
            job$8,                       /* Job number                 */~
            jobdescr$32,                 /* Job number                 */~
            jobnr$8,                     /* Restricted mode            */~
            jobbcost$96,                 /* Job BOM Costs              */~
            jobmcost$96,                 /* Job MISC Costs             */~
            kitmessage$30,               /* Message for Kitting Option */~
            kitmessage2$30,              /* Message for Kitting Option */~
            kitmethod$1,                 /* KITTING METHOD             */~
            klot$6,                      /* Kit from lot               */~
            kstr$3,                      /* Kit from store             */~
            lfac$(20)1,                  /* Field attribute characters */~
            line$60,                     /* Passed to JBCDIST          */~
            lmwc$(2)10,                  /* Labor value of goods       */~
            lmwc(2),                     /* Labor value of goods       */~
            lot$(4)6, lastlot$6,         /* To lots                    */~
            lotreq$1,                    /* System Lot Tracking Flag   */~
            matl$(2)10,                  /* Material value of goods    */~
            matl(2),                     /* Material value of goods    */~
            message$79,                  /* General screen message     */~
            message2$79,                 /* 2nd Gen.Screen message     */~
            modno$2,                     /* Module number              */~
            moduleno$20,                 /* Module id                  */~
            part$25,                     /* Part to be built           */~
            partdescr$34,                /* Part to be built           */~
            pfdescr$(2)79,               /* Description Of Keys Active */~
            pfdescr1$(2)79,              /* Description Of Keys Active */~
            pfkeys$32,                   /* P.F. Keys Active           */~
            pfkeys1$32,                  /* P.F. Keys Active           */~
            pledate$6,                   /* Job planned end date       */~
            priority$1,                  /* Transaction Priority       */~
            postdate$8,                  /* Inventory Posting Date     */~
            qty$(3)10,                   /* Quantities to report       */~
            qty(3),                      /* Quantities reported        */~
            qtyhold(3),                  /* Quantities reported        */~
            qtybld$10,                   /* Quantity to build          */~
            qtycmp$10,                   /* Quantity comp to date      */~
            qtylft$10,                   /* Quantity still in job      */~
            readkey$70,                  /* Work variable              */~
            readkey1$70,                 /* Work variable              */~
            rfac$1,                      /* Route Steps Msg FAC        */~
            rtemsg$49,                   /* Route Steps Exist Message  */~
            rwdate$8,                    /* Complet date for rework    */~
            rwkbcost$96,                 /* Rework BOM Costs           */~
            rwkmcost$96,                 /* Rework MISC Costs          */~
            scrbcost$96,                 /* Scrap BOM Costs            */~
            scrmcost$96,                 /* Scrap MISC Costs           */~
            sfcdate$6,                   /* Shop Floor Posting Date    */~
            status$(3)1,                 /* S/N's Status Codes         */~
            statusmsg$(16)80,            /* job Status Messages        */~
            str$(4)3, laststr$3,         /* To stores                  */~
            summary$1,                   /* Summary indicator          */~
            task$2,                      /* Bckgrnd Task To Involk     */~
            tfac$1,                      /* Field attribute for title  */~
            tttle$79,                    /* Screen header              */~
            tojob$8, ctojob$8, lastjob$8,/* To Job                     */~
            totqty$10,                   /* Total quantity reported    */~
            total$10,                    /* Total                      */~
            trankey$40,                  /* S/N's Transaction key      */~
            userid$3,                    /* Current User's ID          */~
            value$(2)10,                 /* Total value of goods       */~
            vfac$(2)1,                   /* Field attribute characters */~
            work$56,                     /* Work in process G/L acct   */~
            work1$40                     /* Carries S/N's Index Pointer*/

        dim                                                              ~
            f2%(64),                     /* 0 if file is open          */~
            f1%(64),                     /* Record-on-file flags       */~
            fs%(64),                     /* 1 if file open, -1 if does */~
                                         /* not exist, 0 if notyet chkd*/~
            rslt$(64)20                  /* text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #13 ! HNYLOCNS ! Location Quantity Detail File            *~
            * #14 ! LOCATION ! Location Master File                     *~
            * #15 ! JBTIF    ! Job Transaction File                     *~
            * #35 ! PIPCROSS ! PIP cross reference                      *~
            * #36 ! JBPIPXRF ! PIP cross reference (Type 000)           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #13,  "HNYLOCNS",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 700,                                  ~
                         keypos = 1, keylen = 42,                        ~
                         alternate key 1, keypos = 443, keylen = 42,     ~
                                   key 2, keypos = 485, keylen = 42,     ~
                                   key 3, keypos = 527, keylen = 42,     ~
                                   key 4, keypos = 590, keylen = 42      ~

            select #14,  "LOCATION",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 400,                                  ~
                         keypos = 1, keylen = 11,                        ~
                         alternate key 1, keypos = 4, keylen = 11        ~

            select #15,  "JBTIF",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 350,                                  ~
                         keypos = 9, keylen = 18,                        ~
                         alternate key 1, keypos = 1, keylen = 26        ~

            select #35, "PIPCROSS", varc, indexed, recsize = 150,        ~
                         keypos =  1  , keylen =  71,                    ~
                         alt key 1, keypos = 20, keylen = 52,            ~
                             key 2, keypos = 39, keylen = 33

            select #36, "JBPIPXRF", varc, indexed, recsize = 63,         ~
                         keypos = 1, keylen = 63,                        ~
                         alt key 1, keypos = 45, keylen = 19

            call "OPENCHCK" (#13, fs%(13), f2%(13),   0%, rslt$(13))
            call "OPENCHCK" (#14, fs%(14), f2%(14),   0%, rslt$(14))
            call "OPENCHCK" (#15, fs%(15), f2%(15), 100%, rslt$(15))
            call "OPENCHCK" (#35, fs%(35), f2%(35),   0%, rslt$(35))
            call "OPENCHCK" (#36, fs%(36), f2%(36),   0%, rslt$(36))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            status$(1) = "1"   /* Rework    */
            status$(2) = "2"   /* Scrap     */
            status$(3) = "2"   /* Inventory */
            date$ = date
            call "DATEOK" (date$, jtoday%, " ")
            call "EXTRACT" addr ("ID", userid$) /* GET CURRENT USER    */
            ll% = 6%
            call "READ100" (#20, userid$, f1%(20))
                 if f1%(20) = 0 then L09170
                 get #20, using L09130, sfcdate$
L09130:     FMT XX(33), CH(6)

            call "WHICHMON" (#54, sfcdate$, return%)
                if return% > 0 then L09200
L09170:         accept "Invalid Posting Date. Press (RETURN) to exit."
                goto exit_program

L09200:     postdate$ = sfcdate$
            call "DATEFMT" (postdate$)

            postflag% = 1
            tttle$ = "Record disposition of inventory yielded from jobs"
            str(tttle$,62%) = "JBCMPSUB: " & str(cms2v$,,8%)

            readkey$ = "SWITCHS.SFC"
            call "READ100" (#54, readkey$, f1%(54))
               if f1%(54%) = 0% then L10000
            get #54 using L09330, defkitstr$,  defvalmthd$, defaddrtef$,   ~
                                defaddacct$, defkitmthd$, defvalcmthd$,  ~
                                defaddrtel$, defaddrtep$

L09330:         FMT POS(37), CH(3), POS(42), CH(2), CH(1), CH(9), CH(1), ~
                    XX(2), CH(2), 2*CH(1)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            job$ = jobnr$

        inputmode_for_startover
            call "ALLFREE"
            call "JBINUSE" (" ", 1%)  /* Clears In-use Flag for User */
            init(" ") errormsg$, inpmessage$, errmsg2$, lot$(), qtybld$, ~
                      qtylft$, qtycmp$, qty$(), part$, lmwc$(),          ~
                      matl$(), str$(), totqty$, value$(), partdescr$,    ~
                      message$, rwdate$, pledate$, message2$, jobdescr$, ~
                      kitmessage$, kstr$, klot$, kitmethod$, costmethod$,~
                      kitmessage2$, costdescr$, addvalue$, adddescr$,    ~
                      acct$, acctdescr$, tojob$, ctojob$, adjqty$

            mat qty=zer : mat matl=zer : mat lmwc=zer : status% = 0%
            mat cost = zer
            tojobok%, adjqtyok%, qtybld, qtycmp, qtylft = 0
            edit%, comp_target%, rtexist% = 0%

            if from$ <> "C" then L10260
            if blast_the_message <> 0 then L10260
            errmsg2$ = "WARNING - Any Transferring Of Parts Back To Inven~
        ~tory Must Take Place"
            errormsg$ = "Before Reporting Completion Or Their Costs Will ~
        ~Also Be Distributed"
            blast_the_message = 1

L10260:     for fieldnr% = 1% to 16%
                if fieldnr% >  7% and fieldnr% < 10% then L10350
                if fieldnr% > 10% and fieldnr% < 13% then L10350
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10350
L10290:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then exit_program
                      if from$ <> "C" then L10340
                      if keyhit%  = 16 then exit_program
L10340:               if keyhit% <>  0 then       L10290
L10350:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10290
            next fieldnr%

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of edit mode for data entry screens     *~
            *************************************************************

            inpmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."
            edit% = 1%

L11090:     gosub'111(0%)
                  errormsg$ = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11090
            fieldnr% = cursor%(1) - 10
            if fieldnr% <>  8 then L11170
                fieldnr% = 13
                goto L11380
L11170:     if fieldnr% <>  9 then L11191
                fieldnr% = 14
                goto L11380
L11191:     if fieldnr% <> 10 then L11195
                fieldnr% = 15
                goto L11380
L11195:     if fieldnr% <> 11 then L11200
               if kitmessage$ = " " then L11090
               fieldnr% = 16
               goto L11380
L11200:     if fieldnr% < 1 or fieldnr% > 5 or fieldnr% = 3 then L11090
            if fieldnr% < 3 then L11250
                fieldnr% = fieldnr% + 4
                if cursor%(2) > 56 then fieldnr% = fieldnr% + 3
                goto L11380
L11250:     fieldnr% = fieldnr% + 2
            if fieldnr% = 3 then L11380
            if cursor%(2) < 35 then L11360
                fieldnr% = fieldnr% + 1
                if cursor%(2) < 44 then L11360
                fieldnr% = fieldnr% + 1
                if cursor%(2) < 48 then L11380
                fieldnr% = fieldnr% + 1
                if cursor%(2) < 57 then L11380
                fieldnr% = fieldnr% + 3
                  goto L11380

L11360:     if type%=0% then L11000

L11380:     gosub'051(fieldnr%)
            if fieldnr% >  7% and fieldnr% < 10% then gosub'151(fieldnr%)
            if fieldnr% > 10% and fieldnr% < 13% then gosub'151(fieldnr%)
                      if enabled% = 0 then L11000
            mat qtyhold = qty
            gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11380
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11380

            if fieldnr% <> 14% then L11520
               if addvalue$ = " " then L11000
                  if acct$ <> " " then L11000
                  fieldnr% = 15%
                  goto L11380

L11520:     if fieldnr% <>  3% then L11000

            if qtyhold(3) > 0 then L11660
               if qty(3) = 0 then L11660

            for fieldnr% = 4% to 5%
                gosub'051(fieldnr%)
                  if enabled% = 0% then L11640
L11590:         gosub'111(fieldnr%)
                    if keyhit%  =  1 then gosub startover
                    if keyhit% <>  0 then L11590
                gosub'151(fieldnr%)
                    if errormsg$ <> " " then L11590
L11640:     next fieldnr%

L11660:     if qtyhold(2) > 0 then L11790
               if qty(2) = 0 then L11790

            for fieldnr% = 6% to 7%
                gosub'051(fieldnr%)
                  if enabled% = 0% then L11770
L11720:         gosub'111(fieldnr%)
                    if keyhit%  =  1 then gosub startover
                    if keyhit% <>  0 then L11720
                gosub'151(fieldnr%)
                    if errormsg$ <> " " then L11720
L11770:     next fieldnr%

L11790:     if qtyhold(1) > 0 then L11000
               if qty(1) = 0 then L11000

            fieldnr% = 10%
            gosub'051(fieldnr%)
                if enabled% = 0% then L11000
L11850:     gosub'111(fieldnr%)
                if keyhit%  =  1 then gosub startover
                if keyhit% <>  0 then L11850
            gosub'151(fieldnr%)
                if errormsg$ <> " " then L11850

            goto L11000

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave

            if qty(3) = 0 then L19270
            if pos(costmethod$ = "S") <> 0% then L19150
            if str$(3) = " " then L19270
            if hnyqcmthd$ = hex(ff) then L19130
               if pos("FST" = hnyqcmthd$) = 0% then L19270
L19130:           if pos("FST" = hnymcmthd$) = 0% then L19270

L19150:     call "STCCOSTS" (part$, " ", #54, 1%, temp)
                if temp <> 0 then L19270

L19180:     u3% = 2%
            call "ASKUSER" (u3%, "*** ZERO STANDARD ***",                ~
                            part$ & " will be valued at STANDARD, " &    ~
                            "which is currently 0.",                     ~
                            "Press PF1 to return to edit.",              ~
                            "Press RETURN to acknowledge and continue.")
            if u3%  = 1% then L11000
            if u3% <> 0% then L19180

L19270:     gosub dataput
            if from$ <> "C" then inputmode
            REM If in restricted mode...
            if abs(qtybld - (qtycmp + qty(3) + qty(2) + qty(1))) < .01   ~
               then exit_program
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for the page 1 of input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1
                  inpmessage$ = " "
                  on fieldnr% gosub L20260,         /* JOB NUMBER       */~
                                    L20290,         /* QUANTITY REPORTED*/~
                                    L20340,         /* QUANTITY REPORTED*/~
                                    L20390,         /* GOOD STORE       */~
                                    L20460,         /* GOOD LOT         */~
                                    L21000,         /* SCRAP STORE      */~
                                    L21070,         /* SCRAP LOT        */~
                                    L21140,         /* SCRAP VALUE-BOM  */~
                                    L21500,         /* SCRAP VALUE-MSC  */~
                                    L22000,         /* REWORK DATE      */~
                                    L22200,         /* REWORK VALUE-BOM */~
                                    L22500,         /* REWORK VALUE-MSC */~
                                    L23000,         /* COST OPTION      */~
                                    L23100,         /* ADD VALUE        */~
                                    L23200,         /* ADD VALUE ACCT   */~
                                    L23300          /* KIT OPTION       */

                     return

L20260:     REM DEFAULT/ENABLE FOR JOB NUMBER
                inpmessage$ = "Enter/Find Job Number To Process."
                return
L20290:     REM DEFAULT/ENABLE FOR PART NUMBER
                inpmessage$ = "Part Yielded"
                enabled% = 0%
                return

L20340:     REM DEFAULT/ENABLE FOR QUANTITY REPORTED
                inpmessage$= "Enter quantity(s) in respective column(s)."
                if qty$(3) = " " then qty$(3) = qtylft$
                if adjqty$ = " " then adjqty$ = qtylft$
                ctojob$ = tojob$
                if tojob$ = " " then return
                   holdqty3 = qty(3) : qty(3) = -1
                   gosub check_tojob_pip
                   inpmessage$ = inpmessage$ & " To Job Quantity is"
                   call "CONVERT" (tojobqty, -0.2,                       ~
                            str(inpmessage$, len(inpmessage$) + 2%, 10%))
                   qty(3) = holdqty3
                return

L20390:     REM DEFAULT/ENABLE FOR GOOD TO STORE
                inpmessage$= "Enter Store to stock goods, or Job# to kit ~
        ~to. Searches: '?' ALL Jobs, or PF10."
                if tojobok% = 0% then L20422
                   lastjob$ = " " : tojob$ = " "
L20422:         if abs(qty(3)) >= .01 then L20425
L20423:            enabled% = 0%
                   return
L20425:         if type%        <> 0% then L20432
                if comp_target%  = 0% then L20432
                   if tojob$ = " " then L20430
                      str$(3) = " " : lot$(3) = " "
                      goto L20423
L20430:            str$(3) = laststr$ : lot$(3) = lastlot$
                   goto L20423
L20432:         ctojob$ = tojob$
                if str$(3%) = " " then str$(3) = laststr$
                if tojob$   = " " then tojob$  = lastjob$
                   return

L20460:     REM DEFAULT/ENABLE FOR GOOD TO LOT
                inpmessage$ = "Enter Lot Number for finished goods"
                hnyqcmthd$ = hex(ff)
                if lotenbl%     =  0% then L20490
                if str$(3)      = " " then L20490
                if abs(qty(3)) >= .01 then L20500
L20490:            lot$(3)  = " "
L20491:            enabled% = 0%
                   return
L20500:         if type% <> 0% then L20505
                   if comp_target% <> 0% then L20491
L20505:         if lot$(3) <> " " then return
                   if str$(3) = laststr$ then lot$(3) = lastlot$
                   return

L21000:     REM DEFAULT/ENABLE FOR SCRAP TO STORE
                inpmessage$= "Enter Store for scrapped items. Store must ~
        ~be non-numeric."
                if abs(qty(2)) >= .01 then return
                   str$(2) = " "
                   enabled% = 0
                   return

L21070:     REM DEFAULT/ENABLE FOR SCRAP TO LOT
                inpmessage$= "Enter Lot Number for scrapped items."
                if lotenbl%     =  0% then L21110
                if abs(qty(2)) >= .01 then return
L21110:            enabled% = 0%
                   lot$(2) = " "
                   return

L21140:     REM DEFAULT/ENABLE FOR SCRAP MATERIAL VALUE
                if abs(qty(2)) < .001 then enabled% = 0
                if enabled% <> 0 then L21210
                   matl$(2), lmwc$(2) = " "
                   init (hex(00)) scrbcost$, scrmcost$
                   matl(2), lmwc(2) = 0
                   return
L21210:         line$ = "PER UNIT COMPONENT value of scrapped items."
                mat cost0 = zer
                if str(costmethod$,1%,1%) = "Z" then L21260
                if str(costmethod$,1%,1%) = "S" then L21249
                get jobbcost$ using L21232, cost0()
L21232:             FMT 12*PD(14,4)
                if str(costmethod$,1%,1%) <> "B" then L21245
                    mat cost0 = cost
L21245:         temp = adjqty
                if str(costmethod$,1%,1%) = "A" then                     ~
                                          temp = qty(1) + qty(2) + qty(3)
                if str(costmethod$,1%,1%) = "B" then temp = 1
L21249:         if str(costmethod$,1%,1%) = "S" then gosub'099(1%)
                mat cost0 = (1/temp) * cost0
L21260:         call "PACKZERO" (cost0(), jbcost$)
                mat costw = con:mat costt = costw * cost0
                jbtotal = costt(1)
                std% = 1%
                total = 1
                if str(costmethod$,1%,1%) = "S" or                       ~
                   str(costmethod$,1%,1%) = "B" then L21325
                if kitmessage$ <> " " then L21330
                if pos(scrbcost$ > hex(00)) <> 0% then L21330
L21325:         scrbcost$ = jbcost$
L21330:         cost$ = scrbcost$
                gosub L24000
                scrbcost$ = cost$
                matl$(2)  = total$
                matl (2)  = total
                enabled% = 0%
                return

L21500:     REM DEFAULT/ENABLE FOR SCRAP VALUE THIS LEVEL
                if abs(qty(2)) < .001 then enabled% = 0
                if enabled% <> 0 then L21570
                   matl$(2), lmwc$(2) = " "
                   init (hex(00)) scrbcost$, scrmcost$
                   matl(2), lmwc(2) = 0
                   return
L21570:         line$ = "PER UNIT LABOR, WORK CENTER and MISC."
                line$ = line$ & " value of scrapped items."
                mat cost0 = zer
                if str(costmethod$,2%,1%) = "Z" then L21620
                if str(costmethod$,2%,1%) = "S" then L21608
                get jobmcost$ using L21600, cost0()
L21600:             FMT 12*PD(14,4)
                temp = adjqty
                if str(costmethod$,2%,1%) = "A" then                     ~
                                          temp = qty(1) + qty(2) + qty(3)
L21608:         if str(costmethod$,2%,1%) = "S" then gosub'099(2%)
                mat cost0 = (1/temp) * cost0
L21620:         call "PACKZERO" (cost0(), jbcost$)
                mat costw = con:mat costt = costw * cost0
                jbtotal = costt(1)
                std% = 6%
                total = 1
                if str(costmethod$,2%,1%) = "S" then L21685
                if kitmessage$ <> " " then L21690
                if pos(scrmcost$ > hex(00)) <> 0% then L21690
L21685:         scrmcost$ = jbcost$
L21690:         cost$ = scrmcost$
                gosub L24000
                scrmcost$ = cost$
                lmwc$(2)  = total$
                lmwc (2)  = total
                enabled% = 0%
                return

L22000:     REM DEFAULT/ENABLE FOR REWORK COMPLETION DATE
                if abs(qty(1)) < .001 then enabled% = 0
                if enabled% = 0 then rwdate$ = " "
                if enabled% = 0 then return
                inpmessage$= "Enter date rework job is planned to be comp~
        ~leted."
                if rwdate$ <> " " and rwdate$ <> blankdate$ then return
                rwdate$ = pledate$
                if rwdate$ < date then rwdate$ = date
                call "DATEFMT" (rwdate$)
                return

L22200:     REM DEFAULT/ENABLE FOR REWORK MATERIAL VALUE
                if abs(qty(1)) < .001 then enabled% = 0
                if enabled% <> 0 then L22270
                   matl$(1), lmwc$(1) = " "
                   init (hex(00)) rwkbcost$, rwkmcost$
                   matl(1), lmwc(1) = 0
                   return
L22270:         line$ = "PER UNIT COMPONENT value of items"
                line$ = line$ & " to be reworked."
                mat cost0 = zer
                if str(costmethod$,1%,1%) = "Z" then L22320
                if str(costmethod$,1%,1%) = "S" then L22309
                get jobbcost$ using L22292, cost0()
L22292:             FMT 12*PD(14,4)
                if str(costmethod$,1%,1%) <> "B" then L22302
                    mat cost0 = cost
L22302:         temp = adjqty
                if str(costmethod$,1%,1%) = "A" then                     ~
                                          temp = qty(1) + qty(2) + qty(3)
                if str(costmethod$,1%,1%) = "B" then temp = 1
L22309:         if str(costmethod$,1%,1%) = "S" then gosub'099(1%)
                mat cost0 = (1/temp) * cost0
L22320:         call "PACKZERO" (cost0(), jbcost$)
                mat costw = con:mat costt = costw * cost0
                jbtotal = costt(1)
                std% = 1%
                total = 1
                if str(costmethod$,1%,1%) = "S" or                       ~
                   str(costmethod$,1%,1%) = "B" then L22390
                if kitmessage$ <> " " then L22400
                if pos(rwkbcost$ > hex(00)) <> 0% then L22400
L22390:         rwkbcost$ = jbcost$
L22400:         cost$ = rwkbcost$
                gosub L24000
                rwkbcost$ = cost$
                matl$(1)  = total$
                matl (1)  = total
                enabled% = 0%
                return

L22500:     REM DEFAULT/ENABLE FOR REWORK VALUE THIS LEVEL
                if abs(qty(1)) < .001 then enabled% = 0
                if enabled% <> 0 then L22570
                   matl$(1), lmwc$(1), rwdate$ = " "
                   init (hex(00)) rwkbcost$, rwkmcost$
                   matl(1), lmwc(1) = 0
                   return
L22570:         line$ = "PER UNIT VALUE ADDED for"
                line$ = line$ & " items to be reworked."
                mat cost0 = zer
                if str(costmethod$,2%,1%) = "Z" then L22620
                if str(costmethod$,2%,1%) = "S" then L22608
                get jobmcost$ using L22600, cost0()
L22600:             FMT 12*PD(14,4)
                temp = adjqty
                if str(costmethod$,2%,1%) = "A" then                     ~
                                          temp = qty(1) + qty(2) + qty(3)
L22608:         if str(costmethod$,2%,1%) = "S" then gosub'099(2%)
                mat cost0 = (1/temp) * cost0
L22620:         call "PACKZERO" (cost0(), jbcost$)
                mat costw = con:mat costt = costw * cost0
                jbtotal = costt(1)
                std% = 6%
                total = 1
                if str(costmethod$,2%,1%) = "S" then L22690
                if kitmessage$ <> " " then L22700
                if pos(rwkmcost$ > hex(00)) <> 0% then L22700
L22690:         rwkmcost$ = jbcost$
L22700:         cost$ = rwkmcost$
                gosub L24000
                rwkmcost$ = cost$
                lmwc$(1)  = total$
                lmwc (1)  = total
                enabled% = 0%
                return

L23000: REM DEFAULT/ENABLE FOR COST METHOD
                if costmethod$ <> " " then L23032
                   temp = round(adjqty - (qty(1) + qty(2) + qty(3)), 2)
                   if temp > 0 then costmethod$ = defvalmthd$            ~
                               else costmethod$ = defvalcmthd$

L23032:     inpmessage$ = "Completion Value Method: A-All, P-Prorate, S"&~
                           "-Std, Z-Zero; B-BOM (BOM Only)."
            return

L23100: REM DEFAULT/ENABLE FOR ADD VALUE
                if addvalue$ <> " " then L23150
                   if edit% <> 0% then L23150

                   temp = round(adjqty - (qty(1) + qty(2) + qty(3)), 2)
                   if qtycmp = 0 then  L23130  /* First */
                      if temp > 0 then addvalue$ = defaddrtep$           ~
                                  else addvalue$ = defaddrtel$
                      goto L23148

L23130:            if temp = 0 then L23140    /* and Last */
                      addvalue$ = defaddrtef$ : goto L23148

L23140:            addvalue$ = defaddrtef$
                   if addvalue$ = " " then addvalue$ = defaddrtel$

L23148:         if addvalue$ = " " then enabled% = 0%

L23150:         inpmessage$ = "Add Route/Misc. Standard Costs.  Blank = D~
        ~on't, P = Prop. (Backflush), A = All."
                return

L23200: REM DEFAULT/ENABLE FOR ADD VALUE ACCOUNT
            if addvalue$ <> " " then L23240
               acct$, acctdescr$ = " ":enabled% = 0%
               return
L23240:     inpmessage$ = "Enter Account to Expense Value Added."
            if acct$ <> " " then return
               acct$ = defaddacct$
               if acct$ = " " then return
                  call "GLFMT" (acct$)
                  call "GETCODE" (#12, acct$, acctdescr$, 1%, 99, f1%(12))
                  return

L23300: REM DEFAULT/ENABLE FOR KIT OPTIONS
                if kitmessage$ <> " " then L23340
                   enabled% = 0
                   return
L23340:         inpmessage$ = "Enter Store/Lot for Kitting and How to Iss~
        ~ue, All or Proportional (Backflush)."
                if kstr$ = " " then kstr$ = defkitstr$
                kitmessage2$ = "(All or Proportional)"
                if kitmethod$ = " " then kitmethod$ = defkitmthd$
                if kitmethod$ <> "A" then L23420
                   kitmessage2$ = "(All Remaining Components)"
                   return
L23420:        if kitmethod$ <> "P" then return
                  kitmessage2$ = "(Proportional - Backflush)"
                  return

L24000: REM *************************************************************~
            * CALL TO JBCDIST                                           *~
            *************************************************************

            call "JBCDIST"                                               ~
                    (" ",                /* Mode (D = Display Only)    */~
                                         /*      (N = Total not Edited)*/~
                     std%,               /* Which Standard Element?    */~
                                         /*   1 = BOM                  */~
                                         /*   2 = RTE (WC and Labor)   */~
                                         /*   3 = MISC                 */~
                                         /*   4 = Folded Standard      */~
                                         /*   5 = Rolled Standard      */~
                                         /*   6 = This Level (2 + 3)   */~
                     part$,              /* Part Code                  */~
                     partdescr$,         /* Part Description           */~
                     line$,              /* Line 2 (up to 60 char)     */~
                     #54,                /* SYSFILE2 Channel           */~
                     cost$,              /* Costs Passed (12*PD(14,4)) */~
                     total$,             /* Total (Returned)           */~
                                         /* I Mode = Value to Dist.    */~
                     total,              /* Total (Returned)           */~
                                         /* In = Job Quantity          */~
                     jbcost$,            /* Current Job Costs          */~
                     jbtotal)            /* Current Job Total          */~

            return

        deffn'099(bom_or_value_added%) /* 1% = BOM; 2% = Value Added */
            call "STCCOSTS" (part$, " ", #54, 3%, temp, cost0(), cost1(),~
                             cost2(), cost3())
            temp = 1   /* Per Each */
            if bom_or_value_added% <> 1% then L24580
                mat cost0 = cost1
                return
L24580:     mat cost0 = cost2 + cost3
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
            *-----------------------------------------------------------*~
            * Gives the user the ability to start over when he wants to *~
            * or will return user back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear
            call "SERSTOVR" (1%, "5", "1",  #62, #63) /* clear rework */
            call "SERSTOVR" (2%, "8", "1",  #62, #63) /* clear scrap  */
            call "SERSTOVR" (3%, "2", "1",  #62, #63) /* clear invent */
            goto inputmode_for_startover

L30000: REM *************************************************************~
            * LOAD JOB DATA AND CHECK A FEW THINGS                      *~
            *************************************************************

                get #4, using L30035, part$, qtybld, qtycmp, work1$,      ~
                        plstdate$, pledate$, jobbcost$, jobmcost$,       ~
                        rwkmcost$, rwkbcost$, scrmcost$
L30035:             FMT POS(58), CH(25), 2*PD(14,4), POS(153), CH(6),    ~
                        POS(168), 2*CH(6), POS(240), 3*CH(96), POS(536), ~
                        2*CH(96)
                if work1$ = " " or work1$ = blankdate$ then L30075
                   errormsg$ = "Job Has Been Closed as of"
                   call "DATEFMT" (work1$)
                   errormsg$ = errormsg$ & " " & str(work1$,,8)
                   return
L30075:         call "PIPINDEX" (#54, plstdate$, stdate%, u3%)
                get jobbcost$ using L30150, cost0()
                get rwkbcost$ using L30150, cost1()
                mat cost0 = cost0 - cost1
                call "PACKZERO" (cost0(), jobbcost$)
                negbom% = 0%
                for i% = 1% to 12%
                    if cost0(i%) < 0 then negbom% = 1%
                next i%
                if negbom% <> 0% then status% = 1%
                get jobmcost$ using L30150, cost0()
                get rwkmcost$ using L30150, cost1()
                mat cost0 = cost0 + cost1
                get scrmcost$ using L30150, cost1()
                mat cost0 = cost0 - cost1
                call "PACKZERO" (cost0(), jobmcost$)
L30150:             FMT 12*PD(14,4)
                negrte% = 0%
                for i% = 1% to 12%
                    if cost0(i%) < 0 then negrte% = 1%
                next i%
                if negrte% <> 0% then status% = 1%
                init (hex(00)) rwkbcost$, rwkmcost$, scrbcost$, scrmcost$
                call "GETCODE" (#3, part$, partdescr$, 1%, 99, f1%(3))
                   if f1%(3) <> 0 then L30205
L30195:               errormsg$="PART NOT ON FILE OR TYPE INCORRECT"
                      return
L30205:         get #3, using L30210, type$, hnymcmthd$
L30210:              FMT XX(179), CH(3), POS(307), CH(1)
                convert type$ to type%, data goto L30195
                if type% = 0% then inpmessage$ = "BUILD TO ORDER PART"
                if type% = 0% then status% = 1%
                call "LOTENABL" (part$, lotenbl%, ll%, #54, #3)
                call "SERENABL" (part$, tojobok%, temp%, #54, #3)
                   adjqtyok% = tojobok%
                   if tojobok% <> 0% then status% = 1%
                qtylft = round(qtybld - qtycmp, 2)
                call "CONVERT" (qtybld, -0.2, qtybld$)
                call "CONVERT" (qtycmp, -0.2, qtycmp$)
                call "CONVERT" (qtylft, -0.2, qtylft$)
                if qtylft > 0 then L30300
                if tojobok% = 0% then L30300
                     errormsg$="Nothing is left in this job"
                     return

L30300
*       ** If Completions have started, find out where to
                comp_target% = 0% : tojobheld% = 0%
                laststr$, lastlot$, tojob$, ctojob$ = " "
                compstr$, complot$, compjob$, lastjob$ = " "

                init (hex(00)) readkey$
                str(readkey$,,8%) = str(job$,,8%)

L30340:         call "PLOWNEXT" (#7, readkey$, 8%, f1%(7))
                if f1%(7) = 0 then L30410
                     get #7, using L30355, temp$
L30355:                   FMT XX(47), CH(9)
                     if str(temp$,,1%) <> "*" then L30380
                     if tojobok% <> 0% then L30340    /* Cant use it now */
                        if compjob$ = " " then                           ~
                           comp_target% = comp_target% + 1%
                        compjob$ = str(temp$,2%,8%)  /* Last One Used */
                        goto L30340
L30380:              if compstr$ <> " " then L30340   /* Use First One */
                        compstr$ = str(temp$,,3%)
                        complot$ = str(temp$,4%,6%)
                        comp_target% = comp_target% + 1%
                        goto L30340

L30410
*       ** Is there a better guess for a job
                if tojobok% <> 0% then L30585    /* No Need to Look */
                if compjob$ = " " then L30450
                   qty(3) = -1 : tojob$ = compjob$
                   gosub check_tojob_pip
                   if tojobqty > 0 then L30585   /* elligible job, done */
                      ctojob$ = compjob$ : compjob$, tojob$ = " "
                      comp_target% = comp_target% - 1%

L30450:         if type% <> 0% then L30520
                if comp_target% <> 0% then L30585
                   readkey$ = "JOB ORDER: " & job$
                   call "REDALT0" (#36, readkey$, 1%, f1%(36%))
                      if f1%(36%) = 0% then L30520
                   get #36 using L30475, readkey$
L30475:                FMT CH(44)
                   if str(readkey$,20%,25%) <> str(part$,,25%) then L30585
                   if str(readkey$,,10%) <> "JOB ORDER:" then L30585
                      if str(readkey$,12%,8%) = ctojob$ then L30585
                      tojob$ = str(readkey$,12%,8%)
                         qty(3) = -1
                         gosub check_tojob_pip
                         if tojobqty > 0 then L30585
                            tojob$ = " "

L30520:         readkey$ = "JOB ORDER: " & job$
                call "PLOWALTS" (#35, readkey$, 1%, 19%, f1%(35%))
                   if f1%(35%) = 0% then L30585
                get #35 using L30540, readkey$
L30540:             FMT POS(39), CH(19)
                if str(readkey$,,10%) <> "JOB ORDER:" then L30585
                   if str(readkey$,12%,8%) = ctojob$ then L30585
                   tojob$ = str(readkey$,12%,8%)
                      qty(3) = -1
                      gosub check_tojob_pip
                      if tojobqty > 0 then L30585
                         tojob$ = " "

L30585
*       ** Find out What We Got
            qty(3) = 0 : ctojob$ = " "
            if compstr$ = " " then L30596
               laststr$ = compstr$ : lastlot$ = complot$
L30596:     if tojob$ <> " " then L30599
               tojob$ = compjob$
L30599:        lastjob$ = tojob$
            if tojob$ = " " then L30606
               str$(3), lot$(3) = " "
               goto L30700
L30606:     str$(3%) = compstr$ : lot$(3%) = complot$

L30700: REM Find pipouts (read kit list)...
            ckit%, ctype%, cbypr%, cser%, clot% = 0%
            readkey$ = "JOB ORDER: " & job$
            init(hex(00)) str(readkey$,20)
L30720:     call "PLOWNEXT" (#34, readkey$, 19%, f1%(34))
                if f1%(34) = 0 then L30905
            call "READ100" (#3, str(readkey$,20,25), f1%(3))
                if f1%(3) = 0 then L30790
            get #3, using L30745, ptype$
L30745:         FMT XX(179), CH(3)
            convert ptype$ to ptype%, data goto L30790
            if ptype% > 489% and ptype% < 500% then L30720   /* Skip */
            if ptype% > 789% and ptype% < 800% then L30720   /* tools */
            get #34, using L30770, qty
L30770:         FMT POS(57), PD(14,4)
            if qty < 0 then L30870

            if message$ <> " " then L30805
L30790:     message$ = "            All Materials Have NOT Been Issued (K~
        ~itted) To This Job"
            kitmessage$ = "Kit Components from Store/Lot"
L30805:     if ptype% <> 0% then L30815
               ctype% = 1% : status% = 1%
L30815:     call "SERENABL" (str(readkey$,20,25), enabled%, u3%, #54, #3)
               if enabled% <> 1% then L30830
                  cser% = 1% : status% = 1%
L30830:     call "LOTENABL" (str(readkey$,20,25), enabled%, u3%, #54, #3)
               if enabled% <> 2% then L30845
                  clot% = 1% : status% = 1%
L30845:     ckit% = 1%

*          IF MESSAGE2$ <> " " THEN RETURN
            goto L30720

L30870:     if message2$ <> " " then L30885
            message2$ = "              * * Planned By-Products Have Not B~
        ~een Reported * *"
L30885:     cbypr% = 1%
*          IF MESSAGE$ <> " " THEN RETURN
            goto L30720

L30905:     badtrans% = 0%
            readkey$ = str(job$,,8%) & "X" & hex(0000)
            call "PLOWALTS" (#15, readkey$, 1%, 9%, badtrans%)
            if badtrans% <> 0% then status% = 1%

            if status% > 0% then                                         ~
            message2$ = "              * * Press PF12 for Additional Job ~
        ~Information  * *"
            str(message2$,1%,1%) = hex(8c)
            return

        REM *************************************************************~
            *          S A V E   D A T A                                *~
            *-----------------------------------------------------------*~
            * Write data to TIF.                                        *~
            *************************************************************

        dataput
            call "SHOSTAT" ("Updating Transaction Image File")
            call "DATUNFMT" (rwdate$)

            if postflag% = 0 then L31200
                moduleno$ = modno$ : ret% = 0%
                call "JNLINFO" (moduleno$, jnlid$, pstseq%, summary$,    ~
                                " ", sfcdate$, #54, postflag%, ret%)
                postflag% = 0

L31200:     REM Job Quantity Adjustment?
            if abs(adjquan) < .001 then L32000

            call "JB2TIF"               /* Writes to transaction Image */~
                 ("J2",                 /* Task to pass trans to       */~
                  0%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  10%,                  /* Transaction type            */~
                  hex(30),              /* Priority (within 'J2' only) */~
                  job$,                 /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  sfcdate$,             /* G/L posting sequence number */~
                  part$,                /* Inventory Part Code         */~
                  " ",                  /* Inventory Store Code        */~
                  " ",                  /* Inventory Lot Id.           */~
                  adjquan,              /* Quantity to process         */~
                  " ",                  /* used here for passing value */~
                  " ",                  /* used here for passing value */~
                  " ")                  /* Costs                       */

L32000: REM Record completion of scrapped goods...
            trankey$ = str(job$) & bin(2%,3)
            call "SERSAVE" (                                             ~
                        2%,              /* Line Item Pointer.         */~
                        "JC",            /* Source Transaction Type    */~
                        trankey$,        /* Source Transaction Key     */~
                        1%,              /* # Trans to Create File for */~
                        part$,           /* Part Code                  */~
                        userid$,         /* Current User ID            */~
                        status$(2),      /* Change Status to ...       */~
                        "1",             /* Change Status from ...     */~
                        0%,              /* Clear TIF after Save (NO)  */~
                        #54,             /* SYSFILE2 UFB               */~
                        #61,             /* SERTIF UFB                 */~
                        #62,             /* SERMASTR UFB               */~
                        #63)             /* SERWORK  UFB               */

            if abs(qty(2)) < .001 then L33000

            put work$, using L32200, " ", matl(2), 0, 0
            put work1$, using L32210, " ", 2%
L32200:     FMT CH(30), 3*PD(14,4)
L32210:     FMT CH(37), BI(3)
            call "JB2TIF"               /* Writes to transaction Image */~
                 ("J2",                 /* Task to pass trans to       */~
                  0%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  6%,                   /* Transaction type (6 = scrap)*/~
                  hex(30),              /* Priority (within 'J2' only) */~
                  job$,                 /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  sfcdate$,             /* G/L posting sequence number */~
                  part$,                /* Inventory Part Code         */~
                  str$(2),              /* Inventory Store Code        */~
                  lot$(2),              /* Inventory Lot Id.           */~
                  qty(2),               /* Quantity to process         */~
                  work$,                /* used here for passing value */~
                  work1$,               /* used here for passing value */~
                  scrbcost$)            /* Costs                       */

            if pos(scrmcost$ > hex(00)) = 0% then L33000

            get scrmcost$ using L32450, cost0()
L32450:         FMT 12*PD(14,4)
            mat cost0 = (qty(2)) * cost0
            call "PACKZERO" (cost0(), scrmcost$)

            put work$, using L32520, "3", " ", 0, lmwc(2), 0
            work1$ = " "
L32520:     FMT CH(1), CH(29), 3*PD(14,4)
            call "JB2TIF"               /* Writes to transaction Image */~
                 ("J2",                 /* Task to pass trans to       */~
                  0%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  8%,                   /* Transaction type            */~
                  hex(30),              /* Priority (within 'J2' only) */~
                  job$,                 /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  sfcdate$,             /* G/L posting sequence number */~
                  part$,                /* Inventory Part Code         */~
                  str$(2),              /* Inventory Store Code        */~
                  lot$(2),              /* Inventory Lot Id.           */~
                  qty(2),               /* Quantity to process         */~
                  work$,                /* used here for passing value */~
                  work1$,               /* used here for passing value */~
                  scrmcost$)            /* Costs                       */



L33000: REM Record completion of goods going to rework...
            trankey$ = str(job$) & bin(1%,3)
            call "SERSAVE" (                                             ~
                        1%,              /* Line Item Pointer.         */~
                        "JC",            /* Source Transaction Type    */~
                        trankey$,        /* Source Transaction Key     */~
                        1%,              /* # Trans to Create File for */~
                        part$,           /* Part Code                  */~
                        userid$,         /* Current User ID            */~
                        status$(1),      /* Change Status to ...       */~
                        "1",             /* Change Status from ...     */~
                        0%,              /* Clear TIF after Save (NO)  */~
                        #54,             /* SYSFILE2 UFB               */~
                        #61,             /* SERTIF UFB                 */~
                        #62,             /* SERMASTR UFB               */~
                        #63)             /* SERWORK  UFB               */

            if abs(qty(1)) < .001 then L34000
            call "SHOSTAT" ("Queuing Creation Of Rework Job")

            if pos(rwkmcost$ > hex(00)) = 0% then L33740

            get rwkmcost$ using L33460, cost0()
L33460:         FMT 12*PD(14,4)
            mat cost0 = (qty(1)) * cost0
            call "PACKZERO" (cost0(), rwkmcost$)

            put work$, using L33520, " ", 0, lmwc(2), 0, " "
            work1$ = " "
L33520:     FMT CH(20), 3*PD(14,4), CH(8)
            partdescr$ = hex(03)

            call "JB2TIF"               /* Writes to transaction Image */~
                 ("J3",                 /* Task to pass trans to       */~
                  0%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  3%,                   /* Transaction type            */~
                  hex(30),              /* Priority (within 'J2' only) */~
                  job$,                 /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  sfcdate$,             /* G/L posting sequence number */~
                  str(partdescr$,,25),  /* Inventory Part Code         */~
                  str$(1),              /* Inventory Store Code        */~
                  lot$(1),              /* Inventory Lot Id.           */~
                  qty(1),               /* Quantity to process         */~
                  work$,                /* used here for passing value */~
                  work1$,               /* used here for passing value */~
                  rwkmcost$)            /* Costs                       */

L33740:     put work$, using L33770, " ", matl(1), 0, 0
            put work1$, using L33760, rwdate$, " ", 1%
L33760:     FMT CH(6), CH(31), BI(3)
L33770:     FMT CH(30), 3*PD(14,4)
            call "JB2TIF"               /* Writes to transaction Image */~
                 ("J2",                 /* Task to pass trans to       */~
                  0%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  7%,                   /* Transaction type (7=rework) */~
                  hex(30),              /* Priority (within 'J2' only) */~
                  job$,                 /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  sfcdate$,             /* G/L posting sequence number */~
                  part$,                /* Inventory Part Code         */~
                  str$(1),              /* Inventory Store Code        */~
                  lot$(1),              /* Inventory Lot Id.           */~
                  qty(1),               /* Quantity to process         */~
                  work$,                /* used here for passing value */~
                  work1$,               /* used here for rework comp dt*/~
                  rwkbcost$)            /* Costs                       */

L34000: REM Record completion of finished goods...
            trankey$ = str(job$) & bin(3%,3)
            call "SERSAVE" (                                             ~
                        3%,              /* Line Item Pointer.         */~
                        "JC",            /* Source Transaction Type    */~
                        trankey$,        /* Source Transaction Key     */~
                        1%,              /* # Trans to Create File for */~
                        part$,           /* Part Code                  */~
                        userid$,         /* Current User ID            */~
                        status$(3),      /* Change Status to ...       */~
                        "1",             /* Change Status from ...     */~
                        0%,              /* Clear TIF after Save (NO)  */~
                        #54,             /* SYSFILE2 UFB               */~
                        #61,             /* SERTIF UFB                 */~
                        #62,             /* SERMASTR UFB               */~
                        #63)             /* SERWORK  UFB               */

*          IF ABS(QTY(3)) < .001 THEN 35750
            if addvalue$ = " " then L34800

            call "STCCOSTS" (part$, " ", #54, 3%, temp, cost0(), cost1(),~
                             cost2(), cost3())

            if addvalue$ = "A" then qty = qtybld                         ~
                               else qty = qty(1) + qty(2) + qty(3)

            mat cost2 = (qty) * cost2
            mat cost3 = (qty) * cost3
            call "GLUNFMT" (acct$)

            call "PACKZERO" (cost2(), cost$)
            if pos(cost$ > hex(00)) = 0% then L34600
            mat costw = con:mat costt = costw * cost2
            temp = costt(1)/qty

            put work$ using L34360, " ", temp, costt(1), 0, " "
L34360:       FMT CH(20), 3*PD(14,4), CH(9)
            partdescr$ = hex(01) & "* AUTO STD *"
            work1$ = "COMP. REPORT - AUTOMATIC COST ADDITION"

            call "JB2TIF"               /* Writes to transaction Image */~
                 ("J2",                 /* Task to pass trans to       */~
                  0%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  5%,                   /* Transaction type (7=special)*/~
                  hex(30),              /* Priority (within 'J2' only) */~
                  job$,                 /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  sfcdate$,             /* G/L posting sequence number */~
                  partdescr$,           /* Inventory Part Code         */~
                  str(acct$,1,3),       /* Inventory Store Code        */~
                  str(acct$,4,6),       /* Inventory Lot Id.           */~
                  qty,                  /* Quantity to process         */~
                  work$,                /* not used                    */~
                  work1$,               /* Carries S/N's Index Pointer */~
                  cost$)                /* Value Added                 */

L34600:     call "PACKZERO" (cost3(), cost$)
            if pos(cost$ > hex(00)) = 0% then L34800
            mat costw = con:mat costt = costw * cost2
            temp = costt(1)/qty

            put work$ using L34630, " ", temp, costt(1), 0, " "
L34630:       FMT CH(20), 3*PD(14,4), CH(9)
            partdescr$ = hex(03)
            work1$ = "COMP. REPORT - AUTOMATIC COST ADDITION"

            call "JB2TIF"               /* Writes to transaction Image */~
                 ("J2",                 /* Task to pass trans to       */~
                  0%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  5%,                   /* Transaction type (7=special)*/~
                  hex(30),              /* Priority (within 'J2' only) */~
                  job$,                 /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  sfcdate$,             /* G/L posting sequence number */~
                  partdescr$,           /* Inventory Part Code         */~
                  str(acct$,1,3),       /* Inventory Store Code        */~
                  str(acct$,4,6),       /* Inventory Lot Id.           */~
                  qty,                  /* Quantity to process         */~
                  work$,                /* not used                    */~
                  work1$,               /* Carries S/N's Index Pointer */~
                  cost$)                /* Value Added                 */

L34800
*       ** Actual completion Report
        REM Kit It Complete?
            task$ = "J2" : priority$ = hex(30) : function% = 1%
            if tojob$ <> " " then function% = 9%
            put work1$, using L34850, costmethod$, " ", 3%
L34850:         FMT CH(2), CH(35), BI(3)
            put work$ using L34870, tojob$, " ", 0, 0, 0
L34870:         FMT CH(8), CH(22), 3*PD(14,4)
            if str(costmethod$,1,1) = "A" then str(work1$,1,1) = "B"
            if str(costmethod$,2,1) = "A" then str(work1$,2,1) = "B"
            if str(costmethod$,1,1) = "B" then str(work1$,1,1) = "M"
                   /* Sorry I had to do the above, but 'A's changed to */
                   /* 'B's, so 'B' changes to 'M'.                     */

            if kstr$ = " " then L35200

            REM NOTE- This MUST come prior to report finished goods.
            REM priorities on kit cmlpt MUST be before report comp
            task$ = "J1":priority$ = hex(36):function% = 7%:qty = 0
            if kitmethod$ = "A" then L34990
               function% = 8%
               qty = ((qty(1) + qty(2) + qty(3))/adjqty)*10000 /*accrcy*/
L34990:     if tojob$ <> " " then function% = function% + 2%

            call "JB2TIF"               /* Writes to Transaction Image */~
                 ("J1",                 /* Send transaction to JBPOST1 */~
                  0%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  3%,                   /* Transaction type (3=kit cmp)*/~
                  hex(35),              /* Priority                    */~
                  job$,                 /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  sfcdate$,             /* G/L posting sequence number */~
                  " ",                  /* Inventory Part Code         */~
                  kstr$,                /* Inventory Store Code        */~
                  klot$,                /* Inventory Lot Id.           */~
                  qty,                  /* Quantity to process         */~
                  " ",                  /* Not used                    */~
                  " ",                  /* Not used                    */~
                  " ")                  /* Not used                    */~

L35200:     call "JB2TIF"               /* Writes to transaction Image */~
                 (task$,                /* Task to pass trans to       */~
                  0%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  function%,            /* Transaction type (7=special)*/~
                  priority$,            /* Priority (within 'J2' only) */~
                  job$,                 /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  sfcdate$,             /* G/L posting sequence number */~
                  part$,                /* Inventory Part Code         */~
                  str$(3),              /* Inventory Store Code        */~
                  lot$(3),              /* Inventory Lot Id.           */~
                  qty(3),               /* Quantity to process         */~
                  work$,                /* not used                    */~
                  work1$,               /* Carries S/N's Index Pointer */~
                  " ")                  /* not used                    */

*       ** A Little blocking action if we are doing job to job

            if tojob$ = " " then L35740
            str(work$,,8%) = job$
            call "JB2TIF"               /* Writes to transaction Image */~
                 (task$,                /* Task to pass trans to       */~
                  0%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  12%,                  /* Transaction type (7=special)*/~
                  priority$,            /* Priority (within 'J2' only) */~
                  tojob$,               /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  sfcdate$,             /* G/L posting sequence number */~
                  part$,                /* Inventory Part Code         */~
                  " ",                  /* Inventory Store Code        */~
                  " ",                  /* Inventory Lot Id.           */~
                  0,                    /* Quantity to process         */~
                  work$,                /* not used                    */~
                  work1$,               /* Carries S/N's Index Pointer */~
                  " ")                  /* not used                    */

L35740:     if task$ = "J1" then call "TASKUP" ("J1", 0%)
            call "TASKUP" ("J2", 0%)  /* Gets to J2 one way or another */
            return

        REM *************************************************************~
            *                  LOCATION MANAGEMENT                      *~
            *************************************************************

        locations

            qtypassed, qty1, qty2, qty3 = 0
            str$(4), lot$(4) = " "

            if qty$(1) <> " " then                                       ~
                convert qty$(1) to qty1
            if qty$(2) <> " " then                                       ~
                convert qty$(2) to qty2
            if qty$(3) <> " " then                                       ~
                convert qty$(3) to qty3

            qtypassed = qty1 + qty2 + qty3

            for i% = 1% to 3%
                if qty$(i%) = "0" then L36130
                str$(4) = str$(i%)
                lot$(4) = lot$(i%)
L36130:         next i%

            call "HNYLCSUB"   (part$,        /*  Part Number           */~
                               str$(4),      /*  Store Number          */~
                               lot$(4),      /*  Lot Number            */~
                               qtypassed,    /*  Quantity              */~
                               3%,           /*  Action = ADD          */~
                               #54,          /*  SYSFILE2              */~
                               #59,          /*  STORNAME              */~
                               #20,          /*  USERINFO              */~
                               #3,           /*  HNYMASTR              */~
                               #13,          /*  HNYLOCNS              */~
                               #52,          /*  HNYQUAN               */~
                               #14)          /*  LOCATION              */
            return
        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *-----------------------------------------------------------*~
            * Inputs document for first time.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  init(hex(8c)) vfac$()
                  pfdescr$(1) = "(1)Start Over  (8)See Reported Routing  ~
        ~    (13)Instructions   (15)Print Screen"
                  pfdescr$(2) = "(9)Job Summary (10)Jobs Using This Part ~
        ~    (24)Locations      (16)Exit Program"
                  pfkeys$ = hex(01ffffffffffff08090aff0c0dff0f100018)
                  task$ = "Y" : tfac$ = hex(ac)
                  if fieldnr% = 1% then L40135
                     str(pfdescr$(2%),60%) = " "
                     str(pfkeys$,16%, 1%) = hex(ff) : goto L40145

L40135:              str(pfdescr$(1%),,43%) = " "
                     str(pfdescr$(2%),,43%) = " "
                     str(pfkeys$, 1%, 1%) = hex(ff)
                     str(pfkeys$, 8%, 3%) = hex(ffffff)

L40145:           if fieldnr% = 4% and tojobok% = 0% then task$ = " "
                  goto L40380

            deffn'111(fieldnr%)
                  pfdescr$(1) = "(1)Start Over  (8)See Reported Routing  ~
        ~    (13)Instructions   (15)Print Screen"
                  pfdescr$(2) = "(9)Job Summary (10)Jobs Using This Part ~
        ~    (24)Locations (16)Report Completion"
                  pfkeys$ = hex(01ffffffffffff08090aff0c0dff0f100018)
                  task$ = "Y"
                  if fieldnr% = 0% then L40280
                     init(hex(84)) lfac$() : tfac$ = hex(ac)
                     str(pfdescr$(2%),58%) = " "
                     str(pfkeys$,16%, 1%) = hex(ff)

                  if fieldnr% = 4% and tojobok% = 0% then task$ = " "
                  goto L40380

L40280:           init(hex(86)) lfac$() : str(lfac$(),,2) = hex(8484)
                  init(hex(8c)) vfac$()
                     if qty$(2%) <> "0" and value$(2%) = "      0.00"    ~
                        then vfac$(2%) = hex(94)
                     if qty$(1%) <> "0" and value$(1%) = "      0.00"    ~
                        then vfac$(1%) = hex(94)
                  tfac$ = hex(ae)

*              IF ABS(QTY(1)) < .001 AND ABS(QTY(2)) < .001 THEN 40380
*                 STR(PFDESCR$(1),16,28)="(4)Edit Rework/Scrap deflts"
*                 STR(PFKEYS$,4,1) = HEX(04)

L40380:           on fieldnr% gosub L40590,         /* JOB NUMBER       */~
                                    L40590,         /* PART YIELDED     */~
                                    L40610,         /* QUANTITY REPORTED*/~
                                    L40580,         /* GOOD STORE       */~
                                    L40590,         /* GOOD LOT         */~
                                    L40590,         /* SCRP STORE       */~
                                    L40590,         /* SCRP LOT         */~
                                    L40620,         /* SCRP VALUE       */~
                                    L40620,         /* SCRP VALUE       */~
                                    L40590,         /* RWRK DATE        */~
                                    L40620,         /* RWRK VALUE       */~
                                    L40620,         /* RWRK VALUE       */~
                                    L40590,         /* COST METHOD      */~
                                    L40590,         /* ADD VALUE        */~
                                    L40590,         /* ADD VALUE ACCT   */~
                                    L40590          /* KIT OPTIONS      */
                  goto L40660

                  lfac$(fieldnr%) = hex(80) : return    /* Upper/Lower */
L40580:           if tojobok% = 0% then lfac$(20) = hex(81)
L40590:           lfac$(fieldnr%) = hex(81) : return    /* Upper Only  */
L40610:           if adjqtyok% = 0% then lfac$(19) = hex(82)
L40620:           lfac$(fieldnr%) = hex(82) : return    /* Numeric     */

L40660: accept                                                           ~
               at (01,02), "Report completion of jobs",                  ~
               at (01,40), "Post Date:", fac(hex(8c)), postdate$, ch(08),~
               at (01,60), "Todays Date:", fac(hex(8c)), date$  , ch(08),~
               at (02,02), fac(tfac$),   tttle$                 , ch(79),~
               at (03,02), fac(hex(94)), errmsg2$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Job Number   :",                             ~
               at (05,17), fac(lfac$(1)), job$                  , ch(08),~
               at (05,26), fac(hex(8c)),  jobdescr$             , ch(32),~
                                                                         ~
               at (06,02), "Part to Build:",                             ~
               at (06,17), fac(lfac$(2)), part$                 , ch(25),~
               at (06,43), fac(hex(8c)),  partdescr$            , ch(34),~
                                                                         ~
               at (07,02), "Quantity to Build:",                         ~
               at (07,21), fac(hex(84)), qtybld$                , ch(10),~
               at (07,33), "Completed:",                                 ~
               at (07,44), fac(hex(84)), qtycmp$                , ch(10),~
               at (07,56), "Remaining:",                                 ~
               at (07,67), fac(hex(84)), qtylft$                , ch(10),~
               at (08,02), fac(hex(84)), message$               , ch(79),~
               at (09,02), fac(hex(84)), message2$              , ch(79),~
                                                                         ~
               at (10,16),                                               ~
               "+--REMAINING-----FINISHED--+--SCRAPPED--+---REWORK---+", ~
               at (11,07), "Quantity !",                                 ~
               at (11,18), fac(lfac$(19)), adjqty$              , ch(10),~
               at (11,32), fac(lfac$(3)), qty$(3)               , ch(10),~
               at (11,43), "!",                                          ~
               at (11,45), fac(lfac$(3)), qty$(2)               , ch(10),~
               at (11,56), "!",                                          ~
               at (11,58), fac(lfac$(3)), qty$(1)               , ch(10),~
               at (11,69), "!",                                          ~
               at (11,71), fac(hex(8c)), totqty$                , ch(10),~
               at (12,02), "Job/Store/Lot !",                            ~
               at (12,18), fac(lfac$(20)), tojob$               , ch(08),~
               at (12,27), "-or-",                                       ~
               at (12,32), fac(lfac$(4)), str$(3)               , ch(03),~
               at (12,36), fac(lfac$(5)), lot$(3)               , ch(06),~
               at (12,43), "!",                                          ~
               at (12,45), fac(lfac$(6)), str$(2)               , ch(03),~
               at (12,49), fac(lfac$(7)), lot$(2)               , ch(06),~
               at (12,56), "!",                                          ~
               at (12,58), fac(lfac$(10)), rwdate$              , ch(08),~
               at (12,69), "!",                                          ~
               at (13,02), "Rwrk Cmp Date",                              ~
               at (13,16),                                               ~
               "+--------------------------+------------+------------+", ~
               at (14,27), "Component Value !",                          ~
               at (14,45), fac(lfac$( 8)),matl$(2)              , ch(10),~
               at (14,56), "!",                                          ~
               at (14,58), fac(lfac$(11)),matl$(1)              , ch(10),~
               at (14,69), "!",                                          ~
               at (15,27), "    Value Added !",                          ~
               at (15,45), fac(lfac$( 9)),lmwc$(2)              , ch(10),~
               at (15,56), "!",                                          ~
               at (15,58), fac(lfac$(12)),lmwc$(1)              , ch(10),~
               at (15,69), "!",                                          ~
               at (16,43),                                               ~
                                          "+------------+------------+", ~
               at (17,45), fac(vfac$(2%)), value$(2%)           , ch(10),~
               at (17,58), fac(vfac$(1%)), value$(1%)           , ch(10),~
                                                                         ~
               at (18,02), "Valuation Method for Completion - BOM:",     ~
               at (18,41), fac(lfac$(13)), str(costmethod$,1,1) , ch(01),~
               at (18,43), fac(hex(8c)),   str(costdescr$,1,12) , ch(12),~
               at (18,56), "V/Added:",                                   ~
               at (18,65), fac(lfac$(13)), str(costmethod$,2,1) , ch(01),~
               at (18,67), fac(hex(8c)),   str(costdescr$,16,12), ch(12),~
                                                                         ~
               at (19,02), "Add Std. Rte/Misc. Value to Job",            ~
               at (19,34), fac(lfac$(14)), addvalue$            , ch(01),~
               at (19,37), fac(hex(8c)),   adddescr$            , ch(30),~
                                                                         ~
               at (20,02), "Expense Account for Added Value",            ~
               at (20,34), fac(lfac$(15)), acct$                , ch(12),~
               at (20,47), fac(hex(8c)),   acctdescr$           , ch(32),~
                                                                         ~
               at (21,02), fac(hex(8c)), kitmessage$            , ch(30),~
               at (21,34), fac(lfac$(16)), kstr$                , ch(03),~
               at (21,38), fac(lfac$(16)), klot$                , ch(06),~
               at (21,45), fac(lfac$(16)), kitmethod$           , ch(01),~
               at (21,47), fac(hex(8c)), kitmessage2$           , ch(30),~
                                                                         ~
               at (22,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(1)            , ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(2)            , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 8 then L41540
                  passjob$ = job$
                  call "JBSEEACT" (passjob$, #8, #11, #3, #4, #10, #45)
                  goto L40660

L41540:        if keyhit% <> 10 then L41960
                  readkey1$ = part$
                  i(1) = 1.11 : i$() = "JOB ORDER:"
                  descr_map(01) =  12.08 :descr_map(02) = 01   /* Job# */
                  descr_map(03) = -09.28 :descr_map(04) = 10   /* Desc */
                  descr_map(05) =-168.081:descr_map(06) = 39   /* Start*/
                  descr_map(07) =-174.081:descr_map(08) = 50   /* End  */
                  descr_map(09) =  57.08 :descr_map(10) = 67.104 /*Qty */
                  header$(1%) = "  Job Number/Description          " &   ~
                           "      Planned Start / End    Quantity Needed"
                  header$(3%) = hex(a4) & "Below Are Current Jobs That" &~
                           " Are Planned To Use Part " & part$
                  call "PLOWCODE" (#34, readkey1$, " ", 9025%, 1.30,     ~
                             f1%(34), header$(), 0, -12, i(), i$(), "D", ~
                             task$, #4, descr_map())
                  if task$ <> " " then L40660
                  if f1%(34) <> 0% then get #34 using L41900, tojob$
L41900:           FMT POS(12), CH(08)
                  goto L40660

L41960:        if keyhit% <> 12 then L41992
                  gosub L44000
                  goto L40660

L41992:        if keyhit% <> 13 then L42000
                  call "MANUAL" ("JBCMPSUB")
                  goto L40660

L42000:        if keyhit% <> 15 then L42024
                  call "PRNTSCRN"
                  goto L40660

L42024:        if keyhit% <> 24% then L42032
                  gosub locations
                  goto L40660

L42032:        if keyhit% <>  9% then L42040
                  passjob$ = job$
                  call "JBQJOB" (passjob$, #4, #54, #3, #34, #5, #6, #9)
                                    /* JBMASTR2 SYSFILE2 HNYMASTR */
                                    /* PIPOUT JBMATER2 JBVALUE2   */
                  goto L40660

L42040:        if fieldnr% <> 0 then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

L44000: REM *************************************************************~
            * Status Messages Screen                                    *~
            *************************************************************

            if status% = 0% then return
            init (" ") statusmsg$() : tfac$ = hex(ac)

            i% = 0%
            if tojobok% = 0% then L44120
               i% = i% + 1%
               statusmsg$(i%) = "Serial Number Controlled Assembly."

L44120:     if type% <> 0% then L44160
               i% = i% + 1%
               statusmsg$(i%) = "Build to Order Assembly."

L44160:     i% = 2%
            if negbom% = 0% then L44210
               i% = i% + 1%
               statusmsg$(i%) = "Job has NEGATIVE net material costs."

L44210:     if negrte% = 0% then L44250
               i% = i% + 1%
               statusmsg$(i%) = "Job has NEGATIVE net value added costs."

L44250:     i% = 4%
            if ckit% = 0% then L44300
               i% = i% + 1%
               statusmsg$(i%) = "Job has NOT been completely kitted."

L44300:     if ctype% = 0% then L44340
               i% = i% + 1%
               statusmsg$(i%) = "Job has Build to Order components."

L44340:     if cser% = 0% then L44380
               i% = i% + 1%
               statusmsg$(i%) = "Job has Serial # Controlled components."

L44380:     if clot% = 0% then L44420
               i% = i% + 1%
               statusmsg$(i%) = "Job has Lot Controlled components."

L44420:     if cbypr% = 0% then L44451
               i% = i% + 1%
               statusmsg$(i%) = "Job has Unreported By-Products."

L44451:     if badtrans% = 0% then L44455
               statusmsg$(16%) = "Job has Rejected Transactions on File."
               tfac$ = hex(a4)

L44455:     if rtexist% = 0% then rfac$ = hex(9c) else rfac$ = hex(84)

                 pfdescr1$(1) = "(1)Start Over  (8)See Reported Routing  ~
        ~    (13)Instructions   (15)Print Screen"
                 pfdescr1$(2) = "(9)Job Summary (10)Jobs Using This Part ~
        ~    (24)Locations      (16/ENTER)Return"
                 pfkeys1$ = hex(01ffffffffffff08090affff0dff0f100018)

L44720: accept                                                           ~
               at (01,02), "Report completion of jobs",                  ~
               at (01,40), "Post Date:", fac(hex(8c)), postdate$, ch(08),~
               at (01,60), "Todays Date:", fac(hex(8c)), date$  , ch(08),~
               at (02,02), fac(hex(ac)), tttle$                 , ch(79),~
               at (03,02), fac(hex(94)), errmsg2$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Job Number   :",                             ~
               at (05,17), fac(hex(84)),  job$                  , ch(08),~
               at (05,26), fac(hex(8c)),  jobdescr$             , ch(32),~
                                                                         ~
               at (06,02), "Part to Build:",                             ~
               at (06,17), fac(hex(84)),  part$                 , ch(25),~
               at (06,43), fac(hex(8c)),  partdescr$            , ch(34),~
                                                                         ~
               at (07,02), "Quantity to Build:",                         ~
               at (07,21), fac(hex(84)), qtybld$                , ch(10),~
               at (07,33), "Completed:",                                 ~
               at (07,44), fac(hex(84)), qtycmp$                , ch(10),~
               at (07,56), "Remaining:",                                 ~
               at (07,67), fac(hex(84)), qtylft$                , ch(10),~
               at (08,02), fac(tfac$  ), statusmsg$(16)         , ch(79),~
               at (09,02), fac(rfac$),   rtemsg$                , ch(49),~
               at (10,02), "Final Assembly Warnings:",                   ~
               at (11,02), fac(hex(84)), statusmsg$(01)         , ch(79),~
               at (12,02), fac(hex(84)), statusmsg$(02)         , ch(79),~
               at (13,02), "Job Cost Warnings:",                         ~
               at (14,02), fac(hex(84)), statusmsg$(03)         , ch(79),~
               at (15,02), fac(hex(84)), statusmsg$(04)         , ch(79),~
               at (16,02), "Component (Kit List) Warnings:",             ~
               at (17,02), fac(hex(84)), statusmsg$(05)         , ch(79),~
               at (18,02), fac(hex(84)), statusmsg$(06)         , ch(79),~
               at (19,02), fac(hex(84)), statusmsg$(07)         , ch(79),~
               at (20,02), fac(hex(84)), statusmsg$(08)         , ch(79),~
               at (21,02), fac(hex(84)), statusmsg$(09)         , ch(79),~
                                                                         ~
               at (22,02), fac(tfac$  ), statusmsg$(10)         , ch(79),~
               at (23,02), fac(hex(8c)), pfdescr1$(1)           , ch(79),~
               at (24,02), fac(hex(8c)), pfdescr1$(2)           , ch(79),~
                                                                         ~
               keys(pfkeys1$),                                           ~
               key (keyhit%)

               if keyhit% <> 1 then L45650
                  gosub startover
                  goto L44720

L45650:        if keyhit% <> 8 then L45700
                  passjob$ = job$
                  call "JBSEEACT" (passjob$, #8, #11, #3, #4, #10, #45)
                  goto L44720

L45700:        if keyhit% <> 10 then L45900
                  readkey1$ = part$
                  i(1) = 1.11 : i$() = "JOB ORDER:"
                  descr_map(01) =  12.08 :descr_map(02) = 01   /* Job# */
                  descr_map(03) = -09.30 :descr_map(04) = 10   /* Desc */
                  descr_map(05) =-168.081:descr_map(06) = 41   /* Start*/
                  descr_map(07) =-174.081:descr_map(08) = 50   /* End  */
                  descr_map(09) =  57.08 :descr_map(10) = 67.104 /*Qty */
                  header$(1%) = "  Job Number/Description          " &   ~
                           "        Planned Start/End    Quantity Needed"
                  header$(3%) = hex(a4) & "Below Are Current Jobs That" &~
                           " Are Planned To Use Part " & part$
                  call "PLOWCODE" (#34, readkey1$, " ", 9025%, 1.30,     ~
                             f1%(34), header$(), 0, -12, i(), i$(), "D", ~
                             "Y", #4, descr_map())
                  goto L44720

L45900:        if keyhit% <> 13 then L45940
                  call "MANUAL" ("JBCMPSUB")
                  goto L44720

L45940:        if keyhit% <> 15 then L45980
                  call "PRNTSCRN"
                  goto L44720

L45980:        if keyhit% <> 24% then L46020
                  gosub locations
                  goto L44720

L46020:        if keyhit% <>  9% then L46090
                  passjob$ = job$
                  call "JBQJOB" (passjob$, #4, #54, #3, #34, #5, #6, #9)
                                    /* JBMASTR2 SYSFILE2 HNYMASTR */
                                    /* PIPOUT JBMATER2 JBVALUE2   */
                  goto L44720

L46090:        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Tests data for the items on page 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errmsg2$, errormsg$ = " "
                  on fieldnr% gosub L50270,         /* JOB NUMBER       */~
                                         ,         /* PART NUMBER      */~
                                    L50470,         /* QUANTITY REPORTED*/~
                                    L51000,         /* GOOD STORE       */~
                                    L51290,         /* GOOD LOT         */~
                                    L51590,         /* SCRAP STORE      */~
                                    L51700,         /* SCRAP LOT        */~
                                    L51980,         /* SCRAP VALUE      */~
                                    L51980,         /* SCRAP VALUE      */~
                                    L52110,         /* REWORK DATE      */~
                                    L52190,         /* REWORK VALUE     */~
                                    L52190,         /* REWORK VALUE     */~
                                    L52320,         /* COST METHOD      */~
                                    L52430,         /* ADD VALUE        */~
                                    L52570,         /* ADD VALUE ACCT   */~
                                    L52640          /* KIT OPTIONS      */

                     return

L50270: REM TEST DATA FOR JOB NUMBER
                if str(job$,1%,2%) <> "PJ" then L50280
                   errormsg$ = "CANNOT Report Completions For Purchased"&~
                               " Jobs"
                   return
L50280:         if from$ <> "C" then L50360
                   if job$ = jobnr$ then L50320
                   errormsg$ = "Only Job: "&jobnr$&" Can Be Processed"
                   return
L50320:         call "GETCODE" (#4, job$, jobdescr$, 1%, 99, f1%(4))
                     if f1%(4) <> 0 then L50400
                     errormsg$="Job Not On File: " & job$
                     return
L50360:         call "GETCODE" (#4, job$, jobdescr$, 1%, 0, f1%(4))
                     if f1%(4) <> 0 then L50400
                     errormsg$ = "Please Enter An Existing Job Number."
                     return
L50400:         u3% = 2%   /* Check In-use & Write Flag if Free */
                call "JBINUSE" (job$, u3%)
                     if u3% = 0 then L50440
                     errormsg$ = hex(00)
                     return
L50440:         gosub L30000
                if errormsg$ = " " then return
                    call "JBINUSE" (job$, 1%)  /* Clear job in-use */
                return

L50470: REM TEST DATA FOR QUANTITY REPORTED
            call "NUMTEST" (adjqty$, 0, 9e7, errormsg$, 0.2, adjqty)
                if errormsg$ <> " " then return
            adjquan = qtylft - adjqty
            call "NUMTEST" (qty$(3), 0, adjqty, errormsg$, 0.2, qty(3))
                if errormsg$ <> " " then return
            if type% <> 0% then L50527
               if comp_target% <> 0% then L50530
L50527:     if edit% = 0% then L50532
            if qtyhold(3) = 0 then L50532
            if qtyhold(3) >= qty(3) then L50532
L50530:         gosub check_tojob_pip
                   if errormsg$ <> " " then return
L50532:     temp = round(adjqty-qty(3), 2)
            call "NUMTEST" (qty$(2), 0, temp, errormsg$, 0.2, qty(2))
                if errormsg$ <> " " then return
            temp = round(adjqty-qty(3)-qty(2), 2)
            call "NUMTEST" (qty$(1), 0, temp, errormsg$, 0.2, qty(1))
                if errormsg$ <> " " then return
            qty = round(qty(1)+qty(2)+qty(3),2)
            if qty > .009 then L50710
                errormsg$ = "Please Enter Quantity(s)"
                return
L50710:     for c% = 3% to 1% step -(1%)
                trankey$ = str(job$) & bin(c%,3)
                errormsg$ = "*Enter the Serial Numbers to Move Into"
                if c% = 3% then errormsg$ = errormsg$ & " Finished Goods"
                if c% = 2% then errormsg$ = errormsg$ & " Scrap"
                if c% = 1% then errormsg$ = errormsg$ & " Rework"
                call "SERSELCT" (                                        ~
                             part$,      /* Part code                  */~
                             job$,       /* S/N Location to Select from*/~
                             qty(c%),    /* Qty to Assign S/N's To     */~
                             c%,         /* Pointer For Work File Use  */~
                             1%,         /* Average # Lines per Documnt*/~
                             "JC",       /* Source Transaction Type.   */~
                             trankey$,   /* Source Transaction Key.    */~
                             status$(c%),/* Status to Change S/N to.   */~
                             "1",        /* Status to Select/Chg from  */~
                             errormsg$,  /* Returned Error Message     */~
                             #54,        /* SYSFILE2 UFB               */~
                             #3,         /* HNYMASTR UFB               */~
                             #62,        /* SERMASTR UFB               */~
                             #63)        /* SERWORK  UFB               */
                if errormsg$ > " " then c% = 1%
            next c%
            if errormsg$ > " " then return

            call "CONVERT" (qty, -0.2, totqty$)
            if qty(2) > 0 then L50965
               str$(2), lot$(2), matl$(2), lmwc$(2) = " "
               init (hex(00)) scrbcost$, scrmcost$
               matl(2), lmwc(2) = 0
L50965:     if qty(1) > 0 then L50970
               rwdate$, matl$(1), lmwc$(1) = " "
               init (hex(00)) rwkbcost$, rwkmcost$
               matl(1), lmwc(1) = 0
L50970:     return

L51000
*        TEST DATA FOR TO STORE & LOT FINISHED GOODS
          if enabled% = 0 then return
          if tojob$ = " " then L51250
            call "GETCODE" (#4, tojob$, " ", 0%, 0, f1%(4))
               if f1%(4) <> 0 then L51035
                  errormsg$ = "Job Number Is Invalid: " & tojob$
                  return
L51035:        get #4 using L51040, work$, work1$
L51040:            FMT POS(58), CH(25), POS(153), CH(6)
               if work1$ = " " then L51060
                  errormsg$ = "Job Has been Closed: " & tojob$
                  return
L51060:        if work$ <> part$ then L51075
                  errormsg$ = "Job Cannot be for Part:" & part$
                  return
L51075:        if lotenbl%  = 2% then L51085
               if tojobok%  = 0% then L51110
L51085:        call "SERENABL" (work$, temp%, temp1%, #54, #3)
                  if temp% = 0% then L51110
                  errormsg$ = "To Job is for serial numbered item."
                  return :temp1% = temp1%

L51110:        if tojobheld% <> 0% then L51125
                  gosub check_tojob_pip
                    if errormsg$ <> " " then return
                  goto L51145
L51125:        if ctojob$ = tojob$ then L51170
                  gosub check_tojob_pip
                    if errormsg$ <> " " then return
                  if ctojob$ <> " " then call "JBINUSE" (ctojob$, 1%)
                  tojobheld% = 0%
L51145:        u3% = 2%   /* Check In-use & Write Flag if Free */
               call "JBINUSE" (tojob$, u3%)
                  if u3% = 0% then L51170
                  errormsg$ = hex(00)
                  return
L51170:        str$(3), lot$(3) = " " : tojobheld% = 1%
               return

L51250:     call "GETCODE" (#59, str$(3), " ", 0%, 0, f1%(59))
               if f1%(59) <> 0 then L51281
               errormsg$="Store Number Is Invalid: " & str$(3)
               return
L51281:     tojob$ = " "
            if ctojob$    = " " then L51285
            if tojobheld% = 0%  then return
               call "JBINUSE" (ctojob$, 1%)
L51285:        tojobheld% = 0%
               return

L51290
*        Test Data for TO STORE & LOT Finished Goods
        if enabled% = 0 then return
            if type% <> 0% then L51400
                if lot$(3) <> " " then L51350
                     errormsg$ = "Lot Number Can't Be Blank"
                     return
L51350:         readkey1$ = str(part$) & str(str$(3)) & lot$(3)
                hnyqcmthd$ = hex(ff)
                call "READ100" (#52, readkey1$, f1%(52))
                if f1%(52) = 0% then return
                     errormsg$ = "Lot Number has already been used"
                     return
L51400:     if lotenbl% <> 2% or lot$(3) <> " " then L51430
                errormsg$ = "Lot Number required for this Part"
                return
L51430
*          IF STR$(3) = LASTSTR$ AND LOT$(3) = LASTLOT$ THEN RETURN
                hnyqcmthd$ = hex(ff)
                readkey1$ = str(part$) & str(str$(3)) & lot$(3)
                call "READ100" (#52, readkey1$, f1%(52))
                if f1%(52) = 0% then L51550
                get #52 using L51463, hnyqcmthd$
L51463:             FMT POS(403), CH(1)
            if str$(3) = laststr$ and lot$(3) = lastlot$ then return
L51470:              u3% = 2%
                     call "ASKUSER" (u3%, "STORE/LOT EXISTS",            ~
                          "This Store/Lot already exists.",              ~
                          "Press PF-16 to add to this Store/Lot",        ~
                          "-or- RETURN to re-enter data.")
                     if u3% <> 0% and u3% <> 16% then L51470
                     if u3% =  0% then errormsg$ = hex(00)
                     return
L51550:         call "LOTVALID" (part$, str$(3), lot$(3), #54, #3, #52,  ~
                                 errormsg$)
                return

L51590
*        TEST DATA FOR SCRAP STORE
            if enabled% = 0 then return
            u% = 2 : if fieldnr% > 6 then u% = 1
            call "GETCODE" (#59, str$(u%), " ", 0%, 0, f1%(59))
               if f1%(59) <> 0 then L51660
               errormsg$="Store Number Is Invalid: " & str$(u%)
               return
L51660:     if str(str$(u%),,1) >"9" or str(str$(u%),,1)< "0" then return
               errormsg$="Store Number Must Be Non-Numeric: " & str$(u%)
               return

L51700
*        TEST DATA FOR SCRAP LOT
            if enabled% = 0 then return
            if lot$(2)<>" " then L51870
               call "READ101" (#54, "SWITCHS.HNY", f1%(54))
               if f1%(54) = 0% then L51870
                 get #54 using L51760, lotreq$
L51760:             FMT        POS(92), CH(1)
                 if lotreq$ <> "Y" then L51870
                 readkey1$ = str(part$,,)
                 call "READ101" (#3, readkey1$, f1%(3))
                 if f1%(3) = 0% then L51870
                   get #3 using L51820, lotreq$
L51820:               FMT    POS(130), CH(1)
                   if lotreq$ <> "Y" then L51870
                 errormsg$ = "Lot Number required for this Part"
                 return
L51870
*          READKEY1$ = STR(PART$) & STR(STR$(2)) & LOT$(2)
*          CALL "READ100" (#52, READKEY1$, F1%(52))
*          IF F1%(52) = 0% THEN 51920
*              ERRORMSG$ = "This Lot Number has already Been Used"
*              RETURN
            errormsg$ = "LOT-CHECK"
            call "LOTVALID" (part$, str$(2), lot$(2), #54, #3, #52,      ~
                                                              errormsg$)
            return


L51980: REM TEST DATA FOR SCRAP VALUE
*          MATL(2) = 0 : LMWC(2) = 0
*          IF ENABLED% = 0 THEN RETURN
*          TEMP = 9E7
*          IF KIT% = 0 THEN TEMP=ROUND((MLO(1)-MATL(1)*QTY(1))/QTY(2),4)
*          CALL "NUMTEST" (MATL$(2), 0, TEMP, ERRORMSG$, -2.4, MATL(2))
*              IF ERRORMSG$ <> " " THEN RETURN
*          IF KIT% = 0 THEN TEMP=ROUND((MLO(2)-LMWC(1)*QTY(1))/QTY(2),4)
*          CALL "NUMTEST" (LMWC$(2), 0, TEMP, ERRORMSG$, -2.4, LMWC(2))
*              IF ERRORMSG$ <> " " THEN RETURN
            call "CONVERT" (matl(2)+lmwc(2), 2.4, value$(2))
            return

L52110: REM TEST DATA FOR TO REWORK DATE
            if enabled% = 0 then return
            call "DATEOK" (rwdate$, u3%, errormsg$)
                if errormsg$ <> " " then return
            if u3% >= jtoday% then return
                errormsg$ = "Can't be before today: " & rwdate$
               return

L52190: REM TEST DATA FOR REWORK VALUE
*          MATL(1) = 0 : LMWC(1) = 0
*          IF ENABLED% = 0 THEN RETURN
*          TEMP = 9E7
*          IF KIT% = 0 THEN TEMP=ROUND((MLO(1)-MATL(2)*QTY(2))/QTY(1),4)
*          CALL "NUMTEST" (MATL$(1), 0, TEMP, ERRORMSG$, -2.4, MATL(1))
*              IF ERRORMSG$ <> " " THEN RETURN
*          IF KIT% = 0 THEN TEMP=ROUND((MLO(2)-LMWC(2)*QTY(2))/QTY(1),4)
*          CALL "NUMTEST" (LMWC$(1), 0, TEMP, ERRORMSG$, -2.4, LMWC(1))
*              IF ERRORMSG$ <> " " THEN RETURN
            call "CONVERT" (matl(1)+lmwc(1), 2.4, value$(1))
            return

L52320: REM TEST DATA FOR COST METHOD
            costdescr$ = " "
            on pos("APSZB" = str(costmethod$,1,1)) goto                  ~
                            L52343, L52344, L52345, L52346, L52347
               goto L52400
L52343:        str(costdescr$,,15) = "(All)          " : goto L52370
L52344:        str(costdescr$,,15) = "(Prorate)      " : goto L52370
L52345:        str(costdescr$,,15) = "(Standard)     " : goto L52370
L52346:        str(costdescr$,,15) = "(Zero Cost)    " : goto L52370
L52347:        str(costdescr$,,15) = "(BOM Cost)     "
               if adjqty = qty then L52408
                   call "SHOSTAT" ("Analyzing Job's BOM Costs.")
                   call "JBBOMCST" (part$, job$, 1%, #45, #5, #3, cost(),~
                                           tot_cost, stdate%, errormsg$)
                   tot_cost = tot_cost /* Compiler thing */
                   if errormsg$ <> " " then return

L52370:     on pos("APSZ" = str(costmethod$,2,1)) goto                   ~
                            L52373, L52374, L52375, L52376
               goto L52404
L52373:        str(costdescr$,16) = "(All)          " : goto L52390
L52374:        str(costdescr$,16) = "(Prorate)      " : goto L52390
L52375:        str(costdescr$,16) = "(Standard)     " : goto L52390
L52376:        str(costdescr$,16) = "(Zero Cost)    " : goto L52390

L52390:        return
L52400:     errormsg$ = "Enter A, P, S, B or Z as Indicated for BOM."
            return
L52404:     errormsg$ = "Enter A, P, S or Z as Indicated for Added Value."
            return
L52408:     errormsg$ = "'B' is not valid for Final Completions."
            return

L52430: REM TEST DATA FOR ADD VALUE
            adddescr$ = " "
            if addvalue$ <> " " then L52480
               acct$, acctdescr$ = " "
               return
L52480:     if addvalue$ <> "A" then L52510
               adddescr$ = "(Add for Entire Job Quantity)"
               return
L52510:     if addvalue$ <> "P" then L52540
               adddescr$ = "(Proportional - Backflush)"
               return
L52540:     errormsg$ = "Enter Blank, A for ALL or P for Proportional."
            return

L52570: REM TEST DATA FOR ADD VALUE ACCOUNT
            if enabled% = 0% then return
            call "GETCODE" (#12, acct$, acctdescr$, 1%, 0, f1%(12))
               if f1%(12) <> 0% then return
            errormsg$ = "Enter Expense Account for Value Added."
            return

L52640: REM TEST DATA FOR KIT OPTIONS
            if kstr$ <> " " then L52680
               kstr$, klot$, kitmethod$, kitmessage2$ = " "
               return
L52680:     call "GETCODE" (#59, kstr$, " ", 0%, 0, f1%(59))
               if f1%(59) <> 0 then L52720
               errormsg$="Kit From Store Number Is Invalid: " & kstr$
               return
L52720:     if kitmethod$ = " " then kitmethod$ = "A"
            if kitmethod$ <> "A" then L52760
               kitmessage2$ = "(All Remaining Components)"
               return
L52760:     if kitmethod$ <> "P" then L52790
               kitmessage2$ = "(Proportional - Backflush)"
               return
L52790:     errormsg$ = "Indicate A for ALL or P for Proportional."
            return

*       *** Check PIPOUT if completing to a job, WARN someone if it
*       *** doesn't seem to be logicial

        check_tojob_pip

            if tojob$ = " " then return
            if qty(3) = 0 then return

            readkey$ = "JOB ORDER: " & str(tojob$,,8%) & part$
            init(hex(00)) str(readkey$,45) : tojobqty = 0

L53110:     call "PLOWNEXT" (#34, readkey$, 44%, f1%(34%))
               if f1%(34%) = 0% then L53190
            get #34 using L53140, temp
L53140:         FMT POS(57), PD(14,4)
            if temp  <= 0 then L53110
            tojobqty = tojobqty + temp
            goto L53110

L53190:     if qty(3) <= tojobqty then return

L53210: %Completion of ######################### to job: ########
L53220: %Kit List Requires: ########.## : completion is for ########.##

            ask$() = " "
            put ask$(1) using L53210, part$, tojob$
            put ask$(2) using L53220, tojobqty, qty(3)
            ask$(3) = "Press PF8 to Continue. Press PF1 to Re-enter."

L53280:     u3% = 2%
            call "ASKUSER" (u3%, "*** To Job Quantity Warning ***",      ~
                            ask$(1), ask$(2), ask$(3))
            if u3%  = 8% then return
            if u3% <> 1% then L53280
               errormsg$ = "Completeion Quantity/To Job Requirement is" &~
                           " inconsistent."
               return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
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

        exit_program
            if postflag% = 0% then call "JBJNLCLS" ("J2", userid$,       ~
                              moduleno$, jnlid$, pstseq%, ret%)
            end
