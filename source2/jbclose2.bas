        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  JJJJJ  BBBB    CCC   L       OOO    SSS   EEEEE   222    *~
            *    J    B   B  C   C  L      O   O  S      E          2   *~
            *    J    BBBB   C      L      O   O   SSS   EEEE    222    *~
            *  J J    B   B  C   C  L      O   O      S  E      2       *~
            *   J     BBBB    CCC   LLLLL   OOO    SSS   EEEEE  22222   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBCLOSE2 - Herein is provided the means to close a Job.   *~
            *            In order to close Job debits and credits must  *~
            *            equal and no remaining qty to build may exist. *~
            *            The program calls several maintenance subs to  *~
            *            allow adjusting of job costs.                  *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1984, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/23/84 ! ORIGINAL                                 ! BLT *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION - ! RAC *~
            * 12/16/85 ! CHANGED CALL SYNTAX TO SEVERAL SFC SUBS  ! HES *~
            * 03/12/86 ! Added call to CDANPOST.                  ! LDJ *~
            * 10/22/86 ! WCout File Format Changes                ! HES *~
            * 01/06/87 ! WCMASTR delete fix                       ! HES *~
            * 02/20/87 ! Added Serial Numbers Support             ! LDJ *~
            * 07/15/87 ! Standard Cost Enhancement Project        ! ERN *~
            * 03/02/88 ! Change Close Test To Check Totals Only   ! HES *~
            * 06/15/88 ! Added call to JBQCREDT.                  ! JIM *~
            * 10/03/88 ! Slightly Loosened the Requir to Close Job! JDH *~
            * 10/25/88 ! Don't Reverse Committed if from a Job    ! KAB *~
            * 01/19/90 ! Add Alt Key to JBMASTR2 Select Statement ! SID *~
            * 10/12/92 ! Set in-use flag as in other pgms.        ! JDH *~
            * 12/15/92 ! Relieved hold after call to JBDIRSUB.    ! JDH *~
            *          ! Returns to EDITMODE after JBQCREDT call. !     *~
            * 05/26/93 ! CORE Project                             ! KAB *~
            *          !   Visibility of Core Ledger              !     *~
            *          !   Write Cost Set ID to JBMASTR2 on close !     *~
            *          !   Pass New Channels to SUBS              !     *~
            *          !   Warning if Bad Transaction             !     *~
            *          !   Set EST % Complete to 100% on Close    !     *~
            *          !   Handle Rwk PIPIN & On Order via Flag   !     *~
            *          !   Better Handling of Commited Qty        !     *~
            *          !   Release in use prior to JBDIRSUB JIC   !     *~
            * 08/13/93 ! PRR 10941 - Added #16 to JBCMPSUB call.  ! MLJ *~
            * 08/23/93 ! PRR 10576 - Added #51, JBTCBUF2 - Cannot ! MLJ *~
            *          !   close job if bad or unposted time card !     *~
            *          !   labor exists - CLSMSG$(6).             !     *~
            *          ! PRR 11192 - Added VBKLINES, (3)See POs.  !     *~
            *          !   Searches for non-zero open qtys for    !     *~
            *          !   this job.  An ASKUSER msg provides the !     *~
            *          !   results of the search.                 !     *~
            *          ! Purchased Jobs - Cannot close a purchased!     *~
            *          !   job if it has a PO open quantity,      !     *~
            *          !   CLSMSG$(7).                            !     *~
            * 01/12/95 ! Fixed validation of fields > 2.          ! JDH *~
            * 08/12/96 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~

        dim                                                              ~
            actclose$8,                  /* Actual Closing Date        */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            buf2job$8,                   /* JBTCBUF2 Job Number        */~
            costmsg$(3)60,               /* Cost Messages              */~
            cursor%(2),                  /* Cursor Location for Edit   */~
            date$8,                      /* Today's Date               */~
            dfac$1,                      /* Display FAC                */~
            edtmessage$79,               /* Edit Screen Message        */~
            errormsg$79,                 /* Error Message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Input Message              */~
            jbnr$8, jbdescr$30,          /* Job number and Descr       */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* 2nd Screen Line            */~
            location$30,                 /* Serial Number Location     */~
            lot$6,                       /* Lot Number                 */~
            modno$2,                     /* Module Number              */~
            clsmsg$(7)79,                /* Messages                   */~
            part$25, partdescr$32,       /* Job Part                   */~
            parta$25,                    /* Part Number                */~
            plenddate$10,                /* Planned Finish             */~
            plstdate$10,                 /* Planned Start              */~
            po$16,                       /* Purchase Order Number      */~
            poline$3,                    /* Purchase Order Line Number */~
            postc$1,                     /* Post TC To Job Flag        */~
            poqty$10,                    /* PO Open Quantity           */~
            postdate$8,                  /* User's Posting Date (SFC)  */~
            qtycomp$10,                  /* Quantity complete          */~
            qtymake$10,                  /* Quantity to make           */~
            readkey$100, readkeys$100,   /* Working strings            */~
            rwkpip$1,                    /* Rework PIP Flag            */~
            savejob$8,                   /* Save Job Number            */~
            set$8, set_id$4,             /* Cost Set Id at Closing     */~
            stdbom(12), stdbom$96,       /* Std Costs- BOM             */~
            stdfold(12),                 /*            Fold-in         */~
            stdmsc(12), stdmsc$96,       /*            Miscellaneous   */~
            stdrte(12), stdrte$96,       /*            Route/Labor     */~
            stdate$10,                   /* Actual Start               */~
            store$3,                     /* Store number               */~
            temp$8,                      /* Temporary Date             */~
            used%(490),                  /* Work Center Usage          */~
            userid$3,                    /* Current User's ID          */~
            vbkljob$8,                   /* VBKLINES Job Number        */~
            vendor$9,                    /* VBKLINES Vendor Number     */~
            wc$6, wcctl$4,               /* Work Center                */~
            wc$(2000)6,                  /* Work Center Stack          */~
            wc%(2000),                   /* Work Center Usage          */~
            wckey$50,                    /* Work Center Read Key       */~
            wipacct$16, wipdescr$32      /* WIP Account                */


        dim f1%(64),                     /* Read Status                */~
            f2%(64),                     /* Open Status                */~
            fs%(64),                     /* Open Status                */~
            rslt$(64)20                  /* Open Return Message        */

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
            * #1  ! USERINFO ! Security file / Posting Dates            *~
            * #2  ! SYSFILE2 ! System Information File                  *~
            * #3  ! JBMASTR2 ! Job Master                               *~
            * #4  ! HNYMASTR ! Inventory Master                         *~
            * #5  ! HNYQUAN  ! Inventory Quantity & GL defaults         *~
            * #6  ! GLMAIN   ! G/L Chart of Accounts                    *~
            * #7  ! GENCODES ! General Codes File                       *~
            * #8  ! PIPIN    ! Planned IN's to Inventory                *~
            * #9  ! PIPMASTR ! Planned position master                  *~
            * #10 ! SFCUM2   ! So as not to confuse pipflags            *~
            * #12 ! JBSTATUS ! Job status tracking file                 *~
            * #13 ! RTEMASTR ! Standard & alt work center routings      *~
            * #15 ! WCMASTR  ! Work center master file                  *~
            * #16 ! WCOUT    ! Work center useage cross reference       *~
            * #17 ! PIPOUT   ! Pip quants due out                       *~
            * #18 ! JBMATER2 ! Job material ledger                      *~
            * #19 ! JBCREDIT ! Job credits ledger                       *~
            * #20 ! JBVALUE2 ! Job Value Ledger                         *~
            * #21 ! JBMASTRC ! Job Master Core Apppendix                *~
            * #25 ! STORNAME ! Store master                             *~
            * #36 ! JBPIPXRF ! Pip cross reference master               *~
            * #45 ! JBCROSS2 ! Job rte/bom used cross ref.              *~
            * #50 ! JBTIF    ! Job Transactions File                    *~
            * #51 ! JBTCBUF2 ! Time Card Transaction Detail File        *~
            * #52 ! VBKLINES ! Backlog line item file                   *~
            * #61 ! SERTIF   ! Serial Numbers Transaction Image File    *~
            * #62 ! SERMASTR ! Serial Numbers Master File               *~
            * #63 ! SERWORK  ! Serial Numbers Work File                 *~
            * #64 ! HNYPOOL  ! Inventory LIFO/FIFO pool records         *~
            *************************************************************

            select  #1, "USERINFO",                                      ~
                        varc, indexed, recsize = 150,                    ~
                        keypos = 1, keylen = 3

            select  #2, "SYSFILE2",                                      ~
                        varc, indexed, recsize = 500,                    ~
                        keypos = 1, keylen = 20

            select  #3, "JBMASTR2",                                      ~
                        varc, indexed, recsize = 1300,                   ~
                        keypos = 1, keylen = 8,                          ~
                        alt key 1, keypos = 1120, keylen = 19, dup,      ~
                            key 2, keypos =   58, keylen = 25, dup

            select  #4, "HNYMASTR",                                      ~
                        varc, indexed, recsize = 900,                    ~
                        keypos = 1, keylen = 25,                         ~
                        alt key  1, keypos = 102, keylen = 9, dup,       ~
                            key  2, keypos = 90 , keylen = 4, dup

            select  #5, "HNYQUAN",                                       ~
                        varc, indexed, recsize = 650,                    ~
                        keypos = 17, keylen = 44,                        ~
                        alt   key 1, keypos =  1, keylen = 44

            select  #6, "GLMAIN",                                        ~
                        varc, indexed, recsize = 300,                    ~
                        keypos = 1, keylen = 9

            select  #7, "GENCODES",                                      ~
                        varc, indexed, recsize = 128,                    ~
                        keypos = 1, keylen = 24

            select  #8, "PIPIN",                                         ~
                        varc, indexed, recsize =  60,                    ~
                        keypos = 30, keylen = 19,                        ~
                        alt key   1, keypos = 1, keylen = 48

            select  #9, "PIPMASTR",                                      ~
                        varc, indexed, recsize = 2024,                   ~
                        keypos = 2, keylen = 25,                         ~
                        alt key  1, keypos = 1, keylen = 26

            select #10, "SFCUM2",                                        ~
                        varc, indexed, recsize = 1985,                   ~
                        keypos = 1, keylen = 25

           select #12, "JBSTATUS",                                       ~
                        varc, indexed, recsize = 200,                    ~
                        keypos= 1, keylen = 12,                          ~
                        alt key 1, keypos =  21, keylen = 44,            ~
                            key 2, keypos =  29, keylen = 36

           select #13, "RTEMASTR",                                       ~
                        varc, indexed, recsize = 400,                    ~
                        keypos =  5, keylen = 31,                        ~
                        alt key   1, keypos =  1, keylen = 35

           select #15, "WCMASTR",                                        ~
                        varc, indexed, recsize = 2024,                   ~
                        keypos =  2, keylen = 5,                         ~
                        alt key   1, keypos =  1, keylen = 6

           select #16, "WCOUT",                                          ~
                        varc, indexed, recsize =  68,                    ~
                        keypos = 9, keylen = 23,                         ~
                        alt key  1, keypos =  1, keylen = 27

           select #17, "PIPOUT",                                         ~
                        varc, indexed, recsize =  64,                    ~
                        keypos =  1, keylen =  56,                       ~
                        alt key   1, keypos = 20, keylen = 37

            select #18, "JBMATER2",                                      ~
                        varc, indexed, recsize = 400,                    ~
                        keypos = 1, keylen = 22,                         ~
                        alt key  1, keypos = 23, keylen = 48

           select #19, "JBCREDIT",                                       ~
                        varc, indexed, recsize = 500,                    ~
                        keypos =  1, keylen = 22,                        ~
                        alt key   1, keypos = 23,  keylen = 48

           select #20, "JBVALUE2",                                       ~
                        varc, indexed, recsize = 300,                    ~
                        keypos =  1, keylen = 23

           select #21, "JBMASTRC",                                       ~
                        varc, indexed, recsize = 600,                    ~
                        keypos =  1, keylen =  8

           select #25, "STORNAME",                                       ~
                        varc, indexed, recsize = 300,                    ~
                        keypos  =  1,  keylen  =  3

           select #36, "JBPIPXRF",                                       ~
                        varc, indexed, recsize =  63,                    ~
                        keypos = 1, keylen = 63,                         ~
                        alt key  1, keypos = 45, keylen = 19

           select #45, "JBCROSS2",                                       ~
                        varc, indexed, recsize =  94,                    ~
                        keypos = 29, keylen = 19,                        ~
                        alt key   1, keypos =  1, keylen = 47,           ~
                            key   2, keypos = 48, keylen = 47

           select #50, "JBTIF",                                          ~
                        varc, indexed, recsize = 350,                    ~
                        keypos =  9, keylen = 18,                        ~
                        alt key   1, keypos =  1, keylen = 26

            select #51, "JBTCBUF2",                                      ~
                        varc, indexed, recsize = 200,                    ~
                        keypos = 1, keylen = 25

            select #52, "VBKLINES",                                      ~
                        varc, indexed, recsize = 700,                    ~
                        keypos =    1, keylen =  28,                     ~
                        alt key 1, keypos = 333, keylen = 20, dup

            select #61, "SERTIF",                                        ~
                        varc, indexed, recsize = 100,                    ~
                        keypos = 1, keylen = 62

            select #62, "SERMASTR",                                      ~
                        varc, indexed, recsize = 300,                    ~
                        keypos = 52, keylen =  45,                       ~
                        alt key   1, keypos =  32, keylen =  45,         ~
                            key   2, keypos =   1, keylen =  76

            select #63, "SERWORK",                                       ~
                        varc, indexed, recsize = 48,                     ~
                        keypos = 1, keylen = 23

            select #64, "HNYPOOL",                                       ~
                        varc, indexed, recsize = 300,                    ~
                        keypos = 1, keylen = 38

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (# 1, fs%( 1), f2%( 1),   0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2),   0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 100%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4),   0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5),   0%, rslt$( 5))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6),   0%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7),   0%, rslt$( 7))
            call "OPENCHCK" (# 8, fs%( 8), f2%( 8), 100%, rslt$( 8))
            call "OPENCHCK" (# 9, fs%( 9), f2%( 9),   0%, rslt$( 9))
            call "OPENCHCK" (#10, fs%(10), f2%(10),   0%, rslt$(10))
            call "OPENCHCK" (#12, fs%(12), f2%(12),   0%, rslt$(12))
            call "OPENCHCK" (#13, fs%(13), f2%(13),   0%, rslt$(13))
            call "OPENCHCK" (#15, fs%(15), f2%(15),   0%, rslt$(15))
            call "OPENCHCK" (#16, fs%(16), f2%(16),   0%, rslt$(16))
            call "OPENCHCK" (#17, fs%(17), f2%(17),   0%, rslt$(17))
            call "OPENCHCK" (#18, fs%(18), f2%(18),   0%, rslt$(18))
            call "OPENCHCK" (#19, fs%(19), f2%(19), 100%, rslt$(19))
            call "OPENCHCK" (#20, fs%(20), f2%(20), 100%, rslt$(20))
            call "OPENCHCK" (#21, fs%(21), f2%(21),   0%, rslt$(21))
            call "OPENCHCK" (#25, fs%(25), f2%(25),   0%, rslt$(25))
            call "OPENCHCK" (#36, fs%(36), f2%(36),   0%, rslt$(36))
            call "OPENCHCK" (#45, fs%(45), f2%(45),   0%, rslt$(45))
            call "OPENCHCK" (#50, fs%(50), f2%(50), 100%, rslt$(50))
            call "OPENCHCK" (#51, fs%(51), f2%(51),   0%, rslt$(51))
            call "OPENCHCK" (#52, fs%(52), f2%(52),   0%, rslt$(52))
            call "OPENCHCK" (#61, fs%(61), f2%(61),   0%, rslt$(61))
            call "OPENCHCK" (#64, fs%(64), f2%(64),   0%, rslt$(64))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date : call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)
            edtmessage$ = "To Modify Displayed Values, Position Cursor" &~
                          " to Desired Value and Press RETURN."
            str(line2$,62%) = "JBCLOSE2: " & str(cms2v$,,8%)

            call "PIPINDEX" (#2, " ", today%, 0%)

            modno$ = "03"
            call "READ100" (#1, userid$, f1%(1))   /* USERINFO */
            if f1%(1) = 0% then L09200 else get #1 using L09170, postdate$
L09170:         FMT XX(33), CH(6)        /* SFC Date */
            call "WHICHMON" (#2, postdate$, u3%)
            if u3% > 0% then L09250
L09200:         call "ASKUSER" (2%, "INVALID POSTING DATE",              ~
                          "Your Post Date for Shop Floor is invalid",    ~
                          "or not on file.  Please correct.",            ~
                          "Press RETURN to return to menu...")
                goto exit_program
L09250:     call "DATEFMT" (postdate$)

            set$ = " "
            call "STCSETID" (1%, #2, set$, set_id$)

            readkey$ = "SWITCHS.SFC"
            call "READ100" (#2, readkey$, f1%(2%))
               if f1%(2%) = 0% then L10000
            get #2 using L09340, postc$, rwkpip$
L09340:         FMT POS(27), CH(1), POS(70), CH(1)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Input mode main program.                                  *~
            *************************************************************

        inputmode
            init(" ") jbnr$, errormsg$
            call "JBINUSE" (" ", 1%)  /* Clears In-use Flag, if there. */

        inputmode2
            init(" ") actclose$, jbdescr$, part$, partdescr$, plenddate$,~
                      plstdate$, qtycomp$, qtymake$, readkey$, savejob$, ~
                      stdate$, temp$, wipacct$, wipdescr$, clsmsg$()
            qtymake, qtycomp, oldmake = 0
            stdate%, enddate% = 0%

            for fieldnr% = 1% to 1%
                gosub'051(fieldnr%)
                     if enabled% = 0% then L10230
L10190:         gosub'101(fieldnr%)
                     if keyhit%  =  1% then gosub startover
                     if keyhit%  = 16% then exit_program
                     if keyhit% <>  0% then L10190
L10230:         gosub'151(fieldnr%)
                     if errormsg$ <> " " then L10190
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of edit mode for linear screens.        *~
            *************************************************************

        editmode
L11070:     gosub'111(0%)
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  3% then check_for_po
                if keyhit%  =  7% then call_jbcmpsub
                if keyhit%  =  8% then call_jbhnysub
                if keyhit%  =  9% then call_jbdirsub
                if keyhit%  = 10% then call_jbqcredt
                if keyhit%  = 12% then call_jbqjob
                if keyhit%  = 16% then close_job
                if keyhit%  = 32% then save_job
                if keyhit% <>  0% then L11070
            fieldnr% = cursor%(1) - 4%
            if fieldnr% = 2% then L11210
            fieldnr% = cursor%(1) - 5%
            if fieldnr% < 3% or fieldnr% > 7% then L11070

L11210:     gosub'052(fieldnr%)   /* Sets input message only  */
L11220:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11220
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11220
            goto L11070


        call_jbcmpsub
            gosub save_data
            savejob$ = jbnr$
            call "JBINUSE" (jbnr$, 1%)  /* Clear job in-use */
            call "JBCMPSUB" (modno$, "MPC", #4, #3, #19, #12, #9, #13,   ~
                             #16, #15, #45, #5, #2, #25, #1, #17, #6,    ~
                             #18, #20, #61, #62, #63, jbnr$, "C")
            jbnr$ = savejob$
            call "JBINUSE" (jbnr$, 1%)  /* Clear job in-use */
            goto  L11540

        call_jbqcredt
            call "JBQCREDT" (jbnr$, #2, #4, #18, #19, #3, #20, #17, #5,  ~
                #64, #21)
            goto editmode

        call_jbhnysub
            gosub save_data
            savejob$ = jbnr$
            call "JBINUSE" (jbnr$, 1%)  /* Clear job in-use */
            call "JBHNYSUB" (modno$, "MPW", #1, #2, #3, #4, #5, #18, #25,~
                             #61, #62, #63, #17, jbnr$, "RJ", f2%(2))
            jbnr$ = savejob$
            goto L11540

        call_jbdirsub
            gosub save_data
            savejob$ = jbnr$
            call "JBINUSE" (jbnr$, 1%)  /* Clear job in-use */
            call "JBDIRSUB" (modno$, "MVA", " ", " ", jbnr$, "C",        ~
                             #2, #3, #4, #6, #1, #15, #7)
            jbnr$ = savejob$
            call "JBINUSE" (jbnr$, 1%)  /* Clear job in-use */
L11540:     errormsg$ = hex(84) & "Transactions (if any) have been" &    ~
                        " queued for posting. Try to close the Job now."
            goto inputmode2

        call_jbqjob
            u3% = 99%
            if oldmake = qtymake then L11690
L11610:         u3% = 2%
                call "ASKUSER" (u3%, "COST DISPLAY ROUTINE",             ~
                "The display data will not reflect unsaved changes,",    ~
                "To see current info the Job data needs to be saved.",   ~
                "Press RETURN to Continue, PF-1 to Exit, PF-16 to Save.")
                if u3% = 1% then editmode
                if u3% = 0% then L11690
                if u3% = 16% then gosub save_data else L11610
L11690:     savejob$ = jbnr$
            call "JBQJOB" (jbnr$, #3, #2, #4, #17, #18, #20, #21)
            jbnr$ = savejob$
            if u3% = 16% then gosub dataload
            goto editmode

        check_for_po
            init(hex(00)) readkey$
            call "READ104" (#52, readkey$, f1%(52%))
                if f1%(52%) = 0% then L12000
                     goto L11870
L11850:     call "READNEXT" (#52, f1%(52%))
                if f1%(52%) = 0% then L12000
L11870:     get #52 using L11880, vendor$, po$, poline$, poqty, vbkljob$
L11880:         FMT CH(9), CH(16), CH(3), POS(109), PD(14,4), POS(166),  ~
                    CH(8)
            if vbkljob$ <> jbnr$ then L11850
                if poqty = 0 then L11850
            call "CONVERT" (poqty, -2.2, poqty$)
L11930:     u3% = 99%
            call "ASKUSER" (u3%, "*** OPEN PO QUANTITY ***",             ~
                "PO: " & po$ &" Line: " & poline$ & " for Vendor: " &    ~
                vendor$ & " has an open quantity of " & poqty$ & " ,",   ~
                "Press RETURN to continue searching or PF(1) to Return "&~
                "to Edit Mode.")
            if u3% = 0% then L11850
                if u3% = 1% then editmode else L11930
L12000:     u3% = 99%
            call "ASKUSER" (u3%, "*** OPEN PO QUANTITY ***",             ~
                 "No POs found with open quantities for JOB: " & jbnr$,  ~
                 " ", "Press RETURN to acknowledge and contiue Editing.")
            if u3% = 0% then editmode else L12000

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after input/editing.                   *~
            *************************************************************

        close_job
            savejob$ = jbnr$  /* Just in case */
*          U3% = 2%  /* Check in use and hold job if free */
*          CALL "JBINUSE" (JBNR$, U3%)
*          IF U3% <> 0% THEN EDITMODE

            gosub test_if_can_close
            if clsmsg$(3%)= " " and clsmsg$(4%)= " " and clsmsg$(6%)=" " ~
                                and clsmsg$(7%)= " " then L19150
                errormsg$ = "Cannot Close.  See messages below."
*              CALL "JBINUSE" (JBNR$, 1%)  /* Clear job in-use */
                goto editmode
L19150:     actclose$ = date
            gosub purge_detail

        save_job     /* Entry point to just save header changes.       */
            gosub save_data
            goto  inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for the page 1 of input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20110          /* Job number       */
            return

L20110
*        Default/enable for JOB NUMBER
            inpmessage$ = "Enter the Job you wish to Close."
            return


        REM *************************************************************~
            *     P A G E   2   I N P U T   M E S S A G E S             *~
            *-----------------------------------------------------------*~
            * Sets the input messages for page 3 editing.               *~
            *************************************************************

        deffn'052(fieldnr%)
            inpmessage$ = " "
            if fieldnr% = 2% then inpmessage$ =                          ~
                "Enter the Job Description."
            if fieldnr% = 4% then inpmessage$ =                          ~
                "Enter the quantity to be built by this job."
            if fieldnr% = 5% then inpmessage$ =                          ~
                "Enter the WIP account for this job."
            if fieldnr% = 6% then inpmessage$ =                          ~
                "Enter the Planned Start date."
            if fieldnr% = 7% then inpmessage$ =                          ~
                "Enter the Planned End date."
            if fieldnr% = 8% then inpmessage$ =                          ~
                "Enter the Actual Start Date."

            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1984, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
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
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
                call "SERSTOVR" (1%, "6", " ",  #62, #63)
                return clear all
                goto inputmode

        REM *************************************************************~
            *                     D A T A   L O A D                     *~
            *-----------------------------------------------------------*~
            * Load old job from file, if found.                         *~
            *************************************************************

        dataload
            get #3 using L30100, jbdescr$, part$, qtymake, qtycomp,       ~
                                vendor$, po$, poline$,                   ~
                                stdate$, temp$, wipacct$, plstdate$,     ~
                                plenddate$, estcomp
L30100:         FMT POS(9), CH(30), POS(58), CH(25), 2*PD(14,4),         ~
                    POS(108), CH(9), CH(16), CH(3),                      ~
                    POS(147), 2*CH(6), CH(9), 2*CH(6), POS(1139), PD(14,4)
            oldmake = qtymake
            call "PIPINDEX" (#2, plstdate$ , stdate% , 0%)
            call "PIPINDEX" (#2, plenddate$, enddate%, 0%)
            call "CONVERT" (qtymake, -0.2, qtymake$)
            call "CONVERT" (qtycomp, -0.2, qtycomp$)
            call "DATFMTC" (plstdate$ )
            call "DATFMTC" (plenddate$)
            call "DATFMTC" (stdate$   )
            call "DESCRIBE" (#6, wipacct$, wipdescr$, 1%, f1%(6))
            call "GLFMT" (wipacct$)

            call "READ100" (#4, part$, f1%(4))
            if f1%(4) = 1% then get #4 using L30250, partdescr$
L30250:         FMT XX(25), CH(32)
            call "SERLOAD" (1%,          /* Line Item Pointer.         */~
                            "WP",        /* Source Transaction Type    */~
                            jbnr$,       /* Source Transaction Key     */~
                            1%,          /* # Trans to Create File for */~
                            "1",         /* Load from WIP only those   */~
                            jbnr$,       /* S/N's that are in this Job.*/~
                            #2,          /* SYSFILE2 UFB               */~
                            #61,         /* SERTIF   UFB               */~
                            #62,         /* SERMASTR UFB               */~
                            #63, u3%)    /* SERWORK  UFB               */

*        Test for Unkitted components and/or byproducts
            clsmsg$(1), clsmsg$(2) = " "
            readkey$ = "JOB ORDER: " & jbnr$
            init(hex(00)) str(readkey$,20)
L30410:     call "PLOWNEXT" (#17, readkey$, 19%, f1%(17))    /* PIPOUT */
            if f1%(17) = 0% then test_if_can_close
                get #17, using L30440, qty
L30440:              FMT POS(57), PD(14,4)
                call "READ100" (#4, str(readkey$,20,25), f1%(4))
                if f1%(4) = 0% then L30410
                     get #4, using L30480, type$
L30480:                   FMT XX(179), CH(3)
                     convert type$ to type%, data goto L30410
                     if type% > 489% and type% < 500% then L30410 /*SKIP*/
                     if type% > 789% and type% < 800% then L30410 /*TOOL*/
                     if qty < 0 then L30570
                     clsmsg$(1) = "WARNING: All Materials have NOT been"&~
                                  " Issued (Kitted) to this Job."
                     if clsmsg$(2) = " " then L30410 else test_if_can_close

L30570:              clsmsg$(2) = "WARNING: Planned By-Products have" &  ~
                               " NOT been Reported."
                     if clsmsg$(1) = " " then L30410

        test_if_can_close
            clsmsg$(3%), clsmsg$(4%), clsmsg$(6%), clsmsg$(7%) = " "
            init (" ") costmsg$()
            if qtymake = qtycomp then L30660
               clsmsg$(3) = "Cannot Close Job: Quantity Completed does" &~
                            " not equal Quantity to Make."
L30660:     call "READ100" (#3, jbnr$, f1%(3))
            get #3 using L30690, total, totalcr, totalcls
L30690:         FMT POS(232), PD(14,4), POS(528), PD(14,4),              ~
                    POS(1147), PD(14,4)
            if totalcls > 9e9 then totalcls = 0

            coredb, corecr, corecls, corefg = 0
            call "READ100" (#21, jbnr$, f1%(21))
               if f1%(21%) = 0% then L30712
            get #21 using L30701, coredb, corecr, corecls, corefg
L30701:         FMT POS(9), PD(14,4), POS(209), PD(14,4),                ~
                    POS(313), PD(14,4), POS(417), PD(14,4)
            goto L30800

L30712:     str(costmsg$(1),17) = "Total Actual :"
            call "CONVERT" (total, 2.4, str(costmsg$(1),42,10))
            str(costmsg$(2),17) = "Total Credits: "
            call "CONVERT" (totalcr, 2.4, str(costmsg$(2),42,10))

            temp = total - (totalcr + totalcls)
            str(costmsg$(3),17) = "  Job Balance:"
            call "CONVERT" (temp, 2.4, str(costmsg$(3),42,10))

            if abs(temp) >= .01 then L30856
            goto L30860

L30800:     str(costmsg$(1%), 1%) = "Total Actual :"
            str(costmsg$(2%), 1%) = "Total Credits:"
            call "CONVERT" (total, 2.4, str(costmsg$(1%),15%,10%))
            call "CONVERT" (totalcr, 2.4, str(costmsg$(2%),15%,10%))
            str(costmsg$(1%),28%) = "Core Actual :"
            str(costmsg$(2%),28%) = "Core Credits:"
            call "CONVERT" (coredb, 2.4, str(costmsg$(1%),42%,10%))
            call "CONVERT" (corecr, 2.4, str(costmsg$(2%),42%,10%))
            temp = (total + coredb) - (totalcr + corecr)                 ~
                                    - (totalcls + corecls)
            str(costmsg$(3%),28%) = " Job Balance:"
            call "CONVERT" (temp, 2.4, str(costmsg$(3%),42%,10%))

            if abs(temp) >= .01 then L30856
            goto L30860

L30856:     clsmsg$(4%) = "Cannot Close Job: Completed Costs do not" &   ~
                         " equal Actual Costs."

L30860
*       ** Check JBTCBUF2 for bad or unposted time card labor...

            init(hex(00)) readkey$
            call "READ104" (#51, readkey$, f1%(51%))
                if f1%(51%) = 0% then L30888
                    goto L30876
L30872:     call "READNEXT" (#51, f1%(51%))
                if f1%(51%) = 0% then L30888
L30876:     get #51 using L30878, buf2job$
L30878:         FMT POS(26), CH(8)
            if buf2job$ <> jbnr$ then L30872
            if postc$ <> "Y" then L30872
                clsmsg$(6%) = "Cannot Close Job: Bad or Unposted Time " &~
                              "Card Labor Exists."

L30888
*       ** If Purchased Job, PO qty must equal zero...

            if str(jbnr$,1%,2%) <> "PJ" then L30920
                readkey$ = str(vendor$) & str(po$) & str(poline$)
                call "READ100" (#52, readkey$, f1%(52%))
                    if f1%(52%) = 0% then L30920
                get #52 using L30902, poqty
L30902:             FMT POS(109), PD(14,4)
                if poqty = 0 then L30920
                    clsmsg$(7%) = "Cannot Close Job: Purchased Job has "&~
                                  "an open PO quantity."

L30920
*       ** Test for Bad Stuff

            clsmsg$(5%) = " "
            readkey$ = str(jbnr$,,8%) & "X" & hex(0000)
            call "PLOWALTS" (#50, readkey$, 1%, 9%, f1%(50%))
               if f1%(50%) = 0% then return
            clsmsg$(5%) = "*** Warning - Job has Rejected Transactions" &~
                          " on File. ***"
            return

        REM *************************************************************~
            *                        D A T A   P U T                    *~
            *-----------------------------------------------------------*~
            *          Update job record and cleanup files              *~
            *************************************************************
        save_data
            call "SHOSTAT" ("Saving Job Information...")
            call "DATUFMTC" (stdate$)
            call "DATUFMTC" (plstdate$)
            call "DATUFMTC" (plenddate$)
            call "GLUNFMT"  (wipacct$)

            totalstd = 0 : mat stdbom = zer
                           mat stdrte = zer
                           mat stdmsc = zer
                           init (hex(00)) stdbom$, stdrte$, stdmsc$

            temp$, temp1$ = " "
            if actclose$ = " " or actclose$ = blankdate$ then L31180
               call "STCCOSTS" (part$, " ", #2, 3%, totalstd, stdfold(), ~
                                stdbom(), stdrte(), stdmsc())
               temp$ = set$ : temp1$ = set_id$ : estcomp = 100
               call "PACKZERO" (stdbom(), stdbom$)
               call "PACKZERO" (stdrte(), stdrte$)
               call "PACKZERO" (stdmsc(), stdmsc$)

L31180:     call "READ101" (#3, jbnr$, f1%(3))
            put #3 using L31220, jbdescr$, qtymake, stdate$, actclose$,   ~
                                wipacct$, plstdate$, plenddate$,         ~
                                totalstd, stdbom$, stdrte$, stdmsc$,     ~
                                estcomp, temp$, temp1$
L31220:         FMT POS(9), CH(30), POS(83), PD(14,4), POS(147), 2*CH(6),~
                    CH(9), 2*CH(6), POS(824), PD(14,4), 3*CH(96),        ~
                    POS(1139), PD(14,4), POS(1251), CH(8), CH(4)

            if f1%(3) = 0% then write #3 else rewrite #3

            gosub save_serial_numbers
            if actclose$ = " " or actclose$ = blankdate$ then L31310
                gosub chg_component_serial_numbers
                call "CDANPOST" (#3, "D")

L31310
*        Align PIPINs
            readkey$ = "JOB ORDER: " & jbnr$ : qty = 0
            call "READ101" (#8, readkey$, f1%(8))  /* PIPIN */
            if f1%(8) = 0% then L31381
                get #8, using L31360, temp%, qty
L31360:              FMT XX(25), BI(4), XX(19), PD(14,4)
                delete #8
                call "PIPFLAGS" (part$, 1%, temp%, -qty, #9, #10)
L31381:     newqty = 0
            if str(jbnr$,,2%) <> "RW" then L31384
            if rwkpip$ <> "A" then L31390
L31384:        newqty = qtymake - qtycomp
L31390:     if abs(newqty) < .0001 then L31460
                call "PIPFLAGS" (part$, 1%, enddate%, newqty, #9, #10)
                write #8 using L31440, part$, enddate%, readkey$,         ~
                                      newqty, stdate%
L31440:         FMT CH(25), BI(4), CH(19), PD(14,4), BI(4)

L31460
*        Clean up quantity on order
                qty = newqty - qty
                if abs(qty) < .0001 then return
                call "HNYPST1" (part$, "001", " ", 0, 0, qty, 0, 0,      ~
                                #5, #4, #2, f2%(5), f2%(4), f2%(2),      ~
                                0%, err%)
                return


        save_serial_numbers
            readkeys$ = jbnr$
            call "SERSAVE" (1%,          /* Line Item Pointer.         */~
                            "WP",        /* Source Transaction Type    */~
                            readkeys$,   /* Source Transaction Key     */~
                            1%,          /* # Trans to Create File for */~
                            part$,       /* Part Code                  */~
                            userid$,     /* Current User ID            */~
                            "1",         /* Change Status to ...       */~
                            "6",         /* Change Status from ...     */~
                            1%,          /* Clear TIF after Save (YES) */~
                            #2,          /* SYSFILE2 UFB               */~
                            #61,         /* SERTIF UFB                 */~
                            #62,         /* SERMASTR UFB               */~
                            #63)         /* SERWORK  UFB               */
            return

        chg_component_serial_numbers
            REM *** change the status of any component serial numbers ***~
                *** still tied to this Job from '3' to '5'.  Location ***~
                *** changed from Job plus Parent S/N to Parent Part   ***~
                *** plus Parent S/N.                                  ***

            readkeys$ = "3" & jbnr$
L31780:     call "PLOWAL1" (#62, readkeys$, 2%, 9%, f1%(62))
            if f1%(62) = 0% then return
                location$ = part$ & " " & str(readkeys$,10%,20%)
                put #62 using L31820, "5", location$
L31820:              FMT CH(1), CH(30)
                rewrite #62
                goto L31780


        REM *************************************************************~
            *                P U R G E   D A T A                        *~
            *-----------------------------------------------------------*~
            * Clear detail files...                                     *~
            *************************************************************
        purge_detail

L33070
*        Delete old WC Allocations
            init (hex(ff)) wc$()
            mat wc% = zer
            c% = 0
            call "SHOSTAT" ("Purging Capacity Usage Details.")
            wckey$ = "JOB ORDER: " & str(jbnr$) & hex(00000000)
L33130:     call "PLOWNXT1" (#16, wckey$, 19%, f1%(16))  /* WCOUT */
            if f1%(16) = 0% then L33280
                get #16, using L33160, wc$, su%, run%
L33160:              FMT CH(6), XX(25), 2*BI(4)
                delete #16
                if val(str(wc$,5,2),2) < today% then L33130

            /* Build hit list for WCMASTR         */
                search wc$() = wc$ to cursor%() step 6
                if cursor%(1) = 0% then c% = c% + 1%                     ~
                                   else c% = (cursor%(1)+5%) / 6%
                wc$(c%) = wc$
                wc%(c%) = wc%(c%) + su% + run%
                if c%  <> 2000% then L33130

L33280:     /* Adjust for deleted WCOUT records     */
                if c% = 0% then L33490
                for i% = 1% to c%
                  if str(wc$(i%),,1) = hex(ff) then L33460
                    wcctl$ = str(wc$(i%),,4)
                    call "READ101" (#15, str(wc$(i%),,4), f1%(15))
                    if f1%(15) = 0% then L33460
                       get #15, using L33360, used%()
L33360:                    FMT POS(1040), 490*BI(2)
                       for d% = i% to c%
                         if str(wc$(d%),,4) <> wcctl$ then L33430
                             day% = val(str(wc$(d%),5,2),2)
                             if day% < 1% or day% > 490% then L33420
                             used%(day%) = max(0%, used%(day%) - wc%(d%))
L33420:                      wc$(d%) = all(hex(ff))
L33430:                next d%
                       put #15 using L33360, used%()
                       rewrite #15
L33460:         next i%
                if c% = 2000% then L33070

L33490
*        Delete any lingering PIP Outs
            call "SHOSTAT" ("Cleaning Up Planned Inventory Position.")
            readkey$ = "JOB ORDER: " & jbnr$
L33520:     call "PLOWNXT1" (#17, readkey$, 19%, f1%(17)) /* PIPIN */
            if f1%(17) = 0% then L33590
                get #17 using L33550, parta$, temp%, qty
L33550:              FMT XX(19), CH(25), BI(4), XX(8), PD(14,4)
                delete #17
                call "PIPFLAGS" (parta$, 1%, temp%, qty, #9, #10)
                goto L33520
L33590:     call "DELETE" (#36,readkey$,19%)  /* JBPIPXF */

*        Update Quantities Commited
            readkey$ = str(jbnr$,1,8) & all(hex(00))
L33630:     call "PLOWNXT1" (#18, readkey$, 8%, f1%(18))  /* JBMATER2 */
            if f1%(18) = 0% then return
                get #18 using L33660, parta$, store$, lot$, in, out,      ~
                                     ostore$, olot$
L33660:              FMT XX(22), CH(25), CH(3), CH(6), XX(14), PD(14,4), ~
                         POS(330), PD(14,4), POS(342), CH(3), CH(6)
                left = round(in - out, 2)
                if left <= 0 then L33630
                     put #18 using L33710, in
L33710:                   FMT POS(330), PD(14,4)
                     rewrite #18
                     if parta$ = part$ then L33630    /* Part to build   */
                     if str(store$,1,1) <> "*" then L33740 /* From a Job */
                        store$ = ostore$ : lot$ = olot$
                        if store$ = " " then L33630
L33740:                  call "HNYPST1" (parta$, store$, lot$,           ~
                                         0, 0, 0, -left, 0, #5, #4, #2,  ~
                                         f2%(5), f2%(4), f2%(2), 0%, u3%)
                          goto L33630


        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *-----------------------------------------------------------*~
            * Inputs document for first time.                           *~
            *************************************************************

        deffn'101(fieldnr%)
            init(hex(84)) lfac$()
            on fieldnr%  gosub      L40140          /* JOB NUMBER       */
            goto L40210

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40140:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40210:     accept                                                       ~
               at (01,02), "Close Completed Jobs",                       ~
               at (01,62), "Post Date:",                                 ~
               at (01,73), fac(hex(8c)), postdate$              , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Job Number",                                 ~
               at (06,20), fac(lfac$( 1)), jbnr$                , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                     keys(hex(00010d0f10)), key(keyhit%)

            if keyhit% <> 13 then L40410
                call "MANUAL" ("JBCLOSE2")
                goto L40210

L40410:     if keyhit% <> 15 then return
                call "PRNTSCRN"
                goto L40210

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *-----------------------------------------------------------*~
            * Screen for editing page 1 of document.                    *~
            *************************************************************

        deffn'111(fieldnr%)
            if fieldnr% > 0% then L41130
                init(hex(86)) lfac$()
                init(hex(84)) dfac$
                inpmessage$ = edtmessage$
                goto L41330

L41130:     init(hex(8c)) lfac$(), dfac$
            on fieldnr% gosub       L41260,         /* Job Number       */~
                                    L41260,         /* Description      */~
                                    L41290,         /* Quantity to Make */~
                                    L41260,         /* WIP Account      */~
                                    L41260,         /* Planned Start    */~
                                    L41260,         /* Planned Finish   */~
                                    L41260          /* Actual Start     */
            goto L41330

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41260:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L41290:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41330:     accept                                                       ~
               at (01,02), "Close Completed Jobs",                       ~
               at (01,62), "Post Date:",                                 ~
               at (01,73), fac(hex(8c)), postdate$              , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Job Number",                                 ~
               at (05,20), fac(dfac$)    , jbnr$                , ch(08),~
               at (06,02), "Description",                                ~
               at (06,20), fac(lfac$( 2)), jbdescr$             , ch(30),~
               at (07,02), "Part Number",                                ~
               at (07,20), fac(dfac$)   ,  part$                , ch(25),~
               at (07,46), fac(hex(8c)),   partdescr$           , ch(34),~
               at (08,02), "Qty to Make",                                ~
               at (08,20), fac(lfac$( 3)), qtymake$             , ch(10),~
               at (08,46), "Quantity Completed = ",                      ~
               at (08,67), fac(dfac$)    , qtycomp$             , ch(10),~
               at (09,02), "WIP Account",                                ~
               at (09,20), fac(lfac$( 4)), wipacct$             , ch(12),~
               at (09,46), fac(hex(8c))  , wipdescr$            , ch(32),~
               at (10,02), "Planned Start",                              ~
               at (10,20), fac(lfac$( 5)), plstdate$            , ch(10),~
               at (11,02), "Planned Finish",                             ~
               at (11,20), fac(lfac$( 6)), plenddate$           , ch(10),~
               at (11,31), fac(hex(8c))  , costmsg$(1)          , ch(50),~
               at (12,02), "Actual Start",                               ~
               at (12,20), fac(lfac$( 7)), stdate$              , ch(10),~
               at (12,31), fac(hex(8c))  , costmsg$(2)          , ch(50),~
               at (13,31), fac(hex(8c))  , costmsg$(3)          , ch(50),~
                                                                         ~
               at (14,02), fac(dfac$)    , clsmsg$(1)           , ch(79),~
               at (15,02), fac(dfac$)    , clsmsg$(2)           , ch(79),~
               at (16,02), fac(dfac$)    , clsmsg$(3)           , ch(79),~
               at (17,02), fac(dfac$)    , clsmsg$(4)           , ch(79),~
               at (18,02), fac(dfac$)    , clsmsg$(6)           , ch(79),~
               at (19,02), fac(dfac$)    , clsmsg$(7)           , ch(79),~
               at (20,02), fac(dfac$)    , clsmsg$(5)           , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               at (22,02), "(1)Start Over",                              ~
               at (22,16), "(7)Report Full/Partial Completion",          ~
               at (22,51), "(10)CR Ledger",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,16), "(8)Return Components to Inventory",          ~
               at (23,51), "(12)See Costs",                              ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,02), "(3)See POs",                                 ~
               at (24,16), "(9)Manually Adjust Job Cost Data ",          ~
               at (24,51), "(32)Save Job ",                              ~
               at (24,65), "(16)CLOSE JOB",                              ~
                     keys(hex(0001030708090a0c0d0f1020)), key(keyhit%)

            if keyhit% <> 13 then L41860
                call "MANUAL" ("JBCLOSE2")
                goto L41330

L41860:     if keyhit% <> 15 then L41900
                call "PRNTSCRN"
                goto L41330

L41900:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Tests data for the items on page 1.                       *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr%  gosub      L50170,         /* Job Number       */~
                                    L50360,         /* Description      */~
                                    L50390,         /* Quantity to Make */~
                                    L50520,         /* WIP Account      */~
                                    L50630,         /* Planned Start    */~
                                    L50730,         /* Planned Finish   */~
                                    L50820          /* Actual Start     */
            return

L50170
*        Test data for JOB NUMBER
            call "GETCODE" (#3, jbnr$, jbdescr$, 0%, 0, f1%(3))
            if f1%(3) = 1% then L50220
                errormsg$ = "Please enter an existing Job Number"
                return
L50220:     get #3 using L50230, temp$    /* Close Date */
L50230:         FMT POS(153), CH(6)
            if temp$ = " " or temp$ = blankdate$ then L50280
                call "DATEFMT" (temp$)
                errormsg$ = "Job already closed on " & temp$
                return
L50280:     u3% = 2%  /* Check in use and hold job if free */
            call "JBINUSE" (jbnr$, u3%)
            if u3% = 0% then L50310
                errormsg$ = hex(00) : return
L50310:     gosub dataload
            if errormsg$ <> " " then call "JBINUSE" (jbnr$, 1%) /*Clear*/
            if errormsg$ <> " " then return
                return clear all
                goto editmode

L50360
*        Test data for DESCRIPTION
            return

L50390
*        Test data for QUANTITY TO MAKE
            call "NUMTEST" (qtymake$, 0, 9e7, errormsg$, 0.2, qtymake)
            if errormsg$ <> " " then return
                if qtycomp <= qtymake then L50450
                     errormsg$ = "Cannot be less than completed quantity"
                     return
L50450:         call "SERENABL" (part$, enabled%, u3%, #2, #4)
                if enabled% = 1% then gosub enter_serial_numbers
                clsmsg$(3) = "Cannot Close Job: Quantity Completed does"&~
                             " not equal Quantity to Make."
                if qtymake = qtycomp then clsmsg$(3) = " "
                return

L50520
*        Test data for WIP ACCT
            call "GETCODE" (#6,wipacct$, wipdescr$, 1%, 0, f1%(6))
            if f1%(6) <> 0% then L50570
                errormsg$ = "WIP Account Not On File"
                return
L50570:     get #6, using L50580, type$
L50580:         FMT XX(39), CH(1)
            if type$ = "A" then return
                errormsg$ = "Not An Asset Account: " & wipacct$
                return

L50630
*        Test data for PLANNED START
            call "DATEOKC" (plstdate$, err%, errormsg$)
            if errormsg$ <> " " then return
                temp$ = plstdate$
                call "DATUFMTC" (temp$)
                call "PIPINDEX" (#2, temp$, stdate%, 0%)
                if plenddate$ = " " or plenddate$ = blankdate$ then return
                if stdate% <= enddate% then return
                   goto L50900

L50730
*        Test data for PLANNED FINISH
            call "DATEOKC" (plenddate$, err%, errormsg$)
            if errormsg$ <> " " then return
                temp$ = plenddate$
                call "DATUFMTC" (temp$)
                call "PIPINDEX" (#2, temp$, enddate%, 0%)
                if stdate% <= enddate% then return else L50900


L50820
*        Test data for ACTUAL START
            call "DATEOKC" (stdate$, err%, errormsg$)
            if errormsg$ <> " " then return
                temp$ = stdate$
                call "DATUFMTC" (temp$)
                call "PIPINDEX" (#2, temp$, temp%, 0%)

                if temp% <= enddate% then return
L50900:              errormsg$ = "Cannot End Before Starting Date"
                     return


        enter_serial_numbers
            readkey$ = jbnr$
            call "SERINSUB" (part$," "," ",qtymake-qtycomp, 1%, 1%, "JD",~
                             readkey$, errormsg$, #2, #4, #62, #63)
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1984, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
