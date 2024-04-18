        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   RRRR   L      M   M   AAA   N   N  U   U  L       *~
            *  P   P  R   R  L      MM MM  A   A  NN  N  U   U  L       *~
            *  PPPP   RRRR   L      M M M  AAAAA  N N N  U   U  L       *~
            *  P      R   R  L      M   M  A   A  N  NN  U   U  L       *~
            *  P      R   R  LLLLL  M   M  A   A  N   N   UUU   LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLMANUL - ALLOWS USER TO ENTER CHECKS DIRECTLY INTO THE  *~
            *            CHECK REGISTER, WITH THE OPTION TO POST        *~
            *            THE EMPLOYEES ACCRUALS, AND THE OPTION TO POST *~
            *            THE GENERAL LEDGER.                            *~
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
            * 12/23/83 ! ORIGINAL                                 ! HES *~
            * 07/16/84 ! ADDED A PRINT ROUTINE                    ! JUDD*~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION   ! RAC *~
            * 10/13/86 ! Support Of Direct Deposit Slips          ! HES *~
            * 03/20/89 ! Expanded Earnings & Deductions to 100 Max! RJM *~
            *          !   This is the Max in other PRL PGMS.     !     *~
            * 12/26/90 ! Changed Call to JNLINFO for GL Batches,  ! RAC *~
            *          !    added Call to JNLCLOSE for GL Batches !     *~
            * 06/21/91 ! Added G/L Export file code.  Moved and   ! JBK *~
            *          !    conditioned CALL to JNLINFO,          !     *~
            *          !    conditioned CALL to JNLCLOSE.         !     *~
            *          ! Replaced SHOWMSG with SHOSTAT.  Added    !     *~
            *          !    CALL to ALLFREE.  CHECKNR$ now CHECK$.!     *~
            *          !    Fixed minor screen overlay.           !     *~
            * 09/16/91 ! Fixed Posting Messages.                  ! JDH *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 03/10/93 ! PRR 11283 - PF(8)Edit on earnings and    ! MLJ *~
            *          !   deductions now immediately available.  !     *~
            *          !   (8)Edit displayed in line 1 PFkeys     !     *~
            *          !   instead of as a separate msg.          !     *~
            *          ! PRR 11855 - Added ASKUSER when post to   !     *~
            *          !  accruals = "Y" and earnings amt is neg. !     *~
            *          ! MISC - Standardized screens, msgs, & rpt !     *~
            *          !   heading, corrected implied integers,   !     *~
            *          !   renumbered code.                       !     *~
            * 03/22/95 ! PRR 13365 - Removed the left justify of  ! JBK *~
            *          !   the check number after testing.  Force !     *~
            *          !   check number to be right justified and !     *~
            *          !   space filled.                          !     *~
            * 08/27/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            addamount$(100)10,           /* AMOUNT TO ADD TO CHECK     */~
            addunits$(100)10,            /* UNITS TO ADD TO CHECK      */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cashacct$16,                 /* CASH IN BANK ACCOUNT       */~
            cashdescr$32,                /* CASH IN BANK ACCOUNT DESCR */~
            check$8,                     /* CHECK NUMBER               */~
            checkmsg$(3)36,              /* MESSAGE ABOUT CHECK STATUS */~
            checkdate$8,                 /* CHECK DATE                 */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dcat$(100)6,                 /* DEDUCTION CATEGORY         */~
            dcredit$(100)16,             /* DEDUCTION CREDIT ACCOUNT   */~
            ddebit$(100)16,              /* DEDUCTION DEDIT ACCOUNT    */~
            dedamount$(100)10,           /* DEDUCTION AMOUNTS          */~
            dedc$(100)12,                /* DEDUCTION DESCRIPTIONS     */~
            department$4,                /* Employee's Department Code */~
            dispdate$8,                  /* Payroll Posting Disply Date*/~
            dmthd$(100)6,                /* DEDUCTION METHOD           */~
            ecat$(100)4,                 /* EARNINGS CATEGORY          */~
            edate$8,                     /*          AND ENDING        */~
            edebit$(100)16,              /* EARNINGS DEDIT ACCOUNT     */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            empcode$12,                  /* EMPLOYEE CODE              */~
            emp$(100)1,                  /* EMPLOYEE PAYES FLAG        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            fac$(20,4)1,                 /* FIELD ATTRIBUTE CHARACTERS */~
            gl_batch_open$1,             /* GL Batch Created and Open  */~
            grossacct$16,                /* ACCRUAL ACCOUNT            */~
            grossdescr$32,               /* ACCRUAL ACCOUNT DESCRIPTION*/~
            heading$79,                  /* HEADER FOR LINE ITEM SCREEN*/~
            heading1$79,                 /* HEADER FOR LINE ITEM SCREEN*/~
            heading2$79,                 /* HEADER FOR LINE ITEM SCREEN*/~
            i$(24)80,                    /* SCREEN IMAGE               */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            jnlid$3,                     /* JOURNAL ID                 */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line$(20)79,                 /* SCREEN TO DISPLAY TO       */~
            line2$79,                    /* SCREEN LINE 2              */~
            moduleno$20,                 /* MODULE NUMBER              */~
            name$30,                     /* EMPLOYEES NAME             */~
            name1$25,                    /* EMPLOYEES NAME FOR GLDETAIL*/~
            pf$(3)79,                    /* PF KEY SCREEN LITERALS     */~
            pfkeys$(2)32,                /* FUNCTION KEYS FOR KICKS    */~
            postaccruals$1,              /* POST TO EMP ACCRUALS?      */~
            postledger$1,                /* POST TO GENERAL LEDGER ?   */~
            postmsg$79,                  /* Post Message for Scr & Rpt */~
            prldate$6,                   /* GENERAL LEDGER POST DATE   */~
            rate(100),                   /* NORMAL PAY RATE THIS TYPE  */~
            readkey$50,                  /* FOR PLOWS                  */~
            record$(3)150,               /* FOR READS/REWRITES         */~
            runtime$8,                   /* SYSTEM RUN TIME            */~
            sdate$8,                     /* FOR PERIOD STARTING        */~
            subject$(100)10,             /* GROSS SUBJECT TO THE DEDUC */~
            summary$1,                   /* SUMMARY POST INDICATOR     */~
            text$100,                    /* G/L POSTING TEXT           */~
            title$(2)79,                 /* P.F. KEY TITLES FOR FUN    */~
            tran$(24)80,                 /* TRAN FROM CURSOR==>FIELD # */~
            tran1$(24)80,                /* TRAN FROM CURSOR==>FIELD # */~
            type$(100)12,                /* EARNINGS TYPE STUFF        */~
            units$(100)6,                /* UNITS (GOD KNOWS WHAT FOR) */~
            unitsub$(100)10,             /* UNITS SUBJECT TO A DEDUCTIO*/~
            work(3,2),                   /* FOR ACCRUAL UPDATE         */~
            work1(3)                     /* FOR ACCRUAL UPDATE         */~

        dim                              /* G/L Export Posting info    */~
            export_on$1,                 /* G/L Export File processing?*/~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            gl_catg$4,                   /* G/L P/R Earnings Catagory  */~
            gl_deductions$12,            /* G/L P/R Deduction Descr.   */~
            gl_depart$4,                 /* G/L P/R Employee Depart #  */~
            gl_earnings$12,              /* G/L P/R Earnings Descr.    */~
            gl_employee$12,              /* G/L P/R Employee Number    */~
            tran_type$5                  /* G/L transaction type       */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * #01 ! EMPMASTR ! Employee master file                     *~
            * #02 ! PERMASTR ! Personnel master file-ties to EMPMASTR i *~
            * #03 ! EMPEARN1 ! Employee earnings file                   *~
            * #04 ! EMPDEDXN ! Employee deduction file                  *~
            * #05 ! GLMAIN   ! General Ledger Main File                 *~
            * #06 ! PRLCHK   ! System earnings register info            *~
            * #07 ! GLDETAIL ! General ledger detail file               *~
            * #08 ! USERINFO !                                          *~
            * #09 ! SYSFILE2 ! SYSTEMS CLOSET                           *~
            * #10 ! PRLCHK2  ! System earnings register Detail Info     *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "EMPMASTR",                                      ~
                        varc, indexed, recsize =  136,                   ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  1, keypos =   70, keylen =   1, dup

            select #02, "PERMASTR",                                      ~
                        varc, indexed, recsize =  950,                   ~
                        keypos =   39, keylen =  12,                     ~
                        alt key  1, keypos =   28, keylen =  23,         ~
                            key  2, keypos =    2, keylen =  49,         ~
                            key  3, keypos =    1, keylen =  50

            select #03, "EMPEARN1",                                      ~
                        varc, indexed, recsize =  200,                   ~
                        keypos =    1, keylen =  15,                     ~
                        alt key  1, keypos =   16, keylen =  28

            select #04, "EMPDEDXN",                                      ~
                        varc, indexed, recsize =  300,                   ~
                        keypos =    1, keylen =  15,                     ~
                        alt key  1, keypos =   16, keylen =  18, dup

            select #05, "GLMAIN",                                        ~
                        varc, indexed, recsize =  300,                   ~
                        keypos =    1, keylen =   9

            select #06, "PRLCHK",                                        ~
                        varc, indexed, recsize =  120,                   ~
                        keypos =    1, keylen =  23,                     ~
                        alt key  1, keypos =   13, keylen =  11,         ~
                            key  2, keypos =   42, keylen =   9, dup

            select #07, "GLDETAIL",                                      ~
                        varc, indexed, recsize =  160,                   ~
                        keypos =    1, keylen =  26

            select #08, "USERINFO",                                      ~
                        varc, indexed, recsize =  150,                   ~
                        keypos = 1, keylen = 3

            select #09, "SYSFILE2",                                      ~
                        varc, indexed, recsize =  500,                   ~
                        keypos = 1, keylen = 20

            select #10, "PRLCHK2",                                       ~
                        varc, indexed, recsize =  120,                   ~
                        keypos = 1, keylen = 25

        REM Check to See if Payroll/Personnel is Active...
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#01, "SHARE", f2%(01%),rslt$(01%),axd$(01%))
            call "OPENFILE" (#02, "SHARE", f2%(02%),rslt$(02%),axd$(02%))
            call "OPENFILE" (#03, "SHARE", f2%(03%),rslt$(03%),axd$(03%))
            call "OPENFILE" (#04, "SHARE", f2%(04%),rslt$(04%),axd$(04%))
            call "OPENFILE" (#05, "SHARE", f2%(05%),rslt$(05%),axd$(05%))
            call "OPENFILE" (#06, "SHARE", f2%(06%),rslt$(06%),axd$(06%))
            call "OPENFILE" (#07, "SHARE", f2%(07%),rslt$(07%),axd$(07%))
            call "OPENFILE" (#08, "SHARE", f2%(08%),rslt$(08%),axd$(08%))
            call "OPENFILE" (#09, "SHARE", f2%(09%),rslt$(09%),axd$(09%))
            call "OPENFILE" (#10, "SHARE", f2%(10%),rslt$(10%),axd$(10%))

            if f2%(6%) = 0% then L03070
            call "FILEOPEN" (#06, "OUTPT", f2%(06%),rslt$(06%),axd$(06%))
            close #6
            call "OPENFILE" (#06, "SHARE", f2%(06%),rslt$(06%),axd$(06%))

L03070:     if f2%(10%) = 0% then L09000
            call "FILEOPEN" (#10, "OUTPT", f2%(10%),rslt$(10%),axd$(10%))
            close #10
            call "OPENFILE" (#10, "SHARE", f2%(10%),rslt$(10%),axd$(10%))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            runtime$ = " "
            call "TIME" (runtime$)

            date$ = date
            call "DATEFMT" (date$)

            str(line2$,62) = "PRLMANUL: " & str(cms2v$,1%,8%)

            edtmessage$ = "To Modify Displayed Values, Position Cursor "&~
                          "To Desired Value And Press (ENTER)."

            title$(1%)  = "(1)Start Ovr  (2)Col One  (4)Line Above  (8)"&~
                          "Edit  (15)Prnt Scrn"
            title$(2%)  = "(1)Start Over (2)First (3)Last (4)Prev (5)Ne"&~
                          "xt  (6)Down (7)Up"


            heading$ = "EARNINGS TYPE    !UNITS !     UNITS!    AMOUNT!"&~
                       " EXPENSE ACCOUNT!TOTAL GROSS PAY"

            heading1$ ="DEDUCTION EMPLYEE PAID?!UNITS SUBJ! $ SUBJECT! "&~
                       "DEDU AMNT!LIBLTY ACCT!   NET PAY"
            heading2$ ="EARNINGS     !       UNITS       !     AMOUNT !"&~
                       " DEDUCTIONS   !     AMOUNT "

        REM Set Tran String For Edit Mode...
            init(hex(00)) tran$()
            init(hex(01)) str(tran$(5%),26%,11%)
            init(hex(02)) str(tran$(5%),37%,11%)
            init(hex(03)) str(tran$(5%),56%,9%)

            init(hex(00)) tran1$()
            init(hex(01)) str(tran1$(5%),26%)
            init(hex(02)) str(tran1$(5%),37%)
            init(hex(03)) str(tran1$(5%),48%)
            init(hex(04)) str(tran1$(5%),61%)

            copy str(tran$(),321%,1520%) to str(tran$(),401%,1520%)
            copy str(tran1$(),321%,1520%) to str(tran1$(),401%,1520%)

        REM Get Payroll Date For General Ledger Processing...
            call "EXTRACT" addr("ID", userid$)
            call "READ100" (#8, userid$, f1%(8%))
                 if f1%(8%) = 0% then L65000
            get #8, using L09500, prldate$
L09500:             FMT XX(3), XX(12), CH(6)
            call "WHICHPER" (#9, prldate$, whichper%)
                 if whichper% <> 0% then L09560
            dispdate$ = prldate$
            call "DATEFMT" (dispdate$)

L09526:     u3% = 0%
            call "ASKUSER" (u3%, "*** INVALID POSTING DATE ***",         ~
                 "Your Payroll Posting Date is invalid",  dispdate$,     ~
                 "Press RETURN To Acknowledge And Exit Program.")
            if u3% <> 0% then L09526
                goto L65000

L09560:     prldatef$ = prldate$
            call "DATEFMT" (prldatef$)
            jnlid$ = "EMC"
            returncode% = 0%
            moduleno$ = "09"

        REM See If G/L Export Is On...
            export_on$ = "N"
            call "READ100" (#9, "SWITCHS.GL", f1%(9%))
            if f1%(9%) = 1% then get #9 using L09740, export_on$
L09740:         FMT POS(22), CH(1)

        REM *************************************************************~
            *      I N P U T   M O D E   H E A D E R   S C R E E N      *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, empcode$, check$, name$,   ~
                      checkdate$, sdate$, edate$, cashacct$, postledger$,~
                      postaccruals$, checkmsg$(), cashdescr$, grossacct$,~
                      grossdescr$, postmsg$

            call "ALLFREE"

L10120:     for fieldnr% = 1% to  8%
                gosub'051(fieldnr%)
                      if enabled% = 0% then L10210
L10150:         gosub'101(fieldnr%,1%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10170
L10162:                   fieldnr% = fieldnr% - 1%
                          if fieldnr% < 1% then L10120
                          gosub'051(fieldnr%)
                          if enabled% <> 0% then L10150
                          goto L10162
L10170:               if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then L10150
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10150
L10210:         next fieldnr%

        REM *************************************************************~
            *       E D I T   M O D E   H E A D E R   S C R E E N       *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

L11060:     gosub'101(0%,2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       L11210
                  if keyhit% <>  0% then       L11060
            fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 3% or fieldnr% > 8% then L11060

            if fieldnr% > 6% and checkonfile% = 1% then L11060
L11140:     gosub'101(fieldnr%,2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11140
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11140
            goto L11060

L11210:     gosub L30000

        REM *************************************************************~
            *              I N P U T   E A R N I N G S                  *~
            *                                                           *~
            * INPUTS LINE ITEM INFORMATION FOR THE GUYS EARNINGS.       *~
            *************************************************************

        REM Input Earnings Line Items...
            gross, units = 0
            line%, screenline% = 0%
            errormsg$ = " "

L12110:     screenline% = screenline% + 1
                if screenline% < 20% then L12150
            line% = line% + 20%
            screenline% = 1%
L12150:     currentline%, c% = line% + screenline%
            if currentline% > maxlines% then L13000

L12200:     for fieldnr% = 1% to 3%
L12220:         gosub'203(screenline%, fieldnr%)
                      if keyhit%  =  0% then       L12300
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  2% then gosub columnone1
                      if keyhit%  =  4% then gosub lineabove1
                      if keyhit%  =  8% then goto L13000
L12300:         gosub'153(fieldnr%)
                      if errormsg$ <> " " then L12220
                if fieldnr% = 2% and postledger$ = "N" then fieldnr% = 3%
                if fieldnr% = 2% and amount = 0 then fieldnr% = 3%
            next fieldnr%

            convert addamount$(c%) to thisline
            convert addunits$(c%) to uthisline
            gross = round(gross + thisline,2%)
            units = round(units + uthisline,4%)
            goto L12110

L13000: REM *************************************************************~
            *         E D I T   L I N E   I N F O R M A T I O N         *~
            *                                                           *~
            * EDITS LINE INFO.  NOTE THAT THE JOB IS REALLY SIMPLE      *~
            * SINCE THERE CAN BE NO INSERT MODE (TOO MUCH TROUBLE DOWN  *~
            * THE LINE) AND YOU ONLY HAVE ABOUT 2 FIELDS TO EDIT.       *~
            * NOTE ALSO THAT THERE IS NO SUCH THING AS EDIT HEADER INFO *~
            * SINCE THE THING HAS TO HAVE THE SAME EMPLOYEE CODE SINCE  *~
            * EVERYONE WILL PROBABLY HAVE DIFFERENT EARNINGS.           *~
            *************************************************************

            line%, currentline%, screenline% = 0%
L13120:     infomsg$ = "PF(16) To Proceed To Deductions Or Position Cur"&~
                       "sor To Edit"

            gosub'213(0%, 0%)
                  if keyhit%  =  0% then L13290
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then line% = 0%
                  if keyhit%  =  3% then line% = max(0%, maxlines% - 20%)
                  if keyhit%  =  4% then line% = max(0%, line% - 15%)
                  if keyhit%  =  5% then line% = min(line% + 15%, max(0%,~
                                                        maxlines% - 20%))
                  if keyhit%  =  6% then line% = max(0%, line% - 1%)
                  if keyhit%  =  7% then line% = min(line% + 1%, max(0%, ~
                                                        maxlines% - 20%))
                  if keyhit%  = 16% then L13530
                  goto L13120

L13290: REM Which Field...
            fieldnr% = val(str(tran$(cursor%(1%)),cursor%(2%)))
            if fieldnr% = 0% then L13120
                if fieldnr% = 3% and postledger$ = "N" then L13120
            screenline% = max(0%, cursor%(1%) - 4%)
            c%, currentline% = line% + screenline%
            if currentline% > maxlines% then L13120

L13370:         gosub'223(screenline%, fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  0% then L13370
                gosub'153(fieldnr%)
                      if errormsg$ <> " " then L13370
        REM Update "Gross" Field...
            gross, units = 0
            for u3% = 1% to maxlines%
                convert addamount$(u3%) to amount, data goto L13500
                convert addunits$(u3%) to uamount, data goto L13500
                gross = round(gross + amount,2%)
                units = round(units + uamount,4%)
L13500:     next u3%
            goto L13120
L13530:     gosub L31000

        REM *************************************************************~
            *              I N P U T   D E D U C T I O N S              *~
            *                                                           *~
            * INPUTS LINE ITEM INFORMATION FOR THE GUYS DEDUCTIONS.     *~
            *************************************************************

        REM Input Deduction Line Items...
            net = gross
            line%, screenline% = 0%

L14140:     screenline% = screenline% + 1%
            if screenline% < 20% then L14180
                line% = line% + 20%
                screenline% = 1%
L14180:         currentline%, c% = line% + screenline%
                if currentline% > maxlines1% then L15000

            for fieldnr% = 1% to 4%
L14230:         gosub'250(screenline%, fieldnr%)
                      if keyhit%  =  0% then L14290
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  8% then goto L15000
L14290:         gosub'154(fieldnr%)
                      if errormsg$ <> " " then L14230
                if fieldnr% = 3% and postledger$ = "N" then fieldnr% = 4%
                if fieldnr% = 3% and amount = 0 then fieldnr% = 4%
            next fieldnr%

            if emp$(c%) <> "Y" then L14140
                convert dedamount$(c%) to thisline
                net = round(net - thisline, 2%)
                goto L14140

L15000: REM *************************************************************~
            *         E D I T   D E D U C T I O N   L I N E S           *~
            *                                                           *~
            * EDITS EARNINGS LINE INFO.                                 *~
            *************************************************************

            line%, currentline%, screenline% = 0%
L15070:     infomsg$ = "PF(16) To Proceed To Summary Or Position Cursor"&~
                       " To Edit"
            gosub'251(0%, 0%)
                  if keyhit%  =  0% then L15230
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then line% = 0%
                  if keyhit%  =  3% then line% = max(0%, maxlines1% - 20%)
                  if keyhit%  =  4% then line% = max(0%, line% - 15%)
                  if keyhit%  =  5% then line% = min(line% + 15%, max(0%,~
                                                       maxlines1% - 20%))
                  if keyhit%  =  6% then line% = max(0%, line% - 1%)
                  if keyhit%  =  7% then line% = min(line% + 1%, max(0%, ~
                                                       maxlines1% - 20%))
                  if keyhit%  = 16% then       L15470
                  goto L15070

L15230: REM Which Field...
            fieldnr% = val(str(tran1$(cursor%(1%)),cursor%(2%)))
                if fieldnr% = 0% then L15070
            if fieldnr% = 4% and postledger$ = "N" then L15070
                screenline% = max(0%, cursor%(1%) - 4%)
                c%, currentline% = line% + screenline%
                if currentline% > maxlines1% then L15070

L15310:     gosub'252(screenline%, fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L15310
            gosub'154(fieldnr%)
                  if errormsg$ <> " " then L15310

        REM Update "Gross" Field...
            net = gross
            for u3% = 1% to maxlines1%
                if emp$(u3%) <> "Y" then L15440
                    convert dedamount$(u3%) to amount, data goto L15440
                    net = round(net - amount, 2%)
L15440:     next u3%
            goto L15070

L15470:     gosub L32000                   /* FORMAT DATASAVE VARIABLES */
            deductions = round(net - gross, 2%)

L15570:     gosub show_summary
            if keyhit% = 1% then gosub startover
            if keyhit% = 2% then gosub show_lines
            if keyhit% = 16% then datasave
            goto L15570

        REM *************************************************************~
            *  C O L U M N   O N E ,   L I N E   A B O V E   L O G I C  *~
            *                                                           *~
            * FOR EARNINGS INPUT.                                       *~
            *************************************************************

        columnone1
            init(" ") addunits$(c%), addamount$(c%)
            return clear all
            goto L12200

        lineabove1
            if c% = 1% then return
               on fieldnr% gosub L18180,      /* UNITS TO ADD TO GROSS  */~
                                 L18190       /* AMOUNT TO ADD TO GROSS */
               return

L18180:        addunits$ (c%) = addunits$ (c%-1%): return
L18190:        addamount$(c%) = addamount$(c%-1%): return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if postaccruals$ = "Y" then gosub post_accruals
            if postledger$ = "Y" then gosub post_ledger
            gosub save_check
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  inpmessage$ = " "
                  on fieldnr% gosub L20180,         /* EMPLOYEE CODE    */~
                                    L20220,         /* CHECK NUMBER     */~
                                    L20250,         /* CHECK DATE       */~
                                    L20290,         /* START DATE       */~
                                    L20320,         /* ENDING DATE      */~
                                    L20350,         /* CASH ACCOUNT     */~
                                    L20440,         /* POST EMP ACCRUALS*/~
                                    L20510          /* POST G/L         */
                  return

L20180: REM Default/Enable for Employee Code...
            inpmessage$ = "Enter Employee Code Or Leave Blank To Search."
            return

L20220: REM Default/Enable for Check Number...
            inpmessage$ = "Enter A Numeric Check Number."
            return

L20250: REM Default/Enable for Check Date...
            if checkdate$ = " " or ~
               checkdate$ = blankdate$ then checkdate$ = date$
            inpmessage$ = "Enter Check Date."
            return

L20290: REM Default/Enable for Payroll Period Starting Date...
            inpmessage$ = "Enter The Payroll Period Starting Date."
            return

L20320: REM Default/Enable for Payroll Period Ending Date...
            inpmessage$ = "Enter The Payroll Period Ending Date."
            return

L20350: REM Default/Enable for Cash In Bank Account...
            if checkonfile% = 1%                                         ~
                then inpmessage$ = "Memo Only Field Since Check Will NO"&~
                                   "T Be Posted Automatically."          ~
                else inpmessage$ = "Enter The Cash In Bank Account Numb"&~
                                   "er."
            get #1, using L20390, cashacct$, grossacct$
L20390:         FMT XX(36), CH(9), CH(9)
            call "DESCRIBE" (#5, grossacct$, grossdescr$, 1%, f1%(5%))
                if f1%(5%) = 0% then grossdescr$ = " "
            call "GLFMT" (grossacct$)
            call "GLFMT" (cashacct$)
            return

L20440: REM Default/Enable for Post Employee Accruals...
            if postaccruals$ = " " then postaccruals$ = "N"
            if checkonfile% <> 1% then L20480
                enabled% = 0%
                return
L20480:     inpmessage$ = "Enter 'Y' If You Want Units And Amounts To B"&~
                          "e Posted To Employee Accruals."
            return

L20510: REM Default/Enable for Post Amounts To General Ledger...
            if postledger$ = " " then postledger$ = "N"
            if checkonfile% = 1% then L20550
                if cashacct$ = " " then L20550 else L20560
L20550:             enabled% = 0%
                    return
L20560:     inpmessage$ = "Enter 'Y' If You Want Amounts To Be Posted T"&~
                          "o The General Ledger."
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

L30000: REM *************************************************************~
            * R E A D   E M P L O Y E E   E A R N I N G S   R E C O R D *~
            *                                                           *~
            * READS THE EMPLOYEE'S EARNINGS RECORDS INTO MEMORY.        *~
             *************************************************************

        REM Show employee being loaded...
            addunits$(), addamount$(), type$(), edebit$(), units$(),     ~
            ecat$() = " "

            infomsg$ = "Earnings Records For Employee " & name$

        REM Load earnings record...
            maxlines%, c% = 0%
            readkey$ = str(empcode$)

L30160:     call "PLOWNEXT" (#3, readkey$, 12%, f1%(3%))
                 if f1%(3%) = 0% then return
            if maxlines% > 99% then return
                 maxlines%, c% = c% + 1%
                 get   #3, using L30202, type$(c%), ecat$(c%), units$(c%),~
                                 rate(c%), edebit$(c%), usunits, usbucks
L30202:               FMT POS(28), CH(12), CH(4), POS(46), CH(6),        ~
                                   PD(14,4), CH(9), 2*PD(14,4)
                 convert usunits to addunits$(c%), pic(#####.####)
                 convert usbucks to addamount$(c%), pic(#######.##)
                 call "GLFMT" (edebit$(c%))
                 goto L30160

L31000: REM *************************************************************~
            * R E A D   E M P L O Y E E   D E D U C T N S   R E C O R D *~
            *                                                           *~
            * READS THE EMPLOYEE'S DEDUCTIONS RECORDS INTO MEMORY.      *~
             *************************************************************

        REM Show employee being loaded...
            dedamount$(), subject$(), emp$(), ddebit$(), dcredit$(),     ~
            unitsub$(), dedc$(), dcat$(), dmthd$() = " "

            infomsg$ = "Deduction Records For Employee " & name$

        REM Load deduction records...
            maxlines1%, c% = 0%
            readkey$ = str(empcode$)

L31180:     call "PLOWNEXT" (#4, readkey$, 12%, f1%(4%))
                 if f1%(4%) = 0% then return
            if maxlines1% > 99% then return
                 maxlines1%, c% = c% + 1%
                 get   #4, using L31222, dcat$(c%), dmthd$(c%), dedc$(c%),~
                                 emp$(c%), dcredit$(c%), ddebit$(c%)
L31222:              FMT POS(28), 2*CH(6), CH(12), CH(1), 2*CH(9)
                 convert gross to subject$(c%), pic(-######.##)
                 convert units to unitsub$(c%), pic(-####.####)
                 call "GLFMT" (ddebit$(c%))
                 call "GLFMT" (dcredit$(c%))
                 goto L31180

L32000: REM *************************************************************~
            * F O R M A T   V A R I A B L E S   F O R   S U M M A R Y   *~
            *                                                           *~
            * FORMATS THE CHECK INFO FOR SUMMARY DISPLAY SCREEN.        *~
             *************************************************************

            line$() = " "
            t% = 0%  :   gross = 0
            for i% = 1% to maxlines%
                 convert addamount$(i%) to addamount, data goto L32315
                     if addamount = 0 then L32315
                 t% = min(t% + 1%, 15%)
                 addunits = 0
                 convert addunits$(i%) to addunits, data goto L32270
L32270:          gross = round(gross + addamount, 2%)
                 put line$(t%), using L32345, type$(i%), addunits$(i%),   ~
                                units$(i%), addamount$(i%)
L32315:     next i%

L32345: %############ ! ########## ###### ! ########## !


            t%, t1% = 0%
            net = gross
            for i% = 1% to maxlines1%
                convert dedamount$(i%) to dedamount, data goto L32780
                    if dedamount = 0 then L32780
                t% = min(t% + 1%, 20%)
                if emp$(i%) <> "Y" then L32555     /* ONLY EMPLOYEE PAID */
                    t1% = min(t1% + 1%, 15%)      /* DED'S TO CHECK FILE*/
L32555:             str(line$(t%),49%,12%) = dedc$(i%)
                    str(line$(t%),62%,1%)  = "!"
                    if emp$(i%) <> "Y" then L32645
                        str(line$(t%),64%) = dedamount$(i%)
                        net = round(net - dedamount, 2%)
                        goto L32780

L32645:             dd$ = " " & dedamount$(i%) &")"
                    flag3% = 0%
                        for j% = 1% to 10%
                            if flag3% = 1% then L32735
                            if str(dd$,j%+1%,1%) <> " "                  ~
                                                  then str(dd$,j%,1%)="("
                            if str(dd$,j%+1%,1%) <> " "                  ~
                                                  then flag3% = 1%
L32735:                 next j%
                    str(line$(t%), 63%) = str(dd$,,12%)
L32780:     next i%
            return

        REM *************************************************************~
            *       P O S T   E M P L O Y E E   A C C R U A L S         *~
            *                                                           *~
            * THIS ROUTINE WILL POST THE EARNING AND DEDUCTION AMOUNTS  *~
            * TO THE EMPLOYEES ACCRUALS. (MTD, QTD, YTD, *NOT* CURRENT) *~
             *************************************************************

        post_accruals

            call "SHOSTAT" ("Posting Employee Accruals")
            readkey$ = str(empcode$)                        /* EARNINGS */
            t% = 0%

L33140:     call "PLOWNXT1" (#3, readkey$, 12%, f1%(3%))
                if f1%(3%) = 0% then do_deductions
            t% = t% + 1%

            get   # 3, using L33370, record$(1%), work(1%,1%),            ~
                       work(1%,2%), work(2%,1%), work(2%,2%),            ~
                       work(3%,1%), work(3%,2%), record$(2%)

            if str(record$(1%),28%,12%) <> type$(t%) then error_msg
            addunits = 0
            convert addunits$(t%) to addunits, data goto L33250
L33250:     convert addamount$(t%) to addamount, data goto L33140

            for i% = 1% to 3%
                work(i%,1%) = max(0, round(work(i%,1%) + addunits,4%))
                work(i%,2%) = max(0, round(work(i%,2%) + addamount,2%))
            next i%

            rewrite #3, using L33370, record$(1%), work(1%,1%),           ~
                        work(1%,2%), work(2%,1%), work(2%,2%),           ~
                        work(3%,1%), work(3%,2%), str(record$(2%),,36%)
            goto L33140

L33370:     FMT CH(116), 6*PD(14,4), CH(36)

        do_deductions
            readkey$ = str(empcode$)                      /* DEDUCTIONS */
            t% = 0%

L33500:     call "PLOWNXT1" (#4, readkey$, 12%, f1%(4%))
                if f1%(4%) = 0% then return
            t% = t% + 1%

            get   # 4, using L33750, record$(1%), work1(), record$(2%),   ~
                       work(1%,1%), work(1%,2%), work(2%,1%),            ~
                       work(2%,2%), work(3%,1%), work(3%,2%), record$(3%)

            if str(record$(1%),40%,12%) <> dedc$(t%) then error_msg
            unitsub = 0
            convert unitsub$(t%) to unitsub, data goto L33620
            convert subject$(t%) to subject, data goto L33620
L33620:     convert dedamount$(t%) to dedamount, data goto L33500

            for i% = 1% to 3%
                work(i%,1%) = max(0, round(work(i%,1%) + unitsub,4%))
                work(i%,2%) = max(0, round(work(i%,2%) + subject,2%))
                work1(i%)   = max(0, round(work1(i%)   + dedamount,2%))
            next i%

            rewrite #4, using L33750, record$(1%), work1(), record$(2%),  ~
                       work(1%,1%), work(1%,2%), work(2%,1%),work(2%,2%),~
                       work(3%,1%), work(3%,2%), str(record$(3%),,80%)
            goto L33500

L33750:         FMT CH(124), 3*PD(14,4), CH(24), 6*PD(14,4), CH(80)

        error_msg
L33872:     u3% = 0%
            call "ASKUSER" (u3%, "*** ACCRUAL POSTING ERROR ***",        ~
                 "Posting has NOT completed succesfully and the check",  ~
                 "has NOT been saved!",                                  ~
                 "Press RETURN to acknowledge and continue.")
            if u3% <> 0% then L33872
                return clear all
                goto inputmode

        REM *************************************************************~
            *       P O S T   T H E   G E N E R A L   L E D G E R       *~
            *                                                           *~
            * THIS ROUTINE WILL POST THE EARNING AND DEDUCTION AMOUNTS  *~
            * TO THE G/L. METHOD IS SAME AS THE REST OF THE PAYROLL SYST*~
             *************************************************************

        post_ledger

            if gl_batch_open$ = "Y" then L34090
                call "JNLINFO" (moduleno$, jnlid$, pstseq%, summary$,    ~
                                " ", prldate$, #9, f2%(9%), returncode%)
                gl_batch_open$ = "Y"

L34090:     call "SHOSTAT" ("Posting General Ledger")
            call "READ100" (#2, empcode$, f1%(2%))
                get #2, using L34120, name1$
L34120:             FMT XX(1), CH(25)

            if export_on$ <> "Y" then L34140
                gl_check$    = check$
                gl_depart$   = department$
                gl_employee$ = empcode$

L34140: REM Post earnings expense accounts...
            text$ = " "
            str(text$,31%) =  str(empcode$) & check$
            str(text$,69%) = "EMP: " & name1$
            for i% = 1% to maxlines%                        /* EARNINGS */
                str(text$,,30%) =  type$(i%)
                convert addamount$(i%) to expense, data goto L34280
                if abs(expense) < .001 then L34280
                temp$ = edebit$(i%)
                str(text$,1%,30%) =  type$(i%)
                call "GLUNFMT" (temp$)
                if export_on$ <> "Y" then L34240
                    tran_type$   = "EMC01"
                    gl_catg$     = ecat$(i%)
                    gl_earnings$ = type$(i%)
                    gl_postamt   = expense
                    gosub load_gl_info
L34240:         call "GLPOST2" (temp$, expense, 0, prldate$, 0%, "09",   ~
                               text$, jnlid$, pstseq%, userid$, #5, #7,  ~
                               #9, err%, " ", gl_post_info$())
L34280:     next i%

        REM Credit Accrual Account With Gross...
            temp$ = grossacct$
            call "GLUNFMT" (temp$)
            str(text$,1%,30%) =  str(empcode$) & check$
            if export_on$ <> "Y" then L34340
                tran_type$   = "EMC02"
                init (" ")  gl_catg$, gl_earnings$
                gl_postamt   = -gross
                gosub load_gl_info
L34340:     call "GLPOST2" (temp$, 0, gross, prldate$, 0%, "09", text$,  ~
                           jnlid$, pstseq%, userid$, #5, #7, #9, err%,   ~
                           " ", gl_post_info$())

        REM Post Deductions...
            for i% = 1% to maxlines1%                     /* DEDUCTIONS */
                text$ =  str(dedc$(i%)) & " (" & dmthd$(i%)
                text$ = text$ & ")"
                str(text$,31%) = str(empcode$) & check$
                str(text$,69%) = "PRL DED: " & name1$
                convert dedamount$(i%) to liability, data goto L34610
                if abs(liability) < .001 then L34610
                temp$ = ddebit$(i%)
                call "GLUNFMT" (temp$)
                if export_on$ <> "Y" then L34500
                    tran_type$     = "EMC02"
                    if emp$(i%)    = "N" then tran_type$ = "EMC04"
                    gl_deductions$ = dedc$(i%)
                    gl_postamt     = liability
                    gosub load_gl_info
L34500:         call "GLPOST2" (temp$, liability, 0, prldate$, 0%, "09", ~
                               text$, jnlid$, pstseq%, userid$, #5, #7,  ~
                               #9, err%, " ", gl_post_info$())

                temp$ = dcredit$(i%)
                call "GLUNFMT" (temp$)
                if export_on$ <> "Y" then L34570
                    tran_type$     = "EMC03"
                    gl_postamt     = -liability
                    gosub load_gl_info
L34570:         call "GLPOST2" (temp$, 0, liability, prldate$, 0%, "09", ~
                               text$, jnlid$, pstseq%, userid$, #5, #7,  ~
                               #9, err%, " ", gl_post_info$())
L34610:     next i%

        REM Debit Accrual Account With Net For Consistency...
            temp$ = grossacct$
            call "GLUNFMT" (temp$)
            text$ = str(empcode$) & check$
            str(text$,31%) = str(empcode$) & check$
            str(text$,69%) = "MANUAL CHK:" & name1$
            if export_on$ <> "Y" then L34700
                tran_type$     = "EMC02"
                gl_deductions$ = " "
                gl_postamt     = net
                gosub load_gl_info
L34700:     call "GLPOST2" (temp$, net, 0, prldate$, 0%, "09", text$,    ~
                           jnlid$, pstseq%, userid$, #5, #7, #9, err%,   ~
                           " ", gl_post_info$())

        REM Credit Cash...
            temp$ = cashacct$
            call "GLUNFMT" (temp$)
            if export_on$ <> "Y" then L34780
                tran_type$     = "EMC05"
                gl_postamt     = -net
                gosub load_gl_info
L34780:     call "GLPOST2" (temp$, 0, net, prldate$, 0%, "09", text$,    ~
                           jnlid$, pstseq%, userid$, #5, #7, #9, err%,   ~
                           " ", gl_post_info$())
            return

        load_gl_info

            put str(gl_post_info$(),,) using L35390,                      ~
                tran_type$,              /* Transaction Type           */~
                " ",                     /* Currency code              */~
                0,                       /* Currency Units per Book    */~
                gl_postamt,              /* Functional Currency amount */~
                0,                       /* Unit amount                */~
                " ",                     /* Customer code              */~
                " ",                     /* Sales Order number         */~
                " ",                     /* BOL number                 */~
                " ",                     /* Customer Type              */~
                " ",                     /* State                      */~
                " ",                     /* Country                    */~
                " ",                     /* ZIP                        */~
                " ",                     /* Sales Region               */~
                " ",                     /* Sales Tax code             */~
                " ",                     /* Shipping Region            */~
                " ",                     /* Salesman code              */~
                " ",                     /* Invoice Number             */~
                " ",                     /* Part Number                */~
                " ",                     /* Part Category              */~
                " ",                     /* Part Class                 */~
                " ",                     /* Part Generic code          */~
                " ",                     /* Part Type                  */~
                " ",                     /* Part UOM                   */~
                " ",                     /* Store Number               */~
                " ",                     /* Check Receipt Number       */~
                " ",                     /* Vendor code                */~
                " ",                     /* Vendor type                */~
                " ",                     /* Purchase Order             */~
                " ",                     /* Receiver Number            */~
                " ",                     /* Vendor Invoice             */~
                gl_check$,               /* Check Payment Number       */~
                " ",                     /* Project code               */~
                " ",                     /* Job number                 */~
                " ",                     /* Work Center                */~
                " ",                     /* Activity code              */~
                gl_employee$,            /* Employee number            */~
                gl_depart$,              /* Department code            */~
                " ",                     /* Cost Center                */~
                gl_earnings$,            /* Earnings Type              */~
                gl_deductions$,          /* Deduction Type             */~
                gl_catg$,                /* P/R Category               */~
                " ",                     /* Labor class                */~
                " "                      /* Filler                     */

            return

L35390: FMT     CH(5),                   /* Transaction Type           */~
                CH(4),                   /* Currency code              */~
                PD(15,7),                /* Transaction Currency amount*/~
                PD(15,4),                /* Functional Currency amount */~
                PD(15,4),                /* Unit amount                */~
                CH(9),                   /* Customer code              */~
                CH(16),                  /* Sales Order number         */~
                CH(3),                   /* BOL number                 */~
                CH(2),                   /* Customer Type              */~
                CH(2),                   /* State                      */~
                CH(3),                   /* Country                    */~
                CH(9),                   /* ZIP                        */~
                CH(4),                   /* Sales Region               */~
                CH(10),                  /* Sales Tax code             */~
                CH(4),                   /* Shipping Region            */~
                CH(4),                   /* Salesman code              */~
                CH(8),                   /* Invoice Number             */~
                CH(25),                  /* Part Number                */~
                CH(4),                   /* Part Category              */~
                CH(4),                   /* Part Class                 */~
                CH(16),                  /* Part Generic code          */~
                CH(3),                   /* Part Type                  */~
                CH(4),                   /* Part UOM                   */~
                CH(3),                   /* Store Number               */~
                CH(8),                   /* Check Receipt Number       */~
                CH(9),                   /* Vendor code                */~
                CH(4),                   /* Vendor type                */~
                CH(16),                  /* Purchase Order             */~
                CH(16),                  /* Receiver Number            */~
                CH(16),                  /* Vendor Invoice             */~
                CH(8),                   /* Check Payment Number       */~
                CH(8),                   /* Project code               */~
                CH(8),                   /* Job number                 */~
                CH(4),                   /* Work Center                */~
                CH(4),                   /* Activity code              */~
                CH(12),                  /* Employee number            */~
                CH(4),                   /* Department code            */~
                CH(4),                   /* Cost Center                */~
                CH(12),                  /* Earnings Type              */~
                CH(12),                  /* Deduction Type             */~
                CH(4),                   /* P/R Category               */~
                CH(4),                   /* Labor class                */~
                CH(191)                  /* Filler                     */

        REM *************************************************************~
            *                 D A T A      S A V E                      *~
            *                                                           *~
            * THIS IS WHERE THE CHECK IS ACTUALY SAVED.                 *~
             *************************************************************

        save_check
            call "SHOSTAT" ("Saving Check")
            call "DATUNFMT" (checkdate$)
            call "DATUFMTC" (sdate$)
            call "DATUFMTC" (edate$)
            temp$ = cashacct$
            call "GLUNFMT" (temp$)

        REM Determine Next Sequence Number For This Check...
            readkey$ = str(empcode$)
            str(readkey$,13%) = check$
            call "DELETE" (#6, readkey$, 20%)
            str(readkey$, 21%) = " "
            call "DELETE" (#10, readkey$, 20%)

            write #6,  using L36630,                                      ~
                       empcode$, check$, "999", checkdate$, sdate$,      ~
                       edate$, temp$, gross, net, "N", " ", prldate$,    ~
                       department$, " "

        REM Write Check Details...
            temp1$ = grossacct$
            call "GLUNFMT" (temp1$)
        REM Earnings First...
            for i% = 1% to maxlines%
                convert addamount$(i%) to addamount, data goto L36410
                    if addamount = 0 then L36410
                addunits = 0
                convert addunits$(i%) to addunits, data goto L36350
L36350:         temp$ = edebit$(i%)
                call "GLUNFMT" (temp$)
                write #10, using L36780, empcode$, check$, "999",         ~
                                "P", i%, " ", ecat$(i%), type$(i%),      ~
                                units$(i%), addunits, rate(i%),          ~
                                addamount, temp$, temp1$, " "
L36410:     next i%

        REM Deductions...
            for i% = 1% to maxlines1%
                convert dedamount$(i%) to amount, data goto L36590
                    if amount = 0 then L36590
                if emp$(i%) <> "Y" then emp$(i%) = "N"
                subject, unitsub = 0
                convert subject$(i%) to subject, data goto L36500
L36500:         convert unitsub$(i%) to unitsub, data goto L36510
L36510:         temp$ = ddebit$(i%)
                call "GLUNFMT" (temp$)
                temp1$ = dcredit$(i%)
                call "GLUNFMT" (temp1$)
                write #10, using L36780, empcode$, check$, "999",         ~
                                "W", i%, emp$(i%), dcat$(i%), dedc$(i%), ~
                                dmthd$(i%), unitsub, subject, amount,    ~
                                temp$, temp1$, " "
L36590:     next i%
            gosub L38000                                /* PRINT ROUTINE */
            return

L36630:     FMT CH(12),                  /* EMPLOYEE CODE              */~
                CH(8),                   /* CHECK NUMBER               */~
                CH(3),                   /* REVERSE SEQUENCE NUMBER    */~
                CH(6),                   /* DATE OF CHECK              */~
                CH(6),                   /* FIRST DATE THIS PAY PERIOD */~
                CH(6),                   /* LAST DATE THIS PAY PERIOD  */~
                CH(9),                   /* CASH IN BANK ACCOUNT       */~
                PD(14,4),                /* GROSS PAY AMOUNT           */~
                PD(14,4),                /* NET PAY AMOUNT             */~
                CH(1),                   /* RECONCILIATION FLAG        */~
                CH(6),                   /* RECONCILIATION DATE        */~
                CH(6),                   /* POSTING DATE               */~
                CH(4),                   /* EMPLOYEE DEPARTMENT CODE   */~
                CH(37)                   /* EARN. REGISTER FREE SPACE  */~

L36780:     FMT CH(12),                  /* EMPLOYEE CODE              */~
                CH(8),                   /* CHECK NUMBER               */~
                CH(3),                   /* REVERSE SEQUENCE NUMBER    */~
                CH(1),                   /* Record Type (P=PAY, W=WHLD)*/~
                BI(1),                   /* SEQUENCE NUMBER            */~
                CH(1),                   /* Employee Paid? DED only    */~
                CH(6),                   /* CATEGORY                   */~
                CH(12),                  /* EARN/DEDUCTION TYPE        */~
                CH(6),                   /* UNITS DESCR/DED METHOD     */~
                PD(14,4),                /* UNITS / UNITS SUBJECT      */~
                PD(14,4),                /* RATE / DOLLARS SUBJECT     */~
                PD(14,4),                /* AMOUNT                     */~
                CH(9),                   /* DEBIT ACCOUNT              */~
                CH(9),                   /* CREBIT ACCOUNT             */~
                CH(28)                   /* EARN. REGISTER FREE SPACE  */

L38000: REM *************************************************************~
            *         PRINT THE REPORT FOR THIS CHECK                   *~
            *************************************************************

            call "SHOSTAT" ("Printing Check Details")

            if select% = 0% then select printer (134)
            if select% = 0% then print page
            select% = 1%
            pages% = pages% + 1%

            print using L38650, date$, runtime$, userid$, pages%
            print skip (1)
            cd$ = checkdate$
            call "DATEFMT" (cd$)
            print using L38670, empcode$, name$, check$, cd$
            print skip (1)
            print using L38720, gross, prldatef$
            print using L38740, deductions, grossacct$, grossdescr$
            print using L38760, net,         cashacct$,  cashdescr$
            print skip (1)
            print using L38780, postmsg$
            print skip (1)
            print using L38580  /* HEADER */
            print using L38600
            j1%, j2% = 0%
            for jode% = 1% to 99%
L38280:         j1% = j1% + 1%
                if j1% > 99% then L38330
                convert addamount$(j1%) to a, data goto L38280
                if a = 0. then L38280

L38330:         j2% = j2% + 1%
                if j2% > 99% then L38380
                convert dedamount$(j2%) to d, data goto L38330
                if d = 0. then L38330

L38380:         if j1% > 99% and j2% > 99% then L38540
                if j1% > 99% then j1% = 100%
                if j2% > 99% then j2% = 100%

                dd$ = " " & dedamount$(j2%) & " "
                if emp$(j2%) = "Y" then L38480
                if dd$ = " " then L38480
                str(dd$,pos(dd$<>" ")-1%,1%) = "("
                str(dd$,12%) = ")"

L38480:         print using L38620, type$(j1%), edebit$(j1%),             ~
                                   addamount$(j1%), addunits$(j1%),      ~
                                   dedc$(j2%), dcredit$(j2%),            ~
                                   dd$,                                  ~
                                   subject$(j2%), unitsub$(j2%)

L38540:     next jode%
            print page
            return

L38580: %EARN CODE      G/L ACCT  EARN AMOUNT EARN UNITS ! DEDUCTN CODE  ~
        ~ G/L ACCT     DEDUCTN AMT   $ SUBJECT  UNITS SUBJECT

L38600: %------------ ------------ ---------- ---------- ! ------------ -~
        ~-----------  ------------  ----------  -------------

L38620: %############ ############ ########## ########## ! ############ #~
        ~###########  ############  ##########     ##########

L38650: %RUN ########  ########                         MANUAL PAYROLL CH~
        ~ECK ENTRY              OPERATOR ID: ###    PAGE:###

L38670: %EMPLOYEE CODE: ########## ############################## CHECK N~
        ~UMBER: ##########   CHECK DATE: ########

L38720: %      GROSS PAY:-#######.##                    G/L POSTING DATE:~
        ~  ########

L38740: %LESS DEDUCTIONS:-#######.##         PAYROLL ACCRUAL G/L ACCOUNT:~
        ~  ############  ##############################

L38760: %        NET PAY:-#######.##            CASH IN BANK G/L ACCOUNT:~
        ~  ############  ##############################

L38780: %################################################################~
        ~##########################

        REM *************************************************************~
            *      I N P U T   A N D   E D I T   S C R E E N   1        *~
            *                                                           *~
            * INPUT AND EDIT DOCUMENTS                                  *~
            *************************************************************

            deffn'101(fieldnr%,edit%)
                gosub set_pf
                if edit% <> 2% then L40090
                if fieldnr% = 0% then inpmessage$ = "To Modify, Positio"&~
                    "n Cursor to Desired Value and Press RETURN."
L40090:         if fieldnr% > 0% then init(hex(8c)) lfac$()              ~
                                 else init(hex(86)) lfac$()
                if fieldnr% = 0% then L40280
                    lfac$(fieldnr%) = hex(81)

L40280:     accept                                                       ~
               at (01,02), "Directly Input Payroll Checks",              ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)),   date$                , ch(08),~
               at (02,02), fac(hex(ac)),   line2$,                       ~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (06,02), "Employee Code",                              ~
               at (06,30), fac(lfac$( 1%)), empcode$            , ch(12),~
               at (06,45), fac(hex(8c)), name$                  , ch(30),~
               at (07,02), "Check Number",                               ~
               at (07,30), fac(lfac$( 2%)), check$              , ch(08),~
               at (07,45), fac(hex(84)), checkmsg$(1%)          , ch(36),~
               at (08,45), fac(hex(84)), checkmsg$(2%)          , ch(36),~
               at (09,45), fac(hex(84)), checkmsg$(3%)          , ch(36),~
               at (08,02), "Check Date",                                 ~
               at (08,30), fac(lfac$( 3%)), checkdate$          , ch(08),~
               at (09,02), "For Period Starting",                        ~
               at (09,30), fac(lfac$( 4%)), sdate$              , ch(10),~
               at (10,11), "And Ending",                                 ~
               at (10,30), fac(lfac$( 5%)), edate$              , ch(10),~
               at (11,02), "Cash In Bank Account",                       ~
               at (11,30), fac(lfac$( 6%)), cashacct$           , ch(12),~
               at (11,45), fac(hex(8c)), cashdescr$             , ch(32),~
               at (12,02), "Post To Employee Accruals?",                 ~
               at (12,30), fac(lfac$( 7%)), postaccruals$       , ch(01),~
               at (13,02), "Post To General Ledger?",                    ~
               at (13,30), fac(lfac$( 8%)), postledger$         , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$(1%)), key(keyhit%)

               if keyhit% <> 13% then L40860
                  call "MANUAL" ("PRLMANUL")
                  goto L40280

L40860:        if keyhit% <> 15% then L40900
                  call "PRNTSCRN"
                  goto L40280

L40900:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf
            if edit% = 2% then L40960                     /* Input Mode  */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$(1%)= hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40954
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L40954:     if fieldnr% > 1% then L40958
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40958:     return
L40960:                                                  /*  Edit Mode  */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Continue    "
            pfkeys$(1%) = hex(01ffffffffffffffffffffff0dff0f1000)
            return

        REM *************************************************************~
            *           I N P U T   G R O S S   E A R N I N G S         *~
            *                                                           *~
            * SHOWS THE POSSIBLE EANRTYPES FOR THIS EMPLOYEE, ALLOWS    *~
            * THE USER TO ENTER AMOUNT AND UNITS FOR EACH.              *~
            *************************************************************

            deffn'203(screenline%, fieldnr%)
                  init(hex(8c)) fac$()
                  screen% = 1%
                  goto L42107

            deffn'213(screenline%, fieldnr%)
                  init(hex(8e)) fac$()
                  if (maxlines% - line%) < 20% then                      ~
                     init(hex(8c)) str(fac$(),(4%*(maxlines%- line%))+1% )
                  screen% = 2%
                  goto L42107

            deffn'223(screenline%, fieldnr%)
                  init(hex(8c)) fac$()
                  screen% = 2%

L42107:           pfkeys$(1%) = hex(000102ff04ffffff08ffffffffffff0f10)
                  pfkeys$(2%) = hex(000102030405060708ffffffffffff0f10)

                  on fieldnr% gosub L42170,         /* UNITS TO ADD     */~
                                    L42170,         /* AMOUNT TO ADD    */~
                                    L42140          /* ACCOUNT          */
                     goto L42190

L42140:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(screenline%, fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(81)
                      return
L42170:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(82)
                      return

L42190:     accept                                                       ~
               at (01,02), fac(hex(8c)),    title$(screen%)     , ch(64),~
               at (02,02), fac(hex(84)),    infomsg$            , ch(64),~
               at (03,02), fac(hex(94)),    errormsg$           , ch(64),~
               at (04,02), fac(hex(ac)),    heading$            , ch(79),~
               at (05,71), fac(hex(84)),    gross      , pic(-$#####.##),~
                                                                         ~
               at (01,67), "! EMPLOYEE:   "                             ,~
               at (02,67), "!             "                             ,~
               at (03,67), "+-------------"                             ,~
               at (02,69), fac(hex(84)),    empcode$            , ch(12),~
                                                                         ~
               at (05,02), fac(hex(8c)),    type$    (line%+ 1%), ch(12),~
               at (06,02), fac(hex(8c)),    type$    (line%+ 2%), ch(12),~
               at (07,02), fac(hex(8c)),    type$    (line%+ 3%), ch(12),~
               at (08,02), fac(hex(8c)),    type$    (line%+ 4%), ch(12),~
               at (09,02), fac(hex(8c)),    type$    (line%+ 5%), ch(12),~
               at (10,02), fac(hex(8c)),    type$    (line%+ 6%), ch(12),~
               at (11,02), fac(hex(8c)),    type$    (line%+ 7%), ch(12),~
               at (12,02), fac(hex(8c)),    type$    (line%+ 8%), ch(12),~
               at (13,02), fac(hex(8c)),    type$    (line%+ 9%), ch(12),~
               at (14,02), fac(hex(8c)),    type$    (line%+10%), ch(12),~
               at (15,02), fac(hex(8c)),    type$    (line%+11%), ch(12),~
               at (16,02), fac(hex(8c)),    type$    (line%+12%), ch(12),~
               at (17,02), fac(hex(8c)),    type$    (line%+13%), ch(12),~
               at (18,02), fac(hex(8c)),    type$    (line%+14%), ch(12),~
               at (19,02), fac(hex(8c)),    type$    (line%+15%), ch(12),~
               at (20,02), fac(hex(8c)),    type$    (line%+16%), ch(12),~
               at (21,02), fac(hex(8c)),    type$    (line%+17%), ch(12),~
               at (22,02), fac(hex(8c)),    type$    (line%+18%), ch(12),~
               at (23,02), fac(hex(8c)),    type$    (line%+19%), ch(12),~
               at (24,02), fac(hex(8c)),    type$    (line%+20%), ch(12),~
                                                                         ~
               at (05,20), fac(hex(8c)),    units$   (line%+ 1%), ch(06),~
               at (06,20), fac(hex(8c)),    units$   (line%+ 2%), ch(06),~
               at (07,20), fac(hex(8c)),    units$   (line%+ 3%), ch(06),~
               at (08,20), fac(hex(8c)),    units$   (line%+ 4%), ch(06),~
               at (09,20), fac(hex(8c)),    units$   (line%+ 5%), ch(06),~
               at (10,20), fac(hex(8c)),    units$   (line%+ 6%), ch(06),~
               at (11,20), fac(hex(8c)),    units$   (line%+ 7%), ch(06),~
               at (12,20), fac(hex(8c)),    units$   (line%+ 8%), ch(06),~
               at (13,20), fac(hex(8c)),    units$   (line%+ 9%), ch(06),~
               at (14,20), fac(hex(8c)),    units$   (line%+10%), ch(06),~
               at (15,20), fac(hex(8c)),    units$   (line%+11%), ch(06),~
               at (16,20), fac(hex(8c)),    units$   (line%+12%), ch(06),~
               at (17,20), fac(hex(8c)),    units$   (line%+13%), ch(06),~
               at (18,20), fac(hex(8c)),    units$   (line%+14%), ch(06),~
               at (19,20), fac(hex(8c)),    units$   (line%+15%), ch(06),~
               at (20,20), fac(hex(8c)),    units$   (line%+16%), ch(06),~
               at (21,20), fac(hex(8c)),    units$   (line%+17%), ch(06),~
               at (22,20), fac(hex(8c)),    units$   (line%+18%), ch(06),~
               at (23,20), fac(hex(8c)),    units$   (line%+19%), ch(06),~
               at (24,20), fac(hex(8c)),    units$   (line%+20%), ch(06),~
                                                                         ~
               at (05,27), fac(fac$( 1%,1%)),addunits$(line%+ 1%),ch(10),~
               at (06,27), fac(fac$( 2%,1%)),addunits$(line%+ 2%),ch(10),~
               at (07,27), fac(fac$( 3%,1%)),addunits$(line%+ 3%),ch(10),~
               at (08,27), fac(fac$( 4%,1%)),addunits$(line%+ 4%),ch(10),~
               at (09,27), fac(fac$( 5%,1%)),addunits$(line%+ 5%),ch(10),~
               at (10,27), fac(fac$( 6%,1%)),addunits$(line%+ 6%),ch(10),~
               at (11,27), fac(fac$( 7%,1%)),addunits$(line%+ 7%),ch(10),~
               at (12,27), fac(fac$( 8%,1%)),addunits$(line%+ 8%),ch(10),~
               at (13,27), fac(fac$( 9%,1%)),addunits$(line%+ 9%),ch(10),~
               at (14,27), fac(fac$(10%,1%)),addunits$(line%+10%),ch(10),~
               at (15,27), fac(fac$(11%,1%)),addunits$(line%+11%),ch(10),~
               at (16,27), fac(fac$(12%,1%)),addunits$(line%+12%),ch(10),~
               at (17,27), fac(fac$(13%,1%)),addunits$(line%+13%),ch(10),~
               at (18,27), fac(fac$(14%,1%)),addunits$(line%+14%),ch(10),~
               at (19,27), fac(fac$(15%,1%)),addunits$(line%+15%),ch(10),~
               at (20,27), fac(fac$(16%,1%)),addunits$(line%+16%),ch(10),~
               at (21,27), fac(fac$(17%,1%)),addunits$(line%+17%),ch(10),~
               at (22,27), fac(fac$(18%,1%)),addunits$(line%+18%),ch(10),~
               at (23,27), fac(fac$(19%,1%)),addunits$(line%+19%),ch(10),~
               at (24,27), fac(fac$(20%,1%)),addunits$(line%+20%),ch(10),~
                                                                         ~
               at (05,38),fac(fac$( 1%,2%)),addamount$(line%+ 1%),ch(10),~
               at (06,38),fac(fac$( 2%,2%)),addamount$(line%+ 2%),ch(10),~
               at (07,38),fac(fac$( 3%,2%)),addamount$(line%+ 3%),ch(10),~
               at (08,38),fac(fac$( 4%,2%)),addamount$(line%+ 4%),ch(10),~
               at (09,38),fac(fac$( 5%,2%)),addamount$(line%+ 5%),ch(10),~
               at (10,38),fac(fac$( 6%,2%)),addamount$(line%+ 6%),ch(10),~
               at (11,38),fac(fac$( 7%,2%)),addamount$(line%+ 7%),ch(10),~
               at (12,38),fac(fac$( 8%,2%)),addamount$(line%+ 8%),ch(10),~
               at (13,38),fac(fac$( 9%,2%)),addamount$(line%+ 9%),ch(10),~
               at (14,38),fac(fac$(10%,2%)),addamount$(line%+10%),ch(10),~
               at (15,38),fac(fac$(11%,2%)),addamount$(line%+11%),ch(10),~
               at (16,38),fac(fac$(12%,2%)),addamount$(line%+12%),ch(10),~
               at (17,38),fac(fac$(13%,2%)),addamount$(line%+13%),ch(10),~
               at (18,38),fac(fac$(14%,2%)),addamount$(line%+14%),ch(10),~
               at (19,38),fac(fac$(15%,2%)),addamount$(line%+15%),ch(10),~
               at (20,38),fac(fac$(16%,2%)),addamount$(line%+16%),ch(10),~
               at (21,38),fac(fac$(17%,2%)),addamount$(line%+17%),ch(10),~
               at (22,38),fac(fac$(18%,2%)),addamount$(line%+18%),ch(10),~
               at (23,38),fac(fac$(19%,2%)),addamount$(line%+19%),ch(10),~
               at (24,38),fac(fac$(20%,2%)),addamount$(line%+20%),ch(10),~
                                                                         ~
               at (05,51), fac(fac$( 1%,3%)), edebit$(line%+ 1%), ch(12),~
               at (06,51), fac(fac$( 2%,3%)), edebit$(line%+ 2%), ch(12),~
               at (07,51), fac(fac$( 3%,3%)), edebit$(line%+ 3%), ch(12),~
               at (08,51), fac(fac$( 4%,3%)), edebit$(line%+ 4%), ch(12),~
               at (09,51), fac(fac$( 5%,3%)), edebit$(line%+ 5%), ch(12),~
               at (10,51), fac(fac$( 6%,3%)), edebit$(line%+ 6%), ch(12),~
               at (11,51), fac(fac$( 7%,3%)), edebit$(line%+ 7%), ch(12),~
               at (12,51), fac(fac$( 8%,3%)), edebit$(line%+ 8%), ch(12),~
               at (13,51), fac(fac$( 9%,3%)), edebit$(line%+ 9%), ch(12),~
               at (14,51), fac(fac$(10%,3%)), edebit$(line%+10%), ch(12),~
               at (15,51), fac(fac$(11%,3%)), edebit$(line%+11%), ch(12),~
               at (16,51), fac(fac$(12%,3%)), edebit$(line%+12%), ch(12),~
               at (17,51), fac(fac$(13%,3%)), edebit$(line%+13%), ch(12),~
               at (18,51), fac(fac$(14%,3%)), edebit$(line%+14%), ch(12),~
               at (19,51), fac(fac$(15%,3%)), edebit$(line%+15%), ch(12),~
               at (20,51), fac(fac$(16%,3%)), edebit$(line%+16%), ch(12),~
               at (21,51), fac(fac$(17%,3%)), edebit$(line%+17%), ch(12),~
               at (22,51), fac(fac$(18%,3%)), edebit$(line%+18%), ch(12),~
               at (23,51), fac(fac$(19%,3%)), edebit$(line%+19%), ch(12),~
               at (24,51), fac(fac$(20%,3%)), edebit$(line%+20%), ch(12),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)

               if keyhit% <> 15% then L42815
                  call "PRNTSCRN"
                  goto L42190

L42815:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *        I N P U T   D E D U C T I O N   D E T A I L        *~
            *                                                           *~
            * SHOWS THE POSSIBLE DEDUCTIONS FOR THIS EMPLOYEE, ALLOWS   *~
            * THE USER TO ENTER AMOUNT FOR EACH.                        *~
            *************************************************************

            deffn'250(screenline%, fieldnr%)
                  init(hex(8c)) fac$()
                  screen% = 1%
                  goto L43107

            deffn'251(screenline%, fieldnr%)
                  init(hex(8e)) fac$()
                  if (maxlines1% - line%) < 20% then                     ~
                     init(hex(8c)) str(fac$(),(4%*(maxlines1%- line%))+1%)
                  screen% = 2%
                  goto L43107

            deffn'252(screenline%, fieldnr%)
                  init(hex(8c)) fac$()
                  screen% = 2%

L43107:           pfkeys$(1%) = hex(000102040f0810ffffffffffffffffffff)
                  pfkeys$(2%) = hex(00010203040506070f10ffffffffffffff)

                  on fieldnr% gosub L43175,         /* UNITS   SUBJECT  */~
                                    L43175,         /* DOLLARS SUBJECT  */~
                                    L43175,         /* DEDUCTION AMOUNT */~
                                    L43145          /* DEBIT ACCOUNT    */
                     goto L43195

L43145:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(screenline%, fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(81)
                      return
L43175:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(82)
                      return

L43195:     accept                                                       ~
               at (01,02), fac(hex(8c)),    title$(screen%)     , ch(64),~
               at (02,02), fac(hex(84)),    infomsg$            , ch(64),~
               at (03,02), fac(hex(94)),    errormsg$           , ch(64),~
               at (04,02), fac(hex(ac)),    heading1$           , ch(79),~
               at (05,71), fac(hex(8c)),    net        , pic(-$#####.##),~
                                                                         ~
               at (01,67), "! EMPLOYEE:   "                             ,~
               at (02,67), "!             "                             ,~
               at (03,67), "+-------------"                             ,~
               at (02,69), fac(hex(84)),    empcode$            , ch(12),~
                                                                         ~
               at (05,02), fac(hex(8c)),    dedc$    (line%+ 1%), ch(12),~
               at (06,02), fac(hex(8c)),    dedc$    (line%+ 2%), ch(12),~
               at (07,02), fac(hex(8c)),    dedc$    (line%+ 3%), ch(12),~
               at (08,02), fac(hex(8c)),    dedc$    (line%+ 4%), ch(12),~
               at (09,02), fac(hex(8c)),    dedc$    (line%+ 5%), ch(12),~
               at (10,02), fac(hex(8c)),    dedc$    (line%+ 6%), ch(12),~
               at (11,02), fac(hex(8c)),    dedc$    (line%+ 7%), ch(12),~
               at (12,02), fac(hex(8c)),    dedc$    (line%+ 8%), ch(12),~
               at (13,02), fac(hex(8c)),    dedc$    (line%+ 9%), ch(12),~
               at (14,02), fac(hex(8c)),    dedc$    (line%+10%), ch(12),~
               at (15,02), fac(hex(8c)),    dedc$    (line%+11%), ch(12),~
               at (16,02), fac(hex(8c)),    dedc$    (line%+12%), ch(12),~
               at (17,02), fac(hex(8c)),    dedc$    (line%+13%), ch(12),~
               at (18,02), fac(hex(8c)),    dedc$    (line%+14%), ch(12),~
               at (19,02), fac(hex(8c)),    dedc$    (line%+15%), ch(12),~
               at (20,02), fac(hex(8c)),    dedc$    (line%+16%), ch(12),~
               at (21,02), fac(hex(8c)),    dedc$    (line%+17%), ch(12),~
               at (22,02), fac(hex(8c)),    dedc$    (line%+18%), ch(12),~
               at (23,02), fac(hex(8c)),    dedc$    (line%+19%), ch(12),~
               at (24,02), fac(hex(8c)),    dedc$    (line%+20%), ch(12),~
                                                                         ~
               at (05,24), fac(hex(8c)),      emp$   (line%+ 1%), ch(01),~
               at (06,24), fac(hex(8c)),      emp$   (line%+ 2%), ch(01),~
               at (07,24), fac(hex(8c)),      emp$   (line%+ 3%), ch(01),~
               at (08,24), fac(hex(8c)),      emp$   (line%+ 4%), ch(01),~
               at (09,24), fac(hex(8c)),      emp$   (line%+ 5%), ch(01),~
               at (10,24), fac(hex(8c)),      emp$   (line%+ 6%), ch(01),~
               at (11,24), fac(hex(8c)),      emp$   (line%+ 7%), ch(01),~
               at (12,24), fac(hex(8c)),      emp$   (line%+ 8%), ch(01),~
               at (13,24), fac(hex(8c)),      emp$   (line%+ 9%), ch(01),~
               at (14,24), fac(hex(8c)),      emp$   (line%+10%), ch(01),~
               at (15,24), fac(hex(8c)),      emp$   (line%+11%), ch(01),~
               at (16,24), fac(hex(8c)),      emp$   (line%+12%), ch(01),~
               at (17,24), fac(hex(8c)),      emp$   (line%+13%), ch(01),~
               at (18,24), fac(hex(8c)),      emp$   (line%+14%), ch(01),~
               at (19,24), fac(hex(8c)),      emp$   (line%+15%), ch(01),~
               at (20,24), fac(hex(8c)),      emp$   (line%+16%), ch(01),~
               at (21,24), fac(hex(8c)),      emp$   (line%+17%), ch(01),~
               at (22,24), fac(hex(8c)),      emp$   (line%+18%), ch(01),~
               at (23,24), fac(hex(8c)),      emp$   (line%+19%), ch(01),~
               at (24,24), fac(hex(8c)),      emp$   (line%+20%), ch(01),~
                                                                         ~
               at (05,26), fac(fac$( 1%,1%)),unitsub$ (line%+ 1%),ch(10),~
               at (06,26), fac(fac$( 2%,1%)),unitsub$ (line%+ 2%),ch(10),~
               at (07,26), fac(fac$( 3%,1%)),unitsub$ (line%+ 3%),ch(10),~
               at (08,26), fac(fac$( 4%,1%)),unitsub$ (line%+ 4%),ch(10),~
               at (09,26), fac(fac$( 5%,1%)),unitsub$ (line%+ 5%),ch(10),~
               at (10,26), fac(fac$( 6%,1%)),unitsub$ (line%+ 6%),ch(10),~
               at (11,26), fac(fac$( 7%,1%)),unitsub$ (line%+ 7%),ch(10),~
               at (12,26), fac(fac$( 8%,1%)),unitsub$ (line%+ 8%),ch(10),~
               at (13,26), fac(fac$( 9%,1%)),unitsub$ (line%+ 9%),ch(10),~
               at (14,26), fac(fac$(10%,1%)),unitsub$ (line%+10%),ch(10),~
               at (15,26), fac(fac$(11%,1%)),unitsub$ (line%+11%),ch(10),~
               at (16,26), fac(fac$(12%,1%)),unitsub$ (line%+12%),ch(10),~
               at (17,26), fac(fac$(13%,1%)),unitsub$ (line%+13%),ch(10),~
               at (18,26), fac(fac$(14%,1%)),unitsub$ (line%+14%),ch(10),~
               at (19,26), fac(fac$(15%,1%)),unitsub$ (line%+15%),ch(10),~
               at (20,26), fac(fac$(16%,1%)),unitsub$ (line%+16%),ch(10),~
               at (21,26), fac(fac$(17%,1%)),unitsub$ (line%+17%),ch(10),~
               at (22,26), fac(fac$(18%,1%)),unitsub$ (line%+18%),ch(10),~
               at (23,26), fac(fac$(19%,1%)),unitsub$ (line%+19%),ch(10),~
               at (24,26), fac(fac$(20%,1%)),unitsub$ (line%+20%),ch(10),~
                                                                         ~
               at (05,37), fac(fac$( 1%,2%)),subject$ (line%+ 1%),ch(10),~
               at (06,37), fac(fac$( 2%,2%)),subject$ (line%+ 2%),ch(10),~
               at (07,37), fac(fac$( 3%,2%)),subject$ (line%+ 3%),ch(10),~
               at (08,37), fac(fac$( 4%,2%)),subject$ (line%+ 4%),ch(10),~
               at (09,37), fac(fac$( 5%,2%)),subject$ (line%+ 5%),ch(10),~
               at (10,37), fac(fac$( 6%,2%)),subject$ (line%+ 6%),ch(10),~
               at (11,37), fac(fac$( 7%,2%)),subject$ (line%+ 7%),ch(10),~
               at (12,37), fac(fac$( 8%,2%)),subject$ (line%+ 8%),ch(10),~
               at (13,37), fac(fac$( 9%,2%)),subject$ (line%+ 9%),ch(10),~
               at (14,37), fac(fac$(10%,2%)),subject$ (line%+10%),ch(10),~
               at (15,37), fac(fac$(11%,2%)),subject$ (line%+11%),ch(10),~
               at (16,37), fac(fac$(12%,2%)),subject$ (line%+12%),ch(10),~
               at (17,37), fac(fac$(13%,2%)),subject$ (line%+13%),ch(10),~
               at (18,37), fac(fac$(14%,2%)),subject$ (line%+14%),ch(10),~
               at (19,37), fac(fac$(15%,2%)),subject$ (line%+15%),ch(10),~
               at (20,37), fac(fac$(16%,2%)),subject$ (line%+16%),ch(10),~
               at (21,37), fac(fac$(17%,2%)),subject$ (line%+17%),ch(10),~
               at (22,37), fac(fac$(18%,2%)),subject$ (line%+18%),ch(10),~
               at (23,37), fac(fac$(19%,2%)),subject$ (line%+19%),ch(10),~
               at (24,37), fac(fac$(20%,2%)),subject$ (line%+20%),ch(10),~
                                                                         ~
               at (05,48),fac(fac$( 1%,3%)),dedamount$(line%+ 1%),ch(10),~
               at (06,48),fac(fac$( 2%,3%)),dedamount$(line%+ 2%),ch(10),~
               at (07,48),fac(fac$( 3%,3%)),dedamount$(line%+ 3%),ch(10),~
               at (08,48),fac(fac$( 4%,3%)),dedamount$(line%+ 4%),ch(10),~
               at (09,48),fac(fac$( 5%,3%)),dedamount$(line%+ 5%),ch(10),~
               at (10,48),fac(fac$( 6%,3%)),dedamount$(line%+ 6%),ch(10),~
               at (11,48),fac(fac$( 7%,3%)),dedamount$(line%+ 7%),ch(10),~
               at (12,48),fac(fac$( 8%,3%)),dedamount$(line%+ 8%),ch(10),~
               at (13,48),fac(fac$( 9%,3%)),dedamount$(line%+ 9%),ch(10),~
               at (14,48),fac(fac$(10%,3%)),dedamount$(line%+10%),ch(10),~
               at (15,48),fac(fac$(11%,3%)),dedamount$(line%+11%),ch(10),~
               at (16,48),fac(fac$(12%,3%)),dedamount$(line%+12%),ch(10),~
               at (17,48),fac(fac$(13%,3%)),dedamount$(line%+13%),ch(10),~
               at (18,48),fac(fac$(14%,3%)),dedamount$(line%+14%),ch(10),~
               at (19,48),fac(fac$(15%,3%)),dedamount$(line%+15%),ch(10),~
               at (20,48),fac(fac$(16%,3%)),dedamount$(line%+16%),ch(10),~
               at (21,48),fac(fac$(17%,3%)),dedamount$(line%+17%),ch(10),~
               at (22,48),fac(fac$(18%,3%)),dedamount$(line%+18%),ch(10),~
               at (23,48),fac(fac$(19%,3%)),dedamount$(line%+19%),ch(10),~
               at (24,48),fac(fac$(20%,3%)),dedamount$(line%+20%),ch(10),~
                                                                         ~
               at (05,59), fac(fac$( 1%,4%)), dcredit$(line%+ 1%),ch(12),~
               at (06,59), fac(fac$( 2%,4%)), dcredit$(line%+ 2%),ch(12),~
               at (07,59), fac(fac$( 3%,4%)), dcredit$(line%+ 3%),ch(12),~
               at (08,59), fac(fac$( 4%,4%)), dcredit$(line%+ 4%),ch(12),~
               at (09,59), fac(fac$( 5%,4%)), dcredit$(line%+ 5%),ch(12),~
               at (10,59), fac(fac$( 6%,4%)), dcredit$(line%+ 6%),ch(12),~
               at (11,59), fac(fac$( 7%,4%)), dcredit$(line%+ 7%),ch(12),~
               at (12,59), fac(fac$( 8%,4%)), dcredit$(line%+ 8%),ch(12),~
               at (13,59), fac(fac$( 9%,4%)), dcredit$(line%+ 9%),ch(12),~
               at (14,59), fac(fac$(10%,4%)), dcredit$(line%+10%),ch(12),~
               at (15,59), fac(fac$(11%,4%)), dcredit$(line%+11%),ch(12),~
               at (16,59), fac(fac$(12%,4%)), dcredit$(line%+12%),ch(12),~
               at (17,59), fac(fac$(13%,4%)), dcredit$(line%+13%),ch(12),~
               at (18,59), fac(fac$(14%,4%)), dcredit$(line%+14%),ch(12),~
               at (19,59), fac(fac$(15%,4%)), dcredit$(line%+15%),ch(12),~
               at (20,59), fac(fac$(16%,4%)), dcredit$(line%+16%),ch(12),~
               at (21,59), fac(fac$(17%,4%)), dcredit$(line%+17%),ch(12),~
               at (22,59), fac(fac$(18%,4%)), dcredit$(line%+18%),ch(12),~
               at (23,59), fac(fac$(19%,4%)), dcredit$(line%+19%),ch(12),~
               at (24,59), fac(fac$(20%,4%)), dcredit$(line%+20%),ch(12),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)

               if keyhit% <> 15% then L43925
                  call "PRNTSCRN"
                  goto L43195

L43925:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *  D I S P L A Y    E N T I R E    C H E C K    H E A D E R *~
            *                                                           *~
            * DISPLAYS THE ENTIRE HEADER OF A PAYROLL CHECK             *~
            *************************************************************

        show_summary:
            str(line2$,1%,13%) = "Check Summary"
            inpmessage$ = " "
            postmsg$  = "Employee Accruals WILL Be Posted,"
            if postaccruals$= "N" then postmsg$  = "Employee Accruals W"&~
                                       "ill NOT Be Posted,"
            if postledger$  = "N" then postmsg$  = postmsg$  & " G/L Wi"&~
                                       "ll NOT Be Posted"                ~
                                  else postmsg$  = postmsg$  & " G/L Wi"&~
                                       "ll Be Posted"
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "(2)See Detail                           " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Save Check  "


            accept                                                       ~
               at (01,02), "Directly Input Payroll Checks",              ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)),   date$                , ch(08),~
               at (02,02), fac(hex(ac)),   line2$,                       ~
                                                                         ~
               at (04,02), "Employee Code",                              ~
               at (04,18), fac(hex(84)),     empcode$           , ch(09),~
               at (04,20), fac(hex(84)),     name$              , ch(30),~
               at (06,02), "Check Number",                               ~
               at (06,18), fac(hex(84)),     check$             , ch(08),~
               at (08,02), "Payroll Accrual Account",                    ~
               at (08,30), fac(hex(8c)),     grossacct$         , ch(12),~
               at (08,49), fac(hex(8c)),     grossdescr$        , ch(32),~
               at (09,02), "Cash In Bank Account",                       ~
               at (09,30), fac(hex(8c)),     cashacct$          , ch(12),~
               at (09,49), fac(hex(8c)),     cashdescr$         , ch(32),~
               at (11,02), "Gross Pay",                                  ~
               at (11,29), fac(hex(8c)),     gross     ,pic(-$######.##),~
               at (12,02), "Less Deductions",                            ~
               at (12,29), fac(hex(8c)),     deductions,pic(-$######.##),~
               at (13,30), "===========",                                ~
               at (14,02), "Net Pay",                                    ~
               at (14,29), fac(hex(8c)),     net       ,pic(-$######.##),~
               at (16,02), "For Period",                                 ~
               at (16,13), fac(hex(84)),     sdate$             , ch(10),~
               at (16,24), "To ",                                        ~
               at (16,28), fac(hex(84)),     edate$             , ch(10),~
                                                                         ~
               at (16,40), "---------- AUDIT TRAIL DATES ----------",    ~
               at (17,40), "Check Date",                                 ~
               at (17,68), fac(hex(8c)), checkdate$             , ch(08),~
               at (18,40), "General Ledger Posting Date",                ~
               at (18,68), fac(hex(8c)), prldatef$              , ch(08),~
               at (20,02), fac(hex(84)), postmsg$               , ch(79),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(hex(0001020d0e0f10)),                                ~
               key (keyhit%)

            if keyhit% <> 13% then L44710
                call "MANUAL" ("PRLMANUL")
                goto show_summary

L44710:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto show_summary

        REM *************************************************************~
            *       D I S P L A Y   C H E C K   L I N E   I T E M S     *~
            *                                                           *~
            * DISPLAYS THE LINE ITEMS ON A CHECK.                       *~
            *************************************************************

        show_lines:

             accept                                                      ~
               at (01,02), "Press (ENTER) To Return",                    ~
               at (02,02), "Employee",                                   ~
               at (02,12), fac(hex(84)), empcode$               , ch(12),~
               at (02,52), "Check Number",                               ~
               at (02,66), fac(hex(84)), check$                 , ch(08),~
               at (04,02), fac(hex(ac)), heading2$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(8c)), line$( 1%)             , ch(79),~
               at (06,02), fac(hex(8c)), line$( 2%)             , ch(79),~
               at (07,02), fac(hex(8c)), line$( 3%)             , ch(79),~
               at (08,02), fac(hex(8c)), line$( 4%)             , ch(79),~
               at (09,02), fac(hex(8c)), line$( 5%)             , ch(79),~
               at (10,02), fac(hex(8c)), line$( 6%)             , ch(79),~
               at (11,02), fac(hex(8c)), line$( 7%)             , ch(79),~
               at (12,02), fac(hex(8c)), line$( 8%)             , ch(79),~
               at (13,02), fac(hex(8c)), line$( 9%)             , ch(79),~
               at (14,02), fac(hex(8c)), line$(10%)             , ch(79),~
               at (15,02), fac(hex(8c)), line$(11%)             , ch(79),~
               at (16,02), fac(hex(8c)), line$(12%)             , ch(79),~
               at (17,02), fac(hex(8c)), line$(13%)             , ch(79),~
               at (18,02), fac(hex(8c)), line$(14%)             , ch(79),~
               at (19,02), fac(hex(8c)), line$(15%)             , ch(79),~
               at (20,02), fac(hex(8c)), line$(16%)             , ch(79),~
               at (21,02), fac(hex(8c)), line$(17%)             , ch(79),~
               at (22,02), fac(hex(8c)), line$(18%)             , ch(79),~
               at (23,02), fac(hex(8c)), line$(19%)             , ch(79),~
               at (24,02), fac(hex(8c)), line$(20%)             , ch(79),~
                                                                         ~
               keys(hex(00)),                                            ~
               key (keyhit%)

               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50170,         /* EMPLOYEE CODE    */~
                                    L50330,         /* CHECK NUMBER     */~
                                    L50560,         /* CHECK DATE       */~
                                    L50590,         /* START DATE       */~
                                    L50620,         /* ENDING DATE      */~
                                    L50670,         /* CASH ACCOUNT     */~
                                    L50720,         /* POST ACCRUALS?   */~
                                    L50760          /* POST LEDGER?     */
                     return

L50170: REM TEST DATA FOR EMPLOYEE CODE...
            call "GETEMPL" (#2, empcode$, name$, 0%, f1%(2%))
                if f1%(2%) <> 0% then L50230
            errormsg$ = "Employee Code Not In Personnel File"
            return

L50230:     call "READ100" (#1, empcode$, f1%(1%))
                if f1%(1%) = 1% then L50280
                     errormsg$="Employee Not Found In Payroll Master File"
                     return
L50280:     get #2, using L50290, stat$
L50290:         FMT CH(1)
            get #1, using L50294, department$
L50294:         FMT XX(91), CH(4)
            if stat$ <> "C" then errormsg$ = "Employee Is NOT Active"
            return

L50330: REM TEST DATA FOR CHECK NUMBER...
            checkonfile% = 0%
            if str(check$,,1%) <> "D" then L50362
                   convert str(check$,2%) to check%, data goto L50540
                   convert check% to str(check$,2%), pic(0000000)
                   goto L50370
L50362:     call "NUMTEST" (check$, 1,99999999, errormsg$, -0.001, 0)
                if errormsg$ <> " " then return
L50370:     readkey$ = str(empcode$,,12%) & str(check$)
            call "PLOWNEXT" (#6, readkey$, 20%, f1%(6%))
                if f1%(6%) = 0% then L50470
            checkonfile% = 1%
            checkmsg$(1%) = "This check has already been entered,"
            checkmsg$(2%) = "accrual and G/L adjustments will    "
            checkmsg$(3%) = "have to be done manually.           "
            get #6, using L50442, checkdate$, sdate$, edate$, temp$
L50442:         FMT XX(23), CH(6), CH(6), CH(6), POS(84), CH(8)
            call "DATEFMT" (checkdate$)
            call "DATFMTC" (sdate$)
            call "DATFMTC" (edate$)
            if str(check$,,1%) <> "D" or temp$ = " " then L50525
                errormsg$ = "Please Manage Check associated with this D"&~
                            "eposit Slip: " & temp$
                return
L50470:     call "PLOWALTS" (#6, str(readkey$,13%), 1%, 8%, f1%(6%))
                if f1%(6%) = 0% then L50525
            get #6, using L50500, temp$
L50500:         FMT CH(12)
            errormsg$ = "Check Number Already Assigned To: " & temp$
L50525
*          CALL "STRING" ADDR("LJ", CHECK$, 8%)
            return

L50540:         errormsg$ = "Invalid Deposit Slip Number"
                return

L50560: REM TEST DATA FOR CHECK DATE...
            call "DATEOK" (checkdate$, t3%, errormsg$)
            return

L50590: REM TEST DATA FOR PERIOD STARTING DATE...
            call "DATEOKC" (sdate$, u3%, errormsg$)
            return

L50620: REM TEST DATA FOR PERIOD ENDING DATE...
            call "DATEOKC" (edate$, t3%, errormsg$)
            if t3% < u3% and errormsg$ = " " then                        ~
                errormsg$ = "Period Ending Date MUST Be AFter Period St"&~
                            "arting Date"
            return

L50670: REM TEST DATA FOR CASH IN BANK ACCOUNT...
            if cashacct$ = " " then return
            call "GETCODE" (#5, cashacct$, cashdescr$, 1%, 0, f1%(5%))
                if f1%(5%) = 1% then return
            errormsg$ = "Cash In Bank Account Is NOT Valid"
            return

L50720: REM TEST DATA FOR POST EMPLOYEE ACCRUALS...
            if pos("YN" = postaccruals$) = 0% then errormsg$ =           ~
                              "Post Employee Accruals MUST Be 'Y' Or 'N'"
            return

L50760: REM TEST DATA FOR POST TO GENERAL LEDGER...
            if pos("YN" = postledger$) = 0 then errormsg$ =              ~
                              "Post To General Ledger MUST Be 'Y' Or 'N'"
            return

        REM *************************************************************~
            *   T E S T   D A T A   F O R   T A B U L A R   I N P U T   *~
            *                                                           *~
            * TESTS DATA ENTERED FOR EARNINGS.                          *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51110,         /* UNITS FIELD      */~
                                    L51190,         /* AMOUNT FIELD     */~
                                    L51290          /* ACCOUNT          */
                     return
L51110: REM TEST DATA FOR UNITS FIELD...
            if addunits$(c%) = " " then addunits$(c%) = "0"
            convert addunits$(c%) to amount, data goto L51160
            convert round(amount,4%) to addunits$(c%), pic(-####.####)
            return
L51160:         errormsg$ = "Invalid Entry For Units: " & addunits$(c%)
                return

L51190: REM TEST DATA FOR AMOUNT....
            if addamount$(c%) = " " then addamount$(c%) = "0"
            convert addamount$(c%) to amount, data goto L51250
            convert round(amount,2%) to addamount$(c%), pic(-######.##)
            convert addamount$(c%) to amount, data goto L51250
            if amount >= 0 then L51240
                if postaccruals$ <> "Y" then L51240
L51234:     u3% = 0%
            call "ASKUSER" (u3%, "*** NEGATIVE DEDUCTION ***",           ~
                 "Negative deduction amounts do NOT post to employee ac"&~
                 "cruals.", " ", "Press RETURN to acknowledge and conti"&~
                 "ue.")
            if u3% <> 0% then L51234
L51240:     return
L51250:         errormsg$ = "Invalid Entry For Amount: " & addamount$(c%)
                return


L51290: REM TEST DATA FOR ACCOUNT...
            call "GETCODE" (#5, edebit$(c%), infomsg$, 1%, 0, f1%(5%))
            if f1%(5%) = 0% then errormsg$ = "Undefined Account Number"
            return

        REM *************************************************************~
            *   T E S T   D A T A   F O R   T A B U L A R   I N P U T   *~
            *                                                           *~
            * TESTS DATA ENTERED FOR DEDUCTIONS.                        *~
            *************************************************************

            deffn'154(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52120,         /* UNITS SUBJECT    */~
                                    L52191,         /* DOLLARS SUBJECT  */~
                                    L52200,         /* AMOUNT FIELD     */~
                                    L52300          /* ACCOUNT          */
                     return

L52120: REM TEST DATA FOR UNITS...
            if unitsub$(c%) = " " then unitsub$(c%) = "0"
            convert unitsub$(c%) to amount, data goto L52170
            convert round(amount,4%) to unitsub$(c%), pic(-####.####)
            return
L52170:         errormsg$ = "Invalid Entry For Units: " & unitsub$(c%)
                return

L52191: REM TEST DATA FOR DOLLARS...
            if subject$(c%) = " " then subject$(c%) = "0"
            convert subject$(c%) to amount, data goto L52196
            convert round(amount,2%) to subject$(c%), pic(-######.##)
            return
L52196:         errormsg$ = "Invalid Entry For Subject: " & subject$(c%)
                return

L52200: REM TEST DATA FOR AMOUNT...
            if dedamount$(c%) = " " then dedamount$(c%) = "0"
            convert dedamount$(c%) to amount, data goto L52260
            convert round(amount,2%) to dedamount$(c%), pic(-######.##)
            convert dedamount$(c%) to amount, data goto L52260
            return
L52260:         errormsg$ = "Invalid Entry For Amount: " & dedamount$(c%)
                return

L52300: REM TEST DATA FOR ACCOUNT...
            call "GETCODE" (#5, dcredit$(c%), infomsg$, 1%, 0, f1%(5%))
            if f1%(5%) = 0% then errormsg$ = "Undedfined Account Number"
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

            close printer
            if gl_batch_open$ = "Y" then                                 ~
                call "JNLCLOSE" (moduleno$, jnlid$, pstseq%, returncode%)
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
