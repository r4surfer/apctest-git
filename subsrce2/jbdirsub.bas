        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB   DDDD   IIIII  RRRR    SSS   U   U  BBBB    *~
            *    J    B   B  D   D    I    R   R  S      U   U  B   B   *~
            *    J    BBBB   D   D    I    RRRR    SSS   U   U  BBBB    *~
            *  J J    B   B  D   D    I    R   R      S  U   U  B   B   *~
            *   J     BBBB   DDDD   IIIII  R   R   SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBDIRSUB - Direct Input of Labor, Work Center, or Misc.   *~
            *            Costs to Jobs.                                 *~
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
            * 06/17/87 ! Original - From WCDINSUB                 ! KAB *~
            * 03/03/88 ! Dissallow Closed Jobs                    ! HES *~
            * 09/04/90 ! G/L Export file modifications.           ! RAC *~
            * 04/13/92 ! PRR 11703. Added call to JBQJOB.         ! JDH *~
            *          ! PRR 10667. Updte Total when Fxd Cst chngd!     *~
            * 07/24/92 ! PRR 12545. Fixed variables in exit call  ! JDH *~
            *          !   to JBJNLCLS for Jnl MJR.  Thanks Tony. !     *~
            * 04/09/93 ! Added Core Misc. transactions.           ! KAB *~
            * 05/28/93 ! Added Core and Regular Transactions      ! JBK *~
            * 07/20/93 ! PRR 12992. No blank allowed for tran type! JDH *~
            * 07/16/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

            sub "JBDIRSUB" (pmod1$, pjnl1$,        /* Direct  Mod, Jnl */~
                                                   /* (03, MVA)        */~
                                                   /* Blanks OK.       */~
                            pmod2$, pjnl2$,        /* Job/Job Mod, Jnl */~
                                                   /* (03, MJR)        */~
                                                   /* Blanks OK.       */~
                            jobnr$, from$,         /* Control (JBCLOSE)*/~
                                                   /* Blanks OK.       */~
                            #1,                    /* SYSFILE2         */~
                            #2,                    /* JBMASTR2         */~
                            #3,                    /* HNYMASTR         */~
                            #4,                    /* GLMAIN           */~
                            #5,                    /* USERINFO         */~
                            #6,                    /* WCMASTR          */~
                            #7)                    /* GENCODES         */~

        dim                                                              ~
            acctprompt$7,                /* Account Prompt             */~
            activity$4,                  /* Activity                   */~
            activitydescr$32,            /* Activity                   */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bomcost$96,                  /* BOM Costs                  */~
            bomcredit$96,                /* BOM Credits                */~
            ce_acct$(12)12,              /* Closing Entry Accounts     */~
            ce_acct_type$1,              /* Closing Entry Account Type */~
            ce_amt$(12)12,               /* Closing Entry Amounts      */~
            cfac$(20)1,                  /* Close Entry FAC's          */~
            close_hd$29,                 /* Close Entry Heading        */~
            closingcr$96,                /* Closing Entry Credits      */~
            cost(12),                    /* Costs                      */~
            cost1(12),                   /* Costs                      */~
            costtype$1,                  /* Part Costing Method        */~
            coremsc$96,                  /* Core Misc or Shadow Values */~
            coredb$96,                   /* Core Debits                */~
            corecr$96,                   /* Core Credits               */~
            corecls$96,                  /* Core Closing Values        */~
            corefg$96,                   /* Core Finished Goods Values */~
            coreflag_var$9,              /* Core Flags Variance Account*/~
            core_varacct$9,              /* Core Variance Account      */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dacct$12,                    /* Disp Acct (WIP or COREWIP) */~
            dacctd$32,                   /* Disp Acct (WIP or COREWIP) */~
            dacctp$30,                   /* Disp Acct (WIP or COREWIP) */~
            def_adjacct$9,               /* Default Inv Adjustment Acct*/~
            def_varacct$(12)12,          /* Default Inv Variance Accts */~
            dfcore_wip$9,                /* Default Core WIP           */~
            earntype$12,                 /* Earnings Type              */~
            earntypedescr$32,            /* Earnings Type              */~
            edtmessage$79,               /* Edit screen message        */~
            efac$(2)1,                   /* Close Entry FAC's          */~
            emp$12, empdescr$32, empstat$1, /* Employee data           */~
            errormsg$79,                 /* Error message              */~
            tmpacct$9,                   /* Temp. Buffer for exp acct  */~
            expacct$12,                  /* Expense Account            */~
            expacctdescr$32,             /* Expense Account            */~
            freetext$40,                 /* Transaction Text           */~
            fromjob$8, cfromjob$,        /* Contra-Job (instead of EXP)*/~
            hfac$1,                      /* Close Sub-Heading FAC      */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            jbcost$96,                   /* Job Costs                  */~
            jbcorewip$12,                /* Core WIP Account           */~
            jbcorewipd$32,               /* Core WIP Account           */~
            jbnumber$8,                  /* Job Number                 */~
            jbnumberdescr$32,            /* Job Number                 */~
            jbtotal(1),                  /* Job Total                  */~
            jnlid$3,                     /* Journal Id.                */~
            jnlid1$3,                    /* Journal Id. - Direct       */~
            jnlid2$3,                    /* Journal Id. - Job to Job   */~
            jobnr$8,                     /* Passed Job Number          */~
            labr$10,                     /* Calculated Cost            */~
            lclass$4,                    /* Labor Class                */~
            lclassdescr$32,              /* Labor Class                */~
            lcost$96,                    /* Labor Costs                */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line$60,                     /* Second Line of Sub Screen  */~
            line2$79,                    /* Second Line of Screen Headr*/~
            lvlcredit$96,                /* Level Credits              */~
            modno$2,                     /* Module Number              */~
            modno1$2,                    /* Module Number - direct     */~
            modno2$2,                    /* Module Number - Job to Job */~
            msccost$96,                  /* Job Misc Costs             */~
            old_ce_acct_type$1,          /* Closing Entry Account Type */~
            ovhd$10,                     /* Fixed Cost                 */~
            ocost$96,                    /* Fixed Cost                 */~
            part$25,                     /* Job Building Part          */~
            partdescr$34,                /* Job Building Part          */~
            partqty$10,                  /* Job Building Part Quantity */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$20,                      /* PF 4  Screen Literal       */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            postdate$6,                  /* Posting Date Passed        */~
            post$8,                      /* Posting Date Formatted     */~
            readkey$60,                  /* Read/validate Earn type    */~
            rtecost$96,                  /* Job Misc Costs             */~
            scrn$16,                     /* Screen Title               */~
            setid$8,                     /* Standard Cost Set ID       */~
            total$10,                    /* Total Cost                 */~
            totcost(12),                 /* Job Total Costs            */~
            totcredit(12),               /* Job Total Credits          */~
            trantypedescr$30,            /* Transaction Type Prompt    */~
            transtypecodes$(6)7,         /* Valid Transaction Codes    */~
            transtypeerrmsg$(6)20,       /* Transaction Codes Error Msg*/~
            transtypeinpmsg$(6)79,       /* Trans Codes Input Messages */~
            transtypeprompt$(6)13,       /* Trans Codes Prompt Msgs    */~
            trdate$8,                    /* Transaction Date           */~
            type$1,                      /* Transaction Type (L,W,D)   */~
            unitrate$10,                 /* Earnings Rate              */~
            units$10,                    /* Labor Hours                */~
            userid$3,                    /* Current User Id            */~
            wc$4,                        /* Work Center                */~
            wcdescr$32,                  /* Work Center                */~
            wipacct$12,                  /* WIP Account                */~
            wipdescr$32,                 /* WIP Account                */~
            wrk(1,12),                   /* Work Variable              */~
            work$56                      /* Work Variable              */~

        dim f1%(32)                      /* = 1 IF READ WAS SUCCESSFUL */~

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
            * # 1 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 2 ! JBMASTR2 ! Production job master file               *~
            * # 3 ! HNYMASTR ! Inventory Master File                    *~
            * # 4 ! GLMAIN   ! General Ledger CHart Of Accounts File.   *~
            * # 5 ! USERINFO ! Users Default Information File           *~
            * # 6 ! WCMASTR  ! Workcenter Master File                   *~
            * # 7 ! GENCODES ! General Codes File                       *~
            * # 8 ! PERMASTR ! PERSONNEL MASTER FILE                    *~
            * # 9 ! EMPMASTR ! EMPLOYEE FILE MASTER RECORDS.            *~
            * #10 ! EMPEARN1 ! EMPLOYEE EARNINGS FILE                   *~
            * #11 ! PIPOUT   ! Pip quants due out                       *~
            * #12 ! JBMATER2 ! Job material ledger                      *~
            * #13 ! JBVALUE2 ! Job Value Ledger                         *~
            * #14 ! JBMASTRC ! Job Master Core Appendix                 *~
            * #15 ! COREXREF ! Core Part Cross Reference File           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #8,  "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 950,                                   ~
                        keypos = 39, keylen = 12,                        ~
                        alt key  1, keypos =  28, keylen = 23,           ~
                            key  2, keypos =   2, keylen = 49,           ~
                            key  3, keypos =   1, keylen = 50

            select # 9, "EMPMASTR",                                      ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 136,                                    ~
                       keypos = 1, keylen = 12,                          ~
                       alt key  1, keypos = 70, keylen =  1, dup

            select #10, "EMPEARN1",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos = 1, keylen = 15,                         ~
                        alt key 1, keypos = 16, keylen = 28

           select #11, "PIPOUT",                                         ~
                        varc, indexed, recsize =  64,                    ~
                        keypos =  1, keylen =  56,                       ~
                        alt key   1, keypos = 20, keylen = 37

            select #12, "JBMATER2",                                      ~
                        varc, indexed, recsize = 400,                    ~
                        keypos = 1, keylen = 22,                         ~
                        alt key  1, keypos = 23, keylen = 48

           select #13, "JBVALUE2",                                       ~
                        varc, indexed, recsize = 300,                    ~
                        keypos =  1, keylen = 23

           select #14, "JBMASTRC",                                       ~
                        varc, indexed, recsize = 600,                    ~
                        keypos =  1, keylen =  8

           select #15, "COREXREF",                                       ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =   26, keylen =  50,                     ~
                        alt key  1, keypos =    1, keylen =  50,         ~
                            key  2, keypos =   76, keylen =  25, dup

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            if open% <> 0% then goto L09120
                call "OPENCHCK" (# 1, open%, f201%, 0%, " ")
                call "OPENCHCK" (# 8, open%, f201%, 0%, " ")
                call "OPENCHCK" (# 9, open%, f201%, 0%, " ")
                call "OPENCHCK" (#10, open%, f201%, 0%, " ")
                call "OPENCHCK" (#11, open%, f201%, 0%, " ")
                call "OPENCHCK" (#12, open%, f201%, 0%, " ")
                call "OPENCHCK" (#13, open%, f201%, 0%, " ")
                call "OPENCHCK" (#14, open%, f201%, 0%, " ")
                call "OPENCHCK" (#15, open%, f201%, 0%, " ")
                open% = 1%

L09120:     modno1$ = pmod1$ : jnlid1$ = pjnl1$
            modno2$ = pmod2$ : jnlid2$ = pjnl2$

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            pstseq%, pstseq1%, pstseq2%, postflag1%, postflag2% = 0%

            call "READ100" (#5, userid$, f1%(5))
               if f1%(5) <> 0% then L09300
                  u3% = 0%
                  call "ASKUSER" (u3%, "* * * S O R R Y * * *",          ~
                  "You are NOT listed as a valid user in this Data Base",~
                  " ", "Press RETURN to Exit.")
                  goto L65000
L09300:     get #5, using L09310, postdate$
L09310:         FMT POS(34), CH(6)
            call "WHICHPER" (#1, postdate$, whichmonth%)
                if whichmonth% <> 0% then L09390
                  u3% = 0%
                  call "ASKUSER" (u3%, "* * * S O R R Y * * *",          ~
                  "Your Posting Date is NOT valid.", " ",                ~
                  "Press RETURN to Exit.")
                  goto L65000
L09390:     post$ = postdate$
            call "DATEFMT" (post$)

            plowkey$ = "SWITCHS.COR"
            call "READ100" (#1, plowkey$, corebank%)     /* SYSFILE2 */
               if corebank% = 0% then L09495
            get #1 using L09460, type$
L09460:         FMT POS(135), CH(1)
            if type$ <> "Y" then corebank% = 0%
            get #1 using L09480, coreflag_var$, dfcore_wip$
L09480:         FMT POS(30), CH(9), POS(119), CH(9)

L09495:     buckets% = 1%
            call "STCSETID" (buckets%, #1, setid$, " ")

            transtypecodes$(1%) = "LWD"
            transtypecodes$(2%) = "LWD"   & hex(ff) & "C"
            transtypecodes$(3%) = "LWD"   & hex(ff) & "C" & hex(ff) & "F"
            transtypecodes$(4%) = "LWDA"
            transtypecodes$(5%) = "LWDAC"
            transtypecodes$(6%) = "LWDAC" & hex(ff) & "F"

            transtypeprompt$(1%) = "(L,W,D)"
            transtypeprompt$(2%) = "(L,W,D,C)"
            transtypeprompt$(3%) = "(L,W,D,C,F)"
            transtypeprompt$(4%) = "(L,W,D,A)"
            transtypeprompt$(5%) = "(L,W,D,A,C)"
            transtypeprompt$(6%) = "(L,W,D,A,C,F)"

            transtypeinpmsg$(1%) = "Enter Transaction Type. L = Labor, "&~
                                   "W = Work Center, D = Direct Misc."
            transtypeinpmsg$(2%) = "Enter Transaction Type. L = Labor, "&~
                                   "W = Work Center, D = Direct Misc, C"&~
                                   " = Core."
            transtypeinpmsg$(3%) = "Enter Trans Type. L=Labor, W=Work C"&~
                                   "enter, D=Direct Misc, C=Core, F=Cor"&~
                                   "e Cls."
            transtypeinpmsg$(4%) = "Enter Transaction Type. L = Labor, "&~
                                   "W = Work Center, D = Direct Misc, A"&~
                                   " = Close."
            transtypeinpmsg$(5%) = "Enter Trans Type. L=Labor, W=Work C"&~
                                   "enter, D=Direct Misc, A=Close, C=Co"&~
                                   "re."
            transtypeinpmsg$(6%) = "Enter Trans Type. L=Labr, W=Wrk Cnt"&~
                                   "r, D=Dir Misc, A=Cls, C=Core, F=Cor"&~
                                   "e Cls."

            transtypeerrmsg$(1%) = "L, W, or D:"
            transtypeerrmsg$(2%) = "L, W, D, or C:"
            transtypeerrmsg$(3%) = "L, W, D, C, or F:"
            transtypeerrmsg$(4%) = "L, W, D, or A:"
            transtypeerrmsg$(5%) = "L, W, D, A, or C:"
            transtypeerrmsg$(6%) = "L, W, D, A, C, or F:"

            close_hd$ = "     Account           Amount"
            acctprompt$ = "Account"

            closepass% = 0%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$ = " ":pf16$ = "(16)Exit Program"
            gosub L29000

            for fieldnr% = 1 to  4
                if fieldnr% > 1 then pf4$ = "(4)Previous Field"
L10120:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10240
L10140:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10220
L10170:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10140
                         if fieldnr% = 1% then L10120
                         goto L10170
L10220:               if keyhit% = 16  then exit_program
                      if keyhit% <>  0 then       L10140
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10140
            next fieldnr%
            pf16$ = " "  :  startfield% = 4%
            on type% goto inputlabor, inputwc, inputdirect, inputclosing,~
                          inputdirect, , inputclosing

        inputlabor
            for fieldnr% = 4 to 15
                if fieldnr% > 4 then pf4$ = "(4)Previous Field"
L10300:         gosub'052(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10420
L10320:         gosub'102(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10410
L10350:                  fieldnr% = max(4%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10320
                         if fieldnr% = 4% then L10300
                         goto L10350
L10410:               if keyhit% <>  0 then       L10320
L10420:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10320
            next fieldnr%
            goto editlabor

        inputwc
            for fieldnr% = 4 to 12
                if fieldnr% > 4 then pf4$ = "(4)Previous Field"
L10480:         gosub'053(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10600
L10500:         gosub'103(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10590
L10530:                  fieldnr% = max(4%, fieldnr% - 1%)
                         gosub'053(fieldnr%)
                         if enabled% = 1% then L10500
                         if fieldnr% = 4% then L10480
                         goto L10530
L10590:               if keyhit% <>  0 then       L10500
L10600:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10500
            next fieldnr%
            goto editwc

        inputdirect
            for fieldnr% = 4 to  7
                if fieldnr% > 4 then pf4$ = "(4)Previous Field"
L10660:         gosub'054(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10780
L10680:         gosub'104(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10770
L10710:                  fieldnr% = max(4%, fieldnr% - 1%)
                         gosub'054(fieldnr%)
                         if enabled% = 1% then L10680
                         if fieldnr% = 4% then L10660
                         goto L10710
L10770:               if keyhit% <>  0 then       L10680
L10780:         gosub'154(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10680
            next fieldnr%
            goto editdirect

        inputclosing
            for fieldnr% = startfield% to  8%
L10850:         gosub'055(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0% then L10955
L10870:         gosub'105(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10950
L10900:                  fieldnr% = max(4%, fieldnr% - 1%)
                         gosub'055(fieldnr%)
                         if enabled% = 1% then L10870
                         if fieldnr% = 4% then L10850
                         goto L10900
L10950:               if keyhit% <>  0% then       L10870
L10955:         gosub'155(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10870
            next fieldnr%
            goto editclosing

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editlabor
            pf4$  = " "
            pf16$ = "(16)Save Data"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'102(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editlabor
L11430:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 4 or fieldnr% > 15 then editlabor
            if fieldnr% = lastfieldnr% then editlabor
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       L11520
                  pf4$, pf16$ = " "
L11490:     gosub'102(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11490
L11520:     gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11490
                  lastfieldnr% = fieldnr%
            goto L11430

        editwc
            pf4$  = " "
            pf16$ = "(16)Save Data"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'103(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editwc
L11690:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 4 or fieldnr% > 12 then editwc
            if fieldnr% = lastfieldnr% then editwc
            gosub'053(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       L11780
                  pf4$, pf16$ = " "
L11750:     gosub'103(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11750
L11780:     gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11750
                  lastfieldnr% = fieldnr%
            goto L11690

        editdirect
            pf4$  = " "
            pf16$ = "(16)Save Data"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'104(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editdirect
L11940:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 4 or fieldnr% >  7 then editdirect
            if fieldnr% = lastfieldnr% then editdirect
            gosub'054(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editdirect
                  pf4$, pf16$ = " "
L12000:     gosub'104(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12000
            gosub'154(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12000
                  lastfieldnr% = fieldnr%
            goto L11940

        editclosing
            pf4$  = " "
            pf16$ = "(16)Save Data"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'105(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editclosing
L12170:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 4% or fieldnr% > 14% or fieldnr% = 7%          ~
                                                         then editclosing
            if fieldnr% >= 4% and fieldnr% < 6% then L12360
            if fieldnr% <> 14% then L12240
                fieldnr% = 8%
                goto L12360
L12240:     if fieldnr% <> 6% then L12300
                if cursor%(2%) < 41% then L12360
                     if cls_type% = 2% or cls_type% = 6% then editclosing
                          fieldnr% = 7%
                          efac$(2%) = hex(81)
                          goto L12360
L12300:     if cls_type% <> 2% and cls_type% <> 6% then editclosing
            col% = int(cursor%(2%) / 40%) * 6%
            fieldnr% = fieldnr% - 7% + col%
            if fieldnr% > buckets% then editclosing
            init (hex(8c))  cfac$()
            str(cfac$(),fieldnr%, 1%) = hex(81)
            fieldnr% = 7%
L12360:     if fieldnr% = lastfieldnr% then editclosing
            gosub'055(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editclosing
                  pf4$, pf16$ = " "
L12400:     gosub'105(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12400
            gosub'155(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12400
                  lastfieldnr% = fieldnr%
            goto L12170

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if type% <> 4% and type% <> 7% then L19100
                gosub dataput2
                goto L19990
L19100:     gosub dataput
L19990:     goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Job Number         */    ~
                              L20200,         /* x                  */    ~
                              L20200,         /* WIP Account        */    ~
                              L20400          /* Transaction Type   */
            return
L20100: REM Def/Enable Job Number                  JBNUMBER$
            inpmessage$ = "Enter Job Number"
            if from$ <> "C" then L20170
               enabled%  = closepass%
L20170:     jbnumber$ = jobnr$
            closepass% = 1%
            return
L20200: REM Def/Enable Memo Area
            enabled% = 0%
            return
L20400: REM Def/Enable Transaction Type (L,W,D)    TYPE$
*          Maybe types 'C', or 'A' or 'F' also
            inpmessage$ = transtypeinpmsg$(ttp_index%)
            return




        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L24050,             /* Job Number         */~
                              L24050,             /* x                  */~
                              L24050,             /* WIP Account        */~
                              L24100,             /* Transaction Date   */~
                              L24150,             /* Work Center        */~
                              L24200,             /* Activity           */~
                              L24250,             /* Employee           */~
                              L24300,             /* Earnings Type      */~
                              L24350,             /* Labor Class        */~
                              L24550,             /* Fixed Cost         */~
                              L24400,             /* Earnings Rate      */~
                              L24450,             /* Labor Hours        */~
                              L24500,             /* Net   Cost         */~
                              L24600,             /* Expense Account    */~
                              L24650              /* Transaction Text   */~

            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  3  of Input. *~
            *************************************************************

        deffn'053(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L24050,             /* Job Number         */~
                              L24050,             /* x                  */~
                              L24050,             /* WIP Account        */~
                              L24100,             /* Transaction Date   */~
                              L24150,             /* Work Center        */~
                              L24200,             /* Activity           */~
                              L24550,             /* Fixed Cost         */~
                              L24400,             /* Work Center Rate   */~
                              L24450,             /* Work Center Hours  */~
                              L24500,             /* Net   Cost         */~
                              L24600,             /* Expense Account    */~
                              L24650              /* Transaction Text   */~

            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   4     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  4  of Input. *~
            *************************************************************

        deffn'054(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L24050,             /* Job Number         */~
                              L24050,             /* x                  */~
                              L24050,             /* WIP Account        */~
                              L24100,             /* Transaction Date   */~
                              L24550,             /* Total Cost         */~
                              L24600,             /* Expense Account    */~
                              L24650              /* Transaction Text   */~

            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   5     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  5  of Input. *~
            *************************************************************

        deffn'055(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L24050,             /* Job Number         */~
                              L24050,             /* x                  */~
                              L24050,             /* WIP Account        */~
                              L24100,             /* Transaction Date   */~
                              L24750,             /* CE Accounts        */~
                              L26290,             /* Closing Costs      */~
                              L27000,             /* CE Accounts & Amts */~
                              L24650              /* Transaction Text   */~

            return

        REM *************************************************************~
            * Default / Enable  Common Code                             *~
            *************************************************************

L24050: REM Def/Enable Shouldn't Happen
            enabled% = 0%
            return

L24100: REM Def/Enable Transaction Date            TRDATE$
            if trdate$ = " " or trdate$ = blankdate$ then trdate$ = post$
            inpmessage$ = "Enter Transaction Date."
            return

L24150: REM Def/Enable Work Center                 WC$
            inpmessage$ = "Enter Work Center Code."
            return

L24200: REM Def/Enable Activity                    ACTIVITY$
            inpmessage$ = "Enter Activity Code."
            return

L24250: REM Def/Enable Employee                    EMP$
            inpmessage$ = "Enter Employee Code or '?' to see list."
            return
L24300: REM Def/Enable Earnings Type               EARNTYPE$
            inpmessage$ = "Enter Earnings Type or '?' to see list."
            return

L24350: REM Def/Enable Labor Class                 LCLASS$
            inpmessage$ = "Enter Labor Class."
            return

L24400: REM Def/Enable Rate                        UNITRATE$
            inpmessage$ = "Enter Unit Rate. (Cost Per Unit)"
            return

L24450: REM Def/Enable Labor Hours                 UNITS$
            inpmessage$ = "Enter Hours or Units Worked."
            return

L24500: REM Def/Enable Net Cost                    LABR$
            if keyhit% = 4% then L24530
            if labr <> 0 then L24510
               init (hex(00)) lcost$
               goto L24530
L24510:     labr = partqty
            line$ = "Distribute Derived Costs to Job:" & jbnumber$
               get rtecost$ using L24516, cost()
L24516:            FMT 12*PD(14,4)
               get ocost$   using L24516, cost1()
               mat cost = cost + cost1
               call "PACKZERO" (cost(), jbcost$)
               jbtotal(1) = round(rtecost + ovhd, 4%)
            call "JBCDIST" ("N", 2%, part$, partdescr$, line$, #1,       ~
                            lcost$, labr$, labr, jbcost$, jbtotal(1))
L24530:     enabled% = 0%
            return

L24550: REM Def/Enable Fixed Cost                  OVHD$
            if keyhit% = 4% then L24594
            ovhd = partqty
            line$ = "Distribute Fixed Costs to Job:" & jbnumber$
            if type% = 3% or type% = 5% then L24588
               std% = 2%
               get rtecost$ using L24564, cost()
L24564:            FMT 12*PD(14,4)
               get lcost$   using L24564, cost1()
               mat cost = cost + cost1
               call "PACKZERO" (cost(), jbcost$)
               jbtotal(1) = round(rtecost + labr, 4%)
                 goto L24590
L24588:        jbcost$ = msccost$ : jbtotal(1) = msccost : std% = 3%
L24590:     call "JBCDIST" (" ", std%, part$, partdescr$, line$, #1,     ~
                            ocost$, ovhd$, ovhd, jbcost$, jbtotal(1))
L24594:     enabled% = 0%
            return

L24600: REM Def/Enable Expense Account             EXPACCT$
            inpmessage$ = "Enter Expense Account - OR - 'From' Job to Adj~
        ~ust Costs."
            return

L24650: REM Def/Enable Transaction Text            FREETEXT$
            inpmessage$ = "Enter Transaction Text."
            if freetext$ <> " " and startfield% = 5% then enabled% = 0%
            if freetext$ <> " " then return
            if type% = 4% or type% = 7% then L24725
            if fromjob$ = " " then return
               freetext$ = "Val. moved Job to Job: xxxxxxxx/xxxxxxxx"
               str(freetext$,24,8) = jbnumber$
               str(freetext$,33,8) = fromjob$
               return
L24725:     if edit% <> 0% then return
                if type% = 4% then freetext$ = "Closing Entry For Job: "&~
                                                               jbnumber$ ~
                     else freetext$ = "Core Closing Entry For Job: " &   ~
                                                               jbnumber$
                enabled% = 0%
                return
L24750: REM Def/Enable Closing Entry Acct Structure   CE_ACCT_TYPE$
                inpmessage$ = "Enter 'A' for Inventory Adjustment or 'V' ~
        ~for Inventory Variance Accounts."
                old_ce_acct_type$ = ce_acct_type$
                if edit% <> 0% then return

*        Input mode, default the closing entry account structure
            if cls_type% = 0% then return
            ce_acct_type$ = "A"               /* Adjustment Account */
            if mod(cls_type%, 2%) = 0% then ce_acct_type$ = "V"

            init (hex(9c))  hfac$, cfac$()       /* For 'A' types */
            str(efac$()) = hex(8c) & hex(9c)

            if cls_type% <> 2% and cls_type% <> 6% then L24930
                init (hex(8c))  cfac$()          /* For 'V' types */
                init (hex(ac))  hfac$
                init (hex(9c))  efac$()

L24930:     enabled% = 0%  :  old_ce_acct_type$ = ce_acct_type$
            return

L26290: REM Def/Enable Closing Costs               OVHD$
            if keyhit% = 4% then L26740
            ovhd = partqty
            line$ = "Distribute Closing Costs to Job:" & jbnumber$
            if type% = 7% then line$ = "Distribute Core Closing Costs " &~
                                       "to Job:" & jbnumber$
            if edit% <> 0% then L26650

*        Calculate Closing Entries Values for first time
*        Cost side first
            if type% = 7% then L26530
            call "MXSTPGT" addr(rtecost$, cost (1%), 12%, 4%)
            call "MXSTPGT" addr(msccost$, cost1(1%), 12%, 4%)
            mat totcost = cost + cost1
            call "MXSTPGT" addr(bomcost$, cost (1%), 12%, 4%)
            mat totcost = totcost + cost
            if corebank% = 0% then L26420
                if coremastrc% <> 1% then L26420
                     call "MXSTPGT" addr(corefg$, cost1(1%), 12%, 4%)
                     mat totcost = totcost + cost1
L26420:     mat wrk = con  :  mat jbtotal = wrk * totcost
            totcost = jbtotal(1%)
*        Credit side second
            call "MXSTPGT" addr(bomcredit$, cost (1%), 12%, 4%)
            call "MXSTPGT" addr(lvlcredit$, cost1(1%), 12%, 4%)
            mat totcredit = cost + cost1
            call "MXSTPGT" addr(closingcr$, cost (1%), 12%, 4%)
            mat totcredit = totcredit + cost
            mat wrk = con  :  mat jbtotal = wrk * totcredit
            totcredit = jbtotal(1%)
            goto L26650

L26530
*        Calculate Core Closing Entries Values for First Time
*        Cost Side First
            call "MXSTPGT" addr(coredb$,  cost (1%), 12%, 4%)
            call "MXSTPGT" addr(coremsc$, cost1(1%), 12%, 4%)
            mat totcost = cost + cost1
            mat wrk = con  :  mat jbtotal = wrk * totcost
            totcost = jbtotal(1%)
*        Credit Side Second
            call "MXSTPGT" addr(corecr$,  cost (1%), 12%, 4%)
            call "MXSTPGT" addr(corecls$, cost1(1%), 12%, 4%)
            mat totcredit = cost + cost1
            call "MXSTPGT" addr(corefg$,  cost (1%), 12%, 4%)
            mat totcredit = totcredit + cost
            mat wrk = con  :  mat jbtotal = wrk * totcredit
            totcredit = jbtotal(1%)


L26650
*        Calculuate Net Closing Values
            std% = 3%
            mat cost = totcost - totcredit
            call "PACKZERO" (cost(), jbcost$)
            jbtotal(1%) = round(totcost - totcredit, 4)
            if pos(ocost$<>hex(00)) = 0% then ocost$ = jbcost$
            if ovhd$ = " " then call "CONVERT" (jbtotal(1%), 2.4, ovhd$)
            if edit% = 0% then L26740
            if cls_type% = 2% or cls_type% = 6% then L26740
            call "JBCDIST" (" ", std%, part$, partdescr$, line$, #1,     ~
                            ocost$, ovhd$, ovhd, jbcost$, jbtotal(1))
L26740:     enabled% = 0%
            return


L27000: REM Def/Enable Closing Entry Accts and Amts   CE_ACCT$
            inpmessage$ = "Enter Account Number for Closing Entry."
            if cls_type% <> 2% and cls_type% <> 6% then L27050
                inpmessage$ = "Enter Account Number and Amount for Closin~
        ~g Entries."
L27050:     if edit% <> 0% then return
            if cls_type% = 0% then return

            if cls_type% > 1% then L27110
                expacct$ = def_adjacct$
                goto L27160
L27110:     if cls_type% = 2% then L27240
            if cls_type% > 5% then L27240

            expacct$ = core_varacct$

L27160:     init (" ")  ce_acct$(), ce_amt$()
            mat cost = zer
            call "GLFMT" (expacct$)
            init (hex(9c))  cfac$()
            init (hex(8c))  efac$()
            if edit% = 0% then enabled% = 0%
            return

L27240
*        Variance type entry - Closing
            init (hex(9c)) efac$()
            if pos(str(ce_acct$()) <> " ") > 0% then L27380
                mat ce_acct$ = def_varacct$
                call "MXSTPGT" addr(ocost$, cost(1%), 12%, 4%)
                for i% = 1% to 12%
                     if ce_acct$(i%) <> " " then                         ~
                                            call "GLFMT" (ce_acct$(i%))
                     if cost(i%) <> 0 then call "CONVERT" (cost(i%), 2.4,~
                                                           ce_amt$(i%))
                     if cost(i%) = 0 and ce_acct$(i%) <> " "then         ~
                              call "CONVERT" (cost(i%), 2.4, ce_amt$(i%))
                next i%

L27380:     if edit% = 0% then enabled% = 0%
            if edit% = 0% then init (hex(8c))  cfac$()
            if edit% = 0% then init (hex(ac))  hfac$
            return

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, wipacct$, type$,           ~
                      trdate$, wc$, activity$, emp$, earntype$, lclass$, ~
                      unitrate$, units$, ovhd$, total$, expacct$,        ~
                      freetext$, earntypedescr$, empdescr$, wipdescr$,   ~
                      expacctdescr$, wcdescr$, partdescr$,               ~
                      lclassdescr$, activitydescr$, jbnumberdescr$,      ~
                      part$, partqty$, labr$, fromjob$, ce_acct$(),      ~
                      ce_acct_type$, ce_amt$(), core_varacct$, costtype$,~
                      def_adjacct$, def_varacct$(), old_ce_acct_type$,   ~
                      cfromjob$, jbcorewip$, jbcorewipd$

            units, unitrate, labr, ovhd, total=0
            edit%, type%, cls_type% = 0%

            init (hex(00)) ocost$, lcost$

            init (hex(9c))  cfac$(), efac$(), hfac$

            trantypedescr$ = "Transaction Type " & transtypeprompt$(1%)

            call "JBINUSE" (" ", 1%)

            return

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
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload

                get #2, using L30090, jbnumberdescr$, part$, partqty,     ~
                        qtycomp, jbcorewip$, wipacct$,                   ~
                        bomcost$, rtecost$, msccost$,                    ~
                        bomcredit$, lvlcredit$, temp1, closingcr$
L30090:              FMT XX(8), CH(30), XX(19), CH(25), 2*PD(14,4),      ~
                         CH(9), XX(51), CH(9), XX(72), 3*CH(96), XX(8),  ~
                         2*CH(96), XX(419), PD(14,4), CH(96)
                call "CONVERT" (partqty, 2.2, partqty$)
                call "GLFMT" (wipacct$)
                if jbcorewip$ = " " then gosub set_jb_core_wip
                call "GLFMT" (jbcorewip$)
                call "GETCODE" (#4, jbcorewip$, jbcorewipd$, 1%, 99,     ~
                               f1%(4))
                get rtecost$ using L30140, cost()
L30140:             FMT 12*PD(14,4)
                mat wrk = con: mat jbtotal = wrk * cost
                rtecost = jbtotal(1)
                get msccost$ using L30140, cost()
                mat wrk = con: mat jbtotal = wrk * cost
                msccost = jbtotal(1)
                if temp1 > 9e9 then init(hex(00)) closingcr$

*        Test for Core Data for the job
            coremastrc% = 0%
            if corebank% = 0% then L30350
            coredb, corecr, corecls, corefg = 0
            init (hex(00))  coredb$, corecr$, corecls$, corefg$, coremsc$

            call "READ100" (#14, jbnumber$, coremastrc%)
                if coremastrc% = 0% then L30350
            get #14 using L30310, coredb,  coredb$, coremsc$, corecr,     ~
                                 corecr$, corecls, corecls$, corefg,     ~
                                 corefg$
L30310:         FMT POS(9), PD(14,4), CH(96), CH(96), PD(14,4), CH(96),  ~
                    PD(14,4), CH(96), PD(14,4), CH(96)

*        Determine what kind of transactions can be processed
L30350:     ttp_index% = 1%
            if partqty = qtycomp then L30450
                if from$ = "C" then L30400
                     if corebank% = 1% then ttp_index% = 2%
                     goto L30530
L30400:         if corebank% = 0% then L30530
                     if coremastrc% = 1% then ttp_index% = 3% else       ~
                          ttp_index% = 2%
                     goto L30530

L30450:     if from$ = "C" then L30480
                if corebank% = 1% then ttp_index% = 2%
                goto L30530
L30480:     if corebank% <> 0% then L30510
                ttp_index% = 4%
                goto L30530
L30510:     if coremastrc% = 0% then ttp_index% = 5% else ttp_index% = 6%

L30530:     trantypedescr$ = "Transaction Type " &                       ~
                                              transtypeprompt$(ttp_index%)
            return

        set_jb_core_wip
            if corebank% = 0% then return
            jbcorewip$ = dfcore_wip$
            plowkey$ = part$
            call "PLOWNEXT" (#15, plowkey$, 25%, f1%(15%))
               if f1%(15%) = 0% then return
            get #15 using L30860, jbcorewip$
L30860:         FMT POS(146), CH(9)
            if jbcorewip$ = " " then jbcorewip$ = dfcore_wip$
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "SHOSTAT" ("Updating Transaction Image File")

            call "DATUNFMT" (trdate$)
            if fromjob$ <> " " then L31190
            if postflag1% <> 0 then L31160
                if modno1$ = " " then modno1$ = "03"
                if jnlid1$ = " " then jnlid1$ = "MVA"
                call "JNLINFO" (modno1$, jnlid1$, pstseq1%, " ",         ~
                                             " ", trdate$, #1, f201%, 0%)
            postflag1% =  1%

L31160:     modno$ = modno1$ : jnlid$ = jnlid1$ : pstseq% = pstseq1%
            goto L31250

L31190:     if postflag2% <> 0 then L31236
                if modno2$ = " " then modno2$ = "03"
                if jnlid2$ = " " then jnlid2$ = "MJR"
                call "JNLINFO" (modno2$, jnlid2$, pstseq2%, " ",         ~
                                             " ", trdate$, #1, f201%, 0%)
            postflag2% =  1%

L31236:     modno$ = modno2$ : jnlid$ = jnlid2$ : pstseq% = pstseq2%

L31250:     call "GLUNFMT" (expacct$)
            put work$, using L31280, earntype$, lclass$, activity$,       ~
                                    unitrate, labr, ovhd, fromjob$
L31280:         FMT CH(12), 2*CH(4), 3*PD(14,4), CH(8)
            part$ = " "
            put part$, using L31310, type%, emp$, wc$
L31310:         FMT BI(1), CH(12), CH(4)
            get ocost$ using L31330, cost()
L31330:         FMT 12*PD(14,4)
            get lcost$ using L31330, cost1()
            mat cost = cost + cost1
            call "PACKZERO" (cost(), lcost$)
            if fromjob$ = " " then std% = 5% else std% = 3%

            call "JB2TIF"               /* Writes to transaction Image */~
                 ("J2",                 /* Send transaction to JBPOST2 */~
                  1%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  std%,                 /* Transaction type            */~
                  hex(10),              /* Priority (within 'J2' only) */~
                  jbnumber$,            /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  trdate$,              /* Who                         */~
                  part$,                /* Inventory Part (not really) */~
                  str(expacct$,,3),     /* Inventory Store Code """    */~
                  str(expacct$,4,6),    /* Inventory Lot Id.    """    */~
                  units,                /* Quantity to process  """    */~
                  work$,                /* To job, MTkey, anything     */~
                  freetext$,            /* Used for trans free text her*/~
                  lcost$)               /* Costs                       */~

            if std% = 5% then return
            str(work$,,20%) = jbnumber$

            call "JB2TIF"               /* Writes to transaction Image */~
                 ("J2",                 /* Send transaction to JBPOST2 */~
                  0%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  12%,                  /* Transaction type            */~
                  hex(10),              /* Priority (within 'J2' only) */~
                  fromjob$,             /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  trdate$,              /* Who                         */~
                  " ",                  /* Inventory Part (not really) */~
                  " ",                  /* Inventory Store Code """    */~
                  " ",                  /* Inventory Lot Id.    """    */~
                  0,                    /* Quantity to process  """    */~
                  work$,                /* To job, MTkey, anything     */~
                  " ",                  /* Used for trans free text her*/~
                  " ")                  /* Costs                       */

            return

        dataput2                       /* For Closing Entries */
            init (hex(00))  lcost$
            mat cost1 = zer
            if cls_type% = 2% or cls_type% = 6% then L31870
                gosub dataput
                return

L31870:     mat totcredit = cost
            call "MXSTPPT" addr(closingcr$, totcredit(1%), 12%, 4%)
            mat cost1 = zer
            init (hex(00))  lcost$

            for i% = 1% to 12%
                if totcredit(i%) = 0 then L32010
                     init (hex(00))  ocost$, lcost$
                     mat cost  = zer
                     str(ocost$,(i%-1%)*8%+1%,8%) =                      ~
                                         str(closingcr$,(i%-1%)*8%+1%,8%)
                     expacct$ = ce_acct$(i%)
                     gosub dataput
                     call "DATEFMT" (trdate$)
L32010:     next i%

            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

        FMT                 /* FILE: JBMASTR2                          */~
            CH(8),          /* Production job code                     */~
            CH(30),         /* Description of production job           */~
            CH(19),         /* Tag number in level 2 planning          */~
            CH(25),         /* Part code                               */~
            PD(14,4),       /* Quantity to make                        */~
            PD(14,4),       /* Quantity completed to date              */~
            CH(1),          /* Backflush flag                          */~
            CH(47),         /* Filler (Internal, unused space)         */~
            CH(6),          /* Date production job actually started    */~
            CH(6),          /* Date production job actually ended      */~
            CH(9),          /* Work in process GL account code         */~
            CH(6),          /* Date production job planned to start    */~
            CH(6),          /* Date production job planned to be comple*/~
            PD(14,4),       /* Original quantity ordered               */~
            CH(24),         /* Filler (Internal, unused space)         */~
            PD(14,4),       /* The quantity of parts scrapped          */~
            PD(14,4),       /* The quantity of parts reworked          */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            PD(14,4),       /* Actual (aka inventory) Costs            */~
            12*PD(14,4),    /* Actual (aka inventory) Costs            */~
            12*PD(14,4),    /* Actual (aka inventory) Costs            */~
            12*PD(14,4),    /* Actual (aka inventory) Costs            */~
            PD(14,4),       /* Total Credits from Job.                 */~
            12*PD(14,4),    /* Total Credits from Job.                 */~
            12*PD(14,4),    /* Total Credits from Job.                 */~
            12*PD(14,4),    /* Total Credits from Job.                 */~
            PD(14,4),       /* Cost associated with Standard Cost      */~
            12*PD(14,4),    /* Cost associated with Standard Cost      */~
            12*PD(14,4),    /* Cost associated with Standard Cost      */~
            12*PD(14,4),    /* Cost associated with Standard Cost      */~
            CH(181)         /* Filler (Internal, unused space)         */~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              str(line2$,62%) = "JBDIRSUB: " & str(cms2v$,,8%)
              scrn$ = " "
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()

              on fieldnr% gosub L40170,            /* Job Number        */~
                                L40170,            /* x                 */~
                                L40170,            /* WIP Account       */~
                                L40170             /* Transaction Type  */~

              goto L40200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02),                                               ~
                  "Direct Input of Costs to Jobs",                       ~
               at (01,45),                                               ~
                  "Post Date:",                                          ~
               at (01,56), fac(hex(8c)), post$                  , ch(08),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Job Number",                                 ~
               at (05,30), fac(lfac$( 1)), jbnumber$            , ch(08),~
               at (05,49), fac(hex(8c)),   jbnumberdescr$       , ch(32),~
                                                                         ~
               at (06,02), fac(lfac$( 2)), part$                , ch(25),~
               at (06,28), fac(hex(8c)),   partdescr$           , ch(34),~
               at (06,64), "Qty:",                                       ~
               at (06,69), fac(hex(8c)),   partqty$             , ch(10),~
                                                                         ~
               at (07,02), "WIP Account",                                ~
               at (07,30), fac(lfac$( 3)), wipacct$             , ch(12),~
               at (07,49), fac(hex(8c)),   wipdescr$            , ch(32),~
                                                                         ~
               at (08,02), fac(hex(8c)), trantypedescr$         , ch(30),~
               at (08,34), fac(lfac$(4%)), type$                , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(0001040d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40580
                  call "MANUAL" ("JBDIRSUB")
                  goto L40200

L40580:        if keyhit% <> 15 then L40620
                  call "PRNTSCRN"
                  goto L40200

L40620:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%)
              str(line2$,62%) = "JBDIRSUB: " & str(cms2v$,,8%)
              scrn$ = "Labor"
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L41140,            /* Job Number        */~
                                L41135,            /* x                 */~
                                L41140,            /* WIP Account       */~
                                L41140,            /* Transaction Date  */~
                                L41140,            /* Work Center       */~
                                L41140,            /* Activity          */~
                                L41140,            /* Employee          */~
                                L41140,            /* Earnings Type     */~
                                L41140,            /* Labor Class       */~
                                L41145,            /* Fixed Cost        */~
                                L41145,            /* Earnings Rate     */~
                                L41145,            /* Labor Hours       */~
                                L41145,            /* Net   Cost        */~
                                L41140,            /* Expense Account   */~
                                L41135             /* Transaction Text  */~

              goto L41155

L41135:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L41140:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L41145:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41155:     accept                                                       ~
               at (01,02),                                               ~
                  "Direct Input of Costs to Jobs: Labor",                ~
               at (01,33), fac(hex(84)), scrn$                  , ch(12),~
               at (01,45),                                               ~
                  "Post Date:",                                          ~
               at (01,56), fac(hex(8c)), post$                  , ch(08),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Job Number",                                 ~
               at (05,30), fac(lfac$( 1)), jbnumber$            , ch(08),~
               at (05,49), fac(hex(8c)),   jbnumberdescr$       , ch(32),~
                                                                         ~
               at (06,02), fac(lfac$( 2)), part$                , ch(25),~
               at (06,28), fac(hex(8c)),   partdescr$           , ch(34),~
               at (06,64), "Qty:",                                       ~
               at (06,69), fac(hex(8c)),   partqty$             , ch(10),~
                                                                         ~
               at (07,02), "WIP Account",                                ~
               at (07,30), fac(lfac$( 3)), wipacct$             , ch(12),~
               at (07,49), fac(hex(8c)),   wipdescr$            , ch(32),~
                                                                         ~
               at (09,02), "Transaction Date",                           ~
               at (09,30), fac(lfac$( 4)), trdate$              , ch(08),~
                                                                         ~
               at (10,02), "Work Center",                                ~
               at (10,30), fac(lfac$( 5)), wc$                  , ch(04),~
               at (10,49), fac(hex(8c)),   wcdescr$             , ch(32),~
                                                                         ~
               at (11,02), "Activity",                                   ~
               at (11,30), fac(lfac$( 6)), activity$            , ch(04),~
               at (11,49), fac(hex(8c)),   activitydescr$       , ch(32),~
                                                                         ~
               at (12,02), "Employee",                                   ~
               at (12,30), fac(lfac$( 7)), emp$                 , ch(12),~
               at (12,49), fac(hex(8c)),   empdescr$            , ch(32),~
                                                                         ~
               at (13,02), "Earnings Type",                              ~
               at (13,30), fac(lfac$( 8)), earntype$            , ch(12),~
               at (13,49), fac(hex(8c)),   earntypedescr$       , ch(32),~
                                                                         ~
               at (14,02), "Labor Class",                                ~
               at (14,30), fac(lfac$( 9)), lclass$              , ch(04),~
               at (14,49), fac(hex(8c)),   lclassdescr$         , ch(32),~
                                                                         ~
               at (15,02), "Fixed Cost",                                 ~
               at (15,30), fac(lfac$(10)), ovhd$                , ch(10),~
                                                                         ~
               at (16,02), "Earnings Rate",                              ~
               at (16,30), fac(lfac$(11)), unitrate$            , ch(10),~
                                                                         ~
               at (17,02), "Labor Hours",                                ~
               at (17,30), fac(lfac$(12)), units$               , ch(10),~
                                                                         ~
               at (18,02), "Net Cost",                                   ~
               at (18,30), fac(lfac$(13)), labr$                , ch(10),~
               at (18,49), "Total Cost:",                                ~
               at (18,61), fac(hex(84)),   total$               , ch(10),~
                                                                         ~
               at (19,02), "Expense Account",                            ~
               at (19,18), fac(lfac$(14)), expacct$             , ch(12),~
               at (19,31), "-OR- Job",                                   ~
               at (19,40), fac(lfac$(14)), fromjob$             , ch( 8),~
               at (19,49), fac(hex(8c)),   expacctdescr$        , ch(32),~
                                                                         ~
               at (20,02), "Transaction Text",                           ~
               at (20,30), fac(lfac$(15)), freetext$            , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,40), "(8)Job Cost Summary",                        ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(000104080d0f10)),                                ~
               key (keyhit%)
               if keyhit% <> 8% then L41520
                  call "JBQJOB" (jbnumber$, #2,#1,#3,#11,#12,#13,#14)
                  goto L41155

L41520:        if keyhit% <> 13 then L41540
                  call "MANUAL" ("JBDIRSUB")
                  goto L41155

L41540:        if keyhit% <> 15 then L41560
                  call "PRNTSCRN"
                  goto L41155

L41560:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%)
              str(line2$,62%) = "JBDIRSUB: " & str(cms2v$,,8%)
              scrn$ = "Work Cntr"
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42125,            /* Job Number        */~
                                L42120,            /* x                 */~
                                L42125,            /* WIP Account       */~
                                L42125,            /* Transaction Date  */~
                                L42125,            /* Work Center       */~
                                L42125,            /* Activity          */~
                                L42130,            /* Fixed Cost        */~
                                L42130,            /* Work Center Rate  */~
                                L42130,            /* Work Center Hours */~
                                L42130,            /* Net   Cost        */~
                                L42125,            /* Expense Account   */~
                                L42120             /* Transaction Text  */~

              goto L42140

L42120:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42125:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L42130:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42140:     accept                                                       ~
               at (01,32), fac(hex(8c)), scrn$                  , ch(12),~
               at (01,02),                                               ~
                  "Direct Input of Costs to Jobs: Work Cntr",            ~
               at (01,33), fac(hex(84)), scrn$                  , ch(12),~
               at (01,45),                                               ~
                  "Post Date:",                                          ~
               at (01,56), fac(hex(8c)), post$                  , ch(08),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Job Number",                                 ~
               at (05,30), fac(lfac$( 1)), jbnumber$            , ch(08),~
               at (05,49), fac(hex(8c)),   jbnumberdescr$       , ch(32),~
                                                                         ~
               at (06,02), fac(lfac$( 2)), part$                , ch(25),~
               at (06,28), fac(hex(8c)),   partdescr$           , ch(34),~
               at (06,64), "Qty:",                                       ~
               at (06,69), fac(hex(8c)),   partqty$             , ch(10),~
                                                                         ~
               at (07,02), "WIP Account",                                ~
               at (07,30), fac(lfac$( 3)), wipacct$             , ch(12),~
               at (07,49), fac(hex(8c)),   wipdescr$            , ch(32),~
                                                                         ~
               at (09,02), "Transaction Date",                           ~
               at (09,30), fac(lfac$( 4)), trdate$              , ch(08),~
                                                                         ~
               at (10,02), "Work Center",                                ~
               at (10,30), fac(lfac$( 5)), wc$                  , ch(04),~
               at (10,49), fac(hex(8c)),   wcdescr$             , ch(32),~
                                                                         ~
               at (11,02), "Activity",                                   ~
               at (11,30), fac(lfac$( 6)), activity$            , ch(04),~
               at (11,49), fac(hex(8c)),   activitydescr$       , ch(32),~
                                                                         ~
               at (12,02), "Fixed Cost",                                 ~
               at (12,30), fac(lfac$( 7)), ovhd$                , ch(10),~
                                                                         ~
               at (13,02), "Work Center Rate",                           ~
               at (13,30), fac(lfac$( 8)), unitrate$            , ch(10),~
                                                                         ~
               at (14,02), "Work Center Hours",                          ~
               at (14,30), fac(lfac$( 9)), units$               , ch(10),~
                                                                         ~
               at (15,02), "Net Cost",                                   ~
               at (15,30), fac(lfac$(10)), labr$                , ch(10),~
               at (15,49), "Total Cost:",                                ~
               at (15,61), fac(hex(84)),   total$               , ch(10),~
                                                                         ~
               at (16,02), "Expense Account",                            ~
               at (16,18), fac(lfac$(11)), expacct$             , ch(12),~
               at (16,31), "-OR- Job",                                   ~
               at (16,40), fac(lfac$(11)), fromjob$             , ch( 8),~
               at (16,49), fac(hex(8c)),   expacctdescr$        , ch(32),~
                                                                         ~
               at (17,02), "Transaction Text",                           ~
               at (17,30), fac(lfac$(12)), freetext$            , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,40), "(8)Job Cost Summary",                        ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(000104080d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 8% then L42445
                  call "JBQJOB" (jbnumber$, #2,#1,#3,#11,#12,#13,#14)
                  goto L42140

L42445:        if keyhit% <> 13 then L42465
                  call "MANUAL" ("JBDIRSUB")
                  goto L42140

L42465:        if keyhit% <> 15 then L42485
                  call "PRNTSCRN"
                  goto L42140

L42485:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   4                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'104(fieldnr%)
              str(line2$,62%) = "JBDIRSUB: " & str(cms2v$,,8%)
              scrn$ = "Misc."
              dacct$  = wipacct$
              dacctd$ = wipdescr$
              dacctp$ = "WIP Account"
              if type% <> 5% then L43080
                 scrn$ = "Core Misc."
                 dacct$  = jbcorewip$
                 dacctd$ = jbcorewipd$
                 dacctp$ = "Core WIP Account"
L43080:       if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L43200,            /* Job Number        */~
                                L43190,            /* x                 */~
                                L43200,            /* WIP Account       */~
                                L43200,            /* Transaction Date  */~
                                L43210,            /* Total Cost        */~
                                L43200,            /* Expense Account   */~
                                L43190             /* Transaction Text  */~

              goto L43230

L43190:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L43200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L43210:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L43230:     accept                                                       ~
               at (01,02),                                               ~
                  "Direct Input of Costs to Jobs: Misc.",                ~
               at (01,33), fac(hex(84)), scrn$                  , ch(12),~
               at (01,45),                                               ~
                  "Post Date:",                                          ~
               at (01,56), fac(hex(8c)), post$                  , ch(08),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Job Number",                                 ~
               at (05,30), fac(lfac$( 1)), jbnumber$            , ch(08),~
               at (05,49), fac(hex(8c)),   jbnumberdescr$       , ch(32),~
                                                                         ~
               at (06,02), fac(lfac$( 2)), part$                , ch(25),~
               at (06,28), fac(hex(8c)),   partdescr$           , ch(34),~
               at (06,64), "Qty:",                                       ~
               at (06,69), fac(hex(8c)),   partqty$             , ch(10),~
                                                                         ~
               at (07,02), fac(hex(8c)),   dacctp$              , ch(20),~
               at (07,30), fac(lfac$( 3)), dacct$               , ch(12),~
               at (07,49), fac(hex(8c)),   dacctd$              , ch(32),~
                                                                         ~
               at (09,02), "Transaction Date",                           ~
               at (09,30), fac(lfac$( 4)), trdate$              , ch(08),~
                                                                         ~
               at (10,02), "Total Cost",                                 ~
               at (10,30), fac(lfac$( 5)), ovhd$                , ch(10),~
                                                                         ~
               at (11,02), "Expense Account",                            ~
               at (11,18), fac(lfac$( 6)), expacct$             , ch(12),~
               at (11,31), "-OR- Job",                                   ~
               at (11,40), fac(lfac$( 6)), fromjob$             , ch( 8),~
               at (11,49), fac(hex(8c)),   expacctdescr$        , ch(32),~
                                                                         ~
               at (12,02), "Transaction Text",                           ~
               at (12,30), fac(lfac$( 7)), freetext$            , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,40), "(8)Job Cost Summary",                        ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(000104080d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 8% then L43660
                  call "JBQJOB" (jbnumber$, #2,#1,#3,#11,#12,#13,#14)
                  goto L43230

L43660:        if keyhit% <> 13 then L43700
                  call "MANUAL" ("JBDIRSUB")
                  goto L43230

L43700:        if keyhit% <> 15 then L43740
                  call "PRNTSCRN"
                  goto L43230

L43740:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   5                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'105(fieldnr%)
              str(line2$,62%) = "JBDIRSUB: " & str(cms2v$,,8%)
              scrn$ = "Closing"
              dacct$  = wipacct$
              dacctd$ = wipdescr$
              dacctp$ = "WIP Account"
              if type% <> 7% then L44100
                 scrn$ = "Core Close"
                 dacct$  = jbcorewip$
                 dacctd$ = jbcorewipd$
                 dacctp$ = "Core WIP Account"
L44100:       if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              if edit% = 0% then L44240
              if fieldnr% = 7% then L44240
                 init (hex(9c))  cfac$(), efac$()
                 if cls_type% = 2% or cls_type% = 6% then L44180
                     if fieldnr% = 0% then                               ~
                                      str(efac$()) = hex(8c) & hex(86)   ~
                                 else str(efac$()) = hex(8c) & hex(8c)
                  goto L44240
L44180:       for i% = 1% to 12%
                 if ce_acct$(i%) = " " and ce_amt$(i%) = " " then L44220
                 if fieldnr% = 0% then cfac$(i%) = hex(86)               ~
                                  else cfac$(i%) = hex(8c)
L44220:       next i%

L44240:       on fieldnr% gosub L44360,            /* Job Number        */~
                                L44350,            /* x                 */~
                                L44360,            /* WIP Account       */~
                                L44360,            /* Transaction Date  */~
                                L44360,            /* Account Structure */~
                                L44370,            /* Total Cost        */~
                                L44360,            /* Accounts & Amounts*/~
                                L44350             /* Transaction Text  */~

              goto L44390

L44350:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L44360:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L44370:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L44390:     accept                                                       ~
               at (01,02),                                               ~
                  "Direct Input of Costs to Jobs: Misc.",                ~
               at (01,33), fac(hex(84)), scrn$                  , ch(12),~
               at (01,45),                                               ~
                  "Post Date:",                                          ~
               at (01,56), fac(hex(8c)), post$                  , ch(08),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Job Number",                                 ~
               at (05,30), fac(lfac$( 1)), jbnumber$            , ch(08),~
               at (05,49), fac(hex(8c)),   jbnumberdescr$       , ch(32),~
                                                                         ~
               at (06,02), fac(lfac$( 2)), part$                , ch(25),~
               at (06,28), fac(hex(8c)),   partdescr$           , ch(34),~
               at (06,64), "Qty:",                                       ~
               at (06,69), fac(hex(8c)),   partqty$             , ch(10),~
                                                                         ~
               at (07,02), fac(hex(8c)),   dacctp$              , ch(20),~
               at (07,30), fac(lfac$( 3)), dacct$               , ch(12),~
               at (07,49), fac(hex(8c)),   dacctd$              , ch(32),~
                                                                         ~
               at (09,02), "Transaction Date",                           ~
               at (09,30), fac(lfac$( 4)), trdate$              , ch(08),~
                                                                         ~
               at (10,02), "Closing Account(s) (A or V)",                ~
               at (10,30), fac(lfac$( 5%)), ce_acct_type$       , ch(01),~
                                                                         ~
               at (11,02), "Total Cost",                                 ~
               at (11,30), fac(lfac$( 6%)), ovhd$               , ch(10),~
               at (11,45), fac(efac$( 1%)), acctprompt$         , ch(07),~
               at (11,55), fac(efac$( 2%)), expacct$            , ch(12),~
                                                                         ~
               at (12,04), fac(hfac$), close_hd$                , ch(29),~
               at (12,44), fac(hfac$), close_hd$                , ch(29),~
                                                                         ~
               at (13,04), fac(cfac$( 1%)), ce_acct$(1%)        , ch(12),~
               at (13,21), fac(cfac$( 1%)), ce_amt$ (1%)        , ch(12),~
                                                                         ~
               at (14,04), fac(cfac$( 2%)), ce_acct$(2%)        , ch(12),~
               at (14,21), fac(cfac$( 2%)), ce_amt$ (2%)        , ch(12),~
                                                                         ~
               at (15,04), fac(cfac$( 3%)), ce_acct$(3%)        , ch(12),~
               at (15,21), fac(cfac$( 3%)), ce_amt$ (3%)        , ch(12),~
                                                                         ~
               at (16,04), fac(cfac$( 4%)), ce_acct$(4%)        , ch(12),~
               at (16,21), fac(cfac$( 4%)), ce_amt$ (4%)        , ch(12),~
                                                                         ~
               at (17,04), fac(cfac$( 5%)), ce_acct$(5%)        , ch(12),~
               at (17,21), fac(cfac$( 5%)), ce_amt$ (5%)        , ch(12),~
                                                                         ~
               at (18,04), fac(cfac$( 6%)), ce_acct$(6%)        , ch(12),~
               at (18,21), fac(cfac$( 6%)), ce_amt$ (6%)        , ch(12),~
                                                                         ~
               at (13,44), fac(cfac$( 7%)), ce_acct$(7%)        , ch(12),~
               at (13,61), fac(cfac$( 7%)), ce_amt$ (7%)        , ch(12),~
                                                                         ~
               at (14,44), fac(cfac$( 8%)), ce_acct$(8%)        , ch(12),~
               at (14,61), fac(cfac$( 8%)), ce_amt$ (8%)        , ch(12),~
                                                                         ~
               at (15,44), fac(cfac$( 9%)), ce_acct$(9%)        , ch(12),~
               at (15,61), fac(cfac$( 9%)), ce_amt$ (9%)        , ch(12),~
                                                                         ~
               at (16,44), fac(cfac$(10%)), ce_acct$(10%)       , ch(12),~
               at (16,61), fac(cfac$(10%)), ce_amt$ (10%)       , ch(12),~
                                                                         ~
               at (17,44), fac(cfac$(11%)), ce_acct$(11%)       , ch(12),~
               at (17,61), fac(cfac$(11%)), ce_amt$ (11%)       , ch(12),~
                                                                         ~
               at (18,44), fac(cfac$(12%)), ce_acct$(12%)       , ch(12),~
               at (18,61), fac(cfac$(12%)), ce_amt$ (12%)       , ch(12),~
                                                                         ~
               at (19,02), "Transaction Text",                           ~
               at (19,30), fac(lfac$( 8%)), freetext$           , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,40), "(8)Job Cost Summary",                        ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(000104080d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 8% then L45320
                  call "JBQJOB" (jbnumber$, #2,#1,#3,#11,#12,#13,#14)
                  goto L44390

L45320:        if keyhit% <> 13 then L45360
                  call "MANUAL" ("JBDIRSUB")
                  goto L44390

L45360:        if keyhit% <> 15 then L45400
                  call "PRNTSCRN"
                  goto L44390

L45400:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50150,              /* Job Number        */~
                              L50350,              /* x                 */~
                              L50410,              /* WIP Account       */~
                              L50470               /* Transaction Type  */~

            return

L50150: REM Test for Job Number                   JBNUMBER$
                if from$ = "C" then L50210
                    call "GETCODE" (#2, jbnumber$, " ", 0%, 0, f1%(2))
                       if f1%(2) <> 0% then L50280
                     errormsg$ = "Please Enter An Existing Job Number"
                     return
L50210:         if jbnumber$ = jobnr$ then L50240
                errormsg$ = "Only Job Number "&jobnr$&" Can Be Processed"
                     return
L50240:         call "READ100" (#2, jbnumber$, f1%(2))
                   if f1%(2) <> 0% then L50280
                     errormsg$ = "Job Not on File:" & jbnumber$
                     return
L50280:         get #2, using L50290, temp$
L50290:         FMT POS(153), CH(6)
                if temp$ = " " or temp$ = blankdate$ then L50312
                     call "DATEFMT" (temp$)
                     errormsg$ = "Job Closed On: " & temp$
                     return
L50312:         u3% = 2%
                call "JBINUSE" (jbnumber$, u3%)
                   if u3% = 0% then L50320
                      errormsg$ = hex(00)
                      return
L50320:         gosub dataload
                /* RETURN */

L50350: REM Test for x                            X$
                call "GETCODE" (#3, part$, partdescr$, 1%, 99, f1%(3))
                if f1%(3)<>0 then L50410            /*RETURN*/
                     errormsg$="Part Not on File:" & part$
                     return

L50410: REM Test for WIP Account                  WIPACCT$
                call "GETCODE" (#4, wipacct$, wipdescr$, 1%, 0, f1%(4))
                if f1%(4)<>0 then return
                     errormsg$="WIP Account Not on File:" & wipacct$
                     return

L50470: REM Test for Transaction Type (L,W,D)     TYPE$
            if type$ = " " then L50500
            type% = pos(transtypecodes$(ttp_index%) = type$)
            if type% <> 0% then L50550
L50500:        errormsg$ = "Invalid Type: Enter " &                      ~
                                             transtypeerrmsg$(ttp_index%)
               errormsg$ = errormsg$ & type$
               return

L50550
*        Check for closing transaction
            if type% <> 4% and type% <> 7% then return
                get #3 using L50590, costtype$, def_adjacct$,             ~
                                    def_varacct$()
L50590:              FMT POS(307), CH(1), POS(380), 13*CH(9)
                if type% = 7% then L50670

*        Regular closing entry
            if pos("FST" = costtype$) = 0% then cls_type% = 1%           ~
                                           else cls_type% = 2%
            goto L50950

L50670
*        Have core closing entry, does part have a core?
            init (hex(00))  readkey$
            readkey$ = part$
            call "PLOWALTS" (#15, readkey$, 0%, 25%, f1%(15%))
                if f1%(15%) = 0% then L50880
            get #15 using L50730, core_varacct$
L50730:         FMT POS(170), CH(9)
            if core_varacct$ <> " " then L50850

            readkey$ = str(readkey$,26%,25%)
            init (" ")  str(readkey$,26%)
            call "REDALT0" (#15, readkey$, 1%, f1%(15%))
                if f1%(15%) = 0% then L50880
            get #15 using L50730, core_varacct$
                if core_varacct$ <> " " then L50850

            core_varacct$ = coreflag_var$

L50850:     cls_type% = 4%
            goto L50950

L50880
*        Core closing - part number not a reman part
            core_varacct$ = def_adjacct$
            if pos("FST" = costtype$) > 0 then L50930
                cls_type% = 5%
                goto L50950
L50930:     cls_type% = 6%

L50950:     return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L54100,              /* Job Number        */~
                              L54100,              /* x                 */~
                              L54100,              /* WIP Account       */~
                              L54200,              /* Transaction Date  */~
                              L54300,              /* Work Center       */~
                              L54400,              /* Activity          */~
                              L54500,              /* Employee          */~
                              L54600,              /* Earnings Type     */~
                              L54700,              /* Labor Class       */~
                              L55170,              /* Fixed Cost        */~
                              L54800,              /* Earnings Rate     */~
                              L54900,              /* Labor Hours       */~
                              L55000,              /* Net   Cost        */~
                              L55220,              /* Expense Account   */~
                              L55470               /* Transaction Text  */~

            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************

        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L54100,              /* Job Number        */~
                              L54100,              /* x                 */~
                              L54100,              /* WIP Account       */~
                              L54200,              /* Transaction Date  */~
                              L54300,              /* Work Center       */~
                              L54400,              /* Activity          */~
                              L55170,              /* Fixed Cost        */~
                              L54800,              /* Work Center Rate  */~
                              L54900,              /* Work Center Hours */~
                              L55000,              /* Net   Cost        */~
                              L55220,              /* Expense Account   */~
                              L55470               /* Transaction Text  */~

            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 4.                      *~
            *************************************************************

        deffn'154(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L54100,              /* Job Number        */~
                              L54100,              /* x                 */~
                              L54100,              /* WIP Account       */~
                              L54200,              /* Transaction Date  */~
                              L55170,              /* Total Cost        */~
                              L55220,              /* Expense Account   */~
                              L55470               /* Transaction Text  */~

            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 5.                      *~
            *************************************************************

        deffn'155(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L54100,              /* Job Number        */~
                              L54100,              /* x                 */~
                              L54100,              /* WIP Account       */~
                              L54200,              /* Transaction Date  */~
                              L55540,              /* CE Acct Structure */~
                              L55170,              /* Total Cost        */~
                              L55700,              /* Accounts & Amounts*/~
                              L55470               /* Transaction Text  */~

            return

        REM *************************************************************~
            * COMMON VALIDATION CODE SECTION                            *~
            *************************************************************


L54100: REM Test for Job Number                   JBNUMBER$
            return

L54200: REM Test for Transaction Date             TRDATE$
            call "DATEOK" (trdate$, u3%, errormsg$)
               if errormsg$<>" " then return
            temp$=trdate$
            call "DATUNFMT" (temp$)
            call "WHICHPER" (#1, temp$, which%)
               if which% = 0 then errormsg$ = "Date Is Not Valid for G/L ~
        ~Posting"
                     return

L54300: REM Test for Work Center                  WC$
            wcdescr$ = " "
            if wc$ = " " then return
            if wc$ = "?" then temp = 0 else temp = 99
            call "GETCODE" (#6, wc$, wcdescr$, 0%, temp, f1%(6))
               if f1%(6)=0 then wcdescr$="** Work Center Not On File **"
            return

L54400: REM Test for Activity                     ACTIVITY$
            activitydescr$ = " "
            if activity$ = " " then return
                if activity$ = "?" then activity$ = " "
                plowkey$ = "WC ACTVTY" & activity$
                call "PLOWCODE" (#7, plowkey$, activitydescr$, 9%,       ~
                                                            0.3, f1%(7))
                if f1%(7) = 1% then activity$ = str(plowkey$,10) else    ~
                            activitydescr$ = "** Activity Not On File **"
                return

L54500: REM Test for Employee                     EMP$
            empdescr$ = " " : if emp$ = " " then return
            call "GETEMPL" (#8, emp$, empdescr$, 1%, f1%(8))
            if f1%(8) <> 0 then L54525
            empdescr$ = "Employee Not On File"
                 return
L54525:     get #8, empstat$
            if empstat$ = "C" then L54545
                 errormsg$ = "Employee Is Not Active"
                 return
L54545:     call "READ100" (#9, emp$, f1%(9))
            if f1%(9) <> 0 then return
                 errormsg$="Employee Code Not In Payroll Master File"
                 return

L54600: REM Test for Earnings Type                EARNTYPE$
            if earntype$ = " " then return
            readkey$ = str(emp$) & earntype$
            call "PLOWALTS" (#10, readkey$, 1%, 24%, f1%(10))
                if f1%(10) <> 0 then L54650
            call "PLOWCODE" (#10, readkey$, " ", 12%, 1.00, f1%(10))
                if f1%(10) <> 0 then L54650
                errormsg$ = "Invalid Earnings Type"
                return
L54650:     earntype$ = str(readkey$,13)
            get #10, using L54660, unitrate,tmpacct$
L54660:         FMT XX(51), PD(14,4),POS(60),CH(9)
            call "CONVERT" (unitrate, 2.4, unitrate$)
            return

L54700: REM Test for Labor Class                  LCLASS$
            lclassdescr$ = " "
            if lclass$ = " " then return
            if lclass$ = "?" then lclass$ = " "
            plowkey$ = "LBR CLASS" & lclass$
            call "PLOWCODE" (#7, plowkey$, lclassdescr$, 9%, 0.30, f1%(7))
            if f1%(7) <> 0% then L54760
                lclassdescr$ = "** Labor Class Not On File **"
                return
L54760:     lclass$ = str(plowkey$,10)
            expacct$ = tmpacct$
            call "GLFMT"(expacct$)
            return

L54800: REM Test for Earnings Rate                UNITRATE$
            if unitrate$ = " " then unitrate$ = "0"
            call "NUMTEST"(unitrate$, -9e7, 9e7, errormsg$, 2.4, unitrate)
               if errormsg$ <> " " then return
                  goto L54940

L54900: REM Test for Labor Hours                  UNITS$
            if units$ = " " then units$ = "0"
            call "NUMTEST" (units$, -9e7, 9e7, errormsg$, 2.2, units)
               if errormsg$ <> " " then return
L54940:           labr = round(unitrate * units, 4%)
                  call "CONVERT" (labr, -2.4, labr$)
                  gosub L55170

L55000: REM Test for Net   Cost                   LABR$
            if edit% = 0% then return
            if edit% <> 1% then L55090
               if fieldnr% = 13% then return
               if fieldnr% = 12% and cursor%(1) = 16% then return
               if fieldnr% = 11% and cursor%(1) = 17% then return
                  cursor%(1) = 18%
                  return

L55090:        if fieldnr% = 10% then return
               if fieldnr% =  9% and cursor%(1) = 13% then return
               if fieldnr% =  8% and cursor%(1) = 14% then return
                  cursor%(1) = 15%
                  return

               /* NOTE: This overrides user's 'Smart Edit' if app. */

L55170: REM Test for Fixed Cost                   OVHD$
            total = round(labr + ovhd, 4%)
            call "CONVERT" (total, -2.4, total$)
            return

L55220: REM Test for Expense Account              EXPACCT$
            if expacct$ = " " then L55300
            if cfromjob$ <> " " then call "JBINUSE" (cfromjob$, 1%)
            fromjob$, cfromjob$ = " "
            call "GETCODE" (#4, expacct$, expacctdescr$, 0%,0,f1%(4))
               if f1%(4) <> 0% then return
            errormsg$="Expense Account Not on File:" & expacct$
            return

L55300:     if fromjob$ <> " " then L55321
               errormsg$ = "Enter Expense Account or Job Number."
               return
L55321:     if cfromjob$ <> " " then call "JBINUSE" (cfromjob$, 1%)
               cfromjob$ = " "
            call "GETCODE" (#2, fromjob$, wipdescr$, 0%, 0, f1%(2))
               if f1%(2) <> 0% then L55370
                   errormsg$ = "Please Enter An Existing Job Number."
                   return
L55370:        get #2, using L55380, temp$
L55380:        FMT POS(153), CH(6)
               if temp$ = " " or temp$ = blankdate$ then L55430
                   call "DATEFMT" (temp$)
                   errormsg$ = "Job Closed On: " & temp$
                   return
L55430:         if jbnumber$ <> fromjob$ then L55451
                   errormsg$ = "Job Numbers Cannot Be the Same."
                   return
L55451:         u3% = 2%
                call "JBINUSE" (fromjob$, u3%)
                if u3% = 0% then L55456
                   errormsg$ = hex(00)
                   return
L55456:         cfromjob$ = fromjob$
                return

L55470: REM Test for Transaction Text             FREETEXT$
            if freetext$ <> " " then L55510
               errormsg$ = "Transaction Text Cannot Be Blank."
               return
L55510:     edit% = type%
            return

L55540: REM Test for Closing Entry Account Structure  CE_ACCT_TYPE$
            if pos("AV" = ce_acct_type$) > 0% then L55580
               errormsg$ = "Enter 'A' or 'V'."
               return
L55580:     if old_ce_acct_type$ = ce_acct_type$ then return
               if ce_acct_type$ = "V" then L55620
                     cls_type% = cls_type% - 1%         /* 'V' to 'A' */
                     goto L55640
L55620:        cls_type% = cls_type% + 1%               /* 'A' to 'V' */

L55640:        edit% = 0%
               startfield% = 5%
               init (" ")  ovhd$  :  init (hex(00))  ocost$
               return clear all
               goto inputclosing

L55700: REM Test Closing Entries - Accounts and Values    CF_ACCT$
            if cls_type% = 2% or cls_type% = 6% then L55790
                efac$(2%) = hex(8c)
                call "GETCODE" (#4, expacct$, expacctdescr$,0%,0, f1%(4%))
                     if f1%(4%) <> 0% then return
                          errormsg$ = "Account not on File: " & expacct$
                          efac$(2%) = hex(81)
                          return

L55790:     init (hex(8c))  cfac$()
            for i% = 1% to 12%
                if ce_acct$(i%) = " " then L55890
L55820:              call "GETCODE" (#4, ce_acct$(i%), expacctdescr$, 0%,~
                                     0, f1%(4%))
                     if f1%(4%) <> 0% then L55890
                          errormsg$ = "Account not on File: " &          ~
                                      ce_acct$(i%)
                          cfac$(i%) = hex(81)
                          return
L55890:         call "NUMTEST" (ce_amt$(i%), -9e7, 9e7, errormsg$, -2.4, ~
                                cost(i%))
                     if errormsg$ = " " then L55940
                          cfac$(i%) = hex(81)
                          return
L55940:              if cost(i%)  = 0 and ce_acct$(i%) = " " then        ~
                                                        ce_amt$(i%) = " "
                     if cost(i%)  = 0 and ce_acct$(i%) = " " then L55980
                     if cost(i%) <> 0 and ce_acct$(i%) = " " then L55820
L55980:     next i%

            call "MXSTPPT" addr(ocost$, cost(1%), 12%, 4%)
            mat wrk = con:  mat jbtotal = wrk * cost
            call "CONVERT" (jbtotal(1%), 2.4, ovhd$)
            ovhd = jbtotal(1%)
            return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
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

        exit_program

            if postflag1% = 1% then call "JBJNLCLS"                      ~
                          ("J2", userid$, modno1$, jnlid1$, pstseq1%, 0%)
            if postflag2% = 1% then call "JBJNLCLS"                      ~
                          ("J2", userid$, modno2$, jnlid2$, pstseq2%, 0%)
            end
