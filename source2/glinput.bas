        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   GGG   L      IIIII  N   N  PPPP   U   U  TTTTT          *~
            *  G      L        I    NN  N  P   P  U   U    T            *~
            *  G GGG  L        I    N N N  PPPP   U   U    T            *~
            *  G   G  L        I    N  NN  P      U   U    T            *~
            *   GGG   LLLLL  IIIII  N   N  P       UUU     T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLINPUT  - Adds Account Numbers to the GLMAIN file,       *~
            *            creating all info needed to allow posting to   *~
            *            the new account.  Also allows edit of some     *~
            *            account info, and creates simple audit report. *~
            *                                                           *~
            *            Dual books is an option, depending on a flag   *~
            *            set (by GLFLAGS) in SYSFILE2.                  *~
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
            * 04/11/80 ! ORIGINAL (FOR 8TH GENERATION FILE SPECS!)! BCW *~
            * 05/20/87 ! Rewrite, Added Obsolete Flag & Delete    ! HES *~
            * 08/18/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 08/05/88 ! Dual Books depends on a SYSFILE2 flag.   ! JIM *~
            * 10/10/88 ! Added format of account after delete     ! JDH *~
            * 04/25/89 ! Changed STATUS$ check @ 19190            ! LAB *~
            * 06/07/89 ! Modified pgm for suspense acct processing! LAB *~
            * 07/14/93 ! PRR 12870. Option to check LY for Delete.! JDH *~
            * 07/26/94 ! PRR 13263. Check of close-to bfor Delete.! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            _90daysago$6,                /* Date 90 days ago           */~
            account$12, lastaccount$12,  /* G/L Account Number         */~
            act$3,                       /* Action taken on an account */~
            bal(32),                     /* 32 Monthly Balances        */~
            bal$13,                      /* Work Variable For Display  */~
            clsacct$12, laclsacct$12,    /* Close To Account (Year End)*/~
            clsdescr$32, laclsdescr$32,  /* Close To Account Descriptio*/~
            coname$60,                   /* Company name               */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            description$30, lastdescr$30,/* Account Description        */~
            dual_books$1,                /* Dual books in effect?      */~
            errormsg$79,                 /* Error message              */~
            fill$(9)29,                  /* 261 bytes of stuff         */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lauthnbr$12, lauthdsc$30,    /* G/L A/# for local authority*/~
            lasusacct$30,                /* LOCAL AUTHORITY DEFAULT DES*/~
            lafac1$1, lamessag1$74,      /* Loc. Auth message & FAC    */~
            lafac2$1, lamessag2$74,      /* Loc. Auth message & FAC    */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf32$24,                     /* PF 32 SCREEN LITERAL       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf12$18,                     /* PF 8 Screen Literal        */~
            readkey$50, key$99,          /* Keys                       */~
            rptid$6,                     /* Report ID                  */~
            sales$1,                     /* Sales Account?             */~
            saleskey$20,                 /* Key For SYSFILE2           */~
            seq$3,                       /* NExt Detail Seq Number     */~
            status$1,                    /* Account Obsolete Flag      */~
            statdescr$32,                /* Account Obsolete Desription*/~
            stat_susacct$16,             /* Stat Suspense # - SYSFILE2 */~
            step$2, lastep$2,            /* Close On Step Number       */~
            susacct$30,                  /* SUSPENSE ACCT DEFAULT      */~
            time$8,                      /* Time of day                */~
            type$1, types$8,             /* Account Type(s)            */~
            typedescr$26, typedescr$(8)20,/* Account Type Description  */~
            userid$3,                    /* This User's Id             */~
            warn$15,                     /* Warning message            */~
            work$(2)128                  /* Account Balance Fields     */

        dim f1%(32)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! GLMAIN   ! General Ledger CHart Of Accounts File.   *~
            * #02 ! SYSFILE2 ! Caelus Management System Information     *~
            * #03 ! GLCLSETO ! Stores G/L close to accounts and step nu *~
            * #05 ! GLDETAIL ! General Ledger Detail File               *~
            * #06 ! GLBUDGET ! General Ledger Account Budget Amounts    *~
            * #10 ! GLMAIN2  ! G. L. chart of accounts for local auth.  *~
            * #11 ! GLDETAL2 ! G. L. detail records for local authority *~
            * #12 ! GLCROSS2 ! G. L. chart-to-local-authority cross ref.*~
            * #13 ! GLCLSET2 ! L. A. G/L close to accounts and step #   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select #02, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #03, "GLCLSETO",                                      ~
                        varc,     indexed,  recsize =   22,              ~
                        keypos =   14, keylen =   9,                     ~
                        alt key  1, keypos =   12, keylen =  11,         ~
                            key  2, keypos =    3, keylen =   9, dup,    ~
                            key  3, keypos =    1, keylen =  22

            select #05, "GLDETAIL",                                      ~
                        varc,     indexed,  recsize = 160,               ~
                        keypos = 1,    keylen = 26

            select #06, "GLBUDGET"                                       ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos = 1, keylen = 9

            select #10, "GLMAIN2",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select #11, "GLDETAL2",                                      ~
                        varc,     indexed,  recsize = 160,               ~
                        keypos = 1,    keylen = 26

            select #12, "GLCROSS2",                                      ~
                        varc,     indexed,  recsize =  27,               ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  18

            select #13, "GLCLSET2",                                      ~
                        varc,     indexed,  recsize =   22,              ~
                        keypos =   14, keylen =   9,                     ~
                        alt key  1, keypos =   12, keylen =  11,         ~
                            key  2, keypos =    3, keylen =   9, dup,    ~
                            key  3, keypos =    1, keylen =  22

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#02, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#03, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#05, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#06, 0%, 0%, 0%, " ")
            call "READ100" (#02, "FISCAL DATES", f1%(2))
                if f1%(2) = 0% then L02690
                     get #02 using L02688, stat_susacct$
L02688:                   FMT POS(417), CH(16)
L02690:     dual_books$ = "N"                        /* Default to 'no' */
            call "READ100" (#02, "SWITCHS.GL", f1%(2))
                if f1%(2) = 0% then goto L09000
            get #02 using L02730, dual_books$
L02730:         FMT POS(21), CH(1)
            if dual_books$ <> "Y" then goto L09000
                call "OPENCHCK" (#10, 0%, 0%, 100%, " ")
                call "OPENCHCK" (#11, 0%, 0%, 100%, " ")
                call "OPENCHCK" (#12, 0%, 0%, 100%, " ")
                call "OPENCHCK" (#13, 0%, 0%, 100%, " ")

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date
            call "DATE" addr ("G+", str(date$,,6), -90%, _90daysago$, u3%)
            call "DATEFMT" (date$)
            call "EXTRACT" addr ("ID", userid$)
            call "COMPNAME" (12%, coname$, u3%)
            call "TIME" (time$)
            rptid$ = "G/L005"
            types$ = "$ALCRE" & hex(ffff)
            pageline% = 987654321%
            warn$ = "*** WARNING ***"

            typedescr$(1) = "* Unknown *"
            typedescr$(2) = "Cash In Bank Account"
            typedescr$(3) = "Asset Account"
            typedescr$(4) = "Liability Account"
            typedescr$(5) = "Capital Account"
            typedescr$(6) = "Revenue Account"
            typedescr$(7) = "Expense Account"

            lamessag1$ = "New Loc. Auth. acct #. It will be created upo"&~
                "n pressing PF(16)Save Data"
            lamessag2$ = "Stat. G/L is blank. All data fields will be u"&~
                "pdated to Loc. Auth. acct."

            susacct$ = "System Suspense Account"
            lasusacct$ = "Local Authority Suspense Acct"

            if dual_books$ <> "Y"                                        ~
                then hi_fieldnr% = 7%                                    ~
                else hi_fieldnr% = 11%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            account$ = " "
            if dual_books$ <> "N" then                                   ~
                pf32$ = "(32)Exit Program & Purge"
L10080:     pf4$, pf12$ = " " :  pf16$ = "(16)Exit Program"
            gosub L29000

            for fieldnr% = 1% to hi_fieldnr%
                if fieldnr% > 1 then pf4$ = "(4)Previous Field"
                if fieldnr% > 1 then pf16$ = " "
                if fieldnr% > 1 then pf32$ = " "
                gosub'051(fieldnr%, 0%) /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10260
L10160:         if dual_books$ <> "Y" /* Display & accept screen */      ~
                     then gosub'102(fieldnr%)                            ~
                     else gosub'101(fieldnr%, 0%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10240
L10190:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%, 0%)
                         if fieldnr% = 1% then L10080
                         if enabled% = 1% then L10160
                         goto L10190
L10240:               if keyhit% = 16 and fieldnr% = 1 then exit_program
                      if keyhit% = 32 and fieldnr% = 1 then exit_program
                      if keyhit% <>  0 then       L10160
L10260:         gosub'151(fieldnr%, 0%) /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10160
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editmode
            editmode% = 1%
            pf4$  = " "
            if onfile% = 1% then pf12$ = "(12)Delete Account"
            pf32$ = " "
            pf16$ = "(16)Save Data"
            inpmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            if dual_books$ <> "Y" /* Display Screen - No Entry   */      ~
                then gosub'102(0%)                                       ~
                else gosub'101(0%, 1%)
                  errormsg$ = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <> 12 then L11210
                     if onfile% <> 0% then goto delete_account
L11210:           if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editmode
            if cursor%(1) <  6% then goto editmode
            if cursor%(1) > 12% then goto L11250
                fieldnr% = cursor%(1) - 5%
                goto L11340
L11250:     if cursor%(1) < 15% then goto editmode
            if dual_books$ <> "Y" then goto editmode
            if cursor%(1) > 17% then goto editmode
                fieldnr% = cursor%(1) - 7%
                if fieldnr% <> 8% then fieldnr% = fieldnr% + 1%
                if fieldnr% =  8% and cursor%(2) > 42% then fieldnr% = 9%
L11340:     gosub'051(fieldnr%, 1%)     /* Check Enables, Set Defaults */
                  if enabled% = 0% then goto L11430
                  pf4$, pf12$, pf16$, pf32$ = " "
L11370:     if dual_books$ <> "Y"                                        ~
                then gosub'102(fieldnr%)                                 ~
                else gosub'101(fieldnr%, 1%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11370
            gosub'151(fieldnr%, 1%)     /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11370
L11430:     goto editmode

        delete_account
            if account$ <> stat_susacct$ then L11460
                errormsg$ = "You may not delete the Suspense Account"
                goto editmode
L11460:     if account$ <> " " then goto L11490
                errormsg$ = "You may only delete Statutory G/L accounts"
                goto editmode
L11490:     get work$(), using L11500, bal()
L11500:         FMT 32*PD(14,4)

        REM First Test Account Balance (Must Be Zero To Delete)
            balance = 0
            for month% = 1% to 32%
                balance = balance + bal(month%)
            next month%
            balance = round(balance, 2)
            if balance = 0 then L11640
                call "CONVERT" (balance, -2.2, bal$)
                errormsg$ = "Sorry, account has non-zero balance of: " & ~
                    bal$
                goto editmode

L11640: REM Now Test Monthly Activity (Must All Be Zero To Delete)
            for month% = 16 to 32
                if bal(month%) = 0 then L11740
                     temp = month%-15%
                     convert temp to bal$, pic (##)
                     errormsg$ = "Activity for period " & str(bal$,,2)
L11700:              call "CONVERT" (bal(month%), -2.2, bal$)
                     errormsg$ = errormsg$ & " is: " & bal$  &           ~
                        " (must be ZERO)"
                     goto editmode
L11740:     next month%

        gosub check_previous_year_option
            if errormsg$ <> " " then L11700
        gosub check_close_to_reference
            if errormsg$ <> " " then editmode

        REM Now Test G/L Detail For Account Activity Within 90 Days
            readkey$ = account$
            call "GLUNFMT" (str(readkey$,,12))
            str(readkey$,17,10) = str(_90daysago$) & hex(00000000)
            call "PLOWNEXT" (#05, readkey$, 16%, f1%(5))
                if f1%(5) = 0% then all_clear
            u3% = 2%
            call "ASKUSER" (u3%, warn$,     "This Account Has Had " &    ~
                            "Activity Within The Last 90 Days.",         ~
                            "If You Still Wish To Delete It, Press " &   ~
                            "PF(28)", "To Cancel This Delete Request, "& ~
                            "Press Any Other PF Key")
            if u3% <> 28% then goto editmode
            goto actual_delete

        all_clear
            u3% = 2%
            call "ASKUSER" (u3%, warn$,     "If You Continue, The " &    ~
                            "Account Shown Above Will Be Deleted.",      ~
                            "If You Still Wish To Delete It, Press " &   ~
                            "PF(28)", "To Cancel This Delete Request, "& ~
                            "Press Any Other PF Key")
            if u3% <> 28% then goto editmode

        actual_delete
            call "GLUNFMT" (account$)
            readkey$ = account$
            call "DELETE" (#01, readkey$, 9%)
            call "DELETE" (#03, readkey$, 9%)
            call "DELETE" (#05, readkey$, 16%)
            call "DELETE" (#06, readkey$, 9%)
            if dual_books$ = "Y" then call "DELETE" (#12, readkey$, 9%)
            saleskey$ = "SALES1" & str(account$,,9)
            call "DELETE" (#02, saleskey$, 15%)
            call "GLFMT" (account$)
            act$ = "DEL"
            goto print_audit

        REM Now Optionally Test Last Year Activity (Must All Be 0)
        check_previous_year_option
L12140:     u3% = 2%
            call "ASKUSER" (u3%, "* * * CHECK LAST YEAR? * * *",         ~
                 "Press PF16 if you wish to check Last Year's Activity", ~
                 "- or -", "Press RETURN if you don't.")
            if u3% = 0% then return
            if u3% <> 16% then L12140

            for month% = 2% to 14%
                if bal(month%) = 0 then L12300
                     temp = month% - 1%
                     convert temp to bal$, pic (##)
                     errormsg$ = "Activity for LY Period " & str(bal$,,2)
                     return
L12300:     next month%
            return

        check_close_to_reference
*        Checking for a Close-to Reference & warning if found
            errormsg$ = " "
            readkey$ = account$
            call "GLUNFMT" (str(readkey$,,12%))
            call "REDALT0" (#03, readkey$, 2%, f1%(3%))
                if f1%(3%) = 0% then return
            get #03 using L12384, temp$
L12384:         FMT POS(14), CH(9)
            call "GLFMT" (temp$)

            u3% = 2%
            call "ASKUSER" (u3%, "* * * CLOSE TO REFERENCE * * *",       ~
                 account$ & " is referenced as a 'Close To' account " &  ~
                 "for " & temp$, "If You Still Wish To Delete It, "  &   ~
                 "Press PF(28)", "To Cancel This Delete Request, "&      ~
                 "Press Any Other PF Key")
            if u3% = 28% then return
                errormsg$ = "Referenced as a 'Close To' Account."
                return

        REM *************************************************************~
            *        A U D I T   R E P O R T   S E C T I O N            *~
            *-----------------------------------------------------------*~
            * Print Logic For Audit Trail Of Records Added & Deleted... *~
            *************************************************************

        form_control
            select printer(134)
            if page% = 0% then call "SETPRNT" (rptid$, " ", 0%, 0%)
            pageline% = pageline% + 1
            if pageline% < 54 then return
            page% = page% + 1
            print page
            print using L14040, date$, time$, coname$, "-" & rptid$
            print using L14070, page%
            print
            if dual_books$ = "Y"                                         ~
                then print using L14100                                   ~
                else print using L14190
            if dual_books$ = "Y"                                         ~
                then print using L14130                                   ~
                else print using L14220
            print
            pageline% = 6%
            return

        REM *************************************************************~
            * Print line formats.                                       *~
            *************************************************************

L14040: %Run Date: ######## @ ########      #############################~
        ~###############################                       GLINPUT####~
        ~###
L14070: %                                                G/L ACCOUNT MAIN~
        ~TENANCE AUDIT TRAIL                                       Page: #~
        ~###
L14100: %G/L Account  Description                    Type                ~
        ~   Sls Usr Status    Loc Auth G/L Description                    ~
        ~Act
L14130: %------------ ------------------------------ --------------------~
        ~-- --- --- --------- ------------ ------------------------------ ~
        ~---
L14160: %############ ############################## #=##################~
        ~##  #  ### ######### ############ ############################## ~
        ~ ###
L14190: %G/L Account  Description                    Type                ~
        ~   Sls Usr Status    Act
L14220: %------------ ------------------------------ --------------------~
        ~-- --- --- --------- ---
L14250: %############ ############################## #=##################~
        ~##  #  ### ######### ###
L14280: %                                 ** END OF REPORT @ ######## **

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            REM Remove Any Old Data From File...
            call "GLUNFMT" (account$)
            if dual_books$ = "Y" then call "GLUNFM2" (lauthnbr$)
            call "DELETE" (#01, account$, 9%)
            call "DELETE" (#03, account$, 9%)
            if dual_books$ = "Y" then call "DELETE" (#12, account$, 9%)
            gosub dataput
            call "GLFMT" (account$)
            if dual_books$ = "Y" then call "GLFMT2" (lauthnbr$)
            act$ = "ADD"
            if onfile% <> 0% then act$ = "CHG"
        print_audit
            statdescr$ = "Active"
            if status$ <> hex(00) then statdescr$ = "In-active"
            gosub form_control
            if dual_books$ = "Y"                                         ~
                then print using L14160, account$, description$, type$,   ~
                    typedescr$, sales$, userid$, statdescr$, lauthnbr$,  ~
                    lauthdsc$, act$                                      ~
                else print using L14250, account$, description$, type$,   ~
                    typedescr$, sales$, userid$, statdescr$, act$
            lastaccount$ = account$
            lastdescr$   = description$
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%, e%)
            inpmessage$ = " "
            enabled% = 1%
*          IF LAUTHDSC$ = "Local Authority Suspence Acct" AND           ~
*              DESCRIPTION$ = " " THEN ENABLED% = 0%
*          IF DESCRIPTION$ = "System Suspense Account" AND (FIELDNR% <> ~
*              5% AND FIELDNR% <> 6% AND FIELDNR% <> 10% AND FIELDNR% <>~
*              11%) THEN ENABLED% = 0%
            on fieldnr% gosub L20250,         /* Account # (Statutory)  */~
                              L20330,         /* Account Descr  (")     */~
                              L20380,         /* Account Type           */~
                              L20430,         /* Sales Account?         */~
                              L20520,         /* Close To Account       */~
                              L20590,         /* Close Step Number      */~
                              L20660,         /* Account Obsolete Flag  */~
                              L20730,         /* Account # (Loc. Auth.) */~
                              L20810,         /* Account Descr  (")     */~
                              L20850,         /* Close To Account (")   */~
                              L20920          /* Close Step Number (")  */
            return

L20250: REM DEF/ENABLE G/L Account Number          ACCOUNT$
            if account$ <> stat_susacct$ then L20260
                enabled% = 0%
                return
L20260:     if dual_books$ = "Y"                                         ~
                then inpmessage$ = "Enter a G/L Account Number, blanks,"&~
                     " or '?' to see a list of valid accounts"           ~
                else inpmessage$ = "Enter a G/L Account Number, or blan"&~
                     "ks to see a list of valid accounts"
            return

L20330: REM DEF/ENABLE Account Description         DESCRIPTION$
            if account$ = " " then enabled% = 0%
            inpmessage$ = "Type a description of the Account"
            return

L20380: REM DEF/ENABLE Account Type                TYPE$
            if account$ <> stat_susacct$ then L20390
                enabled% = 0%
                return
L20390:     inpmessage$ = "Type: '$'=cash; 'A'=asset; 'L'=liability; 'C"&~
                "'=capital; 'R'=revenue; 'E'=expense"
            return

L20430: REM DEF/ENABLE Sales Account?              SALES$
            if account$ <> stat_susacct$ then L20440
                enabled% = 0%
                return
L20440:     inpmessage$ = "Enter 'Y' if this is a sales account; 'N' if"&~
                     " not."
            if sales$ = " " then sales$ = "N"
            if type$ = "R" or type$ = "E" then return
            enabled% = 0%
            sales$ = "N"
            return

L20520: REM DEF/ENABLE Close To Account (Year End) CLSACCT$
            inpmessage$ = "Enter the Account to be closed to at year en"&~
                "d, blanks or '?' to see list"
            if type$ = "R" or type$ = "E" then return
            if editmode% = 0% then enabled% = 0%
            return

L20590: REM DEF/ENABLE Close On Step Number        STEP$
            inpmessage$ = "Enter the Step # on which to close this acct."
            if clsacct$ = " " then step$ = " "
            if clsacct$ = " " then enabled% = 0%
            if enabled% <> 0% and step$ = " " then step$ = "01"
            return

L20660: REM Def/Enable Account Obsolete Flag       STATUS$
            if account$ <> stat_susacct$ then L20670
                enabled% = 0%
                return
L20670:     if editmode% = 0% then enabled% = 0%
            inpmessage$ = "Enter 'A'=active, 'O'=obsolete, or 'X' to fo"&~
                    "rce postings to suspense"
            if status$ = " " then status$ = "A"
            return

L20730: REM DEF/ENABLE G/L Account Number          LAUTHNBR$
            if account$ <> stat_susacct$ then L20740
                enabled% = 0%
                return
L20740:     if e% = 1%                                                   ~
                then inpmessage$ = "Enter Local Authority acct # and/or"&~
                     " account description"                              ~
                else inpmessage$ = "Enter G/L Account # for local autho"&~
                     "rity or '?' to see list of #s"
            return

L20810: REM DEF/ENABLE Account Description         LAUTHDSC$
            inpmessage$ = "Type a description of the Account"
            return

L20850: REM DEF/ENABLE Close To Account (Year End) LACLSACCT$
            inpmessage$ = "Enter the Account to be closed to at year en"&~
                "d, blanks or '?' to see list"
            if type$ = "R" or type$ = "E" then return
            if editmode% = 0% then enabled% = 0%
            return

L20920: REM DEF/ENABLE Close On Step Number        LASTEP$
            inpmessage$ = "Enter the Step # on which to close this acct."
            if laclsacct$ = " " then lastep$ = " "
            if laclsacct$ = " " then enabled% = 0%
            if enabled% <> 0% and lastep$ = " " then lastep$ = "01"
            return

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$,                            ~
                      description$           , /* Account Descr        */~
                      type$                  , /* Account Type         */~
                      typedescr$             , /* Type Description     */~
                      sales$                 , /* Sales Account?       */~
                      clsacct$, laclsacct$   , /* Close To Account     */~
                      clsdescr$, laclsdescr$ , /* Close To Account Desc*/~
                      step$, lastep$         , /* Close Step Number    */~
                      status$                , /* Account Obsolete Flag*/~
                      statdescr$             , /* Obsolete Desription  */~
                      lauthnbr$              , /* Local Authority Num. */~
                      lauthdsc$                /* Local Authority Desc */
            init(hex(00)) work$(), seq$
            editmode% = 0%
            call "ALLFREE"
            lafac1$, lafac2$ = hex(9c)
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
            call "READ101" (#01, temp$, onfile%)  /* Keep GLPOST2 Out! */
                if onfile% = 0% then return
            get #01, using L30101, description$,type$,status$,seq$,work$()
L30101:     FMT XX(9),         /* Account Number                       */~
                CH(30),        /* Description Of Account               */~
                CH(1),         /* Account Type                         */~
                CH(1),         /* Next Sequences Number                */~
                CH(3),         /* Next GLDETAIL Sequence Number        */~
                2*CH(128)      /* Amounts                              */

            typedescr$ = typedescr$(pos(types$ = type$) + 1%)
            if status$ = hex(01) then status$ = "O"
            if status$ = hex(02) then status$ = "X"
            if status$ <> "O" and status$ <> "X" then status$ = "A"
            gosub describe_status
            call "READ100" (#03, temp$, f1%(3))
                if f1%(3) = 0 then L30270
            get #03, using L30200, clsacct$, step$
L30200:     FMT XX(2), CH(9), CH(2)
            clsdescr$ = " "
            if clsacct$ = " " then L30270
            clsdescr$ = hex(94) & "NOT on file/Inactive"
            call "GLFMT" (clsacct$)
            call "GETCODE" (#01, clsacct$, clsdescr$, 0%, 99, f1%(1))

L30270:     call "READ100" (#02, "SALES1" & str(temp$,,9), f1%(2))
                if f1%(2) = 1 then sales$ = "Y" else sales$ = "N"

        REM Get Local Authority books information
            if dual_books$ <> "Y" then return
            call "READ100" (#12, temp$, f1%(12))   /* GLCROSS2 */
            if f1%(12) = 0% then return
            get #12 using L30335, lauthnbr$
L30335:         FMT POS(10), CH(9)
            call "READ100" (#10, lauthnbr$, f1%(10))   /* GLMAIN2 */
            if f1%(10) <> 0% then get #10 using L30350, lauthdsc$
L30350:         FMT POS(10), CH(30)
            call "READ100" (#13, lauthnbr$, f1%(13))   /* GLCLSET2 */
                if f1%(13) <> 0 then get #13, using L30358, laclsacct$,   ~
                    lastep$
L30358:         FMT XX(2), CH(9), CH(2)
            call "GLFMT2" (lauthnbr$)
            laclsdescr$ = " "
            if laclsacct$ = " " then return
            laclsdescr$ = hex(94) & "NOT on file/Inactive"
            call "GLFMT2" (laclsacct$)
            call "GETCODE" (#10, laclsacct$, laclsdescr$, 0%, 99, f1%(10))
            return

        describe_status
            statdescr$=" "
            if status$="A" then statdescr$="Active"
            if status$="O" then statdescr$="No New Usage"
            if status$="X" then statdescr$="No New Usage, Not Postable"
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        dataput
            if dual_books$ <> "Y" then goto L31150
            if account$ <> " " then goto L31150
                work$() = xor work$()
                call "READ101" (#10, lauthnbr$, f1%(10))
                if f1%(10) = 0% then goto L31530
                get #10 using L31120, work$()
L31120:             FMT POS(45), 2*CH(128)
                delete #10
                goto L31500
L31150:     if status$ <> "O" and status$ <> "X" then status$ = hex(00)
            if status$ = "O" then status$ = hex(01)
            if status$ = "X" then status$ = hex(02)
            write #01, using L31200, account$, description$, type$,       ~
                                   status$, seq$, work$()
L31200:     FMT CH(9),       /* Account Number                         */~
                CH(30),      /* Account Description                    */~
                CH(1),       /* Account Type (A,C,L,R,E)               */~
                CH(1),       /* Account Status Indicator (O,X, or Blnk)*/~
                CH(3),       /* Next GLDETAIL Sequence Number          */~
                2*CH(128)    /* Amounts                                */

            REM Now Write The Close To File...
            if clsacct$ = " " then L31370
            call "GLUNFMT" (clsacct$)
            write #03, using L31320, step$, clsacct$, step$, account$

L31320:     FMT CH(2),       /* Step Number                            */~
                CH(9),       /* Close To Account                       */~
                CH(2),       /* Step Number Again!                     */~
                CH(9)        /* Account Number                         */

L31370:     REM If This Is A Sales Account, Write A Record To SYSFILE2...
            saleskey$ = "SALES1" & str(account$,,9)
            call "DELETE" (#02, saleskey$, 15%)
            if sales$ = "Y" then write #02, using L31440, saleskey$," "," "
            if dual_books$ <> "Y" then return
                saleskey$ = "SALES2" & str(lauthnbr$,,9)
                call "DELETE" (#02, saleskey$, 15%)
                if sales$ = "Y" then write #02, using L31440, saleskey$,  ~
                     " ", " "
L31440:              FMT CH(20), CH(240), CH(240)

        REM Output to the Local Authority files GLCROSS2 & GLMAIN2
            write #12 using L31480, account$, lauthnbr$, account$
L31480:         FMT 3*CH(9)
            work$() = xor work$()
L31500:     call "READ101" (#10, lauthnbr$, f1%(10))
            if f1%(10) <> 0% then get #10 using L31520, fill$()
L31520:         FMT POS(40), 9*CH(29)
L31530:     if f1%(10) = 0                                               ~
                then write #10, using L31200, lauthnbr$, lauthdsc$, type$,~
                    status$, seq$, work$()                               ~
                else rewrite #10 using L31570, lauthdsc$, fill$()
L31570:             FMT POS(10), CH(30), 9*CH(29)

        REM Now Write the Local Authority Close To File (GLCLSET2)
            if laclsacct$ = " " then return
            call "GLUNFM2" (laclsacct$)
            call "READ101" (#13, lauthnbr$, f1%(13))
            if f1%(13) = 0%                                              ~
                then write #13, using L31690, lastep$, laclsacct$,        ~
                    lastep$, lauthnbr$                                   ~
                else rewrite #13, using L31690, lastep$, laclsacct$,      ~
                    lastep$, lauthnbr$

L31690:     FMT CH(2),       /* Step Number                            */~
                CH(9),       /* Close To Account                       */~
                CH(2),       /* Step Number Again!                     */~
                CH(9)        /* Account Number                         */

            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen (with Local Authority).    *~
            *************************************************************

        deffn'101(fieldnr%, e%)
              line2$ = " "
              if fieldnr% = 1 and lastaccount$ <> " " then               ~
                line2$ = "Last Acct Managed: " & lastaccount$ & " " &    ~
                     lastdescr$
              str(line2$,62%) = " GLINPUT: " & str(cms2v$,,8%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40250,         /* Account # (Statutory)*/~
                                L40240,         /* Account Descr (")    */~
                                L40250,         /* Account Type         */~
                                L40250,         /* Sales Account?       */~
                                L40250,         /* Close To Account     */~
                                L40250,         /* Close Step Number    */~
                                L40250,         /* Account Obsolete Flag*/~
                                L40250,         /* Account # (Loc Auth) */~
                                L40240,         /* Account Descr (")    */~
                                L40250,         /* Close To Account (") */~
                                L40250          /* Close Step Number(") */
              goto L40280

L40240:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40250:           lfac$(fieldnr%) = hex(81)             /* Upper Only */
                  if fieldnr% = 8% and e% = 1% then lfac$(9) = hex(80)
                  return
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40280:     accept                                                       ~
               at (01,02), "Manage G/L Chart Of Accounts",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Statutory G/L system:",                      ~
                                                                         ~
               at (06,05), "Statutory Account Number",                   ~
               at (06,30), fac(lfac$( 1)), account$             , ch(12),~
                                                                         ~
               at (07,05), "Account Description",                        ~
               at (07,30), fac(lfac$( 2)), description$         , ch(30),~
                                                                         ~
               at (08,05), "Account Type",                               ~
               at (08,30), fac(lfac$( 3)), type$                , ch(01),~
               at (08,43), fac(hex(8c)), typedescr$             , ch(26),~
                                                                         ~
               at (09,05), "Sales Account?",                             ~
               at (09,30), fac(lfac$( 4)), sales$               , ch(01),~
                                                                         ~
               at (10,05), "Year End 'Close To' Acct",                   ~
               at (10,30), fac(lfac$( 5)), clsacct$             , ch(12),~
               at (10,43), fac(hex(8c)), clsdescr$              , ch(32),~
                                                                         ~
               at (11,05), "Close On Step Number",                       ~
               at (11,30), fac(lfac$( 6)), step$                , ch(02),~
                                                                         ~
               at (12,05), "Account Obsolete Flag",                      ~
               at (12,30), fac(lfac$( 7)), status$              , ch(01),~
               at (12,43), fac(hex(8c)), statdescr$             , ch(32),~
                                                                         ~
               at (14,02), "Local Authority G/L system:",                ~
                                                                         ~
               at (15,05), "Loc.Auth. Account Number",                   ~
               at (15,30), fac(lfac$( 8)), lauthnbr$            , ch(12),~
               at (15,43), fac(lfac$( 9)), lauthdsc$            , ch(30),~
                                                                         ~
               at (16,05), "Year End 'Close To' Acct",                   ~
               at (16,30), fac(lfac$(10)), laclsacct$           , ch(12),~
               at (16,43), fac(hex(8c)), laclsdescr$            , ch(32),~
                                                                         ~
               at (17,05), "Close On Step Number",                       ~
               at (17,30), fac(lfac$(11)), lastep$              , ch(02),~
                                                                         ~
               at (18,05), fac(lafac1$),   lamessag1$           , ch(74),~
               at (19,05), fac(lafac2$),   lamessag2$           , ch(74),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65), "(15)Print Screen",                           ~
               at (23,40), fac(hex(8c)), pf12$                          ,~
               at (24,40), fac(hex(84)), pf32$                          ,~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(000104050c0d0f1020)),                            ~
               key (keyhit%)

               if keyhit% <> 13 then L40830
                  call "MANUAL" ("GLINPUT ")
                  goto L40280

L40830:        if keyhit% <> 15 then L40870
                  call "PRNTSCRN"
                  goto L40280

L40870:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen (without Local Authority). *~
            *************************************************************

        deffn'102(fieldnr%)
              line2$ = " "
              if fieldnr% = 1 and lastaccount$ <> " " then               ~
                line2$ = "Last Acct Managed: " & lastaccount$ & " " &    ~
                     lastdescr$
              str(line2$,62%) = " GLINPUT: " & str(cms2v$,,8%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L41230,         /* Account Number       */~
                                L41220,         /* Account Descr        */~
                                L41230,         /* Account Type         */~
                                L41230,         /* Sales Account?       */~
                                L41230,         /* Close To Account     */~
                                L41230,         /* Close Step Number    */~
                                L41230          /* Account Obsolete Flag*/
              goto L41260

L41220:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L41230:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41260:     accept                                                       ~
               at (01,02), "Manage G/L Chart Of Accounts",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "G/L Account Number",                         ~
               at (06,30), fac(lfac$( 1)), account$             , ch(12),~
                                                                         ~
               at (07,02), "Account Description",                        ~
               at (07,30), fac(lfac$( 2)), description$         , ch(30),~
                                                                         ~
               at (08,02), "Account Type",                               ~
               at (08,30), fac(lfac$( 3)), type$                , ch(01),~
               at (08,43), fac(hex(8c)), typedescr$             , ch(26),~
                                                                         ~
               at (09,02), "Sales Account?",                             ~
               at (09,30), fac(lfac$( 4)), sales$               , ch(01),~
                                                                         ~
               at (10,02), "Close To Account (Year End)",                ~
               at (10,30), fac(lfac$( 5)), clsacct$             , ch(12),~
               at (10,43), fac(hex(8c)), clsdescr$              , ch(32),~
                                                                         ~
               at (11,02), "Close On Step Number",                       ~
               at (11,30), fac(lfac$( 6)), step$                , ch(02),~
                                                                         ~
               at (12,02), "Account Obsolete Flag",                      ~
               at (12,30), fac(lfac$( 7)), status$              , ch(01),~
               at (12,43), fac(hex(8c)), statdescr$             , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65), "(15)Print Screen",                           ~
               at (23,40), fac(hex(8c)), pf12$                          ,~
               at (24,40), fac(hex(84)), pf32$                          ,~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(000104050c0d0f10)),                              ~
               key (keyhit%)

               if keyhit% <> 13 then L41720
                  call "MANUAL" ("GLINPUT ")
                  goto L41260

L41720:        if keyhit% <> 15 then L41760
                  call "PRNTSCRN"
                  goto L41260

L41760:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%, e%)
            errormsg$ = " "
            on fieldnr% gosub L50210,         /* Account # (Statutory)  */~
                              L50520,         /* Account Descr  (")     */~
                              L50590,         /* Account Type           */~
                              L50690,         /* Sales Account?         */~
                              L50780,         /* Close To Account       */~
                              L50910,         /* Close Step Number      */~
                              L51000,         /* Account Obsolete Flag  */~
                              L51080,         /* Account # (Loc Auth)   */~
                              L51310,         /* Account Descr  (")     */~
                              L51360,         /* Close To Account  (")  */~
                              L51490          /* Close Step Number (")  */
            return

L50210: REM TEST FOR G/L Account Number           ACCOUNT$
            init(hex(00)) work$(), seq$
            if account$ =  "?" and dual_books$ <> "Y" then account$ = " "
            if account$ =  "?" and dual_books$ =  "Y" then L50340
            if account$ =  " " and dual_books$ <> "Y" then L50340
            if account$ =  " " and dual_books$ =  "Y" then L50290
            goto L50440

L50290: REM Indicate the modifications will be to the Local Auth account
            description$ = " "
            lafac2$ = hex(84)
            return

L50340: REM Show the GETCODE list of valid accounts
            f1%(1) = -999%
            lafac2$ = hex(9c)
            onfile% = 0%
            account$ = " "
            call "GETCODE" (#01, account$, " ", 0%, 0, f1%(1))
            if f1%(1) <> 0 then L50440
                 errormsg$ = hex(00)
                 return

L50440: REM Validate and "externalize" the account for display
            call "GLVALID" (account$, temp$, errormsg$)
                if errormsg$ <> " " then return
            gosub dataload
            if onfile% = 0% then return
            return clear all
            goto editmode

L50520: REM TEST FOR Account Description          DESCRIPTION$
            if account$ <> " " then goto L50550
                description$ = " " : return
L50550:     if description$ <> " " then return
                errormsg$ = "Sorry, Description may not be blank."
                return

L50590: REM TEST FOR Account Type                 TYPE$
            typedescr$ = " "
            if pos(types$ = type$) > 0% and pos(types$ = type$) < 7%     ~
                then goto L50660
                errormsg$ = "Type is invalid. Enter '$', 'A', 'L', 'C',"&~
                    " 'R', or 'E'"
                return
L50660:     typedescr$ = typedescr$(pos(types$ = type$) + 1%)
            return

L50690: REM TEST FOR Sales Account?               SALES$
            if sales$ = "Y" then L50730
                sales$ = "N"
                return
L50730:     if type$ = "R" or type$ = "E" then return
                errormsg$ = "Only a Revenue or Expense account may be a S~
        ~ales Account"
                return

L50780: REM TEST FOR Close To Account (Year End)  CLSACCT$
            clsdescr$ = " "
            if clsacct$ = " " then step$ = " "
            if clsacct$ = " " then return
            call "GETCODE"(#01, clsacct$, clsdescr$, 0%, 0, u3%)
                if u3% <> 0 then L50870
                errormsg$ = "If a Close To Account is entered, it must be~
        ~ valid"
                return
L50870:         if clsacct$ = account$ then errormsg$ = "Account and Clos~
        ~e To Account may not be the same."
                return

L50910: REM TEST FOR Close On Step Number         STEP$
            if clsacct$ = " " then return
            convert step$ to s%, data goto L50970
            if s% < 1 then L50970
            convert s% to step$, pic(00)
            return
L50970:     errormsg$ = "Step must be between 1 - 99"
            return

L51000: REM TEST FOR Account Obsolete Flag        STATUS$
            if status$ = " " then status$ = "A"
            if pos("AOX" = status$) > 0 then L51050
                errormsg$ = "Please Enter 'A', 'O' (oh), or 'X'"
                return
L51050:     gosub describe_status
            return

L51080: REM TEST FOR G/L Account Number           LAUTHNBR$
            if lauthnbr$ <> " " and lauthnbr$ <> "?" then goto L51170
                lauthnbr$ = " "
                f1%(10) = -999%
                call "GETCODE" (#10, lauthnbr$, lauthdsc$, 0%, 0, f1%(10))
                if f1%(10) <> 0% then goto L51170
                    errormsg$ = "Sorry, you must enter a Local Authorit"&~
                        "y account number."
                    return
L51170:     call "GLVALD2" (lauthnbr$, temp$, errormsg$)
                if errormsg$ <> " " then return
            call "READ100" (#10, temp$, f1%(10))
            lafac1$ = hex(84) : laclsacct$, lastep$ = " "
            if f1%(10) = 0% then goto L51290
            fieldnr% = hi_fieldnr%
                lafac1$ = hex(9c)
                get #10 using L51240, lauthdsc$
L51240:             FMT POS(10), CH(30)
            call "READ100" (#13, temp$, f1%(13))
            if f1%(13) = 0% then goto L51290
                get #13 using L51280, lastep$, laclsacct$
L51280:             FMT CH(2), CH(9)
L51290:     if e% <> 1% then return

L51310: REM TEST FOR Account Description          LAUTHDSC$
            if lauthdsc$ <> " " then return
                errormsg$ = "Sorry, Description may not be blank."
                return

L51360: REM TEST FOR Close To Account (Year End)  LACLSACCT$
            laclsdescr$ = " "
            if laclsacct$ = " " then lastep$ = " "
            if laclsacct$ = " " then return
            call "GETCODE"(#10, laclsacct$, laclsdescr$, 0%, 0, u3%)
                if u3% <> 0 then L51450
                errormsg$ = "If a Close To Account is entered, it must be~
        ~ valid"
                return
L51450:         if laclsacct$ = lauthnbr$ then errormsg$ = "Account and C~
        ~lose To Account may not be the same."
                return

L51490: REM TEST FOR Close On Step Number         LASTEP$
            if laclsacct$ = " " then return
            convert lastep$ to s%, data goto L51550
            if s% < 1 then L51550
            convert s% to lastep$, pic(00)
            return
L51550:     errormsg$ = "Step must be between 1 - 99"
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

        exit_program
            if page% = 0% then L65101
               call "SHOSTAT" ("Printing Audit Trail Report")
               time$ = " " : call "TIME" (time$)
               print : print using L14280, time$  /* 'End of Report' */
               close printer
               call "SETPRNT" (rptid$, " ", 0%, 1%)

L65101:     if keyhit% = 16% then goto L65290

        REM Clean up the local authority set of books. Before deleting a
        REM record, it must have zero balances, have no references in
        REM GLCROSS2, and have had no activity within 90 days.
        REM Of course, we don't do it if dual books is not effective.
            if dual_books$ <> "Y" then goto L65290
            call "SHOSTAT" ("Local Authority file clean-up in process")
            readkey$ = xor readkey$
        clean_up_read
            call "PLOWNEXT" (#10, readkey$, 0%, f1%(10))
            if f1%(10) = 0% then goto L65290
            get #10, using L65160, lauthnbr$, bal()
L65160:         FMT CH(9), POS(45), 32*PD(14,4)
            for i% = 1% to 32%
                if bal(i%) <> 0 then goto clean_up_read
            next i%

        REM All balances are zero ... continue.
            key$ = xor key$
            str(key$,,9) = lauthnbr$
            call "PLOWALTS" (#12, key$, 1%, 9%, f1%(12))
            if f1%(12) <> 0% then goto clean_up_read

        REM There are no cross-references ... continue.
            key$ = lauthnbr$
            str(key$,17,10) = str(_90daysago$) & hex(00000000)
            call "PLOWNEXT" (#11, key$, 16%, f1%(11))
            if f1%(11) <> 0% then goto clean_up_read

        REM No 90-day activity ... continue
            call "DELETE" (#10, lauthnbr$, 9%) /* Delete GLMAIN2  */
            key$ = xor key$ : str(key$,,9) = lauthnbr$
            call "DELETE" (#11, key$, 9%)      /* Delete GLDETAL2 */
            call "DELETE" (#13, lauthnbr$, 9%) /* Delete GLCLSET2 */
            saleskey$ = "SALES2" & str(lauthnbr$,,9)
            call "DELETE" (#02, saleskey$, 15%)
            goto clean_up_read

L65290: REM ... and finally done.
            call "SHOSTAT" ("One Moment Please")
            end
