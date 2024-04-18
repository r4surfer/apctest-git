        REM *************************************************************~
            *                                                           *~
            *   GGG   L       CCC    OOO   DDDD  EEEEE   SSS            *~
            *  G      L      C   C  O   O  D   D E      S               *~
            *  G GGG  L      C      O   O  D   D EEEE    SSS            *~
            *  G   G  L      C   C  O   O  D   D E          S           *~
            *   GGG   LLLLL   CCC    OOO   DDDD  EEEEE   SSS            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLCODES - PRINTS GENERAL LEDGER CHART OF ACCOUNTS.        *~
            *           DOES NOT SHOW BALANCES.                         *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+--------------------WHAT------------------+-WHO-*~
            * 03/17/80 ! ORIGINAL (VERSION 2, NEW G/L LAYOUT)     ! BCW *~
            * 09/30/80 ! ADDITION OF RANGE                        ! TEM *~
            * 03/30/82 ! SHORTENED PAGE LENGTH - OVERPRINTING     ! BEV *~
            * 05/19/82 ! ADDED TERMINAL DISPLAY OF ACCOUNTS       ! ECR *~
            * 11/09/82 ! ADDED SALES ACCOUNT FLAG                 ! ECR *~
            * 05/26/83 ! CORRECTIONS TO SYSFILE2 READ & DELETE    ! GLW *~
            * 08/25/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 08/09/88 ! Dual Books depends on a SYSFILE2 flag.   ! JIM *~
            * 04/13/89 ! Local Authority Accts use GLCLSET2       ! GGO *~
            * 03/31/93 ! PRR 12850. Fixed unfmtd/fmtd compare.    ! JDH *~
            *************************************************************

        dim account$16,                  /* 1 PAGE OF ACCOUNT NUMBERS  */~
            asterisk$21,                 /* Asterisk explanation       */~
            clsacct$16,                  /* Close to account number    */~
            column$79,                   /* Screen column header       */~
            coname$50,                   /* Company name               */~
            descr$30,                    /* PARALLEL ACCOUNT DESCRIPTNS*/~
            date$8,                      /* DATE                       */~
            dual_books$1,                /* Dual books in effect?      */~
            errormsg$79,                 /* Error message              */~
            fac$(2)1,                    /* Screen FACs                */~
            firstacct$16,                /* FIRST GL ACCOUNT IN RANGE  */~
            first$16, last$16,           /* For setting-up RNGHDR      */~
            inputmsg$79,                 /* INPUT MESSAGE FOR SCREEN   */~
            lastacct$16,                 /* LAST GL ACCOUNT IN RANGE   */~
            line$(20)79,                 /* ACCOUNT DISPLAY STRINGS    */~
            line2$79,                    /* Screen line 2              */~
            oldkey$16,                   /* LAST ACCOUNT NUMBER READ.  */~
            pf9$1, retpf9$1, retpf9msg$44,/* RET/PF9 handling          */~
            rptid$6,                     /* Report ID                  */~
            rnghdr$50,                   /* Prints range selected      */~
            sales$1,                     /* SALES ACCOUNT FLAG (Y, )   */~
            set$1, setdescr$30,          /* Set of books to use        */~
            setmsg$35,                   /* Screen message for SET     */~
            step$2,                      /* Step to close on           */~
            subhdr$50,                   /* Sub header                 */~
            time$8,                      /* Time of day                */~
            typedescr$14,                /* ACCOUNT TYPE DESCRIPTION   */~
            typedescr$(8)14,             /* POSSIBLE TYPES TO CHOOSE FR*/~
            type$1                       /* ACCOUNT TYPE.              */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD ON FILE STATUS FLAGS*/~
            rslt$(64)20                  /* RETURN CODE FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * #02 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            * #03 ! SYSFILE2 ! SYSTEM INFORMATION...SALES ACCOUNTS      *~
            * #04 ! GLCLSETO ! CLOSE TO ACCOUNT FILE                    *~
            * #05 ! GLCLSET2 ! CLOSE TO ACCOUNT FILE FOR LOCAL AUTHORITY*~
            * #10 ! GLMAIN2  ! G. L. chart of accounts for local auth.  *~
            *************************************************************

            select #02, "GLMAIN",                                        ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 300,                                    ~
                       keypos = 1, keylen = 9

            select  #03, "SYSFILE2",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select  #04, "GLCLSETO",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 22,                                    ~
                        keypos = 14, keylen = 9 ,                        ~
                        alt key 1, keypos = 12, keylen = 11,             ~
                            key 2, keypos =  3, keylen =  9, dup,        ~
                            key 3, keypos =  1, keylen = 22

            select  #05, "GLCLSET2",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 22,                                    ~
                        keypos = 14, keylen = 9 ,                        ~
                        alt key 1, keypos = 12, keylen = 11,             ~
                            key 2, keypos =  3, keylen =  9, dup,        ~
                            key 3, keypos =  1, keylen = 22

            select #10, "GLMAIN2",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            call "SHOSTAT" ("Opening files; one moment please")

            call "OPENCHCK" (#02, 0%, f2%( 2), 100%, rslt$( 2))
            call "OPENCHCK" (#03, 0%, f2%( 3), 100%, rslt$( 3))
            call "OPENCHCK" (#04, 0%, f2%( 4), 100%, rslt$( 4))
            dual_books$ = "N"                        /* Default to 'no' */
            call "READ100" (#03, "SWITCHS.GL", f1%(3))
                if f1%(3) = 0% then goto L01360
            get #03 using L01310, dual_books$
L01310:         FMT POS(21), CH(1)
            if dual_books$ <> "Y" then goto L01360
                call "OPENCHCK" (#10, 0%, f2%(10), 100%, rslt$(10))
                call "OPENCHCK" (#05, 0%, f2%( 5), 100%, rslt$( 5))

L01360: REM *************************************************************~
            *              I N I T I A L I Z A T I O N                  *~
            *                                                           *~
            *************************************************************

            typedescr$(1) = "* Unknown *"
            typedescr$(2) = "(Cash)"
            typedescr$(3) = "(Asset)"
            typedescr$(4) = "(Liability)"
            typedescr$(5) = "(Capital)"
            typedescr$(6) = "(Revenue)"
            typedescr$(7) = "(Expense)"

            date$ = date : call "DATEFMT" (date$)
            column$ = "Account #    Description                    Type"&~
                "             * Close To #/Step"
            asterisk$ = "* 'Y' = Sales Account"
            rptid$ = "G/L008"
            retpf9msg$ = "(RETURN)Display Chart --or-- (9)Print Chart" & ~
                hex(8c)

            call "EXTRACT" addr("TT", tasktype$)
            call "COMPNAME" (12%, coname$, u3%)

            if dual_books$ =  "Y" then setmsg$ = "Enter code for set of"&~
                " books to use:"

        REM *************************************************************~
            *       I N P U T   R A N G E   T O   S E E                 *~
            *                                                           *~
            * INPUTS THE RANGE OF GENERAL LEDGER ACCOUNTS TO DISP/PRINT *~
            * PRINTS THE ENTIRE CHART IF IN BACKGROUND MODE.            *~
            *************************************************************

            if tasktype$ = "B" then L02010

        inputmode
           init(" ") errormsg$, lastacct$, inputmsg$, set$, setdescr$,   ~
                line$(), first$, last$
           firstacct$ = "ALL"

            for x% = 1% to 2%
L01780:         gosub L03600
                    if keyhit%  =  1 then gosub startover
                    if keyhit%  = 16 then L05560
                    if keyhit% <>  9 and keyhit% <> 0 then L01780
                gosub L04880
                    if errormsg$ <> " "  then L01780
                    if x% < 2% then goto L01870
                    if keyhit%  =  0 then displaymode
                    if keyhit%  =  9 then printmode
L01870:     next x%

        REM *************************************************************~
            *         C O N T R O L   P R I N T I N G                   *~
            *                                                           *~
            * PRINTS THE ENTIRE CHART IF IN BACKGROUND MODE.            *~
            *************************************************************

        printmode
            convert set to set$, pic (#)
            pageline% = 1000 : pg% = 0% : counter%  = 0
            call "SHOSTAT" ("Printing Chart of Accounts")
            go to L02390

L02010: REM SETS RANGE FOR BACKGROUND MODE.
            init(hex(00)) firstacct$
            init(hex(ff)) lastacct$
            go to L02390

        REM *************************************************************~
            *     M A I N   P R O G R A M   F O R   D I S P L A Y       *~
            *                                                           *~
            * READS THE ACCOUNT INFO INTO ARRAYS. IF DONE WITH 20       *~
            * DSIPLAY LINES, DISPLAY THEM.  ALLOW USER TO GO UP AND DOWN*~
            * OR PRINT THE WHOLE THING.                                 *~
            *************************************************************

        displaymode
            convert set to set$, pic (#)
            displayline% = 1
            gosub start_n_end
L02180:     gosub get_next_account
            if f1%(main%) = 0% then goto L02260
            if account$ > lastacct$ then L02260
            gosub format_accounts
            put line$(displayline%) using L05510 , account$, descr$, type$,~
                typedescr$, sales$, clsacct$, step$
            gosub L04110                              /* Screen displayer */
            goto L02180

L02260: REM Exit plow routine--gets out, displays unfilled array, etc... ~
            ... If there's stuff yet to display, do so.
            if displayline% = 1 then gosub L02330   /* SORRY, NO DETL*/
                displayline% = 1000
                gosub L04110                          /* Screen displayer */
                goto inputmode

L02330: REM On exit, this sets up for no details this account
            init(" ")line$()
            tempmsg$ = "NO DETAILS"
            str(line$(8),40-len(tempmsg$)/2)=tempmsg$
            return

L02390: REM *************************************************************~
            *                   M A I N   P R O G R A M                 *~
            *                                                           *~
            * INITIALIZES THE PAGE... READS INTO ARRAYS THE ACCOUNT JUNK*~
            * IF DONE WITH THE ARRAY, PRINTS IT OUT.  PRINTS OUT IF THE *~
            * END OF FILE HAPPENS BEFORE THE PAGE IS FULL.              *~
            *************************************************************

            select printer (87)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            time$ = " " : call "TIME" (time$)
            subhdr$ = "GENERAL LEDGER CHART OF ACCOUNTS"
            call "FMTTITLE" (subhdr$, setdescr$, 2%)
            rnghdr$ = "RANGE:"
            if first$ <> "ALL" then goto L02550
                rnghdr$ = rnghdr$ & " ALL" : goto L02570
L02550:     rnghdr$ = rnghdr$ & " " & first$
            if last$ <> " " then rnghdr$ = rnghdr$ & " TO " & last$
L02570:     call "FMTTITLE" (rnghdr$, " ", 2%)
            gosub start_n_end
L02590:     gosub get_next_account
            if f1%(main%) = 0% then goto L02830
            if account$ > lastacct$ then L02830
            gosub L02680                                 /* Page control */
            gosub format_accounts
            print using L05490 , account$, descr$, type$, typedescr$,      ~
                sales$, clsacct$, step$
            counter% = counter% + 1  /* TOTAL NUMBER OF ACCOUNTS   */
            goto L02590

L02680: REM Page control subroutine.
            pageline% = pageline% + 1
            if pageline% < 54 then return
                pg% = pg% + 1
                print page
                print using L05390 , date$, coname$, "-" & rptid$
                print using L05410 , time$, subhdr$, pg%
                print using L05430 , rnghdr$
                print
                print using L05450 , "#", "#"
                print using L05470
                print
                pageline% = 7
                return

L02830: REM Exit printing (if from background, exit)
            gosub L02680                                 /* PAGE CONTROL */
            print
            print using L05530 , counter%
            if tasktype$ = "B" then goto L05560
                close printer
                call "SETPRNT" (rptid$, " ", 0%, 1%)
                goto inputmode

        start_n_end
            if firstacct$ <= hex(00) then goto L02970
                if set = 1                                               ~
                    then call "GLUNFMT" (firstacct$)                     ~
                    else call "GLUNFM2" (firstacct$)
L02970:     if lastacct$ >= hex(ff)  then goto L03010
                if set = 1                                               ~
                    then call "GLUNFMT" (lastacct$)                      ~
                    else call "GLUNFM2" (lastacct$)
L03010:     oldkey$ = firstacct$
            return

        get_next_account
        REM Loop that reads the accounts in and passes data back to caller
            call "READ102" (#main%, oldkey$, f1%(main%))
            if f1%(main%) = 0 then return
            get #main%, using L03090 , account$, descr$, type$
L03090:         FMT CH(9), CH(30), CH(1)
            if account$ > lastacct$ then return
            oldkey$ = account$
            typedescr$ = typedescr$(pos("$ALCRE" = type$) + 1%)

        REM Set sales account flag field.
            call "READ100" (#03, "SALES" & set$ & str(account$,1,9) &    ~
                " ", f1%(3))
            if f1%(3) = 1 then sales$ = "Y" else sales$ = " "
            step$, clsacct$ = " "
            if set = 1                                                   ~
                then call "READ100" (#04, account$, f1%(4))              ~
            else                                                         ~
                call "READ100" (#05, account$, f1%(5))
            if set = 1 and f1%(4) <> 0                                   ~
                then get #04, using L03260 , step$, clsacct$
            if set = 2 and f1%(5) <> 0                                   ~
                then get #05, using L03260 , step$, clsacct$
L03260:     FMT CH(2), CH(9)
            return

        format_accounts
            if set = 2 then goto L03310
                call "GLFMT" (account$)
                call "GLFMT" (clsacct$)
                return
L03310:     call "GLFMT2" (account$)
            call "GLFMT2" (clsacct$)
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

L03600: REM *************************************************************~
            *         D E F I N E   R A N G E   T O   S E E             *~
            *                                                           *~
            * DEFINES A RANGE TO DISPLAY/PRINT FOR FUN AND GOOD TIMES.  *~
            *************************************************************

            line2$ = " "
            str(line2$,63%) = "GLCODES: " & str(cms2v$,,8%)
            init(hex(8c)) fac$()
            on x% goto L03700 , L03770
L03700:     keyhit% = 0%
            if dual_books$ <> "Y" then return
            inputmsg$ = "Enter '1' to use Statutory books; '2' to use L"&~
                "ocal Authority books"
            fac$(1) = hex(82)
            retpf9$ = hex(9c) : pf9$ = hex(ff)
            goto L03810
L03770:     inputmsg$ = "Enter range of accounts or 'ALL', then press ("&~
                "RETURN) --or-- PF(9)."
            fac$(2) = hex(81)
            retpf9$ = hex(8c) : pf9$ = hex(09)
L03810:     accept                                                       ~
               at (01,02), "Display/Print Chart of Accounts",            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(8c)), setmsg$                , ch(35),~
               at (05,42), fac(fac$(1)), set$                   , ch(01),~
               at (05,44), fac(hex(8c)), setdescr$              , ch(30),~
               at (07,02), "Beginning Account #",                        ~
               at (07,42), fac(fac$(2)), firstacct$             , ch(12),~
               at (08,02), "Ending Account #",                           ~
               at (08,42), fac(fac$(2)), lastacct$              , ch(12),~
               at (21,02), fac(hex(a4)), inputmsg$              , ch(79),~
               at (22,02), fac(retpf9$), retpf9msg$             , ch(44),~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), "(1)Start Over",                              ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)EXIT PROGRAM",                           ~
                                                                         ~
               keys(hex(00010d0f10) & pf9$), key (keyhit%)

               if keyhit% <> 13 then L04070
                  call "MANUAL" ("GLCODES")
                  return

L04070:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

L04110: REM *************************************************************~
            *         DISPLAY CONTROL FOR CHART OF ACCOUNTS             *~
            *                                                           *~
            *************************************************************

            displayline% = displayline% + 1
            if displayline% <= 20 then return
L04180:        gosub L04410                /* AND JUST SHOW SCREEN AS IS */
                     if keyhit%  =  1 then gosub startover
                     if keyhit%  =  2 then       L04260
                     if keyhit%  =  5 then       L04290
                     if keyhit%  = 14 then       L04340
                     if keyhit%  = 16 then       L04370
                     goto L04180           /* FOR STARTOVER, CRAZY KEYHIT*/

L04260:        REM HANDLES CASE WHERE WE WANT TO GO TO FIRST ACCOUNT
                   return clear
                   goto displaymode      /* AND DO FIRST ACCOUNT.      */
L04290:        REM HANDLES CASE WHERE S/HE WANTS NEXT PAGE (15 LINES)
                   displayline% = 6      /* SET LINE COUNTER           */
                   copy str(line$(),1186) to str(line$(),1)
                   init(" ") str(line$(),396)
                   return
L04340:        REM HANDLES CASE WHERE S/HE WANTS TO PRINT THING
                   return clear
                   goto printmode       /* GOES TO PRINT MODE         */
L04370:        REM HANDLES CASE WHER USER WANTS TO SEE A NEW RANGE
                   return clear
                   goto inputmode

L04410: REM *************************************************************~
            *        D I S P L A Y   D E T A I L S   S C R E E N        *~
            *                                                           *~
            * DISPLAYS GENERAL LEDGER TRANSACTION DETAILS FOR THE ACCT  *~
            * NUMBER SPECIFIED.                                         *~
            *************************************************************

L04480:     accept                                                       ~
               at (01,02), "General Ledger Chart of Accounts",           ~
               at (01,35), fac(hex(8c)), setdescr$              , ch(30),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), column$                , ch(79),~
               at (03,02), fac(hex(8c)), line$( 1)              , ch(79),~
               at (04,02), fac(hex(8c)), line$( 2)              , ch(79),~
               at (05,02), fac(hex(8c)), line$( 3)              , ch(79),~
               at (06,02), fac(hex(8c)), line$( 4)              , ch(79),~
               at (07,02), fac(hex(8c)), line$( 5)              , ch(79),~
               at (08,02), fac(hex(8c)), line$( 6)              , ch(79),~
               at (09,02), fac(hex(8c)), line$( 7)              , ch(79),~
               at (10,02), fac(hex(8c)), line$( 8)              , ch(79),~
               at (11,02), fac(hex(8c)), line$( 9)              , ch(79),~
               at (12,02), fac(hex(8c)), line$(10)              , ch(79),~
               at (13,02), fac(hex(8c)), line$(11)              , ch(79),~
               at (14,02), fac(hex(8c)), line$(12)              , ch(79),~
               at (15,02), fac(hex(8c)), line$(13)              , ch(79),~
               at (16,02), fac(hex(8c)), line$(14)              , ch(79),~
               at (17,02), fac(hex(8c)), line$(15)              , ch(79),~
               at (18,02), fac(hex(8c)), line$(16)              , ch(79),~
               at (19,02), fac(hex(8c)), line$(17)              , ch(79),~
               at (20,02), fac(hex(8c)), line$(18)              , ch(79),~
               at (21,02), fac(hex(8c)), line$(19)              , ch(79),~
               at (22,02), fac(hex(ac)), line$(20)              , ch(79),~
               at (23,02), "(1)Start Over",                              ~
               at (23,22), "(5)Next Screen",                             ~
               at (23,45), "(14)Print",                                  ~
               at (23,64), "(15)Print Screen",                           ~
               at (24,02), "(2)First Screen",                            ~
               at (24,22), fac(hex(8c)), asterisk$              , ch(21),~
               at (24,64), "(16)Next Accounts",                          ~
                                                                         ~
               keys(hex(0102050e0f10)), key (keyhit%)

               if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L04480

L04880: REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            *************************************************************

            errormsg$ = " "
            on x% goto L04950 , L05060
L04950: REM Validate code for which set of books to use
            set = 1 : main% = 2% /* Default values */
            if dual_books$ <> "Y" then return
            call "NUMTEST" (set$, 1, 2, errormsg$, 0, set)
            if errormsg$ <> " " then return
            setdescr$ = "(Statutory)"
            if set = 1 then return
                main% = 10%
                setdescr$ = "(Local Authority)"
                return

L05060: REM Handles case for "ALL" accounts
            first$ = firstacct$ : last$ = lastacct$
            if firstacct$ <> "ALL" then L05130
                init(hex(00)) firstacct$
                init(hex(ff)) lastacct$
                return

L05130: REM Handles case for single account
            if lastacct$ = " " then lastacct$ = firstacct$

        REM Handles case for a range of accounts.
            if lastacct$ < firstacct$ then L05300
            if set = 1                                                   ~
                then call "GLVALID" (firstacct$, "         ", errormsg$) ~
                else call "GLVALD2" (firstacct$, "         ", errormsg$)
            if errormsg$ <> " " then return
            if set = 1                                                   ~
                then call "GLVALID" (lastacct$, "         ", errormsg$)  ~
                else call "GLVALD2" (lastacct$, "         ", errormsg$)
            if errormsg$ <> " " then return
                y% = len(firstacct$)
                str(firstacct$,,y%) = str(firstacct$,,y%)                ~
                     addc all(hex(ff))
                return
L05300:     REM HANDLES ERROR MESSAGE -- LAST < FIRST.
                 errormsg$ = "Beginning Account Number may not be great"&~
                    "er than Ending Account Number."
                return

        REM *************************************************************~
            *    I M A G E   S T A T E M E N T S   F O R   P R I N T    *~
            *************************************************************

L05390: %DATE: ########   ###############################################~
        ~###    GLCODES#######
L05410: %TIME: ########   ###############################################~
        ~###        PAGE: ####
L05430: %                 ###############################################~
        ~###
L05450: %ACCOUNT #    DESCRIPTION                    TYPE              SA~
        ~LES CLOSE TO # & STEP
L05470: %------------ ------------------------------ ----------------  --~
        ~--- -----------------
L05490: %############ ############################## # ##############    ~
        ~#   ############  ##
L05510: %############ ############################## # ############## # #~
        ~########### ##
L05530: %*** END OF LISTING ***          NUMBER OF ACCOUNTS PRINTED: ####~
        ~######

L05560: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * DISPLAYS A MESSAGE (ONLY IN FOREGROUND) WHILE LINKING     *~
            * TO THE NEXT PROGRAM.                                      *~
            *************************************************************

            call "SHOSTAT" ("One moment please")
            end
