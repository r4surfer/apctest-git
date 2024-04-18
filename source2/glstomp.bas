        REM *************************************************************~
            *                                                           *~
            *   GGG   L       SSS   TTTTT   OOO   M   M  PPPP           *~
            *  G      L      S        T    O   O  MM MM  P   P          *~
            *  G GGG  L       SSS     T    O   O  M M M  PPPP           *~
            *  G   G  L          S    T    O   O  M   M  P              *~
            *   GGG   LLLLL   SSS     T     OOO   M   M  P              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLSTOMP  - PROGRAM PERMITS ADJUSTMENT OF G/L BALANCES     *~
            *            DIRECTLY W/O BENEFIT OF AN INPUT PROGRAM.      *~
            *            BE CAREFUL WITH THIS ROUTINE AS IT LEAVES NO   *~
            *            VISIBLE AUDIT TRAIL.                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/11/80 ! ORIGINAL                                 ! BCW *~
            * 11/08/83 ! REVISED TO CONFORM TO NEW LEDGER         ! HES *~
            * 03/04/86 ! Change for unformatted Fiscal Dates      ! ERN *~
            * 05/22/87 ! Support Obsolete Account Indicator       ! HES *~
            * 09/21/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 08/10/88 ! Dual Books depends on a SYSFILE2 flag.   ! JIM *~
	    * 06/25/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************


        dim account$16,                  /* ACCOUNT NUMBER             */~
            accountdescr$32,             /* ACCOUNT DESCRIPTION        */~
            activity$(32)12,             /* PERIOD ACTIVITY (ON FILE)  */~
            activity(32),                /* PERIOD ACTIVITY (ON FILE)  */~
            balance$(32)12,              /* SCREEN FORMATED BALANCES   */~
            balance(32),                 /* BALANCES TO/FROM DISK      */~
            blankdate$8,                 /* Blank date for comparison  */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE STRING                */~
            dates$(32)8,                 /* FISCAL PERIOD DATES        */~
            description$30,              /* ACCOUNT DESCR FROM FILE    */~
            dual_books$1,                /* Dual books in effect?      */~
            edtmessage$79,               /* "TO MODIFY DISPLAYED..."   */~
            errormsg$79,                 /* ERROR MESSAGE STRING       */~
            fac$(34)1,                   /* FIELD ATTRIBUTE CHARACTERS */~
            heading$79,                  /* HEADING FOR SCREEN         */~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            inpmessage$79,                                               ~
            lastaccount$16,              /* LAST ACCOUNT DONE.         */~
            line2$79, line2fac$1,                                        ~
            pf16$16,                     /* PF 16 MESSAGE              */~
            pf4$16, pf4fac$1, pf4key$1,  /* PF  4 MESSAGE              */~
            pf8$20, pf8fac$1, pf8key$1,  /* PF  8 MESSAGE              */~
            set$1, setdescr$7,           /* Set of books to use        */~
            setmsg$11,                   /* Screen message for SET     */~
            type$1                       /* TYPE STRING                */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY PLAY ARE AN INTRINSIC PART    */
                     /* OF THE FILE OPEN ROUTINE.                      */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * #01 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            * #03 ! SYSFILE2 ! SYSTEM INFORMATION (MONTHS OPEN LIST)    *~
            * #10 ! GLMAIN2  ! G. L. chart of accounts for local auth.  *~
            *************************************************************

            select #01, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1,                                      ~
                        keylen = 9

            select  #03, "SYSFILE2",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #10, "GLMAIN2",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            call "SHOSTAT" ("Linking to Data Base for Direct Account Bala~
        ~nce Management")
            call "OPENFILE" (#01, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
            call "OPENFILE" (#03, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            dual_books$ = "N"                        /* Default to 'no' */
            blankdate$ = " "
            call "DATUFMTC" (blankdate$)
            call "READ100" (#03, "SWITCHS.GL", f1%(3))
                if f1%(3) = 0% then goto L09000
            get #03 using L02410, dual_books$
L02410:         FMT POS(21), CH(1)
            if dual_books$ <> "Y" then goto L09000
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * HAS TO SET UP PROMPTS FOR ACCOUNT MONTHS TO CHANGE.       *~
            *************************************************************

            if dual_books$ = "Y" then lo% = 1% else lo% = 2%
            if dual_books$ = "Y" then setmsg$ = "G/L System:"
            date$ = date : call "DATEFMT" (date$)
            pf4$ = "(4)Prev Field"
            pf8$ = "(8)Same as Prev Bal"

        REM FIND OUT WHICH MONTH IS CURRENTLY MOST RECENT OPEN.
            call "READ100" (#03, "FISCAL DATES", f1%(3))
                if f1%(3) <> 0 then L09160
            ask% = 2%
            call "ASKUSER" (ask%, "***** FISCAL DATE ERROR *****",       ~
                "An error has occurred in Finding the Fiscal Dates",     ~
                "Press (RETURN) to EXIT this program",                   ~
                "And Correct the Problem")
            goto L65000

L09160:     get #03, using L09170, dates$()
L09170:         FMT XX(20), XX(2), XX(138), 32*CH(8)
            for d% = 1% to 32%
                if d% <> 15% then call "DATEFMT" (dates$(d%))
            next d%
            dates$(1%) = "BAL.FWD."

            edtmessage$ = "To modify BALANCES, position cursor to desir"&~
                "ed balance, then press (RETURN)"
               heading$ =          "Prior Year    Activity       Balance ~
        ~ Current Year  Activity       Balance"

        REM *************************************************************~
            *          I N P U T   A C C O U N T   T O   Z A P          *~
            *                                                           *~
            * GETS THE ACCOUNT NUMBER WE ARE GOING TO CHANGE.           *~
            *************************************************************

        inputmode
            init(" ") errormsg$, account$, accountdescr$, activity$(),   ~
                balance$(), set$, setdescr$
            edit% = 0% : set = 1 : main% = 1% /* Set defaults */
            for fieldnr% = lo% to 34%
L10101:         gosub check_pf4
                gosub check_pf8
                if fieldnr% < 3%                                         ~
                    then pf16$ = "(16)EXIT PROGRAM"                      ~
                    else pf16$ = "(16)EDIT MODE"
                if fieldnr% < 3 then goto L10160
                if dates$(fieldnr%-2%) = " " or ~
		   dates$(fieldnr%-2%) = blankdate$ then L10360
L10160:         gosub'200(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% < 3% then L65000
                      if keyhit%  = 16 then L10330
                      if keyhit% <>  4 then L10250
                         if fieldnr% < 3% then L10160
L10220:                  fieldnr% = max(3%, fieldnr% - 1%)
                         if dates$(fieldnr%-2%) = " " or ~
                            dates$(fieldnr%-2%) = blankdate$ then L10220
                         goto L10101
L10250:               if keyhit% <>  8 then L10280
                         gosub same_as_prev_balance
                         goto L10330
L10280:               if keyhit% <>  9 then L10320
                         mat activity = zer
                         gosub L30250
                         goto L10160
L10320:               if keyhit% <>  0 then L10160
L10330:         gosub'150(fieldnr%)
                      if errormsg$ <> " " then L10160
                      if keyhit%  = 16 then editmode
L10360:         next fieldnr%
                goto editmode

        same_as_prev_balance
            prevsubscript%, subscript% = fieldnr% - 2%
L10410:     prevsubscript% = prevsubscript% - 1%
            if prevsubscript% < 1% then return
            if dates$(prevsubscript%) = " " or ~
               dates$(prevsubscript%) = blankdate$ then L10410
            balance$(subscript%) = balance$(prevsubscript%)
            return

        check_pf4
            if fieldnr% < 4% then goto L10520
                pf4key$ = hex(04)
                pf4fac$ = hex(8c)
                return
L10520:     pf4key$ = hex(ff)
            pf4fac$ = hex(9c)
            return

        check_pf8
            if fieldnr% < 4% then goto L10610
                pf8key$ = hex(08)
                pf8fac$ = hex(8c)
                return
L10610:     pf8key$ = hex(ff)
            pf8fac$ = hex(9c)
            return

        REM *************************************************************~
            *             E D I T   N E W   B A L A N C E S             *~
            *                                                           *~
            * EDITS THE VALUES WE JUST INPUT.                           *~
            * NOTE THAT SINCE WE ARE NOT CREATING DOCUMENTS, WE DO NOT  *~
            * EDIT THE ACCOUNT NUMBER!!! (WE WANT TO MAKE THIS AS HARD  *~
            * AS POSSIBLE TO DO...)                                     *~
            *************************************************************

        editmode
            errormsg$ = " "
            edit% = 1%
            pf16$ = "(16)SAVE DATA"
            pf4key$ = hex(ff) : pf4fac$ = hex(9c)
            inpmessage$ = edtmessage$
            fieldnr% = 0% : gosub check_pf4 : gosub check_pf8
L11150:     gosub'200(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then datasave
                  if keyhit% <>  9 then L11220
                     mat activity = zer
                     gosub L30250
                     goto L11150
L11220:           if keyhit% <>  0 then editmode
            if cursor%(1) <  5% or  cursor%(1) >  21% then goto editmode
            if cursor%(1) = 17% and cursor%(2) >  40% then goto editmode
            if cursor%(1) = 18% and cursor%(2) <= 40% then goto editmode
            if cursor%(1) = 20% and cursor%(2) <= 40% then goto editmode
            if cursor%(1) = 21% and cursor%(2) <= 40% then goto editmode
            fieldnr% = cursor%(1) - 2%
            if cursor%(2) > 40 then fieldnr% = fieldnr% + 15%
            if fieldnr%   <   3 or fieldnr%   > 34 then L11150
            if dates$(fieldnr%-2) = " " or ~
               dates$(fieldnr%-2) = blankdate$ then L11150
            gosub check_pf4 : gosub check_pf8
L11280:     gosub'200(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  8 then L11330
                     gosub same_as_prev_balance
                     goto L11380
L11330:           if keyhit% <>  9 then L11370
                     mat activity = zer
                     gosub L30250
                     goto L11150
L11370:           if keyhit% <>  0 then L11280
L11380:     gosub'150(fieldnr%)
                  if errormsg$ <> " " then L11280
            goto editmode

        REM *************************************************************~
            *  W R I T E   D A T A   T O   F I L E   F R O M   H E R E  *~
            *                                                           *~
            *************************************************************

        datasave
            lastaccount$ = account$
            gosub L31000
            goto inputmode

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *    L O A D S   A N   A C C O U N T   F R O M   F I L E    *~
            *                                                           *~
            * LOADS THE ACCOUNT NUMBER FROM THE FILE.                   *~
            *************************************************************

            temp$ = account$
            if set = 1                                                   ~
                then call "GLUNFMT" (temp$)                              ~
                else call "GLUNFM2" (temp$)
            call "READ101" (#main%, temp$, f1%(main%))
                if f1%(main%) = 0 then return
            get #main%, using L30510, description$,type$,seqnr$,activity()

            gosub L30250
            return

L30250: REM COMPUTE & DISPLAY BALANCE FROM ACTIVITY
            for temp% = 1 to 32
                if dates$(temp%) = " " or ~
                   dates$(temp%) = blankdate$ then activity(temp%) = 0
                if temp% > 1% then L30350
                    balance(1%), activity(1%) = round(activity(1%),2)
                    goto L30380
L30350:         activity(temp%) = round(activity(temp%),2)
                balance(temp%) = round(balance(temp%-1) +                ~
                    activity(temp%), 2)
L30380:         if dates$(temp%) = " " or ~
                   dates$(temp%) = blankdate$ then L30440
                    convert balance(temp%) to balance$(temp%),           ~
                        pic(-########.##)
                    convert activity(temp%) to activity$(temp%),         ~
                        pic(-########.##)
                    goto L30470
L30440:         balance$(temp%), activity$(temp%) = " "
                activity(temp%) = 0
L30470:     next temp%
            return

L30510:     FMT XX(9),                   /* SKIP ACCOUNT NUMBER        */~
                CH(30),                  /* DESCRIPTION                */~
                CH(1),                   /* ACCOUNT TYPE               */~
                CH(4),                   /* OBSOLETE FLAG & SEQUENCE # */~
                32*PD(14,4)              /* BALANCES                   */

L31000: REM *************************************************************~
            *  W R I T E   I N F O R M A T I O N   T O   A C C O U N T  *~
            *                                                           *~
            * WRITES INFORMATION TO FILE THAT WE'VE EDITED IN THE PAST. *~
            *************************************************************

            for temp% = 1 to 32
                if activity$(temp%) = " " then activity$(temp%) = "0"
                if dates$(temp%) = " " or ~
                   dates$(temp%) = blankdate$ then activity$(temp%) = "0"
                convert activity$(temp%) to activity(temp%)
            next temp%

            if set = 1                                                   ~
                then call "GLUNFMT" (account$)                           ~
                else call "GLUNFM2" (account$)
            rewrite #main%, using L31260, account$, description$, type$,  ~
                seqnr$, activity()
            return

L31260:     FMT CH(9),                   /* ACCOUNT NUMBER             */~
                CH(30),                  /* DESCRIPTION                */~
                CH(1),                   /* ACCOUNT TYPE               */~
                CH(4),                   /* OBSOLETE FLAG & SEQUENCE # */~
                32*PD(14,4)              /* BALANCES                    */

        REM *************************************************************~
            *             I N P U T   M O D E   S C R E E N             *~
            *                                                           *~
            * GETS THE ACCOUNT NUMBER TO BE MODIFIED.                   *~
            *************************************************************

            deffn'200(fieldnr%)
            if fieldnr% = 0% then init(hex(86)) fac$()                   ~
                        else init(hex(84)) fac$()
            if dual_books$ <> "Y" then fac$(1) = hex(9c)
                  on fieldnr% gosub L40460,         /* G/L set of books */~
                                    L40490,         /* ACCOUNT #        */~
                                    L40520,         /* BALANCE 1        */~
                                    L40520,         /* BALANCE 2        */~
                                    L40520,         /* BALANCE 3        */~
                                    L40520,         /* BALANCE 4        */~
                                    L40520,         /* BALANCE 5        */~
                                    L40520,         /* BALANCE 6        */~
                                    L40520,         /* BALANCE 7        */~
                                    L40520,         /* BALANCE 8        */~
                                    L40520,         /* BALANCE 9        */~
                                    L40520,         /* BALANCE 10       */~
                                    L40520,         /* BALANCE 11       */~
                                    L40520,         /* BALANCE 12       */~
                                    L40520,         /* BALANCE 13       */~
                                    L40520,         /* BALANCE 14       */~
                                    L40520,         /* BALANCE 15       */~
                                    L40520,         /* BALANCE 16       */~
                                    L40520,         /* BALANCE 17       */~
                                    L40520,         /* BALANCE 18       */~
                                    L40520,         /* BALANCE 19       */~
                                    L40520,         /* BALANCE 20       */~
                                    L40520,         /* BALANCE 21       */~
                                    L40520,         /* BALANCE 22       */~
                                    L40520,         /* BALANCE 23       */~
                                    L40520,         /* BALANCE 24       */~
                                    L40520,         /* BALANCE 25       */~
                                    L40520,         /* BALANCE 26       */~
                                    L40520,         /* BALANCE 27       */~
                                    L40520,         /* BALANCE 28       */~
                                    L40520,         /* BALANCE 29       */~
                                    L40520,         /* BALANCE 30       */~
                                    L40520,         /* BALANCE 31       */~
                                    L40520          /* BALANCE 32       */
                  goto L40650

L40460:     inpmessage$ = "Enter '1' for Statutory G/L set of books; '2"&~
                "' for Local Authority."
            goto L40610
L40490:     inpmessage$ = "Enter Acct Number to modify or blanks to see"&~
                " list"
            goto L40580
L40520:     inpmessage$ = "Enter the desired balance for this period"
            goto L40610

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(fieldnr%) = hex(80)
                      return
L40580:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(fieldnr%) = hex(81)
                      return
L40610:           REM SET FAC'S FOR NUMERIC INPUT.
                      fac$(fieldnr%) = hex(82)
                      return

L40650:     line2$ = " "
            if errormsg$ = " " then goto L40700
                line2fac$ = hex(94)
                line2$ = errormsg$
                goto L40720
L40700:     str(line2$,63) = "GLSTOMP: " & str(cms2v$,,8)
            line2fac$ = hex(ac)
L40720:     accept                                                       ~
               at (01,02), "Direct Edit of G/L Accounts",                ~
               at (01,35), "Last Account:",                              ~
               at (01,49), fac(hex(8c)), lastaccount$           , ch(12),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(line2fac$), line2$               , ch(79),~
                                                                         ~
               at (03,02), fac(hex(8c)),  setmsg$               , ch(11),~
               at (03,14), fac(fac$( 1)), set$                  , ch(01),~
               at (03,16), fac(hex(8c)),  setdescr$             , ch(07),~
                                                                         ~
               at (03,26), "Acct #:",                                    ~
               at (03,34), fac(fac$(2)), account$               , ch(12),~
               at (03,47), fac(hex(84)), accountdescr$          , ch(32),~
                                                                         ~
               at (04,02), fac(hex(a4)), heading$,                       ~
                                                                         ~
               at (05,02), fac(hex(8c)), dates$( 1)             , ch(08),~
               at (06,02), fac(hex(8c)), dates$( 2)             , ch(08),~
               at (07,02), fac(hex(8c)), dates$( 3)             , ch(08),~
               at (08,02), fac(hex(8c)), dates$( 4)             , ch(08),~
               at (09,02), fac(hex(8c)), dates$( 5)             , ch(08),~
               at (10,02), fac(hex(8c)), dates$( 6)             , ch(08),~
               at (11,02), fac(hex(8c)), dates$( 7)             , ch(08),~
               at (12,02), fac(hex(8c)), dates$( 8)             , ch(08),~
               at (13,02), fac(hex(8c)), dates$( 9)             , ch(08),~
               at (14,02), fac(hex(8c)), dates$(10)             , ch(08),~
               at (15,02), fac(hex(8c)), dates$(11)             , ch(08),~
               at (16,02), fac(hex(8c)), dates$(12)             , ch(08),~
               at (17,02), fac(hex(8c)), dates$(13)             , ch(08),~
               at (18,02), fac(hex(8c)), dates$(14)             , ch(08),~
               at (19,02), fac(hex(8c)), dates$(15)             , ch(08),~
                                                                         ~
               at (05,12), fac(hex(9c)), activity$( 1)          , ch(12),~
               at (06,12), fac(hex(84)), activity$( 2)          , ch(12),~
               at (07,12), fac(hex(84)), activity$( 3)          , ch(12),~
               at (08,12), fac(hex(84)), activity$( 4)          , ch(12),~
               at (09,12), fac(hex(84)), activity$( 5)          , ch(12),~
               at (10,12), fac(hex(84)), activity$( 6)          , ch(12),~
               at (11,12), fac(hex(84)), activity$( 7)          , ch(12),~
               at (12,12), fac(hex(84)), activity$( 8)          , ch(12),~
               at (13,12), fac(hex(84)), activity$( 9)          , ch(12),~
               at (14,12), fac(hex(84)), activity$(10)          , ch(12),~
               at (15,12), fac(hex(84)), activity$(11)          , ch(12),~
               at (16,12), fac(hex(84)), activity$(12)          , ch(12),~
               at (17,12), fac(hex(84)), activity$(13)          , ch(12),~
               at (18,12), fac(hex(84)), activity$(14)          , ch(12),~
               at (19,12), fac(hex(84)), activity$(15)          , ch(12),~
                                                                         ~
               at (05,26), fac(fac$( 3)), balance$ ( 1)         , ch(12),~
               at (06,26), fac(fac$( 4)), balance$ ( 2)         , ch(12),~
               at (07,26), fac(fac$( 5)), balance$ ( 3)         , ch(12),~
               at (08,26), fac(fac$( 6)), balance$ ( 4)         , ch(12),~
               at (09,26), fac(fac$( 7)), balance$ ( 5)         , ch(12),~
               at (10,26), fac(fac$( 8)), balance$ ( 6)         , ch(12),~
               at (11,26), fac(fac$( 9)), balance$ ( 7)         , ch(12),~
               at (12,26), fac(fac$(10)), balance$ ( 8)         , ch(12),~
               at (13,26), fac(fac$(11)), balance$ ( 9)         , ch(12),~
               at (14,26), fac(fac$(12)), balance$ (10)         , ch(12),~
               at (15,26), fac(fac$(13)), balance$ (11)         , ch(12),~
               at (16,26), fac(fac$(14)), balance$ (12)         , ch(12),~
               at (17,26), fac(fac$(15)), balance$ (13)         , ch(12),~
               at (18,26), fac(fac$(16)), balance$ (14)         , ch(12),~
               at (19,26), fac(fac$(17)), balance$ (15)         , ch(12),~
                                                                         ~
               at (05,40), fac(hex(8c)), dates$(16)             , ch(08),~
               at (06,40), fac(hex(8c)), dates$(17)             , ch(08),~
               at (07,40), fac(hex(8c)), dates$(18)             , ch(08),~
               at (08,40), fac(hex(8c)), dates$(19)             , ch(08),~
               at (09,40), fac(hex(8c)), dates$(20)             , ch(08),~
               at (10,40), fac(hex(8c)), dates$(21)             , ch(08),~
               at (11,40), fac(hex(8c)), dates$(22)             , ch(08),~
               at (12,40), fac(hex(8c)), dates$(23)             , ch(08),~
               at (13,40), fac(hex(8c)), dates$(24)             , ch(08),~
               at (14,40), fac(hex(8c)), dates$(25)             , ch(08),~
               at (15,40), fac(hex(8c)), dates$(26)             , ch(08),~
               at (16,40), fac(hex(8c)), dates$(27)             , ch(08),~
               at (17,40), fac(hex(8c)), dates$(28)             , ch(08),~
               at (18,40), fac(hex(8c)), dates$(29)             , ch(08),~
               at (19,40), fac(hex(8c)), dates$(30)             , ch(08),~
               at (20,40), fac(hex(8c)), dates$(31)             , ch(08),~
               at (21,40), fac(hex(8c)), dates$(32)             , ch(08),~
                                                                         ~
               at (05,50), fac(hex(84)), activity$(16)          , ch(12),~
               at (06,50), fac(hex(84)), activity$(17)          , ch(12),~
               at (07,50), fac(hex(84)), activity$(18)          , ch(12),~
               at (08,50), fac(hex(84)), activity$(19)          , ch(12),~
               at (09,50), fac(hex(84)), activity$(20)          , ch(12),~
               at (10,50), fac(hex(84)), activity$(21)          , ch(12),~
               at (11,50), fac(hex(84)), activity$(22)          , ch(12),~
               at (12,50), fac(hex(84)), activity$(23)          , ch(12),~
               at (13,50), fac(hex(84)), activity$(24)          , ch(12),~
               at (14,50), fac(hex(84)), activity$(25)          , ch(12),~
               at (15,50), fac(hex(84)), activity$(26)          , ch(12),~
               at (16,50), fac(hex(84)), activity$(27)          , ch(12),~
               at (17,50), fac(hex(84)), activity$(28)          , ch(12),~
               at (18,50), fac(hex(84)), activity$(29)          , ch(12),~
               at (19,50), fac(hex(84)), activity$(30)          , ch(12),~
               at (20,50), fac(hex(84)), activity$(31)          , ch(12),~
               at (21,50), fac(hex(84)), activity$(32)          , ch(12),~
                                                                         ~
               at (05,64), fac(fac$(18)), balance$ (16)         , ch(12),~
               at (06,64), fac(fac$(19)), balance$ (17)         , ch(12),~
               at (07,64), fac(fac$(20)), balance$ (18)         , ch(12),~
               at (08,64), fac(fac$(21)), balance$ (19)         , ch(12),~
               at (09,64), fac(fac$(22)), balance$ (20)         , ch(12),~
               at (10,64), fac(fac$(23)), balance$ (21)         , ch(12),~
               at (11,64), fac(fac$(24)), balance$ (22)         , ch(12),~
               at (12,64), fac(fac$(25)), balance$ (23)         , ch(12),~
               at (13,64), fac(fac$(26)), balance$ (24)         , ch(12),~
               at (14,64), fac(fac$(27)), balance$ (25)         , ch(12),~
               at (15,64), fac(fac$(28)), balance$ (26)         , ch(12),~
               at (16,64), fac(fac$(29)), balance$ (27)         , ch(12),~
               at (17,64), fac(fac$(30)), balance$ (28)         , ch(12),~
               at (18,64), fac(fac$(31)), balance$ (29)         , ch(12),~
               at (19,64), fac(fac$(32)), balance$ (30)         , ch(12),~
               at (20,64), fac(fac$(33)), balance$ (31)         , ch(12),~
               at (21,64), fac(fac$(34)), balance$ (32)         , ch(12),~
                                                                         ~
               at (22,02), fac(hex(a4)), inpmessage$,                    ~
               at (23,02), "(1)Start Over",                              ~
               at (24,02), fac(pf4fac$), pf4$                   , ch(16),~
               at (24,46), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (23,21), fac(pf8fac$), pf8$                   , ch(20),~
               at (24,21), "(9)Zero Balances",                           ~
               at (24,65), fac(hex(84)), pf16$                  , ch(16),~
                                                                         ~
               keys(hex(0001090d0f10) & pf4key$ & pf8key$), key(keyhit%)

               if keyhit% <> 13 then L42060
                  call "MANUAL" ("GLSTOMP")
                  return

L42060:        if keyhit% <> 15 then L42100
                  call "PRNTSCRN"
                  return

L42100:        REM GET CURSOR LOCATION FOR HANDY EDITING.
                   close ws
                   call "SCREEN" addr ("C",u3%,"I",i$(),cursor%())
                   return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS THE ACCOUNT NUMBER TO MAKE SURE IT'S ON FILE, ELSE  *~
            * CHECKS THE VALIDITY OF THE NUMBERS WE ENTER.              *~
            *************************************************************

            deffn'150(fieldnr%)
                  errormsg$ = " "
                  if fieldnr%  = 1% then gosub L50140
                  if fieldnr%  = 2% then gosub L50300
                  if fieldnr%  > 2% and fieldnr% < 35% then gosub L50400
                  return

L50140: REM Test the G/L system code
            call "NUMTEST" (set$, 1, 2, errormsg$, 0, set)
            if errormsg$ = " " then goto L50200
                errormsg$ = "G/L System code must be '1' (Statutory) or"&~
                    " '2' (Local Authority)"
                return
L50200:     setdescr$ = "(Stat.)"
            main% = 1%
            if set = 1 then return
                setdescr$ = "(Local)"
                main% = 10%
                return

L50300:     REM CONVINCES US THAT THE ACCOUNT IS ON FILE.
                f1%(1) = -999%  /* Include Obsolete Accounts */
                call "GETCODE" (#main%, account$, accountdescr$, 1%, 0,  ~
                    f1%(main%))
                if f1%(main%) = 1 then L50370
                   errormsg$ = "Invalid Account:" & account$
                   return
L50370:         gosub L30000        /* ACCT ON FILE, STAY IN INPUT MODE */
                return

L50400:     REM CONVINCES US THAT THE NUMBERS ARE OK.
                subscript% = fieldnr% - 2%
                if balance$(subscript%)=" " then balance$(subscript%)="0"

                convert balance$(subscript%) to balance, data goto L50470
                goto L50510

L50470:            errormsg$ =  "Invalid entry for balance :" &          ~
                      balance$(subscript%)
                   return

L50510:         prevbalance = 0
                prevsubscript% = subscript% - 1%

L50540:         if prevsubscript% < 1% then L50600
                if dates$(prevsubscript%) = blankdate$ then blankdate_code
                if dates$(prevsubscript%) <> " " then L50590
blankdate_code:    prevsubscript% = prevsubscript% - 1%
                   goto L50540

L50590:         prevbalance = balance(prevsubscript%)
L50600:         activity(subscript%) = balance - prevbalance

                if edit% = 0% then L50750

                nextbalance = 0
                nextsubscript% = subscript% + 1%

L50670:         if nextsubscript% > 32% then L50750
                if dates$(nextsubscript%) = blankdate$ then blankdate_code2
                if dates$(nextsubscript%) <> " " then L50720
blankdate_code2:   nextsubscript% = nextsubscript% + 1%
                   goto L50670

L50720:         nextbalance = balance(nextsubscript%)
                activity(nextsubscript%) = nextbalance - balance

L50750:         gosub L30250
                return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One moment, please")
            end
