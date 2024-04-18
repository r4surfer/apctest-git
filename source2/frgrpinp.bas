        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  FFFFF  RRRR    GGG   RRRR   PPPP   IIIII  N   N  PPPP    *~
            *  F      R   R  G      R   R  P   P    I    NN  N  P   P   *~
            *  FFF    RRRR   G GGG  RRRR   PPPP     I    N N N  PPPP    *~
            *  F      R  R   G   G  R   R  P        I    N  NN  P       *~
            *  F      R   R   GGG   R   R  P      IIIII  N   N  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FRGRPINP - DEFFINITION AND MANAGEMENT OF G/L ACCOUNT      *~
            *            GROUPING CODES.  THESE CODES ARE USED BY THE   *~
            *            G/L REPORTING MODULE TO REFLECT RELATIONSHIPS  *~
            *            BETWEEN ACCOUNTS, THUS ALLOWING AN EASIER WAY  *~
            *            TO PULL A GROUP OF ACCOUNTS INTO A REPORT.     *~
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
            * 02/08/85 ! ORIGINAL                                 ! HES *~
            * 05/22/87 ! Support Of Obsolete Flag On GLAMIN       ! HES *~
            * 08/04/87 ! Corrected Copy logic                     ! HES *~
            * 10/12/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 08/15/88 ! Dual Books depends on a SYSFILE2 flag.   ! JIM *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            account$(500)16,             /* ACCOUNT NUMBER             */~
            account$16,                  /* ACCOUNT NUMBER             */~
            acctdescr$(500)30,           /* ACCOUNT DESCRIPTION        */~
            acctdescr$30,                /* ACCOUNT DESCRIPTION        */~
            accttype$(500)9,             /* ACCOUNT TYPE FOR DISPLAY   */~
            accttype$9,                  /* ACCOUNT TYPE (WORK VARIABL)*/~
            afac$1,                      /* FIELD ATTRIBUTE CHARACTER  */~
            append$6,                    /* CODE TO 'COPY' (Append)    */~
            code$6,                      /* GROUPING CODE              */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            descr$30,                    /* REFERENCE TEXT             */~
            dual_books$1,                /* Dual books in effect?      */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            header$79,                   /* Screen Header              */~
            header1$79,                  /* Screen Header              */~
            hfac$1,                      /* FIELD ATTRIBUTE CHARACTER  */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            jfac$(15)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lfac$(15)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line$(500)4,                 /* FOR SCREEN DISPLAY         */~
            message$79,                  /* INPUT MESSAGE              */~
            pfdescr$(3)79,               /* Keys Actice for screen     */~
            pfkeys$32,                   /* Keys Actice for screen     */~
            set$1, setdescr$30,          /* Set of books to use        */~
            setmsg$26,                   /* Screen message for SET     */~
            text$50                      /* DEFAULT TEXT FOR REPORTS   */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            fs%(64),                     /* FILE STATUS CODES          */~
            rslt$(64)20                  /* TEXT FROM FILE OPENING     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.00 01/19/90 CMS2 / CMS-I Merge              "
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
            * #20 ! SYSFILE2 ! System information                       *~
            * -- Statutory files ...                                    *~
            * #01 ! FRGRPMAS ! G/L Grouping Codes File                  *~
            * #02 ! FRGRPLIN ! G/L Grouping Codes Detail File           *~
            * #03 ! GLMAIN   ! General Ledger Main File                 *~
            * -- Local Authority files ...                              *~
            * #11 ! FRGRPMA2 ! G/L Grouping Codes File                  *~
            * #12 ! FRGRPLI2 ! G/L Grouping Codes Detail File           *~
            * #13 ! GLMAIN2  ! General Ledger Main File                 *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01,  "FRGRPMAS", varc, indexed, recsize = 100,       ~
                        keypos = 1, keylen = 6,                          ~
                        alt key 1, keypos = 7, keylen = 30, dup /*DESCR*/

            select #02,  "FRGRPLIN", varc, indexed, recsize = 60,        ~
                        keypos = 1, keylen = 22,                         ~
                        alt key 1, keypos = 7, keylen = 16, dup /*ACCT*/

            select #03,  "GLMAIN", varc, indexed, recsize = 300,         ~
                        keypos =  1, keylen = 9

            select #11,  "FRGRPMA2", varc, indexed, recsize = 100,       ~
                        keypos = 1, keylen = 6,                          ~
                        alt key 1, keypos = 7, keylen = 30, dup /*DESCR*/

            select #12,  "FRGRPLI2", varc, indexed, recsize = 60,        ~
                        keypos = 1, keylen = 22,                         ~
                        alt key 1, keypos = 7, keylen = 16, dup /*ACCT*/

            select #13,  "GLMAIN2", varc, indexed, recsize = 300,        ~
                        keypos =  1, keylen = 9

            select  #20, "SYSFILE2",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            call "SHOSTAT" ("Preparing For Account Grouping Codes Managem~
        ~ent")

            call "OPENCHCK" (#01%, fs%( 1), f2%( 1), 100%, rslt$( 1))
            call "OPENCHCK" (#02%, fs%( 2), f2%( 2), 100%, rslt$( 2))
            call "OPENCHCK" (#03%, fs%( 3), f2%( 3),   0%, rslt$( 3))
            call "OPENCHCK" (#20%, fs%(20), f2%(20),   0%, rslt$(20))
            dual_books$ = "N"                        /* Default to 'no' */
            call "READ100" (#20, "SWITCHS.GL", f1%(20))
                if f1%(20) = 0% then goto L09000
            get #20 using L02535, dual_books$
L02535:         FMT POS(21), CH(1)
            if dual_books$ <> "Y" then goto L09000
                call "OPENCHCK" (#11%, fs%(11), f2%(11), 100%, rslt$(11))
                call "OPENCHCK" (#12%, fs%(12), f2%(12), 100%, rslt$(12))
                call "OPENCHCK" (#13%, fs%(13), f2%(13),   0%, rslt$(13))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            if dual_books$="Y" then setmsg$="G/L System to use (1 or 2)"
            if dual_books$="Y" then lo% = 1% else lo% = 2%
            date$ = date
            call "DATEFMT" (date$)
            header1$ = "          Account      Description               ~
        ~      Account Type"

            allowed% = dim(account$(),1)
            for i% = 1 to allowed%
                convert i% to line$(i%), pic(###)
                str(line$(i%),4,1) = ")"
            next i%

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            errormsg$, message$, code$, descr$, text$, account$(),       ~
                acctdescr$(), accttype$(), append$, set$ = " "
            c%, maxlines%, line%, editmode% = 0
            gmas%=1% : glin%=2% : main%=3% : set=1 : setdescr$ = " "

            for fieldnr% = lo% to 4%
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10200
L10140:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = lo% then L65000
                      if keyhit%  =  9 and fieldnr% =  2% then print_data
                      if keyhit% <>  0 then       L10140
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10140
L10200:         next fieldnr%

            message$ = "Enter The List Of Account Numbers That Make Up Th~
        ~is Grouping Code"

        enter_lines
L10260:     c% = c% + 1
L10270:     gosub'102(c%-line%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  16 then L10390
                  if keyhit% <>  0  then L10270
            gosub'152(c%)
                  if errormsg$ <> " " then L10270

            maxlines% = c%
            if maxlines% = allowed% then L10400
            if maxlines% > 14 then line% = line% + 1
            goto L10260

L10390:     account$(c%), acctdescr$(c%), accttype$(c%) = " "
L10400:     if editmode% = 1 then edtpg2

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editmode
            editmode% = 1
            message$= "To Modify Displayed Values, Position Cursor To Des~
        ~ired Value And Press (ENTER)."
            errormsg$ = " "

L11120:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       edtpg2
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11120
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 3 or fieldnr% >  4 then L11120

            message$= "Press (ENTER) To Validate Data Entered"
L11210:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11210
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11210
            goto L11120

        edtpg2
            message$= "To Modify Displayed Values, Position Cursor To Des~
        ~ired Value And Press (ENTER)."
            errormsg$ = " "

L11330:     gosub'112(0%)
            errormsg$ = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then line%= 0
                  if keyhit%  =  4 then line%= max(0, line%-10)
                  if keyhit%  =  5 then line%= max(0, min(               ~
                                                  line%+10,maxlines%-15))
                  if keyhit%  =  9 then editmode
                  if keyhit%  =  10 then gosub append
                  if keyhit% <> 11 then L11480
                     if maxlines% = allowed% then L11330
                     line% = max(0, maxlines%-14)
                     c% = maxlines%
                     message$= "Press PF(16) To End Inserts"
                     goto enter_lines
L11480:           if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 and keyhit% <> 12 then L11330

            fieldnr% = cursor%(1) - 5
            errormsg$ = "Cursor Must Be Positioned To Desired Account"
            if fieldnr% < 1 or fieldnr% > 15 then L11330
            convert str(line$(line%+fieldnr%),,3) to c%
            errormsg$ = " "
            if account$(c%) = " " then L11330
            if keyhit% = 12 then remove_account

            message$= "Enter A Blank Account Number To Search Through Exi~
        ~sting G/L Accounts"
L11610:     gosub'112(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11610
            gosub'152(c%)
                  if errormsg$ <> " " then L11610
            goto L11330

        remove_account
            message$= "Press (ENTER) To Remove Flashing Account Number Fr~
        ~om This Grouping Code"
            errormsg$ = " "
            gosub'122(fieldnr%)
            if keyhit% <> 0 then edtpg2
            gosub delete_line
            goto edtpg2

        delete_line
            REM Delete Logic...
            for i% = c% to maxlines%
                 account$  (i%) =  account$  (i%+1)
                 acctdescr$(i%) =  acctdescr$(i%+1)
                 accttype$ (i%) =  accttype$ (i%+1)
            next i%
            account$(i%), acctdescr$(i%), accttype$(i%) = " "
            maxlines% = maxlines% - 1
            line% = max(0, min(line%, maxlines%-15))
        return

        REM *************************************************************~
            *            ' E X T E R N A L   C O P Y '                  *~
            *                                                           *~
            * Loads up and appends the specified group code.            *~
            *************************************************************

        append
            dups% = 0
            errormsg$ = "Specified Group Code Does Not Exist: " & append$
            call "GETCODE" (#gmas%, append$, " ", 0%, 1, f1%(gmas%))
                if f1%(gmas%) = 0 then return

            REM Uses existing logic where possible
            u3% = maxlines%
            readkey$ = append$
            gosub load_lines
            for c% = u3% + 1 to maxlines%
L12170:         errormsg$ = " " : t% = c%
                gosub test_for_dup
                if errormsg$ = " " then L12240
                gosub delete_line
                dups% = dups% + 1
                if account$(c%)=" " then L12250 /*Aviod Never Never Land*/
                goto L12170
L12240:     next c%
L12250:     convert maxlines% - u3% to temp$, pic(###)
            errormsg$ = hex(84) & "Number Of Accounts Appended:" & temp$
            line% = min(max(0, u3%-1), max(0, maxlines%-15))
            if dups% = 0 then return
            convert dups% to temp$, pic(###)
            errormsg$ = errormsg$ & ".   " & temp$ &                     ~
                                       " Duplicates Were Found & Ignored"
        return

        REM *************************************************************~
            *       I N P U T   M O D E   F O R   P R I N T             *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        print_data
            errormsg$, message$, startcode$, endcode$ = " "

            for fieldnr% = 1 to  2
                gosub'053(fieldnr%)
                      if enabled% = 0 then L14180
L14120:         gosub'103(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then inputmode
                      if keyhit% <>  0 then       L14120
                gosub'153(fieldnr%)
                      if errormsg$ <> " " then L14120
L14180:         next fieldnr%

        REM *************************************************************~
            *               P R I N T  C O D E S                        *~
            *                                                           *~
            * Prints a report showing accounts tied to each code.       *~
            *************************************************************

            call "SHOSTAT" ("Account Grouping Code print-out in progress")
            pagenumber% = 0
            pageline% = 987654321
            if startcode$ <> "ALL" then L15110
                readkey$ = all(hex(00))
                endcode$ = all(hex(ff))
                goto L15140
L15110:     str(readkey$,,6) = str(startcode$,,6) addc all(hex(ff))
            if endcode$ = " " then endcode$ = startcode$

L15140:     call "PLOWNEXT" (#gmas%, readkey$, 0%, f1%(gmas%))
                if f1%(gmas%) = 1 then L15200
L15160:         if tag% = 0 then print using L16260
                close printer
                goto inputmode

L15200:     if str(readkey$,,6) > endcode$ then L15160

            get #gmas%, using L15240, descr$
L15240:     FMT XX(6), CH(30)
            startcode$ = str(readkey$,,6)

            str(readkey$,7) = all(hex(00))
L15280:     call "PLOWNEXT" (#glin%, readkey$, 6%, f1%(glin%))
                if f1%(glin%) = 1 then L15360
                gosub form_control
                if tag% = 0 then print using L16260                       ~
                     else pageline% = pageline% - 1
                tag% = 1
                goto L15140

L15360:     get #glin%, using L15370, account$
L15370:     FMT XX(6), CH(16)
            call "READ100" (#main%, account$, f1%(main%))
                if set = 1                                               ~
                    then call "GLFMT" (account$)                         ~
                    else call "GLFMT2" (account$)
                if f1%(main%) = 1 then L15430
                acctdescr$ = "*** NOT ON FILE ***"
                accttype$ = " "
                goto L15460
L15430:     get #main%, using L15440, acctdescr$, accttype$
L15440:     FMT XX(9), CH(30), CH(1)

L15460:     gosub format_type
            gosub form_control
            print using L16320, startcode$, descr$, account$, acctdescr$, ~
                                                             accttype$
            tag% = 0
            startcode$, descr$ = " "
            goto L15280

        REM *************************************************************~
            *        P A G E   C O N T R O L   R O U T I N E            *~
            *                                                           *~
            * CONTROLS THE PAGING                                       *~
            *************************************************************

        form_control
                select printer (134)
                pageline% = pageline% + 1
                if pageline% < 58 then return
                   if pagenumber% > 0 and tag% = 0 then print using L16260
                   print page
                   pagenumber% = pagenumber% + 1
                   print using L16220, pagenumber%, date$
                   print
                   print using L16260
                   print using L16290
                   print using L16260
                   pageline% = 6
                   tag% = 1
                   return

L16220: %PAGE ###         G/L   G R O U P I N G   C O D E S   L I S T I N~
        ~ G            DATE: ########

L16260: %+----------+------------------------------+------------+--------~
        ~--------------------+---------+

L16290: %!GROUP CODE!     D E S C R I P T I O N    !ACCOUNT NMBR!     D E~
        ~ S C R I P T I O N  ! TYPE    !

L16320: %!######    !##############################!############!########~
        ~####################!#########!

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            lastcode$ = code$
            gosub L31000
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1
                  on fieldnr% gosub L20065,         /* G/L set to use   */~
                                    L20100,         /* GROUPING CODE    */~
                                    L20200,         /* REFERENCE TEXT   */~
                                    L20300          /* DEFAULT TEXT     */
                     return
L20065: REM Enable code for Statutory or Local Authority G/L set  SET$
            if dual_books$ <> "Y" then enabled% = 0%
            message$ = "Enter '1' to use Statutory books; '2' to use"   &~
                " Local Authority books"
            return
L20100:     REM DEFAULT/ENABLE FOR GROUPING CODE
            message$ = "Leave Code Blank And Press ENTER to Find An Exist~
        ~ing Code."
                return
L20200:     REM DEFAULT/ENABLE FOR REFERENCE TEXT
            message$ = "This Is Memo Text Only, Its Used To Reference Thi~
        ~s Code In The Future."
                return
L20300:     REM DEFAULT/ENABLE FOR DEFAULT TEXT FOR REPORTS
            message$ = "This Is DEFUALT Text For Financial Report Set up.~
        ~ Input Is Optional."
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 3 OF INPUT. *~
            *************************************************************

            deffn'053(fieldnr%)
                  enabled% = 1
                  on fieldnr% gosub L22110,         /* STARTING CODE    */~
                                    L22160          /* ENDING CODE      */
                     return
L22110:     REM DEFAULT/ENABLE FOR STARTING CODE
            startcode$ = "ALL"
            message$ = "ALL Will Print all codes On File. To Print Range,~
        ~ Enter The Code To Start With."
                return
L22160:     REM DEFAULT/ENABLE FOR ENDING CODE
            if startcode$ = "ALL" then enabled% = 0
            message$ = "Enter The Last Code To Print.  Leave Blank To Onl~
        ~y Print The 'Starting Code'."
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
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            ask% = 2%
            call "STARTOVR" (ask%)
            if ask% = 1% then return
            return clear all
            goto L10000

L30000: REM *************************************************************~
            *                L O A D   D A T A                          *~
            *                                                           *~
            * Loads up existing Group Code For Edit                     *~
            *************************************************************

            call "READ100" (#gmas%, code$, f1%(gmas%))
            if f1%(gmas%) = 0 then return
            get #gmas%, using L30100, descr$, text$   /* Get Header Info */
L30100:         FMT XX(6), CH(30), CH(50)

            readkey$ = code$
            onfile% = 1

        load_lines
            str(readkey$,7) = all(hex(00))
L30170:     call "PLOWNEXT" (#glin%, readkey$, 6%, f1%(glin%))
                if f1%(glin%) = 0 then return
            if maxlines% = allowed% then return
            c%, maxlines% = maxlines% + 1
            get #glin%, using L30220, account$(c%)    /* Get Line Info */
L30220:     FMT XX(6), CH(16)
            call "DESCRIBE"(#main%, account$(c%), acctdescr$(c%), 0%,    ~
                f1%(main%))
                if set = 1                                               ~
                    then call "GLFMT" (account$(c%))                     ~
                    else call "GLFMT2" (account$(c%))
                if f1%(main%) = 1 then L30290
                acctdescr$(c%) = hex(94) & "(NO LONGER ON FILE)"
                accttype$ = " "
                goto L30310
L30290:     get #main%, using L30300, accttype$
L30300:     FMT XX(39), CH(1)
L30310:     gosub format_type
            accttype$(c%) = accttype$
            goto L30170

L31000: REM *************************************************************~
            *                S A V E   D A T A                          *~
            *                                                           *~
            * Write new/modified data to disk.                          *~
            *************************************************************

            REM Write out Account list for this code...
            readkey$ = code$
            call "DELETE" (#glin%, code$, 6%)           /* Clear Out */
            call "READ101" (#gmas%, code$, f1%(gmas%))  /* Any Old  */
                if f1%(gmas%) = 1 then delete #gmas%    /* Data    */
            if maxlines% = 0 then return    /* Implied Delete */

            for c% = 1 to maxlines%
                if set = 1                                               ~
                    then call "GLUNFMT" (account$(c%))                   ~
                    else call "GLUNFM2" (account$(c%))
                write #glin%, using L31150, code$, account$(c%),          ~
                    acctdescr$(c%), " "
L31150:             FMT CH(6), CH(16), CH(30), CH(8)
            next c%

            REM Write to Header (code) File...
            write #gmas%, using L31200, code$, descr$, text$, " "
L31200:         FMT CH(6), CH(30), CH(50), CH(14)
            return

        REM *************************************************************~
            *                F O R M A T   'T Y P E'                    *~
            *                                                           *~
            * Expands one character account type into ledgable text...  *~
            *************************************************************

        format_type
            if accttype$ = "$" then accttype$ = "CASH     "
            if accttype$ = "A" then accttype$ = "ASSET    "
            if accttype$ = "L" then accttype$ = "LIABILITY"
            if accttype$ = "C" then accttype$ = "CAPITAL  "
            if accttype$ = "R" then accttype$ = "REVENUE  "
            if accttype$ = "E" then accttype$ = "EXPENSE  "
            if len(accttype$) < 4 then accttype$ = "*UNKNOWN*"
        return

        REM *************************************************************~
            *                      S C R E E N  1                       *~
            *                                                           *~
            * Handles Header Information.                               *~
            *************************************************************

            REM Input Mode Screen Controler
            deffn'101(fieldnr%)
                  pfdescr$(1) ="(1)Start Over        (9)Print Grouping "&~
                               "codes                   (13)Instructions"
                  pfdescr$(2) ="                                       "&~
                               "                        (15)Print Screen"
                  pfdescr$(3) ="                                       "&~
                               "                        (16)Exit Program"
                  pfkeys$ = hex(0001090d0f10)
                  if fieldnr% = lo% then L40171
                     str(pfdescr$(3),64) = " "
                     str(pfkeys$,6,1) = hex(ff)
L40171:           if fieldnr% =  2% then L40180
                     str(pfdescr$(1),22,23) = " "
                     str(pfkeys$,3,1) = hex(ff)
L40180:           init(hex(8c)) lfac$()
                  goto L40420

            REM Edit Mode Screen Controler
            deffn'111(fieldnr%)
                  pfdescr$(1) ="(1)Start Over                          "&~
                               "                        (13)Instructions"
                  pfdescr$(2) ="(2)Edit Account List                   "&~
                               "                        (15)Print Screen"
                  pfdescr$(3) ="                                       "&~
                               "                        (16)SAVE Data   "
                  pfkeys$ = hex(0002010d0f10)
                  init(hex(86)) lfac$()
                  lfac$(1), lfac$(2) = hex(8c) /* Disable SET$, CODE$ */
                  if fieldnr% = 0 then L40420

                  REM Adjust PF Keys Available If Modifing A Field...
                  init(hex(8c)) lfac$()
                  pfdescr$(1) ="(1)Start Over                          "&~
                               "                        (13)Instructions"
                  pfdescr$(2) ="(ENTER) To Continue                    "&~
                               "                        (15)Print Screen"
                  pfdescr$(3) =" "
                  pfkeys$ = hex(00010d0f)

L40420:           REM Common Logic Starts Here...
                  str(pfdescr$(3),63,1)=hex(84) /* Make Sure They See */
                  header$ = "Last Code Managed: XXXXXX"
                  str(header$,62) = "FRGRPINP: " & str(cms2v$,,8)
                  str(header$,20,6) = lastcode$
                  on fieldnr% gosub L40580,         /* G/L set to use   */~
                                    L40550,         /* GROUPING CODE    */~
                                    L40520,         /* REFERENCE TEXT   */~
                                    L40520          /* DEFAULT TEXT     */
                     goto L40620

L40520:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40550:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40580:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      if dual_books$ <> "Y" and fieldnr% = 1% then       ~
                          lfac$(fieldnr%) = hex(9c)
                      return

L40620:     accept                                                       ~
               at (01,02), "Manage G/L Account Grouping Codes",          ~
               at (01,67), "Date:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(8c)),   setmsg$               ,ch(26),~
               at (06,30), fac(lfac$( 1)), set$                 , ch(01),~
               at (06,33), fac(hex(8c)),   setdescr$            , ch(30),~
                                                                         ~
               at (07,02), "Grouping Code",                              ~
               at (07,30), fac(lfac$( 2)), code$                , ch(06),~
                                                                         ~
               at (08,02), "Reference Text",                             ~
               at (08,30), fac(lfac$( 3)), descr$               , ch(30),~
                                                                         ~
               at (09,02), "Default Text For Reports",                   ~
               at (09,30), fac(lfac$( 4)), text$                , ch(45),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$), key (keyhit%)

               if keyhit% <> 13 then L40880
                  call "MANUAL" ("FRGRPINP")
                  goto L40620

L40880:        if keyhit% <> 15 then L40920
                  call "PRNTSCRN"
                  goto L40620

L40920:        if fieldnr% <> 0 then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                      S C R E E N  II                      *~
            *                                                           *~
            * Handles Line Item Information.                            *~
            *************************************************************

            REM Input Mode Screen Controler
            deffn'102(fieldnr%)
                  pfdescr$(1) ="(1)Start Over                          "&~
                               "                        (13)Instructions"
                  pfdescr$(2) ="                                       "&~
                               "                        (15)Print Screen"
                  pfdescr$(3) ="                                       "&~
                               "                        (16)Edit Mode   "
                  pfkeys$ = hex(00010d0f10)
                  init(hex(8c)) lfac$()
                  hfac$ = hex(ac)
                  afac$ = hex(9c)
                  goto L41600

            REM Edit Mode Screen Controler
            deffn'112(fieldnr%)
                  pfdescr$(1) ="(1)Start Over  (4)Prev      (10)Append "&~
                               "from XXXXXX             (13)Instructions"
                  pfdescr$(2) ="(2)First       (5)Next      (11)Add Acc"&~
                               "ounts                   (15)Print Screen"
                  pfdescr$(3) ="               (9)Header    (12)Remove "&~
                               "Accounts                (16)SAVE Data   "
                  pfkeys$ = hex(0001020405090a0b0c0d0f10)
                  init(hex(86)) lfac$()
                  hfac$ = hex(ae)
                  afac$ = hex(81)
                  if fieldnr% = 0 then L41430

                  REM Adjust PF Keys Available If Modifing An Account...
                  init(hex(8c)) lfac$()
                  hfac$ = hex(ac)
                  afac$ = hex(8c)
                  pfdescr$(1) ="(1)Start Over                          "&~
                               "                        (13)Instructions"
                  pfdescr$(2) ="(ENTER) To Continue                    "&~
                               "                        (15)Print Screen"
                  pfdescr$(3) =" "
                  pfkeys$ = hex(00010d0f)
L41430:           goto L41600

            REM Delete Mode Screen Controler
            deffn'122(fieldnr%)
                  pfdescr$(1) ="(1)Abort Delete And Return To Edit Mode"&~
                               "                        (13)Instructions"
                  pfdescr$(2) ="(ENTER) Remove Flashing Account From Li"&~
                               "st                      (15)Print Screen"
                  pfdescr$(3) =" "
                  pfkeys$ = hex(00010d0f10)
                  init(hex(8c)) lfac$()
                  hfac$ = hex(ac)
                  afac$ = hex(9c)
                  lfac$(fieldnr%), jfac$(fieldnr%) = hex(94)
                  goto L41710

L41600:     REM Common Logic Here...
            if fieldnr% > 0 then lfac$(fieldnr%) = hex(81)
            str(pfdescr$(3),63,1)=hex(84) /* Make Sure They See */
            init(hex(9c)) jfac$()
            for i% = 1 to 15
               if account$(line%+i%)<>" " then jfac$(i%) = hex(8c)
            next i%
            if fieldnr% <> 0 then jfac$(fieldnr%) = hex(8c)
            header$ = "G/L Accounts To Be Included When In Thi" &        ~
                      "s Group Code"
            str(header$,62) = "FRGRPINP: " & str(cms2v$,,8)

L41710:     accept                                                       ~
               at (01,02), "Manage G/L Account Grouping Codes",          ~
               at (01,67), "Date:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (03,02), "Grouping Code:",                             ~
               at (03,17), fac(hex(84)), code$                  , ch(06),~
               at (03,24), fac(hex(8c)), descr$                 , ch(30),~
               at (03,55), "Consisting Of XXX Accounts",                 ~
               at (03,69), fac(hex(8c)), maxlines%            , pic(###),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hfac$),   header1$               , ch(79),~
                                                                         ~
               at (06,02), fac(jfac$(01)), line$(line%+01)      , ch(04),~
               at (07,02), fac(jfac$(02)), line$(line%+02)      , ch(04),~
               at (08,02), fac(jfac$(03)), line$(line%+03)      , ch(04),~
               at (09,02), fac(jfac$(04)), line$(line%+04)      , ch(04),~
               at (10,02), fac(jfac$(05)), line$(line%+05)      , ch(04),~
               at (11,02), fac(jfac$(06)), line$(line%+06)      , ch(04),~
               at (12,02), fac(jfac$(07)), line$(line%+07)      , ch(04),~
               at (13,02), fac(jfac$(08)), line$(line%+08)      , ch(04),~
               at (14,02), fac(jfac$(09)), line$(line%+09)      , ch(04),~
               at (15,02), fac(jfac$(10)), line$(line%+10)      , ch(04),~
               at (16,02), fac(jfac$(11)), line$(line%+11)      , ch(04),~
               at (17,02), fac(jfac$(12)), line$(line%+12)      , ch(04),~
               at (18,02), fac(jfac$(13)), line$(line%+13)      , ch(04),~
               at (19,02), fac(jfac$(14)), line$(line%+14)      , ch(04),~
               at (20,02), fac(jfac$(15)), line$(line%+15)      , ch(04),~
                                                                         ~
               at (06,12), fac(lfac$( 1)), account$(line%+01)   , ch(12),~
               at (07,12), fac(lfac$( 2)), account$(line%+02)   , ch(12),~
               at (08,12), fac(lfac$( 3)), account$(line%+03)   , ch(12),~
               at (09,12), fac(lfac$( 4)), account$(line%+04)   , ch(12),~
               at (10,12), fac(lfac$( 5)), account$(line%+05)   , ch(12),~
               at (11,12), fac(lfac$( 6)), account$(line%+06)   , ch(12),~
               at (12,12), fac(lfac$( 7)), account$(line%+07)   , ch(12),~
               at (13,12), fac(lfac$( 8)), account$(line%+08)   , ch(12),~
               at (14,12), fac(lfac$( 9)), account$(line%+09)   , ch(12),~
               at (15,12), fac(lfac$(10)), account$(line%+10)   , ch(12),~
               at (16,12), fac(lfac$(11)), account$(line%+11)   , ch(12),~
               at (17,12), fac(lfac$(12)), account$(line%+12)   , ch(12),~
               at (18,12), fac(lfac$(13)), account$(line%+13)   , ch(12),~
               at (19,12), fac(lfac$(14)), account$(line%+14)   , ch(12),~
               at (20,12), fac(lfac$(15)), account$(line%+15)   , ch(12),~
                                                                         ~
               at (06,25), fac(hex(8c)), acctdescr$(line%+01)   , ch(30),~
               at (07,25), fac(hex(8c)), acctdescr$(line%+02)   , ch(30),~
               at (08,25), fac(hex(8c)), acctdescr$(line%+03)   , ch(30),~
               at (09,25), fac(hex(8c)), acctdescr$(line%+04)   , ch(30),~
               at (10,25), fac(hex(8c)), acctdescr$(line%+05)   , ch(30),~
               at (11,25), fac(hex(8c)), acctdescr$(line%+06)   , ch(30),~
               at (12,25), fac(hex(8c)), acctdescr$(line%+07)   , ch(30),~
               at (13,25), fac(hex(8c)), acctdescr$(line%+08)   , ch(30),~
               at (14,25), fac(hex(8c)), acctdescr$(line%+09)   , ch(30),~
               at (15,25), fac(hex(8c)), acctdescr$(line%+10)   , ch(30),~
               at (16,25), fac(hex(8c)), acctdescr$(line%+11)   , ch(30),~
               at (17,25), fac(hex(8c)), acctdescr$(line%+12)   , ch(30),~
               at (18,25), fac(hex(8c)), acctdescr$(line%+13)   , ch(30),~
               at (19,25), fac(hex(8c)), acctdescr$(line%+14)   , ch(30),~
               at (20,25), fac(hex(8c)), acctdescr$(line%+15)   , ch(30),~
                                                                         ~
               at (06,57), fac(hex(8c)), accttype$(line%+01)    , ch(09),~
               at (07,57), fac(hex(8c)), accttype$(line%+02)    , ch(09),~
               at (08,57), fac(hex(8c)), accttype$(line%+03)    , ch(09),~
               at (09,57), fac(hex(8c)), accttype$(line%+04)    , ch(09),~
               at (10,57), fac(hex(8c)), accttype$(line%+05)    , ch(09),~
               at (11,57), fac(hex(8c)), accttype$(line%+06)    , ch(09),~
               at (12,57), fac(hex(8c)), accttype$(line%+07)    , ch(09),~
               at (13,57), fac(hex(8c)), accttype$(line%+08)    , ch(09),~
               at (14,57), fac(hex(8c)), accttype$(line%+09)    , ch(09),~
               at (15,57), fac(hex(8c)), accttype$(line%+10)    , ch(09),~
               at (16,57), fac(hex(8c)), accttype$(line%+11)    , ch(09),~
               at (17,57), fac(hex(8c)), accttype$(line%+12)    , ch(09),~
               at (18,57), fac(hex(8c)), accttype$(line%+13)    , ch(09),~
               at (19,57), fac(hex(8c)), accttype$(line%+14)    , ch(09),~
               at (20,57), fac(hex(8c)), accttype$(line%+15)    , ch(09),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,02), fac(hex(8c)), pfdescr$(1)            , ch(79),~
               at (22,46), fac(afac$),   append$                , ch(06),~
               at (23,02), fac(hex(8c)), pfdescr$(2)            , ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(3)            , ch(79),~
               keys(pfkeys$), key (keyhit%)

               if keyhit% <> 13 then L42420
                  call "MANUAL" ("FRGRPINP")
                  goto L41710

L42420:        if keyhit% <> 15 then L42460
                  call "PRNTSCRN"
                  goto L41710

L42460:        if fieldnr% <> 0 then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                    S C R E E N  III                       *~
            *                                                           *~
            * Handles print range selection.                            *~
            *************************************************************

            REM Input Mode Screen Controler
            deffn'103(fieldnr%)
                  pfdescr$(1) ="(1)Start Over                          "&~
                               "                        (13)Instructions"
                  pfdescr$(2) ="                                       "&~
                               "                        (15)Print Screen"
                  pfdescr$(3) ="                                       "&~
                               "                        (16)CANCEL Print"
                  pfkeys$ = hex(00010d0f10)
                  init(hex(8c)) lfac$()
                  str(pfdescr$(3),63,1)=hex(84) /* Make Sure They See */
                  header$ = "Print Codes"
                  str(header$,62) = "FRGRPINP: " & str(cms2v$,,8)
                  on fieldnr% gosub L43240,         /* START CODE       */~
                                    L43240          /* END CODE         */
                     goto L43310

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L43240:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L43310:     accept                                                       ~
               at (01,02), "Manage G/L Account Grouping Codes",          ~
               at (01,67), "Date:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Starting Group Code",                        ~
               at (06,30), fac(lfac$( 1)), startcode$           , ch(06),~
               at (07,02), "Ending Group Code",                          ~
               at (07,30), fac(lfac$( 2)), endcode$             , ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$), key (keyhit%)

               if keyhit% <> 13 then L43540
                  call "MANUAL" ("FRGRPINP")
                  goto L43310

L43540:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L43310

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50140,         /* G/L set to use   */~
                                    L50290,         /* GROUPING CODE    */~
                                    L50380,         /* REFERENCE TEXT   */~
                                    L50420          /* DEFAULT TEXT     */
                     return

L50140: REM Test G/L set of books to use
            gmas% = 1% : glin% = 2% : main% = 3% : set = 1
            setdescr$ = " "
            if dual_books$ <> "Y" then return
            call "NUMTEST" (set$, 1, 2, errormsg$, 0, set)
            if errormsg$ = " " then goto L50210
                errormsg$ = "G/L System code must be '1' (Statutory)"   &~
                    " or '2' (Local Authority)"
                return
L50210:     setdescr$ = "Statutory"
            gmas% = 1% : glin% = 2% : main% = 3%
            if set = 1 then goto L50260
                setdescr$ = "Local Authority"
                gmas% = 11% : glin% = 12% : main% = 13%
L50260:     call "PUTPAREN" (setdescr$)
            return

L50290: REM TEST DATA FOR GROUPING CODE
            onfile%, maxlines% = 0
            call "GETCODE" (#gmas%, code$, descr$, 0%, 1, f1%(gmas%))
            if f1%(gmas%) = 1% then L50340
            if code$ <> " " then return
                errormsg$ = "Group Code May NOT Be Blank"
                return

L50340:     gosub L30000
            return clear all
            goto editmode

L50380: REM TEST DATA FOR REFERENCE TEXT
            if descr$=" " then errormsg$="Reference Text may not be blank"
            return

L50420: REM TEST DATA FOR DEFAULT TEXT FOR REPORTS
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152(t%)
            errormsg$ = " "

            REM TEST DATA FOR ACCOUNT NUMBER
            f1%(3) = -999% /* Include Obsolete Accounts */
            call "GETCODE" (#main%, account$(t%), acctdescr$(t%), 0%, 0, ~
                f1%(main%))
                if f1%(main%) = 1 then L51140
                errormsg$ = "Undefined Account Number"
                return
L51140:     get #main%, using L51150, accttype$
L51150:     FMT XX(39), CH(1)
            gosub format_type
            accttype$(t%) = accttype$

            test_for_dup /* Test For Duplicate Entries */
            temp$ = account$(t%)
            account$(t%) = " "
            search account$() = str(temp$,,16) to cursor%() step 16
            account$(t%) = temp$
            if cursor%(1) = 0 then return
                errormsg$ = "This Account Is Already Included In This "  ~
                                                       & "Grouping Code"
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52110,         /* STARTING CODE    */~
                                    L52140          /* ENDING CODE      */
                     return
L52110:     REM TEST DATA FOR STARTING CODE
            return

L52140:     REM TEST DATA FOR ENDING CODE
            if endcode$ = " " then return
            if startcode$ > endcode$ then errormsg$ = "Ending Code Can't ~
        ~Be Greater Then The Starting Code"
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

            call "SHOSTAT" ("One Moment Please")
            end
