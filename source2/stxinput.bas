        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   TTTTT  X   X  IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  S        T     X X     I    NN  N  P   P  U   U    T     *~
            *   SSS     T      X      I    N N N  PPPP   U   U    T     *~
            *      S    T     X X     I    N  NN  P      U   U    T     *~
            *   SSS     T    X   X  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STXINPUT - ADD/CHANGE SALES TAX RECORDS                   *~
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
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/17/83 ! ORIGINAL (TAXINPUT)                      ! KEN *~
            * 05/28/86 ! Name changed, screens enhanced, code     ! ERN *~
            *          !   expanded, and account number added.    !     *~
            *          !   'History' removed.                     !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            account$12,                  /* Sales Tax Account default  */~
            acctdescr$30,                /* Account Description        */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error Message              */~
            filler$43,                   /* Filler                     */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Input Message              */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line 2              */~
            pf12$16, pf14$18, pf16$16,   /* PF Key Descriptions        */~
            plowcode$20,                 /* Plow key for report        */~
            runtime$8,                   /* Report Run Time            */~
            taxcode$10,                  /* Tax Code                   */~
            taxcodedesc$30,              /* Description                */~
            taxpercent$5                 /* Tax Percent                */

        dim f2%(64),                     /* File Status Flags for      */~
            f1%(64)                      /* Record-on-file flags       */


            dim cms2v$50
            cms2v$ = "04.17.01 11/20/86 Order process & planning #2     "

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  !  SATAXES ! SALES TAX FILE                           *~
            *************************************************************

            select # 1, "STXCODES",                                      ~
                        varc, indexed,                                   ~
                        recsize = 100,                                   ~
                        keypos  =   1, keylen = 10

            select # 2, "GLMAIN",                                        ~
                        varc, indexed,                                   ~
                        recsize = 300,                                   ~
                        keypos  =   1, keylen = 9

            call "OPENCHCK" (#1, 0%, f2%(1), 100%, " ")
            call "OPENCHCK" (#2, 0%, f2%(2),   0%, " ")


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor" &~
                          " to Desired Value And Press (ENTER)."

            str(line2$,62) = "STXINPUT: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * INPUT MODE.                                               *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, taxcode$, taxcodedesc$,    ~
                      taxpercent$, filler$, account$, acctdescr$
            taxpercent = 0

            for fieldnr% = 1% to 4%
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10220
L10150:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 14 and fieldnr% = 1 then print_report
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10150
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10150
L10220:         next fieldnr%


L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT MODE.                           *~
            *************************************************************

L11060:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 12 then gosub delete_code
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11060
L11110:     fieldnr% = cursor%(1) - 5%
            if fieldnr% < 2% or fieldnr% > 4% then L11060

L11140:     gosub'051(fieldnr%)
            gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11140
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11140
                  if fieldnr% = cursor%(1) - 5% then L11060 else L11110


        delete_code
            keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "D E L E T E",                     ~
                            "Enter PF-16 to DELETE Tax Code", "-OR-",    ~
                            "Press RETURN to CANCEL delete.")
            if keyhit1% <> 16% then return
                call "DELETE" (#1, taxcode$, 10%)
                return clear all
                goto inputmode


        REM *************************************************************~
            *                P R I N T   R E P O R T                    *~
            * --------------------------------------------------------- *~
            * Prints a listing of the available codes.                  *~
            *************************************************************

        print_report
            page% =   0%
            line% = 857%
            init (hex(00)) plowcode$
            call "SETPRNT" ("STX001", " ", 0%, 0%)
            call "SHOSTAT" ("PRINTING TAX CODE LISTING")
            select printer
            call "TIME" (runtime$)

        report_loop
            call "PLOWNEXT" (#1, plowcode$, 0%, f1%(1))
            if f1%(1) = 1% then L12240
                print
                print "END OF REPORT"
                close printer
                call "SETPRNT" ("STX001", " ", 0%, 1%)
                goto inputmode

L12240:     get #1 using L12260, taxcode$, taxcodedesc$, taxpercent,      ~
                                                                account$
L12260:         FMT CH(10), CH(30), PD(14,4), CH(9)
            call "DESCRIBE" (#2, account$, acctdescr$, 0%, f1%(2))
            call "GLFMT" (account$)
            if line% > 55% then gosub page_heading
            print using L12550, taxcode$, taxcodedesc$, taxpercent,       ~
                                                     account$, acctdescr$
            line% = line% + 1%
            goto report_loop


        page_heading
            print page
            page% = page% + 1% : line% = 7%
            print using L12470, date$
            print using L12490, runtime$, page%
            print
            print using L12510
            print using L12530
            return


L12470: %RUN ########                       T A X   C O D E S   L I S T I~
        ~ N G                                     STXINPUT:STX001
L12490: %    ########                                                    ~
        ~                                             PAGE: ###
L12510: %          TAX CODE        D E S C R I P T I O N                 ~
        ~PERCENT   ACCOUNT CODE  ACCOUNT DESCRIPTION
L12530: %          ----------      ------------------------------        ~
        ~-------   ------------  ------------------------------
L12550: %          ##########      ##############################        ~
        ~ ##.##    ############  ##############################



        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * Saves data on file after input/editing.                   *~
            *************************************************************

        datasave
            gosub L31000
            goto inputmode


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * --------------------------------------------------------- *~
            * Sets DEFAULTS and ENABLES Fields for the Page 1 of input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20150,         /* Tax Code         */~
                                    L20190,         /* Description      */~
                                    L20230,         /* Tax Percent      */~
                                    L20270          /* Sales Tax Acct   */

                     return

L20150
*        Default/Enable for TAX CODE
            inpmessage$ = "Enter Tax Code."
            return

L20190
*        Default/Enable for DESCRIPTION
            inpmessage$ = "Enter Description of this Tax Code."
            return

L20230
*        Default/Enable for TAX PERCENT
            inpmessage$ = "Enter Sales Tax Percent."
            return

L20270
*        Default/Enable for ACCOUNT
            inpmessage$ = "Enter Sales Tax Distribution Account Default."
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover
L29918:     u3% = 2%
            call "STARTOVR" (u3%)
            if u3%  = 1% then return
            if u3% <> 0% then L29918
                return clear all
                goto inputmode


L30000: REM *************************************************************~
            *          L O A D   T A X C O D E   R E C O R D            *~
            * --------------------------------------------------------- *~
            * Loads STXCODES record from disk.                          *~
            *************************************************************

            call "READ101" (#1, taxcode$, f1%(1))
            if f1%(1) = 0% then return
                get #1 using L30150, taxcode$, taxcodedesc$, taxpercent,  ~
                                    account$, filler$
                call "CONVERT" (taxpercent, -2.4, taxpercent$)
                call "DESCRIBE" (#2, account$, acctdescr$, 0%, f1%(2))
                call "GLFMT" (account$)
                return

L30150:     FMT CH(10),                  /* Tax Code                   */~
                CH(30),                  /* Description                */~
                PD(14,4),                /* Tax Percent                */~
                CH(09),                  /* Tax Account                */~
                CH(43)                   /* FILLER                     */


L31000: REM *************************************************************~
            *          S A V E   T A X C O D E   R E C O R D            *~
            * --------------------------------------------------------- *~
            * Writes STXCODES record.                                   *~
            *************************************************************

            call "GLUNFMT" (account$)
            put #1 using L31130, taxcode$, taxcodedesc$, taxpercent,      ~
                                account$, filler$
            if f1%(1) = 1% then rewrite #1 else write #1
            return


L31130:     FMT CH(10),                  /* Tax Code                   */~
                CH(30),                  /* Description                */~
                PD(14,4),                /* Tax Percent                */~
                CH(09),                  /* Account Code               */~
                CH(43)                   /* Filler                     */


        REM *************************************************************~
            *             T H E   S C R E E N                           *~
            * --------------------------------------------------------- *~
            * INPUT AND EDIT for the one and only screen.               *~
            *************************************************************

            deffn'101(fieldnr%)                    /* INPUT Mode       */
                init(hex(8c)) lfac$()
                pf12$, pf14$, pf16$ = " "
                if fieldnr%  = 1% then pf16$ = "(16)Exit Program"
                if fieldnr%  = 1% then pf14$ = "(14)Print Listing"
                goto L40220

            deffn'111(fieldnr%)                    /* EDIT Mode        */
                init(hex(8c)) lfac$()
                pf12$, pf14$, pf16$ = " "
                if fieldnr% <> 0% then L40220
                     init(hex(86)) lfac$()
                     pf12$ = "(12)Delete Code"
                     pf16$ = "(16)Save Data"
                     inpmessage$ = edtmessage$

L40220:     on fieldnr%  gosub       L40310,        /* Tax Code         */~
                                     L40280,        /* Description      */~
                                     L40340,        /* Tax Percent      */~
                                     L40310         /* Account Number   */
            goto L40380

L40280:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40310:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40340:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40380: accept                                                           ~
            at (01,02), "Sales Tax Record Maintenance",                  ~
            at (01,66), "Today: ",                                       ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (06,02), "Tax Code",                                      ~
            at (06,30), fac(lfac$( 1)), taxcode$                , ch(10),~
            at (07,02), "Description",                                   ~
            at (07,30), fac(lfac$( 2)), taxcodedesc$            , ch(30),~
            at (08,02), "Tax Percent",                                   ~
            at (08,30), fac(lfac$( 3)), taxpercent$             , ch(05),~
            at (09,02), "Sales Tax Account",                             ~
            at (09,30), fac(lfac$( 4)), account$                , ch(12),~
            at (09,49), fac(hex(8c)),   acctdescr$              , ch(30),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), "(1)Start Over",                                 ~
            at (23,20), fac(hex(8c)), pf12$,                             ~
            at (23,40), fac(hex(8c)), pf14$,                             ~
            at (22,65), "(13)Instructions",                              ~
            at (23,65), "(15)Print Screen",                              ~
            at (24,65), fac(hex(8c)), pf16$,                             ~
               keys(hex(00010c0d0e0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L40690
                  call "MANUAL" ("STXINPUT")
                  goto L40380

L40690:        if keyhit% <> 15 then L40730
                  call "PRNTSCRN"
                  goto L40380

L40730:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50140,         /* Tax Code         */~
                                    L50250,         /* Description      */~
                                    L50300,         /* Tax Percent      */~
                                    L50340          /* Account          */
                  return

L50140
*        Test Data for TAX CODE
            if taxcode$ <> " " then L50200
                call "GETCODE" (#1, taxcode$, " ", 0%, 0, f1%(1))
                if f1%(1) <> 0% then L50200
                     errormsg$ = hex(00)
                     return
L50200:     gosub L30000
            if f1%(1) = 0% then return
                return clear all
                goto L11000

L50250
*        Test Data for DESCRIPTION
            if taxcodedesc$ <> " " then return
                errormsg$ = "Sorry, Field Can't Be Blank"
                return

L50300
*        Test Data for TAX PERCENT
            call "NUMTEST"(taxpercent$, 0, 100, errormsg$, 2.4,taxpercent)
            return

L50340
*        Test Data for ACCOUNT
            call "GETCODE" (#2, account$, acctdescr$, 0%, 0, f1%(2))
            if f1%(2) = 1% then return
                errormsg$ = "ACCOUNT NOT ON FILE: " & account$
                return


L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please")

            end
