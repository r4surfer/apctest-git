        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC    AAA   TTTTT  IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  C      A   A    T      I    NN  N  P   P  U   U    T     *~
            *  C      AAAAA    T      I    N N N  PPPP   U   U    T     *~
            *  C      A   A    T      I    N  NN  P      U   U    T     *~
            *   CCC   A   A    T    IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CATINPUT - Manage Part Category File.                     *~
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
            * 01/08/81 ! ORIGINAL                                 ! BCW *~
            * 05/28/86 ! Code rewritten.  History removed and     ! ERN *~
            *          !   sales and sales discount account       !     *~
            *          !   defaults added.                        !     *~
            * 08/05/87 ! Fixed Error Message on Invalid Account # !     *~
            *          !   Sales Account Entries are now Optional ! DAW *~
            * 05/03/93 ! Add Count Factor Field (Pieces per Hour) ! RJH *~
            *09/06/2016! (SR77231) mod to add credit account      ! CMN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cat$4,                       /* Part Category Code         */~
            count_factor$10,             /* Counts per hour per part   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr$30,                    /* Category Description       */~
            discacct$16,                 /* Sales Discount Acct Dflt   */~
            discacctdescr$30,            /* Sales Discount Acct Descr  */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error Message              */~
/*          FILLER$148,                     FILLER                     */~
            filler$122,                  /* Filler (SR77231)           */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Input Message              */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line 2              */~
            pf12$16, pf14$18, pf16$16,   /* PF Key Descriptions        */~
            plowcode$20,                 /* Plow key for report        */~
            runtime$8,                   /* Report Run Time            */~
            slsacct$16,                  /* Sales Account Default      */~
            slsacctdescr$30              /* Sales Account Description  */

        dim                                                              ~
            slsCRacct$16,                /* Sales Account Default      */~
            slsCRacctdescr$30,           /* Sales Account Description  */~
            discCRacct$16,               /* Sales Discount Acct Dflt   */~
            discCRacctdescr$30           /* Sales Discount Acct Descr  */



        dim f2%(64),                     /* File Status Flags for      */~
            f1%(64)                      /* Record-on-file flags       */


            dim cms2v$50
            cms2v$ = "R6.02.04 06/29/93 SFC & Cycle Count Enhancements  "

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! CATEGORY ! Part Category File                       *~
            * #2  ! GLMAIN   ! General Ledger File                      *~
            *************************************************************

            select # 1, "CATEGORY",                                      ~
                        varc, indexed,                                   ~
                        recsize = 200,                                   ~
                        keypos  =   1, keylen = 4

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

            str(line2$,62) = "CATINPUT: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * INPUT MODE.                                               *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, cat$, descr$, slsacct$,    ~
                      slsacctdescr$, discacct$, discacctdescr$,          ~
                      count_factor$, slsCRacct$, slsCRacctdescr$,        ~
                      discCRacct$, discCRacctdescr$ /* SR77231 */
            for fieldnr% = 1% to 7%      /* (SR77231)  */
                gosub'051(fieldnr%)      /* Set Input Messages         */
                      if enabled% = 0 then L10220
L10150:         gosub'101(fieldnr%)      /* Input Screen               */
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 14 and fieldnr% = 1 then print_report
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10150
                gosub'151(fieldnr%)      /* Test Data Entered          */
                      if errormsg$ <> " " then L10150
L10220:         next fieldnr%


L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT MODE.                           *~
            *************************************************************

L11060:     gosub'111(0%)                /* Edit Screen                */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 12 then gosub delete_code
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11060
L11110:     fieldnr% = cursor%(1) - 5%
            if fieldnr% < 2% or fieldnr% > 7% then L11060   /* (SR77231)  */

L11140:     gosub'051(fieldnr%)          /* Set Input Messages         */
            gosub'111(fieldnr%)          /* Edit Screen                */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11140
            gosub'151(fieldnr%)          /* Test Data Entered          */
                  if errormsg$ <> " " then L11140
                  if fieldnr% = cursor%(1) - 5% then L11060 else L11110


        delete_code
            keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "D E L E T E",                     ~
                            "Enter PF-16 to DELETE Category Code",       ~
                            "-OR-", "Press RETURN to CANCEL delete.")
            if keyhit1% <> 16% then return
                call "DELETE" (#1, cat$, 4%)
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
            call "SETPRNT" ("CAT001", " ", 0%, 0%)
            call "SHOSTAT" ("PRINTING CATEGORY CODES LISTING")
            select printer
            call "TIME" (runtime$)

        report_loop
            call "PLOWNEXT" (#1, plowcode$, 0%, f1%(1))
            if f1%(1) = 1% then L12240
                runtime$ = " "
                call "TIME" (runtime$)
                print
                print using L12590, runtime$
                close printer
                call "SETPRNT" ("CAT001", " ", 0%, 1%)
                goto inputmode

L12240:     get #1 using L12250, cat$, descr$, slsacct$, discacct$,       ~
/* SR77231 */                   count_factor, slsCRacct$, discCRacct$
L12250:         FMT CH(4), CH(30), 2*CH(9), PD(14,4), 2*CH(9)
            call "DESCRIBE" (#2, slsacct$ , slsacctdescr$ , 0%, f1%(2))
            call "DESCRIBE" (#2, discacct$, discacctdescr$, 0%, f1%(2))
            call "GLFMT" (slsacct$ )
            call "GLFMT" (discacct$)

/*SR77231*/ call "DESCRIBE" (#2, slsCRacct$ , slsCRacctdescr$ , 0%, f1%(2))
            call "DESCRIBE" (#2, discCRacct$, discCRacctdescr$, 0%, f1%(2))
            call "GLFMT" (slsCRacct$ )
/*SR77231*/ call "GLFMT" (discCRacct$)

            call "CONVERT" (count_factor, 2.2, count_factor$)
            if line% > 55% then gosub page_heading
            print using L12560, cat$, descr$, slsacct$, slsacctdescr$,    ~
                               discacct$, discacctdescr$, count_factor$
            line% = line% + 1%
            goto report_loop


        page_heading
            print page
            page% = page% + 1% : line% = 7%
            print using L12480, date$
            print using L12500, runtime$, page%
            print
            print using L12520
            print using L12540
            return


L12480: %RUN ########                       C A T E G O R Y   C O D E S  ~
        ~ L I S T I N G                                     CATINPUT:CAT00~
        ~1

L12500: %    ########                                                    ~
        ~                                                       PAGE:  ###~
        ~#
L12520: %CODE D E S C R I P T I O N         SALES ACCT   SALES ACCOUNT DE~
        ~SCRIPTION     DISCS ACCT   SALES DISCOUNTS ACCNT DESCR  COUNT RAT~
        ~E
L12540: %---- ----------------------------- ------------ ----------------~
        ~------------- ------------ ---------------------------- ---------~
        ~-
L12560: %#### ############################# ############ ################~
        ~############# ############ ############################ #########~
        ~#

L12590: %                                    ***  END OF REPORT  @  #####~
        ~###  ***

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
                  on fieldnr% gosub L20150,         /* Category Code    */~
                                    L20190,         /* Description      */~
                                    L20230,         /* Sales Account    */~
                                    L20270,         /* Sales Disc Acct  */~
                                    L20310,         /* Count Factor     */~
/* (SR77231) */                     L20350,         /* Sales CR Account */~
/* (SR77231) */                     L20390          /* Sales Disc CR Acc*/
                     return

L20150
*        Default/Enable for CATEGORY CODE
            inpmessage$ = "Enter Category Code."
            return

L20190
*        Default/Enable for DESCRIPTION
            inpmessage$ = "Enter Description for this Category."
            return

L20230
*        Default/Enable for SALES ACCOUNT
            inpmessage$ = "Enter default Sales Distribution Account, Ente~
        ~r a '?' to View List of G/L Codes."
            return

L20270
*        Default/Enable for SALES DISCOUNT ACCOUNT
            inpmessage$ = "Enter default Sales Discounts Account, Enter a~
        ~ '?' to View List of G/L Codes."
            return

L20310
*        Default/Enable for Count Factor
            inpmessage$ = "Enter default Count Rate per Part (Number of P~
        ~ieces per Hour).              "
            return

L20350            /* (SR77231)  */
*        Default/Enable for SALES ACCOUNT
            inpmessage$ = "Enter default Sales Credit Distribution Account~
        ~, Enter a '?' to View List of G/L Codes."
            return

L20390            /* (SR77231)  */
*        Default/Enable for SALES DISCOUNT ACCOUNT
            inpmessage$ = "Enter default Sales Discounts Credit Account, E~
        ~nter a '?' to View List of G/L Codes."
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
            *           L O A D   C A T E G O R Y   R E C O R D         *~
            * --------------------------------------------------------- *~
            * Loads CATEGORY record from disk.                          *~
            *************************************************************

            call "READ101" (#1, cat$, f1%(1))
            if f1%(1) = 0% then return
                get #1 using L30160, cat$, descr$, slsacct$, discacct$,   ~
/* SR77231 */                       count_factor, slsCRacct$, discCRacct$,~
                                    filler$
                call "DESCRIBE" (#2, slsacct$, slsacctdescr$, 0%, f1%(2))
                call "DESCRIBE" (#2, discacct$,discacctdescr$,0%, f1%(2))
                call "GLFMT" (slsacct$ )
                call "GLFMT" (discacct$)
                call "CONVERT" (count_factor, -2.2, count_factor$)

/* SR77231 */   call "DESCRIBE" (#2, slsCRacct$, slsCRacctdescr$, 0%, f1%(2))
/* SR77231 */   call "DESCRIBE" (#2, discCRacct$,discCRacctdescr$,0%, f1%(2))
/* SR77231 */   call "GLFMT" (slsCRacct$ )
/* SR77231 */   call "GLFMT" (discCRacct$)
                return

L30160:     FMT CH(04),                  /* Category Code              */~
                CH(30),                  /* Description                */~
                CH(09),                  /* Dflt Sales Account         */~
                CH(09),                  /* Dflt Discounts Account     */~
                PD(14,4),                /* Count Factor               */~
                CH(09),                  /* Dflt Sales CR Account      */~
                CH(09),                  /* Dflt Discounts CR Account  */~
                CH(122)                  /* FILLER                     */

L31000: REM *************************************************************~
            *          S A V E   T A X C O D E   R E C O R D            *~
            * --------------------------------------------------------- *~
            * Writes CATEGORY record.                                   *~
            *************************************************************

            call "GLUNFMT" (slsacct$ )
            call "GLUNFMT" (discacct$)
            put #1 using L31130, cat$, descr$, slsacct$, discacct$,       ~
                                count_factor, slsCRacct$, discCRacct$, " "
            if f1%(1) = 1% then rewrite #1 else write #1
            return


L31130:     FMT CH(04),                  /* Category Code              */~
                CH(30),                  /* Description                */~
                CH(09),                  /* Dflt Sales Account         */~
                CH(09),                  /* Dlft Discounts Account     */~
                PD(14,4),                /* Count Factor               */~
                CH(09),                  /* Dflt Sales CR Account      */~
                CH(09),                  /* Dlft Discounts CR Account  */~
                CH(122)                  /* Filler                     */


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

L40220:     on fieldnr%  gosub       L40310,        /* Code             */~
                                     L40280,        /* Description      */~
                                     L40310,        /* Sales Account    */~
                                     L40310,        /* Discs Account    */~
                                     L40340,        /* Count Factor     */~
                                     L40310,        /* Sales Account    */~
                                     L40310         /* Discs Account    */
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
            at (01,02), "Category Codes Management",                     ~
            at (01,66), "Today: ",                                       ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (06,02), "Category Code",                                 ~
            at (06,30), fac(lfac$( 1)), cat$                    , ch(04),~
            at (07,02), "Description",                                   ~
            at (07,30), fac(lfac$( 2)), descr$                  , ch(30),~
            at (08,02), "Default Sales Account",                         ~
            at (08,30), fac(lfac$( 3)), slsacct$                , ch(12),~
            at (08,49), fac(hex(8c)),   slsacctdescr$           , ch(30),~
            at (09,02), "Sales Discounts Account",                       ~
            at (09,30), fac(lfac$( 4)), discacct$               , ch(12),~
            at (09,49), fac(hex(8c)),   discacctdescr$          , ch(30),~
            at (10,02), "Count Rate per Part    ",                       ~
            at (10,30), fac(lfac$(5%)), count_factor$           , ch(10),~
            at (11,02), "Default Sales CR Acct  ",                       ~
            at (11,30), fac(lfac$( 6)), slsCRacct$              , ch(12),~
            at (11,49), fac(hex(8c)),   slsCRacctdescr$         , ch(30),~
            at (12,02), "Sales Discounts CR Acc ",                       ~
            at (12,30), fac(lfac$( 7)), discCRacct$             , ch(12),~
            at (12,49), fac(hex(8c)),   discCRacctdescr$        , ch(30),~
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
                  call "MANUAL" ("CATINPUT")
                  goto L40380

L40690:        if keyhit% <> 15 then L40730
                  call "PRNTSCRN"
                  goto L40380

L40730:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50140,         /* Category Code    */~
                                    L50250,         /* Description      */~
                                    L50300,         /* Sales Account    */~
                                    L50360,         /* Discount Account */~
                                    L50430,         /* Count Factor     */~
                                    L50470,         /* Sales Account    */~
                                    L50510          /* Discount Account */
                  return

L50140
*        Test Data for CATEGORY CODE
            if cat$ = "?" then  cat$ = " "
            if cat$ <> " " then L50200
                call "GETCODE" (#1, cat$, " ", 0%, 0, f1%(1))
                if f1%(1) <> 0% then L50200
                     errormsg$ = hex(00)
                     return
L50200:     gosub L30000
            if f1%(1) = 0% then return
                return clear all
                goto L11000

L50250
*        Test Data for DESCRIPTION
            if descr$ <> " " then return
                errormsg$ = "Sorry, Field Can't Be Blank"
                return

L50300
*        Test Data for SALES ACCOUNT
            if slsacct$ = " " then return
            slsacctdescr$ = hex(06) & "Select a Sales Account"
            call "GETCODE" (#2, slsacct$, slsacctdescr$, 0%, 0, f1%(2))
            if f1%(2) = 1% then return
                errormsg$ = "Erase Account to Leave Blank, or Press (RETU~
        ~RN) to View Account Code List."
                return

L50360
*        Test Data for DISCOUNTS ACCOUNT
            if discacct$ = " " then return
            discacctdescr$ = hex(06) & "Select a Sales Disc Account"
            call "GETCODE" (#2, discacct$, discacctdescr$, 0%, 0, f1%(2))
            if f1%(2) = 1% then return
                errormsg$ = "Erase Account to Leave Blank, or Press (RETU~
        ~RN) to View Account Code List."
                return

L50430: REM Test for Minimum Manhours             COUNT_FACTOR
            count_factor = 0.0
            convert count_factor$ to count_factor , data goto L50460
            call "CONVERT" (count_factor, -2.2, count_factor$)
            if count_factor >= 0.0 then return
L50460:         errormsg$ = "Please Enter a Positive Number"
            return

L50470:
*        Test Data for SALES CR ACCOUNT
            if slsCRacct$ = " " then return
            slsCRacctdescr$ = hex(06) & "Select a Sales CR Account"
            call "GETCODE" (#2, slsCRacct$, slsCRacctdescr$, 0%, 0, f1%(2))
            if f1%(2) = 1% then return
                errormsg$ = "Erase Account to Leave Blank, or Press (RETU~
        ~RN) to View Account Code List."
                return


L50510:
*        Test Data for DISCOUNTS CR ACCOUNT
            if discCRacct$ = " " then return
            discCRacctdescr$ = hex(06) & "Select a Sales Disc CR Account"
            call "GETCODE" (#2, discCRacct$, discCRacctdescr$, 0%, 0, f1%(2))
            if f1%(2) = 1% then return
                errormsg$ = "Erase Account to Leave Blank, or Press (RETU~
        ~RN) to View Account Code List."
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
