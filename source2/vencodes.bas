        REM *************************************************************~
            *                                                           *~
            *  V   V  EEEEE  N   N   CCC    OOO   DDDD   EEEEE    SSS   *~
            *  V   V  E      NN  N  C      O   O  D   D  E       S      *~
            *  V   V  EEEE   N N N  C      O   O  D   D  EEE      SSS   *~
            *   V V   E      N  NN  C      O   O  D   D  E           S  *~
            *    V    EEEEE  N   N   CCC    OOO   DDDD   EEEEE    SSS   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VENCODES - PRINTS LIST OF VENDOR MASTERS SORTED BY CODE.  *~
            *-----------------------------------------------------------*~
            *                 M O D I F I C A T I O N S                 *~
            *---WHEN---+------------------WHAT--------------------+-WHO-*~
            * 03/17/80 ! ORIGINAL (RIPPED OFF FROM CUSCODES)      ! BCW *~
            * 07/10/80 ! CLEAN UP ALL SORTS OF STUFF.             ! BCW *~
            * 07/15/81 ! ADD PURCHASING ADDRESS                   ! TEM *~
            * 07/13/83 ! CALL TO 'MANUAL' ADDED                   ! HES *~
            * 07/13/83 ! CALLS TO 'FILEOPEN' CHANGED TO 'OPENFILE'! HES *~
            * 09/30/85 ! (1) File format change to VENDOR         ! ERN *~
            *          ! (2) Added Buy From's to Listing          !     *~
            *          ! (3) Added Text to Listing                !     *~
            *          ! (4) Reformat report (big time)           !     *~
            * 02/08/88 ! UPDATED DIM & FMT FOR TAX ID NUMBER      ! BPN *~
            *          ! ALSO ADDED FREIGHT ACCOUNT TO REPORT     ! BPN *~
            * 09/07/89 ! Modifided test of TXTPRINT status.       ! JDH *~
            * 04/09/92 ! PRR 11745 - Added Price/Cost Var Acct.   ! MLJ *~
            * 10/15/92 ! Added Fax Number & Default Currency Code.! JDH *~
            * 01/13/93 ! Page 0 Facs fix                          ! RJH *~
            *************************************************************


        dim                                                              ~
            acct$(7)9,                   /* Account Numbers- Internal  */~
            addr$(6)30,                  /* Name and Address           */~
            bfcode$6,                    /* Buy From Code              */~
            blankline$79,                /* Blank Line for Screen      */~
            buy_froms$1,                 /* List Buy Froms? (Y/N)      */~
            confirm$1,                   /* Confirmation Required?     */~
            contact$20,                  /* Name of Contact at Vendor  */~
            currency$4,                  /* Default Currency Code      */~
            cursor%(2),                  /* Cursor Position            */~
            date$8,                      /* Date for Screen            */~
            errormsg$79,                 /* Error Message              */~
            fax$7,                       /* Fax Number sans Area Code  */~
            firstin$9,                   /* First Vendor to List- Scrn */~
            firstven$9,                  /* First Vendor code in range */~
            fob$30,                      /* Free on Board              */~
            frt$1,                       /* Freight Terms              */~
            hdrdate$45,                  /* Print formatted Date/Time  */~
            i$(24)80,                    /* Screen Image               */~
            lastin$9,                    /* Last Vendor to List- Scrn  */~
            lastven$9,                   /* Last Vendor COde in Range  */~
            linemask$132,                /* Text Print Output Mask     */~
            line2$79,                    /* Second screen line         */~
            mcon$1,                      /* Multi-currency on flag     */~
            phone$10,                    /* Phone Number               */~
            plowkey$30,                  /* Plow Routines Variable     */~
            prtacct$12,                  /* Account Number - Print     */~
            prtacctdescr$30,             /* Account Descr - Print      */~
            prtaccthdr$51,               /* Account & Other Info       */~
            prtbfcode$6,                 /* Buy From Code - Print      */~
            prtcontact$20,               /* Contact's Name - Print     */~
            prtname$30,                  /* Name/Address - Print       */~
            prtvencode$9,                /* Vendor Code - Print        */~
            ship$30,                     /* Ship Via                   */~
            tasktype$1,                  /* Task Type (Online, Batch)  */~
            tax$1,                       /* Taxable?                   */~
            time$8,                      /* System Time                */~
            ten99$4,                     /* 1099 Category Code         */~
            text$1,                      /* Print Text? (Y/N)          */~
            textid$4,                    /* Text ID to Print           */~
            tin$12,                      /* Tax Identification Number  */~
            vencode$9,                   /* Current Vendor Code        */~
            vendescr$30                  /* Internal Description       */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD STATUS FLAGS        */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
        REM *************************************************************
        mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.  THEY ARE AN INTRINSIC PART OF THE   */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !          D E S C R I P T I O N           *~
            *-----+----------+------------------------------------------*~
            * # 1 ! VENDOR   ! Vendor Master File                       *~
            * # 2 ! VENDORBF ! Vendor Buy From File                     *~
            * # 3 ! GLMAIN   ! General Ledger Chart of Accounts         *~
            * # 4 ! TXTFILE  ! System Text File                         *~
            * # 5 ! SYSFILE2 ! Caelus Management System General Informa *~
            *************************************************************

            select #1,  "VENDOR",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 600,                                  ~
                         keypos  =   1, keylen = 9,                      ~
                         alt key  1, keypos = 10, keylen = 30, dup

            select # 2, "VENDORBF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 15

            select # 3, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select # 4, "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos  = 1, keylen = 11

            select # 5, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

        call "SHOSTAT" ("Opening files, one moment please")
            call"OPENFILE"(#1, "SHARE", f2%(1), rslt$(1), axd$(1))
            call"OPENFILE"(#2, "SHARE", f2%(2), rslt$(2), axd$(2))
            call"OPENFILE"(#3, "SHARE", f2%(3), rslt$(3), axd$(3))
            call"OPENFILE"(#4, "SHARE", f2%(4), rslt$(4), axd$(4))
            call"OPENFILE"(#5, "SHARE", f2%(5), rslt$(5), axd$(5))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * SETS CONTROL VARIABLES.                                   *~
            *************************************************************

            date$ = date  :   call "DATEFMT" (date$)
            str(line2$,62) = "VENCODES: " & str(cms2v$,1,8)

            call "EXTRACT" addr("TT", tasktype$)

            line% = 857%  :  u3% = 0%  :  pagenr% = -1%

            str(linemask$, 17) = "!"
            str(linemask$,121) = "!"

            blankline$ = "Enter Report Parameters then Press PF-9 to" &  ~
                         " Print Report."

            mcon$ = "N"
            call "READ100" (#5, "SWITCHS.CUR", f1%(5%))
            if f1%(5%) = 1% then get #5 using L09220, mcon$
L09220:         FMT POS(21), CH(1)

        REM *************************************************************~
            *         I N P U T   R A N G E   T O   P R I N T           *~
            * --------------------------------------------------------- *~
            * INPUTS THE RANGE OF VENDORS TO PRINT.  HANDLES BACKGROUND *~
            * MODE.                                                     *~
            *************************************************************

        inputmode
            init(" ") errormsg$, firstven$, lastven$, firstin$, lastin$

            init("Y") text$, buy_froms$
            firstin$ = "ALL"
            if tasktype$ = "B" then L10240

L10160:     gosub L40000                  /* PARAMETERS SCREEN  */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       L65000
            gosub L50000                  /* TEST DATA        */
                  if errormsg$ <> " " then L10160
            if keyhit% <> 9% then L10160

        select printer(134)
        gosub print_params

        call "SHOSTAT" ("PRINTING VENDOR LISTING")
L10240:     firstven$ = firstin$
            lastven$  = lastin$
*        Set ranges for listing.
            if firstven$ <> "ALL" then L10310       /* Print All of them*/
                init(hex(00)) firstven$
                init(hex(ff)) lastven$
                goto L11000
L10310:     if lastven$ = " " then lastven$ = firstven$
            firstven$ = firstven$ addc hex(fffffffffffffffffe)


L11000: REM *************************************************************~
            *         P L O W   R O U T I N E   F O R   R A N G E       *~
            * --------------------------------------------------------- *~
            * PLOWS THROUGH RANGE OF VENDORS TO PRINT.                  *~
            *************************************************************

            call "SETPRNT" ("VEN001", " ", 0%, 0%)

        remit_to_loop
            call "PLOWNEXT" (#1, firstven$, 0%, f1%(1))
            if f1%(1) = 0 or firstven$ > lastven$ then L11340

            get #1, using L11220, vencode$, vendescr$,                    ~
                                 addr$(), contact$, phone$,              ~
                                 acct$(1), acct$(3), acct$(4), acct$(5), ~
                                 billsdue,discsdue,discpercent,acct$(7), ~
                                 confirm$, frt$, fob$, tax$, ship$,      ~
                                 type$, acct$(2), textid$, tin$, ten99$, ~
                                 acct$(6), currency$, fax$
            bfcode$ = " "
            gosub print_vendor           /* PRINT THIS VENDOR.         */
            if buy_froms$ = "Y" then gosub print_buy_froms
            vendors% = vendors% + 1%
            goto remit_to_loop

L11220:        FMT CH(9),                /* VENDOR CODE                */~
                   CH(30),               /* VENDOR DESCRIPTION         */~
                   6*CH(30),             /* VENDOR NAME & ADDRESS      */~
                   CH(20),               /* CONTACT'S NAME             */~
                   CH(10),               /* PHONE NUMBER               */~
                   4*CH(9),              /* PUR, PAY, CASH, DISC ACCTS */~
                   2*PD(14,4),           /* BILLS, DISCOUNTS DUE (DAYS)*/~
                   PD(14,4),             /* DISCOUNT PERCENT GIVEN     */~
                   POS(318),             /* SKIP BALANCE               */~
                   CH(9),                /* PRICE/COST VARIANCE ACCT   */~
                   POS(414),             /* SKIP HISTORY               */~
                   CH(1),                /* Confirmed                  */~
                   CH(1),                /* Freight                    */~
                   CH(30),               /* F O B                      */~
                   CH(1),                /* Taxable?                   */~
                   CH(30),               /* Ship Via                   */~
                   CH(4),                /* Vendor Type                */~
                   CH(09),               /* INTERIM LIABILITIES ACCT   */~
                   CH(04),               /* TEXT ID                    */~
                   XX(09),               /* Main Vendor Number         */~
                   CH(12),               /* Tax Identification Number  */~
                   CH(04),               /* 1099 Category              */~
                   CH(09),               /* FREIGHT ACCOUNT            */~
                   CH(04),               /* Default Currency Code      */~
                   CH(07)                /* Fax Number                 */

L11340
*            Print total number of Vendors printed.
*              IF VENDORS% = 0 THEN 65000
                time$ = " "  :  call "TIME" (time$)
                print skip(2)
                print using L29150, vendors%, buyfroms%, time$
                goto L65000

        REM *************************************************************~
            * P R I N T   B U Y   F R O M S                             *~
            * --------------------------------------------------------- *~
            * List the Buy froms for the Remit-to vendor                *~
            *************************************************************
        print_buy_froms
            init (" ") acct$()
            plowkey$ = str(vencode$) & hex(00)

        buy_from_loop
            call "PLOWNEXT" (#2, plowkey$, 9%, f1%(2))
            if f1%(2) = 0% then return

            fax$ = " "

            get #2, using L12250, bfcode$, vendescr$,                     ~
                                 addr$(), contact$, phone$,              ~
                                 confirm$, frt$, fob$, ship$, tax$,      ~
                                 acct$(1), acct$(2), textid$

            gosub print_vendor           /* Print this vendor.         */
            buyfroms% = buyfroms% + 1%
            goto buy_from_loop

L12250:        FMT XX(9),                /* VENDOR CODE                */~
                   CH(6),                /* Buy From Code              */~
                   CH(30),               /* VENDOR DESCRIPTION         */~
                   6*CH(30),             /* VENDOR NAME & ADDRESS      */~
                   CH(20),               /* CONTACT'S NAME             */~
                   CH(10),               /* PHONE NUMBER               */~
                   CH(1),                /* Confirmed                  */~
                   CH(1),                /* Freight                    */~
                   CH(30),               /* F O B                      */~
                   CH(30),               /* Ship Via                   */~
                   CH(01),               /* Taxable?                   */~
                   2*CH(09),             /* Purchases/Interim Liab     */~
                   CH(04)                /* Text ID                    */

        REM *************************************************************~
            * P R I N T   V E N D O R                                   *~
            * --------------------------------------------------------- *~
            * Print the stuff for the Vendor (Remit-To or Buy From)     *~
            * We'll go line by line - simple and straight forward.      *~
            *************************************************************
        print_vendor
            if line% > 50% then gosub page_heading

*        Line ONE.
            gosub init_line
            prtvencode$  = vencode$
            prtbfcode$   = bfcode$
            prtname$     = vendescr$
            prtcontact$  = contact$
            prtaccthdr$  = "PURCHASES   :"
            prtacct$     = acct$(1) : gosub descr_acct
            if bfcode$  <> " " then L20105
                prttermshdr$ = "BILL:"
                    convert abs(billsdue) to prtterms$, pic(###0)
                    if billsdue < 0 then str(prtterms$,5) = "P"
L20105:     gosub print_line

*        Line TWO
            gosub init_line
            prtname$     = addr$(1)
            prtcontact$  = "(" & str(phone$,,3) & ") " & str(phone$,4,3)&~
                           "-" & str(phone$, 7)
            prtaccthdr$  = "INTERIM LIAB:"
            prtacct$     = acct$(2) : gosub descr_acct
            if bfcode$  <> " " then L20170
                prttermshdr$ = "DISC:"
                    convert abs(discsdue) to prtterms$, pic(###0)
                    if discsdue < 0 then str(prtterms$,5) = "P"
L20170:     gosub print_line

*        Line THREE
            gosub init_line
            prtname$     = addr$(2)
            if bfcode$  <> " " then L20225
                prtcontact$  = "FAX # " & str(fax$,1,3) &"-"&str(fax$,4,4)
                prtaccthdr$  = "ACCTS PAYBLE:"
                prtacct$     = acct$(3) : gosub descr_acct
                prttermshdr$ = "DSC%:"
                    convert discpercent to prtterms$, pic(##.00)
L20225:     gosub print_line

*        Line FOUR
            gosub init_line
            prtname$     = addr$(3)
            prtcontact$  = "CONFIRM? : " & confirm$
            if bfcode$  <> " " then L20270
                prtaccthdr$  = "CASH IN BANK:"
                prtacct$     = acct$(4) : gosub descr_acct
L20270:     gosub print_line

*        Line FIVE
            gosub init_line
            prtname$     = addr$(4)
            prtcontact$  = "TAXABLE? : " & tax$
            if bfcode$  <> " " then L20314
                prtaccthdr$  = "DISCS TAKEN :"
                prtacct$     = acct$(5) : gosub descr_acct
                prttermshdr$ = "1099:"  :  prtterms$ = ten99$
L20314:     gosub print_line

*        Line SIX
            gosub init_line
            prtname$     = addr$(5)
            if frt$ = "A" then prtcontact$ = "ADD"
            if frt$ = "C" then prtcontact$ = "COLLECT"
            if frt$ = "N" then prtcontact$ = "NOT ADDED"
            prtcontact$ = "FRT TERMS: " & prtcontact$
            if bfcode$  <> " " then L20339
                prtaccthdr$  = "PRC/CST VAR :"
                prtacct$     = acct$(7) : gosub descr_acct
L20339:     gosub print_line

*        Line SEVEN
            gosub init_line
            prtname$     = addr$(6)
            if bfcode$  <> " " then L20370
                prtcontact$  = "TYPE CODE: " & type$
                prtaccthdr$  = "FRT EXPENSE :"
                prtacct$     = acct$(6) : gosub descr_acct
L20370:     gosub print_line

*        LINE EIGHT
            gosub init_line
            prtcontact$  = "TAX ID: " & tin$
            prtaccthdr$  = "F.O.B.      : " & fob$
            gosub print_line

*        Line NINE
            gosub init_line
            prtaccthdr$  = "SHIP        : " & ship$
            if mcon$ <> "Y" then L20440
            if bfcode$  <> " " then L20440
                prtcontact$  = "CURRENCY : " & currency$
L20440:     gosub print_line

*        Print Text if so required.
            if text$ = "N" or textid$ = hex(00000000) or                 ~
                              textid$ = hex(20202020) or                 ~
                              textid$ = hex(ffffffff) then textid$ = " "
            if textid$ <> " " then L20500
                print using L29070
                line% = line% + 1%
                if line% > 50% then gosub page_heading
                return

L20500:     prt% = 0%
L20505:     if line% > 50% then gosub page_heading
            call "TXTPRINT" (#4, f2%(4), 134%, textid$, "VEN001", 30%,   ~
                             line%, 50%, "Y", linemask$, prt%)
            if prt% <> 0% then L20505
            print using L29070
            line% = line% + 1%
            return


*       ******** END OF STRAIGHT FORWARD PRINT ROUTINE *****************

        init_line
*        Clear Print Line variables.
            init (" ")    prtname$, prtcontact$, prtaccthdr$, prtacct$,  ~
                          prtacctdescr$, prttermshdr$, prtterms$,        ~
                          prtvencode$, prtbfcode$
            return


        descr_acct
*        Decode account number and get description.
            prtacctdescr$ = " "
            if prtacct$ = " " then return
            call "READ100" (#3, prtacct$, f1%(3))
            if f1%(3) = 1% then get #3 using L21090, prtacctdescr$  else  ~
                                prtacctdescr$ = "** NOT ON FILE **"
L21090:         FMT XX(9), CH(30)
            call "GLFMT" (prtacct$)
            str(prtaccthdr$,15) = str(prtacct$) & " " & prtacctdescr$
            return

        print_line
*        Print the line.
            print using L29110, prtvencode$, prtbfcode$, prtname$,        ~
                               prtcontact$, prtaccthdr$, prttermshdr$,   ~
                               prtterms$
            line% = line% + 1%
            return


        page_heading
            call "DATE" addr("HD", hdrdate$)
            pagenr% = pagenr% + 1
            print page
            print using L29000, pagenr%, hdrdate$
            print using L29070
            print using L29040
            print using L29070
            line% = 4%
            return


L29000: %PAGE #####   VEN001    V E N D O R   L I S T I N G   B Y   C O D~
        ~ E                     ##########################################~
        ~###

L29040: %  VENDOR - FROM ! VENDOR DESCR/ NAME & ADDRESS ! CONTACT/PHONE/E~
        ~TC. !    GENERAL LEDGER ACCOUNTS / MISC.                !  TERMS

L29070: %---------+------+------------------------------+----------------~
        ~----+---------------------------------------------------+--------~
        ~---

L29110: %######### ######!##############################!################~
        ~####!###################################################!##### ##~
        ~###

L29150: %  LISTED: ###### VENDORS, ###### BUY FROM LOCATIONS, (########)

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode



        print_params

            gosub page_heading
L31945:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L31965
                str(i$(), i%, 1%) = hex(20)
                goto L31945
L31965:     print skip(3)
            print tab(37);
            print "--------------------- Report Selection Parameters ----~
        ~---------"
            for x% = 6% to 17%: print tab(37); i$(x%) : next x%
            print tab(37);
            print "------------------------------------------------------~
        ~---------"
            line% = 99%
            return

L40000: REM *************************************************************~
            *         D E F I N E   R A N G E   T O   P R I N T         *~
            *                                                           *~
            * DEFINES A RANGE TO PRINT FOR FUN AND GOOD TIMES.          *~
            *************************************************************

L40060:     accept                                                       ~
               at (01,02),                                               ~
                  "PRINT VENDORS BY CODE NUMBER",                        ~
               at (01,67), "Date: ",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch( 8),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02),                                               ~
                  "First Vendor",                                        ~
               at (06,30), fac(hex(81)), firstin$               , ch( 9),~
               at (07,02),                                               ~
                  "Last Vendor",                                         ~
               at (07,30), fac(hex(81)), lastin$                , ch( 9),~
               at (08,02), "List Buy From Locs (Y/N)",                   ~
               at (08,30), fac(hex(81)), buy_froms$             , ch(01),~
               at (09,02), "Print Vendor Text? (Y/N)",                   ~
               at (09,30), fac(hex(81)), text$                  , ch(01),~
               at (21,02), fac(hex(ac)), blankline$             , ch(79),~
               at (23,02), "(1)Start Over",                              ~
               at (23,30), "(9)Print Report",                            ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(hex(0001090d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40320
                  call "MANUAL" ("VENCODES")
                  goto L40060

L40320:        if keyhit% <> 15 then L40360
                  call "PRNTSCRN"
                  return

L40360:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

L50000: REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            *************************************************************

            errormsg$ = " "
            if firstin$ <> "ALL" then L50150
                lastin$ = " "
                goto L50270
L50150:     if lastin$ <> " " then L50190           /* --> 1 Vendor     */
                lastin$ = firstin$
L50190:     if lastin$ >= firstin$ then L50270      /* Test Range       */
                errormsg$ = "ILLEGAL RANGE!  PLEASE RESPECIFY."
                return

L50270
*        Test for INCLUDE BUY FROMS
            if pos("YN" = buy_froms$) <> 0% then L50320
                errormsg$ = "Enter Y/N for List Buy From Locations"
                return

L50320
*        Test for LIST TEXT OPTION
            if pos("YN" = text$) <> 0% then return
                errormsg$ = "Enter Y/N for Print Text Option."
                return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("ONE MOMENT PLEASE")
            call "SETPRNT" ("VEN001", " ", 0%, 1%)
            end
