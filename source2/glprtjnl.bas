        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   GGG   L      PPPP   RRRR   TTTTT  JJJJJ  N   N  L       *~
            *  G      L      P   P  R   R    T      J    NN  N  L       *~
            *  G GGG  L      PPPP   RRRR     T      J    N N N  L       *~
            *  G   G  L      P      R   R    T    J J    N  NN  L       *~
            *   GGG   LLLLL  P      R   R    T     J     N   N  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLPRTJNL - This program prints GL detail already posted to*~
            *            GLDETAIL on a real time basis. The program give*~
            *            s the option of printing and deleting the store*~
            *            d records by journal and/or user.              *~
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
            * 05/01/85 ! ORIGINAL (CLONED FROM RECJURNL)          ! RCA *~
            * 04/03/86 ! ADD ACCOUNT SUMMARY PAGE                 ! HES *~
            * 01/06/89 ! Added SETPRNTs. SHOWMSG becomes SHOSTAT. ! JIM *~
            * 12/14/90 ! Changed Call to JNLINFO for GL Batches   ! RAC *~
            * 04/07/92 ! PRR 11046 - Added rec count to SETPRNT.  ! MLJ *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            account$9,                   /* ACCOUNT NUMBER             */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            credit$10,                   /* For print                  */~
            creditsstk$(1500)9,          /* CREDIT ACCOUNT RECAP STACK */~
            creditsstk(1500),            /* PARALLEL STACK FOR AMOUNTS */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            debit$10,                    /* For Print                  */~
            debitsstk$(1500)9,           /* DEBIT RECAP STACK          */~
            debitsstk(1500),             /* PARALLEL STACK FOR AMOUNTS */~
            delflag$3,                   /* Delete after Print ?       */~
            descr$36,                    /* DESCRIPTION                */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            filelist$80,                 /* LIST OF FILES              */~
            firstjnl$3,                  /* First Journal to Print     */~
            firstuserid$3,               /* First User Id to Print     */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            jnlid$3,                     /* JOURNAL ID                 */~
            lastjnl$3,                   /* Last Journal to Print      */~
            lastuserid$3,                /* Last User ID to Print      */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            location$2,                  /* LOCATION OF ACCT ON STACK  */~
            look%(1),                    /* SEARCH POINTER             */~
            modno$2,                     /* MODULE NUMBER              */~
            moduleno$2,                  /* MODULE ID                  */~
            modtitle$32,                 /* MODULE TITLE               */~
            printfile$8,                 /* PRINTFILE TO PROCESS       */~
            prtacct$16,                  /* FOR PRINT                  */~
            prtamt$(2)10,                /* FOR PRINT                  */~
            prttitle$45,                 /* DATE STRING                */~
            prttotaldescr$40,            /* SUMMARY LINE DESCRIPTION   */~
            rcpacct$(2)16,               /* ACCOUNT NUMBERS FOR RECAP  */~
            rcpacctdescr$(2)30,          /* DESCRIPTIONS FOR RECAP     */~
            rcpamt(2),                   /* RECAP AMOUNTS FOR STUFF    */~
            rcpline%(2),                 /* LINE COUNTER FOR RECAP     */~
            rcpptr%(2),                  /* POINTERS INTO RECAP STACK  */~
            readkey$50,                  /* READ KEY                   */~
            ref1$32,                     /* REFERENCE 1                */~
            ref2$32,                     /* REFERENCE 2                */~
            sjnlid$3,                    /* LAST JNLID                 */~
            smodno$2,                    /* LASTMODNO                  */~
            sortflag$(3)1,               /* PRINTFLAG FOR TOTALS       */~
            title$42                     /* JOURNAL TITLE              */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "
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
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            * #3  ! GLMAIN   ! General Ledger Main File                 *~
            * #4  ! HNYADJPF ! PRINT FILE CONTAINING GL JOURNAL DETAIL  *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            select #3,  "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =   9                      ~

            select #4,  "CHEECHEE",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  160,                                  ~
                        keypos =    1, keylen =  19                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#1,  "SHARE", f2%(1 ), rslt$(1 ), axd$(1 ))
            call "OPENFILE" (#3,  "SHARE", f2%(3 ), rslt$(3 ), axd$(3 ))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            filelist$ = "HNYADJPFJBJNLPF "

            REM Do a GETPARM to find printfile to process...
            call "GETPARM" addr("I ", "R", "PRINTFIL", " ", "0001",      ~
                                "GLPRTJL",                               ~
                                "INPUT THE PRINT FILE TO PROCESS ",      ~
                                32%, "K", "PRINTFIL", printfile$, 8%,    ~
                                5%, 32%, "A")

            search filelist$ = printfile$ to look%() step 8
            if look%(1) = 0 then L65000 /* FILE NOT DEFINED */

        REM NOW OPEN THE PRINT FILE TO PROCESS, IF NOT THERE NOTHING TO  ~
            PROCESS.

            call "PUTPRNAM" addr(#4, printfile$)
            call "OPENFILE" (#4,  "SHARE", f2%(4), rslt$(4), axd$(4))
                if f2%(4) <> 0% then L65000
            get rslt$(4) using L09410, rec%
L09410:         FMT POS(17), BI(4)
            rec% = max(100%, rec%/2)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, firstjnl$, lastjnl$,       ~
                      firstuserid$, lastuserid$, sortflag$()
            init(hex(ff)) debitsstk$(), creditsstk$()
            debitsptr%, creditsptr%, pageno%, rcppage% = 0

            for fieldnr% = 1 to  6
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10180
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10180:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

L11060:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       printmode
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  7 then L11060
            if fieldnr% = 7 then fieldnr% = 6

L11130:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11130
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11130
                  if fieldnr% = 1% or fieldnr% = 3% then L11200
            goto L11060

L11200:     fieldnr% = fieldnr% + 1
            goto L11130

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        printmode

            call "SHOSTAT" ("Printing Supplement Journal Report")
            call "SETPRNT" ("G/L007", " ", rec%, 0%)
            select printer (134)
            init (hex(00)) readkey$, smodno$, sjnlid$
            headflag% = 0%
            sortdebits, sortcredits, bigdebits, bigcredits, totaldebits, ~
                totalcredits, colsdone% = 0
            spstseq% = -99%
            if firstjnl$ <> "ALL" then L12160
            init (hex(00)) firstjnl$
            init (hex(ff)) lastjnl$
L12160:     if firstuserid$ <> "ALL" then L12200
            init (hex(00)) firstuserid$
            init (hex(ff)) lastuserid$

L12200:     call "PLOWNEXT" (#4, readkey$, 0%, f1%(4))
                if f1%(4) = 0 then summary
            gosub L30000
            if userid$ < firstuserid$ then L12200
            if userid$ > lastuserid$ then summary
            if jnlid$ < firstjnl$ then L12200
            if jnlid$ > lastjnl$ then summary
            if smodno$ = hex(0000) then smodno$ = modno$
            if modno$ <> smodno$ then total_mod
            if str(sortflag$(),2)<>" " and jnlid$<> sjnlid$ then total_jnl
            if sortflag$(3) <> " " and pstseq% <> spstseq% then total_seq
L12300:     if headflag% = 0 then gosub sub_heading
            headflag% = 1%
            gosub print_record
            goto L12200

        total_seq
            gosub L12490
            goto L12300

        total_jnl
            gosub L12490
            gosub L12620
            goto L12300

        total_mod
            gosub L12770
            goto L12300


L12490:         REM SUBTOTAL FOR SUBTOTAL2
                     if totaldebits = 0 and totalcredits = 0 then L12590
                     if sortflag$(3) = " " then L12550
                     prttotaldescr$ = "** POSTING TOTAL **"
                     gosub'60(totaldebits,totalcredits)
                     headflag% = 0
L12550:              sub1debits = sub1debits + totaldebits
                     sub1credits= sub1credits + totalcredits
                     totaldebits, totalcredits = 0
L12590:              spstseq% = pstseq%
                     return

L12620:         REM SUBTOTAL FOR SUBTOTAL1
                     if sub1debits = 0 and sub1credits = 0 then L12740
                     if str(sortflag$(),2,2) = " " then L12700
                     prttotaldescr$ ="** JOURNAL " & sjnlid$ & " TOTAL **"
                     if sortflag$(3) <> " " then                         ~
                           gosub'80(sub1debits,sub1credits)              ~
                        else gosub'60(sub1debits,sub1credits)
                     if sortflag$(2) <> " " then headflag% = 0
L12700:              sortdebits = sortdebits + sub1debits
                     sortcredits= sortcredits + sub1credits
                     sub1debits, sub1credits = 0
L12740:              sjnlid$ = jnlid$
                     return

L12770:        REM ACTUAL TOTAL ROUTINE FOR MODULE
                    gosub L12490
                    gosub L12620
                    if sortdebits = 0 and sortcredits = 0 then L13020
                    printline% = printline% + 5
                    prttotaldescr$ = "** MODULE "&smodno$&" TOTAL **"
                    if str(sortflag$(),2,2) <> " " then                  ~
                        gosub'80(sortdebits, sortcredits)                ~
                        else gosub'60(sortdebits, sortcredits)
                    bigdebits = bigdebits + sortdebits
                    bigcredits = bigcredits + sortcredits
                    totaldebits, totalcredits, sub1debits, sub1credits,  ~
                        sortdebits, sortcredits = 0
L13020:             smodno$ = modno$
                    return

        print_record                             /* PRINT DETAIL LINE */
                call "DATEFMT" (prtposted$)
                call "CONVERT" (debit, 2.2, debit$)
                call "CONVERT" (credit, 2.2, credit$)
                gosub'162(account$, debit)
                gosub'163(account$, credit)
                if debit = 0  then debit$ = " "
                if credit = 0 then credit$ = " "
                totaldebits = totaldebits + debit
                totalcredits = totalcredits + credit
                printline% = printline% + 1
                if printline% > 59% then gosub heading
                prtacct$ = account$
                call "GLFMT" (prtacct$)
                print using L55180, prtacct$, prtposted$, modno$, jnlid$, ~
                            ref1$, ref2$, descr$, debit$, credit$
                if delflag$ = "NO" then L13220
                   call "DELETE" (#4, readkey$, 19%)
L13220:         return

        deffn'60(subdebits,subcredits)

               print using L55800
               call "CONVERT" (subdebits, 2.2, prtamt$(1))
               call "CONVERT" (subcredits, 2.2, prtamt$(2))
               print using L55210, prttotaldescr$, prtamt$(1), prtamt$(2)
               print using L55060
               printline% = 987654321
               return

        deffn'80(bigprtdebits, bigprtcredits)
            if bigprtdebits = 0 and bigprtcredits = 0 then return
            gosub heading
            print using L55090
            call "CONVERT" (bigprtdebits, 2.2, prtamt$(1))
            call "CONVERT" (bigprtcredits, 2.2, prtamt$(2))
            print using L55270, prttotaldescr$, prtamt$(1), prtamt$(2)
            print using L55090
            print using L55060
            return

        summary
                REM TOTAL FOR REPORT
                     gosub L12770
            prttotaldescr$ = "** GRAND TOTALS **"
            gosub'80(bigdebits, bigcredits)
            gosub L15000
            close printer
            call "SETPRNT" ("G/L007", " ", 0%, 1%)
            goto inputmode

L15000: REM *************************************************************~
            *         P R I N T   D A I L Y   R E C A P   I N F O       *~
            *                                                           *~
            * TAKES THE CONTENTS OF THE VARIOUS STACKS AND POSTS THEM TO*~
            * THE DAILY RECAP.  *******CENSORED********  WILL NOT PRINT *~
            * ANYTHING WHERE THE AMOUNT WAS ZERO OR THE STACK WAS EMPTY *~
            *************************************************************

            if debitsptr% = 0 and creditsptr% = 0 then return

            totaldebits, totalcredits, colsdone% = 0
            mat rcpline% = con
            mat rcpptr% = zer
            rcpacct$() = " "
            gosub L17000        /* SKIP TO TOP OF PAGE.                 */

L15160:     for column% = 1 to 2
                on column% gosub L15230, L15680
                next column%
                print                    /* FREE UP LINE.              */
            if  colsdone% >= 2 then return         /* DONE W/ REPORT   */
                goto L15160

L15230:     REM HANDLES LEFT (DEBITS) COLUMN FOR REPORT
                on rcpline%(1) gosub L15270, L15500, L15540, L15580, L15620
                return

L15270:         REM PRINT CONTENTS OF DEBITS STACK, IF ANY.
                    if debitsptr% = 0 then L15540
                    rcpptr%(1) = rcpptr%(1) + 1
                    rcpamt(1)=debitsstk(rcpptr%(1))
                    rcpacct$(1)=debitsstk$(rcpptr%(1))
                    if rcpacct$(1) <> "OVER FLOW" then L15350
                       rcpacctdescr$(1) = "Rest Of Debits (can't print)"
                       goto L15420
L15350:             REM GET ACCOUNT DESCRIPTION
                        call "READ100" (#3, rcpacct$(1), f1%(3))
                        if f1%(3) = 0                                    ~
                           then rcpacctdescr$(1)="ACCOUNT NOT ON FILE"   ~
                           else get #3, using L15400, rcpacctdescr$(1)
L15400:                                 FMT XX(9), CH(30)
                    call "GLFMT" (rcpacct$(1))
L15420:             print using L17170,rcpacct$(1),rcpacctdescr$(1),      ~
                                         rcpamt(1);
                    totaldebits=totaldebits+debitsstk(rcpptr%(1))
                    totaldebits=round(totaldebits, 2)
                    if rcpptr%(1) < debitsptr% then return
                       rcpline%(1) = 2
                       return
                           FMT XX(9), CH(30)
L15500:         REM PRINTS SEPARATOR LINE.
                    print using L17160, "*--";
                    rcpline%(1) = 3
                    return
L15540:         REM PRINTS TOTAL LINE
                    print using L17180, totaldebits;
                    rcpline%(1) = 4
                    return
L15580:         REM PRINTS STARS
                    print using L17150, "*";
                    rcpline%(1) = 5
                    return
L15620:         REM SETS TO BLANKS AS COLUMN IS DONE.
                    rcpacct$(1), rcpacctdescr$(1) = " "
                    colsdone% = colsdone% + 1
                    rcpline%(1) = 6
                    return

L15680:     REM HANDLES RIGHT HAND COLUMN--CREDITS.
                print tab(70);
                on rcpline%(2) gosub L15720, L15940, L15980, L16020, L16060
                   return
L15720:         REM PRINT THE CREDITS STACK.
                    if creditsptr% = 0 then L15980
                    rcpptr%(2) = rcpptr%(2) + 1
                    rcpamt(2)=creditsstk(rcpptr%(2))
                    rcpacct$(2)=creditsstk$(rcpptr%(2))
                    if rcpacct$(2) <> "OVER FLOW" then L15800
                       rcpacctdescr$(2) = "Rest Of Credits (can't print)"
                       goto L15860
L15800:             REM PRINT ACCOUNT DESCIPTION.
                        call "READ100" (#3, rcpacct$(2), f1%(3))
                        if f1%(3)=0                                      ~
                           then rcpacctdescr$(2)="ACCOUNT NOT ON FILE"   ~
                           else get #3, using L15930, rcpacctdescr$(2)
                    call "GLFMT" (rcpacct$(2))
L15860:             print using L17170,rcpacct$(2),rcpacctdescr$(2),      ~
                               rcpamt(2);
                    totalcredits=totalcredits+creditsstk(rcpptr%(2))
                    totalcredits=round(totalcredits, 2)
                    if rcpptr%(2) < creditsptr% then return
                       rcpline%(2) = 2
                       return
L15930:             FMT XX(9), CH(30)
L15940:         REM PRINT SEPARATOR LINE
                    print using L17160, "*--";
                    rcpline%(2) = 3
                    return
L15980:         REM PRINT TOTAL CREDITS LINE
                    print using L17190, totalcredits;
                    rcpline%(2) = 4
                    return
L16020:         REM PRINT STARS
                    print using L17150,"*";
                    rcpline%(2) = 5
                    return
L16060:         REM BLANK--PASS...
                    rcpline%(2) = 6
                    colsdone% = colsdone% + 1
                    rcpacct$(2), rcpacctdescr$(2) = " "
                    return

L17000:     REM PAGE CONTROL SUBROUTINE FOR PRINTING DAILY RECAP
                   print page
                   rcppage% = rcppage% + 1
                   print using L17250, rcppage%, prttitle$
                   print using L17280, printfile$
                   print using L17300
                   print
                   print using L17340
                   print using L17380,"#","#"
                   print using L17420
                   return

L17150: %**************************#***********************************
L17160: %#--------------+--------------------------------+------------*
L17170: %* ############ ! ############################## !-#######.## *
L17180: %*              ! TOTAL DEBITS                   !-#######.## *
L17190: %*              ! TOTAL CREDITS                  !-#######.## *

        %PAGE ##### ########################################  J O U R N A~
        ~ L  R E C A P          ##########################################~
        ~###

L17250: %PAGE ########        S U P P L E M E N T   J O U R N A L   A C C~
        ~ O U N T   R E C A P   ##########################################~
        ~###
L17280: %REPORT: ########

L17300: %=========================D E B I T S==========================  ~
        ~     ===========================C R E D I T S====================~
        ~==

L17340: %**************************************************************  ~
        ~     ************************************************************~
        ~**

L17380: %* ACCOUNT #    !     D E S C R I P T I O N      !   AMOUNT   *  ~
        ~     * ACCOUNT #    !     D E S C R I P T I O N      !   AMOUNT  ~
        ~ *

L17420: %*--------------+--------------------------------+------------*  ~
        ~     *--------------+--------------------------------+-----------~
        ~-*

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* first Journal    */~
                                    L20200,         /* Last Journal     */~
                                    L20300,         /* First User ID    */~
                                    L20400,         /* Last User ID     */~
                                    L20500,         /* Delete After Prt */~
                                    L20600          /* PRINTMODE        */
                     return
L20100:     REM DEFAULT/ENABLE FOR First Journal to Print
                firstjnl$ = "ALL"
                inpmessage$ = "Enter First Journal ID to Print or Press R~
        ~eturn for 'ALL'"
                return
L20200:     REM DEFAULT/ENABLE FOR Last Journal to Print
                if firstjnl$ = "ALL" then enabled% = 0%
                inpmessage$ = "Enter Last Journal ID to Print"
                return
L20300:     REM DEFAULT/ENABLE FOR First User Id to Print
                firstuserid$ = "ALL"
                inpmessage$ = "Enter First User ID to Print or Press Retu~
        ~rn for 'ALL'"
                return
L20400:     REM DEFAULT/ENABLE FOR Last User ID to Print
                if firstuserid$ = "ALL" then enabled% = 0%
                inpmessage$ = "Enter Last User ID to Print"
                return
L20500:     REM DEFAULT/ENABLE FOR Delete after Print ?
                if delflag$ = " " then delflag$ = "YES"
                inpmessage$ = "Enter 'NO' to not delete after print or pr~
        ~ess RETURN to delete after print"
                return
L20600:     REM DEFAULT/ENABLE FOR Print Mode
                inpmessage$ = "Enter any Character beside the desired Tot~
        ~al Method"
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
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
L29918:     accept                                                       ~
               at (01,27),                                               ~
                  "***** START OVER COMMAND*****",                       ~
               at (04,02),                                               ~
                  "PRESS (1) TO RETURN TO DISPLAY",                      ~
               at (06,02),                                               ~
              "PRESS (ENTER) TO START OVER WITHOUT SAVING CURRENT ENTRY",~
                                                                         ~
               keys(hex(00010f)),                                        ~
               on hex(00010f) goto L29942, L29948, L29952
               return

L29942:        REM START OVER            (ENTER)
                   return clear
                   goto inputmode
L29948:        REM RETURN TO DISPLAY.    (P.F. KEY 1)
                   return
L29952:        REM PRINT SCREEN.         (P.F. KEY 15)
                   call "PRNTSCRN"
                   goto L29918

L30000: REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

            get #4,  using L35180, modno$, jnlid$, pstseq%, userid$,      ~
                     prtposted$, account$, ref1$, ref2$, descr$, debit,  ~
                     credit
            return

        REM *************************************************************~
            *        S T A C K   P U S H I N G   R O U T I N E S        *~
            *                                                           *~
            * PUSHES THE INDICATED INFORMATION ONTO THE DESIRED STACK   *~
            * FOR LATER PROCESSING.                                     *~
            *************************************************************

            deffn'162(account$, amount)  /* RECAP DEBITS ACCUMULATOR   */
                  if abs(amount) < .001 then return
                  search str(debitsstk$(),1) = str(account$)             ~
                               to location$ step 9 /* FIND ACCOUNT #   */
                  if location$ = hex(0000) then L31160  /* PUSH NEW ITEM*/
                     junk% = int(val(location$,2)/9)+1 /* WHICH CELL?  */
                     debitsstk(junk%) = debitsstk(junk%) + amount
                               /* UPDATE AMOUNT FOR EXISTING ACCOUNT   */
                     return
L31160:           REM PUSH NEW ITEM ONTO STACK.
                      if debitsptr% < dim(debitsstk$(),1) then L31220
                         debitsstk$(debitsptr%) = "OVER FLOW"
                         debitsstk(debitsptr%) =                         ~
                         debitsstk(debitsptr%) + amount
                         return
L31220:               debitsptr% = debitsptr% + 1
                      debitsstk$(debitsptr%) = account$
                      debitsstk(debitsptr%) = amount
                      return

            deffn'163(account$, amount)  /* RECAP CREDITS ACCUMULATOR  */
                  if abs(amount) < .001 then return
                  search str(creditsstk$(),1) = str(account$)            ~
                               to location$ step 9
                               /* SCAN ONLY WHAT IS USED OF STACK      */
                  if location$ = hex(0000) then L31370  /* IF NO ==> NEW*/
                     junk% = int(val(location$,2)/9)+1 /* WHICH ELEMENT*/
                     creditsstk(junk%) = creditsstk(junk%) + amount
                                         /* UPDATE AMT FOR THAT ELEMENT*/
                     return
L31370:           REM PUSH NEW ENTRY ONTO SALES ACCOUNT STACK.
                      if creditsptr% < dim(creditsstk$(),1) then L31430
                         creditsstk$(creditsptr%) = "OVER FLOW"
                         creditsstk(creditsptr%) =                       ~
                         creditsstk(creditsptr%) + amount
                         return
L31430:               creditsptr% = creditsptr% + 1
                      creditsstk$(creditsptr%) = account$
                      creditsstk(creditsptr%) = amount
                      return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

        FMT                 /* FILE: SYSFILE2                          */~
            CH(20),         /* Acts as variable text key to SYSFILE2 sy*/~
            CH(480)         /* Filler For Rest of Record or Internal Sp*/~

        FMT                 /* FILE: GLMAIN                            */~
            CH(9),          /* general ledger account number           */~
            CH(30),         /* general ledger account description      */~
            CH(1),          /* general ledger account type (a,l,c,r,e,$*/~
            BI(4),          /* next detail sequence number - gldetail  */~
            PD(14,4),       /* Balance forward (two years back)        */~
            13*PD(14,4),    /* same as current except one year back    */~
            PD(14,4),       /* Closing entries bucket for current year */~
            13*PD(14,4),    /* 13 period activity.1=first period in FY,*/~
            4*PD(14,4)      /* Place to put new years active if old yea*/~

L35180:     FMT                                                          ~
            CH(2),                       /* MODULE NUMBER              */~
            CH(3),                       /* JOURNAL ID                 */~
            BI(4),                       /* POSTING SEQUENCE NUMBER    */~
            CH(3),                       /* USER ID                    */~
            XX(7),                       /* DATE/TIME STAMP            */~
            CH(6),                       /* GL POSTING DATE            */~
            CH(9),                       /* GL ACCOUNT NUMBER          */~
            CH(30),                      /* REF1 TEXT                  */~
            CH(34),                      /* REF2 TEXT                  */~
            CH(36),                      /* GL TEXT                    */~
            2*PD(14,4),                  /* DEBIT, CREDIT              */~
            XX(6)                        /* SYSTEM DATE                */

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40180,         /* first Journal    */~
                                    L40180,         /* Last Journal     */~
                                    L40180,         /* First User ID    */~
                                    L40180,         /* Last User ID     */~
                                    L40180,         /* Delete After Prt */~
                                    L40180          /* PRINTMODE        */
                     goto L40250

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40180:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40250:     accept                                                       ~
               at (01,02),                                               ~
                  "General Ledger Print Journal",                        ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,75),                                               ~
                  "PAGE 1",                                              ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "First Journal to Print",                              ~
               at (06,30), fac(lfac$( 1)), firstjnl$            , ch(03),~
               at (07,02),                                               ~
                  "Last Journal to Print",                               ~
               at (07,30), fac(lfac$( 2)), lastjnl$             , ch(03),~
               at (08,02),                                               ~
                  "First User Id to Print",                              ~
               at (08,30), fac(lfac$( 3)), firstuserid$         , ch(03),~
               at (09,02),                                               ~
                  "Last User ID to Print",                               ~
               at (09,30), fac(lfac$( 4)), lastuserid$          , ch(03),~
               at (10,02),                                               ~
                  "Delete after Print ?",                                ~
               at (10,30), fac(lfac$( 5)), delflag$             , ch(03),~
               at (12,02),                                               ~
                  "Print Totals By:",                                    ~
               at (12,20), fac(lfac$( 6)), sortflag$(1)         , ch(01),~
               at (12,22), "Module",                                     ~
               at (12,30), fac(lfac$( 6)), sortflag$(2)         , ch(01),~
               at (12,32), "Journal",                                    ~
               at (12,41), fac(lfac$( 6)), sortflag$(3)         , ch(01),~
               at (12,43), "Sequence",                                   ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40690
                  call "MANUAL" ("GLPRTJNL")
                  goto L40250

L40690:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40250

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41180,         /* first Journal    */~
                                    L41180,         /* Last Journal     */~
                                    L41180,         /* First User ID    */~
                                    L41180,         /* Last User ID     */~
                                    L41180,         /* Delete After Prt */~
                                    L41180          /* Print Mode       */
                     goto L41250

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41180:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41250:     accept                                                       ~
               at (01,02),                                               ~
                  "General Ledger Print Journal",                        ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,75),                                               ~
                  "PAGE 1",                                              ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "First Journal to Print",                              ~
               at (06,30), fac(lfac$( 1)), firstjnl$            , ch(03),~
               at (07,02),                                               ~
                  "Last Journal to Print",                               ~
               at (07,30), fac(lfac$( 2)), lastjnl$             , ch(03),~
               at (08,02),                                               ~
                  "First User Id to Print",                              ~
               at (08,30), fac(lfac$( 3)), firstuserid$         , ch(03),~
               at (09,02),                                               ~
                  "Last User ID to Print",                               ~
               at (09,30), fac(lfac$( 4)), lastuserid$          , ch(03),~
               at (10,02),                                               ~
                  "Delete after Print ?",                                ~
               at (10,30), fac(lfac$( 5)), delflag$             , ch(03),~
               at (12,02),                                               ~
                  "Print Totals By:",                                    ~
               at (12,20), fac(lfac$( 6)), sortflag$(1)         , ch(01),~
               at (12,22), "Module",                                     ~
               at (12,30), fac(lfac$( 6)), sortflag$(2)         , ch(01),~
               at (12,32), "Journal",                                    ~
               at (12,41), fac(lfac$( 6)), sortflag$(3)         , ch(01),~
               at (12,43), "Sequence",                                   ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,60),                                               ~
                  "(16)PRINT JOURNALS",                                  ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41690
                  call "MANUAL" ("GLPRTJNL")
                  goto L41250

L41690:        if keyhit% <> 15 then L41730
                  call "PRNTSCRN"
                  goto L41250

L41730:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* first Journal    */~
                                    L50200,         /* Last Journal     */~
                                    L50300,         /* First User ID    */~
                                    L50400,         /* Last User ID     */~
                                    L50500,         /* Delete After Prt */~
                                    L50600          /* Print Mode       */
                     return
L50100:     REM TEST DATA FOR First Journal to Print
                return
L50200:     REM TEST DATA FOR Last Journal to Print
                if firstjnl$ = "ALL" then L50290
                if lastjnl$ >= firstjnl$ then L50290
                errormsg$ = "Last Journal must be equal or greater than F~
        ~irst Journal"
L50290:         return
L50300:     REM TEST DATA FOR First User Id to Print
                return
L50400:     REM TEST DATA FOR Last User ID to Print
                if firstuserid$ = "ALL" then L50490
                if lastuserid$ >= firstuserid$ then L50490
                errormsg$ = "Last User must be equal or greater than Firs~
        ~t User"
L50490:         return
L50500:     REM TEST DATA FOR Delete after Print ?
                if delflag$ = "YES" or delflag$ = "NO" then L50590
                errormsg$ = "Answer YES or NO to Delete after Print"
L50590:         return
L50600:     REM TEST DATA FOR Print Mode
                if sortflag$() = " " then L50690
                if sortflag$(1) = " " and sortflag$(2) = " " then L50690
                if sortflag$(1) = " " and sortflag$(3) = " " then L50690
                if sortflag$(2) = " " and sortflag$(3) = " " then L50690
                errormsg$ = "Choose only one Total method"
L50690:         return

        REM *************************************************************~
            *           P R I N T  L I S T  R O U T I N E               *~
            *                                                           *~
            * PRINTS JOURNAL ENTRY LIST FROM EITHER THE BUFFER OR MASTER*~
            *************************************************************

L55060: %================================================================~
        ~=================================================================~
        ~===
L55090: %!                                                               ~
        ~                                                                 ~
        ~  !
        %!######## ## ### ######################### #####################~
        ~######## ###################################!##########!#########~
        ~# !
L55180: %!############### ######## ## ### ######################### #####~
        ~#################### #######################!##########!#########~
        ~# !
L55210: %! ########################################                      ~
        ~                                            !##########!#########~
        ~# !
        %! ########################################                      ~
        ~                                            !##########!#########~
        ~# !
L55270: %! ########################################                      ~
        ~                                             ########## #########~
        ~# !
L55300: %! MODULE: ## ################################  JOURNAL: ### ####~
        ~###################################### POSTING SEQUENCE: ########~
        ~##!
L55330: %! MODULE: ## ################################  JOURNAL: ### ####~
        ~######################################                           ~
        ~  !
L55360: %! MODULE: ## ################################                   ~
        ~                                                                 ~
        ~  !

        sub_heading

            printline% = 100
            gosub heading
            sortflag% = pos(-sortflag$()<>(hex(20)))
            if sortflag% = 0% then sortflag% = 1%
            moduleno$ = modno$
            title$ = " "
            call "JNLINFO" (moduleno$, jnlid$, u3%, " ", title$, " ",    ~
                            #1, f2%(1), 2%)
            if title$ = " " then title$ = "Unknown"
            call "PUTPAREN" (title$)
            modtitle$ = " "
            call "JNLINFO" ("00", moduleno$, 0%, " ", modtitle$, " ",    ~
                            #1, f2%(1), 2%)
            if modtitle$ = " " then modtitle$ = "Unknown"
            call "PUTPAREN" (modtitle$)
            on sortflag% gosub L55570, L55540, L55520
            goto L55590
L55520:     print using L55300, modno$, modtitle$, jnlid$, title$, pstseq%
            return
L55540:     print using L55330, modno$, modtitle$, jnlid$, title$
            return
L55570:     print using L55360, modno$, modtitle$
            return
L55590:     print using L55090
            printline% = printline% + 7
            if printline% > 59 then gosub heading
            print using L55680
            print using L55740
            print using L55770
            print using L55800
            return

L55680: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--+
        %!  DATE  !MD!JNL!     REFERENCE 1         !       REFERENCE 2   ~
        ~        !          DESCRIPTION              !   DEBIT  !  CREDIT ~
        ~  !
L55740: %! ACCOUNT NUMBER!  DATE  !MD!JNL!     REFERENCE 1         !     ~
        ~ REFERENCE 2        !       DESCRIPTION     !   DEBIT  !  CREDIT ~
        ~  !
L55770: %+---------------------------------------------------------------~
        ~--------------------------------------------+----------+---------~
        ~--+
L55800: %!                                                               ~
        ~                                            !          !         ~
        ~  !
        %!                                                               ~
        ~                                            !----------!---------~
        ~--!
        %!                                                               ~
        ~                                             ---------- ---------~
        ~--!

        heading

        call "DATE" addr("HD", prttitle$)
        print page
        pageno% = pageno% + 1
        print using L55970,pageno%, prttitle$
L55970: %PAGE ########        S U P P L E M E N T  J O U R N A L  L I S T~
        ~ I N G                 ##########################################~
        ~###
        print using L56040, printfile$
L56040: %REPORT: ########
        print
        print using L55060
        printline% = 4%
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
