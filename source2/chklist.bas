        REM *************************************************************~
            *                                                           *~
            *   CCC   H   H  K   K  L      IIIII   SSS   TTTTT          *~
            *  C   C  H   H  K  K   L        I    S        T            *~
            *  C      HHHHH  KKK    L        I     SSS     T            *~
            *  C   C  H   H  K  K   L        I        S    T            *~
            *   CCC   H   H  K   K  LLLLL  IIIII   SSS     T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CHKLIST  - PRINT THE CONTENTS OF THE CHECK GENERATION     *~
            *            BUFFER.  NOTE THAT THIS IS DIFFERENT FROM      *~
            *            "CHKPRINT", WHICH PRINTS THE ACTUAL CHECKS     *~
            *            ON PRE-PRINTED CHECK STOCK.                    *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/30/80 ! ORIGINAL                                 ! BCW *~
            * 06/04/81 ! ADDED 16 CHAR. INVOICE NUMBER            ! TOM *~
            * 03/18/82 ! CHG TOT CREDITS, ADD APPROVAL LINES      ! GLW *~
            * 03/29/82 ! ADDED NET CHECK TOTAL                    ! BEV *~
            * 12/13/85 ! Changed vendor file format               ! MJB *~
            * 04/10/86 ! ADDED SCREEN TO SELECT USER ID OR ALL    ! WPH *~
            * 10/02/86 ! Corrected check counter                  ! MJB *~
            * 04/13/87 ! Corrected zeroing of TOTALCREDITS        ! MJB *~
            *************************************************************

        dim                                                              ~
            acct$16,                     /* Account number information */~
            cashacct$16,                 /* Cash in bank account #     */~
            checkdate$6,                 /* Date of this check         */~
            checknr$8,                   /* Check number info          */~
            cursor%(2),                  /* Cursor location for edit   */~
            company$60,                  /* Company / division name    */~
            discacct$16,                 /* Discount account number    */~
            edtmessage$79,               /* Edit message               */~
            errormsg$79,                 /* Failed validation message  */~
            hdrdate$45,                  /* Formatted date/time info   */~
            inpmessage$79,               /* Input message              */~
            invdate$8,                   /* Invoice Date               */~
            invoicenr$16,                /* Invoice number for details */~
            i$(24)80,                    /* Screen position            */~
            linekey$50,                  /* Key to next line item      */~
            line2$79,                    /* Screen header underlined   */~
            linenumber%(2),              /* Line pointer for report    */~
            lfac$(20)1,                  /* Field attribute            */~
            pagenumber$4,                /* Printed page number        */~
            plowkey$50,                  /* Next check this user key   */~
            prtacct$16,                  /* Account number to print    */~
            prtacctdescr$30,             /* Account description to prt */~
            prtchecknr$8,                /* Check number to print      */~
            prtcredit$10,                /* Credit amount to print     */~
            prtdebit$10,                 /* Debit amount to print      */~
            prtdiscount$10,              /* Discount amount to print   */~
            prtinvoicenr$16,             /* Invoice nunber to print    */~
            prtvencode$9,                /* Vendor code to print       */~
            prtvenname$30,               /* Vendor name to print       */~
            user$3,                      /* Userid for display         */~
            ouser$3,                     /* Old user ID when plowing   */~
            nuser$3,                     /* New user ID when plowing   */~
            userdescr$30,                /* User  description          */~
            userid$3,                    /* Userid of current user     */~
            vencode$9                    /* Vendor code information    */~

        dim f2%(64),                     /* File status flags for      */~
            f1%(64),                     /* Record-on-file flags       */~
            rslt$(64)20                  /* Used with OPENCHCK         */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.05 03/04/88 Patch Release                   "
        REM *************************************************************
            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* It is an intrinsic part of the                 */
                     /* File open subroutine.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 2 ! GLMAIN   ! General ledger main file.                *~
            * # 3 ! VENDOR   ! Vendor master record file                *~
            * # 9 ! CHKBUFFR ! Cash disbursements buffer area           *~
            * #10 ! CHKBUF2  ! Cash disbursements check detail buffer   *~
            *************************************************************

            select  #2, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select #3,  "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos=1, keylen=9,                              ~
                        alt key 1, keypos = 10, keylen = 30, dup

            select  #9, "CHKBUFFR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 07,                         ~
                        alternate key 1, keypos =  08, keylen = 07,      ~
                                  key 2, keypos =  24, keylen =  8, dup

            select #10, "CHKBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 20,                         ~
                        alt key 1, keypos = 21, keylen = 16, dup

              call "SHOSTAT" ("Opening Files, One Moment Please")

              rslt$(2)  = "REQUIRED"
              rslt$(3)  = "REQUIRED"
              rslt$(9)  = "REQUIRED"
              rslt$(10) = "REQUIRED"

              call "OPENCHCK" (#2, fs%, f2%( 2),   0%, rslt$( 2))
              call "OPENCHCK" (#3, fs%, f2%( 3),   0%, rslt$( 3))
              call "OPENCHCK" (#9, fs%, f2%( 9),   0%, rslt$( 9))
              call "OPENCHCK" (#10,fs%, f2%(10),   0%, rslt$(10))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            *************************************************************
            ret% = ret%
            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT"  addr ("ID", userid$)
            call "COMPNAME" (12%, company$, ret%)

        REM *************************************************************~
            *                   I N P U T   U S E R  I D                *~
            *                                                           *~
            *************************************************************

        inputmode
            errormsg$,  userdescr$ = " "
            all% = 0%
            for fieldnr% = 1 to 1
                gosub'051(fieldnr%)
L10140:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <> 8 then L10260
                        all% = 1%
                        goto  datasave
L10260:               if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then L10140
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10140
                next fieldnr%

L11000: REM *************************************************************~
            *               E D I T   M O D E   S T U F F               *~
            *                                                           *~
            * Standard linear edit mode stuff for the user id selected  *~
            *************************************************************
        editmode
           ouser$, nuser$ = " "
           edtmessage$ = "To modify displayed values, position cursor to ~
        ~line and press RETURN"
            all%, nrchecks% = 0%
L11180:     gosub'201(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 14 then       datasave
                  if keyhit%  = 16 then       L65000
                  if keyhit% <>  0 then L11000
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 1 then L11000
L11360:     gosub'201(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11360
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11360
            goto L11180

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for the page 1 of input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20120          /* USER ID   */
                     return
L20120:     REM Default/enable for user id
                enabled% = 1
                inpmessage$ = "Enter the USER ID of the person who entere~
        ~d checks or press PF8 to print ALL"
                user$ =  userid$
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the user the ability to start over when he wants to *~
            * else return to the menu.  Notice that he has to push 2    *~
            * different buttons to start over--a little safer.          *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
L29950:     keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1%  = 1% then return
            if keyhit1% <> 0% then L29950
               return clear
               goto inputmode

        datasave
        REM *************************************************************~
            *               G E T   R E A D Y  T O  G O                 *~
            *-----------------------------------------------------------*~
            * Initializes the key for plowing the next checks in the    *~
            * buffer, and other things.                                 *~
            *************************************************************

            if all% <> 1% then L30240
              plowkey$ = all(hex(00))
              ouser$ =   "  "
              goto L30260

L30240:     plowkey$ = user$
L30260:     l% = 0
            pagenumber% = 0
            pageline% = 1000
            totalcredits, totaldebits, totaldiscount, totalnetcheck = 0
            if all% <> 0% then L30340
            ouser$ =   user$
L30340:     call "SETPRNT" ("A/P009", " ", 0%, 0%)
            call "SHOSTAT" ("Printing Contents of Check Holding File")

L30400: REM *************************************************************~
            *           P L O W   F O R   N E X T   C H E C K           *~
            *-----------------------------------------------------------*~
            * Plows through file, looking for next check.  Adds totals  *~
            * to number of checks printed and total amount of this      *~
            * user's checks, and exits if we are done.                  *~
            *************************************************************
            b% = 3%
            if all% = 1% then b% = 0%

            call "PLOWNEXT" (#9, plowkey$, b%, f1%(9))
                 if f1%(9) <> 0 then L30700
                 if nrchecks% > 0 then gosub L30920      /* TOTALS */
                 close printer
                 goto editmode

L30700:     gosub L34480                  /* Load check header info     */
              if l% = 0  then L30800
              if nuser$ = ouser$ then L30820
              gosub   L30920      /* TOTALS */

            if all% = 1% then L30782
                                      /* We only had to do one, we did */
            close printer             /* it, so lets get out           */
            goto inputmode

L30782:       pageline% = 1000
              nrchecks% = 0
              totalnetcheck, totaldebits, tc, totaldiscount,             ~
                                              totalcredits = 0
L30800:    ouser$ = nuser$
L30820:    l% = l% + 1
           nrchecks% = nrchecks% + 1
            gosub L31700                  /* Print information          */
            goto L30400

L30920: REM *************************************************************~
            *   H A N D L E   T O T A L S ,   I F   N E C E S S A R Y   *~
            *-----------------------------------------------------------*~
            * Prints the totals if necessary.  If the number of checks  *~
            * printed was zero, then skip it.  Also, don't print totals *~
            * on a separate page (looks tacky).                         *~
            *************************************************************

            if nrchecks% <> 0 then goto L31100
               if  l% = 0  then bailout
               close printer
               goto editmode

L31100:        totaldebits = round(totaldebits, 2%)
               totalcredits = round(totalcredits, 2%)
               totaldiscount = round(totaldiscount, 2%)
               tc = round((totalcredits + totaldiscount), 2%)
        print using L31540, nrchecks%, ouser$,totalnetcheck,totaldebits,  ~
                                                tc,totaldiscount
               print using L34240
               print
               print using L31500
               print
               print using L31480

           return

L31480: % CHECK HOLDING FILE PROCESSED BY ________________ ON ___/___/___
L31500: % APPROVED FOR PAYMENT BY _____________ ON ___/___/___ SEE NOTES ~
        ~ABOVE FOR CHANGES TO BE MADE BEFORE PROCESSING CHECK HOLDING FILE
L31540: %!*TOTALS*! **** TOTAL OF #### CHECKS IN HOLDING FILE FOR ### ***~
        ~  TOTAL NET CHECKS #######.## *** !#######.##!#######.##!#######.~
        ~##!

        %+--------+------------------------------------------------------~
        ~----------------------------------+----------+----------+--------~
        ~--+

L31700: REM *************************************************************~
            *    P R I N T   A N   E N T R Y   F O R   A   C H E C K    *~
            *-----------------------------------------------------------*~
            * Prints an entry for a check.  Uses the standard 2-column  *~
            * technique.  Note that we have to kluge the report format  *~
            * a little by dropping 2 positions off the account          *~
            * description to make all the information I need fit on the *~
            * page.  If you need to modify this report, you probably    *~
            * want to drop the account description or vendor name first.*~
            *      Note use of variable "TAGPRINTED%", which tells the  *~
            * page control routine whether or not to put a tag line at  *~
            * the bottom of the page--i.e., we've run out at the end of *~
            * a check and printed one anyway.                           *~
            *************************************************************

            colsdone% = 0
            mat linenumber% = con

            REM SET KEY FOR LINE ITEM PLOW ROUTINE HERE.
                linekey$ = vencode$
                str(linekey$, 10) = checknr$

L32140:     for column% = 1 to 2
                on column% gosub L32500, L32800
                next column%
            if colsdone% < 2 then L32340
               if pageline% >= 66 then return
                  if tagprinted% = 1 then return
                     gosub L33780
                     print using L34240
                     tagprinted% = 1
                     return
L32340:     gosub L33780                  /* PAGE CONTROL ROUTINE       */
            print using L34400,                                           ~
                               prtchecknr$, prtvencode$, prtvenname$,    ~
                               prtinvoicenr$, invdate$, prtacct$,        ~
                               prtacctdescr$, prtdebit$, prtcredit$,     ~
                               prtdiscount$
            tagprinted% = 0
            goto L32140

L32500:     REM FIRST  COLUMN--HANDLE VENDOR CODE, NAME, CHECK #, ETC.
                on linenumber%(1) gosub L32560, L32680
                   return
L32560:         REM FIRST  CASE--PRINT VENDOR CODE, ETC.
                    prtchecknr$ = checknr$
                    prtvencode$ = vencode$
                    call "DESCRIBE"(#3,prtvencode$,prtvenname$,0%,f1%(3))
                    linenumber%(1) = 2
                    return
L32680:         REM SECOND CASE--ZAP VARIABLES, END THIS COLUMN.
                    prtchecknr$, prtvencode$, prtvenname$ = " "
                    colsdone% = colsdone% + 1
                    linenumber%(1) = 3
                    return

L32800:     REM SECOND COLUMN--HANDLES INFINITE NUMBER OF CHECK LINE ITEMS
                on linenumber%(2) gosub L32860, L33160, L33400, L33640
                   return
L32860:         REM FIRST  CASE--PRINT LINE ITEM DETAILS OF CHECK.
                    call "PLOWNEXT" (#10, linekey$, 17%, f1%(10))
                         if f1%(10) = 0 then L33160
                    gosub L34960                    /* GET LINE ITEM    */
                    prtinvoicenr$ = invoicenr$
                    prtacct$ = acct$
                    call "DESCRIBE" (#2, acct$, prtacctdescr$, 0%, f1%(2))
                    call "GLFMT" (prtacct$)
                    prtcredit$ = " "
                    call "CONVERT" (amount, 2.2, prtdebit$)
                    totaldebits = totaldebits + amount
                    if disc = 0                                          ~
                       then prtdiscount$ = " "                           ~
                       else call "CONVERT" (disc, 2.2, prtdiscount$)
                    return
L33160:         REM SECOND CASE--PRINT CASH IN BANK ACCOUNT INFORMATION.
                    prtacct$ = cashacct$
                    prtinvoicenr$ = "NET CHECK"
                    call "DESCRIBE"(#2,prtacct$,prtacctdescr$,0%,f1%(2))
                    call "GLFMT" (prtacct$)
                    prtdebit$ = " "
                    call "CONVERT" (netcheck, 2.2, prtcredit$)
                    totalcredits = totalcredits + netcheck
                    totalnetcheck = totalnetcheck + netcheck
                    prtdiscount$ = " "
                    linenumber%(2) = 3
                    return
L33400:         REM THIRD  CASE--PRINT DISCOUNT DUE INFORMATION.
                    if discount = 0 then L33640
                       prtacct$ = discacct$
                       prtinvoicenr$ = " "
                       call"DESCRIBE"(#2,prtacct$,prtacctdescr$,0%,f1%(2))
                       call "GLFMT" (prtacct$)
                       prtdebit$ = " "
                       call "CONVERT" (discount, 2.2, prtcredit$)
                       totaldiscount = totaldiscount + discount
                       prtdiscount$ = " "
                       linenumber%(2) = 4
                       return
L33640:         REM FOURTH CASE--ZAP VARIABLES AND STUFF.
                    init(" ") prtinvoicenr$, prtacct$, prtacctdescr$,    ~
                              prtdebit$, prtcredit$, prtdiscount$
                    colsdone% = colsdone% + 1
                    linenumber%(2) = 5
                    return

L33780:     REM PAGE CONTROL SUBROUTINE.
                pageline% = pageline% + 1
                if pageline% < 66 then return
                   select printer(134)
                   if pagenumber% = 0 then L33940
                      if tagprinted% = 1 then L33940
                         print using L34240
                         tagprinted% = 1
L33940:            print page
                   pagenumber% = pagenumber% + 1
                   pagenumber  = pagenumber%
                   call "CONVERT" (pagenumber, -0.001, pagenumber$)
                   call "DATE" addr("HD", hdrdate$)
                   print using L34140, date$, company$
                   print using L34160, ouser$, pagenumber$
                   print using L34240
                   print using L34320 , "#", "#"
                   print using L34240
                   pageline% = 9
                   return
L34140: %RUN DATE: ########                  ############################~
        ~################################                      CHKLIST:A/P~
        ~009
L34160: %                                    LISTING OF CHECKS IN THE A/P~
        ~ HOLDING FILE FOR USER: ###                              PAGE: ##~
        ~##

L34240: %--------+---------+----------------------------+-------------+--~
        ~------+------------+---------------+----------+----------+-------~
        ~---

L34320: %CONTROL#! VENDOR  !   V E N D O R   N A M E    ! INVOICE #   !IN~
        ~V DATE! GL ACCOUNT !TRUNC. GL DESC.!DEBIT AMT.!CREDIT AMT! DISCOU~
        ~NT

L34400: %########!#########!############################!#############!##~
        ~######!############!###############!##########!##########!#######~
        ~###

L34480: REM *************************************************************~
            *  G E T   C H E C K   H E A D E R   I N F O R M A T I O N  *~
            *-----------------------------------------------------------*~
            * Get check header information from the file.  Even though  *~
            * it's a simple format, we have this read routine so that   *~
            * it can be found easily.                                   *~
            *************************************************************

            get #9, using L34760,                                         ~
                        nuser$,  vencode$, checknr$, checkdate$,         ~
                                 discount, discacct$, cashacct$,         ~
                                 netcheck
            return

L34760:     FMT CH( 3),                  /* User                       */~
                XX(11),                  /* Skip fwd & reverse keys    */~
                CH(9),                   /* Vendor code information    */~
                CH(8),                   /* Check number               */~
                CH(6),                   /* Check date                 */~
                PD(14,4),                /* Discount amount            */~
                CH(9),                   /* Discount account number    */~
                CH(9),                   /* Cash in bank account info  */~
                PD(14,4)                 /* Net check amount           */~

L34960: REM *************************************************************~
            *        G E T   D E T A I L   I N F O R M A T I O N        *~
            *-----------------------------------------------------------*~
            * Loads the detail that we have found from the buffer.      *~
            * Again, this is here out of a long-standing commitment to  *~
            * tradition.                                                *~
            *************************************************************

            get #10, using L35200, invoicenr$, acct$, amount, disc,       ~
                                  invdate$

            call "DATEFMT" (invdate$)
            return

L35200:     FMT XX(9),                   /* Vendor code                */~
                XX(8),                   /* Check number               */~
                XX(3),                   /* Sequence number            */~
                CH(16),                  /* Invoice number             */~
                CH(9),                   /* Debit account number       */~
                XX(1),                   /* Debit account type         */~
                PD(14,4),                /* Debit amount               */~
                PD(14,4),                /* Discount amount            */~
                CH(6)                    /* Invoice Date               */

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *-----------------------------------------------------------*~
            * Inputs document for first time.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                 str(line2$,62%) = " CHKLIST: " & str(cms2v$,,8%)
                 init(hex(84)) lfac$()
                  on fieldnr% gosub L40180          /* USERID           */
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
                  "PRINT LISTING OF CHECKS IN THE HOLDING FILE",         ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "For USER I.D.:",                                      ~
               at (06,17), fac(lfac$( 1)), user$                , ch(03),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,02),                                               ~
                  "(8)List Checks for ALL users",                        ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(0001080d0f10)),                                  ~
               key (keyhit%)

            if keyhit% <> 13 then L40610
                call "MANUAL" ("CHKLIST")
                goto L40250

L40610:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40250

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *-----------------------------------------------------------*~
            * Screen for editing page 1 of document.                    *~
            *************************************************************

            deffn'201(fieldnr%)
                  init(hex(84)) lfac$() : u3% = u3%
                  on fieldnr% gosub L40800          /* USER ID          */~

                     goto L40865

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40800:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40865:     accept                                                       ~
               at (01,02),                                               ~
                  "PRINT LISTING OF CHECKS IN THE HOLDING FILE",         ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "For USER I.D.:",                                      ~
               at (06,17), fac(lfac$( 1)), user$                , ch(03),~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,40),                                               ~
                  "(14)Print Report",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
               keys(hex(00010d0e0f10)),                                  ~
               key (keyhit%)

            if keyhit% <> 13 then L41190
                call "MANUAL" ("CHKLIST")
                goto L40865

L41190:        if keyhit% <> 15 then L41230
                  call "PRNTSCRN"
                  goto L40865

L41230:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50210          /* USER TO PRINT    */~

                  return

L50210:     REM TEST FOR USERID
             /* Confirm valid user                                 */
             /* If blank, show all users with checks in the buffer */
               str(userdescr$, 1, 1) = hex(06)
               str(userdescr$, 2,29) = "USERS W/ CHECKS IN HOLD FILE "
               call "PLOWCODE" (#9, user$, userdescr$,-3%, -.0001, f1%(9))
                      if  f1%(9) <> 0 then return
                      if user$ <> " " then L50280
                     errormsg$ = "No checks for any user in holding file"
                      return
L50280:              errormsg$ = "No checks in file for user: " & user$
                     return

        bailout
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Closes all the files currently open, and also displays    *~
            * a message (chk holding file empt) while linking back to   *~
            * the next program.                                         *~
            *************************************************************

            call "SHOSTAT" ("The check holding file is empty")
            goto L65090

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING BACK TO   *~
            * THE NEXT PROGRAM.                                         *~
            *************************************************************

            call "SHOSTAT" ("Closing Files, One Moment Please")
L65090:     call "SETPRNT" ("A/P009", " ", 0%, 1%)
            end
