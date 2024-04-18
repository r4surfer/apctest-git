        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   H   H  K   K  PPPP   RRRR   IIIII  N   N  TTTTT   *~
            *  C   C  H   H  K  K   P   P  R   R    I    NN  N    T     *~
            *  C      HHHHH  KKK    PPPP   RRRR     I    N N N    T     *~
            *  C   C  H   H  K  K   P      R   R    I    N  NN    T     *~
            *   CCC   H   H  K   K  P      R   R  IIIII  N   N    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CHKPRINT - Clone of original which prints checks just fine*~
            *            But it is now responsive to whether the print  *~
            *            mode is spooled (hold, requeue) or on-line.    *~
            *            It has a separate screen for each, handling the*~
            *            masks differently for each one.                *~
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
            * 01/15/86 ! ORIGINAL                                 ! KEN *~
            * 05/15/86 ! INVOICE FILE FORMAT CHANGES              ! HES *~
            * 11/11/86 ! Modified to print CMS standard form      ! MJB *~
            * 05/19/87 ! PAYLINES- record length for Std cost     ! JIM *~
            * 08/30/89 ! Added Call to Status/Reset Subroutine    ! KAB *~
            *          ! to provide Re-Run capability for Power   !     *~
            *          ! Failure, Paper Jams, Wrong Forms, ETC.   !     *~
            * 03/31/93 ! PRR 12841 EXIT_PROGRAM closes printer.   ! JIM *~
            * 11/01/93 ! PRR 13053,12972.  Formatted ZIP code.    ! JDH *~
            *          ! PRR 11193.  Corrected elements on F1%s.  !     *~
            * 09/25/95 ! PRR 13449.  Added Printer Mode 'P'.      ! JBK *~
            *          !   Applies to UNIX platforms only.        !     *~
            * 11/21/95 ! Changed closing of printer so that one   ! JDH *~
            *          !  can get checks (or view them) without   !     *~
            *          !  having to exit program.                 !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            answer$1,                    /* AOK ON PRINTING?           */~
            bufferkey$11,                /* NEXT CHECK TO READ FROM BUF*/~
            cashacct$9,                  /* CASH IN BANK ACCT THIS CHK */~
            checkdate$8,                 /* CHECK DATE INFORMATION     */~
            checknr$8,                   /* CHECK NUMBER               */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            deletor$50,                  /* A DELETE KEY               */~
            discacct$9,                  /* DISCOUNT TAKEN ACCOUNT     */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            firstcheck$7,                /* FIRST CHECK NUMBER         */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            invoicenr$16,                /* INVOICE NUMBER THIS LINE   */~
            key1$11,                     /* KEY TO BUFFER              */~
            key2$11,                     /* SECOND KEY TO BUFFER       */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lastcheck$7,                 /* LAST CHECK TO PRINT        */~
            line2$79,                    /* Screen Line 2              */~
            masks$2,                     /* NUMBER OF MASKS TO PRINT   */~
            maskmsg$36,                  /* CURRENT PRINT MODE MESSAGE */~
            nextcheck$8,                 /* NEXT AVAILABLE CHECK NO.   */~
            oldchecknr$8,                /* OLD CHECK NUMBER           */~
            payacct$9,                   /* PAYABLES ACCOUNT NUMBER    */~
            payaccttype$1,               /* PAYABLES ACCOUNT TYPE      */~
            printmode$1,                 /* DEFAULT PRINT MODE         */~
            prtaddress$(5)31,            /* ADDRESS OF THIS GUY        */~
            prtamount$20,                /* AMOUNT THIS CHECK          */~
            prtdisc$10,                  /* DISCOUNT AMOUNTS EACH LINE */~
            prtgross$10,                 /* GROSS DETAIL AMOUNTS EA LIN*/~
            prtinvdate$8,                /* INVOICE DATE EACH LINE     */~
            prtinvoice$16,               /* INVOICE NUMBERS EACH LINE  */~
            prtnet$10,                   /* NET AMOUNT PAID EACH LINE  */~
            prttext$100,                 /* X DOLLARS AND Y CENTS      */~
            prtvencode$9,                /* VENDOR CODE TO PRINT       */~
            prtvenname$30,               /* VENDOR NAME TO PRINT       */~
            readkey$50,                  /* KEY TO FIND DATA WITH      */~
            readkey2$50,                 /* KEY TO FIND DATA WITH      */~
            record$(10)20,               /* MORE THAN A RECORD         */~
            ttlgross$10,                 /* Total Gross Amount         */~
            ttldisc$10,                  /* Total Disc Amount          */~
            ttlnet$10,                   /* Total Net Amount           */~
            userid$3,                    /* USERID CURRENT USER        */~
            vencode$9,                   /* VENDOR CODE THIS CHECK     */~
            voidkey$7,                   /* BUFFER KEY FOR VOIDING     */~
            voidrkey$7                   /* BUFFER KEY FOR VOIDING     */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 08/12/96 Last Wang Release               "
        REM *************************************************************
            mat f2% = con :

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! USERINFO ! Users Default Information File           *~
            * #3  ! VENDOR   ! Vendor Master File                       *~
            * #5  ! PAYMASTR ! Payables main file                       *~
            * #6  ! PAYLINES ! PAYABLES LINE ITEM FILE                  *~
            * #9  ! CHKBUFFR ! Cash disbur. check gen. buffer           *~
            * #10 ! CHKBUF2  ! Cash disbur. chk gen  detail bfr         *~
            * #11 ! SYSFILE2 ! Caelus Management System Information     *~
            * #15 ! CSHMASTR ! Cash disbur. check header file           *~
            * #16 ! CSHBUFFR ! Cash disbursements buffer file           *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =   3

            select #3,  "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  600,                                  ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup

            select #5,  "PAYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  350,                                  ~
                        keypos =    1, keylen =  25

            select #6,  "PAYLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  541,                                  ~
                        keypos = 36, keylen = 28,                        ~
                        alt key  1, keypos = 1, keylen = 63,             ~
                            key 2, keypos = 17, keylen = 47

            select #9,  "CHKBUFFR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos =    1, keylen =   7,                     ~
                        alt key  1, keypos =    8, keylen =   7,         ~
                            key  2, keypos =   24, keylen =   8

            select #10, "CHKBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos =    1, keylen =  20,                     ~
                        alt key  1, keypos =   21, keylen =  16, dup

            select #11, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20

            select #15, "CSHMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos =    1, keylen =  17,                     ~
                        alt key  1, keypos =   41, keylen =   9, dup,    ~
                            key  2, keypos =   50, keylen =   6, dup,    ~
                            key  3, keypos =   10, keylen =   8, dup

            select #16, "CSHBUFFR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos =    1, keylen =   7,                     ~
                        alt key  1, keypos =    8, keylen =   7,         ~
                            key  2, keypos =   15, keylen =  17,         ~
                            key  3, keypos =   24, keylen =   8, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#1,  "SHARE", f2%(1 ), rslt$(1 ), axd$(1 ))
            call "OPENFILE" (#3,  "SHARE", f2%(3 ), rslt$(3 ), axd$(3 ))
            call "OPENFILE" (#5,  "SHARE", f2%(5 ), rslt$(5 ), axd$(5 ))
            call "OPENFILE" (#6,  "SHARE", f2%(6 ), rslt$(6 ), axd$(6 ))
            call "OPENFILE" (#9,  "SHARE", f2%(9 ), rslt$(9 ), axd$(9 ))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#11, "SHARE", f2%(11), rslt$(11), axd$(11))
            call "OPENFILE" (#15, "SHARE", f2%(15), rslt$(15), axd$(15))
            call "OPENFILE" (#16, "SHARE", f2%(16), rslt$(16), axd$(16))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

            call "EXTRACT" addr("ID", userid$)
            bufferkey$ = userid$

            call "READ100" (#1, userid$, f1%(1))
               if f1%(1) = 0% then L65000
            str(line2$,62,18) = "CHKPRINT: " & str(cms2v$,1,8)

        check_status
            call "CHKSTAT" (#9, #10, #16, #15, update%)
               if update% >= 0% then exit_program

*       INPUTMODE

            init (" ") nextcheck$, firstcheck$, lastcheck$
            update% = 0% : bufferkey$ = userid$

*          SELECT PRINTER (134)
*          CALL "SETPRNT" ("A/P006", " ", 0%, 0%)

            call "EXTRACT" addr("PM", printmode$)

            on pos("SOHKP" = printmode$) goto L09630, L09650, L09680, L09700,    ~
                                              L09662
               goto L65000

L09630:     maskmsg$ = "Current Print Mode is 'SPOOLED'    "
                goto L12000
L09650:     maskmsg$ = "Current Print Mode is 'ON-LINE'    "
                goto L12000
L09662:     maskmsg$ = "Current Print Mode is 'PRINT-KEEP' "
                goto L12000

L09680:     maskmsg$ = "Current Print Mode is 'HOLD'    "
                goto L10000
L09700:     maskmsg$ = "Current Print Mode is 'KEEP'    "
                goto L10000

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * SECTION FOR HOLD OR KEEP                                  *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

            init(" ") errormsg$, inpmessage$

            for fieldnr% = 1 to  2
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10160
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10120
L10160:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

L11070:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       L11500
                  if keyhit% <>  0 then       L11070
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  3 then L11070
               if fieldnr% = 3 then fieldnr% =2

L11150:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11150
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11150
            goto L11070

L11500: REM HANDLE MASK REQUESTS

            if masks% = 0% then print_checks

            for maski% = 1% to masks%
                gosub print_mask
            next maski%

            goto print_checks

L12000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * SECTION FOR SPOOLED OR ON-LINE                            *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

            init(" ") errormsg$, inpmessage$

            for fieldnr% = 1 to  2
                gosub'052(fieldnr%)
                      if enabled% = 0 then L12270
L12220:         gosub'102(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L12220
                gosub'152(fieldnr%)
                      if errormsg$ <> " " then L12220
L12270:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

L13230:     gosub'112(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       print_checks
                  if keyhit% <>  0 then       L13230
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  3 then L13230
               if fieldnr% = 3 then fieldnr% =2

L13310:     gosub'112(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L13310
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L13310
            goto L13230

        REM *************************************************************~
            * MASK PRINTING CONTROL SECTION                             *~
            *************************************************************

        print_mask

            select printer (134)
            call "SETPRNT" ("A/P006", "MASK", 0%, 0%)

        REM Initialize variables to XX's
                gosub L32000

        REM Print mask

            print using L19190, prtvencode$, prtvenname$, checkdate$,     ~
                                            checknr$
            print skip(2)
            for i = 1 to 5
                print using L19220, prtinvoice$, prtinvdate$, prtgross$,  ~
                                   prtdisc$, prtnet$
            next i
            print skip(6)
            print using L19280
            gosub L17100

        REM Send mask to printer
            close printer
            return

        print_checks

            checknumber% = firstcheck%

            call "SHOSTAT" ("Printing Cash Disbursements Checks")

            select printer (134)
            call "SETPRNT" ("A/P006", " ", 0%, 0%)

L15090: REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            *-----------------------------------------------------------*~
            * This part of the routine is responsible for reading down  *~
            * the list of checks in the "CHK" buffer and printing them. *~
            *************************************************************

        REM Get next check in buffer and go.
                ttlgross, ttldisc, ttlnet = 0
                if checknumber%>lastcheck% then L65000
L15250:         call "PLOWNEXT" (#9, bufferkey$, 3%, f1%(9))
                      if f1%(9) = 0 then L65000
                gosub L30000              /* GET HEADER INFORMATION     */
                      if netcheck <= 0 then L15250
                if str(oldchecknr$,1,1) = "X" then L15310
                   update% = 1%
                   goto L15250

L15310: REM Print-format header information, including amount...
                convert checknumber% to checknr$, pic(00000000)
                put prtamount$, using L15340, netcheck + .005
L15340:             FMT PIC(**********.**)
                prttext$ = " "
                t% = int(netcheck)
                convert t% to prttext$, pic(##########)
                prttext$ = prttext$ & " DOLLARS AND "
                c = (netcheck - t%) * 100 + .5
                convert c to temp$, pic(##)
                tran (temp$, "0 ")replacing
                prttext$ = prttext$ & " "  & str(temp$, 1, 2)            ~
                               & " CENTS ONLY"
                call "DATEFMT" (checkdate$)
                prtvencode$ = vencode$
                call "READ100" (#3, vencode$, f1%(3))
                     if f1%(3) <> 0 then get #3, using L15500,            ~
                                    prtvenname$, prtaddress$()
L15500:              FMT XX(39), CH(30), 5*CH(30)

        REM Format address...
            if str(prtaddress$(5%),17%,1%) <> " " or                     ~
               str(prtaddress$(5%),16%,1%) <> " " or                     ~
               pos(str(prtaddress$(5%),27%,4%) = " ") > 0% then L15509
                temp$ = str(prtaddress$(5%),27%,4%)
                str(prtaddress$(5%),28%,4%) = temp$
                str(prtaddress$(5%),27%,1%) = "-"
L15509:     call "LINSMASH" (prtaddress$())

        REM Prepare to plow through check for this vendor.
                pagenumber% = 0
                readkey$ = vencode$
                str(readkey$, 10) = oldchecknr$
                pageline% = 1000

L15580: REM Plow through and print line items for this check.
                call "PLOWNEXT" (#10, readkey$, 17%, f1%(10))
                     if f1%(10) = 0 then L15700
                gosub L31000                  /* Get data from disk     */
                gosub L16500                  /* Print-format the data  */
                if pageline% > 13 then gosub L18000     /* Page control */
                print using L19220, prtinvoice$, prtinvdate$, prtgross$,  ~
                                   prtdisc$, prtnet$
                pageline% = pageline% + 1
                goto L15580

L15700: REM Now finish the check stub and print the check!
*              IF PAGELINE% = 1000 THEN PAGELINE% = 1%
                call "CONVERT" (ttlgross, 2.2, ttlgross$)
                call "CONVERT" (ttldisc,  2.2, ttldisc$)
                ttlnet = ttlgross - ttldisc
                call "CONVERT" (ttlnet, 2.2, ttlnet$)
                print skip(14% - pageline%)
                print using L19250, ttlgross$, ttldisc$, ttlnet$
                gosub L17100                   /*  Print the Check  */

        REM Write check to buffer
                gosub L33000
                gosub increment_check_number
            goto L15090                        /*  Next Check  */

        REM *************************************************************~
            * INCREMENT CHECK NUMBER                                    *~
            *************************************************************

        increment_check_number

            checknumber% = checknumber% + 1%
            update% = 1%
            if checknumber% > lastcheck% then L16250
            if checknumber% > 9999999% then checknumber% = 1%

            convert checknumber% to nextcheck$, pic(00000000)

                call "REDALT0" (#9,  nextcheck$, 2%, f1%(9%))
                     if f1%(9%) <> 0% then L16250
                call "REDALT0" (#15, nextcheck$, 3%, f1%(15))
                     if f1%(15) <> 0% then L16250
                call "REDALT0" (#16, nextcheck$, 3%, f1%(16))
                     if f1%(16) <> 0% then L16250

                str(nextcheck$,1,1) = "M"

                call "REDALT0" (#15, nextcheck$, 3%, f1%(15))
                     if f1%(15) <> 0% then L16250
                call "REDALT0" (#16, nextcheck$, 3%, f1%(16))
                     if f1%(16)  = 0% then return

L16250:         return clear all
                goto L65000

L16500: REM *************************************************************~
            *  F O R M A T   L I N E   I T E M   I N F O R M A T I O N  *~
            *-----------------------------------------------------------*~
            * Formats all the line item information, such as discount,  *~
            * net, and gross amounts, and formats the invoice date.     *~
            *************************************************************

            prtinvoice$ = invoicenr$
            call "DATEFMT"  (prtinvdate$)
            call "CONVERT" (amount, 2.2, prtgross$)
            call "CONVERT" (discount, 2.2, prtdisc$)
            call "CONVERT" (amount-discount, 2.2, prtnet$)

            ttlgross = ttlgross + amount
            ttldisc  = ttldisc  + discount

            return

        REM *************************************************************~
            *              P R I N T S    C H E C K   O U T             *~
            *                                                           *~
            * PRINTS OUT THE CHECK THAT WE HAVE DEVELOPED IN CORE       *~
            * THERE ARE SEVERAL THINGS THAT NEED DOING.                 *~
            * 1.) IF THIS IS THE FIRST CHECK OF THE RUN, PRINT HEADER   *~
            * 2.) IF CONTINUATION, PRINT "VOID" IN BIG LETTERS.         *~
            * 3.) PRINT VOUCHER AND RETURN.                             *~
            *************************************************************

L17100: REM  ***** Print Check Body *****
                print skip(5)
                print using L19060, checknr$
                print skip(1)
                print using L19090, checkdate$
                print skip(2)
                print using L19120, prttext$, prtamount$
                print skip(1)
                print using L19150, prtvenname$
                print using L19150, prtaddress$(1)
                print using L19150, prtaddress$(2)
                print using L19150, prtaddress$(3)
                print using L19150, prtaddress$(4)
                print using L19150, prtaddress$(5)
                print skip(9)
                return

L17600: REM ***** Print large letter "VOID" for check with continuation.
                print skip(7)
                print using L19330
                print using L19340
                print using L19350
                print using L19360
                print using L19370
                print using L19380
                print using L19390
                print using L19400
                print using L19410
                print using L19420
                print using L19430
                print skip(9)
                return

L17800: REM ***** Print header lines of check voucher.

                print using L19190, prtvencode$, prtvenname$,             ~
                                   checkdate$, checknr$
                print skip(2)
                pageline% = 3%
                return

L18000: REM *************************************************************~
            *       P A G E   C O N T R O L   S U B R O U T I N E       *~
            *                                                           *~
            * This routine does a page control for the check line items.*~
            * The check handles 11 line items with TOTAL or CONTINUED,  *~
            * and prints a void message if required.                    *~
            *************************************************************



            if pagenumber% = 0 then L18310
        REM Handles case for continuation--print trailer, "VOID"
            print using L19280
            gosub L17600        /* Prints "VOID" Message      */
            gosub L35000        /* Puts it on file            */
            gosub increment_check_number
            convert checknumber% to checknr$, pic(00000000)

L18310: REM Handles case for first page--don't print trailer.
                      gosub L17800                  /* Voucher heading  */
                      pagenumber% = pagenumber% + 1
                      return

        REM *************************************************************~
            *  HERE ARE THE IMAGE STATEMENTS                            *~
            *************************************************************

        REM ***** First the check format lines *****

L19060: %                                                                ~
        ~           ########

L19090: %                                                                ~
        ~           ########

L19120: %   ##################################################           ~
        ~   $####,###,###.##

L19150: %          ###############################

        REM ***** And now the Stub portion of the Check *****

L19190: % #########   ################################################   ~
        ~########   ########

L19220: %     ################     ########     ##########     ##########~
        ~     ##########

L19250: %                        *** TOTALS *** ##########     ##########~
        ~     ##########

L19280: %                                                                ~
        ~***** CONTINUED

        REM ***** And finally the infamous V O I D *****

L19330: %         VV        VV  OOOOOOOOOOOO   IIIIIIIIII   DDDDDDDDD
L19340: %         VV        VV  OOOOOOOOOOOO   IIIIIIIIII   DDDDDDDDDD
L19350: %         VV        VV  OO        OO       II       DD       DD
L19360: %         VV        VV  OO        OO       II       DD        DD
L19370: %         VV        VV  OO        OO       II       DD        DD
L19380: %         VV        VV  OO        OO       II       DD        DD
L19390: %          VV      VV   OO        OO       II       DD        DD
L19400: %           VV    VV    OO        OO       II       DD        DD
L19410: %            VV  VV     OO        OO       II       DD       DD
L19420: %             VVVV      OOOOOOOOOOOO   IIIIIIIIII   DDDDDDDDDD
L19430: %              VV       OOOOOOOOOOOO   IIIIIIIIII   DDDDDDDDD


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* No. of Masks     */~
                                    L21200          /* First Check #    */~
                                                   /* Last  Check #    */
                     return
L20100:     REM DEFAULT/ENABLE FOR Number of Masks to Print
                masks$ = " 1"
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 2 OF INPUT. *~
            *************************************************************

            deffn'052(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L21100,         /* Print Mask       */~
                                    L21200          /* First Check #    */~
                                                   /* Last  Check #    */
                     return
L21100:     REM DEFAULT/ENABLE FOR Print a Check Mask?
                answer$ = "Y"
                return
L21200:     REM DEFAULT/ENABLE FOR First Check Number
                call "READ100" (#11, "MODULE.DEFAULTS.AP", f1%(11))
                     if f1%(11) = 0% then L21250
                get #11, using L21240, nextcheck$
L21240:             FMT XX(20), XX(4), XX(4), CH(8)
L21250:         firstcheck% = 1%
                convert nextcheck$ to firstcheck%, data goto L21270
L21270:         convert firstcheck% to nextcheck$, pic(00000000)
                checknr$ = nextcheck$:str(checknr$,1,1) = "M"

                call "REDALT0" (#9,  nextcheck$, 2%, f1%(9%))
                     if f1%(9%) <> 0% then L21350
                call "REDALT0" (#15, nextcheck$, 3%, f1%(15))
                     if f1%(15) <> 0% then L21350
                call "REDALT0" (#15, checknr$  , 3%, f1%(15))
                     if f1%(15) <> 0% then L21350
                call "REDALT0" (#16, nextcheck$, 3%, f1%(16))
                     if f1%(16) <> 0% then L21350
                call "REDALT0" (#16, checknr$  , 3%, f1%(16))
                     if f1%(16)  = 0% then L21380
L21350:         firstcheck% = firstcheck% + 1%
                goto L21270

L21380:         firstcheck$ = str(nextcheck$, 2)

                checknr$ = nextcheck$ : oldchecknr$ = "ZZZZZZZ"
                call "PLOWALTS" (#9,  checknr$, 2%, 1%, f1%(9))
                     if f1%(9) = 0% then L21450
                if str(checknr$,2,7) < str(oldchecknr$,1,7)              ~
                                    then oldchecknr$ = str(checknr$,2,7)

L21450:         checknr$ = nextcheck$
                call "PLOWALTS" (#15, checknr$, 3%, 1%, f1%(15))
                     if f1%(15) = 0% then L21487
                if str(checknr$,2,7) < str(oldchecknr$,1,7)              ~
                                    then oldchecknr$ = str(checknr$,2,7)

L21487:         checknr$ = nextcheck$:str(checknr$,1,1) = "M"
                call "PLOWALTS" (#15, checknr$, 3%, 1%, f1%(15))
                     if f1%(15) = 0% then L21500
                        if len(checknr$) < 8% then L21500
                if str(checknr$,2,7) < str(oldchecknr$,1,7)              ~
                                    then oldchecknr$ = str(checknr$,2,7)

L21500:         checknr$ = nextcheck$
                call "PLOWALTS" (#16, checknr$, 3%, 1%, f1%(16))
                     if f1%(16) = 0% then L21537
                if str(checknr$,2,7) < str(oldchecknr$,1,7)              ~
                                    then oldchecknr$ = str(checknr$,2,7)

L21537:         checknr$ = nextcheck$:str(checknr$,1,1) = "M"
                call "PLOWALTS" (#16, checknr$, 3%, 1%, f1%(16))
                     if f1%(16) = 0% then L21550
                        if len(checknr$) < 8% then L21550
                if str(checknr$,2,7) < str(oldchecknr$,1,7)              ~
                                    then oldchecknr$ = str(checknr$,2,7)

L21550:         if oldchecknr$ = "ZZZZZZZ" then oldchecknr$ = "10000000"
                lastcheck% = 10000000%
                convert oldchecknr$ to lastcheck%, data goto L21580
L21580:         lastcheck% = max(firstcheck%,min(lastcheck%-1%,9999999%))
                convert lastcheck% to lastcheck$, pic(0000000)

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

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)

            if keyhit1% = 1% then return

            return clear all
            goto check_status

L30000: REM *************************************************************~
            * L O A D   C H E C K   H E A D E R   I N F O R M A T I O N *~
            *                                                           *~
            * LOADS A CHECK HEADER FROM THE DISK                        *~
            *************************************************************

            get #9, using L30120, key1$, key2$,                           ~
                    vencode$, oldchecknr$, checkdate$, discount,         ~
                    discacct$, cashacct$, netcheck
            return

L30120:             FMT 2*CH(7),                   /* KEY + REVERSE KEY*/~
                        CH(9),                     /* VENDOR CODE      */~
                        CH(8),                     /* CHECK NUMBER     */~
                        CH(6),                     /* CHECK DATE       */~
                        PD(14,4) ,                 /* DISCOUNT AMOUNT  */~
                        2*CH(9),                   /* DISCOUNT,CASH ACC*/~
                        PD(14,4)                   /* TOTAL CHECK AMT  */

L31000: REM *************************************************************~
            *     N E X T   L I N E   I T E M   T H I S   S T U F F     *~
            *                                                           *~
            * GETS A LINE ITEM FROM BUFFER THAT WE HAVE FOUND WITH THE  *~
            * "PLOWNEXT" ROUTINE.                                       *~
            *************************************************************

            get #10, using L31120,                                        ~
                     invoicenr$, payacct$, payaccttype$, amount,         ~
                     discount, prtinvdate$
            return

L31120:     FMT XX(9),                   /* VENDOR CODE                */~
                XX(8),                   /* CHECK NUMBER               */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                CH(16),                  /* INVOICE PAYING OFF NUMBER  */~
                CH(9),                   /* ACCOUNT NUMBER TO DEBIT    */~
                CH(1),                   /* DEBIT ACCOUNT TYPE         */~
                PD(14,4) ,               /* DEBIT AMOUNT               */~
                PD(14,4) ,               /* DISCOUNT THIS DETAIL #     */~
                CH(6)                    /* INVOICE DATE THIS LINE ITEM*/~

L32000: REM *************************************************************~
            * SET VARIABLES FOR MASK PRINTING                           *~
            *************************************************************

            init("X") vencode$, checknr$, checkdate$, discacct$,         ~
                      cashacct$, prttext$, prtvencode$, prtvenname$,     ~
                      prtaddress$(), invoicenr$, payacct$,               ~
                      payaccttype$, prtinvdate$, prtinvoice$, prtgross$, ~
                      prtdisc$, prtnet$
            init("*") prtamount$
            return

L33000: REM *************************************************************~
            * REWRITE CHECK TO BUFFER WITH CORRECT CHECK NUMBER         *~
            *************************************************************

            REM FIRST THE HEADER.  DELETE OLD ONE.
                init (" ") record$()
                call "READ101" (#9,key1$,f1%(9))
                if f1%(9)=0 then return
                get #9, using L33090, str(record$(), 1)
L33090:                 FMT CH(71)
                deletor$ = str(record$(), 1, 7)
                call "DELETE" (#9, deletor$, 7%)
            REM WRITE THE NEW ONE
                readkey$ = str(record$(), 15, 17)
            write #9, using L33160, key1$, key2$, str(record$(), 15, 9),  ~
                      checknr$, str(record$(), 32)," "
L33160:         FMT CH(07), CH(07), CH(9), CH(8), CH(40), CH(29)

            REM LINE ITEMS; READ, DELETE, AND WRITE
L33190:         call "PLOWNXT1" (#10, readkey$, 17%, f1%(10))
                     if f1%(10) = 0 then return
                get #10, using L33220, str(record$(), 1)
L33220:              FMT CH(100)
                delete #10
                str(record$(), 10, 8) = checknr$
                write #10, using L33260, str(record$(), 1)
L33260:                    FMT CH(100)
                go to L33190

        REM *************************************************************~
            * G E T   P O   N U M B E R   F R O M   M A I N   F I L E   *~
            *************************************************************

            ponumber$ = " "
            readkey2$ = vencode$
            str(readkey2$, 10) = invoicenr$
            call "PLOWNEXT" (#6, readkey2$, 25%, f1%(6))
                 if f1%(6) = 0 then return
            get #6, using L34100, ponumber$
L34100:             FMT XX(16), CH(16)
            return

L35000: REM *************************************************************~
            * WRITE 'VOID' HEADER TO FILE                               *~
            *************************************************************

            REM SET UP KEYS FOR WRITING TO BUFFER--FORWARD AND REVERSE KEY
                voidkey$, voidrkey$ = " "
                call "FMTKEYCK" (#9,voidkey$,voidrkey$)

                write  #9, using L35140,                                  ~
                           voidkey$, voidrkey$, "**VOID** ", checknr$,   ~
                           date, 0, " ", " ", 0, " "

                return

L35140:                         FMT /* TO USE IN WRITING HEADER        */~
                                    2*CH(07),      /* KEY + REVERSE KEY*/~
                                    CH(9),         /* VENDOR CODE      */~
                                    CH(8),         /* CHECK NUMBER     */~
                                    CH(6),         /* CHECK DATE       */~
                                    PD(14,4),      /* DISCOUNT AMOUNT  */~
                                    2*CH(9),       /* DISCOUNT,CASH ACC*/~
                                    PD(14,4),      /* TOTAL CHECK AMT  */~
                                    CH(29)         /* FILL IT OUT      */

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40190,         /* No. of Masks     */~
                                    L40190          /* First Check #    */~
                                                   /* Last  Check #    */
                     goto L40230

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40190:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40230:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Accounts Payable Checks",                       ~
               at (01,67),                                               ~
                  "DATE:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Number of Masks to Print",                            ~
               at (06,30), fac(lfac$( 1)), masks$               , ch(02),~
               at (06,43), fac(hex(8c))  , maskmsg$             , ch(36),~
               at (07,02),                                               ~
                  "First Check Number",                                  ~
               at (07,30), fac(lfac$( 2)), firstcheck$          , ch(07),~
               at (08,02),                                               ~
                  "Last  Check Number",                                  ~
               at (08,30), fac(lfac$( 2)), lastcheck$           , ch(07),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40610
                  call "MANUAL" ("CHKPRINT")
                  goto L40230

L40610:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40230

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'102(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41160,         /* Print Mask       */~
                                    L41190          /* First Check #    */~
                                                   /* Last  Check #    */
                     goto L41230

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41160:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L41190:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41230:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Accounts Payable Checks",                       ~
               at (01,67),                                               ~
                  "DATE:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Print a Check Mask?",                                 ~
               at (06,30), fac(lfac$( 1)), answer$              , ch(01),~
               at (06,43), fac(hex(8c))  , maskmsg$             , ch(36),~
               at (07,02),                                               ~
                  "First Check Number",                                  ~
               at (07,30), fac(lfac$( 2)), firstcheck$          , ch(07),~
               at (08,02),                                               ~
                  "Last Check Number",                                   ~
               at (08,30), fac(lfac$( 2)), lastcheck$           , ch(07),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41610
                  call "MANUAL" ("CHKPRINT")
                  goto L41230

L41610:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41230

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L42190,         /* No. of Masks     */~
                                    L42190          /* First Check #    */~
                                                   /* Last  Check #    */
                     goto L42230

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L42190:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L42230:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Accounts Payable Checks",                       ~
               at (01,67),                                               ~
                  "DATE:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Number of Masks to Print",                            ~
               at (06,30), fac(lfac$( 1)), masks$               , ch(02),~
               at (06,43), fac(hex(8c))  , maskmsg$             , ch(36),~
               at (07,02),                                               ~
                  "First Check Number",                                  ~
               at (07,30), fac(lfac$( 2)), firstcheck$          , ch(07),~
               at (08,02),                                               ~
                  "Last  Check Number",                                  ~
               at (08,30), fac(lfac$( 2)), lastcheck$           , ch(07),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)PRINT CHECKS",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L42630
                  call "MANUAL" ("CHKPRINT")
                  goto L42230

L42630:        if keyhit% <> 15 then L42670
                  call "PRNTSCRN"
                  goto L42230

L42670:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   2       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 2 OF DOCUMENT.                    *~
            *************************************************************

            deffn'112(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L43160,         /* Print Mask       */~
                                    L43190          /* First Check #    */~
                                                   /* Last  Check #    */
                     goto L43230

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L43160:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L43190:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L43230:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Accounts Payable Checks",                       ~
               at (01,67),                                               ~
                  "DATE:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Print a Check Mask?",                                 ~
               at (06,30), fac(lfac$( 1)), answer$              , ch(01),~
               at (06,43), fac(hex(8c))  , maskmsg$             , ch(36),~
               at (07,02),                                               ~
                  "First Check Number",                                  ~
               at (07,30), fac(lfac$( 2)), firstcheck$          , ch(07),~
               at (08,02),                                               ~
                  "Last  Check Number",                                  ~
               at (08,30), fac(lfac$( 2)), lastcheck$           , ch(07),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)PRINT CHECKS",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L43630
                  call "MANUAL" ("CHKPRINT")
                  goto L43230

L43630:        if keyhit% <> 15 then L43670
                  call "PRNTSCRN"
                  goto L43230

L43670:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* No. of Masks     */~
                                    L51200          /* First Check #    */~
                                                   /* Last  Check #    */
                     return
L50100:     REM TEST DATA FOR Number of Masks to Print
                masks% = 0%
                convert masks$ to masks%, data goto L50145
                convert masks% to masks$, pic(##)
                return
L50145:           errormsg$ = "Invalid Entry for Number of Masks."
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51100,         /* Print Mask       */~
                                    L51200          /* First Check #    */~
                                                   /* Last  Check #    */
                     return
L51100:     REM TEST DATA FOR Print a Check Mask?
                if answer$  = "N" then return
                if answer$ <> "Y" then L51150
                   gosub print_mask
                errormsg$ = "Mask has been Printed, Do You Want Another?"
                return
L51150:            errormsg$ = "Answer 'Y' or 'N' Please"
                   return

L51200:     REM TEST DATA FOR First Check Number
                convert firstcheck$ to firstcheck%, data goto L51238
                convert firstcheck% to firstcheck$, pic(0000000)
                convert firstcheck% to nextcheck$, pic(00000000)
                convert lastcheck$ to lastcheck%, data goto L51238
                convert lastcheck% to lastcheck$, pic(0000000)
                if lastcheck% >= firstcheck% then L51248
L51238:            errormsg$ = "Invalid entry for Check Number Range."
                   return

L51248:         checknr$ = nextcheck$:str(checknr$,1,1) = "M"

                call "REDALT0" (#9,  nextcheck$, 2%, f1%(9%))
                     if f1%(9%) <> 0% then L51310
                call "REDALT0" (#15, nextcheck$, 3%, f1%(15))
                     if f1%(15) <> 0% then L51310
                call "REDALT0" (#15, checknr$  , 3%, f1%(15))
                     if f1%(15) <> 0% then L51310
                call "REDALT0" (#16, nextcheck$, 3%, f1%(16))
                     if f1%(16) <> 0% then L51310
                call "REDALT0" (#16, checknr$  , 3%, f1%(16))
                     if f1%(16)  = 0% then L51360
L51310:         errormsg$ = "First Check Number Has Been Assigned."
                   return

L51360:         checknr$ = nextcheck$ : oldchecknr$ = "ZZZZZZZ"
                call "PLOWALTS" (#9,  checknr$, 2%, 1%, f1%(9))
                     if f1%(9) = 0% then L51410
                if str(checknr$,2,7) < str(oldchecknr$,1,7)              ~
                                    then oldchecknr$ = str(checknr$,2,7)

L51410:         checknr$ = nextcheck$
                call "PLOWALTS" (#15, checknr$, 3%, 1%, f1%(15))
                     if f1%(15) = 0% then L51442
                if str(checknr$,2,7) < str(oldchecknr$,1,7)              ~
                                    then oldchecknr$ = str(checknr$,2,7)

L51442:         checknr$ = nextcheck$:str(checknr$,1,1) = "M"
                call "PLOWALTS" (#15, checknr$, 3%, 1%, f1%(15))
                     if f1%(15) = 0% then L51460
                        if len(checknr$) < 8% then L51460
                if str(checknr$,2,7) < str(oldchecknr$,1,7)              ~
                                    then oldchecknr$ = str(checknr$,2,7)

L51460:         checknr$ = nextcheck$
                call "PLOWALTS" (#16, checknr$, 3%, 1%, f1%(16))
                     if f1%(16) = 0% then L51497
                if str(checknr$,2,7) < str(oldchecknr$,1,7)              ~
                                    then oldchecknr$ = str(checknr$,2,7)

L51497:         checknr$ = nextcheck$:str(checknr$,1,1) = "M"
                call "PLOWALTS" (#16, checknr$, 3%, 1%, f1%(16))
                     if f1%(16) = 0% then L51510
                        if len(checknr$) < 8% then L51510
                if str(checknr$,2,7) < str(oldchecknr$,1,7)              ~
                                    then oldchecknr$ = str(checknr$,2,7)

L51510:         if oldchecknr$ = "ZZZZZZZZ" then oldchecknr$ = "10000000"
                nextcheck% = 10000000%
                convert oldchecknr$ to nextcheck%, data goto L51540
L51540:         nextcheck% = max(firstcheck%,min(nextcheck%-1%,9999999%))

                if nextcheck% >= lastcheck% then return

                convert nextcheck% to nextcheck$, pic(00000000)
                errormsg$ = "Highest LAST Check Number Available for Cont~
        ~inuity is # " & nextcheck$
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

            close printer

            if update% = 0% then exit_program
                call "READ101" (#11, "MODULE.DEFAULTS.AP", f1%(11))
                     if f1%(11) = 0% then exit_program
                put #11, using L65240, checknumber%
L65240:             FMT POS(29), PIC(00000000)
                rewrite #11
            goto check_status

        exit_program
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            call "SHOSTAT" ("One Moment Please")
            end update%

