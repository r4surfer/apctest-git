        REM *************************************************************~
            *                                                           *~
            *  V   V  BBBB   K   K  BBBB   Y   Y  PPPP   RRRR   TTTTT   *~
            *  V   V  B   B  K  K   B   B  Y   Y  P   P  R   R    T     *~
            *  V   V  BBBB   KKK    BBBB    YYY   PPPP   RRRR     T     *~
            *   V V   B   B  K  K   B   B    Y    P      R   R    T     *~
            *    V    BBBB   K   K  BBBB     Y    P      R   R    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VBKBYPRT - THIS PROGRAM PRINTS THE VENDR BACKLOG BY PART  *~
            *            NUMBER.                                        *~
            *            CREATES AN INDEXED WORKFILE W/ ALT KEY TO      *~
            *            RE-SORT DATA BY PART, DUEDATE, VEDOR AND THEN  *~
            *            VENDOR P.O. NUMBER.                            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/24/81 ! ORIGINAL                                 ! TOM *~
            * 08/29/83 ! ADDED SCRN DISPLAYS, MORE TOTALS ON REPRT! HES *~
            * 11/12/85 ! Made format changes for VENDOR and       ! ERN *~
            *          ! VBKLINES.                                !     *~
            * 06/15/87 ! Modified printed report format           ! MJB *~
            * 08/07/87 ! Added screen option to allow closed      !     *~
            *          ! orders to appear on reports.             ! DAW *~
            * 08/07/87 ! Fixed BUG on Re-Start from lines 15640   ! DAW *~
            * 08/27/87 ! Added Closed Extension Figures to Report ! DAW *~
            * 08/31/87 ! Swapped open & closed amount columns     ! DAW *~
            * 02/11/88 ! Background mode now available, cleaned up! RJM *~
            *          !  headers                                 !     *~
            * 11/01/88 ! Corrected part description & scrn scroll ! KAB *~
            * 03/31/89 ! Added PF8 to call POSTATUS from Scr'202  ! RJM *~
            * 06/08/89 ! Corrected GOTO @ ln 17150                ! MJB *~
            * 12/23/92 ! Merry Xmas.  PRR 12686  Cleaned up       ! JDH *~
            *          !   workfile handling.                     !     *~
            * 08/31/93 ! PRR 13014  Make sure workfile is gone.   ! JDH *~
            * 08/27/96 ! Millie date conversion                   ! DER *~
            *************************************************************


        dim                                                              ~
            atitle$10,                   /* TITLE FOR SCREEN DISLPAY   */~
            atitle$(2)79,                /* TITLE FOR SCREEN DISLPAY   */~
            back$1,                      /* BACKGROUND FLAG            */~
            back_flag$1,                 /* BACKGROUND or FOREGROUND   */~
            blankdate$8,                 /* blank unfmt date           */~
            closdext$9,                  /* Closed Extension Amount    */~
            closed_orders$1,             /* PRINT CLOSED ORDERS FLAG   */~
            company$60,                  /* Company Name For Title     */~
            currentpart$60,              /* PART being displayed       */~
            cursor%(2),                  /* Cursor Location For Edit   */~
            date$8,                      /* SYSTEM DATE                */~
            ddate$(2)10,                 /* DUE DATE RANGE             */~
            ddate%(2),                   /* DUE DATE RANGE             */~
            duedate$8,                   /* DATE LINE ITEWM DUE VEND   */~
            errormsg$79,                 /* FLASH IT JACK              */~
            edtmessage$79,               /* EDIT MODE MESSAGE          */~
            extension$9,                 /* EXTENSION ON PRINTER       */~
            grandtotal(5),               /* REPORT TOTALS              */~
            grandtotal$(5)10,            /* REPORT TOTALS, FORMATTED   */~
            header$79,                   /* HEADER FOR DISPLAY         */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* FIELD INSTRUCTIONS         */~
            job$8,                       /* JOB  NUMBER                */~
            partcode$25,                 /* PART NUMBER TO COMPARE     */~
            keys$16,                     /* PF KEYS ACTIVE             */~
            lastkey$70,                  /* PLOW KEY                   */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line$(16)79,                 /* LINES FOR SCREEN DISLPAY   */~
            message$78,                  /* INFORMATIONAL MESSAGE      */~
            msg$(4)79,                   /* Misc Messages              */~
            output$1,                    /* PRINT OR DISPLAY ?         */~
            part$25,                     /* PART                       */~
            partdescr$34,                /* PART DESCRIPTION           */~
            partcodedescr$34,            /* PART DESCRIPTION           */~
            pfkeys$32,                   /* FUNCTION KEYS ENABLE LIST  */~
            pfktext$(3)79,               /* FUNCTION KEYS ENABLE LIST  */~
            ponumber$16,                 /* PURCHASE ORDER NUMBER      */~
            price$9,                     /* UNIT PRICE OF ITEM SOLD    */~
            part$(4)25,                  /* PART NUMBER RANGE          */~
            printpart$60,                /* PART & DESC FOR REPORT     */~
            prtvencode$9,                /* PRINT VENDOR   CODE        */~
            qtybck$7,                    /* QUATITY BACKORDERED        */~
            qtyship$7,                   /* QUANTITY SHIPPED           */~
            qtyorderd$8,                 /* QUANTITY ORIG ORDERED      */~
            readkey$70,                  /* PLOW KEY                   */~
            readkey2$70,                 /* PLOW KEY                   */~
            rec$5,                       /* WORKFILE REC NUMBER        */~
            startkey$(200)70,            /* PLOW KEY                   */~
            time$8,                      /* System Time For Titles     */~
            title$24,                    /* For Printing Page 0        */~
            totalforprt(5),              /* TOTAL FOR PART NUMBER      */~
            totalforprt$(5)10,           /* TOTAL FOR PART NUMBER      */~
            userid$3,                    /* CURRENT USER ID            */~
            vencode$9,                   /* VENDOR   CODE FROM FILE    */~
            venname$32,                  /* VENDOR   NAME (DESCRIBE)   */~
            tvendor$9,                   /* VENDOR CODE TO POSTATUS    */~
            tven_po$16,                  /* VENDOR P.O. TO POSTATUS    */~
            verid$18,                    /* Name & Version             */~
            vendor$9,                    /* VENDOR CODE TO COMPARE     */~
            vendor$(4)9,                 /* VENDOR CODE RANGE          */~
            ven_po$(4)16,                /* VENDOR P.O. NUMBER RANGE   */~
            wkey$5                       /* UNIQUE KEY FOR WORKFILE    */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            fs%(64),                     /* = 1 If File Open, -1 If It */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* RESULT CODE FROM FILEOPEN  */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                /* THE VARIABLES F2%() & FS%() SHOULD NOT BE MODIFIED. */
                /* IT IS AN INTRINSIC PART OF THE FILE OPEN SUBROUTINE */


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *------+----------+-----------------------------------------*~
            *FILE #!  PRNAME  ! DESCRIPTION                             *~
            *------+----------+-----------------------------------------*~
            * #  2 ! SYSFILE2 ! SYSTEM INFO (RANGES FOR BACKGROUND)     *~
            * #  3 ! VENDOR   ! VENDOR   MASTER FILE                    *~
            * #  4 ! VBKLINES ! BACK LOG LINE ITEM FILE                 *~
            * # 09 ! WORKFILE ! WORK FILE FOR SELECTED RECORDS          *~
            * # 10 ! RCVLINES ! Receiver Line Items       (for POSTATUS)*~
            * # 11 ! VBKMASTR ! Backlog main header file  (for POSTATUS)*~
            * # 12 ! PAYLINES ! PAYABLES LINE ITEMS FILE  (for POSTATUS)*~
            * # 13 ! PAYMASTR ! PAYABLES MAIN HEADER FILE (for POSTATUS)*~
            * # 14 ! TXTFILE  ! Text File                 (for POSTATUS)*~
            *************************************************************

            select # 2,  "SYSFILE2",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            select  # 3, "VENDOR",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 600,                                  ~
                         keypos = 1, keylen = 9,                         ~
                         alt key  1, keypos = 10, keylen = 30, dup

            select  # 4, "VBKLINES",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 700,                                  ~
                         keypos = 1, keylen = 28

            select  #09, "WORKFILE",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos = 57, keylen = 5,                        ~
                         alt key  1, keypos = 1, keylen = 61

            select  #10, "RCVLINES",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 800,                                  ~
                         keypos= 26, keylen = 52,                        ~
                         alt key 1, keypos =  1, keylen = 69,            ~
                             key 2, keypos = 42, keylen = 36,            ~
                             key 3, keypos =128, keylen = 24

            select  #11, "VBKMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =  1030,                                ~
                         keypos =    1, keylen =  25,                    ~
                         alt key  1, keypos =  10,  keylen =  16

            select  #12, "PAYLINES",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 541,                                  ~
                         keypos = 36, keylen = 28,                       ~
                         alt key 1, keypos = 1, keylen = 63,             ~
                             key 2, keypos = 17, keylen = 47

            select  #13, "PAYMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 350,                                  ~
                         keypos = 1, keylen = 25

            select  #14, "TXTFILE",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos  = 1, keylen = 11

            call "SHOSTAT" ("OPENING FILES, One Moment Please")

            rslt$(2%), rslt$(3%), rslt$(4%), rslt$(5%) = "REQUIRED"
            call "OPENCHCK" (# 2, fs%(2%), f2%( 2%), 0%, rslt$(2%))
            call "OPENCHCK" (# 3, fs%(3%), f2%( 3%), 0%, rslt$(3%))
            call "OPENCHCK" (# 4, fs%(4%), f2%( 4%), 0%, rslt$(4%))

            get rslt$(4%) using L02956, rec_count%
L02956:         FMT POS(17), BI(4)
            rec_count% = rec_count% / 2%

            if min(fs%()) < 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES MISCELLANEOUS INFORMATION NEEDED TO RUN THE   *~
            * PROGRAM.                                                  *~
            *************************************************************

            verid$ = "VBKBYPRT: " & str(cms2v$,,8)

            call "EXTRACT" addr("ID", userid$, "TT", back$)

            call "TIME" (time$)
            date$ = date
            call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)

            wkey%, err% = 0%
            call "COMPNAME" (12%, company$, err%)

            if back$ = "B" then printing_in_background

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, part$(), vendor$(),        ~
                      ven_po$(), ddate$(), closed_orders$, back_flag$

            wkey%  = 0%
            f2%(9%) = 1%

L10140:     for fieldnr% = 1% to  6%
                gosub'161(fieldnr%)
                      if enabled% = 0% then L10300
L10170:         gosub'201(fieldnr%, 1%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10260
L10200:                     fieldnr% = max(1%, fieldnr% - 1%)
                            if fieldnr% = 1% then L10140
                            gosub'161(fieldnr%)
                            if enabled% <> 0% then L10170
                            goto L10200
L10260:               if keyhit%  = 16% then exit_program
                      if keyhit% <>  0% then       L10170
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10170
L10300:     next fieldnr%

        REM *************************************************************~
            *      A L L O W   E D I T   O F   S E L E C T I O N        *~
            *-----------------------------------------------------------*~
            *   also allows option to display or print the Report       *~
            *************************************************************

        editmode
            fieldnr% = 0%
            inpmessage$ = edtmessage$
            output$ = " "

            gosub'201(fieldnr%, 2%)
                if keyhit% = 1%  then  gosub startover
                if keyhit% = 14% then output$ = "P"
                if keyhit% = 14% and back_flag$ = "Y" then               ~
                                      goto access_background
                if keyhit% = 14% and back_flag$ <> "Y" then              ~
                                      goto find_records
                if keyhit% = 16% then      find_records    /* DISPLAY  */
                if keyhit% <> 0% then      editmode

            fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 6% then goto editmode
            gosub'161(fieldnr%)
                if enabled% = 0% then goto editmode
L10530:     gosub'201(fieldnr%, 2%)
                if keyhit% = 1% then  gosub startover
                if keyhit% <> 0% then      L10530
            gosub'151(fieldnr%)
                if errormsg$ <> " " then goto L10530
            goto editmode


        REM *************************************************************~
            *       P L O W   T H R O U G H   A N D   S E L E C T       *~
            *                                                           *~
            * PLOWS THROUGH THE SALES ANALYSIS MASTER RECORDS AND       *~
            * SELECTS ON THE FIELDS WITH THE VALUES WE HAVE SPECIFIED.  *~
            *************************************************************

        find_records

            readkey$ = str(vendor$(3%),,9) & hex(00)
            call "SHOSTAT"  ("Selecting Records...One Moment Please")
            call "FILEBGON" (#09)
            call "WORKOPEN" (#09, "IO   ", rec_count%, f2%(9%))
            wkey% = 1%

L12180:     call "PLOWNEXT" (# 4, readkey$, 0%, f1%(4))
                 if f1%(4) = 0% then L12550

            gosub L30000                            /* UNPACK LINE ITEM */

            if round(qtybck, 2) = 0 and closed_orders$ <> "Y" then L12180


                if partcode$ = " " then L12180       /* NEXT RECORD */
                if part$(1%) = "ALL" then L12340
                if partcode$ > part$(3%)                                 ~
                     and partcode$ <= part$(4%) then L12340
                goto L12180               /* NEXT RECORD, out of range */

L12340:         if ddate$(1%) = "ALL" then L12390
                if duedate% >= ddate%(1%)                                ~
                     and duedate% <= ddate%(2%) then L12390
                goto L12180               /* NEXT RECORD, out of range */

L12390:         if vendor$(1%) = "ALL" then L12410
                if vencode$ > vendor$(4%) then L12180  /* NEXT RECORD */

L12410:         if ven_po$(1%) = "ALL" then L12470
                if ponumber$ > ven_po$(3%)                               ~
                     and ponumber$ <= ven_po$(4%) then L12470
                goto L12180               /* NEXT RECORD, out of range */


L12470:        call "DATUNFMT" (duedate$)
               convert wkey% to wkey$, pic (#####)
               wkey% = wkey% + 1%
             write #09, using L13000, partcode$, duedate$, vencode$,      ~
                     ponumber$, wkey$, job$, partdescr$, qtyorderd,      ~
                     qtyship, qtybck, unitprice, extension, store$
               goto L12180

L12550: REM ------------ RE-OPEN WORKFILE, SORTED ALT KEY --------------

            call "WORKOPN2" (#09, "INPUT", rec_count%, f2%(9%))
                 if back$ = "B" then print_report
                 if back_flag$ <> "Y" and keyhit% = 14% then print_report
            goto show_detail

        REM -------------  REC FORMAT OF WORKFILE  ---------------------
L13000:     FMT CH(25),                  /* PART NUMBER                */~
                CH(6),                   /* DUE DATE                   */~
                CH(9),                   /* VENDOR CODE                */~
                CH(16),                  /* PURCHASE ORDER NUMBER      */~
                CH(5),                   /* UNIQUE KEY                 */~
                CH(8),                   /* JOB NUMBER                 */~
                CH(32),                  /* PART DESCRIPTION           */~
                PD(14,4),                /* QTY ORIGINNALLY ORDERED    */~
                PD(14,4),                /* QTY RECEIVED LAST SHIPMENT */~
                PD(14,4),                /* QTY OUTSTANDING ON ORDER   */~
                PD(14,7),                /* UNIT PRICE OF ITEM         */~
                PD(14,4),                /* EXTENSION (QTY*PRICE)      */~
                CH(3)                    /* STORE NUMBER               */


        rem**************************************************************~
           *       a c c e s s   b a c k g r o u n d   t a s k          *~
           *  only if the print option was selected else just display   *~
           **************************************************************~

        access_background
                call "READ101" (#2, "zVBKBYPRT." & userid$, f1%(2))
                put #2, using L14120, "zVBKBYPRT." & userid$,             ~
                  part$(), ddate$(), vendor$(), ven_po$(),               ~
                  closed_orders$

                if f1%(2) = 0% then write #2  else rewrite #2

L14120:         FMT CH(20), 15*CH(25)

                call "TASKUP" ("ME", 0%)
                goto exit_program

        printing_in_background
                call "READ101" (#2, "zVBKBYPRT." & userid$, f1%(2))
                     if f1%(2) = 0% then error_exit
                get #2, using L14120, errormsg$,                          ~
                  part$(), ddate$(), vendor$(), ven_po$(),               ~
                  closed_orders$

                delete #2

                if ddate$(1%) = "ALL" then L14250
                call "DATEOKC" (ddate$(1%), ddate%(1%), errormsg$)
                call "DATEOKC" (ddate$(2%), ddate%(2%), errormsg$)

L14250:         goto find_records

        error_exit
            message$ = "rptReport VBKBYPRT in background: Canceled."

            call "MESSAGE"addr("XM",str(userid$)&hex(20),message$,78%,0%)
            goto exit_program

        rem******************=-=-=-=-=-=-=-=-=-=-=***********************~
        =-=-=-=-=-=-=-=-=-=-=(        crt        )=-=-=-=-=-=-=-=-=-=-=-=~
        -=-=-=-=-=-=-=-=-=-=-(      display      )-=-=-=-=-=-=-=-=-=-=-=-~
*       ********************-=-=-=-=-=-=-=-=-=-=-***********************~

        show_detail

            line% = 1 : readkey$ = all(hex(00))
            call "PLOWALTS" (#09, readkey$, 1%, 0%, f1%(9%))
                if f1%(9%) = 0% then L15075
                    eod% = 99999
                    partcode$ = " "
L15060:             readkey$ = all(hex(00))
                    scrpage% = 0%
            goto continue_display
L15075:     print at(02,02), bell
            keyhit1% = 2%
            msg$(1) = "NO RECORDS FOUND"
            msg$(2) = "There are no records that meet the selection"  &  ~
                      " criteria. Key RETURN."
            msg$(3) = "THERE ARE " & wkey$ & " RECORDS IN WORKFILE"
            call "ASKUSER" (keyhit1%, msg$(1), " ", msg$(2), msg$(3))

            goto inputmode

        continue_display

            inpmessage$ = "Position Cursor and Press PF 8 to See PO Histo~
        ~ry. . ."
            pfktext$(1) = "(1)Start Over  (3)See Totals   (4)Previous  (1~
        ~3)Instructions  (15)Print Screen"
            pfktext$(2) = "(2)First       (8)PO History   (5)Next      (1~
        ~4)Print Report  (16)Return"
            keys$ = hex(0102030405080d0e0f10)

         atitle$ = "For Part:"

         atitle$(1) = "Date Due! Vendor   !Vendor's name                 ~
        ~!Project ID!Purchase Order"
         atitle$(2) = "   Store! QTY Ord'd!     QTY Ship'd!     QTY Bck'd~
        ~!Unit Price!       Extension!"


L15215:     line$() = " " : r% , line% = 1%
            for u3% = 1% to 8%
                if u3% = 1% then scrpage% = scrpage% + 1%
                if scrpage% > 200% then gosub L15900
                if u3% = 1% then startkey$(scrpage%) = readkey$
                lastkey$ = readkey$

                if u3% = 1% then                                         ~
                     call "PLOWALTS" (#09, readkey$, 1%, 0%, f1%(9%))    ~
                else call "READNEXT" (#09, f1%(9%))
                         if f1%(9%) = 0% then goto hit_end

                if u3% = 1% then part$ = str(key(#09,1%),,25)
                if part$ <> str(key(#09,1%),,25) then part_break
                gosub L31000

                readkey$ = key(#09, 1%)
                call "DESCRIBE" (#3, vencode$, venname$, 0%, f1%(3))
                if f1%(3) = 0% then venname$ = "VENDOR CODE NOT ON FILE"
                put line$(r%), using L15540, duedate$, vencode$,          ~
                      venname$, job$, ponumber$

                put line$(r%+1), using L15555, store$, qtyorderd$,        ~
                          qtyship$, qtybck$, price$, extension$
                part$ = partcode$
                goto L15390

        hit_end
                   eod% = scrpage%

        part_break
                     u3% = 8%
                     readkey$ = lastkey$
                     goto L15395

L15390:         r% = r% + 2
L15395:     next u3%

            currentpart$ = part$ & " (" & partdescr$ & ")"
            init(hex(8c)) lfac$()
            str(lfac$(),,r%-1%) = all(hex(8e))
L15410:     gosub'202
                  if keyhit%  =  1% then  gosub startover
                  if keyhit% <> 16% then  L15435
                     call "FILEBGON" (#09)
                     goto inputmode
L15435:           if keyhit%  = 14% then  print_report
                  if keyhit%  =  3% then  totals
                  if keyhit%  =  2% then  L15060
                  if keyhit% <>  4% then  L15475
                     scrpage% = max(1%, scrpage% - 1%)
                     readkey$ = startkey$(scrpage%)
                     scrpage% = scrpage% - 1%
                     goto L15215
L15475:           if keyhit%  =  5% and eod% <> scrpage% then L15215
                  if keyhit% <>  8% then L15410
                     pl% = mod(cursor%(1),2%)
                     if pl% = 0% then cursor%(1) = cursor%(1) - 1%
                     pl% = cursor%(1) - 4%
                     if pl% < 1% or pl% >= r% then L15410
                     tvendor$ = str(line$(pl%),11,9)
                     tven_po$ = str(line$(pl%),63,16)
                     mode% = 1%
                  call "POSTATUS" (tvendor$, tven_po$, " ", mode%, #4,   ~
                                   #10, #11, #12, #13, #14)
                  goto L15410

L15540: %########! #########!##############################!########  !##~
        ~##############!

L15555: %     ###!##########!     ##########!    ##########!##########!  ~
        ~    ##########!

L15900:   rem************************************************************~
             *            adjust screen page number if > 200            *~
             ************************************************************

            scrpage% = scrpage% - 1%
            for i% = 1% to scrpage%
                startkey$(i%) = startkey$(i% + 1%)
            next i%
            return

          rem******************--------------------**********************~
          --------------------- calc & show totals ----------------------~
          *********************--------------------**********************~

        totals

        venname$ = partcode$    /* SAVE PLACE IN DETAIL DISPLAY */
L16070: pagesave% = scrpage%
        scrpage% = 0%
        x3%, gt = 0
                part$, eod$, line$() = " "
                readkey2$ = all(hex(00))
                line1% = 1%
                totalforpart, totalbck = 0

                               /* READ FIRST RECORD */
                x3% = x3% + 1%
                call "PLOWALTS" (#09, readkey2$, 1%, 0%, f1%(9%))
                     if f1%(9%) = 0% then L16250
                goto L16280
                               /* READ REMAINING RECORDS */
L16210:         x3% = x3% + 1%
                call "READNEXT" (#09, f1%(9%))
                     if f1%(9%) = 0% then L16250
                goto L16280
L16250:            eod$ = "YES"
                   goto L16310

L16280:         gosub L31000
                if x3% <> 1 then L16300
                     part$ = partcode$
                     partcodedescr$ = partdescr$
L16300:         if partcode$ = part$ then L16470
L16310:         totalforpart = round(totalforpart, 2)
                totalbck = round(totalbck, 2)
          currentpart$ = "(" & partcodedescr$ & ")"
          call "RJUSTIFY" (str(currentpart$,,52))
          str(currentpart$,, len(part$)) = part$
          if pos( str(currentpart$, len(part$) + 1)  = "(" ) = 0 then    ~
                  currentpart$ = part$ & partcodedescr$
          put line$(line1%),using L16520,currentpart$,totalbck,totalforpart
          part$ = partcode$
          partcodedescr$ = partdescr$
          line1% = line1% + 1
          if eod$ = "YES"  then show_totals
          totalforpart = extension
          totalbck = qtybck
          gt = round(gt + extension,2)
          if line1% = 16 then show_totals
          goto L16210
L16470:         totalforpart = round(totalforpart + extension,2)
                totalbck = round(totalbck + qtybck,2)
                gt = round(gt + extension ,2)
                goto L16210

L16520:   %####################################################!-######.#~
        ~#!-##,###,###.##!

        REM **************  SCREEN DISPLAY FOR TOTALS *******************
        show_totals
            if line$() = " " then show_detail
            atitle$, currentpart$ = " "
            inpmessage$ = "Active PF Keys Are :"
            pfktext$(1) = "(2)First                                    (1~
        ~3)Instructions   (15)Print Screen"
            pfktext$(2) = "(5) Next                                    (1~
        ~4)Print Report   (16)Return"
            keys$ = hex(02050d0e0f10)

         atitle$(2) = "Part / Part Description                           ~
        ~  ! QTY Bck'd!   Total Value!"
         atitle$(1) = " "
         scrpage% = scrpage% + 1%

            if eod$ = "YES" then put line$(line1%), using L16520,         ~
              "     **TOTAL**     ALL SELECTED DATA ----------->"," ", gt
            init(hex(8c)) lfac$()
L16730:     gosub'202
              line$()  = " "
              line1% = 1%
              if keyhit%  = 2% then L16070
              if keyhit%  = 5% and eod$ = "YES" then L16820
              if keyhit%  = 5% then L16210
              if keyhit% = 14% then print_report
              if keyhit% = 16% then L16820
            goto L16730
L16820:       scrpage% = pagesave%
              partcode$ = venname$
              readkey$ = startkey$(scrpage%)
              scrpage% = scrpage% -1%
              goto continue_display

            rem********************--------------************************~
            *********************** print report ************************~
            ***********************--------------************************~

        print_report

            select printer (134)
            mat grandtotal = zer

            call "SHOSTAT" ("Printing Vendor Backlog by Part")

            call "SETPRNT" ("VBK005", " ", 0%, 0%)
            gosub page_zero
            pageline% = 1000%  :  pagenumber% = 0%

            readkey$ = all(hex(00))

        REM READ THE FIRST RECORD IN WORKFILE
            call "PLOWALTS" (#09, readkey$, 1%, 0%, f1%(9%))
                if f1%(9%) = 0% then L19500
                    gosub L31000
                    goto L18260
L17180:             call "READNEXT" (#09, f1%(9%))
                    if f1%(9%) = 0% then L19500
                    gosub L31000

        REM *************************************************************~
            * SEE IF WE HIT A NEW PART, IF NOT THEN ADD THE TOTAL FIELD *~
            * TO THE TOTALFORPRT FIELDS.                                *~
            *        IF THERE IS A NEW PART NUMBER, THEN PRINT THE      *~
            * PART TOTALS AND SET UP THE NEW PART LINE.                 *~
            *************************************************************

            if partcode$ = part$ then L18320

            for u3% = 1 to 3
                call "CONVERT" (totalforprt(u3%), 0.2, totalforprt$(u3%))
                grandtotal(u3%) = grandtotal(u3%) + totalforprt(u3%)
            next u3%
            for u3% = 4 to 5
                call "CONVERT" (totalforprt(u3%), 2.2, totalforprt$(u3%))
                grandtotal(u3%) = grandtotal(u3%) + totalforprt(u3%)
            next u3%
            if part$ = " " then L18280
            if pageline% + 4% <= 60% then L18200
                pageline% = 1000%
                gosub page_head
L18200:     print using L60280
            print using L60220, totalforprt$(1), totalforprt$(3),         ~
              totalforprt$(5)
            print using L60250, totalforprt$(2), totalforprt$(4)
            print skip(1)
            pageline% =  pageline% + 4%
L18260:     mat totalforprt = zer
            vendor$ = " "

L18280:     if pageline% + 2% <= 60% then L18290
                pageline% = 1000%
                gosub page_head
L18290:     printpart$ = partcode$ & ", " & partdescr$
            print using L60140, printpart$
            pageline% =  pageline% + 1%
L18320:     part$ = partcode$
            totalforprt(5) = round(totalforprt(5) + extension,2)
            totalforprt(4) = round(totalforprt(4) + closdext,2)
            totalforprt(3) = round(totalforprt(3) + qtybck,2)
            totalforprt(2) = round(totalforprt(2) + qtyship,2)
            totalforprt(1) = round(totalforprt(1) + qtyorderd,2)

        REM *************************************************************~
            * SEE IF WE HIT A NEW VEND.  CODE, IF YES THEN PRINT THE    *~
            * TOTAL FOR THIS VEND.   DROP THRU AND PRINT LINE ITEM      *~
            *************************************************************

                if vendor$ = vencode$ then L19000
                    vendor$ = vencode$
                    prtvencode$ = vencode$
                    call "DESCRIBE" (#3, vencode$, venname$, 0%, f1%(3))
                         if f1%(3%) = 0% then venname$ = "VENDOR CODE NOT~
        ~ON FILE"

L19000: rem**************************************************************~
           * print the line item of the detail                          *~
           **************************************************************

                gosub page_head        /* PAGE HEADING, IF NECCESSARY */

                print using L60310, duedate$, prtvencode$, venname$,      ~
                         ponumber$, store$, qtyorderd$, qtyship$,        ~
                         qtybck$, price$, closdext$, extension$

                pageline% = pageline% + 1%
                prtvencode$, venname$  = " "
         goto L17180

L19500: REM *************************************************************~
            *  PRINT THE TOTALS FOR THE LAST PROJ AND EXIT THE PROGRAM  *~
            *                                                           *~
            *************************************************************

             for u3% = 1 to 3
               call "CONVERT" (totalforprt(u3%), 0.2, totalforprt$(u3%))
               grandtotal(u3%) = grandtotal(u3%) + totalforprt(u3%)
               call "CONVERT" (grandtotal(u3%), 0.2, grandtotal$(u3%))
             next u3%
             for u3% = 4 to 5
               call "CONVERT" (totalforprt(u3%), 2.2, totalforprt$(u3%))
               grandtotal(u3%) = grandtotal(u3%) + totalforprt(u3%)
               call "CONVERT" (grandtotal(u3%), 2.2, grandtotal$(u3%))
             next u3%
             if pageline% + 6% > 60% then gosub page_head
             print using L60280
             print using L60220, totalforprt$(1), totalforprt$(3),        ~
               totalforprt$(5)
             print using L60250, totalforprt$(2), totalforprt$(4)
             print skip(2)
             print using L60340, grandtotal$(1), grandtotal$(3),          ~
                grandtotal$(5)
             print using L60370, grandtotal$(2), grandtotal$(4)
             close printer
             call "SETPRNT" ("VBK005", " ", 0%, 1%)
        if back$ <> "B" then L19715
             message$ = "rptReport VBKBYPRT In Background: Completed."
            call "MESSAGE"addr("XM",str(userid$)&hex(20),message$,78%,0%)
             goto exit_program
L19715: call "FILEBGON" (#09)
        goto inputmode

        REM *************************************************************~
            * D E F A U L T / E N A B L E   F O R   R A N G E   P A G E *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE RANGE INPUT SCR. *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 0%
                  inpmessage$ = " "
                  on fieldnr% gosub L20150,        /*  Part Number Range*/~
                                    L20200,        /* Due Date Range    */~
                                    L20250,        /* Vendor Code Range */~
                                    L20320,        /* Vendor Po # Range */~
                                    L20390,        /* Closed Orders Too?*/~
                                    L20460         /* Print in Bkground?*/
                     return
L20150:     REM ********  DEFAULT/ENABLE FOR  PART NUMBER RANGE  *******
                enabled% = 1%
                inpmessage$ = "Enter Range of  Part Numbers for Vender Ba~
        ~cklog"
                if part$(1) = " " then part$(1) = "ALL"
                return

L20200:     REM ********  DEFAULT/ENABLE FOR DUE DATE RANGE  ***********
                enabled% = 1%
                inpmessage$ = "Enter Range of Due Dates"
                if ddate$(1) = " " or ddate$(1%) = blankdate$ then ~
                   ddate$(1) = "ALL"
                return
L20250:     REM ********  DEFAULT/ENABLE FOR VENDOR RANGE  *************
                enabled% = 1%
                inpmessage$ = "Enter Range of Vendor Codes."
                if vendor$(1) = " " then vendor$(1) = "ALL"
                return

L20320:     REM ********  DEFAULT/ENABLE FOR VENDOR P.O. # RANGE  ******
                enabled% = 1%
                inpmessage$ = "Enter Range of Vendor P.O. Numbers."
                if ven_po$(1) = " " then ven_po$(1) = "ALL"
                return

L20390:     REM ********  DEFAULT/ENABLE FOR PRINT BOM TEXT  ***********
                enabled% = 1%
          inpmessage$ = "ENTER 'Y' to include Closed Orders."

                if closed_orders$ = " " then closed_orders$ = "N"
                return

L20460:     REM  DEFAULT/ENABLE FOR PRINT IN BACKGORUND ?
                enabled% = 1%
          inpmessage$ = "Enter 'Y' to Print Report in Background"

                if back_flag$ = " " then back_flag$ = "N"
                return

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

            if wkey% > 0% then  call "FILEBGON" (#09)
            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *    L O A D   B A C K   O R D E R    L I N E   I T E M     *~
            *                                                           *~
            * LOADS THE BACK ORDER LINE ITEM FILE (BCKLINES) AND GETS   *~
            * WHAT WE NEED AND DON'T NEED. YOU CAN'T ALWAYS GET WHAT... *~
            *************************************************************

            get   #4, using L30210, vencode$, ponumber$, partcode$,       ~
                   partdescr$, qtyorderd, qtyship, qtybck, unitprice,    ~
                   extension, duedate$, job$, store$

            call "DATEOK" (duedate$, duedate%, errormsg$)

            return

L30210:     FMT CH(9),                   /* VENDOR CODE                */~
                CH(16),                  /* PURCHASE ORDER NUMBER      */~
                XX(03),                  /* SKIP SEQUENCE NUMBER       */~
                XX(3),                   /* SKIP ITEM NUMBER           */~
                CH(25),                  /* PART NUMBER                */~
                CH(32),                  /* PART DESCRIPTION           */~
                XX(04),                  /* CATEGORY CODE              */~
                PD(14,4),                /* QTY ORIGINNALLY ORDERED    */~
                PD(14,4),                /* QTY RECEIVED LAST SHIPMENT */~
                PD(14,4),                /* QTY OUTSTANDING ON ORDER   */~
                PD(14,7),                /* UNIT PRICE OF ITEM         */~
                PD(14,4),                /* EXTENSION (QTY*PRICE)      */~
                XX(9),                   /* PURCH ACCOUNT NUMBER       */~
                CH(6),                   /* DUE DATE                   */~
                XX(6),                   /* DATE REC'D LAST SHIPMENT   */~
                XX(6),                   /* DATE REC'D NEXT SHIPMENT   */~
                XX(6),                   /* LOT NUMBER LAST SHIPMENT   */~
                CH(8),                   /* PROJECT NUMBER             */~
                CH(3)                    /* STORE NUMBER               */



L31000: REM *************************************************************~
            *    L O A D   F R O M   W O R K F I L E   (BACKGROUND)     *~
            *                                                           *~
            *                                                           *~
            *************************************************************

                get #09, using L13000, partcode$, duedate$, vencode$,     ~
                     ponumber$, rec$, job$, partdescr$, qtyorderd,       ~
                     qtyship, qtybck, unitprice, extension, store$


        REM FOR BACK ORDER REPORT, EXTENSION IS CHANGED TO QTYBCK * PRICE
            unitprice = round(unitprice, 4)
            extension = round(qtybck * unitprice, 2)
            closdext  = qtyship * unitprice

            call "DATEFMT" (duedate$)
            call "CONVERT" (unitprice, 2.4, price$)
            call "CONVERT" (extension, 2.2, extension$)
            call "CONVERT" (closdext,  2.2, closdext$)
            call "CONVERT" (qtyorderd, 0.2, qtyorderd$)
            call "CONVERT" (qtyship,   0.2, qtyship$)
            call "CONVERT" (qtybck,    0.2, qtybck$)
            if qtybck = 0 then qtybck$ = " CLOSED PO"
            return

        REM *************************************************************~
            * I N P U T   R E P O R T   R A N G E    S E L C T I O N S  *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *************************************************************

            deffn'201(fieldnr%, edit%)
                str(header$,62) = "VBKBYPRT: " & cms2v$
                if fieldnr% = 0% then gosub L41900 else gosub L41800
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41110,        /*  Part Number Range*/~
                                    L41110,        /* Due Date Range    */~
                                    L41110,        /* Vendor Code Range */~
                                    L41110,        /* Vendor Po # Range */~
                                    L41110,        /* Closed Orders Too?*/~
                                    L41110         /* Print in Bkground?*/
                     goto L41145

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41110:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41145:     accept                                                       ~
               at (01,02), "Vendor Back Log By Part Number",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Part Number",                                ~
               at (06,26), fac(lfac$( 1)), part$(1)             , ch(25),~
               at (06,53), fac(lfac$( 1)), part$(2)             , ch(25),~
                                                                         ~
               at (07,02), "Due Date",                                   ~
               at (07,26), fac(lfac$( 2)), ddate$(1)            , ch(10),~
               at (07,53), fac(lfac$( 2)), ddate$(2)            , ch(10),~
                                                                         ~
               at (08,02), "Vendor Code",                                ~
               at (08,26), fac(lfac$( 3)), vendor$(1)           , ch(09),~
               at (08,53), fac(lfac$( 3)), vendor$(2)           , ch(09),~
                                                                         ~
               at (09,02), "Vendor P.O. Number",                         ~
               at (09,26), fac(lfac$( 4)), ven_po$(1)           , ch(16),~
               at (09,53), fac(lfac$( 4)), ven_po$(2)           , ch(16),~
                                                                         ~
               at (10,02), "Include Closed Orders?",                     ~
               at (10,26), fac(lfac$( 5)), closed_orders$       , ch(01),~
                                                                         ~
               at (11,02), "Print in Background?",                       ~
               at (11,26), fac(lfac$( 6)), back_flag$           , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13% then L41350
                  call "MANUAL" ("VBKBYJOB")
                  goto L41145

L41350:        if keyhit% <> 0% then L41375
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

L41375:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L41145



        REM *************************************************************~
            *               SELECT PF KEYS                              *~
            *                                                           *~
            *************************************************************

                if edit% = 2% or fieldnr% = 1% then L41800

                pfktext$(1%) = "(1)Start Over                            ~
        ~                      (13)Instructions"
                pfktext$(2%) = "                        (4)Previous Field~
        ~                      (15)Print Screen"
                pfkeys$ = hex(0001040d0f)
                return

L41800: REM *************************************************************~
            *               SELECT PF KEYS FOR FIRST FIELD              *~
            *                      OR EDIT FIELD                        *~
            *************************************************************

                pfktext$(1%) = "(1)Start Over                            ~
        ~                      (13)Instructions"
                pfktext$(2%) = "                                         ~
        ~                      (15)Print Screen"
                if edit% = 2% then L41870
                pfktext$(3%) = "                                         ~
        ~                     " & hex(84) & "(16)Exit Program"

                pfkeys$ = hex(00010d0f10)
                return
L41870:         pfktext$(3%) = " "
                pfkeys$ = hex(00010d0f)
                return

L41900: REM *************************************************************~
            *           SELECT PF KEYS AFTER FIELDS ENTERED             *~
            *                                                           *~
            *************************************************************

                pfktext$(1%) = "(1)Start Over                            ~
        ~                      (13)Instructions"
                pfktext$(2%) = "                                         ~
        ~" & hex(84) & "(14)Print Report " & hex(8c)                      ~
        & "   (15)Print Screen"
                pfktext$(3%) = "                                         ~
        ~                     " & hex(84) & "(16)Display List"
                pfkeys$ = hex(00010d0e0f10)
                return

        REM *************************************************************~
            *       S C R E E N                     D I S P L A Y       *~
            *                                                           *~
            * DUMPS INFO TO SCREEN.                                     *~
            *************************************************************

         deffn'202

L42100:     accept                                                       ~
               "Vendor Back Log By Part",    at(1,60),"Todays Date:",    ~
         at (01,45), fac(hex(8c)), scrpage%, pic(##),                    ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(8c)), atitle$                , ch(10),~
               at (02,13), fac(hex(84)), currentpart$           , ch(52),~
               at (03,02), fac(hex(8c)), atitle$(1)             , ch(79),~
               at (04,02), fac(hex(ac)), atitle$(2)             , ch(79),~
               at (05,02), fac(lfac$( 1)), line$(01)            , ch(79),~
               at (06,02), fac(hex(8c)),   line$(02)            , ch(79),~
               at (07,02), fac(lfac$( 3)), line$(03)            , ch(79),~
               at (08,02), fac(hex(8c)),   line$(04)            , ch(79),~
               at (09,02), fac(lfac$( 5)), line$(05)            , ch(79),~
               at (10,02), fac(hex(8c)),   line$(06)            , ch(79),~
               at (11,02), fac(lfac$( 7)), line$(07)            , ch(79),~
               at (12,02), fac(hex(8c)),   line$(08)            , ch(79),~
               at (13,02), fac(lfac$( 9)), line$(09)            , ch(79),~
               at (14,02), fac(hex(8c)),   line$(10)            , ch(79),~
               at (15,02), fac(lfac$(11)), line$(11)            , ch(79),~
               at (16,02), fac(hex(8c)),   line$(12)            , ch(79),~
               at (17,02), fac(lfac$(13)), line$(13)            , ch(79),~
               at (18,02), fac(hex(8c)),   line$(14)            , ch(79),~
               at (19,02), fac(lfac$(15)), line$(15)            , ch(79),~
               at (20,02), fac(hex(8c)),   line$(16)            , ch(79),~
               at (22,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (23,02), fac(hex(8c)), pfktext$(1)            , ch(79),~
               at (24,02), fac(hex(8c)), pfktext$(2)            , ch(79),~
                                                                         ~
               keys(keys$),                                              ~
               key (keyhit%)

               if keyhit% <> 13 then L42440
                  call "MANUAL" ("VBKBYJOB")
                  goto L42100

L42440:        if keyhit% <> 15 then L42480
                  call "PRNTSCRN"
                  goto L42100

L42480:        if keyhit% <>  8 then return
                  close ws
                  call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *        TEST A FIELD FOR A 'Y' or 'N' ENTRY                *~
            *************************************************************
            deffn'159(x$)
                if x$ = " " then x$ = "N"
                if pos("YN" = x$) = 0 then L49070
                return
L49070:         errormsg$ = "Please Enter 'Y' or 'N'"
                return



        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50140,        /*  Part Number Range*/~
                                    L50240,        /* Due Date Range    */~
                                    L50310,        /* Vendor Code Range */~
                                    L50480,        /* Vendor Po # Range */~
                                    L50520,        /* Closed Orders Too?*/~
                                    L50520         /* Print in Bkground?*/
                     return
L50140:     REM **********  TEST DATA FOR PART NUMBER RANGE  ***********
                call "TESTRNGE"   (part$(1%), part$(2%), part$(3%),      ~
                                   part$(4%), errormsg$)
                return
L50240:     REM **********  TEST DATA FOR NUMBER OF LEVELS  ************
                if ddate$(1%) = "ALL" then return
                call "DATEOKC" (ddate$(1%), ddate%(1%), errormsg$)
                     if errormsg$ <> " " then return
                if ddate$(2%) <> " " and ddate$(2%) <> blankdate$ then L50247
                ddate$(2%) = "20991231"

L50247:         call "DATEOKC" (ddate$(2%), ddate%(2%), errormsg$)
                     if errormsg$ <> " " then return
                if ddate%(1%) > ddate%(2%) then                          ~
                     errormsg$ = "INVALID DATE RANGE!"
                return

L50310:    REM ***********  TEST DATA FOR EFFECTIVITY DATE  *************
                call "TESTRNGE"   (vendor$(1%), vendor$(2%), vendor$(3%),~
                                   vendor$(4%), errormsg$)
                return

L50480:    REM **********  TEST DATA FOR PRINT OPTION FLAG  ************
                call "TESTRNGE"   (ven_po$(1%), ven_po$(2%), ven_po$(3%),~
                                   ven_po$(4%), errormsg$)
                return

L50520:    REM *********  TEST DATA FOR TEXT PRINT FLAG  **************
                gosub'159(closed_orders$)
                return

           REM *********  TEST DATA FOR TEXT PRINT FLAG  **************
                gosub'159(back_flag$)
                return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *************************************************************

L60040: %RUN DATE: ######## @ ########     ##############################~
        ~##############################                      VBKBYPRT: VBK~
        ~005

L60080: %                                V E N D O R   B A C K L O G   B ~
        ~Y   P A R T   N U M B E R                               PAGE: ###

L60110: %                                V E N D O R     P. O. ' s     B ~
        ~Y   P A R T   N U M B E R                               PAGE: ###

L60140: % PART NUMBER: ##################################################~
        ~##########

L60160: % DUE DATE VENDOR    VENDOR NAME                     P.O. NUMBER ~
        ~       STR  QTY ORD QTY SHP QTY BCK UNT PRICE CLOSD EXT  OPEN EXT

L60190: % -------- --------- ------------------------------- ------------~
        ~------ --- -------- ------- ------- --------- --------- ---------

L60220: %                                               ***** TOTALS FOR ~
        ~PART *** ##########      ##########                    ##########

L60250: %                                                                ~
        ~                 ##########                  ##########

L60280: %                                                                ~
        ~           -------- ------- -------           --------- ---------

L60310: % ######## ######### ############################### ############~
        ~###### ### ######## ####### ####### ######### ######### #########

L60340: %                                 ********** REPORT GRAND TOTALS ~
        ~******** ##########      ##########                    ##########

L60370: %                                                                ~
        ~                 ##########                  ##########


        %                                       * * * * *  E N D   O F   ~
        ~R E P O R T  * * * * *

        REM *************************************************************~
            *  P A G E   H E A D I N G / C O N T R O L   R O U T I N E  *~
            *                                                           *~
            * TRACKS WHICH LINE OF THE PAGE WE ARE ON, SKIPS TO NEW PAGE*~
            * AND PRINTS HEADINGS IF WE'RE NOT ABLE TO FIT IT ALL ON ONE*~
            *************************************************************
        page_head

            select printer (134)
            if pageline% < 60 then return
               print page
               pagenumber% = pagenumber% + 1
               print using L60040, date$, time$, company$
               if closed_orders$ <> "Y" then print using L60080,          ~
                                                        pagenumber%      ~
                   else print using L60110, pagenumber%
               print skip (1)
               if pagenumber% = 0% then return
        /*     PRINT USING 60140, PARTCODE$, JOBDESCR$       */
               print skip (1)
               print using L60190
               print using L60160
               print using L60190
               pageline% = 7%
               return

        REM *************************************************************~
            *  P A G E   Z E R O  C O N T R O L   R O U T I N E         *~
            *                                                           *~
            * PRINTS PAGE ZERO CONTAINING THE INPUT RANGES OF THE REPORT*~
            *************************************************************
        page_zero

            pageline% = 1000%
            pagenumber% = -1%
            gosub page_head
            print skip (6)

            read title$
            print title$, tab(27), part$(1%);
            if part$(2) = " " then print                                 ~
                     else print tab(57), "TO", tab(62), part$(2%)

            print skip (1)
            read title$
            print title$, tab(27), ddate$(1%);
            if ddate$(2) = " " or ddate$(2%) = blankdate$ then print  ~
                     else print tab(57), "TO", tab(62), ddate$(2%)

            print skip (1)
            read title$
            print title$, tab(27), vendor$(1%);
            if vendor$(2) = " " then print                               ~
                     else print tab(57), "TO", tab(62), vendor$(2%)

            print skip (1)
            read title$
            print title$, tab(27), ven_po$(1%);
            if ven_po$(2) = " " then print                               ~
                     else print tab(57), "TO", tab(62), ven_po$(2%)

            print skip (1)
            read title$
            print title$, tab(27), closed_orders$

            print skip (1)
            read title$
            print title$, tab(27), back_flag$

            restore
            return

        data "Part Code", "Due Date", "Vendor Code",                     ~
             "Vendor P.O. Number", "Include Close Orders ?",             ~
             "Print in Background?"

        REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************
        exit_program

            call "SHOSTAT" ("Closing Files, One Moment Please.")
            if wkey% > 0% then  call "FILEBGON" (#09)

            end
