        REM *************************************************************~
            *                                                           *~
            *  V   V  BBBB   K   K  BBBB   Y   Y  JJJJJ   OOO   BBBB    *~
            *  V   V  B   B  K  K   B   B  Y   Y    J    O   O  B   B   *~
            *  V   V  BBBB   KKK    BBBB    YYY     J    O   O  BBBB    *~
            *   V V   B   B  K  K   B   B    Y    J J    O   O  B   B   *~
            *    V    BBBB   K   K  BBBB     Y     J      OOO   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VBKBYJOB - This Program Prints the Vendor Backlog By      *~
            *            Project Number.  Watch It! Variables are Named *~
            *            'JOB' this, and 'JOB' that.  The Literals have *~
            *            Been Changed But All References to Job mean    *~
            *            'PROJECT'.  Can Print in Background, by User   *~
            *            Option  And  Sorts By Job, Due Date, Vendor,   *~
            *            And Finally PO Number Using Indexed Workfile.  *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/20/81 ! ORIGINAL                                 ! TOM *~
            * 08/24/83 ! ADDED SCREEN DISPLAY OPTION              ! HES *~
            * 11/12/85 ! Made changes to VENDOR and VBKLINES file ! ERN *~
            *          ! formats plus a little cleanup.           !     *~
            * 03/14/86 ! CHANGED LITERALS FROM 'JOB' TO 'PROJECT' ! WPH *~
            * 08/07/87 ! Added screen option to allow closed      !     *~
            *          ! orders to appear on reports.             ! DAW *~
            * 08/07/87 ! Fixed BUG on Re-Starts from lines 15365  ! DAW *~
            * 08/27/87 ! Changed NUMPRINT to CONVERT              ! DAW *~
            * 08/28/87 ! Added Closed Extension Amount            ! DAW *~
            * 08/28/87 ! ADDED GRAND TOTALS FOR REPORT            ! DAW *~
            * 08/31/87 ! Swapped position of open & close columns ! DAW *~
            * 01/21/87 ! Added page break for new jobs            ! RJM *~
            * 02/08/87 ! Standardized & Put Printing in Background! RJM *~
            * 03/31/89 ! Added PF8 to call POSTATUS from Scr'202  ! RJM *~
            * 06/07/90 ! Fixed PRR 11329.  Program blow up when   ! SID *~
            *          !    it attempts to Reopen an Already open !     *~
            *          !    file.                                 !     *~
            * 08/27/96 ! Millie date conversion                   ! DER *~
            *************************************************************


        dim                                                              ~
            atitle$10,                   /* TITLE FOR SCREEN DISLPAY   */~
            atitle$(2)79,                /* TITLE FOR SCREEN DISLPAY   */~
            back$1,                      /* BACKGROUND FLAG            */~
            blankdate$8,                 /* blank unfmt date           */~
            closdext$10,                 /* Closed Extension Amount    */~
            closed_orders$1,             /* PRINT CLOSED ORDERS FLAG   */~
            company$60,                  /* Company Name For Title     */~
            currentjob$40,               /* JOB  being displayed       */~
            currentpart$60,              /* PART being displayed       */~
            cursor%(2),                  /* Cursor Location For Edit   */~
            date$8,                      /* SYSTEM DATE                */~
            ddate$(2)10,                 /* DUE DATE RANGE             */~
            ddate%(2),                   /* DUE DATE RANGE             */~
            duedate$8,                   /* DATE LINE ITEWM DUE VEND   */~
            errormsg$79,                 /* FLASH IT JACK              */~
            edtmessage$79,               /* EDIT MODE MESSAGE          */~
            extension$10,                /* EXTENSION ON PRINTER       */~
            file$8,                                                      ~
            grndforjob$10,               /* Grand Total Open   Amount  */~
            grndclsjob$10,               /* Grand Total Closed Amount  */~
            header$79,                   /* HEADER FOR DISPLAY         */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* FIELD INSTRUCTIONS         */~
            job$8,                       /* JOB  NUMBER                */~
            jobcode$8,                   /* JOB  NUMBER TO COMPARE     */~
            jobdescr$32,                 /* JOB  DESCRIPTION           */~
            keys$16,                     /* PF KEYS ACTIVE             */~
            lastkey$50,                  /* PLOW KEY                   */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lib$8,                                                       ~
            line$(16)79,                 /* LINES FOR SCREEN DISLPAY   */~
            message$78,                  /* INFORMATIONAL MESSAGE      */~
            msg$(4)79,                   /* Misc Messages              */~
            output$1,                    /* PRINT OR DISPLAY ?         */~
            part$25,                     /* PART                       */~
            partdescr$32,                /* PART DESCRIPTION           */~
            pfkeys$32,                   /* FUNCTION KEYS ENABLE LIST  */~
            pfktext$(3)79,               /* FUNCTION KEYS ENABLE LIST  */~
            ponumber$16,                 /* PURCHASE ORDER NUMBER      */~
            price$9,                     /* UNIT PRICE OF ITEM SOLD    */~
            proj$(4)8,                   /* PROJECT NUMBER RANGE       */~
            prtvencode$9,                /* PRINT VENDOR   CODE        */~
            qtybck$7,                    /* QUATITY BACKORDERED        */~
            qtyship$7,                   /* QUANTITY SHIPPED           */~
            qtyorderd$8,                 /* QUANTITY ORIG ORDERED      */~
            readkey$50,                  /* PLOW KEY                   */~
            readkey2$50,                 /* PLOW KEY                   */~
            rec$5,                       /* WORKFILE REC NUMBER        */~
            startkey$(200)50,            /* PLOW KEY                   */~
            time$8,                      /* System Time For Titles     */~
            title$24,                    /* For Printing Page 0        */~
            totalclsjob$10,              /* Total Closed Amount - Job  */~
            totalforjob$10,              /* TOTAL FOR JOB  NUMBER      */~
            userid$3,                    /* CURRENT USER ID            */~
            tvendor$9,                   /* VENDOR CODE TO POSTATUS    */~
            tven_po$16,                  /* VENDOR P.O. TO POSTATUS    */~
            vencode$9,                   /* VENDOR   CODE FROM FILE    */~
            venname$32,                  /* VENDOR   NAME (DESCRIBE)   */~
            verid$18,                    /* Name & Version             */~
            vendor$9,                    /* VENDOR CODE TO COMPARE     */~
            vendor$(4)9,                 /* VENDOR CODE RANGE          */~
            ven_po$(4)16,                /* VENDOR P.O. NUMBER RANGE   */~
            vol$6,                                                       ~
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
            * #  5 ! JOBMASTR ! PROJECT MASTER FILE                     *~
            * #  9 ! WORKFILE ! WORK FILE FOR SELECTED RECORDS          *~
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

            select  # 5, "JOBMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 700,                                  ~
                         keypos = 1, keylen = 8

            select  # 9, "WORKFILE",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos = 1, keylen = 44

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

            rslt$(2), rslt$(3), rslt$(4), rslt$(5) = "REQUIRED"
            call "OPENCHCK" (# 2, fs%(2), f2%( 2), 0%, rslt$(2))
            call "OPENCHCK" (# 3, fs%(3), f2%( 3), 0%, rslt$(3))
            call "OPENCHCK" (# 4, fs%(4), f2%( 4), 0%, rslt$(4))
            call "OPENCHCK" (# 5, fs%(5), f2%( 5), 0%, rslt$(5))

            if min(fs%()) < 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES MISCELLANEOUS INFORMATION NEEDED TO RUN THE   *~
            * PROGRAM.                                                  *~
            *************************************************************

            verid$ = "VBKBYJOB: " & str(cms2v$,,8)

            call "EXTRACT" addr("ID", userid$, "TT", back$)

            call "TIME" (time$)
            date$ = date
            call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)

            wkey%, err% = 0%
            call "COMPNAME" (12%, company$, err%)

            call "GETNAMES" addr(#4, file$, lib$, vol$)
            call "READFDR" addr(file$,lib$,vol$,0%,"RC",rec_count%,err%)
            if err% > 0% then rec_count% = 1000%
            rec_count% = rec_count% / 4%

            if back$ = "B" then printing_in_background

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, proj$(), vendor$(),        ~
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
            if f2%(9) = 0% then close #9 : f2%(9) = 1
            call "WORKOPEN" (# 9, "IO", rec_count%, f2%(9))
            wkey% = 1%

L12180:     call "PLOWNEXT" (# 4, readkey$, 0%, f1%(4))

                 if f1%(4) = 0% and back$ = "B" then print_report
                 if f1%(4) = 0% and back_flag$ <> "Y" and keyhit% = 14%  ~
                      then print_report
                 if f1%(4) = 0% then show_detail

            gosub L30000                            /* UNPACK LINE ITEM */

            if round(qtybck, 2) = 0 and closed_orders$ <> "Y" then L12180


                if jobcode$ = " " then L12180       /* NEXT RECORD */
                if proj$(1%) = "ALL" then L12340
                if jobcode$ > proj$(3%)                                  ~
                     and jobcode$ <= proj$(4%) then L12340
                goto L12180               /* NEXT RECORD, out of range */

L12340:         if ddate$(1%) = "ALL" then L12390
                if duedate% >= ddate%(1%)                                ~
                     and duedate% <= ddate%(2%) then L12390
                goto L12180               /* NEXT RECORD, out of range */

L12390:        if vencode$ > vendor$(4%) then L12180  /* NEXT RECORD */

                if ven_po$(1%) = "ALL" then L12470
                if ponumber$ > ven_po$(3%)                               ~
                     and ponumber$ <= ven_po$(4%) then L12470
                goto L12180               /* NEXT RECORD, out of range */


L12470:        call "DATUNFMT" (duedate$)
               convert wkey% to wkey$, pic (#####)
               wkey% = wkey% + 1%
             write #9, using L13000, jobcode$, duedate$, vencode$,        ~
                     ponumber$, wkey$, part$, partdescr$, qtyorderd,     ~
                     qtyship, qtybck, unitprice, extension, store$
               goto L12180

L13000:     FMT CH(8),                   /* PROJECT NUMBER             */~
                CH(6),                   /* DUE DATE                   */~
                CH(9),                   /* VENDOR CODE                */~
                CH(16),                  /* PURCHASE ORDER NUMBER      */~
                CH(5),                   /* UNIQUE KEY                 */~
                CH(25),                  /* PART NUMBER                */~
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
                call "READ101" (#2, "zVBKBYJOB." & userid$, f1%(2))
                put #2, using L14120, "zVBKBYJOB." & userid$,             ~
                  proj$(), ddate$(), vendor$(), ven_po$(),               ~
                  closed_orders$

                if f1%(2) = 0% then write #2  else rewrite #2

L14120:         FMT CH(20), 15*CH(25)

                call "TASKUP" ("ME", 0%)
                goto exit_program

        printing_in_background
                call "READ101" (#2, "zVBKBYJOB." & userid$, f1%(2))
                     if f1%(2) = 0% then error_exit
                get #2, using L14120, errormsg$,                          ~
                  proj$(), ddate$(), vendor$(), ven_po$(),               ~
                  closed_orders$

                delete #2

                if ddate$(1%) = "ALL" then L14250
                call "DATEOKC" (ddate$(1%), ddate%(1%), errormsg$)
                call "DATEOKC" (ddate$(2%), ddate%(2%), errormsg$)

L14250:         goto find_records

        error_exit
            message$ = "rptReport VBKBYJOB in background: Canceled."

            call "MESSAGE"addr("XM",str(userid$)&hex(20),message$,78%,0%)
            goto exit_program

        rem******************=-=-=-=-=-=-=-=-=-=-=***********************~
        =-=-=-=-=-=-=-=-=-=-=(        crt        )=-=-=-=-=-=-=-=-=-=-=-=~
        -=-=-=-=-=-=-=-=-=-=-(      display      )-=-=-=-=-=-=-=-=-=-=-=-~
*       ********************-=-=-=-=-=-=-=-=-=-=-***********************~

        show_detail

            line% = 1 : readkey$ = all(hex(00))
            call "PLOWALTS" (# 9, readkey$, 0%, 0%, f1%(9%))
                if f1%(9%) = 0% then L15055
            goto continue_display
L15055:     print at(02,02), bell
            keyhit1% = 2%
            if wkey$ = " " then wkey$ = "0"
            msg$(1) = "NO RECORDS FOUND"
            msg$(2) = "There are no records that meet the selection"  &  ~
                      " criteria. Key RETURN."
            msg$(3) = "THERE ARE " & wkey$ & " RECORDS IN WORKFILE"
            call "ASKUSER" (keyhit1%, msg$(1), " ", msg$(2), msg$(3))
            close #9
            goto inputmode

        continue_display

            inpmessage$ = "Position Cursor and Press PF 8 to See PO Histo~
        ~ry. . ."
            pfktext$(1) = "(1)Start Over  (3)See Totals   (4)Previous  (1~
        ~3)Instructions  (15)Print Screen"
            pfktext$(2) = "(2)First       (8)PO History   (5)Next      (1~
        ~4)Print Report  (16)Return"
            keys$ = hex(0102030405080d0e0f10)

         atitle$ = "For Proj:"

         atitle$(1) = "Date Due! Vendor   !Vendor's name                 ~
        ~      !Purchase Order"
         atitle$(2) = "Part(Part Description)                            ~
        ~      ! QTY Bck'd!Unit Price!"

        eod% = 99999
        jobcode$ = " "
L15200: readkey$ = all(hex(00))
        scrpage% = 0%

L15215:     line$() = " " : r% , line% = 1%
            for u3% = 1% to 7%
                if u3% = 1% then scrpage% = scrpage% + 1%
                if scrpage% > 200% then gosub L15900
                if u3% = 1% then startkey$(scrpage%) = readkey$
                lastkey$ = readkey$
                call "PLOWALTS" (# 9, readkey$, 0%, 0%, f1%(9%))
                         if f1%(9) = 0% then goto hit_end

                gosub L31000
                if u3% = 1% then job$ = jobcode$
                if jobcode$ <> job$ then job_break

                call "DESCRIBE" (#3, vencode$, venname$, 0%, f1%(3))
                if f1%(3) = 0% then venname$ = "VENDOR CODE NOT ON FILE"
                put line$(r%), using L15560, duedate$, vencode$,          ~
                      venname$, ponumber$
                currentpart$ = partdescr$
                call "PUTPAREN" (currentpart$)
                call "RJUSTIFY" (str(currentpart$,,55))
                str(currentpart$,, len(part$)) = part$
                if pos( str(currentpart$, len(part$) + 1)  = "(" ) = 0   ~
                          then  currentpart$ = part$&partdescr$
                put line$(r%+1), using L15570, currentpart$,              ~
                          qtybck$, price$
                job$ = jobcode$
                goto L15390

        hit_end
                   eod% = scrpage%

        job_break  : u3% = 8%
                     readkey$ = lastkey$
                     goto L15395

L15390:         r% = r% + 2
L15395:     next u3%

            call "DESCRIBE" (#5, job$, jobdescr$, 1%, f1%(5))
            if f1%(5) = 0% then jobdescr$ = "(JOB CURRENTLY NOT ON FILE)"
            currentjob$ = job$ & " " & jobdescr$
            init(hex(8c)) lfac$()
            str(lfac$(),,r%-1%) = all(hex(8e))
L15430:     gosub'202
                  if keyhit%  =  1% then  gosub startover
                  if keyhit% <> 16% then  L15455
                     call "FILEBGON" (# 9)
                     goto inputmode
L15455:           if keyhit%  = 14% then  print_report
                  if keyhit%  =  3% then  totals
                  if keyhit%  =  2% then  L15200
                  if keyhit% <>  4% then  L15495
                     scrpage% = max(1%, scrpage% - 1%)
                     readkey$ = startkey$(scrpage%)
                     scrpage% = scrpage% - 1%
                     goto L15215
L15495:           if keyhit%  =  5% and eod% <> scrpage% then L15215
                  if keyhit% <>  8% then L15430
                     pl% = mod(cursor%(1),2%)
                     if pl% = 0% then cursor%(1) = cursor%(1) - 1%
                     pl% = cursor%(1) - 4%
                     if pl% < 1% or pl% >= r% then L15430
                     tvendor$ = str(line$(pl%),11,9)
                     tven_po$ = str(line$(pl%),58,16)
                     mode% = 1%
                  call "POSTATUS" (tvendor$, tven_po$, " ", mode%, #4,   ~
                                   #10, #11, #12, #13, #14)
                  goto L15430

L15560: %########! #########!################################    !#######~
        ~#########
L15570: % #######################################################!#######~
        ~###! #########!

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

        venname$ = jobcode$    /* SAVE PLACE IN DETAIL DISPLAY */
L16070: x3%, gt = 0
                job$, eod$, line$() = " "
                readkey2$ = all(hex(00))
                line1% = 1
                totalforjob, totalclsjob, totalforven = 0
L16120:         x3% = x3% + 1
                call "PLOWALTS" (# 9, readkey2$, 0%, 0%, f1%(9%))
                     if f1%(9%) = 0% then L16170

                goto L16200
L16170:            eod$ = "YES"
                   goto L16230

L16200:         gosub L31000
                if x3% = 1 then job$ = jobcode$
                if jobcode$ = job$ then L16370
L16230:         totalforjob = round(totalforjob, 2)
                call "DESCRIBE" (#5, job$, jobdescr$, 0%, f1%(5))
                if f1%(5) = 0 then jobdescr$ = "PROJECT NOT ON FILE"
                put line$(line1%), using L16420, job$, jobdescr$,         ~
                           totalforjob
                job$ = jobcode$
                line1% = line1% + 1
                if eod$ = "YES"  then show_totals
                totalforjob = extension
                gt = round(gt + extension,2)
           if line1% = 16 then show_totals
           goto L16120
L16370:         totalforjob = round(totalforjob + extension, 2)
                gt = round(gt + extension,2)
                go to L16120

L16420: %######## ! ##############################    !-##,###,###.##!

        REM **************  SCREEN DISPLAY FOR TOTALS *******************
        show_totals
            if line$() = " " then show_detail
            atitle$, currentjob$ = " "
            inpmessage$ = "Active PF Keys Are :"
            pfktext$(1) = "(2)First                                    (1~
        ~3)Instructions   (15)Print Screen"
            pfktext$(2) = "(5) Next                                    (1~
        ~4)Print Report   (16)Return"
            keys$ = hex(02050d0e0f10)

         atitle$(2) = "Project  ! Description                       !Exte~
        ~nded Total!"
         atitle$(1) = " "

            if eod$ = "YES" then put line$(line1%), using L16420,         ~
                     "*TOTAL*","ALL SELECTED DATA ----------->", gt
            init(hex(8c)) lfac$()
L16610:     gosub'202
              line$()  = " "
              line1% = 1%
              if keyhit%  = 2% then L16070
              if keyhit%  = 5% and eod$ = "YES" then L16690
              if keyhit%  = 5% then L16120
              if keyhit% = 14% then print_report
              if keyhit% = 16% then L16690
            goto L16610
L16690:       jobcode$ = venname$
              readkey$ = startkey$(scrpage%)
              scrpage% = scrpage% -1%
              goto continue_display

            rem********************--------------************************~
            *********************** print report ************************~
            ***********************--------------************************~

        print_report

            select printer (134)
            totalforjob, totalclsjob, grndforjob, grndclsjob = 0

            call "SHOSTAT" ("Printing Vendor Backlog by Job")

            call "SETPRNT" ("VBK004", " ", 0%, 0%)
            gosub page_zero
            pageline% = 1000%  :  pagenumber% = 0%

            readkey$ = all(hex(00))

        REM READ THE INDEXED FILE AND PRINT THE DATA
L17120:         call "PLOWALTS" (# 9, readkey$, 0%, 0%, f1%(9%))
                      if f1%(9) = 0% then L19500

                gosub L31000

                if pagenumber% = 0% then job$ = jobcode$

                call "DESCRIBE" (#5, jobcode$, jobdescr$, 0%, f1%(5))
                    if f1%(5) = 0 then jobdescr$ = "PROJECT NOT ON FILE"

                if pagenumber% = 0% then vendor$ = vencode$

                prtvencode$ = vencode$
                call "DESCRIBE" (#3, vencode$, venname$, 0%, f1%(3))
                    if f1%(3) = 0 then venname$ = "VENDOR NOT ON FILE"

        REM *************************************************************~
            * SEE IF WE HIT A NEW PROJ, IF NOT THEN ADD THE TOTAL FIELD *~
            * TO THE TOTALFORJOB FIELD AND DROP THROUGH TO CHECK FOR THE*~
            * VENDOR IF THERE IS A NEW PROJECT   NUMBER, THEN TOTAL THE *~
            * VEND.  FOR THIS PROJ AND INITIALIZE THE VEND  AND PROJECT *~
            * TOTAL FIELDS AND GO ON TO PRINT A NEW PAGE.               *~
            *************************************************************

                if jobcode$ = job$ then L18230
                    call "CONVERT" (totalforjob, 2.2, totalforjob$)
                    call "CONVERT" (totalclsjob, 2.2, totalclsjob$)
                    print using L60100
                    print using L60190, job$, totalclsjob$, totalforjob$
                    print skip(1)
                    pageline% = 1000%
                    gosub page_head                    /* NEXT PAGE */
                    job$ = jobcode$
                    totalforjob, totalclsjob = 0

L18230:         totalforjob = round(totalforjob + extension, 2)
                totalclsjob = round(totalclsjob + closdext,  2)
                grndforjob = grndforjob + extension
                grndclsjob = grndclsjob + closdext

        REM *************************************************************~
            * SEE IF WE HIT A NEW VEND.  CODE, IF YES THEN PRINT THE    *~
            * TOTAL FOR THIS VEND.   DROP THRU AND PRINT LINE ITEM      *~
            *************************************************************

                if vendor$ = vencode$ then L19000
                    vendor$ = vencode$
                    prtvencode$ = vencode$
                    totalforven = 0
                    totalforven = round(totalforven + extension, 2)

L19000: rem**************************************************************~
           * print the line item of the detail                          *~
           **************************************************************

                gosub page_head        /* PAGE HEADING, IF NECCESSARY */

                print using L60250, duedate$, prtvencode$, venname$,      ~
                                   ponumber$

                currentpart$ = part$ & " (" & partdescr$ & ")"

                print using L60320, currentpart$, store$, qtyorderd$,     ~
                      qtyship$, qtybck$, price$, closdext$, extension$

                pageline% = pageline% + 1%
                prtvencode$, venname$  = " "
         goto L17120

L19500: REM *************************************************************~
            *  PRINT THE TOTALS FOR THE LAST PROJ AND EXIT THE PROGRAM  *~
            *                                                           *~
            *************************************************************

             print using L60100
                     if pagenumber% > 0% then L19600
                          goto L19670
L19600:      call "CONVERT" (totalforjob, 2.2, totalforjob$)
             call "CONVERT" (totalclsjob, 2.2, totalclsjob$)
             call "CONVERT" (grndforjob,  2.2, grndforjob$)
             call "CONVERT" (grndclsjob,  2.2, grndclsjob$)
             print using L60190, job$, totalclsjob$, totalforjob$
             print skip(1)
             print using L60220, grndclsjob$, grndforjob$
L19670:      print skip(2)
             print using L60350
             close printer
             call "SETPRNT" ("VBK004", " ", 0%, 1%)
        if back$ <> "B" then L19715
             message$ = "rptReport VBKBYJOB In Background: Completed."
            call "MESSAGE"addr("XM",str(userid$)&hex(20),message$,78%,0%)
             goto exit_program
L19715: call "FILEBGON" (# 9)
        goto inputmode

        REM *************************************************************~
            * D E F A U L T / E N A B L E   F O R   R A N G E   P A G E *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE RANGE INPUT SCR. *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 0%
                  inpmessage$ = " "
                  on fieldnr% gosub L20150,        /* Project Code Range*/~
                                    L20200,        /* Due Date Range    */~
                                    L20250,        /* Vendor Code Range */~
                                    L20320,        /* Vendor Po # Range */~
                                    L20390,        /* Closed Orders Too?*/~
                                    L20460         /* Print in Bkground?*/
                     return
L20150:     REM ********  DEFAULT/ENABLE FOR PROJECT CODE RANGE  *******
                enabled% = 1%
                inpmessage$ = "Enter Range of Project Codes for Vender Ba~
        ~cklog"
                if proj$(1) = " " then proj$(1) = "ALL"
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

            if wkey% > 0% then  call "FILEBGON" (# 9)
            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *    L O A D   B A C K   O R D E R    L I N E   I T E M     *~
            *                                                           *~
            * LOADS THE BACK ORDER LINE ITEM FILE (BCKLINES) AND GETS   *~
            * WHAT WE NEED AND DON'T NEED. YOU CAN'T ALWAYS GET WHAT... *~
            *************************************************************

            get   #4, using L30210, vencode$, ponumber$, part$,partdescr$,~
                   qtyorderd, qtyship, qtybck, unitprice, extension,     ~
                   duedate$, jobcode$, store$

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

                get #9, using L13000, jobcode$, duedate$, vencode$,       ~
                     ponumber$, rec$, part$, partdescr$, qtyorderd,      ~
                     qtyship, qtybck, unitprice, extension, store$


        REM FOR BACK ORDER REPORT, EXTENSION IS CHANGED TO QTYBCK * PRICE
            extension = round(qtybck * unitprice,2)
            closdext  = qtyship * unitprice

            call "DATEFMT" (duedate$)
            call "CONVERT" (unitprice, 2.7, price$)
            call "CONVERT" (extension, 2.2, extension$)
            call "CONVERT" (closdext,  2.2, closdext$)
            call "CONVERT" (qtyorderd, 0.2, qtyorderd$)
            call "CONVERT" (qtyship,   0.2, qtyship$)
            call "CONVERT" (qtybck,    0.2, qtybck$)

            return

        REM *************************************************************~
            * I N P U T   R E P O R T   R A N G E    S E L C T I O N S  *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *************************************************************

            deffn'201(fieldnr%, edit%)
                str(header$,62) = "VBKBYJOB: " & cms2v$
                if fieldnr% = 0% then gosub L41900 else gosub L41800
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41110,        /* Project Code Range*/~
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
               at (01,02), "Vendor Back Log By Project Report",          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Project Code",                               ~
               at (06,30), fac(lfac$( 1)), proj$(1)             , ch(08),~
               at (06,56), fac(lfac$( 1)), proj$(2)             , ch(08),~
                                                                         ~
               at (07,02), "Due Date",                                   ~
               at (07,30), fac(lfac$( 2)), ddate$(1)            , ch(10),~
               at (07,56), fac(lfac$( 2)), ddate$(2)            , ch(10),~
                                                                         ~
               at (08,02), "Vendor Code",                                ~
               at (08,30), fac(lfac$( 3)), vendor$(1)           , ch(09),~
               at (08,56), fac(lfac$( 3)), vendor$(2)           , ch(09),~
                                                                         ~
               at (09,02), "Vendor P.O. Number",                         ~
               at (09,30), fac(lfac$( 4)), ven_po$(1)           , ch(16),~
               at (09,56), fac(lfac$( 4)), ven_po$(2)           , ch(16),~
                                                                         ~
               at (10,02), "Include Closed Orders?",                     ~
               at (10,30), fac(lfac$( 5)), closed_orders$       , ch(01),~
                                                                         ~
               at (11,02), "Print in Background?",                       ~
               at (11,30), fac(lfac$( 6)), back_flag$           , ch(01),~
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
               "Vendor Back Log By Project", at(1,60),"Todays Date:",    ~
         at (01,45), fac(hex(8c)), scrpage%, pic(##),                    ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(8c)), atitle$                , ch(10),~
               at (02,13), fac(hex(84)), currentjob$            , ch(52),~
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
                  on fieldnr% gosub L50140,        /* Project Code Range*/~
                                    L50240,        /* Due Date Range    */~
                                    L50310,        /* Vendor Code Range */~
                                    L50480,        /* Vendor Po # Range */~
                                    L50520,        /* Closed Orders Too?*/~
                                    L50520         /* Print in Bkground?*/
                     return
L50140:     REM **********  TEST DATA FOR PART NUMBER RANGE  ***********
                call "TESTRNGE"   (proj$(1%), proj$(2%), proj$(3%),      ~
                                   proj$(4%), errormsg$)
                return
L50240:     REM **********  TEST DATA FOR NUMBER OF LEVELS  ************
                if ddate$(1%) = "ALL" then td_date_rtn
                call "DATEOKC" (ddate$(1%), ddate%(1%), errormsg$)
                     if errormsg$ <> " " then td_date_rtn
                if ddate$(2%) <> " " and ddate$(2%) <> blankdate$ then L50247
                ddate$(2%) = "20991231"
                call "DATEOKC" (ddate$(2%), ddate%(2%), errormsg$)
                goto td_date_rtn
L50247:         call "DATEOKC" (ddate$(2%), ddate%(2%), errormsg$)
                     if errormsg$ <> " " then td_date_rtn
                if ddate%(1%) > ddate%(2%) then                          ~
                     errormsg$ = "INVALID DATE RANGE!"
              td_date_rtn
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

L60032: %RUN DATE: ######## @ ########     ##############################~
        ~##############################                      VBKBYJOB: VBK~
        ~004

L60040: %                                V E N D O R   B A C K L O G   B ~
        ~Y   P R O J E C T   N U M B E R                         PAGE: ###

L60070: %                                V E N D O R     P. O. ' s     B ~
        ~Y   P R O J E C T   N U M B E R                         PAGE: ###

L60095: % FOR PROJECT :  ########  ###############################

L60100: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--
L60130: %DUE DATE  VENDOR        VENDOR  NAME               OUR P.O. NUMB~
        ~ER    STR  QTY ORD QTY SHP QTY BCK UNIT PRCE  CLOSD EXT    OPEN E~
        ~XT
        %                                                                ~
        ~                           TOTAL FOR VENDOR ########## ##########

L60190: %                                                                ~
        ~     TOTAL FOR PROJECT NUMBER ########  IS   ########### ########~
        ~###
L60220: %                                                                ~
        ~     *****  REPORT GRAND TOTALS  *****       ########### ########~
        ~###
L60250: %######## ######### ############################### ############


        %                                                               #~
        ~#####


L60320: %         #######################################################~
        ~##### ### ######## ####### ####### ######### ##########  ########~
        ~##

L60350: %                                       * * * * *  E N D   O F   ~
        ~R E P O R T  * * * * *

        REM *************************************************************~
            *  P A G E   H E A D I N G / C O N T R O L   R O U T I N E  *~
            *                                                           *~
            * TRACKS WHICH LINE OF THE PAGE WE ARE ON, SKIPS TO NEW PAGE*~
            * AND PRINTS HEADINGS IF WE'RE NOT ABLE TO FIT IT ALL ON ONE*~
            *************************************************************
        page_head

            select printer (134)
            pageline% = pageline% + 1
            if pageline% < 56 then return
               print page
               pagenumber% = pagenumber% + 1
               print using L60032, date$, time$, company$
               if closed_orders$ <> "Y" then print using L60040,          ~
                                                        pagenumber%      ~
                   else print using L60070, pagenumber%
               print skip (1)
               if pagenumber% = 0% then return
               print using L60095, jobcode$, jobdescr$
               print skip (1)
               print using L60100
               print using L60130
               print using L60100
               pageline% = 8
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
            print tab(10), title$, tab(37), proj$(1%);
            if proj$(2) = " " then print                                 ~
                     else print tab(57), "TO", tab(62), proj$(2%)

            print skip (1)
            read title$
            print tab(10), title$, tab(37), ddate$(1%);
            if ddate$(2) = " " or ddate$(2%) = blankdate$ then print ~
                     else print tab(57), "TO", tab(62), ddate$(2%)

            print skip (1)
            read title$
            print tab(10), title$, tab(37), vendor$(1%);
            if vendor$(2) = " " then print                               ~
                     else print tab(57), "TO", tab(62), vendor$(2%)

            print skip (1)
            read title$
            print tab(10), title$, tab(37), ven_po$(1%);
            if ven_po$(2) = " " then print                               ~
                     else print tab(57), "TO", tab(62), ven_po$(2%)

            print skip (1)
            read title$
            print tab(10), title$, tab(37), closed_orders$

            print skip (1)
            read title$
            print tab(10), title$, tab(37), back_flag$

            restore
            return

        data "Project Code", "Due Date", "Vendor Code",                  ~
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
            if wkey% > 0% then  call "FILEBGON" (# 9)

            end
