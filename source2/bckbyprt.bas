        REM *************************************************************~
            *                                                           *~
            *  BBBB    CCC   K   K  BBBB   Y   Y  PPPP   RRRR   TTTTT   *~
            *  B   B  C   C  K  K   B   B  Y   Y  P   P  R   R    T     *~
            *  BBBB   C      KKK    BBBB    YYY   PPPP   RRRR     T     *~
            *  B   B  C   C  K  K   B   B    Y    P      R   R    T     *~
            *  BBBB    CCC   K   K  BBBB     Y    P      R   R    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKBYPRT - THIS PROGRAM PRINTS THE BACKLOG FILE (BCKLINES)*~
            *            BY THE PART NUMBER AND SUBTOTALING BY THE CUSTO*~
            *            MER. ONLY THE FIRST 16 CHARACTERS OF THE PART  *~
            *            NUMBER IS USED IN THE SORT. IF IT IS NECESSARY *~
            *            TO INCREASE TO 25, THE CODE IS REMM'D IN HERE. *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/20/81 ! ORIGINAL                                 ! TOM *~
            * 08/26/83 ! ADDED SCRN DISPLAYS, MORE TOTALS ON REPRT! HES *~
            * 11/11/86 ! 41701 Compatablity, Added Order Date     ! HES *~
            * 05/13/87 ! HNYMASTR record length mod for Std Cost  ! JIM *~
            * 03/22/88 ! Added Grand Totals to report             ! RJM *~
            * 06/13/88 ! QC REWORK  Todays -> Today's             ! RJM *~
            * 03/01/89 ! Remove duplicate CMS2V$ assign statement ! MJB *~
            * 05/17/90 ! Changed qty calcs & only report backlog- ! JDH *~
            *          !  ged items.                              !     *~
            * 06/21/90 ! Standardized screens somewhat. Rpt too.  ! JDH *~
            * 04/12/91 ! PRR 11777 Printed a wrong part number    ! SID *~
            *          !     When there's a page continuation.    !     *~
            *          !     Added input messages to the report   !     *~
            *          !     input screen.                        !     *~
            *          ! Added a Non-Stocked Part Descr to Report ! SID *~
            *************************************************************

        com                                                              ~
            extlen%(15),                 /* EXTERNAL FIELD LENGTHS     */~
            field%(15),                  /* SELECTABLE FIELD LIST      */~
            format$(15)1,                /* DATA TYPE FORMAT CODES     */~
            from$(15)25,                 /* LOW RANGE DATA TEST ITEMS  */~
            fromnr(15),                  /* LOW RANGE NUMERIC TEST ITEM*/~
            length%(15),                 /* INTERNAL FIELD LENGTHS     */~
            position%(15),               /* POSITION IN REC (FROM 1)   */~
            prompt$(15)25,               /* FIELD NAME PROMPT          */~
            record$(12)250,              /* 3 RECORDS * 1000 CHARS EA. */~
            record%(15),                 /* WHICH OF 3 RECORDS IT'S IN */~
            to$(15)25,                   /* HI VALUE RANGE DATA TEST   */~
            tonr(15)                     /* HI RANGE NUMERIC RANGE TEST*/~

        dim                                                              ~
            company$50,                  /* Company header             */~
            currentpart$60,              /* part being displayed       */~
            currhdrpart$60,              /* part being displayed       */~
            cuscode$9,                   /* CUSTOMER CODE FROM FILE    */~
            savename$32,                 /* CUSTOMER NAME (DESCRIBE)   */~
            savecode$9,                  /* CUSTOMER CODE FROM FILE    */~
            cusname$32,                  /* CUSTOMER NAME (DESCRIBE)   */~
            customer$9,                  /* CUSTOMER SWITCH            */~
            custotal(4),                 /* CUSTOMER TOTALS (Print)    */~
            date$8,                      /* SYS DATE                   */~
            duedate$8,                   /* DATE LINE ITEWM DUE CUST   */~
            errormsg$79,                 /* FLASH IT JACK              */~
            extension$10,                /* EXTENSION ON PRINTER       */~
            grandtotal(4),               /* GRAND TOTALS (Print)       */~
            hdr1$8,                      /* Column header              */~
            hdr2$9,                      /* Column header              */~
            hdr3$10,                     /* Column header              */~
            hdr4$10,                     /* Column header              */~
            hdr5$10,                     /* Column header              */~
            hdr6$10,                     /* Column header              */~
            hdr7$16,                     /* Column header              */~
            hdr8$53,                     /* Column header              */~
            hdr9$14,                     /* Column header              */~
            hdrdate$45,                  /* HEADER DATE FOR REPORT     */~
            input$1,                     /*                            */~
            inpmessage$79,               /*                            */~
            inst$(3)79,                  /* INSTRUC FOR SCREEN DISLPAY */~
            invoicenr$8,                 /* INVOICE NUMBER             */~
            job$8,                       /* JOB CODE                   */~
            keys$16,                     /* PF KEYS ACTIVE             */~
            line$(16)79,                 /* LINES FOR SCREEN DISLPAY   */~
            line2$79,                    /* 2nd Header Line            */~
            part$25,                     /* PART NUMBER                */~
            partcode$25,                 /* PART NUMBER TO COMPARE     */~
            partdescr$34,                /* PART DESCRIPTION           */~
            parttotal(4),                /* PART TOTALS (Print)        */~
            price$10,                    /* UNIT PRICE OF ITEM SOLD    */~
            prtcuscode$9,                /* PRINT CUSTOMER CODE        */~
            qtybck$10,                   /* QUATITY BACKORDERED        */~
            qtyship$10,                  /* QUANTITY SHIPPED           */~
            qtyorderd$10,                /* QUANTITY ORIG ORDERED      */~
            sonumber$16,                 /* SALES ORDER NUMBER         */~
            sortkey$64,                  /* KEY TO GO TO SORT FILE     */~
            tempdescr$34,                /* Temp Part Descr            */~
            temppart$60,                 /* Temp Part Code             */~
            title$10,                    /* TITLE FOR SCREEN DISLPAY   */~
            title$(2)79,                 /* TITLE FOR SCREEN DISLPAY   */~
            totalbck$10,                 /* Total part backlogged qty  */~
            readkey$50                   /* READ RECORD WITH THIS      */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *------+----------+-----------------------------------------*~
            *FILE #!  PRNAME  ! DESCRIPTION                             *~
            *------+----------+-----------------------------------------*~
            * #  3 ! CUSTOMER ! CUSTOMER MASTER FILE                    *~
            * #  4 ! BCKLINES ! BACK LOG LINE ITEM FILE                 *~
            * #  5 ! HNYMASTR ! INVENTORY MASTER FILE                   *~
            * #  6 ! BCKMASTR ! BACKLOG MASTER FILE (GET STORE NUMBER)  *~
            * #  9 ! WORKFILE ! WORK FILE FOR SELECTED RECORDS          *~
            *************************************************************

        select #3,  "CUSTOMER"                                           ~
                    varc,                                                ~
                    indexed,                                             ~
                    recsize = 1200,                                      ~
                    keypos=1, keylen=9,                                  ~
                    alt key  1, keypos =   10, keylen =  30, dup,        ~
                        key  2, keypos =  424, keylen =   9, dup,        ~
                        key  3, keypos =  771, keylen =   9, dup,        ~
                        key  4, keypos =  780, keylen =   9, dup

            select  # 4, "BCKLINES",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 10, keylen = 19

            select  # 5, "HNYMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90,  keylen = 4, dup

            select  # 6, "BCKMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 1000,                                 ~
                         keypos =    1, keylen =  25,                    ~
                         alt key  1, keypos =   26, keylen =  16, dup

            select # 9, "WORKFILE",                                      ~
                        consec,                                          ~
                        recsize = 100

            call "SHOSTAT" ("Linking To The Data Base To Show The Backlog~
        ~")

            call "OPENCHCK" (#3, 0%, f2%(3), 0%, " ")
            call "OPENCHCK" (#4, 0%, f2%(4), 0%, " ")
            call "OPENCHCK" (#5, 0%, f2%(5), 0%, " ")
            call "OPENCHCK" (#6, 0%, f2%(6), 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES MISCELLANEOUS INFORMATION NEEDED TO RUN THE   *~
            * PROGRAM.  ALSO INITIALIZES STUFF NEEDED FOR THE SUPER     *~
            * SELECTOR ROUTINE.                                         *~
            *                                                           *~
            * HERE IS HOW THE PARAMETERS FOR THE SELECT (ALSO SORT) KEYS*~
            * WORK.  THE FIRST OF EACH DATA ITEM IS THE NAME OF THE KEY.*~
            * IT IS THE PROMPT THAT APPEARS ON THE SCREEN.              *~
            *      THE FIRST ITEM TO THE RIGHT OF THE PROMPT IS THE     *~
            * DATA TYPE.  THE DATA TYPE IS ONE OF THE FOLLOWING.        *~
            *                                                           *~
            *      U = UPPER CASE ALPHANUMERIC.                         *~
            *      L = UPPER/LOWER CASE ALPHANUMERIC.                   *~
            *      D = DATE-FORMATTED UPPER CASE FIELD.  MUST BE YYMMDD *~
            *      N = NUMERIC 8-BYTE FLOATING PT. ALL OTHER NUMERICS   *~
            *          SHOULD BE COMPARED ASCII, OR MODIFY THE ROUTINE. *~
            *                                                           *~
            * THE REMAINING NUMBERS IDENTIFY RESPECTIVELY THE LENGTH OF *~
            * THE FIELD ON THE DISK, THE NUMBER OF POSITIONS IT FILLS   *~
            * ON THE PAPER, WHICH OF THE UP-TO-3 RECORDS IN CORE THIS   *~
            * FIELD LIES IN, AND THE POSITION OF THE FIELD WITHIN THE   *~
            * RECORD.                                                   *~
            *                                                           *~
            * THE FIRST BLANK PROMPT NAME SIGNIFIES THE END OF THE      *~
            * LIST OF PROMPTS ON THE SYSTEM.                            *~
            *************************************************************

            REM SET UP DATA FOR SELECT INTERPRETATION.
                for temp% = 1 to 15
                    read prompt$(temp%), format$(temp%), length%(temp%), ~
                         extlen%(temp%), record%(temp%), position%(temp%)
                    next temp%

                data "Part Number              ", "U", 25, 25, 1, 032,   ~
                     "Customer Code            ", "U", 09, 09, 1, 001,   ~
                     "S.O. Number              ", "U", 16, 16, 1, 010,   ~
                     "Due Date                 ", "D", 06, 08, 1, 206,   ~
                     "Order Date               ", "D", 06, 08, 1, 301,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001

            date$ = date
            call "DATEFMT" (date$)

            hdr1$ = " Store"
            hdr2$ = "Due Date"
            hdr3$ = " Qty Ord'd"
            hdr4$ = "Qty Ship'd"
            hdr5$ = " Qty Bck'd"
            hdr6$ = "Unit Price"
            hdr7$ = "       Extension"
            hdr8$ = "Part / Part Description"
            hdr9$ = "   Total Value"

           str(line2$,62%) = "BCKBYPRT: " & str(cms2v$,,8%)
           call "COMPNAME" (12%, company$, u3%)

L10000: REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            *                                                           *~
            * GETS THE ***SUPER SELECTOR*** INFORMATION FOR THE PROGRAM *~
            * WHICH WE WILL USE IN THE SELECT PASS, WHICH FOLLOWS.      *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, from$(), to$()
            mat custotal = zer : mat parttotal = zer : mat grandtotal =zer

            inst$(1) = "Enter 'D' or 'P' :"
            inst$(2) = "                                                 ~
        ~              (15)Print Screen"
            inst$(3) = "                                           (13)In~
        ~structions    (16)EXIT PROGRAM"


         title$ = "For Part:"
         currentpart$ = " "
         title$(1) = "Ord Date Customer   Customer's Descriptive Name    ~
        ~ Project ID Sales Order Nmbr "
*       TITLE$(2) = "  Store !Due Date ! QTY Ord'd!QTY Ship'd!QTY Bck'd!~
*       Unit Price!       Extension!"

L10220:    gosub L40000
           if keyhit% = 16 then L65000
           if pos("DP" =  input$) = 0 then L10220
           output$ = "Display"
           if input$ = "P" then output$ = "Report"

L10272:     mat fromnr = zer: mat tonr = zer: mat field% = zer
            init(" ") to$()
            for temp% = 1 to 15
                if prompt$(temp%) = " " then L10310
                   from$(temp%) = "ALL"
L10310:         next temp%

            partdescr$ = "Back Log By Part " & output$
            inpmessage$ = "Press <RETURN> to Print or PF16 to Exit."
L10330:     call "SLCTSCRN" (partdescr$, errormsg$, inpmessage$, keyhit%,~
                                                   "BCKBYPRT: " & cms2v$)
                  if keyhit%  =  1 then L10000
                  if keyhit%  = 16 then L65000
                  if keyhit% <>  0 then L10330
            call "SLCTTEST" (errormsg$, maxfields%)
                  if errormsg$ <> " " then L10330

        REM *************************************************************~
            *       P L O W   T H R O U G H   A N D   S E L E C T       *~
            *                                                           *~
            * PLOWS THROUGH THE SALES ANALYSIS MASTER RECORDS AND       *~
            * SELECTS ON THE FIELDS WITH THE VALUES WE HAVE SPECIFIED.  *~
            *************************************************************

            readkey$, lastsonumber$ = " "
            call "SHOSTAT"  ("Selecting Records...One Moment Please")
            call "WORKOPEN" (#9, "OUTPT", 5000%, f2%(9))

L14110:     call "PLOWNEXT" (#4, readkey$, 0%, f1%(4))
                 if f1%(4) = 0 then L14450          /* GO SORT WORKFILE */
            get #4, using L14124, opn, pre_inv
L14124:         FMT POS(109), PD(14,4), POS(133), PD(14,4)
            if opn - pre_inv <= 0 then L14110  /* Don't list non-backlog */
            get #4, str(record$(),1)

        REM READ THE BCKMASTR FILE AND GET STORE NUMBER SKIP IF ORPHAN
            if lastsonumber$ = str(record$(),10,16) then L14270
            call "READ100" (#6, str(record$(),,25), f1%(6))
                 if f1%(6) <> 0 then L14220
L14190:          str(readkey$, 17) = hex(ffffff)
                 goto L14110

L14220:     get #6, using L14230, sonumber$, store$, order$
L14230:         FMT XX(9), CH(16), POS(803), CH(3), CH(6)
            lastsonumber$ = sonumber$
            if maxfields% < 5 then L14270
            if orderdate$ < from$(5) or orderdate$ > to$(5) then L14190
L14270:     orderdate$, str(record$(),301) = order$
            call "DATEFMT" (orderdate$)

          call "SLCTPASS" (maxfields%, select%)
          if select% = 0 then L14110          /* GET NEXT RECORD  */
          get str(record$(),109,8), using L14330, qtybck, data goto L14110
L14330:        FMT PD(14,4)
               if qtybck = 0 then L14110
               sortkey$ = " "
               str(sortkey$,  1, 25) = str(record$(), 32, 25) /* PART  */
               str(sortkey$, 26,  3) = store$                 /* STORE */
               str(sortkey$, 29,  6) = str(record$(), 206, 6) /* DUE DT*/
               str(sortkey$, 35,  9) = str(record$(), 1, 9)   /* CUST. */
               str(sortkey$, 44, 16) = str(record$(), 10, 16) /* SO #  */
               write #9,using L14420,sortkey$,str(readkey$,,19),orderdate$
L14420:        FMT CH(64), CH(19), CH(8)
               goto L14110

L14450:     REM NOW THAT SELECT ROUTINE HAS FINISHED, SORT THE RESULT.
                call "SLCTSORT" (#9, 59%)

        rem******************=-=-=-=-=-=-=-=-=-=-=***********************~
        =-=-=-=-=-=-=-=-=-=-=         crt         =-=-=-=-=-=-=-=-=-=-=-=~
        -=-=-=-=-=-=-=-=-=-=-       display       -=-=-=-=-=-=-=-=-=-=-=-~
*       ********************-=-=-=-=-=-=-=-=-=-=-***********************~

        show_detail

            line% = 1
            read #9, record = 1, eod goto L15100
            goto L15140
L15100:     errormsg$ = "There are no records that meet the select criter~
        ~ia."
            close #9
            f2%(9) = 1
            call "FILEBGON" (#9)
            goto L10272

L15140:  if output$ = "Report" then L17000

        continue_display
            eod% = 99999

L15280:     line$() = " " : r% = 1
            no_next% = 0%
            for u3% = line% to line% + 7
             read #9, record = u3%, using L15320, sortkey$, readkey$,     ~
                                             orderdate$, eod goto hit_end
L15320:      FMT CH(64), CH(19), CH(8)
             sonumber$ = str(readkey$,,16)
             store$ = str(sortkey$,26,3)
          if str(sortkey$,,25)<>partcode$ and u3% <>line% then part_break
             call "READ100" (#4, str(readkey$, 1, 19), f1%(4))
                   if f1%(4) = 0 then L15510
             gosub L30000              /* GET LINE   RECORD */

             call "DESCRIBE" (#3, cuscode$, cusname$, 0%, f1%(3))
             if f1%(3) = 0 then cusname$ = "CUSTOMER CODE NOT ON FILE"
             put line$(r%), using L15640, orderdate$, cuscode$, cusname$, ~
                                            job$, sonumber$
             put line$(r%+1), using L15660, store$, duedate$, qtyorderd$, ~
                            qtyship$, qtybck$, price$, extension$
             goto L15510

        hit_end     : eod% = line%
                      no_next% = 1%
                      put line$(r%), using L15640, "* EOD *"
                      goto L15520

        part_break  : u3% = line% + 8
                      goto L15520

L15510:     r% = r% + 2
L15520:     next u3%

            call "DESCRIBE" (#5, partcode$, partdescr$, 1%, f1%(5))
            if f1%(5) = 0 then partdescr$ = "(PART NOT ON FILE)"
            currentpart$ = partcode$ & " " & partdescr$
            str(line2$,,61%) = currentpart$
            gosub L41000
                  if keyhit%  =  1 then close #9
                  if keyhit%  =  1 then f2%(9) = 1
                  if keyhit%  =  1 then call "FILEBGON" (#9)
                  if keyhit%  =  1 then goto L10272
                  if keyhit%  = 16 then       L65000
                  if keyhit%  =  3 then       totals
                  if keyhit%  =  2 then  line% = 1
                  if keyhit%  =  4 then  gosub move_back
                  if keyhit%  =  5 then  line% =                         ~
                                          min(line% + (r% - 1)/2, eod%)
                  if keyhit%  = 12 then  L17000
                  partcode$ = sortkey$
                  goto L15280

        move_back
             x3% = 0 : no_next% = 0%
L15624:      line% = max(0, line% - 1)
             if line% = 0 then L15632
             read #9, record = line%, using L15320, sortkey$, readkey$,   ~
                                               orderdate$, eod goto L15624
             store$ = str(sortkey$,26,3) : sonumber$ = str(readkey$,,16)
             if str(sortkey$,,25) = partcode$ then L15624
             if x3% = 0 then L15634
             sortkey$ = partcode$
L15632:      line% = line% + 1 : return
L15634:      partcode$ = sortkey$
             x3% = 1 : goto L15624

L15640: %######## #########  ##############################  ########   #~
        ~###############
L15660: %  ###    ########  ########## ########## ########## ##########  ~
        ~     ##########

          rem*******************-------------------**********************~
          ---------------------- get & show totals ----------------------~
          **********************-------------------**********************~

        totals
            call "SHOSTAT" ("Totaling...")
        cusname$ = partcode$    /* SAVE PLACE IN DETAIL DISPLAY */
L16060: x3%, gt = 0
                part$, eod$, line$() = " "
                line1% = 1
                totalforpart, totalbck = 0
L16100:         x3% = x3% + 1
                read #9, record = x3%, using L16130, sortkey$, readkey$,  ~
                                               orderdate$, eod goto L16150
L16130:         FMT CH(64), CH(19), CH(8)
                store$ = str(sortkey$,26,3)
                sonumber$ = str(readkey$,,16)
                goto L16170
L16150:            eod$ = "YES"
                   goto L16220
L16170:         call "READ100" (#4, str(readkey$, 1, 19), f1%(4))
                      if f1%(4) = 0 then L16100
                gosub L30000
                if x3% = 1 then part$ = partcode$
                if partcode$ = part$ then L16290
L16220:         totalforpart = round(totalforpart, 2)
                call "CONVERT" (totalbck, 0.2, totalbck$)
          call "DESCRIBE" (#5, part$, partdescr$, 1%, f1%(5))
          if f1%(5) = 0 then partdescr$ = "(PART NOT ON FILE)"
          currentpart$ = partdescr$
          call "STRING" addr("RJ", str(currentpart$,,53), 53%)
          str(currentpart$,, len(part$)) = part$
          if pos(currentpart$="(" )=0 then currentpart$ = part$&partdescr$
          put line$(line1%),using L16330,currentpart$, totalbck$,         ~
                                        totalforpart
          part$ = partcode$
          line1% = line1% + 1
          if eod$ = "YES"  then show_totals
          totalforpart = extension
          totalbck = qtybck
          gt = gt + extension
          if line1% = 16 then show_totals
          goto L16100
L16290:         totalforpart = totalforpart + extension
                totalbck = totalbck + qtybck
                gt = gt + extension
                go to L16100

L16330:   %##################################################### ########~
        ~## -##,###,###.##

        REM **************  SCREEN DISPLAY FOR TOTALS *******************
        show_totals
            if line$() = " " then show_detail
            title$, currentpart$ = " "
            inst$(1) = " "
            inst$(2) = "(2)First                                         ~
        ~              (15)Print Screen"
            inst$(3) = "(5)Next                                    (13)In~
        ~structions    (16)RETURN"
            keys$ = hex(ff02050d0f10)

            if eod$ <> "YES" then L16450
            str(inst$(3),,7) = " " : str(keys$, 3, 1) = hex(ff)

L16450:     str(line2$,,61%) = " "

            if eod$ = "YES" then put line$(line1%), using L16330,         ~
              "     **TOTAL**     ALL SELECTED DATA ----------->"," ", gt
            gosub L42000
              line$()  = " "
              line1% = 1
              if keyhit%  = 2% then L16060
              if keyhit%  = 5% then L16100
              partcode$ = cusname$
              goto continue_display

L17000:     rem********************--------------************************~
            *********************** print report ************************~
            ***********************--------------************************~

            call "SHOSTAT" ("Printing Customer Backlog by Part Number")

            select printer (134)
            call "SETPRNT" ("BCK012", " ", 0%, 0%)

            pageline% = 1000 :pagenumber% = 0%
            mat custotal = zer : mat parttotal = zer : mat grandtotal =zer

        REM GET THE FIRST RECORD
L17100:         read #9, record = 1, using L17120, partcode$, store$,     ~
                                     readkey$, orderdate$, eod goto L65000
L17120:                  FMT CH(25),CH(3), XX(36), CH(19), CH(8)
                sonumber$ = str(readkey$,,16)
                call "READ100" (#4, str(readkey$, 1, 19), f1%(4))
                      if f1%(4) = 0 then L17100

                gosub L30000                        /*LOAD LINE   RECORD*/
                call "PUTPAREN" (partdescr$)
                currentpart$ = partcode$ & " " & partdescr$
                currhdrpart$ = currentpart$
                part$ = partcode$
                customer$ = cuscode$
                prtcuscode$ = cuscode$
                call "DESCRIBE" (#3, cuscode$, cusname$, 0%, f1%(3))
                if f1%(3) = 0 then cusname$ = "CUSTOMER NOT ON FILE"
                savename$ = cusname$
                savecode$ = cuscode$
                goto L18000

        REM GET THE NEXT RECORDS USING THE READ COMMAND
L17300:         read #9, using L17120, partcode$, store$, readkey$,       ~
                                              orderdate$, eod goto L19170
                sonumber$ = str(readkey$,,16)

        REM READ DETAIL FILE TO SEE IF RECORD EXISTS (IT SHOULD)
                call "READ100" (#4, str(readkey$, 1, 36), f1%(4))
                      if f1%(4) = 0 then L17300

                gosub L30000              /* GET LINE   RECORD */
                call "PUTPAREN" (partdescr$)
                currentpart$ = partcode$ & " " & partdescr$

L18000: REM *************************************************************~
            * SEE IF WE HIT A NEW PRT, IF NOT THEN ADD THE TOTAL FIELDS *~
            * TO THE PART TOTALS ARRAY AND DROP THROUGH TO CHECK FOR THE*~
            * CUSTOMER. IF THERE IS A NEW PRT NUMBER, THEN TOTAL THE    *~
            * CUST.  FOR THIS PRT AND INITIALIZE THE CUST.  AND PART    *~
            * TOTAL FIELDS AND GO ON TO PRINT A NEW PAGE.               *~
            *************************************************************

                if partcode$ = part$ then L18360
                print using L50120
                pageline% = pageline% + 1
                if pageline% > 54 then gosub L60000
                print using L50180, custotal(1), custotal(2), custotal(3),~
                                   custotal(4)
                print using L50210
                pageline% = pageline% + 2
                mat custotal = zer
                prtcuscode$, customer$ = cuscode$
                call "DESCRIBE" (#3, cuscode$, cusname$, 0%, f1%(3))
                if f1%(3) = 0 then cusname$ = "CUSTOMER NOT ON FILE"
                savename$ = cusname$
                savecode$ = cuscode$
                if pageline% > 55 then gosub L60000
                print using L50240, part$, parttotal(1), parttotal(2),    ~
                                   parttotal(3), parttotal(4)
                pageline% = pageline% + 1
                mat parttotal = zer
                part$ = partcode$
                if pageline% > 50 then gosub L60000
                if pageline% = 7 then L18360
                print
                print using L50100, currentpart$
                print using L50120
                pageline% = pageline% + 3

L18360:         parttotal(4) = round(parttotal(4) + extension, 2)
                parttotal(3) = round(parttotal(3) + qtybck   , 2)
                parttotal(2) = round(parttotal(2) + qtyship  , 2)
                parttotal(1) = round(parttotal(1) + qtyorderd, 2)

                grandtotal(4) = round(grandtotal(4) + extension, 2)
                grandtotal(3) = round(grandtotal(3) + qtybck   , 2)
                grandtotal(2) = round(grandtotal(2) + qtyship  , 2)
                grandtotal(1) = round(grandtotal(1) + qtyorderd, 2)

        REM *************************************************************~
            * SEE IF WE HIT A NEW CUST.  CODE, IF YES THEN PRINT THE    *~
            * TOTAL FOR THIS CUST.   DROP THRU AND PRINT LINE ITEM      *~
            *************************************************************

                if customer$ = cuscode$ then L18610
                print using L50120
                pageline% = pageline% + 1
                if pageline% > 54 then gosub L60000
                print using L50180, custotal(1), custotal(2), custotal(3),~
                                   custotal(4)
                print using L50120
                pageline% = pageline% + 2

                mat custotal = zer
                customer$ = cuscode$
                prtcuscode$ = cuscode$
                call "DESCRIBE" (#3, cuscode$, cusname$, 0%, f1%(3))
                if f1%(3) = 0 then cusname$ = "CUSTOMER CODE NOT ON FILE"
                savename$ = cusname$
                savecode$ = cuscode$

L18610:         custotal(4) = round(custotal(4) + extension, 2)
                custotal(3) = round(custotal(3) + qtybck   , 2)
                custotal(2) = round(custotal(2) + qtyship  , 2)
                custotal(1) = round(custotal(1) + qtyorderd, 2)

        rem**************************************************************~
           * print the line item of the detail                          *~
           **************************************************************

             if pageline% > 55 and pageline% < 1000%                     ~
                                           then print using L50120
             if pageline% > 55 then gosub L60000
             if pageline% <> 7 then L19100

                cusname$ = savename$
                prtcuscode$ = savecode$

L19100:      print using L50300, prtcuscode$, cusname$, orderdate$,       ~
                   duedate$, sonumber$, store$, qtyorderd$, qtyship$,    ~
                   qtybck$, price$, extension$
                pageline% = pageline% + 1
                currhdrpart$ = currentpart$
                prtcuscode$, cusname$  = " "
                goto L17300

L19170: REM *************************************************************~
            *  PRINT THE TOTALS FOR THE LAST PART AND EXIT THE PROGRAM  *~
            *                                                           *~
            *************************************************************

                print using L50120
                pageline% = pageline% + 1
                if pageline% > 54 then gosub L60000
                print using L50180, custotal(1), custotal(2), custotal(3),~
                                   custotal(4)
                print using L50210
                pageline% = pageline% + 2
                if pageline% > 54 then gosub L60000
                print using L50240, part$, parttotal(1), parttotal(2),    ~
                                   parttotal(3), parttotal(4)
                pageline% = pageline% + 2
                if pageline% > 52 then gosub L60000
                print skip(2)
                print using L50340, grandtotal(1), grandtotal(2),         ~
                                   grandtotal(3), grandtotal(4)
                print using L50380

                call "SETPRNT" ("BCK012", " ", 0%, 1%)
                close printer
                if output$ = "Report" then call "FILEBGON" (#9)
                if output$ = "Report" then L65000
                mat custotal   = zer : mat parttotal = zer
                mat grandtotal = zer : pagenumber% = 0%
                goto show_detail

L30000: REM *************************************************************~
            *    L O A D   B A C K   O R D E R    L I N E   I T E M     *~
            *                                                           *~
            * LOADS THE BACK ORDER LINE ITEM FILE (BCKLINES) AND GETS   *~
            * WHAT WE NEED AND DON'T NEED. YOU CAN'T ALWAYS GET WHAT... *~
            *************************************************************

            get   #4, using L30230, cuscode$, partcode$, partdescr$,      ~
                   invd, opn, pre_inv, unitprice, duedate$, job$,        ~
                   invoicenr$

            if partdescr$ = " " then partdescr$ = "PART NOT ON FILE"

            qtyorderd = opn  + invd
            qtyship   = invd + pre_inv
            qtybck    = opn  - pre_inv

        REM FOR BACK ORDER REPORT, EXTENSION IS CHANGED TO QTYBCK * PRICE
            extension = qtybck * unitprice
            call "DATEFMT" (duedate$)
            call "CONVERT" (unitprice, 2.2, price$)
            call "CONVERT" (extension, 2.2, extension$)
            call "CONVERT" (qtyorderd, 0.2, qtyorderd$)
            call "CONVERT" (qtyship,   0.2, qtyship$)
            call "CONVERT" (qtybck,    0.2, qtybck$)
            return

L30230:     FMT CH(9),                   /* CUSTOMER CODE              */~
                XX(22),                  /* SKIP                       */~
                CH(25),                  /* PART NUMBER                */~
                CH(32),                  /* PART DESCRIPTION           */~
                XX(12),                  /* SKIP                       */~
                PD(14,4),                /* QUANTITY SHIPPED & INVOICED*/~
                PD(14,4),                /* QUANTITY OPEN              */~
                XX(16),                  /* SKIP                       */~
                PD(14,4),                /* QUANTITY PREINVOICED       */~
                PD(14,4),                /* UNIT PRICE OF ITEM         */~
                POS(206),                /* SKIP                       */~
                CH(6),                   /* DUE DATE                   */~
                XX(12),                  /* LOT NUMBER                 */~
                CH(8),                   /* PROJ NUMBER                */~
                XX(6),                   /* DATE PROMISED              */~
                POS(247),                /* SKIP                       */~
                CH(8)                    /* INVOICE NUMBER             */

L40000: REM *************************************************************~
            *       S C R E E N                     D I S P L A Y       *~
            *                                                           *~
            * DUMPS INFO TO SCREEN>                                     *~
            *************************************************************

L40060:     accept                                                       ~
               "Back Log By Part       ", at(1,66),"Today:",             ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(8c)), title$(1)              , ch(79),~
               at (05,02), fac(hex(ac)), hdr1$                  , ch(08),~
               at (05,11), fac(hex(ac)), hdr2$                  , ch(09),~
               at (05,21), fac(hex(ac)), hdr3$                  , ch(10),~
               at (05,32), fac(hex(ac)), hdr4$                  , ch(10),~
               at (05,43), fac(hex(ac)), hdr5$                  , ch(10),~
               at (05,54), fac(hex(ac)), hdr6$                  , ch(10),~
               at (05,65), fac(hex(ac)), hdr7$                  , ch(16),~
                                                                         ~
               at (11,02),                                               ~
                     "  Shown above is the heading for the display mode. ~
        ~The same information",                                           ~
               at (12,02),                                               ~
                     "comes out on the report, in an extended format. Ple~
        ~ase select the",                                                 ~
               at (13,02),                                               ~
                     "desired output device by entering a 'D' (for Displa~
        ~y) or 'P' (for Printer)",                                        ~
               at (14,02),                                               ~
                     "Below. Note that the report can be printed from the~
        ~ display mode screen.",                                          ~
               at (22,02), fac(hex(a4)), inst$(1)               , ch(18),~
               at (22,21), fac(hex(81)), input$                 , ch( 1),~
               at (22,23), fac(hex(a4)), str(inst$(1),21)       , ch(58),~
               at (23,02), fac(hex(8c)), inst$(2)               , ch(79),~
               at (24,02), fac(hex(8c)), inst$(3)               , ch(79),~
                                                                         ~
               keys(hex(000d0f10)),                                      ~
               key (keyhit%)

               if keyhit% <> 13 then L40360
                  call "MANUAL" ("BCKBYPRT")
                  goto L40060

L40360:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40060

L41000: REM *************************************************************~
            *       S C R E E N                     D I S P L A Y       *~
            *                                                           *~
            * DUMPS INFO TO SCREEN>                                     *~
            *************************************************************

            gosub set_pf

L41060:     accept                                                       ~
               "Back Log By Part - Detail Screen", at(1,66),"Today:",    ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(8c)), title$(1)              , ch(79),~
               at (05,02), fac(hex(ac)), hdr1$                  , ch(08),~
               at (05,11), fac(hex(ac)), hdr2$                  , ch(09),~
               at (05,21), fac(hex(ac)), hdr3$                  , ch(10),~
               at (05,32), fac(hex(ac)), hdr4$                  , ch(10),~
               at (05,43), fac(hex(ac)), hdr5$                  , ch(10),~
               at (05,54), fac(hex(ac)), hdr6$                  , ch(10),~
               at (05,65), fac(hex(ac)), hdr7$                  , ch(16),~
                                                                         ~
               at (06,02), fac(hex(8c)), line$(01)              , ch(79),~
               at (07,02), fac(hex(8c)), line$(02)              , ch(79),~
               at (08,02), fac(hex(8c)), line$(03)              , ch(79),~
               at (09,02), fac(hex(8c)), line$(04)              , ch(79),~
               at (10,02), fac(hex(8c)), line$(05)              , ch(79),~
               at (11,02), fac(hex(8c)), line$(06)              , ch(79),~
               at (12,02), fac(hex(8c)), line$(07)              , ch(79),~
               at (13,02), fac(hex(8c)), line$(08)              , ch(79),~
               at (14,02), fac(hex(8c)), line$(09)              , ch(79),~
               at (15,02), fac(hex(8c)), line$(10)              , ch(79),~
               at (16,02), fac(hex(8c)), line$(11)              , ch(79),~
               at (17,02), fac(hex(8c)), line$(12)              , ch(79),~
               at (18,02), fac(hex(8c)), line$(13)              , ch(79),~
               at (19,02), fac(hex(8c)), line$(14)              , ch(79),~
               at (20,02), fac(hex(8c)), line$(15)              , ch(79),~
               at (21,02), fac(hex(8c)), line$(16)              , ch(79),~
               at (22,02), fac(hex(a4)), inst$(1)               , ch(79),~
               at (23,02), fac(hex(8c)), inst$(2)               , ch(79),~
               at (24,02), fac(hex(8c)), inst$(3)               , ch(79),~
                                                                         ~
               keys(keys$),                                              ~
               key (keyhit%)

               if keyhit% <> 13 then L41380
                  call "MANUAL" ("BCKBYPRT")
                  goto L41060

L41380:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41060

        set_pf
            inst$(1) = " "
            inst$(2) = "(1)Re-Select  (3)See Totals   (4)Prev Part  (12)P~
        ~rint Report   (15)Print Screen"
            inst$(3) = "(2)First                      (5)Next Scrn  (13)I~
        ~nstructions   (16)EXIT PROGRAM"
            keys$ = hex(01020304050c0d0f10)

            if line% > 1% then L41530
                str(inst$(2), 31, 12) = " " : str(keys$, 2, 1) = hex(ff)
                str(inst$(3), 01, 08) = " " : str(keys$, 2, 1) = hex(ff)
L41530:     if no_next% = 0% then L41550
                str(inst$(3), 31, 12) = " " : str(keys$, 5, 1) = hex(ff)
L41550:     return

L42000: REM *************************************************************~
            *       S C R E E N      <TOTALS>       D I S P L A Y       *~
            *                                                           *~
            * DUMPS TOTALS TO SCREEN                                    *~
            *************************************************************

L42060:     accept                                                       ~
               "Back Log By Part - Totals Screen", at(1,66),"Today:",    ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (05,02), fac(hex(ac)), hdr8$                  , ch(53),~
               at (05,56), fac(hex(ac)), hdr5$                  , ch(10),~
               at (05,67), fac(hex(ac)), hdr9$                  , ch(14),~
                                                                         ~
               at (06,02), fac(hex(8c)), line$(01)              , ch(79),~
               at (07,02), fac(hex(8c)), line$(02)              , ch(79),~
               at (08,02), fac(hex(8c)), line$(03)              , ch(79),~
               at (09,02), fac(hex(8c)), line$(04)              , ch(79),~
               at (10,02), fac(hex(8c)), line$(05)              , ch(79),~
               at (11,02), fac(hex(8c)), line$(06)              , ch(79),~
               at (12,02), fac(hex(8c)), line$(07)              , ch(79),~
               at (13,02), fac(hex(8c)), line$(08)              , ch(79),~
               at (14,02), fac(hex(8c)), line$(09)              , ch(79),~
               at (15,02), fac(hex(8c)), line$(10)              , ch(79),~
               at (16,02), fac(hex(8c)), line$(11)              , ch(79),~
               at (17,02), fac(hex(8c)), line$(12)              , ch(79),~
               at (18,02), fac(hex(8c)), line$(13)              , ch(79),~
               at (19,02), fac(hex(8c)), line$(14)              , ch(79),~
               at (20,02), fac(hex(8c)), line$(15)              , ch(79),~
               at (21,02), fac(hex(8c)), line$(16)              , ch(79),~
               at (22,02), fac(hex(a4)), inst$(1)               , ch(79),~
               at (23,02), fac(hex(8c)), inst$(2)               , ch(79),~
               at (24,02), fac(hex(8c)), inst$(3)               , ch(79),~
                                                                         ~
               keys(keys$),                                              ~
               key (keyhit%)

               if keyhit% <> 13 then L42940
                  call "MANUAL" ("BCKBYPRT")
                  goto L42060

L42940:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L42060

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *                                                           *~
            * HANDLES ALL THE PRINT FORMATTING. DEVELOPED BY ME, NO     *~
            * THANKS TO "EZPRINT" AND ITS NASTY BUGS (V 1.4)            *~
            *************************************************************

L50070: %#######################################  #######################~
        ~###########################                          BCKBYPRT:BCK~
        ~012
L50080: %                                         Backlog by Part Report ~
        ~(ext shown before cust disc)                             Page:  #~
        ~###
L50100: %  FOR PART: ####################################################~
        ~##################################
L50120: %+---------+--------------------------+--------+--------+--------~
        ~--------+---+----------+----------+----------+----------+--------~
        ~--+
L50150: %!CUSTOMER !       CUSTOMER NAME      !ORD DATE!DUE DATE!SALES OR~
        ~DER NMBR!STR!QTY ORD'D !QTY SHIP'D! QTY BCK'D!UNIT PRICE! EXTENSI~
        ~ON!
L50180: %          !TOTAL FOR CUSTOMER                                   ~
        ~            !-#########!-#########!-#########!       -##,###,###.~
        ~##!
L50210: %          +-----------------------------------------------------~
        ~------------+----------+----------+----------+-------------------~
        ~--+
L50240: %                     TOTAL FOR PART #########################   ~
        ~             -######### -######### -#########        -##,###,###.~
        ~##
L50300: %!#########!##########################!########!########!########~
        ~########!###!##########!##########!##########!##########!########~
        ~##!

L50340: %                     GRAND TOTAL FOR ALL PARTS                  ~
        ~             -######### -######### -#########     -#,###,###,###.~
        ~##

L50380: %                                                                ~
        ~             ========== ========== ==========     ===============~
        ~==

L60000: REM *************************************************************~
            *  P A G E   H E A D I N G / C O N T R O L   R O U T I N E  *~
            *                                                           *~
            * TRACKS WHICH LINE OF THE PAGE WE ARE ON, SKIPS TO NEW PAGE*~
            * AND PRINTS HEADINGS IF WE'RE NOT ABLE TO FIT IT ALL ON ONE*~
            *************************************************************

               print page
               call "DATE" addr ("HD", hdrdate$)
               call "SPCESMSH" (hdrdate$, 2%)
               pagenumber% = pagenumber% + 1
               print using L50070, hdrdate$, company$
               print using L50080, pagenumber%
               print skip (1)
               temppart$ = currentpart$
               if part$ = str(currentpart$,,25) then L60140
                  call "DESCRIBE" (#5, part$, tempdescr$, 1%, f1%(5))
                  if f1%(5) = 0 then tempdescr$ = "(PART NOT ON FILE)"
                  currentpart$ = part$ & " " & tempdescr$
L60140:        print using L50100, currentpart$
               currentpart$ = temppart$
               print using L50120
               print using L50150
               print using L50120
               pageline% = 7
               return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("Closing Files, One Moment Please")
            end
