        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   QQQ    CCC   RRRR   EEEEE  JJJJJ  RRRR   PPPP   TTTTT   *~
            *  Q   Q  C   C  R   R  E        J    R   R  P   P    T     *~
            *  Q   Q  C      RRRR   EEEE     J    RRRR   PPPP     T     *~
            *  Q Q Q  C   C  R   R  E      J J    R   R  P        T     *~
            *   QQQ    CCC   R   R  EEEEE   J     R   R  P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * QCREJRPT - Prints Contents of rejections log with various *~
            *            Selection criteria and sort orders.            *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/18/86 ! Original                                 ! KAB *~
            * 06/12/89 ! Now prints Stkng UOM instead of Vend UOM ! LKM *~
            * 01/13/93 ! Page 0 Facs fix,  & End of Report Time.  ! RJH *~
            * 08/27/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            company$60,                  /* Company Name               */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fromto$(2)25,                /* From/To Messages           */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            part$(2)25,                  /* Part Code                  */~
            part$25,                     /* Part Code                  */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            po$19,                       /* PO Number & Seq.           */~
            print_title$60,              /* Report Title               */~
            quantity$10,                 /* Quantity                   */~
            receiver$16,                 /* Receiver Number            */~
            record$200,                  /* Work Area                  */~
            rejcode$(2)6,                /* Rejection Code             */~
            rejcode$6,                   /* Rejection Code             */~
            rejdescr$30,                 /* Rejection Descr.           */~
            rejdate$(2,2)10,             /* Rejection Date             */~
            rejdate$8,                   /* Rejection Date             */~
            rpt_time$8,                  /* Report Time                */~
            sort$1,                      /* Sort Order                 */~
            sortkey$35,                  /* Sort Key                   */~
            total(2),                    /* Totals                     */~
            uom$4,                       /* Unit of Measure            */~
            vendor$(2)9                  /* Vendor Code                */~

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! RCVQCREJ ! REJECTIONS LOG                           *~
            * #2  ! QCRTYPES ! Stores Q/C rejection codes (reasons for  *~
            * #3  ! WORKFILE ! Work Area                                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "RCVQCREJ",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =   26, keylen =  17,                     ~
                        alt key  1, keypos =    1, keylen =  42          ~

            select #2,  "QCRTYPES",                                      ~
                        varc,     indexed,  recsize =  350,              ~
                        keypos =    1, keylen =   6                      ~

            select #3,  "WORKFILE",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   77                     ~

            select #4,  "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90, keylen = 4, dup,  ~
                                   key 3, keypos = 26, keylen = 32, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "COMPNAME" (12%, company$, f1%(3))
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            fromto$(1) = "From"
            fromto$(2) = "To"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$="(4)Previous Field" : pf5$=" ": pf16$="(16)EXIT PROGRAM"
            init(" ") errormsg$, inpmessage$,                            ~
                      vendor$()                   ,/* Vendor Code      */~
                      part$()                     ,/* Part Code        */~
                      rejcode$()                  ,/* Rejection Code   */~
                      rejdate$()                  ,/* Rejection Date   */~
                      sort$                        /* Sort Order       */

            for fieldnr% = 1 to  5
L10310:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10430
L10330:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10410
L10360:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10330
                         if fieldnr% = 1% then L10310
                         goto L10360
L10410:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10330
L10430:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10330
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            pf4$  = " "
            pf5$  = " "
            pf16$ = "(16)PRINT REPORT"
            inpmessage$ = edtmessage$
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       edtpg1
            fieldnr% = cursor%(1) - 6
            if fieldnr% < 1 or fieldnr% >  5 then edtpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf5$, pf16$ = " "
L11190:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11190
            goto edtpg1

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            gosub print_report
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* Vendor Code      */~
                                    L20200,         /* Part Code        */~
                                    L20300,         /* Rejection Code   */~
                                    L20400,         /* Rejection Date   */~
                                    L20500          /* Sort Order       */
                     return
L20100:     REM Vendor Code                           VENDOR$(1)
                if vendor$(1) = " " and vendor$(2) = " " then            ~
                              vendor$(1) = "ALL"
                inpmessage$ = "Enter Range for Vendors to Print"
                return
L20200:     REM Part Code                             PART$(1)
                if part$(1) = " " and part$(2) = " " then                ~
                              part$(1) = "ALL"
                inpmessage$ = "Enter Range for Part Codes to Print"
                return
L20300:     REM Rejection Code                        REJCODE$(1)
                if rejcode$(1) = " " and rejcode$(2) = " " then          ~
                              rejcode$(1) = "ALL"
                inpmessage$ = "Enter Range for Rejection Codes to Print"
                return
L20400:     REM Rejection Date                        REJDATE$(1)
                if (rejdate$(1%,1%) = " " or  ~
                    rejdate$(1%,1%) = blankdate$) and   ~
                   (rejdate$(2%,1%) = " " or ~
                    rejdate$(2%,1%) = blankdate$) then  ~
                              rejdate$(1%,1%) = "ALL"
                inpmessage$ = "Enter Range for Rejection Dates to Print"
                return
L20500:     REM Sort Order                            SORT$
                inpmessage$ = "Enter Sort Selection From List Above"
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        print_report
            call "SHOSTAT" ("Printing Report")
            rpt_time$ = " ":call "TIME" (rpt_time$)
            mat total = zer
            init (hex(00)) plowkey$
            page% = 0%
            line% = 1000%
            select printer (134)
            call "SETPRNT" ("RCV005", " ", 0%, 0%)

L30075:     call "PLOWNEXT" (#3, plowkey$, 0%, f1%(3))
                if f1%(3) = 0% then L30290
L30085:     if line% > 50% then gosub print_heading
                print skip(1)
                line% = line% + 1%
                goto L30170

L30110:     call "PLOWNEXT" (#3, plowkey$, break1%, f1%(3))
                if f1%(3) <> 0% then L30085
                   if pos("34" = sort$) = 0% then L30075
                      gosub subtotal1
                      goto L30075

L30140:     call "PLOWNEXT" (#3, plowkey$, break2%, f1%(3))
                if f1%(3) <> 0% then L30170
                   if pos("34" = sort$) = 0% then L30110
                      gosub subtotal2
                      goto L30110

L30170:     get #3, using L30180, part$, vendor$, receiver$, po$,         ~
                                 quantity, rejdate$, rejcode$
L30180:         FMT POS(36), CH(25), CH(9), POS(78), CH(16), CH(19),     ~
                    POS(169), PD(14,4), XX(8), XX(4), CH(6), CH(6)

*        Get Stocking unit of measure for printing
            uom$ = " "
            call "READ100" (#4, part$, f1%(4))
            if f1%(4) <> 1 then L30215            /* Better not happen */
               get #4 using L30206, uom$
L30206:        FMT POS(74), CH(4)

L30215:     call "DESCRIBE" (#2, rejcode$, rejdescr$, 0%, f1%(2))
            if f1%(2) = 0% then rejdescr$ = "** NOT ON FILE **"

            call "CONVERT" (quantity, 0.2, quantity$)
            call "DATEFMT" (rejdate$)

            if line% > 51% then gosub print_heading
            print using L30780, vendor$, po$, receiver$, part$, quantity$,~
                               uom$, rejdate$, str(rejcode$,1,4),        ~
                               str(rejcode$,5,2), rejdescr$
            line% = line% + 1%
            total(1) = total(1) + quantity
            total(2) = total(2) + quantity
            goto L30140

L30290:     if line% > 999% then gosub print_heading
            rpt_time$ = " "  :  call "TIME" (rpt_time$)
            print skip(2)
            print "                                   ********** E N D   ~
        ~O F   R E P O R T   @ " & rpt_time$ & " ********** "
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            call "FILEBGON" (#3)
            return

        print_heading
            print page
            if page% > 0% then L30365
               gosub L30370
               gosub print_params
               print page
L30365:     page% = page% + 1%
L30370:     print using L30690, date$, rpt_time$, company$
            print_title$ = "REJECTION ANALYSIS REPORT"
            call "FMTTITLE" (print_title$, " ", 12%)
            print using L30710, print_title$, page%
            print_title$ = " "
            if sort$ = "1" then print_title$ = "BY VENDOR/PO NUMBER"
            if sort$ = "2" then print_title$ = "BY VENDOR/REJECTION CODE"
            if sort$ = "3" then print_title$ = "BY PART/REJECTION CODE"
            if sort$ = "4" then print_title$ = "BY PART/VENDOR"
            call "FMTTITLE" (print_title$, " ", 12%)
            print using L30725, print_title$
            print skip(1)
            if page% = 0% then return
            print using L30745
            print using L30760
            line% = 6%
            return

        print_params
L30462:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L30470
                str(i$(), i%, 1%) = hex(20)
                goto L30462
L30470:     print skip(3)
            print tab(37);
            print "--------------------- Report Selection Parameters ----~
        ~---------"
            for x% = 6% to 15%: print tab(37); i$(x%) : next x%
            print tab(37);
            print "------------------------------------------------------~
        ~---------"
            return

        subtotal1

            call "CONVERT" (total(1), 0.2, quantity$)
            print_title$ = "      ***** TOTAL FOR PART:" & part$
            print using L30810
            print using L30820, print_title$, quantity$
            print skip (1)
            line% = line% + 3%
            total(1) = 0
            return

        subtotal2

            call "CONVERT" (total(2), 0.2, quantity$)
            print_title$ = "***** SUBTOTAL FOR"
            if sort$ = "4" then L30620
               print_title$ = print_title$ & " REJECTION CODE:"
               print_title$ = print_title$ & str(rejcode$,1,4)
               print_title$ = print_title$ & " " & str(rejcode$,5,2)
               goto L30630
L30620:     print_title$ = print_title$ & " VENDOR:" & vendor$

L30630:     print using L30800
            print using L30820, print_title$, quantity$
            line% = line% + 2%
            total(2) = 0
            return

        REM *************************************************************~
            *                R E P O R T   F O R M A T S                *~
            *-----------------------------------------------------------*~
            *  Report format lines used by print statements.            *~
            *************************************************************

L30690: % ######## ########                  ############################~
        ~################################                    QCREJRPT:RCV0~
        ~05

L30710: %                                    ############################~
        ~################################                        PAGE:####

L30725: %                                    ############################~
        ~################################


L30745: %VENDOR    PURCHASE ORDER      RECEIVER         PART NUMBER      ~
        ~           QUANTITY U/M    DATE   REJECTION CODE

L30760: %--------- ------------------- ---------------- -----------------~
        ~-------- ---------- ---- -------- -------------------------------~
        ~--

L30780: %######### ################### ################ #################~
        ~######## ########## #### ######## #### ## #######################~
        ~##

L30800: %                                                                ~
        ~         ----------
L30810: %                                                                ~
        ~         ==========
L30820: %          ######################################################~
        ~######## ##########

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "SHOSTAT" ("Sorting File")
            call "WORKOPEN" (#3, "IO   ", 100%, f2%(3))

            init (hex(00)) plowkey$
            if pos("34" = sort$) <> 0% then L31150
              key% = 0%
              if vendor$(1) = "ALL" then L31180
                 str(plowkey$,,9) = vendor$(1)
                 goto L31180

L31150:     key% = 1%
            if part$(1) = "ALL" then L31180
               str(plowkey$,,25) = part$(1)

L31180:     call "PLOWALTS" (#1, plowkey$, key%, 0%, f1%(1))
               if f1%(1) = 0 then return
            get #1, using L31210, part$, vendor$, rejdate$, rejcode$
L31210:         FMT CH(25), CH(9), POS(154), CH(6), CH(6)

            if part$(1) = "ALL" then L31290
               if part$ < part$(1) then L31180
                  if part$ <= part$(2) then L31290
                     if key% = 1% then return
                        goto L31180

L31290:     if vendor$(1) = "ALL" then L31350
               if vendor$ < vendor$(1) then L31180
                  if vendor$ <= vendor$(2) then L31350
                     if key% = 0% then return
                        goto L31180

L31350:     if rejdate$(1,1) = "ALL" then L31390
               if rejdate$ < rejdate$(1,2) then L31180
                  if rejdate$ > rejdate$(2,2) then L31180

L31390:     if rejcode$(1) = "ALL" then L31430
               if rejcode$ < rejcode$(1) then L31180
                  if rejcode$ > rejcode$(2) then L31180

L31430:     get #1, using L31440, record$
L31440:         FMT CH(165)

            sortkey$ = " "
            if pos("34" = sort$) <> 0% then L31530
               break1% = 9%
               str(sortkey$,,9) = str(vendor$,,9)
               if sort$ = "1" then str(sortkey$,10) = str(record$,59,19) ~
                              else str(sortkey$,10) = rejcode$
               if sort$ = "1" then break2% = 25% else break2% = 15%
               goto L31570

L31530:        break1% = 25%
               str(sortkey$,,25) = str(part$,,25)
               if sort$ = "3" then str(sortkey$,26) = rejcode$           ~
                              else str(sortkey$,26) = vendor$
               if sort$ = "3" then break2% = 31% else break2% = 34%

L31570:     write #3, using L31580, sortkey$, record$
L31580:           FMT CH(35), CH(165)
            goto L31180

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  str(line2$,62%) = "QCREJRPT: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40220,         /* Vendor Code      */~
                                    L40220,         /* Part Code        */~
                                    L40220,         /* Rejection Code   */~
                                    L40220,         /* Rejection Date   */~
                                    L40220          /* Sort Order       */
                  if errormsg$ > " " and fieldnr% > 0% then              ~
                     lfac$(fieldnr%) = or hex(10)
                  goto L40290

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40220:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40290:     accept                                                       ~
               at (01,02),                                               ~
                  "Rejection Analysis Report",                           ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,20), fac(hex(ac)), fromto$(1)             , ch(25),~
               at (06,46), fac(hex(ac)), fromto$(2)             , ch(25),~
                                                                         ~
               at (07,02), "Vendor Code",                                ~
               at (07,20), fac(lfac$( 1)), vendor$(1)           , ch(09),~
               at (07,46), fac(lfac$( 1)), vendor$(2)           , ch(09),~
                                                                         ~
               at (08,02), "Part Code",                                  ~
               at (08,20), fac(lfac$( 2)), part$(1)             , ch(25),~
               at (08,46), fac(lfac$( 2)), part$(2)             , ch(25),~
                                                                         ~
               at (09,02), "Rejection Code",                             ~
               at (09,20), fac(lfac$( 3)), str(rejcode$(1),1,4) , ch(04),~
               at (09,25), fac(lfac$( 3)), str(rejcode$(1),5,2) , ch(02),~
               at (09,46), fac(lfac$( 3)), str(rejcode$(2),1,4) , ch(04),~
               at (09,51), fac(lfac$( 3)), str(rejcode$(2),5,2) , ch(02),~
                                                                         ~
               at (10,02), "Rejection Date",                             ~
               at (10,20), fac(lfac$( 4)), rejdate$(1,1)        , ch(10),~
               at (10,46), fac(lfac$( 4)), rejdate$(2,1)        , ch(10),~
                                                                         ~
               at (11,02), "Sort Order",                                 ~
               at (11,20), fac(lfac$( 5)), sort$                , ch(01),~
               at (12,05), "1)By Vendor/P.O",                            ~
               at (13,05), "2)By Vendor/Rejection Code",                 ~
               at (14,05), "3)By Part/Rejection Code",                   ~
               at (15,05), "4)By Part/Vendor",                           ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(000104050d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L40690
                  call "MANUAL" ("QCREJRPT")
                  goto L40290

L40690:        if keyhit% <> 15 then L40730
                  call "PRNTSCRN"
                  goto L40290

L40730:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* Vendor Code      */~
                                    L50200,         /* Part Code        */~
                                    L50300,         /* Rejection Code   */~
                                    L50400,         /* Rejection Date   */~
                                    L50500          /* Sort Order       */
                  return
L50100:     REM Vendor Code                           VENDOR$(1)
                if vendor$(1) <> "ALL" then L50140
                   vendor$(2)  = " "
                   return
L50140:         if vendor$(2) = " " then vendor$(2) = vendor$(1)
                if vendor$(1) > vendor$(2) then                          ~
                   errormsg$ = "Invalid Vendor Range"
                   return
L50200:     REM Part Code                             PART$(1)
                if part$(1) <> "ALL" then L50240
                   part$(2)  = " "
                   return
L50240:         if part$(2) = " " then part$(2) = part$(1)
                if part$(1) > part$(2) then                              ~
                   errormsg$ = "Invalid Part Range"
                   return
L50300:     REM Rejection Code                        REJCODE$(1)
                if rejcode$(1) <> "ALL" then L50340
                   rejcode$(2)  = " "
                   return
L50340:         if rejcode$(2) = " " then rejcode$(2) = rejcode$(1)
                if rejcode$(1) > rejcode$(2) then                        ~
                   errormsg$ = "Invalid Rejection Code Range"
                   return
L50400:     REM Rejection Date                        REJDATE$(1)
                if rejdate$(1%,1%) <> "ALL" then L50410
                   rejdate$(2%,1%)  = " "
                   return
L50410:         call "DATEOKC" (rejdate$(1%,1%), temp, errormsg$)
                   if errormsg$ <> " " then return
                if rejdate$(2%,1%) = " " or rejdate$(2%,1%) = blankdate$ ~
                     then rejdate$(2%,1%) = rejdate$(1%,1%)
                call "DATEOKC" (rejdate$(2%,1%), temp, errormsg$)
                   if errormsg$ <> " " then return
                rejdate$(1%,2%) = rejdate$(1%,1%)
                rejdate$(2%,2%) = rejdate$(2%,1%)
                call "DATUFMTC" (rejdate$(1%,2%))
                call "DATUFMTC" (rejdate$(2%,2%))
                if rejdate$(1%,2%) > rejdate$(2%,2%) then                    ~
                   errormsg$ = "Invalid Date Range"
                   return
L50500:     REM Sort Order                            SORT$
                if pos("1234" = sort$) <> 0% then return
                errormsg$ = "Choose Sort Order Between 1 & 4"
                return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
