        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS   EEEEE  TTTTT  L      TTTTT                        *~
            *  S      E        T    L        T                          *~
            *   SSS   EEEE     T    L        T                          *~
            *      S  E        T    L        T                          *~
            *   SSS   EEEEE    T    LLLLL    T                          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SETLT    - ESTABLISHES WHAT THE PURCHASE LEADTIME IS TO   *~
            *            BE USED ANY GIVEN DAY IN THE PLANNING CALENDAR.*~
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
            * 02/09/84 ! ORIGINAL                                 ! HES *~
            * 04/07/87 ! HNYMASTR file changes                    ! ERN *~
            * 08/30/96 ! Millie date conversion                   ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankline$79,                /* FOR SCREEN DISPLAY         */~
            ldtm$(490)3,                 /* LEADTIMES IN EFFECT        */~
            ldtmfile$(490)3,             /* ON-OFF FILE TRANSLATION    */~
            ldt$3,                       /* LEADTIME FOR SET           */~
            calccyymmdd$(490)8,          /* cal date of ccyymmdd       */~
            calcheck$8,                  /* calendar date              */~
            calend$8,                    /* END OF PLANNING CALENDAR   */~
            calstart$8,                  /* START OF CALENDAR          */~
            ccyymmdd$8,                  /* ccyymmdd                   */~
            curldt$3,                    /* LEADTIME IN HNYMASTR       */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dd%(490),                    /* PRODUCTION CALENDAR        */~
            dd$(31)3,                    /* DOW FOR DAY TO DAY DISPLAY */~
            descr$(2)18,                 /* FULL DATE DESCRIPTION      */~
            dfmt$1,                      /* DATE FORMAT FROM GENEDIT   */~
            dow$(490)3,                  /* PRODUCTION CALENDAR        */~
            dprompt$7,                   /* DATE PROMPT                */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            end$8,                       /* END DATE FOR SET           */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            feb29$4,                     /* test for feb. 29           */~
            hnyldt$10,                   /* LEADTIME IN HNYMASTR       */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lfac$(2)1,                   /* FIELD ATTRIBUTE CHARACTERS */~
            line$(100)75,                /* SCREEN                     */~
            line2$79,                                                    ~
            mar01$4,                     /* test for mar. 1            */~
            mm%(490),                    /* PRODUCTION CALENDAR        */~
            month$(12)9,                 /* CALENDAR MONTHS DESCRIPTION*/~
            part$25,                     /* ENTER DESIRED PART CODE    */~
            partdescr$32,                /* ENTER DESIRED PART CODE    */~
            pfkey4$13,                   /* PREVIUOS SCREEN KEY        */~
            pfkey5$13,                   /* NEXT SCREEN KEY            */~
            seeldt$(31)3,                /* DAY DISPLAY                */~
            start$8,                     /* START DATE FOR SET         */~
            str_dy$2,                    /* string day (DD)            */~
            str_mn$2,                    /* string month (MM)          */~
            str_yr$4,                    /* string year (CCYY)         */~
            temp$8,                      /* temp date string           */~
            test$8,                      /* test date string           */~
            title$75,                    /* FOR SCREEN DISPLAY         */~
            yy%(490),                    /* PRODUCTION CALENDAR        */~
            yymmdd$(490)6                /* PRODUCTION CALENDAR        */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * #01 ! ENGMASTR ! Engineering Master Filer                 *~
            * #04 ! HNYMASTR ! Inventory Master File                    *~
            * #05 ! CALMASTR ! Planning Production Calendar File        *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "ENGMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos =    1, keylen =  29                      ~

            select #04, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key 1,  keypos = 102,  keylen =  9, dup,     ~
                            key 2,  keypos = 90 ,  keylen =  4, dup

            select #05, "CALMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos =    1, keylen =   2                      ~

            call "SHOSTAT" ("Linking to data base to manage Purchase Lead~
        ~time effectivity dates")

            call "OPENFILE" (#01, "SHARE", f2%(01), rslt$(01), axd$(01))
            call "OPENFILE" (#04, "SHARE", f2%(04), rslt$(04), axd$(04))
            call "OPENFILE" (#05, "SHARE", f2%(05), rslt$(05), axd$(05))
            if f2%(4) + f2%(5) <> 0 then L65000

            if f2%(1) = 0 then L09000
            call "OPENFILE" (#01, "OUTPT", f2%(01), rslt$(01), axd$(01))
            close #1
            call "OPENFILE" (#01, "SHARE", f2%(01), rslt$(01), axd$(01))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$, ccyymmdd%, ccyymmdd$)
            str_yr$ = str( ccyymmdd$, 1%, 4% )
            feb29$ = "0229"
            mar01$ = "0301"
            gosub load_calendar

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (RETURN)."

            title$ = "LEADTIME (DAYS)    START DATE                    EN~
        ~D DATE"

            month$(01) = "January  "
            month$(02) = "February "
            month$(03) = "March    "
            month$(04) = "April    "
            month$(05) = "May      "
            month$(06) = "June     "
            month$(07) = "July     "
            month$(08) = "August   "
            month$(09) = "September"
            month$(10) = "October  "
            month$(11) = "November "
            month$(12) = "December "

            call "EXTDTFMT" addr(dfmt$)

            if dfmt$ = "E" then dprompt$ = "(DD/MM)"                     ~
                           else dprompt$ = "(MM/DD)"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, part$, partdescr$, ldt$,   ~
                      ldtm$(), line$(), start$, end$
            if dfmt$ = "E" then end$ = "31/12" else end$ = "12/31"
            start$ = "01/01"

            for fieldnr% = 1 to  1
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10210
L10150:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10150
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10150
L10210:     next fieldnr%
            gosub L30000
            if str(errormsg$,,7) = "WARNING" then home% = 0

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

            l% = home%
L11070:     gosub L41000
            errormsg$ = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then l% = 0
                  if keyhit%  =  4 then l% = max(0, l% - 6)
                  if keyhit%  =  5 then l% = min(max(0, maxlines%-6),    ~
                                                                  l% + 6)
                  if keyhit%  =  9 then gosub see_by_day
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 17 then gosub set_effective
            goto L11070

        set_effective
            convert ldt$ to u3%, data goto L12050
            convert u3% to ldt$, pic(###)
            convert ldt$ to u3%, data goto L12050
            goto L12080
L12050:         errormsg$="Invalid Entry for Leadtime: " & ldt$
                return

L12080:     gosub'255 (start$, start%)
            if errormsg$ <> " " then return
            start% = test%

            gosub'255 (end$, end%)
            if errormsg$ <> " " then return
            end% = test%

            print at (24,1,80); hex(84);                                 ~
            "                                Calculating..."

*       ALL OK, LETS DO IT TO IT

            if start% > end% then L12440
            for i% = start% to end%
               ldtm$(i%) = ldt$
            next i%
            goto L12480
L12440:
            for i% = start% to 365%
               ldtm$(i%) = ldt$
            next i%
            for i% = 1% to end%
               ldtm$(i%) = ldt$
            next i%

L12480:        gosub encode_lead_time
               gosub decode_lead_time
               gosub format_screen
               l% = home%
            return

        deffn'255 (test$, test%)
           /* break apart month and day appropriately */
            if dfmt$ = "E" then str_dy$ = str( test$, 1%, 2% ) ~
                           else str_dy$ = str( test$, 4%, 2% )
            if dfmt$ = "E" then str_mn$ = str( test$, 4%, 2% ) ~
                           else str_mn$ = str( test$, 1%, 2% )

            /* get rid of leap year if entered */
            convert str_mn$ to num_mn%
            convert str_dy$ to num_dy%
            if num_mn% <> 2% and num_dy% <> 29% then make_a_dt
            str_mn$ = "03"
            str_dy$ = "01"

make_a_dt
            temp$ = str_yr$ & str_mn$ & str_dy$
            call "DATECONV" ( temp$ )

L13170:     call "DATEOK" (temp$, u3%, errormsg$)
            if errormsg$ <> " " then return

            call "DATUNFMT" (temp$, ccyymmdd%, calcheck$)

            search str(calccyymmdd$(), 5%) = str( calcheck$, 5%, 4%) ~
                   to cursor%() step 8%

            if cursor%(1%) <> 0% then L13320
               errormsg$ = "Date Could NOT be Found in the Production Calendar"
            return

L13320:     test% = (cursor%(1%)/8%)
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            x% = 0
            call "READ101" (#1, str(part$,,25) & "2001", f1%(1))
                if f1%(1) = 1 then delete #1

            curldt$ = ldtm$(1)
            for i% = 1 to 490
                if ldtm$(i%) = curldt$ then L19150
                x% = 1 : i% = 490
L19150:     next i%

            if x% = 0 then L19370 /*Don't save if all same, update HNYMST*/

            gosub encode_lead_time

            write #1, using L19190, part$, "2001", ldtmfile$()
L19190:     FMT CH(25), CH(4), 490*CH(3)
            goto inputmode

*              Update HNYMASTR (conditionally)
*              Note that this logic is dependent upon logic in HNYINPUT.
*              If the same leadtime is in effect for the whole planning
*              calendar, then the leadtime is stored it the HNYMASTR
*              record only.  HNYINPUT assumes this. In HNYINPUT, the
*              dataload section sets the leadtime field to whatever is in
*              effect today. If nothing is in effect, it assumes that the
*              leadtime already in the HNYMASTR record is the current
*              leadtime. !!Any program that needs to determine the lead-
*              time for any given day must follow the same proceedure.!!
*                        This process serves one purpose:
*              allow HNYMASTR to carry the leadtime if it is not planned
*              to change, therefore reducing the number of (BIG) records
*              written from here.

L19370:     call "READ101" (#4, part$, f1%(4))
            if f1%(4) = 0 then inputmode

            put #4 using L19410, " ", curldt$
L19410:         FMT POS(170), CH(7), POS(177), CH(3)
            rewrite #4
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
        REM DEFAULT/ENABLE FOR ENTER DESIRED PART CODE
            enabled% = 1
            inpmessage$ = "Enter the part number you wish to see effect"&~
                "ivity dates for"
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

L29920:     u3% = 2%
            call "STARTOVR" (u3%)

            if u3% = 1% then return
            if u3% <> 0% then L29920
               return clear
               goto inputmode

L30000: REM *************************************************************~
            *            L O A D    E N G I N E E R I N G               *~
            *                        D A T A                            *~
            * AND FORMAT THE MAIN SCREEN.                               *~
            *************************************************************

           display at(12,27), "Loading Effectivity Dates"

           call "READ100" (#1, str(part$,,25) & "2001", f1%(1))
                     if f1%(1) <> 1 then goto L30300

           get #1, using L30120,   ldtmfile$()
L30120:    FMT XX(29), 490*CH(3)

            gosub decode_lead_time

        format_screen  /* here if record on file or changed during edit */
           l%, x% = 1
           line$() = " "        /* format LINE$() into a summary of */
           for i% = 2 to 491            /* change activity */
            if i% = 491 then L30200
            if ldtm$(i%) = ldtm$(x%) then L30250
L30200:         gosub'99(x%, i%-1)
                put line$(l%), using L30490, ldtm$(x%), descr$(1),        ~
                                                       descr$(2)
                x% = i%
                l% = min(l%+1, 100)
L30250:    next i%
           str(line$(home%+1),68,8) = hex(84038c) & "Today"
           str(line$(home%+1),2,8) =  "Today" & hex(84028c)
           maxlines% = l% - 1
        return

L30300
*       * NOT ON FILE, DEFAULT ASSUMED *********************************
           l%, x% = 1
           gosub'99 (1%, 490%)
           put line$(1), using L30490, curldt$, descr$(1), descr$(2)
           line$(4) = hex(84) & "Please Note The Following:"

           line$(5) = "This part currently does not have any effectivity ~
        ~dates established."
           line$(6) = "Unless otherwise set, it will be assumed that the ~
        ~leadtime set in the"
           line$(7) = "Inventory Master File (currently" & curldt$ & ") i~
        ~s to always be used for this part."
           line$(9) = "This is done to reduce disk space requirements."
           for i% = 1 to 490 : ldtm$(i%) = curldt$ : next i%
           maxlines% = 1
        return


L30490: %           ###     ##################            ###############~
        ~###

        deffn'99 (start%, end%)   /* FORMAT DATE RANGE */
            if today% >= start% and today% <= end% then home% = l% - 1
            descr$(1) = month$(mm%(start%))
            descr$(2) = month$(mm%(end%))
            put str(descr$(1), len(descr$(1)) + 2), using L30610,         ~
                               dd%(start%), yy%(start%)
            put str(descr$(2), len(descr$(2)) + 2), using L30610,         ~
                               dd%(end%), yy%(end%)
        return
L30610: %##, ####

        rem**************************************************************~
            *      l o a d   c a l m a s t r    d a t a                 *~
            *                                                           *~
            * loads and formats the production calendar.                *~
            *************************************************************

        load_calendar

            print at (22,25);hex(84);"Loading Production Calendar";hex(8c)
            call "READ100" (#5,  "10", f1%(5))
                if f1%(5) = 0 then bad_calendar
            get #5, using L33110, str(yymmdd$(),1,1470)
L33110:         FMT XX(2), CH(1470)

            call "READ100" (#5,  "11", f1%(5))
                if f1%(5) = 0 then bad_calendar
            get #5, using L33160, str(yymmdd$(),1471,1470)
L33160:         FMT XX(2), CH(1470)

            call "READ100" (#5,  "20", f1%(5))
                if f1%(5) = 0 then bad_calendar
            get #5, using L33210, yy%()
L33210:         FMT XX(2), 490*BI(4)

            call "READ100" (#5,  "30", f1%(5))
                if f1%(5) = 0 then bad_calendar
            get #5, using L33260, mm%()
L33260:         FMT XX(2), 490*BI(4)

            call "READ100" (#5,  "40", f1%(5))
                if f1%(5) = 0 then bad_calendar
            get #5, using L33310, dd%()
L33310:         FMT XX(2), 490*BI(4)

            call "READ100" (#5,  "50", f1%(5))
                if f1%(5) = 0 then bad_calendar
            get #5, using L33360, dow$()
L33360:         FMT XX(2), 490*CH(3)

            pltbase% = 0%
            today%   = 0%
            for i% = 1% to 490%
               temp$ = yymmdd$(i%)
               call "DATEFMT" ( temp$, temp%, calccyymmdd$(i%) )
               if str( calccyymmdd$(i%), 5%, 4% ) = mar01$ and ~
                  pltbase% = 0% then pltbase% = i%
               if yymmdd$(i%) <> date then L33430
                  today% = i%
L33430:     next i%

            if today% = 0% or pltbase% = 0% then bad_calendar

            calstart$ = yymmdd$(1)
            calend$   = yymmdd$(490)
            call "DATEFMT" (calstart$)
            call "DATEFMT" (calend$)
           return

        bad_calendar
            u3% = 0% /* Window in center of screen */
            call "ASKUSER" (u3%, "*** CALENDAR ERROR ***",               ~
                "The Production calendar is invalid", " ",               ~
                "Press (ENTER) to terminate this program")
            goto L65000

        REM *************************************************************~
            *   ENCODE CALENDAR INFO FROM OPERATOR TO DISK              *~
            *************************************************************
        encode_lead_time


            if pltbase% = 1% then L39100
                for i% = 1% to pltbase% - 1%
                ldtmfile$(i% - pltbase% + 366%) = ldtm$(i%)
                next i%

L39100:         for i% = pltbase% to 365%
                ldtmfile$(i% - pltbase% + 1%) = ldtm$(i%)
                next i%

                for i% = 366% to 490%
                ldtmfile$(i%) = ldtmfile$(i% - 365%)
                next i%

                return

        REM *************************************************************~
            *   DECODE CALENDAR INFO FROM DISK TO OPERATOR              *~
            *************************************************************
        decode_lead_time

            for i% = 1% to 490%
                ldtm$(i%)=ldtmfile$(mod(i%-pltbase%,365%)+1%)
            next i%
            return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  line2$ = "Screen #1"
                  str(line2$, 65) = "SETLT: " & str(cms2v$,,8)
                  on fieldnr% gosub L40140          /* PART NUMBER      */
                     goto L40210

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40140:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40210:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Purchase Leadtime Effectivity dates",          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Part Code:",                                 ~
               at (06,20), fac(lfac$( 1)), part$                , ch(25),~
               at (06,47), fac(hex(8c)),   partdescr$           , ch(32),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40530
                  call "MANUAL" ("SETLT")
                  goto L40210

L40530:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40210

L41000: REM *************************************************************~
            *      S H O W   S U M M A R Y  /  A L L O W   S E T        *~
            *                                                           *~
            * MAIN SCREEN.                                              *~
            *************************************************************
            pfkey4$, pfkey5$ = " "
            if l% <> 0 then pfkey4$ = "(4)Prev dates"
            if line$(l%+11) <> " " then pfkey5$ = "(5)Next dates"
            line2$ = "Screen #2"
            str(line2$, 65) = "SETLT: " & str(cms2v$,,8)

L41090: accept                                                           ~
               at (01,02),                                               ~
                  "Manage Purchase Leadtime Effectivity dates",          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02),                                               ~
        " Below are the CURRENT/PAST/FUTURE leadtimes. Please use caution~
        ~ when changing.",                                                ~
               at (05,02),                                               ~
        "   (Use the PF KEYS defined below to review portions of structur~
        ~e not shown)   ",                                                ~
               at (06,04), fac(hex(84)), part$                  , ch(25),~
               at (06,30), fac(hex(84)), partdescr$             , ch(32),~
               at (06,64), "Date :",                                     ~
               at (06,71), fac(hex(8c)), date$                  , ch(08),~
               at (07,04), fac(hex(ac)), title$                 , ch(75),~
                                                                         ~
               at (08,04), fac(hex(8c)), line$(l% + 01)         , ch(75),~
               at (09,04), fac(hex(8c)), line$(l% + 02)         , ch(75),~
               at (10,04), fac(hex(8c)), line$(l% + 03)         , ch(75),~
               at (11,04), fac(hex(8c)), line$(l% + 04)         , ch(75),~
               at (12,04), fac(hex(8c)), line$(l% + 05)         , ch(75),~
               at (13,04), fac(hex(8c)), line$(l% + 06)         , ch(75),~
               at (14,04), fac(hex(8c)), line$(l% + 07)         , ch(75),~
               at (15,04), fac(hex(8c)), line$(l% + 08)         , ch(75),~
               at (16,04), fac(hex(8c)), line$(l% + 09)         , ch(75),~
               at (17,04), fac(hex(8c)), line$(l% + 10)         , ch(75),~
               at (08,43), "Thru...",                                    ~
                                                                         ~
               at (19,02), "   The Current Prod. Calendar Starts on",    ~
               at (19,42), fac(hex(84)), calstart$              , ch(08),~
               at (19,51), "And Continues Thru",                         ~
               at (19,70), fac(hex(84)), calend$                , ch(08),~
               at (20,02), fac(hex(94)), errormsg$              , ch(79),~
               at (21,02), fac(hex(ac)), blankline$             , ch(79),~
               at (22,02), "(1)Start over",                              ~
               at (22,17), fac(hex(84)), pfkey4$                , ch(13),~
               at (22,32), fac(hex(84)), pfkey5$                , ch(13),~
               at (22,47), "(13)Instructions  (15)Print Screen",         ~
               at (23,02),                                               ~
        "(2)First Effectivity dates      (9)Show Leadtimes by day       (~
        ~16)Save Changes",                                                ~
               at (24,02), "(17) Set leadtime",                          ~
               at (24,20), ldt$                                 , ch(03),~
               at (24,24), "to be in effect from (MM/DD)",               ~
               at (24,45), fac(hex(8c)), dprompt$               , ch(07),~
               at (24,53), start$                               , ch(05),~
               at (24,59), "thru (MM/DD)",                               ~
               at (24,64), fac(hex(8c)), dprompt$               , ch(07),~
               at (24,72), end$                                 , ch(05),~
                                                                         ~
               keys(hex(01020405090d0f1011)),                            ~
               key (keyhit%)

               if keyhit% <> 13 then L41850
                  call "MANUAL" ("SETLT")
                  goto L41090

L41850:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41090

        REM *************************************************************~
           *    S E E   C A P A C I T I E S   B Y   M O N T H           *~
           **************************************************************

        see_by_day

L42060:     convert str(ccyymmdd$, 5%, 2%) to mo%
            convert str(ccyymmdd$, 1%, 4%) to yr%
L42080:     f% = 0%
            for i% = 1 to 490
                if yy%(i%) < yr% then L42150
                if yy%(i%) > yr% then goto L42160
                if mm%(i%) < mo% then L42150
                if mm%(i%) > mo% then goto L42160
                if f% =  0 then f% = i%
L42150:     next i%
L42160:     ld% = i% - 1

            j% = 0
            init(" ")  dd$(), seeldt$()

            for i% = f% to ld%
                j% = j% + 1
                seeldt$(j%) = ldtm$(i%)
                dd$(j%) = dow$(i%)
            next i%
            line2$ = "Screen #3"
            str(line2$, 65) = "SETLT: " & str(cms2v$,,8)

L42290: accept                                                           ~
               at (01,02),                                               ~
                  "Manage Purchase Leadtime Effectivity dates",          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), "Leadtimes Effective: ",                      ~
               at (03,23), fac(hex(84)), part$                  , ch(25),~
               at (03,49), "Month of:",                                  ~
               at (03,58), fac(hex(84)), month$(mo%)            , ch(09),~
               at (03,68), fac(hex(84)), yr%, pic(####),                   ~
               at(04,16), fac(hex(84)), partdescr$, ch(32),              ~
               at (05,03),                                               ~
        "DAY       LEADTIME                    ! DAY        LEADTIME     ~
        ~    ",                                                           ~
               at (06,03), "01", at (06,41), "!  16",                    ~
               at (07,03), "02", at (07,41), "!  17",                    ~
               at (08,03), "03", at (08,41), "!  18",                    ~
               at (09,03), "04", at (09,41), "!  19",                    ~
               at (10,03), "05", at (10,41), "!  20",                    ~
               at (11,03), "06", at (11,41), "!  21",                    ~
               at (12,03), "07", at (12,41), "!  22",                    ~
               at (13,03), "08", at (13,41), "!  23",                    ~
               at (14,03), "09", at (14,41), "!  24",                    ~
               at (15,03), "10", at (15,41), "!  25",                    ~
               at (16,03), "11", at (16,41), "!  26",                    ~
               at (17,03), "12", at (17,41), "!  27",                    ~
               at (18,03), "13", at (18,41), "!  28",                    ~
               at (19,03), "14", at (19,41), "!  29",                    ~
               at (20,03), "15", at (20,41), "!  30",                    ~
                                 at (21,41), "!  31",                    ~
                                                                         ~
           at(06,13), fac(hex(84)), seeldt$(01)             ,ch(03)     ,~
           at(07,13), fac(hex(84)), seeldt$(02)             ,ch(03)     ,~
           at(08,13), fac(hex(84)), seeldt$(03)             ,ch(03)     ,~
           at(09,13), fac(hex(84)), seeldt$(04)             ,ch(03)     ,~
           at(10,13), fac(hex(84)), seeldt$(05)             ,ch(03)     ,~
           at(11,13), fac(hex(84)), seeldt$(06)             ,ch(03)     ,~
           at(12,13), fac(hex(84)), seeldt$(07)             ,ch(03)     ,~
           at(13,13), fac(hex(84)), seeldt$(08)             ,ch(03)     ,~
           at(14,13), fac(hex(84)), seeldt$(09)             ,ch(03)     ,~
           at(15,13), fac(hex(84)), seeldt$(10)             ,ch(03)     ,~
           at(16,13), fac(hex(84)), seeldt$(11)             ,ch(03)     ,~
           at(17,13), fac(hex(84)), seeldt$(12)             ,ch(03)     ,~
           at(18,13), fac(hex(84)), seeldt$(13)             ,ch(03)     ,~
           at(19,13), fac(hex(84)), seeldt$(14)             ,ch(03)     ,~
           at(20,13), fac(hex(84)), seeldt$(15)             ,ch(03)     ,~
           at(06,54), fac(hex(84)), seeldt$(16)             ,ch(03)     ,~
           at(07,54), fac(hex(84)), seeldt$(17)             ,ch(03)     ,~
           at(08,54), fac(hex(84)), seeldt$(18)             ,ch(03)     ,~
           at(09,54), fac(hex(84)), seeldt$(19)             ,ch(03)     ,~
           at(10,54), fac(hex(84)), seeldt$(20)             ,ch(03)     ,~
           at(11,54), fac(hex(84)), seeldt$(21)             ,ch(03)     ,~
           at(12,54), fac(hex(84)), seeldt$(22)             ,ch(03)     ,~
           at(13,54), fac(hex(84)), seeldt$(23)             ,ch(03)     ,~
           at(14,54), fac(hex(84)), seeldt$(24)             ,ch(03)     ,~
           at(15,54), fac(hex(84)), seeldt$(25)             ,ch(03)     ,~
           at(16,54), fac(hex(84)), seeldt$(26)             ,ch(03)     ,~
           at(17,54), fac(hex(84)), seeldt$(27)             ,ch(03)     ,~
           at(18,54), fac(hex(84)), seeldt$(28)             ,ch(03)     ,~
           at(19,54), fac(hex(84)), seeldt$(29)             ,ch(03)     ,~
           at(20,54), fac(hex(84)), seeldt$(30)             ,ch(03)     ,~
           at(21,54), fac(hex(84)), seeldt$(31)             ,ch(03)     ,~
                                                                         ~
            at(06,06), fac(hex(84)),  dd$(01)               ,ch(03)     ,~
            at(07,06), fac(hex(84)),  dd$(02)               ,ch(03)     ,~
            at(08,06), fac(hex(84)),  dd$(03)               ,ch(03)     ,~
            at(09,06), fac(hex(84)),  dd$(04)               ,ch(03)     ,~
            at(10,06), fac(hex(84)),  dd$(05)               ,ch(03)     ,~
            at(11,06), fac(hex(84)),  dd$(06)               ,ch(03)     ,~
            at(12,06), fac(hex(84)),  dd$(07)               ,ch(03)     ,~
            at(13,06), fac(hex(84)),  dd$(08)               ,ch(03)     ,~
            at(14,06), fac(hex(84)),  dd$(09)               ,ch(03)     ,~
            at(15,06), fac(hex(84)),  dd$(10)               ,ch(03)     ,~
            at(16,06), fac(hex(84)),  dd$(11)               ,ch(03)     ,~
            at(17,06), fac(hex(84)),  dd$(12)               ,ch(03)     ,~
            at(18,06), fac(hex(84)),  dd$(13)               ,ch(03)     ,~
            at(19,06), fac(hex(84)),  dd$(14)               ,ch(03)     ,~
            at(20,06), fac(hex(84)),  dd$(15)               ,ch(03)     ,~
            at(06,47), fac(hex(84)),  dd$(16)               ,ch(03)     ,~
            at(07,47), fac(hex(84)),  dd$(17)               ,ch(03)     ,~
            at(08,47), fac(hex(84)),  dd$(18)               ,ch(03)     ,~
            at(09,47), fac(hex(84)),  dd$(19)               ,ch(03)     ,~
            at(10,47), fac(hex(84)),  dd$(20)               ,ch(03)     ,~
            at(11,47), fac(hex(84)),  dd$(21)               ,ch(03)     ,~
            at(12,47), fac(hex(84)),  dd$(22)               ,ch(03)     ,~
            at(13,47), fac(hex(84)),  dd$(23)               ,ch(03)     ,~
            at(14,47), fac(hex(84)),  dd$(24)               ,ch(03)     ,~
            at(15,47), fac(hex(84)),  dd$(25)               ,ch(03)     ,~
            at(16,47), fac(hex(84)),  dd$(26)               ,ch(03)     ,~
            at(17,47), fac(hex(84)),  dd$(27)               ,ch(03)     ,~
            at(18,47), fac(hex(84)),  dd$(28)               ,ch(03)     ,~
            at(19,47), fac(hex(84)),  dd$(29)               ,ch(03)     ,~
            at(20,47), fac(hex(84)),  dd$(30)               ,ch(03)     ,~
            at(21,47), fac(hex(84)),  dd$(31)               ,ch(03)     ,~
                                                                         ~
            at (22,02), fac(hex(a4)), blankline$            ,ch(79)     ,~
        at (23,02), "(2)First Month   (4)Prev Month    (13)Instructions",~
        at (24,02), "                 (5)Next Month",                    ~
        at (23,65), "(15)Print Screen",                                  ~
        at (24,65), "(16)Return",                                        ~
                                                                         ~
                keys(hex(000204050d0f10)),                               ~
                key(keypressed%)

            if keypressed% = 16 then return
            if keypressed% = 0  then return
            if keypressed% = 2  then goto L42060

            if keypressed% <> 4 then goto L43730
                if f% = 1 then goto L42290
                mo% = mm%(f%-1)
                yr% = yy%(f%-1)
                goto L42080

L43730:     if keypressed% <> 5 then goto L43790
                if ld% = 490 then goto L42290
                mo% = mm%(ld%+1)
                yr% = yy%(ld%+1)
                goto L42080

L43790:     if keypressed% <> 13 then goto L43830
                call "MANUAL" ("SETLT")
                goto L42290

L43830:     if keypressed% <> 15 then goto L42290
                call "PRNTSCRN"
                goto L42290

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100          /* PART NUMBER      */
                     return
L50100:     REM TEST DATA FOR ENTER DESIRED PART CODE
                call "GETCODE" (#4, part$, partdescr$, 0%, 0, f1%(4))
                if f1%(4) <> 0 then goto L50150
                     errormsg$ = "Part not found, please re-enter: " &   ~
                          part$
                     return
L50150:         get #4, using L50160, hnyldt$
L50160:         FMT XX(169), CH(10)
                convert hnyldt$ to ltd%, data goto L50220
                convert ltd% to curldt$, pic(###)
                convert curldt$ to ltd%, data goto L50220
                return

L50220:         curldt$ = "  0"
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

            call "SHOSTAT" ("One Moment Please.")

            end
