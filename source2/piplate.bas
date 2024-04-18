        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   IIIII  PPPP   L       AAA   TTTTT  EEEEE          *~
            *  P   P    I    P   P  L      A   A    T    E              *~
            *  PPPP     I    PPPP   L      AAAAA    T    EEEE           *~
            *  P        I    P      L      A   A    T    E              *~
            *  P      IIIII  P      LLLLL  A   A    T    EEEEE          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPLATE  - PRINTS OUT ALL PIP DETAIL PRIOR TO THE DATE    *~
            *            ENTERED. USED TO FIND LATE DETAIL.             *~
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
            * 09/28/83 ! ORIGINAL                                 ! HES *~
            * 01/17/90 ! ADDED CHECK START DATE OPTION FOR PIPINS ! KAB *~
            * 04/17/91 ! PRR 11589/11768  Misspelled 'INVALID'    ! SID *~
            *          !     Changed 'SHOWMSG' to 'SHOSTAT'.      !     *~
            * 10/08/93 ! Purchase Jobs Project - 'BW' and 'RW'    ! JBK *~
            *          !   PIPIN tags Checked for Start Date.     !     *~
            *          ! Misc.- Brought Screen and Report up to   !     *~
            *          !   Current Standards.  Replaced 'STOP'    !     *~
            *          !   with an ASKUSER.  Replaced startover   !     *~
            *          !   section with call to 'STARTOVR'. Added !     *~
            *          !   lots of '%' signs to Intergers.  Added !     *~
            *          !   Report ID.                             !     *~
            * 08/21/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            check_start$1,               /* CHECK UNRELEASED STARTS    */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            pdate$8,                     /* DATE FOR PRINT             */~
            dele$100,                    /* DELETE KEY                 */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            field$10,                    /* DATE FOR PRINT             */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            latedate$8,                  /* PRINT PIP DETAIL PRIOR TO :*/~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Screen Line #2             */~
            part$25,                     /* PART NUMBER                */~
            partdescr$32,                /* PART DESCRIPTION           */~
            readkey$60,                  /* KEY FOR PLOWS              */~
            rptid$8,                     /* Report ID                  */~
            rtitle1$65,                  /* Report Title 1             */~
            rtitle2$65,                  /* Report Title 2             */~
            runtime$8,                   /* Program Run Time           */~
            tag$19,                      /* PIP TAG NUMBER/ID          */~
            tdate$8,                     /* Temporary Date             */~
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
            * #01 ! PIPIN    ! Planned inventory additions detail       *~
            * #02 ! PIPOUT   ! Planned inventory use detail rec         *~
            * #03 ! HNYMASTR ! Inventory Master File                    *~
            * #13 ! CALMASTR ! PRODUCTION CALENDAR, 490 CONSECUTIVE DAYS*~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select #02, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select #03, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

           select #13, "CALMASTR", varc, indexed,                        ~
                     recsize = 1962, keypos = 1   , keylen = 2

            call "SHOSTAT" ("Opening Files, One Moment Please.")

            call "OPENFILE" (#01, "SHARE", f2%(01), rslt$(01), axd$(01))
            call "OPENFILE" (#02, "SHARE", f2%(02), rslt$(02), axd$(02))
            call "OPENFILE" (#03, "SHARE", f2%(03), rslt$(03), axd$(03))
            call "OPENFILE" (#13, "SHARE", f2%(13), rslt$(13), axd$(13))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            gosub loadcalendar

            str(line2$,62%) = " PIPLATE: " & str(cms2v$,,8%)

            call "COMPNAME" (12%, company$, u3%)
            rptid$ = "PIP003"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$

            for fieldnr% = 1% to  2%
                gosub'051(fieldnr%)
                      if enabled% = 0% then L10180
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10180:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

L11060:     gosub'111(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       L11060
            fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  2% then L11060

L11130:     gosub'111(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11130
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11130
            goto L11060

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * Prints Report -- Should be called DATAPRINT.              *~
            *************************************************************

        datasave
            call "SHOSTAT" ("Scanning PIPIN File")
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            select printer (134)
            runtime$ = " "  :  call "TIME" (runtime$)
            line% = 1000%
            page% = 0%
            printany% = 0%
            init (hex(00)) readkey$
            head1$ = "Date  In"
            head2$ = "Planned Start"
            rtitle1$ = "Late Planning Detail Report - Planned In's thru"&~
                       " " & latedate$
            rtitle2$ = "With Start Date Checking"
            if check_start$ <> "Y" then rtitle2$ =                       ~
                                            "With No Start Date Checking"
            call "STRING" addr("CT", rtitle1$, 65%)
            call "STRING" addr("CT", rtitle2$, 65%)

            REM ---- PRINT THE PIPINS FIRST ----
L19150:     call "PLOWALTS" (#1, readkey$, 1%, 0%, f1%(1%))
                     if f1%(1%) = 0% then L19300
                     goto L19190
L19170:     call "PLOWALTS" (#1, readkey$, 1%, 25%, f1%(1%))
                     if f1%(1%) = 0% then L19150
L19190:     gosub L30000   /* DO A GET ON THE PIPIN RECORD */
                     if abs(qty) > .001 then L19198
                        get #1 using L19193, dele$
L19193:                     FMT XX(29), CH(19)
                        call "DELETE" (#1, dele$, 19%)
                        field$ = "*DELETED*"
                        goto L19250
L19198:              if check_start$ <> "Y"  then L19210
                     if str(tag$,,2%)  = "BO" then L19206
                     if str(tag$,,2%)  = "BW" then L19206
                     if str(tag$,,2%)  = "RO" then L19206
                     if str(tag$,,2%)  = "RW" then L19206
                     if str(tag$,,2%) <> "WO" then L19212
L19206:                 if otherday% <= day% then L19220
                        goto L19212
L19210:              if pipday% > day% then init(hex(ff)) str(readkey$,26)
L19212:              if pipday% > day% then L19150
L19220:     if otherday% <= 0% then L19242
               str(field$,3%) = yymmdd$(otherday%)
               call "DATEFMT" (str(field$,3%))
               goto L19250
L19242:     str(field$,3%) = "INVALID"
L19250:     pdate$ = yymmdd$(pipday%)
            call "DATEFMT" (pdate$)
            gosub print_it
            goto L19170

L19300:     REM ---- NOW DO THE PIPOUTS ----
            call "SHOSTAT" ("Scanning PIPOUT File")
            line% = 1000%
            page% = 0%
            init (hex(00)) readkey$
            head1$ = "Date Out"
            head2$ = " "
            rtitle1$ = "Late Planning Detail Report - Planned Out's thr"&~
                       "u " & latedate$
            rtitle2$ = " "
            call "STRING" addr("CT", rtitle1$, 65%)

L19370:     call "PLOWALTS" (#2, readkey$, 1%, 0%, f1%(2%))
                     if f1%(2%) = 0% then print_done
                     goto L19410
L19390:     call "PLOWALTS" (#2, readkey$, 1%, 25%, f1%(2%))
                     if f1%(2%) = 0% then L19370
L19410:     gosub L31000   /* DO A GET ON THE PIPOUT RECORD */
                     if abs(qty) > .001 then L19420
                        get #2 using L19413, dele$
L19413:                     FMT CH(56)
                        call "DELETE" (#2, dele$, 56%)
                        field$ = "*DELETED*"
                        goto L19440
L19420:              if pipday% > day% then init(hex(ff)) str(readkey$,26)
                     if pipday% > day% then L19370
L19440:     pdate$ = yymmdd$(pipday%)
            call "DATEFMT" (pdate$)
            gosub print_it
            goto L19390

        print_it   /* PRINTS THE DETAIL RECORD */
            call "DESCRIBE" (#3, part$, partdescr$, 0%, f1%(3%))
            if f1%(3%) = 0% then partdescr$ = "*** PART NOT ON FILE ***"
            if line% > 60% then gosub print_heading
            line% = line% + 1%  :  printany% = printany% + 1%
            print using L19590, part$, partdescr$, pdate$, tag$,          ~
                  round(qty,2%), field$
            field$ = " "
        return

L19590: %######################### ################################   ###~
        ~#####  ################### -#######.##   #############

        print_heading
            if page% <> 0% then print using L19760
            print page
            page% = page% + 1%
            print using L19740, date$, runtime$, company$, " PIPLATE",    ~
                                                                    rptid$
            print using L19770, rtitle1$, page%
            line% = 2%
            if rtitle2$ = " " then L19670
                print using L19780, rtitle2$
                line% = line% + 1%
L19670:
            print
            print using L19590, "Part Number", "Part Description", head1$,~
                               "Tag Number", "   Quantity", head2$
            print using L19760
            line% = line% + 3%
        return

L19740: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:######

L19760: %================================================================~
        ~======================================================
L19770: %                                  ##############################~
        ~##################################                    PAGE: ####

L19780: %                                  ##############################~
        ~##################################

        print_done
            if printany% = 0% then L19830
            print skip(2)
            runtime$ = " " : call "TIME" (runtime$)
            print using L19870, runtime$
L19830:     close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            goto inputmode

L19870: % * * * * * * * * * *   E N D   O F   R E P O R T   @ ########  *~
        ~ * * * * * * * * *

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 0%
                  on fieldnr% gosub L20100,         /* END DATE         */~
                                    L20200          /* CHECK START      */
                     return
L20100:     rem DEFAULT/ENABLE FOR PRINT PIP DETAIL PRIOR TO :
                enabled% = 1%
                if latedate$ = " " or ~
                   latedate$ = blankdate$ then latedate$ = date$
            inpmessage$ = "All PIP Detail Up To And Including This Date W~
        ~ill Be Printed."
                return
L20200:     REM DEFAULT/ENABLE FOR CHECK START
                check_start$ = "N"
                enabled% = 1%
            inpmessage$ = "'Y' to Check Start Dates for Unreleased advice~
        ~s, 'N' for Completion Only"
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

L30000: REM *************************************************************~
            *            G E T    T H E    P I P I N                    *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

            get #1, using L30060, part$, pipday%, tag$, qty, otherday%
            return

L30060: FMT                      /* FILE: PIPIN                        */~
            CH(25),              /* Part code                          */~
            BI(4),               /* Date in subscript for PIP          */~
            CH(19),              /* Tag number in level 2 planning     */~
            PD(14,4),            /* Quantity of something in packed de */~
            BI(4)                /* Date to start as a date subscript  */~

L31000: REM *************************************************************~
            *            G E T    T H E    P I P O U T                  *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

            get #2, using L35065, tag$, part$, pipday%, qty
            return

L35065: FMT                      /* FILE: PIPOUT                       */~
            CH(19),              /* Tag number in level 2 planning     */~
            CH(25),              /* Part code                          */~
            BI(4),               /* Date out of PIP in date subscript  */~
            XX(8),               /* Time from the system clock         */~
            PD(14,4)             /* Quantity of something in packed de */~

        rem**************************************************************~
            *      l o a d   c a l m a s t r    d a t a                 *~
            *************************************************************

        loadcalendar

            call "READ100" (#13,"10", f1%(13%))
                if f1%(13%) = 0% then L37160
            get #13, using L37090, str(yymmdd$(),1%,1470%)
L37090:         FMT XX(2), CH(1470)

            call "READ100" (#13,"11", f1%(13%))
                if f1%(13%) = 0% then L37160
            get #13, using L37140, str(yymmdd$(),1471%,1470%)
L37140:         FMT XX(2), CH(1470)
            return

L37160:     keyhit% = 0%
            call "ASKUSER" (keyhit%, "CALENDAR ERROR", "Production Cale"&~
                            "ndar NOT Found or is INVALID", " ", "Hit A"&~
                            "ny Key to Continue")
            goto L65000

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40140,         /* END DATE         */~
                                    L40140          /* CHECK START      */
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
               at (01,02), "Find Late PIP Detail",                       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Print PIP Detail Prior To:",                          ~
               at (06,30), fac(lfac$( 1)), latedate$            , ch(08),~
               at (07,02),                                               ~
                  "Check Start Dates?",                                  ~
               at (07,30), fac(lfac$( 2)), check_start$         , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13% then L40530
                  call "MANUAL" ("PIPLATE ")
                  goto L40210

L40530:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L40210

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41140,         /* END DATE         */~
                                    L41140          /* CHECK START      */
                     goto L41210

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41140:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41210:     accept                                                       ~
               at (01,02), "Find Late PIP Detail",                       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Print PIP Detail Prior To:",                          ~
               at (06,30), fac(lfac$( 1)), latedate$            , ch(08),~
               at (07,02),                                               ~
                  "Check Start Dates?",                                  ~
               at (07,30), fac(lfac$( 2)), check_start$         , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Print Report",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13% then L41530
                  call "MANUAL" ("PIPLATE ")
                  goto L41210

L41530:        if keyhit% <> 15% then L41570
                  call "PRNTSCRN"
                  goto L41210

L41570:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* END DATE         */~
                                    L50200          /* CHECK START      */
                     return
L50100:     rem TEST DATA FOR PRINT PIP DETAIL PRIOR TO :
                call "DATEOK" (latedate$, u3%, errormsg$)
                   if errormsg$ <> " " then return
                pdate$ = latedate$
                call "DATUNFMT" (pdate$)
                search yymmdd$() = pdate$ to cursor%() step 6
                tdate$ = yymmdd$(490)
                call "DATEFMT" (tdate$)
                if cursor%(1) = 0 then errormsg$ ="Date Must Be Between "~
                            & date$ & " and " & tdate$
                day% = (cursor%(1) + 5)/6
                return
L50200:     REM TEST DATA FOR PRINT CHECK START
                if check_start$ <> "N" then check_start$ = "Y"
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
