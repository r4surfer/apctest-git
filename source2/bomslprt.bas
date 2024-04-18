        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    OOO   M   M   SSS   L      PPPP   RRRR   TTTTT   *~
            *  B   B  O   O  MM MM  S      L      P   P  R   R    T     *~
            *  BBBB   O   O  M M M   SSS   L      PPPP   RRRR     T     *~
            *  B   B  O   O  M   M      S  L      P      R   R    T     *~
            *  BBBB    OOO   M   M   SSS   LLLLL  P      R   R    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMSLPRT - Prints single level BOM's for a selected range *~
            *             of numbers using a BOM ID or Effectivity date.*~
            *             All look ups & printing is done in BOMSLSUB.  *~
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
            * 10/08/85 ! ORIGINAL                                 ! MJB *~
            * 11/25/85 ! ADDED TEXT PRINT OPTION & PN SORT OPTION ! MJB *~
            * 09/19/86 ! BOMMASTR FORMAT CHANGED                  ! LKM *~
            * 04/08/87 ! Fixed Quantity Extension                 ! MJB *~
            * 04/28/87 ! Added Reference Print Option             ! HES *~
            * 05/13/87 ! HNYMASTR record length mod for Std Cost  ! JIM *~
            * 08/12/87 ! Batch quantity logic installed.          ! JIM *~
            * 03/09/88 ! All lookup & print logic now in BOMSLSUB ! RJM *~
            *          !   Added input of BOM ID and/or Effective !     *~
            *          !   Date. No longer prints all unless asked!     *~
            * 04/18/88 ! Changed FAC on BOM ID to allow Alphanum  ! RJM *~
            * 02/17/89 ! Added PLOWCODE report for master BOM List! MJB *~
            * 03/21/89 ! Fx'd ALL part range to include numerics  ! MLJ *~
            * 04/19/89 ! Added option to include Part Master Text ! RJM *~
            * 04/08/91 ! (PRR 11708) Recompile for changes in     ! RJB *~
            *          !      BOMSLSUB and Added call to ALLFREE  !     *~
            * 06/17/91 ! QC - Moved ALLFREE to the right place.   ! RJB *~
            * 08/01/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**


        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bomid$3,                     /* Bom ID                     */~
            bomkey$31,                   /* Readkey for BOMMASTR       */~
            cursor%(2),                  /* Cursor Location For Edit   */~
            date$8,                      /* Date for screen display    */~
            descr_map(20),               /* Plowcode argument          */~
            effdatef$8,                  /* BOM Effectivity Date       */~
            effdate$8,                   /* BOM Eff. Date (packed)     */~
            endpn$25,                    /* Ending Part No. (Display)  */~
            errormsg$79,                 /* Error Message              */~
            firstpn$25,                  /* Starting Part No. (Lookup) */~
            hdr$(2)133,                  /* Column Header Lines        */~
            header$79,                   /* Screen Header Line         */~
            hnytxt$1,                    /* Print Part Text ?          */~
            i$(24)80,                    /* Screen Image               */~
            incl(1),                     /* Dummy PLOWCODE Argument    */~
            incl$(1)1,                   /* Dummy PLOWCODE Argument    */~
            inpmessage$79,               /* Input Message              */~
            lastpn$25,                   /* Last Part No. (Lookup)     */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            parent$25,                   /* Parent part number         */~
            pfkeys$32,                   /* FUNCTION KEYS ENABLED LISTS*/~
            pfktext$(3)79,               /* FUNCTION KEYS ENABLED LISTS*/~
            plowkey$30,                  /* Key for PLOWCODE Report    */~
            prtref$1,                    /* Print References Flag      */~
            prttxt$1,                    /* Print Text Flag            */~
            rptdescr$35,                 /* Heading for PLOW Report    */~
            startpn$25,                  /* Starting Part No. (Display)*/~
            sortsw$1,                    /* Switch for sort seq        */~
            title$(2)25                  /* Screen column heading      */

        dim f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************

            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! BOMMASTR ! BOM relationship file                    *~
            * #2  ! HNYMASTR ! Inventory Master File                    *~
            * #3  ! TXTFILE  ! System Text File                         *~
            * #4  ! BOMREFER ! Location Reference File                  *~
            * #11 ! ENGMASTR ! BOM and Rte Effectivity Dates            *~
            * #33 ! DUMMY    ! Dummy for PLOWCODE                       *~
            * #34 ! SYSFILE2 ! System File                              *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "BOMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56          ~

            select #2,  "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #3, "TXTFILE",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos = 1, keylen = 11

            select #4, "BOMREFER" ,                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos = 26, keylen = 34,                        ~
                        alt key 1, keypos = 1, keylen = 59

            select #11, "ENGMASTR"                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2015,                                 ~
                         keypos = 1, keylen = 29

            select #33, "DUMMY", varc, indexed,                          ~
                                 recsize = 5, keypos = 1, keylen = 1

            select #34, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#2,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#3,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#4,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#11, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#34, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)
            title$(1%) = "From"  :  title$(2%) = "To"
            f3% = 1%
            select printer(134)
            call "SETPRNT" ("BOM001", " ", 0%, 0%)
        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            call "ALLFREE"
            errormsg$, inpmessage$, startpn$, endpn$, prttxt$, sortsw$,  ~
            hnytxt$, prtref$, effdatef$, bomid$ = " "

L10100:     for fieldnr% = 1% to  7%
                gosub'051(fieldnr%)
                      if enabled% = 0% then L10250
L10130:         gosub'101(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit% <>  4% then L10215
L10170:                    fieldnr% = max(1%, fieldnr% - 1%)
                                if fieldnr% = 1% then L10100
                           gosub'051(fieldnr%)
                                if enabled% = 0% then L10170
                           goto L10130
L10215:               if keyhit%  = 14% then print_master
                      if keyhit% <>  0% then       L10130
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10130
L10250:     next fieldnr%

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

L11060:     inpmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (RETURN)."

L11090:     gosub'111(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       genreport
                  if keyhit% <>  0% then       L11090
            fieldnr% = cursor%(1) - 5%
            if fieldnr% < 1% or fieldnr% >  7% then L11090

            gosub'051(fieldnr%)
                  if enabled% = 0% then L11060
L11160:     gosub'111(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11160
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11160
            goto L11000

        REM *************************************************************~
            *  G E N E R A T E   B I L L S   O F   M A T E R I A L S    *~
            *                                                           *~
            * Prints the selected single level Bills of Material        *~
            *************************************************************

        genreport
            call "SHOSTAT" ("Generating Single Level Parts Lists")

            parent$ = firstpn$
L19110:     call "BOMSLSUB" (parent$, bomid$, prttxt$, hnytxt$, prtref$, ~
                             sortsw$, #1, #2, #3, #4, #11, #34, bomkey$, ~
                             effdate$, "1", " ")
                if bomkey$ = " " then L19200
                parent$ = str(bomkey$,,25)
                if effdate$ = "ALL" then bomid$ = str(bomkey$,26, 3)
                if parent$ > lastpn$ then L19200
            goto L19110

L19200: close printer
        call "SETPRNT" ("BOM001", " ", 0%, 0%)
        goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20140,         /* Part No. Range   */~
                                    L20200,         /* Effectivity Date */~
                                    L20430,         /* Specific Bom ID  */~
                                    L20260,         /* BOM Text Flag    */~
                                    L20490,         /* PART TEXT FLAG   */~
                                    L20310,         /* Print Refs Flag  */~
                                    L20370          /* Sort Flag        */
                     return
L20140:     REM DEFAULT/ENABLE FOR Part Number Range
                if startpn$ = " " then startpn$ = "ALL"
                inpmessage$ = "Enter Part Number Range (ALL, FIRST & LAST~
        ~ are valid)"
                return

L20200:     REM DEFAULT/ENABLE FOR BOM Effectivity Date
                if effdatef$ = " " or effdatef$ = blankdate$ ~
                                 then effdatef$ = date$
                inpmessage$ = "Enter BOM Effectivity Date, leave Blank, o~
        ~r enter 'ALL'"
                return

L20260:     REM DEFAULT/ENABLE FOR BOM Text Print Flag
                if prttxt$ = " " then prttxt$ = "N"
                inpmessage$ = "Enter 'Y' to Include Free BOM Text on"  & ~
                              " Report"
                return

L20310:     REM DEFAULT/ENABLE FOR Reference Print Flag
                if prtref$ = " " then prtref$ = "N"
                inpmessage$ = "Enter 'Y' to Include Reference Designators~
        ~ on Report"
                return

L20370:     REM DEFAULT/ENABLE FOR Sort Flag
                if sortsw$ = " " then sortsw$ = "N"
                inpmessage$ = "Enter 'Y' to Sort Bill of Materials by Com~
        ~ponent Part Number on Report"
                return

L20430:     REM DEFAULT/ENABLE FOR Specific Bom ID
                if effdatef$ <> "ALL" then L20445
                     enabled% = 0%
                     return
L20445:         if effdatef$ = " " or effdatef$ = blankdate$ then        ~
                inpmessage$ = "Enter a Specific BOM ID to be printed for ~
        ~each of the parts in the above range."                           ~
                                   else                                  ~
                inpmessage$ = "Enter Specific BOM ID to be printed, or le~
        ~ave blank to print effective BOM."
                return

L20490:     REM DEFAULT/ENABLE FOR Part Text Print Flag
                if hnytxt$ = " " then hnytxt$ = "N"
                inpmessage$ = "Enter 'Y' to Include Free PART Text on" & ~
                              " Report"
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
            startover% = 2%
            call "STARTOVR" (startover%)
            if startover% = 1% then return
            return clear all
            goto inputmode


        REM *************************************************************~
            *      P R I N T   B O M   M A S T E R   L I S T            *~
            *-----------------------------------------------------------*~
            * Prints a listing of all BOM Headers on BOMMASTR file      *~
            *************************************************************
        print_master
            plowkey$ = " "
            rptdescr$ = hex(06) & "Bills of Materials Master List"
            incl(1) = 0  :  incl$(1) = " "
            hdr$(2) = "Assembly Number           BOMID  RTEID  BOM D" &  ~
                      "escription                 Apprvd On - By    " &  ~
                      "           Phantom      Batch Qty        "

                  /*   xxxxxxxxxxxxxxxxxxxxxxxxx   xxx    xxx  xxxxx   */
                  /*   xxxxxxxxxxxxxxxxxxxxxxxxx   xx/xx/xx   xxxxxx   */
                  /*   xxxxxxxxx     x         999999.99               */

            descr_map( 1) =  26.25  :  descr_map( 2) =   1.00
            descr_map( 3) =  51.03  :  descr_map( 4) =  28.00
            descr_map( 5) =  87.03  :  descr_map( 6) =  35.00
            descr_map( 7) =  57.30  :  descr_map( 8) =  41.00
            descr_map( 9) = 115.061 :  descr_map(10) =  74.00
            descr_map(11) = 121.15  :  descr_map(12) =  85.00
            descr_map(13) = 106.01  :  descr_map(14) = 105.00
            descr_map(15) = 107.08  :  descr_map(16) = 114.1042
            descr_map(17) =  00.00  :  descr_map(18) =   0.00
            descr_map(19) =  00.00  :  descr_map(20) =   0.00

            call "PLOWCODE"(#1, plowkey$, rptdescr$, -9028%, -0.3,       ~
                            f1%(1), hdr$(), 0,  0, incl(), incl$(),      ~
                            "r", " ", #34, descr_map() )

            goto inputmode

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                init(hex(84)) lfac$()
                pfktext$(1) = "(1)Start Over                             ~
        ~                     (13)Instructions"
                pfktext$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3) = "                                       (14~
        ~)BOM Master List     (16)Exit Program"
                pfkeys$ = hex(00010d0f100e)
                if fieldnr% = 1% then L40320
                str(pfktext$(2), 20, 17) = "(4)Previous Field"
                pfktext$(3) = " "
                pfkeys$ = hex(0001040d0f)
                goto L40320

            deffn'111(fieldnr%)
                init(hex(84)) lfac$()
                pfktext$(1) = "(1)Start Over                             ~
        ~                     (13)Instructions"
                pfktext$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3) = "                                          ~
        ~                     (16)Print Report"
                if fieldnr% > 0% then pfktext$(3) = " "
                pfkeys$ = hex(00010d0f10)

L40320:           str(header$,62) = "BOMSLPRT: " & cms2v$
                  str(pfktext$(3),63,1) = hex(84)
                  on fieldnr% gosub L40450,         /* Part No. Range   */~
                                    L40450,         /* Effectivity Date */~
                                    L40450,         /* Specific Bom ID  */~
                                    L40450,         /* BOM TEXT FLAG    */~
                                    L40450,         /* PART TEXT FLAG   */~
                                    L40450,         /* REFR FLAG        */~
                                    L40450          /* Sort Flag        */
                     goto L40520

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40450:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40520:     accept                                                       ~
               at (01,02), "Single Level B.O.M. Explosion",              ~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,29), fac(hex(ac)), title$(1)              , ch(25),~
               at (05,55), fac(hex(ac)), title$(2)              , ch(25),~
                                                                         ~
               at (06,02),                                               ~
                  "Part Number",                                         ~
               at (06,29), fac(lfac$( 1)), startpn$             , ch(25),~
               at (06,55), fac(lfac$( 1)), endpn$               , ch(25),~
                                                                         ~
               at (07,02),                                               ~
                  "Effectivity Date",                                    ~
               at (07,29), fac(lfac$( 2)), effdatef$            , ch(08),~
                                                                         ~
               at (08,02),                                               ~
                  "Specific BOM ID",                                     ~
               at (08,29), fac(lfac$( 3)), bomid$               , ch(03),~
                                                                         ~
               at (09,02),                                               ~
                  "Include Free BOM Text?",                              ~
               at (09,29), fac(lfac$( 4)), prttxt$              , ch(01),~
                                                                         ~
               at (10,02),                                               ~
                  "Include Free Part Text?",                             ~
               at (10,29), fac(lfac$( 5)), hnytxt$              , ch(01),~
                                                                         ~
               at (11,02),                                               ~
                  "Print Reference Locations?",                          ~
               at (11,29), fac(lfac$( 6)), prtref$              , ch(01),~
                                                                         ~
               at (12,02),                                               ~
                  "Sort Bill by Part Number?",                           ~
               at (12,29), fac(lfac$( 7)), sortsw$              , ch(01),~
                                                                         ~
               at (14,04),                                               ~
                  "This program will print a Single Level Parts List",   ~
               at (15,04),                                               ~
                  "for all assemblies in the selected range.  If no",    ~
               at (16,04),                                               ~
                  "Effectivity Date is entered, the specific BOM ID",    ~
               at (17,04),                                               ~
                  "will be printed. If both are entered the effective",  ~
               at (18,04),                                               ~
                  "BOM will be printed if the specific BOM ID is not foun~
        ~d.",                                                             ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13% then L41100
                  call "MANUAL" ("BOMSLPRT")
                  goto L40520

L41100:        if keyhit% <> 15% then L41140
                  call "PRNTSCRN"
                  goto L40520

L41140:        if fieldnr% <> 0% then return
               close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50140,         /* Part No. Range   */~
                                    L50230,         /* Effectivity Date */~
                                    L50490,         /* Specific Bom ID  */~
                                    L50320,         /* BOM TEXT FLAG    */~
                                    L50550,         /* PART TEXT FLAG   */~
                                    L50370,         /* PRINT REFS FLAG  */~
                                    L50420          /* SORT FLAG        */
                     return
L50140:     REM TEST DATA FOR Starting Part Number
                call "TESTRNGE" (startpn$, endpn$, firstpn$, lastpn$,    ~
                                 errormsg$)
                return

L50230:     REM TEST DATA FOR Effectivity Date
                if effdatef$ = " " or effdatef$ = blankdate$ then return
                if effdatef$ <> "ALL" then L50250
                     effdate$ = effdatef$
                     return
L50250:         call "DATEOK" (effdatef$, f3%, errormsg$)
                if errormsg$ <> " " then return
                     effdate$ = effdatef$
                     call "DATUNFMT" (effdate$)
                call "PIPINDEX" (#34, effdate$, f3%, f1%(34))
                     if f1%(34) = 1% then errormsg$ = "Planning Calander ~
        ~Base Date not Found.  Exit & Correct."
                     if f1%(34) = 2% then errormsg$ = "Date Outside of Pl~
        ~anning Calendar"
                return

L50320:     REM TEST DATA FOR Text Print Flag
                if prttxt$ = " " then prttxt$ = "N"
                if pos("YN" = prttxt$) = 0 then L50460
                return

L50370:     REM TEST DATA FOR Reference Print Flag
                if prtref$ = " " then prtref$ = "N"
                if pos("YN" = prtref$) = 0 then L50460
                return

L50420:     REM TEST DATA FOR SORT FLAG
                if sortsw$ = " " then sortsw$ = "N"
                if pos("YN" = sortsw$) = 0 then L50460
                return
L50460:         errormsg$ = "Please Enter 'Y' or 'N'"
                return

L50490:     REM TEST DATA FOR SPECIFIC BOM ID
                if (effdatef$ = " " or effdatef$ = blankdate$) ~
                 and bomid$ = " " then ~
            errormsg$ = "The Specific BOM ID is required if no E~
                         ~ffectivity Date is entered"
                return

L50550:     REM TEST DATA FOR Text Print Flag
                if hnytxt$ = " " then hnytxt$ = "N"
                if pos("YN" = hnytxt$) = 0 then L50460
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
