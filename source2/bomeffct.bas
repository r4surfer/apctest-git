        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    OOO   M   M  EEEEE  FFFFF  FFFFF   CCC   TTTTT   *~
            *  B   B  O   O  MM MM  E      F      F      C   C    T     *~
            *  BBBB   O   O  M M M  EEEE   FFFF   FFFF   C        T     *~
            *  B   B  O   O  M   M  E      F      F      C   C    T     *~
            *  BBBB    OOO   M   M  EEEEE  F      F       CCC     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMEFFCT - FOR PART-NUMBER AND PART-TYPE RANGES SELECTED, *~
            *            PRODUCES A REPORT SHOWING (BY PART) WHICH BOMS *~
            *            ARE EFFECTIVE AND THE DATES EFFECTIVE.         *~
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
            * 03/28/84 ! ORIGINAL                                 ! ERN *~
            * 09/17/86 ! BOMMASTR FORMAT CHANGED                  ! LKM *~
            * 05/13/87 ! HNYMASTR record length mod for Std Cost  ! JIM *~
            * 03/26/92 ! Corrected image statement ln 14170       ! MJB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            abortrsn$(4)60,              /* REASONS FOR ABORTING       */~
            bom$3,                       /* BOM ID                     */~
            bomdescr$30,                 /* BOM DESCRIPTION            */~
            bomkey$50,                   /* BOMMASTR PLOW KEY          */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            eff$(490)3,                  /* BOM'S EFFECTIVE            */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            fromdate$14,                 /* FROM DATE                  */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Screen Line #2             */~
            listgeneric$3,               /* INCLUDE PART-TYPE '000'?   */~
            part$25,                     /* PART CODE                  */~
            partdescr$32,                /* PART DESCRIPTION           */~
            parthgh$25,                  /* PART NUMBER RANGE- HIGH #  */~
            partlow$25,                  /* PART NUMBER RANGE- LOW #   */~
            parttype$3,                  /* PART TYPE CODE             */~
            prntdescr$32,                /* RPT LINE PART DESCRIPTION  */~
            prntpart$25,                 /* RPT LINE PART CODE         */~
            prnttype$3,                  /* RPT LINE TYPE CODE         */~
            rptdates$35,                 /* RPT HDR: DATE RANGE        */~
            rptparts$100,                /*        : PART RANGE        */~
            rpttypes$100,                /*        : TYPE RANGE        */~
            rte$3,                       /* ROUTE ID                   */~
            search%(1),                  /* SEARCH 'TO' RECEIVER       */~
            todate$14,                   /* TO DATE                    */~
            typehgh$3,                   /* PART TYPE RANGE- HIGH TYPE */~
            typelow$3,                   /* PART TYPE RANGE- LOW TYPE  */~
            yymmdd$(490)6                /* DATE CROSS-REFERENCE       */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

            dim cms2v$50
            cms2v$ = "R6.01.04 05/19/92 UNIX Compatibility Changes      "
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
            * #01 ! HNYMASTR ! Inventory Master File                    *~
            * #02 ! ENGMASTR ! Engineering Master Filer                 *~
            * #03 ! CALMASTR ! Planning Production Calendar File        *~
            * #04 ! BOMMASTR ! Bill-of-Materials Master                 *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #02, "ENGMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos =    1, keylen =  29

            select #03, "CALMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos =    1, keylen =   2

            select #04, "BOMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#01, "SHARE", f2%(01), rslt$(01), axd$(01))
            call "OPENFILE" (#02, "SHARE", f2%(02), rslt$(02), axd$(02))
            call "OPENFILE" (#03, "SHARE", f2%(03), rslt$(03), axd$(03))
            call "OPENFILE" (#04, "SHARE", f2%(04), rslt$(04), axd$(04))

            abortrsn$(1) = "INVENTORY MASTER FILE NOT FOUND"
            abortrsn$(2) = "ENGINEERING MASTER FILE NOT FOUND"
            abortrsn$(3) = "CALENDAR MASTER FILE NOT FOUND"
            abortrsn$(4) = "BILL-OF-MATERIALS FILE NOT FOUND"
            for x% = 1% to 4%
                if f2%(x%) =  0% then abortrsn$(x%) = " "
                notopen%   =  notopen% + f2%(x%)
            next x%
            if notopen% <> 0% then goto abortreport

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

            gosub loadcalendar

            REM  DEFINE SELECTION RANGE DEFAULT VALUES
                partlow$     = "ALL"
                typelow$     = "500"
                typehgh$     = "999"
                listgeneric$ = "YES"

            str(line2$,62) = "BOMEFFCT: " & str(cms2v$,1,8)

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

L11060:     gosub'111(0%)                /* GET FIELD NUMBER TO MODIFY */
                  if keyhit%  = 14% then goto  producereport
                  if keyhit%  = 16% then goto  L65000
                  if keyhit% <>  0% then goto  L11060
            fieldnr% = cursor%(1) - 5%
            if fieldnr% < 1% or fieldnr% >  5% then L11060
            if partlow$ = "ALL" and fieldnr% = 2% then goto L11060

L11130:     gosub'111(fieldnr%)          /* GET MODIFICATION           */
                  if keyhit% <>  0% then goto  L11130
            gosub'151(fieldnr%)          /* EDIT FIELD VALUE ENTERED   */
                  if errormsg$ <> " " then goto L11130
            goto L11060

        REM *************************************************************~
            *             R E P O R T                                   *~
            *************************************************************
        producereport

        REM   INITIALIZE
            call "SHOSTAT" ("Printing Report.....")
            select printer(134)

            todate$ = yymmdd$(490) : call "DATEFMT" (todate$)
            rptdates$ = "PERIOD FROM " & date$ & " TO " & todate$
            rptparts$ = "PARTS RANGE: ALL" : if partlow$ ="ALL" then L12150
            rptparts$ = "PART CODES FROM " & partlow$ & " TO " & parthgh$
L12150:     rpttypes$ = "PART TYPES FROM " & typelow$ & " TO " & typehgh$
                if listgeneric$ = "YES" then                             ~
                     rpttypes$ = rpttypes$ & " AND 000"
            line% = 857%

            if partlow$ <> "ALL" then goto L12210
                init (hex(00)) partlow$
                init (hex(ff)) parthgh$
L12210:     if parthgh$ = "END" then init (hex(ff)) parthgh$

            call "READ100" (#1, partlow$, f1%(1))
                if f1%(1) = 1% then goto L12560
            part$ = partlow$

        loop
            call "READ102" (#1, part$, f1%(1))
                if f1%(1) =  0% then goto endreport

L12560:     gosub loadpart
                if part$ > parthgh$ then goto endreport
                if parttype$ = "000" and listgeneric$ = "NO" then loop
                if parttype$ = "000" then goto L12620
                if parttype$ < typelow$ or parttype$ > typehgh$          ~
                     then goto loop

L12620:     firstdate% = date%

            effloop   /* FIND EFFECTIVE CHANGES AND PRINT */
                init (" ") fromdate$, todate$, bom$, bomdescr$, rte$
                if eff$(firstdate%) <> hex(010101) then goto L12700
                     bomdescr$ = "NO BILLS EFFECTIVE"
                     lastdate% = 490%
                     goto printline

L12700:         bom$ = eff$(firstdate%)
                for x% = firstdate% + 1% to 490%
                     if eff$(x%) = bom$ then goto L12740
                          lastdate% = x% : goto L12770
L12740:         next x%
                lastdate% = 490%

L12770:         fromdate$ = yymmdd$(firstdate%)
                     call "DATEFMT" (fromdate$)
                     fromdate$ = fromdate$ & " (###)"
                     convert firstdate% to str(fromdate$,11,3), pic(###)
                todate$   = yymmdd$(lastdate%)
                     call "DATEFMT" (todate$)
                     todate$   = todate$   & " (###)"
                     convert lastdate%  to str(todate$  ,11,3), pic(###)
                gosub loadbom

                printline
                     if line% > 50 then gosub pagehdr
                     print using lin0, prntpart$, prntdescr$, prnttype$, ~
                          fromdate$, todate$, bom$, bomdescr$, rte$
                     line% = line% + 1%
                     init (" ") prntpart$, prntdescr$, prnttype$
                     firstdate% = lastdate% + 1%
                     if firstdate% > 490% then goto loop else goto effloop


        endreport
            close printer
            goto L65000

        pagehdr
            line% = 1% : page% = page% + 1%

            print page
            print using hdr1, rptdates$, page%
            print using hdr2, rptparts$
            print using hdr3, rpttypes$
            print using hdr4
            print using hdr5
            print using hdr6

            prntpart$  = part$
            prntdescr$ = partdescr$
            prnttype$  = parttype$
          return

        REM **********  REPORT IMAGE STATEMENTS  *************************

        hdr1:%################################        BILL-OF-MATERIAL EF~
        ~FECTIVITY REPORT                                        PAGE ###

        hdr2:%###########################################################~
        ~#######################################

        hdr3:%###########################################################~
        ~#######################################

        hdr4:%                                                           ~
        ~           E F F E C T I V E

        hdr5:%     PART CODE                   PART DESCRIPTION          ~
        ~TYPE       FROM           TO          BOM    BOM DESCRIPTION     ~
        ~ROUTE
        hdr6:%------------------------- -------------------------------- ~
        ~----  --------------  --------------  --- ---------------------  ~
        ~-----
        lin0:   %######################### ##############################~
        ~##  ###  ##############  ##############  ### ####################~
        ~#   ###

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
            *   F I L E   A C C E S S I N G   S U B - R O U T I N E S   *~
            *************************************************************

        loadcalendar      /* LOAD CALENDAR XREF VARIABLE YYMMDD$       */
            abortrsn$(1) = "CALENDAR MASTER RECORD MISSING"

            call "READ100" (#3, "10", f1%(3))
                if f1%(3) = 0% then goto abortreport
                get #3 using L30095, str(yymmdd$(),,1470)
L30095:              FMT XX(2), CH(1470)
            call "READ100" (#3, "11", f1%(3))
                if f1%(3) = 0% then goto abortreport
                get #3 using L30095, str(yymmdd$(), 1471, 1470)

            abortrsn$(1) = " "

            call "DATUNFMT" (date$)
            search str(yymmdd$()) = date$ to search%() step 6
            call "DATEFMT" (date$)
            if search%(1) <> 0% then goto L30210
                abortrsn$(1) = "TODAY NOT FOUND IN PLANNING CALENDAR"
                goto abortreport
L30210:     date% = (search%(1) + 5%) / 6%   /* INDEX OF TODAY   */

          return


        loadpart
            get #1 using L30275, part$, partdescr$, parttype$
L30275:         FMT CH(25), CH(32), POS(180), CH(3)
            prntpart$ = part$ : prntdescr$ = partdescr$
                                prnttype$  = parttype$
            init (hex(01)) eff$()
            call "READ100" (#2, str(part$) & "1001", f1%(2))
                if f1%(2) = 0% then return
                     get #2 using L30335, eff$()
L30335:                   FMT XX(29), 490*CH(3)
                     return


        loadbom
            bomkey$ = str(part$) & str(bom$) & "  0"
            call "READ100" (#4, bomkey$, f1%(4))
                if f1%(4) = 1% then goto L30430
                     bomdescr$ = "NO BOM ON FILE"
                     return
L30430:     get #4 using L30435, bomdescr$, rte$
L30435:         FMT  POS(57), CH(30), CH(3)
          return


        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N                     *~
            *                                                           *~
            * SCREEN FOR EDITING OF DOCUMENT.                           *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41180,         /* FROM PART        */~
                                    L41180,         /* TO PART          */~
                                    L41180,         /* LIST GENERICS?   */~
                                    L41180,         /* FROM TYPE        */~
                                    L41180          /* TO TYPE          */
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
               at (01,02), "BOM EFFECTIVITY REPORT",                     ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Part number range - From:",                           ~
               at (06,30), fac(lfac$( 1)), partlow$             , ch(25),~
               at (07,02),                                               ~
                  "                     To:",                            ~
               at (07,30), fac(lfac$( 2)), parthgh$             , ch(25),~
               at (08,02),                                               ~
                  "Include part-type '000'?",                            ~
               at (08,30), fac(lfac$( 3)), listgeneric$         , ch(03),~
               at (09,02),                                               ~
                  "  Part-type range - From:",                           ~
               at (09,30), fac(lfac$( 4)), typelow$             , ch(03),~
               at (10,02),                                               ~
                  "                     To:",                            ~
               at (10,30), fac(lfac$( 5)), typehgh$             , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (23,45), "(14)Print Report",                           ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(hex(000e0d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41690
                  call "MANUAL" ("BOMEFFCT")
                  goto L41250

L41690:        if keyhit% <> 15 then L41730
                  call "PRNTSCRN"
                  goto L41250

L41730:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* FROM PART        */~
                                    L50200,         /* TO PART          */~
                                    L50300,         /* LIST GENERICS?   */~
                                    L50400,         /* FROM TYPE        */~
                                    L50500          /* TO TYPE          */
                     return
L50100:     REM TEST DATA FOR FROM PART NUMBER
                if partlow$ = "ALL" then parthgh$ = " "
                if parthgh$ < partlow$ then parthgh$ = "END"
                return
L50200:     REM TEST DATA FOR TO PART NUMBER
                if parthgh$ = "END" then return
                if parthgh$ < partlow$ then errormsg$ =                  ~
                     "To-Part may not be less than From-Part"
                return
L50300:     REM TEST DATA FOR INCLUDE GENERIC?
                if str(listgeneric$,,1) = "Y" then listgeneric$ = "YES"
                if str(listgeneric$,,1) = "N" then listgeneric$ = "NO "
                if listgeneric$ = "YES" or listgeneric$ = "NO" then return
                     errormsg$ = "Enter 'YES' -or- 'NO'"
                     return
L50400:     REM TEST DATA FOR FROM PART-TYPE
                if typelow$ <> "ALL" then goto L50410
                     typelow$ = "500"  :  typehgh$ = "999"
L50410:         if typelow$ >= "500" and typelow$ <= "999" then L50440
                     errormsg$ = "Part type must be between 500 and 999"
                     return
L50440:         if typehgh$ < typelow$ then typehgh$ = "999"
                return
L50500:     REM TEST DATA FOR TO PART-TYPE
                if typehgh$  = "END" then typehgh$ = "999"
                if typehgh$ >= "500" and typehgh$ <= "999" then L50540
                     errormsg$ = "Part type must be between 500 and 999"
                     return
L50540:         if typehgh$ >= typelow$ then return
                     errormsg$ = "To-type can not be greater than from"
                     return

        REM *************************************************************~
            * S U B S R O U T I N E S ,   E T C . . .                   *~
            *************************************************************

        abortreport  /* INFORM OPERATOR OF PROBLEM(S) AND ABORT */
            errormsg$ = "Cannot continue because of the following:"
            accept                                                       ~
               at (01,02), "BOM EFFECTIVITY REPORT",                     ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), fac(hex(84)), abortrsn$(1),                   ~
               at (07,02), fac(hex(84)), abortrsn$(2),                   ~
               at (08,02), fac(hex(84)), abortrsn$(3),                   ~
               at (09,02), fac(hex(84)), abortrsn$(4),                   ~
               at (15,02), "Press RETURN to return to menu...."

            goto L65000

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

            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
