        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   RRRR   L       CCC   H   H   SSS   U   U  M   M   *~
            *  P   P  R   R  L      C      H   H  S      U   U  MM MM   *~
            *  PPPP   RRRR   L      C      HHHHH   SSS   U   U  M M M   *~
            *  P      R  R   L      C      H   H      S  U   U  M   M   *~
            *  P      R   R  LLLLL   CCC   H   H   SSS    UUU   M   M   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLCHSUM - ALLOWS USER TO GROUP EARNTYPES INTO LEDGABLE   *~
            *            DESCRITIONS FOR CHECK PRINTING.                *~
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
            * 10/13/86 ! ORIGINAL                                 ! HES *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            earntyp$(500)12,             /* PAYROLL EARNINGS TYPE      */~
            earntyp$12,                  /* PAYROLL EARNINGS TYPE      */~
            code$12,                     /* GROUPING CODE              */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            header$79,                   /* Screen Header              */~
            header1$79,                  /* Screen Header              */~
            hfac$1,                      /* FIELD ATTRIBUTE CHARACTER  */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            jfac$(15)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lfac$(15)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line$(500)4,                 /* FOR SCREEN DISPLAY         */~
            message$79,                  /* INPUT MESSAGE              */~
            pfdescr$(3)79,               /* Keys Actice for screen     */~
            pfkeys$32                    /* Keys Actice for screen     */

        dim f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! PRLCHREF ! Earntype cross reference for check printi*~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "PRLCHREF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 50,                                    ~
                        keypos = 1, keylen = 14,                         ~
                        alt key 1, keypos = 15, keylen = 12


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, 0%, 0%, 100%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            header1$ = "          Earntype"

            allowed% = dim(earntyp$(),1)
            for i% = 1 to allowed%
                convert i% to line$(i%), pic(###)
                str(line$(i%),4,1) = ")"
            next i%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            errormsg$, message$, code$, earntyp$() = " "
            c%, maxlines%, line%, editmode% = 0

            for fieldnr% = 1 to  1
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10200
L10140:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit%  =  9 and fieldnr% = 1 then print_data
                      if keyhit% <>  0 then       L10140
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10140
L10200:         next fieldnr%

            message$ = "Enter The List Of Earnings Types That Are To Be S~
        ~ummarized On The Check."

        enter_lines
L10260:     c% = c% + 1
L10270:     gosub'102(c%-line%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  16 then L10390
                  if keyhit% <>  0  then L10270
            gosub'152(c%)
                  if errormsg$ <> " " then L10270

            maxlines% = c%
            if maxlines% = allowed% then L10400
            if maxlines% > 14 then line% = line% + 1
            goto L10260

L10390:     earntyp$(c%) = " "
L10400:     if editmode% = 1 then editmode

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editmode
            editmode% = 1
            message$= "To Modify Displayed Values, Position Cursor To Des~
        ~ired Value And Press (ENTER)."
            errormsg$ = " "

L11330:     gosub'112(0%)
            errormsg$ = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then line%= 0
                  if keyhit%  =  4 then line%= max(0, line%-10)
                  if keyhit%  =  5 then line%= max(0, min(               ~
                                                  line%+10,maxlines%-15))
                  if keyhit% <> 11 then L11480
                     if maxlines% = allowed% then L11330
                     line% = max(0, maxlines%-14)
                     c% = maxlines%
                     message$= "Press PF(16) To End Inserts"
                     goto enter_lines
L11480:           if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 and keyhit% <> 12 then L11330

            fieldnr% = cursor%(1) - 5
            errormsg$ = "Cursor Must Be Positioned To Desired Earntype"
            if fieldnr% < 1 or fieldnr% > 15 then L11330
            convert str(line$(line%+fieldnr%),,3) to c%
            errormsg$ = " "
            if earntyp$(c%) = " " then L11330
            if keyhit% = 12 then remove_type

            message$= " "
L11610:     gosub'112(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11610
            gosub'152(c%)
                  if errormsg$ <> " " then L11610
            goto L11330

        remove_type
            message$= "Press (ENTER) To Remove Flashing Earntype From Thi~
        ~s Grouping Code"
            errormsg$ = " "
            gosub'122(fieldnr%)
            if keyhit% <> 0 then editmode
            gosub delete_line
            goto editmode

        delete_line
            REM Delete Logic...
            for i% = c% to maxlines%
                 earntyp$  (i%) =  earntyp$  (i%+1)
            next i%
            maxlines% = maxlines% - 1
            line% = max(0, min(line%, maxlines%-15))
        return

        REM *************************************************************~
            *       I N P U T   M O D E   F O R   P R I N T             *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        print_data
            errormsg$, message$, startcode$, endcode$ = " "

            for fieldnr% = 1 to  2
                gosub'053(fieldnr%)
                      if enabled% = 0 then L14180
L14120:         gosub'103(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then inputmode
                      if keyhit% <>  0 then       L14120
                gosub'153(fieldnr%)
                      if errormsg$ <> " " then L14120
L14180:         next fieldnr%

        REM *************************************************************~
            *               P R I N T  C O D E S                        *~
            *                                                           *~
            * Prints a report showing earntypes tied to each code.      *~
            *************************************************************
            pagenumber% = 0
            pageline% = 987654321
            if startcode$ <> "ALL" then L15110
                readkey$ = all(hex(00))
                endcode$ = all(hex(ff))
                goto L15140
L15110:     str(readkey$,,12) = str(startcode$,,12) addc all(hex(ff))
            if endcode$ = " " then endcode$ = startcode$

L15140:     call "PLOWNEXT" (#1, readkey$, 0%, f1%(1))
                if f1%(1) = 1 then L15200
L15160:         if tag% = 0 then print using L16260
                close printer
                goto inputmode

L15200:     if str(readkey$,,6) > endcode$ then L15160
            if pageline% = 987654321 then print at(05,02); hex(84);      ~
                                                      "Print In Progress"
            startcode$ = str(readkey$,,12)

            str(readkey$,13) = all(hex(00))
L15280:     call "PLOWNEXT" (#1, readkey$, 12%, f1%(1))
                if f1%(1) = 1 then L15360
                gosub form_control
                if tag% = 0 then print using L16260                       ~
                     else pageline% = pageline% - 1
                tag% = 1
                goto L15140

L15360:     get #1, using L15370, earntyp$
L15370:     FMT XX(14), CH(12)

            gosub form_control
            print using L16320, startcode$, earntyp$

            tag% = 0
            startcode$ = " "
            goto L15280

        REM *************************************************************~
            *        P A G E   C O N T R O L   R O U T I N E            *~
            *                                                           *~
            * CONTROLS THE PAGING                                       *~
            *************************************************************

        form_control
                select printer (134)
                pageline% = pageline% + 1
                if pageline% < 58 then return
                   if pagenumber% > 0 and tag% = 0 then print using L16260
                   print page
                   pagenumber% = pagenumber% + 1
                   print using L16220, pagenumber%, date$
                   print
                   print using L16260
                   print using L16290
                   print using L16260
                   pageline% = 6
                   tag% = 1
                   return

L16220: %PAGE ###         P A Y R O L L   C H E C K   E A R N I N G S   C~
        ~ O D E S      DATE: ########

L16260: %+------------+------------+
L16290: %!SUM/PRINT AS!EARNING TYPE!
L16320: %!############!############!

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            lastcode$ = code$
            gosub L31000
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1
                  on fieldnr% gosub L20100          /* GROUPING CODE    */
                     return
L20100:     REM DEFAULT/ENABLE FOR GROUPING CODE
            message$ = "Leave Code Blank And Press ENTER to Find An Exist~
        ~ing Code."
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 3 OF INPUT. *~
            *************************************************************

            deffn'053(fieldnr%)
                  enabled% = 1
                  on fieldnr% gosub L22110,         /* STARTING CODE    */~
                                    L22160          /* ENDING CODE      */
                     return
L22110:     REM DEFAULT/ENABLE FOR STARTING CODE
            startcode$ = "ALL"
            message$ = "ALL Will Print all codes On File. To Print Range,~
        ~ Enter The Code To Start With."
                return
L22160:     REM DEFAULT/ENABLE FOR ENDING CODE
            if startcode$ = "ALL" then enabled% = 0
            message$ = "Enter The Last Code To Print.  Leave Blank To Onl~
        ~y Print The 'Starting Code'."
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
L29918:     accept                                                       ~
               at (01,27),                                               ~
                  "***** START OVER COMMAND*****",                       ~
               at (04,02),                                               ~
                  "PRESS (1) TO RETURN TO DISPLAY",                      ~
               at (06,02),                                               ~
              "PRESS (ENTER) TO START OVER WITHOUT SAVING CURRENT ENTRY",~
                                                                         ~
               keys(hex(00010f)),                                        ~
               on hex(00010f) goto L29942, L29948, L29952
               return

L29942:        REM START OVER            (ENTER)
                   return clear
                   goto inputmode
L29948:        REM RETURN TO DISPLAY.    (P.F. KEY 1)
                   return
L29952:        REM PRINT SCREEN.         (P.F. KEY 15)
                   call "PRNTSCRN"
                   goto L29918

L30000: REM *************************************************************~
            *                L O A D   D A T A                          *~
            *                                                           *~
            * Loads up existing Group Code For Edit                     *~
            *************************************************************

            onfile%, maxlines% = 0
            readkey$ = code$
            str(readkey$,13) = all(hex(00))
L30170:     call "PLOWNEXT" (#1, readkey$, 12%, f1%(1))
                if f1%(1) = 0 then return
            onfile% = 1%
            if maxlines% = allowed% then return
            c%, maxlines% = maxlines% + 1
            get #1, using L30220, earntyp$(c%)    /* Get Line Info */
L30220:     FMT XX(14), CH(12)
            goto L30170

L31000: REM *************************************************************~
            *                S A V E   D A T A                          *~
            *                                                           *~
            * Write new/modified data to disk.                          *~
            *************************************************************

            REM Write out Earntype list for this code...
            readkey$ = str(code$) & hex(0000)
            call "DELETE" (#1, code$, 12%)
            if maxlines% = 0 then return    /* Implied Delete */

            for c% = 1 to maxlines%
                write #1,using L31150, code$, c%, earntyp$(c%), " "
L31150:         FMT CH(12), BI(2), CH(12), CH(24)
            next c%
            return

        REM *************************************************************~
            *                      S C R E E N  1                       *~
            *                                                           *~
            * Handles Header Information.                               *~
            *************************************************************

            REM Input Mode Screen Controler
            deffn'101(fieldnr%)
                  pfdescr$(1) ="(1)Start Over   (9)Print Cross Referenc"&~
                               "es                      (13)Instructions"
                  pfdescr$(2) ="                                       "&~
                               "                        (15)Print Screen"
                  pfdescr$(3) ="                                       "&~
                               "                        (16)Exit Program"
                  pfkeys$ = hex(0001090d0f10)
                  init(hex(8c)) lfac$()
                  goto L40180

L40180:           REM Common Logic Starts Here...
                  str(pfdescr$(3),63,1)=hex(84) /* Make Sure They See */
                  header$ = " "
                  if lastcode$ <> " " then                               ~
                             header$ = "Last Code Managed: " & lastcode$
                  str(header$,62) = "PRLCHSUM: " & cms2v$
                  on fieldnr% gosub L40320,         /* GROUPING CODE    */~
                                    L40290,         /* REFERENCE TEXT   */~
                                    L40290          /* DEFAULT TEXT     */
                     goto L40390

L40290:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40320:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40390:     accept                                                       ~
               at (01,02), "Manage Payroll Check Earntype Groups",       ~
               at (01,60), "Todays Date:", fac(hex(8c)), date$  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02),                                               ~
                  "Earnings Description To Print On Check",              ~
               at (06,42), fac(lfac$( 1)), code$                , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L40610
                  call "MANUAL" ("PRLCHSUM")
                  goto L40390

L40610:        if keyhit% <> 15 then L40650
                  call "PRNTSCRN"
                  goto L40390

L40650:        if fieldnr% <> 0 then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                      S C R E E N  II                      *~
            *                                                           *~
            * Handles Line Item Information.                            *~
            *************************************************************

            REM Input Mode Screen Controler
            deffn'102(fieldnr%)
                  pfdescr$(1) ="(1)Start Over                          "&~
                               "                        (13)Instructions"
                  pfdescr$(2) ="                                       "&~
                               "                        (15)Print Screen"
                  pfdescr$(3) ="                                       "&~
                               "                           (16)Edit Mode"
                  pfkeys$ = hex(00010d0f10)
                  init(hex(8c)) lfac$()
                  hfac$ = hex(ac)
                  goto L41600

            REM Edit Mode Screen Controler
            deffn'112(fieldnr%)
                  pfdescr$(1) ="(1)Start Over                          "&~
                               "                        (15)Print Screen"
                  pfdescr$(2) ="(2)First       (4)Prev                 "&~
                               "         (11)Add Types  (15)Print Screen"
                  pfdescr$(3) ="               (5)Next                 "&~
                               "         (12)Remove Type   (16)Save Data"
                  pfkeys$ = hex(0001020405090a0b0c0d0f10)
                  init(hex(86)) lfac$()
                  hfac$ = hex(ae)
                  if fieldnr% = 0 then L41430

                  REM Adjust PF Keys Available If Modifing An Earntype...
                  init(hex(8c)) lfac$()
                  hfac$ = hex(ac)
                  afac$ = hex(8c)
                  pfdescr$(1) ="(1)Start Program Over                  "&~
                               "     (13)Instructions   (15)Print Screen"
                  pfdescr$(2) ="(ENTER) To Continue"
                  pfdescr$(3) = " "
                  pfkeys$ = hex(00010d0f)
L41430:           goto L41600

            REM Delete Mode Screen Controler
            deffn'122(fieldnr%)
                  pfdescr$(1) ="(1)Abort Delete And Return To Edit Mode"&~
                               "                        (13)Instructions"
                  pfdescr$(2) ="(ENTER) Remove Flashing Type From List "&~
                               "                        (15)Print Screen"
                  pfdescr$(3) = " "
                  pfkeys$ = hex(00010d0f10)
                  init(hex(8c)) lfac$()
                  hfac$ = hex(ac)
                  afac$ = hex(9c)
                  lfac$(fieldnr%), jfac$(fieldnr%) = hex(94)
                  goto L41710

L41600:     REM Common Logic Here...
            if fieldnr% > 0 then lfac$(fieldnr%) = hex(81)
            str(pfdescr$(3),66,1)=hex(84) /* Make Sure They See */
            init(hex(9c)) jfac$()
            for i% = 1 to 15
               if earntyp$(line%+i%)<>" " then jfac$(i%) = hex(8c)
            next i%
            if fieldnr% <> 0 then jfac$(fieldnr%) = hex(8c)
            header$ = "Earntypes to summarize on checks"
            str(header$,62) = "PRLCHSUM: " & cms2v$

L41710:     accept                                                       ~
               at (01,02), "Manage Payroll Check Earntype Groups",       ~
               at (01,60), "Todays Date:", fac(hex(8c)), date$  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (03,02), "Grouping Code:",                             ~
               at (03,17), fac(hex(84)), code$                  , ch(12),~
               at (03,35), "Consisting Of XXX Earntypes",                ~
               at (03,49), fac(hex(8c)), maxlines%            , pic(###),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hfac$),   header1$               , ch(79),~
                                                                         ~
               at (06,02), fac(jfac$(01)), line$(line%+01)      , ch(04),~
               at (07,02), fac(jfac$(02)), line$(line%+02)      , ch(04),~
               at (08,02), fac(jfac$(03)), line$(line%+03)      , ch(04),~
               at (09,02), fac(jfac$(04)), line$(line%+04)      , ch(04),~
               at (10,02), fac(jfac$(05)), line$(line%+05)      , ch(04),~
               at (11,02), fac(jfac$(06)), line$(line%+06)      , ch(04),~
               at (12,02), fac(jfac$(07)), line$(line%+07)      , ch(04),~
               at (13,02), fac(jfac$(08)), line$(line%+08)      , ch(04),~
               at (14,02), fac(jfac$(09)), line$(line%+09)      , ch(04),~
               at (15,02), fac(jfac$(10)), line$(line%+10)      , ch(04),~
               at (16,02), fac(jfac$(11)), line$(line%+11)      , ch(04),~
               at (17,02), fac(jfac$(12)), line$(line%+12)      , ch(04),~
               at (18,02), fac(jfac$(13)), line$(line%+13)      , ch(04),~
               at (19,02), fac(jfac$(14)), line$(line%+14)      , ch(04),~
               at (20,02), fac(jfac$(15)), line$(line%+15)      , ch(04),~
                                                                         ~
               at (06,12), fac(lfac$( 1)), earntyp$(line%+01)   , ch(12),~
               at (07,12), fac(lfac$( 2)), earntyp$(line%+02)   , ch(12),~
               at (08,12), fac(lfac$( 3)), earntyp$(line%+03)   , ch(12),~
               at (09,12), fac(lfac$( 4)), earntyp$(line%+04)   , ch(12),~
               at (10,12), fac(lfac$( 5)), earntyp$(line%+05)   , ch(12),~
               at (11,12), fac(lfac$( 6)), earntyp$(line%+06)   , ch(12),~
               at (12,12), fac(lfac$( 7)), earntyp$(line%+07)   , ch(12),~
               at (13,12), fac(lfac$( 8)), earntyp$(line%+08)   , ch(12),~
               at (14,12), fac(lfac$( 9)), earntyp$(line%+09)   , ch(12),~
               at (15,12), fac(lfac$(10)), earntyp$(line%+10)   , ch(12),~
               at (16,12), fac(lfac$(11)), earntyp$(line%+11)   , ch(12),~
               at (17,12), fac(lfac$(12)), earntyp$(line%+12)   , ch(12),~
               at (18,12), fac(lfac$(13)), earntyp$(line%+13)   , ch(12),~
               at (19,12), fac(lfac$(14)), earntyp$(line%+14)   , ch(12),~
               at (20,12), fac(lfac$(15)), earntyp$(line%+15)   , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,02), fac(hex(8c)), pfdescr$(1)            , ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(2)            , ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(3)            , ch(79),~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L42420
                  call "MANUAL" ("PRLCHSUM")
                  goto L41710

L42420:        if keyhit% <> 15 then L42460
                  call "PRNTSCRN"
                  goto L41710

L42460:        if fieldnr% <> 0 then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                    S C R E E N  III                       *~
            *                                                           *~
            * Handles print range selection.                            *~
            *************************************************************

            REM Input Mode Screen Controler
            deffn'103(fieldnr%)
                  pfdescr$(1) ="(1)Start Over                          "&~
                               "                        (13)Instructions"
                  pfdescr$(2) ="                                       "&~
                               "                        (15)Print Screen"
                  pfdescr$(3) ="                                       "&~
                               "                (16)Cancel Print Request"
                  pfkeys$ = hex(00010d0f10)
                  init(hex(8c)) lfac$()
                  str(pfdescr$(3),55,1)=hex(84) /* Make Sure They See */
                  header$ = "Print Codes                            " &  ~
                            "                                  Page 3"
                  on fieldnr% gosub L43260,         /* START CODE       */~
                                    L43260          /* END CODE         */
                     goto L43330

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L43260:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L43330:     accept                                                       ~
               at (01,02), "Manage Payroll Check Earntype Groups",       ~
               at (01,67), "DATE:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Starting Group Code",                                 ~
               at (06,30), fac(lfac$( 1)), startcode$           , ch(12),~
               at (07,02),                                               ~
                  "Ending Group Code",                                   ~
               at (07,30), fac(lfac$( 2)), endcode$             , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L43570
                  call "MANUAL" ("PRLCHSUM")
                  goto L43330

L43570:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L43330

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50120          /* GROUPING CODE    */
                     return
L50120:     REM TEST DATA FOR GROUPING CODE
            if code$ <> " " then L50180
            call "PLOWCODE" (#1, code$, " ", -12%, -.001, f1%(1))
                if f1%(1) = 1 then L50180
                   errormsg$ = hex(00)
                   return
L50180:     gosub L30000
            if onfile% = 0 then return
            return clear all
            goto editmode

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152(t%)
            errormsg$ = " "

            REM TEST DATA FOR EARNTYPE
            mat cursor% = zer
            search earntyp$() = str(earntyp$(t%)) to cursor%() step 12
            if cursor%(2) = 0 then L51170
                errormsg$ = "This Earntype Is Already Included In This " ~
                                                       & "Grouping Code"
                return

L51170:     call "REDALT0" (#1, earntyp$(t%), 1%, f1%(1))
                if f1%(1) = 0 then return
            get #1, readkey$
            if str(readkey$,,12) = code$ then return
            errormsg$ = "This Earntype has already been assigned to print~
        ~ as: " & str(readkey$,,12)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52110,         /* STARTING CODE    */~
                                    L52140          /* ENDING CODE      */
                     return
L52110:     REM TEST DATA FOR STARTING CODE
            return

L52140:     REM TEST DATA FOR ENDING CODE
            if endcode$ = " " then return
            if startcode$ > endcode$ then errormsg$ = "Ending Code Can't ~
        ~Be Greater Then The Starting Code"
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
