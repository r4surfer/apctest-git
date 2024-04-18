        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  FFFFF   AAA   TTTTT  X   X  TTTTT  IIIII  N   N  PPPP    *~
            *  F      A   A    T     X X     T      I    NN  N  P   P   *~
            *  FFFF   AAAAA    T      X      T      I    N N N  PPPP    *~
            *  F      A   A    T     X X     T      I    N  NN  P       *~
            *  F      A   A    T    X   X    T    IIIII  N   N  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FATXTINP - THIS PROGRAM ALLOWS EDIT OF FOUR TYPES OF      *~
            *            DESCRIPTIVE TEXT FOR FIXED ASSETS (FREE TEXT,  *~
            *            MARKET VALUE HISTORY, PURCHASE HISTORY, AND    *~
            *            REPAIRS AND MAINTENANCE).                      *~
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
            * 06/02/84 ! ORIGINAL                                 ! NLH *~
            * 09/16/88 ! FAMASTER File Mods, Standardized Screens ! RJM *~
            * 08/07/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            amount$(100)12,              /* AMOUNT                     */~
            asset_code$10,               /* ASSET CODE                 */~
            asset_codedescr$30,          /* ASSET CODE DESCRIPTION     */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            column_heading$(4)47,        /* COLUMN HEADINGS ON REPORT  */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            descr$(100)75,               /* DESCRIPTION                */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            fac$(20,5)1,                 /* FIELD ATTRIBUT CHARACTERS  */~
            hdrdate$45,                  /* REPORT TITLE               */~
            heading$79,                  /* COLUMN HEADINGS            */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* SCREEN HEADER              */~
            pfactive$(2,2)79,            /* ACTIVE PF KEYS MESSAGE     */~
            pfkey$(2)17,                 /* ACTIVE PR KEYS             */~
            plowkey$14,                  /* PLOW KEY                   */~
            report_descr$(4)23,          /* REPORT DESCRIPTION         */~
            savekey$11,                  /* ORIGINAL KEY TO START PLOW */~
            seq$(100)3,                  /* SEQUENCE NUMBER            */~
            seqnr$3,                     /* SEQUENCE NUMBER            */~
            text$(100)75,                /* TEXT STRING                */~
            text_type$1,                 /* (1-4) TYPE OF TEXT         */~
            tdate$(100)8,                /* DATE                       */~
            title$40,                    /* SCREEN TITLE BANNER        */~
            vendor$(100)9                /* VENDOR                     */~

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
            * #01 ! FAMASTER ! Fixed Assets Master File                 *~
            * #07 ! FATEXT   ! Fixed Assets Descriptive Text            *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "FAMASTER",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 900,                                   ~
                        keypos = 120,  keylen = 10,                      ~
                        alt key  1, keypos = 58, keylen =  1, dup


            select #07, "FATEXT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 125,                                   ~
                        keypos = 1,    keylen = 14                       ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#01, "SHARE", f2%(1 ), rslt$(1 ), axd$(1 ))
            call "OPENFILE" (#07, "SHARE", f2%(7 ), rslt$(7 ), axd$(7 ))
            if f2%(7) = 0 then L09000
            call "OPENFILE" (#07, "OUTPT", f2%(7 ), rslt$(7 ), axd$(7 ))
            close #7
            call "OPENFILE" (#07, "SHARE", f2%(7 ), rslt$(7 ), axd$(7 ))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "Position Cursor & Press (RETURN) to Edit Text ~
        ~ Press PF-6 for Full Screen Edit."

            pfactive$(1,1) ="(1)Start Over (4)Prev Page (6)Edit Text    (~
        ~13)Instructions (15)Print Screen"
            pfactive$(1,2) ="(2)First      (5)Next Page (9)Print Text    ~
        ~                (16)SAVE DATA"
            pfactive$(2,1) ="(1)Start Over                              (~
        ~13)Instructions (15)Print Screen"
            pfactive$(2,2) ="                                            ~
        ~                          "
            str(line2$,62) = "FATXTINP: " & str(cms2v$,,8)

            pfkey$(1) = hex(000102040506090d0f10)
            pfkey$(2) = hex(000d0f)

            base% = 0%
            for f% = 1% to 100%
                convert base% + f% to seq$(base%+f%), pic(###)
            next f%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, asset_code$,               ~
                      tdate$(), amount$(), vendor$(), descr$(),          ~
                      asset_codedescr$
L10090:     for fieldnr% = 1 to  1
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10180
L10120:         gosub'101(fieldnr%)
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit%  >  4 then L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10180:         next fieldnr%

                base% = 0%
                screen% = 1%
                if keyhit% = 1% or keyhit% = 3% then edtpg2
                if keyhit% = 2%                 then edtpg3
                if keyhit% = 4%                 then edtpg4
                goto L10090

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *         REPAIRS AND MAINTENANCE AND MARKET VALUE          *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        edtpg2        /* REPAIRS AND MAINTENANCE AND MARKET VALUE */
            init(" ") errormsg$
            title$ = "FIXED ASSETS REPAIRS AND MAINTENENCE "
            heading$ = "SEQ   DATE      AMOUNT    DESCRIPTION"
            if keyhit% = 3% then title$ = "FIXED ASSETS MARKET VALUE HIST~
        ~ORY"
L11120:     gosub'112(0%)
                  lastfieldnr% = base% + 1%
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  9 then L11280
                  if keyhit% <>  2 then L11190
                      base% = 0%
                      goto L11120
L11190:           if keyhit% <>  4 then       L11230
                      if base% - 12% < 0% then L11120
                      base% = base% - 12%
                      goto L11120
L11230:           if keyhit% <>  5 then       L11270
                      if base% + 12% > 85% then L11120
                      base% = base% + 12%
                      goto L11120
L11270:           if keyhit% <> 16 then       L11360
L11280:               for f% = 1% to 100%
                          text$(f%) = str(tdate$(f%),1,8) &              ~
                            str(amount$(f%),1,12) & str(descr$(f%),1,52)
                      next f%
                      if keyhit% = 16 then L11350
                          gosub L35000    /* PRINT REPORT */
                          goto L11120
L11350:               goto datasave
L11360:           if keyhit% <>  0 then       L11400
                      fieldnr% = cursor%(1) - 4%
                      if fieldnr% < 1% or fieldnr% > 16% then L11120
                      goto L11440
L11400:           if keyhit% <>  6 then       L11120
                  screen% = 2%
                  fieldnr% = 1%
                  edtmessage$ = "PRESS (ENTER) AFTER EDITING TEXT"

L11440:     gosub'112(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11440
            for fieldnr% = lastfieldnr% to base% + 16%
            gosub'152(fieldnr%)
                  lastfieldnr% = fieldnr%
                  if errormsg$ <> " " then L11440
            next fieldnr%
            screen% = 1%
            edtmessage$ = "PRESS P.F. KEY 6 TO EDIT TEXT"
            goto L11120

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                    PURCHASE HISTORY                       *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        edtpg3                /* PURCHASE HISTORY */
            init(" ") errormsg$
            title$ = "FIXED ASSETS PURCHASE HISTORY"
            heading$ = "SEQ   DATE      AMOUNT     VENDOR   DESCRIPTION"
L12080:     gosub'113(0%)
                  lastfieldnr% = base% + 1%
                  if keyhit%  =  1 then gosub startover
                  if keyhit% = 9 then L12240
                  if keyhit% <>  2 then L12150
                      base% = 0%
                      goto L12080
L12150:           if keyhit% <>  4 then       L12190
                      if base% - 12% < 0% then L12080
                      base% = base% - 12%
                      goto L12080
L12190:           if keyhit% <>  5 then       L12230
                      if base% + 12% > 85% then L12080
                      base% = base% + 12%
                      goto L12080
L12230:           if keyhit% <> 16 then       L12322
L12240:               for f% = 1% to 100%
                          text$(f%) = str(tdate$(f%),1,8) &              ~
                            str(amount$(f%),1,12) & str(vendor$(f%),1,9)&~
                            str(descr$(f%),1,42)
                      next f%
                      if keyhit% = 16 then L12320
                          gosub L35000   /* PRINT REPORT */
                          goto L12080
L12320:               goto datasave
L12322:           if keyhit% <>  0 then       L12330
                      fieldnr% = cursor%(1) - 4%
                      if fieldnr% < 1% or fieldnr% > 16% then L12080
                      goto L12370
L12330:           if keyhit% <>  6 then       L12080
                  screen% = 2%
                  fieldnr% = 1%
                  edtmessage$ = "PRESS (ENTER) AFTER EDITING TEXT"

L12370:     gosub'113(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12370
            for fieldnr% = lastfieldnr% to base% + 16%
            gosub'153(fieldnr%)
                  lastfieldnr% = fieldnr%
                  if errormsg$ <> " " then L12370
            next fieldnr%
            screen% = 1%
            edtmessage$ = "PRESS P.F. KEY 6 TO EDIT TEXT"
            goto L12080

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                     FREE TEXT                             *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        edtpg4            /* FREE TEXT */
            init(" ") errormsg$
            title$ = "FIXED ASSETS FREE DESCRIPTIVE TEXT"
            heading$ = "SEQ   FREE TEXT"
L13100:     gosub'114(0%)
                  lastfieldnr% = base% + 1%
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <> 9 then L13160
                      gosub L35000
                      goto L13100
L13160:           if keyhit% <>  2 then L13190
                      base% = 0%
                      goto L13100
L13190:           if keyhit% <>  4 then       L13230
                      if base% - 12% < 0% then L13100
                      base% = base% - 12%
                      goto L13100
L13230:           if keyhit% <>  5 then       L13270
                      if base% + 12% > 85% then L13100
                      base% = base% + 12%
                      goto L13100
L13270:           if keyhit%  = 16 then goto  datasave
                  if keyhit% <>  0 then       L13280
                      fieldnr% = cursor%(1) - 4%
                      if fieldnr% < 1% or fieldnr% > 16% then L13100
                      goto L13320
L13280:           if keyhit% <>  6 then       L13100
                  screen% = 2%
                  fieldnr% = 1%
                  edtmessage$ = "PRESS (ENTER) AFTER EDITING TEXT"

L13320:     gosub'114(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L13320
            for fieldnr% = lastfieldnr% to base% + 16%
            gosub'154(fieldnr%)
                  lastfieldnr% = fieldnr%
                  if errormsg$ <> " " then L13320
            next fieldnr%
            screen% = 1%
            edtmessage$ = "PRESS P.F. KEY 6 TO EDIT TEXT"
            goto L13100

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "DELETE" (#07, savekey$, 11%)
            for f% = 1% to 100%
                if text$(f%) = " " then L19066
                convert f% to seqnr$, pic(###)
                write #07, using L19090, savekey$, seqnr$, text$(f%)
L19066:     next f%
            goto inputmode

L19090:     FMT CH(11), CH(3), CH(75)

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20100          /* ASSET CODE       */
                     return
L20100:     REM DEFAULT/ENABLE FOR ASSET CODE
                enabled% = 1%
                inpmessage$ = "TO FIND ASSET CODES, LEAVE BLANK AND PRESS~
        ~ P.F. KEY"
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
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
            startover% = 2%
            call "STARTOVR" (startover%)
            if startover% = 1% then return
            return clear all
            goto inputmode


L32000: REM *************************************************************~
            *              READ THE EXISTING TEXT FROM FILE             *~
            *************************************************************

            text$() = " "
            plowkey$ = str(savekey$,1,11) & hex(00)

L32090:     call "PLOWNEXT" (#07, plowkey$, 11%, f1%(7))
            if f1%(7) = 0 then L32180
            get #07, using L32120, seqnr$
L32120:     FMT XX(11), CH(3), XX(75)
            convert seqnr$ to temp%, data goto L32090
            get #07, using L32150, text$(temp%)
L32150:     FMT XX(14), CH(75)
            goto L32090

L32180:     if keyhit% <> 1% and keyhit% <> 3% then L32250
            for f% = 1% to 100%
                tdate$(f%) = str(text$(f%),1,8)
                amount$(f%) = str(text$(f%),9,12)
                descr$(f%) = str(text$(f%),21,55)
            next f%

L32250:     if keyhit% <> 2% then L32330
            for f% = 1% to 100%
                tdate$(f%) = str(text$(f%),1,8)
                amount$(f%) = str(text$(f%),9,12)
                vendor$(f%) = str(text$(f%),21,9)
                descr$(f%) = str(text$(f%),30,46)
            next f%

L32330:     return

L35000: REM *************************************************************~
            *                PRINT THE TEXT                             *~
            *************************************************************

            select printer (134)
            call "DATE" addr("HD", hdrdate$)
            pagenr% = 0%
            report_descr$(1%) = "REPAIRS AND MAINTENANCE"
            report_descr$(2%) = "PURCHASE HISTORY"
            report_descr$(3%) = "MARKET VALUE HISTORY"
            report_descr$(4%) = "FREE TEXT"
            column_heading$(1%) = "SEQ   DATE      AMOUNT    DESCRIPTION"
            column_heading$(2%) = "SEQ   DATE      AMOUNT     VENDOR   DE~
        ~SCRIPTION"
            column_heading$(3%) = "SEQ   DATE      AMOUNT    DESCRIPTION"
            column_heading$(4%) = "SEQ    FREE TEXT"

            convert text_type$ to rep_type%

            for f% = 100% to 1% step -1%
                if text$(f%) = " " then L35120
                last_line% = f%
                goto L35140
L35120:     next f%

L35140:     gosub L38000               /* PRINT REPORT HEADING */
            for f% = 1% to last_line%
                if line% > 55% then gosub L38000
                on rep_type% goto L35180, L35200, L35180, L35220
L35180:         print using L38240, seq$(f%), tdate$(f%), amount$(f%),    ~
                                   descr$(f%)
                    goto L35240
L35200:         print using L38270, seq$(f%), tdate$(f%), amount$(f%),    ~
                                   vendor$(f%), descr$(f%)
                    goto L35240
L35220:         print using L38300, seq$(f%), text$(f%)
L35240:         line% = line% + 1%
            next f%
            print using L38330
            close printer
            return

L38000: REM *************************************************************~
            *           REPORT HEADING & IMAGE STATEMENTS               *~
            *************************************************************

            pagenr% = pagenr% + 1%
            print page
            print using L38160, pagenr%, report_descr$(rep_type%),hdrdate$
            print
            print using L38200, asset_code$, asset_codedescr$
            print
            print using L38220, column_heading$(rep_type%)
            print using L38235
            line% = 6%
            return


L38160: %PAGE #####    FIXED ASSETS DESCRIPTIVE TEXT - ##################~
        ~#####                 ###########################################~
        ~##

L38200: %ASSET CODE:  ##########  ##############################

L38220: %###############################################

L38235: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--

L38240: %### ######## ########.##- ######################################~
        ~##############

L38270: %### ######## ########.##- ######### ############################~
        ~##############

L38300: %### ############################################################~
        ~###############

L38330: %END OF TEXT

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40140          /* ASSET CODE       */
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
                  "FIXED ASSETS DESCRIPTIVE TEXT",                       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
                                                                         ~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "ASSET CODE",                                          ~
               at (06,15), fac(lfac$( 1)), asset_code$          , ch(10),~
               at (09,02), "PF KEY",                                     ~
               at (10,03), "(1) REPAIRS AND MAINTENANCE",                ~
               at (11,03), "(2) PURCHASE HISTORY",                       ~
               at (12,03), "(3) MARKET VALUE HISTORY",                   ~
               at (13,03), "(4) FREE TEXT",                              ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(00010203040d0f10)),                              ~
               key (keyhit%)

               if keyhit% <> 13 then L40530
                  call "MANUAL" ("FATXTINP")
                  goto L40210

L40530:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40210

        REM *************************************************************~
            *   EDIT MODE FOR REPAIRS & MAINT. AND MARKET VALUE HISTORY *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 2 OF DOCUMENT.                    *~
            *************************************************************

            deffn'112(fieldnr%)
                  if fieldnr% = 0% then init(hex(84)) fac$()             ~
                                   else init(hex(8c)) fac$()
                  r% = fieldnr%
                  if fieldnr% = 0% then L45060
                  if keyhit% <> 6% then L45070
                  fieldnr% = 1%
L45060:           for r% = 1% to 16%
L45070:             if fieldnr% > 0% then fac$(r%,2%) = hex(81) /* DATE*/~
                                     else fac$(r%,2%) = hex(86)
                    if fieldnr% > 0% then fac$(r%,3%) = hex(82) /* AMT */~
                                     else fac$(r%,3%) = hex(84)
                    if fieldnr% > 0% then fac$(r%,4%) = hex(81) /* DESC*/~
                                     else fac$(r%,4%) = hex(84)
                    if keyhit% <> 6% and fieldnr% > 0% then L45106
                  next r%
L45106:   put str(line2$,,60) using L45107, asset_code$, asset_codedescr$
L45107: %Asset: ##########  ##############################

L45115:     accept                                                       ~
               at (01,02), fac(hex(8c)), title$                 , ch(79),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), fac(hex(8c)), heading$               , ch(79),~
            at (05,02), fac(hex(8c)),    seq$(base%+ 1%)        , ch(03),~
            at (06,02), fac(hex(8c)),    seq$(base%+ 2%)        , ch(03),~
            at (07,02), fac(hex(8c)),    seq$(base%+ 3%)        , ch(03),~
            at (08,02), fac(hex(8c)),    seq$(base%+ 4%)        , ch(03),~
            at (09,02), fac(hex(8c)),    seq$(base%+ 5%)        , ch(03),~
            at (10,02), fac(hex(8c)),    seq$(base%+ 6%)        , ch(03),~
            at (11,02), fac(hex(8c)),    seq$(base%+ 7%)        , ch(03),~
            at (12,02), fac(hex(8c)),    seq$(base%+ 8%)        , ch(03),~
            at (13,02), fac(hex(8c)),    seq$(base%+ 9%)        , ch(03),~
            at (14,02), fac(hex(8c)),    seq$(base%+10%)        , ch(03),~
            at (15,02), fac(hex(8c)),    seq$(base%+11%)        , ch(03),~
            at (16,02), fac(hex(8c)),    seq$(base%+12%)        , ch(03),~
            at (17,02), fac(hex(8c)),    seq$(base%+13%)        , ch(03),~
            at (18,02), fac(hex(8c)),    seq$(base%+14%)        , ch(03),~
            at (19,02), fac(hex(8c)),    seq$(base%+15%)        , ch(03),~
            at (20,02), fac(hex(8c)),    seq$(base%+16%)        , ch(03),~
                                                                         ~
            at (05,06), fac(fac$( 1,2)),  tdate$(base%+ 1%)     , ch(08),~
            at (06,06), fac(fac$( 2,2)),  tdate$(base%+ 2%)     , ch(08),~
            at (07,06), fac(fac$( 3,2)),  tdate$(base%+ 3%)     , ch(08),~
            at (08,06), fac(fac$( 4,2)),  tdate$(base%+ 4%)     , ch(08),~
            at (09,06), fac(fac$( 5,2)),  tdate$(base%+ 5%)     , ch(08),~
            at (10,06), fac(fac$( 6,2)),  tdate$(base%+ 6%)     , ch(08),~
            at (11,06), fac(fac$( 7,2)),  tdate$(base%+ 7%)     , ch(08),~
            at (12,06), fac(fac$( 8,2)),  tdate$(base%+ 8%)     , ch(08),~
            at (13,06), fac(fac$( 9,2)),  tdate$(base%+ 9%)     , ch(08),~
            at (14,06), fac(fac$(10,2)),  tdate$(base%+10%)     , ch(08),~
            at (15,06), fac(fac$(11,2)),  tdate$(base%+11%)     , ch(08),~
            at (16,06), fac(fac$(12,2)),  tdate$(base%+12%)     , ch(08),~
            at (17,06), fac(fac$(13,2)),  tdate$(base%+13%)     , ch(08),~
            at (18,06), fac(fac$(14,2)),  tdate$(base%+14%)     , ch(08),~
            at (19,06), fac(fac$(15,2)),  tdate$(base%+15%)     , ch(08),~
            at (20,06), fac(fac$(16,2)),  tdate$(base%+16%)     , ch(08),~
                                                                         ~
            at (05,15), fac(fac$( 1,3)),  amount$(base%+ 1%)    , ch(12),~
            at (06,15), fac(fac$( 2,3)),  amount$(base%+ 2%)    , ch(12),~
            at (07,15), fac(fac$( 3,3)),  amount$(base%+ 3%)    , ch(12),~
            at (08,15), fac(fac$( 4,3)),  amount$(base%+ 4%)    , ch(12),~
            at (09,15), fac(fac$( 5,3)),  amount$(base%+ 5%)    , ch(12),~
            at (10,15), fac(fac$( 6,3)),  amount$(base%+ 6%)    , ch(12),~
            at (11,15), fac(fac$( 7,3)),  amount$(base%+ 7%)    , ch(12),~
            at (12,15), fac(fac$( 8,3)),  amount$(base%+ 8%)    , ch(12),~
            at (13,15), fac(fac$( 9,3)),  amount$(base%+ 9%)    , ch(12),~
            at (14,15), fac(fac$(10,3)),  amount$(base%+10%)    , ch(12),~
            at (15,15), fac(fac$(11,3)),  amount$(base%+11%)    , ch(12),~
            at (16,15), fac(fac$(12,3)),  amount$(base%+12%)    , ch(12),~
            at (17,15), fac(fac$(13,3)),  amount$(base%+13%)    , ch(12),~
            at (18,15), fac(fac$(14,3)),  amount$(base%+14%)    , ch(12),~
            at (19,15), fac(fac$(15,3)),  amount$(base%+15%)    , ch(12),~
            at (20,15), fac(fac$(16,3)),  amount$(base%+16%)    , ch(12),~
                                                                         ~
            at (05,28), fac(fac$( 1,4)),  descr$(base%+ 1%)     , ch(52),~
            at (06,28), fac(fac$( 2,4)),  descr$(base%+ 2%)     , ch(52),~
            at (07,28), fac(fac$( 3,4)),  descr$(base%+ 3%)     , ch(52),~
            at (08,28), fac(fac$( 4,4)),  descr$(base%+ 4%)     , ch(52),~
            at (09,28), fac(fac$( 5,4)),  descr$(base%+ 5%)     , ch(52),~
            at (10,28), fac(fac$( 6,4)),  descr$(base%+ 6%)     , ch(52),~
            at (11,28), fac(fac$( 7,4)),  descr$(base%+ 7%)     , ch(52),~
            at (12,28), fac(fac$( 8,4)),  descr$(base%+ 8%)     , ch(52),~
            at (13,28), fac(fac$( 9,4)),  descr$(base%+ 9%)     , ch(52),~
            at (14,28), fac(fac$(10,4)),  descr$(base%+10%)     , ch(52),~
            at (15,28), fac(fac$(11,4)),  descr$(base%+11%)     , ch(52),~
            at (16,28), fac(fac$(12,4)),  descr$(base%+12%)     , ch(52),~
            at (17,28), fac(fac$(13,4)),  descr$(base%+13%)     , ch(52),~
            at (18,28), fac(fac$(14,4)),  descr$(base%+14%)     , ch(52),~
            at (19,28), fac(fac$(15,4)),  descr$(base%+15%)     , ch(52),~
            at (20,28), fac(fac$(16,4)),  descr$(base%+16%)     , ch(52),~
                                                                         ~
               at (22,02), fac(hex(a4)),   edtmessage$          , ch(79),~
            at (23,02), fac(hex(8c)),    pfactive$(screen%,1%)  , ch(79),~
            at (24,02), fac(hex(8c)),    pfactive$(screen%,2%)  , ch(79),~
                                                                         ~
               keys(pfkey$(screen%)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L45560
                  call "MANUAL" ("FATXTINP")
                  goto L45115

L45560:        if keyhit% <> 15 then L45580
                  call "PRNTSCRN"
                  goto L45115

L45580:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *       EDIT MODE FOR PURCHASE HISTORY                      *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 3 OF DOCUMENT.                    *~
            *************************************************************

            deffn'113(fieldnr%)
                  if fieldnr% = 0% then init(hex(84)) fac$()             ~
                                   else init(hex(8c)) fac$()
                  r% = fieldnr%
                  if fieldnr% = 0% then L46060
                  if keyhit% <> 6% then L46070
                  fieldnr% = 1%
L46060:           for r% = 1% to 16%
L46070:             if fieldnr% > 0% then fac$(r%,2%) = hex(81) /* DATE*/~
                                     else fac$(r%,2%) = hex(86)
                    if fieldnr% > 0% then fac$(r%,3%) = hex(82) /* AMT */~
                                     else fac$(r%,3%) = hex(84)
                    if fieldnr% > 0% then fac$(r%,4%) = hex(81) /* DESC*/~
                                     else fac$(r%,4%) = hex(84)
                    if fieldnr% > 0% then fac$(r%,5%) = hex(81) /* Vend*/~
                                     else fac$(r%,5%) = hex(84)
                    if keyhit% <> 6% and fieldnr% > 0% then L46106
                  next r%
L46106:   put str(line2$,,60) using L46107, asset_code$, asset_codedescr$
L46107: %ASSET: ##########  ##############################

L46115:     accept                                                       ~
               at (01,02), fac(hex(8c)), title$                 , ch(79),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), fac(hex(8c)), heading$               , ch(79),~
            at (05,02), fac(hex(8c)),    seq$(base%+ 1%)        , ch(03),~
            at (06,02), fac(hex(8c)),    seq$(base%+ 2%)        , ch(03),~
            at (07,02), fac(hex(8c)),    seq$(base%+ 3%)        , ch(03),~
            at (08,02), fac(hex(8c)),    seq$(base%+ 4%)        , ch(03),~
            at (09,02), fac(hex(8c)),    seq$(base%+ 5%)        , ch(03),~
            at (10,02), fac(hex(8c)),    seq$(base%+ 6%)        , ch(03),~
            at (11,02), fac(hex(8c)),    seq$(base%+ 7%)        , ch(03),~
            at (12,02), fac(hex(8c)),    seq$(base%+ 8%)        , ch(03),~
            at (13,02), fac(hex(8c)),    seq$(base%+ 9%)        , ch(03),~
            at (14,02), fac(hex(8c)),    seq$(base%+10%)        , ch(03),~
            at (15,02), fac(hex(8c)),    seq$(base%+11%)        , ch(03),~
            at (16,02), fac(hex(8c)),    seq$(base%+12%)        , ch(03),~
            at (17,02), fac(hex(8c)),    seq$(base%+13%)        , ch(03),~
            at (18,02), fac(hex(8c)),    seq$(base%+14%)        , ch(03),~
            at (19,02), fac(hex(8c)),    seq$(base%+15%)        , ch(03),~
            at (20,02), fac(hex(8c)),    seq$(base%+16%)        , ch(03),~
                                                                         ~
            at (05,06), fac(fac$( 1,2)),  tdate$(base%+ 1%)     , ch(08),~
            at (06,06), fac(fac$( 2,2)),  tdate$(base%+ 2%)     , ch(08),~
            at (07,06), fac(fac$( 3,2)),  tdate$(base%+ 3%)     , ch(08),~
            at (08,06), fac(fac$( 4,2)),  tdate$(base%+ 4%)     , ch(08),~
            at (09,06), fac(fac$( 5,2)),  tdate$(base%+ 5%)     , ch(08),~
            at (10,06), fac(fac$( 6,2)),  tdate$(base%+ 6%)     , ch(08),~
            at (11,06), fac(fac$( 7,2)),  tdate$(base%+ 7%)     , ch(08),~
            at (12,06), fac(fac$( 8,2)),  tdate$(base%+ 8%)     , ch(08),~
            at (13,06), fac(fac$( 9,2)),  tdate$(base%+ 9%)     , ch(08),~
            at (14,06), fac(fac$(10,2)),  tdate$(base%+10%)     , ch(08),~
            at (15,06), fac(fac$(11,2)),  tdate$(base%+11%)     , ch(08),~
            at (16,06), fac(fac$(12,2)),  tdate$(base%+12%)     , ch(08),~
            at (17,06), fac(fac$(13,2)),  tdate$(base%+13%)     , ch(08),~
            at (18,06), fac(fac$(14,2)),  tdate$(base%+14%)     , ch(08),~
            at (19,06), fac(fac$(15,2)),  tdate$(base%+15%)     , ch(08),~
            at (20,06), fac(fac$(16,2)),  tdate$(base%+16%)     , ch(08),~
                                                                         ~
            at (05,15), fac(fac$( 1,3)),  amount$(base%+ 1%)    , ch(12),~
            at (06,15), fac(fac$( 2,3)),  amount$(base%+ 2%)    , ch(12),~
            at (07,15), fac(fac$( 3,3)),  amount$(base%+ 3%)    , ch(12),~
            at (08,15), fac(fac$( 4,3)),  amount$(base%+ 4%)    , ch(12),~
            at (09,15), fac(fac$( 5,3)),  amount$(base%+ 5%)    , ch(12),~
            at (10,15), fac(fac$( 6,3)),  amount$(base%+ 6%)    , ch(12),~
            at (11,15), fac(fac$( 7,3)),  amount$(base%+ 7%)    , ch(12),~
            at (12,15), fac(fac$( 8,3)),  amount$(base%+ 8%)    , ch(12),~
            at (13,15), fac(fac$( 9,3)),  amount$(base%+ 9%)    , ch(12),~
            at (14,15), fac(fac$(10,3)),  amount$(base%+10%)    , ch(12),~
            at (15,15), fac(fac$(11,3)),  amount$(base%+11%)    , ch(12),~
            at (16,15), fac(fac$(12,3)),  amount$(base%+12%)    , ch(12),~
            at (17,15), fac(fac$(13,3)),  amount$(base%+13%)    , ch(12),~
            at (18,15), fac(fac$(14,3)),  amount$(base%+14%)    , ch(12),~
            at (19,15), fac(fac$(15,3)),  amount$(base%+15%)    , ch(12),~
            at (20,15), fac(fac$(16,3)),  amount$(base%+16%)    , ch(12),~
                                                                         ~
            at (05,28), fac(fac$( 1,5)),  vendor$(base%+ 1%)    , ch(09),~
            at (06,28), fac(fac$( 2,5)),  vendor$(base%+ 2%)    , ch(09),~
            at (07,28), fac(fac$( 3,5)),  vendor$(base%+ 3%)    , ch(09),~
            at (08,28), fac(fac$( 4,5)),  vendor$(base%+ 4%)    , ch(09),~
            at (09,28), fac(fac$( 5,5)),  vendor$(base%+ 5%)    , ch(09),~
            at (10,28), fac(fac$( 6,5)),  vendor$(base%+ 6%)    , ch(09),~
            at (11,28), fac(fac$( 7,5)),  vendor$(base%+ 7%)    , ch(09),~
            at (12,28), fac(fac$( 8,5)),  vendor$(base%+ 8%)    , ch(09),~
            at (13,28), fac(fac$( 9,5)),  vendor$(base%+ 9%)    , ch(09),~
            at (14,28), fac(fac$(10,5)),  vendor$(base%+10%)    , ch(09),~
            at (15,28), fac(fac$(11,5)),  vendor$(base%+11%)    , ch(09),~
            at (16,28), fac(fac$(12,5)),  vendor$(base%+12%)    , ch(09),~
            at (17,28), fac(fac$(13,5)),  vendor$(base%+13%)    , ch(09),~
            at (18,28), fac(fac$(14,5)),  vendor$(base%+14%)    , ch(09),~
            at (19,28), fac(fac$(15,5)),  vendor$(base%+15%)    , ch(09),~
            at (20,28), fac(fac$(16,5)),  vendor$(base%+16%)    , ch(09),~
                                                                         ~
            at (05,38), fac(fac$( 1,4)),  descr$(base%+ 1%)     , ch(42),~
            at (06,38), fac(fac$( 2,4)),  descr$(base%+ 2%)     , ch(42),~
            at (07,38), fac(fac$( 3,4)),  descr$(base%+ 3%)     , ch(42),~
            at (08,38), fac(fac$( 4,4)),  descr$(base%+ 4%)     , ch(42),~
            at (09,38), fac(fac$( 5,4)),  descr$(base%+ 5%)     , ch(42),~
            at (10,38), fac(fac$( 6,4)),  descr$(base%+ 6%)     , ch(42),~
            at (11,38), fac(fac$( 7,4)),  descr$(base%+ 7%)     , ch(42),~
            at (12,38), fac(fac$( 8,4)),  descr$(base%+ 8%)     , ch(42),~
            at (13,38), fac(fac$( 9,4)),  descr$(base%+ 9%)     , ch(42),~
            at (14,38), fac(fac$(10,4)),  descr$(base%+10%)     , ch(42),~
            at (15,38), fac(fac$(11,4)),  descr$(base%+11%)     , ch(42),~
            at (16,38), fac(fac$(12,4)),  descr$(base%+12%)     , ch(42),~
            at (17,38), fac(fac$(13,4)),  descr$(base%+13%)     , ch(42),~
            at (18,38), fac(fac$(14,4)),  descr$(base%+14%)     , ch(42),~
            at (19,38), fac(fac$(15,4)),  descr$(base%+15%)     , ch(42),~
            at (20,38), fac(fac$(16,4)),  descr$(base%+16%)     , ch(42),~
                                                                         ~
               at (22,02), fac(hex(a4)),   edtmessage$          , ch(79),~
            at (23,02), fac(hex(8c)),    pfactive$(screen%,1%)  , ch(79),~
            at (24,02), fac(hex(8c)),    pfactive$(screen%,2%)  , ch(79),~
                                                                         ~
               keys(pfkey$(screen%)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L46645
                  call "MANUAL" ("FATXTINP")
                  goto L46115

L46645:        if keyhit% <> 15 then L46665
                  call "PRNTSCRN"
                  goto L46115

L46665:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *       EDIT MODE FOR FREE TEXT                             *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 4 OF DOCUMENT.                    *~
            *************************************************************

            deffn'114(fieldnr%)
                  if fieldnr% = 0% then init(hex(84)) fac$()             ~
                                   else init(hex(8c)) fac$()
                  r% = fieldnr%
                  if fieldnr% = 0% then L47072
                  if keyhit% <> 6% then L47084
                  fieldnr% = 1%
L47072:           for r% = 1% to 16%
L47084:             if fieldnr% > 0% then fac$(r%,4%) = hex(81) /* DESC*/~
                                     else fac$(r%,4%) = hex(84)
                    if keyhit% <> 6% and fieldnr% > 0% then L47110
                  next r%

L47110:   put str(line2$,,60) using L45107, asset_code$, asset_codedescr$
        %ASSET: ##########  ##############################

L47120:     accept                                                       ~
               at (01,02), fac(hex(8c)), title$                 , ch(79),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), fac(hex(8c)), heading$               , ch(79),~
            at (05,02), fac(hex(8c)),    seq$(base%+ 1%)        , ch(03),~
            at (06,02), fac(hex(8c)),    seq$(base%+ 2%)        , ch(03),~
            at (07,02), fac(hex(8c)),    seq$(base%+ 3%)        , ch(03),~
            at (08,02), fac(hex(8c)),    seq$(base%+ 4%)        , ch(03),~
            at (09,02), fac(hex(8c)),    seq$(base%+ 5%)        , ch(03),~
            at (10,02), fac(hex(8c)),    seq$(base%+ 6%)        , ch(03),~
            at (11,02), fac(hex(8c)),    seq$(base%+ 7%)        , ch(03),~
            at (12,02), fac(hex(8c)),    seq$(base%+ 8%)        , ch(03),~
            at (13,02), fac(hex(8c)),    seq$(base%+ 9%)        , ch(03),~
            at (14,02), fac(hex(8c)),    seq$(base%+10%)        , ch(03),~
            at (15,02), fac(hex(8c)),    seq$(base%+11%)        , ch(03),~
            at (16,02), fac(hex(8c)),    seq$(base%+12%)        , ch(03),~
            at (17,02), fac(hex(8c)),    seq$(base%+13%)        , ch(03),~
            at (18,02), fac(hex(8c)),    seq$(base%+14%)        , ch(03),~
            at (19,02), fac(hex(8c)),    seq$(base%+15%)        , ch(03),~
            at (20,02), fac(hex(8c)),    seq$(base%+16%)        , ch(03),~
                                                                         ~
            at (05,06), fac(fac$( 1,4)),  text$(base%+ 1%)      , ch(75),~
            at (06,06), fac(fac$( 2,4)),  text$(base%+ 2%)      , ch(75),~
            at (07,06), fac(fac$( 3,4)),  text$(base%+ 3%)      , ch(75),~
            at (08,06), fac(fac$( 4,4)),  text$(base%+ 4%)      , ch(75),~
            at (09,06), fac(fac$( 5,4)),  text$(base%+ 5%)      , ch(75),~
            at (10,06), fac(fac$( 6,4)),  text$(base%+ 6%)      , ch(75),~
            at (11,06), fac(fac$( 7,4)),  text$(base%+ 7%)      , ch(75),~
            at (12,06), fac(fac$( 8,4)),  text$(base%+ 8%)      , ch(75),~
            at (13,06), fac(fac$( 9,4)),  text$(base%+ 9%)      , ch(75),~
            at (14,06), fac(fac$(10,4)),  text$(base%+10%)      , ch(75),~
            at (15,06), fac(fac$(11,4)),  text$(base%+11%)      , ch(75),~
            at (16,06), fac(fac$(12,4)),  text$(base%+12%)      , ch(75),~
            at (17,06), fac(fac$(13,4)),  text$(base%+13%)      , ch(75),~
            at (18,06), fac(fac$(14,4)),  text$(base%+14%)      , ch(75),~
            at (19,06), fac(fac$(15,4)),  text$(base%+15%)      , ch(75),~
            at (20,06), fac(fac$(16,4)),  text$(base%+16%)      , ch(75),~
                                                                         ~
               at (22,02), fac(hex(a4)),   edtmessage$          , ch(79),~
            at (23,02), fac(hex(8c)),    pfactive$(screen%,1%)  , ch(79),~
            at (24,02), fac(hex(8c)),    pfactive$(screen%,2%)  , ch(79),~
                                                                         ~
               keys(pfkey$(screen%)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L47590
                  call "MANUAL" ("FATXTINP")
                  goto L47120

L47590:        if keyhit% <> 15 then L47630
                  call "PRNTSCRN"
                  goto L47120

L47630:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100          /* ASSET CODE       */
                     return
L50100:     REM TEST DATA FOR ASSET CODE
                if asset_code$ <> " " then L50121
                call "GETCODE" (#01, asset_code$, asset_codedescr$,0,0,  ~
                    f1%(1))
                if f1%(1) = 0 then L50140
                goto L50145

L50121:         call "READ100" (#01, asset_code$, f1%(1))
                if f1%(1) = 0 then L50140
                get #01, using L50124, asset_codedescr$
L50124:         FMT XX(129), CH(30)
                goto L50145

L50140:         errormsg$ = "INVALID ASSET CODE"
                return

L50145:         convert keyhit% to text_type$, pic(#)
                savekey$ = str(asset_code$,1,10) & text_type$ & " "
                gosub L32000
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152(fieldnr%)
                errormsg$ = " "
            REM TEST DATA FOR AMOUNT
                if amount$(fieldnr%) = " " then L51300
                convert amount$(fieldnr%) to amt, data goto L51250
                if amt = 0 then L51300
                convert amt to amount$(fieldnr%), pic(########.##-)
                goto L51300
L51250:         errormsg$ = "INVALID NUMBER: " & amount$(fieldnr%)

L51300:     REM TEST DATA FOR DATE
                if tdate$(fieldnr%) = " " or tdate$(fieldnr%) = blankdate$ ~
                                                               then return
                call "DATEOK" (tdate$(fieldnr%),temp%,errormsg$)
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 3.                       *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
            REM TEST DATA FOR AMOUNT
                if amount$(fieldnr%) = " " then L52125
                convert amount$(fieldnr%) to amt, data goto L52105
                if amt = 0 then L52125
                convert amt to amount$(fieldnr%), pic(########.##-)
                goto L52125
L52105:         errormsg$ = "INVALID NUMBER: " & amount$(fieldnr%)

L52125:     REM TEST DATA FOR DATE
                if tdate$(fieldnr%) = " " or tdate$(fieldnr%) = blankdate$ ~
                                                               then return
                call "DATEOK" (tdate$(fieldnr%),temp%,errormsg$)
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 4.                       *~
            *************************************************************

            deffn'154(fieldnr%)
                  errormsg$ = " "
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

            call "SHOSTAT" ("ONE MOMENT PLEASE")

            end
