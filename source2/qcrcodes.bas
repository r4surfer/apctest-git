        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   QQQ    CCC   RRRR    CCC    OOO   DDDD   EEEEE   SSS    *~
            *  Q   Q  C   C  R   R  C   C  O   O  D   D  E      S       *~
            *  Q   Q  C      RRRR   C      O   O  D   D  EEEE    SSS    *~
            *  Q Q Q  C   C  R   R  C   C  O   O  D   D  E          S   *~
            *   QQQ    CCC   R   R   CCC    OOO   DDDD   EEEEE   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * QCRCODES - MANAGES QUALITY CONTROL REJECTION CODES.  CODES*~
            *            ARE USED TO DEFINED THE REASON FOR REJECTION.  *~
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
            * 01/23/84 ! ORIGINAL                                 ! HES *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            category$4,                  /* INVENTORY CATEGORY CODE    */~
            catdescr$30,                 /* CATEGORY DESCRIPTION       */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            descr$30,                    /* ABBREVIATED DESCRIPTION    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Screen line #2             */~
            number$2,                    /* REJECTION REASON NUMBER    */~
            hdr$60,                      /* Header   for ASKUSER       */~
            msg$(3)80,                   /* Messages for ASKUSER       */~
            reason$(5)60                 /* EXTENDED DESCRIPTION       */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

            dim cms2v$50
            cms2v$ = "04.17.01 11/20/86 Order process & planning #2     "

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
            * #01 ! CATEGORY ! INVENTORY CATEGORIES                     *~
            * #02 ! QCRTYPES ! Stores Q/C rejection codes (reasons for  *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "CATEGORY",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos =  1,   keylen = 4

            select #02, "QCRTYPES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  350,                                  ~
                        keypos =  1,   keylen = 6

            call "SHOSTAT" ("Linking To The Data Base To Manage Q/C Rejec~
        ~tion Codes")

            call "OPENFILE" (#01, "SHARE", f2%(01), rslt$(01), axd$(01))
            call "OPENFILE" (#02, "SHARE", f2%(02), rslt$(02), axd$(02))

            if f2%(2) = 0 then L09000
            call "OPENFILE" (#02, "OUTPT", f2%(02), rslt$(02), axd$(02))
            close #2
            call "OPENFILE" (#02, "SHARE", f2%(02), rslt$(02), axd$(02))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

            str(line2$,62) = "QCRCODES: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, number$, descr$, reason$(),~
                                 catdescr$
            for fieldnr% = 1 to  4
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10180
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10180:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editmode
L11070:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  8 then gosub delete_it
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11070
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  10 or fieldnr% = 3 then L11070
            if fieldnr% = 4 then fieldnr% = 3
            if fieldnr%  > 4 then fieldnr% = 4

L11160:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11160
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11160
            goto L11070

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub L31000
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20100,         /* CATEGORY CODE    */~
                                    L20200,         /* REASON NUMBER    */~
                                    L20300,         /* SHORT DESCRIPTION*/~
                                    L20400          /* LONG DESCRIPTION */
                     return
L20100:     REM DEFAULT/ENABLE FOR INVENTORY CATEGORY CODE
                enabled% = 1
                inpmessage$ = "Enter Category Code"
                return
L20200:     REM DEFAULT/ENABLE FOR REJECTION REASON NUMBER
                enabled% = 1
                inpmessage$ = "Enter Number for Rejection Reason"
                return
L20300:     REM DEFAULT/ENABLE FOR ABBREVIATED DESCRIPTION
                enabled% = 1
                inpmessage$= "Enter a short description for the rejection"
                return
L20400:     REM DEFAULT/ENABLE FOR EXTENDED DESCRIPTION
                enabled% = 1
                inpmessage$ = "Enter up to 5 lines to describe the reject~
        ~ion"
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
            * K I L L  R E C O R D  L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO DELETE THIS RECORD OR       *~
            * RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH           *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

L29740: delete_it: REM ALLOW USER OPPORTUNITY TO ABORT.
            ask% = 2%
            hdr$ = "***** DELETE COMMAND *****"
            msg$(1) = "Press (1) to return to display"
            msg$(2) = "   OR   "
            msg$(3) = "Press RETURN to *DELETE* this entry"
            call "ASKUSER" (ask%, hdr$, msg$(1), msg$(2), msg$(3))
            if ask% = 1% then return
            if ask% <> 0% then L29740

        REM Delete it
               call "READ101" (#2, str(category$,,4) & number$, f1%(2))
                     if f1%(2) = 1 then delete #2
               return clear
               goto inputmode

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *    L O A D   T H E   R E J E C T I O N   R E C O R D      *~
            *                                                           *~
            * 'GETS' THE DATA FROM THE FILE.                            *~
            *************************************************************

            get #2, using L30060, descr$, reason$(), filler$
            return

L30060: FMT                      /* FILE: QCRTYPES                     */~
            XX(4),               /* Inventory Catagories               */~
            XX(2),               /* Generic for any code in the system */~
            CH(30),              /* Genreic for a description with a c */~
            5*CH(60),            /* Extended Description               */~
            CH(14)               /* filler for rest of record or inter */~

L31000: REM *************************************************************~
            *    S A V E   T H E   R E J E C T I O N   R E C O R D      *~
            *                                                           *~
            * 'GETS' THE DATA FROM THE FILE.                            *~
            *************************************************************

            call "READ101" (#2, str(category$,,4) & number$, f1%(2))
                     if f1%(2) = 1 then delete #2
            write #2, using L31090, category$, number$, descr$, reason$(),~
                                 filler$
            return

L31090: FMT                      /* FILE: QCRTYPES                     */~
            CH(4),               /* Inventory Catagories               */~
            CH(2),               /* Generic for any code in the system */~
            CH(30),              /* Genreic for a description with a c */~
            5*CH(60),            /* Extended Description               */~
            CH(14)               /* filler for rest of record or inter */~

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40170,         /* CATEGORY CODE    */~
                                    L40170,         /* REASON NUMBER    */~
                                    L40170,         /* SHORT DESCRIPTION*/~
                                    L40170          /* LONG DESCRIPTION */
                     goto L40240

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40240:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Q/C Rejection Codes",                          ~
               at (01,67),                                               ~
                  "DATE:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Inventory category code",                             ~
               at (06,30), fac(lfac$( 1)), category$            , ch(04),~
               at (06,40), fac(hex(8c)), catdescr$              , ch(30),~
               at (07,02),                                               ~
                  "Rejection reason number",                             ~
               at (07,30), fac(lfac$( 2)), number$              , ch(02),~
               at (09,02),                                               ~
                  "Abbreviated description",                             ~
               at (09,30), fac(lfac$( 3)), descr$               , ch(30),~
               at (10,02),                                               ~
                  "Extended description",                                ~
               at (11,10), fac(lfac$( 4)), reason$(1)           , ch(60),~
               at (12,10), fac(lfac$( 4)), reason$(2)           , ch(60),~
               at (13,10), fac(lfac$( 4)), reason$(3)           , ch(60),~
               at (14,10), fac(lfac$( 4)), reason$(4)           , ch(60),~
               at (15,10), fac(lfac$( 4)), reason$(5)           , ch(60),~
               at (11,02), "Line 1)",                                    ~
               at (12,02), "Line 2)",                                    ~
               at (13,02), "Line 3)",                                    ~
               at (14,02), "Line 4)",                                    ~
               at (15,02), "Line 5)",                                    ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Exit program",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40750
                  call "MANUAL" ("QCRCODES")
                  goto L40240

L40750:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40240

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41170,         /* CATEGORY CODE    */~
                                    L41170,         /* REASON NUMBER    */~
                                    L41170,         /* SHORT DESCRIPTION*/~
                                    L41170          /* LONG DESCRIPTION */
                     goto L41240

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41240:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Q/C Rejection Codes",                          ~
               at (01,67),                                               ~
                  "DATE:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Inventory category code",                             ~
               at (06,30), fac(lfac$( 1)), category$            , ch(04),~
               at (06,40), fac(hex(8c)), catdescr$              , ch(30),~
               at (07,02),                                               ~
                  "Rejection reason number",                             ~
               at (07,30), fac(lfac$( 2)), number$              , ch(02),~
               at (09,02),                                               ~
                  "Abbreviated description",                             ~
               at (09,30), fac(lfac$( 3)), descr$               , ch(30),~
               at (10,02),                                               ~
                  "Extended description",                                ~
               at (11,10), fac(lfac$( 4)), reason$(1)           , ch(60),~
               at (12,10), fac(lfac$( 4)), reason$(2)           , ch(60),~
               at (13,10), fac(lfac$( 4)), reason$(3)           , ch(60),~
               at (14,10), fac(lfac$( 4)), reason$(4)           , ch(60),~
               at (15,10), fac(lfac$( 4)), reason$(5)           , ch(60),~
               at (11,02), "Line 1)",                                    ~
               at (12,02), "Line 2)",                                    ~
               at (13,02), "Line 3)",                                    ~
               at (14,02), "Line 4)",                                    ~
               at (15,02), "Line 5)",                                    ~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,02),                                               ~
                  "(8)Delete",                                           ~
               at (24,65),                                               ~
                  "(16)Save Data",                                       ~
                                                                         ~
               keys(hex(0001080d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L41750
                  call "MANUAL" ("QCRCODES")
                  goto L41240

L41750:        if keyhit% <> 15 then L41790
                  call "PRNTSCRN"
                  goto L41240

L41790:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50130,         /* CATEGORY CODE    */~
                                    L50160,         /* REASON NUMBER    */~
                                    L50230,         /* SHORT DESCRIPTION*/~
                                    L50260          /* LONG DESCRIPTION */
                     return
L50130:     REM TEST DATA FOR INVENTORY CATEGORY CODE
            call "DESCRIBE" (#1, category$, catdescr$, 1%, f1%(1))
                if f1%(1) = 0 then catdescr$ = "Undefined category code"
                return
L50160:     REM TEST DATA FOR REJECTION REASON NUMBER
                convert number$ to n%, data goto L50210
                convert abs(n%) to number$, pic(00)
                call "READ100" (#2, str(category$,,4) & number$, f1%(2))
                if f1%(2) = 0 then return
                gosub L30000
                goto editmode

L50210:         errormsg$ = "Invalid entry for reason number"
                return
L50230:     REM TEST DATA FOR ABBREVIATED DESCRIPTION
                if descr$ = " " then errormsg$ = hex(00)
                return
L50260:     REM TEST DATA FOR EXTENDED DESCRIPTION
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

            call "SHOSTAT" ("Data Base Integrity Check In Process")
            end
