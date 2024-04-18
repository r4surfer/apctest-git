        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  W   W   CCC   RRRR   EEEEE   SSS   TTTTT  RRRR           *~
            *  W   W  C   C  R   R  E      S        T    R   R          *~
            *  W   W  C      RRRR   EEEE    SSS     T    RRRR           *~
            *  W W W  C   C  R   R  E          S    T    R   R          *~
            *   W W    CCC   R   R  EEEEE   SSS     T    R   R          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * WCRESTR  - Rebuild WCMASTR file from details.             *~
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
            * 09/06/83 ! ORIGINAL                                 ! KEN *~
            * 10/22/86 ! WCout file format change                 ! HES *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            base$8,                      /* Planning Calendar Base Date*/~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            hdr$60,                      /* Header for ASKUSER         */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Screen line #2             */~
            msg$(3)80,                   /* Lines for ASKUSER          */~
            readkey$50,                                                  ~
            restore$3                    /* RESTORE OK?                */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.17.01 11/20/86 Order process & planning #2     "
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
            * # 1 ! SYSFILE2 ! System catch all file                    *~
            * #11 ! WCMASTR  ! Work center master file                  *~
            * #23 ! WCOUT    ! Planned work center use detail rec       *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1 , "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos =    1, keylen =  20

            select #11, "WCMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6          ~

            select #23, "WCOUT",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   68,                                  ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =    1, keylen =  27

            call "SHOSTAT" ("Opening Files, One Moment Please")

            rslt$(11),rslt$(1) = "REQUIRED"
            call "OPENFILE" (#11, "IO   ", f2%(11), rslt$(11), axd$(11))
                if f2%(11) <> 0 then L65000

            call "OPENFILE" (#1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
                if f2%(1) <> 0 then L65000
            call "OPENFILE" (#23, "IO   ", f2%(23), rslt$(23), axd$(23))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

            call "READ100" (#1, "MONTHS OPEN", f1%(1))
                if f1%(1) = 0 then L09230
            get #1, using L09150, base$
L09150:     FMT XX(32), CH(6)

            call "PIPINDEX" (#1, base$, index%, return%)
                if return% = 0 then L09280
L09190:     ask% = 2%
            hdr$ = "*** PLANNING CALENDAR ERROR ***"
            msg$(1) = "Can't Find Planning Calendar Base Date"
            msg$(2) = "Please Press RETURN to Exit Program"
L09230:     msg$(3) = "And Correct Error."
            call "ASKUSER" (ask%, hdr$, msg$(1), msg$(2), msg$(3))
            if ask% <> 0% then L09190
            goto L65000

L09280:     call "DATEFMT" (base$)

            str(line2$,62) = "WCRESTR : " & str(cms2v$,1,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, restore$

            for fieldnr% = 1 to  2
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

L11060:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  2 then L11060

L11130:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11130
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11130
            goto L11060

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if restore$="NO" then L65000
            gosub L30000
            goto L65000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20110,         /* PURGE DATE       */~
                                    L20130          /* RESTORE OK?      */
                     return
L20110:     REM DEFAULT/ENABLE FOR PURGE DATE
                return
L20130:     REM DEFAULT/ENABLE FOR RESTORE OK?
            restore$="YES"
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

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *        R E S T O R E  W O R K  C E N T E R S              *~
            *                                                           *~
            * PROCESSING LOOP                                           *~
            *************************************************************

            if index% < 2 then L30180

            REM Purge Old details...
            display " "
            call "SHOSTAT" ("Purging Expired Detail Records")
            readkey$ = all(hex(00))
L30120:     call "PLOWAL1" (#23, readkey$, 1%, 0%, f1%(23))
                if f1%(23) = 0 then L30190
            if val(str(readkey$,5,2),2) <= index% then delete #23        ~
                                       else str(readkey$,5,2) = hex(ffff)
            goto L30120

L30180:     REM Now Re-align the WCMASTR file...
L30190:     call "WCBUILD" ("ALL^", #11, #23)
            return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40150,         /* PURGE DATE       */~
                                    L40150          /* RESTORE OK?      */
                     goto L40220

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40150:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40220:     accept                                                       ~
               at (01,02),                                               ~
                  "Reset Work Center Capacity Utilization",              ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Purge Orphan Details Older Than:",                    ~
               at (06,35), fac(lfac$( 1)), base$                , ch(08),~
               at (07,02),                                               ~
                  "Restore OK?",                                         ~
               at (07,30), fac(lfac$( 2)), restore$             , ch(03),~
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

               if keyhit% <> 13 then L40570
                  call "MANUAL" ("WCRESTR ")
                  goto L40220

L40570:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40220

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41150,         /* PURGE DATE       */~
                                    L41150          /* RESTORE OK?      */
                     goto L41220

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41150:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41220:     accept                                                       ~
               at (01,02),                                               ~
                  "Reset Work Center Capacity Utilization",              ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Purge Orphan Details Older Than:",                    ~
               at (06,35), fac(lfac$( 1)), base$                , ch(08),~
               at (07,02),                                               ~
                  "Restore OK?",                                         ~
               at (07,30), fac(lfac$( 2)), restore$             , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Process  ",                                       ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41570
                  call "MANUAL" ("WCRESTR ")
                  goto L41220

L41570:        if keyhit% <> 15 then L41610
                  call "PRNTSCRN"
                  goto L41220

L41610:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50110,         /* PURGE DATE       */~
                                    L50250          /* RESTORE OK?      */
                     return
L50110:     REM TEST DATA FOR RESTORE OK?
            call "DATEOK" (base$, u3%, errormsg$)
                if errormsg$ <> " " then return
            call "DATUNFMT" (base$)
            if base$ < date then L50180
                errormsg$ = "Date Must Be Before Today"
                goto L50210
L50180:     call "PIPINDEX" (#1, base$, index%, return%)
                if return% = 0 then L50210
                errormsg$ = "Date Must Be Within The Planning Calendar"
L50210:         call "DATEFMT" (base$)
                return

L50250:     REM TEST DATA FOR RESTORE OK?
            if restore$="NO" then return
            if restore$="YES" then return
                errormsg$="ANSWER YES OR NO"
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

