        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB   RRRR   EEEEE  L      X   X  RRRR   FFFFF   *~
            *    J    B   B  R   R  E      L       X X   R   R  F       *~
            *    J    BBBB   RRRR   EEE    L        X    RRRR   FFFF    *~
            *  J J    B   B  R   R  E      L       X X   R   R  F       *~
            *   JJ    BBBB   R   R  EEEEE  LLLLL  X   X  R   R  F       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBRELXRF - Manage list of Part Class Codes that the       *~
            *            indicated user is allowed to process. Ie.,     *~
            *            what parts is the user allowed to release to   *~
            *            shop floor (create/release job).               *~
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
            * 01/31/86 ! ORIGINAL                                 ! HES *~
            * 11/05/92 ! PRR 12631 - Now auto deletes Scheduler   ! MLJ *~
            *          !  from JBRELUSR if not found in USERLCMS. !     *~
            * 11/20/92 ! Standardized STARTOVER procedure and     ! MLJ *~
            *          !  display of screen lines 1 & 2.          !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            code$(500)3,                 /* CLASS CODES                */~
            code$3,                      /* CLASS CODE                 */~
            codedescr$(500)32,           /* CLASS CODE DESCRIPTIONS    */~
            codedescr$30,                /* CLASS CODE DESCRIPTION     */~
            afac$1,                      /* FIELD ATTRIBUTE CHARACTER  */~
            append$3,                    /* CODE TO 'COPY' (Append)    */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            descr$30,                    /* USER NAME                  */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            header1$79,                  /* Screen Header              */~
            hfac$1,                      /* FIELD ATTRIBUTE CHARACTER  */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            jfac$(15)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lfac$(15)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line$(500)4,                 /* FOR SCREEN DISPLAY         */~
            line2$79,                    /* SCREEN LINE 2              */~
            message$79,                  /* INPUT MESSAGE              */~
            pfdescr$(2)79,               /* Keys Actice for screen     */~
            pfkeys$32,                   /* Keys Actice for screen     */~
            readkey$50,                  /* Work Variable              */~
            syslib$8,                    /* OP SYS LIBRARY (@SYSTEM@)  */~
            sysvol$6,                    /* OP SYS VOLUME              */~
            user$3                       /* USER TO UPDATE             */

        dim f2%(10),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(10)                      /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
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
            * #1  ! USERLCMS ! Program Access Control User Info file    *~
            *  1  ! JBRELUSR ! User Work Order Release Cross Reference  *~
            * #3  ! GENCODES ! General System Codes File                *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "USERLCMS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 400,                                   ~
                        keypos =    1, keylen =   3,                     ~
                        alt key 1, keypos =  4, keylen = 30, dup

            select #2,  "JBRELUSR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 70,                                    ~
                        keypos =    1, keylen =   6,                     ~
                        alt key 1, keypos =  4, keylen = 6

            select #3,  "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos =    1,  keylen = 24

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "EXTRACT" addr("XL", syslib$, "XV", sysvol$)
            call "READFDR" addr("USERLCMS", syslib$, sysvol$, 0%, f2%(1))
                if f2%(1) <> 0% then L65000
            call "PUTNAMES" addr(#1, "USERLCMS", syslib$, sysvol$)
            call "WORKOPN2" (#1, "SHARE", 0%, f2%(1))
        REM OPEN NOGETPARM, #1, SHARED, FILE = "USERLCMS",               ~
                                  LIBRARY = SYSLIB$, VOLUME = SYSVOL$

            call "OPENCHCK" (#2, 0%, f2%(2), 100%, " ")
            call "OPENCHCK" (#3, 0%, f2%(3),   0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            header1$ = "          Class        Description"

            allowed% = dim(code$(),1)
            for i% = 1 to allowed%
                convert i% to line$(i%), pic(###)
                str(line$(i%),4,1) = ")"
            next i%

            str(line2$,62) = "JBRELXRF: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            errormsg$,message$,user$,code$(),codedescr$(),descr$ = " "
            c%, maxlines%, line%, editmode% = 0
            select ws

            for fieldnr% = 1 to  1
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10210
L10140:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit%  =  3 and fieldnr% = 1 then print_data
                      if keyhit% <>  0 then       L10140
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10140
L10210:         next fieldnr%

            message$ = "Enter Scheduler Part Classes That This User Can S~
        ~tart Jobs For. 'ALL' for all."

        enter_lines
L10270:     c% = c% + 1
L10280:     gosub'102(c%-line%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  16 then L10410
                  if keyhit% <>  0  then L10280
            gosub'152(c%)
                  if errormsg$ <> " " then L10280

            maxlines% = c%
            if maxlines% = allowed% then L11000
            if code$(c%) = "ALL" then L11000
            if maxlines% > 14 then line% = line% + 1
            goto L10270

L10410:     code$(c%), codedescr$(c%) = " "

L11000: REM *************************************************************~
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
                  if keyhit%  =  10 then gosub append
                  if keyhit% <> 11 then L11480
                     if maxlines% = allowed% then L11330
                     line% = max(0, maxlines%-14)
                     c% = maxlines%
                     message$= "Press PF(16) To End Inserts"
                     goto enter_lines
L11480:           if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 and keyhit% <> 12 then L11330

            fieldnr% = cursor%(1) - 5
            errormsg$ = "Cursor Must Be Positioned To Desired Class"
            if fieldnr% < 1 or fieldnr% > 15 then L11330
            convert str(line$(line%+fieldnr%),,3) to c%
            errormsg$ = " "
            if code$(c%) = " " then L11330
            if keyhit% = 12 then remove_class

            message$= "Enter A Blank Class Code To Search Through Existin~
        ~g Classes"
L11610:     gosub'112(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11610
            gosub'152(c%)
                  if errormsg$ <> " " then L11610
            goto editmode

        remove_class
            message$= "Press (ENTER) To Remove Flashing Class Code From T~
        ~his Persons List"
            errormsg$ = " "
            gosub'122(fieldnr%)
            if keyhit% <> 0 then editmode
            gosub delete_line
            goto editmode

        delete_line
            REM Delete Logic...
            for i% = c% to maxlines%
                 code$     (i%) =  code$     (i%+1)
                 codedescr$(i%) =  codedescr$(i%+1)
            next i%
            maxlines% = maxlines% - 1
            line% = max(0, min(line%, maxlines%-15))
        return

        REM *************************************************************~
            *            ' E X T E R N A L   C O P Y '                  *~
            *                                                           *~
            * Loads up and appends the specified Users codes.           *~
            *************************************************************

        append
            dups% = 0
            errormsg$ = "Specified User Does Not Exist: " & append$
            call "GETCODE" (#1, append$, " ", 0%, 1, f1%(1))
                if f1%(1) = 0 then return

            REM Uses existing logic where possible
            u3% = maxlines%
            readkey$ = append$
            gosub load_lines
            if u3% = maxlines% then L12260
            for c% = u3% + 1 to maxlines%
L12180:         if code$(c%)=" " then L12260 /*Aviod Never Never Land*/
                t% = c%
                gosub test_for_dup
                if errormsg$ = " " then L12250
                gosub delete_line
                dups% = dups% + 1
                goto L12180
L12250:     next c%
L12260:     convert maxlines% - u3% to temp$, pic(###)
            errormsg$ = hex(84) & "Number Of Classes Appended:" & temp$
            line% = min(max(0, u3%-1), max(0, maxlines%-15))
            if dups% = 0 then return
            convert dups% to temp$, pic(###)
            errormsg$ = errormsg$ & ".   " & temp$ &                     ~
                                       " Duplicates Were Found & Ignored"
        return

        REM *************************************************************~
            *       I N P U T   M O D E   F O R   P R I N T             *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        print_data
            errormsg$, message$, startuser$, enduser$ = " "

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
            * Prints a report showing Classes tied to each user.        *~
            *************************************************************

            pagenumber% = 0
            pageline% = 987654321
            if startuser$ <> "ALL" then L15120
                readkey$ = all(hex(00))
                enduser$ = all(hex(ff))
                goto L15150
L15120:     str(readkey$,,3) = str(startuser$,,3) addc all(hex(ff))
            if enduser$ = " " then enduser$ = startuser$

L15150:     call "PLOWNEXT" (#1, readkey$, 0%, f1%(1))
                if f1%(1) = 1 then L15210
L15170:         if tag% = 0 then print using L16260
                close printer
                goto inputmode

L15210:     if str(readkey$,,3) > enduser$ then L15170
            if pageline% = 987654321 then print at(05,02); hex(84);      ~
                                                      "Print In Progress"
            get #1, using L15250, descr$
L15250:     FMT XX(3), CH(30)
            startuser$ = str(readkey$,,3)

            str(readkey$,4) = all(hex(00))
L15290:     call "PLOWNEXT" (#2, readkey$, 3%, f1%(2))
                if f1%(2) = 1 then L15370
                gosub form_control
                if tag% = 0 then print using L16260                       ~
                     else pageline% = pageline% - 1
                tag% = 1
                goto L15150

L15370:     get #2, using L15380, code$
L15380:     FMT XX(3), CH(3)
            call "READ100" (#3, "PSCLASSES" & code$, f1%(3))
                if f1%(3) = 1 then L15450
                codedescr$ = "*** NOT ON FILE ***"
                if code$ <> "ALL" then L15480
                codedescr$ = "(Can Start Job For ANY Part)"
                goto L15480
L15450:     get #3, using L15460, codedescr$
L15460:     FMT XX(24), CH(30)

L15480:     gosub form_control
            print using L16320, startuser$, descr$, code$, codedescr$
            tag% = 0
            startuser$, descr$ = " "
            goto L15290

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

L16220: %PAGE ###   PRODUCTION SCHEDULER / PART CROSS REFERENCE LISTING  ~
        ~       DATE: ########

L16260: %+----------+------------------------------+------------+--------~
        ~--------------------+

L16290: %!SCHEDULER !            N A M E           ! PART CLASS !   D E S~
        ~ C R I P T I O N    !

L16320: %!   ###    !##############################!     ###    !########~
        ~####################!

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            lastuser$ = user$
            gosub L31000
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1
                  on fieldnr% gosub L20100          /* USER ID          */
                     return
L20100:     REM DEFAULT/ENABLE FOR USER
            message$ = "Leave User Blank And Press (ENTER) to Search CMS ~
        ~User List."
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
            startuser$ = "ALL"
            message$ = "ALL Will Print all codes On File. To Print Range,~
        ~ Enter The Code To Start With."
                return
L22160:     REM DEFAULT/ENABLE FOR ENDING CODE
            if startuser$ = "ALL" then enabled% = 0
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

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *                L O A D   D A T A                          *~
            *                                                           *~
            * Loads up existing User For Edit                           *~
            *************************************************************

            maxlines% = 0
            readkey$ = user$

        load_lines
            str(readkey$,4) = all(hex(00))
L30110:     call "PLOWNEXT" (#2, readkey$, 3%, f1%(2))
                if f1%(2) = 0 then return
            if maxlines% = allowed% then return
            c%, maxlines% = maxlines% + 1
            get #2, using L30160, code$(c%)    /* Get Line Info */
L30160:     FMT XX(3), CH(3)
            if code$(c%) <> "ALL" then L30200
                codedescr$(c%) = "(Can Start Job For ANY Part)"
                goto L30110
L30200:     call "DESCRIBE"(#3, "PSCLASSES" & code$(c%), codedescr$(c%), ~
                                                              1%, f1%(3))
                if f1%(3) = 1 then L30110
                codedescr$(c%) = hex(94) & "(NO LONGER ON FILE)"
                goto L30110


L31000: REM *************************************************************~
            *                S A V E   D A T A                          *~
            *                                                           *~
            * Write new/modified data to disk.                          *~
            *************************************************************

            REM Write out list for this user...
            readkey$ = user$
            call "DELETE" (#2, user$, 3%)           /* Clear Out */
            if maxlines% = 0 then return    /* Implied Delete */

            REM Check for 'ALL' status...
            search code$() = "ALL" to cursor%() step 3
                if cursor%(1) = 0 then L31170
            code$() = "ALL"
            maxlines% = 1

L31170:     for c% = 1 to maxlines%
            write #2,using L31190, user$, code$(c%), user$, descr$, " "
L31190:     FMT 3*CH(3), CH(30), CH(31)
            next c%
            return

        REM *************************************************************~
            *                      S C R E E N  1                       *~
            *                                                           *~
            * Handles Header Information.                               *~
            *************************************************************

            REM Input Mode Screen Controler
            deffn'101(fieldnr%)
                  pfdescr$(1) ="(1)Start Over                      (13)"&~
                               "Instructions            (15)Print Screen"
                  pfdescr$(2) ="(3)Print Cross Reference           (14)"&~
                               "See Current Schedulers  (16)Exit Program"
                  pfkeys$ = hex(0001030d0f100e)
                  if fieldnr% = 1 then L40160
                     str(pfdescr$(3),,25), str(pfdescr$(2),64) = " "
                     str(pfkeys$,3,1), str(pfkeys$,6,1) = hex(ff)
L40160:           init(hex(8c)) lfac$()

                  REM Common Logic Starts Here...
                  str(pfdescr$(2),63,1)=hex(84) /* Make Sure They See */
                  str(line2$,1%,53%) =  "Last User Managed:"
                  str(line2$,20%,3%) = lastuser$
                  on fieldnr% gosub L40290          /* USER ID          */
                     goto L40360

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40290:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40360:     accept                                                       ~
               at (01,02), "Manage Production Scheduler/Part Cross Refere~
        ~nce",                                                            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "PRODUCTION SCHEDULER CODE (USERID)",                  ~
               at (06,38), fac(lfac$( 1)), user$                , ch(03),~
                                                                         ~
               at (22,02), fac(hex(a4)),   message$             , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L40570
                  call "MANUAL" ("JBRELXRF")
                  goto L40360

L40570:        if keyhit% <> 14 then L40610
                  call "PLOWCODE" (#2, user$, " ", -3%, -0.30, f1%(2))
                  goto L40360

L40610:        if keyhit% <> 15 then L40650
                  call "PRNTSCRN"
                  goto L40360

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
                               "     (13)Instructions   (15)Print Screen"
                  pfdescr$(2) ="                                       "&~
                               "                           (16)Edit Mode"
                  pfkeys$ = hex(00010d0f10)
                  init(hex(8c)) lfac$()
                  hfac$ = hex(ac)
                  afac$ = hex(9c)
                  goto L41530

            REM Edit Mode Screen Controler
            deffn'112(fieldnr%)
                  pfdescr$(1) ="(1)Start Over  (4)Prev                 "&~
                               "        (11)Add Codes   (15)Print Screen"
                  pfdescr$(2) ="(2)First       (5)Next (10)Copy Schedul"&~
                               "er XXX  (12)Remove Code    (16)Save Data"
                  pfkeys$ = hex(0001020405ff0a0b0c0d0f10)
                  init(hex(86)) lfac$()
                  hfac$ = hex(ae)
                  afac$ = hex(81)
                  if fieldnr% = 0 then L41380

                  REM Adjust PF Keys Available If Modifing A Line...
                  init(hex(8c)) lfac$()
                  hfac$ = hex(ac)
                  afac$ = hex(8c)
                  pfdescr$(1) ="(1)Start Program Over                  "&~
                               "     (13)Instructions   (15)Print Screen"
                  pfdescr$(2) ="(ENTER) To Continue"
                  pfkeys$ = hex(00010d0f)
L41380:           goto L41530

            REM Delete Mode Screen Controler
            deffn'122(fieldnr%)
                  pfdescr$(1) ="(1)Abort Delete And Return To Edit Mode"&~
                               "                        (13)Instructions"
                  pfdescr$(2) ="(ENTER) Remove Flash Code From List    "&~
                               "                        (15)Print Screen"
                  pfkeys$ = hex(00010d0f10)
                  init(hex(8c)) lfac$()
                  hfac$ = hex(ac)
                  afac$ = hex(9c)
                  lfac$(fieldnr%), jfac$(fieldnr%) = hex(94)
                  goto L41640

L41530:     REM Common Logic Here...
            if fieldnr% > 0 then lfac$(fieldnr%) = hex(81)
            str(pfdescr$(2),66,1)=hex(84) /* Make Sure They See */
            init(hex(9c)) jfac$()
            for i% = 1 to 15
               if code$(line%+i%)<>" " then jfac$(i%) = hex(8c)
            next i%
            if fieldnr% <> 0 then jfac$(fieldnr%) = hex(8c)
            str(line2$,1%,53%) = "Scheduler Part Class Codes this Sched"&~
                      "uler may release"

L41640:     accept                                                       ~
               at (01,02), "Manage Production Scheduler/Part Cross Refere~
        ~nce",                                                            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), "User Id.:",                                  ~
               at (03,12), fac(hex(84)), user$                  , ch(03),~
               at (03,16), fac(hex(8c)), descr$                 , ch(30),~
               at (03,58), "Consisting Of XXX Codes",                    ~
               at (03,72), fac(hex(8c)), maxlines%            , pic(###),~
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
               at (06,13), fac(lfac$( 1)), code$(line%+01)      , ch(03),~
               at (07,13), fac(lfac$( 2)), code$(line%+02)      , ch(03),~
               at (08,13), fac(lfac$( 3)), code$(line%+03)      , ch(03),~
               at (09,13), fac(lfac$( 4)), code$(line%+04)      , ch(03),~
               at (10,13), fac(lfac$( 5)), code$(line%+05)      , ch(03),~
               at (11,13), fac(lfac$( 6)), code$(line%+06)      , ch(03),~
               at (12,13), fac(lfac$( 7)), code$(line%+07)      , ch(03),~
               at (13,13), fac(lfac$( 8)), code$(line%+08)      , ch(03),~
               at (14,13), fac(lfac$( 9)), code$(line%+09)      , ch(03),~
               at (15,13), fac(lfac$(10)), code$(line%+10)      , ch(03),~
               at (16,13), fac(lfac$(11)), code$(line%+11)      , ch(03),~
               at (17,13), fac(lfac$(12)), code$(line%+12)      , ch(03),~
               at (18,13), fac(lfac$(13)), code$(line%+13)      , ch(03),~
               at (19,13), fac(lfac$(14)), code$(line%+14)      , ch(03),~
               at (20,13), fac(lfac$(15)), code$(line%+15)      , ch(03),~
                                                                         ~
               at (06,25), fac(hex(8c)), codedescr$(line%+01)   , ch(32),~
               at (07,25), fac(hex(8c)), codedescr$(line%+02)   , ch(32),~
               at (08,25), fac(hex(8c)), codedescr$(line%+03)   , ch(32),~
               at (09,25), fac(hex(8c)), codedescr$(line%+04)   , ch(32),~
               at (10,25), fac(hex(8c)), codedescr$(line%+05)   , ch(32),~
               at (11,25), fac(hex(8c)), codedescr$(line%+06)   , ch(32),~
               at (12,25), fac(hex(8c)), codedescr$(line%+07)   , ch(32),~
               at (13,25), fac(hex(8c)), codedescr$(line%+08)   , ch(32),~
               at (14,25), fac(hex(8c)), codedescr$(line%+09)   , ch(32),~
               at (15,25), fac(hex(8c)), codedescr$(line%+10)   , ch(32),~
               at (16,25), fac(hex(8c)), codedescr$(line%+11)   , ch(32),~
               at (17,25), fac(hex(8c)), codedescr$(line%+12)   , ch(32),~
               at (18,25), fac(hex(8c)), codedescr$(line%+13)   , ch(32),~
               at (19,25), fac(hex(8c)), codedescr$(line%+14)   , ch(32),~
               at (20,25), fac(hex(8c)), codedescr$(line%+15)   , ch(32),~
                                                                         ~
               at (22,02), fac(hex(a4)), message$               , ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(1)            , ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(2)            , ch(79),~
               at (24,44), fac(afac$),   append$                , ch(03),~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L42350
                  call "MANUAL" ("JBRELXRF")
                  goto L41640

L42350:        if keyhit% <> 15 then L42390
                  call "PRNTSCRN"
                  goto L41640

L42390:        if fieldnr% <> 0 then return
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
                               "     (13)Instructions   (15)Print Screen"
                  pfdescr$(2) ="                                       "&~
                               "                (16)Cancel Print Request"
                  pfkeys$ = hex(00010d0f10)
                  init(hex(8c)) lfac$()
                  str(pfdescr$(2),55,1)=hex(84) /* Make Sure They See */
                  str(line2$,1%,53%) = "Print Codes"
                  on fieldnr% gosub L43240,         /* START CODE       */~
                                    L43240          /* END CODE         */
                     goto L43310

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L43240:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L43310:     accept                                                       ~
               at (01,02), "Print User/Part Cross Reference",            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Starting User Id.",                                   ~
               at (06,30), fac(lfac$( 1)), startuser$           , ch(03),~
               at (07,02),                                               ~
                  "Ending User Id.",                                     ~
               at (07,30), fac(lfac$( 2)), enduser$             , ch(03),~
                                                                         ~
               at (22,02), fac(hex(a4)),   message$             , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L43540
                  call "MANUAL" ("JBRELXRF")
                  goto L43310

L43540:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L43310

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50120          /* USER ID          */
                     return
L50120: REM TEST DATA FOR USER ID....
            call "GETCODE" (#1, user$, descr$, 0%, 1, f1%(1))
                if f1%(1%) = 1% then L50180

        REM  Scheduler not in USERLCMS, check JBRELUSR if there, delete...
            readkey$ = str(user$) & hex(000000)
            call "PLOWNXT1" (#2, readkey$, 3%, f1%(2%))
                if f1%(2%) = 1% then L50168
            errormsg$ = "Scheduler " & user$ & " is not a valid CMS user"
            return

L50168:     call "DELETE" (#2, readkey$, 3%)
            errormsg$ = "Scheduler " & user$ & " is not a valid CMS use"&~
                        "r.  Now deleted from the Scheduler file."
            return

L50180:     gosub L30000
            if maxlines% = 0 then return
            return clear all
            goto editmode

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152(t%)
            errormsg$ = " "

            REM TEST DATA FOR CODE
            if code$(t%) <> "ALL" then L51130
                codedescr$(t%) = "(Can Start Job For ANY Part)"
                return
L51130:     readkey$ = "PSCLASSES" & code$(t%)
            codedescr$(t%) = hex(06) & "Select Scheduler Part Class"
            f1%(3) = -(max(1,t%-line%) +5)
            call "PLOWCODE" (#3, readkey$, codedescr$(t%),9%,0.3,f1%(3))
                if f1%(3) = 0% then L51210
            code$(t%) = str(readkey$,10)
            call "PUTPAREN" (codedescr$(t%))
            goto test_for_dup
L51210:         errormsg$ = "Undefined Part Class"
                return

            test_for_dup /* Test For Duplicate Entries */
            errormsg$ = " "
            temp$ = code$(t%)
            code$(t%) = " "
            search code$() = str(temp$,,3) to cursor%() step 3
            code$(t%) = temp$
            if cursor%(1) = 0 then return
                errormsg$ = "This Code Is Already Included In This "     ~
                                                           & "Users List"
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
            if enduser$ = " " then return
            if startuser$ > enduser$ then errormsg$ = "Ending Code Can't ~
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
