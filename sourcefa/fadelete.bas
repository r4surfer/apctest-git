        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  FFFFF   AAA   DDDD   EEEEE  L      EEEEE  TTTTT  EEEEE   *~
            *  F      A   A  D   D  E      L      E        T    E       *~
            *  FFFF   AAAAA  D   D  EEEE   L      EEEE     T    EEEE    *~
            *  F      A   A  D   D  E      L      E        T    E       *~
            *  F      A   A  DDDD   EEEEE  LLLLL  EEEEE    T    EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FADELETE - THIS PROGRAM ALLOWS DELETION OF FIXED ASSETS   *~
            *            AND DESCRIPTIVE TEXT ONE ASSET AT A TIME.      *~
            *            A REPORT PRINTS EACH TIME AN ASSET IS DELETED. *~
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
            * 07/16/84 ! ORIGINAL                                 ! NLH *~
            * 09/16/88 ! File Mods & Screen Standardization       ! RJM *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            asset_code$10,               /* ASSET CODE                 */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            descr1$30,                   /* DESCRIPTION LINE 1         */~
            descr2$30,                   /* DESCRIPTION LINE 2         */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            hdrdate$45,                  /* HEADING DATE               */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lastasset$10,                /* LAST ASSET DELETED         */~
            line2$79,                                                    ~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            typedescr$11                 /* RECORD TYPE DESCRIPTION    */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.00 01/19/90 CMS2 / CMS-I Merge              "
        REM *************************************************************~
            MAT F2% = CON

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
            * #07 ! FATEXT   ! Fixed Assets Descriptive Text File       *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "FAMASTER",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 900,                                   ~
                        keypos = 120,  keylen = 10,                      ~
                        alt key  1, keypos = 58, keylen =   1, dup

            select #07, "FATEXT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 125,                                   ~
                        keypos = 1,    keylen = 14                       ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#01, "SHARE", f2%(1 ), rslt$(1 ), axd$(1 ))
            call "OPENFILE" (#07, "SHARE", f2%(7 ), rslt$(7 ), axd$(7 ))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            line% = 1000%
            pagenr% = 0%
            str(line2$,62) = "FADELETE: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, asset_code$

            for fieldnr% = 1% to  1%
                gosub'051(fieldnr%)
                      if enabled% = 0% then L10180
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% and fieldnr% = 1 then L65000
                      if keyhit% <>  0% then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10180:         next fieldnr%

        REM *************************************************************~
            *        CONFIRM THAT THEY WANT TO DELETE THE ASSET         *~
            *                                                           *~
            *                                                           *~
            *************************************************************

            inpmessage$ = "To Delete This Asset, Press P.F. Key 16, Else ~
        ~Press P.F. Key 1 To Start Over"

L11090:     gosub L41000
                  if keyhit% =  1 then gosub startover
                  if keyhit% = 16 then       L19000
                  goto L11090

L19000: REM *************************************************************~
            *                                                           *~
            *                  DELETE ROUTINE                           *~
            *                                                           *~
            *************************************************************

            call "READ101" (#01, asset_code$, f1%(1)) /* DELETE ASSET */
            if f1%(1) <> 0 then delete #01

            call "DELETE" (#07, asset_code$, 10%)    /* DELETE TEXT */

            lastasset$ = asset_code$

            gosub L19500

            print using L19920, asset_code$, descr1$, descr2$, type_code$,~
                               typedescr$
            goto inputmode


L19500: REM PRINT PAGE HEADING
            line% = line% + 1
            if line% < 55 then return
            select printer (134)
            call "DATE" addr ("HD", hdrdate$)
            pagenr% = pagenr% + 1%
            print page
            print using L19800, pagenr%, hdrdate$
            print
            print using L19880
            print using L19840
            print
            line% = 5%
            return


L19800: %PAGE #####              DELETED FIXED ASSETS REPORT             ~
        ~                       ##########################################~
        ~###

L19840: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~---

L19880: %ASSET CODE     DESCRIPTION LINE 1                 DESCRIPTION LI~
        ~NE 2                TYPE


L19920: %##########     ##############################     ##############~
        ~################     # ###########

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
                inpmessage$ = "ENTER (RETURN) TO FIND ASSET CODES"
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
            *                                                           *~
            *                  LOAD ASSET DATA                          *~
            *                                                           *~
            *************************************************************

            get #01, using L35030, descr1$, descr2$, type_code$
            if type_code$ = "1" then typedescr$ = "(GROUP)"
            if type_code$ = "2" then typedescr$ = "(SUB-GROUP)"
            if type_code$ = "3" then typedescr$ = "(ASSET)"

            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

L35030: FMT                      /* FILE: FAMASTER                     */~
            XX(129),             /* SKIP THIS SPACE                    */~
            CH(30),              /* Fixed Asset Description Line #1    */~
            CH(30),              /* Fixed Asset Description Line #2    */~
            CH(1)                /* Fixed Asset Record Type Code       */

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()

                  put str(line2$,,60) using L40076, lastasset$
L40076: %Last Asset: ##########
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
                  "DELETE FIXED ASSETS",                                 ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "ASSET CODE",                                          ~
               at (06,30), fac(lfac$( 1)), asset_code$          , ch(10),~
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

               if keyhit% <> 13 then L40530
                  call "MANUAL" ("FADELETE")
                  goto L40210

L40530:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40210

L41000: REM *************************************************************~
            *                  CONFIRM SCREEN                           *~
            *                                                           *~
            *                                                           *~
            *************************************************************

            init(hex(84)) lfac$()

L41210:     accept                                                       ~
               at (01,02),                                               ~
                  "DELETE FIXED ASSETS",                                 ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "ASSET CODE",                                          ~
               at (06,30), fac(lfac$( 1)), asset_code$          , ch(10),~
               at (07,02),                                               ~
                  "DESCRIPTION #1",                                      ~
               at (07,30), fac(lfac$( 2)), descr1$              , ch(30),~
               at (08,02),                                               ~
                  "DESCRIPTION #2",                                      ~
               at (08,30), fac(lfac$( 3)), descr2$              , ch(30),~
               at (09,02),                                               ~
                  "RECORD TYPE",                                         ~
               at (09,30), fac(lfac$( 4)), type_code$           , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,60),                                               ~
                  "(13)Instructions",                                    ~
               at (23,60),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,60),                                               ~
                  "(16)DELETE This Asset",                               ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41530
                  call "MANUAL" ("FADELETE")
                  goto L41210

L41530:        if keyhit% <> 15 then L41570
                  call "PRNTSCRN"
                  goto L41210

L41570:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               u3% = u3%
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
            if asset_code$ <> " " then L50110
            call "GETCODE" (#01, asset_code$, " ", 0%, 0, f1%(1))
            if f1%(1) = 0 then L50130

L50110:     call "READ100" (#01, asset_code$, f1%(1))
            if f1%(1) <> 0 then L50200
L50130:     errormsg$ = "ASSET NOT ON FILE: " & asset_code$
                return
L50200:     gosub L30000
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
