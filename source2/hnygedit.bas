        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y   GGG   EEEEE  DDDD   IIIII  TTTTT   *~
            *  H   H  NN  N  Y   Y  G      E      D   D    I      T     *~
            *  HHHHH  N N N   YYY   G GGG  EEEE   D   D    I      T     *~
            *  H   H  N  NN    Y    G   G  E      D   D    I      T     *~
            *  H   H  N   N    Y     GGG   EEEEE  DDDD   IIIII    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYGEDIT - SEE ALL PARTS MARKED WITH GENERIC REFERNCE     *~
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
            * 09/14/83 ! ORIGINAL                                 ! KEN *~
            * 05/13/87 ! Std Costing Changes                      ! ERN *~
            * 11/04/87 ! Allowed Entry of GENERIC Description.    !     *~
            *          ! Converted to Standard Sub-routines.      ! DAW *~
            * ??/??/?? ! Added Plowcode                           ! ??? *~
            * 06/29/88 ! More Clean-up                            ! KAB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            header$79,                   /* Header                     */~
            readkey$60,                  /* Misc. Read Key             */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            savedescr$30,                /* Temporary Description      */~
            gendescr$60,                 /* GENERIC DESCRIPTION        */~
            generic$16,                  /* GENERIC PART DESIGNATION   */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            line2$79,                    /* Second Line of Screen      */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            part$(20)25,                 /* Part codes                 */~
            partdescr$(20)32,            /* Part Descriptions          */~
            plowkey$41                   /* Misc Plowkey String        */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.01.00 07/01/88 General Release R5.01.00        "
        REM *************************************************************

            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! HNYGENER ! generic part xref                        *~
            * # 2 ! HNYMASTR ! Inventory Master File                    *~
            *************************************************************~

            select # 1, "HNYGENER", varc, indexed, recsize =  100,       ~
                        keypos =   17, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  41          ~

            select # 2, "HNYMASTR", varc, indexed, recsize =  900,       ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (# 1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
            call "OPENFILE" (# 2, "SHARE", f2%( 2), rslt$( 2), axd$( 2))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            inpmessage$ = "Enter '?' to View List of Generic Codes."
            str(line2$,62) = "HNYGEDIT: " & str(cms2v$,,8)
            edtmessage$ = "To Modify GENERIC Description, Position Cursor~
        ~ and Press [RETURN]."

            header$="Part Code                     Description"
            init(hex(00)) generic$

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init (" ")  errormsg$, gendescr$, part$(), partdescr$()
            max% = 99%

L10090:     for fieldnr% = 1 to 1
                gosub'051(fieldnr%)      /* Default/Enables */
                      if enabled% = 0 then L10180
L10120:         gosub'101(fieldnr%)      /* Screen 1        */
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  8 then L10090
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%)      /* Test Data       */
                      if errormsg$ <> " " then L10120
L10180:     next fieldnr%

            savedescr$ = gendescr$

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editmode
L11070:     gosub'111(0%)                /* Screen 2        */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then gosub startarray
                  if keyhit%  =  5 then gosub continuearray
                  if keyhit%  =  8 then       L10090
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11070
            fieldnr% = cursor%(1) - 5
            if fieldnr% <> 2% then L11070

L11170:     gosub'111(fieldnr%)          /* Screen 2        */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11170
            gosub'151(fieldnr%)          /* Test Data       */
                  if errormsg$ <> " " then L11170
            goto L11070

        startarray
            init (hex(00)) readkey$
            str(readkey$,1,16) = generic$
            max% = 99%
        continuearray
            if max% < 10% then return
            init (" ") part$(), partdescr$()
            max%=0%
L11310:     if max%=10% then return
            call "PLOWALTS" (#1,readkey$, 1%,16%,f1%(1))
            if f1%(1)=0 then L11400
            max%=max%+1
            get #1, using L11360, part$(max%)
L11360:         FMT XX(16), CH(25)
            call "DESCRIBE" (#2, part$(max%), partdescr$(max%),0%,f1%(2))
            goto L11310

L11400:     partdescr$(max% + 1%) = hex(8c) & "* * * End of File * * *"
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if savedescr$ = gendescr$ then inputmode
            init (hex(00)) readkey$
            str(readkey$,1,16)=generic$
L19140:     call "PLOWAL1" (#1, readkey$, 1%, 16%, f1%(1))
               if f1%(1) = 0% then inputmode
            put #1 using L19170, gendescr$
L19170:         FMT POS(42), CH(30)
            rewrite #1
            goto L19140

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* GENERIC          */~
                                    L20200          /* GEN DESCR        */
                     return

L20100: REM Default/Enable for GENERIC PART DESIGNATION
            call "PLOWALTS" (#1,generic$,1%,0%,f1%(1))
            if f1%(1)=0 then generic$,gendescr$ =" " else                ~
                get #1, using L20104, gendescr$
L20104:              FMT XX(41), CH(30)
                return

L20200: REM Default/Enable for GENERIC DESCRIPTION
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

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40150,         /* GENERIC          */~
                                    L40150          /* GEN DESCR        */
                     goto L40220

L40150:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return

L40220:     accept                                                       ~
               at (01,02),                                               ~
                  "GENERIC CROSS REFERENCES",                            ~
               at (01,66), "TODAY:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Generic Part Designation",                            ~
               at (06,30), fac(lfac$( 1)), generic$             , ch(16),~
               at (07,02),                                               ~
                  "Generic Description",                                 ~
               at (07,30), fac(hex(8d)),   gendescr$            , ch(30),~
                                                                         ~
               at (20,02), fac(hex(ac)),   inpmessage$          , ch(79),~
               at (21,02),                                               ~
                  "(1)Start Over",                                       ~
               at (21,65),                                               ~
                  "(13)Instructions",                                    ~
               at (22,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (23,40),                                               ~
                  "(8)Next Reference",                                   ~
               at (23,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(0001080d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40570
                  call "MANUAL" ("HNYGEDIT")
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
                  init(hex(84)) lfac$() : lfac$(2) = hex(86)
                  if fieldnr% <> 0% then init(hex(8c)) lfac$()
                  on fieldnr% gosub L41130,         /* GENERIC          */~
                                    L41130          /* GEN DESCR        */
                     goto L41170

L41130:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return

L41170:     accept                                                       ~
               at (01,02),                                               ~
                  "GENERIC CROSS REFERENCES",                            ~
               at (01,66), "TODAY:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Generic Part Designation",                            ~
               at (06,30), fac(lfac$( 1)), generic$             , ch(16),~
               at (07,02),                                               ~
                  "Generic Description",                                 ~
               at (07,30), fac(lfac$( 2)), gendescr$            , ch(30),~
               at (08,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (09,02), fac(hex(84)), part$(1)               , ch(25),~
               at (10,02), fac(hex(84)), part$(2)               , ch(25),~
               at (11,02), fac(hex(84)), part$(3)               , ch(25),~
               at (12,02), fac(hex(84)), part$(4)               , ch(25),~
               at (13,02), fac(hex(84)), part$(5)               , ch(25),~
               at (14,02), fac(hex(84)), part$(6)               , ch(25),~
               at (15,02), fac(hex(84)), part$(7)               , ch(25),~
               at (16,02), fac(hex(84)), part$(8)               , ch(25),~
               at (17,02), fac(hex(84)), part$(9)               , ch(25),~
               at (18,02), fac(hex(84)), part$(10)              , ch(25),~
                                                                         ~
               at (09,30), fac(hex(84)), partdescr$(1)          , ch(32),~
               at (10,30), fac(hex(84)), partdescr$(2)          , ch(32),~
               at (11,30), fac(hex(84)), partdescr$(3)          , ch(32),~
               at (12,30), fac(hex(84)), partdescr$(4)          , ch(32),~
               at (13,30), fac(hex(84)), partdescr$(5)          , ch(32),~
               at (14,30), fac(hex(84)), partdescr$(6)          , ch(32),~
               at (15,30), fac(hex(84)), partdescr$(7)          , ch(32),~
               at (16,30), fac(hex(84)), partdescr$(8)          , ch(32),~
               at (17,30), fac(hex(84)), partdescr$(9)          , ch(32),~
               at (18,30), fac(hex(84)), partdescr$(10)         , ch(32),~
                                                                         ~
               at (20,02), fac(hex(ac)),   edtmessage$          , ch(79),~
               at (21,02),                                               ~
                  "(1)Start Over",                                       ~
               at (21,65),                                               ~
                  "(13)Instructions",                                    ~
               at (22,02),                                               ~
                  "(2)First Page",                                       ~
               at (22,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (23,24),                                               ~
                  "(5)Next Page",                                        ~
               at (23,40),                                               ~
                  "(8)Next Reference",                                   ~
               at (23,65),                                               ~
                  "(16)Save Data",                                       ~
                                                                         ~
               keys(hex(00010205080d0f10)),                              ~
               key (keyhit%)

               if keyhit% <> 13 then L42000
                  call "MANUAL" ("HNYGEDIT")
                  goto L41170

L42000:        if keyhit% <> 15 then L42020
                  call "PRNTSCRN"
                  goto L41170

L42020:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* GENERIC          */~
                                    L50300          /* GEN DESCR        */
                     return

L50100: REM Test Data for GENERIC PART DESIGNATION
            if generic$ = "?" then L50230
            call "PLOWALTS" (#1,str(generic$,,16) & hex(00),1%,16%,f1%(1))
L50130:     if f1%(1) = 0 then L50200
                get #1, using L50150, generic$, gendescr$
L50150:              FMT CH(16), POS(42), CH(30)
                return clear
                return clear
                gosub startarray
                goto editmode
L50200:     errormsg$="Enter or Select Generic Code to Review."
                return

L50230:     init(hex(00)) plowkey$
            gendescr$ = hex(06) & "Select Generic Description to View."
            call "PLOWCODE" (#1, plowkey$, gendescr$, -16%, -1.41, f1%(1))
            goto L50130

L50300: REM Test Data for GENERIC DESCRIPTION
                return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN.                      *~
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
