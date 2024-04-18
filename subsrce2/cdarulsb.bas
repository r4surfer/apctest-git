        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   CCC   DDDD    AAA   RRRR   U   U  L       SSS   BBBB    *~
            *  C   C  D   D  A   A  R   R  U   U  L      S      B   B   *~
            *  C      D   D  AAAAA  RRRR   U   U  L       SSS   BBBB    *~
            *  C   C  D   D  A   A  R   R  U   U  L          S  B   B   *~
            *   CCC   DDDD   A   A  R   R   UUU   LLLLL   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CDARULSB - Manage Validation Rules For Host Based Data    *~
            *            collection routines.  If MODE% is not zero,    *~
            *            then subroutine acts as displayer only.        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/13/94 ! Original                                 ! HES *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

            sub "CDARULSB" (defcode1$, mode%, #1, #3, #4)

        dim                                                              ~
            action$1,                    /* Action                     */~
            bok$1,                       /* Blank Ok Flag              */~
            code$8,                      /* Validation Code            */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            defcode$8,                   /* Default Validation Code    */~
            defcode1$8,                  /* Default Validation Code    */~
            descr$30,                    /* Description                */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            field1$(6)25,                /* Field 1                    */~
            field2$(6)25,                /* Field 2                    */~
            field3$(6)25,                /* Field 3                    */~
            field4$(6)25,                /* Field 4                    */~
            hdr$(2)79,                   /* PLOWCODE Argument          */~
            i$(24)80,                    /* Screen Image               */~
            incl(1),                     /* PLOWCODE Argument          */~
            incl$(1)3,                   /* PLOWCODE Argument          */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            title$(6)53,                 /* Screen Sub Titles          */~
            type$(6)1,                   /* Type                       */~
            userid$3                     /* Current User Id            */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! CDARULES ! Host CDA Field Validation Rules          *~
            * #02 ! GENCODES ! Control System Codes File                *~
            * #03 ! CDAPGLIN ! CDA Host Prompt Set Detail File          *~
            * #04 ! CDAPGMAS ! CDA Host Prompt Set Header File          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #02, "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1,  keylen = 24

            defcode$ = defcode1$
            if userid$ <> " " then L10000
               call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "CDARULSB: " & str(cms2v$,,8)
            title$(1) = "Alpha Range"
            title$(2) = "File      Table       Validation"
              str(title$(2),9%,2%) = hex(84ac)
              str(title$(2),21%,2%) = hex(84ac)
            title$(3) = "Numeric Range            Decimals"
              str(title$(3),24%,2%) = hex(84ac)
            title$(4) = "Alpha Picture"
            title$(5) = "Date Range (Days from Today)"
            title$(6) = "Time"

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            if mode% = 0% then L10130
               call "READ100" (#1, defcode$, f1%(1%))
               if f1%(1%) = 0% then end
                  gosub L30000
                  goto editpg1
L10130:     editmode% = 0%

            for fieldnr% = 1% to 10%
L10160:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10300
L10180:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10260
L10210:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10180
                         if fieldnr% = 1% then L10160
                         goto L10210
L10260:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10180
                gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10180
L10300:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            editmode% = 1%
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  errormsg$ = " "
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 12% then gosub delete_validation
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11150:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% > 3% then fieldnr% = 0%
            if fieldnr% = 3% and cursor%(2%) > 35% then fieldnr% = 4%
            if cursor%(1%) = 12% and cursor%(2%) > 45% then fieldnr% = 10%
            if cursor%(1%) = 13% and cursor%(2%) > 45% then fieldnr% = 10%
            if cursor%(1%) = 15% and cursor%(2%) > 45% then fieldnr% = 11%
            if fieldnr% > 0% then L11310
               if cursor%(1%) =  9% then fieldnr% = 6%
               if cursor%(1%) = 10% then fieldnr% = 6%
               if cursor%(1%) = 12% then fieldnr% = 7%
               if cursor%(1%) = 13% then fieldnr% = 7%
               if cursor%(1%) = 15% then fieldnr% = 8%
               if cursor%(1%) = 16% then fieldnr% = 8%
               if cursor%(1%) = 18% then fieldnr% = 9%
               if cursor%(1%) = 19% then fieldnr% = 9%

L11310:     if fieldnr% < 2% or fieldnr% > 11% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11350:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11350
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11350
                  lastfieldnr% = fieldnr%
            goto L11150

        delete_validation
            call "REDALT0" (#3, code$, 1%, f1%(3%))
            if f1%(3) = 0% then L11490
               errormsg$ = "Can't Delete, Used By Prompt Set: "&key(#3,0)
               return

L11490:     key% = 2%
            call "ASKUSER" (key%, "Delete", "To Delete This Validation Ro~
        ~utine, Press PF(28)", " ", "Press RETURN To Cancel Delete Request~
        ~")
            if key% <> 28% then return
               call "READ101" (#1, code$, f1%(1))
               if f1%(1) <> 0% then delete #1
               return clear all
               goto L10000

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if mode% <> 0% then end
               gosub dataput
               goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20220,         /* Validation Code        */~
                              L20270,         /* Description            */~
                              L20300,         /* Is Blank Ok?           */~
                              L20340,         /* Action                 */~
                              L20380,         /* Type                   */~
                              L20410,         /* Validation Type 1      */~
                              L20490,         /* Validation Type 2      */~
                              L20590,         /* Validation Type 3      */~
                              L20710,         /* Validation Type 4      */~
                              L20790,         /* Validation Type 5      */~
                              L20890          /* Validation Type 6      */
            if mode% <> 0% then enabled% = 0%
            return

L20220: REM Def/Enable Validation Code             CODE$
            if code$ = " " then code$ = defcode$
            defcode$ = " "
            return

L20270: REM Def/Enable Description                 DESCR$
            return

L20300: REM Def/Enable Is Blank Ok?                BOK$
            if bok$ = " " then bok$ = "N"
            return

L20340: REM Def/Enable Action                      ACTION$
            if action$ = " " then action$ = "E"
            return

L20380: REM Def/Enable Type                        TYPE$()
            return

L20410: REM Def/Enable Validation Type 1           FIELD1$,FIELD2$
            if editmode% = 1% and type$(1%) <> " " then return
            if editmode% = 1% then gosub L20920
               if type$(1%) = " " then L20460
                  return
L20460:        enabled% = 0%
               return

L20490: REM Def/Enable Validation Type 2           FIELD1$,FIELD2$,FIELD3$
            if editmode% = 1% and type$(2%) <> " " then return
            if editmode% = 1% then gosub L20920          /* FIELD4$ */
               if type$(2%) = " " then L20560
                  field3$(2%) = "V"
                  field4$(2%) = "Y"
                  return
L20560:        enabled% = 0%
               return

L20590: REM Def/Enable Validation Type 3           FIELD1$,FIELD2$,FIELD3$
            if editmode% = 1% and type$(3%) <> " " then return
            if editmode% = 1% then gosub L20920          /* FIELD4$ */
               if type$(3%) = " " then L20680
                  field1$(3%) = "-999999999"
                  field2$(3%) = "9999999999"
                  field3$(3%) = "0"
                  field4$(3%) = "0"
                  return
L20680:        enabled% = 0%
               return

L20710: REM Def/Enable Validation Type 4           FIELD1$,FIELD2$,FIELD3$
            if editmode% = 1% and type$(4%) <> " " then return
            if editmode% = 1% then gosub L20920
               if type$(4%) = " " then L20760
                  return
L20760:        enabled% = 0%
               return

L20790: REM Def/Enable Validation Type 5           FIELD1$,FIELD2$
            if editmode% = 1% and type$(5%) <> " " then return
            if editmode% = 1% then gosub L20920
               if type$(5%) = " " then L20860
                  field1$(5%) = "999"
                  field2$(5%) = "999"
                  return
L20860:        enabled% = 0%
               return

L20890: REM Def/Enable Validation Type 6           NO FIELDS!
            enabled% = 0%

L20920:     field1$(), field2$(), field3$(), field4$(), type$() = " "
            type$(fieldnr%-5%) = "X"
            return   /* GOSUBed from multi places... */

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28120
                inpmessage$ = edtmessage$
                if mode%<>0% then inpmessage$="Modification Not Allowed"
                return

L28120
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Validation Code.  Leave Blank & Press RETURN To Search.",~
         "Enter Description                                            ",~
         "Enter 'Y' if a BLANK response is to be considered valid",      ~
         "Enter Validation Action: 'E' = Error, 'W' = Warning Only     ",~
         "Select Desired Validation (ONE ONLY) By Placing Non-Blank Chara~
        ~cter in a Box",                                                  ~
         "Enter Alpha Range To Be Used For Validation                  ",~
         "Enter File Name. For GENCODES, enter TABLE. B=Blank ok,V=Valid ~
        ~only,N=NOT valid",                                               ~
         "Enter Numeric Range For Validation, # Of Decimal Places To Carr~
        ~y",                                                              ~
         "'#' = Any character allowed, '+' = Numeric only, any other char~
        ~acter is constnt",                                               ~
         "Enter Date Range By Indicated Number of Days Variance (+ or -) ~
        ~From System date",                                               ~
         "DUMMY"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      action$, code$, descr$, field1$(), field2$(), bok$,~
                      field3$(), field4$(), type$()
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            get #1, using L31120, code$, descr$, action$, bok$, x%,       ~
                    field1$(1%), field2$(1%), field3$(1%), field4$(1%)
            type$(x%) = "X"
            if x% = 1% then return
               field1$(x%) = field1$(1%)
               field2$(x%) = field2$(1%)
               field3$(x%) = field3$(1%)
               field4$(x%) = field4$(1%)
            field1$(1%), field2$(1%), field3$(1%), field4$(1%) = " "
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "READ101" (#1, code$, f1%(1))
            if f1%(1) <> 0% then delete #1

            x% = pos(type$()<>" ")
            write #1, using L31120, code$, descr$, action$, bok$, x%,     ~
                  field1$(x%), field2$(x%), field3$(x%), field4$(x%), " "
L31120:     FMT CH(8), CH(30), 2*CH(1), PIC(0), 4*CH(25), CH(59)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40240,         /* Validation Code   */   ~
                                L40230,         /* Description       */   ~
                                L40240,         /* Is Blank OK?      */   ~
                                L40240,         /* Action            */   ~
                                L40240,         /* Type              */   ~
                                L40240,         /* Validation Type 1 */   ~
                                L40240,         /* Validation Type 2 */   ~
                                L40240,         /* Validation Type 3 */   ~
                                L40240,         /* Validation Type 4 */   ~
                                L40240          /* Validation Type 5 */
              goto L40270

L40230:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40240:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40270:     accept                                                       ~
               at (01,02),                                               ~
                  "Create/Manage Host CDA Validation Rules",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Validation Code",                            ~
               at (04,20), fac(lfac$( 1)), code$                , ch(08),~
                                                                         ~
               at (06,02), "Description",                                ~
               at (06,20), fac(lfac$( 2)), descr$               , ch(30),~
                                                                         ~
               at (07,02), "Is BLANK Allowed?",                          ~
               at (07,20), fac(lfac$( 3)), bok$                 , ch(01),~
                                                                         ~
               at (07,35), "Action:",                                    ~
               at (07,43), fac(lfac$( 4)), action$              , ch(01),~
                                                                         ~
               at (09,02), fac(lfac$( 5)), type$(1)             , ch(01),~
               at (12,02), fac(lfac$( 5)), type$(2)             , ch(01),~
               at (15,02), fac(lfac$( 5)), type$(3)             , ch(01),~
               at (18,02), fac(lfac$( 5)), type$(4)             , ch(01),~
               at (12,47), fac(lfac$( 5)), type$(5)             , ch(01),~
               at (15,47), fac(lfac$( 5)), type$(6)             , ch(01),~
                                                                         ~
               at (09,04), fac(hex(ac)),   title$(1),                    ~
               at (10,04), fac(lfac$( 6)), field1$(1%)          , ch(25),~
               at (10,32), fac(lfac$( 6)), field2$(1%)          , ch(25),~
                                                                         ~
               at (12,04), fac(hex(ac)),   str(title$(2),,33%),          ~
               at (13,04), fac(lfac$( 7)), field1$(2%)          , ch(08),~
               at (13,14), fac(lfac$( 7)), field2$(2%)          , ch(09),~
               at (13,26), fac(lfac$( 7)), field3$(2%)          , ch(01),~
               at (13,28), "Lookup?",                                    ~
               at (13,36), fac(lfac$( 7)), field4$(2%)          , ch(01),~
                                                                         ~
               at (15,04), fac(hex(ac)),   str(title$(3),,34%),          ~
               at (16,04), fac(lfac$( 8)), field1$(3%)          , ch(10),~
               at (16,17), fac(lfac$( 8)), field2$(3%)          , ch(10),~
               at (16,29), "Min:",                                       ~
               at (16,34), fac(lfac$( 8)), field3$(3%)          , ch(01),~
               at (16,37), "Max:",                                       ~
               at (16,42), fac(lfac$( 8)), field4$(3%)          , ch(01),~
                                                                         ~
               at (18,04), fac(hex(ac)),   str(title$(4),,41%),          ~
               at (19,04), fac(lfac$( 9)), field1$(4%)          , ch(25),~
               at (19,30), "Min:",                                       ~
               at (19,35), fac(lfac$( 9)), field2$(4%)          , ch(02),~
               at (19,39), "Max:",                                       ~
               at (19,44), fac(lfac$( 9)), field3$(4%)          , ch(02),~
                                                                         ~
               at (12,49), fac(hex(ac)),   str(title$(5),,28%),          ~
               at (13,49), "-       +",                                  ~
               at (13,51), fac(lfac$(10)), field1$(5%)          , ch(03),~
               at (13,59), fac(lfac$(10)), field2$(5%)          , ch(03),~
                                                                         ~
               at (15,49), fac(hex(ac)),   str(title$(6),,04%),          ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40970
                  call "MANUAL" ("CDARULSB") : goto L40270

L40970:        if keyhit% <> 14 then L41070
                  REM Show Where This Validation Routine Is Used...
                  incl(1)  = 0 : incl$() = " "
                  hdr$(1) = hex(a4) & "Shown below is the Where Used " & ~
                            "List For This Validation Routine"
                  plowkey$ = code$
                  call "PLOWCODE" (#3, plowkey$, hdr$(1), 8008%, 1.30,   ~
                     f1%(3), hdr$(), 10, 21, incl(), incl$()," ","Y", #4)
                  goto L40270

L41070:        if keyhit% <> 15 then L41100
                  call "PRNTSCRN" : goto L40270

L41100:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41290     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L41250
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L41250:     if fieldnr% > 2% then L41270
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41270:     return

L41290: if fieldnr% > 0% then L41430  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "(12)Delete Record      (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "(14)Where Used         (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffff0c0d0e0f1000)
            if mode% = 0% then L41420
               pf$(3) = "                                        " &     ~
                        "(14)Where Used         (16)Exit Display"
               str(pf$(1),,60) = " "
               str(pfkeys$,1,1), str(pfkeys$,12,1) = hex(ff)
L41420:     return
L41430:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50190,         /* Validation Code        */~
                              L50320,         /* Description            */~
                              L50360,         /* Is Blank OK?           */~
                              L50410,         /* Action                 */~
                              L50460,         /* Type                   */~
                              L50600,         /* Validation Type 1      */~
                              L50680,         /* Validation Type 2      */~
                              L50890,         /* Validation Type 3      */~
                              L51060,         /* Validation Type 4      */~
                              L51150          /* Validation Type 5      */
            return
L50190: REM Test for Validation Code              CODE$
            if code$ = "?" then code$ = " ": if code$ <> " " then L50250
               call "GETCODE" (#1, code$, " ", 0%, 1, f1%(1))
               if f1%(1) <> 0% then L50280
                  errormsg$ = hex(00)
                  return
L50250:     call "READ100" (#1, code$, f1%(1))
            if f1%(1) = 0% then return

L50280:     gosub dataload
            return clear all
            goto editpg1

L50320: REM Test for Description                  DESCR$
            if descr$ = " " then errormsg$ = "Field Can't Be Left Blank"
            return

L50360: REM Test for Is Blank Ok?                 BOK$
            if pos("YN"=bok$) > 0% then return
               errormsg$ = "Please Enter 'Y' or 'N'"
               return

L50410: REM Test for Action                       ACTION$
            if pos("EW"=action$) > 0% then return
               errormsg$ = "Please Enter 'E' or 'W'"
               return

L50460: REM Test for Type                         TYPE$
            x% = pos(type$()<>" ")
            if x% = 0% then L50540
               type$(x%) = " "
               y% = pos(type$()<>" ")
               if y% > 0% then L50560
                  type$(x%) = "X"
                  return
L50540:     errormsg$ = "Please 'X' a box"
            return
L50560:     errormsg$ = "Please 'X' only ONE box"
            type$(x%) = "X"
            return

L50600: REM Test for Validation Type 1            FIELD1$,FIELD2$
            if field1$(1%) > " " or field2$(1%) > " " then L50640
               errormsg$ = "Please Enter Desire Range"
               return
L50640:     if field1$(1%) <= field2$(1%) then return
               errormsg$ = "First Value Must Not Be Greater Than Second"
               return

L50680: REM Test for Validation Type 2            FIELD1$,FIELD2$,FIELD3$
            if field1$(2%) > " " then L50720
               errormsg$ = "File Name Can't Be Blank"
               return
L50720:     if field1$(2%) <> "GENCODES" then L50800
               plowkey$ = all(hex(00))
               str(plowkey$, 10%) = field2$(2%)
               call "PLOWCODE" (#2, plowkey$, " ", 9%, 0.30, f1%(2))
               field2$(2%) = str(plowkey$,10)
               if f1%(2) <> 0% then L50820
                  errormsg$ = "Please Select A Valid GENCODES Table"
                  return
L50800:     field2$(2%) = " "

L50820:     if pos("VN" = field3$(2%)) > 0% then L50850
               errormsg$ = "Enter 'V', or 'N': " & field3$(2%)
               return
L50850:     if pos("YNA123456789" = field4$(2%)) > 0% then return
               errormsg$="Enter Y, N, A, or Alt Key# (1-9) for Lookup"
               return

L50890: REM Test for Validation Type 3            FIELD1$,FIELD2$,FIELD3$
            call "NUMTEST" (field3$(3%), 0, 9, errormsg$, 0.0, field3)
            if errormsg$ <> " " then return
            call "NUMTEST" (field4$(3%), field3,9,errormsg$, 0.0, field4)
            if errormsg$ <> " " then return
            field0 = field3 + (field4/10)
            call "NUMTEST" (field1$(3%),-9e9,9e10,errormsg$,field0,field1)
            if errormsg$ <> " " then return
            call "NUMTEST" (field2$(3%),-9e9,9e10,errormsg$,field0,field2)
            if errormsg$ <> " " then return
            if field1 <> 0 or field2 <> 0 then L51020
               errormsg$ = "Please Enter Desire Range"
               return
L51020:     if field1 <= field2 then return
               errormsg$ = "First Value Must Not Be Greater Than Second"
               return

L51060: REM Test for Validation Type 4            FIELD1$,FIELD2$,FIELD3$
            call "NUMTEST" (field2$(4%), 0, 25, errormsg$, 0.0, field2)
            if errormsg$ <> " " then return
            call "NUMTEST" (field3$(4%),field2,25,errormsg$, 0.0, field3)
            if errormsg$ <> " " then return
            if len(field1$(4%)) = field3 then return
               errormsg$ = "Picture Spec must be same length as 'Max'"
               return

L51150: REM Test for Validation Type 5            FIELD1$,FIELD2$
            call "NUMTEST" (field1$(5%), 0, 999, errormsg$, 0.0, field1)
            if errormsg$ <> " " then return
            call "NUMTEST" (field2$(5%), 0, 999, errormsg$, 0.0, field2)
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            end
