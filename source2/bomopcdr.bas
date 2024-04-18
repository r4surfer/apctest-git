        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  BBBB    OOO   M   M   OOO   PPPP    CCC   DDDD   RRRR    *~
            *  B   B  O   O  MM MM  O   O  P   P  C   C  D   D  R   R   *~
            *  BBBB   O   O  M M M  O   O  PPPP   C      D   D  RRRR    *~
            *  B   B  O   O  M   M  O   O  P      C   C  D   D  R   R   *~
            *  BBBB    OOO   M   M   OOO   P       CCC   DDDD   R    R  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMOPCDR - Driver program for BOMOPCST sub allows user to *~
            *            select the sales order, line, and set flags    *~
            *            dictating how the report is printed.  The sub  *~
            *            is then called which does all the work.        *~
            *                                                           *~
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
            * 04/12/94 ! Original                                 ! WPH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            assypartdescr$32,            /* Top Assembly Part Number   */~
            assypart$25,                 /* Top Assembly Part Number   */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            demand$19,                   /* SO number + line           */~
            edtmessage$79,               /* Edit screen message        */~
            effdate$6,                   /* Effective date (due date)  */~
            errormsg$79,                 /* Error message              */~
            explode_all$1,               /* Explode All Parts?         */~
            print_descr$1,               /* Print Part Descriptions?   */~
            soline$3,                    /* Sales Order Line           */~
            sonumber$16,                 /* Sales Order Number         */~
            i$(24)80,                    /* Screen Image               */~
            inbomid$3,                   /* BOM version                */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            msg$79,                      /* More plowcode goodies      */~
            message$79,                  /* Text for use in ASKUSER    */~
            parttype$3,                  /* Part type                  */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey2$99,                 /* Miscellaneous Read/Plow Key*/~
            plowhdr$(3)79,               /* Plowcode header string     */~
            rpttitle$60,                 /* Report Title               */~
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
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
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
            * #04 ! HNYMASTR ! Inventory Master File                    *~
            * #05 ! BOMMASTR ! BOM relationship file                    *~
            * #07 ! BOMSPEC  ! options selected file                    *~
            * #10 ! SYSFILE2 ! Caelus Management System Information     *~
            * #11 ! ENGMASTR ! Engineering Master Filer                 *~
            * #12 ! BCKLINES ! Sales Order Line Items File              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #04, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #05, "BOMMASTR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  31,                     ~
                            alt key 1, keypos = 1, keylen = 56

            select #07, "BOMSPEC",                                       ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  54,                     ~
                        alt key  1, keypos =   57, keylen =  23          ~

            select #10, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #11, "ENGMASTR",                                      ~
                        varc,     indexed,  recsize = 2015,              ~
                        keypos =    1, keylen =  29                      ~

            select #12, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19                      ~

            select #01, "WORKFILE",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos = 1,    keylen = 33,                      ~
                        alt key  1, keypos =   26, keylen =  8

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))
            call "OPENCHCK" (#07, fs%(07), f2%(07), 0%, rslt$(07))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, k%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "Option Bill Cost Roll-up Audit" &               ~
                        " Report                       "

            call "STRING" addr("CT", rpttitle$, 60%, rpttitle$)

            str(line2$,62) = "BOMOPCST: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  4%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       call_the_sub
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  4% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *     C A L L   T H E   S U B R O U T I N E                 *~
            *-----------------------------------------------------------*~
            * Data Extraction section for report.                       *~
            *************************************************************

        call_the_sub

            str(demand$,1,16) = str(sonumber$,,)
            str(demand$,17,3) = str(soline$,,)

            init(hex(00)) readkey$
            str(readkey$,1,19) = str(demand$,,)

            call "READ100"(#12, readkey$, f1%(12))
               if f1%(12) = 1% then L13078
            message$ = "Couldn't read BCKLINES file for SO " & readkey$
               goto L13120
L13078:     get #12, using L13079, assypart$, effdate$
L13079:        FMT POS(32), CH(25), POS(206), CH(6)

            call "READ100"(#4, assypart$, f1%(4))
               if f1%(4) = 1% then L13095
            message$ = "Couldn't read HNYMASTR file for part " & assypart$
               goto L13120
L13095:     get #4, using L13096, assypartdescr$,   parttype$
L13096:        FMT POS(26), CH(32), POS(180), CH(3)

*       If part is not generic then blow out with askuser message
            if parttype$ = "000" then L13180
            message$ = "The part sold on this sales order line is not " &~
                       "generic part."
L13120:     k% = 2%
            call "ASKUSER"(k%, "* * * CANNOT CONTINUE * * *",            ~
            message$,                                                    ~
            "Try another sales order line, or inform your system admin"& ~
            "istrator.",                                                 ~
            "Press any key to acknowledge this message.")
            goto inputmode


L13180
*       Check to see if options have been selected

            init (hex(00)) readkey2$
            str(readkey2$,1,19) = str(demand$,,)
            call "PLOWALTS" (#7, readkey2$, 1%, 19%, f1%(7))
                if f1%(7) = 1% then L13192
                message$ = "Options not yet selected for this SO Line."
                   goto L13120
L13192:     get #7 using L13193, inbomid$
L13193:         FMT POS(51), CH(3)

*       Open the workfile and get going

           call "BOMOPCST"                                               ~
                 (demand$,               /* Sales Order + Line #       */~
                 explode_all$,           /* Flag, 'N' = explode generic*/~
                 print_descr$,           /* Flag to print part descrpts*/~
                 assypart$,              /* Top level part number      */~
                 effdate$,               /* Due date                   */~
                 #4,                     /* HNYMASTR                   */~
                 #5,                     /* BOMMASTR                   */~
                 #7,                     /* BOMSPEC                    */~
                 #10,                    /* SYSFILE2                   */~
                 #11,                    /* ENGMASTR                   */~
                 0%  )                   /* Return Code                */

            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S                         *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES for data entry fields           *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Sales Order            */~
                              L20200,         /* Sales Order Line       */~
                              L20300,         /* Explode All Parts      */~
                              L20400          /* Print Description      */
            return
L20100: REM Def/Enable Sales Order Number          SONUMBER$


            return

L20200: REM Def/Enable Sales Order Line            SOLINE$


            return

L20300: REM Def/Enable Explode All Parts?          EXPLODE_ALL$
            if   explode_all$      = " " then                            ~
                 explode_all$      = "Y"
            return

L20400: REM Def/Enable Print Part Descriptions?    PRINT_DESCR$
            if   print_descr$      = " " then                            ~
                 print_descr$      = "N"
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter The Sales Order Number, or Leave Blank To Select.      ",~
         "Enter The Sales Order Line #, or Leave Blank To Select.      ",~
         "Enter 'Y' to explode all assemblies, or 'N' for only generics",~
         "Enter 'Y' To Cause Part Descriptions To Be Printed.          "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      explode_all$, print_descr$, soline$, sonumber$,    ~
                      effdate$, assypart$
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

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40090,         /* Sales Order       */   ~
                                L40090,         /* Sales Order Line  */   ~
                                L40090,         /* Explode All Parts */   ~
                                L40090          /* Print Description */
              goto L40105

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40090:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40105:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Option Bill Cost Roll-up Audit Report",         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (07,02), "Sales Order Number",                         ~
               at (07,30), fac(lfac$( 1)), sonumber$            , ch(16),~
                                                                         ~
               at (08,02), "Sales Order Line",                           ~
               at (08,30), fac(lfac$( 2)), soline$              , ch(03),~
                                                                         ~
               at (09,02), "Explode All Parts?",                         ~
               at (09,32), fac(lfac$( 3)), explode_all$         , ch(01),~
                                                                         ~
               at (10,02), "Print Part Descriptions?",                   ~
               at (10,32), fac(lfac$( 4)), print_descr$         , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40290
                  call "MANUAL" ("BOMOPCST") : goto L40105

L40290:        if keyhit% <> 15% then L40305
                  call "PRNTSCRN" : goto L40105

L40305:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40400     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40385
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L40390
L40385:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40390:     return

L40400: if fieldnr% > 0% then L40445  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40445:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* Sales Order            */~
                              L50200,         /* Sales Order Line       */~
                              L50300,         /* Explode All Parts      */~
                              L50400          /* Print Description      */
            return
L50100: REM Test for Sales Order Number           SONUMBER$
            init(hex(00)) readkey$
            if sonumber$ = "?" then sonumber$ = " "
            if sonumber$ = " " then  L50130
               str(readkey$,1,16) = str(sonumber$,,)
               goto L50135
L50130:     readkey$ = hex(010203)
L50135:     plowhdr$(2%) = "  SO Number          Customer Code"
            msg$ = hex(0684)& "Select Sales Order, or PF16 To Return"
            call "PLOWCODE" (#12, readkey$, msg$,  -3016%,-0.09, f1%(12),~
                                 plowhdr$(), 0, 1)
            if f1%(12%) <> 0% then L50170
                errormsg$ = "Enter or Select a valid Sales Order Number"
                return
L50170:     sonumber$ = str(readkey$,1,16)
            return

L50200: REM Test for Sales Order Line             SOLINE$
            if soline$ = "?" then soline$ = " "
            init(hex(00)) readkey$
            str(readkey$,1,16) = str(sonumber$,,16%)
            call "RJUSTIFY" (soline$)
            str(readkey$,17,03) = str(soline$,,3%)
            plowhdr$()= "  Line         Part Number"

            msg$ = hex(06) & "Listed Below Are The Existing Lines for " &~
                                                      "S.O.:" &  sonumber$
            call "PLOWCODE" (#12, readkey$, msg$, 3016%, .25,            ~
                                           f1%(12%), plowhdr$(), 0,32 )

            if f1%(12%) <> 0% then L50275
                errormsg$ = "Line not on file" : return
L50275:     soline$ = str(readkey$,17%,3%)
            return


L50300: REM Test for Explode All Parts?           EXPLODE_ALL$
            if explode_all$ = "Y" or explode_all$ = "N" then return
            errormsg$ = "Please Enter 'Y' or 'N'."
            return

L50400: REM Test for Print Part Descriptions?     PRINT_DESCR$
            if print_descr$ = "Y" or print_descr$ = "N" then return
            errormsg$ = "Please Enter 'Y' or 'N'."
            return

        REM THISPROGRAMWASGENERATEDBYGENRPPGMAPROPRIETRYPRODUCTOFCAELUS**~
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
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
