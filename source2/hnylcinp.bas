        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y  L       CCC   IIIII  N   N  PPPP    *~
            *  H   H  NN  N  Y   Y  L      C   C    I    NN  N  P   P   *~
            *  HHHHH  N N N   YYY   L      C        I    N N N  PPPP    *~
            *  H   H  N  NN    Y    L      C   C    I    N  NN  P       *~
            *  H   H  N   N    Y    LLLLL   CCC   IIIII  N   N  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYLCINP - Define the list of valid inventory locations   *~
            *            for each warehouse.                            *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/22/88 ! Original                                 ! WPH *~
            * 02/12/90 ! Added 4th alternate key (HNYLOCNS), and  ! MLJ *~
            *          !  updated 'Select Files' documentation.   !     *~
            * 03/09/90 ! Added variable fields processing         ! MLJ *~
            * 07/18/91 ! QC rework- no longer forces plow on entry! MLJ *~
            *          !  of Location code, only '?'.             ! MLJ *~
            * 06/10/94 ! A '?' in the location fields now begins  ! MLJ *~
            *          !  at top of list instead of middle.       !     *~
            * 01/10/95 ! Fixed problem that stopped you from      ! JDH *~
            *          !  adding new locations.                   !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            comments$(2)30,              /* Comments                   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descrip$30,                  /* Description                */~
            deletekey$99,                /* Devastation key            */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            error$35,                    /* Misc. text string          */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            location$8,                  /* Location Number            */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            store$3,                     /* Warehouse                  */~
            storedescr$32,               /* Warehouse                  */~
            today$6,                     /*                            */~
            userid$3,                    /* Current User Id            */~
            vfs$200                      /* Variable Fields            */

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
            cms2v$ = "R6.03.02 01/17/95 'A' PRRs & Critical Problems    "
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
            * #01 ! STORNAME ! STORE INFORMATION FILE                   *~
            * #02 ! LOCATION ! Location Master file                     *~
            * #03 ! HNYLOCNS ! Location Quantity Detail File            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3                      ~

            select #02, "LOCATION"                                       ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos = 1,    keylen = 11 ,                     ~
                        alt key  1, keypos = 4, keylen = 11

            select #3,  "HNYLOCNS",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =  443, keylen =  42,         ~
                            key  2, keypos =  485, keylen =  42,         ~
                            key  3, keypos =  527, keylen =  42,         ~
                            key  4, keypos =  590, keylen =  42

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 300%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            today$ = date$
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "HNYLCINP: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
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

        REM Variable Fields input...
            str(line2$,,60) = "This Location: " & location$ & " and Sto"&~
                              "re: " & store$
            call "VFINPSUB" ("LOCATION", "I", "Manage Location Master F"&~
                            "ile", str(line2$,,60), "NN", vfs$, keyhit%)
            if keyhit% = 1% then inputmode

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  5% then       edit_vf
                  if keyhit%  =  12% then gosub delete_location
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
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

        edit_vf
            str(line2$,,60) = "This Location: " & location$ & " and Sto"&~
                              "re: " & store$
            call "VFINPSUB" ("LOCATION", "E", "Manage Location Master F"&~
                            "ile", str(line2$,,60), "YN", vfs$, keyhit%)
            if keyhit% =  1% then inputmode
            if keyhit% =  4% then editpg1
            if keyhit% = 16% then datasave
                             goto editpg1
        delete_location
            keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "D E L E T E",                     ~
                            "Press PF-24 to DELETE this Location Record",~
                            "-OR-", "Press RETURN to CANCEL delete.")
            if keyhit1% <> 24% then return

        REM  See if quantity records exist for this location
            plowkey$ = all(hex(00))
            str(plowkey$,1,3) = store$
            str(plowkey$,4,8) = location$
            call "PLOWNEXT" (#3, plowkey$, 11%, f1%(3))
                if f1%(3) = 0% then L11470
            keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "LOCATION HAS QUANTITIES ON FILE", ~
                            "Enter PF-23 to DELETE Location Quantities", ~
                            "-OR-", "Press RETURN to CANCEL delete.")
            if keyhit1% <> 23% then return

        REM Send the quantity records to la-la land 'cause they said to
            deletekey$ = all(hex(00))
            str(deletekey$,1,11) = str(plowkey$,1,11)
            call "DELETE" (#3, deletekey$, 11%)       /* nail em all */

        REM Now zip the location record from the master file.
L11470:     deletekey$ = plowkey$
            call "DELETE" (#2, deletekey$, 11%)       /* nail one    */
                return clear all
                goto inputmode



        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Warehouse              */~
                              L20200,         /* Location Number        */~
                              L20300,         /* Description            */~
                              L20400          /* Comments               */
            return
L20100: REM Def/Enable Warehouse                   STORE$
            return

L20200: REM Def/Enable Location Number             LOCATION$
            return

L20300: REM Def/Enable Description                 DESCRIP$
            return

L20400: REM Def/Enable Comments                    COMMENTS$
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter the Store/Warehouse code, previously defined.          ",~
         "Enter the Location Number, or '?' to see locations on file for ~
        ~warehouse.",                                                     ~
         "Enter a description for this location (optional)             ",~
         "Enter any comments for this location (optional)              "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                 comments$(1), comments$(2), descrip$, location$, store$,~
                      storedescr$, vfs$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload

            get #2 using L30140,  descrip$, comments$(1), comments$(2),   ~
                                 vfs$

L30140:     FMT  XX(14), 3*CH(30), POS(114), CH(200)

            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput

            call "READ101" (#2, plowkey$, f1%(2))

            put #2  using L31090 , store$, location$, store$, descrip$,   ~
                                 comments$(1),comments$(2), userid$,     ~
                                 today$, vfs$, " "
L31090:     FMT  CH(3), CH(8), CH(3),  3* CH(30) , CH(3), CH(6), CH(200),~
                 CH(87)

            if f1%(2) = 0% then L31120

            rewrite #2
            return

L31120:     write #2
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
              on fieldnr% gosub L40090,         /* Warehouse         */   ~
                                L40090,         /* Location Number   */   ~
                                L40085,         /* Description       */   ~
                                L40085          /* Comments          */
              goto L40105

L40085:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40090:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40105:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage List of Valid Locations for each Warehouse",   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Warehouse",                                  ~
               at (06,30), fac(lfac$( 1)), store$               , ch(03),~
               at (06,49), fac(hex(8c)),   storedescr$          , ch(32),~
                                                                         ~
               at (07,02), "Location Number",                            ~
               at (07,30), fac(lfac$( 2)), location$            , ch(08),~
                                                                         ~
               at (08,02), "Description",                                ~
               at (08,30), fac(lfac$( 3)), descrip$             , ch(30),~
                                                                         ~
               at (09,02), "Comments",                                   ~
               at (09,30), fac(lfac$( 4)), comments$(1)         , ch(30),~
               at (10,30), fac(lfac$( 4)), comments$(2)         , ch(30),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40265
                  call "MANUAL" ("HNYLCINP") : goto L40105

L40265:        if keyhit% <> 15 then L40280
                  call "PRNTSCRN" : goto L40105

               if edit% = 2% then return
L40280:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40375     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40355
                str(pf$(3),63)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40355:     if fieldnr% > 2% then L40365
                str(pf$(1),18,22) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40365:     return

L40375: if fieldnr% > 0% then L40420  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (5)Next Screen        (" &        ~
                     "12)Delete Location     (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffff05ffffffff17180c0dff0f1000)
            return
L40420:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* Warehouse              */~
                              L50200,         /* Location Number        */~
                              L50410,         /* Description            */~
                              L50440          /* Comments               */
            return
L50100: REM Test for Warehouse                    STORE$
                storedescr$ = hex(06) & "Valid Warehouses"
                call "GETCODE" (#1, store$, storedescr$, 1%, .3, f1%(1))
                if f1%(1) = 1% then return
                errormsg$ = "Warehouse/Store must be already defined!"
                return
            return

L50200: REM Test for Location Number              LOCATION$
            if location$ = " " then L50380
                if location$ = "?" then location$ = " "
            plowkey$ = all(hex(00))
            str(plowkey$,1,3) = str(store$,,)
            str(plowkey$,4,8) = str(location$,,)

            error$ = hex(06) & "Locations on file for store: " & store$

                call "PLOWCODE" (#2, plowkey$, error$, 3%, 0, f1%(2))
                store$ = str(plowkey$,1,3)
                location$ = str(plowkey$,4,8)

                if location$ = " " then L50380

            call "READ100" (#2, plowkey$, f1%(2))
            if f1%(2) <> 0% then gosub dataload else return
            return clear all
            goto editpg1

L50380:     errormsg$ = "Location Number cannot be blank"
            return

L50410: REM Test for Description                  DESCRIP$
            return

L50440: REM Test for Comments                     COMMENTS$
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
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
