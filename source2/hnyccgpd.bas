        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y   CCC    CCC    GGG   PPPP   DDDD    *~
            *  H   H  NN  N  Y   Y  C   C  C   C  G      P   P  D   D   *~
            *  HHHHH  N N N   YYY   C      C      G GGG  PPPP   D   D   *~
            *  H   H  N  NN    Y    C   C  C   C  G   G  P      D   D   *~
            *  H   H  N   N    Y     CCC    CCC    GGG   P      DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYCCGPD -   This program will allow the user to define   *~
            *              Cycle Count Groups and write the informations*~
            *              to HNYCCGRP file. This file will provide a   *~
            *              place to store infomation common to all      *~
            *              members of the group.                        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/30/92 ! Original (Cycle Count Project)           ! SID *~
            * 09/29/92 ! Mod PLOWing GroupName per Caelus Standard! SID *~
            *          !   and input message.                     !     *~
            * 01/12/93 ! Page 0 Facs fix                          ! RJH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr$50,                    /* PLOWCODE Header Descr      */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fmgroupname$6,               /* FROM CC Group Name         */~
            from$6,                      /* Literal for Input Screen   */~
            groupdescr$30,               /* Description                */~
            groupname$6,                 /* Cycle Count Group Name     */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /*                            */~
            record$(4)154,               /* Arrary for Print Report    */~
            rpttitle$60,                 /* Report Title               */~
            text$(4)45,                  /* Text                       */~
            togroupname$6,               /* TO CC Group Name           */~
            to$6,                        /* Literal for Input Screen   */~
            userid$3,                    /* Current User Id            */~
            variable$200                 /* Variable Fields            */~

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
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
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
            * #01 ! HNYCCGRP ! Cycle Count Group File                   *~
            * #02 ! HNYCCMST ! Cycle Count Master File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYCCGRP",                                      ~
                        varc,     indexed,  recsize = 616,               ~
                        keypos =  1,   keylen =   6                      ~

            select #02, "HNYCCMST",                                      ~
                        varc,     indexed,  recsize = 796,               ~
                        keypos =    1,   keylen =   44,                  ~
                        alt  key 1, keypos =  45, keylen =   6, dup,     ~
                             key 2, keypos =  81, keylen =   7, dup,     ~
                             key 3, keypos =  73, keylen =  15, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 100%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            ret% = 0
            call "COMPNAME" (12%, company$, ret%)
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "Cycle Count Group Name Report"
            call "STRING" addr("CT", rpttitle$, 60%, rpttitle$)

            from$ = "From" : to$ = "To"
            str(line2$,62) = "HNYCCGPD: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode_1
            call "ALLFREE"
            gosub initialize_variables

            for fieldnr% = 1% to  3%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10201
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10201:               if keyhit% = 14% then goto inputmode_2
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

*        Perform Input for Variable Fields
            call "VFINPSUB" ("HNYCCGRP", "I", "Manage Cycle Count Group" ~
                  & "Name", str(line2$,,60), "NN", variable$, keyhit%)
            if keyhit% = 1% then inputmode_1

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  9% then gosub edit_user_defined_fields
                  if keyhit%  = 12% then       delete_group_name
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% = 1% or fieldnr% = 3% then editpg1
            if fieldnr% = 4% or fieldnr% = 5% or                         ~
               fieldnr% = 6% or fieldnr% = 7% then fieldnr% = 3%
            if fieldnr% < 1% or fieldnr% >  3% then editpg1
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
            *       I N P U T   M O D E   R E P O R T  S C R E E N      *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode_2
            init(" ") fmgroupname$, togroupname$, errormsg$
            inpmessage$ = "Enter Cycle Count Group Range to Print or "   ~
                        & "Enter '?' to See Groups on File."
            fmgroupname$ = "ALL"
            for fieldnr% = 1% to  1%
L11370:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% = 16% then inputmode_1 /* Back to Main */
                      if keyhit% <> 0% then       L11370
                gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L11370
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   F O R  R E P O R T             *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg2
            inpmessage$ = edtmessage$
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       generate_report
                  if keyhit% <>  0% then       editpg2
            fieldnr% = cursor%(1%) - 6%
            if fieldnr% <> 1% then editpg2
            inpmessage$ = "Enter Cycle Count Group Range to Print or "   ~
                        & "Enter '?' to See Groups on File."
L11960:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11960
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11960
                  lastfieldnr% = fieldnr%
            goto editpg2

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto inputmode_1

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Cycle Count Group      */~
                              L20200,         /* Description            */~
                              L20300          /* Text                   */
            return
L20100: REM Def/Enable Cycle Count Group Name      GROUPNAME$
            return

L20200: REM Def/Enable Description                 GROUPDESCR$
            return

L20300: REM Def/Enable Text                        TEXT$()
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
         "Enter Cycle Count Group Name.  Enter Blank or '?' to See Group ~
        ~on File.",                                                       ~
         "Enter Cycle Count Group Name Description                     ",~
         "Enter Text                                                   "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, variable$,                 ~
                      groupdescr$, groupname$, text$()
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
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
            goto inputmode_1

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            get #01 using L30080, groupname$, groupdescr$, text$(),       ~
                                 variable$
L30080:         FMT    CH(6), CH(30), 4*CH(45), CH(200)
            goto editpg1

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "READ101" (#01, groupname$, f1%(01))
            put #01 using L31080, groupname$, groupdescr$, text$(),       ~
                                 variable$, " "
L31080:        FMT    CH(6), CH(30), 4*CH(45), CH(200), CH(200)
            if f1%(01) = 1% then rewrite #01 else write #01
            goto inputmode_1

        edit_user_defined_fields
            call "VFINPSUB" ("HNYCCGRP", "E", "Manage Cycle Count Group" ~
                     & "Name", str(line2$,,60), "YN", variable$, keyhit%)
            if keyhit% =  1% then inputmode_1
            if keyhit% =  4% then editpg1
            if keyhit% = 16% then datasave
            goto editpg1

        delete_group_name
            errormsg$ = " "
            readkey$ = groupname$
            call "REDALT0" (#02, readkey$, 1%, f1%(02))
              if f1%(02) = 1% then errormsg$ = "Group Name Still Active"
            if errormsg$ <> " " then return
            keyhit% = 2%
               call "ASKUSER" (keyhit%, "CONFIRMATION",                  ~
               hex(8c) & "Group Name:" & hex(84) & groupname$ & hex(8c) &~
                    "Is about to Be Deleted" & hex(84),                  ~
                     hex(8c) & "To Delete, Press PF(16)." & hex(84),     ~
                    "Hit any other PF key To abort delete")
               if keyhit% = 16% then call "DELETE" (#01, readkey$, 6%)
            goto inputmode_1

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************

        generate_report
            call "SHOSTAT" ("Report Generation in Progress")
            select printer(134)
            call "SETPRNT" ("HNY045", " ", 0%, 0%)
            time$ = " "  :  call "TIME" (time$)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            if lcntr% > 56% then gosub page_head

            if fmgroupname$ = "ALL" then init (" ") plowkey$             ~
                  else str(plowkey$,,6) = fmgroupname$ addc all(hex(ff))
L34120:     call "PLOWNEXT" (#01, plowkey$, 0%, f1%(01))
              if f1%(01) = 0% then end_report
                get #01, record$()
            if fmgroupname$ = "ALL" then L34131
            if str(record$(1),1,6) > togroupname$ then end_report

L34131:     gosub print_groupname
            goto L34120  /* Go Get Next Record */

        end_report                /* Report Ending Routine */
            time$ = " "  :  call "TIME" (time$)
            print skip(2)
            print using L60170, time$             /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            goto inputmode_1  /* Back to Main Input Screen */

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "HNYCCGPD"
            print using L60110, rpttitle$, pcntr%
            print
            if pcntr% = 0% then goto print_params
            print "Group Name      Description                          T~
        ~ext"
            print "----------      ------------------------------       -~
        ~--------------------------------------------"
            lcntr% = 5%
            return

        print_params           /* Print Page Zero */
L34315:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L34335
                str(i$(), i%, 1%) = hex(20)
                goto L34315
L34335:     print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            return

        print_groupname
            if lcntr% > 56% then gosub page_head
            print using L60200, str(record$(),,6), str(record$(),7,30),   ~
                               str(record$(),37,45)
            print using L60220, str(record$(),82,45)
            print using L60220, str(record$(),127,45)
            print using L60220, str(record$(),172,45)
            print

            init(" ") record$()
            lcntr% = lcntr% + 5%
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
              on fieldnr% gosub L40085,         /* Cycle Count Group */   ~
                                L40080,         /* Description       */   ~
                                L40080          /* Text              */
              goto L40100

L40080:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40085:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40100:     accept                                                       ~
               at (01,02),                                               ~
                  "Define Cycle Count Groups",                           ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Cycle Count Group Name",                     ~
               at (06,30), fac(lfac$( 1)), groupname$           , ch(06),~
                                                                         ~
               at (07,02), "Description",                                ~
               at (07,30), fac(lfac$( 2)), groupdescr$          , ch(30),~
                                                                         ~
               at (09,02), "Text",                                       ~
               at (09,30), fac(lfac$( 3)), text$(1)             , ch(45),~
               at (10,30), fac(lfac$( 3)), text$(2)             , ch(45),~
               at (11,30), fac(lfac$( 3)), text$(3)             , ch(45),~
               at (12,30), fac(lfac$( 3)), text$(4)             , ch(45),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40240
                  call "MANUAL" ("HNYCCGPD") : goto L40100

L40240:        if keyhit% <> 15 then L40255
                  call "PRNTSCRN" : goto L40100

L40255:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40350     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40330
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40330:     if fieldnr% > 2% then L40340
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
                str(pf$(2),38,26) = "(14)Print Group Name"
                str(pfkeys$,14,1) = hex(0e)
L40340:     return

L40350: if fieldnr% > 0% then L40395  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (9)User Defined Fields " &        ~
                     "      (12)Delete       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffff09ffff0c0dff0f1000)
            return
L40395:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               R E P O R T   S C R E E N                   *~
            *-----------------------------------------------------------*~
            *************************************************************~

        deffn'102(fieldnr%, edit%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              if fieldnr% = 0% then L42210
              lfac$(fieldnr%) = hex(81)  /* Upper Only */

L42210:     accept                                                       ~
               at (01,02),                                               ~
                  "Input Report Selection Criteria",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)), from$                  , ch(06),~
               at (06,40), fac(hex(ac)), to$                    , ch(06),~
                                                                         ~
               at (07,02), "Cycle Count Group Range"            ,        ~
               at (07,30), fac(lfac$( 1)), fmgroupname$         , ch(06),~
               at (07,40), fac(lfac$( 1)), togroupname$         , ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L42690
                  call "MANUAL" ("HNYCCGPD") : goto L42210

L42690:        if keyhit% <> 15 then L42750
                  call "PRNTSCRN" : goto L42210

L42750:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L43130     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L43070
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L43090
L43070:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L43090:     return

L43130: if fieldnr% > 0% then L43310  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L43310:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Cycle Count Group      */~
                              L50200,         /* Description            */~
                              L50300          /* Text                   */
            return
L50100: REM Test for Cycle Count Group Name       GROUPNAME$
            init (" ") plowkey$
            if groupname$ = " " or groupname$ = "?"  then L50130
            call "READ100" (#01, groupname$, f1%(01))
                if f1%(01) = 0% then return else dataload
L50130:     descr$ = hex(06) & "Select a Cycle Count Group Name"
            call "PLOWCODE" (#01, plowkey$, descr$, 0%, 0.30, f1%(01))
                if f1%(01) = 1% then dataload
                if groupname$ = " " or groupname$ = "?" then             ~
                   errormsg$ = "No Cycle Count Group Name Selected"
                return

L50200: REM Test for Description                  GROUPDESCR$
            return

L50300: REM Test for Text                         TEXT$
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Report Screen                  *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51724          /* Cycle Count Group      */
            if fmgroupname$ = "ALL" then return
            if fmgroupname$ <> " " and togroupname$ = " " then           ~
               togroupname$ = fmgroupname$
            return

L51724: REM Test for Cycle Count Group Name Range
            if fmgroupname$ = "ALL" and togroupname$ = " " then return
            plowkey$ = fmgroupname$ : k% = 1%
            descr$ = hex(06) & "Select FROM Cycle Count Group Name"
L51764:     call "PLOWCODE" (#01, plowkey$, descr$, 0%, 0.30, f1%(01))
               if f1%(01) = 0% then return
               if k% = 1% then L51824
                  togroupname$ = str(plowkey$,,6)
                  if fmgroupname$ <= togroupname$ then return
                  errormsg$ = "FROM can't be greater than TO" : return
L51824:        k% = k% + 1%
               fmgroupname$ = str(plowkey$,,6)
               descr$ = hex(06) & "Select TO Cycle Count Group Name  "
               plowkey$ = togroupname$
               goto L51764

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************~

*       * Header Line 1
L60070: %RUN ########   ########              ###########################~
        ~#################################                 ########:HNY045

*       * Header Line 2
L60110: %                                     ###########################~
        ~#################################                     PAGE: #####

        %** Report Title for page 0
        %############################################################

L60170: %                          * * * * * * * * * *   E N D   O F   R ~
        ~E P O R T  (########)  * * * * * * * * * *


L60200: %######          ##############################       ###########~
        ~##################################
L60220: %                                                     ###########~
        ~##################################
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
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SETPRNT" ("VEN003", " ", 0%, 1%)
            call "SHOSTAT" ("One Moment Please")

            end
