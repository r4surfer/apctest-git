        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCUSRID                             *~
            *  Creation Date     - 08/24/95                             *~
            *  Last Modified Date- 11/13/97                             *~
            *  Description       - This Program displays User ID Info   *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/24/95 ! New Program for (APC) - Last Mod Date    ! JBF *~
            *          !                                          !     *~
            * 10/31/97 ! Changed Program Version ID To 60403      ! DJD *~
            *          !                                          !     *~
            *************************************************************

        dim                                                              ~
            apc_user_id$3,               /* User ID                    */~
            apc_emp_id$5,                /* Employee ID #              */~
            apc_last_name$15,            /* User Last Name             */~
            apc_first_name$15,           /* User First Name            */~
            apc_mi$1,                    /* User Middle Initial        */~
            apc_menu_name$16,            /* User Caelus Menu Name      */~
            apc_user_pswd$10,            /* User Login Password        */~
            apc_dtc$2,                   /* DTC Connection             */~
            apc_port$2,                  /* DTC Port # Connection      */~
            apc_jack$4,                  /* Wall Jack #                */~
            apc_mon_pc$1,                /* HP Monitor/PC Flag         */~
            apc_user_rem$30,             /* Remarks about User Setup   */~
            apc_dte$6,                   /* Date Last Modified         */~
            apc_usr$3,                   /* User Last Modified         */~
            apc_filler$15,               /* Record Filler Area         */~
            apc_key$3,                   /* Record Read Key            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            readkey$24,                  /* Gencodes Read Key          */~
            dtc_desc$25,                 /* DTC Description            */~
            desc$32,                     /* Gencodes Description       */~
            title$40,                    /* Report Title Field         */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/13/97 APC CAELUS USER INFO           "
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
            * #01 ! APCUSRID ! APC Master User Information File         *~
            * #02 ! USERLCMS ! Caelus Master User ID File               *~
            * #03 ! APCEQDTC ! APC Computer DTC Master                  *~
            * #04 ! GENCODES ! System Code Table File                   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCUSRID",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =   3,                     ~
                        alt key  1, keypos =   4, keylen =  5,           ~
                            key  2, keypos =   9, keylen =  31, dup

            select #2,  "USERLCMS",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    1, keylen =   3,                     ~
                        alt key  1, keypos =   4, keylen =  30, dup

            select #3,  "APCEQDTC",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =    1, keylen =   2

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =   24

            call "SHOSTAT" ("Opening File, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 100%, rslt$(1%))
            call "OPENOLIB" (#2, "SHARE", f2%(2%), rslt$(2%), axd$ )
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),   0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),   0%, rslt$(4%))

            mat f1% = zer

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

            str(line2$,62) = "APCUSRID: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 12%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0%    then L10250

L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1%   then gosub startover
                      if keyhit% <>  4%   then L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10100
                         goto L10160
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% = 14%    then gosub gen_user
                      if keyhit% <> 0%    then L10130

L10250:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 12% then gosub dataput
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then editpg1

L11140:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 12% then editpg1
            if fieldnr% = lastfieldnr%         then editpg1

            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then editpg1

L11210:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11210

            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11210
                  lastfieldnr% = fieldnr%
            goto L11140

        REM *************************************************************~
            *           P r o c e s s   D a t a                         *~
            *************************************************************

        gen_user
            gosub select_user
            gosub generate_user_id
            close printer
        return clear all
        goto inputmode

        select_user
            title$  = "********   APC User ID Report   ********"
            pageno% = 0%
            lcnt%   = 99%
            date$   = date
            call "DATEFMT" (date$)
            call "TIME" (xtime$)
            call "SETPRNT" (" ","USR1",0%,0%)
            select printer(134)
        return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28120
                inpmessage$ = edtmessage$
            return

*        Define the Input Message for the Screen/Field Indicated
L28120:     if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
                read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Valid User ID?                                       ",~
         "Enter a Valid Employee ID?                                   ",~
         "Enter the Employee's First Name?                             ",~
         "Enter the Employee's Middle Initial?                         ",~
         "Enter the Employee's Last Name?                              ",~
         "Enter the Employee's Primary Caelus Menu?                    ",~
         "Enter the Employee's Logon Password?                         ",~
         "Enter the Employee's DTC No.?                                ",~
         "Enter the Employee's DTC/Port No.?                           ",~
         "Enter the Employee's Data Jack No.?                          ",~
         "Set HP/PC Flag?  (HP = 'Y' (Default), PC = 'N')              ",~
         "Enter Remarks about the Employee's Setup?                    "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$, apc_user_id$, apc_emp_id$, ~
                     apc_last_name$, apc_first_name$, apc_mi$,           ~
                     apc_menu_name$, apc_user_pswd$, apc_dtc$, dtc_desc$,~
                     apc_port$, apc_jack$, apc_mon_pc$, apc_user_rem$,   ~
                     apc_dte$, apc_usr$, apc_filler$, readkey$
            rec% = 0%
            return

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
            rec% = 0%
            apc_key$ = apc_user_id$

            read #1,key = apc_key$, eod goto L30300

        get_user_data
            get #1, using L35040,                                         ~
                     apc_user_id$,       /* User ID                    */~
                     apc_emp_id$,        /* Employee ID                */~
                     apc_last_name$,     /* Employee Last Name         */~
                     apc_first_name$,    /* Employee First Name        */~
                     apc_mi$,            /* Employee Middle Initial    */~
                     apc_menu_name$,     /* Caelus Primary Menu        */~
                     apc_user_pswd$,     /* Employee Logon Password    */~
                     apc_dtc$,           /* DTC Connection             */~
                     apc_port$,          /* DTC/Port Connection        */~
                     apc_jack$,          /* Employee Data Jack         */~
                     apc_mon_pc$,        /* HP Monitor/PC Flag         */~
                     apc_user_rem$,      /* Employee Setup Remarks     */~
                     apc_dte$,           /* Date Last Modified         */~
                     apc_usr$            /* User Last Modified         */
            rec% = 1%
            gosub lookup_dtc
L30300:     return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        dataput
            call "SHOSTAT" ( "Updating APCUSRID Data" )
            apc_key$ = apc_user_id$
            apc_dte$ = date$
            apc_usr$ = userid$

            read #1,hold,key = apc_key$, eod goto L31160
                delete #1
                if keyhit% = 12% then goto L31340

L31160:     put #1, using L35040,                                         ~
                     apc_user_id$,       /* User ID                    */~
                     apc_emp_id$,        /* Employee ID                */~
                     apc_last_name$,     /* Employee Last Name         */~
                     apc_first_name$,    /* Employee First Name        */~
                     apc_mi$,            /* Employee Middle Initial    */~
                     apc_menu_name$,     /* Caelus Primary Menu        */~
                     apc_user_pswd$,     /* Employee Logon Password    */~
                     apc_dtc$,           /* DTC Connection             */~
                     apc_port$,          /* DTC/Port Connection        */~
                     apc_jack$,          /* Employee Data Jack         */~
                     apc_mon_pc$,        /* HP Monitor/PC Flag         */~
                     apc_user_rem$,      /* Employee Setup Remarks     */~
                     apc_dte$,           /* Date Last Modified         */~
                     apc_usr$,           /* User Last Modified         */~
                     apc_filler$         /* Record Filler Area         */

            write #1, eod goto L31360

L31340: return clear all
        goto inputmode
L31360:     call "SHOSTAT" ( "ERROR - Unable to Update APCUSRIN" ) : stop
        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35040:     FMT CH(03),                  /* User ID                    */~
                CH(05),                  /* Employee ID                */~
                CH(15),                  /* Employee Last Name         */~
                CH(15),                  /* Employee First Name        */~
                CH(01),                  /* Employee Middle Initial    */~
                CH(16),                  /* Caelus Primary Menu        */~
                CH(10),                  /* Employee Login Password    */~
                CH(02),                  /* Employee DTC Connection    */~
                CH(02),                  /* Employee Port Connection   */~
                CH(04),                  /* Employee Data Jack #       */~
                CH(01),                  /* HP Monitor/PC Flag         */~
                CH(30),                  /* Employee Setup Remarks     */~
                CH(06),                  /* Date Last Modified         */~
                CH(03),                  /* User Last Modified         */~
                CH(15)                   /* Record Filler Area         */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            gosub'050(1%, fieldnr%)
            gosub set_pf1
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()

            on fieldnr% gosub L40270,           /* User ID            */  ~
                              L40270,           /* Employee ID        */  ~
                              L40270,           /* First Name         */  ~
                              L40270,           /* Middle Initial     */  ~
                              L40270,           /* Last Name          */  ~
                              L40270,           /* Caelus Menu Name   */  ~
                              L40270,           /* User Logon Password*/  ~
                              L40280,           /* DTC Connection     */  ~
                              L40280,           /* Port Connection    */  ~
                              L40270,           /* Data Jack #        */  ~
                              L40270,           /* HP Monitor/PC Flag */  ~
                              L40260            /* Setup Remarks      */
            goto L40310

L40260:         lfac$(fieldnr%) = hex(80)  :  return   /* Up / Low   */
L40270:         lfac$(fieldnr%) = hex(81)  :  return   /* Upper Only */
L40280:         lfac$(fieldnr%) = hex(82)  :  return   /* Numeric    */

        accept_screen
L40310:     accept                                                       ~
                at (01,02),                                              ~
                   "APC User Information Utility Program",               ~
                                                                         ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                                                                         ~
                at (04,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (06,02), "APC User ID                :"      ,        ~
                at (06,35), fac(lfac$(1%)), apc_user_id$        , ch(03),~
                                                                         ~
                at (07,02), "APC Employee ID            :"      ,        ~
                at (07,35), fac(lfac$(2%)), apc_emp_id$         , ch(05),~
                                                                         ~
                at (08,02), "APC Employee First Name    :"      ,        ~
                at (08,35), fac(lfac$(3%)), apc_first_name$     , ch(15),~
                                                                         ~
                at (09,02), "APC Employee Middle Initial:"      ,        ~
                at (09,35), fac(lfac$(4%)), apc_mi$             , ch(01),~
                                                                         ~
                at (10,02), "APC Employee Last Name     :"      ,        ~
                at (10,35), fac(lfac$(5%)), apc_last_name$      , ch(15),~
                                                                         ~
                at (11,02), "Caelus Primary Menu Name   :"      ,        ~
                at (11,35), fac(lfac$(6%)), apc_menu_name$      , ch(16),~
                                                                         ~
                at (12,02), "APC Employee Logon Password:"      ,        ~
                at (12,35), fac(lfac$(7%)), apc_user_pswd$      , ch(10),~
                                                                         ~
                at (13,02), "APC Employee DTC No.       :"      ,        ~
                at (13,35), fac(lfac$(8%)), apc_dtc$            , ch(02),~
                at (13,40), fac(hex(84)),   dtc_desc$           , ch(25),~
                                                                         ~
                at (14,02), "APC Employee DTC/Port No.  :"      ,        ~
                at (14,35), fac(lfac$(9%)), apc_port$           , ch(02),~
                                                                         ~
                at (15,02), "APC Employee Data Jack No. :"      ,        ~
                at (15,35), fac(lfac$(10%)), apc_jack$          , ch(04),~
                                                                         ~
                at (16,02), "HP Terminal / PC Flag      :"      ,        ~
                at (16,35), fac(lfac$(11%)), apc_mon_pc$        , ch(01),~
                                                                         ~
                at (17,02), "APC Employee Setup Remarks :"      ,        ~
                at (17,35), fac(lfac$(12%)), apc_user_rem$      , ch(30),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                                                                         ~
                at (22,02), fac(hex(8c)),   pf$(1)              , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2)              , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3)              , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                if keyhit% <> 15 then goto L40880
                     call "PRNTSCRN"
                     goto accept_screen

L40880:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf1
        if edit% = 2% then L41110     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Report      "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"

            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)

            if fieldnr% = 1% then L41070
                str(pf$(1%),64%) = " " : str(pfkey$,14%,1%) = hex(ff)
                str(pf$(3%),64%) = " " : str(pfkey$,16%,1%) = hex(ff)

L41070:     if fieldnr% > 1% then L41090
                str(pf$(2%),18%,20%) = " " : str(pfkey$,4%,1%) = hex(ff)
L41090:     return

L41110: if fieldnr% > 0% then L41260  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (12)Delete Record      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "

            pfkeys$ = hex(01ffffffffffffffffffff0cff0e0f1000)

            if rec% <> 0% then goto L41230
                str(pf$(2%),18%,20%) = " " : str(pfkey$,12%,1%) = hex(ff)
L41230:     return

                                     /*  Edit Mode - Enabled    */
L41260:     pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "

            pfkeys$ = hex(01ffffffffffffffffffffffffff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50230,         /* User ID               */ ~
                              L50410,         /* Employee ID           */ ~
                              L50380,         /* Employee First Name   */ ~
                              L50380,         /* Employee Mid. Initial */ ~
                              L50380,         /* Employee Last Name    */ ~
                              L50380,         /* Caelus Menu Name      */ ~
                              L50380,         /* Employee Password     */ ~
                              L50570,         /* Employee DTC No.      */ ~
                              L50700,         /* Employee DTC/Port No. */ ~
                              L50820,         /* Employee Data Jack No.*/ ~
                              L50930,         /* HP Monitor/PC Flag    */ ~
                              L50380          /* User Setup Remarks    */
            return

        REM User ID                              APC_USER_ID$
L50230:     if apc_user_id$ = " " then L50360
            if edit% <> 1% then L50380
            apc_key$ = apc_user_id$

            read #2,key = apc_key$, using L50290, apc_menu_name$,         ~
                eod goto L50360
L50290:         FMT POS(74), CH(16)

            gosub dataload

            if rec% <> 1% then L50350
            fieldnr% = 12%
L50350: return
L50360:     errormsg$ = "(Error) - Invalid User ID (Required)?"
            init(" ") apc_user_id$
L50380: return

        REM Employee ID No.                      APC_EMP_ID$
L50410:     if str(apc_emp_id$,1%,1%) = "A" then L50470
            convert apc_emp_id$ to emp%, data goto L50520

            convert emp% to apc_emp_id$, pic (00000)

            goto L50510
L50470:     convert str(apc_emp_id$,2%,4%) to emp%, data goto L50520

            convert emp% to str(apc_emp_id$,2%,4%), pic (0000)

L50510: return
L50520:     errormsg$ = "(Error) - Invalid Employee ID No.?"
            init(" ") apc_jack$
        return

        REM DTC No.                              APC_DTC$
L50570:     convert apc_dtc$ to apc_dtc%, data goto L50650

            convert apc_dtc% to apc_dtc$, pic (00)

            read #3,key = apc_dtc$,       eod  goto L50650

            gosub lookup_dtc
        return
L50650:     errormsg$ = "(Error) - Invalid DTC No. (1 - 8)?"
            init(" ") apc_dtc$
        return

        REM DTC/Port No.                         APC_PORT$
L50700:     convert apc_port$ to apc_port%, data goto L50770

            convert apc_port% to apc_port$, pic (00)

            apc_port% = apc_port% + 1%
            if apc_port% < 1% or apc_port% > 16% then L50770
        return
L50770:     errormsg$ = "(Error) - Invalid DTC/Port No. (0 - 15)?"
            init(" ") apc_port$
        return

        REM Data Jack No.                        APC_JACK$
L50820:     if str(apc_jack$,1%,1%) <> "A" then L50880
            convert str(apc_jack$,2%,3%) to apc_jack%, data goto L50880

            convert apc_jack% to str(apc_jack$,2%,3%), pic (000)

        return
L50880:     errormsg$ = "(Error) - Invalid Data Jack No.?"
            init(" ") apc_jack$
        return

        REM HP Monitor / PC Flag                 APC_MON_PC$
L50930:     if apc_mon_pc$ = "Y" or   apc_mon_pc$ = "N"   then L50990
            if apc_mon_pc$ = " " then apc_mon_pc$ = "Y"
            if apc_mon_pc$ = "Y" then L50990

            errormsg$ = "(Error) - HP/PC Flag (Y or N)?"
            init("Y") apc_mon_pc$
L50990: return

        REM *************************************************************~
            *      R e p o r t   F o r m a t   S t a t e m e n t s      *~
            *************************************************************

                                    /* User Information Report Columns */
L55060: %+---------------------------------------------------------------~
        ~-------------+
L55090: %!---------------------------------------------------------------~
        ~-------------!
L55120: %!                                                               ~
        ~             !

                                    /* Header Format                   */
L55170: %! ######## @ ########   ########################################~
        ~   Page: ### !
                                    /* Detail Format                   */
L55210: %!            User ID: ###            Employee ID: #####         ~
        ~             !
L55230: %! Employee First Name:      ###############                     ~
        ~             !
L55250: %! Employee Middle Initial:  #                                   ~
        ~             !
L55270: %! Employee Last Name:       ###############                     ~
        ~             !
L55290: %! Caelus Primary Menu Name: ################                    ~
        ~             !
L55310: %! Employee Logon Password:  ##########                          ~
        ~             !
L55330: %! Employee DTC No.:         ##                                  ~
        ~             !
L55350: %! Employee DTC/Port No.:    ##                                  ~
        ~             !
L55370: %! Employee Data Jack No.:   ####                                ~
        ~             !
L55390: %! HP Terminal / PC Flag:    #                                   ~
        ~             !
L55410: %! Employee Setup Remarks:   ##############################      ~
        ~             !

        REM *************************************************************~
            *           S p e c i a l   S u b r o u t i n e s           *~
            *************************************************************

        lookup_dtc
            readkey$ = " "
            str(readkey$,1%,9%)   = "APC  EQ03"
            str(readkey$,10%,15%) =  apc_dtc$

            read #4,key = readkey$, using L60100, desc$, eod goto L60140
L60100:         FMT POS(25), CH(25)

            dtc_desc$ = str(desc$,1%,25%)
        return
L60140:     errormsg$ = "(Error) - Invalid DTC Lookup?"
        return

        header_user_id
            if lcnt% <> 99% then print using L55060
            pageno% = pageno% + 1%
            print page
            print using L55060
            print using L55170, date$, xtime$, title$, pageno%
            lcnt% = 3%
        return

        detail_user_id
            gosub header_user_id

            print using L55090
            print using L55210, apc_user_id$, apc_emp_id$
            print using L55120
            print using L55230, apc_first_name$
            print using L55120
            print using L55250, apc_mi$
            print using L55120
            print using L55270, apc_last_name$
            print using L55120
            print using L55290, apc_menu_name$
            print using L55120
            print using L55310, apc_user_pswd$
            print using L55120
            print using L55330, apc_dtc$
            print using L55120
            print using L55350, apc_port$
            print using L55120
            print using L55370, apc_jack$
            print using L55120
            print using L55390, apc_mon_pc$
            print using L55120
            print using L55410, apc_user_rem$
            print using L55120

            for i% = 1% to 12%
                print using L55090
                print using L55120
            next i%
        return

        generate_user_id
            call "SHOSTAT" ("Creating (User ID) Report")
            init(" ") apc_key$

        generate_next_user_id
            read #1,key > apc_key$, eod goto generate_done_user_id

            gosub get_user_data

            apc_key$ = apc_user_id$
            gosub detail_user_id

            goto generate_next_user_id

        generate_done_user_id
            print using L55120
            print using L55060
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")
        end
