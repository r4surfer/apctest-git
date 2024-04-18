        REM *************************************************************~
            *        Glass Re-Make Reason Codes, Barcode Labels         *~
            *  Program Name      - APCBAR01                             *~
            *  Creation Date     - 03/14/97 - Barcode File - (APCBARGN) *~
            *  Last Modified Date- 11/13/97                             *~
            *  Written By        - Roy H. Hoffman                       *~
            *                                                           *~
            *  Description       - Print Barcode labels for Specified   *~
            *                      GENCODES table File (PLAN REMK)      *~
            *                                                           *~
            *                    - 'STUFF%' is Set to the Number of     *~
            *                               Dummy Labels needed at the  *~
            *                               Beginning and End of the    *~
            *                               Label Print run.            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/14/97 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/13/97 ! Mod for Upgrade to new Release R6.04.03  ! RHH *~
            *************************************************************

        dim                                                              ~
            table$9,                     /* Gencodes Table Name        */~
            qty$2,                       /* LABEL QUANTITY             */~
            date$8,                      /* Date for screen display    */~
            desc$25,                     /* GENCODES Description Field */~
            readkey$24,                  /* GENCODES File Read Key     */~
            gen_rec$24,                  /* GENCODES Code Field        */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            cursor%(2%),                 /* Cursor location for edit   */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(2%),                     /* = 0 if the file is open    */~
            f1%(2%),                     /* = 1 if READ was successful */~
            fs%(2%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(2%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Glass Re-Make, Reason Code Labels "
            pname$ = "APCBAR01 - Rev: R6.04"

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
            * #1  ! GENCODES ! CAELUS General Codes File                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =   24

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" ( #1, fs%(1%), f2%( 1%), 0%, rslt$(1%))

            str(rslt$(2%), 1%,6%) = "OUTPUT"   /* File Cannot Exist */
            str(rslt$(2%), 7%,8%) = "00005000" /* Create for ? No. Rec.*/
            str(rslt$(2%),15%,3%) = "100"      /* Dpack Percentage  */
            str(rslt$(2%),18%,3%) = "100"      /* Ipack Percentage  */

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

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 2%
L10090:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10210
L10110:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10190
L10140:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10110
                         if fieldnr% = 1% then L10090
                         goto L10140
L10190:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10110
L10210:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10110
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
                  if keyhit%  = 14% then gosub begin_process
                  if keyhit%  = 16% then goto  exit_program
                  if keyhit% <>  0% then       editpg1

L11130:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 2% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
                  lastfieldnr% = fieldnr%
            goto L11130

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************
        begin_process
            call "SHOSTAT" ("Creating Labels")
            select #2, "APCBARGN", consec, recsize = 64
            call "OPENFILE" (#2, "OUTPT", 0%, rslt$(2%), " " )

            for i% = 1% to qty%
                init(" ") readkey$, gen_rec$
                str(readkey$,1%,9%)   =  table$
                read #1,key > readkey$, using L19150, gen_rec$, desc$,    ~
                                                           eod goto L19190
L19150:             FMT CH(24), CH(25)
                gosub process_gencodes
            next i%

L19190:     close #2
        return clear all
        goto inputmode

        process_gencodes                 /* Set for (PLAN REMK) Table  */
            put #2, using L35030,                                         ~
                "{1``",                            /* Label Def.   ( 4)*/~
                str(gen_rec$,10%,2%), "`",         /* Gencode Code ( 3)*/~
                str(gen_rec$,10%,2%), "`",         /* Barcode      ( 3)*/~
                desc$,                "`",         /* Description  (26)*/~
                "}"                                /* End of Label ( 1)*/
                                                   /* Total = 37       */
            write #2, eod goto error_sub

            readkey$ = gen_rec$
            read #1,key > readkey$, using L19150, gen_rec$, desc$,        ~
                                                           eod goto L19380
            if str(gen_rec$,1%,9%) <> table$ then L19380
            goto process_gencodes
L19380: return
        error_sub
            call "SHOSTAT" ("Error-With Barcode ("&gen_rec$&")")
            stop
            i% = qty%
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
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************
        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28100
                inpmessage$ = edtmessage$
            return

L28100
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
                read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Valid GENCODES Table Name only Valid for (PLAN REMK)?",~
         "Enter a Valid Quantity of Labels.                            "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, table$, qty$

        return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
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
        REM  DATALOAD
        REM  RETURN

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT
        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
L35030:     FMT CH(4),                             /* Label Def.   ( 4)*/~
                CH(2), CH(1),                      /* GENCODES Code( 3)*/~
                CH(2), CH(1),                      /* Barcode      ( 3)*/~
                CH(25), CH(1),                     /* Description  (26)*/~
                CH(1)                              /* End of Label ( 1)*/
        REM                                        /* Total Bytes= 37  */
        REM                                        /* Output File= 64  */

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

            on fieldnr% gosub L40170,           /* Gencodes Table       */~
                              L40180            /* Label Quantity       */
            goto L40200

                lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:         lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40180:         lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                at (06,02), "Gencodes Table Name  :",                    ~
                at (06,25), fac(lfac$(1%)), table$              , ch(09),~
                                                                         ~
                at (07,02), "Quantity of Labels   :",                    ~
                at (07,25), fac(lfac$(2%)), qty$                , ch(02),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                if keyhit% <> 15 then goto L40460
                     call "PRNTSCRN"
                     goto L40200

L40460:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf1
        if edit% = 2% then L40650     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40630
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40630:     return

L40650: if fieldnr% > 0% then L40740  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Print Labels"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40740:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************
        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50110,         /* GENCODES Table        */ ~
                              L50290
            return

L50110: REM Edit GENCODES Table Name              TABLE$
            if table$ <> " " then goto L50150
               table$ = "PLAN REMK"

L50150:     init(" ") readkey$, gen_rec$
            str(readkey$,1%,9%)   =  table$
            read #1,key > readkey$, using L50180, gen_rec$, eod goto L50220
L50180:         FMT CH(24)
            if str(gen_rec$,1%,9%) = table$ then return
            if table$ <> "PLAN REMK" then goto L50250
        return
L50220:     errormsg$ = "(Error) - Must Enter a Valid GENCODES Table ?"
            init(" ") table$, gen_rec$
        return
L50250:     errormsg$ = "(Error) - Utility only valid for (PLAN REMK)?"
            init(" ") table$, gen_rec$
        return

L50290: REM Edit Label Quantity                   QTY$
            qty% = 1%
            convert qty$ to qty%, data goto L50320
L50320:
            convert qty% to qty$, pic(00)
        return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*
        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
