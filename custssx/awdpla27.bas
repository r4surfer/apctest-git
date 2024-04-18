        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLA27                             *~
            *  Creation Date     - 01/15/05                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New subroutine to build new complaint*~
            *                      Codes Column 2.                      *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Spec. Comm (Screen 1) -                                  *~
            *                         PF(10) Create Customer File.      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/15/05 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            *************************************************************

            sub "AWDPLA27" (#1,          /* COMPCOL1                  */~
                            #2,          /* Gencodes                  */~
                            #3  )        /* COMPCOL2                  */

        dim                              /*                            */~
            genkey$24,                   /* Gencodes Readkey           */~
            gen_desc$30,                 /* Gencodes Description       */~
            col_key$6,                   /* Generic Column Key         */~
            compcol2$5,                  /* Complaint Column2 Readkey  */~
            col2_desc$60,                /* Column 2 Description       */~
            comp$4,                      /* Complaint & Code 1 Code    */~
            comp_desc$30,                /* Complaint & Code 1 Descr   */~
            col2$1,                      /* New Column 2               */~
            valid$36                     /* Valid Codes                */

        dim                              /* (Program) - Variables      */~
            hdr$45, msg$(3%)79,          /* Askuser - Var's            */~
            date$8,                      /* REPORT TITLE               */~
            wk_date$8,                   /* Work Date                  */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */



        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Complaint Code Definition Col2   "
            pname$ = "AWDPLA27 - Rev: R6.04"

        REM *************************************************************

            mat f2% = con
            mat fs% = con
            init(" ") rslt$()

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! COMPCOL1 ! New Complaint Column 1                   *~
            * #2  ! GENCODES ! Master Code Tables File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************



        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            wk_date$ = date
            call "DATEFMT" (date$)
            valid$ = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 3%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
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
                  if keyhit%  =  8% then goto  delete_record
                  if keyhit%  = 14% then gosub datasave         
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L11150:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 3% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11200:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11200
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11200
                  lastfieldnr% = fieldnr%
            goto L11150

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************



        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            if rec% = 1% and (fieldnr% = 1% or fieldnr% = 2%) then ~ 
               enabled% = 0%
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Valid Existing Combo Complaint Code and Column 1?    ",~
         "Enter a Valid Column 2 ?                                     ",~
         "Enter a Valid Description for Column 2 ?                     "


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") genkey$, gen_desc$, compcol2$, col2_desc$, comp$,  ~
                      col2$, comp_desc$

            rec% = 0%

        return

        REM *************************************************************~
            *************************************************************

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
        dataload
               rec% = 0%
               init(" ") compcol2$
               compcol2$ = comp$ & col2$

               read #3, key = compcol2$, eod goto no_record

               get #3, using L35040, comp$,          /* Complaint Code */~
                                     col2$,          /* Column 2       */~
                                     col2_desc$      /* Column 2 Desc  */
               rec% = 1%
        no_record
        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
            datasave
               init(" ") compcol2$
               compcol2$ = comp$ & col2$

               read #3, hold, key = compcol2$, eod goto datawrite
                         delete #3

                         if del% = 1% then return
            datawrite
               put #3, using L35040, comp$,          /* Complaint Code */~
                                     col2$,          /* Column 2       */~
                                     col2_desc$      /* Column 2 Desc  */


               write #3, eod goto datawrite_error

            return clear all
            goto inputmode
    datawrite_error
            errormsg$ = "ERROR WRITING COLUMN 2 - CONTACT SYSTEMS"
            gosub error_prompt
            return clear all
            goto inputmode


        delete_record
            del% = 1%
            gosub datasave

            del% = 0%
            goto INPUTMODE


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

                                         /* COMPCOL2 - New File Layout */
L35040: FMT CH(04),         /* Complaint Code                          */~
            CH(01),         /* Column 2                                */~
            CH(60)          /* Column 2 Description                    */


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
REM L40070:   gosub set_pf1

              gosub'050(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40200,         /* Existing Comp Code   */~
                                L40200,         /* Column 1             */~
                                L40200          /* Description          */


              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Comp & Col 1  :",                            ~
               at (03,19), fac(lfac$(1%)), comp$                , ch(04),~
               at (03,40), fac(hex(84)), comp_desc$             , ch(30),~
                                                                         ~
               at (04,02), "Column 2      :",                            ~
               at (04,19), fac(lfac$(2%)), col2$                , ch(01),~
                                                                         ~
               at (05,02), "Column 2 Desc :",                            ~
               at (05,19), fac(lfac$(3%)), col2_desc$           , ch(60),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 15 then goto L40790
                  call "PRNTSCRN"


L40790:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40990     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            return

L40990: if fieldnr% > 0% then L41100  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "(8)Delete Col2         (14) Data Save  "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffff08ffffffffff0e0f1000)
                if rec% = 0% then str(pf$(1%),41%,15%) = " "
                if rec% = 0% then str(pfkeys$,15%,2%) = " "
            return
L41100:
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
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
            on fieldnr% gosub L50150,         /* Complaint Code        */~
                              L50500,         /* New Column 1          */~
                              L51000          /* Description           */~

            return

L50150: REM Complaint Code                        COMP$                 
           if comp$ <> " " then goto L50180
           init(" ") genkey$, gen_desc$
REM        genkey$ = comp$
           gosub lookup_complaint
           comp$ = str(genkey$,1%,4%)
           if comp$ = " " then goto L50190

L50180:    
           init(" ") genkey$, gen_desc$
           genkey$ = comp$
           gosub test_complaint
           if f1%(1) = 0% then goto L50190
           comp_desc$ = gen_desc$
        return
L50190:     errormsg$ = "(Error) - Invalid Complaint Code ?"
            gosub error_prompt
            init(" ") genkey$, gen_desc$, comp$, comp_desc$
        return
        lookup_complaint
           call "PLOWCODE" (#1, genkey$, gen_desc$, 0%, .30, f1%(1))
        return
        test_complaint
           call "DESCRIBE" (#1, genkey$, gen_desc$, 0%, f1%(1))

        return



L50500: REM New Complaint Column 2                COL2$
            if col2$ <> " " then goto L50510
                init(" ") col_key$
                str(col_key$,1%,4%) = comp$
                gosub plow_col2
                col2$ = str(col_key$,5%,1%)
L50510:

            p% = 0%
            p% = pos(valid$ = col2$)
            if p% = 0% then goto L50590
            gosub dataload
               if rec% = 0% then return

                  fieldnr% = 99%
                  edit%    = 2%
        return
L50590:     errormsg$ = "(Error) - Invalid Complaint Column 2 ?"
            gosub error_prompt
            init(" ") col2$
        return

L51000: REM Description for column2               COL2_DESC$
        
        return

        plow_col2
           call "PLOWCODE" (#3, col_key$, col2_desc$, 4%, .30, f1%(3))
        return


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************


        
        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return




        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end

