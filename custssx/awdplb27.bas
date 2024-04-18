        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLB27                             *~
            *  Creation Date     - 01/15/05                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New subroutine to build new complaint*~
            *                      Codes Column 3.                      *~
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

            sub "AWDPLB27" (#1,          /* COMPCOL1                  */~
                            #2,          /* Gencodes                  */~
                            #3,          /* COMPCOL2                  */~
                            #4,          /* COMPCOL3                  */~
                            #5,          /* COMPCOL4                  */~
                            #6,          /* COMPCOL5                  */~
                            #7,          /* COMPCOL6                  */~
                            #8   )       /* COMPCOL7                  */

        dim                              /*                            */~
            genkey$24,                   /* Gencodes Readkey           */~
            gen_desc$30,                 /* Gencodes Description       */~
            compcol3$6,                  /* Complaint Column3 Readkey  */~
            col3_desc$60,                /* Column 3 Description       */~
            comp$5,                      /* Complaint & Code 1,2 Code  */~
            comp_desc$30,                /* Complaint & Code 1,2 Descr */~
            col3$1,                      /* New Column 3               */~
            col_key$6,                   /* Generic Column Key         */~
            col4$1,                      /* New Column 4               */~
            col4_desc$60,                /* New Column 4 Desc          */~
            col5$1,                      /* New Column 5               */~
            col5_desc$60,                /* New Column 5 Desc          */~
            col6$1,                      /* New Column 6               */~
            col6_desc$60,                /* New Column 6 Desc          */~
            col7$1,                      /* New Column 7               */~
            col7_desc$60,                /* New Column 7 Desc          */~
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
            apc$   = "(New) Complaint Code Definition Col3   "
            pname$ = "AWDPLB27 - Rev: R6.04"

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
            valid$ = "0123456789ZABCDEFGHIJKLMNOPQRSTUVWXYZ"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 11%
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

                  if keyhit%  =  8% then goto  delete_record3
                  if keyhit%  =  9% then goto  delete_record4
                  if keyhit%  = 10% then goto  delete_record5
                  if keyhit%  = 11% then goto  delete_record6
                  if keyhit%  = 12% then goto  delete_record7

                  if keyhit%  = 14% then gosub datasave         
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L11150:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 11% then editpg1
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
         "Enter a Valid Existing Combo Complaint Code and Column 1,2?  ",~
         "Enter a Valid Column 3 ?                                     ",~
         "Enter a Valid Description for Column 3 ?                     ",~
         "Enter a Valid Column 4 ?                                     ",~
         "Enter a Valid Description for Column 4 ?                     ",~
         "Enter a Valid Column 5 ?                                     ",~
         "Enter a Valid Description for Column 5 ?                     ",~
         "Enter a Valid Column 6 ?                                     ",~
         "Enter a Valid Description for Column 6 ?                     ",~
         "Enter a Valid Column 7 ?                                     ",~
         "Enter a Valid Description for Column 7 ?                     "


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") genkey$, gen_desc$, compcol3$, col3_desc$, comp$,  ~
                      col3$, comp_desc$, col4$, col4_desc$, col5$,       ~
                      col5_desc$, col6$, col6_desc$, col7$, col7_desc$

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
               init(" ") compcol3$
               compcol3$ = comp$ & col3$

               read #4, key = compcol3$, eod goto no_record

               get #4, using L35040, comp$,          /* Complaint Code */~
                                     col3$,          /* Column 3       */~
                                     col3_desc$      /* Column 3 Desc  */
               rec% = 1%
        no_record
        return

        dataload4
               rec4% = 0%
               init(" ") col_key$
REM               col_key$ = comp$ & col3$ & col4$
               col_key$ = comp$ & col4$

               read #5, key = col_key$, eod goto no_record4

               get #5, using L35090, col_key$,       /* Column Key     */~
                                     col4$,          /* Column 4       */~
                                     col4_desc$      /* Column 4 Desc  */
               rec4% = 1%
        no_record4
        return

        dataload5
               rec5% = 0%
               init(" ") col_key$
REM               col_key$ = comp$ & col3$ & col5$
               col_key$ = comp$ & col5$

               read #6, key = col_key$, eod goto no_record5

               get #6, using L35090, col_key$,       /* Column key     */~
                                     col5$,          /* Column 5       */~
                                     col5_desc$      /* Column 5 Desc  */
               rec5% = 1%
        no_record5
        return

        dataload6
               rec6% = 0%
               init(" ") col_key$
REM               col_key$ = comp$ & col3$ & col6$
               col_key$ = comp$ & col6$

               read #7, key = col_key$, eod goto no_record6

               get #7, using L35090, col_key$,       /* Column key     */~
                                     col6$,          /* Column 6       */~
                                     col6_desc$      /* Column 6 Desc  */
               rec6% = 1%
        no_record6
        return


        dataload7
               rec7% = 0%
               init(" ") col_key$
REM               col_key$ = comp$ & col3$ & col7$
               col_key$ = comp$ & col7$

               read #8, key = col_key$, eod goto no_record7

               get #8, using L35090, col_key$,       /* Column key     */~
                                     col7$,          /* Column 7       */~
                                     col7_desc$      /* Column 7 Desc  */
               rec7% = 1%
        no_record7
        return


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
            datasave
               init(" ") compcol3$
               compcol3$ = comp$ & col3$

               read #4, hold, key = compcol3$, eod goto datawrite
                         delete #4
            datawrite
               put #4, using L35040, comp$,          /* Complaint Code */~
                                     col3$,          /* Column 3       */~
                                     col3_desc$      /* Column 3 Desc  */


               write #4, eod goto datawrite_error

               gosub datasave4
               gosub datasave5
               gosub datasave6
               gosub datasave7
            return clear all
            goto inputmode
    datawrite_error
            errormsg$ = "ERROR WRITING COLUMN 3 - CONTACT SYSTEMS"
            gosub error_prompt
            return clear all
            goto inputmode

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E  4,5,6,7 *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

            datasave4
               init(" ") col_key$
               col_key$ = comp$ & col4$

               read #5, hold, key = col_key$, eod goto datawrite4
                         delete #5

                         if del% = 1% then return
            datawrite4
               put #5, using L35040, comp$,          /* Complaint Code */~
                                     col4$,          /* Column 4       */~
                                     col4_desc$      /* Column 4 Desc  */


               write #5, eod goto datawrite_error4

            return
    datawrite_error4
            errormsg$ = "ERROR WRITING COLUMN 4 - CONTACT SYSTEMS"
            gosub error_prompt
            return clear all
            goto inputmode


            datasave5
               init(" ") col_key$
               col_key$ = comp$ & col5$

               read #6, hold, key = col_key$, eod goto datawrite5
                         delete #6

                         if del% = 1% then return
            datawrite5
               put #6, using L35040, comp$,          /* Complaint Code */~
                                     col5$,          /* Column 5       */~
                                     col5_desc$      /* Column 5 Desc  */


               write #6, eod goto datawrite_error5

            return
    datawrite_error5
            errormsg$ = "ERROR WRITING COLUMN 5 - CONTACT SYSTEMS"
            gosub error_prompt
            return clear all
            goto inputmode


            datasave6
               init(" ") col_key$
               col_key$ = comp$ & col6$

               read #7, hold, key = col_key$, eod goto datawrite6
                         delete #7

                         if del% = 1% then return
            datawrite6
               put #7, using L35040, comp$,          /* Complaint Code */~
                                     col6$,          /* Column 6       */~
                                     col6_desc$      /* Column 6 Desc  */

               write #7, eod goto datawrite_error6

            return
    datawrite_error6
            errormsg$ = "ERROR WRITING COLUMN 6 - CONTACT SYSTEMS"
            gosub error_prompt
            return clear all
            goto inputmode


            datasave7
               init(" ") col_key$
               col_key$ = comp$ & col7$

               read #8, hold, key = col_key$, eod goto datawrite7
                         delete #8

                         if del% = 1% then return
            datawrite7
               put #8, using L35040, comp$,          /* Complaint Code */~
                                     col7$,          /* Column 7       */~
                                     col7_desc$      /* Column 7 Desc  */


               write #8, eod goto datawrite_error7

            return
    datawrite_error7
            errormsg$ = "ERROR WRITING COLUMN 7 - CONTACT SYSTEMS"
            gosub error_prompt
            return clear all
            goto inputmode


        REM *************************************************************~
            *          D E L E T E  D A T A  3, 4, 5, 6, 7              *~
            *-----------------------------------------------------------*~
            * Deletes Data From Files.                                  *~
            *************************************************************



        delete_record3
            del% = 1%
            gosub datasave

            del% = 0%
            goto INPUTMODE


        delete_record4
            del% = 1%
            gosub datasave4

            del% = 0%
            goto INPUTMODE


        delete_record5
            del% = 1%
            gosub datasave5

            del% = 0%
            goto INPUTMODE

        delete_record6
            del% = 1%
            gosub datasave6

            del% = 0%
            goto INPUTMODE

        delete_record7
            del% = 1%
            gosub datasave7

            del% = 0%
            goto INPUTMODE
    

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

                                         /* COMPCOL3 - New File Layout */
L35040: FMT CH(05),         /* Complaint Code                          */~
            CH(01),         /* Column 3                                */~
            CH(60)          /* Column 3 Description                    */

L35080: FMT CH(04),         /* Column Key                              */~
            CH(01),         /* Column 3                                */~
            CH(01),         /* Column 4,5,6,7                          */~
            CH(60)          /* Column 4,5,6,7 Description              */

L35090: FMT CH(05),         /* Column Key                              */~
            CH(01),         /* Column 4,5,6,7                          */~
            CH(60)          /* Column 4,5,6,7 Description              */


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
                                L40200,         /* Column 3             */~
                                L40200,         /* Description          */~
                                L40200,         /* Column 4             */~
                                L40200,         /* Description          */~
                                L40200,         /* Column 5             */~
                                L40200,         /* Description          */~
                                L40200,         /* Column 6             */~
                                L40200,         /* Description          */~
                                L40200,         /* Column 7             */~
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
               at (03,02), "Comp & Col 1,2:",                            ~
               at (03,19), fac(lfac$(1%)), comp$                , ch(05),~
               at (03,40), fac(hex(84)), comp_desc$             , ch(30),~
                                                                         ~
               at (04,02), "Column 3      :",                            ~
               at (04,19), fac(lfac$(2%)), col3$                , ch(01),~
                                                                         ~
               at (05,02), "Column 3 Desc :",                            ~
               at (05,19), fac(lfac$(3%)), col3_desc$           , ch(60),~
                                                                         ~
               at (06,02), "Column 4      :",                            ~
               at (06,19), fac(lfac$(4%)), col4$                , ch(01),~
                                                                         ~
               at (07,02), "Column 4 Desc :",                            ~
               at (07,19), fac(lfac$(5%)), col4_desc$           , ch(60),~
                                                                         ~
               at (08,02), "Column 5      :",                            ~
               at (08,19), fac(lfac$(6%)), col5$                , ch(01),~
                                                                         ~
               at (09,02), "Column 5 Desc :",                            ~
               at (09,19), fac(lfac$(7%)), col5_desc$           , ch(60),~
                                                                         ~
               at (10,02), "Column 6      :",                            ~
               at (10,19), fac(lfac$(8%)), col6$                , ch(01),~
                                                                         ~
               at (11,02), "Column 6 Desc :",                            ~
               at (11,19), fac(lfac$(9%)), col6_desc$           , ch(60),~
                                                                         ~
               at (12,02), "Column 7      :",                            ~
               at (12,19), fac(lfac$(10%)), col7$               , ch(01),~
                                                                         ~
               at (13,02), "Column 7 Desc :",                            ~
               at (13,19), fac(lfac$(11%)), col7_desc$          , ch(60),~
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
            pf$(1%) = "(1)Start Over          (8)Delete Col3   " &       ~
                      "(11)Delete Col6        (14) Data Save  "
            pf$(2%) = "                       (9)Delete Col4   " &       ~
                      "(12)Delete Col7        (15)Print Screen"
            pf$(3%) = "                       (10)Delete Col5  " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffff08090a0b0cff0e0f1000)

                if rec% = 0% then str(pf$(1%),24%,15%) = " "
                if rec% = 0% then str(pfkeys$,15%,2%) = " "

                if rec4% = 0% then str(pf$(2%),24%,15%) = " "
                if rec4% = 0% then str(pfkeys$,17%,2%) = " "

                if rec5% = 0% then str(pf$(3%),24%,16%) = " "
                if rec5% = 0% then str(pfkeys$,19%,2%) = " "

                if rec6% = 0% then str(pf$(1%),41%,15%) = " "
                if rec6% = 0% then str(pfkeys$,21%,2%) = " "

                if rec7% = 0% then str(pf$(2%),41%,15%) = " "
                if rec7% = 0% then str(pfkeys$,23%,2%) = " "

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
                              L50500,         /* New Column 3          */~
                              L51000,         /* Description           */~
                              L51500,         /* New Column 4          */~
                              L52000,         /* Description           */~
                              L52500,         /* New Column 5          */~
                              L53000,         /* Description           */~
                              L53500,         /* New Column 6          */~
                              L54000,         /* Description           */~
                              L54500,         /* New Column 7          */~
                              L55000          /* Description           */


            return

L50150: REM Complaint Code                        COMP$                 
           if comp$ <> " " then goto L50180
           init(" ") genkey$, gen_desc$
REM        genkey$ = comp$
           gosub lookup_complaint
           comp$ = str(genkey$,1%,5%)
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
           call "PLOWCODE" (#3, genkey$, gen_desc$, 0%, .30, f1%(1))
        return
        test_complaint
           call "DESCRIBE" (#3, genkey$, gen_desc$, 0%, f1%(1))

        return



L50500: REM New Complaint Column 3                COL3$
            if col3$ <> " " then goto L50510
                init(" ") col_key$
                str(col_key$,1%,5%) = comp$
                gosub plow_col3
                col3$ = str(col_key$,6%,1%)

L50510:
            p% = 0%
            p% = pos(valid$ = col3$)
            if p% = 0% then goto L50590
            gosub dataload
               if rec% = 0% then return
                  fieldnr% = 3%
REM               edit%    = 2%
        return
L50590:     errormsg$ = "(Error) - Invalid Complaint Column 3 ?"
            gosub error_prompt
            init(" ") col3$
        return

L51000: REM Description for column3               COL3_DESC$
        
        return

        plow_col3
           call "PLOWCODE" (#4, col_key$, col3_desc$, 5%, .30, f1%(4))
        return


L51500: REM New Complaint Column 4                COL4$
            if col4$ <> " " then goto L51510
                init(" ") col_key$
                str(col_key$,1%,5%) = comp$ & col3$
                gosub plow_col4
                col4$ = str(col_key$,5%,1%)

L51510:
            p% = 0%
            p% = pos(valid$ = col4$)
            if p% = 0% then goto L51590

            gosub dataload4
               if rec4% = 0% then return

                  fieldnr% = 5%
REM               edit%    = 2%
        return
L51590:     errormsg$ = "(Error) - Invalid Complaint Column 4 ?"
            gosub error_prompt
            init(" ") col4$
        return

L52000: REM Description for column4               COL4_DESC$
        
        return


        plow_col4
           call "PLOWCODE" (#5, col_key$, col4_desc$, 5%, .30, f1%(5))
        return

L52500: REM New Complaint Column 5                COL5$
            if col5$ <> " " then goto L52510
                init(" ") col_key$
                str(col_key$,1%,5%) = comp$ & col3$
                gosub plow_col5
                col5$ = str(col_key$,5%,1%)

L52510:
            p% = 0%
            p% = pos(valid$ = col5$)
            if p% = 0% then goto L52590

            gosub dataload5
               if rec5% = 0% then return

                  fieldnr% = 7%
REM               edit%    = 2%
        return
L52590:     errormsg$ = "(Error) - Invalid Complaint Column 5 ?"
            gosub error_prompt
            init(" ") col5$
        return

L53000: REM Description for column5               COL5_DESC$
        
        return



        plow_col5
           call "PLOWCODE" (#6, col_key$, col5_desc$, 5%, .30, f1%(6))
        return

L53500: REM New Complaint Column 6                COL6$ 
            if col6$ <> " " then goto L53510
                init(" ") col_key$
                str(col_key$,1%,5%) = comp$ & col3$
                gosub plow_col6
                col6$ = str(col_key$,5%,1%)

L53510:         
            p% = 0%
            p% = pos(valid$ = col6$)
            if p% = 0% then goto L53590

            gosub dataload6
               if rec6% = 0% then return

                  fieldnr% = 9%
REM               edit%    = 2%

        return
L53590:     errormsg$ = "(Error) - Invalid Complaint Column 6 ?"
            gosub error_prompt
            init(" ") col6$
        return

L54000: REM Description for column6               COL6_DESC$
        
        return

        plow_col6
           call "PLOWCODE" (#7, col_key$, col6_desc$, 5%, .30, f1%(7))
        return

L54500: REM New Complaint Column 7                COL7$
            if col7$ <> " " then goto L54510
                init(" ") col_key$
                str(col_key$,1%,5%) = comp$ & col3$
                gosub plow_col7
                col7$ = str(col_key$,5%,1%)

L54510:
            p% = 0%
            p% = pos(valid$ = col7$)
            if p% = 0% then goto L54590


            gosub dataload7
               if rec7% = 0% then return

                  fieldnr% = 11%
                  edit%    = 2%
        return
L54590:     errormsg$ = "(Error) - Invalid Complaint Column 7 ?"
            gosub error_prompt
            init(" ") col7$
        return

L55000: REM Description for column7               COL7_DESC$
        
        return


        plow_col7
           call "PLOWCODE" (#8, col_key$, col7_desc$, 5%, .30, f1%(8))
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

