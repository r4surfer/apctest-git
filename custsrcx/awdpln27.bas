        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN27                             *~
            *  Creation Date     - 01/15/05                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New program to build new complaint   *~
            *                      Codes.                               *~
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

        dim                              /*                            */~
            genkey$24,                   /* Gencodes Readkey           */~
            gen_desc$30,                 /* Gencodes Description       */~
            col_key$6,                   /* Generic Column Key         */~
            compcol1$4,                  /* Complaint Column1 Readkey  */~
            col1_desc$60,                /* Column 1 Description       */~
            comp$3,                      /* Existing Complaint Code    */~
            comp_desc$30,                /* Existing Complaint Descr   */~
            col1$1,                      /* New Column 1               */~
            valid$36                     /* Valid Codes                */

        dim                              /* (Program) - Variables      */~
            filename$8,                  /* Used By EWDOPEN            */~
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
            apc$   = "(New) New Complaint Code Definition    "
            pname$ = "AWDPLN27 - Rev: R6.04"

        REM *************************************************************

            mat f2% = con

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

            select #1,  "COMPCOL1",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  4 

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24

            select #3, "COMPCOL2",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  5 

            select #4, "COMPCOL3",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  6

            select #5, "COMPCOL4",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  6

            select #6, "COMPCOL5",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  6

            select #7, "COMPCOL6",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  6

            select #8, "COMPCOL7",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  6

            call "SHOSTAT" ("Initialization")


            call "OPENCHCK" (#1, fs%(1%), f2%(1%),20%, rslt$(1%))

            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error

            call "OPENCHCK" (#3, fs%(3%), f2%(3%),20%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),20%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),20%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),20%, rslt$(6%))
            call "OPENCHCK" (#7, fs%(7%), f2%(7%),20%, rslt$(7%))
            call "OPENCHCK" (#8, fs%(8%), f2%(8%),20%, rslt$(8%))


            mat f1% = zer

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
         "Enter a Valid Existing Complaint Code ?                      ",~
         "Enter a Valid Column 1 ?                                     ",~
         "Enter a Valid Description for Column 1 ?                     "


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") genkey$, gen_desc$, compcol1$, col1_desc$, comp$,  ~
                      col1$, comp_desc$

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
               init(" ") compcol1$
               compcol1$ = comp$ & col1$

               read #1, key = compcol1$, eod goto no_record

               get #1, using L35040, comp$,          /* Complaint Code */~
                                     col1$,          /* Column 1       */~
                                     col1_desc$      /* Column 1 Desc  */
               rec% = 1%
        no_record
        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
            datasave
               init(" ") compcol1$
               compcol1$ = comp$ & col1$

               read #1, hold, key = compcol1$, eod goto datawrite
                         delete #1

                         if del% = 1% then return
            datawrite
               put #1, using L35040, comp$,          /* Complaint Code */~
                                     col1$,          /* Column 1       */~
                                     col1_desc$      /* Column 1 Desc  */


               write #1, eod goto datawrite_error

            return clear all
            goto inputmode
    datawrite_error
            errormsg$ = "ERROR WRITING COLUMN 1 - CONTACT SYSTEMS"
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

                                         /* COMPCOL1 - New File Layout */
L35040: FMT CH(03),         /* Complaint Code                          */~
            CH(01),         /* Column 1                                */~
            CH(60)          /* Column 1 Description                    */


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
               at (03,02), "Complaint Code:",                            ~
               at (03,19), fac(lfac$(1%)), comp$                , ch(03),~
               at (03,40), fac(hex(84)), comp_desc$             , ch(30),~
                                                                         ~
               at (04,02), "Column 1      :",                            ~
               at (04,19), fac(lfac$(2%)), col1$                , ch(01),~
                                                                         ~
               at (05,02), "Column 1 Desc :",                            ~
               at (05,19), fac(lfac$(3%)), col1_desc$           , ch(60),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2%  then goto L40240
                    call "AWDPLA27" (#1,#2,#3)
                           goto L40790
L40240:

               if keyhit% <> 3%  then goto L40260
                    call "AWDPLB27" (#1,#2,#3,#4,#5,#6,#7,#8)
                           goto L40790
L40260:


               if keyhit% <> 15 then goto L40790
                  call "PRNTSCRN"


L40790:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40990     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (2)Column2             " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                 (3)Column3             " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01020304ffffffffffffffffffff0f1000)
            return

L40990: if fieldnr% > 0% then L41100  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "(8)Delete Col1         (14) Data Save  "
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
           genkey$ = "COMPLAINT" & comp$
           gosub lookup_complaint
           comp$ = str(genkey$,10%,3%)
           if comp$ = " " then goto L50190

L50180:    
           init(" ") genkey$, gen_desc$
           genkey$ = "COMPLAINT" & comp$
           gosub test_complaint
           if f1%(2) = 0% then goto L50190
           comp_desc$ = gen_desc$
        return
L50190:     errormsg$ = "(Error) - Invalid Complaint Code ?"
            gosub error_prompt
            init(" ") genkey$, gen_desc$, comp$, comp_desc$
        return
        lookup_complaint
           call "PLOWCODE" (#2, genkey$, gen_desc$, 9%, .30, f1%(2))
        return
        test_complaint
           call "DESCRIBE" (#2, genkey$, gen_desc$, 0%, f1%(2))

        return



L50500: REM New Complaint Column 1                COL1$
            if col1$ <> " " then goto L50510
                init(" ") col_key$
                str(col_key$,1%,3%) = comp$
                gosub plow_col1
                col1$ = str(col_key$,4%,1%)
L50510:
            p% = 0%
            p% = pos(valid$ = col1$)
            if p% = 0% then goto L50590
            gosub dataload
               if rec% = 0% then return

                  fieldnr% = 99%
                  edit%    = 2%
        return
L50590:     errormsg$ = "(Error) - Invalid Complaint Column 1 ?"
            gosub error_prompt
            init(" ") col1$
        return

L51000: REM Description for column1               COL1_DESC$
        
        return


        plow_col1
           call "PLOWCODE" (#1, col_key$, col1_desc$, 3%, .30, f1%(1))
        return


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************




        open_error
           comp% = 2%
           hdr$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        
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

