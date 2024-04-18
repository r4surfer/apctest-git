        REM *************************************************************~
            *                ( As of 07/14/00 - CMG )                   *~
            *      Last Modified Date -                                 *~
            *                                                           *~
            *  EWDCST01 -    New program to create a cross-reference    *~
            *             between linealmate and costing.  Because      *~
            *             of sending a qty of one to the smart saws     *~
            *             and actually two or more (gang cutting).      *~
            *                                                           *~            
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/29/99 ! (New) Program -                          ! CMG *~
            *************************************************************

        dim                                                              ~
            gc_key$31,                   /* Gang Cutting Key           */~
            gencdkey$50,                 /* Generic Readkey            */~
            readkey$24,                  /* GENCODES LOOKUP KEY        */~            
            model$3,                     /* Model Number               */~
            mod_desc$30,                 /* Model Description          */~
            raw_mat$25,                  /* Raw Material Number        */~
            raw_desc$32,                 /* Raw Material Description   */~            
            qty$2,                       /* Quantity Cut               */~
            type$1,                      /* Equation Type (1 thru K)   */~
            type_desc$30,                /* Equation Type Description  */~
            equation$2,                  /* Equation Number            */~
            eq_desc$30,                  /* Equation Number Description*/~
            desc$(21%)16,                /* TYPE CODE DESC$            */~            
                                         /*                            */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            msg$(3%)79,                  /* Used by ASKUSER            */~
            hdr$79,                      /* Error Message Header       */~            
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            fs%(5%),                     /*                            */~
            f2%(5%),                     /*                            */~ 
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim title$38,                    /* Rpt Tiltle                 */~
            xtime$8,                     /* Rpt Time                   */~
            mod$(2%)3,                   /* Rpt Model Codes            */~
            beg_mod$3, ed_mod$3           /* Rpt Selection Model Codes  */
            
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Cross-Reference Pgm-Costing to Linealmate"
            pname$ = "EWDCST01 - Rev: R7.00"

        REM *************************************************************


                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! EWDCSTGC ! New Cross-Reference from Lineal to Cost  *~
            * # 2 ! GENCODES ! General Codes File                       *~
            * # 3 ! HNYMASTR ! Inventory Master File                    *~            
            *************************************************************~
            *       FILE SELECTION AND OPEN CALLS                       *~
            *************************************************************
            
            select #1,   "EWDCSTGC",                                   ~
                        varc,     indexed,  recsize = 64,              ~
                        keypos =    1, keylen =   31

            select #2,  "GENCODES",                                    ~
                        varc,                                          ~
                        indexed,                                       ~
                        recsize = 128,                                 ~
                        keypos = 1, keylen = 24

            select # 3, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup                        

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 100%, " ")
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),   0%, " ")     
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),   0%, " ")
            
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

            for fieldnr% = 1% to   5%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 14% then gosub inputmode_report
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
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
                  if keyhit%  = 16% then       dataput
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 5% then editpg1
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
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode_report
            gosub initialize_variables_report

            for fieldnr% = 1% to   1%
L11500:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L11540
L11510:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L11530
L11520:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L11510
                         if fieldnr% = 1% then L11500
                         goto L11520
L11530:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L11510
L11540:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L11510
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub gen_report
                  if keyhit% <>  0% then       editpg2
L11550:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 1% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L11560:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11560
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11560
                  lastfieldnr% = fieldnr%
            goto L11550

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return

        deffn'052(fieldnr%)
            enabled% = 1%
        return
        
        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
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
         "Enter a Valid Model Number?                                  ",~
         "Enter a Cut Equation Type Code ( 1 thru K )?                 ",~
         "Enter a Equation Number and the Associated Description?      ",~         
         "Enter a Valid Raw Material Number?                           ",~
         "Enter Actual Qty Cut?                                        "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28115
                inpmessage$ = edtmessage$
                return

L28115
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Beginning & Ending Model Number?                             "
         
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
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, gc_key$, model$, mod_desc$, ~
                      raw_mat$, raw_desc$, qty$, type$, type_desc$,       ~
                      equation$, eq_desc$

              rec% = 0%
              desc$(1%) = "Code-Eq. Type   "
              desc$(2%) = "(1)-Wd Cut      "
              desc$(3%) = "(2)-Ht Cut      "
              desc$(4%) = "(3)-Cot Wd Cut  "
              desc$(5%) = "(4)-Or Wd Cut   "
              desc$(6%) = "(5)-1/3,1/3,1/3 "
              desc$(7%) = "(6)-TSO-Wd Cut  "
              desc$(8%) = "(7)-TSO-Ht Cut  "
              desc$(9%) = "(8)-BSO-Wd Cut  "
              desc$(10%) = "(9)-BSO-Ht Cut  "
              desc$(11%) = "(A)-Wd Misc Part"
              desc$(12%) = "(B)-Ht Misc Part"
              desc$(13%) = "(C)-Wd Lit Part "
              desc$(14%) = "(D)-Ht Lit Part "
              desc$(15%) = "(E)-Wd Costmate "
              desc$(16%) = "(F)-Ht Costmate "
              desc$(17%) = "(G)-Wd Costmate "
              desc$(18%) = "(H)-Ht Costmate "
              desc$(19%) = "(I)-Wd Screen   "
              desc$(20%) = "(J)-Ht Screen   "
              desc$(21%) = "(K)-Wd (Avail)  "            
        return

        initialize_variables_report
            init(" ") errormsg$, inpmessage$, beg_mod$, ed_mod$, gc_key$
            
        return
        
        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload
            rec% = 0%
            init(" ") gc_key$
            str(gc_key$,1%,3%)  = model$
            str(gc_key$,4%,25%) = raw_mat$
            str(gc_key$,29%,1%) = type$
            str(gc_key$,30%,2%) = equation$            
            
            read #1, hold, key  = gc_key$, eod goto L30000
 
                   gosub data_get
            
            rec% = 1%


L30000: return

        data_get
            get #1, using L35040, model$,      /* Model Number           */~
                                  raw_mat$,    /* Raw Material Number    */~
                                  type$,       /* Equation Type          */~
                                  equation$,   /* Equation Number        */~
                                  qty$         /* Actual Qty Cut         */
        return
        
        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        dataput
            init(" ") gc_key$
            str(gc_key$,1%,3%)  = model$
            str(gc_key$,4%,25%) = raw_mat$
            str(gc_key$,29%,1%) = type$
            str(gc_key$,30%,2%) = equation$            

            read #1, hold, key  = gc_key$, eod goto L31010

                 delete #1
                 
L31010:     put #1, using L35040, model$,      /* Model Number           */~
                                  raw_mat$,    /* Raw Material Number    */~
                                  type$,       /* Equation Type          */~
                                  equation$,   /* Equation Number        */~                                  
                                  qty$         /* Actual Qty Cut         */

                    
            write #1, eod goto L31010
        return clear all
        goto inputmode

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35040:     FMT CH(03),                  /* Model Number               */~
                CH(25),                  /* Raw Material Number        */~
                CH(01),                  /* Equation Type              */~
                CH(02),                  /* Equation Number            */~                
                CH(02)                   /* Actual Qty Cut             */

        REM *************************************************************~
            *          G E N E R A T E    R E P O R T                   *~
            *-----------------------------------------------------------*~
            * Generates Report based from user criteria.                *~
            *************************************************************
        gen_report
            call "SHOSTAT" (" Generating Report ")
            gosub select_printer
            init(" ") gc_key$
            str(gc_key$,1%,3%)  = beg_mod$
            if str(gc_key$,1%,3%) = "ALL" then str(gc_key$,1%,3%) = " "
            read #1, key  > gc_key$, eod goto gen_report_done
            goto L35110
        gen_report_next
            init(" ") model$, raw_mat$, type$, equation$, qty$
            read #1, eod goto gen_report_done            
 
L35110:        gosub data_get
               str(gc_key$,1%,3%)  = model$
               str(gc_key$,4%,25%) = raw_mat$
               str(gc_key$,29%,1%) = type$
               str(gc_key$,30%,2%) = equation$

               if str(ed_mod$,1%,3%) = "ALL" then goto L35100
               if model$ > ed_mod$ then goto gen_report_done
L35100:        gosub prt_dtl
               goto gen_report_next
        gen_report_done
            print using L55040
            call "SETPRNT" (" ","EWDC",0%,1%)
        return clear all
        goto inputmode
            
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
              on fieldnr% gosub L40160,       /* Model Number          */~
                                L40160,       /* Equation Type         */~
                                L40165,       /* Equation Number       */~              
                                L40160,       /* Raw Material Number   */~
                                L40165        /* Actual Qty Cut        */
              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40165:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
                                                                         ~
               at (04,02), "Model Number     :",                         ~
               at (04,20), fac(lfac$(1%)), model$               , ch(03),~
               at (04,47), fac(hex(84)), mod_desc$              , ch(30),~               
                                                                         ~
               at (05,02), "Eq Type (1 - K)  :",                         ~
               at (05,20), fac(lfac$(2%)), type$                , ch(01),~
               at (05,47), fac(hex(84)), type_desc$             , ch(30),~
                                                                         ~               
               at (06,02), "Equation Number  :",                         ~
               at (06,20), fac(lfac$(3%)), equation$            , ch(02),~
               at (06,47), fac(hex(84)),   eq_desc$             , ch(30),~
                                                                         ~               
               at (07,02), "Raw Material     :",                         ~
               at (07,20), fac(lfac$(4%)), raw_mat$             , ch(25),~
               at (07,47), fac(hex(84)), raw_desc$              , ch(32),~
                                                                         ~               
               at (08,02), "Quantity         :",                         ~
               at (08,20), fac(lfac$(5%)), qty$                 , ch(02),~
                                                                         ~                                                                                        
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 12% then goto L40410
                  if rec% <> 1% then goto L40410
                     delete #1
                     return clear all
                     goto inputmode
                     
L40410:        if keyhit% <> 15% then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

                                                                        
        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &        ~
                      "                       (14)Print Report"
            pf$(2%) = "                 (12)Delete Rec         " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffff0cff0e0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(1%),64%) = " " : str(pfkeys$,14%,1%) = hex(ff)            
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                      (15)Print Screen "
            pf$(3%) = "                                        " &        ~
                      "                      (16)Save Data    "
            pfkeys$ = hex(01ffffffffff0708ffffffffffff0f1000)
         return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                                       "
            pf$(3%) = "                                        " &        ~
                      "                                       " 
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40265        /* Model Number          */
                                
              goto L42190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40265:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
                                                                         ~
               at (04,02), "Beginning & Ending Model Number     :",      ~
               at (04,45), fac(lfac$(1%)), beg_mod$             , ch(03),~
               at (04,55), fac(lfac$(1%)), ed_mod$              , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

                     
               if keyhit% <> 15% then goto L42420
                  call "PRNTSCRN"
                  goto L40190

L42420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

                                                                        
        set_pf2
        if edit% = 2% then L42610     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffff0cffff0f1000)
            if fieldnr% = 1% then L42570
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L42570:     if fieldnr% > 1% then L42590
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42590:     return

L42610: if fieldnr% > 0% then L42700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                      (14)Print Report "
            pf$(2%) = "                                        " &        ~
                      "                      (15)Print Screen "
            pf$(3%) = "                                        " &        ~
                      "                                       "
            pfkeys$ = hex(01ffffffffff0708ffffffffff0e0f1000)
         return
L42700:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                                       "
            pf$(3%) = "                                        " &        ~
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
            on fieldnr% gosub   L50000,       /* Model Number          */~
                                L50200,       /* Equation Type         */~
                                L50300,       /* Equation Number       */~                                
                                L50400,       /* Raw Material Number   */~
                                L50600        /* Actual Qty Cut        */            

            return

L50000: Rem Enter a Model Number                            model$, mod_desc$
            init(" ") gencdkey$
            str(gencdkey$,1%,9%) = "MODEL    "
            str(gencdkey$,10%,3%) = model$
            read #2, key = gencdkey$, using L50010, mod_desc$,    ~
                                        eod goto L50020
L50010:     FMT XX(24), CH(30)
        return
L50020:     errormsg$ = "Invalid Model Number?"
            gosub error_prompt
            init(" ") model$, mod_desc$
        return

L50200: REM Equation Type Code                             type$, type_desc$
           type% = 0%
           if type$ <> " " then goto L50220
              goto L50230
L50220:    type% = pos("123456789ABCDEFGHIJK" = type$)

           if type% < 1% or type% > 20% then goto L50230

           type_desc$ = desc$(type% + 1%)

        return
L50230:    errormsg$ = "Invalid Equation Type Code ( 1 thru K )."
           init(" ") type$, type_desc$
        return

L50300: REM Equation Number                                equation$, eq_desc$
           equation% = 0%
           if equation$ <> " " then goto L50310
              equation$ = "01"
L50310:    convert equation$ to equation%, data goto L50320

           convert equation% to equation$, pic(00)

           gosub lookup_equation
           if e% = 0% then goto L50330

           if equation% < 1% or equation% > 99% then goto L50320

        return
L50320:    errormsg$ = "Invalid Equation Number"
           init(" ") equation$, eq_desc$
        return
L50330:    errormsg$ = "(Error) - Not Defined in 'EQUATIONS' Table"
           init(" ") equation$, eq_desc$
        return

L50400: Rem Enter a Raw Material Number                   raw_mat$, raw_desc$
            init(" ") gencdkey$
            str(gencdkey$,1%,25%) = raw_mat$
            read #3, key = gencdkey$, using L50410, raw_desc$,    ~
                                        eod goto L50420
L50410:     FMT XX(25), CH(32)            
            gosub dataload
              if rec%  = 0% then return
              fieldnr% = 7%
        return
L50420:     errormsg$ = "Invalid Raw Material Number?"
            gosub error_prompt
            init(" ") raw_mat$, raw_desc$
        return        

L50600: Rem Enter Actual Quantity Cut                         qty$
            qty% = 0%
            convert qty$ to qty%, data goto L50610
            
L50610:     convert qty% to qty$, pic(00)

        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************
            
        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub   L52000        /* Model Number          */            

            return

L52000: Rem Enter Beginning & Ending Model Number         beg_mod$, ed_mod$
            if beg_mod$ <> " " then goto L52010
L52030:        beg_mod$, ed_mod$ = "ALL"
            return
            
L52010:     if str(beg_mod$,1%,3%) = "ALL" then goto L52030
            if str(ed_mod$,1%,3%) = " " then str(ed_mod$,1%,3%) = beg_mod$
            mod$(1%) = beg_mod$
            mod$(1%) = ed_mod$

            for i% = 1% to 2%
            
              init(" ") gencdkey$
              str(gencdkey$,1%,9%) = "MODEL    "
              str(gencdkey$,10%,3%) = beg_mod$
              read #2, key = gencdkey$, eod goto L52020

            next i%
            
        return
L52020:     errormsg$ = "Invalid Beginning/Ending Model Number?"
            gosub error_prompt
            init(" ") beg_mod$, ed_mod$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~-----------+

L55070: %!---------------------------------------------------------------~
        ~-----------!        

L55140: %! ######## @ ########   ######################################  ~
        ~ Page: ### !

L55150: %!  Model !     Raw Material Number    !  Eq Type  !  Eq Number !~
        ~  Act Qty  !
        
L55160: %!   ###  !  ######################### !     #     !     ##     !~
        ~    ##     !

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************
            
        lookup_equation
          e% = 0%                             /* GET CUT DESCRIPTION   */
          eq_desc$ = " "
          readkey$ = " "
          str(readkey$,1%,9%)  = "EQUATIONS"
          str(readkey$,10%,2%) = str(model$,1%,1%) & "-"  /* PROD LINE */
          str(readkey$,12%,2%) = type$ & "-"              /* EQ TYPE   */
          str(readkey$,14%,2%) = equation$                /* EQ NUMBER */
          call "DESCRIBE" (#2, readkey$, eq_desc$, 0%, f1%(2%))
          if f1%(2%) = 0% then return
          e% = 1%
        return        

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        select_printer
            title$ = "  EWD Costing Cross-Reference Report  "
            pageno% = 0%
            lcnt%   = 99%
            date$ = date
            call "DATEFMT" (date$)
            call "SETPRNT" (" ","EWDC",0%,0%)
            select printer(134)
        return

        prt_header
            init(" ") xtime$
            call "TIME" (xtime$)
            if lcnt% <> 99% then print using L55040
            pageno% = pageno% + 1%
            print page
            print using L55040
            print using L55140, date$, xtime$, title$, pageno%
            print using L55150
            lcnt% = 3%
        return

        prt_dtl
            if lcnt% > 60% then gosub prt_header
               print using L55070
               print using L55160, model$, raw_mat$, type$, equation$, qty$
               lcnt% = lcnt% + 2%
        return

        
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            end