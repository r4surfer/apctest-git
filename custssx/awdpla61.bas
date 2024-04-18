        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLA61                             *~
            *  Creation Date     - 12/28/04                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie Gregory                     *~
            *                                                           *~
            *  Description       - Report for Master Product Balance    *~
            *                      Cross Referenceerate report.         *~
            *                      Model Specified                      *~
            *                                                           *~
            *  Code Tables Used  - Model                                *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/28/04 ! (New) Program - copied & mod AWDPLN60.   ! CMG *~
            * 06/30/14 ! AWD001 Added vendor2 & type2             ! PWW *~
            * 07/14/14 ! AWD002 Added Max Weight along w/AWD001   ! PWW *~
            *************************************************************

            sub "AWDPLA61" (#1, #4)

        dim                                                              ~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            wk_rec$256,                  /* Read Record                */~
            time$8,                      /* System time                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */



        dim beg_mdl$3,                   /* Beg Vendor Code            */~
            end_mdl$3,                   /* End Vendor Code            */~
            beg_ven$1,                   /* Beg Vendor Code            */~
            end_ven$1,                   /* End Vendor Code            */~
            beg_type$1,                  /* Beg Bal Type               */~
            end_type$1,                  /* End Bal Type               */~
            model$3,                     /* Model                      */~
            top_bot$1,                   /* top / bot code             */~
            vendor$1,                    /* Vendor                     */~
            bal_type$1                   /* Balance Type               */

           


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(AWD) Product Balance Cross Reference "
            pname$ = "AWDPLA61 - Rev: R7.00"      

        REM *************************************************************


                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! AWDPLNBL ! Product Balance Cross Reference          *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
           *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************



        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            today% = 0%
            call "DATEOKC" (date$, today%, errormsg$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            time$ = " "  :  call "TIME" (time$)
            pg% = 0%  :  ln% = 99%
            select printer (134)


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   3%
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
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
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
                  if keyhit%  = 14% then goto  print_report
                  if keyhit%  = 16% then goto  exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 3% then editpg1
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
            *             P R O C E S S   D A T A                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************
                                               
        print_report                           
            call "SHOSTAT" ("Printing Balance  Report...")
            init(" ") readkey$, vendor$, type$ 
            ln%  = 99%                         

            if beg_mdl$ = "ALL" then goto load_data
               readkey$ = beg_mdl$

            goto load_data
            goto exit_program


        load_data                                      
            read #1, key > readkey$,  using L20210, wk_rec$,             ~
                    eod goto load_data_done
L20210:         fmt ch(256)

            readkey$ = str(wk_rec$,1%,4%)
            model$   = str(wk_rec$,1%,3%)
            vendor$  = str(wk_rec$,5%,1%)
            type$    = str(wk_rec$,6%,1%) 
            vendor2$ = str(wk_rec$,31%,1%)              /*<AWD001>*/
            type2$   = str(wk_rec$,32%,1%)


            if beg_mdl$ = "ALL" then goto not_mdl
            if beg_mdl$ < model$ or end_mdl$ > model$ then goto load_data
not_mdl:

            if beg_ven$ = "A" then goto not_ven
            if beg_ven$ < vendor$ or end_ven$ > vendor$ then goto load_data
not_ven:

            if beg_type$ = "A" then goto not_type
            if beg_type$ < type$ or end_type$ > type$ then goto load_data

not_type:

            top_bot$ = str(wk_rec$,4%,1%)
            get str(wk_rec$) using L20220, friction, max_weight /*<AWD001>*/
L20220:            FMT POS(07), PD(14,4), POS(33), PD(14,4)
            gosub print_data      
            goto load_data
            
            load_data_done
            goto exit_program



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
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Valid Model Range or 'ALL'                           ",~
         "Enter a Valid Balance Vendor Range or 'A'LL                  ",~
         "Enter a Valid Balance Type Range  or 'A'LL                   "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, beg_ven$, end_ven$,        ~
                      beg_type$, end_type$, vendor$, type$, model$,      ~
                      beg_mdl$, end_mdl$, top_bot$,  vendor$, bal_type$

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
        REM DATALOAD
        REM RETURN

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
              on fieldnr% gosub L40160,          /* Beg/End Model      */~
                                L40160,          /* Beg/End Vendor     */~
                                L40160           /* Beg/End Type       */

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,24), fac(hex(a4)), apc$                   , ch(39),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Beg/End Model   :",                          ~
               at (03,25), fac(lfac$(1%)), beg_mdl$             , ch(03),~
               at (03,40), fac(lfac$(1%)), end_mdl$             , ch(03),~
                                                                         ~
               at (04,02), "Beg/End Vendor  :",                          ~
               at (04,25), fac(lfac$(2%)), beg_ven$             , ch(01),~
               at (04,40), fac(lfac$(2%)), end_ven$             , ch(01),~
                                                                         ~
               at (05,02), "Beg/End Bal Type:",                          ~
               at (05,25), fac(lfac$(3%)), beg_type$            , ch(01),~
               at (05,40), fac(lfac$(3%)), end_type$            , ch(01),~
                                                                         ~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 15 then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return

L40700:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
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
            on fieldnr% gosub L50010,        /* Beg/End Model          */~
                              L50050,        /* Beg/End Vendor         */~
                              L50100         /* Beg/End Type           */~

            return

L50010: Rem Enter a Valid Beg/End Model            beg_mdl$, end_mdl$ 
            if beg_mdl$ = " " then beg_mdl$ = "ALL"
            if end_mdl$ = " " then end_mdl$ = beg_mdl$
    
            if end_mdl$ = "ALL" then beg_mdl$ = "ALL"
            if beg_mdl$ = "ALL" then end_mdl$ = "ALL"

            if beg_mdl$ = "ALL" or end_mdl$ = "ALL" then return

            readkey$ = "MODEL    " & beg_mdl$
            gosub test_model   

            if errormsg$ <> " " then return

            readkey$ = "MODEL    " & end_mdl$
            gosub test_model 

            if errormsg$ <> " " then return
            
            

        return
        test_model 
           call "DESCRIBE" (#4, readkey$, desc$, 0%, f1%(4))
           if f1%(4) = 0% then errormsg$ = "Invalid Model "
        return

L50050: Rem Enter a Valid Beg/End Bal Vendor       beg_ven$, end_ven$ 
            if beg_ven$ = " " then beg_ven$ = "A"
            if end_ven$ = " " then end_ven$ = beg_ven$
    
            if end_ven$ = "A" then beg_ven$ = "A"
            if beg_ven$ = "A" then end_ven$ = "A"

            if beg_ven$ = "A" or end_ven$ = "A" then return

            readkey$ = "BAL VENOR" & beg_ven$
            gosub test_vendor

            if errormsg$ <> " " then return

            readkey$ = "BAL VENOR" & end_ven$
            gosub test_vendor

            if errormsg$ <> " " then return
            
            

        return
        test_vendor
           call "DESCRIBE" (#4, readkey$, desc$, 0%, f1%(4))
           if f1%(4) = 0% then errormsg$ = "Invalid Vendor"
        return

L50100: Rem Enter a Valid Beg/End Bal Type         beg_type$, end_type$ 

            if beg_type$ = " " then beg_type$ = "A"
            if end_type$ = " " then end_type$ = beg_type$
    
            if end_type$ = "A" then beg_type$ = "A"
            if beg_type$ = "A" then end_type$ = "A"

            if beg_type$ = "A" or end_type$ = "A" then return

            readkey$ = "BAL TYPES" & beg_type$
            gosub test_types 

            if errormsg$ <> " " then return

            readkey$ = "BAL TYPES" & end_type$
            gosub test_types 

            if errormsg$ <> " " then return
            
            

        return
        test_types  
           call "DESCRIBE" (#4, readkey$, desc$, 0%, f1%(4))
           if f1%(4) = 0% then errormsg$ = "Invalid Type  "
        return

        REM *************************************************************~
            *          R E P O R T   G E N E R A T I O N                *~
            *************************************************************

        print_data        
            if ln% > 58% then gosub print_headings
            print using L64040, model$, top_bot$, vendor$, type$, friction,~
                                vendor2$, type2$, max_weight  /*<AWD001>*/
            ln% = ln% + 1%
            return

        print_headings
            pg% = pg% + 1%
            print page
            print using L64000, date$, time$, apc$, userid$, pg%
            print using L64010, beg_mdl$, end_mdl$, beg_ven$,   ~
                                end_ven$, beg_type$, end_type$

            print
            print using L64020                                
            print using L64030  
            ln% = 5%
            return
    

            
        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L64000: %  Date: ##########  Time: ########       #######################~
        ~################# (AWDPLA59)              User: ###  Page: ####

L64010: %  Beg/End Model : ###  ###      Beg/End Vendor : #  #      Beg/~
        ~End Type : #  #                

/*L64020: % Model  Top/Bot  Vendor  Type  Friction   <AWD001>         */            
L64020: % Model  Top/Bot  Vendor  Type  Friction  Vendor2  Type2 Max Weight               


L64030: % -----  -------  ------  ----  --------  -------  ----- ----------
/*L64030: % -----  -------  ------  ----  --------   <AWD001>         */

/*L64040: %  ###      #        #      #   ###.####   <AWD001>         */
L64040: %  ###      #        #      #   ###.####     #       #   ###.####

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************


        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            close printer

            end

