        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN64                             *~
            *  Creation Date     - 04/11/2012                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie Sanders                     *~
            *                                                           *~
            *  Description       - This Program creates the Screen for  *~
            *                      Manufactured Product Explosion of    *~
            *                      NFRC data.                           *~
            *                                                           *~
            *  Files Used        - (NFRCDATA) -Data loaded from corp    *~
            *                    - (NFRCGLS)  -Glass Code Data          *~
            *                    - (NFRCMDL)  -Model to Series data     *~
            *                                                           *~
            *                                                           *~
            *  Subroutines  - (AWDPLA64) - looks up data to be displayed*~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *04/11/2012! New Program for (AWD) - Last Mod Date    ! CMS *~
            *          !                                          !     *~
            *************************************************************

        dim                                                              ~
            hdr$40,                      /* Askuser Header             */~
            msg$(3%)79,                  /* Askuser Messages           */~
            filename$20,                 /* File Name                  */~        
            apc_scr$120,                 /* Screen Description         */~
            apc_prt$60,                  /* Print Description          */~
            apc_sze$20,                  /* Size Description           */~
            sub_scr$120,                 /* Screen Description         */~
            sub_prt$60,                  /* Print Description          */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim                                                              ~
            scr_part$45,                 /* Screen Part                */~
            scr_part_desc$30,            /* MFG Part Number Description*/~
            txt$(20)60,                  /* NFRC DESCRIPTIONS          */~            
            nf_part$25,                  /* MFG Part Number            */~
            nf_subpart$20,               /* MFG Subpart                */~
            ufactor$10,                  /* UFactor                    */~
            sheat$10,                    /* Solar Heat                 */~
            vtranmit$10,                 /* Visible Transmittance      */~
            cpdnumber$30                 /* Cpd Number                 */
            
        dim                                                               ~
            warehouse$4,                     /* Warehouse               */~
            series$10,                       /* Series                  */~
            style$10,                        /* Style                   */~
            spacercode$10,                   /* SpacerCode              */~
            igthickness$10,                  /* IG THICKNESS            */~
            pane1$5,                         /* Pane1                   */~
            pane2$5,                         /* Pane2                   */~
            pane3$5,                         /* Pane3                   */~
            gridcode$5,                      /* Grid Code               */~
            gridsize$10,                     /* Grid Size               */~
            gapfill1$10,                     /* GapFill1                */~
            gapfill2$10,                     /* GapFill2                */~
            framecode$5,                     /* FrameCode               */~
            sashcode$5                       /* SashCode                */            
            

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "  Utility to Lookup/Display NFRC Data   "
            pname$ = "AWDPLN64 -           "

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
            * #1  ! NFRCDATA ! NFRC data from corp                      *~
            * #2  ! NFRCMDL  ! Model to Corp Series                     *~
            * #3  ! NFRCGLS  ! Glass Data                               *~
            * #4  ! GENCODES ! System Master Code Table Files           *~             
            * #5  ! AMTBOMIF ! Validation Data                          *~            
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "NFRCDATA",                                      ~
                        varc,     indexed,  recsize = 1024,               ~
                        keypos = 1,    keylen = 104,                      ~
                        alt key  1, keypos    =   1, keylen = 24, dup,    ~ 
                            key  2, keypos    = 105, keylen = 30, dup

            select #2,  "NFRCMDL",                                       ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 25,    keylen = 20,                     ~
                        alt key 1, keypos = 1, keylen = 44            

            select #3,  "NFRCGLS",                                       ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,    keylen = 7
                        
            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24                        
                        
            select #5,  "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32                         

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            
            filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error    
            filename$ = "AMTBOMIF" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error            

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
            gosub initialize_screen_data

            for fieldnr% = 1% to  1%
L01750:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L01870
L01770:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L01850
L01800:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L01770
                         if fieldnr% = 1% then L01750
                         goto L01800
L01850:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L01770
L01870:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L01770
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            if keyhit% = 0% then gosub initialize_screen_data
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub calc_nfrc
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L02040:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 1% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L02090:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L02090
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L02090
                  lastfieldnr% = fieldnr%
            goto L02040

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            
REM            str(scr_part$,1%,25%)  = "                         "
            str(scr_part$,26%,20%) = "00000000000000000000"
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L02380
                inpmessage$ = edtmessage$
                return

L02380
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Valid Manufactured Product Part Number, Min. = 19 Dg."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, apc_scr$, apc_prt$,        ~
              apc_sze$, sub_scr$, sub_prt$, scr_part$, scr_part_desc$,   ~
              nf_part$, nf_subpart$, ufactor$, sheat$, vtranmit$,        ~
              cpdnumber$

             init(" ") warehouse$, series$, style$, spacercode$,         ~
                igthickness$, pane1$, pane2$, pane3$, gridcode$,         ~
                gridsize$, gapfill1$, gapfill2$, framecode$, sashcode$
        return
        
        initialize_screen_data
           init(" ") txt$()
        return

        REM *************************************************************~
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
        REM DATALOAD
        REM RETURN

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Update Store Data and Part Data                           *~
            *************************************************************
        REM DATAPUT
        REM RETURN

        
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
              on fieldnr% gosub L03150           /* Part Number       */

              goto L03180

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L03150:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L03180:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,15), fac(hex(84)), apc_prt$               , ch(60),~
               at (04,02), "Part Number:",                               ~
               at (04,15), fac(lfac$(1%)), str(scr_part$,1,25)  , ch(25),~
               at (04,45), fac(lfac$(1%)), str(scr_part$,26,20) , ch(20),~
               at (05,45), fac(hex(84)), scr_part_desc$         , ch(30),~
               at (07,10), fac(hex(84)), txt$( 1%)              , ch(60),~
               at (08,10), fac(hex(84)), txt$( 2%)              , ch(60),~
               at (09,10), fac(hex(84)), txt$( 3%)              , ch(60),~
               at (10,10), fac(hex(84)), txt$( 4%)              , ch(60),~
               at (11,10), fac(hex(84)), txt$( 5%)              , ch(60),~
               at (12,10), fac(hex(84)), txt$( 6%)              , ch(60),~
               at (13,10), fac(hex(84)), txt$( 7%)              , ch(60),~
               at (14,10), fac(hex(84)), txt$( 8%)              , ch(60),~
               at (15,10), fac(hex(84)), txt$( 9%)              , ch(60),~
               at (16,10), fac(hex(84)), txt$(10%)              , ch(60),~
               at (17,10), fac(hex(84)), txt$(11%)              , ch(60),~
               at (18,10), fac(hex(84)), txt$(12%)              , ch(60),~
               at (19,10), fac(hex(84)), txt$(13%)              , ch(60),~
               at (20,10), fac(hex(84)), txt$(14%)              , ch(60),~
               at (21,10), fac(hex(84)), txt$(15%)              , ch(60),~
                                                                         ~
               at (22,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L03660
                  call "PRNTSCRN"
                  goto L03180

L03660:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L03850      /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(2%) = "                 (14)Lookup NFRC Data   " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ff0304ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L03790
                str(pf$(2%),64%)   = " " : str(pfkeys$,16%,1%) = hex(ff)
L03790:     if fieldnr% > 1% then L03810
                str(pf$(1%),18%,20%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L03810:     
            return

L03850: if fieldnr% > 0% then L03920   /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (15)Print Screen"
            pf$(2%) = "                 (14)Lookup NFRC Data   " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L03920:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
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
            on fieldnr% gosub L04120           /* Part Number           */

            return

L04120: REM MFG Part Number                       SCR_PART$

          if scr_part$ <> " " then goto L04200
             goto L04250

L04200:      nf_part$ = str(scr_part$,1%,25%)
             nf_subpart$ = str(scr_part$,26%,20%)
             gosub lookup_description
             if err% <> 0% then goto L04250
                scr_part_desc$ = apc_sze$
        return
L04250:     errormsg$ = "(Error) - Invalid Part Number."
            scr_part$, scr_part_desc$, nf_part$, nf_subpart$ = " "
            str(scr_part$,26%,20%) = "00000000000000000000"
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        lookup_description
           err% = 0%

           call "AWDDESCR" (nf_part$, nf_subpart$,                   ~
                            apc_scr$, apc_prt$, sub_scr$,            ~
                                 sub_prt$, apc_sze$, #5, err% )
        return

        calc_nfrc
           call "SHOSTAT" ("Looking up NFRC Data")

           call "AWDPLA64" (nf_part$,nf_subpart$, ufactor$, sheat$,      ~
                            vtranmit$, cpdnumber$, warehouse$,           ~
                            series$, style$, spacercode$, igthickness$,  ~
                            pane1$, pane2$, pane3$, gridcode$, gridsize$,~
                            gapfill1$, gapfill2$, framecode$, sashcode$, ~
                            #1, #2, #3, #4, err%)
           

           gosub build_screen
           if err% <> 0% then return
              if err% = 1% then errormsg$ =                            ~
                  "(Error) - No Model to Series Lookup.   NFRCMDL      "
              if err% = 2% then errormsg$ =                            ~
                  "(Error) - No Glass Data Lookup. NFRCGLS             "                  
              if err% = 3% then errormsg$ =                            ~
                  "(Error) - No Intercept Lookup. Gencodes Intercept   "
              if err% = 4% then errormsg$ =                            ~
                  "(Error) - No Spacer Lookup.  Gencodes SPACERCODE    "                                    
              if err% = 4% then errormsg$ =                            ~
                  "(Error) - No NFRCDATA lookup                        "                  
        return

        build_screen
        REM (1) - 8%,21%  (2) - 32%,10%  (3) - 45%,2%  (4) - 50%,8%

            init(" ") txt$()
            txt$(1%) = "Ufactor --> " & ufactor$
            txt$(2%) = "SHGC    --> " & sheat$
            txt$(3%) = "Vtrans  --> " & vtranmit$
            txt$(4%) = "CPD Num --> " & cpdnumber$
          
                        
            str(txt$(6%),1%,29%)   = "Warehouse --> " & warehouse$
            str(txt$(7%),1%,29%)   = "Series    --> " & series$
            str(txt$(8%),1%,29%)   = "Style     --> " & style$
            str(txt$(9%),1%,29%)   = "Spacer    --> " & spacercode$
            str(txt$(10%),1%,29%)  = "IG Thcknes--> " & igthickness$
            str(txt$(11%),1%,29%)  = "Pane1     --> " & pane1$
            str(txt$(12%),1%,29%)  = "Pane2     --> " & pane2$

            str(txt$(6%),30%,29%)  = "Pane3     --> " & pane3$                     
            str(txt$(7%),30%,29%)  = "Grid Code --> " & gridcode$
            str(txt$(8%),30%,29%)  = "Grid Size --> " & gridsize$
            str(txt$(9%),30%,29%)  = "Gap Fill1 --> " & gapfill1$
            str(txt$(10%),30%,29%) = "Gap Fill2 --> " & gapfill2$                     
            str(txt$(11%),30%,29%) = "Frame Code--> " & framecode$
            str(txt$(12%),30%,29%) = "Sash Code --> " & sashcode$            
            
        return
        
        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_error 
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return
                        


        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end


